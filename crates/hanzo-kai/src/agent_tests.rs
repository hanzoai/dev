use super::*;
use crate::fake;
use crate::fake::on;
use pretty_assertions::assert_eq;
use program::Kind;
use std::sync::atomic::Ordering;

/// Kai for tests: `kai.toml` is `settings`, answers come from `rule`.
fn kai(
    settings: &str,
    rule: impl Fn(&str, &program::Question, &Value) -> Vec<f64> + Send + Sync + 'static,
) -> (tempfile::TempDir, Arc<fake::Kai>, Arc<Kai>) {
    let home = tempfile::tempdir().unwrap();
    let settings = crate::Settings::parse(settings, home.path()).unwrap();
    let ask = Arc::new(fake::Kai::new(rule));
    let kai = Arc::new(Kai::with(
        settings,
        crate::Decider::with(ask.clone() as crate::decide::Shared),
    ));
    (home, ask, kai)
}

fn thread(kai: &Arc<Kai>) -> (Arc<Thread>, Arc<fake::Thread>) {
    let act = Arc::new(fake::Thread::default());
    let thread = Arc::new(Thread::new(
        Arc::clone(kai),
        "enso-auto".into(),
        act.clone(),
    ));
    (thread, act)
}

/// Deny what removes files recursively, allow the rest; no escalation.
fn risky(_: &str, q: &program::Question, state: &Value) -> Vec<f64> {
    match q.kind {
        Kind::Choice => {
            let rm = state["command"]
                .as_str()
                .unwrap_or_default()
                .contains("rm -rf");
            on(q, if rm { "deny" } else { "allow" }, 0.9)
        }
        _ => vec![0.95, 0.05],
    }
}

fn exec(command: &str) -> Value {
    json!({"tool": "exec_command", "command": ["bash", "-lc", command], "cwd": "/repo"})
}

async fn start(thread: &Arc<Thread>, turn: &str, text: &str) {
    let request = Request {
        text: text.into(),
        images: false,
    };
    thread.route(turn, request).await;
}

#[tokio::test(flavor = "multi_thread")]
async fn an_enforced_verdict_tightens_the_policy_and_never_loosens_it() {
    let (home, _, kai) = kai("[ops.risk]\nmode = \"enforced\"", risky);
    let (thread, _) = thread(&kai);
    start(&thread, "t1", "clean the build").await;
    let refused = thread
        .risk("t1", "c1", Verdict::Allow, exec("rm -rf /"))
        .await;
    let allowed = thread.risk("t1", "c2", Verdict::Allow, exec("ls")).await;
    let denied = thread.risk("t1", "c3", Verdict::Deny, exec("ls")).await;
    assert_eq!(
        (refused, allowed, denied),
        (Verdict::Deny, Verdict::Allow, Verdict::Deny)
    );

    thread.finished("t1", "c1", "shell".into(), "blocked").await;
    let risk: Vec<Value> = fake::lines(&home.path().join("kai/decisions.jsonl"), 1)
        .into_iter()
        .filter(|l| l["ops"].get("risk").is_some())
        .collect();
    let line = &risk[0];
    assert_eq!(line["family"], "dev");
    assert_eq!(line["request"], "t1");
    assert_eq!(line["sku"], "enso-auto");
    assert_eq!(line["outcome"]["finish"], "blocked");
    let step = &line["ops"]["risk"];
    assert_eq!(step["program"], "agent.command-risk@1");
    assert_eq!(step["mode"], "enforced");
    assert_eq!(step["gates"], json!(["enforced"]));
    assert_eq!(step["kai"], "deny");
    assert_eq!(step["taken"], "deny");
    assert_eq!(step["applied"], true);
    assert_eq!(step["model"], "laya-agent@fake");
    assert!(step["states"][0].as_str().unwrap().starts_with("sha256:"));
    let verdict = &step["answers"][0]["verdict"];
    assert!(
        (verdict["deny"].as_f64().unwrap() - 0.9).abs() < 1e-9,
        "{verdict}"
    );
    assert!(
        !line.to_string().contains("rm -rf"),
        "a line holds hashes, never the command"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_shadow_verdict_is_recorded_and_changes_nothing() {
    let (home, _, kai) = kai("", risky);
    let (thread, act) = thread(&kai);
    start(&thread, "t1", "clean").await;
    let verdict = thread
        .risk("t1", "c1", Verdict::Allow, exec("rm -rf target"))
        .await;
    assert_eq!(verdict, Verdict::Allow);
    thread.finished("t1", "c1", "shell".into(), "ok").await;
    thread.turn_ended("completed", String::new());
    let lines = fake::lines(&home.path().join("kai/decisions.jsonl"), 3);
    let risk = lines
        .iter()
        .find(|l| l["ops"].get("risk").is_some())
        .unwrap();
    assert_eq!(risk["ops"]["risk"]["mode"], "shadow");
    assert_eq!(risk["ops"]["risk"]["gates"], json!(["shadow"]));
    assert_eq!(risk["ops"]["risk"]["kai"], "deny");
    assert_eq!(risk["ops"]["risk"]["taken"], "allow");
    assert_eq!(risk["ops"]["risk"]["applied"], false);
    assert!(
        act.warned.lock().unwrap().is_empty(),
        "shadow shows nothing"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn an_advisory_verdict_is_shown_and_the_policy_decides() {
    let (_home, _, kai) = kai("[ops.risk]\nmode = \"advisory\"", risky);
    let (thread, act) = thread(&kai);
    start(&thread, "t1", "clean").await;
    let verdict = thread
        .risk("t1", "c1", Verdict::Allow, exec("rm -rf target"))
        .await;
    assert_eq!(verdict, Verdict::Allow);
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    while act.warned.lock().unwrap().is_empty() && std::time::Instant::now() < deadline {
        tokio::time::sleep(std::time::Duration::from_millis(20)).await;
    }
    let warned = act.warned.lock().unwrap().clone();
    assert_eq!(warned, ["Kai (advisory): would refuse `rm -rf target`"]);
}

#[tokio::test(flavor = "multi_thread")]
async fn an_enforced_shortlist_keeps_the_top_k_tools_once_a_turn() {
    let (home, ask, kai) = kai("[ops.tools]\nmode = \"enforced\"\nk = 2", |_, _, state| {
        let p = match state["tool"]["name"].as_str().unwrap_or_default() {
            "hanzo.git" => 0.9,
            "hanzo.fs" => 0.8,
            "hanzo.search" => 0.6,
            _ => 0.1,
        };
        vec![1.0 - p, p]
    });
    let (thread, _) = thread(&kai);
    let cards: Vec<Card> = ["hanzo.browser", "hanzo.fs", "hanzo.git", "hanzo.search"]
        .iter()
        .map(|n| Card {
            name: n.to_string(),
            description: format!("the {n} tool"),
        })
        .collect();
    let before = ask.calls.load(Ordering::SeqCst);
    let keep = thread.shortlist("t1", cards.clone()).await.unwrap();
    let again = thread.shortlist("t1", cards).await.unwrap();
    assert_eq!(
        ask.calls.load(Ordering::SeqCst) - before,
        1,
        "one decision a turn"
    );
    let expected: HashSet<String> = ["hanzo.git", "hanzo.fs"]
        .iter()
        .map(|s| s.to_string())
        .collect();
    assert_eq!(keep, expected);
    assert_eq!(again, expected);

    thread.finished("t1", "c1", "hanzo.git".into(), "ok").await;
    thread.turn_ended("completed", String::new());
    let lines = fake::lines(&home.path().join("kai/decisions.jsonl"), 1);
    let tools = lines
        .iter()
        .find(|l| l["ops"].get("tools").is_some())
        .unwrap();
    assert_eq!(tools["ops"]["tools"]["kai"], "hanzo.git,hanzo.fs");
    assert_eq!(tools["ops"]["tools"]["applied"], true);
    assert_eq!(tools["ops"]["tools"]["states"].as_array().unwrap().len(), 4);
    assert_eq!(tools["outcome"]["result"]["called"], json!(["hanzo.git"]));
}

#[tokio::test(flavor = "multi_thread")]
async fn enforced_routing_hints_tier_and_effort() {
    let settings = "[ops.model]\nmode = \"enforced\"\n[ops.reasoning]\nmode = \"enforced\"";
    let (_home, _, kai) = kai(settings, |id, q, _| match id {
        "tier" => on(q, "large", 0.9),
        "budget" => vec![0.02, 0.03, 0.05, 0.9],
        _ => vec![0.9, 0.1],
    });
    let (thread, _) = thread(&kai);
    let hints = thread
        .route(
            "t1",
            Request {
                text: "prove the lemma".into(),
                images: false,
            },
        )
        .await;
    let expected: BTreeMap<String, String> = [("kai.tier", "large"), ("kai.effort", "high")]
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();
    assert_eq!(hints, expected);
}

#[tokio::test(flavor = "multi_thread")]
async fn shadow_routing_sends_no_hints() {
    let (_home, _, kai) = kai("", |id, q, _| match id {
        "tier" => on(q, "large", 0.9),
        _ => on(q, &q.keys()[0], 0.9),
    });
    let (thread, _) = thread(&kai);
    let hints = thread
        .route(
            "t1",
            Request {
                text: "hello".into(),
                images: false,
            },
        )
        .await;
    assert!(hints.is_empty());
}

#[tokio::test(flavor = "multi_thread")]
async fn an_unfinished_request_is_continued_twice_then_handed_back() {
    let (home, _, kai) = kai("[ops.complete]\nmode = \"enforced\"", |id, q, _| match id {
        "done" => vec![0.9, 0.1],
        "verified" => vec![0.8, 0.2],
        _ => on(q, "2", 0.8),
    });
    let (thread, act) = thread(&kai);
    start(&thread, "t1", "add the feature and its tests").await;
    thread.answered("I added the feature.".into());
    thread.idle().await;
    // A turn Kai started carries no user text: the request and the count stay.
    start(&thread, "t2", "").await;
    thread.idle().await;
    start(&thread, "t3", "").await;
    thread.idle().await;
    assert_eq!(act.resumed.lock().unwrap().len(), 2);
    assert_eq!(
        act.warned.lock().unwrap().clone(),
        ["Kai: this request does not look finished after two more tries; over to you."]
    );
    let lines = fake::lines(&home.path().join("kai/decisions.jsonl"), 3);
    let taken: Vec<&str> = lines
        .iter()
        .filter_map(|l| l["ops"]["complete"]["taken"].as_str())
        .collect();
    assert_eq!(taken, ["retry", "retry", "escalate"]);
    assert_eq!(lines.last().unwrap()["outcome"]["escalated"], true);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_new_request_resets_the_continuations() {
    let (_home, _, kai) = kai("[ops.complete]\nmode = \"enforced\"", |id, q, _| match id {
        "done" => vec![0.9, 0.1],
        _ => on(q, &q.keys()[0], 0.9),
    });
    let (thread, act) = thread(&kai);
    for turn in ["t1", "t2", "t3"] {
        start(&thread, turn, "a new request").await;
        thread.idle().await;
    }
    assert_eq!(act.resumed.lock().unwrap().len(), 3);
    assert!(act.warned.lock().unwrap().is_empty());
}

#[tokio::test(flavor = "multi_thread")]
async fn progress_steers_a_looping_agent_once_a_turn() {
    let (_home, _, kai) = kai("[ops.progress]\nmode = \"enforced\"", |id, q, _| match id {
        "status" => on(q, "looping", 0.85),
        _ => vec![0.9, 0.1],
    });
    let (thread, act) = thread(&kai);
    start(&thread, "t1", "fix the build").await;
    for (i, result) in ["failed", "failed", "failed", "failed"].iter().enumerate() {
        thread
            .finished("t1", &format!("c{i}"), "shell".into(), result)
            .await;
    }
    let steered = act.steered.lock().unwrap().clone();
    assert_eq!(steered.len(), 1, "{steered:?}");
    assert!(steered[0].contains("looping"), "{}", steered[0]);
}

#[tokio::test(flavor = "multi_thread")]
async fn enforced_context_names_the_files_to_read_first() {
    let (_home, _, kai) = kai(
        "[ops.context]\nmode = \"enforced\"\nk = 1",
        |_, _, state| {
            let p = if state["source"] == "src/parser.rs" {
                0.9
            } else {
                0.2
            };
            vec![1.0 - p, p]
        },
    );
    let (thread, _) = thread(&kai);
    let repo = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(repo.path().join("src")).unwrap();
    std::fs::write(repo.path().join("src/parser.rs"), "fn parse() {}").unwrap();
    std::fs::write(repo.path().join("src/lexer.rs"), "fn lex() { parse }").unwrap();
    let body = thread
        .context(
            "t1",
            "the parser rejects empty input".into(),
            repo.path().to_path_buf(),
        )
        .await
        .unwrap();
    assert!(body.contains("- src/parser.rs"), "{body}");
    assert!(!body.contains("lexer"), "{body}");
}

#[test]
fn a_command_reads_as_its_script() {
    let state = command_state(&exec("cargo test -p foo"));
    assert_eq!(
        state,
        json!({"tool": "exec_command", "command": "cargo test -p foo", "cwd": "/repo"})
    );
}
