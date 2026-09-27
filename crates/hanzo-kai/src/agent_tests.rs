use super::*;
use crate::program::Kind;
use crate::program::Question;
use crate::testing;
use crate::testing::on;
use pretty_assertions::assert_eq;
use wiremock::MockServer;

/// Kai for tests: `kai.toml` is `settings`, pointed at a server answering by `rule`.
async fn kai(
    settings: &str,
    rule: impl Fn(&str, &Question, &Value) -> Vec<f64> + Send + Sync + 'static,
) -> (tempfile::TempDir, MockServer, Arc<Kai>) {
    let home = tempfile::tempdir().unwrap();
    let server = testing::kai(rule).await;
    let settings = testing::settings(&server, settings, home.path());
    let kai = Arc::new(Kai::open(settings, None).unwrap());
    (home, server, kai)
}

fn thread(kai: &Arc<Kai>) -> (Arc<Thread>, Arc<testing::Thread>) {
    let act = Arc::new(testing::Thread::default());
    let thread = Arc::new(Thread::new(
        Arc::clone(kai),
        "enso-auto".into(),
        act.clone(),
    ));
    (thread, act)
}

/// Deny what removes files recursively, allow the rest; no escalation.
fn risky(_: &str, q: &Question, state: &Value) -> Vec<f64> {
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
    let (home, _server, kai) = kai("[ops.risk]\nmode = \"enforced\"", risky).await;
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
    let risk: Vec<Value> = testing::lines(&home.path().join("kai/decisions.jsonl"), 1)
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
    assert_eq!(step["model"], "laya-agent@test");
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
    let (home, _server, kai) = kai("", risky).await;
    let (thread, act) = thread(&kai);
    start(&thread, "t1", "clean").await;
    let verdict = thread
        .risk("t1", "c1", Verdict::Allow, exec("rm -rf target"))
        .await;
    assert_eq!(verdict, Verdict::Allow);
    thread.finished("t1", "c1", "shell".into(), "ok").await;
    thread.turn_ended("completed", String::new());
    let lines = testing::lines(&home.path().join("kai/decisions.jsonl"), 3);
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
    let (_home, _server, kai) = kai("[ops.risk]\nmode = \"advisory\"", risky).await;
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
    let (home, server, kai) = kai("[ops.tools]\nmode = \"enforced\"\nk = 2", |_, _, state| {
        let p = match state["tool"]["name"].as_str().unwrap_or_default() {
            "hanzo.git" => 0.9,
            "hanzo.fs" => 0.8,
            "hanzo.search" => 0.6,
            _ => 0.1,
        };
        vec![1.0 - p, p]
    })
    .await;
    let (thread, _) = thread(&kai);
    let cards: Vec<Card> = ["hanzo.browser", "hanzo.fs", "hanzo.git", "hanzo.search"]
        .iter()
        .map(|n| Card {
            name: n.to_string(),
            description: format!("the {n} tool"),
        })
        .collect();
    let keep = thread.shortlist("t1", cards.clone()).await.unwrap();
    let again = thread.shortlist("t1", cards).await.unwrap();
    assert_eq!(
        testing::asked(&server).await,
        4,
        "one decision a turn, one request a tool"
    );
    let expected: HashSet<String> = ["hanzo.git", "hanzo.fs"]
        .iter()
        .map(|s| s.to_string())
        .collect();
    assert_eq!(keep, expected);
    assert_eq!(again, expected);

    thread.finished("t1", "c1", "hanzo.git".into(), "ok").await;
    thread.turn_ended("completed", String::new());
    let lines = testing::lines(&home.path().join("kai/decisions.jsonl"), 1);
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
    let (_home, _server, kai) = kai(settings, |id, q, _| match id {
        "tier" => on(q, "large", 0.9),
        "budget" => vec![0.02, 0.03, 0.05, 0.9],
        _ => vec![0.9, 0.1],
    })
    .await;
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
    let (_home, _server, kai) = kai("", |id, q, _| match id {
        "tier" => on(q, "large", 0.9),
        _ => on(q, &q.keys()[0], 0.9),
    })
    .await;
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
    let (home, _server, kai) = kai("[ops.complete]\nmode = \"enforced\"", |id, q, _| match id {
        "done" => vec![0.9, 0.1],
        "verified" => vec![0.8, 0.2],
        _ => on(q, "2", 0.8),
    })
    .await;
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
    let lines = testing::lines(&home.path().join("kai/decisions.jsonl"), 3);
    let taken: Vec<&str> = lines
        .iter()
        .filter_map(|l| l["ops"]["complete"]["taken"].as_str())
        .collect();
    assert_eq!(taken, ["retry", "retry", "escalate"]);
    assert_eq!(lines.last().unwrap()["outcome"]["escalated"], true);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_new_request_resets_the_continuations() {
    let (_home, _server, kai) = kai("[ops.complete]\nmode = \"enforced\"", |id, q, _| match id {
        "done" => vec![0.9, 0.1],
        _ => on(q, &q.keys()[0], 0.9),
    })
    .await;
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
    let (_home, _server, kai) = kai("[ops.progress]\nmode = \"enforced\"", |id, q, _| match id {
        "status" => on(q, "looping", 0.85),
        _ => vec![0.9, 0.1],
    })
    .await;
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
    let (_home, _server, kai) = kai(
        "[ops.context]\nmode = \"enforced\"\nk = 1",
        |_, _, state| {
            let p = if state["source"] == "src/parser.rs" {
                0.9
            } else {
                0.2
            };
            vec![1.0 - p, p]
        },
    )
    .await;
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

/// Every operation that acts on the loop, enforced.
const ENFORCED: &str = "[ops.tools]\nmode = \"enforced\"\n[ops.risk]\nmode = \"enforced\"\n\
[ops.model]\nmode = \"enforced\"\n[ops.reasoning]\nmode = \"enforced\"";

fn cards() -> Vec<Card> {
    ["hanzo.fs", "hanzo.git"]
        .iter()
        .map(|n| Card {
            name: n.to_string(),
            description: format!("the {n} tool"),
        })
        .collect()
}

/// A turn through every point where the loop asks Kai, with what the loop got back.
async fn turn(
    thread: &Arc<Thread>,
) -> (BTreeMap<String, String>, Option<HashSet<String>>, Verdict) {
    let hints = thread
        .route(
            "t1",
            Request {
                text: "list the files".into(),
                images: false,
            },
        )
        .await;
    let keep = thread.shortlist("t1", cards()).await;
    let verdict = thread
        .risk("t1", "c1", Verdict::Allow, exec("rm -rf target"))
        .await;
    thread.finished("t1", "c1", "shell".into(), "ok").await;
    thread.turn_ended("completed", String::new());
    (hints, keep, verdict)
}

#[tokio::test(flavor = "multi_thread")]
async fn with_kai_on_the_loop_takes_its_tools_tier_and_verdict() {
    let (home, _server, kai) = kai(ENFORCED, |id, q, state| match (id, q.kind) {
        ("tier", _) => on(q, "small", 0.8),
        ("needed", _) => {
            let p = if state["tool"]["name"] == "hanzo.fs" {
                0.9
            } else {
                0.1
            };
            vec![1.0 - p, p]
        }
        (_, Kind::Choice) => risky(id, q, state),
        (_, Kind::Score) => on(q, "1", 0.7),
        (_, Kind::Noul) => vec![0.95, 0.05],
    })
    .await;
    let (thread, act) = thread(&kai);
    let (hints, keep, verdict) = turn(&thread).await;
    assert_eq!(hints.get("kai.tier").map(String::as_str), Some("small"));
    assert_eq!(hints.get("kai.effort").map(String::as_str), Some("low"));
    assert_eq!(keep, Some(["hanzo.fs".to_string()].into_iter().collect()));
    assert_eq!(verdict, Verdict::Deny);
    assert!(act.warned.lock().unwrap().is_empty());
    // Every decision is a line, with its distributions.
    let lines = testing::lines(&home.path().join("kai/decisions.jsonl"), 4);
    let ops: Vec<&str> = lines
        .iter()
        .flat_map(|l| l["ops"].as_object().unwrap().keys())
        .map(String::as_str)
        .collect();
    for op in ["model", "reasoning", "tools", "risk"] {
        assert!(ops.contains(&op), "{op} in {ops:?}");
    }
    let tier = lines
        .iter()
        .find(|l| l["ops"].get("model").is_some())
        .unwrap();
    let dist = &tier["ops"]["model"]["answers"][0]["tier"];
    assert!(
        (dist["small"].as_f64().unwrap() - 0.8).abs() < 1e-9,
        "{dist}"
    );
    assert_eq!(tier["ops"]["model"]["taken"], "small");
}

#[tokio::test(flavor = "multi_thread")]
async fn kai_that_does_not_answer_leaves_the_loop_as_it_was() {
    // A server that answers nothing it is asked: every request is a 404.
    let server = MockServer::start().await;
    let home = tempfile::tempdir().unwrap();
    let settings = testing::settings(&server, ENFORCED, home.path());
    let kai = Arc::new(Kai::open(settings, None).unwrap());
    let (thread, act) = thread(&kai);
    let started = std::time::Instant::now();
    let (hints, keep, verdict) = turn(&thread).await;
    assert_eq!(
        (hints, keep, verdict),
        (BTreeMap::new(), None, Verdict::Allow)
    );
    // The first decision finds Kai down; nothing after it asks again.
    assert_eq!(
        testing::asked(&server).await,
        2,
        "model and reasoning, then nothing"
    );
    let (hints, keep, verdict) = turn(&thread).await;
    assert_eq!(
        (hints, keep, verdict),
        (BTreeMap::new(), None, Verdict::Allow)
    );
    assert_eq!(testing::asked(&server).await, 2);
    assert!(started.elapsed() < std::time::Duration::from_secs(5));
    assert!(act.warned.lock().unwrap().is_empty(), "nothing shown");
    std::thread::sleep(std::time::Duration::from_millis(200));
    assert!(
        !home.path().join("kai/decisions.jsonl").exists(),
        "no decision, no line"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn an_unreachable_kai_fails_fast() {
    let port = std::net::TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port();
    let home = tempfile::tempdir().unwrap();
    let text = format!("url = \"http://127.0.0.1:{port}/v1\"\n{ENFORCED}");
    let settings = crate::Settings::parse(&text, home.path()).unwrap();
    let kai = Arc::new(Kai::open(settings, None).unwrap());
    let (thread, _) = thread(&kai);
    let started = std::time::Instant::now();
    let (hints, keep, verdict) = turn(&thread).await;
    assert_eq!(
        (hints, keep, verdict),
        (BTreeMap::new(), None, Verdict::Allow)
    );
    assert!(started.elapsed() < std::time::Duration::from_secs(3));
    assert!(kai.decider.down());
}

#[tokio::test(flavor = "multi_thread")]
async fn an_answer_under_another_calibration_runs_in_shadow() {
    let home = tempfile::tempdir().unwrap();
    let server = testing::kai_with("cal_0000000000000000", risky).await;
    let settings = testing::settings(&server, "[ops.risk]\nmode = \"enforced\"", home.path());
    let kai = Arc::new(Kai::open(settings, None).unwrap());
    let (thread, _) = thread(&kai);
    start(&thread, "t1", "clean").await;
    let verdict = thread
        .risk("t1", "c1", Verdict::Allow, exec("rm -rf /"))
        .await;
    assert_eq!(verdict, Verdict::Allow, "thresholds not validated here");
    thread.finished("t1", "c1", "shell".into(), "ok").await;
    let lines = testing::lines(&home.path().join("kai/decisions.jsonl"), 1);
    let risk = lines
        .iter()
        .find(|l| l["ops"].get("risk").is_some())
        .unwrap();
    assert_eq!(risk["ops"]["risk"]["gates"], json!(["shadow"]));
    assert_eq!(risk["ops"]["risk"]["kai"], "deny");
}

#[tokio::test(flavor = "multi_thread")]
async fn tools_that_arrive_mid_turn_are_decided_again() {
    let (_home, server, kai) = kai("[ops.tools]\nmode = \"enforced\"", |_, _, state| {
        let p = if state["tool"]["name"] == "hanzo.git" {
            0.9
        } else {
            0.1
        };
        vec![1.0 - p, p]
    })
    .await;
    let (thread, _) = thread(&kai);
    // The first step offers none: the MCP server is still starting.
    assert_eq!(thread.shortlist("t1", Vec::new()).await, None);
    let first = thread.shortlist("t1", cards()).await;
    assert_eq!(first, Some(["hanzo.git".to_string()].into_iter().collect()));
    let mut more = cards();
    more.push(Card {
        name: "hanzo.git2".into(),
        description: "another".into(),
    });
    thread.shortlist("t1", more.clone()).await;
    thread.shortlist("t1", more).await;
    assert_eq!(
        testing::asked(&server).await,
        2 + 3,
        "once per offer, not per step"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_server_that_answers_without_distributions_is_kai_not_answering() {
    // A login page, say: 200, and not the Decisions API.
    let server = MockServer::start().await;
    wiremock::Mock::given(wiremock::matchers::method("POST"))
        .respond_with(wiremock::ResponseTemplate::new(200).set_body_string("<html>sign in</html>"))
        .mount(&server)
        .await;
    let home = tempfile::tempdir().unwrap();
    let settings = testing::settings(&server, ENFORCED, home.path());
    let kai = Arc::new(Kai::open(settings, None).unwrap());
    let (thread, _) = thread(&kai);
    let (hints, keep, verdict) = turn(&thread).await;
    assert_eq!(
        (hints, keep, verdict),
        (BTreeMap::new(), None, Verdict::Allow)
    );
    assert!(kai.decider.down());
    assert_eq!(
        testing::asked(&server).await,
        2,
        "model and reasoning, then nothing"
    );
    std::thread::sleep(std::time::Duration::from_millis(200));
    assert!(!home.path().join("kai/decisions.jsonl").exists());
}
