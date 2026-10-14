use super::*;
use crate::program::Kind;
use crate::testing;
use crate::testing::on;
use pretty_assertions::assert_eq;
use std::process::Command;
use wiremock::MockServer;

/// A fault no single change of the generic transforms mends (`^` is not among their operators): the
/// search finds nothing and the model writes the edit.
const BUGGY: &str = "def add(a, b):\n    return a ^ b\n\n\ndef mul(a, b):\n    return a * b\n";
const FIXED: &str = "def add(a, b):\n    return a + b\n\n\ndef mul(a, b):\n    return a * b\n";
/// A fault one change mends.
const SIMPLE: &str = "def add(a, b):\n    return a - b\n\n\ndef mul(a, b):\n    return a * b\n";
const TEST: &str = "from add import add\n\n\ndef test_add():\n    assert add(2, 3) == 5\n\n\ntest_add()\n";

/// A repository whose test fails: `src/add.py` is wrong.
fn repo_with(source: &str) -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    std::fs::create_dir_all(root.join("src")).unwrap();
    std::fs::create_dir_all(root.join("tests")).unwrap();
    std::fs::write(root.join("src/add.py"), source).unwrap();
    std::fs::write(root.join("tests/test_add.py"), TEST).unwrap();
    let git = |args: &[&str]| {
        let ok = Command::new("git")
            .args(["-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false"])
            .args(args)
            .current_dir(root)
            .output()
            .unwrap()
            .status
            .success();
        assert!(ok, "git {args:?}");
    };
    git(&["init", "-q"]);
    git(&["add", "."]);
    git(&["commit", "-q", "-m", "start"]);
    dir
}

fn repo() -> tempfile::TempDir {
    repo_with(BUGGY)
}

fn edit_json(old: &str, new: &str) -> String {
    json!({"edits": [{"file": "src/add.py", "line": 2, "old": old, "new": new}]}).to_string()
}

fn fix() -> String {
    edit_json("a ^ b", "a + b")
}

/// Kai choosing site `c{site}` and answering `next` by `next(test)`.
async fn kai(site: usize, next: impl Fn(&Value) -> (&'static str, f64) + Send + Sync + 'static) -> MockServer {
    testing::kai(move |id, q, state| match (id, q.kind) {
        ("site", _) => on(q, &format!("c{site}"), 0.9),
        ("action", _) => on(q, "search", 0.9),
        ("effort", _) => on(q, "low", 0.9),
        (_, Kind::Noul) => vec![0.5, 0.5],
        (_, Kind::Choice) => {
            let (label, p) = next(&state["test"]);
            on(q, label, p)
        }
        (_, Kind::Score) => on(q, "0", 0.9),
    })
    .await
}

/// Kai as an honest judge of the tests.
async fn honest() -> MockServer {
    kai(0, |test| {
        if test["passed"] == true {
            ("done", 0.9)
        } else {
            ("retry", 0.8)
        }
    })
    .await
}

/// Kai and Zen at their servers, the trace and the ledger in the repository's `.git`.
fn setup(root: &Path, kai: &str, zen: &MockServer) -> Setup {
    let home = root.join(".git/dev");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::write(
        home.join("kai.toml"),
        format!(
            "url = \"{kai}/v1\"\ntrace = \"kai.jsonl\"\n[generate]\nurl = \"{}/v1\"\nmodel = \"zen-test\"\n",
            zen.uri()
        ),
    )
    .unwrap();
    Setup::new(&home, || None).unwrap()
}

fn options(max: u32, control: Control) -> Options {
    Options {
        task: json!({"test": "PYTHONPATH=src python3 tests/test_add.py", "goal": "make the test pass"}),
        failure: None,
        search: true,
        discover: true,
        max,
        control,
        limit: Duration::from_secs(20),
    }
}

async fn run_flow(root: &Path, kai: &str, zen: &MockServer, options: Options) -> Report {
    let setup = setup(root, kai, zen);
    let root = root.to_path_buf();
    run_with(setup, root, options).await.unwrap()
}

fn trace(root: &Path) -> PathBuf {
    root.join(".git/dev/kai.jsonl")
}

fn ledger(root: &Path) -> Vec<Value> {
    std::fs::read_to_string(root.join(".git/dev/ledger.jsonl"))
        .unwrap_or_default()
        .lines()
        .map(|l| serde_json::from_str(l).unwrap())
        .collect()
}

fn read(root: &Path) -> String {
    std::fs::read_to_string(root.join("src/add.py")).unwrap()
}

/// The user message of every Zen request `zen` received, in order.
async fn asked_zen(zen: &MockServer) -> Vec<String> {
    zen.received_requests()
        .await
        .unwrap_or_default()
        .iter()
        .map(testing::prompt)
        .collect()
}

#[tokio::test(flavor = "multi_thread")]
async fn a_single_fault_is_found_by_search_and_the_model_is_never_asked() {
    let repo = repo_with(SIMPLE);
    let kai = honest().await;
    let zen = testing::zen(|_| String::new()).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "done");
    assert!(report.attempts.is_empty());
    assert!(asked_zen(&zen).await.is_empty(), "no generation");
    let fast = report.fast.as_ref().unwrap();
    assert!(fast.solved && fast.tried >= 1 && fast.runs >= 1, "{fast:?}");
    assert_eq!(fast.kind.as_deref(), Some("operator"));
    assert_eq!(read(repo.path()), SIMPLE.replace("a - b", "a + b"));
    assert!(report.summary().contains("solved by a operator change"), "{}", report.summary());

    let lines = ledger(repo.path());
    let kinds: Vec<&str> = lines.iter().map(|l| l["kind"].as_str().unwrap()).collect();
    assert_eq!(kinds.first(), Some(&"action"));
    assert!(kinds.contains(&"verify") && kinds.last() == Some(&"end"), "{kinds:?}");
    let mutate = lines.iter().find(|l| l["operation"]["type"] == "mutate").unwrap();
    assert_eq!(mutate["status"], "selected");
    assert_eq!(mutate["path"], json!(["eligible", "attempted", "completed", "selected"]));
    assert!(lines.iter().all(|l| l["task"] == lines[0]["task"]));
    let ids: std::collections::HashSet<&str> = lines.iter().filter_map(|l| l["id"].as_str()).collect();
    assert_eq!(ids.len(), lines.iter().filter(|l| l["id"].is_string()).count(), "ids are unique");
}

#[tokio::test(flavor = "multi_thread")]
async fn the_model_writes_the_edit_when_search_finds_none_and_the_flow_is_done() {
    let repo = repo();
    let kai = honest().await;
    let zen = testing::zen(|_| fix()).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "done");
    assert_eq!(report.attempts.len(), 1);
    let a = &report.attempts[0];
    assert_eq!((a.applied, a.passed, a.effort.as_str()), (true, true, "low"));
    assert_eq!(a.shown.first().map(String::as_str), Some("add"));
    assert!(!report.fast.as_ref().unwrap().solved);
    assert_eq!(read(repo.path()), FIXED);
    assert!(report.missing.is_none());
    for step in ["test0", "tree", "rank", "site", "propose", "search", "total"] {
        assert!(report.ms.contains_key(step), "{step}: {:?}", report.ms);
    }
    for step in ["bundle", "generate", "apply", "test"] {
        assert!(a.ms.contains_key(step), "{step}: {:?}", a.ms);
    }
    assert_eq!(a.generation.as_ref().unwrap().prompt, 100);

    let sent = asked_zen(&zen).await;
    assert_eq!(sent.len(), 1);
    assert!(sent[0].contains("src/add.py:1-2 (add)"), "{}", sent[0]);
    assert!(sent[0].contains("   2|     return a ^ b"), "{}", sent[0]);
    assert!(sent[0].contains("AssertionError"), "{}", sent[0]);

    let lines = testing::lines(&trace(repo.path()), 1);
    let line = &lines[0];
    assert_eq!(line["family"], "dev.flow");
    assert_eq!(line["sku"], "flow.fix@2");
    assert_eq!(line["outcome"]["finish"], "done");
    assert_eq!(line["outcome"]["result"]["generation"]["prompt_tokens"], 100);

    // A run that passes needs no inference: the ledger says so, and Kai was not asked about it.
    let led = ledger(repo.path());
    assert!(led.iter().any(|l| l["kind"] == "fast" && l["op"] == "turn"), "{led:?}");
    let ops: Vec<&str> = led.iter().filter_map(|l| l["operation"]["type"].as_str()).collect();
    for op in ["test", "search", "mutate", "generate", "edit"] {
        assert!(ops.contains(&op), "{op}: {ops:?}");
    }
    let generate = led.iter().find(|l| l["operation"]["type"] == "generate").unwrap();
    assert_eq!(generate["source"], "model");
    assert!(led.iter().any(|l| l["kind"] == "fallback" && l["op"] == "search"));
}

#[tokio::test(flavor = "multi_thread")]
async fn a_wrong_edit_is_undone_and_the_retry_sees_it() {
    let repo = repo();
    let kai = honest().await;
    let zen = testing::zen(|prompt| {
        if prompt.contains("Your previous edit") {
            fix()
        } else {
            edit_json("a ^ b", "a | b")
        }
    })
    .await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "done", "{}", report.summary());
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "done"]);
    assert!(report.attempts[0].applied && !report.attempts[0].passed);
    assert_eq!(read(repo.path()), FIXED);
    let sent = asked_zen(&zen).await;
    assert!(sent[1].contains("applied and the test still failed"), "{}", sent[1]);
    assert!(sent[1].contains("-a ^ b +a | b"), "the retry sees the edit: {}", sent[1]);
    assert!(sent[1].contains("AssertionError"), "and what the tests said after it");
    assert_eq!(testing::lines(&trace(repo.path()), 2).len(), 2);
    let led = ledger(repo.path());
    let generates: Vec<&Value> = led.iter().filter(|l| l["operation"]["type"] == "generate").collect();
    assert_eq!(generates[0]["source"], "model");
    assert_eq!(generates[1]["source"], "recover", "the second attempt is recover's");
    assert!(led.iter().any(|l| l["kind"] == "recover" && l["attempt"] == 1));
    assert!(led.iter().any(|l| l["operation"]["type"] == "revert" && l["status"] == "applied"));
    // Kai was asked what next once, after the failed run, in one request.
    assert!(led.iter().any(|l| l["operation"]["type"] == "decide" && l["operation"]["program"] == "fix.turn@1"));
}

#[tokio::test(flavor = "multi_thread")]
async fn the_same_failure_after_another_edit_makes_recover_choose_full_effort_then_escalate() {
    let repo = repo();
    let kai = honest().await;
    // Every edit leaves the same failing assertion.
    let zen = testing::zen({
        let n = std::sync::atomic::AtomicUsize::new(0);
        move |_| edit_json("a ^ b", &format!("a | {}", n.fetch_add(1, std::sync::atomic::Ordering::SeqCst)))
    })
    .await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(4, Control::Kai)).await;
    let efforts: Vec<&str> = report.attempts.iter().map(|a| a.effort.as_str()).collect();
    assert_eq!(efforts, ["low", "full", "full"].get(..report.attempts.len()).unwrap(), "{}", report.summary());
    assert_eq!(report.outcome, "escalate", "{}", report.summary());
    assert_eq!(read(repo.path()), BUGGY, "the tree is as it was");
    let requests = zen.received_requests().await.unwrap();
    let thinking: Vec<bool> = requests
        .iter()
        .map(|r| {
            let body: Value = serde_json::from_slice(&r.body).unwrap();
            body["chat_template_kwargs"]["enable_thinking"] == true
        })
        .collect();
    assert_eq!(thinking[0], false);
    assert!(thinking[1..].iter().all(|t| *t), "{thinking:?}");
    let led = ledger(repo.path());
    let strategies: Vec<&str> = led.iter().filter(|l| l["kind"] == "recover").filter_map(|l| l["strategy"].as_str()).collect();
    assert_eq!(strategies.first(), Some(&"full"), "{strategies:?}");
    assert_eq!(strategies.last(), Some(&"escalate"), "{strategies:?}");
}

#[tokio::test(flavor = "multi_thread")]
async fn an_edit_that_does_not_apply_is_refused_and_the_retry_is_told_why() {
    let repo = repo();
    let kai = honest().await;
    let zen = testing::zen(|prompt| {
        if prompt.contains("did not apply") {
            fix()
        } else {
            edit_json("a minus b", "a + b")
        }
    })
    .await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "done");
    assert!(report.attempts[0].refused.as_deref().unwrap().contains("does not stand"));
    assert!(!report.attempts[0].applied);
    assert!(!report.attempts[0].ms.contains_key("test"), "nothing was run");
    assert_eq!(report.attempts[0].taken, "retry");
}

#[tokio::test(flavor = "multi_thread")]
async fn out_of_attempts_the_flow_escalates_and_leaves_the_tree_as_it_was() {
    let repo = repo();
    let kai = honest().await;
    let zen = testing::zen(|_| edit_json("a ^ b", "0")).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(2, Control::Kai)).await;
    assert_eq!(report.outcome, "escalate");
    assert_eq!(report.attempts.last().unwrap().taken, "escalate");
    assert_eq!(read(repo.path()), BUGGY);
    assert!(report.summary().contains("+0"), "{}", report.summary());
}

#[tokio::test(flavor = "multi_thread")]
async fn kai_cannot_call_a_failing_attempt_done() {
    let repo = repo();
    let kai = kai(0, |_| ("done", 0.95)).await;
    let zen = testing::zen(|_| edit_json("a ^ b", "4")).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(2, Control::Kai)).await;
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "escalate"]);
}

#[tokio::test(flavor = "multi_thread")]
async fn kai_may_escalate_early_and_rule_control_never_asks_it() {
    let repo = repo();
    let kai = kai(0, |_| ("escalate", 0.9)).await;
    let zen = testing::zen(|_| edit_json("a ^ b", "0")).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.attempts.len(), 1);
    assert_eq!(report.outcome, "escalate");
    let asked = testing::asked(&kai).await;
    assert!(asked >= 1);

    let repo = self::repo();
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Rule)).await;
    assert_eq!(report.outcome, "escalate");
    assert!(report.attempts.iter().all(|a| a.by == "rule"));
    assert_eq!(testing::asked(&kai).await, asked, "rule control asks Kai nothing");
}

#[tokio::test(flavor = "multi_thread")]
async fn without_kai_the_rule_decides_and_the_flow_still_finishes() {
    let repo = repo();
    // Kai's address answers nothing: every request is a 404.
    let kai = MockServer::start().await;
    let zen = testing::zen(|prompt| {
        if prompt.contains("Your previous edit") {
            fix()
        } else {
            edit_json("a ^ b", "0")
        }
    })
    .await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "done");
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "done"]);
    assert!(report.attempts.iter().all(|a| a.kai.is_none()));
    assert_eq!(read(repo.path()), FIXED);
    assert!(report.summary().matches("kai did not answer").count() <= 1);
}

#[tokio::test(flavor = "multi_thread")]
async fn passing_tests_need_no_attempt() {
    let repo = repo();
    std::fs::write(repo.path().join("src/add.py"), FIXED).unwrap();
    let kai = honest().await;
    let zen = testing::zen(|_| String::new()).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "passing");
    assert!(report.attempts.is_empty());
    assert!(asked_zen(&zen).await.is_empty(), "no attempt, no Zen");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_failure_in_hand_is_not_run_to_see_it() {
    let repo = repo_with(SIMPLE);
    let kai = honest().await;
    let zen = testing::zen(|_| String::new()).await;
    let mut options = options(3, Control::Kai);
    options.failure = Some("tests/test_add.py:5: in test_add\n    assert add(2, 3) == 5\nAssertionError\n".into());
    // A test command that cannot be run to see it fail: only candidates run it.
    options.task["test"] = json!("PYTHONPATH=src python3 tests/test_add.py");
    let report = run_flow(repo.path(), &kai.uri(), &zen, options).await;
    assert_eq!(report.outcome, "done");
    assert!(report.ms["test0"] < 5.0, "{:?}", report.ms);
    let led = ledger(repo.path());
    assert_eq!(led[0]["result"]["given"], true);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_test_file_is_never_edited() {
    let repo = repo();
    let kai = honest().await;
    let zen = testing::zen(|_| {
        json!({"edits": [{"file": "tests/test_add.py", "line": 5, "old": "== 5", "new": "== -1"}]}).to_string()
    })
    .await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(1, Control::Kai)).await;
    assert_eq!(report.outcome, "escalate");
    assert!(report.attempts[0].refused.as_deref().unwrap().contains("test file"));
    assert_eq!(std::fs::read_to_string(repo.path().join("tests/test_add.py")).unwrap(), TEST);
}

#[test]
fn recover_never_replays_the_same_failure() {
    let f = |fp: &str, rev: &str| Failure { fingerprint: fp.into(), revision: rev.into() };
    // The failure changed: progress, try again.
    assert_eq!(recover(&[f("a", "base"), f("b", "r1")], Strategy::Retry, 2).0, Strategy::Retry);
    // The same failure after another edit: full effort once, then escalate.
    assert_eq!(recover(&[f("a", "base"), f("a", "r1")], Strategy::Retry, 2).0, Strategy::Full);
    assert_eq!(recover(&[f("a", "r1"), f("a", "r2")], Strategy::Full, 2).0, Strategy::Escalate);
    // The same failure at the same revision: nothing changed at all.
    assert_eq!(recover(&[f("a", "r1"), f("a", "r1")], Strategy::Retry, 2).0, Strategy::Escalate);
    // No attempts left.
    assert_eq!(recover(&[f("a", "base"), f("b", "r1")], Strategy::Retry, 0).0, Strategy::Escalate);
}

/// Two faults in two files, a test that counts its failures as a runner does.
fn two_faults() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    std::fs::create_dir_all(root.join("src")).unwrap();
    std::fs::create_dir_all(root.join("tests")).unwrap();
    std::fs::write(root.join("src/a.py"), "def f(a, b):\n    return a - b\n").unwrap();
    std::fs::write(root.join("src/b.py"), "def g(a, b):\n    return a - b\n").unwrap();
    std::fs::write(
        root.join("tests/test_both.py"),
        "import sys\nsys.path.insert(0, 'src')\nfrom a import f\nfrom b import g\nbad = []\nif f(3, 2) != 5:\n    bad.append('f')\nif g(7, 2) != 9:\n    bad.append('g')\nprint(f'{len(bad)} failed, {2 - len(bad)} passed')\nsys.exit(1 if bad else 0)\n",
    )
    .unwrap();
    let git = |args: &[&str]| {
        let ok = Command::new("git")
            .args(["-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false"])
            .args(args)
            .current_dir(root)
            .output()
            .unwrap()
            .status
            .success();
        assert!(ok, "git {args:?}");
    };
    git(&["init", "-q"]);
    git(&["add", "."]);
    git(&["commit", "-q", "-m", "start"]);
    dir
}

#[tokio::test(flavor = "multi_thread")]
async fn a_step_that_fixes_one_of_two_faults_is_kept_and_the_state_looked_at_again() {
    let repo = two_faults();
    let kai = honest().await;
    let zen = testing::zen(|_| String::new()).await;
    let mut options = options(3, Control::Kai);
    options.task["test"] = json!("python3 tests/test_both.py");
    let report = run_flow(repo.path(), &kai.uri(), &zen, options).await;
    assert_eq!(report.outcome, "done", "{}", report.summary());
    assert!(asked_zen(&zen).await.is_empty(), "no generation: the search did both");
    let fast = report.fast.as_ref().unwrap();
    assert!(fast.solved && fast.kept == 1 && fast.rounds == 2, "{fast:?}");
    assert_eq!(report.failing, 2);
    assert_eq!(std::fs::read_to_string(repo.path().join("src/a.py")).unwrap(), "def f(a, b):\n    return a + b\n");
    assert_eq!(std::fs::read_to_string(repo.path().join("src/b.py")).unwrap(), "def g(a, b):\n    return a + b\n");
    assert_eq!(report.patch.len(), 2, "{:?}", report.patch);
    let led = ledger(repo.path());
    let mutates = led.iter().filter(|l| l["operation"]["type"] == "mutate").count();
    assert_eq!(mutates, 2);
    let verifies: Vec<&Value> = led.iter().filter(|l| l["kind"] == "verify").collect();
    assert_eq!(verifies.iter().filter(|v| v["passed"] == true).count(), 1, "one pass, the last");
}

#[tokio::test(flavor = "multi_thread")]
async fn kept_progress_is_undone_when_the_flow_does_not_end_done() {
    let repo = two_faults();
    // The search keeps one step; the model then refuses to help and the attempts run out.
    let kai = kai(0, |_| ("escalate", 0.9)).await;
    let zen = testing::zen(|_| "no edit".to_string()).await;
    let mut options = options(1, Control::Kai);
    options.task["test"] = json!("python3 tests/test_both.py");
    // A second fault the generic changes cannot mend.
    std::fs::write(repo.path().join("src/b.py"), "def g(a, b):\n    return a ^ b\n").unwrap();
    let report = run_flow(repo.path(), &kai.uri(), &zen, options).await;
    assert_eq!(report.outcome, "escalate", "{}", report.summary());
    assert_eq!(std::fs::read_to_string(repo.path().join("src/a.py")).unwrap(), "def f(a, b):\n    return a - b\n", "a kept step is put back");
    assert_eq!(std::fs::read_to_string(repo.path().join("src/b.py")).unwrap(), "def g(a, b):\n    return a ^ b\n");
}

#[test]
fn tgrep_puts_the_function_that_spells_the_message_into_the_ranking() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    std::fs::create_dir_all(root.join("pkg")).unwrap();
    std::fs::write(
        root.join("pkg/core.py"),
        "def check(x):\n    if x < 0:\n        raise ValueError(\"negative values are not allowed here\")\n    return x\n\n\ndef other(x):\n    return x\n",
    )
    .unwrap();
    let failure = "tests/test_core.py:4: in test_check\n    run(-1)\nE   ValueError: negative values are not allowed here\nFAILED tests/test_core.py::test_check - ValueError: negative values are not allowed here\n";
    let mut tree = Tree::open(root);
    let mut plan = locate::plan(&mut tree, failure);
    // The ranker does not know it: take it out and let search put it back.
    plan.ranked.retain(|r| r.sym.as_ref().is_none_or(|s| s.qual != "check"));
    let ledger = Ledger::new("t", root.join("ledger.jsonl"));
    let (added, _) = discover(&Tgrep, root, &mut plan, &ledger, "base");
    assert_eq!(added, vec!["check".to_string()]);
    let at = plan.ranked.iter().position(|r| r.sym.as_ref().is_some_and(|s| s.qual == "check")).unwrap();
    assert!(at <= 3, "within the first four: {at}");
    let line: Value = serde_json::from_str(std::fs::read_to_string(root.join("ledger.jsonl")).unwrap().lines().next().unwrap()).unwrap();
    assert_eq!(line["operation"]["engine"], "tgrep");
    assert_eq!(line["result"]["functions"][0]["by"], "message");
}

#[test]
fn spelled_takes_quoted_messages_and_error_tails_and_leaves_paths_and_asserts() {
    let got = spelled("ValueError: negative values are not allowed here\nKeyError: 'missing key in the table'\nassert x == 4\nopen('src/a.py')");
    assert!(got.contains(&"negative values are not allowed here".to_string()), "{got:?}");
    assert!(got.contains(&"missing key in the table".to_string()), "{got:?}");
    assert!(!got.iter().any(|s| s.contains("src/a.py") || s.starts_with("assert")), "{got:?}");
}
