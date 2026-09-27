use super::*;
use crate::program::Kind;
use crate::testing;
use crate::testing::on;
use pretty_assertions::assert_eq;
use std::process::Command;
use wiremock::MockServer;

const BUGGY: &str = "echo $(( $1 - $2 ))\n";
const FIXED: &str = "echo $(( $1 + $2 ))\n";

/// A repository whose test fails: `src/add.sh` subtracts.
fn repo() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    std::fs::create_dir_all(root.join("src")).unwrap();
    std::fs::write(root.join("src/add.sh"), BUGGY).unwrap();
    std::fs::write(root.join("src/mul.sh"), "echo $(( $1 * $2 ))\n").unwrap();
    std::fs::write(root.join("README.md"), "Shell arithmetic.\n").unwrap();
    std::fs::write(
        root.join("test.sh"),
        "r=$(sh src/add.sh 2 3)\n[ \"$r\" = 5 ] || { echo \"FAIL src/add.sh: add 2 3 gave $r, want 5\"; exit 1; }\necho ok\n",
    )
    .unwrap();
    let git = |args: &[&str]| {
        let ok = Command::new("git")
            .args([
                "-c",
                "user.name=t",
                "-c",
                "user.email=t@t",
                "-c",
                "commit.gpgsign=false",
            ])
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

fn diff(from: &str, to: &str) -> String {
    format!(
        "```diff\n--- a/src/add.sh\n+++ b/src/add.sh\n@@ -1 +1 @@\n-{}\n+{}\n```\n",
        from.trim_end(),
        to.trim_end()
    )
}

/// Kai rating `src/add.sh` the file to change, and answering `next` by `next(test output)`.
async fn kai(next: impl Fn(&Value) -> (&'static str, f64) + Send + Sync + 'static) -> MockServer {
    testing::kai(move |_, q, state| match q.kind {
        Kind::Noul => {
            let p = if state["file"]["path"] == "src/add.sh" {
                0.9
            } else {
                0.2
            };
            vec![1.0 - p, p]
        }
        Kind::Choice => {
            let (label, p) = next(&state["test"]);
            on(q, label, p)
        }
        Kind::Score => on(q, "0", 0.9),
    })
    .await
}

/// Kai as an honest judge of the tests.
async fn honest() -> MockServer {
    kai(|test| {
        if test["passed"] == true {
            ("done", 0.9)
        } else {
            ("retry", 0.8)
        }
    })
    .await
}

/// Kai and Zen at their servers, the trace in the repository's `.git`.
fn setup(root: &Path, kai: &str, zen: &MockServer) -> Setup {
    let home = root.join(".git/dev");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::write(
        home.join("kai.toml"),
        format!("url = \"{kai}/v1\"\ntrace = \"kai.jsonl\"\n"),
    )
    .unwrap();
    Setup::new(&home, &format!("{}/v1", zen.uri()), None).unwrap()
}

fn options(max: u32, control: Control) -> Options {
    Options {
        task: json!({"test": "sh test.sh", "goal": "make the test pass"}),
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

fn read(root: &Path) -> String {
    std::fs::read_to_string(root.join("src/add.sh")).unwrap()
}

/// The inputs of every Zen request `zen` received, in order.
async fn asked_zen(zen: &MockServer) -> Vec<Value> {
    zen.received_requests()
        .await
        .unwrap_or_default()
        .iter()
        .map(testing::inputs)
        .collect()
}

#[tokio::test(flavor = "multi_thread")]
async fn kai_picks_the_file_zen_patches_it_and_the_flow_is_done() {
    let repo = repo();
    let kai = honest().await;
    let zen = testing::zen(|_| diff(BUGGY, FIXED)).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "done");
    assert_eq!(report.attempts.len(), 1);
    let a = &report.attempts[0];
    assert_eq!(a.file.as_deref(), Some("src/add.sh"));
    assert_eq!((a.by.as_str(), a.applied, a.passed), ("kai", true, true));
    let (answer, p) = a.kai.clone().unwrap();
    assert_eq!(answer, "done");
    assert!((p - 0.9).abs() < 1e-6, "{p}");
    assert_eq!(read(repo.path()), FIXED);
    assert!(report.missing.is_none());
    assert!(
        report.summary().contains("outcome: done"),
        "{}",
        report.summary()
    );

    let sent = asked_zen(&zen).await;
    assert_eq!(
        sent[0]["read"]["path"], "src/add.sh",
        "Zen sees the file Kai picked"
    );
    assert!(
        sent[0]["failure"]["output"]
            .as_str()
            .unwrap()
            .contains("gave -1")
    );

    let lines = testing::lines(&trace(repo.path()), 1);
    let line = &lines[0];
    assert_eq!(line["family"], "dev.flow");
    assert_eq!(line["sku"], "flow.fix@1");
    assert_eq!(line["outcome"]["finish"], "done");
    assert_eq!(line["outcome"]["result"]["passed"], true);
    assert_eq!(line["ops"]["pick"]["program"], "fix.pick@1");
    assert_eq!(line["ops"]["pick"]["kai"], "src/add.sh");
    assert_eq!(line["ops"]["pick"]["applied"], true);
    assert_eq!(line["ops"]["next"]["kai"], "done");
    assert_eq!(line["ops"]["next"]["taken"], "done");
    assert!(
        line["ops"]["pick"]["answers"].as_array().unwrap().len() >= 2,
        "one per candidate"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_wrong_patch_is_undone_and_the_retry_sees_it() {
    let repo = repo();
    let kai = honest().await;
    let zen = testing::zen(|context| {
        if context["last"].is_null() {
            diff(BUGGY, "echo $(( $1 * $2 ))")
        } else {
            diff(BUGGY, FIXED)
        }
    })
    .await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "done");
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "done"]);
    assert!(report.attempts[0].applied && !report.attempts[0].passed);
    assert_eq!(read(repo.path()), FIXED);
    let sent = asked_zen(&zen).await;
    assert!(
        sent[1]["last"]["test"]["output"]
            .as_str()
            .unwrap()
            .contains("gave 6"),
        "the retry sees the last attempt"
    );
    assert_eq!(testing::lines(&trace(repo.path()), 2).len(), 2);
}

#[tokio::test(flavor = "multi_thread")]
async fn out_of_attempts_the_flow_escalates_and_leaves_the_tree_as_it_was() {
    let repo = repo();
    let kai = honest().await;
    let zen = testing::zen(|_| diff(BUGGY, "echo 0")).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(2, Control::Kai)).await;
    assert_eq!(report.outcome, "escalate");
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "escalate"]);
    assert_eq!(read(repo.path()), BUGGY);
    assert!(report.summary().contains("+echo 0"), "{}", report.summary());
}

#[tokio::test(flavor = "multi_thread")]
async fn kai_cannot_call_a_failing_attempt_done() {
    let repo = repo();
    let kai = kai(|_| ("done", 0.95)).await;
    let zen = testing::zen(|_| diff(BUGGY, "echo 4")).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(2, Control::Kai)).await;
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "escalate"]);
}

#[tokio::test(flavor = "multi_thread")]
async fn kai_may_escalate_early_and_rule_control_never_asks_it() {
    let repo = repo();
    let kai = kai(|_| ("escalate", 0.9)).await;
    let zen = testing::zen(|_| diff(BUGGY, "echo 0")).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.attempts.len(), 1);
    assert_eq!(report.outcome, "escalate");
    let asked = testing::asked(&kai).await;

    let repo = self::repo();
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Rule)).await;
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "retry", "escalate"]);
    assert!(report.attempts.iter().all(|a| a.by == "rule"));
    assert_eq!(
        testing::asked(&kai).await,
        asked,
        "rule control asks Kai nothing"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn without_kai_the_rule_decides_and_says_so_once() {
    let repo = repo();
    // Kai's address answers nothing: every request is a 404.
    let kai = MockServer::start().await;
    let zen = testing::zen(|context| {
        if context["last"].is_null() {
            diff(BUGGY, "echo 0")
        } else {
            diff(BUGGY, FIXED)
        }
    })
    .await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "done");
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "done"]);
    assert!(
        report
            .attempts
            .iter()
            .all(|a| a.by == "rule" && a.kai.is_none())
    );
    assert_eq!(read(repo.path()), FIXED);
    let summary = report.summary();
    assert_eq!(
        summary.matches("kai did not answer").count(),
        1,
        "{summary}"
    );
    // The first pick found Kai down, one request per candidate of the repository's four
    // files; nothing asked it again.
    let asked = testing::asked(&kai).await;
    assert!((1..=4).contains(&asked), "{asked}");
}

#[tokio::test(flavor = "multi_thread")]
async fn passing_tests_need_no_attempt() {
    let repo = repo();
    std::fs::write(repo.path().join("src/add.sh"), FIXED).unwrap();
    let kai = honest().await;
    let zen = testing::zen(|_| String::new()).await;
    let report = run_flow(repo.path(), &kai.uri(), &zen, options(3, Control::Kai)).await;
    assert_eq!(report.outcome, "passing");
    assert!(report.attempts.is_empty());
    assert!(asked_zen(&zen).await.is_empty(), "no attempt, no Zen");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_patch_the_tests_rewrote_ends_the_flow_instead_of_retrying_on_it() {
    let repo = repo();
    let kai = honest().await;
    let zen = testing::zen(|_| diff(BUGGY, "echo 0")).await;
    // Once patched, the test command rewrites the line the patch changed.
    let mut options = options(3, Control::Kai);
    options.task["test"] =
        json!("grep -q 'echo 0' src/add.sh && printf 'echo 7\\n' > src/add.sh; sh test.sh");
    let setup = setup(repo.path(), &kai.uri(), &zen);
    let root = repo.path().to_path_buf();
    let Err(error) = run_with(setup, root, options).await else {
        panic!("the flow went on over a tree it could not restore");
    };
    assert!(error.contains("no longer reverses"), "{error}");
}
