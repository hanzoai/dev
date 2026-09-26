use super::*;
use crate::fake;
use crate::fake::on;
use pretty_assertions::assert_eq;
use program::Kind;
use std::process::Command;

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
fn kai(next: impl Fn(&Value) -> (&'static str, f64) + Send + Sync + 'static) -> Judge {
    let ask = fake::Kai::new(move |_, q, state| match q.kind {
        Kind::Noul => {
            let p = if state["option"]["label"] == "src/add.sh" {
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
    });
    Judge(Arc::new(crate::Decider::with(Arc::new(ask))))
}

/// Kai as an honest judge of the tests.
fn honest() -> Judge {
    kai(|test| {
        if test["passed"] == true {
            ("done", 0.9)
        } else {
            ("retry", 0.8)
        }
    })
}

fn zen(answer: impl Fn(&Value) -> String + Send + Sync + 'static) -> fake::Zen {
    fake::Zen(Box::new(move |_, context| answer(context)))
}

fn options(max: u32, control: Control) -> Options {
    Options {
        task: json!({"test": "sh test.sh", "goal": "make the test pass"}),
        max,
        control,
        limit: Duration::from_secs(20),
    }
}

fn run_flow(
    root: &Path,
    judge: &Judge,
    zen: &fake::Zen,
    options: &Options,
    trace: &Trace,
) -> Report {
    run(&load("fix").unwrap(), root, options, judge, zen, trace).unwrap()
}

fn read(root: &Path) -> String {
    std::fs::read_to_string(root.join("src/add.sh")).unwrap()
}

#[test]
fn the_shipped_flow_is_a_valid_program() {
    let flow = load("fix").unwrap();
    assert_eq!(flow.id, "flow.fix");
    assert!(flow.node("test").is_some() && flow.node("next").is_some());
}

#[test]
fn kai_picks_the_file_zen_patches_it_and_the_flow_is_done() {
    let repo = repo();
    let trace = Trace::new(repo.path().join(".git/kai.jsonl"));
    let zen = zen(|context| {
        assert_eq!(
            context["read"]["path"], "src/add.sh",
            "Zen sees the file Kai picked"
        );
        assert!(
            context["failure"]["output"]
                .as_str()
                .unwrap()
                .contains("gave -1")
        );
        diff(BUGGY, FIXED)
    });
    let report = run_flow(
        repo.path(),
        &honest(),
        &zen,
        &options(3, Control::Kai),
        &trace,
    );
    assert_eq!(report.outcome, "done");
    assert_eq!(report.attempts.len(), 1);
    let a = &report.attempts[0];
    assert_eq!(a.file.as_deref(), Some("src/add.sh"));
    assert_eq!((a.by.as_str(), a.applied, a.passed), ("kai", true, true));
    let (answer, p) = a.kai.clone().unwrap();
    assert_eq!(answer, "done");
    assert!((p - 0.9).abs() < 1e-6, "{p}");
    assert_eq!(read(repo.path()), FIXED);
    assert!(
        report.summary().contains("outcome: done"),
        "{}",
        report.summary()
    );

    let lines = fake::lines(trace.path(), 1);
    let line = &lines[0];
    assert_eq!(line["family"], "dev.flow");
    assert_eq!(line["sku"], "flow.fix@1");
    assert_eq!(line["outcome"]["finish"], "done");
    assert_eq!(line["outcome"]["result"]["passed"], true);
    assert_eq!(line["ops"]["pick"]["kai"], "src/add.sh");
    assert_eq!(line["ops"]["pick"]["applied"], true);
    assert_eq!(line["ops"]["next"]["kai"], "done");
    assert_eq!(line["ops"]["next"]["taken"], "done");
    assert!(
        line["ops"]["pick"]["answers"].as_array().unwrap().len() >= 2,
        "one per candidate"
    );
}

#[test]
fn a_wrong_patch_is_undone_and_the_retry_sees_it() {
    let repo = repo();
    let trace = Trace::new(repo.path().join(".git/kai.jsonl"));
    let zen = zen(|context| {
        if context["last"].is_null() {
            diff(BUGGY, "echo $(( $1 * $2 ))")
        } else {
            assert!(
                context["last"]["test"]["output"]
                    .as_str()
                    .unwrap()
                    .contains("gave 6")
            );
            diff(BUGGY, FIXED)
        }
    });
    let report = run_flow(
        repo.path(),
        &honest(),
        &zen,
        &options(3, Control::Kai),
        &trace,
    );
    assert_eq!(report.outcome, "done");
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "done"]);
    assert!(report.attempts[0].applied && !report.attempts[0].passed);
    assert_eq!(read(repo.path()), FIXED);
    assert_eq!(fake::lines(trace.path(), 2).len(), 2);
}

#[test]
fn out_of_attempts_the_flow_escalates_and_leaves_the_tree_as_it_was() {
    let repo = repo();
    let trace = Trace::new(repo.path().join(".git/kai.jsonl"));
    let zen = zen(|_| diff(BUGGY, "echo 0"));
    let report = run_flow(
        repo.path(),
        &honest(),
        &zen,
        &options(2, Control::Kai),
        &trace,
    );
    assert_eq!(report.outcome, "escalate");
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "escalate"]);
    assert_eq!(read(repo.path()), BUGGY);
    assert!(report.summary().contains("+echo 0"), "{}", report.summary());
}

#[test]
fn kai_cannot_call_a_failing_attempt_done() {
    let repo = repo();
    let trace = Trace::new(repo.path().join(".git/kai.jsonl"));
    let judge = kai(|_| ("done", 0.95));
    let zen = zen(|_| diff(BUGGY, "echo 4"));
    let report = run_flow(repo.path(), &judge, &zen, &options(2, Control::Kai), &trace);
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "escalate"]);
}

#[test]
fn kai_may_escalate_early_and_rule_control_ignores_it() {
    let repo = repo();
    let trace = Trace::new(repo.path().join(".git/kai.jsonl"));
    let judge = kai(|_| ("escalate", 0.9));
    let zen = zen(|_| diff(BUGGY, "echo 0"));
    let report = run_flow(repo.path(), &judge, &zen, &options(3, Control::Kai), &trace);
    assert_eq!(report.attempts.len(), 1);
    assert_eq!(report.outcome, "escalate");

    let repo = self::repo();
    let report = run_flow(
        repo.path(),
        &judge,
        &zen,
        &options(3, Control::Rule),
        &trace,
    );
    let taken: Vec<&str> = report.attempts.iter().map(|a| a.taken.as_str()).collect();
    assert_eq!(taken, ["retry", "retry", "escalate"]);
    assert!(report.attempts.iter().all(|a| a.by == "rule"));
}

#[test]
fn passing_tests_need_no_attempt() {
    let repo = repo();
    std::fs::write(repo.path().join("src/add.sh"), FIXED).unwrap();
    let trace = Trace::new(repo.path().join(".git/kai.jsonl"));
    let zen = zen(|_| unreachable!("no attempt, no Zen"));
    let report = run_flow(
        repo.path(),
        &honest(),
        &zen,
        &options(3, Control::Kai),
        &trace,
    );
    assert_eq!(report.outcome, "passing");
    assert!(report.attempts.is_empty());
}
