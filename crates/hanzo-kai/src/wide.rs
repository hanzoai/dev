//! A wider test before `done`.
//!
//! The failing command passing settles the failing tests and nothing near them. Before a flow
//! says `done`, the tests around the failing ones run too: the directory of every test file the
//! command or the failing output names, in place of the files and selectors that narrowed the
//! command. A failure that was there before any edit is not the edit's; a test failing now that
//! was not then makes the edit not done. What failed before is read only when a run after an edit
//! fails: a run that passes shows no new failure whatever failed before, so on most tasks it is
//! never read. The run is capped at [`LIMIT`]; a run that does not end by then says nothing. Only
//! the command and the failing output are read, never anything of the task beyond them.

use crate::tools;
use serde_json::Value;
use std::collections::BTreeSet;
use std::path::Path;
use std::time::Duration;

/// The longest a wider run may take.
pub const LIMIT: Duration = Duration::from_secs(20);

/// The wider command, and what failed in it before any edit once that is read.
#[derive(Debug, Clone)]
pub struct Wide {
    pub command: String,
    pub before: Option<Before>,
}

/// What the wider run said before any edit.
#[derive(Debug, Clone, Default)]
pub struct Before {
    pub failing: BTreeSet<String>,
    /// Its exit code; -2 when it did not end within [`LIMIT`], and then it says nothing.
    pub code: i64,
}

/// What a wider run found.
#[derive(Debug, Clone)]
pub struct Verdict {
    /// Tests failing now that did not before.
    pub new: Vec<String>,
    /// The run did not end within [`LIMIT`]: it says nothing.
    pub timed_out: bool,
    /// The run: `{code, output, passed, ms}`.
    pub run: Value,
}

/// The test files a pytest run's failing output names: `FAILED tests/a.py::test_x - ...`.
fn named(output: &str) -> Vec<String> {
    failing(output)
        .into_iter()
        .filter_map(|id| id.split("::").next().filter(|f| f.ends_with(".py")).map(str::to_string))
        .collect()
}

/// The pytest command that runs the tests around the failing ones, when it differs from
/// `command`: the directory of each test file `command` or `output` names stands in for the files
/// and drops `-k` and `-m`. `None` for a command that is not plain pytest or already runs them.
pub fn command(command: &str, output: &str) -> Option<String> {
    if command.contains(['"', '\''])
        || !command.split_whitespace().any(|t| t == "pytest" || t.ends_with("/pytest"))
    {
        return None;
    }
    let dir = |file: &str| match Path::new(file).parent().and_then(Path::to_str) {
        Some("") | None => ".".to_string(),
        Some(d) => d.to_string(),
    };
    let mut out: Vec<String> = Vec::new();
    let mut dirs: Vec<String> = Vec::new();
    let mut tokens = command.split_whitespace();
    let mut pytest = false;
    while let Some(t) = tokens.next() {
        pytest |= t == "pytest" || t.ends_with("/pytest");
        // Pytest's selectors narrow the run; before pytest `-m` is python's own.
        if pytest && (t == "-k" || t == "-m" || t == "--deselect") {
            tokens.next();
        } else if t.starts_with("--deselect=") {
            continue;
        } else if t.split("::").next().is_some_and(|f| f.ends_with(".py")) {
            let d = dir(t.split("::").next().unwrap_or_default());
            if !dirs.contains(&d) {
                dirs.push(d.clone());
                out.push(d);
            }
        } else {
            out.push(t.to_string());
        }
    }
    for file in named(output) {
        let d = dir(&file);
        if !dirs.contains(&d) {
            dirs.push(d.clone());
            out.push(d);
        }
    }
    let wider = out.join(" ");
    (!dirs.is_empty() && wider != command.split_whitespace().collect::<Vec<_>>().join(" ")).then_some(wider)
}

/// The tests a pytest run's output names as failing or erroring.
pub fn failing(output: &str) -> BTreeSet<String> {
    output
        .lines()
        .filter_map(|l| {
            let rest = l.strip_prefix("FAILED ").or_else(|| l.strip_prefix("ERROR "))?;
            Some(rest.split(" - ").next()?.trim().to_string())
        })
        .filter(|id| !id.is_empty())
        .collect()
}

impl Wide {
    /// The wider command for `command_line` and its failing `output`; `None` when there is none.
    pub fn new(command_line: &str, output: &str) -> Option<Wide> {
        Some(Wide { command: command(command_line, output)?, before: None })
    }

    /// The wider run on the tree as it is, `{code, output, passed, ms}`; stopped as soon as `stop`
    /// says so (code -3).
    pub fn run(&self, root: &Path, stop: &dyn Fn() -> bool) -> Value {
        let mut run = tools::shell_until(root, &self.command, LIMIT, stop);
        run["passed"] = Value::Bool(run["code"] == 0);
        run
    }

    /// Whether `run`, after an edit, needs what failed before to be judged: it failed, and ended.
    pub fn needs_before(run: &Value) -> bool {
        !matches!(run["code"].as_i64(), Some(0 | -2 | -3))
    }

    /// What a run before any edit says.
    pub fn read(run: &Value) -> Before {
        Before { failing: failing(run["output"].as_str().unwrap_or_default()), code: run["code"].as_i64().unwrap_or(-1) }
    }

    /// What `run`, after an edit, says against what failed before.
    pub fn verdict(&self, run: Value) -> Verdict {
        let code = run["code"].as_i64().unwrap_or(-1);
        let timed_out = code == -2;
        let mut new = Vec::new();
        // A run that passes shows nothing new; one before any edit that said nothing leaves
        // nothing to judge against.
        if let Some(before) = self.before.as_ref().filter(|b| b.code >= 0)
            && Wide::needs_before(&run)
        {
            let now = failing(run["output"].as_str().unwrap_or_default());
            new = now.difference(&before.failing).cloned().collect();
            // A run that dies without naming a test (a collection error in a file that read before).
            if new.is_empty() && now.is_empty() && before.code == 0 {
                new.push(format!("the wider run exited {code}"));
            }
        }
        Verdict { new, timed_out, run }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    const BASE: &str = "PYTHONPATH=src:. python -m pytest -q -p no:cacheprovider -o addopts=";

    #[test]
    fn the_wider_command_is_the_directory_of_the_failing_test_file() {
        assert_eq!(command(&format!("{BASE} tests/test_a.py"), "").as_deref(), Some(&*format!("{BASE} tests")));
        assert_eq!(
            command(&format!("{BASE} pkg/tests/test_a.py::test_x -k spam"), "").as_deref(),
            Some(&*format!("{BASE} pkg/tests"))
        );
        assert_eq!(command(&format!("{BASE} test_a.py"), "").as_deref(), Some(&*format!("{BASE} .")));
        // The diagnostic names a file the command did not.
        let out = "FAILED other/test_b.py::test_y - AssertionError\n";
        assert_eq!(
            command(&format!("{BASE} tests/test_a.py"), out).as_deref(),
            Some(&*format!("{BASE} tests other"))
        );
    }

    #[test]
    fn nothing_is_widened_that_is_already_wide_or_is_not_pytest() {
        assert_eq!(command(&format!("{BASE} tests"), ""), None);
        assert_eq!(command(BASE, ""), None);
        assert_eq!(command("cargo test -p x", ""), None);
        assert_eq!(command("pytest 'tests/test a.py'", ""), None);
    }

    #[test]
    fn failing_tests_are_the_runners_summary_lines() {
        let out = "FAILED tests/a.py::test_x - assert 1 == 2\nERROR tests/b.py - ImportError\n1 failed, 1 error in 0.1s\n";
        assert_eq!(
            failing(out).into_iter().collect::<Vec<_>>(),
            ["tests/a.py::test_x", "tests/b.py"]
        );
    }

    #[test]
    fn a_failure_there_before_the_edit_is_not_the_edits() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::create_dir_all(root.join("t")).unwrap();
        // A "runner" that prints FAILED lines for the files that hold a marker.
        let script = "for f in t/*.txt; do grep -q BAD $f && echo \"FAILED $f::case - boom\"; done; grep -rq BAD t && exit 1; exit 0";
        std::fs::write(root.join("t/old.txt"), "BAD").unwrap();
        std::fs::write(root.join("t/new.txt"), "good").unwrap();
        let mut wide = Wide { command: script.to_string(), before: None };
        let run = wide.run(root, &|| false);
        assert!(Wide::needs_before(&run), "the run before fails");
        wide.before = Some(Wide::read(&run));
        let v = wide.verdict(wide.run(root, &|| false));
        assert!(v.new.is_empty(), "{:?}", v.new);
        std::fs::write(root.join("t/new.txt"), "BAD").unwrap();
        let v = wide.verdict(wide.run(root, &|| false));
        assert_eq!(v.new, ["t/new.txt::case"]);
        assert!(!v.timed_out);
    }

    #[test]
    fn a_run_that_passes_needs_nothing_from_before_and_a_stopped_one_says_nothing() {
        let dir = tempfile::tempdir().unwrap();
        let wide = Wide { command: "true".into(), before: None };
        let run = wide.run(dir.path(), &|| false);
        assert!(!Wide::needs_before(&run));
        assert!(wide.verdict(run).new.is_empty());
        let wide = Wide { command: "sleep 5".into(), before: None };
        let run = wide.run(dir.path(), &|| true);
        assert_eq!(run["code"], -3);
        assert!(!Wide::needs_before(&run));
    }
}
