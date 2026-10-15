//! A wider test before `done`.
//!
//! The failing command passing settles the failing tests and nothing near them. Before a flow
//! says `done`, the tests around the failing ones run too: the directory of every test file the
//! command or the failing output names, in place of the files and selectors that narrowed the
//! command. They ran once before any edit, so a failure that was there already is not the edit's;
//! a test failing now that was not then makes the edit not done. The run is capped at [`LIMIT`]; a
//! run that does not end by then says nothing. Only the command and the failing output are read,
//! never anything of the task beyond them.

use crate::tools;
use serde_json::Value;
use std::collections::BTreeSet;
use std::path::Path;
use std::time::Duration;

/// The longest a wider run may take.
pub const LIMIT: Duration = Duration::from_secs(20);

/// The wider command, and what failed in it before any edit.
#[derive(Debug, Clone)]
pub struct Wide {
    pub command: String,
    pub before: BTreeSet<String>,
    pub before_code: i64,
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
    /// The wider run on the tree as it is, before any edit, for what fails in it already. `None`
    /// when there is no wider command or the run does not end within [`LIMIT`].
    pub fn before(root: &Path, command_line: &str, output: &str) -> Option<Wide> {
        let command = command(command_line, output)?;
        let run = tools::test(root, &command, LIMIT);
        let code = run["code"].as_i64().unwrap_or(-1);
        if code == -2 || code == -1 {
            return None;
        }
        Some(Wide {
            before: failing(run["output"].as_str().unwrap_or_default()),
            before_code: code,
            command,
        })
    }

    /// The wider run on the tree as it is, against what failed before.
    pub fn check(&self, root: &Path) -> Verdict {
        let run = tools::test(root, &self.command, LIMIT);
        let code = run["code"].as_i64().unwrap_or(-1);
        let timed_out = code == -2;
        let now = failing(run["output"].as_str().unwrap_or_default());
        let mut new: Vec<String> = now.difference(&self.before).cloned().collect();
        // A run that dies without naming a test (a collection error in a file that read before).
        if new.is_empty() && now.is_empty() && code != 0 && !timed_out && self.before_code == 0 {
            new.push(format!("the wider run exited {code}"));
        }
        Verdict { new: if timed_out { Vec::new() } else { new }, timed_out, run }
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
        let before = Wide {
            command: script.to_string(),
            before: ["t/old.txt::case".to_string()].into(),
            before_code: 1,
        };
        let v = before.check(root);
        assert!(v.new.is_empty(), "{:?}", v.new);
        std::fs::write(root.join("t/new.txt"), "BAD").unwrap();
        let v = before.check(root);
        assert_eq!(v.new, ["t/new.txt::case"]);
        assert!(!v.timed_out);
    }
}
