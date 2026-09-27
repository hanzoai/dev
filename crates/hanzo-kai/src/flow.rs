//! Flows: a pipeline over a repository, attempt after attempt.
//!
//! `fix` makes a failing test pass. Each attempt: retrieval ranks the repository's files by the
//! words they share with the failure (and the previous attempt), Kai picks the file to change
//! among those (`fix.pick@1`), Zen writes a diff against it, the diff is applied, the tests
//! run, and Kai says done, retry or escalate (`fix.next@1`). Only Zen generates; retrieval,
//! patching and testing are deterministic tools, and Kai is a classifier over their output.
//!
//! Under Kai control Kai's `next` acts when its gate accepts it and the tests and the budget
//! permit it: `done` only once the tests pass, `retry` only while attempts remain, `escalate`
//! always. Under rule control, or when Kai does not answer, the tests and the budget decide
//! alone and the best retrieval match is read. An attempt that is retried or escalated is
//! undone, so the next starts from the original tree and a flow that ends anywhere but `done`
//! leaves the tree as it found it.

use crate::decide;
use crate::decide::Call;
use crate::decide::Decider;
use crate::decide::Decision;
use crate::program::Program;
use crate::tools;
use crate::trace::Line;
use crate::trace::Step;
use crate::trace::Trace;
use hanzo_config::kai::Mode;
use serde_json::Value;
use serde_json::json;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

/// The flow's name in the trace.
const SKU: &str = "flow.fix@1";
/// Candidate files retrieval offers Kai.
const CANDIDATES: usize = 8;
/// How much of a candidate file Kai reads.
const HEAD: usize = 2000;

const PROMPT: &str = "The test command in `task` fails with the output in `failure`. The file \
most likely at fault is `read.path`; its content is `read.content`. When `last` is not null, it \
is the previous attempt: its patch and the test output after it. Write the smallest change that \
makes the test pass without weakening the test, as one unified diff against the repository root.";

/// Who steers the loop and picks the file: Kai, or the tests, the budget and the ranking.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Control {
    Rule,
    Kai,
}

pub struct Options {
    /// `{test, goal}`.
    pub task: Value,
    /// Most attempts.
    pub max: u32,
    pub control: Control,
    /// The longest one test run may take.
    pub limit: Duration,
}

/// One attempt, as the report shows it.
#[derive(Debug, Clone, PartialEq)]
pub struct Attempt {
    pub n: u32,
    /// The file read, and who picked it.
    pub file: Option<String>,
    pub by: String,
    pub applied: bool,
    pub passed: bool,
    /// Kai's answer to `next` and its probability.
    pub kai: Option<(String, f64)>,
    pub rule: String,
    pub taken: String,
}

pub struct Report {
    pub attempts: Vec<Attempt>,
    /// `done`, `escalate`, or `passing` when the tests passed before any attempt.
    pub outcome: String,
    /// The last attempt's patch, as Zen wrote it.
    pub patch: String,
    /// Why Kai did not answer, the first time it did not.
    pub missing: Option<String>,
}

impl Report {
    pub fn summary(&self) -> String {
        let mut out = String::new();
        if self.outcome == "passing" {
            out.push_str("The tests already pass; nothing to do.\n");
            return out;
        }
        for a in &self.attempts {
            let kai = a
                .kai
                .as_ref()
                .map(|(answer, p)| format!("{answer} ({p:.2})"))
                .unwrap_or_else(|| "none".into());
            out.push_str(&format!(
                "attempt {}: file {} (by {}), patch {}, tests {}; next: kai {kai}, rule {} -> {}\n",
                a.n,
                a.file.as_deref().unwrap_or("-"),
                a.by,
                if a.applied { "applied" } else { "not applied" },
                if a.passed { "pass" } else { "fail" },
                a.rule,
                a.taken,
            ));
        }
        if let Some(why) = &self.missing {
            out.push_str(&format!(
                "kai did not answer ({why}); the tests and the budget decided\n"
            ));
        }
        out.push_str(&format!("outcome: {}\n", self.outcome));
        if self.outcome == "escalate" && !self.patch.is_empty() {
            out.push_str("the tree is as it was; the last patch tried:\n");
            out.push_str(&tools::diff_of(&self.patch));
        }
        out
    }
}

/// A flow's backends: Kai and its two programs, Zen, the trace.
pub struct Setup {
    pub decider: Arc<Decider>,
    pub pick: Program,
    pub next: Program,
    pub zen: crate::zen::Zen,
    pub trace: Trace,
    runtime: tokio::runtime::Handle,
}

impl Setup {
    /// Kai and the trace from the profile at `home` (`kai.toml`, or its defaults), Zen at
    /// `base`, both signed with `key`. Call from within the async runtime.
    pub fn new(home: &Path, base: &str, key: Option<String>) -> Result<Setup, String> {
        let settings = hanzo_config::kai::Kai::load_or_default(home).map_err(|e| e.to_string())?;
        Ok(Setup {
            decider: Arc::new(Decider::new(&settings.url, &settings.model, key.clone())),
            pick: Program::load("fix.pick@1")?,
            next: Program::load("fix.next@1")?,
            zen: crate::zen::Zen::new(base, key, &settings.zen),
            trace: Trace::new(settings.trace),
            runtime: tokio::runtime::Handle::current(),
        })
    }

    /// Kai's ruling on `states` under `program`; `None` when Kai does not answer, and why
    /// in `missing` the first time.
    fn decide(
        &self,
        program: &Program,
        states: Vec<Value>,
        missing: &mut Option<String>,
    ) -> Option<Decision> {
        let call = Call {
            mode: Mode::Enforced,
            k: None,
            base: None,
        };
        match self
            .runtime
            .block_on(self.decider.decide(program, states, call))
        {
            Ok(d) => Some(d),
            Err(why) => {
                missing.get_or_insert(why);
                None
            }
        }
    }
}

/// The text of a value: strings as they are, anything else as JSON.
fn text(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Null => String::new(),
        Value::Object(m) => m.values().map(text).collect::<Vec<_>>().join("\n"),
        Value::Array(a) => a.iter().map(text).collect::<Vec<_>>().join("\n"),
        other => other.to_string(),
    }
}

/// A distribution's likeliest option and its probability.
fn likeliest(dist: &Value) -> Option<(String, f64)> {
    dist.as_object()?
        .iter()
        .filter_map(|(k, v)| Some((k.clone(), v.as_f64()?)))
        .max_by(|a, b| a.1.total_cmp(&b.1))
}

/// Runs the `fix` flow over the repository at `root`. Blocks: call it off the async runtime.
pub fn run(root: &Path, options: &Options, setup: &Setup) -> Result<Report, String> {
    let command = options.task["test"].as_str().unwrap_or_default();
    if command.is_empty() {
        return Err("the task names no test command".into());
    }
    let failure = tools::test(root, command, options.limit);
    if failure["passed"] == true {
        return Ok(Report {
            attempts: Vec::new(),
            outcome: "passing".into(),
            patch: String::new(),
            missing: None,
        });
    }
    let kai = options.control == Control::Kai;
    let run_id = crate::trace::now();
    let mut last = Value::Null;
    let mut report = Report {
        attempts: Vec::new(),
        outcome: String::new(),
        patch: String::new(),
        missing: None,
    };
    for n in 1..=options.max.max(1) {
        let query = [&options.task, &failure, &last]
            .into_iter()
            .map(text)
            .collect::<Vec<_>>()
            .join("\n");
        let candidates: Vec<String> = tools::search(root, &query, CANDIDATES, tools::HEAD)
            .into_iter()
            .map(|hit| hit.path)
            .collect();
        if candidates.is_empty() {
            return Err("no file in the repository shares a word with the failure".into());
        }

        // Kai picks the file among the candidates; the best match otherwise.
        let pick = if kai {
            let states = candidates
                .iter()
                .map(|path| {
                    let head = tools::head(root, path, HEAD).unwrap_or_default();
                    json!({
                        "failure": {"code": failure["code"], "output": failure["output"]},
                        "last": last,
                        "file": {"path": path, "head": head},
                    })
                })
                .collect();
            setup.decide(&setup.pick, states, &mut report.missing)
        } else {
            None
        };
        let rated = |d: &Decision, i: usize| match d.results.get(i) {
            Some(decide::Row::Ruled(r)) => r.answers["pick"]["true"].as_f64(),
            _ => None,
        };
        let (index, by) = match &pick {
            Some(d) => (0..candidates.len())
                .filter_map(|i| Some((i, rated(d, i)?)))
                .max_by(|a, b| a.1.total_cmp(&b.1))
                .map_or((0, "rule"), |(i, _)| (i, "kai")),
            None => (0, "rule"),
        };
        let path = candidates[index].clone();
        let content = tools::head(root, &path, tools::HEAD)
            .ok_or_else(|| format!("{path} is not a text file"))?;

        // Zen writes the change; the tools apply it and run the tests.
        let context = json!({
            "task": options.task,
            "failure": failure,
            "read": {"path": path, "content": content},
            "last": last,
        });
        let answer = setup.zen.generate(PROMPT, &context)?;
        let applied = tools::patch(root, &answer);
        let test = tools::test(root, command, options.limit);
        let passed = test["passed"] == true;
        let was_applied = applied["applied"] == true;

        // Kai says what next, within what the tests and the budget permit.
        let next = if kai {
            setup.decide(
                &setup.next,
                vec![json!({"test": test, "last": last})],
                &mut report.missing,
            )
        } else {
            None
        };
        let signal = next
            .as_ref()
            .and_then(decide::first)
            .and_then(|r| r.signals.get("next"));
        let rule = match (passed, n < options.max) {
            (true, _) => "done",
            (false, true) => "retry",
            (false, false) => "escalate",
        };
        let permitted: &[&str] = match rule {
            "done" => &["done", "escalate"],
            "retry" => &["retry", "escalate"],
            _ => &["escalate"],
        };
        let taken = signal
            .and_then(decide::accepted)
            .filter(|answer| permitted.contains(answer))
            .unwrap_or(rule)
            .to_string();
        let attempt = Attempt {
            n,
            file: Some(path.clone()),
            by: by.to_string(),
            applied: was_applied,
            passed,
            kai: signal.map(|s| (s.answer.clone(), s.certainty)),
            rule: rule.into(),
            taken: taken.clone(),
        };
        setup.trace.write(&line(
            &candidates,
            pick.as_ref(),
            next.as_ref(),
            &attempt,
            &run_id,
        ));
        report.attempts.push(attempt);
        report.patch.clone_from(&answer);
        // Undo the attempt: the next starts from the tree as the flow found it. A tree the
        // patch no longer reverses from (the test command rewrote a file) ends the flow.
        if taken != "done" && was_applied && !tools::revert(root, &answer) {
            return Err(format!(
                "attempt {n}: the patch no longer reverses cleanly, so the working tree still \
                 holds it; see `git diff`"
            ));
        }
        if taken != "retry" {
            report.outcome = taken;
            return Ok(report);
        }
        last = json!({"patch": tools::diff_of(&answer), "apply": applied, "test": test});
    }
    report.outcome = "escalate".into();
    Ok(report)
}

/// An attempt's trace line: Kai's pick and its `next`, with what was done and what followed.
fn line(
    candidates: &[String],
    pick: Option<&Decision>,
    next: Option<&Decision>,
    attempt: &Attempt,
    run: &str,
) -> Line {
    let mut ops = std::collections::BTreeMap::new();
    if let Some(d) = pick {
        let mut step = Step::new(&d.program, Mode::Enforced.name());
        step.fill(d);
        step.kai = d
            .results
            .iter()
            .zip(candidates)
            .filter_map(|(row, path)| match row {
                decide::Row::Ruled(r) => Some((path, r.answers["pick"]["true"].as_f64()?)),
                decide::Row::Failed { .. } => None,
            })
            .max_by(|a, b| a.1.total_cmp(&b.1))
            .map(|(path, _)| path.clone())
            .unwrap_or_default();
        step.taken = attempt.file.clone().unwrap_or_default();
        step.applied = attempt.by == "kai";
        ops.insert("pick".to_string(), step);
    }
    if let Some(d) = next {
        let mut step = Step::new(&d.program, Mode::Enforced.name());
        step.fill(d);
        step.kai = decide::first(d)
            .and_then(|r| likeliest(&r.answers["next"]))
            .map(|(a, _)| a)
            .unwrap_or_default();
        step.taken = attempt.taken.clone();
        step.applied = attempt
            .kai
            .as_ref()
            .is_some_and(|(a, _)| *a == attempt.taken)
            && attempt.taken != attempt.rule;
        ops.insert("next".to_string(), step);
    }
    Line {
        time: crate::trace::now(),
        request: format!("{run}#{}", attempt.n),
        family: "dev.flow".into(),
        sku: SKU.into(),
        ms: ops.values().map(|s| s.ms).sum(),
        outcome: crate::trace::Outcome {
            status: i64::from(!attempt.passed),
            finish: attempt.taken.clone(),
            escalated: attempt.taken == "escalate",
            result: json!({"passed": attempt.passed, "applied": attempt.applied, "attempt": attempt.n}),
            ..Default::default()
        },
        ops,
        ..Line::default()
    }
}

/// Runs the `fix` flow at `root` with `setup`, off the async runtime.
pub async fn run_with(setup: Setup, root: PathBuf, options: Options) -> Result<Report, String> {
    tokio::task::spawn_blocking(move || run(&root, &options, &setup))
        .await
        .map_err(|e| e.to_string())?
}

#[cfg(test)]
#[path = "flow_tests.rs"]
mod tests;
