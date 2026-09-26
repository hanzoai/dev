//! Flows: a Decision Program run over a repository, attempt after attempt.
//!
//! A flow is a Decision Program in the `program` crate's schema, run by its executor. Its
//! solver nodes are Dev's deterministic tools (`search`, `read`, `patch`, `test`, `git`), its
//! kai nodes are Kai, its zen nodes are Zen, and nothing else calls a model. Evidence is
//! resolved by URI: `dev:task` is the task given on the command line, `dev:test` the failing
//! test run the attempt starts from, `dev:last` the previous attempt (null on the first) and
//! `git:status` HEAD with the working diff. A program that retrieves its options from
//! `dev:files` is bound, each attempt, to the repository files that share the most words with
//! its evidence: Kai chooses among those, never among every file.
//!
//! Two nodes steer the loop: `test`, whose output says whether the tests pass, and `next`, a
//! Kai choice over `done`, `retry` and `escalate`. Under Kai control Kai's answer acts when
//! its probability reaches [`CERTAIN`] and the tests and the budget permit it: `done` only once
//! the tests pass, `retry` only while attempts remain, `escalate` always. Under rule control
//! the tests and the budget decide alone. A `read` node reads the file Kai rated most likely
//! under Kai control, and the best retrieval match under rule control.
//!
//! An attempt that is retried or escalated is undone, so the next starts from the original
//! tree and a flow that ends anywhere but `done` leaves the tree as it found it. Attempts
//! share the node cache: a node whose inputs did not change is not run again.

use crate::decide::Decider;
use crate::tools;
use crate::trace::Line;
use crate::trace::Step;
use crate::trace::Trace;
use indexmap::IndexMap;
use program::Package;
use program::Program;
use program::Question;
use program::Revision;
use program::Snapshot;
use program::program::Alternative;
use serde_json::Value;
use serde_json::json;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

/// The probability at which Kai's answer to `next` acts.
pub const CERTAIN: f64 = 0.5;

/// The flows Dev ships, by name.
pub const SHIPPED: &[(&str, &str)] = &[("fix", include_str!("../flows/fix.json"))];

/// A shipped flow by name, or a flow file.
pub fn load(name: &str) -> Result<Program, String> {
    let text = match SHIPPED.iter().find(|(n, _)| *n == name) {
        Some((_, text)) => (*text).to_string(),
        None => std::fs::read_to_string(name).map_err(|e| format!("{name}: {e}"))?,
    };
    Program::parse(&text).map_err(|e| format!("{name}: {e}"))
}

/// Who steers the loop and picks the file: Kai, or the tests, the budget and the ranking.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Control {
    Rule,
    Kai,
}

pub struct Options {
    /// `dev:task`: `{test, goal}`.
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
    /// Nodes run, of all; the rest came from the cache.
    pub ran: usize,
    pub nodes: usize,
}

pub struct Report {
    pub attempts: Vec<Attempt>,
    /// `done`, `escalate`, or `passing` when the tests passed before any attempt.
    pub outcome: String,
    /// The last attempt's Decision Package.
    pub package: Option<Package>,
    /// The last attempt's patch, as Zen wrote it.
    pub patch: String,
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
                "attempt {}: file {} (by {}), patch {}, tests {}; next: kai {kai}, rule {} -> {}; {} of {} nodes ran\n",
                a.n,
                a.file.as_deref().unwrap_or("-"),
                a.by,
                if a.applied { "applied" } else { "not applied" },
                if a.passed { "pass" } else { "fail" },
                a.rule,
                a.taken,
                a.ran,
                a.nodes,
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

/// Kai answering a flow's kai nodes, one question at a time.
pub struct Judge(pub Arc<Decider>);

impl program::Judge for Judge {
    fn judge(&self, model: &str, question: &Question, state: &Value) -> program::Result<Vec<f64>> {
        let backend = |e: String| program::Error::Backend(e);
        let ask = self.0.ask().map_err(backend)?;
        let job = control::Job {
            state: state.clone(),
            questions: [("q".to_string(), question.clone())].into_iter().collect(),
        };
        let mut out = ask
            .ask(model, &[job])
            .map_err(|e| backend(format!("{e:#}")))?;
        let answers = out
            .pop()
            .ok_or_else(|| backend("no answer".into()))?
            .map_err(|e| backend(format!("{e:#}")))?;
        answers
            .into_values()
            .next()
            .ok_or_else(|| backend("no answer".into()))
    }

    fn revision(&self, model: &str) -> program::Result<Revision> {
        let ask = self.0.ask().map_err(program::Error::Backend)?;
        ask.revision(model)
            .map_err(|e| program::Error::Backend(format!("{e:#}")))
    }
}

/// Dev's deterministic tools as a flow's solvers.
struct Tools<'a> {
    root: &'a Path,
    control: Control,
    options: &'a Options,
}

impl Tools<'_> {
    /// The file to read: Kai's likeliest under Kai control, the best match under rule control.
    fn read(&self, context: &Value) -> Result<Value, String> {
        let options = context["options"].as_array().cloned().unwrap_or_default();
        if options.is_empty() {
            return Err("read: no candidate files".into());
        }
        // A kai node's output keyed by option id, `{id: {"false": p, "true": q}}`.
        let rated = context["inputs"]
            .as_object()
            .into_iter()
            .flat_map(|m| m.values())
            .find(|v| {
                options
                    .iter()
                    .all(|o| v.get(o["id"].as_str().unwrap_or_default()).is_some())
            });
        let (option, by, p) = match (self.control, rated) {
            (Control::Kai, Some(rated)) => {
                let p = |o: &Value| {
                    rated[o["id"].as_str().unwrap_or_default()]["true"]
                        .as_f64()
                        .unwrap_or(0.0)
                };
                let Some(best) = options.iter().max_by(|a, b| p(a).total_cmp(&p(b))) else {
                    return Err("read: no candidate files".into());
                };
                (best, "kai", Some(p(best)))
            }
            _ => (&options[0], "rule", None),
        };
        let path = option["label"].as_str().unwrap_or_default();
        let content = tools::head(self.root, path, tools::HEAD)
            .ok_or_else(|| format!("read: {path} is not a text file"))?;
        Ok(json!({"path": path, "content": content, "by": by, "p": p}))
    }
}

/// The text of a node's inputs: strings as they are, anything else as JSON.
fn text(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Null => String::new(),
        Value::Object(m) => m.values().map(text).collect::<Vec<_>>().join("\n"),
        Value::Array(a) => a.iter().map(text).collect::<Vec<_>>().join("\n"),
        other => other.to_string(),
    }
}

impl program::Solver for Tools<'_> {
    fn solve(&self, solver: &str, params: &Value, context: &Value) -> program::Result<Value> {
        let inputs = &context["inputs"];
        let out = match solver {
            "read" => self.read(context),
            "patch" => {
                let answer = inputs
                    .as_object()
                    .into_iter()
                    .flat_map(|m| m.values())
                    .find_map(Value::as_str)
                    .unwrap_or_default();
                Ok(tools::patch(self.root, answer))
            }
            "test" => {
                let command = params["command"]
                    .as_str()
                    .or_else(|| self.options.task["test"].as_str())
                    .unwrap_or_default();
                if command.is_empty() {
                    Err("test: no test command".to_string())
                } else {
                    Ok(tools::test(self.root, command, self.options.limit))
                }
            }
            "git" => {
                let args: Vec<String> = params["args"]
                    .as_array()
                    .into_iter()
                    .flatten()
                    .filter_map(|a| a.as_str().map(str::to_string))
                    .collect();
                tools::git(self.root, &args)
            }
            "search" => {
                let query = params["query"]
                    .as_str()
                    .map(str::to_string)
                    .unwrap_or_else(|| text(inputs));
                let top = params["top"].as_u64().unwrap_or(8) as usize;
                let hits = tools::search(self.root, &query, top, tools::HEAD);
                Ok(json!({"files": hits
                    .iter()
                    .map(|h| json!({"path": h.path, "score": h.score}))
                    .collect::<Vec<_>>()}))
            }
            other => Err(format!(
                "no tool {other:?}; a flow's tools are read, patch, test, git and search"
            )),
        };
        out.map_err(program::Error::Backend)
    }

    fn revision(&self, solver: &str) -> program::Result<Revision> {
        Ok(Revision {
            model: format!("dev.{solver}@1"),
            calibration: None,
        })
    }
}

/// The evidence a flow reads, by URI.
fn evidence(
    uri: &str,
    task: &Value,
    failure: &Value,
    last: &Value,
    root: &Path,
) -> Result<Value, String> {
    Ok(match uri {
        "dev:task" => task.clone(),
        "dev:test" => failure.clone(),
        "dev:last" => last.clone(),
        "git:status" => {
            let head = tools::git(root, &["rev-parse".into(), "HEAD".into()])?;
            let diff = tools::git(root, &["diff".into()])?;
            json!({"head": head["output"].as_str().unwrap_or_default().trim(), "diff": diff["output"]})
        }
        other => {
            return Err(format!(
                "no evidence source {other:?}; a flow reads dev:task, dev:test, dev:last and git:status"
            ));
        }
    })
}

/// A Kai node's distribution: its likeliest label and probability.
fn likeliest(dist: &Value) -> Option<(String, f64)> {
    dist.as_object()?
        .iter()
        .filter_map(|(k, v)| Some((k.clone(), v.as_f64()?)))
        .max_by(|a, b| a.1.total_cmp(&b.1))
}

/// Runs `flow` over the repository at `root`. Blocks: call it off the async runtime.
pub fn run(
    flow: &Program,
    root: &Path,
    options: &Options,
    judge: &dyn program::Judge,
    zen: &dyn program::Zen,
    trace: &Trace,
) -> Result<Report, String> {
    for id in ["test", "next"] {
        if flow.node(id).is_none() {
            return Err(format!("{}: a flow needs a node {id:?}", flow.id));
        }
    }
    let command = options.task["test"].as_str().unwrap_or_default();
    if command.is_empty() {
        return Err("the task names no test command".into());
    }
    let failure = tools::test(root, command, options.limit);
    if failure["passed"] == true {
        return Ok(Report {
            attempts: Vec::new(),
            outcome: "passing".into(),
            package: None,
            patch: String::new(),
        });
    }
    let solvers = Tools {
        root,
        control: options.control,
        options,
    };
    let absent = program::Absent::default();
    let rt = program::Runtime {
        judge,
        zen,
        solver: &solvers,
        human: &absent,
    };
    let run_id = crate::trace::now();
    let mut cache = program::Cache::default();
    let mut last = Value::Null;
    let mut report = Report {
        attempts: Vec::new(),
        outcome: String::new(),
        package: None,
        patch: String::new(),
    };
    for n in 1..=options.max.max(1) {
        let time = chrono::Utc::now().timestamp();
        let mut snapshots = IndexMap::new();
        for e in &flow.evidence {
            let content = evidence(&e.uri, &options.task, &failure, &last, root)?;
            snapshots.insert(e.id.clone(), Snapshot { content, time });
        }
        let bound = match &flow.retrieve {
            Some(r) if r.graph == "dev:files" => {
                let query = snapshots
                    .values()
                    .map(|s| text(&s.content))
                    .collect::<Vec<_>>()
                    .join("\n");
                let options: Vec<Alternative> =
                    tools::search(root, &query, r.top as usize, tools::HEAD)
                        .into_iter()
                        .enumerate()
                        .map(|(i, hit)| Alternative {
                            id: format!("f{i}"),
                            label: hit.path,
                            attributes: [("score".to_string(), hit.score)].into_iter().collect(),
                        })
                        .collect();
                if options.is_empty() {
                    return Err("no file in the repository shares a word with the failure".into());
                }
                flow.bind(options).map_err(|e| e.to_string())?
            }
            Some(r) => {
                return Err(format!(
                    "no graph {:?}; a flow retrieves from dev:files",
                    r.graph
                ));
            }
            None => flow.clone(),
        };
        let package = program::execute(&bound, &snapshots, &[], &rt, &mut cache)
            .map_err(|e| e.to_string())?;
        let output = |id: &str| {
            package
                .results
                .get(id)
                .map(|o| o.output.clone())
                .unwrap_or(Value::Null)
        };
        let test = output("test");
        let passed = test["passed"] == true;
        let read = bound
            .nodes
            .iter()
            .find(|n| matches!(&n.op, program::Op::Solver(s) if s.solver == "read"))
            .map(|n| output(&n.id));
        let applied = bound
            .nodes
            .iter()
            .find(|n| matches!(&n.op, program::Op::Solver(s) if s.solver == "patch"))
            .map(|n| output(&n.id));
        let zen_answer = bound
            .nodes
            .iter()
            .find(|n| matches!(n.op, program::Op::Zen(_)))
            .map(|n| output(&n.id))
            .and_then(|v| v.as_str().map(str::to_string))
            .unwrap_or_default();
        let kai = likeliest(&output("next"));
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
        let taken = match (&kai, options.control) {
            (Some((answer, p)), Control::Kai)
                if *p >= CERTAIN && permitted.contains(&answer.as_str()) =>
            {
                answer.clone()
            }
            _ => rule.to_string(),
        };
        let was_applied = applied.as_ref().is_some_and(|a| a["applied"] == true);
        let attempt = Attempt {
            n,
            file: read
                .as_ref()
                .and_then(|r| r["path"].as_str().map(str::to_string)),
            by: read
                .as_ref()
                .and_then(|r| r["by"].as_str().map(str::to_string))
                .unwrap_or_default(),
            applied: was_applied,
            passed,
            kai: kai.clone(),
            rule: rule.into(),
            taken: taken.clone(),
            ran: package
                .trace
                .iter()
                .filter(|s| s.source == program::Source::Run)
                .count(),
            nodes: package.trace.len(),
        };
        trace.write(&line(
            flow,
            &bound,
            &package,
            &attempt,
            &run_id,
            options.control,
        ));
        report.attempts.push(attempt);
        report.patch = zen_answer.clone();
        report.package = Some(package);
        if taken != "done" && was_applied {
            // Undo the attempt: the next starts from the tree as the flow found it.
            tools::revert(root, &zen_answer);
        }
        match taken.as_str() {
            "retry" => {
                last =
                    json!({"patch": tools::diff_of(&zen_answer), "apply": applied, "test": test});
            }
            _ => {
                report.outcome = taken;
                return Ok(report);
            }
        }
    }
    report.outcome = "escalate".into();
    Ok(report)
}

/// An attempt's trace line: Kai's pick and its `next`, with what was done and what followed.
fn line(
    flow: &Program,
    bound: &Program,
    package: &Package,
    attempt: &Attempt,
    run: &str,
    control: Control,
) -> Line {
    let mode = match control {
        Control::Kai => "enforced",
        Control::Rule => "shadow",
    };
    let mut ops = std::collections::BTreeMap::new();
    for node in &bound.nodes {
        let program::Op::Kai(q) = &node.op else {
            continue;
        };
        let Some(out) = package.results.get(&node.id) else {
            continue;
        };
        let mut step = Step::new(&format!("{}@{}#{}", flow.id, flow.version, node.id), mode);
        step.hash = bound.hash();
        step.model = out
            .revision
            .as_ref()
            .map(|r| r.model.clone())
            .unwrap_or_default();
        match q.scope {
            program::program::Scope::Option => {
                for o in &bound.options {
                    step.states
                        .push(program::canon::hash(&json!({"source": o.label})));
                    step.gates.push(mode.into());
                    let mut answers = IndexMap::new();
                    answers.insert(node.id.clone(), out.output[&o.id].clone());
                    step.answers.push(answers);
                }
                step.kai = bound
                    .options
                    .iter()
                    .max_by(|a, b| {
                        let p = |o: &Alternative| out.output[&o.id]["true"].as_f64().unwrap_or(0.0);
                        p(a).total_cmp(&p(b))
                    })
                    .map(|o| o.label.clone())
                    .unwrap_or_default();
                step.taken = attempt.file.clone().unwrap_or_default();
                step.applied = attempt.by == "kai";
            }
            program::program::Scope::Program => {
                step.states.push(package.results[&node.id].inputs.clone());
                step.gates.push(mode.into());
                let mut answers = IndexMap::new();
                answers.insert(node.id.clone(), out.output.clone());
                step.answers.push(answers);
                step.kai = likeliest(&out.output).map(|(a, _)| a).unwrap_or_default();
                if node.id == "next" {
                    step.taken = attempt.taken.clone();
                    step.applied = attempt
                        .kai
                        .as_ref()
                        .is_some_and(|(a, _)| *a == attempt.taken)
                        && attempt.taken != attempt.rule;
                }
            }
        }
        ops.insert(node.id.clone(), step);
    }
    Line {
        time: crate::trace::now(),
        request: format!("{run}#{}", attempt.n),
        family: "dev.flow".into(),
        sku: format!("{}@{}", flow.id, flow.version),
        ms: package.trace.iter().map(|s| s.ms).sum(),
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

/// A flow's backends and settings from the profile at `home`: Kai in process, Zen through
/// the Hanzo API.
pub struct Setup {
    pub decider: Arc<Decider>,
    pub zen: crate::zen::Zen,
    pub trace: Trace,
}

impl Setup {
    pub fn new(home: &Path, base: &str, key: Option<String>) -> Result<Setup, String> {
        let settings = hanzo_config::kai::Kai::load_or_default(home).map_err(|e| e.to_string())?;
        Ok(Setup {
            decider: Arc::new(Decider::open(
                settings.models.clone(),
                settings.device.clone(),
            )),
            zen: crate::zen::Zen::new(base, key, &settings.zen),
            trace: Trace::new(settings.trace),
        })
    }
}

/// Runs `flow` at `root` with Kai and Zen from `setup`, off the async runtime.
pub async fn run_with(
    setup: Setup,
    flow: Program,
    root: PathBuf,
    options: Options,
) -> Result<Report, String> {
    tokio::task::spawn_blocking(move || {
        let judge = Judge(Arc::clone(&setup.decider));
        run(&flow, &root, &options, &judge, &setup.zen, &setup.trace)
    })
    .await
    .map_err(|e| e.to_string())?
}

#[cfg(test)]
#[path = "flow_tests.rs"]
mod tests;
