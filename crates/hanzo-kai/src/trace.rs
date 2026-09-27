//! The decision trace: one JSON line per decision, for training.
//!
//! The shape is the line Enso's control writes per request (`time`, `request`, `family`,
//! `sku`, token counts, `ms`, `outcome`, and `ops` keyed by operation, each with its program,
//! hash, mode, the gate's mode per state, state hashes, Kai's distributions and signals, what
//! Kai would do, what was done and whether Kai's answer was applied), so one pipeline reads
//! both. A line carries state hashes and distributions, never the text of a request, a
//! command or an answer. Dev adds `outcome.result`, what followed the decision.

use crate::decide::Decision;
use crate::decide::Row;
use crate::program::Signal;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use std::collections::BTreeMap;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Line {
    pub time: String,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub request: String,
    pub family: String,
    pub sku: String,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub served: String,
    pub prompt_tokens: i64,
    pub cached_tokens: i64,
    pub completion_tokens: i64,
    pub reasoning_tokens: i64,
    pub ms: f64,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub cost: String,
    pub outcome: Outcome,
    pub ops: BTreeMap<String, Step>,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Outcome {
    pub status: i64,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub finish: String,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub escalated: bool,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub error: String,
    /// What followed the decision: the tools called, a command's result, a flow's tests.
    #[serde(default, skip_serializing_if = "Value::is_null")]
    pub result: Value,
}

/// One operation's decision.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Step {
    pub program: String,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub hash: String,
    /// As configured.
    pub mode: String,
    /// Per state, as the gate ran it.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub gates: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub states: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub answers: Vec<IndexMap<String, Value>>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub signals: Vec<IndexMap<String, Signal>>,
    /// The checkpoint that answered.
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub model: String,
    /// What Kai would do.
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub kai: String,
    /// What was done.
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub taken: String,
    pub applied: bool,
    #[serde(default, skip_serializing_if = "is_zero")]
    pub ms: f64,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub error: String,
}

fn is_zero(ms: &f64) -> bool {
    *ms == 0.0
}

impl Step {
    pub fn new(program: &str, mode: &str) -> Step {
        Step {
            program: program.to_string(),
            mode: mode.to_string(),
            ..Step::default()
        }
    }

    /// Fills the step from Kai's decision: every state's gate, hash, distributions and
    /// signals, in order.
    pub fn fill(&mut self, decision: &Decision) {
        self.program = decision.program.clone();
        self.hash = decision.hash.clone();
        self.model = decision.model.clone();
        self.ms = decision.ms;
        for row in &decision.results {
            match row {
                Row::Ruled(r) => {
                    self.states.push(r.state.clone());
                    self.gates.push(r.mode.name().to_string());
                    self.answers.push(r.answers.clone());
                    self.signals.push(r.signals.clone());
                }
                Row::Failed { state, error } => {
                    self.states.push(state.clone());
                    self.gates.push(String::new());
                    self.answers.push(IndexMap::new());
                    self.signals.push(IndexMap::new());
                    if self.error.is_empty() {
                        self.error = error.clone();
                    }
                }
            }
        }
    }
}

/// Now, as Enso writes it: RFC 3339, UTC, nanoseconds.
pub fn now() -> String {
    chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Nanos, true)
}

/// Appends lines to one file. Writes are whole lines, one at a time.
pub struct Trace {
    path: PathBuf,
    file: Mutex<Option<std::fs::File>>,
}

impl Trace {
    pub fn new(path: PathBuf) -> Trace {
        Trace {
            path,
            file: Mutex::new(None),
        }
    }

    /// Appends `line`. A trace that cannot be written is logged and skipped: the loop never
    /// waits on it.
    pub fn write(&self, line: &Line) {
        let Ok(mut text) = serde_json::to_vec(line) else {
            return;
        };
        text.push(b'\n');
        let mut file = self
            .file
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if file.is_none() {
            if let Some(dir) = self.path.parent() {
                let _ = std::fs::create_dir_all(dir);
            }
            match std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&self.path)
            {
                Ok(f) => *file = Some(f),
                Err(e) => {
                    tracing::warn!("kai trace {}: {e}", self.path.display());
                    return;
                }
            }
        }
        if let Some(f) = file.as_mut()
            && let Err(e) = f.write_all(&text)
        {
            tracing::warn!("kai trace {}: {e}", self.path.display());
        }
    }
}

/// A line that is written once both its decision and its outcome are in, in either order.
pub struct Record {
    trace: Arc<Trace>,
    inner: Mutex<Pending>,
}

struct Pending {
    line: Line,
    op: String,
    decided: bool,
    closed: bool,
    /// No decision was made: the line is never written.
    void: bool,
}

impl Record {
    pub fn new(trace: Arc<Trace>, line: Line, op: &str, step: Step) -> Arc<Record> {
        let mut line = line;
        line.ops.insert(op.to_string(), step);
        Arc::new(Record {
            trace,
            inner: Mutex::new(Pending {
                line,
                op: op.to_string(),
                decided: false,
                closed: false,
                void: false,
            }),
        })
    }

    fn with(&self, f: impl FnOnce(&mut Pending)) {
        let mut p = self
            .inner
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let was = p.decided && p.closed;
        f(&mut p);
        if !was && p.decided && p.closed && !p.void {
            self.trace.write(&p.line);
        }
    }

    /// The decision is in: `f` fills the step, and the outcome when the decision is its own.
    pub fn decided(&self, f: impl FnOnce(&mut Step, &mut Outcome)) {
        self.with(|p| {
            if let Some(step) = p.line.ops.get_mut(&p.op) {
                f(step, &mut p.line.outcome);
                p.line.ms = step.ms;
            }
            p.decided = true;
        });
    }

    /// Kai did not answer: nothing was decided, and the line is dropped.
    pub fn void(&self) {
        self.with(|p| {
            p.void = true;
            p.decided = true;
        });
    }

    /// The outcome is in: `f` fills the line. Only the first close counts.
    pub fn closed(&self, f: impl FnOnce(&mut Line)) {
        self.with(|p| {
            if !p.closed {
                f(&mut p.line);
                p.closed = true;
            }
        });
    }
}
