//! Kai's settings: `kai.toml` in the product home.
//!
//! Kai answers over the Decisions API, `POST {url}/decisions`, signed with the Hanzo
//! credential Dev already uses. Each operation names the program it asks (its questions and
//! gate), the mode it runs in, the thresholds merged over the program's, and for a selection
//! how many candidates it keeps. `shadow` records the decision, `advisory` also shows it, and
//! only `enforced` lets it act, and then only toward the stricter outcome. A missing key is
//! its default; every operation defaults to `shadow`. Without the file Kai does not run in
//! the agent loop.

use serde::Deserialize;
use std::collections::BTreeMap;
use std::io;
use std::path::Path;
use std::path::PathBuf;

/// The file, under the product home.
pub const FILE: &str = "kai.toml";

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Mode {
    /// Recorded; never shown, never applied.
    #[default]
    Shadow,
    /// Recorded and shown; the deterministic rule still decides.
    Advisory,
    /// Joined with the deterministic rule: it may tighten, never loosen.
    Enforced,
}

impl Mode {
    pub fn name(self) -> &'static str {
        match self {
            Mode::Shadow => "shadow",
            Mode::Advisory => "advisory",
            Mode::Enforced => "enforced",
        }
    }
}

/// The operations Kai decides in the agent loop. The names are Enso's, so one pipeline reads
/// both traces.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operation {
    /// Which tools the model sees directly.
    Tools,
    /// Whether a command runs, asks first or is refused.
    Risk,
    /// Which model tier serves the turn.
    Model,
    /// How much the serving model reasons.
    Reasoning,
    /// Which files the turn starts from.
    Context,
    /// Whether the agent is advancing.
    Progress,
    /// Whether the request is done.
    Complete,
    /// Whether verified outcomes warrant bounded follow-up learning.
    Learn,
}

impl Operation {
    pub const ALL: [Operation; 8] = [
        Operation::Tools,
        Operation::Risk,
        Operation::Model,
        Operation::Reasoning,
        Operation::Context,
        Operation::Progress,
        Operation::Complete,
        Operation::Learn,
    ];

    pub fn name(self) -> &'static str {
        match self {
            Operation::Tools => "tools",
            Operation::Risk => "risk",
            Operation::Model => "model",
            Operation::Reasoning => "reasoning",
            Operation::Context => "context",
            Operation::Progress => "progress",
            Operation::Complete => "complete",
            Operation::Learn => "learn",
        }
    }

    /// The shipped program an operation runs unless `kai.toml` names another.
    pub fn program(self) -> &'static str {
        match self {
            Operation::Tools => "tools.select@1",
            Operation::Risk => "agent.command-risk@1",
            Operation::Model => "router.model@1",
            Operation::Reasoning => "reasoning.budget@1",
            Operation::Context => "context.select@1",
            Operation::Progress => "agent.progress@1",
            Operation::Complete => "agent.complete@1",
            Operation::Learn => "agent.learn@1",
        }
    }

    /// How many candidates a selection keeps unless `kai.toml` says.
    fn k(self) -> Option<usize> {
        match self {
            Operation::Tools => Some(8),
            Operation::Context => Some(4),
            _ => None,
        }
    }
}

/// One operation's settings as written.
#[derive(Debug, Clone, Default, PartialEq, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct Written {
    program: Option<String>,
    mode: Mode,
    thresholds: BTreeMap<String, f64>,
    k: Option<usize>,
}

/// One operation's settings, defaults applied.
#[derive(Debug, Clone, PartialEq)]
pub struct Op {
    /// A program Dev ships (`agent.command-risk@1`) or a path to a program file.
    pub program: String,
    pub mode: Mode,
    /// Merged over the program's own, by question.
    pub thresholds: BTreeMap<String, f64>,
    /// Candidates a selection keeps.
    pub k: Option<usize>,
}

/// The generator as written.
#[derive(Debug, Clone, Default, PartialEq, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct Writing {
    url: Option<String>,
    model: Option<String>,
    spend: Option<PathBuf>,
    budget: Option<f64>,
    escalate: Option<String>,
    calls: Option<PathBuf>,
}

#[derive(Debug, Clone, Default, PartialEq, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct File {
    url: Option<String>,
    model: Option<String>,
    trace: Option<PathBuf>,
    generate: Writing,
    ops: BTreeMap<String, Written>,
}

/// The model that writes code: Zen over an OpenAI-compatible `POST {url}/chat/completions`.
#[derive(Debug, Clone, PartialEq)]
pub struct Generate {
    /// Hanzo's API, or any server that speaks it; Hanzo's credential goes to Hanzo's alone.
    pub url: String,
    pub model: String,
    /// Where each paid call is recorded, JSON lines with the model that served it and its cost.
    pub spend: PathBuf,
    /// Dollars the spend file may hold before a call is refused; none, no limit.
    pub budget: Option<f64>,
    /// The model the attempt after an escalation is written by, at the same endpoint and with
    /// the same credential: `openai/gpt-6-sol` at Hanzo's API, none elsewhere. Never an
    /// Anthropic model.
    pub escalate: Option<String>,
    /// Where each model call is kept, by the hash of its request; a repeated request is answered
    /// from there. `$HOME/.cache/hanzo/dev/calls` unless `kai.toml` names another.
    pub calls: Option<PathBuf>,
}

/// The escalation model at Hanzo's API unless `kai.toml` names another.
pub const ESCALATE: &str = "openai/gpt-6-sol";

#[derive(Debug, Clone, PartialEq)]
pub struct Kai {
    /// The Decisions API base: `POST {url}/decisions`.
    pub url: String,
    /// The model every question is put to.
    pub model: String,
    /// The decision trace, JSON lines, one per decision.
    pub trace: PathBuf,
    /// The model a flow's generation nodes call.
    pub generate: Generate,
    ops: BTreeMap<&'static str, Op>,
}

impl Kai {
    /// `home/kai.toml`, or `None` when there is none.
    pub fn load(home: &Path) -> io::Result<Option<Kai>> {
        match std::fs::read_to_string(home.join(FILE)) {
            Ok(text) => Kai::parse(&text, home).map(Some),
            Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
            Err(e) => Err(e),
        }
    }

    /// `home/kai.toml`, or the defaults when there is none.
    pub fn load_or_default(home: &Path) -> io::Result<Kai> {
        match Kai::load(home)? {
            Some(kai) => Ok(kai),
            None => Kai::parse("", home),
        }
    }

    /// Settings from `text`; a relative trace path is under `home`.
    pub fn parse(text: &str, home: &Path) -> io::Result<Kai> {
        let invalid =
            |e: String| io::Error::new(io::ErrorKind::InvalidData, format!("{FILE}: {e}"));
        let file: File = toml::from_str(text).map_err(|e| invalid(e.to_string()))?;
        if let Some(name) = file
            .ops
            .keys()
            .find(|name| !Operation::ALL.iter().any(|op| op.name() == name.as_str()))
        {
            let known: Vec<&str> = Operation::ALL.iter().map(|op| op.name()).collect();
            return Err(invalid(format!(
                "no operation {name:?}; the operations are {}",
                known.join(", ")
            )));
        }
        let ops = Operation::ALL
            .iter()
            .map(|op| {
                let written = file.ops.get(op.name()).cloned().unwrap_or_default();
                let settings = Op {
                    program: written.program.unwrap_or_else(|| op.program().to_string()),
                    mode: written.mode,
                    thresholds: written.thresholds,
                    k: written.k.or(op.k()),
                };
                (op.name(), settings)
            })
            .collect();
        let generate_url = file
            .generate
            .url
            .unwrap_or_else(|| hanzo::API_BASE.to_string())
            .trim_end_matches('/')
            .to_string();
        let escalate = match file.generate.escalate {
            Some(name) if name.trim().is_empty() => None,
            Some(name) => Some(name),
            None => hanzo::is_hanzo(Some(&generate_url)).then(|| ESCALATE.to_string()),
        };
        if let Some(name) = &escalate {
            let lower = name.to_lowercase();
            if lower.contains("anthropic") || lower.contains("claude") {
                return Err(invalid(format!(
                    "generate.escalate: {name:?} is an Anthropic model; the escalation model is not"
                )));
            }
        }
        Ok(Kai {
            url: file
                .url
                .unwrap_or_else(|| hanzo::API_BASE.to_string())
                .trim_end_matches('/')
                .to_string(),
            model: file.model.unwrap_or_else(|| "laya-agent".to_string()),
            trace: home.join(
                file.trace
                    .unwrap_or_else(|| PathBuf::from("kai/decisions.jsonl")),
            ),
            generate: Generate {
                url: generate_url,
                model: file
                    .generate
                    .model
                    .unwrap_or_else(|| "zen6-coder".to_string()),
                spend: home.join(
                    file.generate
                        .spend
                        .unwrap_or_else(|| PathBuf::from("kai/spend.jsonl")),
                ),
                budget: file.generate.budget,
                escalate,
                calls: file.generate.calls.map(|dir| home.join(dir)).or_else(|| {
                    std::env::var_os("HOME")
                        .map(|h| PathBuf::from(h).join(".cache/hanzo/dev/calls"))
                }),
            },
            ops,
        })
    }

    pub fn op(&self, op: Operation) -> &Op {
        &self.ops[op.name()]
    }
}

#[cfg(test)]
#[path = "kai_tests.rs"]
mod tests;
