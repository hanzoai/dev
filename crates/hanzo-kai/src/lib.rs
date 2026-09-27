//! Kai in Hanzo Dev.
//!
//! Zen generates, Enso routes, Kai decides, policy governs. Kai is reached over the Decisions
//! API (`POST https://api.hanzo.ai/v1/decisions`), signed with the Hanzo credential Dev
//! already uses; nothing of it is linked in. In the agent loop ([`agent`]) Kai makes the
//! loop's decisions per turn, each a program of typed questions ([`program`]) run in the mode
//! `kai.toml` gives it. A flow ([`flow`]) runs over a repository, its steps deterministic
//! tools, Kai decisions or Zen generation, under Kai's control or the tests' and the budget's.
//! Every decision is a line in the trace ([`trace`]), the shape Enso's control writes. When
//! Kai does not answer, Dev decides as it does without it ([`decide`]).

pub mod agent;
pub mod decide;
pub mod flow;
pub mod program;
#[cfg(test)]
mod testing;
pub mod tools;
pub mod trace;
pub mod zen;

pub use agent::extension::install;
pub use decide::Decider;
pub use hanzo_config::kai::Kai as Settings;

use hanzo_config::kai::Operation;
use program::Program;
use std::collections::HashMap;
use std::sync::Arc;
use trace::Trace;

/// Kai for one profile: its settings, the programs its operations ask, the client that asks
/// them, and the trace.
pub struct Kai {
    pub settings: Settings,
    pub decider: Arc<Decider>,
    pub trace: Arc<Trace>,
    programs: HashMap<Operation, Arc<Program>>,
}

impl Kai {
    /// Kai from `settings`, signed with `key`. Fails when an operation's program does not
    /// load or its thresholds name no question.
    pub fn open(settings: Settings, key: Option<String>) -> Result<Kai, String> {
        let mut programs = HashMap::new();
        for op in Operation::ALL {
            let o = settings.op(op);
            let mut program = Program::load(&o.program)?;
            program
                .configure(o.thresholds.iter().map(|(q, t)| (q.clone(), *t)))
                .map_err(|e| format!("kai.toml: ops.{}: {e}", op.name()))?;
            programs.insert(op, Arc::new(program));
        }
        let decider = Decider::new(&settings.url, &settings.model, key);
        Ok(Kai {
            trace: Arc::new(Trace::new(settings.trace.clone())),
            decider: Arc::new(decider),
            settings,
            programs,
        })
    }

    /// The program `op` asks, its thresholds applied.
    pub fn program(&self, op: Operation) -> Arc<Program> {
        Arc::clone(&self.programs[&op])
    }
}
