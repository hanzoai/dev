//! Kai in Hanzo Dev.
//!
//! Zen generates, Enso routes, Kai decides, policy governs. In the agent loop ([`agent`]) Kai
//! makes the loop's decisions per turn, each a Decision Program run through `control` in
//! process, each in the mode `kai.toml` gives it. A flow ([`flow`]) is a Decision Program run
//! over a repository, its steps deterministic tools, Kai decisions or Zen generation, under
//! Kai's control or the tests' and the budget's. Every decision is a line in the trace
//! ([`trace`]), the shape Enso's control writes.

pub mod agent;
pub mod decide;
#[cfg(test)]
mod fake;
pub mod flow;
pub mod tools;
pub mod trace;
pub mod zen;

pub use agent::extension::install;
pub use decide::Decider;
pub use hanzo_config::kai::Kai as Settings;

use std::sync::Arc;
use trace::Trace;

/// Kai for one profile: its settings, the checkpoint it answers from, and the trace.
pub struct Kai {
    pub settings: Settings,
    pub decider: Arc<Decider>,
    pub trace: Arc<Trace>,
}

impl Kai {
    /// Kai in process, from `settings`; the checkpoint loads on the first decision.
    pub fn open(settings: Settings) -> Kai {
        let decider = Decider::open(settings.models.clone(), settings.device.clone());
        Kai::with(settings, decider)
    }

    pub fn with(settings: Settings, decider: Decider) -> Kai {
        let trace = Arc::new(Trace::new(settings.trace.clone()));
        Kai {
            settings,
            decider: Arc::new(decider),
            trace,
        }
    }
}
