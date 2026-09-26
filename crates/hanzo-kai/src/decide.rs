//! Kai answering: the decision runtime's checkpoints in process, opened on first use, or any
//! other [`Ask`] (a fake, in tests).

use control::Ask;
use control::Call;
use control::Decision;
use control::Row;
use hanzo_config::kai::Mode;
use hanzo_config::kai::Op;
use program::policy::Signal;
use serde_json::Value;
use std::sync::Arc;
use std::sync::OnceLock;
use std::time::Duration;

pub type Shared = Arc<dyn Ask + Send + Sync>;

type Open = Box<dyn Fn() -> Result<Shared, String> + Send + Sync>;

/// Kai, opened once, answering from any thread.
pub struct Decider {
    open: Open,
    ask: OnceLock<Result<Shared, String>>,
}

impl Decider {
    /// Kai in process: `models` are decision runtime checkpoints, loaded on the first
    /// decision, on `device`.
    pub fn open(models: Vec<String>, device: String) -> Decider {
        Decider {
            open: Box::new(move || {
                let device = control::device(&device).map_err(|e| format!("{e:#}"))?;
                let kai = control::Kai::open(&models, &device).map_err(|e| format!("{e:#}"))?;
                Ok(Arc::new(kai) as Shared)
            }),
            ask: OnceLock::new(),
        }
    }

    /// Any answerer.
    pub fn with(ask: Shared) -> Decider {
        let ask = OnceLock::from(Ok(ask));
        Decider {
            open: Box::new(|| Err("unreachable".into())),
            ask,
        }
    }

    /// The answerer, opening it on first use. Blocks while it opens.
    pub fn ask(&self) -> Result<Shared, String> {
        self.ask.get_or_init(|| (self.open)()).clone()
    }

    /// Runs `call`. Blocks: call it off the async runtime.
    pub fn decide_blocking(&self, call: &Call) -> Result<Decision, String> {
        let ask = self.ask()?;
        control::decide(ask.as_ref(), call).map_err(|e| format!("{e:#}"))
    }

    /// Runs `call` on a blocking thread, waiting at most `wait` for it.
    pub async fn decide(self: &Arc<Self>, call: Call, wait: Duration) -> Result<Decision, String> {
        let me = Arc::clone(self);
        let run = tokio::task::spawn_blocking(move || me.decide_blocking(&call));
        match tokio::time::timeout(wait, run).await {
            Ok(Ok(decision)) => decision,
            Ok(Err(e)) => Err(format!("kai: {e}")),
            Err(_) => Err("late".into()),
        }
    }
}

pub fn mode(mode: Mode) -> program::Mode {
    match mode {
        Mode::Shadow => program::Mode::Shadow,
        Mode::Advisory => program::Mode::Advisory,
        Mode::Enforced => program::Mode::Enforced,
    }
}

/// A call for `op` over `states`: its program, mode, thresholds and k.
pub fn call(op: &Op, states: Vec<Value>, base: Option<program::Verdict>) -> Call {
    let program = match std::fs::read_to_string(&op.program) {
        // A path names a program file; anything else is a shipped id.
        Ok(text) => serde_json::from_str(&text).unwrap_or(Value::String(op.program.clone())),
        Err(_) => Value::String(op.program.clone()),
    };
    Call {
        program,
        states,
        mode: Some(mode(op.mode)),
        thresholds: op.thresholds.iter().map(|(q, t)| (q.clone(), *t)).collect(),
        k: op.k,
        base,
    }
}

/// A state's ruling, when Kai may act on it: its gate ran enforced.
pub fn enforced(decision: &Decision, index: usize) -> Option<&control::Ruled> {
    match decision.results.get(index)? {
        Row::Ruled(r) if r.mode == program::Mode::Enforced => Some(r),
        _ => None,
    }
}

/// Every state ruled under an enforced gate.
pub fn all_enforced(decision: &Decision) -> bool {
    !decision.results.is_empty()
        && (0..decision.results.len()).all(|i| enforced(decision, i).is_some())
}

/// The first state's ruling, whatever its mode.
pub fn first(decision: &Decision) -> Option<&control::Ruled> {
    match decision.results.first()? {
        Row::Ruled(r) => Some(r),
        Row::Failed { .. } => None,
    }
}

/// A signal's answer when it is accepted (a choice or score) or holds (a noul).
pub fn accepted(signal: &Signal) -> Option<&str> {
    let yes = signal.accepted.or(signal.holds)?;
    yes.then_some(signal.answer.as_str())
}

pub fn verdict(v: program::Verdict) -> hanzo_loop::Verdict {
    match v {
        program::Verdict::Allow => hanzo_loop::Verdict::Allow,
        program::Verdict::Ask => hanzo_loop::Verdict::Ask,
        program::Verdict::Deny => hanzo_loop::Verdict::Deny,
    }
}

pub fn policy(v: hanzo_loop::Verdict) -> program::Verdict {
    match v {
        hanzo_loop::Verdict::Allow => program::Verdict::Allow,
        hanzo_loop::Verdict::Ask => program::Verdict::Ask,
        hanzo_loop::Verdict::Deny => program::Verdict::Deny,
    }
}

/// `text` cut to at most `n` characters.
pub fn cut(text: &str, n: usize) -> String {
    match text.char_indices().nth(n) {
        Some((at, _)) => text[..at].to_string(),
        None => text.to_string(),
    }
}
