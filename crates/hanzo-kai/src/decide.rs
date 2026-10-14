//! Kai answering over the Decisions API: one request per state, the states of a decision in
//! parallel, over a [`Transport`] (`POST {url}/decisions` over HTTP, or the local engine's ZAP
//! socket when it serves Kai).
//!
//! Kai is optional. When it cannot be reached — no connection, a timeout, a refusal, a server
//! error, or no state of a decision answered — the decision fails at once, that program is marked
//! down for [`COOLDOWN`], and every decision of it in that window fails without a request, so the
//! loop runs as it would without Kai. A program's outage is its own: one capability's timeout never
//! disables another (`site` timing out leaves `turn` asked). The first failure is logged; nothing
//! else is until a decision asked after it is answered.

use crate::program::Program;
use crate::program::Signal;
use crate::program::digest;
use hanzo_config::kai::Mode;
use hanzo_loop::Verdict;
use indexmap::IndexMap;
use serde_json::Value;
use serde_json::json;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::sync::Mutex;
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// How long a specific capability stays in cooldown after a failure before a half-open probe.
pub const CAPABILITY_COOLDOWN: Duration = Duration::from_secs(10);
/// Legacy global cooldown fallback.
pub const COOLDOWN: Duration = Duration::from_secs(10);
/// The longest a connection may take to open.
const CONNECT: Duration = Duration::from_secs(2);
/// The longest one request may take.
const REQUEST: Duration = Duration::from_secs(10);

/// Bounded per-capability default timeouts
pub const TIMEOUT_FAST: Duration = Duration::from_millis(150);
pub const TIMEOUT_STEP: Duration = Duration::from_millis(500);
pub const TIMEOUT_SEARCH: Duration = Duration::from_millis(1000);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakerState {
    Closed,
    HalfOpen,
    Open,
}

#[derive(Debug, Clone)]
pub struct CapabilityHealth {
    pub state: BreakerState,
    pub until: Option<Instant>,
    pub fell: Option<Instant>,
    pub consecutive_failures: u32,
    pub logged: bool,
}

impl Default for CapabilityHealth {
    fn default() -> Self {
        Self {
            state: BreakerState::Closed,
            until: None,
            fell: None,
            consecutive_failures: 0,
            logged: false,
        }
    }
}

/// How a request reaches Kai and its answer comes back: the HTTP status and the body. The status
/// is the Decisions API's whatever the wire: 200, 400 or 422 for a request Kai refused, anything
/// else for a server that cannot answer. An error is a connection that did not hold.
pub trait Transport: Send + Sync {
    fn send<'a>(&'a self, body: &'a Value) -> Pin<Box<dyn Future<Output = Result<(u16, String), String>> + Send + 'a>>;
}

/// `POST {url}/decisions` over HTTP, signed with `key`.
pub struct Http {
    url: String,
    key: Option<String>,
    client: reqwest::Client,
}

impl Http {
    pub fn new(url: &str, key: Option<String>) -> Http {
        Http {
            url: format!("{}/decisions", url.trim_end_matches('/')),
            key,
            client: reqwest::Client::builder()
                .connect_timeout(CONNECT)
                .timeout(REQUEST)
                .pool_idle_timeout(Duration::from_secs(300))
                .tcp_nodelay(true)
                .build()
                .unwrap_or_default(),
        }
    }
}

impl Transport for Http {
    fn send<'a>(&'a self, body: &'a Value) -> Pin<Box<dyn Future<Output = Result<(u16, String), String>> + Send + 'a>> {
        Box::pin(async move {
            let mut request = self.client.post(&self.url).json(body);
            if let Some(key) = &self.key {
                request = request.bearer_auth(key);
            }
            let response = request.send().await.map_err(|e| e.without_url().to_string())?;
            let status = response.status().as_u16();
            let text = response.text().await.map_err(|e| e.without_url().to_string())?;
            Ok((status, text))
        })
    }
}

/// What a decision is asked with: the operation's mode, how many states a selection keeps,
/// and the deterministic verdict a verdict rule joins.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Call {
    pub mode: Mode,
    pub k: Option<usize>,
    pub base: Option<Verdict>,
    pub capability: Option<&'static str>,
    pub timeout: Option<Duration>,
}

impl Call {
    pub fn simple(mode: Mode, k: Option<usize>, base: Option<Verdict>) -> Self {
        Self {
            mode,
            k,
            base,
            capability: None,
            timeout: None,
        }
    }
}

impl Call {
    pub fn simple(mode: Mode, k: Option<usize>, base: Option<Verdict>) -> Self {
        Self {
            mode,
            k,
            base,
            capability: None,
            timeout: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decision {
    /// `name@version`.
    pub program: String,
    /// The program as run, thresholds applied.
    pub hash: String,
    /// The checkpoint that answered.
    pub model: String,
    /// Wall time of the decision, milliseconds.
    pub ms: f64,
    pub results: Vec<Row>,
    /// A selection: the states kept, most probable first.
    pub selected: Option<Vec<usize>>,
}

/// One state's ruling, or why it has none.
#[derive(Debug, Clone, PartialEq)]
pub enum Row {
    Ruled(Ruled),
    Failed { state: String, error: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ruled {
    /// `sha256:` of the state.
    pub state: String,
    /// The mode as run: shadow when the answer came from another calibration.
    pub mode: Mode,
    /// Question -> distribution over its options, `{option: p}`.
    pub answers: IndexMap<String, Value>,
    pub signals: IndexMap<String, Signal>,
    /// Kai's verdict, for a program with a verdict rule.
    pub verdict: Option<Verdict>,
    /// The verdict that applies: the base joined with Kai's when enforced.
    pub effective: Option<Verdict>,
}

/// Why a state has no answer.
enum Fault {
    /// Kai cannot be reached: the decision fails and Kai goes down.
    Down(String),
    /// This state's request was refused or its answer unreadable.
    Row(String),
}

struct Asked {
    answers: IndexMap<String, Vec<f64>>,
    checkpoint: String,
    calibration: Option<String>,
}

#[derive(Default)]
struct Health {
    /// Kai is down until then.
    until: Option<Instant>,
    /// When it last failed: only a decision asked after that says it is back.
    fell: Option<Instant>,
    /// Whether the outage was logged.
    logged: bool,
}

/// Kai, answering from any task.
pub struct Decider {
    url: String,
    model: String,
    transport: Arc<dyn Transport>,
    /// Per program: one capability's outage is not another's.
    health: Mutex<HashMap<String, Health>>,
    capabilities: Mutex<HashMap<String, CapabilityHealth>>,
}

impl Decider {
    /// Kai at `url` (`https://api.hanzo.ai/v1`) as `model`, signed with `key`, over HTTP.
    pub fn new(url: &str, model: &str, key: Option<String>) -> Decider {
        Decider::over(url, model, Arc::new(Http::new(url, key)))
    }

    /// Kai as `model` over `transport`; `url` names it in the log.
    pub fn over(url: &str, model: &str, transport: Arc<dyn Transport>) -> Decider {
        Decider {
            url: url.to_string(),
            model: model.to_string(),
            transport,
            health: Mutex::new(HashMap::new()),
            capabilities: Mutex::new(HashMap::new()),
        }
    }

    fn health(&self) -> std::sync::MutexGuard<'_, HashMap<String, Health>> {
        self.health
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    /// Whether some program of Kai's is down: the agent loop, whose programs fail together, skips
    /// Kai while it is.
    pub fn down(&self) -> bool {
        let now = Instant::now();
        self.health().values().any(|h| h.until.is_some_and(|until| now < until))
    }

    /// Whether `program` is down: a decision of it now fails without a request.
    pub fn down_for(&self, program: &str) -> bool {
        self.health()
            .get(program)
            .and_then(|h| h.until)
            .is_some_and(|until| Instant::now() < until)
    }

<<<<<<< HEAD
    fn fell(&self, program: &str, why: &str) {
        let mut all = self.health();
        let h = all.entry(program.to_string()).or_default();
=======
    /// Whether a specific capability is down. Returns false if Closed or HalfOpen (permits probe).
    pub fn is_capability_down(&self, capability: &str) -> bool {
        let mut caps = self
            .capabilities
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let entry = caps.entry(capability.to_string()).or_default();
        match entry.state {
            BreakerState::Closed => false,
            BreakerState::Open => {
                if let Some(until) = entry.until {
                    if Instant::now() >= until {
                        entry.state = BreakerState::HalfOpen;
                        return false;
                    }
                }
                true
            }
            BreakerState::HalfOpen => false,
        }
    }

    pub fn capability_fell(&self, capability: &str, why: &str) {
        let mut caps = self
            .capabilities
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let entry = caps.entry(capability.to_string()).or_default();
        let now = Instant::now();
        entry.consecutive_failures += 1;
        entry.state = BreakerState::Open;
        entry.until = Some(now + CAPABILITY_COOLDOWN);
        entry.fell = Some(now);
        if !entry.logged {
            entry.logged = true;
            tracing::warn!(
                "kai: capability '{capability}' at {} circuit open ({why}); cooling down for {}s",
                self.url,
                CAPABILITY_COOLDOWN.as_secs()
            );
        }
    }

    pub fn capability_answered(&self, capability: &str, started: Instant) {
        let mut caps = self
            .capabilities
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let entry = caps.entry(capability.to_string()).or_default();
        if entry.fell.is_some_and(|fell| started <= fell) {
            return;
        }
        if entry.logged {
            tracing::info!(
                "kai: capability '{capability}' at {} recovered to closed circuit",
                self.url
            );
        }
        *entry = CapabilityHealth::default();
    }

    fn fell(&self, program: &str, why: &str) {
        let mut all = self.health();
        let h = all.entry(program.to_string()).or_default();
        let now = Instant::now();
        h.until = Some(now + COOLDOWN);
        h.fell = Some(now);
        if !h.logged {
            h.logged = true;
            tracing::warn!(
                "kai: {} does not answer {program} ({why}); deciding without Kai",
                self.url
            );
        }
    }

    /// A decision of `program` asked at `started` was answered. One asked before the last failure
    /// says nothing about Kai now.
    fn answered(&self, program: &str, started: Instant) {
        let mut all = self.health();
        let Some(h) = all.get_mut(program) else {
            return;
        };
        if h.fell.is_some_and(|fell| started <= fell) {
            return;
        }
        if h.logged {
            tracing::info!("kai: {} answers {program} again", self.url);
        }
        *h = Health::default();
    }

    /// One state's answers.
    async fn ask(&self, program: &Program, state: &Value, timeout: Option<Duration>) -> Result<Asked, Fault> {
        let body = json!({
            "model": self.model,
            "state": state,
            "questions": program.questions,
        });
        let send_fut = self.transport.send(&body);
        let send_res = match timeout {
            Some(t) => match tokio::time::timeout(t, send_fut).await {
                Ok(r) => r,
                Err(_) => return Err(Fault::Down("timeout".to_string())),
            },
            None => send_fut.await,
        };
        let (status, text) = send_res.map_err(Fault::Down)?;
        if status == 400 || status == 422 {
            let why: Value = serde_json::from_str(&text).unwrap_or_default();
            let message = why["error"]["message"]
                .as_str()
                .or_else(|| why["error"].as_str())
                .unwrap_or("refused")
                .to_string();
            return Err(Fault::Row(format!("{status}: {message}")));
        }
        if !(200..300).contains(&status) {
            return Err(Fault::Down(status.to_string()));
        }
        let answer: Value = serde_json::from_str(&text)
            .map_err(|e| Fault::Row(format!("an answer that is not JSON: {e}")))?;
        let answers = program
            .questions
            .iter()
            .map(|(id, q)| {
                let a = &answer["answers"][id];
                distribution(q, a)
                    .map(|p| (id.clone(), p))
                    .ok_or_else(|| Fault::Row(format!("no distribution for {id:?}")))
            })
            .collect::<Result<IndexMap<_, _>, _>>()?;
        Ok(Asked {
            answers,
            checkpoint: answer["routing"]["checkpoint"]
                .as_str()
                .or_else(|| answer["model"].as_str())
                .unwrap_or_default()
                .to_string(),
            calibration: answer["routing"]["calibration"]
                .as_str()
                .map(str::to_string),
        })
    }

    /// Kai's ruling on every state. Fails when Kai is down or cannot be reached.
    pub async fn decide(
        &self,
        program: &Program,
        states: Vec<Value>,
        call: Call,
    ) -> Result<Decision, String> {
        if self.down_for(&program.id) {
            return Err("kai is down".into());
        }
        if let Some(cap) = call.capability {
            if self.is_capability_down(cap) {
                return Err(format!("kai: capability '{cap}' circuit open"));
            }
        }
        let started = Instant::now();
        let timeout = call.timeout;
        let asked =
            futures::future::join_all(states.iter().map(|state| self.ask(program, state, timeout))).await;
        let down = asked.iter().find_map(|a| match a {
            Err(Fault::Down(why)) => Some(why.clone()),
            _ => None,
        });
        // No state answered: whatever answered is not Kai (a login page, a model without
        // distributions), and it is treated as Kai not answering.
        let silent =
            (!asked.is_empty() && asked.iter().all(Result::is_err)).then(|| match asked.first() {
                Some(Err(Fault::Row(why))) => why.clone(),
                _ => "no answer".to_string(),
            });
        if let Some(why) = down.or(silent) {
            self.fell(&program.id, &why);
            if let Some(cap) = call.capability {
                self.capability_fell(cap, &why);
            }
            return Err(format!("kai: {why}"));
        }
        if let Some(cap) = call.capability {
            self.capability_answered(cap, started);
        }
        self.answered(&program.id, started);
        let mut model = String::new();
        let results: Vec<Row> = asked
            .into_iter()
            .zip(&states)
            .map(|(asked, state)| match asked {
                Ok(a) => {
                    if model.is_empty() {
                        model.clone_from(&a.checkpoint);
                    }
                    Row::Ruled(rule(program, state, a, call))
                }
                Err(Fault::Row(error) | Fault::Down(error)) => Row::Failed {
                    state: digest(state),
                    error,
                },
            })
            .collect();
        let selected = call.k.map(|k| select(program, &results, k));
        Ok(Decision {
            program: program.id.clone(),
            hash: program.hash(),
            model,
            ms: started.elapsed().as_secs_f64() * 1000.0,
            results,
            selected,
        })
    }
}

/// An answer's distribution over `q`'s options, in option order.
fn distribution(q: &crate::program::Question, answer: &Value) -> Option<Vec<f64>> {
    use crate::program::Kind;
    let keys = q.keys();
    let p: Vec<f64> = match q.kind {
        Kind::Noul => {
            let yes = answer["noul"].as_f64()?;
            vec![1.0 - yes, yes]
        }
        Kind::Choice | Kind::Score => {
            let probabilities = answer["probabilities"].as_object()?;
            keys.iter()
                .map(|k| probabilities.get(k).and_then(Value::as_f64))
                .collect::<Option<Vec<f64>>>()?
        }
    };
    (p.len() == keys.len() && p.iter().all(|v| (0.0..=1.0).contains(v))).then_some(p)
}

/// One state's ruling: its signals, Kai's verdict, and the mode it ran in.
fn rule(program: &Program, state: &Value, asked: Asked, call: Call) -> Ruled {
    let signals = program.signals(&asked.answers);
    let verdict = program.verdict(&signals);
    let calibrated = program
        .calibration
        .as_ref()
        .is_none_or(|c| asked.calibration.as_ref() == Some(c));
    let mode = if calibrated { call.mode } else { Mode::Shadow };
    let effective = call.base.map(|base| match (mode, verdict) {
        (Mode::Enforced, Some(kai)) => base.join(kai),
        _ => base,
    });
    let answers = program
        .questions
        .iter()
        .filter_map(|(id, q)| {
            let p = asked.answers.get(id)?;
            let dist: serde_json::Map<String, Value> = q
                .keys()
                .into_iter()
                .zip(p)
                .map(|(k, p)| (k, json!(p)))
                .collect();
            Some((id.clone(), Value::Object(dist)))
        })
        .collect();
    Ruled {
        state: digest(state),
        mode,
        answers,
        signals,
        verdict,
        effective,
    }
}

/// The `k` states whose first noul holds, most probable first.
fn select(program: &Program, results: &[Row], k: usize) -> Vec<usize> {
    let Some(noul) = program.first_noul() else {
        return Vec::new();
    };
    let mut held: Vec<(usize, f64)> = results
        .iter()
        .enumerate()
        .filter_map(|(i, row)| match row {
            Row::Ruled(r) if r.signals.get(noul)?.holds == Some(true) => {
                Some((i, r.answers[noul]["true"].as_f64().unwrap_or(0.0)))
            }
            _ => None,
        })
        .collect();
    held.sort_by(|a, b| b.1.total_cmp(&a.1));
    held.into_iter().take(k).map(|(i, _)| i).collect()
}

/// A state's ruling, when Kai may act on it: its gate ran enforced.
pub fn enforced(decision: &Decision, index: usize) -> Option<&Ruled> {
    match decision.results.get(index)? {
        Row::Ruled(r) if r.mode == Mode::Enforced => Some(r),
        _ => None,
    }
}

/// Every state ruled under an enforced gate.
pub fn all_enforced(decision: &Decision) -> bool {
    !decision.results.is_empty()
        && (0..decision.results.len()).all(|i| enforced(decision, i).is_some())
}

/// The first state's ruling, whatever its mode.
pub fn first(decision: &Decision) -> Option<&Ruled> {
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

/// `text` cut to at most `n` characters.
pub fn cut(text: &str, n: usize) -> String {
    match text.char_indices().nth(n) {
        Some((at, _)) => text[..at].to_string(),
        None => text.to_string(),
    }
}
