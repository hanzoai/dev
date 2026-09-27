//! Kai answering over the Decisions API: `POST {url}/decisions`, one request per state, the
//! states of a decision in parallel.
//!
//! Kai is optional. When it cannot be reached — no connection, a timeout, a refusal, a server
//! error, or no state of a decision answered — the decision fails at once, Kai is marked down
//! for [`COOLDOWN`], and every decision in that window fails without a request, so the loop runs
//! as it would without Kai. The first failure is logged; nothing else is until a decision asked
//! after it is answered.

use crate::program::Program;
use crate::program::Signal;
use crate::program::digest;
use hanzo_config::kai::Mode;
use hanzo_loop::Verdict;
use indexmap::IndexMap;
use serde_json::Value;
use serde_json::json;
use std::sync::Mutex;
use std::time::Duration;
use std::time::Instant;

/// How long Kai stays down after it fails to answer.
pub const COOLDOWN: Duration = Duration::from_secs(60);
/// The longest a connection may take to open.
const CONNECT: Duration = Duration::from_secs(2);
/// The longest one request may take.
const REQUEST: Duration = Duration::from_secs(10);

/// What a decision is asked with: the operation's mode, how many states a selection keeps,
/// and the deterministic verdict a verdict rule joins.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Call {
    pub mode: Mode,
    pub k: Option<usize>,
    pub base: Option<Verdict>,
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

/// Kai over HTTP, answering from any task.
pub struct Decider {
    url: String,
    model: String,
    key: Option<String>,
    client: reqwest::Client,
    health: Mutex<Health>,
}

impl Decider {
    /// Kai at `url` (`https://api.hanzo.ai/v1`) as `model`, signed with `key`.
    pub fn new(url: &str, model: &str, key: Option<String>) -> Decider {
        Decider {
            url: format!("{}/decisions", url.trim_end_matches('/')),
            model: model.to_string(),
            key,
            client: reqwest::Client::builder()
                .connect_timeout(CONNECT)
                .timeout(REQUEST)
                .build()
                .unwrap_or_default(),
            health: Mutex::new(Health::default()),
        }
    }

    fn health(&self) -> std::sync::MutexGuard<'_, Health> {
        self.health
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    /// Whether Kai is down: a decision now fails without a request.
    pub fn down(&self) -> bool {
        self.health()
            .until
            .is_some_and(|until| Instant::now() < until)
    }

    fn fell(&self, why: &str) {
        let mut h = self.health();
        let now = Instant::now();
        h.until = Some(now + COOLDOWN);
        h.fell = Some(now);
        if !h.logged {
            h.logged = true;
            tracing::warn!(
                "kai: {} does not answer ({why}); deciding without Kai",
                self.url
            );
        }
    }

    /// A decision asked at `started` was answered. One asked before the last failure says
    /// nothing about Kai now.
    fn answered(&self, started: Instant) {
        let mut h = self.health();
        if h.fell.is_some_and(|fell| started <= fell) {
            return;
        }
        if h.logged {
            tracing::info!("kai: {} answers again", self.url);
        }
        *h = Health::default();
    }

    /// One state's answers.
    async fn ask(&self, program: &Program, state: &Value) -> Result<Asked, Fault> {
        let body = json!({
            "model": self.model,
            "state": state,
            "questions": program.questions,
        });
        let mut request = self.client.post(&self.url).json(&body);
        if let Some(key) = &self.key {
            request = request.bearer_auth(key);
        }
        let response = request
            .send()
            .await
            .map_err(|e| Fault::Down(e.without_url().to_string()))?;
        let status = response.status();
        let text = response
            .text()
            .await
            .map_err(|e| Fault::Down(e.without_url().to_string()))?;
        if status == reqwest::StatusCode::BAD_REQUEST
            || status == reqwest::StatusCode::UNPROCESSABLE_ENTITY
        {
            let why: Value = serde_json::from_str(&text).unwrap_or_default();
            let message = why["error"]["message"]
                .as_str()
                .or_else(|| why["error"].as_str())
                .unwrap_or("refused")
                .to_string();
            return Err(Fault::Row(format!("{status}: {message}")));
        }
        if !status.is_success() {
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
        if self.down() {
            return Err("kai is down".into());
        }
        let started = Instant::now();
        let asked =
            futures::future::join_all(states.iter().map(|state| self.ask(program, state))).await;
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
            self.fell(&why);
            return Err(format!("kai: {why}"));
        }
        self.answered(started);
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
