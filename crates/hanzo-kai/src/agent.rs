//! Kai in the agent loop: one controller per thread.
//!
//! Each turn Kai decides which MCP tools the model sees (`tools`), whether each command runs
//! (`risk`, joined with the approval policy), which model tier and reasoning budget to hint
//! to Enso (`model`, `reasoning`), which files the turn starts from (`context`), whether the
//! agent is advancing (`progress`) and whether the request is done (`complete`). An
//! operation's mode sets what its decision may do: `shadow` records it, `advisory` also
//! shows it, `enforced` applies it, and then only toward the stricter outcome. A decision
//! that is not enforced runs off the loop's path; an enforced one is waited for, at most
//! [`WAIT`], and the loop goes on as without Kai when it is late or fails. While Kai is down
//! no decision is asked at all.
//!
//! Every decision is a line in the trace, written once what followed it is known: the turn's
//! end for tools, routing, context and progress, the command's end for risk, the decision
//! itself for completion. A decision Kai did not answer writes no line.

use crate::Kai;
use crate::decide;
use crate::decide::Call;
use crate::decide::Decision;
use crate::decide::cut;
use crate::tools;
use crate::trace::Line;
use crate::trace::Outcome;
use crate::trace::Record;
use crate::trace::Step;
use hanzo_config::kai::Mode;
use hanzo_config::kai::Op;
use hanzo_config::kai::Operation;
use hanzo_loop::Answer;
use hanzo_loop::Card;
use hanzo_loop::Controller;
use hanzo_loop::Request;
use hanzo_loop::Verdict;
use serde_json::Value;
use serde_json::json;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;

/// The longest the loop waits for an enforced decision: past the client's own timeout, so a
/// request that times out marks Kai down rather than being dropped.
pub const WAIT: Duration = Duration::from_secs(12);
/// Candidate files a context decision reads.
const CANDIDATES: usize = 16;
/// Continuations Kai may start for one request before it hands the request back.
const CONTINUATIONS: u32 = 2;
/// Steps before progress is judged.
const STEPS: usize = 3;

/// What Kai does to a thread beyond answering the loop.
pub trait Act: Send + Sync {
    /// Shows `message` to the user.
    fn warn(&self, turn: &str, message: String);
    /// Adds `text` to the running turn's input.
    fn steer(&self, text: String) -> Answer<'_, ()>;
    /// Starts a turn with `text` when the thread is idle; whether one started.
    fn resume(&self, text: String) -> Answer<'_, bool>;
}

#[derive(Default)]
struct State {
    turn: String,
    request: Request,
    turns: u64,
    /// The tools last offered.
    names: Vec<String>,
    /// This turn's shortlist, once decided, and the tools it was decided over.
    shortlist: Option<(String, Vec<String>, Option<HashSet<String>>)>,
    /// Tools called this turn.
    called: Vec<String>,
    /// This request's steps: `{tool, result}`.
    steps: Vec<Value>,
    steered: bool,
    /// The last agent message.
    answer: String,
    continued: u32,
    /// Lines that close when the turn ends.
    open: Vec<Arc<Record>>,
    /// Risk lines that close when their command ends, by call id.
    calls: HashMap<String, Arc<Record>>,
    /// Token totals at the turn's start and now: input, cached, output, reasoning.
    start: [i64; 4],
    total: [i64; 4],
    /// The last request's input tokens.
    prompt: i64,
}

/// Kai's controller for one thread.
pub struct Thread {
    kai: Arc<Kai>,
    sku: String,
    act: Arc<dyn Act>,
    state: Mutex<State>,
}

impl Thread {
    pub fn new(kai: Arc<Kai>, sku: String, act: Arc<dyn Act>) -> Thread {
        Thread {
            kai,
            sku,
            act,
            state: Mutex::new(State::default()),
        }
    }

    fn state(&self) -> std::sync::MutexGuard<'_, State> {
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    fn op(&self, op: Operation) -> Op {
        self.kai.settings.op(op).clone()
    }

    fn request(&self) -> String {
        self.state().request.text.clone()
    }

    /// Whether Kai is down: its operations are skipped.
    fn down(&self) -> bool {
        self.kai.decider.down()
    }

    /// A line for `op` in `turn`, not yet decided or closed.
    fn record(&self, turn: &str, name: &str, op: &Op) -> Arc<Record> {
        let line = Line {
            time: crate::trace::now(),
            request: turn.to_string(),
            family: "dev".into(),
            sku: self.sku.clone(),
            ..Line::default()
        };
        Record::new(
            Arc::clone(&self.kai.trace),
            line,
            name,
            Step::new(&op.program, op.mode.name()),
        )
    }

    /// Opens a line that closes with the turn.
    fn open(&self, turn: &str, name: &str, op: &Op) -> Arc<Record> {
        let record = self.record(turn, name, op);
        self.state().open.push(Arc::clone(&record));
        record
    }

    /// Asks Kai `kind`'s program over `states`: waited for when enforced, else off the
    /// loop's path. `then` reads the decision into the line and says what to act on; its
    /// answer is returned only when enforced. A decision Kai does not answer voids the line
    /// and acts on nothing.
    async fn decide<T: Send + 'static>(
        self: &Arc<Self>,
        kind: Operation,
        record: Arc<Record>,
        states: Vec<Value>,
        base: Option<Verdict>,
        then: impl FnOnce(&Arc<Thread>, &Decision, &mut Step, &mut Outcome) -> Option<T>
        + Send
        + 'static,
    ) -> Option<T> {
        let op = self.op(kind);
        let program = self.kai.program(kind);
        let decider = Arc::clone(&self.kai.decider);
        let call = Call {
            mode: op.mode,
            k: op.k,
            base,
        };
        if op.mode == Mode::Enforced {
            let decision = tokio::time::timeout(WAIT, decider.decide(&program, states, call))
                .await
                .unwrap_or_else(|_| Err("late".into()));
            let Ok(decision) = decision else {
                record.void();
                return None;
            };
            let mut out = None;
            record.decided(|step, outcome| {
                step.fill(&decision);
                out = then(self, &decision, step, outcome);
            });
            return out;
        }
        let me = Arc::clone(self);
        tokio::spawn(async move {
            let Ok(decision) = decider.decide(&program, states, call).await else {
                record.void();
                return;
            };
            record.decided(|step, outcome| {
                step.fill(&decision);
                let _ = then(&me, &decision, step, outcome);
                step.applied = false;
            });
        });
        None
    }

    /// Advises the user, when `op` is advisory.
    fn advise(&self, op: &Op, turn: &str, message: impl FnOnce() -> Option<String>) {
        if op.mode == Mode::Advisory
            && let Some(message) = message()
        {
            self.act.warn(turn, format!("Kai (advisory): {message}"));
        }
    }

    /// The tools `turn` keeps in view, of `cards`.
    /// Decided once a turn, and again when the tools on offer change: MCP servers that finish
    /// starting mid-turn add theirs.
    async fn shortlist(self: &Arc<Self>, turn: &str, cards: Vec<Card>) -> Option<HashSet<String>> {
        let names: Vec<String> = cards.iter().map(|c| c.name.clone()).collect();
        {
            let state = self.state();
            if let Some((t, offered, keep)) = &state.shortlist
                && t == turn
                && *offered == names
            {
                return keep.clone();
            }
        }
        let keep = if cards.is_empty() || self.down() {
            None
        } else {
            let op = self.op(Operation::Tools);
            let request = self.request();
            let states = cards
                .iter()
                .map(|c| {
                    json!({
                        "request": cut(&request, 2000),
                        "tool": {"name": c.name, "description": cut(&c.description, 500)},
                    })
                })
                .collect();
            let record = self.open(turn, "tools", &op);
            let (turn_id, all) = (turn.to_string(), names.clone());
            let advisory = op.clone();
            self.decide(
                Operation::Tools,
                record,
                states,
                None,
                move |me, d, step, _| {
                    let chosen: Vec<String> = d
                        .selected
                        .iter()
                        .flatten()
                        .filter_map(|&i| all.get(i).cloned())
                        .collect();
                    step.kai = chosen.join(",");
                    let keep = (advisory.mode == Mode::Enforced && decide::all_enforced(d))
                        .then(|| chosen.iter().cloned().collect::<HashSet<String>>());
                    step.taken = match &keep {
                        Some(_) => step.kai.clone(),
                        None => "all".into(),
                    };
                    step.applied = keep.is_some();
                    me.advise(&advisory, &turn_id, || {
                        Some(format!(
                            "would show {} of {} tools: {}",
                            chosen.len(),
                            all.len(),
                            chosen.join(", ")
                        ))
                    });
                    keep
                },
            )
            .await
        };
        let mut state = self.state();
        state.names = names.clone();
        state.shortlist = Some((turn.to_string(), names, keep.clone()));
        keep
    }

    /// The command's verdict: the policy's joined with Kai's when enforced.
    async fn risk(
        self: &Arc<Self>,
        turn: &str,
        call_id: &str,
        policy: Verdict,
        action: Value,
    ) -> Verdict {
        if self.down() {
            return policy;
        }
        let op = self.op(Operation::Risk);
        let command = command_state(&action);
        let shown = command["command"]
            .as_str()
            .unwrap_or("this command")
            .to_string();
        let record = self.record(turn, "risk", &op);
        self.state()
            .calls
            .insert(call_id.to_string(), Arc::clone(&record));
        let (turn_id, advisory) = (turn.to_string(), op.clone());
        let decided = self
            .decide(
                Operation::Risk,
                record,
                vec![command],
                Some(policy),
                move |me, d, step, _| {
                    let kai = decide::first(d).and_then(|r| r.verdict);
                    step.kai = kai.map(Verdict::name).unwrap_or_default().into();
                    let taken = decide::enforced(d, 0)
                        .and_then(|r| r.effective)
                        .map_or(policy, |v| v.join(policy));
                    step.taken = taken.name().into();
                    step.applied = taken != policy;
                    me.advise(&advisory, &turn_id, || {
                        let kai = kai?;
                        (kai > policy).then(|| match kai {
                            Verdict::Deny => format!("would refuse `{}`", cut(&shown, 120)),
                            _ => format!("would ask before `{}`", cut(&shown, 120)),
                        })
                    });
                    Some(taken)
                },
            )
            .await;
        decided.unwrap_or(policy).join(policy)
    }

    /// Routing hints for the turn: Kai's model tier and reasoning budget, when enforced.
    async fn route(self: &Arc<Self>, turn: &str, request: Request) -> BTreeMap<String, String> {
        let down = self.down();
        let state = {
            let mut s = self.state();
            s.turn = turn.to_string();
            s.turns += 1;
            s.called.clear();
            s.steered = false;
            s.shortlist = None;
            if !request.text.is_empty() {
                // A new request from the user; a turn Kai started carries none.
                s.request = request;
                s.steps.clear();
                s.continued = 0;
            }
            json!({
                "request": cut(&s.request.text, 4000),
                "system": "",
                "turns": s.turns,
                "tools": s.names,
                "prompt_tokens": s.prompt,
                "images": s.request.images,
            })
        };
        if down {
            return BTreeMap::new();
        }
        let mut asks = Vec::new();
        for (kind, question) in [(Operation::Model, "tier"), (Operation::Reasoning, "budget")] {
            let op = self.op(kind);
            let record = self.open(turn, kind.name(), &op);
            let states = vec![state.clone()];
            let (turn_id, advisory) = (turn.to_string(), op.clone());
            let me = Arc::clone(self);
            asks.push(async move {
                me.decide(kind, record, states, None, move |me, d, step, _| {
                    let answer = decide::first(d)
                        .and_then(|r| r.signals.get(question))
                        .map(|s| s.answer.clone());
                    step.kai = answer.clone().unwrap_or_default();
                    let hint = decide::enforced(d, 0)
                        .and_then(|r| r.signals.get(question))
                        .and_then(decide::accepted)
                        .map(|a| hint_value(question, a));
                    step.taken = hint.clone().unwrap_or_default();
                    step.applied = hint.is_some();
                    me.advise(&advisory, &turn_id, || {
                        Some(format!("{question} {}", hint_value(question, &answer?)))
                    });
                    hint.map(|h| (format!("kai.{}", hint_key(question)), h))
                })
                .await
            });
        }
        // Tier and budget are asked together.
        futures::future::join_all(asks)
            .await
            .into_iter()
            .flatten()
            .collect()
    }

    /// Files the turn should start from, for a request made in `cwd`.
    async fn context(self: &Arc<Self>, turn: &str, text: String, cwd: PathBuf) -> Option<String> {
        if text.trim().is_empty() || self.down() {
            return None;
        }
        let op = self.op(Operation::Context);
        let record = self.open(turn, "context", &op);
        if op.mode == Mode::Enforced {
            return self.select(turn.to_string(), record, text, cwd).await;
        }
        // Nothing can act on the answer: the search runs off the turn's path too.
        let (me, turn) = (Arc::clone(self), turn.to_string());
        tokio::spawn(async move { me.select(turn, record, text, cwd).await });
        None
    }

    /// Retrieves candidate files for `text` and asks Kai which matter.
    async fn select(
        self: &Arc<Self>,
        turn: String,
        record: Arc<Record>,
        text: String,
        cwd: PathBuf,
    ) -> Option<String> {
        let query = text.clone();
        let files = tokio::task::spawn_blocking(move || {
            tools::search(&cwd, &query, CANDIDATES, 2000)
                .into_iter()
                .filter_map(|hit| {
                    let head = tools::head(&cwd, &hit.path, 2000)?;
                    Some((hit.path, head))
                })
                .collect::<Vec<_>>()
        })
        .await
        .unwrap_or_default();
        if files.is_empty() {
            record.void();
            return None;
        }
        let op = self.op(Operation::Context);
        let states = files
            .iter()
            .map(|(path, head)| {
                json!({"request": cut(&text, 2000), "chunk": cut(head, 2000), "source": path})
            })
            .collect();
        let paths: Vec<String> = files.into_iter().map(|(p, _)| p).collect();
        let advisory = op.clone();
        let turn_id = turn;
        self.decide(
            Operation::Context,
            record,
            states,
            None,
            move |me, d, step, _| {
                let chosen: Vec<String> = d
                    .selected
                    .iter()
                    .flatten()
                    .filter_map(|&i| paths.get(i).cloned())
                    .collect();
                step.kai = chosen.join(",");
                me.advise(&advisory, &turn_id, || {
                    (!chosen.is_empty()).then(|| format!("would start from {}", chosen.join(", ")))
                });
                let apply = advisory.mode == Mode::Enforced
                    && decide::all_enforced(d)
                    && !chosen.is_empty();
                step.applied = apply;
                if !apply {
                    return None;
                }
                step.taken = step.kai.clone();
                let list: Vec<String> = chosen.iter().map(|p| format!("- {p}")).collect();
                Some(format!(
                    "Kai: the files most likely to matter for this request; read them first.\n{}",
                    list.join("\n")
                ))
            },
        )
        .await
    }

    /// A tool call ended: close its risk line and judge progress.
    async fn finished(self: &Arc<Self>, turn: &str, call_id: &str, tool: String, result: &str) {
        let (risk, steps) = {
            let mut s = self.state();
            s.called.push(tool.clone());
            s.steps.push(json!({"tool": tool, "result": result}));
            let len = s.steps.len();
            let steps: Vec<Value> = s.steps[len.saturating_sub(8)..].to_vec();
            (s.calls.remove(call_id), (len >= STEPS).then_some(steps))
        };
        if let Some(record) = risk {
            record.closed(|line| {
                line.outcome.finish = result.to_string();
                line.outcome.result = json!({"tool": tool});
            });
        }
        let Some(steps) = steps else {
            return;
        };
        if self.down() {
            return;
        }
        let op = self.op(Operation::Progress);
        let state = json!({"request": cut(&self.request(), 2000), "steps": steps});
        let record = self.open(turn, "progress", &op);
        let (turn_id, advisory) = (turn.to_string(), op.clone());
        let steer = self
            .decide(Operation::Progress, record, vec![state], None, move |me, d, step, _| {
                let first = decide::first(d)?;
                let status = first.signals.get("status").map(|s| s.answer.clone());
                let asks = first.signals.get("needs_user").and_then(|s| s.holds) == Some(true);
                step.kai = status.clone().unwrap_or_default();
                let row = decide::enforced(d, 0);
                let stuck = row
                    .and_then(|r| r.signals.get("status"))
                    .and_then(decide::accepted)
                    .filter(|s| matches!(*s, "stalled" | "looping" | "blocked"))
                    .map(str::to_string);
                let ask = row.and_then(|r| r.signals.get("needs_user")).and_then(|s| s.holds) == Some(true);
                me.advise(&advisory, &turn_id, || {
                    let status = status?;
                    (status != "advancing" || asks).then(|| format!("the agent looks {status}"))
                });
                let text = match (stuck, ask) {
                    (_, true) => "Kai: stop here and ask the user how to proceed; say what you tried and what blocks you.".to_string(),
                    (Some(s), false) => format!("Kai: the last steps look {s}. Change approach instead of repeating it, or ask the user."),
                    (None, false) => return None,
                };
                step.taken = if ask { "ask".into() } else { "steer".into() };
                step.applied = true;
                Some(text)
            })
            .await;
        if let Some(text) = steer {
            let first = {
                let mut s = self.state();
                !std::mem::replace(&mut s.steered, true)
            };
            if first {
                self.act.steer(text).await;
            }
        }
    }

    /// The thread went idle after a turn completed: is the request done?
    async fn idle(self: &Arc<Self>) {
        let (turn, request, answer, continued) = {
            let s = self.state();
            (
                s.turn.clone(),
                s.request.text.clone(),
                s.answer.clone(),
                s.continued,
            )
        };
        if request.trim().is_empty() || self.down() {
            return;
        }
        let op = self.op(Operation::Complete);
        let state = json!({"request": cut(&request, 2000), "answer": cut(&answer, 3000)});
        let record = self.record(&turn, "complete", &op);
        // Its own outcome: what Kai started, or handed back.
        record.closed(|line| {
            line.outcome.finish = "idle".into();
            line.outcome.result = json!({"continued": continued});
        });
        let (turn_id, advisory) = (turn.clone(), op.clone());
        let next = self
            .decide(
                Operation::Complete,
                record,
                vec![state],
                None,
                move |me, d, step, outcome| {
                    let done = decide::first(d)
                        .and_then(|r| r.signals.get("done"))
                        .and_then(|s| s.holds);
                    step.kai = match done {
                        Some(true) => "done".into(),
                        Some(false) => "retry".into(),
                        None => String::new(),
                    };
                    me.advise(&advisory, &turn_id, || {
                        (done == Some(false)).then(|| "this does not look finished".to_string())
                    });
                    let not_done = decide::enforced(d, 0)
                        .and_then(|r| r.signals.get("done"))
                        .and_then(|s| s.holds)
                        == Some(false);
                    let taken = match (not_done, continued < CONTINUATIONS) {
                        (false, _) => "done",
                        (true, true) => "retry",
                        (true, false) => "escalate",
                    };
                    step.taken = taken.into();
                    step.applied = not_done;
                    outcome.finish = taken.into();
                    outcome.escalated = taken == "escalate";
                    not_done.then_some(taken)
                },
            )
            .await;
        match next {
            Some("retry") => {
                self.state().continued += 1;
                self.act
                    .resume(
                        "Kai: the request is not finished. Continue until it is done, and show \
                         it is: run the tests or the commands that prove it. If something blocks \
                         you, say what."
                            .to_string(),
                    )
                    .await;
            }
            Some(_) => self.act.warn(
                &turn,
                "Kai: this request does not look finished after two more tries; over to you."
                    .to_string(),
            ),
            None => {}
        }
    }

    fn turn_started(&self, turn: &str, start: [i64; 4]) {
        let mut s = self.state();
        s.turn = turn.to_string();
        s.start = start;
        s.total = start;
    }

    fn usage(&self, total: [i64; 4], prompt: i64) {
        let mut s = self.state();
        s.total = total;
        s.prompt = prompt;
    }

    fn answered(&self, text: String) {
        if !text.is_empty() {
            self.state().answer = text;
        }
    }

    /// The turn ended: close every line still open with its outcome.
    fn turn_ended(&self, finish: &str, error: String) {
        let (open, calls, called, used) = {
            let mut s = self.state();
            let used: Vec<i64> = s.total.iter().zip(s.start).map(|(t, b)| t - b).collect();
            let open = std::mem::take(&mut s.open);
            let calls: Vec<Arc<Record>> = s.calls.drain().map(|(_, r)| r).collect();
            (open, calls, s.called.clone(), used)
        };
        for record in open.iter().chain(calls.iter()) {
            record.closed(|line| {
                line.prompt_tokens = used[0];
                line.cached_tokens = used[1];
                line.completion_tokens = used[2];
                line.reasoning_tokens = used[3];
                line.outcome.finish = finish.to_string();
                line.outcome.error = error.clone();
                if line.outcome.result.is_null() {
                    line.outcome.result = json!({"called": called});
                }
            });
        }
    }
}

fn hint_key(question: &str) -> &'static str {
    match question {
        "tier" => "tier",
        _ => "effort",
    }
}

/// Budget levels as reasoning efforts: none, low, medium, high.
fn hint_value(question: &str, answer: &str) -> String {
    if question != "budget" {
        return answer.to_string();
    }
    match answer {
        "0" => "none",
        "1" => "low",
        "2" => "medium",
        "3" => "high",
        other => other,
    }
    .to_string()
}

/// A command as Kai reads it: the tool, the command line, where it runs, why.
pub fn command_state(action: &Value) -> Value {
    let mut state = serde_json::Map::new();
    if let Some(tool) = action.get("tool") {
        state.insert("tool".into(), tool.clone());
    }
    if let Some(argv) = action.get("command").and_then(Value::as_array) {
        let argv: Vec<&str> = argv.iter().filter_map(Value::as_str).collect();
        let line = match argv.as_slice() {
            [_, flag, script] if matches!(*flag, "-c" | "-lc") => (*script).to_string(),
            _ => argv.join(" "),
        };
        state.insert("command".into(), Value::String(cut(&line, 2000)));
    }
    for key in ["cwd", "justification", "server", "tool_name", "files"] {
        if let Some(v) = action.get(key) {
            state.insert(key.into(), v.clone());
        }
    }
    if let Some(patch) = action.get("patch").and_then(Value::as_str) {
        state.insert("patch".into(), Value::String(cut(patch, 2000)));
    }
    if let Some(args) = action.get("arguments") {
        state.insert(
            "arguments".into(),
            Value::String(cut(&args.to_string(), 2000)),
        );
    }
    Value::Object(state)
}

/// The loop's handle on a thread's controller.
pub struct Shared(pub Arc<Thread>);

impl Controller for Shared {
    fn tools<'a>(&'a self, turn: &'a str, cards: Vec<Card>) -> Answer<'a, Option<HashSet<String>>> {
        Box::pin(self.0.shortlist(turn, cards))
    }

    fn command<'a>(
        &'a self,
        turn: &'a str,
        call: &'a str,
        policy: Verdict,
        action: Value,
    ) -> Answer<'a, Verdict> {
        Box::pin(self.0.risk(turn, call, policy, action))
    }

    fn route<'a>(
        &'a self,
        turn: &'a str,
        request: Request,
    ) -> Answer<'a, BTreeMap<String, String>> {
        Box::pin(self.0.route(turn, request))
    }
}

/// The contributors that feed the controller what the loop does.
pub mod extension {
    use super::*;
    use codex_core::ThreadManager;
    use codex_core::TurnInput;
    use codex_core::TurnInputRequest;
    use codex_core::TurnStartOptions;
    use codex_core::config::Config;
    use codex_core::context::ContextualUserFragment;
    use codex_core::context::InternalContextSource;
    use codex_core::context::InternalModelContextFragment;
    use codex_extension_api::ExtensionData;
    use codex_extension_api::ExtensionEventSink;
    use codex_extension_api::ExtensionFuture;
    use codex_extension_api::ExtensionMetrics;
    use codex_extension_api::ExtensionRegistryBuilder;
    use codex_extension_api::ExtensionWarning;
    use codex_extension_api::ThreadIdleCause;
    use codex_extension_api::ThreadIdleInput;
    use codex_extension_api::ThreadLifecycleContributor;
    use codex_extension_api::ThreadStartInput;
    use codex_extension_api::TokenUsageContributor;
    use codex_extension_api::ToolCallOutcome;
    use codex_extension_api::ToolFinishInput;
    use codex_extension_api::ToolLifecycleContributor;
    use codex_extension_api::ToolLifecycleFuture;
    use codex_extension_api::TurnAbortInput;
    use codex_extension_api::TurnErrorInput;
    use codex_extension_api::TurnInputContext;
    use codex_extension_api::TurnInputContributor;
    use codex_extension_api::TurnItemContributor;
    use codex_extension_api::TurnLifecycleContributor;
    use codex_extension_api::TurnStartInput;
    use codex_extension_api::TurnStopInput;
    use codex_protocol::ThreadId;
    use codex_protocol::items::AgentMessageContent;
    use codex_protocol::items::TurnItem;
    use codex_protocol::models::ResponseItem;
    use codex_protocol::protocol::SessionSource;
    use codex_protocol::protocol::TokenUsage;
    use codex_protocol::protocol::TokenUsageInfo;
    use std::sync::Weak;

    /// Installs Kai's contributors. Kai runs for a thread whose profile has a `kai.toml`, and
    /// not for subagents or internal sessions.
    pub fn install(builder: &mut ExtensionRegistryBuilder<Config>, manager: Weak<ThreadManager>) {
        let agent = Arc::new(Agent {
            events: builder.event_sink(),
            manager,
            homes: Mutex::new(HashMap::new()),
        });
        builder.thread_lifecycle_contributor(agent.clone());
        builder.turn_lifecycle_contributor(agent.clone());
        builder.turn_input_contributor(agent.clone());
        builder.tool_lifecycle_contributor(agent.clone());
        builder.token_usage_contributor(agent.clone());
        builder.turn_item_contributor(agent);
    }

    struct Agent {
        events: Arc<dyn ExtensionEventSink>,
        manager: Weak<ThreadManager>,
        /// Kai per profile home, opened once: every thread shares its client and trace.
        homes: Mutex<HashMap<PathBuf, Option<Arc<Kai>>>>,
    }

    impl Agent {
        fn kai(&self, home: PathBuf) -> Option<Arc<Kai>> {
            let mut homes = self
                .homes
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            homes
                .entry(home)
                .or_insert_with_key(|home| {
                    let settings = match hanzo_config::kai::Kai::load(home) {
                        Ok(settings) => settings?,
                        Err(e) => {
                            tracing::warn!("kai: {e}");
                            return None;
                        }
                    };
                    match Kai::open(settings, hanzo_config::hanzo_credential()) {
                        Ok(kai) => Some(Arc::new(kai)),
                        Err(e) => {
                            tracing::warn!("kai: {e}");
                            None
                        }
                    }
                })
                .clone()
        }
    }

    fn thread(store: &ExtensionData) -> Option<Arc<Thread>> {
        store.get::<Shared>().map(|shared| Arc::clone(&shared.0))
    }

    fn totals(usage: &TokenUsage) -> [i64; 4] {
        [
            usage.input_tokens,
            usage.cached_input_tokens,
            usage.output_tokens,
            usage.reasoning_output_tokens,
        ]
    }

    /// Kai acting on a live thread through the thread manager.
    struct Host {
        thread: ThreadId,
        events: Arc<dyn ExtensionEventSink>,
        manager: Weak<ThreadManager>,
    }

    fn item(text: String) -> ResponseItem {
        ContextualUserFragment::into(InternalModelContextFragment::new(
            InternalContextSource::from_static("kai"),
            text,
        ))
    }

    impl Act for Host {
        fn warn(&self, turn: &str, message: String) {
            self.events.emit_warning(ExtensionWarning {
                thread_id: self.thread.to_string(),
                turn_id: Some(turn.to_string()),
                message,
            });
        }

        fn steer(&self, text: String) -> Answer<'_, ()> {
            Box::pin(async move {
                let Some(manager) = self.manager.upgrade() else {
                    return;
                };
                if let Ok(thread) = manager.get_thread(self.thread).await {
                    let _ = thread.inject_if_running(vec![item(text)]).await;
                }
            })
        }

        fn resume(&self, text: String) -> Answer<'_, bool> {
            Box::pin(async move {
                let Some(manager) = self.manager.upgrade() else {
                    return false;
                };
                let Ok(thread) = manager.get_thread(self.thread).await else {
                    return false;
                };
                let request = TurnInputRequest::new(TurnInput::ResponseItem(item(text))).on_start(
                    TurnStartOptions {
                        turn_trigger: Some("kai".to_string()),
                        ..TurnStartOptions::default()
                    },
                );
                matches!(
                    thread.start_turn_if_idle(request).await,
                    Ok(codex_core::StartIfIdleSubmission::Started { .. })
                )
            })
        }
    }

    impl ThreadLifecycleContributor<Config> for Agent {
        fn on_thread_start<'a>(
            &'a self,
            input: ThreadStartInput<'a, Config>,
        ) -> ExtensionFuture<'a, ()> {
            Box::pin(async move {
                if matches!(
                    input.session_source,
                    SessionSource::SubAgent(_) | SessionSource::Internal(_)
                ) {
                    return;
                }
                let Some(kai) = self.kai(input.config.codex_home.to_path_buf()) else {
                    return;
                };
                let Ok(id) = ThreadId::from_string(input.thread_store.level_id()) else {
                    return;
                };
                let act = Arc::new(Host {
                    thread: id,
                    events: Arc::clone(&self.events),
                    manager: self.manager.clone(),
                });
                let sku = input.config.model.clone().unwrap_or_default();
                let thread = Arc::new(Thread::new(kai, sku, act));
                input
                    .thread_store
                    .insert(hanzo_loop::Handle(Arc::new(Shared(Arc::clone(&thread)))));
                input.thread_store.insert(Shared(thread));
            })
        }

        fn on_thread_idle<'a>(&'a self, input: ThreadIdleInput<'a>) -> ExtensionFuture<'a, ()> {
            Box::pin(async move {
                if input.cause == ThreadIdleCause::Completed
                    && let Some(thread) = thread(input.thread_store)
                {
                    thread.idle().await;
                }
            })
        }
    }

    impl TurnLifecycleContributor for Agent {
        fn on_turn_start<'a>(&'a self, input: TurnStartInput<'a>) -> ExtensionFuture<'a, ()> {
            Box::pin(async move {
                if let Some(thread) = thread(input.thread_store) {
                    let start = input.token_usage_at_turn_start.map(totals).unwrap_or_default();
                    thread.turn_started(input.turn_id, start);
                }
            })
        }

        fn on_turn_stop<'a>(&'a self, input: TurnStopInput<'a>) -> ExtensionFuture<'a, ()> {
            Box::pin(async move {
                if let Some(thread) = thread(input.thread_store) {
                    thread.turn_ended("completed", String::new());
                }
            })
        }

        fn on_turn_abort<'a>(&'a self, input: TurnAbortInput<'a>) -> ExtensionFuture<'a, ()> {
            Box::pin(async move {
                if let Some(thread) = thread(input.thread_store) {
                    thread.turn_ended("aborted", format!("{:?}", input.reason));
                }
            })
        }

        fn on_turn_error<'a>(&'a self, input: TurnErrorInput<'a>) -> ExtensionFuture<'a, ()> {
            Box::pin(async move {
                if let Some(thread) = thread(input.thread_store) {
                    thread.turn_ended("error", format!("{:?}", input.error));
                }
            })
        }
    }

    impl TurnInputContributor for Agent {
        fn contribute<'a>(
            &'a self,
            input: TurnInputContext<'a>,
            _metrics: Option<Arc<dyn ExtensionMetrics>>,
            _session_store: &'a ExtensionData,
            thread_store: &'a ExtensionData,
            _turn_store: &'a ExtensionData,
        ) -> ExtensionFuture<'a, Vec<Box<dyn ContextualUserFragment + Send>>> {
            Box::pin(async move {
                let Some(thread) = thread(thread_store) else {
                    return Vec::new();
                };
                let text: Vec<&str> = input
                    .user_input
                    .iter()
                    .filter_map(|item| match item {
                        codex_protocol::user_input::UserInput::Text { text, .. } => {
                            Some(text.as_str())
                        }
                        _ => None,
                    })
                    .collect();
                let Some(cwd) = input
                    .environments
                    .iter()
                    .find(|e| e.is_primary)
                    .map(|e| e.cwd.to_path_buf())
                else {
                    return Vec::new();
                };
                match thread.context(&input.turn_id, text.join("\n"), cwd).await {
                    Some(body) => vec![Box::new(InternalModelContextFragment::new(
                        InternalContextSource::from_static("kai"),
                        body,
                    ))
                        as Box<dyn ContextualUserFragment + Send>],
                    None => Vec::new(),
                }
            })
        }
    }

    impl ToolLifecycleContributor for Agent {
        fn on_tool_finish<'a>(&'a self, input: ToolFinishInput<'a>) -> ToolLifecycleFuture<'a> {
            Box::pin(async move {
                let Some(thread) = thread(input.thread_store) else {
                    return;
                };
                let result = match input.outcome {
                    ToolCallOutcome::Completed { success: true } => "ok",
                    ToolCallOutcome::Completed { success: false } => "failed",
                    ToolCallOutcome::Blocked => "blocked",
                    ToolCallOutcome::Failed { .. } => "error",
                    ToolCallOutcome::Aborted => "aborted",
                };
                thread
                    .finished(
                        input.turn_id,
                        input.call_id,
                        input.tool_name.to_string(),
                        result,
                    )
                    .await;
            })
        }
    }

    impl TokenUsageContributor for Agent {
        fn on_token_usage<'a>(
            &'a self,
            _session_store: &'a ExtensionData,
            thread_store: &'a ExtensionData,
            _turn_store: &'a ExtensionData,
            usage: &'a TokenUsageInfo,
        ) -> ExtensionFuture<'a, ()> {
            Box::pin(async move {
                if let Some(thread) = thread(thread_store) {
                    thread.usage(
                        totals(&usage.total_token_usage),
                        usage.last_token_usage.input_tokens,
                    );
                }
            })
        }
    }

    impl TurnItemContributor for Agent {
        fn contribute<'a>(
            &'a self,
            thread_store: &'a ExtensionData,
            _turn_store: &'a ExtensionData,
            item: &'a mut TurnItem,
        ) -> ExtensionFuture<'a, Result<(), String>> {
            Box::pin(async move {
                if let (Some(thread), TurnItem::AgentMessage(message)) =
                    (thread(thread_store), &*item)
                {
                    let text: Vec<&str> = message
                        .content
                        .iter()
                        .map(|c| match c {
                            AgentMessageContent::Text { text } => text.as_str(),
                        })
                        .collect();
                    thread.answered(text.join("\n"));
                }
                Ok(())
            })
        }
    }
}

#[cfg(test)]
#[path = "agent_tests.rs"]
mod tests;
