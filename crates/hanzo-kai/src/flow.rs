//! Flows: a pipeline over a repository, action after action.
//!
//! `fix` makes a failing test pass. The loop owns one task state and records every action in the
//! ledger ([`crate::ledger`]). The tests run once to fail; the failure is read against the tree
//! and every function is ranked by where the fix likely is ([`crate::locate`]); a model is shown
//! the best of them and the failure, nothing else, and answers with an edit ([`crate::edit`]);
//! the edit is made and the tests run. The tests are the whole gate: only the verifier's record
//! settles an outcome.
//!
//! The controller is Kai. In the order of cheapness, an action that applies runs with no
//! inference (a ranking that is sure of itself picks the site and the action; a run that passes
//! needs no decision). Where it does not, Kai decides, all of a state's questions in one request:
//! `fix.site@1` (the site, the action, the effort) at each new state, `fix.turn@1` (next, done,
//! correct, grounded, complete, risk) after a failed run. The action is a search of small
//! mechanical changes against the visible test, or a model's edit: low effort first, then full,
//! then [`recover`]. Search ([`crate::find`], tgrep) puts the lines that spell the failure's
//! messages and names into the ranking before any of this.
//!
//! An edit that leaves fewer tests failing is progress and is kept; the state is then looked at
//! again, new failure, new ranking, new decision. An edit that does not is undone. A failed run
//! is fingerprinted, and the same fingerprint again without a change of state makes recovery
//! choose another action or escalate, never replay. Kai returns a choice the gate accepts or
//! nothing; under rule control, or when Kai does not answer, the ranking, the tests and the
//! budget decide alone. A flow that ends anywhere but `done` leaves the tree as it found it.
//!
//! To escalate is to hand the next attempt to a stronger model (`generate.escalate` in
//! `kai.toml`, `openai/gpt-6-sol` at Hanzo's API): Kai's `fix.turn@1` answering `escalate`, two
//! attempts failed, or recovery finding nothing changed moves the flow to it, and the ledger says
//! which, and which model wrote each edit. Only when the stronger model is spent too, or none is
//! configured, does `escalate` end the flow for a person to look. A run that passes the failing
//! command is not done until the tests around it ([`crate::wide`]) show no new failure.
//!
//! Every step is timed, and the trace carries the times with Kai's answers.

use crate::decide;
use crate::decide::Call;
use crate::decide::Decider;
use crate::decide::Decision;
use crate::diagnostic;
use crate::edit;
use crate::find::Finder;
use crate::find::Tgrep;
use crate::ledger;
use crate::ledger::Ledger;
use crate::ledger::Action;
use crate::ledger::Operation;
use crate::ledger::Precondition;
use crate::ledger::Source;
use crate::ledger::Status;
use crate::locate;
use crate::locate::Plan;
use crate::locate::Ranked;
use crate::mutate;
use crate::program::Kind;
use crate::program::Program;
use crate::program::Question;
use crate::symbols::Sym;
use crate::symbols::Tree;
use crate::tools;
use crate::trace::Line;
use crate::trace::Step;
use crate::trace::Trace;
use crate::wide;
use crate::wide::Wide;
use crate::zen::Effort;
use crate::zen::Generation;
use hanzo_config::kai::Mode;
use indexmap::IndexMap;
use serde_json::Value;
use serde_json::json;
use std::collections::HashSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

/// The flow's name in the trace.
const SKU: &str = "flow.fix@2";
/// Functions Kai chooses among.
const SITES: usize = 6;
/// The probability at which Kai's pick of the site reorders the ranking.
const SITE: f64 = 0.5;
/// The probability at which Kai's call for full effort is taken.
const EFFORT: f64 = 0.7;
/// The probability at which Kai's call to generate rather than search is taken.
const ACTION: f64 = 0.7;
/// Functions tgrep's evidence adds to the ranking, and the queries it is drawn from.
const EVIDENCE: usize = 3;
const QUERIES: usize = 6;
/// Rounds of search a run may keep progress in.
const STEPS: usize = 6;
/// Lines of functions the first attempt shows; each later one shows this many more.
const LINES: u32 = 90;
/// Most lines of one function shown.
const SHOWN: u32 = 100;
/// Lines of the failing test shown.
const TEST: u32 = 30;
/// Characters of the failing output shown.
const OUTPUT: usize = 2000;
/// How long the search of candidate edits may run before the model writes the edit.
const SEARCH: Duration = Duration::from_millis(2500);
/// A line of code is shown to this many characters.
const WIDE: usize = 300;
/// The ranker's probability the first attempt's functions add up to at least.
const COVER: f64 = 0.8;

const SYSTEM: &str = "You repair one failing test with the smallest possible edit. You get the \
test command, its failing output and numbered excerpts of the repository (line numbers are \
1-based and are not part of the code). Reply with ONE JSON object and nothing else:\n\
{\"edits\":[{\"file\":\"<path as in the excerpts>\",\"line\":<line old starts on>,\"old\":\"<exact \
text from the excerpt>\",\"new\":\"<its replacement>\"}]}\n\
`old` is copied exactly, without line numbers, and is the shortest text that stands only once \
near `line`: usually part of one line. Keep indentation. Change only what is wrong. Never edit \
tests. No prose, no code fences.";

/// Who steers the loop and picks the site: Kai, or the tests, the budget and the ranking.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Control {
    Rule,
    Kai,
}

pub struct Options {
    /// `{test, goal}`.
    pub task: Value,
    /// A failing run's output already in hand: the tests are not run to see them fail.
    pub failure: Option<String>,
    /// Whether candidate edits are tried against the visible test before the model is asked.
    pub search: bool,
    /// Whether tgrep's evidence is put into the ranking.
    pub discover: bool,
    /// Most attempts.
    pub max: u32,
    pub control: Control,
    /// The longest one test run may take.
    pub limit: Duration,
}

/// What `recover` does after a failed run.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Strategy {
    /// Another attempt at the same effort, with the edit and the failure in front of the model.
    Retry,
    /// Another attempt at full effort over a wider view.
    Full,
    /// No further attempt.
    Escalate,
}

impl Strategy {
    fn name(self) -> &'static str {
        match self {
            Strategy::Retry => "retry",
            Strategy::Full => "full",
            Strategy::Escalate => "escalate",
        }
    }
}

/// What the loop knows of a failed run.
#[derive(Debug, Clone)]
struct Failure {
    fingerprint: String,
    revision: String,
}

/// After a failed run, what to do. The same failure again with no change of state is no
/// progress: the same strategy is not replayed.
fn recover(failures: &[Failure], last: Strategy, remaining: u32) -> (Strategy, String) {
    let Some(now) = failures.last() else {
        return (Strategy::Retry, "no failure recorded".into());
    };
    if remaining == 0 {
        return (Strategy::Escalate, "the attempts are spent".into());
    }
    let before = failures.iter().rev().nth(1);
    match before {
        None => (Strategy::Retry, "the first failure: the model sees its edit and what the tests said".into()),
        Some(b) if b.fingerprint == now.fingerprint && b.revision == now.revision => {
            (Strategy::Escalate, "the same failure at the same revision: nothing changed".into())
        }
        Some(b) if b.fingerprint == now.fingerprint => {
            if last == Strategy::Full {
                (Strategy::Escalate, "the same failure after a full-effort attempt".into())
            } else {
                (Strategy::Full, "the same failure after another edit: full effort over a wider view".into())
            }
        }
        Some(_) => (Strategy::Retry, "the failure changed: the edit moved the state".into()),
    }
}

/// One attempt, as the report shows it.
#[derive(Debug, Clone, PartialEq)]
pub struct Attempt {
    pub n: u32,
    /// The functions shown, best first, and who ordered them.
    pub shown: Vec<String>,
    pub by: String,
    /// Kai's choice of site and its probability.
    pub site: Option<(String, f64)>,
    pub edits: Vec<String>,
    /// Why the edit was not made, when it was not.
    pub refused: Option<String>,
    pub applied: bool,
    pub passed: bool,
    /// The edit left fewer tests failing and stays.
    pub kept: bool,
    /// Kai's answer to `next` and its probability.
    pub kai: Option<(String, f64)>,
    /// Kai's answer was what moved the flow off the rule's path.
    pub kai_applied: bool,
    pub rule: String,
    pub taken: String,
    pub effort: String,
    /// The model that wrote the edit.
    pub model: String,
    /// Written by the escalation model.
    pub escalated: bool,
    /// Who moved the next attempt to the escalation model: `kai`, `failed twice` or `recover`.
    pub switch: Option<String>,
    pub generation: Option<Generation>,
    /// The end of the tests' output after the edit.
    pub after: String,
    /// Step -> milliseconds.
    pub ms: IndexMap<String, f64>,
}

pub struct Report {
    pub attempts: Vec<Attempt>,
    /// `done`, `escalate`, or `passing` when the tests passed before any attempt.
    pub outcome: String,
    /// The edits of the answer when done, else the last edit tried.
    pub patch: Vec<String>,
    /// Why Kai did not answer, the first time it did not.
    pub missing: Option<String>,
    /// Steps before the first attempt, and the whole run, in milliseconds.
    pub ms: IndexMap<String, f64>,
    /// Where the ledger is, when it was written.
    pub ledger: Option<PathBuf>,
    /// The search of candidate edits, when it ran.
    pub fast: Option<Fast>,
    /// The functions tgrep's evidence put into the ranking.
    pub evidence: Vec<String>,
    /// How many tests failed when the run began.
    pub failing: usize,
}

/// The search of candidate edits against the visible test, and what it found, over the run.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Fast {
    /// Rounds of search: one at each new state.
    pub rounds: usize,
    pub proposed: usize,
    pub tried: usize,
    /// Test runs that ran to an end.
    pub runs: usize,
    pub workers: usize,
    /// A candidate made the visible test pass.
    pub solved: bool,
    /// Steps toward a pass the search kept.
    pub kept: usize,
    /// The kind of the last candidate kept, and the line it changed.
    pub kind: Option<String>,
    pub edit: Option<String>,
    pub note: String,
    pub ms: IndexMap<String, f64>,
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
            let site = a
                .site
                .as_ref()
                .map(|(s, p)| format!(", site {s} ({p:.2})"))
                .unwrap_or_default();
            out.push_str(&format!(
                "attempt {}: {} (by {}{site}, {} effort, {}), edit {}, tests {}; next: kai {kai}, rule {} -> {}{}\n",
                a.n,
                a.shown.first().map_or("-", String::as_str),
                a.by,
                a.effort,
                a.model,
                match (&a.refused, a.applied, a.kept) {
                    (Some(why), _, _) => format!("refused ({why})"),
                    (None, true, true) => "applied and kept".into(),
                    (None, true, false) => "applied".into(),
                    (None, false, _) => "not applied".into(),
                },
                if a.passed { "pass" } else { "fail" },
                a.rule,
                a.taken,
                a.switch.as_ref().map_or(String::new(), |by| format!(" (the next attempt by the escalation model: {by})")),
            ));
            let times: Vec<String> = a.ms.iter().map(|(k, v)| format!("{k} {v:.0}")).collect();
            out.push_str(&format!("  ms: {}\n", times.join(", ")));
            if let Some(g) = &a.generation {
                out.push_str(&format!(
                    "  model {}{}: first token {:.0} ms, {:.0} ms, {} in, {} out\n",
                    g.served,
                    if g.cached { " (kept call)" } else { "" },
                    g.first,
                    g.total,
                    g.prompt,
                    g.completion
                ));
            }
        }
        if let Some(why) = &self.missing {
            out.push_str(&format!(
                "kai did not answer ({why}); the tests and the budget decided\n"
            ));
        }
        if !self.evidence.is_empty() {
            out.push_str(&format!("tgrep evidence: {}\n", self.evidence.join(", ")));
        }
        if let Some(fast) = &self.fast {
            out.push_str(&format!(
                "search: {} rounds, {} candidates proposed, {} tried, {} test runs on {} workers, {} step(s) kept, {}{}\n",
                fast.rounds,
                fast.proposed,
                fast.tried,
                fast.runs,
                fast.workers,
                fast.kept,
                match (&fast.kind, &fast.edit, fast.solved) {
                    (Some(kind), Some(edit), true) => format!("solved by a {kind} change: {edit}"),
                    _ => "no candidate passed alone".to_string(),
                },
                if fast.note.is_empty() { String::new() } else { format!(" ({})", fast.note) },
            ));
        }
        let times: Vec<String> = self.ms.iter().map(|(k, v)| format!("{k} {v:.0}")).collect();
        out.push_str(&format!("ms: {}\n", times.join(", ")));
        out.push_str(&format!("outcome: {}\n", self.outcome));
        if self.outcome == "escalate" && !self.patch.is_empty() {
            out.push_str("the tree is as it was; the last edit tried:\n");
            for line in &self.patch {
                out.push_str(&format!("  {line}\n"));
            }
        }
        out
    }
}

/// A flow's backends: Kai and its program, Zen, the trace and the ledger.
pub struct Setup {
    pub decider: Arc<Decider>,
    pub turn: Program,
    pub zen: crate::zen::Zen,
    pub trace: Trace,
    /// Where the failure's messages and names are in the tree.
    pub finder: Arc<dyn Finder>,
    ledger: PathBuf,
    runtime: tokio::runtime::Handle,
}

impl Setup {
    /// Kai, Zen and the trace from the profile at `home` (`kai.toml`, or its defaults). `key`
    /// finds the Hanzo credential and is called only when the generator is Hanzo's API: a local
    /// Zen asks for none. Call from within the async runtime.
    pub fn new(home: &Path, key: impl FnOnce() -> Option<String>) -> Result<Setup, String> {
        let settings = hanzo_config::kai::Kai::load_or_default(home).map_err(|e| e.to_string())?;
        let key = hanzo_config::is_hanzo(Some(&settings.generate.url)).then(key).flatten();
        let mut turn = Program::load("fix.turn@1")?;
        turn.configure([])?;
        Ok(Setup {
            decider: Arc::new(Decider::new(&settings.url, &settings.model, None)),
            turn,
            zen: crate::zen::Zen::new(&settings.generate, key),
            trace: Trace::new(settings.trace.clone()),
            finder: Arc::new(Tgrep),
            // The ledger sits beside the trace.
            ledger: settings.trace.with_file_name("ledger.jsonl"),
            runtime: tokio::runtime::Handle::current(),
        })
    }

    /// Kai's ruling on `state` under `program`; `None` when Kai does not answer, and why in
    /// `missing` the first time.
    fn decide(&self, program: &Program, state: Value, missing: &mut Option<String>) -> Option<Decision> {
        let call = Call::simple(Mode::Enforced, None, None);
        match self
            .runtime
            .block_on(self.decider.decide(program, vec![state], call))
        {
            Ok(d) => Some(d),
            Err(why) => {
                missing.get_or_insert(why);
                None
            }
        }
    }
}

fn ms(since: Instant) -> f64 {
    since.elapsed().as_secs_f64() * 1000.0
}

/// A distribution's likeliest option and its probability.
fn likeliest(dist: &Value) -> Option<(String, f64)> {
    dist.as_object()?
        .iter()
        .filter_map(|(k, v)| Some((k.clone(), v.as_f64()?)))
        .max_by(|a, b| a.1.total_cmp(&b.1))
}

/// The last `n` characters of `text`, from the start of a line.
fn tail(text: &str, n: usize) -> String {
    let count = text.chars().count();
    if count <= n {
        return text.trim_end().to_string();
    }
    let cut: String = text.chars().skip(count - n).collect();
    match cut.find('\n') {
        Some(i) => cut[i + 1..].trim_end().to_string(),
        None => cut,
    }
}

/// The first lines of a function's body that say something, for Kai to read an option by.
fn gist(text: &str, sym: &Sym) -> String {
    let body: Vec<&str> = text
        .split('\n')
        .skip(sym.start as usize - 1)
        .take(((sym.end - sym.start + 1) as usize).min(6))
        .collect();
    let joined = body.iter().map(|l| l.trim()).collect::<Vec<_>>().join(" ");
    decide::cut(&joined, 200)
}

/// The sites Kai chooses among: the ranking's best functions.
fn sites(plan: &Plan) -> Vec<usize> {
    plan.ranked
        .iter()
        .enumerate()
        .filter(|(_, r)| r.sym.is_some())
        .map(|(i, _)| i)
        .take(SITES)
        .collect()
}

/// `fix.site@1`: the site among the ranked functions, the action, and the effort writing the fix
/// takes; one request.
fn site_program(options: &[(String, String)]) -> Program {
    let criteria: IndexMap<String, Value> = options
        .iter()
        .map(|(label, text)| (label.clone(), json!(text)))
        .collect();
    let mut questions = IndexMap::new();
    questions.insert(
        "site".to_string(),
        Question {
            kind: Kind::Choice,
            instructions: "A test fails with the output in the state. Which function holds the \
                           fault that makes it fail?"
                .into(),
            criteria: json!(criteria),
        },
    );
    questions.insert(
        "action".to_string(),
        Question {
            kind: Kind::Choice,
            instructions: "Is the fault one small mechanical change (an operator or comparison flipped, a constant off by one, arguments swapped), or does a model have to write the edit?".into(),
            criteria: json!({
                "search": "One small mechanical change at the best sites: try each against the failing test.",
                "generate": "Not one small change: a model writes the edit.",
            }),
        },
    );
    questions.insert(
        "effort".to_string(),
        Question {
            kind: Kind::Choice,
            instructions: "How much reasoning does writing the fix for this failure take?".into(),
            criteria: json!({
                "low": "A short local edit: the cause is in the code the failure points to.",
                "full": "A subtle or multi-part fix: the cause is not where the failure shows.",
            }),
        },
    );
    Program {
        id: "fix.site@1".into(),
        description: "Which of the ranked functions holds the fault, what to do about it, and how hard the fix is.".into(),
        calibration: None,
        questions,
        thresholds: IndexMap::new(),
        verdict: None,
    }
}

/// What to do about a state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Act {
    /// Candidate edits against the visible test.
    Search,
    /// A model writes the edit.
    Generate,
}

/// The order the ranking's entries are shown in, the action, and who chose.
struct Order {
    /// Indices into `plan.ranked`, best first.
    by_rank: Vec<usize>,
    by: &'static str,
    site: Option<(String, f64)>,
    act: Act,
    effort: Effort,
    decision: Option<Decision>,
    ms: f64,
}

/// `lines` of `text` from `start`, numbered.
fn numbered(text: &str, start: u32, end: u32) -> String {
    let width = 4.max(end.to_string().len());
    text.split('\n')
        .enumerate()
        .skip(start as usize - 1)
        .take((end - start + 1) as usize)
        .map(|(i, l)| {
            let l = if l.chars().count() > WIDE {
                format!("{}...", l.chars().take(WIDE).collect::<String>())
            } else {
                l.trim_end_matches('\r').to_string()
            };
            format!("{:>width$}| {l}", i + 1)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// The excerpts to show: the functions in `order`, until their ranking probability adds up to
/// `cover` or their lines to `lines`, then the failing test. The names of what is shown, and the
/// files with the digest of what the model saw of each.
fn excerpts(plan: &Plan, tree: &mut Tree, order: &[usize], lines: u32, cover: f64) -> (String, Vec<String>, Vec<(String, String)>) {
    let mut shown: Vec<(String, u32, u32)> = Vec::new();
    let mut names = Vec::new();
    let mut parts = Vec::new();
    let mut seen: Vec<(String, String)> = Vec::new();
    let (mut total, mut sum) = (0, 0.0);
    for i in order {
        let r: &Ranked = &plan.ranked[*i];
        let (file, start, end, why) = match (&r.sym, locate::window(r)) {
            (Some(sym), Some((s, e))) if r.line.is_some() => (sym.file.clone(), s, e, sym.qual.clone()),
            (Some(sym), _) => {
                let frame = plan
                    .diagnostic
                    .frames
                    .iter()
                    .find(|f| f.file == sym.file && sym.start <= f.line && f.line <= sym.end)
                    .map(|f| f.line);
                let size = sym.end - sym.start + 1;
                let (s, e) = match frame {
                    Some(line) if size > SHOWN => {
                        let s = line.saturating_sub(SHOWN / 2).max(sym.start);
                        (s, (s + SHOWN - 1).min(sym.end))
                    }
                    _ => (sym.start, sym.end.min(sym.start + SHOWN - 1)),
                };
                (sym.file.clone(), s, e, sym.qual.clone())
            }
            (None, Some((s, e))) => (r.file.clone(), s, e, r.file.clone()),
            (None, None) => continue,
        };
        // A function inside one already shown, or holding one, adds nothing.
        if shown.iter().any(|(f, s, e)| *f == file && start <= *e && *s <= end) {
            continue;
        }
        let size = end - start + 1;
        if !parts.is_empty() && total + size > lines {
            break;
        }
        let Some(text) = tree.read(&file) else {
            continue;
        };
        parts.push(format!("{file}:{start}-{end} ({why})\n{}", numbered(text, start, end)));
        if !seen.iter().any(|(f, _)| *f == file) {
            seen.push((file.clone(), ledger::digest(text.as_bytes())));
        }
        shown.push((file, start, end));
        names.push(why);
        total += size;
        sum += r.score;
        if sum >= cover && !parts.is_empty() {
            break;
        }
    }
    if let Some(t) = &plan.test
        && !shown.iter().any(|(f, s, e)| *f == t.file && t.start <= *e && *s <= t.end)
        && let Some(text) = tree.read(&t.file)
    {
        let end = t.end.min(t.start + TEST - 1);
        parts.push(format!(
            "{}:{}-{} (the failing test {})\n{}",
            t.file,
            t.start,
            end,
            t.qual,
            numbered(text, t.start, end)
        ));
    }
    (parts.join("\n\n"), names, seen)
}

/// What the model is asked.
fn prompt(command: &str, goal: &str, failure: &str, code: &str, last: Option<&Attempt>, tried: &[String]) -> String {
    let mut out = format!("Test command: `{command}`\n");
    if !goal.is_empty() {
        out.push_str(&format!("Goal: {goal}\n"));
    }
    out.push_str(&format!("\nFailing output:\n{failure}\n"));
    if let Some(last) = last {
        match (&last.refused, last.kept) {
            (Some(why), _) => out.push_str(&format!(
                "\nYour previous edit did not apply: {why}. The files are unchanged.\n"
            )),
            (None, true) => out.push_str(&format!(
                "\nYour previous edit was applied and kept: it fixed some of the failures, and it is in the code below. The edit:\n{}\nWhat still fails is the failing output above.\n",
                last.edits.join("\n"),
            )),
            (None, false) => out.push_str(&format!(
                "\nYour previous edit was applied and the test still failed; it is reverted. The edit:\n{}\nThe test output after it:\n{}\n",
                last.edits.join("\n"),
                last.after
            )),
        }
    }
    if !tried.is_empty() {
        out.push_str("\nEdits already tried that did not fix it; do not repeat them:\n");
        for t in tried.iter().rev().take(4) {
            out.push_str(&format!("- {t}\n"));
        }
    }
    out.push_str(&format!("\nCode:\n\n{code}\n"));
    out
}

/// A short stable id for a run.
fn task_id() -> String {
    ledger::digest(format!("{}{}", crate::trace::now(), std::process::id()).as_bytes())
}

/// Quoted strings and message tails in the failing run's errors: what the code that raised them
/// spells.
fn spelled(text: &str) -> Vec<String> {
    use regex::Regex;
    use std::sync::LazyLock;
    static QUOTED: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#""([^"\n]{8,100})"|'([^'\n]{8,100})'"#).expect("a fixed pattern"));
    static TAIL: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[A-Za-z_][\w.]*(?:Error|Exception|Failure): (.{10,100})$").expect("a fixed pattern"));
    let mut out: Vec<String> = Vec::new();
    for line in text.lines() {
        for c in QUOTED.captures_iter(line) {
            let s = c.get(1).or_else(|| c.get(2)).map_or("", |m| m.as_str()).trim().to_string();
            // A path or a lone identifier is a name, not a message.
            if (s.contains(' ') || s.len() >= 14) && !s.contains('/') && !out.contains(&s) {
                out.push(s);
            }
        }
        if let Some(c) = TAIL.captures(line.trim()) {
            let s = c[1].trim().to_string();
            if s.contains(' ') && !s.starts_with("assert") && !out.contains(&s) {
                out.push(s);
            }
        }
    }
    out
}

/// What search finds for the failure: the functions whose lines spell a message it printed or
/// define a name its message spells go into the ranking, within its first four, behind the
/// ranker's own best three. The names of the functions added or moved.
fn discover(finder: &dyn Finder, root: &Path, plan: &mut Plan, ledger: &Ledger, revision: &str) -> (Vec<String>, f64) {
    let begun = Instant::now();
    let mut a = ledger.action(revision, Operation::Search { engine: "tgrep".into() }, Source::Rule, "where the failure's messages and names are spelled in the tree");
    a.evidence = plan.diagnostic.frames.iter().take(3).map(|f| format!("{}:{}", f.file, f.line)).collect();
    a.expected = "the lines that spell the failure's messages and names".into();
    a.advance(Status::Attempted);
    let mut hits: Vec<(String, u32, &str)> = Vec::new();
    let mut queries = 0;
    for text in spelled(&plan.diagnostic.text).into_iter().take(QUERIES) {
        queries += 1;
        if let Ok(found) = finder.text(root, &text, 8) {
            hits.extend(found.into_iter().filter(|f| !crate::symbols::is_test(&f.file)).take(3).map(|f| (f.file, f.line, "message")));
        }
    }
    // Names the error message itself spells (an attribute it says is missing, a key, a type), not
    // every identifier on the failing lines: those the ranker already weighs.
    let in_message: HashSet<&str> = plan.diagnostic.text.split(|c: char| !(c.is_alphanumeric() || c == '_')).collect();
    let names: Vec<&String> = plan
        .diagnostic
        .names
        .iter()
        .filter(|n| {
            n.len() >= 5
                && in_message.contains(n.as_str())
                && (n.contains('_') || n.chars().skip(1).any(char::is_uppercase))
                && !n.ends_with("Error")
                && !n.ends_with("Exception")
        })
        .take(QUERIES)
        .collect();
    for name in names {
        queries += 1;
        if let Ok(found) = finder.symbol(root, name, 6) {
            hits.extend(found.into_iter().filter(|f| f.definition && !crate::symbols::is_test(&f.file)).take(2).map(|f| (f.file, f.line, "name")));
        }
    }
    a.advance(Status::Completed);
    // The function each hit is in.
    let mut funcs: Vec<(Sym, &str)> = Vec::new();
    for (file, line, why) in &hits {
        let inside = plan
            .map
            .symbols
            .iter()
            .filter(|s| s.file == *file && s.start <= *line && *line <= s.end && !matches!(s.kind, crate::symbols::Kind::Class))
            .min_by_key(|s| s.end - s.start);
        if let Some(sym) = inside
            && !funcs.iter().any(|(f, _)| f.file == sym.file && f.start == sym.start)
        {
            funcs.push((sym.clone(), why));
        }
    }
    funcs.truncate(EVIDENCE);
    a.advance(Status::Validated);
    let mut added = Vec::new();
    for (sym, _) in &funcs {
        let at = plan
            .ranked
            .iter()
            .position(|r| r.sym.as_ref().is_some_and(|s| s.file == sym.file && s.start == sym.start));
        let keep = 3.min(plan.ranked.len());
        match at {
            Some(i) if i < 4 => continue,
            Some(i) => {
                let r = plan.ranked.remove(i);
                plan.ranked.insert(keep, r);
            }
            None => plan.ranked.insert(
                keep,
                Ranked {
                    file: sym.file.clone(),
                    sym: Some(sym.clone()),
                    score: 0.0,
                    line: None,
                },
            ),
        }
        added.push(sym.qual.clone());
    }
    a.ms = ms(begun);
    a.result = json!({"queries": queries, "hits": hits.len(), "functions": funcs.iter().map(|(s, w)| json!({"symbol": s.qual, "file": s.file, "by": w})).collect::<Vec<_>>(), "moved": added});
    if !added.is_empty() {
        a.advance(Status::Selected);
    }
    ledger.record(&a);
    (added, a.ms)
}

/// The tests around the failing ones, after an edit that passed the failing command: the run and
/// the tests failing that did not before. Recorded as an action of its own. `None` without a
/// wider command.
fn widen(wide: &Option<Wide>, root: &Path, ledger: &Ledger, revision: &str) -> Option<wide::Verdict> {
    let w = wide.as_ref()?;
    let mut a = ledger.action(revision, Operation::Test { command: w.command.clone() }, Source::Rule, "the tests around the failing ones, before done");
    a.preconditions = vec![Precondition::Revision { revision: revision.into() }];
    a.expected = "no test fails that did not fail before the edit".into();
    a.advance(Status::Attempted);
    let v = w.check(root);
    a.ms = v.run["ms"].as_f64().unwrap_or(0.0);
    a.advance(Status::Completed);
    a.advance(if v.new.is_empty() { Status::Verified } else { Status::Rejected });
    a.result = json!({"new": v.new, "timed_out": v.timed_out, "code": v.run["code"]});
    ledger.record(&a);
    Some(v)
}

/// What the choice of site, action and effort is made from.
struct Context<'a> {
    goal: &'a str,
    revision: &'a str,
    fingerprint: &'a str,
    /// The failing tests now.
    failing: usize,
    /// What was done last and how it went.
    previous: Value,
}

/// The site, the action and the effort for a state. A ranking that is sure of itself needs no
/// inference; where it is not, Kai chooses among the best in one request.
fn choose(plan: &Plan, tree: &mut Tree, kai: bool, setup: &Setup, ledger: &Ledger, cx: &Context, missing: &mut Option<String>) -> Order {
    let begun = Instant::now();
    let sure = plan.oneshot >= plan.sure();
    let among = sites(plan);
    let mut order = Order {
        by_rank: (0..plan.ranked.len()).collect(),
        by: "rule",
        site: None,
        act: Act::Search,
        effort: Effort::Low,
        decision: None,
        ms: 0.0,
    };
    if kai && among.len() >= 2 {
        if sure {
            ledger.write(
                "fast",
                json!({"id": ledger.next(), "revision": cx.revision, "op": "site", "source": "rule",
                       "reason": format!("the ranking is sure (p {:.2} >= {:.2}): one candidate applies, no inference", plan.oneshot, plan.sure())}),
            );
        } else {
            ledger.fallback(cx.revision, "site", "rule", "kai", &format!("the ranking is not sure (p {:.2} < {:.2})", plan.oneshot, plan.sure()));
            let mut options = Vec::new();
            let mut candidates = Vec::new();
            for (k, i) in among.iter().enumerate() {
                let sym = plan.ranked[*i].sym.as_ref().expect("sites have symbols");
                let text = tree.read(&sym.file).unwrap_or("").to_string();
                options.push((format!("c{k}"), format!("{} in {}: {}", sym.qual, sym.file, gist(&text, sym))));
                candidates.push(sym.qual.clone());
            }
            // Everything the choice depends on: the task, the revision and what it was read from,
            // the failure and its fingerprint, what came before, what is allowed.
            let state = json!({
                "task": cx.goal,
                "revision": cx.revision,
                "failing": cx.failing,
                "failure": decide::cut(&plan.diagnostic.text, 600),
                "fingerprint": cx.fingerprint,
                "tests": plan.diagnostic.tests.iter().take(3).collect::<Vec<_>>(),
                "evidence": plan.diagnostic.frames.iter().take(4).map(|f| format!("{}:{}", f.file, f.line)).collect::<Vec<_>>(),
                "previous": cx.previous,
                "allowed": {"action": ["search", "generate"], "effort": ["low", "full"], "edits": "source files of the tree, never a test"},
            });
            let mut a = ledger.action(cx.revision, Operation::Decide { program: "fix.site@1".into() }, Source::Kai, "the best sites, the action and the effort, in one request");
            a.evidence = candidates.clone();
            a.expected = "a site, an action and an effort the gate accepts, or abstention".into();
            a.advance(Status::Attempted);
            let asked = Instant::now();
            order.decision = setup.decide(&site_program(&options), state, missing);
            a.ms = ms(asked);
            let first = order.decision.as_ref().and_then(decide::first);
            let site = first.and_then(|r| likeliest(r.answers.get("site")?));
            let action = first.and_then(|r| likeliest(r.answers.get("action")?));
            let effort = first.and_then(|r| likeliest(r.answers.get("effort")?));
            if let Some((label, p)) = site
                && let Some(k) = label.strip_prefix('c').and_then(|k| k.parse::<usize>().ok())
            {
                order.site = Some((candidates[k].clone(), p));
                if p >= SITE {
                    let at = among[k];
                    order.by_rank.retain(|i| *i != at);
                    order.by_rank.insert(0, at);
                    order.by = "kai";
                }
            }
            let mut generate = false;
            if let Some((label, p)) = &action
                && label == "generate"
                && *p >= ACTION
            {
                order.act = Act::Generate;
                generate = true;
            }
            let mut full = false;
            if let Some((label, p)) = &effort
                && label == "full"
                && *p >= EFFORT
            {
                order.effort = Effort::Full;
                full = true;
            }
            if order.decision.is_some() {
                a.advance(Status::Completed);
                a.advance(Status::Validated);
                a.advance(if order.by == "kai" || generate || full { Status::Selected } else { Status::Abstained });
            } else {
                a.fail(missing.as_deref().unwrap_or(""));
            }
            a.result = json!({"site": order.site, "applied": order.by == "kai", "action": action, "action_generate": generate, "effort": effort, "effort_full": full, "error": missing});
            ledger.record(&a);
        }
    }
    order.ms = ms(begun);
    order
}

/// The state the loop looks at: the tree as it is, what the failure says of it, the choice made.
struct Seen {
    tree: Tree,
    plan: Plan,
    order: Order,
    shown: String,
    fingerprint: String,
}

/// Runs the `fix` flow over the repository at `root`. Blocks: call it off the async runtime.
pub fn run(root: &Path, options: &Options, setup: &Setup) -> Result<Report, String> {
    let whole = Instant::now();
    let command = options.task["test"].as_str().unwrap_or_default();
    let goal = options.task["goal"].as_str().unwrap_or_default();
    if command.is_empty() {
        return Err("the task names no test command".into());
    }
    let task = task_id();
    let ledger = Ledger::new(&task, setup.ledger.clone());
    let kai = options.control == Control::Kai;
    let mut steps: IndexMap<String, f64> = IndexMap::new();
    let mut report = Report {
        attempts: Vec::new(),
        outcome: String::new(),
        patch: Vec::new(),
        missing: None,
        ms: IndexMap::new(),
        ledger: Some(setup.ledger.clone()),
        fast: None,
        evidence: Vec::new(),
        failing: 0,
    };
    // The task state: the files the run has touched give the revision; the edits kept are in the
    // tree; the failures give recover its memory.
    let mut touched: Vec<String> = Vec::new();
    // Each touched file's digest when the run began, so a revert is the revision it left.
    let mut origin: std::collections::BTreeMap<String, String> = std::collections::BTreeMap::new();
    let mut kept: Vec<edit::Applied> = Vec::new();
    let mut kept_summary: Vec<String> = Vec::new();
    let mut failures: Vec<Failure> = Vec::new();
    let mut tried: Vec<String> = Vec::new();
    let mut fast = Fast::default();

    // The tests run once to fail: the verifier's first record. Given a failing run's output, they
    // are not run to see it.
    let mut a = ledger.action("base", Operation::Test { command: command.into() }, Source::Rule, "the task's own test command: the state to repair");
    a.expected = "the tests fail".into();
    a.advance(Status::Attempted);
    let begun = Instant::now();
    let (mut output, passed0, code0) = match &options.failure {
        Some(given) => {
            a.reason = "the failing output was given: the tests are not run to see it".into();
            (given.clone(), false, 1)
        }
        None => {
            let run = tools::test(root, command, options.limit);
            (
                run["output"].as_str().unwrap_or_default().to_string(),
                run["passed"] == true,
                run["code"].as_i64().unwrap_or(-1),
            )
        }
    };
    a.ms = ms(begun);
    steps.insert("test0".into(), a.ms);
    a.advance(Status::Completed);
    if !passed0 {
        a.advance(Status::Verified);
    }
    a.result = json!({"passed": passed0, "code": code0, "given": options.failure.is_some()});
    ledger.record(&a);
    ledger.verify(&a.id, "base", passed0, code0, &ledger::fingerprint(&output), &tail(&output, 600), a.ms);
    if passed0 {
        report.outcome = "passing".into();
        return Ok(report);
    }
    let mut failing = diagnostic::failures(&output).max(1);
    let errors0 = diagnostic::errors(&output);
    report.failing = failing;
    let test_ms = steps["test0"];
    // The tests around the failing ones, once before any edit: what fails there already.
    let begun = Instant::now();
    let wide = Wide::before(root, command, &output);
    if let Some(w) = &wide {
        let mut a = ledger.action("base", Operation::Test { command: w.command.clone() }, Source::Rule, "the tests around the failing ones, before any edit: what already fails there");
        a.advance(Status::Attempted);
        a.advance(Status::Completed);
        a.ms = ms(begun);
        a.result = json!({"code": w.before_code, "failing": w.before});
        ledger.record(&a);
        steps.insert("wide0".into(), a.ms);
    }

    let mut seen: Option<Seen> = None;
    let mut look = true;
    // The states searched, by the edits kept when they were: a reverted attempt is no new state.
    let mut searched: HashSet<usize> = HashSet::new();
    let mut last: Option<Attempt> = None;
    // The edits tried and put back, so the model is not asked the same question twice.
    let mut history: Vec<String> = Vec::new();
    let mut strategy = Strategy::Retry;
    // The next attempts are the escalation model's, and the attempts that did not pass so far.
    let mut stronger = false;
    let mut lost = 0u32;
    let mut refused = 0;
    let mut n = 0u32;
    let mut guard = 0u32;
    'solve: loop {
        guard += 1;
        if guard > options.max.max(1) * 3 + STEPS as u32 {
            break;
        }
        let revision = ledger::revision_over(root, &touched, &origin);

        // Look at the state: the tree as it is, the failure ranked against it, the evidence
        // search finds for it, the choice of site, action and effort.
        if look {
            let begun = Instant::now();
            let mut tree = Tree::open(root);
            *steps.entry("tree".into()).or_insert(0.0) += ms(begun);
            let begun = Instant::now();
            let mut plan = locate::plan(&mut tree, &output);
            *steps.entry("rank".into()).or_insert(0.0) += ms(begun);
            if plan.ranked.is_empty() {
                if report.attempts.is_empty() && fast.rounds == 0 {
                    return Err("nothing in the repository is ranked: the failure points at no function".into());
                }
                break;
            }
            let fingerprint = ledger::fingerprint(&output);
            if failures.is_empty() {
                let mut a = ledger.action(&revision, Operation::Search { engine: "locate".into() }, Source::Rule, "rank every function by where the fix likely is");
                a.evidence = plan.diagnostic.frames.iter().take(6).map(|f| format!("{}:{}", f.file, f.line)).collect();
                a.expected = "the fix site among the best".into();
                a.advance(Status::Attempted);
                a.advance(Status::Completed);
                a.advance(Status::Validated);
                a.ms = steps["rank"] + steps["tree"];
                a.result = json!({
                    "ranked": plan.ranked.iter().take(SITES).map(|r| json!({"symbol": r.sym.as_ref().map(|s| s.qual.clone()), "file": r.file, "score": r.score})).collect::<Vec<_>>(),
                    "oneshot": plan.oneshot,
                });
                ledger.record(&a);
                failures.push(Failure { fingerprint: fingerprint.clone(), revision: revision.clone() });
            }
            if options.discover {
                let (added, took) = discover(setup.finder.as_ref(), root, &mut plan, &ledger, &revision);
                *steps.entry("discover".into()).or_insert(0.0) += took;
                report.evidence.extend(added);
            }
            let begun = Instant::now();
            let previous = last.as_ref().map_or(Value::Null, |l| {
                json!({"edits": l.edits, "kept": l.kept, "passed": l.passed, "effort": l.effort, "fingerprint_changed": failures.len() >= 2 && failures[failures.len() - 1].fingerprint != failures[failures.len() - 2].fingerprint})
            });
            let cx = Context { goal, revision: &revision, fingerprint: &fingerprint, failing, previous };
            let order = choose(&plan, &mut tree, kai, setup, &ledger, &cx, &mut report.missing);
            *steps.entry("site".into()).or_insert(0.0) += ms(begun);
            seen = Some(Seen { tree, plan, order, shown: tail(&output, OUTPUT), fingerprint });
            look = false;
        }
        let Some(s) = seen.as_mut() else { break };
        let fingerprint = s.fingerprint.clone();

        // The cheap action first: candidate edits at the best sites against the visible test.
        // A test that takes longer than the cap holds cannot be tried candidate after candidate.
        let slow = diagnostic::seconds(&output).is_some_and(|t| t * 1.5 > SEARCH.as_secs_f64());
        if slow && options.search && s.order.act == Act::Search && searched.insert(kept.len()) {
            ledger.fallback(&revision, "search", "rule", "model", &format!("the visible test takes {:.1} s: longer than the search cap of {:.1} s holds", diagnostic::seconds(&output).unwrap_or(0.0), SEARCH.as_secs_f64()));
        }
        if !slow && options.search && s.order.act == Act::Search && fast.rounds < STEPS && searched.insert(kept.len()) {
            let begun = Instant::now();
            let candidates = mutate::propose(&mut s.tree, &s.plan, &s.order.by_rank);
            *steps.entry("propose".into()).or_insert(0.0) += ms(begun);
            fast.rounds += 1;
            fast.proposed += candidates.len();
            if !candidates.is_empty() {
                let source = if s.order.by == "kai" { Source::Kai } else { Source::Rule };
                let mut m = ledger.action(&revision, Operation::Mutate { candidates: candidates.len() }, source, "candidate edits at the best sites, each against the visible test");
                m.evidence = candidates.iter().take(3).map(|c| format!("{}:{} {}", c.file, c.line, c.kind)).collect();
                m.expected = "one candidate passes the visible test, or leaves fewer failing".into();
                m.advance(Status::Attempted);
                let begun = Instant::now();
                // A candidate that runs far past the starting run's time is a loop it made.
                let floor = if options.failure.is_some() { 10.0 } else { 3.0 };
                let limit = Duration::from_secs_f64((test_ms / 1000.0 * 3.0 + 1.0).clamp(floor, options.limit.as_secs_f64().max(floor)));
                let workers = std::thread::available_parallelism().map_or(2, usize::from);
                let found = mutate::search(root, command, &candidates, failing, errors0, SEARCH, limit, workers);
                m.ms = ms(begun);
                fast.tried += found.tried;
                fast.runs += found.runs;
                fast.workers = fast.workers.max(found.workers);
                if !found.note.is_empty() {
                    fast.note.clone_from(&found.note);
                }
                *fast.ms.entry("copy".into()).or_insert(0.0) += found.copy;
                *fast.ms.entry("search".into()).or_insert(0.0) += found.total;
                *steps.entry("search".into()).or_insert(0.0) += found.total;
                let pick = found.winner.or(found.progress);
                m.advance(Status::Completed);
                m.advance(if pick.is_some() { Status::Selected } else { Status::Rejected });
                m.result = json!({"tried": found.tried, "runs": found.runs, "workers": found.workers, "winner": found.winner, "progress": found.progress, "remaining": found.remaining, "note": found.note, "copy_ms": found.copy});
                ledger.record(&m);
                if let Some(w) = pick {
                    let c = &candidates[w];
                    let mut e = ledger.action(&revision, Operation::Edit { edits: Vec::new() }, Source::Rule, if found.winner.is_some() { "the candidate that passed the visible test" } else { "the candidate that left the fewest tests failing" });
                    e.preconditions = vec![Precondition::Revision { revision: revision.clone() }];
                    e.authorization = "auto: a source file of the tree, never a test".into();
                    e.expected = "the tests pass, or fewer fail".into();
                    e.advance(Status::Attempted);
                    let begun = Instant::now();
                    match edit::apply(root, &[edit::Edit { file: c.file.clone(), line: c.line, old: c.old.clone(), new: c.new.clone() }]) {
                        Ok(made) => {
                            e.ms = ms(begun);
                            let files = made.files();
                            for (f, d) in made.originals() {
                                origin.entry(f).or_insert(d);
                            }
                            for f in &files {
                                if !touched.contains(f) {
                                    touched.push(f.clone());
                                }
                            }
                            let rev = ledger::revision_over(root, &touched, &origin);
                            e.operation = Operation::Edit { edits: made.summary.clone() };
                            e.advance(Status::Applied);
                            e.result = json!({"files": files, "revision": rev, "kind": c.kind});
                            ledger.record(&e);
                            // The candidate ran in a copy of the tree; the tree itself is what is
                            // verified.
                            let mut t = ledger.action(&rev, Operation::Test { command: command.into() }, Source::Rule, "verify the candidate in the tree itself");
                            t.preconditions = vec![Precondition::Revision { revision: rev.clone() }];
                            t.expected = "the tests pass, or fewer fail".into();
                            t.advance(Status::Attempted);
                            let begun = Instant::now();
                            let test = tools::test(root, command, options.limit);
                            t.ms = ms(begun);
                            *steps.entry("verify".into()).or_insert(0.0) += t.ms;
                            let mut out = test["output"].as_str().unwrap_or_default().to_string();
                            let mut passed = test["passed"] == true;
                            let mut test = test;
                            // Passing the failing command is not done until the tests around it agree.
                            let mut broke = None;
                            if passed && let Some(v) = widen(&wide, root, &ledger, &rev) {
                                *steps.entry("wide".into()).or_insert(0.0) += v.run["ms"].as_f64().unwrap_or(0.0);
                                if !v.new.is_empty() {
                                    broke = Some(format!("the candidate passed the failing command and broke {}", v.new.join(", ")));
                                    passed = false;
                                    out = v.run["output"].as_str().unwrap_or_default().to_string();
                                    test = v.run;
                                }
                            }
                            let now = diagnostic::failures(&out);
                            t.advance(Status::Completed);
                            let better = !passed && now >= 1 && now < failing && !diagnostic::broken(&out) && diagnostic::errors(&out) <= errors0;
                            t.advance(if passed || better { Status::Verified } else { Status::Rejected });
                            t.result = json!({"passed": passed, "code": test["code"], "failing": now});
                            ledger.record(&t);
                            ledger.verify(&t.id, &rev, passed, test["code"].as_i64().unwrap_or(-1), &ledger::fingerprint(&out), &tail(&out, 600), t.ms);
                            if passed {
                                fast.solved = true;
                                fast.kind = Some(c.kind.to_string());
                                fast.edit = made.summary.first().cloned();
                                kept_summary.extend(made.summary.clone());
                                report.patch = kept_summary.clone();
                                report.outcome = "done".into();
                                steps.insert("total".into(), ms(whole));
                                report.ms = steps;
                                report.fast = Some(fast.clone());
                                setup.trace.write(&Line {
                                    time: crate::trace::now(),
                                    request: format!("{task}#0"),
                                    family: "dev.flow".into(),
                                    sku: SKU.into(),
                                    ms: report.ms["total"],
                                    outcome: crate::trace::Outcome {
                                        status: 0,
                                        finish: "done".into(),
                                        result: json!({"passed": true, "by": "search", "tried": fast.tried, "runs": fast.runs, "kept": fast.kept, "kind": fast.kind, "edit": fast.edit, "ms": report.ms}),
                                        ..Default::default()
                                    },
                                    ..Line::default()
                                });
                                ledger.write("end", json!({"id": ledger.next(), "outcome": "done", "by": "search", "ms": report.ms}));
                                return Ok(report);
                            }
                            if better {
                                // A step toward a pass: it stays, and the state is looked at again.
                                fast.kept += 1;
                                fast.kind = Some(c.kind.to_string());
                                fast.edit = made.summary.first().cloned();
                                kept_summary.extend(made.summary.clone());
                                kept.push(made);
                                failures.push(Failure { fingerprint: ledger::fingerprint(&out), revision: rev.clone() });
                                tried.push(rev);
                                output = out;
                                failing = now;
                                look = true;
                                last = None;
                                continue 'solve;
                            }
                            edit::revert(root, &made);
                            fast.note = broke.unwrap_or_else(|| "the candidate passed in a copy and not in the tree".into());
                        }
                        Err(why) => {
                            e.advance(Status::Rejected);
                            e.result = json!({"error": why});
                            ledger.record(&e);
                            fast.note = format!("the candidate did not apply: {why}");
                        }
                    }
                }
                ledger.fallback(&revision, "search", "rule", "model", &format!("no candidate settled it ({} tried of {} in {:.0} ms)", found.tried, candidates.len(), found.total));
            }
        }

        // The model writes the edit.
        n += 1;
        // After an escalation the escalation model writes, at full effort over a wider view.
        let escalation = stronger.then(|| setup.zen.escalation()).flatten();
        let model = escalation.unwrap_or_else(|| setup.zen.model()).to_string();
        let wider = strategy == Strategy::Full || escalation.is_some();
        let effort = if wider { Effort::Full } else { s.order.effort };
        let mut ms_of: IndexMap<String, f64> = IndexMap::new();
        let begun = Instant::now();
        let (code, shown, saw) = excerpts(
            &s.plan,
            &mut s.tree,
            &s.order.by_rank,
            LINES * n.min(3) * if wider { 2 } else { 1 },
            if n == 1 { COVER } else { 0.999 },
        );
        ms_of.insert("bundle".into(), ms(begun));
        let user = prompt(command, goal, &s.shown, &code, last.as_ref(), &history);
        let source = if n == 1 && kept.is_empty() { Source::Model } else { Source::Recover };
        let mut attempt = Attempt {
            n,
            shown,
            by: s.order.by.to_string(),
            site: s.order.site.clone(),
            edits: Vec::new(),
            refused: None,
            applied: false,
            passed: false,
            kept: false,
            kai: None,
            rule: String::new(),
            taken: String::new(),
            effort: format!("{effort:?}").to_lowercase(),
            model: model.clone(),
            escalated: escalation.is_some(),
            switch: None,
            kai_applied: false,
            generation: None,
            after: String::new(),
            ms: IndexMap::new(),
        };
        let mut g = ledger.action(&revision, Operation::Generate { model: model.clone(), effort: attempt.effort.clone() }, source, if escalation.is_some() { "recover: the escalation model writes the attempt" } else if n == 1 { "the first edit" } else { "recover: another attempt" });
        g.evidence = saw.iter().map(|(f, d)| format!("{f}@{d}")).collect();
        g.expected = "one JSON edit that applies".into();
        g.advance(Status::Attempted);
        let begun = Instant::now();
        let generated = setup.zen.generate(&model, SYSTEM, &user, effort);
        g.ms = ms(begun);
        ms_of.insert("generate".into(), g.ms);
        let answer = match generated {
            Ok(x) => {
                g.result = json!({"model": x.model, "served": x.served, "cached": x.cached, "escalated": escalation.is_some(), "first_ms": x.first, "total_ms": x.total, "prompt_tokens": x.prompt, "completion_tokens": x.completion});
                let text = x.text.clone();
                attempt.generation = Some(x);
                g.advance(Status::Completed);
                Ok(text)
            }
            Err(why) => {
                g.fail(&why);
                g.result = json!({"error": why});
                Err(why)
            }
        };
        ledger.record(&g);

        // The edit is made against the revision and the files the model saw.
        let mut e = ledger.action(&revision, Operation::Edit { edits: Vec::new() }, source, "make the model's edit");
        e.preconditions = std::iter::once(Precondition::Revision { revision: revision.clone() })
            .chain(saw.iter().map(|(f, d)| Precondition::Digest { file: f.clone(), digest: d.clone() }))
            .collect();
        e.authorization = "auto: a source file of the tree, never a test".into();
        e.expected = "the edit applies and the tests then pass".into();
        e.advance(Status::Attempted);
        let begun = Instant::now();
        let held = e.preconditions.iter().find_map(|p| match p {
            Precondition::Revision { revision: r } if *r != ledger::revision_over(root, &touched, &origin) => Some(format!("the workspace is no longer at revision {r}")),
            Precondition::Digest { file, digest } if ledger::digest(&std::fs::read(root.join(file)).unwrap_or_default()) != *digest => {
                Some(format!("{file} changed since the model read it"))
            }
            _ => None,
        });
        let applied = match (&answer, held) {
            (_, Some(why)) => Err(format!("a precondition does not hold: {why}")),
            (Ok(text), None) => edit::parse(text).and_then(|edits| edit::apply(root, &edits)),
            (Err(why), None) => Err(why.clone()),
        };
        let after = match &applied {
            Ok(made) => {
                let files = made.files();
                for (f, d) in made.originals() {
                    origin.entry(f).or_insert(d);
                }
                let rev = ledger::revision_over(root, &files.iter().chain(touched.iter()).cloned().collect::<Vec<_>>(), &origin);
                // The same edit again is no new state: recover chooses another action.
                if tried.contains(&rev) {
                    edit::revert(root, made);
                    Err("the edit repeats an earlier attempt's: the workspace would be in a state already tried".to_string())
                } else {
                    Ok((made.clone(), files, rev))
                }
            }
            Err(why) => Err(why.clone()),
        };
        e.ms = ms(begun);
        ms_of.insert("apply".into(), e.ms);
        let mut test = Value::Null;
        match &after {
            Ok((made, files, rev)) => {
                e.advance(Status::Applied);
                e.operation = Operation::Edit { edits: made.summary.clone() };
                e.result = json!({"files": files, "revision": rev, "model": model});
                attempt.applied = true;
                attempt.edits = made.summary.clone();
            }
            Err(why) => {
                e.advance(Status::Rejected);
                e.result = json!({"error": why, "model": model});
                attempt.refused = Some(why.clone());
            }
        }
        ledger.record(&e);

        // The tests are the verifier: only their record settles the outcome.
        let mut after_rev = revision.clone();
        let mut now = failing;
        let mut after_fp = fingerprint.clone();
        if let Ok((_, files, rev)) = &after {
            for f in files {
                if !touched.contains(f) {
                    touched.push(f.clone());
                }
            }
            after_rev.clone_from(rev);
            let mut t = ledger.action(rev, Operation::Test { command: command.into() }, Source::Rule, "verify the edit");
            t.preconditions = vec![Precondition::Revision { revision: rev.clone() }];
            t.expected = "the tests pass".into();
            t.advance(Status::Attempted);
            let begun = Instant::now();
            test = tools::test(root, command, options.limit);
            t.ms = ms(begun);
            ms_of.insert("test".into(), t.ms);
            // Passing the failing command is not done until the tests around it agree.
            if test["passed"] == true
                && let Some(v) = widen(&wide, root, &ledger, rev)
            {
                ms_of.insert("wide".into(), v.run["ms"].as_f64().unwrap_or(0.0));
                if !v.new.is_empty() {
                    test = v.run;
                }
            }
            let text = test["output"].as_str().unwrap_or_default();
            attempt.after = tail(text, OUTPUT);
            attempt.passed = test["passed"] == true;
            now = diagnostic::failures(text);
            after_fp = ledger::fingerprint(text);
            t.advance(Status::Completed);
            t.advance(if attempt.passed { Status::Verified } else { Status::Rejected });
            t.result = json!({"passed": attempt.passed, "code": test["code"], "failing": now});
            ledger.record(&t);
            ledger.verify(&t.id, rev, attempt.passed, test["code"].as_i64().unwrap_or(-1), &after_fp, &attempt.after, t.ms);
            tried.push(rev.clone());
        }

        // A run that passes needs no decision. A run that fails: Kai says what next, and reads
        // the patch, in one request.
        let mut turn: Option<Decision> = None;
        let mut turn_action: Option<Action> = None;
        if attempt.applied && !attempt.passed {
            if kai {
                ledger.fallback(&after_rev, "turn", "rule", "kai", "the verifier failed: what next, and is the patch correct and grounded");
                let begun = Instant::now();
                let mut a = ledger.action(&after_rev, Operation::Decide { program: "fix.turn@1".into() }, Source::Kai, "next, done, correct, grounded, complete, risk, in one request");
                a.advance(Status::Attempted);
                turn = setup.decide(
                    &setup.turn,
                    json!({
                        "goal": goal,
                        "attempt": n,
                        "of": options.max,
                        "revision": after_rev,
                        "test": {"passed": test["passed"], "code": test["code"], "output": tail(test["output"].as_str().unwrap_or_default(), 800)},
                        "failing": {"before": failing, "now": now},
                        "fingerprint": after_fp,
                        "previous": {"action": g.id, "effort": attempt.effort, "failure": failures.last().map(|f| f.fingerprint.clone())},
                        "patch": attempt.edits,
                        "allowed": {"next": ["done", "retry", "escalate"]},
                    }),
                    &mut report.missing,
                );
                a.ms = ms(begun);
                ms_of.insert("kai".into(), a.ms);
                if turn.is_some() {
                    a.advance(Status::Completed);
                    a.advance(Status::Validated);
                } else {
                    a.fail(report.missing.as_deref().unwrap_or(""));
                }
                a.result = json!({"answers": turn.as_ref().and_then(decide::first).map(|r| r.answers.clone())});
                turn_action = Some(a);
            }
        } else if attempt.passed {
            ledger.write(
                "fast",
                json!({"id": ledger.next(), "revision": after_rev, "op": "turn", "source": "rule",
                       "reason": "the verifier passed: the task is done, no inference"}),
            );
        }
        let signals = turn.as_ref().and_then(decide::first).map(|r| &r.signals);
        let signal = signals.and_then(|s| s.get("next"));
        let remaining = options.max.saturating_sub(n);
        let rule = match (attempt.passed, remaining > 0) {
            (true, _) => "done",
            (false, true) => "retry",
            (false, false) => "escalate",
        };
        let permitted: &[&str] = match rule {
            "done" => &["done"],
            "retry" => &["retry", "escalate"],
            _ => &["escalate"],
        };
        let answer = signal.and_then(decide::accepted).filter(|answer| permitted.contains(answer));
        let kai_escalates = answer == Some("escalate") && !attempt.passed;

        // recover: the failure's fingerprint and the state it happened in decide what is next.
        let progress = attempt.applied
            && !attempt.passed
            && now >= 1
            && now < failing
            && !diagnostic::broken(test["output"].as_str().unwrap_or_default())
            && diagnostic::errors(test["output"].as_str().unwrap_or_default()) <= errors0;
        let mut why = String::new();
        let mut stuck = false;
        if attempt.applied && !attempt.passed {
            refused = 0;
            failures.push(Failure { fingerprint: after_fp.clone(), revision: after_rev.clone() });
            let (next, reason) = recover(&failures, strategy, remaining);
            why = reason;
            // Fewer tests failing is a change of state: recover does not call it a replay.
            stuck = remaining > 0 && next == Strategy::Escalate && !progress;
            strategy = if progress { Strategy::Retry } else { next };
        } else if !attempt.applied {
            // An edit that was refused is not a failed run: the model is told why, and twice in a
            // row it is given full effort over a wider view.
            refused += 1;
            strategy = if refused >= 2 { Strategy::Full } else { Strategy::Retry };
            why = format!("the edit was refused: {}", attempt.refused.clone().unwrap_or_default());
        }
        // To escalate is to hand the next attempt to the escalation model: Kai said so, two
        // attempts have failed, or recovery found nothing changed. With none to hand it to, or
        // with it spent, there is nothing stronger and a person must look.
        let mut switch: Option<&str> = None;
        let mut ends = false;
        if !attempt.passed {
            lost += 1;
            let can = setup.zen.escalation().is_some();
            if remaining == 0 {
                ends = true;
            } else if can && !stronger {
                switch = if kai_escalates {
                    Some("kai")
                } else if lost >= 2 {
                    Some("failed twice")
                } else if stuck {
                    Some("recover")
                } else {
                    None
                };
            } else if stuck || (kai_escalates && !can) {
                ends = true;
            }
        }
        stronger |= switch.is_some();
        let taken = if attempt.passed {
            "done".to_string()
        } else if ends || switch.is_some() {
            "escalate".to_string()
        } else {
            "retry".to_string()
        };
        // Kai's answer acted when it was what moved the flow off the rule's path.
        let kai_applied = kai_escalates && (switch == Some("kai") || (ends && remaining > 0));
        if let Some(mut a) = turn_action {
            let effect = match (switch, ends) {
                (Some(_), _) => format!("the next attempt is by {}", setup.zen.escalation().unwrap_or_default()),
                (None, true) => "the flow ends: a person must look".to_string(),
                (None, false) => "the next attempt is the same model's".to_string(),
            };
            if turn.is_some() {
                a.advance(if answer.is_some() { Status::Selected } else { Status::Abstained });
            }
            a.result["answer"] = json!(signal.map(|s| json!({"answer": s.answer, "certainty": s.certainty, "accepted": s.accepted})));
            a.result["applied"] = json!(kai_applied);
            a.result["taken"] = json!(taken);
            a.result["effect"] = json!(effect);
            ledger.record(&a);
        }
        if !attempt.passed {
            ledger.write(
                "recover",
                json!({"id": ledger.next(), "attempt": n, "fingerprint": after_fp, "revision": after_rev,
                       "prior": e.id, "strategy": strategy.name(), "taken": taken, "reason": why,
                       "model": model, "switch": switch, "next_model": if stronger { setup.zen.escalation() } else { Some(setup.zen.model()) },
                       "failing": {"before": failing, "now": now}, "progress": progress,
                       "status": if ends { "escalated" } else { "attempted" }}),
            );
        }
        attempt.kai = signal.map(|s| (s.answer.clone(), s.certainty));
        attempt.kai_applied = kai_applied;
        attempt.switch = switch.map(str::to_string);
        attempt.rule = rule.into();
        attempt.taken = taken.clone();
        attempt.ms = ms_of;
        setup.trace.write(&line(&s.order, turn.as_ref(), &attempt, &task, &why));
        if !attempt.edits.is_empty() {
            report.patch.clone_from(&attempt.edits);
        }

        // An edit that left fewer tests failing stays; any other that did not settle the task is
        // undone.
        if taken != "done"
            && let Ok((made, _, _)) = &after
            && attempt.applied
        {
            if progress && !ends {
                attempt.kept = true;
                kept_summary.extend(made.summary.clone());
                kept.push(made.clone());
                let mut k = ledger.action(&after_rev, Operation::Edit { edits: made.summary.clone() }, Source::Recover, "fewer tests fail: the edit stays");
                k.advance(Status::Attempted);
                k.advance(Status::Applied);
                k.advance(Status::Verified);
                k.result = json!({"failing": {"before": failing, "now": now}});
                ledger.record(&k);
                output = test["output"].as_str().unwrap_or_default().to_string();
                failing = now;
                look = true;
            } else {
                history.extend(made.summary.iter().map(|e| decide::cut(e, 300)));
                let mut r = ledger.action(&after_rev, Operation::Revert { files: made.files() }, Source::Rule, "the attempt did not settle the task: back to the files before it");
                r.advance(Status::Attempted);
                let begun = Instant::now();
                let ok = edit::revert(root, made);
                r.ms = ms(begun);
                r.advance(if ok { Status::Applied } else { Status::Failed });
                ledger.record(&r);
                if !ok {
                    return Err(format!(
                        "attempt {n}: the files cannot be put back, so the working tree still holds the edit; see `git diff`"
                    ));
                }
            }
        }
        report.attempts.push(attempt.clone());
        if taken == "done" {
            kept_summary.extend(attempt.edits.clone());
            report.patch = kept_summary.clone();
            report.outcome = taken;
            break;
        }
        if ends {
            report.outcome = "escalate".into();
            break;
        }
        last = Some(attempt);
    }
    if report.outcome.is_empty() {
        report.outcome = "escalate".into();
    }
    // A flow that does not end done leaves the tree as it found it.
    if report.outcome != "done" {
        while let Some(made) = kept.pop() {
            if !edit::revert(root, &made) {
                return Err("the files cannot be put back, so the working tree still holds kept edits; see `git diff`".into());
            }
        }
    }
    steps.insert("total".into(), ms(whole));
    report.ms = steps;
    report.fast = Some(fast);
    ledger.write("end", json!({"id": ledger.next(), "outcome": report.outcome, "ms": report.ms}));
    Ok(report)
}

/// An attempt's trace line: Kai's site and turn, with what was done, what followed and how long
/// each step took.
fn line(order: &Order, turn: Option<&Decision>, attempt: &Attempt, run: &str, why: &str) -> Line {
    let mut ops = std::collections::BTreeMap::new();
    if let Some(d) = &order.decision {
        let mut step = Step::new(&d.program, Mode::Enforced.name());
        step.fill(d);
        step.kai = order.site.as_ref().map(|(s, _)| s.clone()).unwrap_or_default();
        step.taken = attempt.shown.first().cloned().unwrap_or_default();
        step.applied = attempt.by == "kai";
        ops.insert("site".to_string(), step);
    }
    if let Some(d) = turn {
        let mut step = Step::new(&d.program, Mode::Enforced.name());
        step.fill(d);
        step.kai = decide::first(d)
            .and_then(|r| likeliest(&r.answers["next"]))
            .map(|(a, _)| a)
            .unwrap_or_default();
        step.taken = attempt.taken.clone();
        step.applied = attempt.kai_applied;
        ops.insert("turn".to_string(), step);
    }
    let generation = attempt.generation.as_ref().map(|g| {
        json!({"model": g.model, "served": g.served, "cached": g.cached, "first_ms": g.first, "total_ms": g.total, "prompt_tokens": g.prompt, "completion_tokens": g.completion})
    });
    Line {
        time: crate::trace::now(),
        request: format!("{run}#{}", attempt.n),
        family: "dev.flow".into(),
        sku: SKU.into(),
        ms: attempt.ms.values().sum(),
        prompt_tokens: attempt.generation.as_ref().map_or(0, |g| g.prompt as i64),
        completion_tokens: attempt.generation.as_ref().map_or(0, |g| g.completion as i64),
        outcome: crate::trace::Outcome {
            status: i64::from(!attempt.passed),
            finish: attempt.taken.clone(),
            escalated: attempt.taken == "escalate",
            result: json!({
                "passed": attempt.passed,
                "applied": attempt.applied,
                "attempt": attempt.n,
                "shown": attempt.shown,
                "by": attempt.by,
                "effort": attempt.effort,
                "model": attempt.model,
                "escalated": attempt.escalated,
                "switch": attempt.switch,
                "refused": attempt.refused,
                "recover": why,
                "ms": attempt.ms,
                "generation": generation,
            }),
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
