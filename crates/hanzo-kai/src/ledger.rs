//! The ledger: every action of a run, replayable.
//!
//! The loop owns one task state and one ledger. An [`Action`] is a typed operation with the
//! revision it is bound to, the evidence that led to it, the preconditions checked immediately
//! before it runs, the authorization it ran under, what it is expected to produce and how that
//! is verified, and who proposed it ([`Source`]): a rule, Kai, a model or `recover`. An expected
//! outcome is never accepted because something predicted it: only the verifier's record settles
//! it. Every entry has a stable id (`<task>.<n>`), so time and cost are attributable exactly, and
//! credentials and sensitive output are redacted before a line is written.

use regex::Regex;
use serde::Serialize;
use serde_json::Value;
use serde_json::json;
use sha2::Digest;
use sha2::Sha256;
use std::collections::BTreeMap;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::sync::LazyLock;
use std::sync::Mutex;

/// Characters of one output a ledger line keeps.
const OUTPUT: usize = 600;

/// Who proposed an action.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Source {
    /// An action that applied with no inference: one candidate, preconditions holding.
    Rule,
    /// Kai's structured decision.
    Kai,
    /// A model's generation.
    Model,
    /// `recover`, after a failed verification.
    Recover,
}

/// Where an action stands. A decision passes eligible, attempted, completed, validated, selected,
/// applied and verified in that order, or ends early as abstained, rejected, timed out, failed or
/// escalated. `verified` is the independent check's word, never a prediction. Whether an action was
/// beneficial is never set here: only a benchmark sets it, from a measured gain.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Status {
    /// Its preconditions hold; nothing has run.
    Eligible,
    /// It has been started.
    Attempted,
    /// It returned something.
    Completed,
    /// What it returned is well formed and within what is allowed.
    Validated,
    /// It was chosen among the alternatives.
    Selected,
    /// Its effect is in the workspace.
    Applied,
    /// The verifier confirmed its effect.
    Verified,
    /// It declined to choose: no answer the gate accepts.
    Abstained,
    /// It was refused: a precondition did not hold, or the verifier refused its effect.
    Rejected,
    TimedOut,
    Failed,
    Escalated,
}

/// What an action does.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Operation {
    /// Rank the tree's functions by where the fix likely is.
    Search { engine: String },
    /// Kai's structured decision: one request, every question a state needs.
    Decide { program: String },
    /// A model writes an edit.
    Generate { model: String, effort: String },
    /// Candidate edits at the best sites, each against the visible test.
    Mutate { candidates: usize },
    /// Make the edits in the tree.
    Edit { edits: Vec<String> },
    /// Run the tests: the verifier.
    Test { command: String },
    /// Put back the files an edit changed.
    Revert { files: Vec<String> },
}

/// A condition checked immediately before an action runs.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Precondition {
    /// The workspace is at this revision.
    Revision { revision: String },
    /// The file holds what the evidence showed.
    Digest { file: String, digest: String },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Action {
    pub id: String,
    pub task: String,
    pub revision: String,
    pub evidence: Vec<String>,
    pub operation: Operation,
    pub preconditions: Vec<Precondition>,
    pub authorization: String,
    pub expected: String,
    pub verification: String,
    pub source: Source,
    pub status: Status,
    /// The states it passed through, in order.
    pub path: Vec<Status>,
    /// Why this action, in a sentence.
    pub reason: String,
    pub ms: f64,
    pub result: Value,
}

impl Action {
    /// Moves to `status`, remembering the path.
    pub fn advance(&mut self, status: Status) {
        self.status = status;
        self.path.push(status);
    }

    /// Ends in `Failed`, or `TimedOut` when `why` says the call timed out.
    pub fn fail(&mut self, why: &str) {
        let w = why.to_lowercase();
        self.advance(if w.contains("timed out") || w.contains("timeout") || w.contains("deadline") {
            Status::TimedOut
        } else {
            Status::Failed
        });
    }
}

/// The ledger of one run, appended to one file.
pub struct Ledger {
    task: String,
    path: PathBuf,
    seq: Mutex<u32>,
    file: Mutex<Option<std::fs::File>>,
}

static SECRETS: LazyLock<Vec<(Regex, &'static str)>> = LazyLock::new(|| {
    [
        (r"(?i)bearer\s+[A-Za-z0-9._~+/=-]{8,}", "Bearer <redacted>"),
        (r"\b(?:sk|hk|pk|rk)-[A-Za-z0-9_-]{12,}", "<redacted>"),
        (r"\bgh[pousr]_[A-Za-z0-9]{20,}", "<redacted>"),
        (r"\bAKIA[0-9A-Z]{16}\b", "<redacted>"),
        (r"\beyJ[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}", "<redacted>"),
        (r"(?i)\b([A-Z0-9_]*(?:KEY|TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIAL)[A-Z0-9_]*)=\S+", "$1=<redacted>"),
    ]
    .into_iter()
    .map(|(p, r)| (Regex::new(p).expect("a fixed pattern"), r))
    .collect()
});

/// `text` with credentials replaced.
pub fn redact(text: &str) -> String {
    SECRETS
        .iter()
        .fold(text.to_string(), |t, (re, with)| re.replace_all(&t, *with).into_owned())
}

fn scrub(value: &mut Value) {
    match value {
        Value::String(s) => {
            *s = redact(&crate::decide::cut(s, OUTPUT * 4));
        }
        Value::Array(a) => a.iter_mut().for_each(scrub),
        Value::Object(o) => o.values_mut().for_each(scrub),
        _ => {}
    }
}

/// The first 12 hex digits of the SHA-256 of `bytes`.
pub fn digest(bytes: &[u8]) -> String {
    let h = Sha256::digest(bytes);
    h.iter().take(6).map(|b| format!("{b:02x}")).collect()
}

/// The revision of the files a run has touched: a hash of their contents, so a revert returns to
/// the revision it left, and an unrelated file's change moves nothing.
pub fn revision(root: &Path, files: &[String]) -> String {
    let mut seen: BTreeMap<&str, String> = BTreeMap::new();
    for f in files {
        let bytes = std::fs::read(root.join(f)).unwrap_or_default();
        seen.insert(f, digest(&bytes));
    }
    if seen.is_empty() {
        return "base".into();
    }
    digest(format!("{seen:?}").as_bytes())
}

/// [`revision`] against the files' contents when the run began: the revision of a tree that holds
/// them all again is `base`, so a revert returns to the revision it left.
pub fn revision_over(root: &Path, files: &[String], origin: &BTreeMap<String, String>) -> String {
    let mut seen: BTreeMap<&str, String> = BTreeMap::new();
    let mut same = true;
    for f in files {
        let d = digest(&std::fs::read(root.join(f)).unwrap_or_default());
        same &= origin.get(f) == Some(&d);
        seen.insert(f, d);
    }
    if same {
        return "base".into();
    }
    digest(format!("{seen:?}").as_bytes())
}

/// A failure with what varies from run to run taken out, so the same failure is the same
/// fingerprint: colours, absolute paths, line numbers, addresses and ids, durations.
pub fn fingerprint(output: &str) -> String {
    static NORMAL: LazyLock<Vec<(Regex, &'static str)>> = LazyLock::new(|| {
        [
            (r"\x1b\[[0-9;]*m", ""),
            (r"(?:/[\w.@+-]+){2,}/", "<dir>/"),
            (r":\d+(?::\d+)?\b", ":<n>"),
            (r"\bline \d+\b", "line <n>"),
            (r"\b0x[0-9a-fA-F]+\b", "<addr>"),
            (r"\b[0-9a-f]{8,}\b", "<id>"),
            (r"\b\d+(?:\.\d+)?\s?(?:ms|s|seconds)\b", "<time>"),
            (r"\bin \d+(?:\.\d+)?s\b", "in <time>"),
        ]
        .into_iter()
        .map(|(p, r)| (Regex::new(p).expect("a fixed pattern"), r))
        .collect()
    });
    let text = NORMAL
        .iter()
        .fold(output.to_string(), |t, (re, with)| re.replace_all(&t, *with).into_owned());
    // The words of the failure, in order, without the layout.
    let words: Vec<&str> = text.split_whitespace().collect();
    digest(words.join(" ").as_bytes())
}

impl Ledger {
    pub fn new(task: &str, path: PathBuf) -> Ledger {
        Ledger {
            task: task.to_string(),
            path,
            seq: Mutex::new(0),
            file: Mutex::new(None),
        }
    }

    pub fn task(&self) -> &str {
        &self.task
    }

    /// The next stable id.
    pub fn next(&self) -> String {
        let mut n = self.seq.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
        *n += 1;
        format!("{}.{}", self.task, *n)
    }

    /// An action's skeleton at `revision`, with the next id.
    pub fn action(&self, revision: &str, operation: Operation, source: Source, reason: &str) -> Action {
        Action {
            id: self.next(),
            task: self.task.clone(),
            revision: revision.to_string(),
            evidence: Vec::new(),
            operation,
            preconditions: Vec::new(),
            authorization: "auto: the task's own tools".into(),
            expected: String::new(),
            verification: "the tests".into(),
            source,
            status: Status::Eligible,
            path: vec![Status::Eligible],
            reason: reason.to_string(),
            ms: 0.0,
            result: Value::Null,
        }
    }

    /// Appends a line: redacted, whole, one at a time. A ledger that cannot be written is
    /// logged and skipped: the loop never waits on it.
    pub fn write(&self, kind: &str, body: Value) {
        let mut line = json!({"time": crate::trace::now(), "kind": kind, "task": self.task});
        if let (Some(l), Value::Object(b)) = (line.as_object_mut(), body) {
            l.extend(b);
        }
        scrub(&mut line);
        let Ok(mut text) = serde_json::to_vec(&line) else {
            return;
        };
        text.push(b'\n');
        let mut file = self.file.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
        if file.is_none() {
            if let Some(dir) = self.path.parent() {
                let _ = std::fs::create_dir_all(dir);
            }
            match std::fs::OpenOptions::new().create(true).append(true).open(&self.path) {
                Ok(f) => *file = Some(f),
                Err(e) => {
                    tracing::warn!("kai ledger {}: {e}", self.path.display());
                    return;
                }
            }
        }
        if let Some(f) = file.as_mut()
            && let Err(e) = f.write_all(&text)
        {
            tracing::warn!("kai ledger {}: {e}", self.path.display());
        }
    }

    /// An action, finished: its line.
    pub fn record(&self, action: &Action) {
        if let Ok(v) = serde_json::to_value(action) {
            self.write("action", v);
        }
    }

    /// A step that did not need an inference, or fell from a deterministic one to a model, and
    /// why.
    pub fn fallback(&self, revision: &str, op: &str, from: &str, to: &str, reason: &str) {
        self.write(
            "fallback",
            json!({"id": self.next(), "revision": revision, "op": op, "from": from, "to": to, "reason": reason}),
        );
    }

    /// A verification: the independent check's record, which alone settles an outcome.
    pub fn verify(&self, action: &str, revision: &str, passed: bool, code: i64, fingerprint: &str, output: &str, ms: f64) {
        self.write(
            "verify",
            json!({"id": self.next(), "action": action, "revision": revision, "passed": passed, "code": code,
                   "fingerprint": fingerprint, "output": crate::decide::cut(output, OUTPUT), "ms": ms}),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn credentials_are_redacted_before_they_are_written() {
        let t = "curl -H 'Authorization: Bearer abcdef0123456789' sk-abcdefghijklmnop API_KEY=hunter2 eyJhbGciOiJI.eyJzdWIiOiIx.c2lnbmF0dXJl ghp_abcdefghijklmnopqrstuvwxyz";
        let r = redact(t);
        for leak in ["abcdef0123456789", "sk-abcdefghijklmnop", "hunter2", "eyJhbGciOiJI", "ghp_abc"] {
            assert!(!r.contains(leak), "{leak} in {r}");
        }
        assert!(r.contains("API_KEY=<redacted>"), "{r}");
    }

    #[test]
    fn a_failure_is_the_same_fingerprint_whatever_varies_between_runs() {
        let a = "tests/test_x.py:44: in test_a\n/home/z/w/t1/pkg/m.py:631: TypeError at 0x7f12ab in 0.16s id 9f86d081884c7d65\nE assert 3 == 4";
        let b = "tests/test_x.py:47: in test_a\n/tmp/other/dir/pkg/m.py:640: TypeError at 0x55aa00 in 0.21s id aaaaaaaabbbbbbbb\nE assert 3 == 4";
        assert_eq!(fingerprint(a), fingerprint(b));
        assert_ne!(fingerprint(a), fingerprint("E assert 3 == 5"));
    }

    #[test]
    fn a_revision_is_the_contents_of_the_files_touched_and_a_revert_returns_to_it() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("a.py"), "x = 1\n").unwrap();
        let files = vec!["a.py".to_string()];
        let before = revision(dir.path(), &files);
        std::fs::write(dir.path().join("a.py"), "x = 2\n").unwrap();
        let during = revision(dir.path(), &files);
        std::fs::write(dir.path().join("a.py"), "x = 1\n").unwrap();
        assert_ne!(before, during);
        assert_eq!(before, revision(dir.path(), &files));
        assert_eq!(revision(dir.path(), &[]), "base");
    }

    #[test]
    fn a_revert_returns_to_the_base_revision() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("a.py"), "x = 1\n").unwrap();
        let files = vec!["a.py".to_string()];
        let origin = BTreeMap::from([("a.py".to_string(), digest(b"x = 1\n"))]);
        assert_eq!(revision_over(dir.path(), &files, &origin), "base");
        std::fs::write(dir.path().join("a.py"), "x = 2\n").unwrap();
        let moved = revision_over(dir.path(), &files, &origin);
        assert_ne!(moved, "base");
        std::fs::write(dir.path().join("a.py"), "x = 1\n").unwrap();
        assert_eq!(revision_over(dir.path(), &files, &origin), "base");
    }

    #[test]
    fn every_entry_has_a_stable_id_and_the_lines_are_whole_json() {
        let dir = tempfile::tempdir().unwrap();
        let ledger = Ledger::new("run1", dir.path().join("kai/ledger.jsonl"));
        let mut a = ledger.action("base", Operation::Test { command: "pytest".into() }, Source::Rule, "the task's test command");
        a.advance(Status::Attempted);
        a.advance(Status::Verified);
        a.result = json!({"output": "Authorization: Bearer abcdefghij1234"});
        ledger.record(&a);
        ledger.fallback("base", "site", "rule", "kai", "ranking not confident");
        let text = std::fs::read_to_string(dir.path().join("kai/ledger.jsonl")).unwrap();
        let lines: Vec<Value> = text.lines().map(|l| serde_json::from_str(l).unwrap()).collect();
        assert_eq!(lines[0]["id"], "run1.1");
        assert_eq!(lines[0]["kind"], "action");
        assert_eq!(lines[0]["operation"]["type"], "test");
        assert_eq!(lines[0]["source"], "rule");
        assert_eq!(lines[0]["status"], "verified");
        assert_eq!(lines[0]["path"], json!(["eligible", "attempted", "verified"]));
        assert!(!text.contains("abcdefghij1234"));
        assert_eq!(lines[1]["id"], "run1.2");
        assert_eq!(lines[1]["reason"], "ranking not confident");
    }
}
