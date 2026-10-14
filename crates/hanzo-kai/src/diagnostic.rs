//! What a failing run says, read out of its output: pytest's short and long tracebacks, Python
//! tracebacks, rustc and cargo test, and any `path:line[:col]`.
//!
//! The reading is kai-gateway's `diagnostic.ts`, line for line, so the ranker that was fit on its
//! output ([`crate::locate`]) sees the same frames, tests and names here.

use regex::Regex;
use std::collections::HashSet;
use std::sync::LazyLock;

/// Most identifiers kept.
const NAMES: usize = 64;
/// Most characters of error text kept.
const TEXT: usize = 2_000;

/// One place the output points at.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Frame {
    pub file: String,
    /// 1-based.
    pub line: u32,
    pub symbol: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    /// An assertion is the only evidence.
    Assert,
    /// A raised error other than an assertion.
    Exception,
    /// rustc errors.
    Compile,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub kind: Kind,
    /// In the order the output names them; relative to the tree when under it.
    pub frames: Vec<Frame>,
    /// Failing test ids and names, as the runner printed them.
    pub tests: Vec<String>,
    /// Identifiers on the failing source lines and in the error messages, first spelling kept.
    pub names: Vec<String>,
    /// The error lines themselves, at most 2,000 characters.
    pub text: String,
}

impl Diagnostic {
    /// Whether the output holds a diagnostic at all: a failure of a known kind, a frame, or a
    /// failing test.
    pub fn found(&self) -> bool {
        self.kind != Kind::Unknown || !self.frames.is_empty() || !self.tests.is_empty()
    }
}

fn re(pattern: &str) -> Regex {
    Regex::new(pattern).expect("a fixed pattern")
}

static PYTEST_FRAME: LazyLock<Regex> = LazyLock::new(|| {
    re(r"^(\S+\.py):(\d+):(?:\s+(?:in\s+(\S+)|([A-Za-z_][\w.]*)))?\s*$")
});
static TRACEBACK: LazyLock<Regex> =
    LazyLock::new(|| re(r#"^\s*File "([^"]+)", line (\d+)(?:, in (\S+))?"#));
static RUSTC_LEVEL: LazyLock<Regex> =
    LazyLock::new(|| re(r"^(error|warning|help|note)(?:\[(E\d+)\])?: (.*)$"));
static RUSTC_SPAN: LazyLock<Regex> = LazyLock::new(|| re(r"^\s*--> (\S+?):(\d+):(\d+)\s*$"));
static RUSTC_CODE: LazyLock<Regex> = LazyLock::new(|| re(r"^\s*\d+\s*\|(.*)$"));
static RUSTC_LABEL: LazyLock<Regex> = LazyLock::new(|| re(r"^\s*(?:=|\|)"));
static PANIC: LazyLock<Regex> = LazyLock::new(|| {
    re(r"^thread '([^']+)'(?: \(\d+\))? panicked at (?:'(.*)', )?(\S+?):(\d+):(\d+):?\s*$")
});
static CARGO_TEST: LazyLock<Regex> = LazyLock::new(|| {
    re(r"^(?:---- (\S+) stdout ----|(?:test )?(\S+) (?:\.\.\.|---) FAILED)\s*$")
});
static PYTEST_SUMMARY: LazyLock<Regex> =
    LazyLock::new(|| re(r"^(?:FAILED|ERROR) (\S+?)(?: - (.*))?\s*$"));
static PYTEST_HEADER: LazyLock<Regex> = LazyLock::new(|| re(r"^_{3,} (\S+) _{3,}\s*$"));
static E_LINE: LazyLock<Regex> = LazyLock::new(|| re(r"^E\s+(.*)$"));
static RAISED: LazyLock<Regex> = LazyLock::new(|| {
    re(r"^([A-Za-z_][\w.]*(?:Error|Exception|Exit|Interrupt|Failure))(?::\s.*)?$")
});
static GENERIC: LazyLock<Regex> = LazyLock::new(|| {
    re(r#"(?:^|[\s("'`=])((?:[\w.-]+/)*[\w.-]*[\w-]\.[A-Za-z][A-Za-z0-9]{0,7}):(\d+)(?::\d+)?"#)
});
static IDENT: LazyLock<Regex> = LazyLock::new(|| re(r"[A-Za-z_][A-Za-z0-9_]*"));
static ASSERTIVE: LazyLock<Regex> =
    LazyLock::new(|| re(r"^(?:assert\b|AssertionError\b|assertion\b)"));
static ERROR_CODE: LazyLock<Regex> = LazyLock::new(|| re(r"^e\d+$"));
static INDENTED: LazyLock<Regex> = LazyLock::new(|| re(r"^\s+\S"));

static COUNT: LazyLock<Regex> = LazyLock::new(|| re(r"\b(\d+) (?:failed|failures?|errors?)\b"));

/// How many tests a run reports failing: the numbers on the runner's last summary line that
/// counts them (pytest's `2 failed, 3 passed`, cargo's `test result: FAILED. 3 passed; 2 failed`),
/// else the failing tests it names. Zero when it says neither.
pub fn failures(output: &str) -> usize {
    let counted = output.lines().rev().find(|l| COUNT.is_match(l));
    match counted {
        Some(line) => COUNT.captures_iter(line).filter_map(|c| c[1].parse::<usize>().ok()).sum(),
        None => parse(output, None).tests.iter().filter(|t| t.contains("::") || t.contains(' ')).count(),
    }
}

static ERRORS: LazyLock<Regex> = LazyLock::new(|| re(r"\b(\d+) errors?\b"));
static BROKEN: LazyLock<Regex> = LazyLock::new(|| re(r"(?m)^(?:E\s+)?(?:SyntaxError|IndentationError|TabError)\b"));

/// How many of the failures are errors (a test that could not run: an import, a fixture) and not
/// failed assertions: pytest's `1 error`.
pub fn errors(output: &str) -> usize {
    output
        .lines()
        .rev()
        .find(|l| COUNT.is_match(l))
        .map_or(0, |line| ERRORS.captures_iter(line).filter_map(|c| c[1].parse::<usize>().ok()).sum())
}

/// Whether the run could not even read the code: a syntax or indentation error. An edit that
/// makes it so is no repair, however few tests the runner then counts.
pub fn broken(output: &str) -> bool {
    BROKEN.is_match(output)
}

static SECONDS: LazyLock<Regex> = LazyLock::new(|| re(r"\bin (\d+(?:\.\d+)?)s\b"));

/// The seconds the runner says the run took (`1 failed in 0.16s`), when it says.
pub fn seconds(output: &str) -> Option<f64> {
    output
        .lines()
        .rev()
        .take(6)
        .find_map(|l| SECONDS.captures(l))
        .and_then(|c| c[1].parse().ok())
}

/// Keywords and the runners' own words: on every failing line, and no help in finding the fix.
const NOISE: &[&str] = &[
    "and", "as", "assert", "async", "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda",
    "nonlocal", "not", "or", "pass", "raise", "return", "try", "while", "with", "yield", "none",
    "true", "false", "self", "cls", "fn", "let", "mut", "impl", "pub", "use", "mod", "struct",
    "enum", "match", "ref", "move", "crate", "super", "loop", "const", "static", "type", "unsafe",
    "dyn", "trait", "where", "assertionerror", "assertion", "failed", "left", "right", "thread",
    "panicked", "error", "warning", "note", "the", "an", "to", "of", "at", "by", "be", "on", "it",
    "this", "that", "but", "was", "were", "are",
];

fn assertive(message: &str) -> bool {
    ASSERTIVE.is_match(message.trim())
}

/// A path as the tree spells it: the tree's own absolute prefix and `./` dropped.
fn relative(file: &str, root: Option<&str>) -> String {
    let mut path = file;
    if let Some(root) = root {
        let base = format!("{}/", root.trim_end_matches('/'));
        if let Some(rest) = path.strip_prefix(&base) {
            path = rest;
        }
    }
    let mut path = path;
    while let Some(rest) = path.strip_prefix("./") {
        path = rest;
    }
    path.to_string()
}

/// The parse in progress.
#[derive(Default)]
struct Reading {
    frames: Vec<Frame>,
    tests: Vec<String>,
    names: Vec<String>,
    messages: Vec<String>,
    seen_frames: HashSet<String>,
    seen_tests: HashSet<String>,
    seen_names: HashSet<String>,
    seen_messages: HashSet<String>,
    asserts: bool,
    exceptions: bool,
    compile: bool,
}

impl Reading {
    fn frame(&mut self, file: &str, line: &str, symbol: Option<&str>, root: Option<&str>) {
        let line: u32 = line.parse().unwrap_or(0);
        let at = Frame {
            file: relative(file, root),
            line,
            symbol: symbol.map(str::to_string),
        };
        let key = format!("{}:{}", at.file, at.line);
        if at.line < 1 || !self.seen_frames.insert(key) {
            return;
        }
        self.frames.push(at);
    }

    fn test(&mut self, id: &str) {
        if self.seen_tests.insert(id.to_string()) {
            self.tests.push(id.to_string());
        }
    }

    fn named(&mut self, line: &str) {
        for found in IDENT.find_iter(line) {
            let name = found.as_str();
            let lower = name.to_lowercase();
            // rustc's error codes (E0308) name no code of the tree's.
            if self.names.len() >= NAMES
                || name.len() < 2
                || NOISE.contains(&lower.as_str())
                || self.seen_names.contains(&lower)
                || ERROR_CODE.is_match(&lower)
            {
                continue;
            }
            self.seen_names.insert(lower);
            self.names.push(name.to_string());
        }
    }

    fn message(&mut self, line: &str) {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            return;
        }
        self.named(trimmed);
        if self.seen_messages.insert(trimmed.to_string()) {
            self.messages.push(trimmed.to_string());
        }
    }

    /// An error the run reported: an assertion, or any other raised type.
    fn classify(&mut self, line: &str) {
        if assertive(line) {
            self.asserts = true;
        } else if RAISED.is_match(line.trim()) {
            self.exceptions = true;
        }
    }
}

/// The diagnostic in `text`; paths under `root` are made relative to it.
pub fn parse(text: &str, root: Option<&str>) -> Diagnostic {
    let mut r = Reading::default();
    // rustc's level for the spans and code lines under its header: only an error's are the
    // failure. An `error:` without a code counts once a span follows it.
    let mut level: Option<bool> = None; // Some(true): error, Some(false): other
    let mut pending: Option<String> = None;
    // pytest and Python print the failing source line right after a frame.
    let mut code_next = false;
    // A panic's message follows it, up to a blank line; its first line says what kind it was.
    let mut panic: Option<bool> = None; // Some(true): first, Some(false): rest
    for line in text.split('\n') {
        let line = line.strip_suffix('\r').unwrap_or(line);
        let after_frame = code_next;
        code_next = false;

        if let Some(m) = RUSTC_LEVEL.captures(line) {
            let error = &m[1] == "error";
            level = Some(error);
            pending = None;
            if error && m.get(2).is_some() {
                r.compile = true;
                r.message(line);
            } else if error {
                pending = Some(line.to_string());
            }
            continue;
        }
        if let Some(m) = RUSTC_SPAN.captures(line) {
            if level == Some(true) {
                r.compile = true;
                r.frame(&m[1], &m[2], None, root);
                if let Some(p) = pending.take() {
                    r.message(&p);
                }
            }
            continue;
        }
        if level.is_some()
            && let Some(m) = RUSTC_CODE.captures(line)
        {
            if level == Some(true) {
                r.named(&m[1]);
            }
            continue;
        }
        if level.is_some() && RUSTC_LABEL.is_match(line) {
            continue;
        }
        if line.trim().is_empty() {
            level = None;
            pending = None;
        }

        if let Some(m) = PANIC.captures(line) {
            if &m[1] != "main" {
                r.test(&m[1]);
            }
            r.frame(&m[3], &m[4], None, root);
            match m.get(2) {
                None => panic = Some(true),
                Some(msg) => {
                    if assertive(msg.as_str()) {
                        r.asserts = true;
                    } else {
                        r.exceptions = true;
                    }
                    r.message(msg.as_str());
                }
            }
            continue;
        }
        if let Some(first) = panic {
            if line.trim().is_empty() || line.starts_with("note:") {
                panic = None;
            } else {
                if first {
                    if assertive(line) {
                        r.asserts = true;
                    } else {
                        r.exceptions = true;
                    }
                }
                panic = Some(false);
                r.message(line);
                continue;
            }
        }
        if let Some(m) = CARGO_TEST.captures(line) {
            let id = m.get(1).or_else(|| m.get(2)).map_or("", |g| g.as_str());
            r.test(id);
            continue;
        }
        if let Some(m) = PYTEST_FRAME.captures(line) {
            r.frame(&m[1], &m[2], m.get(3).map(|g| g.as_str()), root);
            if let Some(kind) = m.get(4) {
                r.classify(kind.as_str());
            }
            code_next = m.get(3).is_some();
            continue;
        }
        if let Some(m) = TRACEBACK.captures(line) {
            r.frame(&m[1], &m[2], m.get(3).map(|g| g.as_str()), root);
            code_next = true;
            continue;
        }
        if let Some(m) = PYTEST_SUMMARY.captures(line) {
            r.test(&m[1]);
            if let Some(msg) = m.get(2) {
                r.classify(msg.as_str());
                r.message(msg.as_str());
            }
            continue;
        }
        if let Some(m) = PYTEST_HEADER.captures(line) {
            r.test(&m[1]);
            continue;
        }
        if let Some(m) = E_LINE.captures(line) {
            r.classify(&m[1]);
            r.message(&m[1]);
            continue;
        }
        // pytest's long traceback marks the failing line with `>`; the short one and Python's
        // put it after the frame.
        if line.starts_with('>') || (after_frame && INDENTED.is_match(line)) {
            r.named(line.strip_prefix('>').unwrap_or(line));
            continue;
        }
        if RAISED.is_match(line) {
            r.classify(line);
            r.message(line);
            continue;
        }
        for m in GENERIC.captures_iter(line) {
            // The path is not followed by a word character or a slash.
            let end = m.get(0).map_or(0, |g| g.end());
            let next = line[end..].chars().next();
            if next.is_some_and(|c| c.is_alphanumeric() || c == '_' || c == '/') {
                continue;
            }
            r.frame(&m[1], &m[2], None, root);
        }
    }
    let kind = if r.compile {
        Kind::Compile
    } else if r.exceptions {
        Kind::Exception
    } else if r.asserts {
        Kind::Assert
    } else {
        Kind::Unknown
    };
    let mut text = r.messages.join("\n");
    if let Some((at, _)) = text.char_indices().nth(TEXT) {
        text.truncate(at);
    }
    Diagnostic {
        kind,
        frames: r.frames,
        tests: r.tests,
        names: r.names,
        text,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn errors_and_syntax_errors_are_told_from_failed_tests() {
        assert_eq!(errors("1 failed, 2 passed, 1 error in 0.1s"), 1);
        assert_eq!(errors("2 failed, 4 passed in 0.1s"), 0);
        assert!(broken("  File \"m.py\", line 3\n    for ch not in value\nSyntaxError: invalid syntax\n"));
        assert!(broken("E   IndentationError: unexpected indent"));
        assert!(!broken("E   AssertionError: no SyntaxError here"));
    }

    #[test]
    fn the_runners_own_seconds() {
        assert_eq!(seconds("....F\n1 failed, 4 passed in 0.16s"), Some(0.16));
        assert_eq!(seconds("== 2 failed in 12s =="), Some(12.0));
        assert_eq!(seconds("no time here"), None);
    }

    #[test]
    fn failures_are_the_runners_own_count() {
        assert_eq!(failures("....F.\n== 2 failed, 3 passed in 0.1s =="), 2);
        assert_eq!(failures("1 failed, 4 passed, 1 error in 0.16s"), 2);
        assert_eq!(failures("test result: FAILED. 3 passed; 2 failed; 0 ignored"), 2);
        assert_eq!(failures("all good"), 0);
    }

    #[test]
    fn a_pytest_short_traceback_names_its_frames_tests_and_error() {
        let out = "....F   [100%]\n=== FAILURES ===\n___ test_histogram_zero_interquartile_range ___\ntests/test_statsutils.py:44: in test_histogram_zero_interquartile_range\n    counts = Stats(data).get_histogram_counts()\n             ^^^^^^^^^^^^^^^^^^^^^^\nboltons/statsutils.py:631: in get_histogram_counts\n    idxs = [bisect.bisect(d, bins) - 1 for d in self.data]\nE   TypeError: object of type 'int' has no len()\n=== short test summary info ===\nFAILED tests/test_statsutils.py::test_histogram_zero_interquartile_range - Ty...\n1 failed, 4 passed in 0.16s\n";
        let d = parse(out, None);
        assert_eq!(d.kind, Kind::Exception);
        let frames: Vec<_> = d.frames.iter().map(|f| (f.file.as_str(), f.line)).collect();
        assert_eq!(
            frames,
            vec![("tests/test_statsutils.py", 44), ("boltons/statsutils.py", 631)]
        );
        assert_eq!(
            d.tests,
            vec![
                "test_histogram_zero_interquartile_range",
                "tests/test_statsutils.py::test_histogram_zero_interquartile_range"
            ]
        );
        assert!(d.names.iter().any(|n| n == "get_histogram_counts"));
        assert!(d.text.contains("TypeError: object of type 'int' has no len()"));
    }

    #[test]
    fn rustc_errors_are_a_compile_failure_at_their_span() {
        let out = "error[E0308]: mismatched types\n --> src/lib.rs:7:5\n  |\n7 |     1u8\n  |     ^^^ expected `u32`\n\nwarning: unused\n --> src/other.rs:1:1\n";
        let d = parse(out, None);
        assert_eq!(d.kind, Kind::Compile);
        assert_eq!(d.frames.len(), 1);
        assert_eq!((d.frames[0].file.as_str(), d.frames[0].line), ("src/lib.rs", 7));
    }

    #[test]
    fn a_panic_is_an_exception_unless_it_asserts() {
        let out = "---- add::tests::sums stdout ----\nthread 'add::tests::sums' (12) panicked at src/add.rs:9:5:\nassertion `left == right` failed\n  left: 2\n right: 3\n";
        let d = parse(out, None);
        assert_eq!(d.kind, Kind::Assert);
        assert_eq!(d.tests, vec!["add::tests::sums"]);
        assert_eq!((d.frames[0].file.as_str(), d.frames[0].line), ("src/add.rs", 9));
    }

    #[test]
    fn a_path_and_line_anywhere_is_a_frame_but_not_a_longer_word() {
        let d = parse("see src/x.py:12 and src/y.py:3x\n", None);
        let frames: Vec<_> = d.frames.iter().map(|f| (f.file.as_str(), f.line)).collect();
        assert_eq!(frames, vec![("src/x.py", 12)]);
    }

    #[test]
    fn the_trees_absolute_prefix_is_dropped() {
        let d = parse("/w/t/pkg/a.py:5: in f\n", Some("/w/t"));
        assert_eq!(d.frames[0].file, "pkg/a.py");
    }
}
