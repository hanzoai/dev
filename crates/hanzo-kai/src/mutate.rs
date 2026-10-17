//! Candidate edits at the best sites, tried against the visible test.
//!
//! A single fault in code is often one small change from the code that works: a comparison
//! flipped, an operator swapped, a constant off by one, two arguments exchanged, a condition
//! inverted, a `return` dropped. [`propose`] lists those changes, generic transforms of a line's
//! tokens that name no repository, file or task, at the lines of the ranked sites, best first.
//! [`search`] tries them in order, several at once, each in its own copy of the tree, against the
//! one test command the task shows, and stops at the first that passes. It never sees a hidden
//! test, and it gives up at a time cap so the model writes the edit instead; while the model
//! asked beside it has not answered, that wait is the search's, to a longer cap.
//!
//! What this can solve is a single fault of that kind. It is a search, not a repair of anything
//! else, and a candidate that passes the visible test is only as right as that test.

use crate::diagnostic::Diagnostic;
use crate::locate::Plan;
use crate::symbols::Tree;
use crate::tools;
use std::collections::HashSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Mutex;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

/// Functions whose lines are tried.
const SITES: usize = 4;
/// Most candidates a site contributes.
const PER_SITE: usize = 96;
/// Most candidates in all.
const CANDIDATES: usize = 160;
/// Most tests run at once.
const WORKERS: usize = 8;
/// A tree larger than this is not copied for a search.
const TREE: u64 = 128 * 1024 * 1024;
/// A file larger than this is not copied.
const FILE: u64 = 8 * 1024 * 1024;
/// Lines of a function considered.
const LINES: u32 = 150;
/// The longest a search runs past its cap while the model it stands beside has not answered.
pub const LONGEST: Duration = Duration::from_secs(8);

/// One change to one line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Candidate {
    pub file: String,
    /// 1-based.
    pub line: u32,
    /// The line as it is, and as the candidate would have it.
    pub old: String,
    pub new: String,
    pub kind: &'static str,
    /// The rank of the site it is at.
    pub site: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
    Word,
    Num,
    Str,
    Op,
    Open,
    Close,
    Comma,
}

#[derive(Debug, Clone)]
struct Token {
    kind: Tok,
    start: usize,
    end: usize,
}

const OPS: &[&str] = &[
    "**=", "//=", ">>=", "<<=", "==", "!=", "<=", ">=", "->", "=>", "**", "//", "<<", ">>", "+=", "-=",
    "*=", "/=", "%=", "&&", "||", ":=", "<>",
];

/// A line's tokens, over its masked text: comments blank and string contents filled.
fn scan(m: &[u8]) -> Vec<Token> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < m.len() {
        let c = m[i];
        if c == b' ' || c == b'\t' || c == b'\r' {
            i += 1;
        } else if c == 0 || c == b'"' || c == b'\'' || c == b'`' {
            let start = i;
            i += 1;
            while i < m.len() && m[i] == 0 {
                i += 1;
            }
            out.push(Token { kind: Tok::Str, start, end: i });
        } else if c.is_ascii_alphabetic() || c == b'_' {
            let start = i;
            while i < m.len() && (m[i].is_ascii_alphanumeric() || m[i] == b'_') {
                i += 1;
            }
            out.push(Token { kind: Tok::Word, start, end: i });
        } else if c.is_ascii_digit() {
            let start = i;
            while i < m.len() && (m[i].is_ascii_alphanumeric() || m[i] == b'_' || m[i] == b'.') {
                i += 1;
            }
            out.push(Token { kind: Tok::Num, start, end: i });
        } else if matches!(c, b'(' | b'[' | b'{') {
            out.push(Token { kind: Tok::Open, start: i, end: i + 1 });
            i += 1;
        } else if matches!(c, b')' | b']' | b'}') {
            out.push(Token { kind: Tok::Close, start: i, end: i + 1 });
            i += 1;
        } else if c == b',' {
            out.push(Token { kind: Tok::Comma, start: i, end: i + 1 });
            i += 1;
        } else {
            let rest = &m[i..];
            let len = OPS
                .iter()
                .find(|op| rest.starts_with(op.as_bytes()))
                .map_or(1, |op| op.len());
            out.push(Token { kind: Tok::Op, start: i, end: i + len });
            i += len;
        }
    }
    out
}

/// Whether a token ends an operand: a name, a number, a string or a closing bracket.
fn operand_end(t: Option<&Token>) -> bool {
    t.is_some_and(|t| matches!(t.kind, Tok::Word | Tok::Num | Tok::Str | Tok::Close))
}

fn operand_start(t: Option<&Token>) -> bool {
    t.is_some_and(|t| matches!(t.kind, Tok::Word | Tok::Num | Tok::Str | Tok::Open))
}

const KEYWORDS: &[&str] = &[
    "and", "or", "not", "is", "in", "if", "elif", "else", "while", "for", "return", "lambda", "yield",
    "await", "assert", "del", "with", "as", "from", "import", "raise", "except", "print",
];

fn is_keyword(w: &str) -> bool {
    KEYWORDS.contains(&w)
}

const DUAL: &[(&str, &str)] = &[
    ("min", "max"), ("any", "all"), ("startswith", "endswith"), ("lstrip", "rstrip"),
    ("lower", "upper"), ("ljust", "rjust"), ("first", "last"), ("keys", "values"),
    ("True", "False"), ("true", "false"),
];

/// The changes to the tokens of one line: `(kind, start, end, replacement)` over the line's text.
fn changes(line: &str, m: &[u8], python: bool) -> Vec<(&'static str, usize, usize, String)> {
    let tokens = scan(m);
    let text = |t: &Token| &line[t.start..t.end];
    let mut out: Vec<(&'static str, usize, usize, String)> = Vec::new();
    let first = tokens.first().map(|t| text(t));
    // A `for` waiting for its `in`: that `in` is a loop's, not a membership test.
    let mut looping = false;
    let condition = python && matches!(first, Some("if" | "elif" | "while")) && line.trim_end().ends_with(':');
    for (i, t) in tokens.iter().enumerate() {
        let prev = i.checked_sub(1).and_then(|p| tokens.get(p));
        let next = tokens.get(i + 1);
        let s = text(t);
        match t.kind {
            Tok::Op => {
                let binary = operand_end(prev) && operand_start(next);
                let swaps: &[&str] = match s {
                    "==" => &["!="],
                    "!=" => &["=="],
                    "<" if binary => &["<=", ">"],
                    ">" if binary => &[">=", "<"],
                    "<=" => &["<", ">="],
                    ">=" => &[">", "<="],
                    _ => &[],
                };
                for r in swaps {
                    out.push(("compare", t.start, t.end, (*r).to_string()));
                }
                let arith: &[&str] = match s {
                    "+" if binary => &["-", "*", "/", "//", "%"],
                    "-" if binary => &["+", "*", "/", "//", "%"],
                    "*" if binary => &["/", "+", "-", "//", "%"],
                    "/" if binary => &["*", "//", "+", "-", "%"],
                    "//" if binary => &["/", "*", "+", "-", "%"],
                    "%" if binary => &["*", "//", "/", "+", "-"],
                    "**" if binary => &["*"],
                    "+=" => &["-="],
                    "-=" => &["+="],
                    "*=" => &["/="],
                    "/=" => &["*="],
                    "&&" => &["||"],
                    "||" => &["&&"],
                    _ => &[],
                };
                for r in arith {
                    out.push(("operator", t.start, t.end, (*r).to_string()));
                }
            }
            Tok::Word => {
                if python {
                    match s {
                        "and" => out.push(("boolean", t.start, t.end, "or".into())),
                        "or" => out.push(("boolean", t.start, t.end, "and".into())),
                        "not" if next.map(|n| text(n)) != Some("in") => {
                            let end = next.map_or(t.end, |n| n.start);
                            out.push(("boolean", t.start, end, String::new()));
                        }
                        "is" => match next.map(|n| text(n)) {
                            Some("not") => out.push(("boolean", t.start, next.map_or(t.end, |n| n.end), "is".into())),
                            _ => out.push(("boolean", t.start, t.end, "is not".into())),
                        },
                        "in" => {
                            let membership = !looping && !prev.is_some_and(|p| text(p) == "not");
                            looping = false;
                            if membership {
                                out.push(("boolean", t.start, t.end, "not in".into()));
                            }
                        }
                        "for" => looping = true,
                        _ => {}
                    }
                    if s == "not" && next.map(|n| text(n)) == Some("in") {
                        out.push(("boolean", t.start, next.map_or(t.end, |n| n.end), "in".into()));
                    }
                }
                if let Some((_, b)) = DUAL.iter().find(|(a, _)| *a == s) {
                    out.push(("dual", t.start, t.end, (*b).to_string()));
                } else if let Some((a, _)) = DUAL.iter().find(|(_, b)| *b == s) {
                    out.push(("dual", t.start, t.end, (*a).to_string()));
                }
            }
            Tok::Num => {
                let digits = s.replace('_', "");
                if let Ok(v) = digits.parse::<u64>()
                    && !prev.is_some_and(|p| text(p) == ".")
                {
                    out.push(("constant", t.start, t.end, (v + 1).to_string()));
                    if v > 0 {
                        out.push(("constant", t.start, t.end, (v - 1).to_string()));
                    }
                } else if s.contains('.')
                    && let Ok(v) = digits.parse::<f64>()
                    && v.is_finite()
                    && v != 0.0
                {
                    // A float doubled or halved.
                    out.push(("constant", t.start, t.end, format!("{:?}", v / 2.0)));
                    out.push(("constant", t.start, t.end, format!("{:?}", v * 2.0)));
                }
            }
            _ => {}
        }
        // len(x) and range(..., n) are off by one when their result is.
        if t.kind == Tok::Word
            && matches!(s, "len" | "range")
            && next.is_some_and(|n| n.kind == Tok::Open && text(n) == "(")
        {
            if let Some(close) = matching(&tokens, i + 1) {
                let end = tokens[close].end;
                let at = if s == "range" { tokens[close].start } else { end };
                for r in [" - 1", " + 1"] {
                    out.push(("bound", at, at, r.to_string()));
                }
            }
        }
        // f(a, b): the first two arguments exchanged, and the last two.
        if t.kind == Tok::Word
            && !is_keyword(s)
            && next.is_some_and(|n| n.kind == Tok::Open && text(n) == "(")
            && let Some(close) = matching(&tokens, i + 1)
        {
            let args = split(&tokens, i + 1, close);
            let plain = args.iter().all(|(a, b)| !has_keyword_argument(&tokens, line, *a, *b));
            if args.len() >= 2 && plain {
                let swap = |x: usize, y: usize| -> (usize, usize, String) {
                    let (xs, xe) = (tokens[args[x].0].start, tokens[args[x].1].end);
                    let (ys, ye) = (tokens[args[y].0].start, tokens[args[y].1].end);
                    (xs, ye, format!("{}{}{}", &line[ys..ye], &line[xe..ys], &line[xs..xe]))
                };
                let (a, b, r) = swap(0, 1);
                out.push(("arguments", a, b, r));
                if args.len() >= 3 {
                    let (a, b, r) = swap(args.len() - 2, args.len() - 1);
                    out.push(("arguments", a, b, r));
                }
            }
        }
    }
    if condition {
        let body = line.trim_end();
        let kw = first.unwrap_or("if");
        let at = line.find(kw).unwrap_or(0) + kw.len();
        let cond = body[at..body.len() - 1].trim();
        if let Some(rest) = cond.strip_prefix("not ") {
            let rest = rest.trim();
            let rest = rest.strip_prefix('(').and_then(|r| r.strip_suffix(')')).unwrap_or(rest);
            out.push(("condition", at, body.len() - 1, format!(" {rest}")));
        } else if !cond.is_empty() {
            out.push(("condition", at, body.len() - 1, format!(" not ({cond})")));
        }
    }
    out
}

/// The index of the closing bracket matching the opening one at `open`.
fn matching(tokens: &[Token], open: usize) -> Option<usize> {
    let mut depth = 0;
    for (i, t) in tokens.iter().enumerate().skip(open) {
        match t.kind {
            Tok::Open => depth += 1,
            Tok::Close => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

/// The arguments of the call opened at `open`: token index ranges `(first, last)`.
fn split(tokens: &[Token], open: usize, close: usize) -> Vec<(usize, usize)> {
    let mut args = Vec::new();
    let (mut depth, mut from) = (0, open + 1);
    for i in open + 1..close {
        match tokens[i].kind {
            Tok::Open => depth += 1,
            Tok::Close => depth -= 1,
            Tok::Comma if depth == 0 => {
                if i > from {
                    args.push((from, i - 1));
                }
                from = i + 1;
            }
            _ => {}
        }
    }
    if close > from {
        args.push((from, close - 1));
    }
    args
}

/// Whether the argument spanning tokens `a..=b` is `name=value`.
fn has_keyword_argument(tokens: &[Token], line: &str, a: usize, b: usize) -> bool {
    tokens.len() > a + 1 && a < b && &line[tokens[a + 1].start..tokens[a + 1].end] == "=" && tokens[a].kind == Tok::Word
}

/// What a line says of the failure: where the output points, and the names it uses.
struct Focus<'a> {
    frames: HashSet<(String, u32)>,
    names: HashSet<String>,
    d: &'a Diagnostic,
}

impl Focus<'_> {
    fn score(&self, file: &str, number: u32, line: &str, frame: Option<u32>) -> f64 {
        let mut s = 0.0;
        if self.frames.contains(&(file.to_string(), number)) {
            s += 100.0;
        }
        for w in line.split(|c: char| !(c.is_alphanumeric() || c == '_')) {
            if !w.is_empty() && self.names.contains(w) {
                s += 10.0;
            }
        }
        if let Some(f) = frame {
            s -= f64::from(f.abs_diff(number)) * 0.01;
        }
        s
    }
}

/// The candidates at the best sites of `plan`, best first.
pub fn propose(tree: &mut Tree, plan: &Plan, order: &[usize]) -> Vec<Candidate> {
    let focus = Focus {
        frames: plan.diagnostic.frames.iter().map(|f| (f.file.clone(), f.line)).collect(),
        names: plan.diagnostic.names.iter().cloned().collect(),
        d: &plan.diagnostic,
    };
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for (rank, r) in order
        .iter()
        .map(|i| &plan.ranked[*i])
        .filter(|r| r.sym.is_some() && !crate::symbols::is_test(&r.file))
        .take(SITES)
        .enumerate()
    {
        let Some(sym) = &r.sym else { continue };
        let python = sym.file.ends_with(".py");
        let Some(text) = tree.read(&sym.file).map(str::to_string) else {
            continue;
        };
        let Some(masked) = crate::symbols::masked(&sym.file, &text) else {
            continue;
        };
        let masked_lines: Vec<&[u8]> = masked.split(|b| *b == b'\n').collect();
        let lines: Vec<&str> = text.split('\n').collect();
        let frame = focus
            .d
            .frames
            .iter()
            .find(|f| f.file == sym.file && sym.start <= f.line && f.line <= sym.end)
            .map(|f| f.line);
        let mut site: Vec<(f64, Candidate)> = Vec::new();
        let last = sym.end.min(sym.start + LINES - 1);
        for number in sym.start..=last {
            let (Some(line), Some(m)) = (lines.get(number as usize - 1), masked_lines.get(number as usize - 1)) else {
                break;
            };
            let line = line.trim_end_matches('\r');
            let trimmed = line.trim_start();
            if trimmed.is_empty() || trimmed.starts_with(['#', '@']) || trimmed.starts_with("import ") || trimmed.starts_with("from ") {
                continue;
            }
            let base = focus.score(&sym.file, number, line, frame);
            for (n, (kind, a, b, replacement)) in changes(line, m, python).into_iter().enumerate() {
                if !line.is_char_boundary(a) || !line.is_char_boundary(b) {
                    continue;
                }
                let new = format!("{}{}{}", &line[..a], replacement, &line[b..]);
                if new == line || !seen.insert((sym.file.clone(), number, new.clone())) {
                    continue;
                }
                // Earlier changes of a line before later ones.
                site.push((base - n as f64 * 0.001, Candidate { file: sym.file.clone(), line: number, old: line.to_string(), new, kind, site: rank }));
            }
            // A statement that is a bare expression has dropped its `return`; a call may be
            // meant for its effect, so only a function's last statement may be one.
            if python && indent(line) > 0 {
                let body = trimmed;
                let keyword = ["return", "raise", "pass", "break", "continue", "yield", "del", "assert", "print", "with", "for", "while", "if", "else", "elif", "try", "except", "finally", "def", "class", "global", "nonlocal"]
                    .iter()
                    .any(|k| body.starts_with(k) && !body[k.len()..].starts_with(|c: char| c.is_alphanumeric() || c == '_'));
                let bare = !body.starts_with(['#', '@', ')', ']', '}', '"', '\''])
                    && !keyword
                    && !has_assignment(body)
                    && !body.ends_with([':', ',', '\\'])
                    && balance_zero(body);
                // A call may be meant for its effect: it is tried after the lines that say more.
                let call = body.ends_with(')');
                if bare {
                    let pad = &line[..line.len() - trimmed.len()];
                    let new = format!("{pad}return {body}");
                    if seen.insert((sym.file.clone(), number, new.clone())) {
                        let rate = if call && number != sym.end { base - 50.0 } else { base };
                        site.push((rate, Candidate { file: sym.file.clone(), line: number, old: line.to_string(), new, kind: "return", site: rank }));
                    }
                }
            }
        }
        site.sort_by(|a, b| b.0.total_cmp(&a.0));
        out.extend(site.into_iter().take(PER_SITE).map(|(_, c)| c));
        if out.len() >= CANDIDATES {
            break;
        }
    }
    out.truncate(CANDIDATES);
    out
}

fn indent(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

/// Whether every bracket the text opens it closes.
fn balance_zero(body: &str) -> bool {
    body.bytes().fold(0i32, |n, b| match b {
        b'(' | b'[' | b'{' => n + 1,
        b')' | b']' | b'}' => n - 1,
        _ => n,
    }) == 0
}

/// Whether a statement assigns: an `=` outside brackets that is not part of a comparison.
fn has_assignment(body: &str) -> bool {
    let b = body.as_bytes();
    let mut depth = 0;
    for i in 0..b.len() {
        match b[i] {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth -= 1,
            b'=' if depth == 0 => {
                let before = i.checked_sub(1).map(|p| b[p]);
                let after = b.get(i + 1);
                if !matches!(before, Some(b'=' | b'!' | b'<' | b'>')) && after != Some(&b'=') {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

/// pytest's options whose value is the next word.
const VALUED: &[&str] = &[
    "-p", "-k", "-m", "-o", "-c", "-W", "-r", "--rootdir", "--deselect", "--ignore", "--ignore-glob", "--confcutdir", "--basetemp",
    "--override-ini", "--config-file", "--junitxml", "--junit-xml", "--tb", "--maxfail", "--durations", "--log-file", "--import-mode",
];

/// The command a candidate is tried with when the visible one is too slow to try candidate after
/// candidate: a plain pytest command narrowed to the tests the failing `output` names, its paths
/// and node ids in the tree at `root` replaced by theirs. Only those tests can tell a candidate
/// that mends the fault from one that does not, so the rest of their files need not run; the
/// candidate the search picks is verified with the whole command in the tree all the same.
/// `None` for a command that is not plain pytest or an output that names no failing test.
pub fn screen(root: &Path, command: &str, output: &str) -> Option<String> {
    if command.contains(['"', '\'', '\\']) {
        return None;
    }
    let mut tokens = command.split_whitespace();
    let mut kept: Vec<&str> = Vec::new();
    let mut pytest = false;
    for t in tokens.by_ref() {
        kept.push(t);
        if t == "pytest" || t.ends_with("/pytest") {
            pytest = true;
            break;
        }
    }
    if !pytest {
        return None;
    }
    // After pytest: a path in the tree, or a node id in one, selects tests; every other word is an
    // option or the value of one and stays.
    let mut value = false;
    for t in tokens {
        let selects = !value && !t.starts_with('-') && root.join(t.split("::").next().unwrap_or(t)).exists();
        value = VALUED.contains(&t);
        if !selects {
            kept.push(t);
        }
    }
    let ids = crate::wide::failing(output);
    if ids.is_empty() || ids.iter().any(|id| id.contains('\'') || id.starts_with('-')) {
        return None;
    }
    Some(format!("{} {}", kept.join(" "), ids.iter().map(|id| format!("'{id}'")).collect::<Vec<_>>().join(" ")))
}

/// What a search found and what it cost.
#[derive(Debug, Clone, Default)]
pub struct Search {
    /// The first candidate, in order, that made the visible test pass.
    pub winner: Option<usize>,
    /// Otherwise the candidate that left the fewest tests failing, fewer than before: a step
    /// toward a task with more than one fault.
    pub progress: Option<usize>,
    /// What the test printed with the winner or the progress in place.
    pub output: String,
    /// Tests still failing with it.
    pub remaining: usize,
    /// Candidates started.
    pub tried: usize,
    /// Test runs that ran to an end.
    pub runs: usize,
    pub workers: usize,
    /// Milliseconds copying the tree, and in all.
    pub copy: f64,
    pub total: f64,
    pub note: String,
}

fn size(dir: &Path, total: &mut u64) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for e in entries.flatten() {
        let name = e.file_name().to_string_lossy().into_owned();
        let Ok(kind) = e.file_type() else { continue };
        if kind.is_dir() {
            if !skipped(&name) {
                size(&e.path(), total);
            }
        } else if let Ok(m) = e.metadata() {
            *total += m.len().min(FILE);
        }
        if *total > TREE {
            return;
        }
    }
}

fn skipped(name: &str) -> bool {
    matches!(name, ".git" | "__pycache__" | ".pytest_cache" | "node_modules" | "target" | ".venv" | "venv" | ".mypy_cache" | ".ruff_cache" | ".tox")
}

fn copy(from: &Path, to: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(to)?;
    for e in std::fs::read_dir(from)?.flatten() {
        let name = e.file_name();
        let kind = e.file_type()?;
        let dest = to.join(&name);
        if kind.is_symlink() {
            if let Ok(target) = std::fs::read_link(e.path()) {
                #[cfg(unix)]
                let _ = std::os::unix::fs::symlink(&target, &dest);
                #[cfg(windows)]
                let _ = if e.path().is_dir() {
                    std::os::windows::fs::symlink_dir(&target, &dest)
                } else {
                    std::os::windows::fs::symlink_file(&target, &dest)
                };
            }
        } else if kind.is_dir() {
            if !skipped(&name.to_string_lossy()) {
                copy(&e.path(), &dest)?;
            }
        } else if e.metadata()?.len() <= FILE {
            std::fs::copy(e.path(), dest)?;
        }
    }
    Ok(())
}

/// Replaces `number`'s line of `text` with `new`.
fn with_line(text: &str, number: u32, new: &str) -> String {
    let mut lines: Vec<&str> = text.split('\n').collect();
    lines[number as usize - 1] = new;
    lines.join("\n")
}

/// Tries `candidates` in order against `command`, `workers` at a time in copies of the tree at
/// `root`, until one passes, `cap` has gone, or they are spent. While `waiting` says the model
/// asked beside the search has not answered, the search goes on past `cap`, to [`LONGEST`] at
/// most: that time is spent waiting either way. A candidate's run is cut at `limit` (a mutation
/// can loop). `failing` is how many tests fail now: when none passes, the candidate that leaves
/// the fewest failing, if fewer than that and the code still reads, is the search's progress;
/// `errors` of the failing are tests that could not run, and may not grow.
#[allow(clippy::too_many_arguments)]
pub fn search(
    root: &Path,
    command: &str,
    candidates: &[Candidate],
    failing: usize,
    errors: usize,
    cap: Duration,
    waiting: &(dyn Fn() -> bool + Sync),
    limit: Duration,
    workers: usize,
) -> Search {
    let begun = Instant::now();
    let mut found = Search::default();
    if candidates.is_empty() {
        found.note = "no candidates".into();
        return found;
    }
    let mut bytes = 0;
    size(root, &mut bytes);
    if bytes > TREE {
        found.note = "the tree is too large to copy".into();
        return found;
    }
    let workers = workers.clamp(1, WORKERS).min(candidates.len());
    found.workers = workers;
    let scratch = match tempfile::tempdir() {
        Ok(d) => d,
        Err(e) => {
            found.note = format!("no scratch directory: {e}");
            return found;
        }
    };
    let next = AtomicUsize::new(0);
    let tried = AtomicUsize::new(0);
    let runs = AtomicUsize::new(0);
    let winner: Mutex<Option<(usize, String)>> = Mutex::new(None);
    // (tests left failing, candidate, output) of the best step so far.
    let step: Mutex<Option<(usize, usize, String)>> = Mutex::new(None);
    let copy_ms = Mutex::new(0.0_f64);
    let beaten = |i: usize| winner.lock().unwrap_or_else(std::sync::PoisonError::into_inner).as_ref().is_some_and(|(w, _)| *w < i);
    let over = || {
        let t = begun.elapsed();
        t > cap && (t > LONGEST.max(cap) || !waiting())
    };
    std::thread::scope(|s| {
        for w in 0..workers {
            let (next, tried, runs, winner, step, copy_ms, beaten, over) = (&next, &tried, &runs, &winner, &step, &copy_ms, &beaten, &over);
            let dir: PathBuf = scratch.path().join(format!("w{w}"));
            s.spawn(move || {
                let copying = Instant::now();
                if copy(root, &dir).is_err() {
                    return;
                }
                {
                    let mut c = copy_ms.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                    *c = c.max(copying.elapsed().as_secs_f64() * 1000.0);
                }
                let mut originals: std::collections::HashMap<String, String> = std::collections::HashMap::new();
                loop {
                    if over() {
                        break;
                    }
                    let i = next.fetch_add(1, Ordering::SeqCst);
                    let Some(c) = candidates.get(i) else { break };
                    if beaten(i) {
                        break;
                    }
                    tried.fetch_add(1, Ordering::SeqCst);
                    let path = dir.join(&c.file);
                    let original = originals
                        .entry(c.file.clone())
                        .or_insert_with(|| std::fs::read_to_string(&path).unwrap_or_default())
                        .clone();
                    if std::fs::write(&path, with_line(&original, c.line, &c.new)).is_err() {
                        continue;
                    }
                    // A test still running when the cap is reached is stopped: a round of search costs
                    // the cap at most, however long the tests are.
                    let run = tools::shell_until(&dir, command, limit, &|| beaten(i) || over());
                    let _ = std::fs::write(&path, &original);
                    let code = run["code"].as_i64().unwrap_or(-1);
                    if code == -2 || code == -3 {
                        continue;
                    }
                    runs.fetch_add(1, Ordering::SeqCst);
                    let output = run["output"].as_str().unwrap_or_default().to_string();
                    if code == 0 {
                        let mut f = winner.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                        if f.as_ref().is_none_or(|(x, _)| i < *x) {
                            *f = Some((i, output));
                        }
                    } else if failing > 1 && !crate::diagnostic::broken(&output) {
                        let left = crate::diagnostic::failures(&output);
                        // Fewer failing, and no more tests that could not run than before.
                        if left >= 1 && left < failing && crate::diagnostic::errors(&output) <= errors {
                            let mut b = step.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                            if b.as_ref().is_none_or(|(l, x, _)| (left, i) < (*l, *x)) {
                                *b = Some((left, i, output));
                            }
                        }
                    }
                }
            });
        }
    });
    if let Some((i, output)) = winner.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner) {
        found.winner = Some(i);
        found.output = output;
    } else if let Some((left, i, output)) = step.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner) {
        found.progress = Some(i);
        found.remaining = left;
        found.output = output;
    }
    found.tried = tried.load(Ordering::SeqCst);
    found.runs = runs.load(Ordering::SeqCst);
    found.copy = *copy_ms.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
    found.total = begun.elapsed().as_secs_f64() * 1000.0;
    found
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbols::Tree;
    use pretty_assertions::assert_eq;

    fn kinds(line: &str, python: bool) -> Vec<(&'static str, String)> {
        let m = crate::symbols::masked(if python { "a.py" } else { "a.rs" }, line).unwrap();
        changes(line, &m, python)
            .into_iter()
            .map(|(k, a, b, r)| (k, format!("{}{}{}", &line[..a], r, &line[b..])))
            .collect()
    }

    #[test]
    fn a_comparison_an_operator_and_a_constant_each_have_their_flips() {
        let got = kinds("    return a < b + 1", true);
        assert!(got.contains(&("compare", "    return a <= b + 1".into())), "{got:?}");
        assert!(got.contains(&("compare", "    return a > b + 1".into())), "{got:?}");
        assert!(got.contains(&("operator", "    return a < b - 1".into())), "{got:?}");
        assert!(got.contains(&("constant", "    return a < b + 2".into())), "{got:?}");
        assert!(got.contains(&("constant", "    return a < b + 0".into())), "{got:?}");
    }

    #[test]
    fn every_other_arithmetic_operator_and_a_float_halved_or_doubled_are_tried() {
        let got = kinds("    return a * b", true);
        for r in ["a / b", "a + b", "a - b", "a // b", "a % b"] {
            assert!(got.contains(&("operator", format!("    return {r}"))), "{r}: {got:?}");
        }
        let got = kinds("    x = 2.0", true);
        assert!(got.contains(&("constant", "    x = 1.0".into())) && got.contains(&("constant", "    x = 4.0".into())), "{got:?}");
    }

    #[test]
    fn a_statement_that_is_a_bare_expression_gets_its_return_back_even_when_it_is_a_call() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("tests")).unwrap();
        std::fs::write(dir.path().join("m.py"), "def f(a):\n    if a:\n        compute(a)\n    return 0\n\n\ndef compute(x):\n    return x\n").unwrap();
        std::fs::write(dir.path().join("tests/test_m.py"), "from m import f\n\n\ndef test_f():\n    assert f(3) == 3\n").unwrap();
        let failure = "m.py:4: in f\n    return 0\ntests/test_m.py:5: in test_f\n    assert f(3) == 3\nE   assert 0 == 3\nFAILED tests/test_m.py::test_f - assert 0 == 3\n";
        let mut tree = Tree::open(dir.path());
        let plan = crate::locate::plan(&mut tree, failure);
        let order: Vec<usize> = (0..plan.ranked.len()).collect();
        let got = propose(&mut tree, &plan, &order);
        assert!(got.iter().any(|c| c.kind == "return" && c.new == "        return compute(a)"), "{:?}", got.iter().map(|c| (&c.kind, &c.new)).collect::<Vec<_>>());
    }

    #[test]
    fn the_in_of_a_loop_or_a_comprehension_is_not_a_membership_test() {
        let got = kinds("    if any(ch.isspace() for ch in value):", true);
        assert!(!got.iter().any(|(_, s)| s.contains("for ch not in")), "{got:?}");
        let got = kinds("    if x in seen and any(c for c in y):", true);
        assert!(got.contains(&("boolean", "    if x not in seen and any(c for c in y):".into())), "{got:?}");
        assert!(!got.iter().any(|(_, s)| s.contains("c not in y")), "{got:?}");
    }

    #[test]
    fn nothing_inside_a_string_or_a_comment_changes() {
        let got = kinds("    x = \"a < b and 3\"  # a == 4", true);
        assert!(got.is_empty(), "{got:?}");
    }

    #[test]
    fn unary_minus_and_splats_are_not_binary_operators() {
        let got = kinds("    f(*args, -x, **kw)", true);
        assert!(!got.iter().any(|(k, _)| *k == "operator"), "{got:?}");
    }

    #[test]
    fn arguments_swap_and_a_condition_inverts() {
        let got = kinds("    return pow(base, exp)", true);
        assert!(got.contains(&("arguments", "    return pow(exp, base)".into())), "{got:?}");
        let got = kinds("    if x in seen and not done:", true);
        assert!(got.contains(&("condition", "    if not (x in seen and not done):".into())), "{got:?}");
        assert!(got.contains(&("boolean", "    if x in seen or not done:".into())), "{got:?}");
        assert!(got.contains(&("boolean", "    if x not in seen and not done:".into())), "{got:?}");
        assert!(got.contains(&("boolean", "    if x in seen and done:".into())), "{got:?}");
        let got = kinds("    if not ok:", true);
        assert!(got.contains(&("condition", "    if ok:".into())), "{got:?}");
    }

    #[test]
    fn keyword_arguments_are_not_swapped_and_len_and_range_shift_by_one() {
        let got = kinds("    f(a, key=b)", true);
        assert!(!got.iter().any(|(k, _)| *k == "arguments"), "{got:?}");
        let got = kinds("    for i in range(len(xs)):", true);
        assert!(got.contains(&("bound", "    for i in range(len(xs) - 1):".into())), "{got:?}");
        assert!(got.contains(&("bound", "    for i in range(len(xs) + 1):".into())), "{got:?}");
        assert!(!got.iter().any(|(k, s)| *k == "boolean" && s.contains("not in")), "for-in is not membership: {got:?}");
    }

    #[test]
    fn candidates_start_at_the_failing_line_of_the_best_site() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("pkg")).unwrap();
        std::fs::create_dir_all(dir.path().join("tests")).unwrap();
        std::fs::write(dir.path().join("pkg/m.py"), "def f(a, b):\n    c = a + 1\n    return a - b\n").unwrap();
        std::fs::write(dir.path().join("tests/test_m.py"), "from pkg.m import f\n\n\ndef test_f():\n    assert f(3, 2) == 5\n").unwrap();
        let failure = "pkg/m.py:3: in f\n    return a - b\nE   assert 1 == 5\ntests/test_m.py:5: in test_f\n    assert f(3, 2) == 5\nFAILED tests/test_m.py::test_f - assert 1 == 5\n";
        let mut tree = Tree::open(dir.path());
        let plan = crate::locate::plan(&mut tree, failure);
        let order: Vec<usize> = (0..plan.ranked.len()).collect();
        let got = propose(&mut tree, &plan, &order);
        assert_eq!(got[0].line, 3);
        assert_eq!(got[0].new, "    return a + b");
        assert!(got.iter().all(|c| c.file == "pkg/m.py"));
    }

    #[test]
    fn a_search_finds_the_first_candidate_that_passes_and_leaves_the_tree_alone() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("m.py"), "def f(a, b):\n    return a - b\n").unwrap();
        std::fs::write(dir.path().join("t.py"), "from m import f\nassert f(3, 2) == 5\n").unwrap();
        let cands: Vec<Candidate> = ["    return a * b", "    return a + b", "    return b - a"]
            .iter()
            .map(|n| Candidate { file: "m.py".into(), line: 2, old: "    return a - b".into(), new: (*n).into(), kind: "operator", site: 0 })
            .collect();
        let found = search(dir.path(), "python3 t.py", &cands, 1, 0, Duration::from_secs(20), &|| false, Duration::from_secs(10), 2);
        assert_eq!(found.winner, Some(1), "{found:?}");
        assert_eq!(std::fs::read_to_string(dir.path().join("m.py")).unwrap(), "def f(a, b):\n    return a - b\n");
        let none = search(dir.path(), "python3 t.py", &cands[..1], 1, 0, Duration::from_secs(20), &|| false, Duration::from_secs(10), 1);
        assert_eq!(none.winner, None);
    }

    #[test]
    fn a_candidate_is_tried_on_the_failing_tests_alone() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("tests")).unwrap();
        std::fs::write(dir.path().join("tests/test_a.py"), "").unwrap();
        let out = "FAILED tests/test_a.py::test_x[a&b?c] - assert 1 == 2\nFAILED tests/test_a.py::test_y - boom\n2 failed, 9 passed in 0.31s\n";
        let base = "PYTHONPATH=src:. python -m pytest -q -p no:cacheprovider -o addopts=";
        assert_eq!(
            screen(dir.path(), &format!("{base} tests/test_a.py"), out).as_deref(),
            Some(&*format!("{base} 'tests/test_a.py::test_x[a&b?c]' 'tests/test_a.py::test_y'"))
        );
        // A directory or a node id is a selector too; an option's value is not.
        assert_eq!(
            screen(dir.path(), &format!("{base} tests tests/test_a.py::test_y"), out).as_deref(),
            Some(&*format!("{base} 'tests/test_a.py::test_x[a&b?c]' 'tests/test_a.py::test_y'"))
        );
        assert_eq!(
            screen(dir.path(), "pytest -q --rootdir tests -c tests/test_a.py tests/test_a.py", out).as_deref(),
            Some("pytest -q --rootdir tests -c tests/test_a.py 'tests/test_a.py::test_x[a&b?c]' 'tests/test_a.py::test_y'"),
            "the value of an option that takes one stays"
        );
        assert_eq!(screen(dir.path(), "cargo test", out), None, "not pytest");
        assert_eq!(screen(dir.path(), &format!("{base} tests"), "1 failed in 0.1s\n"), None, "no failing test named");
        assert_eq!(screen(dir.path(), "pytest 'tests/test a.py'", out), None, "quoted words are left alone");
    }

    #[test]
    fn a_search_goes_on_past_its_cap_while_the_model_beside_it_has_not_answered() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("m.py"), "def f(a, b):\n    return a - b\n").unwrap();
        std::fs::write(dir.path().join("t.py"), "import time\nfrom m import f\ntime.sleep(0.05)\nassert f(3, 2) == 5\n").unwrap();
        let cands: Vec<Candidate> = (0..200)
            .map(|k| Candidate { file: "m.py".into(), line: 2, old: "    return a - b".into(), new: format!("    return a - b - {k}"), kind: "constant", site: 0 })
            .collect();
        let alone = search(dir.path(), "python3 t.py", &cands, 1, 0, Duration::ZERO, &|| false, Duration::from_secs(10), 1);
        assert_eq!(alone.tried, 0, "past its cap with nothing to wait for, the search stops: {alone:?}");
        let begun = Instant::now();
        let beside = search(dir.path(), "python3 t.py", &cands, 1, 0, Duration::ZERO, &|| begun.elapsed() < Duration::from_millis(1500), Duration::from_secs(10), 1);
        assert!(beside.tried >= 1 && beside.total >= 1400.0, "it went on while the model wrote: {beside:?}");
        assert!(beside.tried < cands.len(), "and stopped short of its candidates: {beside:?}");
    }

    #[test]
    fn with_two_faults_the_candidate_that_fixes_one_is_the_searchs_progress() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("m.py"), "def f(a, b):\n    return a - b\n\n\ndef g(a, b):\n    return a * b\n").unwrap();
        // Two tests; `python3 t.py` prints a runner-style summary and exits 1 unless both pass.
        std::fs::write(
            dir.path().join("t.py"),
            "import sys\nfrom m import f, g\nbad = (f(3, 2) != 5) + (g(3, 2) != 1)\nprint(f'{bad} failed, {2 - bad} passed')\nsys.exit(1 if bad else 0)\n",
        )
        .unwrap();
        let cands: Vec<Candidate> = [(2, "    return a * b"), (2, "    return a + b"), (6, "    return a // b")]
            .iter()
            .map(|(line, n)| Candidate { file: "m.py".into(), line: *line, old: String::new(), new: (*n).into(), kind: "operator", site: 0 })
            .collect();
        let found = search(dir.path(), "python3 t.py", &cands, 2, 0, Duration::from_secs(20), &|| false, Duration::from_secs(10), 2);
        assert_eq!(found.winner, None, "{found:?}");
        assert_eq!(found.progress, Some(1), "the f fix leaves one failing: {found:?}");
        assert_eq!(found.remaining, 1);
        assert!(found.output.contains("1 failed"), "{}", found.output);
    }
}
