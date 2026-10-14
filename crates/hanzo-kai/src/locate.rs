//! Where the fix is: a failing run's evidence ranked over every function of the tree.
//!
//! kai-gateway's learned preflight policy (`policy.ts`) in Rust, with its weights
//! (`programs/locate.json`, written by bench/code/onegen/policies.py from the curriculum's
//! train repositories) unchanged. The 25 features are computed from the failing diagnostic and
//! the working tree; a softmax over them ranks every function and method; a logistic head over
//! the ranking's shape says whether the best of it holds the fix site. For a compiler's
//! diagnostic, whose frames are exact, the frames themselves are the ranking.

use crate::diagnostic::Diagnostic;
use crate::diagnostic::Kind as Failure;
use crate::symbols::Kind;
use crate::symbols::Map;
use crate::symbols::Sym;
use crate::symbols::Tree;
use crate::symbols::is_test;
use regex::Regex;
use serde::Deserialize;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::LazyLock;

const WEIGHTS: &str = include_str!("../programs/locate.json");

/// Longest function body whose lines count.
const CAP: u32 = 200;
/// Functions a ranking keeps.
const TOP: usize = 48;
/// Most functions a bundle reads.
const PICKS: usize = 48;
/// Lines of functions one bundle reads.
const BUDGET: u32 = 1200;
/// Frames' window either side of a compiler's line.
const WINDOW: u32 = 20;
/// Lines of a symbol read for a compiler's frame without a line.
const SYMBOL: u32 = 80;

const NAMES: [&str; 25] = [
    "frame_file", "frame_sym", "called", "name_in_diag", "name_tok", "test_tok", "import_file",
    "import_name", "bm25", "bm25_test", "bm25_text", "bm25_name", "bm25_file", "callee",
    "callee2", "class_called", "ctor_called", "dunder_op", "parent_reached", "dups", "log_len",
    "is_method", "is_private", "is_dunder", "file_syms",
];

#[derive(Deserialize)]
struct Head {
    mu: Vec<f64>,
    sd: Vec<f64>,
    w: Vec<f64>,
    #[serde(default)]
    b: f64,
    #[serde(default)]
    tau: f64,
}

#[derive(Deserialize)]
struct Weights {
    names: Vec<String>,
    mu: Vec<f64>,
    sd: Vec<f64>,
    w: Vec<f64>,
    gamma: f64,
    escalate: Head,
}

static FIT: LazyLock<Weights> = LazyLock::new(|| {
    let fit: Weights = serde_json::from_str(WEIGHTS).expect("programs/locate.json");
    assert_eq!(fit.names, NAMES, "programs/locate.json names the features in order");
    fit
});

fn re(pattern: &str) -> Regex {
    Regex::new(pattern).expect("a fixed pattern")
}

static CALL: LazyLock<Regex> = LazyLock::new(|| re(r"([A-Za-z_][A-Za-z0-9_]*)\s*\("));
static CAMEL: LazyLock<Regex> = LazyLock::new(|| re(r"([a-z0-9])([A-Z])"));
static FROM_IMPORT: LazyLock<Regex> =
    LazyLock::new(|| re(r"(?m)^[ \t]*from[ \t]+([\w.]+)[ \t]+import[ \t]+\(?([^\n)]+)"));
static IMPORT: LazyLock<Regex> = LazyLock::new(|| re(r"(?m)^[ \t]*import[ \t]+([\w., \t]+)"));
static AS: LazyLock<Regex> = LazyLock::new(|| re(r"\s+as\s+"));
static PRIVATE: LazyLock<Regex> = LazyLock::new(|| re(r"^_[^_]"));
static DUNDER_NAME: LazyLock<Regex> = LazyLock::new(|| re(r"^__.*__$"));
static TEST_PREFIX: LazyLock<Regex> = LazyLock::new(|| re(r"(?i)^test_?"));
static WORD: LazyLock<Regex> = LazyLock::new(|| re(r"^\w+$"));

/// The operators and builtins that run a dunder method without naming it.
static DUNDER: LazyLock<HashMap<&'static str, Regex>> = LazyLock::new(|| {
    [
        ("__add__", r"\+"), ("__radd__", r"\+"), ("__sub__", "-"), ("__rsub__", "-"),
        ("__mul__", r"\*"), ("__truediv__", "/"), ("__floordiv__", "//"), ("__mod__", "%"),
        ("__eq__", "=="), ("__ne__", "!="), ("__lt__", "<"), ("__le__", "<="), ("__gt__", ">"),
        ("__ge__", ">="), ("__hash__", r"\bhash\(|\bset\(|\{"), ("__repr__", r"\brepr\(|\bstr\("),
        ("__str__", r"\bstr\(|\bformat\("), ("__len__", r"\blen\("),
        ("__iter__", r"\bfor\b|\blist\(|\bsorted\(|\btuple\("), ("__getitem__", r"\w\["),
        ("__contains__", r"\bin\b"), ("__bool__", r"\bnot\b|\bbool\("),
        ("__neg__", r"(?:^|[(\s=])-\w"), ("__call__", r"\)\("), ("__enter__", r"\bwith\b"),
        ("__exit__", r"\bwith\b"),
    ]
    .into_iter()
    .map(|(name, pattern)| (name, re(pattern)))
    .collect()
});

const KEYWORDS: &[&str] = &[
    "and", "as", "assert", "async", "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda",
    "nonlocal", "not", "or", "pass", "raise", "return", "try", "while", "with", "yield", "none",
    "true", "false", "self", "cls", "fn", "let", "mut", "impl", "pub", "use", "mod", "str", "int",
];

/// Lower-case word pieces of an identifier or a text: `parseXml_text` is parse, xml, text.
pub fn tokens(text: &str) -> Vec<String> {
    CAMEL
        .replace_all(text, "$1 $2")
        .to_lowercase()
        .split(|c: char| !(c.is_ascii_lowercase() || c.is_ascii_digit()))
        .filter(|t| t.len() > 1 && !KEYWORDS.contains(t))
        .map(str::to_string)
        .collect()
}

fn calls(text: &str) -> HashSet<String> {
    CALL.captures_iter(text).map(|c| c[1].to_string()).collect()
}

/// The module a file is imported as: `src/pkg/mod.py` is pkg.mod, `pkg/__init__.py` is pkg.
fn dotted(file: &str) -> String {
    let stem = file.rsplit_once('.').map_or(file, |(s, _)| s);
    let stem = stem
        .strip_prefix("src/")
        .or_else(|| stem.strip_prefix("lib/"))
        .unwrap_or(stem);
    stem.strip_suffix("/__init__").unwrap_or(stem).replace('/', ".")
}

/// What the failing tests import: dotted modules and attributes, and the bare names brought in.
fn imports(text: &str, targets: &mut HashSet<String>, names: &mut HashSet<String>) {
    for m in FROM_IMPORT.captures_iter(text) {
        let base = m[1].to_string();
        targets.insert(base.clone());
        for part in m[2].split(',') {
            let name = AS.split(part.trim()).next().unwrap_or("").trim().to_string();
            if !name.is_empty() && WORD.is_match(&name) {
                targets.insert(format!("{base}.{name}"));
                names.insert(name);
            }
        }
    }
    for m in IMPORT.captures_iter(text) {
        for part in m[1].split(',') {
            targets.insert(AS.split(part.trim()).next().unwrap_or("").trim().to_string());
        }
    }
}

fn innermost<'a>(syms: &'a [Sym], file: &str, line: u32) -> Option<&'a Sym> {
    syms.iter()
        .filter(|s| s.file == file && s.start <= line && line <= s.end)
        .min_by_key(|s| s.end - s.start)
}

/// The failing test's own function: the test frame's enclosing symbol.
pub fn failing<'a>(d: &Diagnostic, map: &'a Map) -> Option<&'a Sym> {
    d.frames
        .iter()
        .filter(|f| is_test(&f.file))
        .find_map(|f| innermost(&map.symbols, &f.file, f.line))
}

/// A place to look, best first; `score` in [0, 1].
#[derive(Debug, Clone, PartialEq)]
pub struct Ranked {
    pub file: String,
    pub sym: Option<Sym>,
    pub score: f64,
    /// A compiler frame's line.
    pub line: Option<u32>,
}

struct Candidate {
    sym: Sym,
    f: [f64; 25],
}

/// BM25 of a query over documents, normalised by the best in this tree.
fn bm25(docs: &[Vec<String>], query: &HashSet<String>) -> Vec<f64> {
    let mut df: HashMap<&str, usize> = HashMap::new();
    for doc in docs {
        for t in doc.iter().collect::<HashSet<_>>() {
            *df.entry(t.as_str()).or_default() += 1;
        }
    }
    let avg = docs.iter().map(Vec::len).sum::<usize>() as f64 / docs.len().max(1) as f64;
    let raw: Vec<f64> = docs
        .iter()
        .map(|doc| {
            let mut tf: HashMap<&str, usize> = HashMap::new();
            for t in doc {
                *tf.entry(t.as_str()).or_default() += 1;
            }
            let mut score = 0.0;
            for q in query {
                let Some(n) = tf.get(q.as_str()) else {
                    continue;
                };
                let d = df.get(q.as_str()).copied().unwrap_or(0) as f64;
                let idf = (1.0 + (docs.len() as f64 - d + 0.5) / (d + 0.5)).ln();
                let n = *n as f64;
                score += (idf * n * 2.2) / (n + 1.2 * (0.25 + (0.75 * doc.len() as f64) / avg.max(1.0)));
            }
            score
        })
        .collect();
    let top = raw.iter().fold(1e-9_f64, |a, b| a.max(*b));
    raw.into_iter().map(|v| v / top).collect()
}

/// Every function and method of the tree's sources with its features, for this diagnostic.
fn candidates(d: &Diagnostic, map: &Map, tree: &mut Tree) -> Vec<Candidate> {
    let syms: Vec<&Sym> = map
        .symbols
        .iter()
        .filter(|s| {
            (matches!(s.kind, Kind::Fn | Kind::Method) || (s.kind == Kind::Class && s.end - s.start <= 80))
                && !is_test(&s.file)
                && !s.file.starts_with("examples/")
                && !s.file.starts_with("demos/")
        })
        .collect();
    // Every file read once; a symbol's lines are slices of its file's.
    let mut needed: Vec<&str> = syms.iter().map(|s| s.file.as_str()).collect();
    needed.extend(d.frames.iter().filter(|f| is_test(&f.file)).map(|f| f.file.as_str()));
    needed.extend(map.tests.iter().map(String::as_str));
    for file in needed {
        tree.read(file);
    }
    let tree: &Tree = tree;
    let mut split: HashMap<&str, Vec<&str>> = HashMap::new();
    let mut lines_in = |file: &'_ str, start: u32, end: u32| -> String {
        let Some(text) = tree.text(file) else {
            return String::new();
        };
        let all = split.entry(tree.key(file)).or_insert_with(|| text.split('\n').collect());
        all.iter().skip(start as usize - 1).take((end - start + 1) as usize).copied().collect::<Vec<_>>().join("\n")
    };
    let texts: Vec<String> = syms.iter().map(|s| lines_in(&s.file, s.start, s.end)).collect();

    let frames: Vec<_> = d.frames.iter().filter(|f| !is_test(&f.file)).collect();
    let test = failing(d, map);
    let mut test_text = test.map(|t| lines_in(&t.file, t.start, t.end)).unwrap_or_default();
    if let Some(t) = test
        && t.kind == Kind::Method
        && let Some(parent) = map
            .symbols
            .iter()
            .find(|s| s.file == t.file && s.kind == Kind::Class && s.start <= t.start && t.end <= s.end)
    {
        test_text = lines_in(&parent.file, parent.start, parent.end);
    }
    let mut test_files: HashSet<String> = d
        .frames
        .iter()
        .filter(|f| is_test(&f.file))
        .map(|f| f.file.clone())
        .collect();
    for t in &d.tests {
        let head = t.split("::").next().unwrap_or("");
        if map.tests.iter().any(|m| m == head) {
            test_files.insert(head.to_string());
        }
    }
    let (mut targets, mut names) = (HashSet::new(), HashSet::new());
    let mut ordered: Vec<&String> = test_files.iter().collect();
    ordered.sort();
    for file in ordered {
        imports(tree.text(file).unwrap_or(""), &mut targets, &mut names);
    }
    let called = calls(&test_text);
    let last = |t: &String| t.split("::").last().unwrap_or(t).to_string();
    let query: HashSet<String> = d
        .names
        .iter()
        .flat_map(|n| tokens(n))
        .chain(called.iter().flat_map(|n| tokens(n)))
        .chain(d.tests.iter().flat_map(|t| tokens(&last(t))))
        .collect();
    let test_tokens: HashSet<String> = d
        .tests
        .iter()
        .flat_map(|t| tokens(&TEST_PREFIX.replace(&last(t), "")))
        .collect();

    let bodies: Vec<Vec<String>> = texts.iter().map(|t| tokens(t)).collect();
    let b_query = bm25(&bodies, &query);
    let b_test = bm25(&bodies, &tokens(&test_text).into_iter().collect());
    let b_text = bm25(&bodies, &tokens(&d.text).into_iter().collect());
    let name_docs: Vec<Vec<String>> = syms.iter().map(|s| tokens(&format!("{} {}", s.qual, s.file))).collect();
    let b_name = bm25(&name_docs, &query);
    let mut file_best: HashMap<&str, f64> = HashMap::new();
    let mut file_count: HashMap<&str, usize> = HashMap::new();
    for (i, s) in syms.iter().enumerate() {
        let best = file_best.entry(s.file.as_str()).or_insert(0.0);
        *best = best.max(b_query[i]);
        *file_count.entry(s.file.as_str()).or_default() += 1;
    }

    let mut direct: HashSet<usize> = HashSet::new();
    for (i, s) in syms.iter().enumerate() {
        if called.contains(&s.name)
            || d.names.contains(&s.name)
            || frames.iter().any(|f| f.file == s.file && s.start <= f.line && f.line <= s.end)
        {
            direct.insert(i);
        }
    }
    // The call graph by name, as far as two calls from what the failing test calls; a name that
    // many functions share is no evidence.
    let mut by_name: HashMap<&str, Vec<usize>> = HashMap::new();
    for (i, s) in syms.iter().enumerate() {
        by_name.entry(s.name.as_str()).or_default().push(i);
    }
    let reach = |from: &HashSet<usize>| -> HashSet<usize> {
        let mut out = HashSet::new();
        for i in from {
            for c in calls(&texts[*i]) {
                if let Some(hit) = by_name.get(c.as_str())
                    && hit.len() <= 8
                {
                    out.extend(hit.iter().copied());
                }
            }
        }
        out
    };
    let one = reach(&direct);
    let two = reach(&one);
    let reached: HashSet<usize> = direct.union(&one).copied().collect();
    let owner = |s: &Sym| -> String {
        if s.kind == Kind::Method {
            let mut parts = s.qual.split('.').collect::<Vec<_>>();
            parts.pop();
            parts.pop().unwrap_or("").to_string()
        } else {
            String::new()
        }
    };
    let test_body = format!("{test_text}\n{}", d.text);
    let flag = |b: bool| if b { 1.0 } else { 0.0 };

    syms.iter()
        .enumerate()
        .map(|(i, s)| {
            let pieces = tokens(&s.name);
            let share = |set: &HashSet<String>| {
                if pieces.is_empty() {
                    0.0
                } else {
                    pieces.iter().filter(|p| set.contains(*p)).count() as f64 / pieces.len() as f64
                }
            };
            let own = owner(s);
            let f = [
                flag(frames.iter().any(|x| x.file == s.file)),
                flag(frames.iter().any(|x| x.file == s.file && s.start <= x.line && x.line <= s.end)),
                flag(called.contains(&s.name)),
                flag(d.names.contains(&s.name)),
                share(&query),
                share(&test_tokens),
                flag(targets.contains(&dotted(&s.file))),
                flag(names.contains(&s.name)),
                b_query[i],
                b_test[i],
                b_text[i],
                b_name[i],
                file_best.get(s.file.as_str()).copied().unwrap_or(0.0),
                flag(one.contains(&i)),
                flag(two.contains(&i) && !one.contains(&i)),
                flag(!own.is_empty() && (called.contains(&own) || d.names.contains(&own))),
                flag((s.name == "__init__" || s.name == "__new__") && !own.is_empty() && called.contains(&own)),
                flag(DUNDER.get(s.name.as_str()).is_some_and(|r| r.is_match(&test_body))),
                flag(syms.iter().enumerate().any(|(j, o)| {
                    j != i && o.file == s.file && o.start <= s.start && s.end <= o.end && reached.contains(&j)
                })),
                (by_name.get(s.name.as_str()).map_or(1, Vec::len) as f64).ln(),
                (1.0 + f64::from(s.end - s.start + 1)).ln(),
                flag(s.kind == Kind::Method),
                flag(PRIVATE.is_match(&s.name)),
                flag(DUNDER_NAME.is_match(&s.name)),
                (1.0 + file_count.get(s.file.as_str()).copied().unwrap_or(0) as f64).ln(),
            ];
            Candidate { sym: (*s).clone(), f }
        })
        .collect()
}

fn dot(w: &[f64], mu: &[f64], sd: &[f64], x: &[f64]) -> f64 {
    x.iter()
        .enumerate()
        .map(|(i, v)| ((v - mu[i]) / sd[i]) * w[i])
        .sum()
}

fn softmax(scores: &[f64]) -> Vec<f64> {
    let m = scores.iter().fold(f64::NEG_INFINITY, |a, b| a.max(*b));
    let e: Vec<f64> = scores.iter().map(|s| (s - m).exp()).collect();
    let sum: f64 = e.iter().sum();
    e.into_iter().map(|v| v / sum).collect()
}

/// A compiler's diagnostic: the frames it names, source before tests.
fn frames(d: &Diagnostic, map: &Map) -> Vec<Ranked> {
    let files: HashSet<&str> = map.files.iter().map(String::as_str).collect();
    let mut seen = HashSet::new();
    let (mut sources, mut tests) = (Vec::new(), Vec::new());
    for frame in &d.frames {
        let Some(file) = resolve(&frame.file, map, &files) else {
            continue;
        };
        if !seen.insert((file.clone(), frame.line)) {
            continue;
        }
        let sym = innermost(&map.symbols, &file, frame.line).cloned();
        let named = sym.as_ref().is_some_and(|s| {
            d.tests
                .iter()
                .any(|t| t.split("::").last().unwrap_or(t) == s.name)
        });
        let entry = (file.clone(), frame.line, sym);
        if is_test(&file) || named {
            tests.push(entry);
        } else {
            sources.push(entry);
        }
    }
    sources
        .into_iter()
        .chain(tests)
        .enumerate()
        .map(|(i, (file, line, sym))| Ranked {
            file,
            sym,
            score: 1.0 / (1.0 + i as f64),
            line: Some(line),
        })
        .collect()
}

/// A path the output names, as the tree spells it: exact, else the one file it is the tail of.
fn resolve(file: &str, map: &Map, files: &HashSet<&str>) -> Option<String> {
    if files.contains(file) {
        return Some(file.to_string());
    }
    let tails: Vec<&String> = map
        .files
        .iter()
        .filter(|c| file.ends_with(&format!("/{c}")) && (!file.starts_with('/') || c.contains('/')))
        .collect();
    let longest = tails.iter().map(|t| t.len()).max()?;
    let best: Vec<_> = tails.into_iter().filter(|t| t.len() == longest).collect();
    (best.len() == 1).then(|| best[0].clone())
}

/// Every candidate's file, name, lines and features for `failure`, in the map's order: what the
/// fit saw, for a test against kai-gateway's own extraction.
pub fn features(tree: &mut Tree, failure: &str) -> Vec<(Sym, [f64; 25])> {
    let diagnostic = crate::diagnostic::parse(failure, tree.root().to_str());
    let map = tree.map();
    candidates(&diagnostic, &map, tree)
        .into_iter()
        .map(|c| (c.sym, c.f))
        .collect()
}

/// The evidence of one failing run read against one tree.
pub struct Plan {
    pub diagnostic: Diagnostic,
    pub map: Map,
    /// Best first.
    pub ranked: Vec<Ranked>,
    /// The failing test's own function.
    pub test: Option<Sym>,
    /// The probability that the ranking's best holds the fix site: a one-shot edit is worth
    /// trying at once above [`Plan::sure`].
    pub oneshot: f64,
}

/// Reads `failure` (a failing run's output) against the tree at `root`.
pub fn plan(tree: &mut Tree, failure: &str) -> Plan {
    let diagnostic = crate::diagnostic::parse(failure, tree.root().to_str());
    let map = tree.map();
    let ranked = if diagnostic.kind == Failure::Compile {
        frames(&diagnostic, &map)
    } else {
        let all = candidates(&diagnostic, &map, tree);
        if all.is_empty() {
            frames(&diagnostic, &map)
        } else {
            let p = softmax(&all.iter().map(|c| dot(&FIT.w, &FIT.mu, &FIT.sd, &c.f)).collect::<Vec<_>>());
            let mut ranked: Vec<Ranked> = all
                .into_iter()
                .zip(p)
                .map(|(c, p)| Ranked {
                    file: c.sym.file.clone(),
                    sym: Some(c.sym),
                    score: p,
                    line: None,
                })
                .collect();
            ranked.sort_by(|a, b| b.score.total_cmp(&a.score));
            ranked.truncate(TOP);
            ranked
        }
    };
    let test = failing(&diagnostic, &map).cloned();
    let oneshot = escalate(&ranked, &diagnostic);
    Plan {
        diagnostic,
        map,
        ranked,
        test,
        oneshot,
    }
}

impl Plan {
    /// The threshold above which the ranking's best is trusted alone (fit with the weights).
    pub fn sure(&self) -> f64 {
        FIT.escalate.tau
    }
}

/// The functions to read, the ranking's best first, until their probabilities add up to the fit
/// `gamma` or the line budget is spent.
pub fn cuts(ranked: &[Ranked]) -> Vec<&Ranked> {
    let (mut out, mut cum, mut lines) = (Vec::new(), 0.0, 0);
    for x in ranked.iter().take(PICKS) {
        let size = x.sym.as_ref().map_or(0, |s| (s.end - s.start + 1).min(CAP));
        if !out.is_empty() && lines + size > BUDGET {
            break;
        }
        out.push(x);
        lines += size;
        cum += x.score;
        if cum >= FIT.gamma {
            break;
        }
    }
    out
}

/// The shape of a ranking and of the bundle read for it: what the escalation head sees.
fn shape(p: &[f64], d: &Diagnostic, picks: usize, lines: u32) -> [f64; 8] {
    let top = &p[..p.len().min(8)];
    let sum = top.iter().sum::<f64>();
    let sum = if sum == 0.0 { 1.0 } else { sum };
    let entropy = -top.iter().map(|v| (v / sum) * ((v / sum) + 1e-12).ln()).sum::<f64>();
    let at = |i: usize| p.get(i).copied().unwrap_or(0.0);
    [
        at(0),
        at(0) - at(1),
        entropy,
        if d.kind == Failure::Assert { 1.0 } else { 0.0 },
        if d.frames.iter().any(|f| !is_test(&f.file)) { 1.0 } else { 0.0 },
        (d.tests.len() as f64).ln_1p(),
        (picks as f64).ln_1p(),
        f64::from(lines).ln_1p(),
    ]
}

/// The probability that the best of `ranked` holds the fix site.
fn escalate(ranked: &[Ranked], d: &Diagnostic) -> f64 {
    let top = ranked.first();
    if d.kind == Failure::Compile || ranked.iter().any(|x| x.line.is_some()) {
        // A compiler's frames are exact: one-shot unless the best is test code.
        return match top {
            Some(t) if t.line.is_some() && !t.sym.as_ref().is_some_and(|s| is_test(&s.file)) && !is_test(&t.file) => 1.0,
            _ => 0.0,
        };
    }
    let picks = cuts(ranked);
    let lines: u32 = picks
        .iter()
        .map(|x| x.sym.as_ref().map_or(0, |s| (s.end - s.start + 1).min(CAP)))
        .sum();
    let scores: Vec<f64> = ranked.iter().map(|x| x.score).collect();
    let e = &FIT.escalate;
    let z = dot(&e.w, &e.mu, &e.sd, &shape(&scores, d, picks.len(), lines)) + e.b;
    1.0 / (1.0 + (-z).exp())
}

/// The lines to read for a compiler's frame: the window around its line within its symbol.
pub fn window(x: &Ranked) -> Option<(u32, u32)> {
    match (x.line, &x.sym) {
        (Some(line), sym) => Some((
            line.saturating_sub(WINDOW).max(1).max(sym.as_ref().map_or(1, |s| s.start)),
            (line + WINDOW).min(sym.as_ref().map_or(u32::MAX, |s| s.end)),
        )),
        (None, Some(s)) => Some((s.start, s.end.min(s.start + SYMBOL - 1))),
        (None, None) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn tokens_split_identifiers_and_drop_keywords() {
        assert_eq!(tokens("parseXml_text self"), ["parse", "xml", "text"]);
        assert_eq!(dotted("src/pkg/mod.py"), "pkg.mod");
        assert_eq!(dotted("pkg/__init__.py"), "pkg");
    }

    #[test]
    fn the_function_a_failing_test_calls_by_name_ranks_first() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::create_dir_all(root.join("pkg")).unwrap();
        std::fs::create_dir_all(root.join("tests")).unwrap();
        std::fs::write(
            root.join("pkg/maths.py"),
            "def add(a, b):\n    return a - b\n\ndef mul(a, b):\n    return a * b\n\ndef sub(a, b):\n    return a - b\n",
        )
        .unwrap();
        std::fs::write(
            root.join("tests/test_maths.py"),
            "from pkg.maths import add\n\ndef test_add():\n    assert add(2, 3) == 5\n",
        )
        .unwrap();
        let failure = "___ test_add ___\ntests/test_maths.py:4: in test_add\n    assert add(2, 3) == 5\nE   assert -1 == 5\nFAILED tests/test_maths.py::test_add - assert -1 == 5\n";
        let mut tree = Tree::open(root);
        let plan = plan(&mut tree, failure);
        assert_eq!(plan.ranked[0].sym.as_ref().unwrap().qual, "add");
        assert!(plan.ranked[0].score > plan.ranked[1].score);
        assert_eq!(plan.test.as_ref().unwrap().qual, "test_add");
        let total: f64 = plan.ranked.iter().map(|r| r.score).sum();
        assert!(total <= 1.0 + 1e-9);
    }

    #[test]
    fn a_compilers_frames_are_the_ranking() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::create_dir_all(root.join("src")).unwrap();
        std::fs::write(root.join("src/lib.rs"), "fn f() -> u32 {\n    1u8\n}\n").unwrap();
        let failure = "error[E0308]: mismatched types\n --> src/lib.rs:2:5\n  |\n2 |     1u8\n";
        let mut tree = Tree::open(root);
        let plan = plan(&mut tree, failure);
        assert_eq!(plan.ranked.len(), 1);
        assert_eq!(plan.ranked[0].line, Some(2));
        assert_eq!(window(&plan.ranked[0]), Some((1, 3)));
        assert_eq!(plan.oneshot, 1.0);
    }

    /// kai-gateway's extraction of the same trees, written by kai-gateway scripts/features.mjs from main over
    /// `~/.cache/kai-onegen`: every feature of every candidate of the first task of each
    /// repository. `cargo test -p hanzo-kai --release parity -- --ignored`.
    #[test]
    #[ignore = "needs the onegen curriculum's trees and features under ~/.cache/kai-onegen"]
    fn the_features_are_kai_gateways_on_the_curriculum() {
        let data = std::path::PathBuf::from(std::env::var("HOME").unwrap()).join(".cache/kai-onegen");
        let records: std::collections::HashMap<String, serde_json::Value> =
            std::fs::read_to_string(data.join("records.jsonl"))
                .unwrap()
                .lines()
                .map(|l| serde_json::from_str::<serde_json::Value>(l).unwrap())
                .map(|r| (r["id"].as_str().unwrap().to_string(), r))
                .collect();
        let mut repos = std::collections::HashSet::new();
        let mut checked = 0;
        for line in std::fs::read_to_string(data.join("features.current.jsonl")).unwrap().lines() {
            let golden: serde_json::Value = serde_json::from_str(line).unwrap();
            let id = golden["id"].as_str().unwrap();
            let record = &records[id];
            if !repos.insert(record["repo"].as_str().unwrap().to_string()) {
                continue;
            }
            let mut tree = Tree::open(&data.join("trees").join(id));
            let ours = features(&mut tree, record["prompt"].as_str().unwrap());
            let theirs = golden["cands"].as_array().unwrap();
            if ours.len() != theirs.len() {
                let key = |f: &str, q: &str, s: u64| format!("{f}:{q}:{s}");
                let t: std::collections::HashSet<String> = theirs.iter().map(|t| key(t["file"].as_str().unwrap(), t["qual"].as_str().unwrap(), t["start"].as_u64().unwrap())).collect();
                let o: std::collections::HashSet<String> = ours.iter().map(|(s, _)| key(&s.file, &s.qual, u64::from(s.start))).collect();
                eprintln!("only ours: {:?}", o.difference(&t).take(8).collect::<Vec<_>>());
                eprintln!("only theirs: {:?}", t.difference(&o).take(8).collect::<Vec<_>>());
            }
            assert_eq!(ours.len(), theirs.len(), "{id}: candidates");
            let mut worst = 0.0_f64;
            for (i, (sym, f)) in ours.iter().enumerate() {
                let t = &theirs[i];
                assert_eq!((sym.file.as_str(), sym.qual.as_str(), u64::from(sym.start), u64::from(sym.end)),
                           (t["file"].as_str().unwrap(), t["qual"].as_str().unwrap(), t["start"].as_u64().unwrap(), t["end"].as_u64().unwrap()),
                           "{id}: candidate {i}");
                for (j, v) in f.iter().enumerate() {
                    let g = t["f"][j].as_f64().unwrap();
                    let d = (v - g).abs();
                    worst = worst.max(d);
                    assert!(d < 1e-6, "{id}: {} of {}: {v} against {g}", NAMES[j], sym.qual);
                }
            }
            eprintln!("{id}: {} candidates, worst difference {worst:e}", ours.len());
            checked += 1;
        }
        assert!(checked >= 5, "{checked} repositories checked");
    }
}
