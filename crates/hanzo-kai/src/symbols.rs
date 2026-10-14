//! A working tree read-only, and the functions, classes and methods in it.
//!
//! kai-gateway's `repomap.ts`, so the ranker fit on its map ([`crate::locate`]) reads the same
//! symbols here. Nothing is executed (not even `git`); nothing outside the tree and nothing that
//! looks like a secret is read.

use regex::Regex;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::path::Path;
use std::path::PathBuf;
use std::sync::LazyLock;

const SKIP: &[&str] = &["node_modules", "target", "dist", "build", "__pycache__"];
const LIST: usize = 20_000;
const READ: u64 = 200 * 1024;
const FILES: usize = 400;
const BYTES: usize = 2 * 1024 * 1024;
/// What a string literal's text becomes in [`mask`]: no space, so a line holding only it is not
/// blank.
const FILL: u8 = 0;

fn re(pattern: &str) -> Regex {
    Regex::new(pattern).expect("a fixed pattern")
}

static SECRET: LazyLock<Regex> = LazyLock::new(|| {
    re(r"(?i)^(?:\.env.*|.*\.(?:pem|key|p12|pfx)|id_(?:rsa|dsa|ecdsa|ed25519).*|credentials.*|\.npmrc|\.netrc|\.pypirc)$")
});
static TEST_FILE: LazyLock<Regex> = LazyLock::new(|| {
    re(r"^(?:test_.*\.py|.*_test\.py|conftest\.py|.*\.(?:test|spec)\.[cm]?[jt]sx?|.*_test\.rs)$")
});

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Fn,
    Class,
    Method,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Sym {
    pub file: String,
    pub name: String,
    /// `Class.method`, `outer.inner`, or the name alone; `.` on every language.
    pub qual: String,
    pub kind: Kind,
    /// 1-based, decorators and attributes included.
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone, Default)]
pub struct Map {
    pub files: Vec<String>,
    /// Code files that are not tests.
    pub sources: Vec<String>,
    pub tests: Vec<String>,
    pub symbols: Vec<Sym>,
}

/// A name that may hold a credential, refused on a path's every segment.
fn secret(path: &str) -> bool {
    path.split('/').any(|part| SECRET.is_match(part))
}

/// Whether a path is test code, by the conventions of pytest, cargo and the JS runners.
pub fn is_test(path: &str) -> bool {
    let parts: Vec<&str> = path.split('/').collect();
    let (name, dirs) = parts.split_last().map_or(("", &[][..]), |(n, d)| (*n, d));
    dirs.iter()
        .any(|d| matches!(*d, "tests" | "test" | "testing" | "__tests__"))
        || TEST_FILE.is_match(name)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Lang {
    Py,
    Rs,
    Js,
}

fn lang(path: &str) -> Option<Lang> {
    let ext = path.rsplit_once('.')?.1.to_lowercase();
    match ext.as_str() {
        "py" => Some(Lang::Py),
        "rs" => Some(Lang::Rs),
        "ts" | "tsx" | "js" | "jsx" | "mjs" | "cjs" | "mts" | "cts" => Some(Lang::Js),
        _ => None,
    }
}

/// The tree at `root`, listed once and read through a cache.
pub struct Tree {
    root: PathBuf,
    listed: Vec<String>,
    texts: HashMap<String, Option<String>>,
}

impl Tree {
    /// Lists the tree: relative `/`-separated paths, directories breadth first and entries by
    /// name; hidden directories, dependencies and build output left out, and no link followed.
    pub fn open(root: &Path) -> Tree {
        let mut listed = Vec::new();
        if let Ok(real) = root.canonicalize() {
            let mut pending = VecDeque::from([String::new()]);
            while let Some(dir) = pending.pop_front() {
                if listed.len() >= LIST {
                    break;
                }
                let Ok(entries) = std::fs::read_dir(real.join(&dir)) else {
                    continue;
                };
                let mut entries: Vec<_> = entries.flatten().collect();
                entries.sort_by_key(|e| e.file_name());
                for entry in entries {
                    let name = entry.file_name().to_string_lossy().into_owned();
                    let path = if dir.is_empty() {
                        name.clone()
                    } else {
                        format!("{dir}/{name}")
                    };
                    let Ok(kind) = entry.file_type() else {
                        continue;
                    };
                    if kind.is_dir() {
                        if !name.starts_with('.') && !SKIP.contains(&name.as_str()) {
                            pending.push_back(path);
                        }
                    } else if kind.is_file() && !secret(&name) && listed.len() < LIST {
                        listed.push(path);
                    }
                }
            }
        }
        Tree {
            root: root.to_path_buf(),
            listed,
            texts: HashMap::new(),
        }
    }

    /// A file already read by [`Tree::read`].
    pub fn text(&self, path: &str) -> Option<&str> {
        self.texts.get(path)?.as_deref()
    }

    /// The tree's own spelling of a path already read, to key borrows of its text by.
    pub fn key(&self, path: &str) -> &str {
        self.texts.get_key_value(path).map_or("", |(k, _)| k.as_str())
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn list(&self) -> &[String] {
        &self.listed
    }

    /// The file's text: UTF-8 (lossy), at most 200 KB, a regular file under the root. `None`
    /// for anything else.
    pub fn read(&mut self, path: &str) -> Option<&str> {
        if !self.texts.contains_key(path) {
            let text = self.load(path);
            self.texts.insert(path.to_string(), text);
        }
        self.texts[path].as_deref()
    }

    fn load(&self, path: &str) -> Option<String> {
        if path.is_empty() || path.starts_with('/') || path.contains('\0') || secret(path) {
            return None;
        }
        let parts: Vec<&str> = path.split('/').collect();
        let dirs = &parts[..parts.len() - 1];
        if dirs.iter().any(|p| p.starts_with('.') || SKIP.contains(p)) {
            return None;
        }
        let full = self.root.canonicalize().ok()?.join(path);
        let meta = std::fs::symlink_metadata(&full).ok()?;
        if !meta.is_file() || meta.len() > READ {
            return None;
        }
        // A directory on the way may be a link out of the tree.
        if !full.canonicalize().ok()?.starts_with(self.root.canonicalize().ok()?) {
            return None;
        }
        let bytes = std::fs::read(&full).ok()?;
        if bytes.contains(&0) {
            return None;
        }
        Some(String::from_utf8_lossy(&bytes).into_owned())
    }

    /// The files, code split into sources and tests, and the symbols of the first 400 code
    /// files (sources first) or 2 MB.
    pub fn map(&mut self) -> Map {
        let files = self.listed.clone();
        let code: Vec<&String> = files.iter().filter(|f| lang(f).is_some()).collect();
        let sources: Vec<String> = code.iter().filter(|f| !is_test(f)).map(|f| (*f).clone()).collect();
        let tests: Vec<String> = code.iter().filter(|f| is_test(f)).map(|f| (*f).clone()).collect();
        let mut symbols = Vec::new();
        let (mut count, mut bytes) = (0, 0);
        for file in sources.iter().chain(&tests) {
            if count >= FILES || bytes >= BYTES {
                break;
            }
            let Some(text) = self.read(file) else {
                continue;
            };
            count += 1;
            bytes += text.encode_utf16().count();
            symbols.extend(symbols_of(file, text));
        }
        Map {
            files,
            sources,
            tests,
            symbols,
        }
    }
}

/// The functions, classes and methods a file declares; empty for a language not read.
pub fn symbols_of(path: &str, text: &str) -> Vec<Sym> {
    match lang(path) {
        Some(Lang::Py) => python(path, text),
        Some(l @ (Lang::Rs | Lang::Js)) => braced(path, text, l),
        None => Vec::new(),
    }
}

/// `text` as [`mask`] makes it for the language of `path`: comments blank, string contents
/// filled, offsets and lines the source's. `None` for a language not read.
pub fn masked(path: &str, text: &str) -> Option<Vec<u8>> {
    lang(path).map(|l| mask(text, l))
}

/// `text` with every comment blanked to spaces and every string literal but its first character
/// to FILL, newlines kept: offsets and lines are the source's, nothing quoted can look like
/// code, and a line that starts inside a string (FILL first) is told from one that starts a
/// statement.
fn mask(text: &str, language: Lang) -> Vec<u8> {
    let t = text.as_bytes();
    let mut out = t.to_vec();
    let n = t.len();
    let eol = |from: usize| t[from..].iter().position(|b| *b == b'\n').map_or(n, |p| from + p);
    let blank = |out: &mut Vec<u8>, from: usize, to: usize| {
        for b in &mut out[from..to] {
            if *b != b'\n' {
                *b = b' ';
            }
        }
    };
    let quote = |out: &mut Vec<u8>, i: usize, end: usize| {
        for b in &mut out[(i + 1).min(end)..end] {
            if *b != b'\n' {
                *b = FILL;
            }
        }
    };
    // The end of a quoted literal opened before `start` with `q`; escapes skip a character, and
    // a one-line quote ends at a newline.
    let close = |start: usize, q: &[u8], lines: bool| -> usize {
        let mut j = start;
        while j < n {
            if t[j] == b'\\' {
                j += 2;
            } else if t[j..].starts_with(q) {
                return j + q.len();
            } else if !lines && t[j] == b'\n' {
                return j;
            } else {
                j += 1;
            }
        }
        n
    };
    let mut i = 0;
    while i < n {
        let ch = t[i];
        if language == Lang::Py {
            if ch == b'#' {
                let end = eol(i);
                blank(&mut out, i, end);
                i = end;
            } else if ch == b'"' || ch == b'\'' {
                let triple = t[i..].starts_with(&[ch, ch, ch]);
                let end = if triple {
                    close(i + 3, &[ch, ch, ch], true)
                } else {
                    close(i + 1, &[ch], false)
                };
                quote(&mut out, i, end.min(n));
                i = end.min(n).max(i + 1);
            } else {
                i += 1;
            }
            continue;
        }
        if t[i..].starts_with(b"//") {
            let end = eol(i);
            blank(&mut out, i, end);
            i = end;
        } else if t[i..].starts_with(b"/*") {
            // Rust's block comments nest; JavaScript's do not.
            let (mut depth, mut j) = (0i32, i);
            while j < n {
                if t[j..].starts_with(b"/*") {
                    depth += i32::from(language == Lang::Rs || depth == 0);
                    j += 2;
                } else if t[j..].starts_with(b"*/") {
                    depth -= 1;
                    j += 2;
                    if depth == 0 {
                        break;
                    }
                } else {
                    j += 1;
                }
            }
            blank(&mut out, i, j.min(n));
            i = j.min(n);
        } else if ch == b'"' {
            let end = close(i + 1, b"\"", language == Lang::Rs).min(n);
            quote(&mut out, i, end);
            i = end.max(i + 1);
        } else if language == Lang::Js && (ch == b'\'' || ch == b'`') {
            let end = close(i + 1, &[ch], ch == b'`').min(n);
            quote(&mut out, i, end);
            i = end.max(i + 1);
        } else if language == Lang::Rs
            && (ch == b'r' || ch == b'b')
            && !(i > 0 && (t[i - 1].is_ascii_alphanumeric() || t[i - 1] == b'_'))
        {
            // b?r#*"
            let mut j = i;
            if t[j] == b'b' {
                j += 1;
            }
            if j < n && t[j] == b'r' {
                let mut k = j + 1;
                while k < n && t[k] == b'#' {
                    k += 1;
                }
                if k < n && t[k] == b'"' {
                    let hashes = &t[j + 1..k];
                    let mut closer = vec![b'"'];
                    closer.extend_from_slice(hashes);
                    let end = find(t, &closer, k + 1).map_or(n, |p| p + closer.len());
                    quote(&mut out, i, end);
                    i = end.max(i + 1);
                    continue;
                }
            }
            i += 1;
        } else if language == Lang::Rs && ch == b'\'' {
            // A char literal, not a lifetime ('a): one character or one escape, then the
            // closing quote.
            match char_literal(t, i) {
                Some(len) => {
                    quote(&mut out, i, i + len);
                    i += len;
                }
                None => i += 1,
            }
        } else {
            i += 1;
        }
    }
    out
}

fn find(hay: &[u8], needle: &[u8], from: usize) -> Option<usize> {
    if from >= hay.len() {
        return None;
    }
    hay[from..]
        .windows(needle.len())
        .position(|w| w == needle)
        .map(|p| from + p)
}

/// The length of a Rust char literal opened at `i`, if one is.
fn char_literal(t: &[u8], i: usize) -> Option<usize> {
    let rest = std::str::from_utf8(&t[i..t.len().min(i + 16)]).ok().or_else(|| {
        // A multi-byte character may cut the window; try shorter windows.
        (1..16).rev().find_map(|k| std::str::from_utf8(&t[i..t.len().min(i + k)]).ok())
    })?;
    static CHAR: LazyLock<Regex> = LazyLock::new(|| {
        re(r"^'(?:[^'\\\n]|\\(?:x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]{1,6}\}|.))'")
    });
    CHAR.find(rest).map(|m| m.end())
}

/// Columns of indentation, a tab to the next multiple of 8.
fn indent(line: &[u8]) -> usize {
    let mut width = 0;
    for b in line {
        match b {
            b' ' => width += 1,
            b'\t' => width += 8 - (width % 8),
            _ => break,
        }
    }
    width
}

fn balance(line: &[u8]) -> i64 {
    line.iter().fold(0, |n, b| match b {
        b'(' | b'[' | b'{' => n + 1,
        b')' | b']' | b'}' => n - 1,
        _ => n,
    })
}

static DEF: LazyLock<Regex> =
    LazyLock::new(|| re(r"^\s*(?:async\s+)?(def|class)\s+([A-Za-z_]\w*)"));
static DECORATOR: LazyLock<Regex> = LazyLock::new(|| re(r"^\s*@"));

fn first_char(line: &[u8]) -> Option<u8> {
    line.iter().copied().find(|b| *b != b' ' && *b != b'\t' && *b != b'\r')
}

/// Python by indentation: a block ends before the next statement indented no deeper than its
/// header. Comment lines, blank in the mask, neither end a block nor belong to it; a line inside
/// a string does not end one, wherever it is indented.
fn python(file: &str, text: &str) -> Vec<Sym> {
    let masked = mask(text, Lang::Py);
    let lines: Vec<&[u8]> = masked.split(|b| *b == b'\n').collect();
    let mut found: Vec<Sym> = Vec::new();
    // Indices into `found` of the symbols still open.
    let mut open: Vec<usize> = Vec::new();
    for i in 0..lines.len() {
        let line = String::from_utf8_lossy(lines[i]);
        let Some(m) = DEF.captures(&line) else {
            continue;
        };
        let level = indent(lines[i]);
        // The header runs until its brackets close: a signature can span lines.
        let mut header = i;
        let mut depth = balance(lines[i]);
        while depth > 0 && header + 1 < lines.len() {
            header += 1;
            depth += balance(lines[header]);
        }
        let mut end = header;
        for (k, l) in lines.iter().enumerate().skip(header + 1) {
            let Some(first) = first_char(l) else {
                continue;
            };
            if first != FILL && indent(l) <= level {
                break;
            }
            end = k;
        }
        // Decorators directly above, a call spread over lines included.
        let mut start = i;
        let mut depth = 0i64;
        let mut k = i;
        while k > 0 {
            k -= 1;
            depth -= balance(lines[k]);
            if depth < 0 {
                break;
            }
            if depth > 0 {
                continue;
            }
            let l = String::from_utf8_lossy(lines[k]);
            if !DECORATOR.is_match(&l) || indent(lines[k]) != level {
                break;
            }
            start = k;
        }
        while open.last().is_some_and(|p| found[*p].end < (i + 1) as u32) {
            open.pop();
        }
        let parent = open.last().map(|p| &found[*p]);
        let name = m[2].to_string();
        let sym = Sym {
            file: file.to_string(),
            qual: parent.map_or(name.clone(), |p| format!("{}.{}", p.qual, name)),
            kind: if &m[1] == "class" {
                Kind::Class
            } else if parent.is_some_and(|p| p.kind == Kind::Class) {
                Kind::Method
            } else {
                Kind::Fn
            },
            name,
            start: (start + 1) as u32,
            end: (end + 1) as u32,
        };
        found.push(sym);
        open.push(found.len() - 1);
    }
    found
}

#[derive(Debug, Clone)]
struct Item {
    name: String,
    /// `container` (Rust's impl and mod) only scopes the items in it.
    kind: ItemKind,
    /// Offset of the keyword, and of the body's braces (`close` the `;` of a body-less struct).
    at: usize,
    open: usize,
    close: usize,
    /// `impl` and `trait` make the functions in them methods.
    owner: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ItemKind {
    Fn,
    Class,
    Container,
}

enum Body {
    Braces(usize, usize),
    Semi(usize),
}

/// Rust, and JavaScript and TypeScript, by their braces over the masked text.
fn braced(file: &str, text: &str, language: Lang) -> Vec<Sym> {
    let masked_bytes = mask(text, language);
    let masked = String::from_utf8_lossy(&masked_bytes).into_owned();
    let m = masked.as_bytes();
    let mut pairs: HashMap<usize, usize> = HashMap::new();
    let mut stack = Vec::new();
    for (i, b) in m.iter().enumerate() {
        if *b == b'{' {
            stack.push(i);
        } else if *b == b'}'
            && let Some(o) = stack.pop()
        {
            pairs.insert(o, i);
        }
    }
    let mut starts = vec![0usize];
    for (i, b) in m.iter().enumerate() {
        if *b == b'\n' {
            starts.push(i + 1);
        }
    }
    let line_at = |offset: usize| -> u32 { starts.partition_point(|s| *s <= offset) as u32 };
    // The body after a declaration: its first `{` outside parentheses and brackets, or the `;`
    // that means it has none.
    let body = |from: usize| -> Option<Body> {
        let mut depth = 0i64;
        for (i, ch) in m.iter().enumerate().skip(from) {
            match ch {
                b'(' | b'[' => depth += 1,
                b')' | b']' => depth -= 1,
                b';' if depth <= 0 => return Some(Body::Semi(i)),
                b'}' if depth <= 0 => return None,
                b'{' if depth <= 0 => return pairs.get(&i).map(|c| Body::Braces(i, *c)),
                _ => {}
            }
        }
        None
    };
    let mut items: Vec<Item> = Vec::new();
    let add = |items: &mut Vec<Item>, name: &str, kind: ItemKind, at: usize, from: usize, owner: bool| {
        match body(from) {
            Some(Body::Braces(open, close)) => items.push(Item {
                name: name.to_string(),
                kind,
                at,
                open,
                close,
                owner,
            }),
            Some(Body::Semi(semi)) if kind == ItemKind::Class => items.push(Item {
                name: name.to_string(),
                kind,
                at,
                open: semi,
                close: semi,
                owner,
            }),
            _ => {}
        }
    };
    if language == Lang::Rs {
        static FN: LazyLock<Regex> = LazyLock::new(|| re(r"\bfn\s+(?:r#)?([A-Za-z_]\w*)"));
        static TYPE: LazyLock<Regex> =
            LazyLock::new(|| re(r"\b(struct|enum|union|trait)\s+([A-Za-z_]\w*)"));
        static MOD: LazyLock<Regex> = LazyLock::new(|| re(r"\bmod\s+([A-Za-z_]\w*)\s*\{"));
        static IMPL: LazyLock<Regex> = LazyLock::new(|| {
            re(r"(?m)^[ \t]*(?:pub(?:\([^)]*\))?\s+)?(?:unsafe\s+)?(?:default\s+)?impl\b")
        });
        for c in FN.captures_iter(&masked) {
            let all = c.get(0).unwrap();
            add(&mut items, &c[1], ItemKind::Fn, all.start(), all.end(), false);
        }
        for c in TYPE.captures_iter(&masked) {
            let all = c.get(0).unwrap();
            add(&mut items, &c[2], ItemKind::Class, all.start(), all.end(), &c[1] == "trait");
        }
        for c in MOD.captures_iter(&masked) {
            let all = c.get(0).unwrap();
            add(&mut items, &c[1], ItemKind::Container, all.start(), all.end() - 1, false);
        }
        for c in IMPL.find_iter(&masked) {
            if let Some(Body::Braces(open, close)) = body(c.end()) {
                items.push(Item {
                    name: impl_type(&masked[c.end()..open]),
                    kind: ItemKind::Container,
                    at: c.start(),
                    open,
                    close,
                    owner: true,
                });
            }
        }
    } else {
        static FUNCTION: LazyLock<Regex> =
            LazyLock::new(|| re(r"\bfunction\b\s*\*?\s*([A-Za-z_$][\w$]*)"));
        static CLASS: LazyLock<Regex> = LazyLock::new(|| re(r"\bclass\s+([A-Za-z_$][\w$]*)"));
        static ASSIGNED: LazyLock<Regex> = LazyLock::new(|| {
            re(r"\b(?:const|let|var)\s+([A-Za-z_$][\w$]*)\s*(?::[^=;]*)?=\s*(?:async\s+)?")
        });
        static LEAD: LazyLock<Regex> =
            LazyLock::new(|| re(r"^(?:function\b|\(|[A-Za-z_$][\w$]*\s*=>)"));
        static ARROW: LazyLock<Regex> = LazyLock::new(|| {
            re(r"^(?:function\b[^{;]*|(?:\([^;]*?\)|[A-Za-z_$][\w$]*)\s*(?::[^=;{]*)?=>\s*)(\{)?")
        });
        static METHOD: LazyLock<Regex> = LazyLock::new(|| {
            re(r"(?:^|[;}\n])\s*(?:(?:public|private|protected|static|async|readonly|override|abstract|get|set)\s+|\*\s*)*(#?[A-Za-z_$][\w$]*)\s*(?:<[^>(){};]*>)?\s*\(")
        });
        for c in FUNCTION.captures_iter(&masked) {
            let all = c.get(0).unwrap();
            add(&mut items, &c[1], ItemKind::Fn, all.start(), all.end(), false);
        }
        for c in CLASS.captures_iter(&masked) {
            if &c[1] == "extends" {
                continue;
            }
            let all = c.get(0).unwrap();
            if let Some(Body::Braces(open, close)) = body(all.end()) {
                items.push(Item {
                    name: c[1].to_string(),
                    kind: ItemKind::Class,
                    at: all.start(),
                    open,
                    close,
                    owner: true,
                });
            }
        }
        for c in ASSIGNED.captures_iter(&masked) {
            let all = c.get(0).unwrap();
            // An arrow function or a function expression, up to its body or the end of its
            // statement.
            let tail_end = (all.end() + 2_000).min(masked.len());
            let tail_end = (all.end()..=tail_end).rev().find(|e| masked.is_char_boundary(*e)).unwrap_or(all.end());
            let rest = &masked[all.end()..tail_end];
            if !LEAD.is_match(rest) {
                continue;
            }
            let Some(arrow) = ARROW.captures(rest) else {
                continue;
            };
            if arrow.get(1).is_some() {
                let open = all.end() + arrow.get(0).unwrap().end() - 1;
                if let Some(close) = pairs.get(&open) {
                    items.push(Item {
                        name: c[1].to_string(),
                        kind: ItemKind::Fn,
                        at: all.start(),
                        open,
                        close: *close,
                        owner: false,
                    });
                }
            } else {
                let semi = masked[all.end()..].find(';').map(|p| all.end() + p);
                let newline = masked[all.end()..].find('\n').map(|p| all.end() + p);
                let end = semi.or(newline).unwrap_or(masked.len() - 1);
                items.push(Item {
                    name: c[1].to_string(),
                    kind: ItemKind::Fn,
                    at: all.start(),
                    open: end,
                    close: end,
                    owner: false,
                });
            }
        }
        // Methods: a name and its parameters at the top level of a class body.
        let classes: Vec<Item> = items.iter().filter(|i| i.kind == ItemKind::Class).cloned().collect();
        for owner in classes {
            if owner.open == owner.close {
                continue;
            }
            let inner = &masked[owner.open + 1..owner.close];
            for c in METHOD.captures_iter(inner) {
                let name = c[1].to_string();
                if ["if", "for", "while", "switch", "catch", "return", "function", "with"]
                    .contains(&name.as_str())
                {
                    continue;
                }
                let whole = c.get(0).unwrap();
                let at = owner.open + 1 + whole.start() + whole.as_str().rfind(&name).unwrap_or(0);
                if innermost(&pairs, at) != Some(owner.open) {
                    continue;
                }
                if let Some(Body::Braces(open, close)) = body(owner.open + 1 + whole.end() - 1) {
                    items.push(Item {
                        name,
                        kind: ItemKind::Fn,
                        at,
                        open,
                        close,
                        owner: false,
                    });
                }
            }
        }
    }

    items.sort_by_key(|i| i.at);
    let lines: Vec<&str> = masked.split('\n').collect();
    let mut found = Vec::new();
    for (index, item) in items.iter().enumerate() {
        if item.kind == ItemKind::Container {
            continue;
        }
        // The items whose bodies hold this one, outermost first.
        let mut chain: Vec<&Item> = items
            .iter()
            .enumerate()
            .filter(|(j, o)| *j != index && o.open < item.at && item.at < o.close && o.open != o.close)
            .map(|(_, o)| o)
            .collect();
        chain.sort_by_key(|o| o.open);
        let parent = chain.last();
        let path: Vec<&str> = chain
            .iter()
            .filter(|o| o.kind != ItemKind::Container || o.owner)
            .map(|o| o.name.as_str())
            .collect();
        let mut start = line_at(item.at);
        // Attributes (`#[test]`) directly above belong to the item.
        if language == Lang::Rs {
            while start > 1
                && lines
                    .get(start as usize - 2)
                    .is_some_and(|l| l.trim_start().starts_with("#[") || l.trim_start().starts_with("#!["))
            {
                start -= 1;
            }
        }
        let mut qual = path.join(".");
        if !qual.is_empty() {
            qual.push('.');
        }
        qual.push_str(&item.name);
        found.push(Sym {
            file: file.to_string(),
            name: item.name.clone(),
            qual,
            kind: if item.kind == ItemKind::Class {
                Kind::Class
            } else if parent.is_some_and(|p| p.owner) {
                Kind::Method
            } else {
                Kind::Fn
            },
            start,
            end: line_at(item.close),
        });
    }
    found
}

/// The innermost brace pair holding `offset`, by its opening brace.
fn innermost(pairs: &HashMap<usize, usize>, offset: usize) -> Option<usize> {
    pairs
        .iter()
        .filter(|(open, close)| **open < offset && offset < **close)
        .map(|(open, _)| *open)
        .max()
}

/// The type an impl block is for: `impl<T> Display for Wrapper<T> where …` is `Wrapper`.
fn impl_type(header: &str) -> String {
    static GENERIC: LazyLock<Regex> = LazyLock::new(|| re(r"<[^<>]*>"));
    static WORD: LazyLock<Regex> = LazyLock::new(|| re(r"[A-Za-z_]\w*"));
    let mut text = header.to_string();
    loop {
        let next = GENERIC.replace_all(&text, " ").into_owned();
        if next == text {
            break;
        }
        text = next;
    }
    let text = text.split("where").next().unwrap_or("").to_string();
    let target = text.rsplit_once("for").map_or(text.as_str(), |(_, after)| after);
    WORD.find_iter(target)
        .map(|m| m.as_str())
        .filter(|w| *w != "dyn" && *w != "mut")
        .last()
        .unwrap_or("impl")
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn quals(path: &str, text: &str) -> Vec<(String, Kind, u32, u32)> {
        symbols_of(path, text)
            .into_iter()
            .map(|s| (s.qual, s.kind, s.start, s.end))
            .collect()
    }

    #[test]
    fn python_methods_nest_under_their_class_with_decorators_included() {
        let src = "class A:\n    @property\n    def x(self):\n        return '''\ndef not_code():\n'''\n\n    def y(self):\n        pass\n\ndef top():\n    pass\n";
        assert_eq!(
            quals("a.py", src),
            vec![
                ("A".into(), Kind::Class, 1, 9),
                ("A.x".into(), Kind::Method, 2, 6),
                ("A.y".into(), Kind::Method, 8, 9),
                ("top".into(), Kind::Fn, 11, 12),
            ]
        );
    }

    #[test]
    fn a_python_signature_spread_over_lines_is_one_header() {
        let src = "def f(a,\n      b):\n    return a\n";
        assert_eq!(quals("a.py", src), vec![("f".into(), Kind::Fn, 1, 3)]);
    }

    #[test]
    fn rust_impl_methods_are_methods_of_the_type_and_attributes_belong_to_the_item() {
        let src = "struct W;\nimpl W {\n    #[inline]\n    fn go(&self) -> u8 {\n        let s = \"fn fake() {}\";\n        1\n    }\n}\nfn free() {}\n";
        assert_eq!(
            quals("a.rs", src),
            vec![
                ("W".into(), Kind::Class, 1, 1),
                ("W.go".into(), Kind::Method, 3, 7),
                ("free".into(), Kind::Fn, 9, 9),
            ]
        );
    }

    #[test]
    fn javascript_functions_arrows_and_class_methods_are_found() {
        let src = "function a() {\n  return 1;\n}\nconst b = (x) => {\n  return x;\n};\nclass C {\n  m(x) {\n    return x;\n  }\n}\n";
        let got = quals("a.js", src);
        let names: Vec<_> = got.iter().map(|g| g.0.as_str()).collect();
        assert_eq!(names, vec!["a", "b", "C", "C.m"]);
    }

    #[test]
    fn tests_are_told_from_sources_and_secrets_are_not_listed() {
        assert!(is_test("tests/test_a.py") && is_test("pkg/a_test.py") && is_test("web/a.spec.ts"));
        assert!(!is_test("pkg/a.py"));
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("a.py"), "def f():\n    pass\n").unwrap();
        std::fs::write(dir.path().join(".env"), "KEY=1\n").unwrap();
        std::fs::write(dir.path().join("id_rsa"), "x").unwrap();
        let mut tree = Tree::open(dir.path());
        assert_eq!(tree.list(), ["a.py"]);
        assert!(tree.read(".env").is_none());
        assert_eq!(tree.map().symbols.len(), 1);
    }
}
