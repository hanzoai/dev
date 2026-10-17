//! Edits: the smallest change a model can state, and the tool that makes it.
//!
//! An edit names a file, the line its `old` text starts on, that text exactly (a part of a line, or
//! several lines) and its replacement. It is checked against the file, never against a diff's
//! line counts, so a model that miscounts a hunk header costs nothing. A test file is never
//! edited, and a path leaves the tree never.

use crate::symbols::is_test;
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::Path;

/// Most files one answer may change.
const FILES: usize = 10;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edit {
    pub file: String,
    /// The line `old` starts on, 1-based; a hint when the text stands in several places.
    pub line: u32,
    pub old: String,
    pub new: String,
}

/// What was wrong with the JSON values that held no edits.
#[derive(Default)]
struct Miss {
    /// A value that said there are none: `{"edits": []}`.
    empty: bool,
    /// The first value that was no edit, or did not read.
    bad: Option<String>,
}

/// The first non-empty edits among the JSON values that open in `text`.
fn first(text: &str, miss: &mut Miss) -> Option<Vec<Edit>> {
    let mut pos = 0;
    while let Some(offset) = text[pos..].find(['{', '[']) {
        let at = pos + offset;
        pos = at + 1;
        match serde_json::Deserializer::from_str(&text[at..]).into_iter::<Value>().next() {
            Some(Ok(value)) => match extract_edits(&value) {
                Ok(edits) => return Some(edits),
                Err(why) if why == "no edits" => miss.empty = true,
                Err(why) => {
                    miss.bad.get_or_insert(why);
                }
            },
            Some(Err(e)) => {
                miss.bad.get_or_insert(format!("the answer's JSON does not parse: {e}"));
            }
            None => {}
        }
    }
    None
}

/// The edits in a model's answer: `{"edits": [...]}`, one edit object, or a list of them.
/// Markdown fences and prose around or before the JSON are handled robustly. When there are
/// none, the error says which of three it is: no edits, JSON that does not read or is no edit,
/// or no JSON.
pub fn parse(answer: &str) -> Result<Vec<Edit>, String> {
    let mut miss = Miss::default();
    // 1. Markdown code blocks: ```json ... ``` or ``` ... ```
    let mut search = answer;
    while let Some(start) = search.find("```") {
        let after_fence = &search[start + 3..];
        let content_start = after_fence.find('\n').map_or(0, |i| i + 1);
        let block = &after_fence[content_start..];
        let Some(end) = block.find("```") else {
            break;
        };
        if let Some(edits) = first(block[..end].trim(), &mut miss) {
            return Ok(edits);
        }
        search = &block[end + 3..];
    }
    // 2. Every opening brace or bracket of the answer, for a valid edit JSON structure.
    if let Some(edits) = first(answer, &mut miss) {
        return Ok(edits);
    }
    match miss {
        Miss { empty: true, .. } => Err("no edits".into()),
        Miss { bad: Some(why), .. } => Err(why),
        _ => Err("the answer holds no JSON".into()),
    }
}

fn extract_edits(value: &Value) -> Result<Vec<Edit>, String> {
    let list: Vec<Value> = match value {
        Value::Array(a) => a.clone(),
        Value::Object(o) if o.contains_key("edits") => value["edits"]
            .as_array()
            .cloned()
            .ok_or_else(|| "`edits` is not a list".to_string())?,
        Value::Object(o) if o.contains_key("file") || (o.contains_key("old") && o.contains_key("new")) => {
            vec![value.clone()]
        }
        _ => return Err("the answer's JSON is not an edit".into()),
    };
    if list.is_empty() {
        return Err("no edits".into());
    }
    list.iter().map(edit).collect()
}

/// Whether a refusal is about how an answer is written and not about what it says: the model can
/// state the same edit again so that it reads and applies.
pub fn spelling(why: &str) -> bool {
    [
        "holds no JSON",
        "does not parse",
        "is not a list",
        "is not an edit",
        "an edit lacks",
        "does not stand in the file",
        "name a longer text",
        "is empty",
        "no such file",
    ]
    .iter()
    .any(|m| why.contains(m))
}

/// For an answer whose old text is not in its file: the file's own lines around where the edit
/// says it stands (or where its first line is found), numbered, for the model to say it again.
pub fn near(root: &Path, answer: &str) -> String {
    let Ok(edits) = parse(answer) else {
        return String::new();
    };
    let mut out = String::new();
    for e in edits.iter().take(3) {
        if !safe(&e.file) || is_test(&e.file) {
            continue;
        }
        let Ok(text) = std::fs::read_to_string(root.join(&e.file)) else {
            continue;
        };
        if locate(&text, e).is_ok() {
            continue;
        }
        let lines: Vec<&str> = text.lines().collect();
        let first = e.old.lines().map(str::trim).find(|l| !l.is_empty());
        let at = Some(e.line as usize)
            .filter(|l| (1..=lines.len()).contains(l))
            .or_else(|| first.and_then(|f| lines.iter().position(|l| l.contains(f)).map(|i| i + 1)));
        let Some(at) = at else {
            continue;
        };
        let (lo, hi) = (at.saturating_sub(8).max(1), (at + 8).min(lines.len()));
        out.push_str(&format!("\n{} as it stands, lines {lo} to {hi} (line numbers are not part of the code):\n", e.file));
        for i in lo..=hi {
            out.push_str(&format!("{i:>5}  {}\n", lines[i - 1]));
        }
    }
    out
}

fn edit(v: &Value) -> Result<Edit, String> {
    let text = |key: &str| {
        v[key]
            .as_str()
            .map(str::to_string)
            .ok_or_else(|| format!("an edit lacks `{key}`"))
    };
    let line = v["line"]
        .as_u64()
        .or_else(|| v["start"].as_u64())
        .or_else(|| v["line"].as_str().and_then(|s| s.parse().ok()))
        .unwrap_or(0);
    Ok(Edit {
        file: text("file")?,
        line: u32::try_from(line).unwrap_or(0),
        old: text("old")?,
        new: text("new")?,
    })
}

/// A change made to the tree, with what it replaced.
#[derive(Debug, Clone, Default)]
pub struct Applied {
    /// File -> its text before the change.
    before: BTreeMap<String, String>,
    /// One line an edit: `file:line -old +new`, for a retry's prompt and the report.
    pub summary: Vec<String>,
}

impl Applied {
    /// Each file changed with the digest of its text before the change.
    pub fn originals(&self) -> Vec<(String, String)> {
        self.before
            .iter()
            .map(|(f, t)| (f.clone(), crate::ledger::digest(t.as_bytes())))
            .collect()
    }

    pub fn files(&self) -> Vec<String> {
        self.before.keys().cloned().collect()
    }

    /// Each file changed with its text before the change.
    pub fn texts(&self) -> &BTreeMap<String, String> {
        &self.before
    }
}

fn safe(file: &str) -> bool {
    !file.is_empty()
        && !file.starts_with('/')
        && !file.contains('\0')
        && !file.split('/').any(|p| p == ".." || p.is_empty() || p.starts_with('.'))
}

/// The byte offsets `old` starts at in `text`.
fn occurrences(text: &str, old: &str) -> Vec<usize> {
    text.match_indices(old).map(|(i, _)| i).collect()
}

fn line_of(text: &str, offset: usize) -> u32 {
    text[..offset].bytes().filter(|b| *b == b'\n').count() as u32 + 1
}

/// Where `old` stands in `text`: its one place, or of several the nearest to `line` when that
/// is clear. Trailing whitespace on a line is not compared.
fn locate(text: &str, edit: &Edit) -> Result<(usize, usize), String> {
    let mut at = occurrences(text, &edit.old);
    let mut len = edit.old.len();
    if at.is_empty() {
        // The model's lines may differ from the file's in trailing space only.
        let strip = |s: &str| s.split('\n').map(str::trim_end).collect::<Vec<_>>().join("\n");
        let want = strip(&edit.old);
        if want.is_empty() {
            return Err(format!("the old text of {} is empty", edit.file));
        }
        let lines: Vec<&str> = text.split('\n').collect();
        let n = want.split('\n').count();
        let mut found = Vec::new();
        let mut offset = 0;
        for i in 0..lines.len() {
            if i + n <= lines.len() && strip(&lines[i..i + n].join("\n")) == want {
                found.push((offset, lines[i..i + n].join("\n").len()));
            }
            offset += lines[i].len() + 1;
        }
        return match found.as_slice() {
            [one] => Ok(*one),
            [] => Err(format!(
                "the old text of {}:{} does not stand in the file",
                edit.file, edit.line
            )),
            _ => nearest(text, edit, found.iter().map(|f| f.0).collect(), found[0].1),
        };
    }
    if at.len() == 1 {
        return Ok((at.remove(0), len));
    }
    len = edit.old.len();
    nearest(text, edit, at, len)
}

fn nearest(text: &str, edit: &Edit, at: Vec<usize>, len: usize) -> Result<(usize, usize), String> {
    let mut by: Vec<(u32, usize)> = at
        .iter()
        .map(|o| (line_of(text, *o).abs_diff(edit.line), *o))
        .collect();
    by.sort();
    if by.len() > 1 && by[0].0 == by[1].0 {
        return Err(format!(
            "the old text of {}:{} stands in {} places; name a longer text",
            edit.file,
            edit.line,
            by.len()
        ));
    }
    Ok((by[0].1, len))
}

fn clip(s: &str) -> String {
    let one = s.replace('\n', "\\n");
    match one.char_indices().nth(160) {
        Some((i, _)) => format!("{}...", &one[..i]),
        None => one,
    }
}

/// Makes `edits` in the tree at `root`, all or none: a change that cannot be made leaves every
/// file as it was.
pub fn apply(root: &Path, edits: &[Edit]) -> Result<Applied, String> {
    let mut by: BTreeMap<&str, Vec<&Edit>> = BTreeMap::new();
    for e in edits {
        if !safe(&e.file) {
            return Err(format!("{:?} is not a path in the tree", e.file));
        }
        if is_test(&e.file) {
            return Err(format!("{} is a test file; tests are not edited", e.file));
        }
        if e.old.is_empty() {
            return Err(format!("the old text of {}:{} is empty", e.file, e.line));
        }
        by.entry(&e.file).or_default().push(e);
    }
    if by.len() > FILES {
        return Err(format!("{} files in one answer; at most {FILES}", by.len()));
    }
    let mut applied = Applied::default();
    let mut after: BTreeMap<String, String> = BTreeMap::new();
    for (file, es) in by {
        let path = root.join(file);
        let meta = std::fs::symlink_metadata(&path).map_err(|_| format!("no such file {file}"))?;
        if !meta.is_file() {
            return Err(format!("{file} is not a regular file"));
        }
        let original = std::fs::read_to_string(&path).map_err(|e| format!("{file}: {e}"))?;
        let mut text = original.clone();
        // Edits of one file from the bottom up, so an earlier edit's lines do not move.
        let mut placed = Vec::new();
        for e in &es {
            let (start, len) = locate(&original, e)?;
            placed.push((start, len, *e));
        }
        placed.sort_by_key(|p| std::cmp::Reverse(p.0));
        for pair in placed.windows(2) {
            if pair[1].0 + pair[1].1 > pair[0].0 {
                return Err(format!("two edits of {file} overlap"));
            }
        }
        for (start, len, e) in placed {
            applied.summary.push(format!(
                "{file}:{} -{} +{}",
                line_of(&original, start),
                clip(&e.old),
                clip(&e.new)
            ));
            text.replace_range(start..start + len, &e.new);
        }
        applied.before.insert(file.to_string(), original);
        after.insert(file.to_string(), text);
    }
    for (file, text) in &after {
        std::fs::write(root.join(file), text).map_err(|e| format!("{file}: {e}"))?;
    }
    Ok(applied)
}

/// Puts the files `applied` changed back. False when a file cannot be written.
pub fn revert(root: &Path, applied: &Applied) -> bool {
    applied
        .before
        .iter()
        .all(|(file, text)| std::fs::write(root.join(file), text).is_ok())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn tree(files: &[(&str, &str)]) -> tempfile::TempDir {
        let dir = tempfile::tempdir().unwrap();
        for (name, text) in files {
            let path = dir.path().join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        dir
    }

    #[test]
    fn an_answer_with_prose_and_fences_around_its_json_parses() {
        let a = "Here:\n```json\n{\"edits\":[{\"file\":\"a.py\",\"line\":3,\"old\":\"x - 1\",\"new\":\"x + 1\"}]}\n```\n";
        assert_eq!(
            parse(a).unwrap(),
            vec![Edit { file: "a.py".into(), line: 3, old: "x - 1".into(), new: "x + 1".into() }]
        );
        let with_braces_in_prose = "I looked at {function_name} and [array_index]. The fix is:\n```json\n{\"edits\":[{\"file\":\"a.py\",\"line\":3,\"old\":\"x - 1\",\"new\":\"x + 1\"}]}\n```\n";
        assert_eq!(
            parse(with_braces_in_prose).unwrap(),
            vec![Edit { file: "a.py".into(), line: 3, old: "x - 1".into(), new: "x + 1".into() }]
        );
        let single = "{\"file\":\"a.py\",\"line\":\"3\",\"old\":\"a\",\"new\":\"b\"}";
        assert_eq!(parse(single).unwrap()[0].line, 3);
        assert!(parse("no json here").is_err());
        assert!(parse("{\"edits\":[]}").is_err());
    }

    #[test]
    fn a_miss_says_which_it_is_no_edits_json_that_is_no_edit_or_that_does_not_read_or_no_json() {
        assert_eq!(parse("so {\"edits\":[]}").unwrap_err(), "no edits");
        // Braces in the prose that do not read do not hide an honest empty answer.
        assert_eq!(parse("I looked at {function_name}. {\"edits\":[]}").unwrap_err(), "no edits");
        assert_eq!(parse("a {\"b\":1} c").unwrap_err(), "the answer's JSON is not an edit");
        assert!(parse("{\"edits\":[{\"file\":\"a.py\",\"old\":\"x").unwrap_err().contains("does not parse"));
        assert_eq!(parse("{\"edits\": 3}").unwrap_err(), "`edits` is not a list");
        assert_eq!(parse("{\"edits\":[{\"file\":\"a.py\"}]}").unwrap_err(), "an edit lacks `old`");
        assert_eq!(parse("no json here").unwrap_err(), "the answer holds no JSON");
        // An answer that reasons first and ends in the edit still parses.
        let edit = "{\"edits\":[{\"file\":\"a.py\",\"line\":1,\"old\":\"p\",\"new\":\"q\"}]}";
        assert_eq!(parse(&format!("Try {{x}}, [] and then: {edit}")).unwrap()[0].old, "p");
    }

    #[test]
    fn only_how_an_answer_is_written_is_said_again_and_the_file_is_shown_where_it_was_meant() {
        for why in ["the answer holds no JSON", "the answer's JSON does not parse: x", "an edit lacks `file`", "the old text of a.py:3 does not stand in the file", "the old text of a.py:3 stands in 2 places; name a longer text"] {
            assert!(spelling(why), "{why}");
        }
        for why in ["no edits", "a.py is a test file; tests are not edited", "\"\" is not a path in the tree", "the edit repeats an earlier attempt's: the workspace would be in a state already tried"] {
            assert!(!spelling(why), "{why}");
        }
        let dir = tree(&[("pkg/a.py", "one\ntwo\nthree\nfour\n")]);
        let said = "{\"edits\":[{\"file\":\"pkg/a.py\",\"line\":3,\"old\":\"thr33\",\"new\":\"x\"}]}";
        let shown = near(dir.path(), said);
        assert!(shown.contains("pkg/a.py as it stands, lines 1 to 4") && shown.contains("    3  three"), "{shown}");
        // Where the line is not given, the file's own line that holds the old text's first line is found.
        let by_text = near(dir.path(), "{\"edits\":[{\"file\":\"pkg/a.py\",\"line\":0,\"old\":\"two\\nzzz\",\"new\":\"x\"}]}");
        assert!(by_text.contains("    2  two"), "{by_text}");
        // A text that stands, a test file and an answer that does not read show nothing.
        assert_eq!(near(dir.path(), "{\"edits\":[{\"file\":\"pkg/a.py\",\"line\":3,\"old\":\"three\",\"new\":\"x\"}]}"), "");
        assert_eq!(near(dir.path(), "not json"), "");
    }

    #[test]
    fn a_part_of_a_line_is_replaced_and_reverted_exactly() {
        let dir = tree(&[("pkg/a.py", "def f(x):\n    return x - 1\n")]);
        let e = Edit { file: "pkg/a.py".into(), line: 2, old: "x - 1".into(), new: "x + 1".into() };
        let applied = apply(dir.path(), &[e]).unwrap();
        assert_eq!(std::fs::read_to_string(dir.path().join("pkg/a.py")).unwrap(), "def f(x):\n    return x + 1\n");
        assert_eq!(applied.summary, vec!["pkg/a.py:2 -x - 1 +x + 1".to_string()]);
        assert!(revert(dir.path(), &applied));
        assert_eq!(std::fs::read_to_string(dir.path().join("pkg/a.py")).unwrap(), "def f(x):\n    return x - 1\n");
    }

    #[test]
    fn the_nearest_place_wins_and_a_tie_is_refused() {
        let dir = tree(&[("a.py", "a = 1\nb = 2\na = 1\n")]);
        let near = Edit { file: "a.py".into(), line: 3, old: "a = 1".into(), new: "a = 9".into() };
        apply(dir.path(), &[near]).unwrap();
        assert_eq!(std::fs::read_to_string(dir.path().join("a.py")).unwrap(), "a = 1\nb = 2\na = 9\n");
        let dir = tree(&[("a.py", "a = 1\nb = 2\na = 1\n")]);
        let tie = Edit { file: "a.py".into(), line: 2, old: "a = 1".into(), new: "a = 9".into() };
        let e = apply(dir.path(), &[tie]).unwrap_err();
        assert!(e.contains("2 places"), "{e}");
    }

    #[test]
    fn trailing_space_is_not_compared_and_nothing_changes_on_a_refusal() {
        let dir = tree(&[("a.py", "if x:  \n    y = 1\n"), ("b.py", "z = 0\n")]);
        let ok = Edit { file: "a.py".into(), line: 1, old: "if x:\n    y = 1".into(), new: "if x:\n    y = 2".into() };
        let bad = Edit { file: "b.py".into(), line: 1, old: "nope".into(), new: "z = 1".into() };
        assert!(apply(dir.path(), &[ok.clone(), bad]).is_err());
        assert_eq!(std::fs::read_to_string(dir.path().join("a.py")).unwrap(), "if x:  \n    y = 1\n");
        apply(dir.path(), &[ok]).unwrap();
        assert_eq!(std::fs::read_to_string(dir.path().join("a.py")).unwrap(), "if x:\n    y = 2\n");
    }

    #[test]
    fn tests_and_paths_outside_the_tree_are_never_edited() {
        let dir = tree(&[("tests/test_a.py", "assert 1\n"), ("a.py", "x\n")]);
        for file in ["tests/test_a.py", "../a.py", "/etc/passwd", ".git/config"] {
            let e = Edit { file: file.into(), line: 1, old: "x".into(), new: "y".into() };
            assert!(apply(dir.path(), &[e]).is_err(), "{file}");
        }
    }
}
