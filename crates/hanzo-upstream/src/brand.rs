//! The product's name, wherever upstream prints its own.
//!
//! Anchored edits in `patches/upstream.json` cover wording that has to change in
//! one particular way. The name is a rule instead: in a string literal of
//! non-test code, the word upstream calls itself by becomes ours. A rule survives
//! an upstream that rewords a message, where an anchor would stop preparation at
//! every bump. `patches/brand.json` says where it applies and what it leaves
//! alone: names a file, a package or a wire value really carries.

use anyhow::Context;
use anyhow::Result;
use serde::Deserialize;
use std::path::Path;
use std::path::PathBuf;

#[derive(Deserialize)]
struct Rule {
    /// Directories under the checkout where the rule applies.
    roots: Vec<String>,
    /// A file whose path contains any of these is left alone.
    skip_files: Vec<String>,
    /// A literal containing any of these is left alone.
    skip_literals: Vec<String>,
    /// Whole phrases, replaced before the word itself.
    phrases: Vec<[String; 2]>,
    word: String,
    to: String,
}

/// Rebrand the checkout and say how many literals changed.
pub fn run(root: &Path, source: &Path) -> Result<usize> {
    let rule: Rule = serde_json::from_slice(&std::fs::read(root.join("patches/brand.json"))?)
        .context("patches/brand.json")?;
    let mut changed = 0;
    for directory in &rule.roots {
        for path in rust_files(&source.join(directory))? {
            let relative = path.strip_prefix(source).unwrap_or(&path).to_string_lossy().into_owned();
            if is_test_path(&relative) || rule.skip_files.iter().any(|skip| relative.contains(skip)) {
                continue;
            }
            let text = std::fs::read_to_string(&path)
                .with_context(|| format!("read {}", path.display()))?;
            if let Some((edited, count)) = rebrand(&rule, &text) {
                std::fs::write(&path, edited)?;
                changed += count;
            }
        }
    }
    Ok(changed)
}

fn rust_files(directory: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    let mut pending = vec![directory.to_path_buf()];
    while let Some(next) = pending.pop() {
        let Ok(entries) = std::fs::read_dir(&next) else {
            continue;
        };
        for entry in entries {
            let path = entry?.path();
            if path.is_dir() {
                pending.push(path);
            } else if path.extension().is_some_and(|extension| extension == "rs") {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}

fn is_test_path(path: &str) -> bool {
    path.ends_with("_test.rs")
        || path.ends_with("_tests.rs")
        || path.ends_with("/tests.rs")
        || path.contains("/test/")
        || path.contains("/tests/")
        || path.contains("snapshots")
        || path.contains("test_support")
        || path.contains("test_fixtures")
}

/// Where a file's test code starts: everything from here on is left alone.
fn starts_tests(line: &str) -> bool {
    let line = line.trim_start();
    line.starts_with("#[cfg(test)]")
        || line
            .strip_prefix("mod tests")
            .is_some_and(|rest| rest.chars().next().is_none_or(|next| !is_word(next)))
}

fn is_word(character: char) -> bool {
    character.is_ascii_alphanumeric() || character == '_'
}

/// The text with its literals rebranded, and how many changed.
fn rebrand(rule: &Rule, text: &str) -> Option<(String, usize)> {
    let mut out = String::with_capacity(text.len());
    let mut count = 0;
    let mut in_tests = false;
    for line in text.split_inclusive('\n') {
        in_tests = in_tests || starts_tests(line);
        let mentions = line.contains(&rule.word) || rule.phrases.iter().any(|[from, _]| line.contains(from));
        if in_tests || line.trim_start().starts_with("//") || !mentions {
            out.push_str(line);
            continue;
        }
        let mut rest = 0;
        for (start, end) in literals(line) {
            let literal = &line[start..end];
            if rule.skip_literals.iter().any(|skip| literal.contains(skip)) {
                continue;
            }
            let new = name(rule, literal);
            if new != literal {
                out.push_str(&line[rest..start]);
                out.push_str(&new);
                rest = end;
                count += 1;
            }
        }
        out.push_str(&line[rest..]);
    }
    (count > 0).then_some((out, count))
}

/// The byte ranges of the string literals on one line: a quote, then anything
/// but a quote or a backslash, or a backslash and the character it escapes, up
/// to the closing quote. An opening quote with no closing one on the line is not
/// a literal.
fn literals(line: &str) -> Vec<(usize, usize)> {
    let bytes = line.as_bytes();
    let mut found = Vec::new();
    let mut at = 0;
    while at < bytes.len() {
        // The character `'"'` is a quote that opens nothing.
        if bytes[at..].starts_with(b"'\"'") {
            at += 3;
            continue;
        }
        if bytes[at] != b'"' {
            at += 1;
            continue;
        }
        let mut end = at + 1;
        let mut closed = false;
        while end < bytes.len() {
            match bytes[end] {
                b'"' => {
                    closed = true;
                    break;
                }
                b'\\' if end + 1 < bytes.len() && bytes[end + 1] != b'\n' => end += 2,
                b'\\' | b'\n' => break,
                _ => end += 1,
            }
        }
        if closed {
            found.push((at, end + 1));
            at = end + 1;
        } else {
            at += 1;
        }
    }
    found
}

fn name(rule: &Rule, literal: &str) -> String {
    let mut text = literal.to_string();
    for [from, to] in &rule.phrases {
        text = text.replace(from, to);
    }
    let mut out = String::with_capacity(text.len());
    let mut last = 0;
    for (at, _) in text.match_indices(&rule.word) {
        let before = text[..at].chars().next_back();
        let after = text[at + rule.word.len()..].chars().next();
        let starts_word = before.is_some_and(is_word) && !text[..at].ends_with("\\n");
        if starts_word || after.is_some_and(is_word) {
            continue;
        }
        out.push_str(&text[last..at]);
        out.push_str(&rule.to);
        last = at + rule.word.len();
    }
    out.push_str(&text[last..]);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rule() -> Rule {
        Rule {
            roots: vec![],
            skip_files: vec![],
            skip_literals: vec!["Codex Apps".into(), "## ".into()],
            phrases: vec![["OpenAI Codex".into(), "Hanzo Dev".into()], ["`codex ".into(), "`dev ".into()]],
            word: "Codex".into(),
            to: "Dev".into(),
        }
    }

    fn brand(text: &str) -> String {
        rebrand(&rule(), text).map_or_else(|| text.to_string(), |(edited, _)| edited)
    }

    #[test]
    fn the_word_in_a_literal_becomes_ours() {
        assert_eq!(brand("let a = \"Codex is ready\";\n"), "let a = \"Dev is ready\";\n");
        assert_eq!(brand("x(\"OpenAI Codex v1\")\n"), "x(\"Hanzo Dev v1\")\n");
        assert_eq!(brand("x(\"run `codex login`\")\n"), "x(\"run `dev login`\")\n");
    }

    #[test]
    fn an_identifier_is_not_a_name() {
        let code = "let a = CodexAuth::new(\"CodexOp\", \"my_Codex\", \"Codex2\");\n";
        assert_eq!(brand(code), code);
    }

    #[test]
    fn a_word_after_a_line_break_in_a_literal_is_a_name() {
        assert_eq!(brand("x(\"one\\nCodex exits\")\n"), "x(\"one\\nDev exits\")\n");
    }

    #[test]
    fn a_literal_that_names_a_real_thing_is_left_alone() {
        let code = "x(\"Codex Apps tools\", \"y\");\nx(\"## My request for Codex:\");\n";
        assert_eq!(brand(code), code);
    }

    #[test]
    fn only_the_literal_changes_not_the_code_around_it() {
        assert_eq!(
            brand("fn f(codex: Codex) { g(\"Codex\", Codex::new()); }\n"),
            "fn f(codex: Codex) { g(\"Dev\", Codex::new()); }\n"
        );
    }

    #[test]
    fn comments_and_tests_are_left_alone() {
        let code = "// say \"Codex\"\nlet a = 1;\n#[cfg(test)]\nmod tests {\n  let b = \"Codex\";\n}\n";
        assert_eq!(brand(code), code);
        let code = "mod tests_support_free {}\nlet a = \"Codex\";\n";
        assert_eq!(brand(code), "mod tests_support_free {}\nlet a = \"Dev\";\n");
    }

    #[test]
    fn a_quote_that_never_closes_is_not_a_literal() {
        let code = "let c = '\"'; let s = \"Codex\";\n";
        let found = literals(code);
        assert_eq!(found.len(), 1);
        assert_eq!(&code[found[0].0..found[0].1], "\"Codex\"");
        let continued = "push_str(\"first\\nCodex exits\\\n";
        assert!(literals(continued).is_empty());
    }

    #[test]
    fn a_second_pass_changes_nothing() {
        let once = brand("a(\"Codex\"); b(\"`codex x`\");\n");
        assert!(rebrand(&rule(), &once).is_none());
    }
}
