//! Make the pinned upstream checkout ours.
//!
//! Upstream hard-codes its own product name as string literals, so wearing our
//! name means editing them. The edits land in the submodule checkout itself:
//! `git -C upstream/codex status` shows them, and that is the honest picture —
//! the checkout is a build input we own, not a pristine mirror.
//!
//! Every edit is anchored to text that must appear an exact number of times. If
//! upstream moves the ground under one, preparation stops and names the file
//! rather than branding the wrong line.

use anyhow::Context;
use anyhow::Result;
use anyhow::bail;
use serde::Deserialize;

/// One anchored substitution against an upstream file.
#[derive(Deserialize)]
struct Edit {
    file: String,
    old: String,
    new: String,
    count: usize,
}

impl Edit {
    /// The edited text, or `None` when this edit is already in the file.
    ///
    /// An edit that appends leaves its own anchor behind, so "the anchor is
    /// still there" cannot mean "not yet applied". Finding the *result* is what
    /// means done, and it is what stops a second run from stacking the edit.
    fn apply(&self, text: &str) -> Result<Option<String>> {
        if text.contains(&self.new) {
            return Ok(None);
        }
        let found = text.matches(&self.old).count();
        if found != self.count {
            bail!(
                "{}: expected {} anchors, found {found}. Upstream moved; re-anchor the edit.",
                self.file,
                self.count
            );
        }
        Ok(Some(text.replace(&self.old, &self.new)))
    }
}

pub fn run(arguments: &[String]) -> Result<()> {
    if !arguments.is_empty() {
        bail!("usage: upstream prepare");
    }
    let root = crate::root();
    let source = root.join("upstream/codex/codex-rs");
    if !source.join("Cargo.toml").is_file() {
        bail!("{}: run `git submodule update --init`", source.display());
    }

    let mut applied = 0;
    for edit in serde_json::from_slice::<Vec<Edit>>(&std::fs::read(root.join("patches/upstream.json"))?)?
    {
        let path = source.join(&edit.file);
        let text =
            std::fs::read_to_string(&path).with_context(|| format!("read {}", path.display()))?;
        if let Some(edited) = edit.apply(&text)? {
            std::fs::write(&path, edited)?;
            applied += 1;
        }
    }

    let revision = crate::git(&source, &["rev-parse", "HEAD"])?;
    println!("upstream {}: {applied} edits applied", &revision[..12]);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::Edit;

    fn edit(old: &str, new: &str, count: usize) -> Edit {
        Edit { file: "f.rs".into(), old: old.into(), new: new.into(), count }
    }

    #[test]
    fn an_edit_replaces_every_anchor_it_promised() {
        let e = edit("Codex", "Hanzo Dev", 2);
        assert_eq!(e.apply("Codex and Codex").unwrap().unwrap(), "Hanzo Dev and Hanzo Dev");
    }

    /// The bug this file shipped once: an appending edit leaves its own anchor
    /// behind, so a second run stacked it and produced duplicate lines.
    #[test]
    fn an_appending_edit_does_not_stack_on_a_second_run() {
        let e = edit("[dependencies]", "[dependencies]\nhanzo-tui = { path = \"x\" }", 1);
        let once = e.apply("[dependencies]\nanyhow = \"1\"").unwrap().unwrap();
        assert_eq!(once.matches("hanzo-tui").count(), 1);
        assert!(e.apply(&once).unwrap().is_none(), "already applied");
    }

    #[test]
    fn an_edit_already_applied_is_not_applied_again() {
        let e = edit("Codex", "Hanzo Dev", 1);
        assert!(e.apply("Hanzo Dev").unwrap().is_none());
    }

    /// The whole point of the count: upstream moving the ground under an anchor
    /// must stop preparation, not brand whatever happens to match.
    #[test]
    fn a_moved_anchor_stops_preparation() {
        let e = edit("Codex", "Hanzo Dev", 2);
        let error = e.apply("Codex once only").unwrap_err().to_string();
        assert!(error.contains("expected 2 anchors, found 1"), "{error}");
        assert!(error.contains("re-anchor"), "{error}");
    }

    #[test]
    fn an_anchor_that_vanished_stops_preparation() {
        let e = edit("Codex", "Hanzo Dev", 1);
        assert!(e.apply("nothing to match here").is_err());
    }

    /// Every edit we ship must be well-formed, or preparation fails at runtime
    /// in CI rather than here.
    #[test]
    fn the_shipped_edits_are_well_formed() {
        let raw = std::fs::read(concat!(env!("CARGO_MANIFEST_DIR"), "/../../patches/upstream.json"))
            .expect("patches/upstream.json is readable");
        let edits: Vec<Edit> = serde_json::from_slice(&raw).expect("valid JSON");
        assert!(!edits.is_empty());
        for e in &edits {
            assert!(e.count > 0, "{}: an edit that matches nothing is dead", e.file);
            assert!(!e.old.is_empty(), "{}: empty anchor matches everywhere", e.file);
            assert_ne!(e.old, e.new, "{}: a no-op edit", e.file);
        }
    }
}
