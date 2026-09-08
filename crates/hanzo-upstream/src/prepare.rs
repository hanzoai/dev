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
    for edit in serde_json::from_slice::<Vec<Edit>>(&std::fs::read(root.join("patches/codex.json"))?)?
    {
        let path = source.join(&edit.file);
        let text =
            std::fs::read_to_string(&path).with_context(|| format!("read {}", path.display()))?;
        // An edit that appends leaves its own anchor in place, so "the anchor is
        // still there" cannot mean "not yet applied". Finding the result is what
        // means done — and it is what keeps a second run from stacking the edit.
        if text.contains(&edit.new) {
            continue;
        }
        let found = text.matches(&edit.old).count();
        if found != edit.count {
            bail!(
                "{}: expected {} anchors, found {found}. Upstream moved; re-anchor the edit.",
                edit.file,
                edit.count
            );
        }
        std::fs::write(&path, text.replace(&edit.old, &edit.new))?;
        applied += 1;
    }

    let revision = crate::git(&source, &["rev-parse", "HEAD"])?;
    println!("upstream {}: {applied} edits applied", &revision[..12]);
    Ok(())
}
