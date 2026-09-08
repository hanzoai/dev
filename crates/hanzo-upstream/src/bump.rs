//! Move the pinned upstream submodules to newer revisions.
//!
//! The gitlink is the whole of the pin. Our edits live in the checkout, so they
//! are discarded before moving and re-applied by `prepare` afterwards.

use anyhow::Result;
use anyhow::bail;

pub fn run(arguments: &[String]) -> Result<()> {
    let mut revisions = [
        ("codex", "origin/main".to_string()),
        ("code", "origin/main".to_string()),
    ];
    let mut rest = arguments.iter();
    while let Some(flag) = rest.next() {
        let name = flag.strip_prefix("--").unwrap_or_default();
        let Some(slot) = revisions.iter_mut().find(|(key, _)| *key == name) else {
            bail!("usage: upstream bump [--codex REV] [--code REV]");
        };
        let Some(value) = rest.next() else {
            bail!("{flag} needs a revision");
        };
        slot.1 = value.clone();
    }
    for (name, revision) in &revisions {
        let repository = crate::root().join("upstream").join(name);
        crate::git(&repository, &["reset", "--quiet", "--hard"])?;
        crate::git(&repository, &["fetch", "--quiet", "origin"])?;
        crate::git(&repository, &["checkout", "--quiet", "--detach", revision])?;
        println!("{name}: {}", crate::git(&repository, &["rev-parse", "HEAD"])?);
    }
    Ok(())
}
