//! Manages the pinned upstream submodules.
//!
//! The only Hanzo edits to upstream are the anchored substitutions in
//! `patches/codex.json`, applied to the submodule checkout in place.

mod bump;
mod prepare;
mod v8;

use anyhow::Context;
use anyhow::Result;
use anyhow::bail;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

const USAGE: &str = "\
usage: upstream <command>

  prepare   apply the owned edits to the pinned upstream checkout
  bump      move the submodules to newer upstream revisions, rebasing the delta
  v8        download and verify the upstream V8 build the test suite links
";

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let rest = args.get(1..).unwrap_or_default();
    match args.first().map(String::as_str) {
        Some("prepare") => prepare::run(rest),
        Some("bump") => bump::run(rest),
        Some("v8") => v8::run(rest),
        Some("-h" | "--help") => {
            print!("{USAGE}");
            Ok(())
        }
        other => {
            eprint!("{USAGE}");
            bail!(match other {
                Some(name) => format!("unknown command: {name}"),
                None => "no command given".to_string(),
            })
        }
    }
}

/// The repository root, fixed at compile time so the binary can run from anywhere.
pub fn root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("crates/hanzo-graft sits two levels below the root")
}

pub fn git(directory: &Path, arguments: &[&str]) -> Result<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(directory)
        .args(arguments)
        .output()
        .with_context(|| format!("git {}", arguments.join(" ")))?;
    if !output.status.success() {
        bail!(
            "git {} failed in {}: {}",
            arguments.join(" "),
            directory.display(),
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(String::from_utf8(output.stdout)?.trim().to_string())
}

pub fn sha256(chunks: &[&[u8]]) -> String {
    use sha2::Digest;
    let mut hasher = sha2::Sha256::new();
    for chunk in chunks {
        hasher.update(chunk);
    }
    hex::encode(hasher.finalize())
}

/// Copy a tree, preserving symlinks and the executable bit.
pub fn copy(from: &Path, to: &Path) -> Result<()> {
    std::fs::create_dir_all(to)?;
    for entry in std::fs::read_dir(from)? {
        let entry = entry?;
        let source = entry.path();
        let destination = to.join(entry.file_name());
        let kind = entry.file_type()?;
        if kind.is_symlink() {
            link(&std::fs::read_link(&source)?, &destination)?;
        } else if kind.is_dir() {
            copy(&source, &destination)?;
        } else {
            std::fs::copy(&source, &destination)
                .with_context(|| format!("copy {}", source.display()))?;
        }
    }
    Ok(())
}

#[cfg(unix)]
fn link(target: &Path, destination: &PathBuf) -> Result<()> {
    std::os::unix::fs::symlink(target, destination)?;
    Ok(())
}

#[cfg(not(unix))]
fn link(target: &Path, destination: &PathBuf) -> Result<()> {
    // Windows reserves symlink creation for privileged processes; the content is
    // what the build needs, so resolve the link instead of reproducing it.
    let resolved = destination
        .parent()
        .map(|parent| parent.join(target))
        .unwrap_or_else(|| target.to_path_buf());
    if resolved.is_dir() {
        copy(&resolved, destination)
    } else {
        std::fs::copy(&resolved, destination)?;
        Ok(())
    }
}
