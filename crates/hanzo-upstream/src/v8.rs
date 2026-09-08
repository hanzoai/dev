//! Fetch the V8 build the JavaScript runtime links against.
//!
//! Upstream publishes the archive with a signed manifest of SHA-256 digests.
//! The manifest itself is pinned by the submodule, so nothing here trusts the
//! network: every byte is checked against a digest that came from the gitlink.

use anyhow::Context;
use anyhow::Result;
use anyhow::bail;
use std::collections::BTreeMap;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

const PROFILE: &str = "ptrcomp_sandbox_release";

pub fn run(arguments: &[String]) -> Result<()> {
    let target = match arguments {
        [] => host()?,
        [flag, value] if flag == "--target" => value.clone(),
        _ => bail!("usage: upstream v8 [--target TRIPLE]"),
    };
    if !target
        .chars()
        .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-' || c == '_')
    {
        bail!("invalid Rust target: {target}");
    }
    let root = crate::root();
    let codex = root.join("upstream/codex");
    let version = version(&codex.join("codex-rs/Cargo.toml"))?;

    let manifest = codex.join(format!(
        "third_party/v8/rusty_v8_{}_release_manifests.sha256",
        version.replace('.', "_")
    ));
    let trusted = digests(&std::fs::read_to_string(&manifest).with_context(|| {
        format!("{}: upstream does not publish this V8 version", manifest.display())
    })?);
    let name = format!("rusty_v8_{PROFILE}_{target}.sha256");
    let Some(digest) = trusted.get(&name) else {
        bail!("no upstream V8 checksum for {target}");
    };

    let directory = root.join("target/v8").join(&target);
    std::fs::create_dir_all(&directory)?;
    let base = format!("https://github.com/openai/codex/releases/download/rusty-v8-v{version}");
    let checksums = download(&directory, &base, &name, digest)?;

    let entries = digests(&std::fs::read_to_string(checksums)?);
    let archive = if target.ends_with("windows-msvc") {
        format!("rusty_v8_{PROFILE}_{target}.lib.gz")
    } else {
        format!("librusty_v8_{PROFILE}_{target}.a.gz")
    };
    let bindings = format!("src_binding_{PROFILE}_{target}.rs");
    if entries.len() != 2 || !entries.contains_key(&archive) || !entries.contains_key(&bindings) {
        bail!("expected exactly the archive and Rust binding checksums");
    }

    let mut exports = String::new();
    for (key, file) in [("RUSTY_V8_ARCHIVE", &archive), ("RUSTY_V8_SRC_BINDING_PATH", &bindings)] {
        let path = download(&directory, &base, file, &entries[file])?;
        exports.push_str(&format!("export {key}={}\n", path.canonicalize()?.display()));
        if let Ok(github) = std::env::var("GITHUB_ENV") {
            use std::io::Write;
            writeln!(
                std::fs::OpenOptions::new().append(true).open(github)?,
                "{key}={}",
                path.canonicalize()?.display()
            )?;
        }
    }
    std::fs::write(root.join("target/v8-env.sh"), exports)?;
    println!("verified upstream V8 {version} for {target}");
    Ok(())
}

fn host() -> Result<String> {
    let output = Command::new("rustc").arg("-vV").output()?;
    String::from_utf8(output.stdout)?
        .lines()
        .find_map(|line| line.strip_prefix("host: ").map(str::to_string))
        .context("rustc -vV did not report a host triple")
}

/// The exact `v8 = "=X.Y.Z"` pin from upstream's workspace manifest.
fn version(manifest: &Path) -> Result<String> {
    std::fs::read_to_string(manifest)?
        .lines()
        .find_map(|line| line.strip_prefix("v8 = \"=")?.strip_suffix('"').map(str::to_string))
        .context("no exact v8 pin in the upstream workspace manifest")
}

fn digests(text: &str) -> BTreeMap<String, String> {
    text.lines()
        .filter_map(|line| {
            let (digest, name) = line.split_once("  ")?;
            Some((name.trim().to_string(), digest.trim().to_string()))
        })
        .collect()
}

fn download(directory: &Path, base: &str, name: &str, digest: &str) -> Result<PathBuf> {
    if Path::new(name).file_name().is_none_or(|file| file != name) {
        bail!("unexpected artifact path: {name}");
    }
    let path = directory.join(name);
    if path.exists() && crate::sha256(&[&std::fs::read(&path)?]) == digest {
        return Ok(path);
    }
    let partial = path.with_extension("part");
    let status = Command::new("curl")
        .args(["-fsSL", "--retry", "4", "--connect-timeout", "20"])
        .arg(format!("{base}/{name}"))
        .arg("-o")
        .arg(&partial)
        .status()?;
    if !status.success() {
        bail!("could not download {name}");
    }
    if crate::sha256(&[&std::fs::read(&partial)?]) != digest {
        bail!("checksum mismatch: {name}");
    }
    std::fs::rename(&partial, &path)?;
    Ok(path)
}
