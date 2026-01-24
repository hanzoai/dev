use std::process::Command;

fn main() {
    // Prefer an explicit CODE_VERSION provided by CI; fall back to git describe
    // or the crate's package version for local builds.
    let version = std::env::var("CODE_VERSION").unwrap_or_else(|_| {
        // Try to get version from git
        git_version().unwrap_or_else(|| env!("CARGO_PKG_VERSION").to_string())
    });

    // Inject the version as a rustc env so it participates in the compiler
    // invocation hash (sccache-friendly) and guarantees a cache miss when
    // the version changes.
    println!("cargo:rustc-env=CODE_VERSION={}", version);

    // Ensure dependent crates rebuild when CODE_VERSION changes even if the
    // source graph stays the same.
    println!("cargo:rerun-if-env-changed=CODE_VERSION");

    // Rebuild if git HEAD changes
    if let Some(git_dir) = find_git_dir() {
        println!("cargo:rerun-if-changed={}/HEAD", git_dir);
        println!("cargo:rerun-if-changed={}/refs/heads", git_dir);
        println!("cargo:rerun-if-changed={}/refs/tags", git_dir);
    }
}

fn git_version() -> Option<String> {
    // Try git describe --tags --always
    let output = Command::new("git")
        .args(["describe", "--tags", "--always", "--dirty=-dev"])
        .output()
        .ok()?;

    if output.status.success() {
        let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if !version.is_empty() {
            return Some(version);
        }
    }

    // Fallback: try to get short commit hash
    let output = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()?;

    if output.status.success() {
        let hash = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if !hash.is_empty() {
            return Some(format!("0.0.0-g{}", hash));
        }
    }

    None
}

fn find_git_dir() -> Option<String> {
    let output = Command::new("git")
        .args(["rev-parse", "--git-dir"])
        .output()
        .ok()?;

    if output.status.success() {
        let dir = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if !dir.is_empty() {
            return Some(dir);
        }
    }
    None
}
