use std::process::Command;

fn git_output(args: &[&str]) -> Option<String> {
    let out = Command::new("git").args(args).output().ok()?;
    if !out.status.success() {
        return None;
    }
    let text = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if text.is_empty() {
        return None;
    }
    Some(text)
}

fn main() {
    // Prefer an explicit CODE_VERSION provided by CI; fall back to the
    // crate's package version to keep local builds sane.
    let version =
        std::env::var("CODE_VERSION").unwrap_or_else(|_| env!("CARGO_PKG_VERSION").to_string());

    // Build metadata is optional and intentionally independent from semantic
    // version parsing in runtime code. These fields are for human diagnostics
    // (`dev --version`, `dev doctor`) so users can confirm tag/hash/date.
    let git_describe = std::env::var("CODE_GIT_DESCRIBE")
        .ok()
        .filter(|v| !v.is_empty())
        .or_else(|| git_output(&["describe", "--tags", "--always", "--dirty"]));
    let git_sha = std::env::var("CODE_GIT_SHA")
        .ok()
        .filter(|v| !v.is_empty())
        .or_else(|| git_output(&["rev-parse", "--short=12", "HEAD"]));
    let git_date = std::env::var("CODE_GIT_DATE")
        .ok()
        .filter(|v| !v.is_empty())
        .or_else(|| git_output(&["show", "-s", "--format=%cs", "HEAD"]));

    let version_long = match (git_describe.as_deref(), git_date.as_deref()) {
        (Some(desc), Some(date)) => format!("{version} ({desc}, {date})"),
        (Some(desc), None) => format!("{version} ({desc})"),
        _ => version.clone(),
    };

    // Inject the version values as rustc envs so they participate in the
    // compiler invocation hash (sccache-friendly) and guarantee cache misses
    // when metadata changes.
    println!("cargo:rustc-env=CODE_VERSION={version}");
    println!("cargo:rustc-env=CODE_VERSION_LONG={version_long}");
    if let Some(desc) = git_describe {
        println!("cargo:rustc-env=CODE_GIT_DESCRIBE={desc}");
    }
    if let Some(sha) = git_sha {
        println!("cargo:rustc-env=CODE_GIT_SHA={sha}");
    }
    if let Some(date) = git_date {
        println!("cargo:rustc-env=CODE_GIT_DATE={date}");
    }

    // Ensure dependent crates rebuild when explicit build metadata changes.
    println!("cargo:rerun-if-env-changed=CODE_VERSION");
    println!("cargo:rerun-if-env-changed=CODE_GIT_DESCRIBE");
    println!("cargo:rerun-if-env-changed=CODE_GIT_SHA");
    println!("cargo:rerun-if-env-changed=CODE_GIT_DATE");
}
