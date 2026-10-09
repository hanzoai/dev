//! The product's version, wherever upstream reads its own.
//!
//! Upstream compiles `env!("CARGO_PKG_VERSION")` into dozens of crates: the
//! header, the User-Agent, `client_version`, telemetry; a bare `version` in
//! clap's `#[command(...)]` expands to the same read. A release that stamps
//! its number into every manifest therefore recompiles every one of them, and
//! nothing a cache saved from the last release can be used. The version is a
//! rule instead: each of those reads asks `hanzo_version::get()` at run time,
//! the binary crate sets it from `DEV_VERSION`, and a new release recompiles
//! the binary crate alone.
//!
//! A rule, like the name, because an anchor per read would miss the next one
//! upstream adds, and that one would quietly print the tree's version. A read in
//! a `const` cannot become a call, so the few upstream has are anchored edits in
//! `patches/upstream.json`, applied before this; the build fails on any other.

use anyhow::Context;
use anyhow::Result;
use std::path::Path;
use std::path::PathBuf;

const CRATE: &str = "hanzo-version";
const READ: &str = "env!(\"CARGO_PKG_VERSION\")";
const CALL: &str = "hanzo_version::get()";

/// Route every version read in the checkout to the run-time value, give each
/// crate that now names it the dependency, and say how many reads changed.
pub fn run(root: &Path, source: &Path) -> Result<usize> {
    let mut reads = 0;
    let mut readers = Vec::new();
    for path in rust_files(source)? {
        // A build script runs before the crate exists; it has no run time to ask.
        if path.file_name().is_some_and(|name| name == "build.rs") {
            continue;
        }
        let text =
            std::fs::read_to_string(&path).with_context(|| format!("read {}", path.display()))?;
        let (edited, versions) = clap_versions(&text.replace(READ, CALL));
        let found = text.matches(READ).count() + versions;
        if found > 0 {
            std::fs::write(&path, edited)?;
            reads += found;
        }
        if found > 0 || text.contains("hanzo_version::") {
            if let Some(manifest) = manifest_of(&path, source) {
                readers.push(manifest);
            }
        }
    }
    readers.sort();
    readers.dedup();
    for manifest in readers {
        let text = std::fs::read_to_string(&manifest)
            .with_context(|| format!("read {}", manifest.display()))?;
        let to = dependency_path(root, manifest.parent().unwrap_or(source));
        if let Some(edited) = depend(&text, &to) {
            std::fs::write(&manifest, edited)?;
        }
    }
    Ok(reads)
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
                if path.file_name().is_none_or(|name| name != "target") {
                    pending.push(path);
                }
            } else if path.extension().is_some_and(|extension| extension == "rs") {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}

/// The text with every bare `version` item of a `#[command(...)]` or
/// `#[clap(...)]` attribute given the run-time value, and how many there were.
/// An item is bare when a `(` or `,` comes before it and a `,` or `)` after.
fn clap_versions(text: &str) -> (String, usize) {
    let mut out = String::with_capacity(text.len());
    let mut count = 0;
    let mut rest = text;
    while let Some(open) = ["#[command(", "#[clap("]
        .iter()
        .filter_map(|a| rest.find(a).map(|at| at + a.len()))
        .min()
    {
        let Some(close) = rest[open..].find(")]").map(|at| open + at) else {
            break;
        };
        out.push_str(&rest[..open]);
        let body = &rest[open..close];
        let mut at = 0;
        while let Some(found) = body[at..].find("version").map(|offset| at + offset) {
            let before = body[..found].trim_end().chars().last();
            let after = body[found + "version".len()..].trim_start().chars().next();
            out.push_str(&body[at..found]);
            if matches!(before, None | Some(',')) && matches!(after, None | Some(',')) {
                out.push_str("version = ");
                out.push_str(CALL);
                count += 1;
            } else {
                out.push_str("version");
            }
            at = found + "version".len();
        }
        out.push_str(&body[at..]);
        rest = &rest[close..];
    }
    out.push_str(rest);
    (out, count)
}

/// The manifest of the package a source file belongs to: the nearest one above it
/// that declares a package, never the workspace's.
fn manifest_of(file: &Path, source: &Path) -> Option<PathBuf> {
    let mut directory = file.parent();
    while let Some(next) = directory {
        let manifest = next.join("Cargo.toml");
        if std::fs::read_to_string(&manifest)
            .is_ok_and(|text| text.lines().any(|line| line.trim() == "[package]"))
        {
            return Some(manifest);
        }
        if next == source {
            return None;
        }
        directory = next.parent();
    }
    None
}

/// `crates/hanzo-version` as a path from a package's directory.
fn dependency_path(root: &Path, package: &Path) -> String {
    let depth = package
        .strip_prefix(root)
        .map_or(0, |relative| relative.components().count());
    format!("{}crates/{CRATE}", "../".repeat(depth))
}

/// The manifest with the dependency added, or `None` when it already has it.
///
/// The line goes at the end of `[dependencies]`, not straight after its header:
/// anchored edits in `patches/upstream.json` add lines right after the header
/// and know themselves applied by finding the header and their line together.
fn depend(text: &str, to: &str) -> Option<String> {
    let lines: Vec<&str> = text.split_inclusive('\n').collect();
    if lines.iter().any(|line| {
        line.trim_start().starts_with(&format!("{CRATE} "))
            || line.trim_start().starts_with(&format!("{CRATE}="))
    }) {
        return None;
    }
    let entry = format!("{CRATE} = {{ path = \"{to}\" }}\n");
    let Some(header) = lines
        .iter()
        .position(|line| line.trim() == "[dependencies]")
    else {
        let separator = if text.is_empty() || text.ends_with('\n') {
            ""
        } else {
            "\n"
        };
        return Some(format!("{text}{separator}\n[dependencies]\n{entry}"));
    };
    let end = lines[header + 1..]
        .iter()
        .position(|line| line.trim_start().starts_with('['))
        .map_or(lines.len(), |offset| header + 1 + offset);
    // After the table's last entry, so the blank line before the next table stays.
    let mut at = end;
    while at > header + 1 && lines[at - 1].trim().is_empty() {
        at -= 1;
    }
    let mut out = String::with_capacity(text.len() + entry.len() + 1);
    for line in &lines[..at] {
        out.push_str(line);
    }
    if !out.ends_with('\n') {
        out.push('\n');
    }
    out.push_str(&entry);
    for line in &lines[at..] {
        out.push_str(line);
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_dependency_closes_the_table_and_leaves_an_edit_behind_the_header_whole() {
        let manifest = "[package]\nname = \"codex-tui\"\n\n[dependencies]\nhanzo-tui = { path = \"x\" }\nanyhow = \"1\"\n\n[dev-dependencies]\ninsta = \"1\"\n";
        let edited = depend(manifest, "../crates/hanzo-version").unwrap();
        assert_eq!(
            edited,
            "[package]\nname = \"codex-tui\"\n\n[dependencies]\nhanzo-tui = { path = \"x\" }\nanyhow = \"1\"\nhanzo-version = { path = \"../crates/hanzo-version\" }\n\n[dev-dependencies]\ninsta = \"1\"\n"
        );
        assert!(
            edited.contains("[dependencies]\nhanzo-tui = { path = \"x\" }"),
            "the anchored edit still finds itself"
        );
        assert!(
            depend(&edited, "../crates/hanzo-version").is_none(),
            "a second run adds nothing"
        );
    }

    #[test]
    fn a_table_that_runs_to_the_end_of_the_file_takes_the_line_last() {
        let edited = depend(
            "[package]\nname = \"a\"\n[dependencies]\nserde = \"1\"",
            "p",
        )
        .unwrap();
        assert_eq!(
            edited,
            "[package]\nname = \"a\"\n[dependencies]\nserde = \"1\"\nhanzo-version = { path = \"p\" }\n"
        );
    }

    #[test]
    fn a_package_with_no_dependencies_gets_the_table() {
        let edited = depend("[package]\nname = \"a\"\n", "p").unwrap();
        assert_eq!(
            edited,
            "[package]\nname = \"a\"\n\n[dependencies]\nhanzo-version = { path = \"p\" }\n"
        );
    }

    #[test]
    fn a_target_table_is_not_the_dependencies_table() {
        let manifest = "[package]\nname = \"a\"\n\n[target.'cfg(windows)'.dependencies]\nwindows = \"1\"\n\n[dependencies]\nserde = \"1\"\n";
        let edited = depend(manifest, "p").unwrap();
        assert!(
            edited.ends_with("[dependencies]\nserde = \"1\"\nhanzo-version = { path = \"p\" }\n"),
            "{edited}"
        );
    }

    #[test]
    fn a_bare_clap_version_asks_at_run_time_and_a_given_one_is_left_alone() {
        let text = "#[derive(Parser)]\n#[command(version)]\nstruct A;\n#[command(\n    version,\n    override_usage = \"dev exec, version\"\n)]\nstruct B;\n#[command(author = \"x\", version, about = \"y\")]\nstruct C;\n#[command(version = \"1\")]\nstruct D;\n#[arg(long = \"version\")]\nx: bool,\n";
        let (edited, count) = clap_versions(text);
        assert_eq!(count, 3, "{edited}");
        assert!(
            edited.contains("#[command(version = hanzo_version::get())]\nstruct A"),
            "{edited}"
        );
        assert!(edited.contains("#[command(\n    version = hanzo_version::get(),\n    override_usage = \"dev exec, version\"\n)]"), "{edited}");
        assert!(
            edited.contains(
                "#[command(author = \"x\", version = hanzo_version::get(), about = \"y\")]"
            ),
            "{edited}"
        );
        assert!(edited.contains("#[command(version = \"1\")]"), "{edited}");
        assert!(edited.contains("#[arg(long = \"version\")]"), "{edited}");
        assert_eq!(
            clap_versions(&edited),
            (edited.clone(), 0),
            "a second run changes nothing"
        );
    }

    #[test]
    fn the_path_climbs_from_the_package_to_the_root() {
        let root = Path::new("/r");
        assert_eq!(
            dependency_path(root, Path::new("/r/upstream/codex/codex-rs/tui")),
            "../../../../crates/hanzo-version"
        );
        assert_eq!(
            dependency_path(root, Path::new("/r/upstream/codex/codex-rs/ext/goal")),
            "../../../../../crates/hanzo-version"
        );
    }

    /// The whole rule on a scratch checkout: reads become calls in every Rust file
    /// but a build script, each package that now names the call depends on the
    /// crate once, and a second run changes nothing.
    #[test]
    fn a_checkout_reads_its_version_at_run_time() {
        let root = tempdir();
        let source = root.join("upstream/codex/codex-rs");
        let package = source.join("core");
        std::fs::create_dir_all(package.join("src/nested")).unwrap();
        std::fs::write(
            source.join("Cargo.toml"),
            "[workspace]\nmembers = [\"core\"]\n",
        )
        .unwrap();
        std::fs::write(
            package.join("Cargo.toml"),
            "[package]\nname = \"core\"\n\n[dependencies]\nserde = \"1\"\n",
        )
        .unwrap();
        std::fs::write(
            package.join("build.rs"),
            "fn main() { let _ = env!(\"CARGO_PKG_VERSION\"); }\n",
        )
        .unwrap();
        std::fs::write(
            package.join("src/nested/ua.rs"),
            "pub fn ua() -> String { format!(\"x/{}\", env!(\"CARGO_PKG_VERSION\")) }\npub fn v() -> &'static str { env!(\"CARGO_PKG_VERSION\") }\n#[derive(clap::Parser)]\n#[command(version)]\npub struct Cli;\n",
        )
        .unwrap();

        assert_eq!(run(&root, &source).unwrap(), 3);
        let ua = std::fs::read_to_string(package.join("src/nested/ua.rs")).unwrap();
        assert_eq!(ua.matches("hanzo_version::get()").count(), 3);
        assert!(
            ua.contains("#[command(version = hanzo_version::get())]"),
            "{ua}"
        );
        assert!(!ua.contains("CARGO_PKG_VERSION"));
        let build = std::fs::read_to_string(package.join("build.rs")).unwrap();
        assert!(
            build.contains("env!(\"CARGO_PKG_VERSION\")"),
            "a build script keeps its own"
        );
        let manifest = std::fs::read_to_string(package.join("Cargo.toml")).unwrap();
        assert!(
            manifest.ends_with(
                "serde = \"1\"\nhanzo-version = { path = \"../../../../crates/hanzo-version\" }\n"
            ),
            "{manifest}"
        );
        let workspace = std::fs::read_to_string(source.join("Cargo.toml")).unwrap();
        assert!(
            !workspace.contains(CRATE),
            "the workspace manifest is not a package"
        );

        assert_eq!(run(&root, &source).unwrap(), 0);
        assert_eq!(
            std::fs::read_to_string(package.join("Cargo.toml")).unwrap(),
            manifest
        );
        std::fs::remove_dir_all(&root).unwrap();
    }

    fn tempdir() -> PathBuf {
        let directory =
            std::env::temp_dir().join(format!("hanzo-version-rule-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&directory);
        std::fs::create_dir_all(&directory).unwrap();
        directory
    }
}
