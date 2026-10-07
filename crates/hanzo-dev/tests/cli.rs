//! What the command surface promises, checked against the built binary.
//!
//! These run `dev` rather than calling into it, because the defects worth
//! catching here are the ones that only appear once clap assembles the whole
//! parser — an argument the front door and a subcommand both define, say.

use std::path::Path;
use std::process::Command;
use std::process::Output;

/// Every subcommand `dev` offers. Building each one's parser is the point:
/// clap only rejects a duplicate argument when it assembles that subcommand.
const COMMANDS: &[&str] = &[
    "exec", "login", "logout", "serve", "resume", "fork", "review", "agents", "queue", "archive",
    "unarchive", "delete", "cloud", "mcp", "plugin", "doctor", "features", "flow", "sandbox",
    "completion",
];

fn dev(home: &Path, arguments: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_dev"))
        .args(arguments)
        .env("DEV_HOME", home)
        .output()
        .expect("dev is built before its tests run")
}

fn text(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn home() -> tempfile::TempDir {
    tempfile::tempdir().expect("a writable temporary directory")
}

#[test]
fn every_subcommand_assembles_a_parser() {
    let home = home();
    for command in COMMANDS {
        let output = dev(home.path(), &[command, "--help"]);
        assert!(
            output.status.success(),
            "dev {command} --help: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn flow_offers_fix_and_fix_needs_a_test_command() {
    let home = home();
    assert!(text(&dev(home.path(), &["flow", "--help"])).contains("fix"));
    let help = text(&dev(home.path(), &["flow", "fix", "--help"]));
    assert!(help.contains("--test") && help.contains("--control"), "{help}");
    assert!(!dev(home.path(), &["flow", "fix"]).status.success());
}

#[test]
fn completion_scripts_name_the_product() {
    let home = home();
    assert!(text(&dev(home.path(), &["completion", "bash"])).contains("_dev()"));
    assert!(text(&dev(home.path(), &["completion", "zsh"])).contains("compdef dev"));
}

/// Every surface a user reads, not just the front page. The banner `dev exec`
/// prints was upstream's for a while precisely because only `--help` was
/// checked, and `--help` never shows it.
#[test]
fn no_surface_carries_an_upstream_product_name() {
    let home = home();
    let mut surfaces = vec![("--help".to_string(), text(&dev(home.path(), &["--help"])))];
    for command in COMMANDS {
        surfaces.push((
            format!("{command} --help"),
            text(&dev(home.path(), &[command, "--help"])),
        ));
    }
    for (surface, body) in surfaces {
        for name in ["OpenAI", "Codex", "codex"] {
            // Login names the provider whose credential the user is supplying.
            if surface == "login --help" && name == "OpenAI" {
                continue;
            }
            // Two things legitimately carry the word and must not be renamed:
            // `CODEX_HOME`, an environment variable upstream owns, and
            // `codex/…`, which names a method on the app-server wire protocol.
            // Renaming either would break something real; branding is about the
            // product name a reader sees, not identifiers on a wire.
            let leaked: Vec<_> = body
                .lines()
                .filter(|line| {
                    line.contains(name)
                        && !line.contains("CODEX_HOME")
                        && !line.contains("`codex/")
                })
                .collect();
            assert!(leaked.is_empty(), "`dev {surface}` says {name}: {leaked:?}");
        }
    }
}

/// The product answers for one version, everywhere it states one.
#[test]
fn the_product_states_one_version() {
    let home = home();
    let version = text(&dev(home.path(), &["--version"]));
    let number = version.split_whitespace().nth(1).expect("`dev <version>`").to_string();
    assert!(version.starts_with("dev "), "{version}");
    let doctor = text(&dev(home.path(), &["doctor", "--summary", "--no-color"]));
    assert!(doctor.contains(&number), "doctor reports a different version: {doctor:.120}");
}

/// The first feature upstream still offers as a switch. Names come and go with
/// each upstream release, so the round trip asks for one instead of naming it.
fn experimental_feature(listed: &str) -> String {
    listed
        .lines()
        .find_map(|line| {
            let columns: Vec<_> = line.split("  ").map(str::trim).filter(|c| !c.is_empty()).collect();
            (columns.get(1) == Some(&"experimental")).then(|| columns[0].to_string())
        })
        .expect("an experimental feature is listed")
}

#[test]
fn features_round_trip_through_the_product_config() {
    let home = home();
    let feature = experimental_feature(&text(&dev(home.path(), &["features", "list"])));
    assert!(dev(home.path(), &["features", "enable", &feature]).status.success());
    let listed = text(&dev(home.path(), &["features", "list"]));
    let row = listed
        .lines()
        .find(|line| line.split_whitespace().next() == Some(feature.as_str()))
        .expect("the feature is listed");
    assert!(row.ends_with("on"), "{row}");

    assert!(dev(home.path(), &["features", "disable", &feature]).status.success());
    let listed = text(&dev(home.path(), &["features", "list"]));
    let row = listed
        .lines()
        .find(|line| line.split_whitespace().next() == Some(feature.as_str()))
        .unwrap();
    assert!(row.ends_with("off"), "{row}");
}

#[test]
fn an_unknown_feature_is_refused() {
    let home = home();
    let output = dev(home.path(), &["features", "enable", "not_a_feature"]);
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("no such feature"));
}

#[test]
fn doctor_reports_as_json() {
    let home = home();
    let report = text(&dev(home.path(), &["doctor", "--json"]));
    assert!(
        serde_json::from_str::<serde_json::Value>(&report).is_ok(),
        "doctor did not emit JSON: {report:.200}"
    );
}
