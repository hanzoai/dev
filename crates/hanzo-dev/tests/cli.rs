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
    "unarchive", "delete", "cloud", "mcp", "plugin", "doctor", "features", "sandbox", "completion",
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
fn completion_scripts_name_the_product() {
    let home = home();
    assert!(text(&dev(home.path(), &["completion", "bash"])).contains("_dev()"));
    assert!(text(&dev(home.path(), &["completion", "zsh"])).contains("compdef dev"));
}

#[test]
fn the_interface_carries_no_upstream_product_name() {
    let home = home();
    let help = text(&dev(home.path(), &["--help"]));
    for name in ["OpenAI", "Codex", "ChatGPT Codex"] {
        // `CODEX_HOME` still reaches the profile flag from a crate the island
        // cannot reach. Everything else is ours to answer for.
        let leaked: Vec<_> = help
            .lines()
            .filter(|line| line.contains(name) && !line.contains("CODEX_HOME"))
            .collect();
        assert!(leaked.is_empty(), "{name} reaches the interface: {leaked:?}");
    }
}

#[test]
fn features_round_trip_through_the_product_config() {
    let home = home();
    assert!(dev(home.path(), &["features", "enable", "transcript_v2"]).status.success());
    let listed = text(&dev(home.path(), &["features", "list"]));
    let row = listed
        .lines()
        .find(|line| line.starts_with("transcript_v2"))
        .expect("the feature is listed");
    assert!(row.ends_with("on"), "{row}");

    assert!(dev(home.path(), &["features", "disable", "transcript_v2"]).status.success());
    let listed = text(&dev(home.path(), &["features", "list"]));
    let row = listed.lines().find(|line| line.starts_with("transcript_v2")).unwrap();
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
