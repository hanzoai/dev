use std::env;
use std::process::Command;

fn bin_path() -> String {
    // Cargo sets this for integration tests so they can execute the package's bins.
    env::var("CARGO_BIN_EXE_code").expect("CARGO_BIN_EXE_code env var not set by cargo")
}

#[test]
fn version_prints() {
    let out = Command::new(bin_path())
        .arg("--version")
        .output()
        .expect("failed to run code --version");
    assert!(out.status.success());
    assert!(!out.stdout.is_empty());
}

#[test]
fn completion_bash_emits() {
    let out = Command::new(bin_path())
        .args(["completion", "--shell", "bash"])
        .output()
        .expect("failed to run code completion");
    assert!(out.status.success());
    assert!(!out.stdout.is_empty());
}

#[test]
fn doctor_runs() {
    let status = Command::new(bin_path())
        .arg("doctor")
        .status()
        .expect("failed to run code doctor");
    assert!(status.success());
}

