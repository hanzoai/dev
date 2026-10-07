//! Build the staticlib, link a C program against it, run it.
//!
//! `cargo test` builds a test harness, never the staticlib, so this test asks
//! cargo for the archive it is about to link — and asks rustc which system
//! libraries that archive needs, rather than keeping a per-platform list here.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

/// Where the staticlib and the C program are built: a target directory of
/// its own, so this build never waits on the lock the test run already holds.
fn shed() -> PathBuf {
    root().join("target/abi")
}

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("workspace root")
}

fn run(command: &mut Command) -> (String, String) {
    let what = format!("{command:?}");
    let output = command.output().unwrap_or_else(|e| panic!("{what}: {e}"));
    let out = String::from_utf8_lossy(&output.stdout).into_owned();
    let err = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(output.status.success(), "{what} failed\n{out}\n{err}");
    (out, err)
}

#[test]
fn a_c_program_drives_the_loop_through_the_staticlib() {
    let shed = shed();
    let (_, notes) = run(Command::new(env!("CARGO"))
        .current_dir(root())
        .args(["rustc", "-p", "dev-ffi", "--lib"])
        .arg("--target-dir")
        .arg(&shed)
        .args(["--", "--print", "native-static-libs"]));

    let archive = shed.join("debug/libdev.a");
    assert!(archive.is_file(), "no staticlib at {}", archive.display());

    let libs: Vec<String> = notes
        .lines()
        .find_map(|line| line.split_once("native-static-libs:"))
        .map(|(_, libs)| libs.split_whitespace().map(str::to_string).collect())
        .expect("rustc did not print the libraries the archive needs");

    let program = shed.join("abi");
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());
    run(Command::new(cc)
        .arg("-std=c11")
        .arg("-Wall")
        .arg("-Wextra")
        .arg("-Werror")
        .arg("-I")
        .arg(manifest.join("include"))
        .arg(manifest.join("tests/abi.c"))
        .arg(&archive)
        .args(&libs)
        .arg("-o")
        .arg(&program));

    let (out, err) = run(&mut Command::new(&program));
    assert_eq!(out.trim(), "ok", "{err}");
}
