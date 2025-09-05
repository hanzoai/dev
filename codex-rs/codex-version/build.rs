use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    // Prefer an explicit CODE_VERSION provided by CI; fall back to the
    // crate's package version to keep local builds sane.
    let version = env::var("CODE_VERSION").unwrap_or_else(|_| env!("CARGO_PKG_VERSION").to_string());

    // Emit a small Rust source file that bakes the version into the crate's
    // compiled inputs so that cache systems like sccache invalidate when the
    // version changes.
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR is set by Cargo"));
    let dst = out_dir.join("code_version.rs");

    // Use inlined formatting to avoid warnings per repo policy.
    let contents = format!("pub const CODE_VERSION: &str = \"{}\";\n", version);
    fs::write(&dst, contents).expect("write code_version.rs");

    // Ensure dependent crates rebuild when CODE_VERSION changes even if the
    // source graph stays the same.
    println!("cargo:rerun-if-env-changed=CODE_VERSION");
}

