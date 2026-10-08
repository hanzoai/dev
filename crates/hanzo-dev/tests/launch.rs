//! A bare `dev` starts, and it reads only Dev's own project configuration.
//!
//! Upstream's default is a shared background server installed from a package directory
//! that Dev does not ship, so a launch has to run its server inside the process or the
//! first command ends in an error. And a project keeps Dev's settings in `.hanzo/dev`, the
//! layout of `~/.hanzo/dev`: another product's `.codex` beside it is that product's, and
//! its model must not become ours.

#![cfg(unix)]

use portable_pty::CommandBuilder;
use portable_pty::PtySize;
use portable_pty::native_pty_system;
use std::io::Read;
use std::sync::mpsc;
use std::time::Duration;
use std::time::Instant;

const COMPOSER: &str = "Ask Hanzo Dev to do anything";
const PACKAGE_ERROR: &str = "no complete local package";

#[test]
fn a_bare_launch_starts_in_process_and_reads_only_the_projects_hanzo_config() {
    let home = tempfile::tempdir().expect("a scratch home");
    let work = tempfile::tempdir().expect("a scratch directory");
    let work_path = work.path().canonicalize().expect("a real path");
    std::fs::write(
        home.path().join("config.toml"),
        format!(
            "model_provider = \"local\"\nmodel = \"local-model\"\n\n\
             [model_providers.local]\nname = \"Local\"\nbase_url = \"http://127.0.0.1:9/v1\"\nwire_api = \"responses\"\n\n\
             [projects.\"{}\"]\ntrust_level = \"trusted\"\n",
            work_path.display()
        ),
    )
    .expect("a config");
    std::fs::create_dir_all(work_path.join(".hanzo/dev")).expect("the project's directory");
    std::fs::write(work_path.join(".hanzo/dev/config.toml"), "model = \"project-model\"\n")
        .expect("the project's config");
    std::fs::create_dir(work_path.join(".codex")).expect("another product's directory");
    std::fs::write(work_path.join(".codex/config.toml"), "model = \"gpt-6-astra\"\n")
        .expect("another product's config");

    let pair = native_pty_system()
        .openpty(PtySize { rows: 36, cols: 110, pixel_width: 0, pixel_height: 0 })
        .expect("a terminal");
    let mut command = CommandBuilder::new(env!("CARGO_BIN_EXE_dev"));
    command.cwd(&work_path);
    command.env("TERM", "xterm-256color");
    command.env("DEV_HOME", home.path());
    command.env("HOME", home.path());
    command.env_remove("CODEX_HOME");
    let mut child = pair.slave.spawn_command(command).expect("dev starts");
    drop(pair.slave);

    let mut reader = pair.master.try_clone_reader().expect("a reader");
    let (sender, received) = mpsc::channel::<Vec<u8>>();
    std::thread::spawn(move || {
        let mut buffer = [0u8; 8192];
        while let Ok(count) = reader.read(&mut buffer) {
            if count == 0 || sender.send(buffer[..count].to_vec()).is_err() {
                break;
            }
        }
    });

    let mut screen = vt100::Parser::new(36, 110, 0);
    let deadline = Instant::now() + Duration::from_secs(60);
    let mut shown = String::new();
    while Instant::now() < deadline {
        if let Ok(bytes) = received.recv_timeout(Duration::from_millis(200)) {
            screen.process(&bytes);
            shown = screen.screen().contents();
            if shown.contains(PACKAGE_ERROR) || (shown.contains(COMPOSER) && shown.contains("project-model")) {
                break;
            }
        }
    }
    let _ = child.kill();

    assert!(!shown.contains(PACKAGE_ERROR), "a bare launch asked for a package:\n{shown}");
    assert!(shown.contains(COMPOSER), "a bare launch never reached the composer:\n{shown}");
    assert!(shown.contains("project-model"), "the project's own config was not read:\n{shown}");
    assert!(
        !shown.to_ascii_lowercase().contains("gpt-6"),
        "another product's project config set the model:\n{shown}"
    );
}
