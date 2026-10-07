//! The Hanzo sign-in as the interface shows it. The credential lives in the
//! Hanzo CLI, so each call here asks that CLI and hands back what it printed,
//! one line at a time.

use std::io::BufRead;
use std::io::BufReader;
use std::io::ErrorKind;
use std::process::Command;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::mpsc;
use std::time::Duration;

const PROGRAM: &str = "hanzo";
const MISSING: &str =
    "The Hanzo CLI is not installed. Install it with `curl -fsSL https://hanzo.sh | sh`.";
const PATIENCE: Duration = Duration::from_secs(15);

/// Who is signed in, as `hanzo auth show` reports it.
pub fn show(mut emit: impl FnMut(String)) {
    let text = capture(PROGRAM, &["auth", "show"]);
    if text.to_ascii_lowercase().contains("not signed in") {
        emit("Not signed in. Use /login to sign in.".to_string());
        return;
    }
    each_line(&text, &mut emit);
}

/// Sign out of the active Hanzo identity.
pub fn logout(mut emit: impl FnMut(String)) {
    each_line(&capture(PROGRAM, &["auth", "logout"]), &mut emit);
}

/// Sign in through Hanzo IAM without leaving the interface. The sign-in runs
/// beside it, and every line it prints, such as the address to open, reaches
/// `emit` as it arrives.
pub fn login(emit: impl Fn(String) + Send + Sync + 'static) {
    std::thread::spawn(move || stream(PROGRAM, &["auth", "login", "--provider", "hanzo"], emit));
}

fn each_line(text: &str, emit: &mut impl FnMut(String)) {
    text.lines()
        .map(str::trim_end)
        .filter(|line| !line.trim().is_empty())
        .for_each(|line| emit(line.to_string()));
}

/// What `program args` printed, or why it could not run.
fn capture(program: &str, args: &[&str]) -> String {
    let mut command = Command::new(program);
    command.args(args).stdin(Stdio::null());
    let (sender, receiver) = mpsc::channel();
    std::thread::spawn(move || {
        let _ = sender.send(command.output());
    });
    match receiver.recv_timeout(PATIENCE) {
        Ok(Ok(output)) => {
            let mut text = String::from_utf8_lossy(&output.stdout).into_owned();
            text.push('\n');
            text.push_str(&String::from_utf8_lossy(&output.stderr));
            text
        }
        Ok(Err(error)) if error.kind() == ErrorKind::NotFound => MISSING.to_string(),
        Ok(Err(error)) => format!("Could not run the Hanzo CLI: {error}"),
        Err(_) => "The Hanzo CLI did not answer.".to_string(),
    }
}

/// Run `program args` to the end, passing along each line of output as it is
/// printed, then say how it ended.
fn stream(program: &str, args: &[&str], emit: impl Fn(String) + Send + Sync + 'static) {
    let emit = Arc::new(emit);
    let mut child = match Command::new(program)
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(child) => child,
        Err(error) if error.kind() == ErrorKind::NotFound => return emit(MISSING.to_string()),
        Err(error) => return emit(format!("Could not run the Hanzo CLI: {error}")),
    };
    let readers: Vec<_> = [
        child.stdout.take().map(|pipe| Box::new(pipe) as Box<dyn std::io::Read + Send>),
        child.stderr.take().map(|pipe| Box::new(pipe) as Box<dyn std::io::Read + Send>),
    ]
    .into_iter()
    .flatten()
    .map(|pipe| {
        let emit = Arc::clone(&emit);
        std::thread::spawn(move || {
            for line in BufReader::new(pipe).lines().map_while(Result::ok) {
                if !line.trim().is_empty() {
                    emit(line);
                }
            }
        })
    })
    .collect();
    let status = child.wait();
    for reader in readers {
        let _ = reader.join();
    }
    match status {
        Ok(status) if status.success() => emit("Signed in to Hanzo.".to_string()),
        Ok(status) => emit(format!("Sign-in did not complete ({status}).")),
        Err(error) => emit(format!("Sign-in did not complete: {error}")),
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;
    use std::path::PathBuf;
    use std::sync::Mutex;

    /// A stand-in for the Hanzo CLI that prints a fixed script and exits.
    fn cli(name: &str, body: &str) -> PathBuf {
        let path = std::env::temp_dir().join(format!("hanzo-tui-{}-{name}", std::process::id()));
        std::fs::write(&path, format!("#!/bin/sh\n{body}\n")).expect("write the stand-in");
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755))
            .expect("make it executable");
        path
    }

    #[test]
    fn a_missing_cli_says_how_to_install_it() {
        assert_eq!(capture("hanzo-tui-no-such-program", &[]), MISSING);
    }

    #[test]
    fn lines_arrive_in_order_and_the_end_is_reported() {
        let program = cli("login", "echo 'Open https://hanzo.id/device'; echo 'Code: ABCD' >&2");
        let seen = Arc::new(Mutex::new(Vec::new()));
        let sink = Arc::clone(&seen);
        stream(program.to_str().expect("utf-8 path"), &[], move |line| {
            sink.lock().expect("lock").push(line);
        });
        let mut seen = seen.lock().expect("lock").clone();
        assert_eq!(seen.pop().as_deref(), Some("Signed in to Hanzo."));
        seen.sort();
        assert_eq!(seen, ["Code: ABCD", "Open https://hanzo.id/device"]);
        std::fs::remove_file(program).expect("clean up");
    }

    #[test]
    fn a_failed_sign_in_is_not_reported_as_one() {
        let program = cli("fail", "echo 'denied'; exit 3");
        let seen = Arc::new(Mutex::new(Vec::new()));
        let sink = Arc::clone(&seen);
        stream(program.to_str().expect("utf-8 path"), &[], move |line| {
            sink.lock().expect("lock").push(line);
        });
        let seen = seen.lock().expect("lock").clone();
        assert_eq!(seen.first().map(String::as_str), Some("denied"));
        assert!(seen.last().is_some_and(|line| line.starts_with("Sign-in did not complete")));
        std::fs::remove_file(program).expect("clean up");
    }
}
