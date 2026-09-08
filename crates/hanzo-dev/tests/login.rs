//! Sign-in routing, credential handling, and the actual terminal picker.
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::process::Output;
use std::process::Stdio;
use std::time::Duration;

fn login(home: &Path, args: &[&str], input: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_dev"))
        .arg("login")
        .args(args)
        .env("DEV_HOME", home)
        .env_remove("HANZO_USER_KEY")
        .env_remove("OPENAI_API_KEY")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(input.as_bytes())
        .unwrap();
    child.wait_with_output().unwrap()
}

fn config(home: &Path) -> toml::Value {
    toml::from_str(&std::fs::read_to_string(home.join("config.toml")).unwrap()).unwrap()
}

#[test]
fn noninteractive_login_requires_an_explicit_method() {
    let home = tempfile::tempdir().unwrap();
    let output = login(home.path(), &[], "");
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("Choose a sign-in method"));
    assert_eq!(
        config(home.path())["model_provider"].as_str(),
        Some("hanzo")
    );
    assert!(!home.path().join("auth.json").exists());
}

#[test]
fn pasted_key_defaults_to_hanzo_and_is_available_to_the_next_process() {
    let home = tempfile::tempdir().unwrap();
    let secret = "sk-live-test-credential";
    let output = login(home.path(), &["--with-api-key"], &format!("{secret}\n"));
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(!String::from_utf8_lossy(&output.stdout).contains(secret));
    assert!(!String::from_utf8_lossy(&output.stderr).contains(secret));
    assert_eq!(
        config(home.path()),
        toml::from_str::<toml::Value>(hanzo_config::DEFAULT_CONFIG).unwrap()
    );
    assert_eq!(
        std::fs::read_to_string(home.path().join("hanzo-api-key")).unwrap(),
        secret
    );
    assert!(!home.path().join("auth.json").exists());
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        assert_eq!(
            std::fs::metadata(home.path().join("hanzo-api-key"))
                .unwrap()
                .permissions()
                .mode()
                & 0o777,
            0o600
        );
    }
    let status = login(home.path(), &["status"], "");
    assert!(status.status.success());
    assert_eq!(
        String::from_utf8(status.stdout).unwrap().trim(),
        "Signed in to Hanzo."
    );
}

#[test]
fn openai_key_uses_openai_storage_and_model_catalog() {
    let home = tempfile::tempdir().unwrap();
    let output = login(
        home.path(),
        &[
            "--chatgpt",
            "--with-api-key",
            "-c",
            "cli_auth_credentials_store=\"file\"",
        ],
        "sk-proj-example-key\n",
    );
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let mut expected: toml::Value = toml::from_str(hanzo_config::DEFAULT_CONFIG).unwrap();
    expected["model_provider"] = "openai".into();
    expected.as_table_mut().unwrap().remove("model");
    assert_eq!(config(home.path()), expected);
    let auth: serde_json::Value =
        serde_json::from_slice(&std::fs::read(home.path().join("auth.json")).unwrap()).unwrap();
    assert_eq!(auth["OPENAI_API_KEY"], "sk-proj-example-key");
    assert!(!home.path().join("hanzo-api-key").exists());
    assert!(!String::from_utf8_lossy(&output.stdout).contains("sk-proj-example-key"));
}

#[test]
fn refused_keys_and_conflicting_flags_leave_credentials_untouched() {
    for (args, key) in [
        (vec!["--with-api-key"], ""),
        (vec!["--with-api-key"], "sk-proj-wrong-provider"),
        (
            vec!["--chatgpt", "--with-api-key"],
            "sk-live-wrong-provider",
        ),
        (vec!["--hanzo", "--chatgpt"], ""),
    ] {
        let home = tempfile::tempdir().unwrap();
        let output = login(home.path(), &args, key);
        assert!(!output.status.success());
        assert!(!home.path().join("hanzo-api-key").exists());
        assert!(!home.path().join("auth.json").exists());
        assert_eq!(
            config(home.path())["model_provider"].as_str(),
            Some("hanzo")
        );
    }
}

#[test]
fn disabled_openai_login_keeps_the_hanzo_provider_and_key() {
    let home = tempfile::tempdir().unwrap();
    assert!(
        login(home.path(), &["--with-api-key"], "hk-existing-key")
            .status
            .success()
    );
    let before = config(home.path());
    let output = login(
        home.path(),
        &[
            "--chatgpt",
            "--with-api-key",
            "-c",
            "forced_login_method=\"chatgpt\"",
        ],
        "sk-proj-new-key",
    );
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("disabled by your configuration"));
    assert_eq!(config(home.path()), before);
    assert_eq!(
        std::fs::read_to_string(home.path().join("hanzo-api-key")).unwrap(),
        "hk-existing-key"
    );
    assert!(!home.path().join("auth.json").exists());
}

#[test]
fn logout_removes_only_the_selected_providers_saved_key() {
    let home = tempfile::tempdir().unwrap();
    assert!(
        login(
            home.path(),
            &["--with-api-key"],
            "hk-keep-until-hanzo-logout"
        )
        .status
        .success()
    );
    assert!(
        login(
            home.path(),
            &[
                "--chatgpt",
                "--with-api-key",
                "-c",
                "cli_auth_credentials_store=\"file\""
            ],
            "sk-proj-remove-me"
        )
        .status
        .success()
    );
    let output = Command::new(env!("CARGO_BIN_EXE_dev"))
        .args(["logout", "-c", "cli_auth_credentials_store=\"file\""])
        .env("DEV_HOME", home.path())
        .env_remove("HANZO_USER_KEY")
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(home.path().join("hanzo-api-key").exists());
    assert!(!home.path().join("auth.json").exists());
    hanzo_config::activate_provider(home.path(), hanzo_config::Provider::Hanzo).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_dev"))
        .arg("logout")
        .env("DEV_HOME", home.path())
        .env_remove("HANZO_USER_KEY")
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(!home.path().join("hanzo-api-key").exists());
}

#[cfg(unix)]
#[test]
fn hanzo_browser_login_delegates_and_only_changes_provider_on_success() {
    use std::os::unix::fs::PermissionsExt;
    for code in [0, 7] {
        let home = tempfile::tempdir().unwrap();
        std::fs::write(
            home.path().join("config.toml"),
            "model_provider = 'openai'\n# keep this\n",
        )
        .unwrap();
        let binary = home.path().join("hanzo");
        std::fs::write(
            &binary,
            format!("#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$DEV_HOME/invocation\"\nexit {code}\n"),
        )
        .unwrap();
        std::fs::set_permissions(binary, std::fs::Permissions::from_mode(0o755)).unwrap();
        let output = Command::new(env!("CARGO_BIN_EXE_dev"))
            .args(["login", "--hanzo"])
            .env("DEV_HOME", home.path())
            .env("PATH", home.path())
            .env_remove("HANZO_USER_KEY")
            .output()
            .unwrap();
        assert_eq!(output.status.success(), code == 0);
        assert_eq!(
            std::fs::read_to_string(home.path().join("invocation")).unwrap(),
            "auth\nlogin\n--provider\nhanzo\n"
        );
        let expected = if code == 0 { "hanzo" } else { "openai" };
        assert_eq!(
            config(home.path())["model_provider"].as_str(),
            Some(expected)
        );
        if code == 0 {
            let expected: toml::Value = toml::from_str(hanzo_config::DEFAULT_CONFIG).unwrap();
            assert_eq!(
                config(home.path())["model_providers"]["hanzo"],
                expected["model_providers"]["hanzo"]
            );
        }
        assert!(
            std::fs::read_to_string(home.path().join("config.toml"))
                .unwrap()
                .contains("# keep this")
        );
    }
}

async fn read_until(
    process: &mut codex_utils_pty::SpawnedProcess,
    parser: &mut vt100::Parser,
    needle: &str,
) -> Vec<u8> {
    tokio::time::timeout(Duration::from_secs(20), async {
        let mut raw = Vec::new();
        while let Some(bytes) = process.stdout_rx.recv().await {
            parser.process(&bytes);
            raw.extend(bytes);
            if parser.screen().contents().contains(needle) {
                return raw;
            }
        }
        panic!(
            "terminal closed before {needle:?}: {}",
            parser.screen().contents()
        );
    })
    .await
    .expect("terminal response")
}

#[tokio::test]
async fn picker_defaults_to_hanzo_and_hides_a_pasted_key() {
    let home = tempfile::tempdir().unwrap();
    let mut env: std::collections::HashMap<String, String> = std::env::vars().collect();
    env.insert(
        "DEV_HOME".into(),
        home.path().to_string_lossy().into_owned(),
    );
    env.insert("TERM".into(), "xterm-256color".into());
    env.insert("NO_COLOR".into(), "1".into());
    env.remove("HANZO_USER_KEY");
    let mut process = codex_utils_pty::spawn_pty_process(
        env!("CARGO_BIN_EXE_dev"),
        &["login".into()],
        home.path(),
        &env,
        &None,
        codex_utils_pty::TerminalSize { rows: 14, cols: 88 },
        &[],
    )
    .await
    .unwrap();
    let mut parser = vt100::Parser::new(14, 88, 0);
    read_until(&mut process, &mut parser, "Paste an OpenAI API key").await;
    insta::assert_snapshot!("login_picker", parser.screen().contents());
    process
        .session
        .writer_sender()
        .send(b"\x1b[B\x1b[B\r".to_vec())
        .await
        .unwrap();
    read_until(&mut process, &mut parser, "API key (hidden)").await;
    let secret = "sk-live-hidden-test-credential";
    process
        .session
        .writer_sender()
        .send(format!("{secret}\r").into_bytes())
        .await
        .unwrap();
    let raw = read_until(&mut process, &mut parser, "Hanzo API key saved.").await;
    assert!(!String::from_utf8_lossy(&raw).contains(secret));
    assert_eq!(
        tokio::time::timeout(Duration::from_secs(10), &mut process.exit_rx)
            .await
            .unwrap()
            .unwrap(),
        0
    );
    assert_eq!(
        std::fs::read_to_string(home.path().join("hanzo-api-key")).unwrap(),
        secret
    );
}
