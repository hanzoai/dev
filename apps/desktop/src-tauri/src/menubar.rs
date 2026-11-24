//! The menu-bar item is a native helper (`apps/desktop/menubar`) bundled with the app. It reads
//! `/v1/usage` from this app's own server; this module starts it, relays its actions, and
//! stops it with the app (the helper exits when its stdin closes).

use crate::commands::{local_bearer_token, AppState};
use std::path::PathBuf;
use std::process::Stdio;
use tauri::{AppHandle, Emitter, Manager};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;

const HELPER: &str = "HanzoMenubar.app/Contents/MacOS/HanzoMenubar";

fn helper_path(app: &AppHandle) -> Option<PathBuf> {
    let bundled = app
        .path()
        .resource_dir()
        .ok()?
        .join("resources/menubar")
        .join(HELPER);
    let staged = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("resources/menubar")
        .join(HELPER);
    [bundled, staged].into_iter().find(|p| p.is_file())
}

fn show_main(app: &AppHandle) {
    if let Some(window) = app.get_webview_window("main") {
        let _ = window.unminimize();
        let _ = window.show();
        let _ = window.set_focus();
    }
}

/// One line per user action: `show`, `quit`, or a menu action the web app handles.
fn relay(app: &AppHandle, action: &str) {
    match action {
        "quit" => app.exit(0),
        "show" => show_main(app),
        other => {
            show_main(app);
            let _ = app.emit("desktop:menu-action", other.to_string());
        }
    }
}

pub fn start(app: &AppHandle) {
    let Some(path) = helper_path(app) else { return };
    let handle = app.clone();
    tauri::async_runtime::spawn(async move {
        let state = handle.state::<AppState>();
        let url = state.supervisor.config.http_base_url.clone();
        // The backend takes a moment to come up; wait for its token before showing anything.
        let token = loop {
            match local_bearer_token(&state).await {
                Ok(t) => break t,
                Err(_) => tokio::time::sleep(std::time::Duration::from_secs(2)).await,
            }
        };
        let Ok(mut child) = Command::new(path)
            .env("HANZO_DEV_URL", &url)
            .env("HANZO_DEV_TOKEN", &token)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
        else {
            return;
        };
        let _stdin = child.stdin.take();
        if let Some(out) = child.stdout.take() {
            let mut lines = BufReader::new(out).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                relay(&handle, line.trim());
            }
        }
        let _ = child.wait().await;
    });
}
