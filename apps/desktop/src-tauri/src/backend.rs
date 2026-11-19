use crate::auth::{current_desktop_bootstrap_token, now_unix_ms};
use serde::Serialize;
use std::env;
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::io::AsyncWriteExt;
use tokio::process::Command;

#[derive(Clone)]
pub struct BackendRuntimeConfig {
    pub port: u16,
    pub bind_host: String,
    pub http_base_url: String,
    pub ws_base_url: String,
    pub hanzo_home: PathBuf,
    pub state_dir: PathBuf,
    pub bootstrap_secret: String,
    pub host_scope: String,
    pub is_dev: bool,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct DesktopBackendBootstrapPayload {
    mode: &'static str,
    no_browser: bool,
    port: u16,
    hanzo_home: String,
    host: String,
    desktop_bootstrap_token: String,
    desktop_bootstrap_secret: String,
    shell_environment_prepared: bool,
    tailscale_serve_enabled: bool,
    tailscale_serve_port: u16,
    desktop_app_update: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    resource_monitor_path: Option<String>,
}

pub struct BackendSupervisor {
    pub config: BackendRuntimeConfig,
    shutting_down: Arc<AtomicBool>,
    child_pid: Arc<AtomicU32>,
}

pub fn pick_available_port(preferred_start: u16) -> u16 {
    for candidate in preferred_start..=preferred_start.saturating_add(200) {
        if TcpListener::bind(("127.0.0.1", candidate)).is_ok() {
            return candidate;
        }
    }
    TcpListener::bind(("127.0.0.1", 0))
        .and_then(|sock| sock.local_addr())
        .map(|addr| addr.port())
        .unwrap_or(preferred_start)
}

pub fn resolve_hanzo_home() -> PathBuf {
    if let Ok(explicit) = env::var("HANZO_DEV_HOME") {
        let trimmed = explicit.trim();
        if !trimmed.is_empty() {
            return PathBuf::from(trimmed);
        }
    }
    dirs::home_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".hanzo-dev")
}

fn augmented_path() -> String {
    let home = dirs::home_dir().unwrap_or_else(|| PathBuf::from("/Users/Shared"));
    let mut entries: Vec<String> = vec![
        home.join(".local/bin").to_string_lossy().into_owned(),
        home.join(".hanzo/bin").to_string_lossy().into_owned(),
        home.join(".cargo/bin").to_string_lossy().into_owned(),
        "/opt/homebrew/bin".to_string(),
        "/opt/homebrew/sbin".to_string(),
        "/usr/local/bin".to_string(),
        "/usr/bin".to_string(),
        "/bin".to_string(),
        "/usr/sbin".to_string(),
        "/sbin".to_string(),
    ];
    if let Ok(existing) = env::var("PATH") {
        for part in existing.split(':') {
            let trimmed = part.trim();
            if !trimmed.is_empty() && !entries.iter().any(|e| e == trimmed) {
                entries.push(trimmed.to_string());
            }
        }
    }
    entries.join(":")
}

fn resolve_node_executable() -> PathBuf {
    if let Ok(explicit) = env::var("HANZO_NODE_BINARY") {
        let p = PathBuf::from(explicit.trim());
        if p.is_file() {
            return p;
        }
    }
    let candidates = [
        "/opt/homebrew/bin/node",
        "/usr/local/bin/node",
        "/usr/bin/node",
    ];
    for candidate in candidates {
        let p = PathBuf::from(candidate);
        if p.is_file() {
            return p;
        }
    }
    if let Some(home) = dirs::home_dir() {
        for rel in [".nvm/current/bin/node", ".volta/bin/node", ".local/share/mise/shims/node"] {
            let p = home.join(rel);
            if p.is_file() {
                return p;
            }
        }
    }
    PathBuf::from("node")
}

fn resolve_repo_root() -> Option<PathBuf> {
    if let Ok(explicit) = env::var("HANZO_DEV_REPO_ROOT") {
        let p = PathBuf::from(explicit.trim());
        if p.join("apps/server/dist/bin.mjs").is_file() {
            return Some(p);
        }
    }
    let compile_manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    if let Some(repo) = compile_manifest.parent().and_then(|p| p.parent()).and_then(|p| p.parent()) {
        if repo.join("apps/server/dist/bin.mjs").is_file() {
            return Some(repo.to_path_buf());
        }
    }
    if let Ok(exe) = env::current_exe() {
        let mut cursor = exe.parent();
        while let Some(dir) = cursor {
            if dir.join("apps/server/dist/bin.mjs").is_file() {
                return Some(dir.to_path_buf());
            }
            if dir.join("Resources/server/dist/bin.mjs").is_file() {
                return Some(dir.to_path_buf());
            }
            cursor = dir.parent();
        }
    }
    None
}

fn resolve_backend_entry_and_cwd() -> Option<(PathBuf, PathBuf, Option<PathBuf>)> {
    if let Ok(explicit) = env::var("HANZO_DEV_BACKEND_ENTRY") {
        let entry = PathBuf::from(explicit.trim());
        if entry.is_file() {
            let cwd = entry
                .parent()
                .and_then(Path::parent)
                .map(Path::to_path_buf)
                .unwrap_or_else(|| PathBuf::from("."));
            return Some((entry, cwd, None));
        }
    }
    let root = resolve_repo_root()?;
    let packaged_entry = root.join("Resources/server/dist/bin.mjs");
    if packaged_entry.is_file() {
        let monitor = root.join("Resources/hanzo-resource-monitor");
        return Some((
            packaged_entry,
            root.join("Resources/server"),
            monitor.is_file().then_some(monitor),
        ));
    }
    let repo_entry = root.join("apps/server/dist/bin.mjs");
    if repo_entry.is_file() {
        let monitor = root.join("native/resource-monitor/target/release/hanzo-resource-monitor");
        return Some((
            repo_entry,
            root.join("apps/server"),
            monitor.is_file().then_some(monitor),
        ));
    }
    None
}

impl BackendSupervisor {
    pub fn start(config: BackendRuntimeConfig, local_enabled: bool) -> Self {
        let shutting_down = Arc::new(AtomicBool::new(false));
        let child_pid = Arc::new(AtomicU32::new(0));

        if local_enabled {
            let cfg = config.clone();
            let stop_flag = Arc::clone(&shutting_down);
            let pid_slot = Arc::clone(&child_pid);

            tauri::async_runtime::spawn(async move {
                let mut backoff_ms = 500u64;
                while !stop_flag.load(Ordering::SeqCst) {
                    let Some((entry_path, backend_cwd, monitor_path)) =
                        resolve_backend_entry_and_cwd()
                    else {
                        eprintln!("[hanzo-tauri] Backend entry bin.mjs not found; retrying...");
                        tokio::time::sleep(Duration::from_secs(2)).await;
                        continue;
                    };

                    let node_bin = resolve_node_executable();
                    let payload = DesktopBackendBootstrapPayload {
                        mode: "desktop",
                        no_browser: true,
                        port: cfg.port,
                        hanzo_home: cfg.hanzo_home.to_string_lossy().into_owned(),
                        host: cfg.bind_host.clone(),
                        desktop_bootstrap_token: current_desktop_bootstrap_token(
                            &cfg.bootstrap_secret,
                            now_unix_ms(),
                        ),
                        desktop_bootstrap_secret: cfg.bootstrap_secret.clone(),
                        shell_environment_prepared: true,
                        tailscale_serve_enabled: false,
                        tailscale_serve_port: 443,
                        desktop_app_update: false,
                        resource_monitor_path: monitor_path
                            .map(|p| p.to_string_lossy().into_owned()),
                    };

                    let Ok(bootstrap_json) = serde_json::to_vec(&payload) else {
                        break;
                    };

                    let mut cmd = Command::new(&node_bin);
                    cmd.arg(&entry_path)
                        .arg("--bootstrap-fd")
                        .arg("0")
                        .current_dir(&backend_cwd)
                        .env("PATH", augmented_path())
                        .env("HANZO_DEV_DESKTOP_HOST_SCOPE", &cfg.host_scope)
                        .env_remove("ELECTRON_RUN_AS_NODE")
                        .env_remove("HANZO_DEV_PORT")
                        .env_remove("HANZO_DEV_MODE")
                        .env_remove("HANZO_DEV_NO_BROWSER")
                        .env_remove("HANZO_DEV_HOST")
                        .stdin(Stdio::piped())
                        .stdout(Stdio::inherit())
                        .stderr(Stdio::inherit());

                    match cmd.spawn() {
                        Ok(mut child) => {
                            if let Some(pid) = child.id() {
                                pid_slot.store(pid, Ordering::SeqCst);
                            }
                            if let Some(mut stdin) = child.stdin.take() {
                                let _ = stdin.write_all(&bootstrap_json).await;
                                let _ = stdin.shutdown().await;
                            }

                            let started_at = std::time::Instant::now();
                            let _ = child.wait().await;
                            pid_slot.store(0, Ordering::SeqCst);

                            if stop_flag.load(Ordering::SeqCst) {
                                break;
                            }
                            if started_at.elapsed() > Duration::from_secs(15) {
                                backoff_ms = 500;
                            }
                        }
                        Err(err) => {
                            eprintln!(
                                "[hanzo-tauri] Failed to spawn {} {}: {err}",
                                node_bin.display(),
                                entry_path.display()
                            );
                        }
                    }

                    tokio::time::sleep(Duration::from_millis(backoff_ms)).await;
                    backoff_ms = (backoff_ms * 2).min(10_000);
                }
            });
        }

        Self {
            config,
            shutting_down,
            child_pid,
        }
    }

    pub fn shutdown(&self) {
        self.shutting_down.store(true, Ordering::SeqCst);
        let pid = self.child_pid.swap(0, Ordering::SeqCst);
        if pid == 0 {
            return;
        }
        #[cfg(unix)]
        unsafe {
            libc::kill(pid as i32, libc::SIGTERM);
        }
        #[cfg(windows)]
        {
            let _ = std::process::Command::new("taskkill")
                .args(["/PID", &pid.to_string(), "/T", "/F"])
                .status();
        }
    }
}
