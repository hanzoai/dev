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

#[derive(Debug, Clone)]
pub enum ResolvedBackendTarget {
    NodeScript {
        node_bin: PathBuf,
        entry_mjs: PathBuf,
        cwd: PathBuf,
        resource_monitor: Option<PathBuf>,
    },
    StandaloneBinary {
        executable: PathBuf,
        cwd: PathBuf,
        resource_monitor: Option<PathBuf>,
    },
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

pub fn augmented_path() -> String {
    let home = dirs::home_dir().unwrap_or_else(|| PathBuf::from("/Users/Shared"));
    let mut entries: Vec<String> = vec![
        home.join(".local/bin").to_string_lossy().into_owned(),
        home.join(".hanzo/bin").to_string_lossy().into_owned(),
        home.join(".opencode/bin").to_string_lossy().into_owned(),
        home.join(".antigravity/antigravity/bin")
            .to_string_lossy()
            .into_owned(),
        home.join(".cargo/bin").to_string_lossy().into_owned(),
        "/opt/homebrew/bin".to_string(),
        "/opt/homebrew/sbin".to_string(),
        "/usr/local/bin".to_string(),
        "/usr/bin".to_string(),
        "/bin".to_string(),
        "/usr/sbin".to_string(),
        "/sbin".to_string(),
    ];
    let sep = if cfg!(windows) { ';' } else { ':' };
    if let Ok(existing) = env::var("PATH") {
        for part in existing.split(sep) {
            let trimmed = part.trim();
            if !trimmed.is_empty() && !entries.iter().any(|e| e == trimmed) {
                entries.push(trimmed.to_string());
            }
        }
    }
    entries.join(&sep.to_string())
}

pub fn resolve_node_executable() -> PathBuf {
    if let Ok(explicit) = env::var("HANZO_NODE_BINARY") {
        let p = PathBuf::from(explicit.trim());
        if p.is_file() {
            return p;
        }
    }
    #[cfg(windows)]
    let candidates = [
        r"C:\Program Files\nodejs\node.exe",
        r"C:\Program Files (x86)\nodejs\node.exe",
    ];
    #[cfg(not(windows))]
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
        for rel in [
            ".nvm/current/bin/node",
            ".volta/bin/node",
            ".local/share/mise/shims/node",
            ".fnm/current/bin/node",
        ] {
            let p = home.join(rel);
            if p.is_file() {
                return p;
            }
        }
    }
    PathBuf::from(if cfg!(windows) { "node.exe" } else { "node" })
}

fn check_resource_dir(resource_root: &Path) -> Option<ResolvedBackendTarget> {
    let monitor_name = if cfg!(windows) {
        "hanzo-resource-monitor.exe"
    } else {
        "hanzo-resource-monitor"
    };
    let standalone_name = if cfg!(windows) { "hanzo.exe" } else { "hanzo" };

    for prefix in [resource_root.join("resources"), resource_root.to_path_buf()] {
        let monitor = prefix.join(monitor_name);
        let monitor_opt = monitor.is_file().then_some(monitor);

        let standalone_exe = prefix.join("server").join(standalone_name);
        if standalone_exe.is_file() {
            return Some(ResolvedBackendTarget::StandaloneBinary {
                executable: standalone_exe,
                cwd: prefix.join("server"),
                resource_monitor: monitor_opt,
            });
        }

        let entry_mjs = prefix.join("server/dist/bin.mjs");
        if entry_mjs.is_file() {
            return Some(ResolvedBackendTarget::NodeScript {
                node_bin: resolve_node_executable(),
                entry_mjs,
                cwd: prefix.join("server"),
                resource_monitor: monitor_opt,
            });
        }
    }
    None
}

pub fn resolve_backend_target() -> Option<ResolvedBackendTarget> {
    if let Ok(explicit) = env::var("HANZO_DEV_BACKEND_ENTRY") {
        let entry = PathBuf::from(explicit.trim());
        if entry.is_file() {
            let cwd = entry
                .parent()
                .and_then(Path::parent)
                .map(Path::to_path_buf)
                .unwrap_or_else(|| PathBuf::from("."));
            return Some(ResolvedBackendTarget::NodeScript {
                node_bin: resolve_node_executable(),
                entry_mjs: entry,
                cwd,
                resource_monitor: None,
            });
        }
    }

    if let Ok(explicit_repo) = env::var("HANZO_DEV_REPO_ROOT") {
        let repo = PathBuf::from(explicit_repo.trim());
        let entry_mjs = repo.join("apps/server/dist/bin.mjs");
        if entry_mjs.is_file() {
            let monitor =
                repo.join("native/resource-monitor/target/release/hanzo-resource-monitor");
            return Some(ResolvedBackendTarget::NodeScript {
                node_bin: resolve_node_executable(),
                entry_mjs,
                cwd: repo.join("apps/server"),
                resource_monitor: monitor.is_file().then_some(monitor),
            });
        }
    }

    // Check packaged Tauri bundle Resources directories next to the executable first.
    if let Ok(exe) = env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            let mut candidates = vec![
                exe_dir.to_path_buf(),
                exe_dir.join("Resources"),
                exe_dir.join("../Resources"),
                exe_dir.join("../lib/Hanzo Dev"),
                exe_dir.join("../lib/hanzo-dev-desktop"),
            ];
            let mut cursor = exe_dir.parent();
            while let Some(dir) = cursor {
                if dir.join("apps/server/dist/bin.mjs").is_file() {
                    let monitor =
                        dir.join("native/resource-monitor/target/release/hanzo-resource-monitor");
                    return Some(ResolvedBackendTarget::NodeScript {
                        node_bin: resolve_node_executable(),
                        entry_mjs: dir.join("apps/server/dist/bin.mjs"),
                        cwd: dir.join("apps/server"),
                        resource_monitor: monitor.is_file().then_some(monitor),
                    });
                }
                candidates.push(dir.join("Resources"));
                cursor = dir.parent();
            }
            for candidate in candidates {
                if let Some(found) = check_resource_dir(&candidate) {
                    return Some(found);
                }
            }
        }
    }

    // Fallback to compile-time workspace root during local cargo development.
    let compile_manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    if let Some(repo) = compile_manifest
        .parent()
        .and_then(|p| p.parent())
        .and_then(|p| p.parent())
    {
        let entry_mjs = repo.join("apps/server/dist/bin.mjs");
        if entry_mjs.is_file() {
            let monitor =
                repo.join("native/resource-monitor/target/release/hanzo-resource-monitor");
            return Some(ResolvedBackendTarget::NodeScript {
                node_bin: resolve_node_executable(),
                entry_mjs,
                cwd: repo.join("apps/server"),
                resource_monitor: monitor.is_file().then_some(monitor),
            });
        }
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
                    let Some(target) = resolve_backend_target() else {
                        eprintln!("[hanzo-tauri] Backend entry not found; retrying...");
                        tokio::time::sleep(Duration::from_secs(2)).await;
                        continue;
                    };

                    let (program, entry_arg, backend_cwd, monitor_path) = match target {
                        ResolvedBackendTarget::NodeScript {
                            node_bin,
                            entry_mjs,
                            cwd,
                            resource_monitor,
                        } => (node_bin, Some(entry_mjs), cwd, resource_monitor),
                        ResolvedBackendTarget::StandaloneBinary {
                            executable,
                            cwd,
                            resource_monitor,
                        } => (executable, None, cwd, resource_monitor),
                    };

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

                    let mut cmd = Command::new(&program);
                    if let Some(ref script) = entry_arg {
                        cmd.arg(script);
                    }
                    cmd.arg("--bootstrap-fd")
                        .arg("0")
                        .current_dir(&backend_cwd)
                        .env("PATH", augmented_path())
                        .env("HANZO_DEV_DESKTOP_HOST_SCOPE", &cfg.host_scope)
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
                            eprintln!("[hanzo-tauri] Failed to spawn {}: {err}", program.display());
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
