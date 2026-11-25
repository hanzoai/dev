use crate::backend::pick_available_port;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::net::TcpStream;
use std::process::Stdio;
use std::sync::Mutex;
use std::time::{Duration, Instant};
use tokio::process::Command;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct SshEnvironmentTargetInput {
    pub alias: String,
    pub hostname: String,
    pub username: Option<String>,
    pub port: Option<u16>,
}

#[derive(Debug, Clone)]
struct ActiveSshTunnel {
    local_port: u16,
    remote_port: u16,
    pid: u32,
}

pub struct SshTunnelSupervisor {
    tunnels: Mutex<HashMap<String, ActiveSshTunnel>>,
}

pub fn ssh_target_key(target: &SshEnvironmentTargetInput) -> String {
    let user = target.username.as_deref().unwrap_or("");
    let port = target.port.unwrap_or(22);
    format!("{user}@{}:{port}", target.hostname.trim())
}

pub fn build_ssh_tunnel_args(
    target: &SshEnvironmentTargetInput,
    local_port: u16,
    remote_port: u16,
) -> Vec<String> {
    let mut args = vec![
        "-N".to_string(),
        "-o".to_string(),
        "BatchMode=yes".to_string(),
        "-o".to_string(),
        "ExitOnForwardFailure=yes".to_string(),
        "-o".to_string(),
        "ConnectTimeout=10".to_string(),
        "-o".to_string(),
        "ServerAliveInterval=15".to_string(),
        "-o".to_string(),
        "ServerAliveCountMax=3".to_string(),
        "-L".to_string(),
        format!("127.0.0.1:{local_port}:127.0.0.1:{remote_port}"),
    ];
    if let Some(port) = target.port {
        args.push("-p".to_string());
        args.push(port.to_string());
    }
    let destination = match target.username.as_deref().map(str::trim) {
        Some(user) if !user.is_empty() => format!("{user}@{}", target.hostname.trim()),
        _ => target.hostname.trim().to_string(),
    };
    args.push(destination);
    args
}

fn terminate_pid(pid: u32) {
    if pid == 0 {
        return;
    }
    #[cfg(unix)]
    unsafe {
        libc::kill(pid as i32, libc::SIGTERM);
    }
    #[cfg(windows)]
    {
        let mut tk = std::process::Command::new("taskkill");
        tk.args(["/PID", &pid.to_string(), "/T", "/F"]);
        crate::hide_std_console(&mut tk);
        let _ = tk.status();
    }
}

fn is_pid_alive(pid: u32) -> bool {
    if pid == 0 {
        return false;
    }
    #[cfg(unix)]
    unsafe {
        libc::kill(pid as i32, 0) == 0
    }
    #[cfg(not(unix))]
    {
        let _ = pid;
        true
    }
}

fn is_local_port_reachable(port: u16) -> bool {
    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], port));
    TcpStream::connect_timeout(&addr, Duration::from_millis(250)).is_ok()
}

impl SshTunnelSupervisor {
    pub fn new() -> Self {
        Self {
            tunnels: Mutex::new(HashMap::new()),
        }
    }

    pub async fn ensure_environment(
        &self,
        target: SshEnvironmentTargetInput,
    ) -> Result<Value, String> {
        if target.hostname.trim().is_empty() {
            return Err("SSH target hostname must not be empty.".to_string());
        }

        let key = ssh_target_key(&target);
        if let Some(existing) = self
            .tunnels
            .lock()
            .ok()
            .and_then(|map| map.get(&key).cloned())
        {
            if is_pid_alive(existing.pid) && is_local_port_reachable(existing.local_port) {
                return Ok(build_bootstrap_response(
                    &target,
                    existing.local_port,
                    existing.remote_port,
                ));
            }
            terminate_pid(existing.pid);
            if let Ok(mut map) = self.tunnels.lock() {
                map.remove(&key);
            }
        }

        let remote_port = std::env::var("HANZO_SSH_REMOTE_PORT")
            .ok()
            .and_then(|s| s.trim().parse::<u16>().ok())
            .unwrap_or(3773);
        let local_port = pick_available_port(23773);
        let args = build_ssh_tunnel_args(&target, local_port, remote_port);

        let mut ssh_cmd = Command::new("ssh");
        ssh_cmd
            .args(&args)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped());
        crate::hide_tokio_console(&mut ssh_cmd);
        let mut child = ssh_cmd
            .spawn()
            .map_err(|e| format!("Failed to launch ssh client: {e}"))?;

        let pid = child.id().unwrap_or(0);
        let deadline = Instant::now() + Duration::from_secs(10);

        while Instant::now() < deadline {
            if let Ok(Some(exit_status)) = child.try_wait() {
                let mut stderr_buf = String::new();
                if let Some(mut stderr) = child.stderr.take() {
                    use tokio::io::AsyncReadExt;
                    let _ = stderr.read_to_string(&mut stderr_buf).await;
                }
                let detail = stderr_buf.trim();
                return Err(if detail.is_empty() {
                    format!(
                        "SSH tunnel to {} exited prematurely ({exit_status}). Ensure `hanzo serve` is running on remote port {remote_port}.",
                        target.hostname
                    )
                } else {
                    format!("SSH tunnel failed: {detail}")
                });
            }

            // Once ssh binds the local port, TCP connect succeeds or forwards to the remote port.
            if is_local_port_reachable(local_port) {
                if let Ok(mut map) = self.tunnels.lock() {
                    map.insert(
                        key,
                        ActiveSshTunnel {
                            local_port,
                            remote_port,
                            pid,
                        },
                    );
                }
                // Reap child asynchronously when disconnected so zombies never accumulate.
                tokio::spawn(async move {
                    let _ = child.wait().await;
                });
                return Ok(build_bootstrap_response(&target, local_port, remote_port));
            }

            tokio::time::sleep(Duration::from_millis(120)).await;
        }

        terminate_pid(pid);
        let _ = child.kill().await;
        Err(format!(
            "Timed out establishing SSH tunnel to {}:{remote_port}. Verify SSH key access and that Hanzo Dev server is listening on 127.0.0.1:{remote_port}.",
            target.hostname
        ))
    }

    pub fn disconnect(&self, target: &SshEnvironmentTargetInput) {
        let key = ssh_target_key(target);
        if let Ok(mut map) = self.tunnels.lock() {
            if let Some(active) = map.remove(&key) {
                terminate_pid(active.pid);
            }
        }
    }

    pub fn shutdown_all(&self) {
        if let Ok(mut map) = self.tunnels.lock() {
            for (_, active) in map.drain() {
                terminate_pid(active.pid);
            }
        }
    }
}

fn build_bootstrap_response(
    target: &SshEnvironmentTargetInput,
    local_port: u16,
    remote_port: u16,
) -> Value {
    json!({
        "target": {
            "alias": target.alias,
            "hostname": target.hostname,
            "username": target.username,
            "port": target.port,
        },
        "httpBaseUrl": format!("http://127.0.0.1:{local_port}"),
        "wsBaseUrl": format!("ws://127.0.0.1:{local_port}"),
        "pairingToken": Value::Null,
        "remotePort": remote_port,
        "remoteServerKind": "external",
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_ssh_tunnel_args_with_port_and_user() {
        let target = SshEnvironmentTargetInput {
            alias: "dg1".to_string(),
            hostname: "10.0.1.42".to_string(),
            username: Some("ubuntu".to_string()),
            port: Some(2222),
        };
        let args = build_ssh_tunnel_args(&target, 23773, 3773);
        assert!(args.contains(&"-N".to_string()));
        assert!(args.contains(&"BatchMode=yes".to_string()));
        assert!(args.contains(&"ExitOnForwardFailure=yes".to_string()));
        assert!(args.contains(&"127.0.0.1:23773:127.0.0.1:3773".to_string()));
        assert!(args.contains(&"-p".to_string()));
        assert!(args.contains(&"2222".to_string()));
        assert_eq!(args.last().map(String::as_str), Some("ubuntu@10.0.1.42"));
    }

    #[test]
    fn builds_ssh_tunnel_args_without_optional_user_or_port() {
        let target = SshEnvironmentTargetInput {
            alias: "bastion".to_string(),
            hostname: "bastion.hanzo.ai".to_string(),
            username: None,
            port: None,
        };
        let args = build_ssh_tunnel_args(&target, 23774, 3773);
        assert!(!args.contains(&"-p".to_string()));
        assert_eq!(args.last().map(String::as_str), Some("bastion.hanzo.ai"));
    }
}
