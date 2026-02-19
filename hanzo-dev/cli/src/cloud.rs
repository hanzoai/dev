//! Cloud tunnel integration for Hanzo Dev.
//!
//! When `--cloud` or `HANZO_CLOUD=1` is set, spawns a background tunnel
//! connection that registers this dev instance with the Hanzo cloud relay
//! or directly with a local bot gateway, making it remotely manageable
//! from app.hanzo.bot.
//!
//! Two connection modes:
//! - **Tunnel protocol** (default): Connects to a cloud relay at
//!   `wss://api.hanzo.ai/v1/relay` using the simple JSON tunnel protocol.
//! - **Gateway protocol**: Connects to a local bot gateway at
//!   `ws://127.0.0.1:18789/v1/tunnel` using the tunnel adapter endpoint.
//!   Use `--cloud ws://127.0.0.1:18789/v1/tunnel` or
//!   `--cloud local` for this mode.

use clap::Parser;
use hanzo_tunnel::commands::{CommandDispatcher, run_tunnel_dispatch_loop};
use hanzo_tunnel::terminal::TerminalManager;
use hanzo_tunnel::{AppKind, TunnelConfig, TunnelConnection};
use std::sync::Arc;
use tokio::sync::mpsc;
use tracing::{debug, info, warn};

const DEFAULT_RELAY_URL: &str = "wss://api.hanzo.ai/v1/relay";
const DEFAULT_GATEWAY_URL: &str = "ws://127.0.0.1:18789/v1/tunnel";

/// CLI arguments for the cloud tunnel.
#[derive(Debug, Parser)]
pub struct CloudArgs {
    /// Connect to the cloud relay or local bot gateway.
    ///
    /// Pass a URL to use a custom endpoint, or one of:
    ///   --cloud         → cloud relay (wss://api.hanzo.ai/v1/relay)
    ///   --cloud local   → local bot gateway (ws://127.0.0.1:18789/v1/tunnel)
    ///   --cloud <URL>   → custom endpoint
    ///
    /// Also enabled by HANZO_CLOUD=1 env var.
    #[clap(
        long = "cloud",
        global = true,
        value_name = "URL",
        num_args = 0..=1,
        default_missing_value = DEFAULT_RELAY_URL,
    )]
    pub cloud_url: Option<String>,
}

impl CloudArgs {
    /// Determine if cloud mode is enabled (via flag or env var).
    pub fn is_enabled(&self) -> bool {
        if self.cloud_url.is_some() {
            return true;
        }
        matches!(
            std::env::var("HANZO_CLOUD").as_deref(),
            Ok("1") | Ok("true")
        )
    }

    /// Get the relay URL to use.
    pub fn relay_url(&self) -> String {
        let url = self
            .cloud_url
            .clone()
            .unwrap_or_else(|| {
                std::env::var("HANZO_CLOUD_URL").unwrap_or_else(|_| DEFAULT_RELAY_URL.into())
            });
        // "local" is shorthand for the default gateway URL.
        if url == "local" || url == "gateway" {
            DEFAULT_GATEWAY_URL.into()
        } else {
            url
        }
    }

    /// Whether the target is a local bot gateway (vs cloud relay).
    pub fn is_gateway(&self) -> bool {
        let url = self.relay_url();
        url.contains("/v1/tunnel") || url.contains("127.0.0.1:18789") || url.contains("localhost:18789")
    }
}

/// A handle to the cloud connection. Keeps it alive until dropped.
pub struct CloudHandle {
    conn: Arc<TunnelConnection>,
}

impl CloudHandle {
    /// Get the instance/node ID.
    pub fn id(&self) -> &str {
        &self.conn.instance_id
    }
}

impl Drop for CloudHandle {
    fn drop(&mut self) {
        self.conn.shutdown();
    }
}

/// Connect to the cloud relay or bot gateway if enabled.
///
/// Connects via the tunnel protocol, waits for registration confirmation,
/// and spawns a background dispatch loop that handles incoming commands
/// (terminal.*, system.*, dev.*).
pub async fn maybe_connect(args: &CloudArgs) -> Option<CloudHandle> {
    if !args.is_enabled() {
        return None;
    }

    let relay_url = args.relay_url();
    let auth_token = resolve_auth_token();

    if auth_token.is_empty() {
        warn!("no auth token found for cloud tunnel (set HANZO_API_KEY or run `dev login`)");
        return None;
    }

    info!(relay_url = %relay_url, "connecting to cloud");

    let config = TunnelConfig {
        relay_url: relay_url.clone(),
        auth_token: auth_token.clone(),
        app_kind: AppKind::Dev,
        display_name: hostname(),
        capabilities: dev_capabilities(),
        commands: dev_commands(),
        cwd: std::env::current_dir().ok(),
        version: hanzo_version::version().to_string(),
        ..Default::default()
    };

    match hanzo_tunnel::connect_and_register(config).await {
        Ok((connection, session_url)) => {
            let conn = Arc::new(connection);

            if let Some(ref url) = session_url {
                // Show the user where their instance is available.
                eprintln!("\x1b[2m  cloud: {url}\x1b[0m");
                info!(instance_id = %conn.instance_id, session_url = %url, "registered with cloud");
            } else {
                debug!(instance_id = %conn.instance_id, "connected (no session URL)");
            }

            // Set up command dispatch — terminal sessions, system commands, dev launch.
            let (terminal_tx, terminal_rx) = mpsc::channel(256);
            let terminal = Arc::new(TerminalManager::new(terminal_tx));
            let dispatcher = Arc::new(CommandDispatcher::new(terminal));

            // Spawn the dispatch loop in the background.
            let dispatch_conn = conn.clone();
            tokio::spawn(async move {
                run_tunnel_dispatch_loop(dispatch_conn, dispatcher, terminal_rx).await;
            });

            Some(CloudHandle { conn })
        }
        Err(e) => {
            warn!(error = %e, "failed to connect to cloud (continuing without)");
            None
        }
    }
}

/// Capabilities advertised by dev nodes.
fn dev_capabilities() -> Vec<String> {
    vec![
        "chat".into(),
        "exec".into(),
        "tools".into(),
        "review".into(),
        "mcp".into(),
        "terminal".into(),
    ]
}

/// Commands supported by dev nodes.
fn dev_commands() -> Vec<String> {
    vec![
        "dev.launch".into(),
        "dev.chat".into(),
        "dev.status".into(),
        "terminal.open".into(),
        "terminal.input".into(),
        "terminal.close".into(),
        "terminal.resize".into(),
        "terminal.list".into(),
        "system.run".into(),
        "system.info".into(),
    ]
}

/// Resolve an auth token from environment or auth file.
fn resolve_auth_token() -> String {
    if let Ok(key) = std::env::var("HANZO_API_KEY") {
        return key;
    }
    if let Ok(key) = std::env::var("OPENAI_API_KEY") {
        return key;
    }

    if let Ok(home) = std::env::var("HOME") {
        let auth_path = std::path::PathBuf::from(home).join(".hanzo").join("auth.json");
        if let Ok(contents) = std::fs::read_to_string(&auth_path) {
            if let Ok(auth) = serde_json::from_str::<serde_json::Value>(&contents) {
                for key in &["HANZO_API_KEY", "OPENAI_API_KEY"] {
                    if let Some(val) = auth.get(key).and_then(|v| v.as_str()) {
                        if !val.is_empty() {
                            return val.to_string();
                        }
                    }
                }
            }
        }
    }

    String::new()
}

/// Get the local hostname.
fn hostname() -> String {
    std::env::var("HOSTNAME")
        .or_else(|_| std::env::var("HOST"))
        .or_else(|_| {
            std::process::Command::new("hostname")
                .output()
                .ok()
                .and_then(|o| String::from_utf8(o.stdout).ok())
                .map(|s| s.trim().to_string())
                .ok_or(std::env::VarError::NotPresent)
        })
        .unwrap_or_else(|_| "dev".into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cloud_args_default_disabled() {
        let args = CloudArgs { cloud_url: None };
        assert!(args.cloud_url.is_none());
    }

    #[test]
    fn cloud_args_custom_url() {
        let args = CloudArgs {
            cloud_url: Some("ws://127.0.0.1:8080".into()),
        };
        assert!(args.is_enabled());
        assert_eq!(args.relay_url(), "ws://127.0.0.1:8080");
    }

    #[test]
    fn cloud_args_default_url() {
        let args = CloudArgs {
            cloud_url: Some(DEFAULT_RELAY_URL.into()),
        };
        assert!(args.relay_url().contains("api.hanzo.ai"));
    }

    #[test]
    fn cloud_args_local_shorthand() {
        let args = CloudArgs {
            cloud_url: Some("local".into()),
        };
        assert!(args.relay_url().contains("127.0.0.1:18789"));
        assert!(args.relay_url().contains("/v1/tunnel"));
    }

    #[test]
    fn cloud_args_is_gateway() {
        let local = CloudArgs {
            cloud_url: Some("local".into()),
        };
        assert!(local.is_gateway());

        let cloud = CloudArgs {
            cloud_url: Some(DEFAULT_RELAY_URL.into()),
        };
        assert!(!cloud.is_gateway());
    }

    #[test]
    fn dev_commands_include_terminal() {
        let cmds = dev_commands();
        assert!(cmds.contains(&"terminal.open".to_string()));
        assert!(cmds.contains(&"dev.launch".to_string()));
        assert!(cmds.contains(&"system.run".to_string()));
    }
}
