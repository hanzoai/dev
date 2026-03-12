//! Tunnel integration — provides ZT as a transport for hanzo-tunnel.
//!
//! When enabled, the dev CLI can use `zt://` relay URLs to route
//! tunnel traffic through the ZT fabric instead of plain WebSocket.
//!
//! The [`run_zt_transport`] function implements the same JSON frame protocol
//! as the WebSocket transport in hanzo-tunnel, but over a ZtConnection
//! (AsyncRead + AsyncWrite) using newline-delimited JSON.

#[cfg(feature = "tunnel")]
mod zt_tunnel {
    use crate::auth::HanzoJwtCredentials;
    use crate::config::ConfigBuilder;
    use crate::connection::ZtConnection;
    use crate::context::ZtContext;
    use crate::error::ZtError;
    use std::sync::Arc;
    use std::time::Duration;
    use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
    use tokio::sync::{mpsc, watch};
    use tracing::{debug, error, warn};

    /// Configuration for ZT tunnel transport
    pub struct ZtTunnelConfig {
        pub controller_url: String,
        pub service_name: String,
        pub billing_enabled: bool,
    }

    impl Default for ZtTunnelConfig {
        fn default() -> Self {
            Self {
                controller_url: "https://zt-api.hanzo.ai".to_string(),
                service_name: "hanzo-relay".to_string(),
                billing_enabled: true,
            }
        }
    }

    /// Create a ZT context configured for tunnel use
    pub async fn create_tunnel_context(
        config: &ZtTunnelConfig,
    ) -> std::result::Result<Arc<ZtContext>, ZtError> {
        let creds = HanzoJwtCredentials::resolve()?;
        let zt_config = ConfigBuilder::new()
            .controller_url(&config.controller_url)
            .credentials(creds)
            .billing(config.billing_enabled)
            .build()?;

        let ctx = ZtContext::new(zt_config).await?;
        ctx.authenticate().await?;

        Ok(Arc::new(ctx))
    }

    /// Run the ZT frame transport loop.
    ///
    /// Reads and writes newline-delimited JSON frames over the ZtConnection,
    /// matching the same `Frame` protocol that the WebSocket transport uses.
    /// This allows the tunnel dispatch infrastructure (TunnelConnection,
    /// CommandDispatcher, etc.) to work identically over ZT.
    pub async fn run_zt_transport(
        conn: ZtConnection,
        mut outgoing_rx: mpsc::Receiver<hanzo_tunnel::Frame>,
        incoming_tx: mpsc::Sender<hanzo_tunnel::Frame>,
        mut shutdown_rx: watch::Receiver<bool>,
    ) {
        let (read_half, mut write_half) = tokio::io::split(conn);
        let mut reader = BufReader::new(read_half);
        let mut line_buf = String::new();

        let heartbeat_interval = Duration::from_secs(30);
        let mut heartbeat = tokio::time::interval(heartbeat_interval);
        heartbeat.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);

        loop {
            tokio::select! {
                // Shutdown signal
                _ = shutdown_rx.changed() => {
                    if *shutdown_rx.borrow() {
                        debug!("ZT transport shutdown");
                        let _ = write_half.shutdown().await;
                        return;
                    }
                }

                // Outgoing frame → ZT connection
                Some(frame) = outgoing_rx.recv() => {
                    match serde_json::to_string(&frame) {
                        Ok(mut json) => {
                            json.push('\n');
                            if let Err(e) = write_half.write_all(json.as_bytes()).await {
                                error!(error = %e, "ZT write error");
                                return;
                            }
                            if let Err(e) = write_half.flush().await {
                                error!(error = %e, "ZT flush error");
                                return;
                            }
                        }
                        Err(e) => {
                            error!(error = %e, "frame serialization error");
                        }
                    }
                }

                // Incoming data from ZT connection → frame
                result = reader.read_line(&mut line_buf) => {
                    match result {
                        Ok(0) => {
                            debug!("ZT connection EOF");
                            return;
                        }
                        Ok(_) => {
                            let trimmed = line_buf.trim();
                            if !trimmed.is_empty() {
                                match serde_json::from_str::<hanzo_tunnel::Frame>(trimmed) {
                                    Ok(hanzo_tunnel::Frame::Ping) => {
                                        // Auto-respond to pings
                                        if let Ok(mut pong) = serde_json::to_string(&hanzo_tunnel::Frame::Pong) {
                                            pong.push('\n');
                                            let _ = write_half.write_all(pong.as_bytes()).await;
                                            let _ = write_half.flush().await;
                                        }
                                    }
                                    Ok(hanzo_tunnel::Frame::Pong) => {
                                        debug!("received pong via ZT");
                                    }
                                    Ok(frame) => {
                                        if incoming_tx.send(frame).await.is_err() {
                                            return; // receiver dropped
                                        }
                                    }
                                    Err(e) => {
                                        warn!(error = %e, line = %trimmed, "invalid frame on ZT");
                                    }
                                }
                            }
                            line_buf.clear();
                        }
                        Err(e) => {
                            error!(error = %e, "ZT read error");
                            return;
                        }
                    }
                }

                // Heartbeat
                _ = heartbeat.tick() => {
                    if let Ok(mut ping) = serde_json::to_string(&hanzo_tunnel::Frame::Ping) {
                        ping.push('\n');
                        if write_half.write_all(ping.as_bytes()).await.is_err() {
                            return;
                        }
                        let _ = write_half.flush().await;
                    }
                }
            }
        }
    }
}

#[cfg(feature = "tunnel")]
pub use zt_tunnel::{ZtTunnelConfig, create_tunnel_context, run_zt_transport};
