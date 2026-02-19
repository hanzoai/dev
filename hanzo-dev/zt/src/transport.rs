//! ZAP Transport implementation over ZT connections.
//!
//! Registers the `zt://` URL scheme with the ZAP transport system,
//! allowing ZAP RPC calls to flow through the ZT fabric.

#[cfg(feature = "zap")]
mod zt_transport {
    use crate::auth::HanzoJwtCredentials;
    use crate::config::ConfigBuilder;
    use crate::context::ZtContext;
    use crate::error::ZtError;
    use hanzo_zap::transport::Transport;
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::Arc;
    use tokio::sync::Mutex;

    /// ZAP Transport that routes messages through the ZT fabric.
    ///
    /// Use `zt://service-name` URLs with the ZAP client to route
    /// RPC calls through zero-trust overlay networking.
    pub struct ZtTransport {
        #[allow(dead_code)]
        ctx: Arc<ZtContext>,
        conn: Arc<Mutex<Option<crate::connection::ZtConnection>>>,
        service_name: String,
        connected: std::sync::atomic::AtomicBool,
    }

    impl ZtTransport {
        /// Create a new ZT transport for the given service
        pub async fn new(
            controller_url: &str,
            service_name: &str,
        ) -> std::result::Result<Self, ZtError> {
            let creds = HanzoJwtCredentials::resolve()?;
            let config = ConfigBuilder::new()
                .controller_url(controller_url)
                .credentials(creds)
                .billing(true)
                .build()?;

            let ctx = ZtContext::new(config).await?;
            ctx.authenticate().await?;

            let conn = ctx.dial(service_name).await?;

            Ok(Self {
                ctx: Arc::new(ctx),
                conn: Arc::new(Mutex::new(Some(conn))),
                service_name: service_name.to_string(),
                connected: std::sync::atomic::AtomicBool::new(true),
            })
        }

        /// Create from an existing ZT context and service name
        pub async fn from_context(
            ctx: Arc<ZtContext>,
            service_name: &str,
        ) -> std::result::Result<Self, ZtError> {
            let conn = ctx.dial(service_name).await?;

            Ok(Self {
                ctx,
                conn: Arc::new(Mutex::new(Some(conn))),
                service_name: service_name.to_string(),
                connected: std::sync::atomic::AtomicBool::new(true),
            })
        }
    }

    impl Transport for ZtTransport {
        fn send(&self, data: &[u8]) -> Pin<Box<dyn Future<Output = hanzo_zap::error::Result<()>> + Send + '_>> {
            let data = data.to_vec();
            Box::pin(async move {
                let conn = self.conn.lock().await;
                match conn.as_ref() {
                    Some(c) => c.send(&data).await.map_err(|e| {
                        hanzo_zap::error::Error::Transport(e.to_string())
                    }),
                    None => Err(hanzo_zap::error::Error::Transport(
                        "connection closed".into(),
                    )),
                }
            })
        }

        fn recv(&self) -> Pin<Box<dyn Future<Output = hanzo_zap::error::Result<Vec<u8>>> + Send + '_>> {
            Box::pin(async move {
                let mut conn = self.conn.lock().await;
                match conn.as_mut() {
                    Some(c) => c.recv().await.map_err(|e| {
                        hanzo_zap::error::Error::Transport(e.to_string())
                    }),
                    None => Err(hanzo_zap::error::Error::Transport(
                        "connection closed".into(),
                    )),
                }
            })
        }

        fn close(&self) -> Pin<Box<dyn Future<Output = hanzo_zap::error::Result<()>> + Send + '_>> {
            Box::pin(async move {
                let mut conn = self.conn.lock().await;
                if let Some(c) = conn.as_mut() {
                    let _ = c.close().await;
                }
                *conn = None;
                self.connected
                    .store(false, std::sync::atomic::Ordering::Release);
                Ok(())
            })
        }

        fn is_connected(&self) -> bool {
            self.connected.load(std::sync::atomic::Ordering::Acquire)
        }

        fn local_addr(&self) -> Option<String> {
            Some(format!("zt://local/{}", self.service_name))
        }

        fn peer_addr(&self) -> Option<String> {
            Some(format!("zt://{}", self.service_name))
        }
    }
}

#[cfg(feature = "zap")]
pub use zt_transport::ZtTransport;
