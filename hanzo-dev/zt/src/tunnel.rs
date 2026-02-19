//! Tunnel integration — provides ZT as a transport for hanzo-tunnel.
//!
//! When enabled, the dev CLI can use `zt://` relay URLs to route
//! tunnel traffic through the ZT fabric instead of plain WebSocket.

#[cfg(feature = "tunnel")]
mod zt_tunnel {
    use crate::auth::HanzoJwtCredentials;
    use crate::config::ConfigBuilder;
    use crate::context::ZtContext;
    use crate::error::ZtError;
    use std::sync::Arc;

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
}

#[cfg(feature = "tunnel")]
pub use zt_tunnel::{ZtTunnelConfig, create_tunnel_context};
