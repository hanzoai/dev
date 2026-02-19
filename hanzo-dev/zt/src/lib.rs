//! # hanzo-zt
//!
//! Zero Trust networking SDK for Hanzo services.
//!
//! Provides authenticated, billed access to services running on the Hanzo
//! Zero Trust fabric. All connections are routed through ZT edge routers
//! with mTLS encryption and NAT traversal.
//!
//! ## Quick Start
//!
//! ```no_run
//! use hanzo_zt::{ZtContext, Config, ConfigBuilder, HanzoJwtCredentials};
//!
//! # async fn example() -> hanzo_zt::Result<()> {
//! let creds = HanzoJwtCredentials::resolve()?;
//! let config = ConfigBuilder::new()
//!     .controller_url("https://zt-api.hanzo.ai")
//!     .credentials(creds)
//!     .billing(true)
//!     .build()?;
//!
//! let ctx = ZtContext::new(config).await?;
//! ctx.authenticate().await?;
//!
//! // Dial a service
//! let conn = ctx.dial("my-service").await?;
//! # Ok(())
//! # }
//! ```

pub mod auth;
pub mod billing;
pub mod config;
pub mod connection;
pub mod context;
pub mod controller;
pub mod error;
pub mod events;
pub mod models;
pub mod transport;
pub mod tunnel;

// Re-exports
pub use auth::{ApiKeyCredentials, Credentials, HanzoJwtCredentials};
pub use billing::{BillingGuard, UsageRecord};
pub use config::{Config, ConfigBuilder};
pub use connection::ZtConnection;
pub use context::{ZtContext, ZtListener};
pub use error::{Result, ZtError};
pub use events::{EventEmitter, ZtEvent};
pub use models::*;

#[cfg(feature = "zap")]
pub use transport::ZtTransport;

#[cfg(feature = "tunnel")]
pub use tunnel::{ZtTunnelConfig, create_tunnel_context};
