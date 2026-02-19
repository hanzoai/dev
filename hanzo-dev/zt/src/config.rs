use crate::auth::Credentials;
use crate::error::{Result, ZtError};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

/// Configuration for a ZT context
#[derive(Clone)]
pub struct Config {
    /// ZT controller URL (e.g. "https://zt-api.hanzo.ai")
    pub controller_url: String,

    /// Authentication credentials
    pub credentials: Arc<dyn Credentials>,

    /// Whether to enforce billing checks before dial
    pub billing_enabled: bool,

    /// Commerce API URL for billing
    pub commerce_url: String,

    /// Path to identity file (optional, for cert-based auth)
    pub identity_file: Option<PathBuf>,

    /// Connection timeout
    pub connect_timeout: Duration,

    /// Request timeout for controller API calls
    pub request_timeout: Duration,
}

impl std::fmt::Debug for Config {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Config")
            .field("controller_url", &self.controller_url)
            .field("billing_enabled", &self.billing_enabled)
            .field("commerce_url", &self.commerce_url)
            .field("identity_file", &self.identity_file)
            .field("connect_timeout", &self.connect_timeout)
            .field("request_timeout", &self.request_timeout)
            .finish()
    }
}

/// Builder for ZT configuration
pub struct ConfigBuilder {
    controller_url: Option<String>,
    credentials: Option<Arc<dyn Credentials>>,
    billing_enabled: bool,
    commerce_url: String,
    identity_file: Option<PathBuf>,
    connect_timeout: Duration,
    request_timeout: Duration,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        Self {
            controller_url: None,
            credentials: None,
            billing_enabled: true,
            commerce_url: "https://api.hanzo.ai/commerce".to_string(),
            identity_file: None,
            connect_timeout: Duration::from_secs(30),
            request_timeout: Duration::from_secs(15),
        }
    }

    pub fn controller_url(mut self, url: impl Into<String>) -> Self {
        self.controller_url = Some(url.into());
        self
    }

    pub fn credentials(mut self, creds: impl Credentials + 'static) -> Self {
        self.credentials = Some(Arc::new(creds));
        self
    }

    pub fn billing(mut self, enabled: bool) -> Self {
        self.billing_enabled = enabled;
        self
    }

    pub fn commerce_url(mut self, url: impl Into<String>) -> Self {
        self.commerce_url = url.into();
        self
    }

    pub fn identity_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.identity_file = Some(path.into());
        self
    }

    pub fn connect_timeout(mut self, timeout: Duration) -> Self {
        self.connect_timeout = timeout;
        self
    }

    pub fn request_timeout(mut self, timeout: Duration) -> Self {
        self.request_timeout = timeout;
        self
    }

    pub fn build(self) -> Result<Config> {
        let controller_url = self.controller_url.ok_or_else(|| {
            ZtError::InvalidConfig("controller_url is required".into())
        })?;

        let credentials = self.credentials.ok_or_else(|| {
            ZtError::InvalidConfig("credentials are required".into())
        })?;

        Ok(Config {
            controller_url,
            credentials,
            billing_enabled: self.billing_enabled,
            commerce_url: self.commerce_url,
            identity_file: self.identity_file,
            connect_timeout: self.connect_timeout,
            request_timeout: self.request_timeout,
        })
    }
}

impl Default for ConfigBuilder {
    fn default() -> Self {
        Self::new()
    }
}
