//! KMS configuration

use crate::kms::{KmsProvider, error::{KmsError, Result}};
use serde::{Deserialize, Serialize};

/// KMS configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KmsConfig {
    /// KMS provider to use
    pub provider: KmsProvider,

    /// Local KMS key directory (for local provider)
    pub local_key_dir: Option<String>,

    /// AWS region (for AWS KMS)
    pub aws_region: Option<String>,

    /// AWS access key ID (optional, uses default credential chain if not set)
    pub aws_access_key_id: Option<String>,

    /// AWS secret access key
    pub aws_secret_access_key: Option<String>,

    /// Vault address (for HashiCorp Vault)
    pub vault_addr: Option<String>,

    /// Vault token
    pub vault_token: Option<String>,

    /// Vault mount path for transit secrets engine
    pub vault_mount_path: Option<String>,

    /// Cache TTL in seconds (0 to disable)
    pub cache_ttl_secs: u64,

    /// Maximum retry attempts for backend operations
    pub max_retries: u32,

    /// Retry backoff base in milliseconds
    pub retry_backoff_ms: u64,
}

impl Default for KmsConfig {
    fn default() -> Self {
        Self {
            provider: KmsProvider::Local,
            local_key_dir: None,
            aws_region: None,
            aws_access_key_id: None,
            aws_secret_access_key: None,
            vault_addr: None,
            vault_token: None,
            vault_mount_path: Some("transit".to_string()),
            cache_ttl_secs: 300, // 5 minutes
            max_retries: 3,
            retry_backoff_ms: 100,
        }
    }
}

impl KmsConfig {
    /// Load configuration from environment variables
    pub fn from_env() -> Result<Self> {
        let provider = std::env::var("HANZO_KMS_PROVIDER")
            .unwrap_or_else(|_| "local".to_string())
            .parse()?;

        Ok(Self {
            provider,
            local_key_dir: std::env::var("HANZO_KMS_LOCAL_DIR").ok(),
            aws_region: std::env::var("AWS_REGION")
                .or_else(|_| std::env::var("AWS_DEFAULT_REGION"))
                .ok(),
            aws_access_key_id: std::env::var("AWS_ACCESS_KEY_ID").ok(),
            aws_secret_access_key: std::env::var("AWS_SECRET_ACCESS_KEY").ok(),
            vault_addr: std::env::var("VAULT_ADDR").ok(),
            vault_token: std::env::var("VAULT_TOKEN").ok(),
            vault_mount_path: std::env::var("VAULT_MOUNT_PATH").ok(),
            cache_ttl_secs: std::env::var("HANZO_KMS_CACHE_TTL")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(300),
            max_retries: std::env::var("HANZO_KMS_MAX_RETRIES")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(3),
            retry_backoff_ms: std::env::var("HANZO_KMS_RETRY_BACKOFF_MS")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(100),
        })
    }

    /// Validate configuration for the selected provider
    pub fn validate(&self) -> Result<()> {
        match self.provider {
            KmsProvider::Local => Ok(()),
            KmsProvider::Aws => {
                if self.aws_region.is_none() {
                    return Err(KmsError::ConfigError(
                        "AWS_REGION is required for AWS KMS".to_string(),
                    ));
                }
                Ok(())
            }
            KmsProvider::Vault => {
                if self.vault_addr.is_none() {
                    return Err(KmsError::ConfigError(
                        "VAULT_ADDR is required for HashiCorp Vault".to_string(),
                    ));
                }
                if self.vault_token.is_none() {
                    return Err(KmsError::ConfigError(
                        "VAULT_TOKEN is required for HashiCorp Vault".to_string(),
                    ));
                }
                Ok(())
            }
        }
    }
}
