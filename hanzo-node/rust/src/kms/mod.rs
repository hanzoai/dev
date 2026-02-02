//! KMS (Key Management Service) module for hanzo-node
//!
//! Provides a unified interface for key management across multiple backends:
//! - Local (development)
//! - AWS KMS
//! - HashiCorp Vault

pub mod backends;
pub mod cache;
pub mod config;
pub mod envelope;
pub mod error;
pub mod traits;
pub mod types;

pub use backends::{aws::AwsKms, local::LocalKms, vault::VaultKms};
pub use cache::KeyCache;
pub use config::KmsConfig;
pub use envelope::EnvelopeEncryption;
pub use error::{KmsError, Result};
pub use traits::KeyManagementService;
pub use types::{KeyMetadata, KeyType, KeyUsage};

use crate::storage::KeyValueStore;
use std::sync::Arc;

/// KMS provider enum for runtime backend selection
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KmsProvider {
    Local,
    Aws,
    Vault,
}

impl std::str::FromStr for KmsProvider {
    type Err = KmsError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "local" | "dev" => Ok(Self::Local),
            "aws" | "aws-kms" => Ok(Self::Aws),
            "vault" | "hashicorp-vault" => Ok(Self::Vault),
            _ => Err(KmsError::InvalidProvider(s.to_string())),
        }
    }
}

/// Create a KMS instance based on configuration
///
/// For the local provider, a KeyValueStore backend must be provided.
pub async fn create_kms(
    config: &KmsConfig,
    storage: Option<Arc<dyn KeyValueStore>>,
) -> Result<Arc<dyn KeyManagementService>> {
    match config.provider {
        KmsProvider::Local => {
            let storage = storage.ok_or_else(|| {
                KmsError::ConfigError("Local KMS requires storage backend".to_string())
            })?;
            let kms = LocalKms::new(storage)?;
            Ok(Arc::new(kms))
        }
        KmsProvider::Aws => {
            let kms = AwsKms::new(config).await?;
            Ok(Arc::new(kms))
        }
        KmsProvider::Vault => {
            let kms = VaultKms::new(config).await?;
            Ok(Arc::new(kms))
        }
    }
}
