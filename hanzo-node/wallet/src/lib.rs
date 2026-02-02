//! Hanzo MPC Wallet
//!
//! Multi-Party Computation wallet providing threshold signatures for:
//! - FROST Schnorr signatures (Bitcoin Taproot, Solana)
//! - ECDSA threshold signatures (EVM chains)
//!
//! This crate implements:
//! - Pedersen DKG for distributed key generation
//! - FROST signing protocol for Schnorr signatures
//! - Threshold ECDSA signing for Ethereum compatibility
//! - Encrypted key share storage
//! - Policy-based approval engine

pub mod dkg;
pub mod error;
pub mod grpc;
pub mod policy;
pub mod signing;
pub mod storage;

pub use dkg::{DkgCoordinator, DkgParticipant, DkgRound1Package, DkgRound2Package, KeyShare};
pub use error::{WalletError, WalletResult};
pub use grpc::WalletService;
pub use policy::{ApprovalPolicy, PolicyEngine, PolicyRule};
pub use signing::{
    EcdsaSigner, FrostSigner, SigningCoordinator, SigningRequest, SigningResponse,
};
pub use storage::{EncryptedKeyStore, KeyMetadata};

/// Curve type for key generation and signing
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum CurveType {
    /// secp256k1 - used by Bitcoin, Ethereum, and most EVM chains
    Secp256k1,
    /// ed25519 - used by Solana, NEAR, and other chains
    Ed25519,
}

impl std::fmt::Display for CurveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CurveType::Secp256k1 => write!(f, "secp256k1"),
            CurveType::Ed25519 => write!(f, "ed25519"),
        }
    }
}

/// Signature type produced by the MPC wallet
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum SignatureType {
    /// Schnorr signature (FROST) - for Bitcoin Taproot
    FrostSchnorr,
    /// ECDSA signature - for Ethereum and EVM chains
    Ecdsa,
    /// Ed25519 signature - for Solana
    Ed25519,
}

impl std::fmt::Display for SignatureType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignatureType::FrostSchnorr => write!(f, "frost-schnorr"),
            SignatureType::Ecdsa => write!(f, "ecdsa"),
            SignatureType::Ed25519 => write!(f, "ed25519"),
        }
    }
}

/// Configuration for the MPC wallet
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct WalletConfig {
    /// Minimum number of signers required (threshold)
    pub threshold: u16,
    /// Total number of signers
    pub total_signers: u16,
    /// Storage path for encrypted key shares
    pub storage_path: std::path::PathBuf,
    /// gRPC listen address
    pub grpc_addr: std::net::SocketAddr,
    /// Policy configuration
    pub policy: PolicyConfig,
}

/// Policy configuration
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct PolicyConfig {
    /// Maximum transaction value without additional approval
    pub max_auto_approve_value: Option<u64>,
    /// Required approvers for high-value transactions
    pub required_approvers: Vec<String>,
    /// Whitelist of allowed destination addresses
    pub whitelist: Vec<String>,
    /// Rate limit: max transactions per hour
    pub rate_limit_per_hour: Option<u32>,
}

impl Default for WalletConfig {
    fn default() -> Self {
        Self {
            threshold: 2,
            total_signers: 3,
            storage_path: std::path::PathBuf::from("./wallet_data"),
            grpc_addr: "127.0.0.1:50051".parse().unwrap_or_else(|_| {
                std::net::SocketAddr::from(([127, 0, 0, 1], 50051))
            }),
            policy: PolicyConfig::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_curve_type_display() {
        assert_eq!(CurveType::Secp256k1.to_string(), "secp256k1");
        assert_eq!(CurveType::Ed25519.to_string(), "ed25519");
    }

    #[test]
    fn test_signature_type_display() {
        assert_eq!(SignatureType::FrostSchnorr.to_string(), "frost-schnorr");
        assert_eq!(SignatureType::Ecdsa.to_string(), "ecdsa");
        assert_eq!(SignatureType::Ed25519.to_string(), "ed25519");
    }

    #[test]
    fn test_default_config() {
        let config = WalletConfig::default();
        assert_eq!(config.threshold, 2);
        assert_eq!(config.total_signers, 3);
    }
}
