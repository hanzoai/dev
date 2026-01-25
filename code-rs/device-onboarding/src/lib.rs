//! Device Onboarding via QR Code for Hanzo Node
//!
//! This crate provides QR code-based device pairing and onboarding
//! for the Hanzo Node ecosystem. It supports:
//!
//! - QR code generation with cryptographic pairing information
//! - Secure pairing protocol with hybrid key exchange
//! - Post-quantum resistant cryptography (X25519 + ML-KEM ready)
//! - Device identity management via W3C DIDs
//!
//! # Example
//!
//! ```rust,ignore
//! use code_device_onboarding::{QRGenerator, PairingType};
//!
//! // Generate a pairing QR code
//! let generator = QRGenerator::new(
//!     "did:hanzo:zeekay",
//!     signing_key,
//! );
//!
//! let payload = generator.generate_pairing_qr(
//!     PairingType::DeviceOnboarding,
//!     Some(300), // 5 minute TTL
//! )?;
//!
//! // Render to PNG
//! let png_bytes = generator.render_qr(&payload)?;
//! ```

pub mod error;
pub mod pairing;
pub mod qr;
pub mod crypto;

pub use error::*;
pub use pairing::*;
pub use qr::*;
pub use crypto::*;
