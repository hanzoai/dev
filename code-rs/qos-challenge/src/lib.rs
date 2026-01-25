//! QoS Challenge System for Hanzo Network Compute Marketplace.
//!
//! This crate implements the Quality of Service (QoS) Challenge System that ensures
//! compute providers maintain advertised service levels through cryptographic
//! challenges, verifiable proofs, and economic incentives.
//!
//! # Architecture
//!
//! The system consists of several key components:
//!
//! - **Challenge Generator**: Creates cryptographically random challenges for providers
//! - **Verification Engine**: Verifies proofs submitted by providers
//! - **Scoring System**: Calculates composite QoS scores from challenge results
//! - **Penalty/Reward Engine**: Applies economic incentives based on performance
//!
//! # Example
//!
//! ```rust,ignore
//! use code_qos_challenge::{
//!     ChallengeGenerator, VerificationEngine, ScoringEngine,
//!     types::{ChallengeType, ComputeMetric},
//! };
//!
//! // Create a challenge generator
//! let generator = ChallengeGenerator::new([0u8; 32]);
//!
//! // Generate a compute challenge
//! let challenge = generator.generate_challenge(
//!     ChallengeType::Compute(ComputeMetric::GpuFp32),
//!     5,  // difficulty
//!     30_000,  // 30 second deadline
//! );
//!
//! // Provider executes challenge and submits proof...
//! // Verification engine validates the proof
//! ```

#![deny(clippy::print_stdout, clippy::print_stderr)]

pub mod types;
pub mod challenge;
pub mod verification;
pub mod scoring;
pub mod economics;
pub mod crypto;
pub mod error;

// Re-export main types for convenience
pub use challenge::ChallengeGenerator;
pub use error::{QoSError, Result};
pub use scoring::ScoringEngine;
pub use types::*;
pub use verification::VerificationEngine;
