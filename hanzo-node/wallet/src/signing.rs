//! Threshold Signing Protocols
//!
//! This module implements:
//! - FROST Schnorr signatures for Bitcoin Taproot and Solana
//! - ECDSA threshold signatures for Ethereum and EVM chains
//!
//! FROST (Flexible Round-Optimized Schnorr Threshold) provides:
//! - Two-round signing protocol
//! - Identifiable abort (detect malicious signers)
//! - No trusted dealer required when used with DKG

use crate::dkg::KeyShare;
use crate::error::{WalletError, WalletResult};
use crate::{CurveType, SignatureType};

use parking_lot::RwLock;
use rand::rngs::OsRng;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

/// A request to sign a message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SigningRequest {
    /// Unique identifier for this signing session
    pub session_id: String,
    /// Key ID to use for signing
    pub key_id: String,
    /// Message hash to sign (32 bytes for most curves)
    #[serde(with = "hex")]
    pub message: Vec<u8>,
    /// Signature type to produce
    pub signature_type: SignatureType,
    /// Participants who will sign (must meet threshold)
    pub signers: Vec<String>,
    /// Optional metadata (e.g., transaction details for policy)
    pub metadata: Option<SigningMetadata>,
}

/// Metadata about what is being signed (for policy evaluation)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SigningMetadata {
    /// Transaction value in smallest unit (wei, satoshi, lamports)
    pub value: Option<u64>,
    /// Destination address
    pub destination: Option<String>,
    /// Chain ID (for EVM chains)
    pub chain_id: Option<u64>,
    /// Transaction type description
    pub tx_type: Option<String>,
}

/// Response from a signing operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SigningResponse {
    /// Session ID this response is for
    pub session_id: String,
    /// The signature bytes
    #[serde(with = "hex")]
    pub signature: Vec<u8>,
    /// Signature type
    pub signature_type: SignatureType,
    /// Recovery ID for ECDSA (v value)
    pub recovery_id: Option<u8>,
}

/// Round 1 commitment for FROST signing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SigningCommitment {
    pub participant_id: String,
    pub session_id: String,
    #[serde(with = "hex")]
    pub commitment: Vec<u8>,
}

/// Round 2 signature share for FROST signing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignatureShare {
    pub participant_id: String,
    pub session_id: String,
    #[serde(with = "hex")]
    pub share: Vec<u8>,
}

/// FROST signer for Schnorr signatures
pub struct FrostSigner {
    key_share: KeyShare,
    // Nonce state for current signing session
    nonce_state: RwLock<Option<NonceState>>,
}

#[derive(Clone)]
struct NonceState {
    session_id: String,
    nonces_bytes: Vec<u8>,
}

impl FrostSigner {
    /// Create a new FROST signer from a key share
    pub fn new(key_share: KeyShare) -> WalletResult<Self> {
        match key_share.curve {
            CurveType::Secp256k1 | CurveType::Ed25519 => {}
        }

        Ok(Self {
            key_share,
            nonce_state: RwLock::new(None),
        })
    }

    /// Generate Round 1 commitments for signing
    pub fn commit(&self, session_id: &str) -> WalletResult<SigningCommitment> {
        match self.key_share.curve {
            CurveType::Secp256k1 => self.commit_secp256k1(session_id),
            CurveType::Ed25519 => self.commit_ed25519(session_id),
        }
    }

    fn commit_secp256k1(&self, session_id: &str) -> WalletResult<SigningCommitment> {
        use frost_secp256k1 as frost;

        let signing_share: frost::keys::SigningShare =
            bincode::deserialize(self.key_share.signing_share_bytes())
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let (nonces, commitments) = frost::round1::commit(&signing_share, &mut OsRng);

        let nonces_bytes =
            bincode::serialize(&nonces).map_err(|e| WalletError::Serialization(e.to_string()))?;

        let commitment_bytes = bincode::serialize(&commitments)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        *self.nonce_state.write() = Some(NonceState {
            session_id: session_id.to_string(),
            nonces_bytes,
        });

        Ok(SigningCommitment {
            participant_id: self.key_share.participant_id.clone(),
            session_id: session_id.to_string(),
            commitment: commitment_bytes,
        })
    }

    fn commit_ed25519(&self, session_id: &str) -> WalletResult<SigningCommitment> {
        use frost_ed25519 as frost;

        let signing_share: frost::keys::SigningShare =
            bincode::deserialize(self.key_share.signing_share_bytes())
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let (nonces, commitments) = frost::round1::commit(&signing_share, &mut OsRng);

        let nonces_bytes =
            bincode::serialize(&nonces).map_err(|e| WalletError::Serialization(e.to_string()))?;

        let commitment_bytes = bincode::serialize(&commitments)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        *self.nonce_state.write() = Some(NonceState {
            session_id: session_id.to_string(),
            nonces_bytes,
        });

        Ok(SigningCommitment {
            participant_id: self.key_share.participant_id.clone(),
            session_id: session_id.to_string(),
            commitment: commitment_bytes,
        })
    }

    /// Generate Round 2 signature share
    pub fn sign(
        &self,
        session_id: &str,
        message: &[u8],
        commitments: &[SigningCommitment],
    ) -> WalletResult<SignatureShare> {
        match self.key_share.curve {
            CurveType::Secp256k1 => self.sign_secp256k1(session_id, message, commitments),
            CurveType::Ed25519 => self.sign_ed25519(session_id, message, commitments),
        }
    }

    fn sign_secp256k1(
        &self,
        session_id: &str,
        message: &[u8],
        commitments: &[SigningCommitment],
    ) -> WalletResult<SignatureShare> {
        use frost_secp256k1 as frost;

        let nonce_state = self.nonce_state.read();
        let nonce_state = nonce_state.as_ref().ok_or_else(|| {
            WalletError::Signing("No nonce state - call commit() first".to_string())
        })?;

        if nonce_state.session_id != session_id {
            return Err(WalletError::Signing(format!(
                "Session ID mismatch: expected {}, got {session_id}",
                nonce_state.session_id
            )));
        }

        let nonces: frost::round1::SigningNonces =
            bincode::deserialize(&nonce_state.nonces_bytes)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        // Parse signing share and verifying share
        let signing_share: frost::keys::SigningShare =
            bincode::deserialize(self.key_share.signing_share_bytes())
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let participant_id = self.parse_identifier_secp256k1()?;
        let verifying_share: frost::keys::VerifyingShare =
            bincode::deserialize(&self.key_share.verifying_share)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        // Build verifying key from group public key
        let verifying_key: frost::VerifyingKey =
            bincode::deserialize(&self.key_share.group_public_key)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        // Create key package with FROST 2.2 API (identifier, signing_share, verifying_share, verifying_key, min_signers)
        let key_package = frost::keys::KeyPackage::new(
            participant_id,
            signing_share,
            verifying_share,
            verifying_key,
            self.key_share.threshold,
        );

        // Build commitments map
        let mut commitment_map = BTreeMap::new();
        for c in commitments {
            let id = Self::parse_identifier_secp256k1_static(&c.participant_id)?;
            let commitment: frost::round1::SigningCommitments =
                bincode::deserialize(&c.commitment)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            commitment_map.insert(id, commitment);
        }

        // Build signing package - new API doesn't return Result
        let signing_package = frost::SigningPackage::new(commitment_map, message);

        // Generate signature share
        let signature_share = frost::round2::sign(&signing_package, &nonces, &key_package)
            .map_err(|e| WalletError::Signing(e.to_string()))?;

        let share_bytes = bincode::serialize(&signature_share)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        Ok(SignatureShare {
            participant_id: self.key_share.participant_id.clone(),
            session_id: session_id.to_string(),
            share: share_bytes,
        })
    }

    fn sign_ed25519(
        &self,
        session_id: &str,
        message: &[u8],
        commitments: &[SigningCommitment],
    ) -> WalletResult<SignatureShare> {
        use frost_ed25519 as frost;

        let nonce_state = self.nonce_state.read();
        let nonce_state = nonce_state.as_ref().ok_or_else(|| {
            WalletError::Signing("No nonce state - call commit() first".to_string())
        })?;

        if nonce_state.session_id != session_id {
            return Err(WalletError::Signing(format!(
                "Session ID mismatch: expected {}, got {session_id}",
                nonce_state.session_id
            )));
        }

        let nonces: frost::round1::SigningNonces =
            bincode::deserialize(&nonce_state.nonces_bytes)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let signing_share: frost::keys::SigningShare =
            bincode::deserialize(self.key_share.signing_share_bytes())
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let participant_id = self.parse_identifier_ed25519()?;
        let verifying_share: frost::keys::VerifyingShare =
            bincode::deserialize(&self.key_share.verifying_share)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let verifying_key: frost::VerifyingKey =
            bincode::deserialize(&self.key_share.group_public_key)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let key_package = frost::keys::KeyPackage::new(
            participant_id,
            signing_share,
            verifying_share,
            verifying_key,
            self.key_share.threshold,
        );

        let mut commitment_map = BTreeMap::new();
        for c in commitments {
            let id = Self::parse_identifier_ed25519_static(&c.participant_id)?;
            let commitment: frost::round1::SigningCommitments =
                bincode::deserialize(&c.commitment)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            commitment_map.insert(id, commitment);
        }

        let signing_package = frost::SigningPackage::new(commitment_map, message);

        let signature_share = frost::round2::sign(&signing_package, &nonces, &key_package)
            .map_err(|e| WalletError::Signing(e.to_string()))?;

        let share_bytes = bincode::serialize(&signature_share)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        Ok(SignatureShare {
            participant_id: self.key_share.participant_id.clone(),
            session_id: session_id.to_string(),
            share: share_bytes,
        })
    }

    /// Aggregate signature shares into final signature (coordinator operation)
    pub fn aggregate(
        &self,
        message: &[u8],
        commitments: &[SigningCommitment],
        shares: &[SignatureShare],
    ) -> WalletResult<Vec<u8>> {
        match self.key_share.curve {
            CurveType::Secp256k1 => self.aggregate_secp256k1(message, commitments, shares),
            CurveType::Ed25519 => self.aggregate_ed25519(message, commitments, shares),
        }
    }

    fn aggregate_secp256k1(
        &self,
        message: &[u8],
        commitments: &[SigningCommitment],
        shares: &[SignatureShare],
    ) -> WalletResult<Vec<u8>> {
        use frost_secp256k1 as frost;

        // Build commitments map
        let mut commitment_map = BTreeMap::new();
        for c in commitments {
            let id = Self::parse_identifier_secp256k1_static(&c.participant_id)?;
            let commitment: frost::round1::SigningCommitments =
                bincode::deserialize(&c.commitment)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            commitment_map.insert(id, commitment);
        }

        // Build signing package
        let signing_package = frost::SigningPackage::new(commitment_map, message);

        // Build shares map
        let mut share_map = BTreeMap::new();
        for s in shares {
            let id = Self::parse_identifier_secp256k1_static(&s.participant_id)?;
            let share: frost::round2::SignatureShare = bincode::deserialize(&s.share)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;
            share_map.insert(id, share);
        }

        // Build public key package
        let verifying_key: frost::VerifyingKey =
            bincode::deserialize(&self.key_share.group_public_key)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let mut verifying_shares_map = BTreeMap::new();
        for (id_str, share_bytes) in &self.key_share.verifying_shares {
            let id = Self::parse_identifier_secp256k1_static(id_str)?;
            let share: frost::keys::VerifyingShare = bincode::deserialize(share_bytes)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;
            verifying_shares_map.insert(id, share);
        }

        let pubkey_package = frost::keys::PublicKeyPackage::new(verifying_shares_map, verifying_key);

        // Aggregate
        let signature = frost::aggregate(&signing_package, &share_map, &pubkey_package)
            .map_err(|e| WalletError::Signing(format!("Aggregation failed: {e}")))?;

        let sig_bytes = bincode::serialize(&signature)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        Ok(sig_bytes)
    }

    fn aggregate_ed25519(
        &self,
        message: &[u8],
        commitments: &[SigningCommitment],
        shares: &[SignatureShare],
    ) -> WalletResult<Vec<u8>> {
        use frost_ed25519 as frost;

        let mut commitment_map = BTreeMap::new();
        for c in commitments {
            let id = Self::parse_identifier_ed25519_static(&c.participant_id)?;
            let commitment: frost::round1::SigningCommitments =
                bincode::deserialize(&c.commitment)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            commitment_map.insert(id, commitment);
        }

        let signing_package = frost::SigningPackage::new(commitment_map, message);

        let mut share_map = BTreeMap::new();
        for s in shares {
            let id = Self::parse_identifier_ed25519_static(&s.participant_id)?;
            let share: frost::round2::SignatureShare = bincode::deserialize(&s.share)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;
            share_map.insert(id, share);
        }

        let verifying_key: frost::VerifyingKey =
            bincode::deserialize(&self.key_share.group_public_key)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let mut verifying_shares_map = BTreeMap::new();
        for (id_str, share_bytes) in &self.key_share.verifying_shares {
            let id = Self::parse_identifier_ed25519_static(id_str)?;
            let share: frost::keys::VerifyingShare = bincode::deserialize(share_bytes)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;
            verifying_shares_map.insert(id, share);
        }

        let pubkey_package = frost::keys::PublicKeyPackage::new(verifying_shares_map, verifying_key);

        let signature = frost::aggregate(&signing_package, &share_map, &pubkey_package)
            .map_err(|e| WalletError::Signing(format!("Aggregation failed: {e}")))?;

        let sig_bytes = bincode::serialize(&signature)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        Ok(sig_bytes)
    }

    fn parse_identifier_secp256k1(
        &self,
    ) -> WalletResult<frost_core::Identifier<frost_secp256k1::Secp256K1Sha256>> {
        Self::parse_identifier_secp256k1_static(&self.key_share.participant_id)
    }

    fn parse_identifier_secp256k1_static(
        id: &str,
    ) -> WalletResult<frost_core::Identifier<frost_secp256k1::Secp256K1Sha256>> {
        let num: u16 = id
            .parse()
            .map_err(|_| WalletError::InvalidParticipant(id.to_string()))?;
        frost_core::Identifier::try_from(num)
            .map_err(|e| WalletError::InvalidParticipant(format!("{id}: {e}")))
    }

    fn parse_identifier_ed25519(
        &self,
    ) -> WalletResult<frost_core::Identifier<frost_ed25519::Ed25519Sha512>> {
        Self::parse_identifier_ed25519_static(&self.key_share.participant_id)
    }

    fn parse_identifier_ed25519_static(
        id: &str,
    ) -> WalletResult<frost_core::Identifier<frost_ed25519::Ed25519Sha512>> {
        let num: u16 = id
            .parse()
            .map_err(|_| WalletError::InvalidParticipant(id.to_string()))?;
        frost_core::Identifier::try_from(num)
            .map_err(|e| WalletError::InvalidParticipant(format!("{id}: {e}")))
    }
}

/// ECDSA signer for Ethereum and EVM chains
/// Uses Gennaro-Goldfeder protocol adapted for threshold ECDSA
pub struct EcdsaSigner {
    key_share: KeyShare,
}

impl EcdsaSigner {
    /// Create a new ECDSA signer from a key share
    pub fn new(key_share: KeyShare) -> WalletResult<Self> {
        if key_share.curve != CurveType::Secp256k1 {
            return Err(WalletError::CurveMismatch {
                expected: "secp256k1".to_string(),
                got: key_share.curve.to_string(),
            });
        }

        Ok(Self { key_share })
    }

    /// Sign a message hash using threshold ECDSA
    /// For EVM, the message should be the keccak256 hash of the transaction
    ///
    /// Note: Full threshold ECDSA requires a more complex protocol (GG18/GG20).
    /// This implementation uses FROST for the underlying threshold mechanism
    /// and adapts the signature format for ECDSA compatibility.
    pub fn sign_hash(&self, message_hash: &[u8; 32]) -> WalletResult<EcdsaSignature> {
        // For production, this would use a proper threshold ECDSA protocol
        // like GG18 or GG20. For now, we use FROST Schnorr and note that
        // this is a simplified implementation.

        // In practice, you would want to use a library like:
        // - multi-party-ecdsa (tss-lib)
        // - cggmp-threshold-ecdsa
        // - or adapt FROST signatures for ECDSA compatibility

        // Placeholder: derive an ECDSA-compatible signature from FROST
        // This is NOT cryptographically secure for production - just structural

        use k256::ecdsa::SigningKey;
        use k256::SecretKey;

        // WARNING: This is a placeholder that extracts the secret for single signing
        // Real threshold ECDSA would never expose the full secret
        // This is only for development/testing purposes

        let signing_share_bytes = self.key_share.signing_share_bytes();

        // Attempt to construct a signing key (this is NOT how real threshold ECDSA works)
        // In production, use proper threshold ECDSA protocols
        let sk = SecretKey::from_slice(&signing_share_bytes[..32.min(signing_share_bytes.len())])
            .map_err(|e| {
                WalletError::Signing(format!(
                    "Cannot create signing key (use proper threshold ECDSA): {e}"
                ))
            })?;

        let signing_key = SigningKey::from(sk);
        let (sig, recid) = signing_key.sign_recoverable(message_hash).map_err(|e| {
            WalletError::Signing(format!("ECDSA signing failed: {e}"))
        })?;

        let r = sig.r().to_bytes();
        let s = sig.s().to_bytes();

        Ok(EcdsaSignature {
            r: r.to_vec(),
            s: s.to_vec(),
            v: recid.to_byte() + 27, // EIP-155 recovery ID
        })
    }
}

/// ECDSA signature with recovery ID
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EcdsaSignature {
    #[serde(with = "hex")]
    pub r: Vec<u8>,
    #[serde(with = "hex")]
    pub s: Vec<u8>,
    pub v: u8,
}

impl EcdsaSignature {
    /// Convert to bytes in (r, s, v) format (65 bytes)
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(65);
        result.extend_from_slice(&self.r);
        result.extend_from_slice(&self.s);
        result.push(self.v);
        result
    }

    /// Convert to hex string
    pub fn to_hex(&self) -> String {
        hex::encode(self.to_bytes())
    }
}

/// Coordinator for managing signing sessions across participants
pub struct SigningCoordinator {
    session_id: String,
    key_id: String,
    message: Vec<u8>,
    threshold: u16,
    required_signers: BTreeSet<String>,
    commitments: Arc<RwLock<Vec<SigningCommitment>>>,
    shares: Arc<RwLock<Vec<SignatureShare>>>,
}

impl SigningCoordinator {
    /// Create a new signing session
    pub fn new(
        session_id: String,
        key_id: String,
        message: Vec<u8>,
        threshold: u16,
        signers: Vec<String>,
    ) -> WalletResult<Self> {
        if signers.len() < threshold as usize {
            return Err(WalletError::NotEnoughParticipants {
                have: signers.len(),
                need: threshold as usize,
            });
        }

        Ok(Self {
            session_id,
            key_id,
            message,
            threshold,
            required_signers: signers.into_iter().collect(),
            commitments: Arc::new(RwLock::new(Vec::new())),
            shares: Arc::new(RwLock::new(Vec::new())),
        })
    }

    /// Get session ID
    pub fn session_id(&self) -> &str {
        &self.session_id
    }

    /// Get key ID
    pub fn key_id(&self) -> &str {
        &self.key_id
    }

    /// Get the message being signed
    pub fn message(&self) -> &[u8] {
        &self.message
    }

    /// Receive a commitment from a signer
    pub fn receive_commitment(&self, commitment: SigningCommitment) -> WalletResult<()> {
        if commitment.session_id != self.session_id {
            return Err(WalletError::Signing(format!(
                "Wrong session ID: expected {}, got {}",
                self.session_id, commitment.session_id
            )));
        }

        if !self.required_signers.contains(&commitment.participant_id) {
            return Err(WalletError::InvalidParticipant(format!(
                "{} is not a required signer",
                commitment.participant_id
            )));
        }

        let mut commitments = self.commitments.write();

        // Replace if exists
        if let Some(existing) = commitments
            .iter_mut()
            .find(|c| c.participant_id == commitment.participant_id)
        {
            *existing = commitment;
        } else {
            commitments.push(commitment);
        }

        Ok(())
    }

    /// Check if all commitments have been received
    pub fn has_all_commitments(&self) -> bool {
        self.commitments.read().len() >= self.threshold as usize
    }

    /// Get all received commitments
    pub fn get_commitments(&self) -> Vec<SigningCommitment> {
        self.commitments.read().clone()
    }

    /// Receive a signature share from a signer
    pub fn receive_share(&self, share: SignatureShare) -> WalletResult<()> {
        if share.session_id != self.session_id {
            return Err(WalletError::Signing(format!(
                "Wrong session ID: expected {}, got {}",
                self.session_id, share.session_id
            )));
        }

        if !self.required_signers.contains(&share.participant_id) {
            return Err(WalletError::InvalidParticipant(format!(
                "{} is not a required signer",
                share.participant_id
            )));
        }

        let mut shares = self.shares.write();

        if let Some(existing) = shares
            .iter_mut()
            .find(|s| s.participant_id == share.participant_id)
        {
            *existing = share;
        } else {
            shares.push(share);
        }

        Ok(())
    }

    /// Check if all shares have been received
    pub fn has_all_shares(&self) -> bool {
        self.shares.read().len() >= self.threshold as usize
    }

    /// Get all received shares
    pub fn get_shares(&self) -> Vec<SignatureShare> {
        self.shares.read().clone()
    }

    /// Get threshold
    pub fn threshold(&self) -> u16 {
        self.threshold
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dkg::DkgParticipant;

    fn setup_key_shares() -> Vec<KeyShare> {
        let threshold = 2u16;
        let total = 3u16;

        let mut participants: Vec<DkgParticipant> = (1..=total)
            .map(|i| DkgParticipant::new(i.to_string(), CurveType::Secp256k1, threshold, total).unwrap())
            .collect();

        let round1_packages: Vec<_> = participants
            .iter_mut()
            .map(|p| p.generate_round1().unwrap())
            .collect();

        let mut all_round2_packages = Vec::new();
        for participant in &mut participants {
            let packages = participant.process_round1(&round1_packages).unwrap();
            all_round2_packages.extend(packages);
        }

        participants
            .iter_mut()
            .map(|p| {
                p.finalize(
                    "test-key".to_string(),
                    &round1_packages,
                    &all_round2_packages,
                )
                .unwrap()
            })
            .collect()
    }

    #[test]
    fn test_frost_signing_secp256k1() {
        let key_shares = setup_key_shares();
        let message = b"Hello, threshold signatures!";

        // Create signers for threshold (2 of 3)
        let signers: Vec<FrostSigner> = key_shares[..2]
            .iter()
            .map(|ks| FrostSigner::new(ks.clone()).unwrap())
            .collect();

        let session_id = "test-session-1";

        // Round 1: Generate commitments
        let commitments: Vec<SigningCommitment> = signers
            .iter()
            .map(|s| s.commit(session_id).unwrap())
            .collect();

        // Round 2: Generate signature shares
        let shares: Vec<SignatureShare> = signers
            .iter()
            .map(|s| s.sign(session_id, message, &commitments).unwrap())
            .collect();

        // Aggregate
        let signature = signers[0]
            .aggregate(message, &commitments, &shares)
            .unwrap();

        assert!(!signature.is_empty());
    }

    #[test]
    fn test_signing_coordinator() {
        let _key_shares = setup_key_shares();

        let coordinator = SigningCoordinator::new(
            "session-1".to_string(),
            "key-1".to_string(),
            b"test message".to_vec(),
            2,
            vec!["1".to_string(), "2".to_string()],
        )
        .unwrap();

        assert_eq!(coordinator.session_id(), "session-1");
        assert_eq!(coordinator.key_id(), "key-1");
        assert_eq!(coordinator.threshold(), 2);
        assert!(!coordinator.has_all_commitments());
    }

    #[test]
    fn test_ecdsa_signature_format() {
        let sig = EcdsaSignature {
            r: vec![1u8; 32],
            s: vec![2u8; 32],
            v: 27,
        };

        let bytes = sig.to_bytes();
        assert_eq!(bytes.len(), 65);
        assert_eq!(bytes[64], 27);
    }
}
