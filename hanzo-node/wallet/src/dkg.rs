//! Distributed Key Generation (DKG) using Pedersen's protocol
//!
//! This module implements the FROST DKG protocol for generating threshold
//! key shares without a trusted dealer. Each participant generates their
//! own secret share while the group collectively generates the public key.
//!
//! The protocol has two rounds:
//! 1. Round 1: Each participant generates commitments and sends to all others
//! 2. Round 2: Each participant generates secret shares for each other participant

use crate::error::{WalletError, WalletResult};
use crate::CurveType;

use frost_core::Identifier;
use parking_lot::RwLock;
use rand::rngs::OsRng;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::sync::Arc;
use zeroize::Zeroize;

/// Round 1 package containing commitments from a participant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DkgRound1Package {
    /// Participant identifier
    pub participant_id: String,
    /// Serialized commitment data (curve-specific)
    pub commitment: Vec<u8>,
    /// Proof of knowledge of the secret
    pub proof_of_knowledge: Vec<u8>,
}

/// Round 2 package containing secret shares for each participant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DkgRound2Package {
    /// Source participant identifier
    pub from_participant: String,
    /// Target participant identifier
    pub to_participant: String,
    /// Encrypted secret share
    pub encrypted_share: Vec<u8>,
}

/// A participant's key share after DKG completion
#[derive(Clone, Serialize, Deserialize)]
pub struct KeyShare {
    /// Unique identifier for this key
    pub key_id: String,
    /// Participant identifier
    pub participant_id: String,
    /// Curve type
    pub curve: CurveType,
    /// Threshold required for signing
    pub threshold: u16,
    /// Total number of participants
    pub total_participants: u16,
    /// Serialized signing share (secret - zeroize on drop)
    pub signing_share: Vec<u8>,
    /// Serialized verifying share (public)
    pub verifying_share: Vec<u8>,
    /// Serialized group verifying key (public key)
    pub group_public_key: Vec<u8>,
    /// All participant verifying shares for verification
    pub verifying_shares: BTreeMap<String, Vec<u8>>,
}

impl std::fmt::Debug for KeyShare {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("KeyShare")
            .field("key_id", &self.key_id)
            .field("participant_id", &self.participant_id)
            .field("curve", &self.curve)
            .field("threshold", &self.threshold)
            .field("total_participants", &self.total_participants)
            .field("signing_share", &"[REDACTED]")
            .field("verifying_share", &hex::encode(&self.verifying_share))
            .field("group_public_key", &hex::encode(&self.group_public_key))
            .finish()
    }
}

impl Drop for KeyShare {
    fn drop(&mut self) {
        self.signing_share.zeroize();
    }
}

impl KeyShare {
    /// Get the signing share bytes (for internal use only)
    pub(crate) fn signing_share_bytes(&self) -> &[u8] {
        &self.signing_share
    }

    /// Get the group public key as hex string
    pub fn group_public_key_hex(&self) -> String {
        hex::encode(&self.group_public_key)
    }
}

/// DKG state machine for a single participant
#[derive(Debug)]
enum DkgState {
    Initialized,
    Round1Complete { round1_secret: Vec<u8> },
    #[allow(dead_code)]
    Complete { key_share: KeyShare },
    #[allow(dead_code)]
    Failed { error: String },
}

/// A DKG participant that generates their own key share
pub struct DkgParticipant {
    participant_id: String,
    curve: CurveType,
    threshold: u16,
    total_participants: u16,
    state: DkgState,
}

impl DkgParticipant {
    /// Create a new DKG participant
    pub fn new(
        participant_id: String,
        curve: CurveType,
        threshold: u16,
        total_participants: u16,
    ) -> WalletResult<Self> {
        if threshold == 0 || threshold > total_participants {
            return Err(WalletError::InvalidThreshold {
                threshold,
                total: total_participants,
            });
        }

        Ok(Self {
            participant_id,
            curve,
            threshold,
            total_participants,
            state: DkgState::Initialized,
        })
    }

    /// Generate Round 1 package (commitments)
    pub fn generate_round1(&mut self) -> WalletResult<DkgRound1Package> {
        match self.curve {
            CurveType::Secp256k1 => self.generate_round1_secp256k1(),
            CurveType::Ed25519 => self.generate_round1_ed25519(),
        }
    }

    fn generate_round1_secp256k1(&mut self) -> WalletResult<DkgRound1Package> {
        use frost_secp256k1 as frost;

        let participant_id = self.parse_identifier_secp256k1()?;

        let (round1_secret, round1_package) = frost::keys::dkg::part1(
            participant_id,
            self.total_participants,
            self.threshold,
            &mut OsRng,
        )
        .map_err(|e| WalletError::Dkg(format!("Round 1 generation failed: {e}")))?;

        let commitment = bincode::serialize(&round1_package)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let secret_bytes = bincode::serialize(&round1_secret)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        self.state = DkgState::Round1Complete {
            round1_secret: secret_bytes,
        };

        Ok(DkgRound1Package {
            participant_id: self.participant_id.clone(),
            commitment,
            proof_of_knowledge: vec![], // Included in FROST package
        })
    }

    fn generate_round1_ed25519(&mut self) -> WalletResult<DkgRound1Package> {
        use frost_ed25519 as frost;

        let participant_id = self.parse_identifier_ed25519()?;

        let (round1_secret, round1_package) = frost::keys::dkg::part1(
            participant_id,
            self.total_participants,
            self.threshold,
            &mut OsRng,
        )
        .map_err(|e| WalletError::Dkg(format!("Round 1 generation failed: {e}")))?;

        let commitment = bincode::serialize(&round1_package)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let secret_bytes = bincode::serialize(&round1_secret)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        self.state = DkgState::Round1Complete {
            round1_secret: secret_bytes,
        };

        Ok(DkgRound1Package {
            participant_id: self.participant_id.clone(),
            commitment,
            proof_of_knowledge: vec![],
        })
    }

    /// Process Round 1 packages from all participants and generate Round 2 packages
    pub fn process_round1(
        &mut self,
        round1_packages: &[DkgRound1Package],
    ) -> WalletResult<Vec<DkgRound2Package>> {
        match self.curve {
            CurveType::Secp256k1 => self.process_round1_secp256k1(round1_packages),
            CurveType::Ed25519 => self.process_round1_ed25519(round1_packages),
        }
    }

    fn process_round1_secp256k1(
        &mut self,
        round1_packages: &[DkgRound1Package],
    ) -> WalletResult<Vec<DkgRound2Package>> {
        use frost_secp256k1 as frost;

        let round1_secret = match &self.state {
            DkgState::Round1Complete { round1_secret } => {
                bincode::deserialize::<frost::keys::dkg::round1::SecretPackage>(round1_secret)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?
            }
            _ => return Err(WalletError::Dkg("Invalid state for round 1 processing".into())),
        };

        let mut packages_map = BTreeMap::new();
        for pkg in round1_packages {
            if pkg.participant_id == self.participant_id {
                continue; // Skip our own package
            }
            let id = Self::parse_identifier_secp256k1_static(&pkg.participant_id)?;
            let round1_pkg: frost::keys::dkg::round1::Package =
                bincode::deserialize(&pkg.commitment)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            packages_map.insert(id, round1_pkg);
        }

        let (round2_secret, round2_packages) =
            frost::keys::dkg::part2(round1_secret, &packages_map)
                .map_err(|e| WalletError::Dkg(format!("Round 2 generation failed: {e}")))?;

        // Store round2 secret for finalization
        let secret_bytes = bincode::serialize(&round2_secret)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        self.state = DkgState::Round1Complete {
            round1_secret: secret_bytes,
        };

        let mut result = Vec::new();
        for (target_id, package) in round2_packages {
            let serialized = bincode::serialize(&package)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

            // Serialize Identifier to get participant id string
            let target_id_bytes = target_id.serialize();
            let target_id_str = Self::identifier_bytes_to_string(&target_id_bytes);
            result.push(DkgRound2Package {
                from_participant: self.participant_id.clone(),
                to_participant: target_id_str,
                encrypted_share: serialized,
            });
        }

        Ok(result)
    }

    fn process_round1_ed25519(
        &mut self,
        round1_packages: &[DkgRound1Package],
    ) -> WalletResult<Vec<DkgRound2Package>> {
        use frost_ed25519 as frost;

        let round1_secret = match &self.state {
            DkgState::Round1Complete { round1_secret } => {
                bincode::deserialize::<frost::keys::dkg::round1::SecretPackage>(round1_secret)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?
            }
            _ => return Err(WalletError::Dkg("Invalid state for round 1 processing".into())),
        };

        let mut packages_map = BTreeMap::new();
        for pkg in round1_packages {
            if pkg.participant_id == self.participant_id {
                continue;
            }
            let id = Self::parse_identifier_ed25519_static(&pkg.participant_id)?;
            let round1_pkg: frost::keys::dkg::round1::Package =
                bincode::deserialize(&pkg.commitment)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            packages_map.insert(id, round1_pkg);
        }

        let (round2_secret, round2_packages) =
            frost::keys::dkg::part2(round1_secret, &packages_map)
                .map_err(|e| WalletError::Dkg(format!("Round 2 generation failed: {e}")))?;

        let secret_bytes = bincode::serialize(&round2_secret)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        self.state = DkgState::Round1Complete {
            round1_secret: secret_bytes,
        };

        let mut result = Vec::new();
        for (target_id, package) in round2_packages {
            let serialized = bincode::serialize(&package)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

            let target_id_bytes = target_id.serialize();
            let target_id_str = Self::identifier_bytes_to_string(&target_id_bytes);
            result.push(DkgRound2Package {
                from_participant: self.participant_id.clone(),
                to_participant: target_id_str,
                encrypted_share: serialized,
            });
        }

        Ok(result)
    }

    /// Finalize DKG and produce the key share
    pub fn finalize(
        &mut self,
        key_id: String,
        round1_packages: &[DkgRound1Package],
        round2_packages: &[DkgRound2Package],
    ) -> WalletResult<KeyShare> {
        match self.curve {
            CurveType::Secp256k1 => {
                self.finalize_secp256k1(key_id, round1_packages, round2_packages)
            }
            CurveType::Ed25519 => self.finalize_ed25519(key_id, round1_packages, round2_packages),
        }
    }

    fn finalize_secp256k1(
        &mut self,
        key_id: String,
        round1_packages: &[DkgRound1Package],
        round2_packages: &[DkgRound2Package],
    ) -> WalletResult<KeyShare> {
        use frost_secp256k1 as frost;

        let round2_secret = match &self.state {
            DkgState::Round1Complete { round1_secret } => {
                bincode::deserialize::<frost::keys::dkg::round2::SecretPackage>(round1_secret)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?
            }
            _ => return Err(WalletError::Dkg("Invalid state for finalization".into())),
        };

        let mut round1_map = BTreeMap::new();
        for pkg in round1_packages {
            if pkg.participant_id == self.participant_id {
                continue;
            }
            let id = Self::parse_identifier_secp256k1_static(&pkg.participant_id)?;
            let round1_pkg: frost::keys::dkg::round1::Package =
                bincode::deserialize(&pkg.commitment)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            round1_map.insert(id, round1_pkg);
        }

        let mut round2_map = BTreeMap::new();
        for pkg in round2_packages {
            if pkg.to_participant != self.participant_id {
                continue; // Only packages addressed to us
            }
            let id = Self::parse_identifier_secp256k1_static(&pkg.from_participant)?;
            let round2_pkg: frost::keys::dkg::round2::Package =
                bincode::deserialize(&pkg.encrypted_share)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            round2_map.insert(id, round2_pkg);
        }

        let (key_package, pubkey_package) =
            frost::keys::dkg::part3(&round2_secret, &round1_map, &round2_map)
                .map_err(|e| WalletError::Dkg(format!("DKG finalization failed: {e}")))?;

        let signing_share = bincode::serialize(key_package.signing_share())
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let verifying_share = bincode::serialize(key_package.verifying_share())
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let group_public_key = bincode::serialize(pubkey_package.verifying_key())
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let mut verifying_shares = BTreeMap::new();
        for (id, share) in pubkey_package.verifying_shares() {
            let share_bytes = bincode::serialize(share)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;
            let id_bytes = id.serialize();
            let id_str = Self::identifier_bytes_to_string(&id_bytes);
            verifying_shares.insert(id_str, share_bytes);
        }

        let key_share = KeyShare {
            key_id,
            participant_id: self.participant_id.clone(),
            curve: self.curve,
            threshold: self.threshold,
            total_participants: self.total_participants,
            signing_share,
            verifying_share,
            group_public_key,
            verifying_shares,
        };

        self.state = DkgState::Complete {
            key_share: key_share.clone(),
        };

        Ok(key_share)
    }

    fn finalize_ed25519(
        &mut self,
        key_id: String,
        round1_packages: &[DkgRound1Package],
        round2_packages: &[DkgRound2Package],
    ) -> WalletResult<KeyShare> {
        use frost_ed25519 as frost;

        let round2_secret = match &self.state {
            DkgState::Round1Complete { round1_secret } => {
                bincode::deserialize::<frost::keys::dkg::round2::SecretPackage>(round1_secret)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?
            }
            _ => return Err(WalletError::Dkg("Invalid state for finalization".into())),
        };

        let mut round1_map = BTreeMap::new();
        for pkg in round1_packages {
            if pkg.participant_id == self.participant_id {
                continue;
            }
            let id = Self::parse_identifier_ed25519_static(&pkg.participant_id)?;
            let round1_pkg: frost::keys::dkg::round1::Package =
                bincode::deserialize(&pkg.commitment)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            round1_map.insert(id, round1_pkg);
        }

        let mut round2_map = BTreeMap::new();
        for pkg in round2_packages {
            if pkg.to_participant != self.participant_id {
                continue;
            }
            let id = Self::parse_identifier_ed25519_static(&pkg.from_participant)?;
            let round2_pkg: frost::keys::dkg::round2::Package =
                bincode::deserialize(&pkg.encrypted_share)
                    .map_err(|e| WalletError::Serialization(e.to_string()))?;
            round2_map.insert(id, round2_pkg);
        }

        let (key_package, pubkey_package) =
            frost::keys::dkg::part3(&round2_secret, &round1_map, &round2_map)
                .map_err(|e| WalletError::Dkg(format!("DKG finalization failed: {e}")))?;

        let signing_share = bincode::serialize(key_package.signing_share())
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let verifying_share = bincode::serialize(key_package.verifying_share())
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let group_public_key = bincode::serialize(pubkey_package.verifying_key())
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let mut verifying_shares = BTreeMap::new();
        for (id, share) in pubkey_package.verifying_shares() {
            let share_bytes = bincode::serialize(share)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;
            let id_bytes = id.serialize();
            let id_str = Self::identifier_bytes_to_string(&id_bytes);
            verifying_shares.insert(id_str, share_bytes);
        }

        let key_share = KeyShare {
            key_id,
            participant_id: self.participant_id.clone(),
            curve: self.curve,
            threshold: self.threshold,
            total_participants: self.total_participants,
            signing_share,
            verifying_share,
            group_public_key,
            verifying_shares,
        };

        self.state = DkgState::Complete {
            key_share: key_share.clone(),
        };

        Ok(key_share)
    }

    /// Convert identifier serialized bytes back to string representation
    ///
    /// FROST Identifier serializes differently per cipher suite:
    /// - secp256k1: 32-byte big-endian scalar
    /// - ed25519: 32-byte little-endian scalar
    ///
    /// For small values (1-65535), we extract the u16 value from the appropriate position.
    fn identifier_bytes_to_string(bytes: &[u8]) -> String {
        if bytes.is_empty() {
            return "0".to_string();
        }

        // For a 32-byte scalar, check if it's little-endian (ed25519) or big-endian (secp256k1)
        // We can detect by checking if the value bytes are at the start (little-endian) or end (big-endian)
        // For small values, non-zero bytes will be concentrated at one end

        // Count leading and trailing zeros
        let leading_zeros = bytes.iter().take_while(|&&b| b == 0).count();
        let trailing_zeros = bytes.iter().rev().take_while(|&&b| b == 0).count();

        // If more trailing zeros, it's big-endian (value at start, which is impossible for small values)
        // If more leading zeros, it's big-endian (value at end)
        // Actually for little-endian small values: [value_lo, value_hi, 0, 0, 0, ...]
        // For big-endian small values: [..., 0, 0, 0, value_hi, value_lo]

        if bytes.len() >= 2 {
            if leading_zeros >= bytes.len() - 2 {
                // Big-endian: value is at the end
                let len = bytes.len();
                let value = u16::from_be_bytes([bytes[len - 2], bytes[len - 1]]);
                return value.to_string();
            } else if trailing_zeros >= bytes.len() - 2 {
                // Little-endian: value is at the start
                let value = u16::from_le_bytes([bytes[0], bytes[1]]);
                return value.to_string();
            }
        }

        // For single byte or unknown format, use hex
        if bytes.len() == 1 {
            bytes[0].to_string()
        } else {
            hex::encode(bytes)
        }
    }

    fn parse_identifier_secp256k1(
        &self,
    ) -> WalletResult<Identifier<frost_secp256k1::Secp256K1Sha256>> {
        Self::parse_identifier_secp256k1_static(&self.participant_id)
    }

    fn parse_identifier_secp256k1_static(
        id: &str,
    ) -> WalletResult<Identifier<frost_secp256k1::Secp256K1Sha256>> {
        let num: u16 = id
            .parse()
            .map_err(|_| WalletError::InvalidParticipant(id.to_string()))?;
        Identifier::try_from(num)
            .map_err(|e| WalletError::InvalidParticipant(format!("{id}: {e}")))
    }

    fn parse_identifier_ed25519(&self) -> WalletResult<Identifier<frost_ed25519::Ed25519Sha512>> {
        Self::parse_identifier_ed25519_static(&self.participant_id)
    }

    fn parse_identifier_ed25519_static(
        id: &str,
    ) -> WalletResult<Identifier<frost_ed25519::Ed25519Sha512>> {
        let num: u16 = id
            .parse()
            .map_err(|_| WalletError::InvalidParticipant(id.to_string()))?;
        Identifier::try_from(num)
            .map_err(|e| WalletError::InvalidParticipant(format!("{id}: {e}")))
    }
}

/// Coordinator for managing DKG sessions across multiple participants
pub struct DkgCoordinator {
    curve: CurveType,
    threshold: u16,
    total_participants: u16,
    round1_packages: Arc<RwLock<Vec<DkgRound1Package>>>,
    round2_packages: Arc<RwLock<Vec<DkgRound2Package>>>,
}

impl DkgCoordinator {
    /// Create a new DKG coordinator
    pub fn new(curve: CurveType, threshold: u16, total_participants: u16) -> WalletResult<Self> {
        if threshold == 0 || threshold > total_participants {
            return Err(WalletError::InvalidThreshold {
                threshold,
                total: total_participants,
            });
        }

        Ok(Self {
            curve,
            threshold,
            total_participants,
            round1_packages: Arc::new(RwLock::new(Vec::new())),
            round2_packages: Arc::new(RwLock::new(Vec::new())),
        })
    }

    /// Receive a Round 1 package from a participant
    pub fn receive_round1(&self, package: DkgRound1Package) {
        let mut packages = self.round1_packages.write();
        // Replace if exists, otherwise add
        if let Some(existing) = packages
            .iter_mut()
            .find(|p| p.participant_id == package.participant_id)
        {
            *existing = package;
        } else {
            packages.push(package);
        }
    }

    /// Check if all Round 1 packages have been received
    pub fn is_round1_complete(&self) -> bool {
        self.round1_packages.read().len() == self.total_participants as usize
    }

    /// Get all Round 1 packages
    pub fn get_round1_packages(&self) -> Vec<DkgRound1Package> {
        self.round1_packages.read().clone()
    }

    /// Receive a Round 2 package
    pub fn receive_round2(&self, package: DkgRound2Package) {
        let mut packages = self.round2_packages.write();
        packages.push(package);
    }

    /// Check if all Round 2 packages have been received
    /// Each participant sends n-1 packages, so total is n*(n-1)
    pub fn is_round2_complete(&self) -> bool {
        let expected =
            self.total_participants as usize * (self.total_participants as usize - 1);
        self.round2_packages.read().len() == expected
    }

    /// Get all Round 2 packages
    pub fn get_round2_packages(&self) -> Vec<DkgRound2Package> {
        self.round2_packages.read().clone()
    }

    /// Get curve type
    pub fn curve(&self) -> CurveType {
        self.curve
    }

    /// Get threshold
    pub fn threshold(&self) -> u16 {
        self.threshold
    }

    /// Get total participants
    pub fn total_participants(&self) -> u16 {
        self.total_participants
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invalid_threshold() {
        let result = DkgParticipant::new("1".into(), CurveType::Secp256k1, 0, 3);
        assert!(result.is_err());

        let result = DkgParticipant::new("1".into(), CurveType::Secp256k1, 4, 3);
        assert!(result.is_err());
    }

    #[test]
    fn test_coordinator_creation() {
        let coordinator = DkgCoordinator::new(CurveType::Secp256k1, 2, 3);
        assert!(coordinator.is_ok());
        let coordinator = coordinator.unwrap();
        assert_eq!(coordinator.threshold(), 2);
        assert_eq!(coordinator.total_participants(), 3);
    }

    #[test]
    fn test_full_dkg_secp256k1() {
        // 2-of-3 threshold
        let threshold = 2;
        let total = 3;

        // Create participants
        let mut participants: Vec<DkgParticipant> = (1..=total)
            .map(|i| {
                DkgParticipant::new(i.to_string(), CurveType::Secp256k1, threshold, total)
                    .expect("Failed to create participant")
            })
            .collect();

        // Round 1: Generate commitments
        let round1_packages: Vec<DkgRound1Package> = participants
            .iter_mut()
            .map(|p| p.generate_round1().expect("Round 1 failed"))
            .collect();

        // Round 2: Generate shares
        let mut all_round2_packages = Vec::new();
        for participant in &mut participants {
            let packages = participant
                .process_round1(&round1_packages)
                .expect("Process round 1 failed");
            all_round2_packages.extend(packages);
        }

        // Finalize: Generate key shares
        let key_shares: Vec<KeyShare> = participants
            .iter_mut()
            .map(|p| {
                p.finalize(
                    "test-key-1".to_string(),
                    &round1_packages,
                    &all_round2_packages,
                )
                .expect("Finalization failed")
            })
            .collect();

        // Verify all participants have the same group public key
        let first_pk = &key_shares[0].group_public_key;
        for share in &key_shares {
            assert_eq!(
                &share.group_public_key, first_pk,
                "Group public keys should match"
            );
        }

        // Verify each share has unique verifying share
        let verifying_shares: Vec<&Vec<u8>> =
            key_shares.iter().map(|s| &s.verifying_share).collect();
        for i in 0..verifying_shares.len() {
            for j in (i + 1)..verifying_shares.len() {
                assert_ne!(
                    verifying_shares[i], verifying_shares[j],
                    "Verifying shares should be unique"
                );
            }
        }
    }

    #[test]
    fn test_full_dkg_ed25519() {
        let threshold = 2;
        let total = 3;

        let mut participants: Vec<DkgParticipant> = (1..=total)
            .map(|i| {
                DkgParticipant::new(i.to_string(), CurveType::Ed25519, threshold, total)
                    .expect("Failed to create participant")
            })
            .collect();

        let round1_packages: Vec<DkgRound1Package> = participants
            .iter_mut()
            .map(|p| p.generate_round1().expect("Round 1 failed"))
            .collect();

        let mut all_round2_packages = Vec::new();
        for participant in &mut participants {
            let packages = participant
                .process_round1(&round1_packages)
                .expect("Process round 1 failed");
            all_round2_packages.extend(packages);
        }

        let key_shares: Vec<KeyShare> = participants
            .iter_mut()
            .map(|p| {
                p.finalize(
                    "test-key-ed25519".to_string(),
                    &round1_packages,
                    &all_round2_packages,
                )
                .expect("Finalization failed")
            })
            .collect();

        let first_pk = &key_shares[0].group_public_key;
        for share in &key_shares {
            assert_eq!(&share.group_public_key, first_pk);
        }
    }
}
