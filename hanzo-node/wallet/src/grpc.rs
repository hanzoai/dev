//! gRPC Service Implementation for MPC Wallet
//!
//! Provides the network interface for distributed key generation,
//! threshold signing, and policy management.

use crate::dkg::{DkgCoordinator, DkgParticipant, DkgRound1Package, DkgRound2Package, KeyShare};
use crate::error::{WalletError, WalletResult};
use crate::policy::{ApprovalPolicy, PolicyEngine, PolicyResult, PolicyRule};
use crate::signing::{
    FrostSigner, SignatureShare, SigningCommitment, SigningCoordinator, SigningMetadata,
};
use crate::storage::{EncryptedKeyStore, KeyMetadata};
use crate::{CurveType, SignatureType};

use parking_lot::RwLock;
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use tonic::{Request, Response, Status};

// Include generated proto code
pub mod proto {
    tonic::include_proto!("hanzo.wallet.v1");
}

use proto::wallet_service_server::{WalletService as WalletServiceTrait, WalletServiceServer};
use proto::*;

/// Session state for DKG
struct DkgSession {
    coordinator: DkgCoordinator,
    #[allow(dead_code)]
    participant: Option<DkgParticipant>,
    round1_packages: Vec<DkgRound1Package>,
    round2_packages: Vec<DkgRound2Package>,
}

/// Session state for signing
struct SigningSession {
    coordinator: SigningCoordinator,
    commitments: Vec<SigningCommitment>,
    shares: Vec<SignatureShare>,
    key_share: KeyShare,
}

/// MPC Wallet gRPC Service
pub struct WalletService {
    /// Encrypted key storage
    store: Arc<EncryptedKeyStore>,
    /// Policy engine
    policy_engine: Arc<PolicyEngine>,
    /// Active DKG sessions
    dkg_sessions: Arc<RwLock<HashMap<String, DkgSession>>>,
    /// Active signing sessions
    signing_sessions: Arc<RwLock<HashMap<String, SigningSession>>>,
    /// Service version
    version: String,
}

impl WalletService {
    /// Create a new wallet service
    pub fn new<P: AsRef<Path>>(storage_path: P) -> WalletResult<Self> {
        let store = EncryptedKeyStore::open(storage_path)?;

        Ok(Self {
            store: Arc::new(store),
            policy_engine: Arc::new(PolicyEngine::new()),
            dkg_sessions: Arc::new(RwLock::new(HashMap::new())),
            signing_sessions: Arc::new(RwLock::new(HashMap::new())),
            version: env!("CARGO_PKG_VERSION").to_string(),
        })
    }

    /// Create a new wallet service with in-memory storage (for testing)
    pub fn new_in_memory() -> WalletResult<Self> {
        let store = EncryptedKeyStore::open_in_memory()?;

        Ok(Self {
            store: Arc::new(store),
            policy_engine: Arc::new(PolicyEngine::new()),
            dkg_sessions: Arc::new(RwLock::new(HashMap::new())),
            signing_sessions: Arc::new(RwLock::new(HashMap::new())),
            version: env!("CARGO_PKG_VERSION").to_string(),
        })
    }

    /// Create gRPC server
    pub fn into_server(self) -> WalletServiceServer<Self> {
        WalletServiceServer::new(self)
    }

    /// Get the policy engine
    pub fn policy_engine(&self) -> &PolicyEngine {
        &self.policy_engine
    }

    // Helper: Convert proto CurveType to internal CurveType
    fn proto_to_curve(curve: i32) -> WalletResult<CurveType> {
        match curve {
            1 => Ok(CurveType::Secp256k1),
            2 => Ok(CurveType::Ed25519),
            _ => Err(WalletError::InvalidMessage("Invalid curve type".into())),
        }
    }

    // Helper: Convert internal CurveType to proto
    fn curve_to_proto(curve: CurveType) -> i32 {
        match curve {
            CurveType::Secp256k1 => 1,
            CurveType::Ed25519 => 2,
        }
    }

    // Helper: Convert proto SignatureType to internal
    fn proto_to_sig_type(sig_type: i32) -> WalletResult<SignatureType> {
        match sig_type {
            1 => Ok(SignatureType::FrostSchnorr),
            2 => Ok(SignatureType::Ecdsa),
            3 => Ok(SignatureType::Ed25519),
            _ => Err(WalletError::InvalidMessage("Invalid signature type".into())),
        }
    }

    // Helper: Convert internal SignatureType to proto
    fn sig_type_to_proto(sig_type: SignatureType) -> i32 {
        match sig_type {
            SignatureType::FrostSchnorr => 1,
            SignatureType::Ecdsa => 2,
            SignatureType::Ed25519 => 3,
        }
    }

    // Helper: Convert KeyMetadata to proto
    fn metadata_to_proto(meta: &KeyMetadata) -> proto::KeyMetadata {
        proto::KeyMetadata {
            key_id: meta.key_id.clone(),
            curve: Self::curve_to_proto(meta.curve),
            threshold: meta.threshold as u32,
            total_participants: meta.total_participants as u32,
            group_public_key: meta.group_public_key.clone(),
            created_at: meta.created_at,
            last_used_at: meta.last_used_at.unwrap_or(0),
            description: meta.description.clone().unwrap_or_default(),
            labels: meta.labels.clone(),
        }
    }

    // Helper: Convert proto SigningMetadata to internal
    fn proto_to_signing_metadata(meta: Option<&proto::SigningMetadata>) -> SigningMetadata {
        meta.map(|m| SigningMetadata {
            value: if m.value > 0 { Some(m.value) } else { None },
            destination: if m.destination.is_empty() {
                None
            } else {
                Some(m.destination.clone())
            },
            chain_id: if m.chain_id > 0 {
                Some(m.chain_id)
            } else {
                None
            },
            tx_type: if m.tx_type.is_empty() {
                None
            } else {
                Some(m.tx_type.clone())
            },
        })
        .unwrap_or(SigningMetadata {
            value: None,
            destination: None,
            chain_id: None,
            tx_type: None,
        })
    }

    // Helper: Convert PolicyResult to proto
    fn policy_result_to_proto(result: &PolicyResult) -> proto::PolicyEvaluationResult {
        let (status, reason, required, received) = match result {
            PolicyResult::Approved => (
                policy_evaluation_result::Status::Approved as i32,
                String::new(),
                0,
                0,
            ),
            PolicyResult::RequiresApproval {
                reason,
                required_approvers,
            } => (
                policy_evaluation_result::Status::RequiresApproval as i32,
                reason.clone(),
                *required_approvers as u32,
                0,
            ),
            PolicyResult::Denied { reason } => (
                policy_evaluation_result::Status::Denied as i32,
                reason.clone(),
                0,
                0,
            ),
            PolicyResult::Pending { received, required } => (
                policy_evaluation_result::Status::Pending as i32,
                String::new(),
                *required as u32,
                *received as u32,
            ),
        };

        proto::PolicyEvaluationResult {
            status,
            reason,
            required_approvers: required,
            received_approvers: received,
        }
    }
}

#[tonic::async_trait]
impl WalletServiceTrait for WalletService {
    // ===================
    // DKG Operations
    // ===================

    async fn initiate_dkg(
        &self,
        request: Request<InitiateDkgRequest>,
    ) -> Result<Response<InitiateDkgResponse>, Status> {
        let req = request.into_inner();

        let curve = Self::proto_to_curve(req.curve)?;
        let threshold = req.threshold as u16;
        let total = req.total_participants as u16;

        let coordinator = DkgCoordinator::new(curve, threshold, total)?;

        let session = DkgSession {
            coordinator,
            participant: None,
            round1_packages: Vec::new(),
            round2_packages: Vec::new(),
        };

        self.dkg_sessions
            .write()
            .insert(req.session_id.clone(), session);

        Ok(Response::new(InitiateDkgResponse {
            success: true,
            session_id: req.session_id,
            message: "DKG session initiated".to_string(),
        }))
    }

    async fn submit_dkg_round1(
        &self,
        request: Request<DkgRound1Request>,
    ) -> Result<Response<DkgRound1Response>, Status> {
        let req = request.into_inner();

        let mut sessions = self.dkg_sessions.write();
        let session = sessions
            .get_mut(&req.session_id)
            .ok_or_else(|| Status::not_found("DKG session not found"))?;

        let package = DkgRound1Package {
            participant_id: req.participant_id,
            commitment: req.commitment,
            proof_of_knowledge: req.proof_of_knowledge,
        };

        session.coordinator.receive_round1(package.clone());
        session.round1_packages.push(package);

        Ok(Response::new(DkgRound1Response {
            success: true,
            message: "Round 1 package received".to_string(),
        }))
    }

    async fn get_dkg_round1_packages(
        &self,
        request: Request<GetDkgRound1Request>,
    ) -> Result<Response<GetDkgRound1Response>, Status> {
        let req = request.into_inner();

        let sessions = self.dkg_sessions.read();
        let session = sessions
            .get(&req.session_id)
            .ok_or_else(|| Status::not_found("DKG session not found"))?;

        let packages: Vec<proto::DkgRound1Package> = session
            .round1_packages
            .iter()
            .map(|p| proto::DkgRound1Package {
                participant_id: p.participant_id.clone(),
                commitment: p.commitment.clone(),
                proof_of_knowledge: p.proof_of_knowledge.clone(),
            })
            .collect();

        Ok(Response::new(GetDkgRound1Response {
            packages,
            all_received: session.coordinator.is_round1_complete(),
        }))
    }

    async fn submit_dkg_round2(
        &self,
        request: Request<DkgRound2Request>,
    ) -> Result<Response<DkgRound2Response>, Status> {
        let req = request.into_inner();

        let mut sessions = self.dkg_sessions.write();
        let session = sessions
            .get_mut(&req.session_id)
            .ok_or_else(|| Status::not_found("DKG session not found"))?;

        let package = DkgRound2Package {
            from_participant: req.from_participant,
            to_participant: req.to_participant,
            encrypted_share: req.encrypted_share,
        };

        session.coordinator.receive_round2(package.clone());
        session.round2_packages.push(package);

        Ok(Response::new(DkgRound2Response {
            success: true,
            message: "Round 2 package received".to_string(),
        }))
    }

    async fn get_dkg_round2_packages(
        &self,
        request: Request<GetDkgRound2Request>,
    ) -> Result<Response<GetDkgRound2Response>, Status> {
        let req = request.into_inner();

        let sessions = self.dkg_sessions.read();
        let session = sessions
            .get(&req.session_id)
            .ok_or_else(|| Status::not_found("DKG session not found"))?;

        let packages: Vec<proto::DkgRound2Package> = session
            .round2_packages
            .iter()
            .filter(|p| p.to_participant == req.participant_id)
            .map(|p| proto::DkgRound2Package {
                from_participant: p.from_participant.clone(),
                to_participant: p.to_participant.clone(),
                encrypted_share: p.encrypted_share.clone(),
            })
            .collect();

        Ok(Response::new(GetDkgRound2Response {
            packages,
            all_received: session.coordinator.is_round2_complete(),
        }))
    }

    async fn finalize_dkg(
        &self,
        request: Request<FinalizeDkgRequest>,
    ) -> Result<Response<FinalizeDkgResponse>, Status> {
        let req = request.into_inner();

        let sessions = self.dkg_sessions.read();
        let session = sessions
            .get(&req.session_id)
            .ok_or_else(|| Status::not_found("DKG session not found"))?;

        // For now, return success with metadata
        // In production, this would finalize the DKG for the participant
        let description = if req.description.is_empty() {
            None
        } else {
            Some(req.description)
        };

        // Create metadata from coordinator info
        let meta = proto::KeyMetadata {
            key_id: req.key_id.clone(),
            curve: Self::curve_to_proto(session.coordinator.curve()),
            threshold: session.coordinator.threshold() as u32,
            total_participants: session.coordinator.total_participants() as u32,
            group_public_key: String::new(), // Would be set after finalization
            created_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0),
            last_used_at: 0,
            description: description.unwrap_or_default(),
            labels: HashMap::new(),
        };

        Ok(Response::new(FinalizeDkgResponse {
            success: true,
            message: "DKG finalized".to_string(),
            key_metadata: Some(meta),
        }))
    }

    // ===================
    // Key Management
    // ===================

    async fn list_keys(
        &self,
        request: Request<ListKeysRequest>,
    ) -> Result<Response<ListKeysResponse>, Status> {
        let req = request.into_inner();

        let keys = if req.curve_filter != 0 {
            let curve = Self::proto_to_curve(req.curve_filter)?;
            self.store.list_by_curve(curve)
        } else {
            self.store.list()
        };

        let proto_keys: Vec<proto::KeyMetadata> =
            keys.iter().map(Self::metadata_to_proto).collect();

        Ok(Response::new(ListKeysResponse { keys: proto_keys }))
    }

    async fn get_key(
        &self,
        request: Request<GetKeyRequest>,
    ) -> Result<Response<GetKeyResponse>, Status> {
        let req = request.into_inner();

        let meta = self
            .store
            .get_metadata(&req.key_id)
            .ok_or_else(|| Status::not_found("Key not found"))?;

        Ok(Response::new(GetKeyResponse {
            key_metadata: Some(Self::metadata_to_proto(&meta)),
        }))
    }

    async fn delete_key(
        &self,
        request: Request<DeleteKeyRequest>,
    ) -> Result<Response<DeleteKeyResponse>, Status> {
        let req = request.into_inner();

        self.store.delete(&req.key_id)?;

        Ok(Response::new(DeleteKeyResponse {
            success: true,
            message: "Key deleted".to_string(),
        }))
    }

    async fn export_key(
        &self,
        request: Request<ExportKeyRequest>,
    ) -> Result<Response<ExportKeyResponse>, Status> {
        let req = request.into_inner();

        let encrypted_data = self.store.export(&req.key_id)?;

        // Compute checksum
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(&encrypted_data);
        let checksum = hex::encode(hasher.finalize());

        Ok(Response::new(ExportKeyResponse {
            encrypted_data,
            checksum,
        }))
    }

    async fn import_key(
        &self,
        request: Request<ImportKeyRequest>,
    ) -> Result<Response<ImportKeyResponse>, Status> {
        let req = request.into_inner();

        self.store
            .import(&req.key_id, &req.encrypted_data, &req.password)?;

        let meta = self
            .store
            .get_metadata(&req.key_id)
            .ok_or_else(|| Status::internal("Failed to get imported key metadata"))?;

        Ok(Response::new(ImportKeyResponse {
            success: true,
            message: "Key imported".to_string(),
            key_metadata: Some(Self::metadata_to_proto(&meta)),
        }))
    }

    // ===================
    // Signing Operations
    // ===================

    async fn initiate_signing(
        &self,
        request: Request<InitiateSigningRequest>,
    ) -> Result<Response<InitiateSigningResponse>, Status> {
        let req = request.into_inner();

        // Evaluate policy
        let metadata = Self::proto_to_signing_metadata(req.metadata.as_ref());
        let policy_result = self
            .policy_engine
            .evaluate(&metadata)
            .map_err(|e| Status::internal(e.to_string()))?;

        // If denied, return early
        if policy_result.is_denied() {
            return Ok(Response::new(InitiateSigningResponse {
                success: false,
                session_id: req.session_id,
                message: "Policy denied".to_string(),
                policy_result: Some(Self::policy_result_to_proto(&policy_result)),
            }));
        }

        // Get key metadata to determine threshold
        let key_meta = self
            .store
            .get_metadata(&req.key_id)
            .ok_or_else(|| Status::not_found("Key not found"))?;

        // Note: In production, you'd load the actual key share here
        // For now, create coordinator
        let coordinator = SigningCoordinator::new(
            req.session_id.clone(),
            req.key_id.clone(),
            req.message.clone(),
            key_meta.threshold,
            req.signers.clone(),
        )?;

        // For demonstration, create a placeholder KeyShare
        // In production, this would be loaded from storage with password
        let placeholder_key_share = KeyShare {
            key_id: req.key_id.clone(),
            participant_id: "1".to_string(),
            curve: key_meta.curve,
            threshold: key_meta.threshold,
            total_participants: key_meta.total_participants,
            signing_share: vec![],
            verifying_share: vec![],
            group_public_key: hex::decode(&key_meta.group_public_key).unwrap_or_default(),
            verifying_shares: std::collections::BTreeMap::new(),
        };

        let session = SigningSession {
            coordinator,
            commitments: Vec::new(),
            shares: Vec::new(),
            key_share: placeholder_key_share,
        };

        self.signing_sessions
            .write()
            .insert(req.session_id.clone(), session);

        Ok(Response::new(InitiateSigningResponse {
            success: true,
            session_id: req.session_id,
            message: "Signing session initiated".to_string(),
            policy_result: Some(Self::policy_result_to_proto(&policy_result)),
        }))
    }

    async fn submit_signing_commitment(
        &self,
        request: Request<SigningCommitmentRequest>,
    ) -> Result<Response<SigningCommitmentResponse>, Status> {
        let req = request.into_inner();

        let mut sessions = self.signing_sessions.write();
        let session = sessions
            .get_mut(&req.session_id)
            .ok_or_else(|| Status::not_found("Signing session not found"))?;

        let commitment = SigningCommitment {
            participant_id: req.participant_id,
            session_id: req.session_id,
            commitment: req.commitment,
        };

        session.coordinator.receive_commitment(commitment.clone())?;
        session.commitments.push(commitment);

        Ok(Response::new(SigningCommitmentResponse {
            success: true,
            message: "Commitment received".to_string(),
        }))
    }

    async fn get_signing_commitments(
        &self,
        request: Request<GetCommitmentsRequest>,
    ) -> Result<Response<GetCommitmentsResponse>, Status> {
        let req = request.into_inner();

        let sessions = self.signing_sessions.read();
        let session = sessions
            .get(&req.session_id)
            .ok_or_else(|| Status::not_found("Signing session not found"))?;

        let commitments: Vec<SigningCommitmentData> = session
            .commitments
            .iter()
            .map(|c| SigningCommitmentData {
                participant_id: c.participant_id.clone(),
                commitment: c.commitment.clone(),
            })
            .collect();

        Ok(Response::new(GetCommitmentsResponse {
            commitments,
            all_received: session.coordinator.has_all_commitments(),
        }))
    }

    async fn submit_signature_share(
        &self,
        request: Request<SignatureShareRequest>,
    ) -> Result<Response<SignatureShareResponse>, Status> {
        let req = request.into_inner();

        let mut sessions = self.signing_sessions.write();
        let session = sessions
            .get_mut(&req.session_id)
            .ok_or_else(|| Status::not_found("Signing session not found"))?;

        let share = SignatureShare {
            participant_id: req.participant_id,
            session_id: req.session_id,
            share: req.share,
        };

        session.coordinator.receive_share(share.clone())?;
        session.shares.push(share);

        Ok(Response::new(SignatureShareResponse {
            success: true,
            message: "Signature share received".to_string(),
        }))
    }

    async fn get_signature_shares(
        &self,
        request: Request<GetSharesRequest>,
    ) -> Result<Response<GetSharesResponse>, Status> {
        let req = request.into_inner();

        let sessions = self.signing_sessions.read();
        let session = sessions
            .get(&req.session_id)
            .ok_or_else(|| Status::not_found("Signing session not found"))?;

        let shares: Vec<SignatureShareData> = session
            .shares
            .iter()
            .map(|s| SignatureShareData {
                participant_id: s.participant_id.clone(),
                share: s.share.clone(),
            })
            .collect();

        Ok(Response::new(GetSharesResponse {
            shares,
            all_received: session.coordinator.has_all_shares(),
        }))
    }

    async fn finalize_signing(
        &self,
        request: Request<FinalizeSigningRequest>,
    ) -> Result<Response<FinalizeSigningResponse>, Status> {
        let req = request.into_inner();

        let sessions = self.signing_sessions.read();
        let session = sessions
            .get(&req.session_id)
            .ok_or_else(|| Status::not_found("Signing session not found"))?;

        if !session.coordinator.has_all_shares() {
            return Err(Status::failed_precondition("Not all shares received"));
        }

        // Aggregate signatures
        let signer = FrostSigner::new(session.key_share.clone())?;
        let signature = signer.aggregate(
            session.coordinator.message(),
            &session.commitments,
            &session.shares,
        )?;

        Ok(Response::new(FinalizeSigningResponse {
            success: true,
            message: "Signature aggregated".to_string(),
            signature,
            signature_type: Self::sig_type_to_proto(SignatureType::FrostSchnorr),
            recovery_id: 0,
        }))
    }

    async fn sign(
        &self,
        request: Request<SignRequest>,
    ) -> Result<Response<SignResponse>, Status> {
        let req = request.into_inner();

        // Load key share
        let key_share = self.store.load(&req.key_id, &req.password)?;

        // Evaluate policy
        let metadata = Self::proto_to_signing_metadata(req.metadata.as_ref());
        let policy_result = self
            .policy_engine
            .evaluate(&metadata)
            .map_err(|e| Status::internal(e.to_string()))?;

        if policy_result.is_denied() {
            if let PolicyResult::Denied { reason } = policy_result {
                return Ok(Response::new(SignResponse {
                    success: false,
                    error_message: reason,
                    signature: vec![],
                    signature_type: 0,
                    recovery_id: 0,
                }));
            }
        }

        // For single-signer scenarios (or coordinator with local share)
        let sig_type = Self::proto_to_sig_type(req.signature_type)?;

        let (signature, recovery_id) = match sig_type {
            SignatureType::Ecdsa => {
                use crate::signing::EcdsaSigner;

                let signer = EcdsaSigner::new(key_share)?;
                let message_array: [u8; 32] = req
                    .message
                    .try_into()
                    .map_err(|_| Status::invalid_argument("Message must be 32 bytes"))?;

                let ecdsa_sig = signer.sign_hash(&message_array)?;
                (ecdsa_sig.to_bytes(), Some(ecdsa_sig.v as u32))
            }
            SignatureType::FrostSchnorr | SignatureType::Ed25519 => {
                // For FROST, need threshold signers - return placeholder
                return Err(Status::unimplemented(
                    "FROST signing requires multiple participants. Use signing session APIs.",
                ));
            }
        };

        // Record transaction for policy tracking
        self.policy_engine.record_transaction(&metadata);

        Ok(Response::new(SignResponse {
            success: true,
            error_message: String::new(),
            signature,
            signature_type: Self::sig_type_to_proto(sig_type),
            recovery_id: recovery_id.unwrap_or(0),
        }))
    }

    // ===================
    // Policy Management
    // ===================

    async fn set_policy(
        &self,
        request: Request<SetPolicyRequest>,
    ) -> Result<Response<SetPolicyResponse>, Status> {
        let req = request.into_inner();

        let proto_policy = req
            .policy
            .ok_or_else(|| Status::invalid_argument("Policy required"))?;

        let rules: Vec<PolicyRule> = proto_policy
            .rules
            .into_iter()
            .filter_map(|r| r.rule)
            .filter_map(|rule| match rule {
                policy_rule::Rule::MaxAutoApprove(r) => {
                    Some(PolicyRule::MaxAutoApproveValue { value: r.value })
                }
                policy_rule::Rule::RequireApprovers(r) => Some(PolicyRule::RequireApprovers {
                    count: r.count as usize,
                    for_value_above: if r.for_value_above > 0 {
                        Some(r.for_value_above)
                    } else {
                        None
                    },
                }),
                policy_rule::Rule::Whitelist(r) => {
                    Some(PolicyRule::WhitelistOnly { addresses: r.addresses })
                }
                policy_rule::Rule::Blacklist(r) => {
                    Some(PolicyRule::Blacklist { addresses: r.addresses })
                }
                policy_rule::Rule::RateLimit(r) => Some(PolicyRule::RateLimit {
                    max_count: r.max_count,
                    window_seconds: r.window_seconds,
                }),
                policy_rule::Rule::DailyLimit(r) => {
                    Some(PolicyRule::DailySpendingLimit { max_value: r.max_value })
                }
                policy_rule::Rule::AllowedChains(r) => {
                    Some(PolicyRule::AllowedChains { chain_ids: r.chain_ids })
                }
                policy_rule::Rule::TimeRestriction(r) => Some(PolicyRule::TimeRestriction {
                    allowed_hours_start: r.allowed_hours_start as u8,
                    allowed_hours_end: r.allowed_hours_end as u8,
                    timezone_offset_hours: r.timezone_offset_hours as i8,
                }),
            })
            .collect();

        let policy = ApprovalPolicy {
            name: proto_policy.name,
            description: if proto_policy.description.is_empty() {
                None
            } else {
                Some(proto_policy.description)
            },
            rules,
            enabled: proto_policy.enabled,
            priority: proto_policy.priority,
        };

        self.policy_engine.add_policy(policy);

        Ok(Response::new(SetPolicyResponse {
            success: true,
            message: "Policy set".to_string(),
        }))
    }

    async fn get_policy(
        &self,
        request: Request<GetPolicyRequest>,
    ) -> Result<Response<GetPolicyResponse>, Status> {
        let req = request.into_inner();

        let policies = self.policy_engine.get_policies();
        let policy = policies
            .iter()
            .find(|p| p.name == req.name)
            .ok_or_else(|| Status::not_found("Policy not found"))?;

        // Convert to proto (simplified - would need full conversion in production)
        let proto_policy = proto::Policy {
            name: policy.name.clone(),
            description: policy.description.clone().unwrap_or_default(),
            rules: vec![], // Would need conversion
            enabled: policy.enabled,
            priority: policy.priority,
        };

        Ok(Response::new(GetPolicyResponse {
            policy: Some(proto_policy),
        }))
    }

    async fn list_policies(
        &self,
        _request: Request<ListPoliciesRequest>,
    ) -> Result<Response<ListPoliciesResponse>, Status> {
        let policies = self.policy_engine.get_policies();

        let proto_policies: Vec<proto::Policy> = policies
            .iter()
            .map(|p| proto::Policy {
                name: p.name.clone(),
                description: p.description.clone().unwrap_or_default(),
                rules: vec![],
                enabled: p.enabled,
                priority: p.priority,
            })
            .collect();

        Ok(Response::new(ListPoliciesResponse {
            policies: proto_policies,
        }))
    }

    async fn delete_policy(
        &self,
        request: Request<DeletePolicyRequest>,
    ) -> Result<Response<DeletePolicyResponse>, Status> {
        let req = request.into_inner();

        self.policy_engine.remove_policy(&req.name);

        Ok(Response::new(DeletePolicyResponse {
            success: true,
            message: "Policy deleted".to_string(),
        }))
    }

    async fn evaluate_policy(
        &self,
        request: Request<EvaluatePolicyRequest>,
    ) -> Result<Response<EvaluatePolicyResponse>, Status> {
        let req = request.into_inner();

        let metadata = Self::proto_to_signing_metadata(req.metadata.as_ref());
        let result = self
            .policy_engine
            .evaluate(&metadata)
            .map_err(|e| Status::internal(e.to_string()))?;

        Ok(Response::new(EvaluatePolicyResponse {
            result: Some(Self::policy_result_to_proto(&result)),
        }))
    }

    async fn submit_approval(
        &self,
        request: Request<SubmitApprovalRequest>,
    ) -> Result<Response<SubmitApprovalResponse>, Status> {
        let req = request.into_inner();

        // In production, verify approver's signature
        let result = self.policy_engine.submit_approval(&req.session_id, &req.approver_id, 2);

        Ok(Response::new(SubmitApprovalResponse {
            success: true,
            message: "Approval submitted".to_string(),
            result: Some(Self::policy_result_to_proto(&result)),
        }))
    }

    // ===================
    // Health & Status
    // ===================

    async fn health(
        &self,
        _request: Request<HealthRequest>,
    ) -> Result<Response<HealthResponse>, Status> {
        Ok(Response::new(HealthResponse {
            healthy: true,
            version: self.version.clone(),
        }))
    }

    async fn get_status(
        &self,
        _request: Request<GetStatusRequest>,
    ) -> Result<Response<GetStatusResponse>, Status> {
        let keys = self.store.list();
        let dkg_sessions = self.dkg_sessions.read().len();
        let signing_sessions = self.signing_sessions.read().len();

        let mut capabilities = HashMap::new();
        capabilities.insert("frost_secp256k1".to_string(), "enabled".to_string());
        capabilities.insert("frost_ed25519".to_string(), "enabled".to_string());
        capabilities.insert("ecdsa".to_string(), "enabled".to_string());

        Ok(Response::new(GetStatusResponse {
            ready: true,
            total_keys: keys.len() as u32,
            active_sessions: (dkg_sessions + signing_sessions) as u32,
            pending_approvals: 0, // Would track in policy engine
            capabilities,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_health_check() {
        let service = WalletService::new_in_memory().unwrap();
        let request = Request::new(HealthRequest {});

        let response = service.health(request).await.unwrap();
        let inner = response.into_inner();

        assert!(inner.healthy);
        assert!(!inner.version.is_empty());
    }

    #[tokio::test]
    async fn test_get_status() {
        let service = WalletService::new_in_memory().unwrap();
        let request = Request::new(GetStatusRequest {});

        let response = service.get_status(request).await.unwrap();
        let inner = response.into_inner();

        assert!(inner.ready);
        assert!(inner.capabilities.contains_key("frost_secp256k1"));
    }

    #[tokio::test]
    async fn test_list_empty_keys() {
        let service = WalletService::new_in_memory().unwrap();
        let request = Request::new(ListKeysRequest { curve_filter: 0 });

        let response = service.list_keys(request).await.unwrap();
        let inner = response.into_inner();

        assert!(inner.keys.is_empty());
    }

    #[tokio::test]
    async fn test_initiate_dkg() {
        let service = WalletService::new_in_memory().unwrap();

        let request = Request::new(InitiateDkgRequest {
            session_id: "test-session".to_string(),
            curve: 1, // secp256k1
            threshold: 2,
            total_participants: 3,
            participant_ids: vec!["1".to_string(), "2".to_string(), "3".to_string()],
        });

        let response = service.initiate_dkg(request).await.unwrap();
        let inner = response.into_inner();

        assert!(inner.success);
        assert_eq!(inner.session_id, "test-session");
    }
}
