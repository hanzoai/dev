//! Pairing protocol implementation

use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use serde::{Deserialize, Serialize};
use x25519_dalek::PublicKey as X25519PublicKey;

use crate::{
    crypto::{
        decrypt, derive_shared_secret, encrypt, generate_challenge, x25519_key_exchange,
        SharedSecret,
    },
    error::PairingError,
    qr::{EphemeralKeyPair, QRPairingPayload},
};

/// Device type for pairing
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum DeviceType {
    Desktop,
    Mobile,
    IoT,
    Server,
    Embedded,
    Unknown,
}

/// Device information sent during pairing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceInfo {
    /// Human-readable device name
    pub name: String,

    /// Device type
    pub device_type: DeviceType,

    /// Device capabilities
    #[serde(default)]
    pub capabilities: Vec<String>,

    /// Device model/manufacturer (optional)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,

    /// Operating system (optional)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub os: Option<String>,
}

/// Pairing request from new device to controller
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PairingRequest {
    /// Device's ephemeral X25519 public key (base64url)
    pub device_ephemeral_pk: String,

    /// Device's long-term signing public key (base64url)
    pub device_signing_pk: String,

    /// Echo of nonce from QR code (base64url)
    pub nonce: String,

    /// Device information
    pub device_info: DeviceInfo,
}

impl PairingRequest {
    /// Create a new pairing request
    pub fn new(
        device_ephemeral: &EphemeralKeyPair,
        device_signing_pk: &VerifyingKey,
        qr_nonce: &[u8; 16],
        device_info: DeviceInfo,
    ) -> Self {
        Self {
            device_ephemeral_pk: URL_SAFE_NO_PAD.encode(device_ephemeral.public.as_bytes()),
            device_signing_pk: URL_SAFE_NO_PAD.encode(device_signing_pk.as_bytes()),
            nonce: URL_SAFE_NO_PAD.encode(qr_nonce),
            device_info,
        }
    }

    /// Get the device's ephemeral public key
    pub fn get_ephemeral_pk(&self) -> Result<X25519PublicKey, PairingError> {
        let bytes = URL_SAFE_NO_PAD
            .decode(&self.device_ephemeral_pk)
            .map_err(|_| PairingError::InvalidRequest)?;

        let arr: [u8; 32] = bytes
            .try_into()
            .map_err(|_| PairingError::InvalidRequest)?;

        Ok(X25519PublicKey::from(arr))
    }

    /// Get the device's signing public key
    pub fn get_signing_pk(&self) -> Result<VerifyingKey, PairingError> {
        let bytes = URL_SAFE_NO_PAD
            .decode(&self.device_signing_pk)
            .map_err(|_| PairingError::InvalidRequest)?;

        let arr: [u8; 32] = bytes
            .try_into()
            .map_err(|_| PairingError::InvalidRequest)?;

        VerifyingKey::from_bytes(&arr).map_err(|_| PairingError::InvalidRequest)
    }
}

/// Challenge from controller to device
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PairingChallenge {
    /// Encrypted challenge data
    pub encrypted_challenge: String,

    /// AEAD nonce (base64url)
    pub nonce: String,
}

impl PairingChallenge {
    /// Create a new challenge
    pub fn new(shared_secret: &SharedSecret) -> Result<(Self, [u8; 32]), PairingError> {
        let challenge = generate_challenge();
        let key = shared_secret.derive_encryption_key(b"pairing-challenge");

        let (ciphertext, nonce) =
            encrypt(&key, &challenge, None).map_err(|e| PairingError::EncryptionError(e.to_string()))?;

        Ok((
            Self {
                encrypted_challenge: URL_SAFE_NO_PAD.encode(&ciphertext),
                nonce: URL_SAFE_NO_PAD.encode(nonce),
            },
            challenge,
        ))
    }

    /// Decrypt the challenge
    pub fn decrypt(&self, shared_secret: &SharedSecret) -> Result<[u8; 32], PairingError> {
        let key = shared_secret.derive_encryption_key(b"pairing-challenge");

        let ciphertext = URL_SAFE_NO_PAD
            .decode(&self.encrypted_challenge)
            .map_err(|e| PairingError::DecryptionError(e.to_string()))?;

        let nonce_bytes = URL_SAFE_NO_PAD
            .decode(&self.nonce)
            .map_err(|e| PairingError::DecryptionError(e.to_string()))?;

        let nonce: [u8; 12] = nonce_bytes
            .try_into()
            .map_err(|_| PairingError::DecryptionError("Invalid nonce".to_string()))?;

        let plaintext =
            decrypt(&key, &ciphertext, &nonce, None).map_err(|e| PairingError::DecryptionError(e.to_string()))?;

        plaintext
            .try_into()
            .map_err(|_| PairingError::DecryptionError("Invalid challenge length".to_string()))
    }
}

/// Response from device to controller
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PairingResponse {
    /// Signed challenge (base64url)
    pub signed_challenge: String,

    /// Signature over the entire pairing transcript (base64url)
    pub transcript_signature: String,
}

impl PairingResponse {
    /// Create a new pairing response
    pub fn new(
        challenge: &[u8; 32],
        transcript_hash: &[u8; 32],
        signing_key: &SigningKey,
    ) -> Self {
        let challenge_sig = signing_key.sign(challenge);
        let transcript_sig = signing_key.sign(transcript_hash);

        Self {
            signed_challenge: URL_SAFE_NO_PAD.encode(challenge_sig.to_bytes()),
            transcript_signature: URL_SAFE_NO_PAD.encode(transcript_sig.to_bytes()),
        }
    }

    /// Verify the response
    pub fn verify(
        &self,
        expected_challenge: &[u8; 32],
        transcript_hash: &[u8; 32],
        device_verifying_key: &VerifyingKey,
    ) -> Result<bool, PairingError> {
        // Verify challenge signature
        let challenge_sig_bytes = URL_SAFE_NO_PAD
            .decode(&self.signed_challenge)
            .map_err(|_| PairingError::ChallengeVerificationFailed)?;

        let challenge_sig: Signature = challenge_sig_bytes
            .as_slice()
            .try_into()
            .map_err(|_| PairingError::ChallengeVerificationFailed)?;

        if device_verifying_key
            .verify(expected_challenge, &challenge_sig)
            .is_err()
        {
            return Ok(false);
        }

        // Verify transcript signature
        let transcript_sig_bytes = URL_SAFE_NO_PAD
            .decode(&self.transcript_signature)
            .map_err(|_| PairingError::ChallengeVerificationFailed)?;

        let transcript_sig: Signature = transcript_sig_bytes
            .as_slice()
            .try_into()
            .map_err(|_| PairingError::ChallengeVerificationFailed)?;

        Ok(device_verifying_key
            .verify(transcript_hash, &transcript_sig)
            .is_ok())
    }
}

/// Pairing confirmation from controller to device
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PairingConfirm {
    /// Encrypted device credentials
    pub encrypted_credentials: String,

    /// AEAD nonce (base64url)
    pub nonce: String,

    /// Device's assigned DID
    pub device_did: String,
}

/// Device credentials issued after successful pairing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceCredentials {
    /// Device DID
    pub device_did: String,

    /// Root identity DID
    pub root_did: String,

    /// Relay addresses for network bootstrap
    pub relay_addrs: Vec<String>,

    /// DHT bootstrap peers
    pub bootstrap_peers: Vec<String>,

    /// Registry contract address
    pub registry_address: Option<String>,

    /// RPC endpoints
    pub rpc_endpoints: Vec<String>,

    /// Device capabilities
    pub capabilities: DeviceCapabilities,
}

/// Capabilities granted to a device
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DeviceCapabilities {
    /// Can sign messages on behalf of user
    #[serde(default)]
    pub can_sign: bool,

    /// Can access all inboxes
    #[serde(default)]
    pub full_inbox_access: bool,

    /// Can modify settings
    #[serde(default)]
    pub can_modify_settings: bool,

    /// Can add new devices
    #[serde(default)]
    pub can_add_devices: bool,

    /// Specific tools the device can use
    #[serde(default)]
    pub allowed_tools: Vec<String>,
}

impl PairingConfirm {
    /// Create a new pairing confirmation
    pub fn new(
        credentials: &DeviceCredentials,
        shared_secret: &SharedSecret,
    ) -> Result<Self, PairingError> {
        let key = shared_secret.derive_encryption_key(b"pairing-confirm");
        let credentials_json =
            serde_json::to_vec(credentials).map_err(|e| PairingError::Failed(e.to_string()))?;

        let (ciphertext, nonce) = encrypt(&key, &credentials_json, None)
            .map_err(|e| PairingError::EncryptionError(e.to_string()))?;

        Ok(Self {
            encrypted_credentials: URL_SAFE_NO_PAD.encode(&ciphertext),
            nonce: URL_SAFE_NO_PAD.encode(nonce),
            device_did: credentials.device_did.clone(),
        })
    }

    /// Decrypt and extract credentials
    pub fn decrypt_credentials(
        &self,
        shared_secret: &SharedSecret,
    ) -> Result<DeviceCredentials, PairingError> {
        let key = shared_secret.derive_encryption_key(b"pairing-confirm");

        let ciphertext = URL_SAFE_NO_PAD
            .decode(&self.encrypted_credentials)
            .map_err(|e| PairingError::DecryptionError(e.to_string()))?;

        let nonce_bytes = URL_SAFE_NO_PAD
            .decode(&self.nonce)
            .map_err(|e| PairingError::DecryptionError(e.to_string()))?;

        let nonce: [u8; 12] = nonce_bytes
            .try_into()
            .map_err(|_| PairingError::DecryptionError("Invalid nonce".to_string()))?;

        let plaintext =
            decrypt(&key, &ciphertext, &nonce, None).map_err(|e| PairingError::DecryptionError(e.to_string()))?;

        serde_json::from_slice(&plaintext).map_err(|e| PairingError::Failed(e.to_string()))
    }
}

/// Pairing session state on the controller side
pub struct ControllerPairingSession {
    /// QR payload that was displayed
    qr_payload: QRPairingPayload,

    /// Our ephemeral key pair
    ephemeral: EphemeralKeyPair,

    /// Shared secret (after request received)
    shared_secret: Option<SharedSecret>,

    /// Challenge sent to device
    challenge: Option<[u8; 32]>,

    /// Device info from request
    device_info: Option<DeviceInfo>,

    /// Device's signing public key
    device_signing_pk: Option<VerifyingKey>,
}

impl ControllerPairingSession {
    /// Create a new session from QR payload
    pub fn new(qr_payload: QRPairingPayload, ephemeral: EphemeralKeyPair) -> Self {
        Self {
            qr_payload,
            ephemeral,
            shared_secret: None,
            challenge: None,
            device_info: None,
            device_signing_pk: None,
        }
    }

    /// Process a pairing request from a device
    pub fn process_request(
        &mut self,
        request: &PairingRequest,
    ) -> Result<PairingChallenge, PairingError> {
        // Verify nonce matches
        let expected_nonce = self.qr_payload.get_nonce_bytes()?;
        let request_nonce = URL_SAFE_NO_PAD
            .decode(&request.nonce)
            .map_err(|_| PairingError::InvalidRequest)?;

        if request_nonce != expected_nonce {
            return Err(PairingError::InvalidRequest);
        }

        // Extract device's ephemeral public key
        let device_ephemeral_pk = request.get_ephemeral_pk()?;

        // Compute shared secret
        let shared_secret = x25519_key_exchange(&self.ephemeral.secret, &device_ephemeral_pk);

        // Derive and store shared secret with nonce binding
        self.shared_secret = Some(derive_shared_secret(shared_secret.as_bytes(), Some(&expected_nonce)));

        // Store device info and signing key
        self.device_info = Some(request.device_info.clone());
        self.device_signing_pk = Some(request.get_signing_pk()?);

        // Create challenge
        let shared = self.shared_secret.as_ref().ok_or(PairingError::Failed(
            "Shared secret not established".to_string(),
        ))?;
        let (challenge_msg, challenge) = PairingChallenge::new(shared)?;
        self.challenge = Some(challenge);

        Ok(challenge_msg)
    }

    /// Verify a pairing response
    pub fn verify_response(&self, response: &PairingResponse) -> Result<bool, PairingError> {
        let challenge = self.challenge.ok_or(PairingError::Failed(
            "No challenge sent yet".to_string(),
        ))?;

        let device_signing_pk = self.device_signing_pk.as_ref().ok_or(PairingError::Failed(
            "No device signing key".to_string(),
        ))?;

        // Compute transcript hash
        let transcript_hash = self.compute_transcript_hash()?;

        response.verify(&challenge, &transcript_hash, device_signing_pk)
    }

    /// Create pairing confirmation
    pub fn create_confirmation(
        &self,
        root_did: &str,
        device_id: &str,
        network_config: NetworkConfig,
    ) -> Result<PairingConfirm, PairingError> {
        let shared_secret = self.shared_secret.as_ref().ok_or(PairingError::Failed(
            "No shared secret".to_string(),
        ))?;

        let credentials = DeviceCredentials {
            device_did: format!("{root_did}/device/{device_id}"),
            root_did: root_did.to_string(),
            relay_addrs: network_config.relay_addrs,
            bootstrap_peers: network_config.bootstrap_peers,
            registry_address: network_config.registry_address,
            rpc_endpoints: network_config.rpc_endpoints,
            capabilities: network_config.capabilities,
        };

        PairingConfirm::new(&credentials, shared_secret)
    }

    /// Get the device info
    pub fn device_info(&self) -> Option<&DeviceInfo> {
        self.device_info.as_ref()
    }

    fn compute_transcript_hash(&self) -> Result<[u8; 32], PairingError> {
        use sha2::{Digest, Sha256};

        let mut hasher = Sha256::new();

        // Include QR payload
        hasher.update(self.qr_payload.controller_did.as_bytes());
        hasher.update(self.qr_payload.get_nonce_bytes()?);
        hasher.update(self.qr_payload.timestamp.to_le_bytes());

        // Include challenge
        if let Some(challenge) = &self.challenge {
            hasher.update(challenge);
        }

        Ok(hasher.finalize().into())
    }
}

/// Network configuration for device credentials
#[derive(Default)]
pub struct NetworkConfig {
    pub relay_addrs: Vec<String>,
    pub bootstrap_peers: Vec<String>,
    pub registry_address: Option<String>,
    pub rpc_endpoints: Vec<String>,
    pub capabilities: DeviceCapabilities,
}

/// Pairing session state on the device side
pub struct DevicePairingSession {
    /// Scanned QR payload
    qr_payload: QRPairingPayload,

    /// Our ephemeral key pair
    ephemeral: EphemeralKeyPair,

    /// Our long-term signing key
    signing_key: SigningKey,

    /// Shared secret
    shared_secret: Option<SharedSecret>,

    /// Received credentials
    credentials: Option<DeviceCredentials>,
}

impl DevicePairingSession {
    /// Create a new session from scanned QR
    pub fn new(qr_payload: QRPairingPayload, signing_key: SigningKey) -> Result<Self, PairingError> {
        // Verify QR is not expired
        if qr_payload.is_expired() {
            return Err(PairingError::Failed("QR code expired".to_string()));
        }

        Ok(Self {
            qr_payload,
            ephemeral: EphemeralKeyPair::generate(),
            signing_key,
            shared_secret: None,
            credentials: None,
        })
    }

    /// Create pairing request
    pub fn create_request(&mut self, device_info: DeviceInfo) -> Result<PairingRequest, PairingError> {
        // Compute shared secret with controller's ephemeral key
        let controller_pk_bytes = self.qr_payload.get_ephemeral_pk_bytes()?;
        let controller_pk = X25519PublicKey::from(controller_pk_bytes);
        let shared_secret = x25519_key_exchange(&self.ephemeral.secret, &controller_pk);

        // Derive with nonce binding
        let nonce = self.qr_payload.get_nonce_bytes()?;
        self.shared_secret = Some(derive_shared_secret(shared_secret.as_bytes(), Some(&nonce)));

        Ok(PairingRequest::new(
            &self.ephemeral,
            &self.signing_key.verifying_key(),
            &nonce,
            device_info,
        ))
    }

    /// Process challenge from controller
    pub fn process_challenge(
        &self,
        challenge: &PairingChallenge,
    ) -> Result<PairingResponse, PairingError> {
        let shared_secret = self.shared_secret.as_ref().ok_or(PairingError::Failed(
            "No shared secret established".to_string(),
        ))?;

        // Decrypt challenge
        let challenge_bytes = challenge.decrypt(shared_secret)?;

        // Compute transcript hash
        let transcript_hash = self.compute_transcript_hash(&challenge_bytes)?;

        // Create response
        Ok(PairingResponse::new(
            &challenge_bytes,
            &transcript_hash,
            &self.signing_key,
        ))
    }

    /// Process confirmation and extract credentials
    pub fn process_confirmation(
        &mut self,
        confirm: &PairingConfirm,
    ) -> Result<&DeviceCredentials, PairingError> {
        let shared_secret = self.shared_secret.as_ref().ok_or(PairingError::Failed(
            "No shared secret established".to_string(),
        ))?;

        let credentials = confirm.decrypt_credentials(shared_secret)?;
        self.credentials = Some(credentials);

        // SAFETY: We just set self.credentials to Some above
        self.credentials
            .as_ref()
            .ok_or_else(|| PairingError::Failed("Credentials not set".to_string()))
    }

    fn compute_transcript_hash(&self, challenge: &[u8; 32]) -> Result<[u8; 32], PairingError> {
        use sha2::{Digest, Sha256};

        let mut hasher = Sha256::new();

        // Include QR payload
        hasher.update(self.qr_payload.controller_did.as_bytes());
        hasher.update(self.qr_payload.get_nonce_bytes()?);
        hasher.update(self.qr_payload.timestamp.to_le_bytes());

        // Include challenge
        hasher.update(challenge);

        Ok(hasher.finalize().into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::rngs::OsRng;

    fn generate_test_keys() -> SigningKey {
        SigningKey::generate(&mut OsRng)
    }

    #[test]
    fn test_full_pairing_flow() {
        // Controller generates QR
        let controller_signing_key = generate_test_keys();
        let controller_did = "did:hanzo:controller";

        let generator =
            crate::qr::QRGenerator::new(controller_did, controller_signing_key.clone());
        let (qr_payload, controller_ephemeral) = generator
            .generate_pairing_qr(crate::qr::PairingType::DeviceOnboarding, Some(300))
            .expect("Failed to generate QR");

        // Device scans QR and creates session
        let device_signing_key = generate_test_keys();
        let mut device_session =
            DevicePairingSession::new(qr_payload.clone(), device_signing_key)
                .expect("Failed to create device session");

        // Controller creates session
        let mut controller_session = ControllerPairingSession::new(qr_payload, controller_ephemeral);

        // Device creates pairing request
        let device_info = DeviceInfo {
            name: "Test Device".to_string(),
            device_type: DeviceType::Desktop,
            capabilities: vec!["inference".to_string()],
            model: None,
            os: None,
        };
        let request = device_session
            .create_request(device_info)
            .expect("Failed to create request");

        // Controller processes request
        let challenge = controller_session
            .process_request(&request)
            .expect("Failed to process request");

        // Device processes challenge
        let response = device_session
            .process_challenge(&challenge)
            .expect("Failed to process challenge");

        // Controller verifies response
        let verified = controller_session
            .verify_response(&response)
            .expect("Failed to verify response");
        assert!(verified);

        // Controller creates confirmation
        let network_config = NetworkConfig::default();
        let confirm = controller_session
            .create_confirmation(controller_did, "device-1", network_config)
            .expect("Failed to create confirmation");

        // Device processes confirmation
        let credentials = device_session
            .process_confirmation(&confirm)
            .expect("Failed to process confirmation");

        assert_eq!(credentials.root_did, controller_did);
        assert_eq!(credentials.device_did, "did:hanzo:controller/device/device-1");
    }

    #[test]
    fn test_pairing_request_serialization() {
        let ephemeral = EphemeralKeyPair::generate();
        let signing_key = generate_test_keys();
        let nonce = [0x42u8; 16];

        let device_info = DeviceInfo {
            name: "Test".to_string(),
            device_type: DeviceType::Mobile,
            capabilities: vec![],
            model: Some("iPhone".to_string()),
            os: Some("iOS".to_string()),
        };

        let request = PairingRequest::new(
            &ephemeral,
            &signing_key.verifying_key(),
            &nonce,
            device_info,
        );

        let json = serde_json::to_string(&request).expect("Failed to serialize");
        let parsed: PairingRequest = serde_json::from_str(&json).expect("Failed to parse");

        assert_eq!(request.nonce, parsed.nonce);
        assert_eq!(
            request.device_info.device_type,
            parsed.device_info.device_type
        );
    }

    #[test]
    fn test_device_credentials_serialization() {
        let credentials = DeviceCredentials {
            device_did: "did:hanzo:user/device/1".to_string(),
            root_did: "did:hanzo:user".to_string(),
            relay_addrs: vec!["/ip4/1.2.3.4/tcp/4001".to_string()],
            bootstrap_peers: vec![],
            registry_address: Some("0x123".to_string()),
            rpc_endpoints: vec!["https://rpc.hanzo.network".to_string()],
            capabilities: DeviceCapabilities {
                can_sign: true,
                full_inbox_access: false,
                can_modify_settings: false,
                can_add_devices: false,
                allowed_tools: vec!["search".to_string()],
            },
        };

        let json = serde_json::to_string(&credentials).expect("Failed to serialize");
        let parsed: DeviceCredentials = serde_json::from_str(&json).expect("Failed to parse");

        assert_eq!(credentials.device_did, parsed.device_did);
        assert_eq!(credentials.capabilities.can_sign, parsed.capabilities.can_sign);
    }
}
