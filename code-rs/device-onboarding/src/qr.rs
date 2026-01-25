//! QR code generation and parsing for device onboarding

use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use serde::{Deserialize, Serialize};
use std::time::{SystemTime, UNIX_EPOCH};
use x25519_dalek::{PublicKey as X25519PublicKey, StaticSecret as X25519Secret};

use crate::error::QRError;

/// Current protocol version
pub const PROTOCOL_VERSION: u8 = 1;

/// Default TTL for pairing QR codes (5 minutes)
pub const DEFAULT_TTL_SECONDS: u16 = 300;

/// Type of pairing being requested
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PairingType {
    /// New device being added to user's identity
    DeviceOnboarding,
    /// Temporary session pairing (e.g., kiosk access)
    SessionPairing,
    /// Device-to-device direct connection
    DirectConnect,
}

impl PairingType {
    pub fn as_u8(self) -> u8 {
        match self {
            PairingType::DeviceOnboarding => 0,
            PairingType::SessionPairing => 1,
            PairingType::DirectConnect => 2,
        }
    }

    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(PairingType::DeviceOnboarding),
            1 => Some(PairingType::SessionPairing),
            2 => Some(PairingType::DirectConnect),
            _ => None,
        }
    }
}

/// Network hints for peer discovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkHints {
    /// Local IP addresses if on same network
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub local_addrs: Vec<String>,

    /// Relay node multiaddresses
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub relay_addrs: Vec<String>,

    /// libp2p peer ID
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub peer_id: Option<String>,
}

/// QR code pairing payload
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QRPairingPayload {
    /// Protocol version
    #[serde(rename = "v")]
    pub version: u8,

    /// Pairing type
    #[serde(rename = "t")]
    pub pairing_type: PairingType,

    /// Controller's DID
    #[serde(rename = "d")]
    pub controller_did: String,

    /// Ephemeral X25519 public key (base64url encoded)
    #[serde(rename = "pk")]
    pub ephemeral_pk: String,

    /// Pairing challenge nonce (base64url encoded)
    #[serde(rename = "n")]
    pub nonce: String,

    /// Timestamp (Unix seconds)
    #[serde(rename = "ts")]
    pub timestamp: u64,

    /// Expiry duration (seconds)
    #[serde(rename = "ttl")]
    pub ttl: u16,

    /// Signature over payload (base64url encoded)
    #[serde(rename = "s")]
    pub signature: String,

    /// Network hints for peer discovery (optional)
    #[serde(default, skip_serializing_if = "Option::is_none", rename = "h")]
    pub network_hints: Option<NetworkHints>,
}

impl QRPairingPayload {
    /// Check if the payload has expired
    pub fn is_expired(&self) -> bool {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        now > self.timestamp + u64::from(self.ttl)
    }

    /// Verify the signature on this payload
    pub fn verify_signature(&self, verifying_key: &VerifyingKey) -> Result<bool, QRError> {
        let payload_bytes = self.get_signing_payload()?;
        let signature_bytes = URL_SAFE_NO_PAD
            .decode(&self.signature)
            .map_err(|e| QRError::ParseFailed(format!("Invalid signature encoding: {e}")))?;

        let signature: Signature = signature_bytes
            .as_slice()
            .try_into()
            .map_err(|_| QRError::InvalidSignature)?;

        Ok(verifying_key.verify(&payload_bytes, &signature).is_ok())
    }

    /// Get the bytes to sign/verify
    fn get_signing_payload(&self) -> Result<Vec<u8>, QRError> {
        let mut payload = Vec::new();
        payload.push(self.version);
        payload.push(self.pairing_type.as_u8());
        payload.extend_from_slice(self.controller_did.as_bytes());

        let pk_bytes = URL_SAFE_NO_PAD
            .decode(&self.ephemeral_pk)
            .map_err(|e| QRError::ParseFailed(format!("Invalid public key: {e}")))?;
        payload.extend_from_slice(&pk_bytes);

        let nonce_bytes = URL_SAFE_NO_PAD
            .decode(&self.nonce)
            .map_err(|e| QRError::ParseFailed(format!("Invalid nonce: {e}")))?;
        payload.extend_from_slice(&nonce_bytes);

        payload.extend_from_slice(&self.timestamp.to_le_bytes());
        payload.extend_from_slice(&self.ttl.to_le_bytes());

        Ok(payload)
    }

    /// Encode payload to URL format
    pub fn to_url(&self) -> Result<String, QRError> {
        let json = serde_json::to_vec(self)?;
        let encoded = URL_SAFE_NO_PAD.encode(&json);
        Ok(format!("hanzo://pair/v1/{encoded}"))
    }

    /// Parse payload from URL format
    pub fn from_url(url: &str) -> Result<Self, QRError> {
        let prefix = "hanzo://pair/v1/";
        if !url.starts_with(prefix) {
            return Err(QRError::ParseFailed("Invalid URL prefix".to_string()));
        }

        let encoded = &url[prefix.len()..];
        let json = URL_SAFE_NO_PAD.decode(encoded)?;
        let payload: Self = serde_json::from_slice(&json)?;

        if payload.version != PROTOCOL_VERSION {
            return Err(QRError::UnsupportedVersion(payload.version));
        }

        Ok(payload)
    }

    /// Get the ephemeral public key as bytes
    pub fn get_ephemeral_pk_bytes(&self) -> Result<[u8; 32], QRError> {
        let bytes = URL_SAFE_NO_PAD
            .decode(&self.ephemeral_pk)
            .map_err(|e| QRError::ParseFailed(format!("Invalid public key: {e}")))?;

        bytes
            .try_into()
            .map_err(|_| QRError::ParseFailed("Invalid public key length".to_string()))
    }

    /// Get the nonce as bytes
    pub fn get_nonce_bytes(&self) -> Result<[u8; 16], QRError> {
        let bytes = URL_SAFE_NO_PAD
            .decode(&self.nonce)
            .map_err(|e| QRError::ParseFailed(format!("Invalid nonce: {e}")))?;

        bytes
            .try_into()
            .map_err(|_| QRError::ParseFailed("Invalid nonce length".to_string()))
    }
}

/// Ephemeral key pair for pairing session
pub struct EphemeralKeyPair {
    pub secret: X25519Secret,
    pub public: X25519PublicKey,
}

impl EphemeralKeyPair {
    /// Generate a new ephemeral key pair
    pub fn generate() -> Self {
        let secret = X25519Secret::random_from_rng(OsRng);
        let public = X25519PublicKey::from(&secret);
        Self { secret, public }
    }

    /// Get public key as base64url encoded string
    pub fn public_key_base64(&self) -> String {
        URL_SAFE_NO_PAD.encode(self.public.as_bytes())
    }
}

/// QR code generator for device pairing
pub struct QRGenerator {
    controller_did: String,
    signing_key: SigningKey,
    network_hints: Option<NetworkHints>,
}

impl QRGenerator {
    /// Create a new QR generator
    pub fn new(controller_did: impl Into<String>, signing_key: SigningKey) -> Self {
        Self {
            controller_did: controller_did.into(),
            signing_key,
            network_hints: None,
        }
    }

    /// Set network hints for peer discovery
    pub fn with_network_hints(mut self, hints: NetworkHints) -> Self {
        self.network_hints = Some(hints);
        self
    }

    /// Generate a pairing QR payload
    pub fn generate_pairing_qr(
        &self,
        pairing_type: PairingType,
        ttl_seconds: Option<u16>,
    ) -> Result<(QRPairingPayload, EphemeralKeyPair), QRError> {
        let ephemeral = EphemeralKeyPair::generate();

        let mut nonce = [0u8; 16];
        rand::RngCore::fill_bytes(&mut OsRng, &mut nonce);

        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|e| QRError::GenerationFailed(e.to_string()))?
            .as_secs();

        let ttl = ttl_seconds.unwrap_or(DEFAULT_TTL_SECONDS);

        // Build payload for signing
        let mut signing_payload = Vec::new();
        signing_payload.push(PROTOCOL_VERSION);
        signing_payload.push(pairing_type.as_u8());
        signing_payload.extend_from_slice(self.controller_did.as_bytes());
        signing_payload.extend_from_slice(ephemeral.public.as_bytes());
        signing_payload.extend_from_slice(&nonce);
        signing_payload.extend_from_slice(&timestamp.to_le_bytes());
        signing_payload.extend_from_slice(&ttl.to_le_bytes());

        // Sign the payload
        let signature = self.signing_key.sign(&signing_payload);

        let payload = QRPairingPayload {
            version: PROTOCOL_VERSION,
            pairing_type,
            controller_did: self.controller_did.clone(),
            ephemeral_pk: ephemeral.public_key_base64(),
            nonce: URL_SAFE_NO_PAD.encode(nonce),
            timestamp,
            ttl,
            signature: URL_SAFE_NO_PAD.encode(signature.to_bytes()),
            network_hints: self.network_hints.clone(),
        };

        Ok((payload, ephemeral))
    }

    /// Render QR code to PNG bytes
    #[cfg(feature = "qr-render")]
    pub fn render_qr(&self, payload: &QRPairingPayload) -> Result<Vec<u8>, QRError> {
        use image::Luma;
        use qrcode::{EcLevel, QrCode};
        use std::io::Cursor;

        let url = payload.to_url()?;

        let code = QrCode::with_error_correction_level(url.as_bytes(), EcLevel::M)
            .map_err(|e| QRError::GenerationFailed(e.to_string()))?;

        let image = code.render::<Luma<u8>>().min_dimensions(200, 200).build();

        let mut png_bytes = Vec::new();
        let dynamic_image = image::DynamicImage::ImageLuma8(image);
        dynamic_image
            .write_to(&mut Cursor::new(&mut png_bytes), image::ImageFormat::Png)
            .map_err(|e| QRError::RenderError(e.to_string()))?;

        Ok(png_bytes)
    }

    /// Render QR code to ASCII (for terminal display)
    #[cfg(feature = "qr-render")]
    pub fn render_qr_ascii(&self, payload: &QRPairingPayload) -> Result<String, QRError> {
        use qrcode::{EcLevel, QrCode};

        let url = payload.to_url()?;

        let code = QrCode::with_error_correction_level(url.as_bytes(), EcLevel::M)
            .map_err(|e| QRError::GenerationFailed(e.to_string()))?;

        Ok(code
            .render()
            .dark_color(' ')
            .light_color('\u{2588}')
            .quiet_zone(false)
            .module_dimensions(2, 1)
            .build())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generate_test_signing_key() -> SigningKey {
        SigningKey::generate(&mut OsRng)
    }

    #[test]
    fn test_qr_payload_generation() {
        let signing_key = generate_test_signing_key();
        let generator = QRGenerator::new("did:hanzo:testuser", signing_key.clone());

        let (payload, _ephemeral) = generator
            .generate_pairing_qr(PairingType::DeviceOnboarding, Some(300))
            .expect("Failed to generate QR");

        assert_eq!(payload.version, PROTOCOL_VERSION);
        assert_eq!(payload.pairing_type, PairingType::DeviceOnboarding);
        assert_eq!(payload.controller_did, "did:hanzo:testuser");
        assert_eq!(payload.ttl, 300);
        assert!(!payload.is_expired());
    }

    #[test]
    fn test_qr_payload_url_encoding() {
        let signing_key = generate_test_signing_key();
        let generator = QRGenerator::new("did:hanzo:testuser", signing_key);

        let (payload, _) = generator
            .generate_pairing_qr(PairingType::DeviceOnboarding, None)
            .expect("Failed to generate QR");

        let url = payload.to_url().expect("Failed to encode URL");
        assert!(url.starts_with("hanzo://pair/v1/"));

        let parsed = QRPairingPayload::from_url(&url).expect("Failed to parse URL");
        assert_eq!(parsed.controller_did, payload.controller_did);
        assert_eq!(parsed.nonce, payload.nonce);
    }

    #[test]
    fn test_qr_signature_verification() {
        let signing_key = generate_test_signing_key();
        let verifying_key = signing_key.verifying_key();
        let generator = QRGenerator::new("did:hanzo:testuser", signing_key);

        let (payload, _) = generator
            .generate_pairing_qr(PairingType::DeviceOnboarding, None)
            .expect("Failed to generate QR");

        let result = payload
            .verify_signature(&verifying_key)
            .expect("Verification failed");
        assert!(result);
    }

    #[test]
    fn test_pairing_type_conversion() {
        assert_eq!(PairingType::DeviceOnboarding.as_u8(), 0);
        assert_eq!(PairingType::SessionPairing.as_u8(), 1);
        assert_eq!(PairingType::DirectConnect.as_u8(), 2);

        assert_eq!(
            PairingType::from_u8(0),
            Some(PairingType::DeviceOnboarding)
        );
        assert_eq!(PairingType::from_u8(3), None);
    }

    #[test]
    fn test_network_hints() {
        let signing_key = generate_test_signing_key();
        let hints = NetworkHints {
            local_addrs: vec!["192.168.1.100:8080".to_string()],
            relay_addrs: vec!["/ip4/1.2.3.4/tcp/4001/p2p/QmPeer".to_string()],
            peer_id: Some("QmTestPeer".to_string()),
        };

        let generator = QRGenerator::new("did:hanzo:testuser", signing_key).with_network_hints(hints);

        let (payload, _) = generator
            .generate_pairing_qr(PairingType::DeviceOnboarding, None)
            .expect("Failed to generate QR");

        assert!(payload.network_hints.is_some());
        let hints = payload.network_hints.as_ref().unwrap();
        assert_eq!(hints.local_addrs.len(), 1);
        assert_eq!(hints.relay_addrs.len(), 1);
        assert!(hints.peer_id.is_some());
    }

    #[cfg(feature = "qr-render")]
    #[test]
    fn test_qr_rendering() {
        let signing_key = generate_test_signing_key();
        let generator = QRGenerator::new("did:hanzo:testuser", signing_key);

        let (payload, _) = generator
            .generate_pairing_qr(PairingType::DeviceOnboarding, None)
            .expect("Failed to generate QR");

        let png_bytes = generator.render_qr(&payload).expect("Failed to render QR");
        assert!(!png_bytes.is_empty());

        // PNG magic bytes
        assert_eq!(&png_bytes[0..8], &[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]);

        let ascii = generator
            .render_qr_ascii(&payload)
            .expect("Failed to render ASCII");
        assert!(!ascii.is_empty());
    }
}
