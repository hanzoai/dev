//! Integration tests for the device onboarding crate.
//!
//! These tests verify the full pairing flow between a controller and device,
//! including edge cases and error conditions.

use code_device_onboarding::{
    ControllerPairingSession, DeviceCapabilities, DeviceInfo, DevicePairingSession, DeviceType,
    EphemeralKeyPair, NetworkConfig, NetworkHints, PairingChallenge, PairingConfirm,
    PairingRequest, PairingResponse, PairingType, QRGenerator, QRPairingPayload,
    SharedSecret, PROTOCOL_VERSION,
};
use ed25519_dalek::SigningKey;
use rand::rngs::OsRng;

fn generate_signing_key() -> SigningKey {
    SigningKey::generate(&mut OsRng)
}

fn make_device_info(name: &str, device_type: DeviceType) -> DeviceInfo {
    DeviceInfo {
        name: name.to_string(),
        device_type,
        capabilities: vec!["inference".to_string(), "storage".to_string()],
        model: Some("Test Model".to_string()),
        os: Some("TestOS 1.0".to_string()),
    }
}

/// Test the complete pairing flow from QR generation to credential issuance.
#[test]
fn test_complete_pairing_flow() {
    // === Controller side: Generate QR code ===
    let controller_key = generate_signing_key();
    let controller_did = "did:hanzo:alice";

    let generator = QRGenerator::new(controller_did, controller_key.clone());
    let (qr_payload, controller_ephemeral) = generator
        .generate_pairing_qr(PairingType::DeviceOnboarding, Some(300))
        .expect("Failed to generate QR");

    // Verify QR payload properties
    assert_eq!(qr_payload.version, PROTOCOL_VERSION);
    assert_eq!(qr_payload.pairing_type, PairingType::DeviceOnboarding);
    assert_eq!(qr_payload.ttl, 300);
    assert!(!qr_payload.is_expired());

    // === Device side: Scan QR and create session ===
    let device_key = generate_signing_key();
    let mut device_session = DevicePairingSession::new(qr_payload.clone(), device_key)
        .expect("Failed to create device session");

    // === Controller side: Create session ===
    let mut controller_session =
        ControllerPairingSession::new(qr_payload, controller_ephemeral);

    // === Device: Create pairing request ===
    let device_info = make_device_info("My Laptop", DeviceType::Desktop);
    let request = device_session
        .create_request(device_info)
        .expect("Failed to create request");

    // === Controller: Process request and send challenge ===
    let challenge = controller_session
        .process_request(&request)
        .expect("Failed to process request");

    // Verify controller received device info
    let stored_info = controller_session.device_info();
    assert!(stored_info.is_some());
    assert_eq!(stored_info.as_ref().unwrap().name, "My Laptop");
    assert_eq!(stored_info.as_ref().unwrap().device_type, DeviceType::Desktop);

    // === Device: Process challenge and create response ===
    let response = device_session
        .process_challenge(&challenge)
        .expect("Failed to process challenge");

    // === Controller: Verify response ===
    let verified = controller_session
        .verify_response(&response)
        .expect("Failed to verify response");
    assert!(verified, "Response verification should succeed");

    // === Controller: Create confirmation with credentials ===
    let network_config = NetworkConfig {
        relay_addrs: vec!["/ip4/1.2.3.4/tcp/4001/p2p/QmRelay".to_string()],
        bootstrap_peers: vec!["/dnsaddr/bootstrap.hanzo.network".to_string()],
        registry_address: Some("0x1234567890abcdef".to_string()),
        rpc_endpoints: vec!["https://rpc.hanzo.network".to_string()],
        capabilities: DeviceCapabilities {
            can_sign: true,
            full_inbox_access: false,
            can_modify_settings: false,
            can_add_devices: false,
            allowed_tools: vec!["search".to_string(), "calendar".to_string()],
        },
    };

    let confirm = controller_session
        .create_confirmation(controller_did, "laptop-1", network_config)
        .expect("Failed to create confirmation");

    assert_eq!(
        confirm.device_did,
        "did:hanzo:alice/device/laptop-1"
    );

    // === Device: Process confirmation and extract credentials ===
    let credentials = device_session
        .process_confirmation(&confirm)
        .expect("Failed to process confirmation");

    // Verify credentials
    assert_eq!(credentials.device_did, "did:hanzo:alice/device/laptop-1");
    assert_eq!(credentials.root_did, controller_did);
    assert_eq!(credentials.relay_addrs.len(), 1);
    assert_eq!(credentials.bootstrap_peers.len(), 1);
    assert!(credentials.registry_address.is_some());
    assert!(credentials.capabilities.can_sign);
    assert!(!credentials.capabilities.can_add_devices);
    assert_eq!(credentials.capabilities.allowed_tools.len(), 2);
}

/// Test pairing with network hints included in QR code.
#[test]
fn test_pairing_with_network_hints() {
    let controller_key = generate_signing_key();

    let hints = NetworkHints {
        local_addrs: vec![
            "192.168.1.100:8080".to_string(),
            "192.168.1.100:8081".to_string(),
        ],
        relay_addrs: vec!["/ip4/relay.hanzo.network/tcp/4001".to_string()],
        peer_id: Some("QmControllerPeerId".to_string()),
    };

    let generator = QRGenerator::new("did:hanzo:bob", controller_key)
        .with_network_hints(hints);

    let (qr_payload, _) = generator
        .generate_pairing_qr(PairingType::DirectConnect, Some(120))
        .expect("Failed to generate QR");

    assert_eq!(qr_payload.pairing_type, PairingType::DirectConnect);
    assert_eq!(qr_payload.ttl, 120);

    let hints = qr_payload.network_hints.expect("Should have hints");
    assert_eq!(hints.local_addrs.len(), 2);
    assert_eq!(hints.relay_addrs.len(), 1);
    assert_eq!(hints.peer_id, Some("QmControllerPeerId".to_string()));
}

/// Test QR payload URL encoding and decoding roundtrip.
#[test]
fn test_qr_url_roundtrip() {
    let controller_key = generate_signing_key();
    let generator = QRGenerator::new("did:hanzo:charlie", controller_key.clone());

    let (original, _) = generator
        .generate_pairing_qr(PairingType::SessionPairing, Some(600))
        .expect("Failed to generate QR");

    // Encode to URL
    let url = original.to_url().expect("Failed to encode URL");
    assert!(url.starts_with("hanzo://pair/v1/"));

    // Decode back
    let decoded = QRPairingPayload::from_url(&url).expect("Failed to decode URL");

    // Verify all fields match
    assert_eq!(decoded.version, original.version);
    assert_eq!(decoded.pairing_type, original.pairing_type);
    assert_eq!(decoded.controller_did, original.controller_did);
    assert_eq!(decoded.ephemeral_pk, original.ephemeral_pk);
    assert_eq!(decoded.nonce, original.nonce);
    assert_eq!(decoded.timestamp, original.timestamp);
    assert_eq!(decoded.ttl, original.ttl);
    assert_eq!(decoded.signature, original.signature);
}

/// Test signature verification on QR payload.
#[test]
fn test_qr_signature_verification() {
    let controller_key = generate_signing_key();
    let verifying_key = controller_key.verifying_key();
    let generator = QRGenerator::new("did:hanzo:dave", controller_key);

    let (payload, _) = generator
        .generate_pairing_qr(PairingType::DeviceOnboarding, None)
        .expect("Failed to generate QR");

    // Correct key should verify
    let result = payload.verify_signature(&verifying_key);
    assert!(result.is_ok());
    assert!(result.unwrap());

    // Wrong key should fail
    let wrong_key = generate_signing_key().verifying_key();
    let result = payload.verify_signature(&wrong_key);
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

/// Test that pairing fails with wrong nonce.
#[test]
fn test_pairing_wrong_nonce() {
    let controller_key = generate_signing_key();
    let generator = QRGenerator::new("did:hanzo:eve", controller_key);

    let (qr_payload, controller_ephemeral) = generator
        .generate_pairing_qr(PairingType::DeviceOnboarding, Some(300))
        .expect("Failed to generate QR");

    let mut controller_session =
        ControllerPairingSession::new(qr_payload.clone(), controller_ephemeral);

    // Create a request with wrong nonce
    let device_ephemeral = EphemeralKeyPair::generate();
    let device_key = generate_signing_key();
    let wrong_nonce = [0xFFu8; 16]; // Wrong nonce

    let request = PairingRequest::new(
        &device_ephemeral,
        &device_key.verifying_key(),
        &wrong_nonce,
        make_device_info("Attacker", DeviceType::Unknown),
    );

    // Should fail
    let result = controller_session.process_request(&request);
    assert!(result.is_err());
}

/// Test multiple device types.
#[test]
fn test_device_types() {
    let device_types = [
        DeviceType::Desktop,
        DeviceType::Mobile,
        DeviceType::IoT,
        DeviceType::Server,
        DeviceType::Embedded,
        DeviceType::Unknown,
    ];

    for device_type in device_types {
        let controller_key = generate_signing_key();
        let generator = QRGenerator::new("did:hanzo:frank", controller_key);

        let (qr_payload, controller_ephemeral) = generator
            .generate_pairing_qr(PairingType::DeviceOnboarding, Some(300))
            .expect("Failed to generate QR");

        let device_key = generate_signing_key();
        let mut device_session = DevicePairingSession::new(qr_payload.clone(), device_key)
            .expect("Failed to create device session");

        let mut controller_session =
            ControllerPairingSession::new(qr_payload, controller_ephemeral);

        let device_info = make_device_info("Device", device_type.clone());
        let request = device_session
            .create_request(device_info)
            .expect("Failed to create request");

        let _ = controller_session.process_request(&request);
        let stored_type = &controller_session.device_info().unwrap().device_type;
        assert_eq!(*stored_type, device_type);
    }
}

/// Test all pairing types.
#[test]
fn test_pairing_types() {
    let pairing_types = [
        PairingType::DeviceOnboarding,
        PairingType::SessionPairing,
        PairingType::DirectConnect,
    ];

    for pt in pairing_types {
        let controller_key = generate_signing_key();
        let generator = QRGenerator::new("did:hanzo:grace", controller_key);

        let (qr_payload, _) = generator
            .generate_pairing_qr(pt, Some(300))
            .expect("Failed to generate QR");

        assert_eq!(qr_payload.pairing_type, pt);

        // Test u8 conversion
        assert_eq!(PairingType::from_u8(pt.as_u8()), Some(pt));
    }

    // Invalid u8 should return None
    assert_eq!(PairingType::from_u8(99), None);
}

/// Test shared secret derivation consistency.
#[test]
fn test_shared_secret_consistency() {
    use code_device_onboarding::{x25519_key_exchange, derive_shared_secret};
    use x25519_dalek::{PublicKey, StaticSecret};

    // Simulate controller and device
    let controller_secret = StaticSecret::random_from_rng(OsRng);
    let controller_public = PublicKey::from(&controller_secret);

    let device_secret = StaticSecret::random_from_rng(OsRng);
    let device_public = PublicKey::from(&device_secret);

    // Both sides compute shared secret
    let controller_shared = x25519_key_exchange(&controller_secret, &device_public);
    let device_shared = x25519_key_exchange(&device_secret, &controller_public);

    // Raw shared secrets should match
    assert_eq!(
        controller_shared.as_bytes(),
        device_shared.as_bytes()
    );

    // Derived keys should also match
    let nonce = [0x42u8; 16];
    let controller_derived = derive_shared_secret(controller_shared.as_bytes(), Some(&nonce));
    let device_derived = derive_shared_secret(device_shared.as_bytes(), Some(&nonce));

    assert_eq!(
        controller_derived.as_bytes(),
        device_derived.as_bytes()
    );

    // Different nonce should produce different result
    let different_nonce = [0x99u8; 16];
    let different_derived = derive_shared_secret(controller_shared.as_bytes(), Some(&different_nonce));
    assert_ne!(
        controller_derived.as_bytes(),
        different_derived.as_bytes()
    );
}

/// Test encryption/decryption roundtrip with shared secret.
#[test]
fn test_encryption_roundtrip() {
    use code_device_onboarding::{encrypt, decrypt};

    let key = [0x42u8; 32];
    let plaintext = b"This is a secret message for device pairing";

    // Without AAD
    let (ciphertext, nonce) = encrypt(&key, plaintext, None)
        .expect("Encryption failed");

    let decrypted = decrypt(&key, &ciphertext, &nonce, None)
        .expect("Decryption failed");

    assert_eq!(decrypted, plaintext);

    // With AAD
    let aad = b"hanzo-pairing-v1";
    let (ciphertext2, nonce2) = encrypt(&key, plaintext, Some(aad))
        .expect("Encryption with AAD failed");

    let decrypted2 = decrypt(&key, &ciphertext2, &nonce2, Some(aad))
        .expect("Decryption with AAD failed");

    assert_eq!(decrypted2, plaintext);

    // Wrong AAD should fail
    let result = decrypt(&key, &ciphertext2, &nonce2, Some(b"wrong-aad"));
    assert!(result.is_err());
}

/// Test credential encryption and decryption in pairing confirm.
#[test]
fn test_credentials_encryption() {
    use code_device_onboarding::DeviceCredentials;

    let shared = SharedSecret::from_bytes([0x42u8; 32]);

    let credentials = DeviceCredentials {
        device_did: "did:hanzo:user/device/test".to_string(),
        root_did: "did:hanzo:user".to_string(),
        relay_addrs: vec!["/ip4/1.2.3.4/tcp/4001".to_string()],
        bootstrap_peers: vec!["/dnsaddr/bootstrap.example.com".to_string()],
        registry_address: Some("0xabcdef".to_string()),
        rpc_endpoints: vec!["https://rpc1.example.com".to_string()],
        capabilities: DeviceCapabilities {
            can_sign: true,
            full_inbox_access: true,
            can_modify_settings: false,
            can_add_devices: false,
            allowed_tools: vec!["tool1".to_string()],
        },
    };

    let confirm = PairingConfirm::new(&credentials, &shared)
        .expect("Failed to create confirmation");

    let decrypted = confirm.decrypt_credentials(&shared)
        .expect("Failed to decrypt credentials");

    assert_eq!(decrypted.device_did, credentials.device_did);
    assert_eq!(decrypted.root_did, credentials.root_did);
    assert_eq!(decrypted.relay_addrs, credentials.relay_addrs);
    assert_eq!(decrypted.capabilities.can_sign, credentials.capabilities.can_sign);
}

/// Test challenge creation and response verification.
#[test]
fn test_challenge_response() {
    let shared = SharedSecret::from_bytes([0x42u8; 32]);
    let device_key = generate_signing_key();

    // Create challenge
    let (challenge_msg, challenge_bytes) = PairingChallenge::new(&shared)
        .expect("Failed to create challenge");

    // Device decrypts challenge
    let decrypted_challenge = challenge_msg.decrypt(&shared)
        .expect("Failed to decrypt challenge");

    assert_eq!(decrypted_challenge, challenge_bytes);

    // Device creates response
    let transcript_hash = [0x11u8; 32];
    let response = PairingResponse::new(
        &decrypted_challenge,
        &transcript_hash,
        &device_key,
    );

    // Verify response
    let verified = response.verify(
        &challenge_bytes,
        &transcript_hash,
        &device_key.verifying_key(),
    ).expect("Verification failed");

    assert!(verified);

    // Wrong challenge should fail
    let wrong_challenge = [0xFFu8; 32];
    let verified = response.verify(
        &wrong_challenge,
        &transcript_hash,
        &device_key.verifying_key(),
    ).expect("Verification failed");

    assert!(!verified);
}

#[cfg(feature = "qr-render")]
mod qr_render_tests {
    use super::*;

    /// Test QR code PNG rendering.
    #[test]
    fn test_qr_png_rendering() {
        let controller_key = generate_signing_key();
        let generator = QRGenerator::new("did:hanzo:henry", controller_key);

        let (payload, _) = generator
            .generate_pairing_qr(PairingType::DeviceOnboarding, None)
            .expect("Failed to generate QR");

        let png_bytes = generator.render_qr(&payload)
            .expect("Failed to render PNG");

        // Check PNG magic bytes
        assert!(png_bytes.len() > 100);
        assert_eq!(&png_bytes[0..8], &[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]);
    }

    /// Test QR code ASCII rendering.
    #[test]
    fn test_qr_ascii_rendering() {
        let controller_key = generate_signing_key();
        let generator = QRGenerator::new("did:hanzo:ivy", controller_key);

        let (payload, _) = generator
            .generate_pairing_qr(PairingType::DeviceOnboarding, None)
            .expect("Failed to generate QR");

        let ascii = generator.render_qr_ascii(&payload)
            .expect("Failed to render ASCII");

        // Should have reasonable dimensions
        assert!(!ascii.is_empty());
        let lines: Vec<&str> = ascii.lines().collect();
        assert!(lines.len() > 10);
    }
}
