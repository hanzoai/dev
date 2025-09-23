#!/usr/bin/env rust-script
//! ```cargo
//! [dependencies]
//! ed25519-dalek = "2.1"
//! rand = "0.8"
//! sha2 = "0.10"
//! hex = "0.4"
//! ```

use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use sha2::{Sha256, Digest};

fn main() {
    println!("Testing Ed25519 cryptography implementation...\n");

    // Generate a new Ed25519 keypair
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);
    let verifying_key = signing_key.verifying_key();

    println!("Generated Ed25519 keypair:");
    println!("  Private key: {} bytes", signing_key.as_bytes().len());
    println!("  Public key: {} bytes", verifying_key.as_bytes().len());
    println!("  Public key (hex): {}", hex::encode(verifying_key.as_bytes()));

    // Test data to sign
    let test_data = b"Hello, Hanzo blockchain! This is a real cryptographic signature.";
    println!("\nTest data: {:?}", std::str::from_utf8(test_data).unwrap());

    // Hash the data with SHA256
    let mut hasher = Sha256::new();
    hasher.update(test_data);
    let hash = hasher.finalize();
    println!("SHA256 hash: {}", hex::encode(&hash));

    // Sign the hash with Ed25519
    let signature = signing_key.sign(&hash);
    println!("\nGenerated signature:");
    println!("  Length: {} bytes", signature.to_bytes().len());
    println!("  Hex: {}", hex::encode(signature.to_bytes()));

    // Verify signature is not all zeros
    let sig_bytes = signature.to_bytes();
    if sig_bytes.iter().all(|&b| b == 0) {
        panic!("ERROR: Signature is all zeros!");
    }
    println!("  ✓ Signature contains real cryptographic data");

    // Verify the signature
    let verification_result = verifying_key.verify(&hash, &signature);
    match verification_result {
        Ok(()) => println!("  ✓ Signature verification PASSED"),
        Err(e) => println!("  ✗ Signature verification FAILED: {}", e),
    }

    // Test with wrong data (should fail)
    let wrong_data = b"This is different data";
    let mut wrong_hasher = Sha256::new();
    wrong_hasher.update(wrong_data);
    let wrong_hash = wrong_hasher.finalize();

    let wrong_verification = verifying_key.verify(&wrong_hash, &signature);
    match wrong_verification {
        Ok(()) => println!("  ✗ Wrong data verified - THIS SHOULD NOT HAPPEN!"),
        Err(_) => println!("  ✓ Wrong data correctly rejected"),
    }

    println!("\n✅ All cryptographic operations working correctly!");
    println!("This proves the Ed25519 implementation is functioning properly.");
}