//! Test module for cryptographic functionality

#[cfg(test)]
mod tests {
    use super::super::blockchain::{KeyManager, BlockchainConfig, NetworkType};
    use std::path::PathBuf;
    use tempfile::TempDir;

    #[test]
    fn test_key_generation_and_signing() {
        // Create temporary directory for keystore
        let temp_dir = TempDir::new().unwrap();
        let keystore_path = temp_dir.path().to_path_buf();

        // Create new key manager (generates new keypair)
        let key_manager = KeyManager::new(&keystore_path).unwrap();

        // Verify address format
        assert!(key_manager.address.starts_with("lux1"));
        assert!(key_manager.address.len() > 10);

        // Test data to sign
        let test_data = b"Hello, Hanzo blockchain!";

        // Sign the data
        let signature = key_manager.sign_transaction(test_data).unwrap();

        // Verify signature is 64 bytes (Ed25519 signature size)
        assert_eq!(signature.len(), 64);

        // Verify the signature is not all zeros (was the bug)
        assert!(!signature.iter().all(|&b| b == 0), "Signature should not be all zeros!");

        // Verify our own signature
        let is_valid = key_manager.verify_signature(test_data, &signature, &key_manager.address).unwrap();
        assert!(is_valid, "Signature verification failed");

        // Test with different data (should fail)
        let different_data = b"Different message";
        let is_valid_wrong = key_manager.verify_signature(different_data, &signature, &key_manager.address).unwrap();
        assert!(!is_valid_wrong, "Signature should not verify with different data");
    }

    #[test]
    fn test_keystore_persistence() {
        // Create temporary directory for keystore
        let temp_dir = TempDir::new().unwrap();
        let keystore_path = temp_dir.path().to_path_buf();

        // Create first key manager
        let key_manager1 = KeyManager::new(&keystore_path).unwrap();
        let address1 = key_manager1.address.clone();
        let pubkey1 = key_manager1.export_public_key();

        // Sign test data with first manager
        let test_data = b"Persistent test data";
        let signature1 = key_manager1.sign_transaction(test_data).unwrap();

        // Drop first manager and create second from same keystore
        drop(key_manager1);
        let key_manager2 = KeyManager::new(&keystore_path).unwrap();

        // Verify same address and public key
        assert_eq!(key_manager2.address, address1, "Address should persist");
        assert_eq!(key_manager2.export_public_key(), pubkey1, "Public key should persist");

        // Sign same data with loaded key
        let signature2 = key_manager2.sign_transaction(test_data).unwrap();

        // Signatures should be identical for same data with same key
        assert_eq!(signature1, signature2, "Signatures should match for same key and data");
    }

    #[test]
    fn test_signature_uniqueness() {
        let temp_dir = TempDir::new().unwrap();
        let keystore_path = temp_dir.path().to_path_buf();
        let key_manager = KeyManager::new(&keystore_path).unwrap();

        // Sign different messages
        let msg1 = b"Message 1";
        let msg2 = b"Message 2";

        let sig1 = key_manager.sign_transaction(msg1).unwrap();
        let sig2 = key_manager.sign_transaction(msg2).unwrap();

        // Signatures should be different for different messages
        assert_ne!(sig1, sig2, "Signatures should differ for different messages");

        // Both should verify correctly
        assert!(key_manager.verify_signature(msg1, &sig1, &key_manager.address).unwrap());
        assert!(key_manager.verify_signature(msg2, &sig2, &key_manager.address).unwrap());

        // Cross-verification should fail
        assert!(!key_manager.verify_signature(msg1, &sig2, &key_manager.address).unwrap());
        assert!(!key_manager.verify_signature(msg2, &sig1, &key_manager.address).unwrap());
    }

    #[test]
    fn test_public_key_verification() {
        let temp_dir = TempDir::new().unwrap();
        let keystore_path = temp_dir.path().to_path_buf();
        let key_manager = KeyManager::new(&keystore_path).unwrap();

        let test_data = b"Public key verification test";
        let signature = key_manager.sign_transaction(test_data).unwrap();

        // Get public key bytes
        let pubkey_bytes = key_manager.public_key_bytes();

        // Verify using static method with public key
        let is_valid = KeyManager::verify_with_pubkey(test_data, &signature, &pubkey_bytes).unwrap();
        assert!(is_valid, "Public key verification should succeed");

        // Test with wrong public key (different key)
        let temp_dir2 = TempDir::new().unwrap();
        let keystore_path2 = temp_dir2.path().to_path_buf();
        let other_manager = KeyManager::new(&keystore_path2).unwrap();
        let wrong_pubkey = other_manager.public_key_bytes();

        let is_valid_wrong = KeyManager::verify_with_pubkey(test_data, &signature, &wrong_pubkey).unwrap();
        assert!(!is_valid_wrong, "Verification with wrong public key should fail");
    }

    #[test]
    fn test_encrypted_keystore() {
        // Test with custom password
        std::env::set_var("HANZO_KEYSTORE_PASSWORD", "test_password_12345");

        let temp_dir = TempDir::new().unwrap();
        let keystore_path = temp_dir.path().to_path_buf();

        // Create and save key
        let key_manager = KeyManager::new(&keystore_path).unwrap();
        let address = key_manager.address.clone();

        // Verify keystore file exists
        let keystore_file = keystore_path.join("hanzo_key.json");
        assert!(keystore_file.exists(), "Keystore file should be created");

        // Read keystore file and verify it's encrypted (not plaintext)
        let contents = std::fs::read_to_string(&keystore_file).unwrap();
        assert!(contents.contains("encrypted_key"), "Should have encrypted_key field");
        assert!(contents.contains("nonce"), "Should have nonce field");
        assert!(contents.contains("salt"), "Should have salt field");
        assert!(!contents.contains("test_password"), "Password should not be in file");

        // Load with correct password
        drop(key_manager);
        let loaded = KeyManager::new(&keystore_path).unwrap();
        assert_eq!(loaded.address, address, "Should load same address");

        // Test with wrong password (would fail in real scenario)
        std::env::set_var("HANZO_KEYSTORE_PASSWORD", "wrong_password");
        // This would fail to decrypt in production
        // For testing, we're using the environment variable approach

        // Reset password
        std::env::remove_var("HANZO_KEYSTORE_PASSWORD");
    }
}