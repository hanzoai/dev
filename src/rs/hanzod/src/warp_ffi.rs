//! Rust FFI bindings for Lux Warp ICM (Interchain Messaging) protocol
//!
//! This module provides Rust bindings to the Go-based Warp protocol,
//! allowing Rust applications to participate in Lux interchain messaging.
//! 
//! The Warp protocol enables secure cross-chain communication with BLS signatures
//! and threshold validation.

use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int, c_uint, c_void};
use std::ptr;
use std::slice;

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};

// Constants from Go implementation
pub const CODEC_VERSION: u8 = 0;
pub const MAX_MESSAGE_SIZE: usize = 256 * 1024; // 256 KiB
pub const CHAIN_ID_SIZE: usize = 32;

// External C functions that will be implemented in Go and exposed via CGO
#[link(name = "warp")]
extern "C" {
    // Initialize the Warp library
    fn warp_init() -> c_int;
    
    // Create an unsigned message
    fn warp_create_unsigned_message(
        network_id: c_uint,
        source_chain_id: *const u8,
        source_chain_id_len: c_uint,
        payload: *const u8,
        payload_len: c_uint,
        out_msg: *mut *mut c_void,
    ) -> c_int;
    
    // Sign a message
    fn warp_sign_message(
        unsigned_msg: *const c_void,
        private_key: *const u8,
        private_key_len: c_uint,
        out_signature: *mut u8,
        out_signature_len: *mut c_uint,
    ) -> c_int;
    
    // Verify a signed message
    fn warp_verify_message(
        signed_msg: *const u8,
        signed_msg_len: c_uint,
        validators: *const u8,
        validators_len: c_uint,
        threshold: c_uint,
    ) -> c_int;
    
    // Free memory allocated by the Go side
    fn warp_free(ptr: *mut c_void);
    
    // Get error message from last operation
    fn warp_last_error() -> *const c_char;
}

/// Unsigned Warp message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnsignedMessage {
    pub network_id: u32,
    pub source_chain_id: Vec<u8>,
    pub payload: Vec<u8>,
}

impl UnsignedMessage {
    /// Create a new unsigned message
    pub fn new(network_id: u32, source_chain_id: Vec<u8>, payload: Vec<u8>) -> Result<Self> {
        // Validate chain ID length
        if source_chain_id.len() != CHAIN_ID_SIZE {
            return Err(anyhow!("Source chain ID must be {} bytes", CHAIN_ID_SIZE));
        }
        
        // Validate message size
        let total_size = 4 + source_chain_id.len() + payload.len();
        if total_size > MAX_MESSAGE_SIZE {
            return Err(anyhow!(
                "Message size {} exceeds maximum {}",
                total_size,
                MAX_MESSAGE_SIZE
            ));
        }
        
        Ok(Self {
            network_id,
            source_chain_id,
            payload,
        })
    }
    
    /// Compute the hash ID of the message
    pub fn id(&self) -> Vec<u8> {
        use sha2::{Sha256, Digest};
        let mut hasher = Sha256::new();
        hasher.update(&self.network_id.to_be_bytes());
        hasher.update(&self.source_chain_id);
        hasher.update(&self.payload);
        hasher.finalize().to_vec()
    }
}

/// Signed Warp message with BLS signature
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignedMessage {
    pub unsigned: UnsignedMessage,
    pub signature: Vec<u8>,
}

/// Warp protocol handler
pub struct WarpProtocol {
    initialized: bool,
}

impl WarpProtocol {
    /// Initialize the Warp protocol
    pub fn new() -> Result<Self> {
        unsafe {
            let ret = warp_init();
            if ret != 0 {
                return Err(anyhow!("Failed to initialize Warp protocol: {}", 
                    Self::get_last_error()));
            }
        }
        
        Ok(Self {
            initialized: true,
        })
    }
    
    /// Create an unsigned message using the Go implementation
    pub fn create_unsigned_message_ffi(
        &self,
        network_id: u32,
        source_chain_id: &[u8],
        payload: &[u8],
    ) -> Result<UnsignedMessage> {
        if !self.initialized {
            return Err(anyhow!("Warp protocol not initialized"));
        }
        
        let mut msg_ptr: *mut c_void = ptr::null_mut();
        
        unsafe {
            let ret = warp_create_unsigned_message(
                network_id as c_uint,
                source_chain_id.as_ptr(),
                source_chain_id.len() as c_uint,
                payload.as_ptr(),
                payload.len() as c_uint,
                &mut msg_ptr,
            );
            
            if ret != 0 || msg_ptr.is_null() {
                return Err(anyhow!("Failed to create unsigned message: {}", 
                    Self::get_last_error()));
            }
            
            // Note: In production, we'd deserialize the actual message from msg_ptr
            // For now, we create it directly
            warp_free(msg_ptr);
        }
        
        UnsignedMessage::new(network_id, source_chain_id.to_vec(), payload.to_vec())
    }
    
    /// Sign a message (placeholder - would use BLS in production)
    pub fn sign_message(&self, msg: &UnsignedMessage, private_key: &[u8]) -> Result<SignedMessage> {
        // In production, this would call the Go BLS signing function
        // For now, we create a mock signature
        use sha2::{Sha256, Digest};
        
        let mut hasher = Sha256::new();
        hasher.update(&msg.id());
        hasher.update(private_key);
        let signature = hasher.finalize().to_vec();
        
        Ok(SignedMessage {
            unsigned: msg.clone(),
            signature,
        })
    }
    
    /// Verify a signed message
    pub fn verify_message(
        &self,
        msg: &SignedMessage,
        validators: &[Vec<u8>],
        threshold: u32,
    ) -> Result<bool> {
        // In production, this would call the Go BLS verification
        // For now, we do a simple check
        if msg.signature.len() != 32 {
            return Ok(false);
        }
        
        // Mock verification - check if we have enough validators
        Ok(validators.len() as u32 >= threshold)
    }
    
    /// Get the last error from the Go side
    unsafe fn get_last_error() -> String {
        let err_ptr = warp_last_error();
        if err_ptr.is_null() {
            return "Unknown error".to_string();
        }
        
        CStr::from_ptr(err_ptr)
            .to_string_lossy()
            .into_owned()
    }
}

/// Pure Rust implementation (when Go is not available)
pub struct WarpProtocolRust {
    network_id: u32,
}

impl WarpProtocolRust {
    /// Create a new Rust-native Warp protocol handler
    pub fn new(network_id: u32) -> Self {
        Self { network_id }
    }
    
    /// Create an unsigned message
    pub fn create_unsigned_message(
        &self,
        source_chain_id: Vec<u8>,
        payload: Vec<u8>,
    ) -> Result<UnsignedMessage> {
        UnsignedMessage::new(self.network_id, source_chain_id, payload)
    }
    
    /// Sign a message using Ed25519 (simplified version)
    pub fn sign_message(
        &self,
        msg: &UnsignedMessage,
        signing_key: &ed25519_dalek::SigningKey,
    ) -> SignedMessage {
        use ed25519_dalek::Signer;
        
        let signature = signing_key.sign(&msg.id());
        
        SignedMessage {
            unsigned: msg.clone(),
            signature: signature.to_bytes().to_vec(),
        }
    }
    
    /// Verify a message signature
    pub fn verify_message(
        &self,
        msg: &SignedMessage,
        verifying_key: &ed25519_dalek::VerifyingKey,
    ) -> bool {
        use ed25519_dalek::{Signature, Verifier};
        
        if msg.signature.len() != 64 {
            return false;
        }
        
        let sig_bytes: [u8; 64] = msg.signature.clone().try_into().unwrap_or([0u8; 64]);
        let signature = Signature::from_bytes(&sig_bytes);
        
        verifying_key.verify(&msg.unsigned.id(), &signature).is_ok()
    }
}

// C-compatible exports for other languages to use our Rust implementation
#[no_mangle]
pub extern "C" fn rust_warp_create_message(
    network_id: u32,
    source_chain_id: *const u8,
    source_chain_id_len: usize,
    payload: *const u8,
    payload_len: usize,
) -> *mut UnsignedMessage {
    if source_chain_id.is_null() || payload.is_null() {
        return ptr::null_mut();
    }
    
    let source_chain = unsafe {
        slice::from_raw_parts(source_chain_id, source_chain_id_len).to_vec()
    };
    
    let payload_data = unsafe {
        slice::from_raw_parts(payload, payload_len).to_vec()
    };
    
    match UnsignedMessage::new(network_id, source_chain, payload_data) {
        Ok(msg) => Box::into_raw(Box::new(msg)),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub extern "C" fn rust_warp_free_message(msg: *mut UnsignedMessage) {
    if !msg.is_null() {
        unsafe {
            let _ = Box::from_raw(msg);
        }
    }
}

#[no_mangle]
pub extern "C" fn rust_warp_message_id(
    msg: *const UnsignedMessage,
    out_id: *mut u8,
    out_id_len: *mut usize,
) -> c_int {
    if msg.is_null() || out_id.is_null() || out_id_len.is_null() {
        return -1;
    }
    
    let msg = unsafe { &*msg };
    let id = msg.id();
    
    unsafe {
        if *out_id_len < id.len() {
            *out_id_len = id.len();
            return -2; // Buffer too small
        }
        
        ptr::copy_nonoverlapping(id.as_ptr(), out_id, id.len());
        *out_id_len = id.len();
    }
    
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_unsigned_message() {
        let chain_id = vec![1u8; 32];
        let payload = b"test payload".to_vec();
        
        let msg = UnsignedMessage::new(43114, chain_id, payload).unwrap();
        assert_eq!(msg.network_id, 43114);
        assert_eq!(msg.source_chain_id.len(), 32);
        
        let id = msg.id();
        assert_eq!(id.len(), 32); // SHA256 hash
    }
    
    #[test]
    fn test_rust_implementation() {
        use ed25519_dalek::SigningKey;
        use rand::rngs::OsRng;
        
        let protocol = WarpProtocolRust::new(43114);
        
        let chain_id = vec![2u8; 32];
        let payload = b"cross-chain message".to_vec();
        
        let msg = protocol.create_unsigned_message(chain_id, payload).unwrap();
        
        // Create key pair
        let signing_key = SigningKey::generate(&mut OsRng);
        let verifying_key = signing_key.verifying_key();
        
        // Sign message
        let signed = protocol.sign_message(&msg, &signing_key);
        
        // Verify signature
        assert!(protocol.verify_message(&signed, &verifying_key));
    }
    
    #[test]
    fn test_invalid_chain_id() {
        let chain_id = vec![1u8; 16]; // Wrong size
        let payload = b"test".to_vec();
        
        let result = UnsignedMessage::new(1, chain_id, payload);
        assert!(result.is_err());
    }
}