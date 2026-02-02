//! Storage traits for pluggable backends
//!
//! Provides a unified key-value interface for KMS and IAM persistence
//! with change callbacks for Lux consensus replication.

use crate::Result;
use std::sync::Arc;

/// Callback for state changes (tree, key, value - None means delete)
pub type StateChangeCallback = Arc<dyn Fn(&str, &[u8], Option<&[u8]>) + Send + Sync>;

/// Key-value store trait for KMS/IAM persistence
///
/// This trait abstracts the storage layer, allowing different backends
/// (sled, in-memory, remote) while providing hooks for consensus replication.
pub trait KeyValueStore: Send + Sync {
    /// Get a value by key from a specific tree
    fn kv_get(&self, tree: &str, key: &[u8]) -> Result<Option<Vec<u8>>>;

    /// Put a key-value pair into a specific tree
    fn kv_put(&self, tree: &str, key: &[u8], value: &[u8]) -> Result<()>;

    /// Delete a key from a specific tree, returns true if key existed
    fn kv_delete(&self, tree: &str, key: &[u8]) -> Result<bool>;

    /// List all key-value pairs in a tree
    fn kv_list(&self, tree: &str) -> Result<Vec<(Vec<u8>, Vec<u8>)>>;

    /// Set a callback for state changes (for consensus replication)
    fn set_change_callback(&self, cb: StateChangeCallback);
}
