//! Consensus integration for Lux replication
//!
//! This module provides the interface between hanzo-node's local storage
//! and the Lux consensus layer for state replication across nodes.
//!
//! Uses ZAP (Zero-copy Application Protocol) for P2P communication,
//! NOT gRPC. ZAP is Lux's native binary wire protocol.

mod lux;
pub mod zap;

pub use lux::LuxReplicator;

use crate::Result;
use std::sync::Arc;
use tracing::debug;

/// Replication request with traits for Lux indexing
#[derive(Debug, Clone)]
pub struct ReplicationRequest {
    pub tree: String,
    pub key: Vec<u8>,
    pub value: Option<Vec<u8>>,
    pub traits: Vec<Vec<u8>>,
}

/// Trait for cross-chain state replication
pub trait StateReplicator: Send + Sync {
    fn apply(&self, requests: Vec<ReplicationRequest>) -> Result<()>;
}

/// Convert tree name to Lux-compatible traits for indexing
fn tree_to_traits(tree: &str) -> Vec<Vec<u8>> {
    match tree {
        "kms:keys" => vec![b"kms".to_vec(), b"keys".to_vec()],
        "iam:apikeys" => vec![b"iam".to_vec(), b"apikeys".to_vec()],
        "iam:roles" => vec![b"iam".to_vec(), b"roles".to_vec()],
        "iam:bindings" => vec![b"iam".to_vec(), b"bindings".to_vec()],
        _ => vec![tree.as_bytes().to_vec()],
    }
}

/// Consensus replicator for Lux network integration
///
/// Receives state change notifications from the storage layer and
/// propagates them to the Lux consensus network for replication.
pub struct ConsensusReplicator {
    replicator: Option<Arc<dyn StateReplicator>>,
}

impl ConsensusReplicator {
    /// Create a new consensus replicator without a backend
    pub fn new() -> Self {
        Self { replicator: None }
    }

    /// Create a new consensus replicator with a state replicator backend
    pub fn with_replicator(replicator: Arc<dyn StateReplicator>) -> Self {
        Self {
            replicator: Some(replicator),
        }
    }

    /// Handle a state change from the storage layer
    ///
    /// This is called whenever a key-value pair is created, updated, or deleted.
    /// The replicator will package the change and submit it to the Lux consensus
    /// network for ordering and replication to other nodes.
    ///
    /// # Arguments
    /// * `tree` - The storage tree (namespace) that changed
    /// * `key` - The key that was modified
    /// * `value` - The new value, or None if the key was deleted
    pub fn on_state_change(&self, tree: &str, key: &[u8], value: Option<&[u8]>) {
        let req = ReplicationRequest {
            tree: tree.to_string(),
            key: key.to_vec(),
            value: value.map(|v| v.to_vec()),
            traits: tree_to_traits(tree),
        };

        if let Some(ref replicator) = self.replicator {
            if let Err(e) = replicator.apply(vec![req]) {
                tracing::error!(error = %e, "Failed to replicate state");
            }
        } else {
            debug!(
                tree,
                key_len = key.len(),
                "State change (no replicator configured)"
            );
        }
    }

    /// Apply a replicated state change from the Lux network
    ///
    /// This is called when a state change from another node is
    /// received through Lux consensus. The change should be applied
    /// to the local storage without triggering another replication.
    #[allow(dead_code)]
    pub fn apply_replicated_change(&self, _tree: &str, _key: &[u8], _value: Option<&[u8]>) {
        // Future implementation will apply changes from other nodes
        // without triggering the change callback (to avoid loops)
    }
}

impl Default for ConsensusReplicator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consensus_replicator_new() {
        let replicator = ConsensusReplicator::new();
        // Should not panic
        replicator.on_state_change("test:tree", b"key", Some(b"value"));
    }

    #[test]
    fn test_consensus_replicator_delete() {
        let replicator = ConsensusReplicator::default();
        // Delete operation (value is None)
        replicator.on_state_change("test:tree", b"key", None);
    }

    #[test]
    fn test_tree_to_traits_kms() {
        let traits = tree_to_traits("kms:keys");
        assert_eq!(traits.len(), 2);
        assert_eq!(traits[0], b"kms".to_vec());
        assert_eq!(traits[1], b"keys".to_vec());
    }

    #[test]
    fn test_tree_to_traits_iam() {
        let traits = tree_to_traits("iam:apikeys");
        assert_eq!(traits.len(), 2);
        assert_eq!(traits[0], b"iam".to_vec());
        assert_eq!(traits[1], b"apikeys".to_vec());
    }

    #[test]
    fn test_tree_to_traits_unknown() {
        let traits = tree_to_traits("custom:tree");
        assert_eq!(traits.len(), 1);
        assert_eq!(traits[0], b"custom:tree".to_vec());
    }

    struct MockReplicator {
        applied: std::sync::Mutex<Vec<ReplicationRequest>>,
    }

    impl MockReplicator {
        fn new() -> Self {
            Self {
                applied: std::sync::Mutex::new(Vec::new()),
            }
        }

        fn applied_count(&self) -> usize {
            self.applied.lock().map(|g| g.len()).unwrap_or(0)
        }
    }

    impl StateReplicator for MockReplicator {
        fn apply(&self, requests: Vec<ReplicationRequest>) -> Result<()> {
            if let Ok(mut applied) = self.applied.lock() {
                applied.extend(requests);
            }
            Ok(())
        }
    }

    #[test]
    fn test_with_replicator() {
        let mock = Arc::new(MockReplicator::new());
        let replicator = ConsensusReplicator::with_replicator(mock.clone());

        replicator.on_state_change("kms:keys", b"key1", Some(b"value1"));
        assert_eq!(mock.applied_count(), 1);

        replicator.on_state_change("iam:roles", b"key2", None);
        assert_eq!(mock.applied_count(), 2);
    }

    #[test]
    fn test_replication_request_structure() {
        let req = ReplicationRequest {
            tree: "test:tree".to_string(),
            key: b"testkey".to_vec(),
            value: Some(b"testvalue".to_vec()),
            traits: vec![b"test".to_vec(), b"tree".to_vec()],
        };

        assert_eq!(req.tree, "test:tree");
        assert_eq!(req.key, b"testkey".to_vec());
        assert_eq!(req.value, Some(b"testvalue".to_vec()));
        assert_eq!(req.traits.len(), 2);
    }
}
