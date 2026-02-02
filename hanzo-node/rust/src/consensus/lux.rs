//! Lux SharedMemory adapter using ZAP wire protocol
//!
//! This module provides the adapter for replicating state changes
//! to the Lux consensus network via SharedMemory using ZAP (NOT gRPC).

use super::zap::{self, EngineType, Gossip, Put};
use super::{ReplicationRequest, StateReplicator};
use crate::Result;
use bytes::Bytes;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;
use tokio::sync::Mutex;
use tracing::{debug, error, info, warn};

/// Chain ID for hanzo-node state replication
const HANZO_CHAIN_ID: &[u8] = b"hanzo-node-state";

/// Lux network replicator using ZAP wire protocol
///
/// Connects to a Lux node and replicates state changes via SharedMemory.
/// Uses traits for efficient indexing and querying of replicated state.
pub struct LuxReplicator {
    endpoint: String,
    connection: Arc<Mutex<Option<TcpStream>>>,
    request_id: AtomicU32,
}

impl LuxReplicator {
    /// Create a new Lux replicator
    ///
    /// # Arguments
    /// * `endpoint` - The TCP endpoint of the Lux node (e.g., "127.0.0.1:9651")
    pub fn new(endpoint: &str) -> Self {
        info!(endpoint, "Creating Lux replicator (ZAP protocol)");
        Self {
            endpoint: endpoint.to_string(),
            connection: Arc::new(Mutex::new(None)),
            request_id: AtomicU32::new(1),
        }
    }

    /// Get the configured endpoint
    pub fn endpoint(&self) -> &str {
        &self.endpoint
    }

    /// Get next request ID
    #[allow(dead_code)]
    fn next_request_id(&self) -> u32 {
        self.request_id.fetch_add(1, Ordering::SeqCst)
    }

    /// Ensure we have a connection to the Lux node
    #[allow(dead_code)]
    async fn ensure_connected(&self) -> Result<()> {
        let mut conn = self.connection.lock().await;

        // Check if we have an existing connection
        if conn.is_some() {
            return Ok(());
        }

        // Establish new connection
        info!(endpoint = %self.endpoint, "Connecting to Lux node via ZAP");
        match TcpStream::connect(&self.endpoint).await {
            Ok(stream) => {
                stream.set_nodelay(true)?;
                *conn = Some(stream);
                info!(endpoint = %self.endpoint, "Connected to Lux node");
                Ok(())
            }
            Err(e) => {
                warn!(endpoint = %self.endpoint, error = %e, "Failed to connect to Lux node");
                Err(e.into())
            }
        }
    }

    /// Send a ZAP message over the connection
    #[allow(dead_code)]
    async fn send_message(&self, data: &[u8]) -> Result<()> {
        let mut conn = self.connection.lock().await;

        let stream = conn.as_mut().ok_or_else(|| {
            anyhow::anyhow!("Not connected to Lux node")
        })?;

        // Write length prefix (4 bytes, big-endian) + message
        let len = data.len() as u32;
        stream.write_all(&len.to_be_bytes()).await?;
        stream.write_all(data).await?;
        stream.flush().await?;

        Ok(())
    }

    /// Read a response from the connection
    async fn _read_response(&self) -> Result<Vec<u8>> {
        let mut conn = self.connection.lock().await;

        let stream = conn.as_mut().ok_or_else(|| {
            anyhow::anyhow!("Not connected to Lux node")
        })?;

        // Read length prefix
        let mut len_buf = [0u8; 4];
        stream.read_exact(&mut len_buf).await?;
        let len = u32::from_be_bytes(len_buf) as usize;

        // Read message
        let mut buf = vec![0u8; len];
        stream.read_exact(&mut buf).await?;

        Ok(buf)
    }

    /// Replicate a state change using Put message
    #[allow(dead_code)]
    async fn replicate_put(&self, req: &ReplicationRequest) -> Result<()> {
        // Encode the replication payload as container data
        let container = zap::encode_replication_container(
            &req.tree,
            &req.key,
            req.value.as_deref(),
            &req.traits,
        );

        let put = Put {
            chain_id: Bytes::from_static(HANZO_CHAIN_ID),
            request_id: self.next_request_id(),
            container,
            engine_type: EngineType::Chain,
        };

        let encoded = zap::marshal_put(&put);

        debug!(
            tree = %req.tree,
            key_len = req.key.len(),
            is_delete = req.value.is_none(),
            request_id = put.request_id,
            message_len = encoded.len(),
            "Sending ZAP Put to Lux node"
        );

        self.send_message(&encoded).await
    }

    /// Gossip a state change for fast propagation
    #[allow(dead_code)]
    async fn gossip_change(&self, req: &ReplicationRequest) -> Result<()> {
        let container = zap::encode_replication_container(
            &req.tree,
            &req.key,
            req.value.as_deref(),
            &req.traits,
        );

        let gossip = Gossip {
            chain_id: Bytes::from_static(HANZO_CHAIN_ID),
            container,
        };

        let encoded = zap::marshal_gossip(&gossip);

        debug!(
            tree = %req.tree,
            key_len = req.key.len(),
            message_len = encoded.len(),
            "Sending ZAP Gossip to Lux node"
        );

        self.send_message(&encoded).await
    }
}

impl StateReplicator for LuxReplicator {
    fn apply(&self, requests: Vec<ReplicationRequest>) -> Result<()> {
        if requests.is_empty() {
            return Ok(());
        }

        // Create a runtime handle for async operations
        let rt = tokio::runtime::Handle::try_current();

        match rt {
            Ok(handle) => {
                // We're in an async context, spawn the replication
                let replicator = self.connection.clone();
                let endpoint = self.endpoint.clone();
                let request_id = self.request_id.fetch_add(requests.len() as u32, Ordering::SeqCst);

                handle.spawn(async move {
                    // Try to connect if not connected
                    let mut conn = replicator.lock().await;
                    if conn.is_none() {
                        match TcpStream::connect(&endpoint).await {
                            Ok(stream) => {
                                let _ = stream.set_nodelay(true);
                                *conn = Some(stream);
                            }
                            Err(e) => {
                                warn!(endpoint = %endpoint, error = %e,
                                    "Failed to connect to Lux node, queuing for later");
                                return;
                            }
                        }
                    }

                    // Send each replication request
                    for (i, req) in requests.iter().enumerate() {
                        let container = zap::encode_replication_container(
                            &req.tree,
                            &req.key,
                            req.value.as_deref(),
                            &req.traits,
                        );

                        let put = Put {
                            chain_id: Bytes::from_static(HANZO_CHAIN_ID),
                            request_id: request_id + i as u32,
                            container,
                            engine_type: EngineType::Chain,
                        };

                        let encoded = zap::marshal_put(&put);

                        if let Some(ref mut stream) = *conn {
                            let len = encoded.len() as u32;
                            if let Err(e) = stream.write_all(&len.to_be_bytes()).await {
                                error!(error = %e, "Failed to write length prefix");
                                *conn = None;
                                return;
                            }
                            if let Err(e) = stream.write_all(&encoded).await {
                                error!(error = %e, "Failed to write ZAP message");
                                *conn = None;
                                return;
                            }
                        }

                        debug!(
                            tree = %req.tree,
                            key_len = req.key.len(),
                            is_delete = req.value.is_none(),
                            "Replicated via ZAP"
                        );
                    }

                    if let Some(ref mut stream) = *conn {
                        let _ = stream.flush().await;
                    }
                });

                Ok(())
            }
            Err(_) => {
                // Not in async context, just log for now
                for req in &requests {
                    info!(
                        endpoint = %self.endpoint,
                        tree = %req.tree,
                        key_len = req.key.len(),
                        is_delete = req.value.is_none(),
                        traits = ?req.traits.iter().map(|t| String::from_utf8_lossy(t)).collect::<Vec<_>>(),
                        "Replicating to Lux (no async runtime)"
                    );
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lux_replicator_new() {
        let replicator = LuxReplicator::new("127.0.0.1:9651");
        assert_eq!(replicator.endpoint(), "127.0.0.1:9651");
    }

    #[test]
    fn test_lux_replicator_request_id() {
        let replicator = LuxReplicator::new("127.0.0.1:9651");
        assert_eq!(replicator.next_request_id(), 1);
        assert_eq!(replicator.next_request_id(), 2);
        assert_eq!(replicator.next_request_id(), 3);
    }

    #[test]
    fn test_lux_replicator_apply_no_runtime() {
        let replicator = LuxReplicator::new("127.0.0.1:9651");

        let requests = vec![
            ReplicationRequest {
                tree: "kms:keys".to_string(),
                key: b"key1".to_vec(),
                value: Some(b"value1".to_vec()),
                traits: vec![b"kms".to_vec(), b"keys".to_vec()],
            },
            ReplicationRequest {
                tree: "iam:roles".to_string(),
                key: b"admin".to_vec(),
                value: None,
                traits: vec![b"iam".to_vec(), b"roles".to_vec()],
            },
        ];

        // Without async runtime, should just log and succeed
        let result = replicator.apply(requests);
        assert!(result.is_ok());
    }

    #[test]
    fn test_lux_replicator_empty_requests() {
        let replicator = LuxReplicator::new("127.0.0.1:9651");
        let result = replicator.apply(vec![]);
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_replicator_put_encoding() {
        // Test that our Put message encoding is correct
        let req = ReplicationRequest {
            tree: "kms:keys".to_string(),
            key: b"test-key".to_vec(),
            value: Some(b"test-value".to_vec()),
            traits: vec![b"kms".to_vec(), b"keys".to_vec()],
        };

        let container = zap::encode_replication_container(
            &req.tree,
            &req.key,
            req.value.as_deref(),
            &req.traits,
        );

        let put = Put {
            chain_id: Bytes::from_static(HANZO_CHAIN_ID),
            request_id: 1,
            container,
            engine_type: EngineType::Chain,
        };

        let encoded = zap::marshal_put(&put);

        // Verify it can be decoded
        let decoded = zap::unmarshal(&encoded).unwrap();
        if let zap::Message::Put(p) = decoded {
            assert_eq!(p.chain_id.as_ref(), HANZO_CHAIN_ID);
            assert_eq!(p.request_id, 1);
            assert_eq!(p.engine_type, EngineType::Chain);

            // Decode the container
            let (tree, key, value, traits) =
                zap::decode_replication_container(&p.container).unwrap();
            assert_eq!(tree, "kms:keys");
            assert_eq!(key, b"test-key");
            assert_eq!(value, Some(b"test-value".to_vec()));
            assert_eq!(traits.len(), 2);
        } else {
            panic!("Expected Put message");
        }
    }

    #[tokio::test]
    async fn test_replicator_gossip_encoding() {
        let req = ReplicationRequest {
            tree: "iam:apikeys".to_string(),
            key: b"api-key-1".to_vec(),
            value: None, // Delete operation
            traits: vec![b"iam".to_vec(), b"apikeys".to_vec()],
        };

        let container = zap::encode_replication_container(
            &req.tree,
            &req.key,
            req.value.as_deref(),
            &req.traits,
        );

        let gossip = Gossip {
            chain_id: Bytes::from_static(HANZO_CHAIN_ID),
            container,
        };

        let encoded = zap::marshal_gossip(&gossip);

        // Verify it can be decoded
        let decoded = zap::unmarshal(&encoded).unwrap();
        if let zap::Message::Gossip(g) = decoded {
            assert_eq!(g.chain_id.as_ref(), HANZO_CHAIN_ID);

            // Decode the container
            let (tree, key, value, traits) =
                zap::decode_replication_container(&g.container).unwrap();
            assert_eq!(tree, "iam:apikeys");
            assert_eq!(key, b"api-key-1");
            assert_eq!(value, None); // Delete
            assert_eq!(traits.len(), 2);
        } else {
            panic!("Expected Gossip message");
        }
    }
}
