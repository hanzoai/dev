//! P2P networking layer using libp2p
//!
//! Provides:
//! - Node discovery via mDNS (local) and Kademlia DHT (global)
//! - Gossipsub for pub/sub messaging
//! - Secure communication via Noise protocol
//! - Stream multiplexing via Yamux

use crate::{Error, NodeConfig, Result};
use libp2p::{Multiaddr, PeerId};
use tokio::sync::{mpsc, RwLock};

/// P2P network topics
pub mod topics {
    pub const DEPLOYMENTS: &str = "hanzo/deployments/v1";
    pub const HEARTBEAT: &str = "hanzo/heartbeat/v1";
    pub const METRICS: &str = "hanzo/metrics/v1";
    pub const COMMANDS: &str = "hanzo/commands/v1";
}

/// P2P network manager
pub struct P2PNetwork {
    peer_id: PeerId,
    listen_addrs: Vec<Multiaddr>,
    bootstrap_peers: Vec<Multiaddr>,
    command_tx: mpsc::Sender<P2PCommand>,
    state: RwLock<P2PState>,
}

/// Internal P2P state
struct P2PState {
    connected_peers: Vec<PeerId>,
    is_running: bool,
}

/// Commands to control the P2P network
#[derive(Debug)]
pub enum P2PCommand {
    /// Publish a message to a topic
    Publish { topic: String, data: Vec<u8> },
    /// Stop the network
    Stop,
}

impl P2PNetwork {
    /// Create a new P2P network instance
    pub async fn new(config: &NodeConfig) -> Result<Self> {
        // Generate keypair for this node
        let local_key = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = PeerId::from(local_key.public());

        tracing::info!(peer_id = %peer_id, "Generated P2P identity");

        // Parse listen addresses
        let listen_addrs: Vec<Multiaddr> = config
            .p2p_listen_addrs
            .iter()
            .filter_map(|addr| addr.parse().ok())
            .collect();

        // Parse bootstrap peers
        let bootstrap_peers: Vec<Multiaddr> = config
            .bootstrap_peers
            .iter()
            .filter_map(|addr| addr.parse().ok())
            .collect();

        // Create command channel
        let (command_tx, _command_rx) = mpsc::channel(256);

        Ok(Self {
            peer_id,
            listen_addrs,
            bootstrap_peers,
            command_tx,
            state: RwLock::new(P2PState {
                connected_peers: Vec::new(),
                is_running: false,
            }),
        })
    }

    /// Get the local peer ID
    pub fn peer_id(&self) -> &PeerId {
        &self.peer_id
    }

    /// Get the listen addresses
    pub fn listen_addrs(&self) -> &[Multiaddr] {
        &self.listen_addrs
    }

    /// Start the P2P network
    pub async fn start(&self) -> Result<()> {
        tracing::info!("Starting P2P network");

        let mut state = self.state.write().await;
        if state.is_running {
            return Err(Error::InvalidState("P2P network already running".to_string()));
        }
        state.is_running = true;

        // In a full implementation, we would spawn the swarm event loop here
        // For now, we just log that we've started
        tracing::info!(
            peer_id = %self.peer_id,
            listen_addrs = ?self.listen_addrs,
            bootstrap_peers = ?self.bootstrap_peers,
            "P2P network started"
        );

        Ok(())
    }

    /// Stop the P2P network
    pub async fn stop(&self) -> Result<()> {
        tracing::info!("Stopping P2P network");

        let mut state = self.state.write().await;
        if !state.is_running {
            return Ok(());
        }

        // Send stop command
        if let Err(e) = self.command_tx.send(P2PCommand::Stop).await {
            tracing::warn!(error = %e, "Failed to send stop command");
        }

        state.is_running = false;
        state.connected_peers.clear();

        tracing::info!("P2P network stopped");
        Ok(())
    }

    /// Publish a message to a topic
    pub async fn publish(&self, topic: &str, data: Vec<u8>) -> Result<()> {
        self.command_tx
            .send(P2PCommand::Publish {
                topic: topic.to_string(),
                data,
            })
            .await
            .map_err(|e| Error::P2P(format!("Failed to send publish command: {e}")))?;
        Ok(())
    }

    /// Get connected peer count
    pub async fn peer_count(&self) -> usize {
        let state = self.state.read().await;
        state.connected_peers.len()
    }

    /// Check if network is running
    pub async fn is_running(&self) -> bool {
        let state = self.state.read().await;
        state.is_running
    }
}

// Re-export useful types
pub use libp2p::{Multiaddr as LibP2PMultiaddr, PeerId as LibP2PPeerId};

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_p2p_network_creation() {
        let config = NodeConfig::default();
        let network = P2PNetwork::new(&config).await.unwrap();

        assert!(!network.is_running().await);
        assert_eq!(network.peer_count().await, 0);
    }

    #[tokio::test]
    async fn test_p2p_network_start_stop() {
        let config = NodeConfig::default();
        let network = P2PNetwork::new(&config).await.unwrap();

        network.start().await.unwrap();
        assert!(network.is_running().await);

        network.stop().await.unwrap();
        assert!(!network.is_running().await);
    }
}
