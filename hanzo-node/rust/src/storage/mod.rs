//! Storage layer for hanzo-node
//!
//! Provides persistent storage for:
//! - Deployment state and metadata
//! - Container information
//! - Node metrics history
//! - Configuration

use crate::{Error, Result};
use serde::{de::DeserializeOwned, Serialize};
use sled::Db;
use std::path::Path;

/// Storage trees (namespaces)
const DEPLOYMENTS_TREE: &str = "deployments";
const CONTAINERS_TREE: &str = "containers";
const METRICS_TREE: &str = "metrics";
const CONFIG_TREE: &str = "config";

/// Persistent storage backed by sled
pub struct Storage {
    db: Db,
}

impl Storage {
    /// Create a new storage instance
    pub fn new<P: AsRef<Path>>(data_dir: P) -> Result<Self> {
        let db_path = data_dir.as_ref().join("hanzo-node.db");
        tracing::info!(path = %db_path.display(), "Opening storage database");

        let db = sled::open(&db_path)?;

        Ok(Self { db })
    }

    /// Get a reference to the underlying database
    pub fn db(&self) -> &Db {
        &self.db
    }

    // ========== Deployment Operations ==========

    /// Store a deployment
    pub fn put_deployment(&self, id: &str, deployment: &Deployment) -> Result<()> {
        let tree = self.db.open_tree(DEPLOYMENTS_TREE)?;
        let value = serde_json::to_vec(deployment)?;
        tree.insert(id, value)?;
        Ok(())
    }

    /// Get a deployment by ID
    pub fn get_deployment(&self, id: &str) -> Result<Option<Deployment>> {
        let tree = self.db.open_tree(DEPLOYMENTS_TREE)?;
        match tree.get(id)? {
            Some(bytes) => {
                let deployment: Deployment = serde_json::from_slice(&bytes)?;
                Ok(Some(deployment))
            }
            None => Ok(None),
        }
    }

    /// List all deployments
    pub fn list_deployments(&self) -> Result<Vec<Deployment>> {
        let tree = self.db.open_tree(DEPLOYMENTS_TREE)?;
        let mut deployments = Vec::new();

        for result in tree.iter() {
            let (_, value) = result?;
            let deployment: Deployment = serde_json::from_slice(&value)?;
            deployments.push(deployment);
        }

        Ok(deployments)
    }

    /// Delete a deployment
    pub fn delete_deployment(&self, id: &str) -> Result<bool> {
        let tree = self.db.open_tree(DEPLOYMENTS_TREE)?;
        let existed = tree.remove(id)?.is_some();
        Ok(existed)
    }

    // ========== Container Operations ==========

    /// Store container info
    pub fn put_container(&self, id: &str, container: &ContainerInfo) -> Result<()> {
        let tree = self.db.open_tree(CONTAINERS_TREE)?;
        let value = serde_json::to_vec(container)?;
        tree.insert(id, value)?;
        Ok(())
    }

    /// Get container info
    pub fn get_container(&self, id: &str) -> Result<Option<ContainerInfo>> {
        self.get_value(CONTAINERS_TREE, id)
    }

    /// List all containers
    pub fn list_containers(&self) -> Result<Vec<ContainerInfo>> {
        let tree = self.db.open_tree(CONTAINERS_TREE)?;
        let mut containers = Vec::new();

        for result in tree.iter() {
            let (_, value) = result?;
            let container: ContainerInfo = serde_json::from_slice(&value)?;
            containers.push(container);
        }

        Ok(containers)
    }

    /// List containers by deployment
    pub fn list_containers_by_deployment(&self, deployment_id: &str) -> Result<Vec<ContainerInfo>> {
        let containers = self.list_containers()?;
        Ok(containers
            .into_iter()
            .filter(|c| c.deployment_id.as_deref() == Some(deployment_id))
            .collect())
    }

    /// Delete container info
    pub fn delete_container(&self, id: &str) -> Result<bool> {
        let tree = self.db.open_tree(CONTAINERS_TREE)?;
        let existed = tree.remove(id)?.is_some();
        Ok(existed)
    }

    // ========== Metrics Operations ==========

    /// Store metrics snapshot
    pub fn put_metrics(&self, timestamp: u64, metrics: &NodeMetrics) -> Result<()> {
        let tree = self.db.open_tree(METRICS_TREE)?;
        let key = timestamp.to_be_bytes();
        let value = serde_json::to_vec(metrics)?;
        tree.insert(key, value)?;

        // Prune old metrics (keep last 24 hours)
        let cutoff = timestamp.saturating_sub(24 * 60 * 60);
        let mut to_remove = Vec::new();
        for result in tree.iter() {
            let (key, _) = result?;
            if key.len() == 8 {
                let ts = u64::from_be_bytes(
                    key.as_ref()
                        .try_into()
                        .map_err(|_| Error::Internal("Invalid key length".to_string()))?,
                );
                if ts < cutoff {
                    to_remove.push(key);
                }
            }
        }
        for key in to_remove {
            tree.remove(key)?;
        }

        Ok(())
    }

    /// Get recent metrics
    pub fn get_recent_metrics(&self, count: usize) -> Result<Vec<(u64, NodeMetrics)>> {
        let tree = self.db.open_tree(METRICS_TREE)?;
        let mut metrics = Vec::new();

        // Iterate in reverse to get most recent first
        for result in tree.iter().rev().take(count) {
            let (key, value) = result?;
            if key.len() == 8 {
                let timestamp = u64::from_be_bytes(
                    key.as_ref()
                        .try_into()
                        .map_err(|_| Error::Internal("Invalid key length".to_string()))?,
                );
                let m: NodeMetrics = serde_json::from_slice(&value)?;
                metrics.push((timestamp, m));
            }
        }

        Ok(metrics)
    }

    // ========== Config Operations ==========

    /// Store config value
    pub fn put_config<T: Serialize>(&self, key: &str, value: &T) -> Result<()> {
        let tree = self.db.open_tree(CONFIG_TREE)?;
        let bytes = serde_json::to_vec(value)?;
        tree.insert(key, bytes)?;
        Ok(())
    }

    /// Get config value
    pub fn get_config<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>> {
        self.get_value(CONFIG_TREE, key)
    }

    // ========== Generic Helpers ==========

    fn get_value<T: DeserializeOwned>(&self, tree_name: &str, key: &str) -> Result<Option<T>> {
        let tree = self.db.open_tree(tree_name)?;
        match tree.get(key)? {
            Some(bytes) => {
                let value: T = serde_json::from_slice(&bytes)?;
                Ok(Some(value))
            }
            None => Ok(None),
        }
    }

    /// Flush all pending writes to disk
    pub fn flush(&self) -> Result<()> {
        self.db.flush()?;
        Ok(())
    }
}

/// Deployment metadata
#[derive(Debug, Clone, Serialize, serde::Deserialize)]
pub struct Deployment {
    pub id: String,
    pub name: String,
    pub status: DeploymentStatus,
    pub spec: serde_json::Value, // ComposeSpec as JSON
    pub environment: std::collections::HashMap<String, String>,
    pub namespace: String,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,
    pub replicas: u32,
    pub ready_replicas: u32,
    pub endpoints: Vec<String>,
    pub labels: std::collections::HashMap<String, String>,
}

/// Deployment status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DeploymentStatus {
    Pending,
    Creating,
    Running,
    Updating,
    Stopping,
    Stopped,
    Failed,
    Removing,
}

impl std::fmt::Display for DeploymentStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeploymentStatus::Pending => write!(f, "pending"),
            DeploymentStatus::Creating => write!(f, "creating"),
            DeploymentStatus::Running => write!(f, "running"),
            DeploymentStatus::Updating => write!(f, "updating"),
            DeploymentStatus::Stopping => write!(f, "stopping"),
            DeploymentStatus::Stopped => write!(f, "stopped"),
            DeploymentStatus::Failed => write!(f, "failed"),
            DeploymentStatus::Removing => write!(f, "removing"),
        }
    }
}

/// Container information
#[derive(Debug, Clone, Serialize, serde::Deserialize)]
pub struct ContainerInfo {
    pub id: String,
    pub name: String,
    pub image: String,
    pub status: String,
    pub state: String,
    pub created: u64,
    pub ports: Vec<String>,
    pub labels: std::collections::HashMap<String, String>,
    pub deployment_id: Option<String>,
}

/// Node metrics snapshot
#[derive(Debug, Clone, Serialize, serde::Deserialize)]
pub struct NodeMetrics {
    pub cpu_usage: f64,
    pub memory_usage: f64,
    pub disk_usage: f64,
    pub network_rx_bytes: f64,
    pub network_tx_bytes: f64,
    pub active_containers: u32,
}

impl Default for NodeMetrics {
    fn default() -> Self {
        Self {
            cpu_usage: 0.0,
            memory_usage: 0.0,
            disk_usage: 0.0,
            network_rx_bytes: 0.0,
            network_tx_bytes: 0.0,
            active_containers: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_storage_deployment_crud() {
        let tmp = TempDir::new().unwrap();
        let storage = Storage::new(tmp.path()).unwrap();

        let deployment = Deployment {
            id: "test-1".to_string(),
            name: "test-deployment".to_string(),
            status: DeploymentStatus::Running,
            spec: serde_json::json!({}),
            environment: std::collections::HashMap::new(),
            namespace: "default".to_string(),
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            replicas: 3,
            ready_replicas: 3,
            endpoints: vec!["http://localhost:8080".to_string()],
            labels: std::collections::HashMap::new(),
        };

        // Create
        storage.put_deployment(&deployment.id, &deployment).unwrap();

        // Read
        let fetched = storage.get_deployment(&deployment.id).unwrap().unwrap();
        assert_eq!(fetched.name, "test-deployment");

        // List
        let all = storage.list_deployments().unwrap();
        assert_eq!(all.len(), 1);

        // Delete
        let deleted = storage.delete_deployment(&deployment.id).unwrap();
        assert!(deleted);

        let none = storage.get_deployment(&deployment.id).unwrap();
        assert!(none.is_none());
    }
}
