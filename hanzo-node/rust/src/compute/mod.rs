//! Compute orchestration layer
//!
//! Manages container deployments and lifecycle:
//! - Deployment creation and updates
//! - Container lifecycle management
//! - Resource allocation and limits
//! - Health monitoring

use crate::storage::{ContainerInfo, Deployment, DeploymentStatus, NodeMetrics, Storage};
use crate::{Error, Result};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Compute manager for container orchestration
pub struct ComputeManager {
    storage: Arc<Storage>,
    state: RwLock<ComputeState>,
}

/// Internal compute state
struct ComputeState {
    /// Active deployments being managed
    active_deployments: HashMap<String, DeploymentRuntime>,
    /// Whether we're accepting new deployments
    draining: bool,
}

/// Runtime state for a deployment
#[derive(Debug)]
#[allow(dead_code)]
struct DeploymentRuntime {
    deployment_id: String,
    containers: Vec<String>,
    status: DeploymentStatus,
}

impl ComputeManager {
    /// Create a new compute manager
    pub fn new(storage: Arc<Storage>) -> Result<Self> {
        Ok(Self {
            storage,
            state: RwLock::new(ComputeState {
                active_deployments: HashMap::new(),
                draining: false,
            }),
        })
    }

    /// Deploy a new application
    pub async fn deploy(&self, request: DeployRequest) -> Result<DeployResponse> {
        let state = self.state.read().await;
        if state.draining {
            return Err(Error::InvalidState(
                "Node is draining, not accepting new deployments".to_string(),
            ));
        }
        drop(state);

        let deployment_id = request
            .deployment_id
            .filter(|id| !id.is_empty())
            .unwrap_or_else(|| Uuid::new_v4().to_string());

        tracing::info!(
            deployment_id = %deployment_id,
            name = %request.name,
            "Creating deployment"
        );

        // Check if deployment already exists
        if self.storage.get_deployment(&deployment_id)?.is_some() {
            return Err(Error::AlreadyExists(
                "Deployment".to_string(),
                deployment_id,
            ));
        }

        let now = chrono::Utc::now();

        // Create deployment record
        let deployment = Deployment {
            id: deployment_id.clone(),
            name: request.name.clone(),
            status: DeploymentStatus::Creating,
            spec: request.spec.clone(),
            environment: request.environment.clone(),
            namespace: request.namespace.clone().unwrap_or_else(|| "default".to_string()),
            created_at: now,
            updated_at: now,
            replicas: request.replicas.unwrap_or(1),
            ready_replicas: 0,
            endpoints: Vec::new(),
            labels: request.labels.clone().unwrap_or_default(),
        };

        // Store deployment
        self.storage.put_deployment(&deployment_id, &deployment)?;

        // Create containers (simulated for now)
        let containers = self.create_containers(&deployment).await?;

        // Update deployment with endpoints
        let mut deployment = deployment;
        deployment.status = DeploymentStatus::Running;
        deployment.ready_replicas = deployment.replicas;
        deployment.endpoints = containers
            .iter()
            .filter_map(|c| c.ports.first().cloned())
            .collect();

        self.storage.put_deployment(&deployment_id, &deployment)?;

        // Update runtime state
        {
            let mut state = self.state.write().await;
            state.active_deployments.insert(
                deployment_id.clone(),
                DeploymentRuntime {
                    deployment_id: deployment_id.clone(),
                    containers: containers.iter().map(|c| c.id.clone()).collect(),
                    status: DeploymentStatus::Running,
                },
            );
        }

        tracing::info!(
            deployment_id = %deployment_id,
            containers = containers.len(),
            "Deployment created successfully"
        );

        Ok(DeployResponse {
            success: true,
            deployment_id,
            message: "Deployment created successfully".to_string(),
            endpoints: deployment.endpoints,
        })
    }

    /// Stop a deployment
    pub async fn stop(&self, deployment_id: &str, force: bool) -> Result<()> {
        tracing::info!(
            deployment_id = %deployment_id,
            force = force,
            "Stopping deployment"
        );

        let deployment = self
            .storage
            .get_deployment(deployment_id)?
            .ok_or_else(|| Error::NotFound("Deployment".to_string(), deployment_id.to_string()))?;

        // Update status
        let mut deployment = deployment;
        deployment.status = DeploymentStatus::Stopping;
        deployment.updated_at = chrono::Utc::now();
        self.storage.put_deployment(deployment_id, &deployment)?;

        // Stop containers
        let containers = self.storage.list_containers_by_deployment(deployment_id)?;
        for container in containers {
            self.stop_container(&container.id, force).await?;
        }

        // Update final status
        deployment.status = DeploymentStatus::Stopped;
        deployment.ready_replicas = 0;
        deployment.updated_at = chrono::Utc::now();
        self.storage.put_deployment(deployment_id, &deployment)?;

        // Update runtime state
        {
            let mut state = self.state.write().await;
            if let Some(runtime) = state.active_deployments.get_mut(deployment_id) {
                runtime.status = DeploymentStatus::Stopped;
            }
        }

        tracing::info!(deployment_id = %deployment_id, "Deployment stopped");
        Ok(())
    }

    /// Remove a deployment
    pub async fn remove(&self, deployment_id: &str, remove_volumes: bool) -> Result<()> {
        tracing::info!(
            deployment_id = %deployment_id,
            remove_volumes = remove_volumes,
            "Removing deployment"
        );

        // Stop first if running
        if let Some(deployment) = self.storage.get_deployment(deployment_id)? {
            if deployment.status == DeploymentStatus::Running {
                self.stop(deployment_id, true).await?;
            }
        }

        // Remove containers
        let containers = self.storage.list_containers_by_deployment(deployment_id)?;
        for container in containers {
            self.storage.delete_container(&container.id)?;
        }

        // Remove deployment
        self.storage.delete_deployment(deployment_id)?;

        // Update runtime state
        {
            let mut state = self.state.write().await;
            state.active_deployments.remove(deployment_id);
        }

        tracing::info!(deployment_id = %deployment_id, "Deployment removed");
        Ok(())
    }

    /// Scale a deployment
    pub async fn scale(
        &self,
        deployment_id: &str,
        service_name: Option<&str>,
        replicas: u32,
    ) -> Result<ScaleResponse> {
        tracing::info!(
            deployment_id = %deployment_id,
            service = ?service_name,
            replicas = replicas,
            "Scaling deployment"
        );

        let deployment = self
            .storage
            .get_deployment(deployment_id)?
            .ok_or_else(|| Error::NotFound("Deployment".to_string(), deployment_id.to_string()))?;

        let previous_replicas = deployment.replicas;

        // Update deployment
        let mut deployment = deployment;
        deployment.replicas = replicas;
        deployment.ready_replicas = replicas;
        deployment.updated_at = chrono::Utc::now();
        self.storage.put_deployment(deployment_id, &deployment)?;

        tracing::info!(
            deployment_id = %deployment_id,
            previous = previous_replicas,
            current = replicas,
            "Deployment scaled"
        );

        Ok(ScaleResponse {
            success: true,
            message: format!("Scaled from {previous_replicas} to {replicas} replicas"),
            previous_replicas,
            current_replicas: replicas,
        })
    }

    /// Drain the node (stop accepting new work, wait for existing to complete)
    pub async fn drain(&self) -> Result<()> {
        tracing::info!("Draining compute manager");

        let mut state = self.state.write().await;
        state.draining = true;

        // In a real implementation, we would wait for containers to finish
        // For now, we just stop everything
        let deployment_ids: Vec<String> = state
            .active_deployments
            .keys()
            .cloned()
            .collect();
        drop(state);

        for deployment_id in deployment_ids {
            if let Err(e) = self.stop(&deployment_id, false).await {
                tracing::warn!(deployment_id = %deployment_id, error = %e, "Failed to stop deployment during drain");
            }
        }

        tracing::info!("Compute manager drained");
        Ok(())
    }

    /// Get current node metrics
    pub async fn get_metrics(&self) -> NodeMetrics {
        let state = self.state.read().await;
        let active_containers: u32 = state
            .active_deployments
            .values()
            .map(|d| d.containers.len() as u32)
            .sum();

        // In a real implementation, we would gather actual system metrics
        NodeMetrics {
            cpu_usage: 0.0,
            memory_usage: 0.0,
            disk_usage: 0.0,
            network_rx_bytes: 0.0,
            network_tx_bytes: 0.0,
            active_containers,
        }
    }

    /// List all containers
    pub async fn list_containers(&self, all: bool) -> Result<Vec<ContainerInfo>> {
        let containers = self.storage.list_containers()?;
        if all {
            Ok(containers)
        } else {
            Ok(containers
                .into_iter()
                .filter(|c| c.state == "running")
                .collect())
        }
    }

    /// Get container info
    pub async fn get_container(&self, id: &str) -> Result<ContainerInfo> {
        self.storage
            .get_container(id)?
            .ok_or_else(|| Error::NotFound("Container".to_string(), id.to_string()))
    }

    // ========== Internal Helpers ==========

    /// Create containers for a deployment (simulated)
    async fn create_containers(&self, deployment: &Deployment) -> Result<Vec<ContainerInfo>> {
        let mut containers = Vec::new();
        let now = chrono::Utc::now().timestamp() as u64;

        for i in 0..deployment.replicas {
            let container_id = format!("{}-{}", deployment.id, i);
            let container_name = format!("{}-{}", deployment.name, i);

            let container = ContainerInfo {
                id: container_id.clone(),
                name: container_name,
                image: "placeholder:latest".to_string(),
                status: "running".to_string(),
                state: "running".to_string(),
                created: now,
                ports: vec![format!("{}:{}", 8080 + i, 8080)],
                labels: deployment.labels.clone(),
                deployment_id: Some(deployment.id.clone()),
            };

            self.storage.put_container(&container_id, &container)?;
            containers.push(container);
        }

        Ok(containers)
    }

    /// Stop a container
    async fn stop_container(&self, id: &str, _force: bool) -> Result<()> {
        if let Some(mut container) = self.storage.get_container(id)? {
            container.status = "stopped".to_string();
            container.state = "exited".to_string();
            self.storage.put_container(id, &container)?;
        }
        Ok(())
    }
}

/// Deploy request
#[derive(Debug, Clone)]
pub struct DeployRequest {
    pub deployment_id: Option<String>,
    pub name: String,
    pub spec: serde_json::Value,
    pub environment: HashMap<String, String>,
    pub namespace: Option<String>,
    pub replicas: Option<u32>,
    pub labels: Option<HashMap<String, String>>,
}

/// Deploy response
#[derive(Debug, Clone)]
pub struct DeployResponse {
    pub success: bool,
    pub deployment_id: String,
    pub message: String,
    pub endpoints: Vec<String>,
}

/// Scale response
#[derive(Debug, Clone)]
pub struct ScaleResponse {
    pub success: bool,
    pub message: String,
    pub previous_replicas: u32,
    pub current_replicas: u32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_deployment_lifecycle() {
        let tmp = TempDir::new().unwrap();
        let storage = Arc::new(Storage::new(tmp.path()).unwrap());
        let compute = ComputeManager::new(storage).unwrap();

        // Deploy
        let request = DeployRequest {
            deployment_id: Some("test-deploy".to_string()),
            name: "test-app".to_string(),
            spec: serde_json::json!({}),
            environment: HashMap::new(),
            namespace: None,
            replicas: Some(2),
            labels: None,
        };

        let response = compute.deploy(request).await.unwrap();
        assert!(response.success);
        assert_eq!(response.deployment_id, "test-deploy");

        // Scale
        let scale = compute.scale("test-deploy", None, 3).await.unwrap();
        assert!(scale.success);
        assert_eq!(scale.previous_replicas, 2);
        assert_eq!(scale.current_replicas, 3);

        // Stop
        compute.stop("test-deploy", false).await.unwrap();

        // Remove
        compute.remove("test-deploy", false).await.unwrap();
    }
}
