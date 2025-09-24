//! Workload manager for scheduling containers across different runtimes

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;
use chrono::Utc;

use crate::runtime_detection::{
    RuntimeProvider, ContainerConfig,
    SandboxType,
};

/// Workload type for container scheduling
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WorkloadType {
    /// AI inference workload
    Inference {
        model: String,
        engine: String, // ollama, vllm, hanzo-engine
    },
    /// Embedding generation workload
    Embedding {
        model: String,
    },
    /// Vector database workload
    VectorDB {
        engine: String, // qdrant, weaviate, chroma
    },
    /// Blockchain node workload
    Blockchain {
        chain: String, // lux, ethereum, bitcoin
    },
    /// General compute workload
    Compute {
        image: String,
        command: Vec<String>,
    },
}

/// Workload scheduling request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkloadRequest {
    pub id: String,
    pub workload_type: WorkloadType,
    pub resources: ResourceRequirements,
    pub sandbox_type: Option<SandboxType>,
}

/// Resource requirements for workload
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceRequirements {
    pub memory_mb: Option<u64>,
    pub cpu_cores: Option<f64>,
    pub gpu: Option<bool>,
    pub storage_gb: Option<u64>,
}

/// Workload status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkloadStatus {
    pub id: String,
    pub container_id: Option<String>,
    pub runtime_type: Option<String>,
    pub status: String,
    pub created_at: String,
    pub started_at: Option<String>,
    pub completed_at: Option<String>,
    pub error: Option<String>,
}

/// Workload manager that schedules containers across runtimes
pub struct WorkloadManager {
    runtime_providers: Arc<Vec<Arc<dyn RuntimeProvider>>>,
    workloads: Arc<RwLock<Vec<WorkloadStatus>>>,
    sled_db: Arc<sled::Db>,
}

impl WorkloadManager {
    /// Create a new workload manager
    pub fn new(
        runtime_providers: Arc<Vec<Arc<dyn RuntimeProvider>>>,
        sled_db: Arc<sled::Db>,
    ) -> Self {
        Self {
            runtime_providers,
            workloads: Arc::new(RwLock::new(Vec::new())),
            sled_db,
        }
    }

    /// Schedule a workload on the best available runtime
    pub async fn schedule_workload(&self, request: WorkloadRequest) -> Result<WorkloadStatus> {
        // Find suitable runtime based on requirements
        let runtime = self.select_runtime(&request).await?;

        // Create container configuration
        let config = self.build_container_config(&request)?;

        // Determine container image based on workload type
        let image = self.get_container_image(&request.workload_type)?;

        // Generate container name
        let container_name = format!("hanzod-{}", request.id);

        // Create workload status
        let mut status = WorkloadStatus {
            id: request.id.clone(),
            container_id: None,
            runtime_type: Some(format!("{:?}", runtime.info().runtime_type)),
            status: "creating".to_string(),
            created_at: Utc::now().to_rfc3339(),
            started_at: None,
            completed_at: None,
            error: None,
        };

        // Store initial status
        self.store_workload_status(&status)?;

        // Create container
        match runtime.create_container(&image, &container_name, config).await {
            Ok(container_id) => {
                status.container_id = Some(container_id.clone());
                status.status = "created".to_string();

                // Start container
                match runtime.start_container(&container_id).await {
                    Ok(_) => {
                        status.status = "running".to_string();
                        status.started_at = Some(Utc::now().to_rfc3339());
                    }
                    Err(e) => {
                        status.status = "failed".to_string();
                        status.error = Some(format!("Failed to start container: {}", e));
                    }
                }
            }
            Err(e) => {
                status.status = "failed".to_string();
                status.error = Some(format!("Failed to create container: {}", e));
            }
        }

        // Update status
        self.store_workload_status(&status)?;
        self.workloads.write().await.push(status.clone());

        Ok(status)
    }

    /// Select the best runtime for a workload
    async fn select_runtime(&self, request: &WorkloadRequest) -> Result<&Arc<dyn RuntimeProvider>> {
        // Filter runtimes based on requirements
        for provider in self.runtime_providers.iter() {
            let info = provider.info();

            // Check if runtime is active
            if !info.is_active {
                continue;
            }

            // Check sandbox type compatibility
            if let Some(sandbox_type) = &request.sandbox_type {
                match sandbox_type {
                    SandboxType::MicroVM if !info.capabilities.supports_microvm => continue,
                    SandboxType::Wasm if !info.capabilities.supports_wasm => continue,
                    _ => {}
                }
            }

            // Check GPU requirement
            if request.resources.gpu == Some(true) && !info.capabilities.supports_gpu {
                continue;
            }

            // Check if runtime is healthy
            if provider.health_check().await.unwrap_or(false) {
                return Ok(provider);
            }
        }

        Err(anyhow!("No suitable runtime found for workload"))
    }

    /// Build container configuration from workload request
    fn build_container_config(&self, request: &WorkloadRequest) -> Result<ContainerConfig> {
        let mut env = std::collections::HashMap::new();

        // Add workload-specific environment variables
        match &request.workload_type {
            WorkloadType::Inference { model, engine } => {
                env.insert("MODEL".to_string(), model.clone());
                env.insert("ENGINE".to_string(), engine.clone());
            }
            WorkloadType::Embedding { model } => {
                env.insert("EMBEDDING_MODEL".to_string(), model.clone());
            }
            WorkloadType::VectorDB { engine } => {
                env.insert("VECTOR_ENGINE".to_string(), engine.clone());
            }
            WorkloadType::Blockchain { chain } => {
                env.insert("CHAIN".to_string(), chain.clone());
            }
            WorkloadType::Compute { .. } => {}
        }

        Ok(ContainerConfig {
            env,
            mounts: Vec::new(),
            memory_limit: request.resources.memory_mb.map(|mb| mb * 1024 * 1024),
            cpu_limit: request.resources.cpu_cores,
            sandbox_type: request.sandbox_type.clone(),
        })
    }

    /// Get container image for workload type
    fn get_container_image(&self, workload_type: &WorkloadType) -> Result<String> {
        Ok(match workload_type {
            WorkloadType::Inference { engine, .. } => {
                match engine.as_str() {
                    "ollama" => "ollama/ollama:latest",
                    "vllm" => "vllm/vllm-openai:latest",
                    "hanzo-engine" => "hanzoai/engine:latest",
                    _ => "ollama/ollama:latest",
                }
            }
            WorkloadType::Embedding { .. } => {
                "hanzoai/embeddings:latest"
            }
            WorkloadType::VectorDB { engine } => {
                match engine.as_str() {
                    "qdrant" => "qdrant/qdrant:latest",
                    "weaviate" => "semitechnologies/weaviate:latest",
                    "chroma" => "chromadb/chroma:latest",
                    _ => "qdrant/qdrant:latest",
                }
            }
            WorkloadType::Blockchain { chain } => {
                match chain.as_str() {
                    "lux" => "luxfi/node:latest",
                    "ethereum" => "ethereum/client-go:latest",
                    "bitcoin" => "lncm/bitcoind:latest",
                    _ => return Err(anyhow!("Unknown blockchain: {}", chain)),
                }
            }
            WorkloadType::Compute { image, .. } => {
                image
            }
        }.to_string())
    }

    /// Store workload status in sled database
    fn store_workload_status(&self, status: &WorkloadStatus) -> Result<()> {
        let key = format!("workload_{}", status.id);
        let value = serde_json::to_vec(status)?;
        self.sled_db.insert(key.as_bytes(), value)?;
        self.sled_db.flush()?;
        Ok(())
    }

    /// Get status of a workload
    pub async fn get_workload_status(&self, id: &str) -> Result<Option<WorkloadStatus>> {
        let key = format!("workload_{}", id);
        if let Ok(Some(value)) = self.sled_db.get(key.as_bytes()) {
            let status: WorkloadStatus = serde_json::from_slice(value.as_ref())?;
            return Ok(Some(status));
        }

        // Check in-memory list
        let workloads = self.workloads.read().await;
        Ok(workloads.iter().find(|w| w.id == id).cloned())
    }

    /// List all workloads
    pub async fn list_workloads(&self) -> Result<Vec<WorkloadStatus>> {
        let workloads = self.workloads.read().await;
        Ok(workloads.clone())
    }

    /// Stop a workload
    #[allow(dead_code)]
    pub async fn stop_workload(&self, id: &str) -> Result<()> {
        // Get workload status
        let mut status = self.get_workload_status(id).await?
            .ok_or_else(|| anyhow!("Workload not found"))?;

        // Find the runtime that's running this workload
        if let Some(container_id) = &status.container_id {
            for provider in self.runtime_providers.iter() {
                // Try to stop on each provider (only the right one will succeed)
                if provider.stop_container(container_id).await.is_ok() {
                    status.status = "stopped".to_string();
                    status.completed_at = Some(Utc::now().to_rfc3339());
                    self.store_workload_status(&status)?;

                    // Update in-memory list
                    let mut workloads = self.workloads.write().await;
                    if let Some(w) = workloads.iter_mut().find(|w| w.id == id) {
                        *w = status;
                    }

                    return Ok(());
                }
            }
        }

        Err(anyhow!("Failed to stop workload"))
    }

    /// Execute command in workload container
    #[allow(dead_code)]
    pub async fn exec_in_workload(&self, id: &str, command: Vec<String>) -> Result<String> {
        // Get workload status
        let status = self.get_workload_status(id).await?
            .ok_or_else(|| anyhow!("Workload not found"))?;

        // Find container ID
        let container_id = status.container_id
            .ok_or_else(|| anyhow!("Workload has no container"))?;

        // Execute on the appropriate runtime
        for provider in self.runtime_providers.iter() {
            // Try exec on each provider
            if let Ok(output) = provider.exec_in_container(&container_id, command.clone()).await {
                return Ok(output);
            }
        }

        Err(anyhow!("Failed to execute command in workload"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime_detection::RuntimeDetector;
    use std::path::PathBuf;

    #[tokio::test]
    async fn test_workload_manager() {
        // Setup
        let detector = RuntimeDetector::new().await.unwrap();
        let providers: Vec<Arc<dyn RuntimeProvider>> = Vec::new();
        let sled_db = Arc::new(sled::open("/tmp/test_workload_db").unwrap());

        let manager = WorkloadManager::new(Arc::new(providers), sled_db);

        // Create a test workload request
        let request = WorkloadRequest {
            id: uuid::Uuid::new_v4().to_string(),
            workload_type: WorkloadType::Inference {
                model: "llama3:8b".to_string(),
                engine: "ollama".to_string(),
            },
            resources: ResourceRequirements {
                memory_mb: Some(8192),
                cpu_cores: Some(4.0),
                gpu: Some(false),
                storage_gb: None,
            },
            sandbox_type: None,
        };

        println!("Created test workload request: {:?}", request);
    }
}