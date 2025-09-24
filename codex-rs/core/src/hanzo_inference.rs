use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::process::Command;
use std::sync::Arc;
use tokio::sync::RwLock;

// ===== Configuration =====

/// Configuration for Hanzo services
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HanzoConfig {
    /// Path to hanzod binary
    pub hanzod_path: Option<String>,
    /// HTTP API port
    pub http_port: u16,
    /// gRPC port for advanced operations
    pub grpc_port: u16,
    /// Model configuration
    pub inference_model: String,
    pub embedding_model: String,
    /// Service toggles
    pub enable_inference: bool,
    pub enable_compute: bool,
    pub enable_embeddings: bool,
    pub enable_vector_search: bool,
}

impl Default for HanzoConfig {
    fn default() -> Self {
        Self {
            hanzod_path: Some("/Users/z/work/hanzo/hanzod/target/release/hanzod".to_string()),
            http_port: 8080,
            grpc_port: 50051,
            inference_model: "qwen3-4b-thinking-2507".to_string(),
            embedding_model: "nomic-embed-text".to_string(),
            enable_inference: true,
            enable_compute: true,
            enable_embeddings: true,
            enable_vector_search: true,
        }
    }
}

// ===== Base Trait =====

/// Base trait for Hanzo endpoints
#[async_trait::async_trait]
pub trait HanzoEndpoint: Send + Sync {
    async fn base_url(&self) -> String;

    async fn is_available(&self) -> bool {
        let url = format!("{}/health", self.base_url().await);
        match reqwest::get(&url).await {
            Ok(response) => response.status().is_success(),
            Err(_) => false,
        }
    }
}

// ===== Inference Endpoint (LLM text generation) =====

/// LLM inference endpoint for text generation
pub struct HanzoInferenceEndpoint {
    config: Arc<RwLock<HanzoConfig>>,
    process: Arc<RwLock<Option<std::process::Child>>>,
}

impl HanzoInferenceEndpoint {
    pub fn new(config: Arc<RwLock<HanzoConfig>>) -> Self {
        Self {
            config,
            process: Arc::new(RwLock::new(None)),
        }
    }

    pub async fn start(&self) -> Result<()> {
        let config = self.config.read().await;
        if !config.enable_inference {
            return Ok(());
        }

        if self.process.read().await.is_some() {
            return Ok(());
        }

        let hanzod_path = config.hanzod_path.clone().unwrap_or_else(|| "hanzod".to_string());
        let mut cmd = Command::new(&hanzod_path);
        cmd.env("HANZOD_MODE", "inference");
        cmd.env("HANZOD_MODEL", &config.inference_model);
        cmd.env("HANZOD_PORT", config.http_port.to_string());

        let child = cmd.spawn()?;
        *self.process.write().await = Some(child);
        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;
        Ok(())
    }

    pub async fn stop(&self) -> Result<()> {
        if let Some(mut child) = self.process.write().await.take() {
            child.kill()?;
            child.wait()?;
        }
        Ok(())
    }

    /// Generate text completion
    pub async fn complete(&self, prompt: String, temperature: Option<f32>, max_tokens: Option<u32>) -> Result<String> {
        let url = format!("{}/v1/completions", self.base_url().await);
        let config = self.config.read().await;

        let request = serde_json::json!({
            "model": config.inference_model,
            "prompt": prompt,
            "temperature": temperature.unwrap_or(0.7),
            "max_tokens": max_tokens.unwrap_or(2048),
        });

        let client = reqwest::Client::new();
        let response = client.post(&url).json(&request).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Inference failed: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        Ok(json["choices"][0]["text"].as_str().unwrap_or("").to_string())
    }

    /// Chat completion with messages
    pub async fn chat(&self, messages: Vec<ChatMessage>) -> Result<String> {
        let url = format!("{}/v1/chat/completions", self.base_url().await);
        let config = self.config.read().await;

        let request = serde_json::json!({
            "model": config.inference_model,
            "messages": messages,
        });

        let client = reqwest::Client::new();
        let response = client.post(&url).json(&request).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Chat failed: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        Ok(json["choices"][0]["message"]["content"].as_str().unwrap_or("").to_string())
    }
}

#[async_trait::async_trait]
impl HanzoEndpoint for HanzoInferenceEndpoint {
    async fn base_url(&self) -> String {
        let config = self.config.read().await;
        format!("http://localhost:{}", config.http_port)
    }
}

// ===== Compute Endpoint (container workloads, distributed tasks) =====

/// Compute endpoint for container workloads and distributed tasks
pub struct HanzoComputeEndpoint {
    config: Arc<RwLock<HanzoConfig>>,
    process: Arc<RwLock<Option<std::process::Child>>>,
}

impl HanzoComputeEndpoint {
    pub fn new(config: Arc<RwLock<HanzoConfig>>) -> Self {
        Self {
            config,
            process: Arc::new(RwLock::new(None)),
        }
    }

    pub async fn start(&self) -> Result<()> {
        let config = self.config.read().await;
        if !config.enable_compute {
            return Ok(());
        }

        if self.process.read().await.is_some() {
            return Ok(());
        }

        let hanzod_path = config.hanzod_path.clone().unwrap_or_else(|| "hanzod".to_string());
        let mut cmd = Command::new(&hanzod_path);
        cmd.env("HANZOD_MODE", "compute");
        cmd.env("HANZOD_GRPC_PORT", config.grpc_port.to_string());

        let child = cmd.spawn()?;
        *self.process.write().await = Some(child);
        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;
        Ok(())
    }

    pub async fn stop(&self) -> Result<()> {
        if let Some(mut child) = self.process.write().await.take() {
            child.kill()?;
            child.wait()?;
        }
        Ok(())
    }

    /// Submit a compute job
    pub async fn submit_job(&self, job: ComputeJob) -> Result<String> {
        let url = format!("{}/v1/jobs", self.base_url().await);
        
        let client = reqwest::Client::new();
        let response = client.post(&url).json(&job).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Job submission failed: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        Ok(json["job_id"].as_str().unwrap_or("").to_string())
    }

    /// Get job status
    pub async fn job_status(&self, job_id: &str) -> Result<JobStatus> {
        let url = format!("{}/v1/jobs/{}", self.base_url().await, job_id);
        
        let client = reqwest::Client::new();
        let response = client.get(&url).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to get job status: {}", response.text().await?);
        }

        Ok(response.json().await?)
    }

    /// Schedule a container workload
    pub async fn schedule_container(&self, spec: ContainerSpec) -> Result<String> {
        let url = format!("{}/v1/containers", self.base_url().await);
        
        let client = reqwest::Client::new();
        let response = client.post(&url).json(&spec).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Container scheduling failed: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        Ok(json["container_id"].as_str().unwrap_or("").to_string())
    }
}

#[async_trait::async_trait]
impl HanzoEndpoint for HanzoComputeEndpoint {
    async fn base_url(&self) -> String {
        let config = self.config.read().await;
        format!("http://localhost:{}", config.http_port)
    }
}

// ===== Embedding Endpoint =====

/// Manager for Hanzo embedding endpoint
pub struct HanzoEmbeddingEndpoint {
    config: Arc<RwLock<HanzoConfig>>,
}

impl HanzoEmbeddingEndpoint {
    pub fn new(config: Arc<RwLock<HanzoConfig>>) -> Self {
        Self { config }
    }

    /// Generate embeddings for text
    pub async fn embed(&self, text: &str) -> Result<Vec<f32>> {
        let url = format!("{}/v1/embeddings", self.base_url().await);
        let config = self.config.read().await;

        let request = serde_json::json!({
            "input": text,
            "model": config.embedding_model
        });

        let client = reqwest::Client::new();
        let response = client.post(&url).json(&request).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Embedding generation failed: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        let embedding = json["data"][0]["embedding"]
            .as_array()
            .ok_or_else(|| anyhow::anyhow!("No embedding in response"))?
            .iter()
            .filter_map(|v| v.as_f64().map(|f| f as f32))
            .collect();

        Ok(embedding)
    }

    /// Batch embed multiple texts
    pub async fn embed_batch(&self, texts: Vec<&str>) -> Result<Vec<Vec<f32>>> {
        let url = format!("{}/v1/embeddings", self.base_url().await);
        let config = self.config.read().await;

        let request = serde_json::json!({
            "input": texts,
            "model": config.embedding_model
        });

        let client = reqwest::Client::new();
        let response = client.post(&url).json(&request).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Batch embedding failed: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        let embeddings = json["data"]
            .as_array()
            .ok_or_else(|| anyhow::anyhow!("No data in response"))?
            .iter()
            .filter_map(|item| {
                item["embedding"].as_array().map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_f64().map(|f| f as f32))
                        .collect()
                })
            })
            .collect();

        Ok(embeddings)
    }
}

#[async_trait::async_trait]
impl HanzoEndpoint for HanzoEmbeddingEndpoint {
    async fn base_url(&self) -> String {
        let config = self.config.read().await;
        format!("http://localhost:{}", config.http_port)
    }
}

// ===== Vector Search Endpoint =====

/// Manager for Hanzo vector search endpoint
pub struct HanzoVectorEndpoint {
    config: Arc<RwLock<HanzoConfig>>,
}

impl HanzoVectorEndpoint {
    pub fn new(config: Arc<RwLock<HanzoConfig>>) -> Self {
        Self { config }
    }

    /// Perform vector similarity search
    pub async fn search(&self, query: Vec<f32>, top_k: usize, threshold: Option<f32>) -> Result<Vec<VectorSearchResult>> {
        let url = format!("{}/v1/vector/search", self.base_url().await);

        let request = serde_json::json!({
            "query": query,
            "top_k": top_k,
            "threshold": threshold.unwrap_or(0.7)
        });

        let client = reqwest::Client::new();
        let response = client.post(&url).json(&request).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Vector search failed: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        let results = json["results"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|v| {
                Some(VectorSearchResult {
                    id: v["id"].as_str()?.to_string(),
                    score: v["score"].as_f64()? as f32,
                    metadata: v["metadata"].clone(),
                })
            })
            .collect();

        Ok(results)
    }

    /// Add vectors to the database
    pub async fn add(&self, vectors: Vec<VectorEntry>) -> Result<()> {
        let url = format!("{}/v1/vector/add", self.base_url().await);

        let request = serde_json::json!({
            "vectors": vectors
        });

        let client = reqwest::Client::new();
        let response = client.post(&url).json(&request).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to add vectors: {}", response.text().await?);
        }

        Ok(())
    }

    /// Delete vectors by IDs
    pub async fn delete(&self, ids: Vec<String>) -> Result<()> {
        let url = format!("{}/v1/vector/delete", self.base_url().await);

        let request = serde_json::json!({
            "ids": ids
        });

        let client = reqwest::Client::new();
        let response = client.post(&url).json(&request).send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to delete vectors: {}", response.text().await?);
        }

        Ok(())
    }
}

#[async_trait::async_trait]
impl HanzoEndpoint for HanzoVectorEndpoint {
    async fn base_url(&self) -> String {
        let config = self.config.read().await;
        format!("http://localhost:{}", config.http_port)
    }
}

// ===== Data Types =====

/// Chat message for conversations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatMessage {
    pub role: String,
    pub content: String,
}

/// Compute job specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComputeJob {
    pub name: String,
    pub image: String,
    pub command: Vec<String>,
    pub resources: ResourceRequirements,
    pub env: Option<Vec<EnvVar>>,
}

/// Container specification for workloads
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerSpec {
    pub name: String,
    pub image: String,
    pub command: Vec<String>,
    pub args: Vec<String>,
    pub env: Option<Vec<EnvVar>>,
    pub resources: ResourceRequirements,
}

/// Resource requirements for compute
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceRequirements {
    pub cpu: String,
    pub memory: String,
    pub gpu: Option<u32>,
}

/// Environment variable
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvVar {
    pub name: String,
    pub value: String,
}

/// Job status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobStatus {
    pub job_id: String,
    pub status: String,
    pub message: Option<String>,
    pub started_at: Option<String>,
    pub completed_at: Option<String>,
}

/// Vector search result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VectorSearchResult {
    pub id: String,
    pub score: f32,
    pub metadata: serde_json::Value,
}

/// Vector entry for storage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VectorEntry {
    pub id: String,
    pub vector: Vec<f32>,
    pub metadata: serde_json::Value,
}

// ===== Manager =====

/// Unified Hanzo manager
pub struct HanzoManager {
    config: Arc<RwLock<HanzoConfig>>,
    pub inference: HanzoInferenceEndpoint,
    pub compute: HanzoComputeEndpoint,
    pub embedding: HanzoEmbeddingEndpoint,
    pub vector: HanzoVectorEndpoint,
}

impl HanzoManager {
    pub fn new(config: HanzoConfig) -> Self {
        let config = Arc::new(RwLock::new(config));

        Self {
            inference: HanzoInferenceEndpoint::new(config.clone()),
            compute: HanzoComputeEndpoint::new(config.clone()),
            embedding: HanzoEmbeddingEndpoint::new(config.clone()),
            vector: HanzoVectorEndpoint::new(config.clone()),
            config,
        }
    }

    /// Start enabled services
    pub async fn start_all(&self) -> Result<()> {
        let config = self.config.read().await;

        if config.enable_inference {
            self.inference.start().await?;
        }

        if config.enable_compute {
            self.compute.start().await?;
        }

        Ok(())
    }

    /// Stop all services
    pub async fn stop_all(&self) -> Result<()> {
        self.inference.stop().await?;
        self.compute.stop().await?;
        Ok(())
    }

    /// Check health of all services
    pub async fn health_check(&self) -> HealthStatus {
        let config = self.config.read().await;
        
        HealthStatus {
            inference: config.enable_inference && self.inference.is_available().await,
            compute: config.enable_compute && self.compute.is_available().await,
            embedding: config.enable_embeddings && self.embedding.is_available().await,
            vector: config.enable_vector_search && self.vector.is_available().await,
        }
    }
}

/// Health status for all services
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthStatus {
    pub inference: bool,
    pub compute: bool,
    pub embedding: bool,
    pub vector: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_config_defaults() {
        let config = HanzoConfig::default();
        assert_eq!(config.http_port, 8080);
        assert_eq!(config.grpc_port, 50051);
        assert!(config.enable_inference);
        assert!(config.enable_compute);
    }

    #[tokio::test]
    async fn test_manager_creation() {
        let config = HanzoConfig::default();
        let manager = HanzoManager::new(config);

        assert_eq!(manager.inference.base_url().await, "http://localhost:8080");
        assert_eq!(manager.compute.base_url().await, "http://localhost:8080");
    }

    #[tokio::test]
    async fn test_health_check() {
        let config = HanzoConfig::default();
        let manager = HanzoManager::new(config);

        let health = manager.health_check().await;
        // Services won't be running in test
        assert!(!health.inference);
        assert!(!health.compute);
        assert!(!health.embedding);
        assert!(!health.vector);
    }
}