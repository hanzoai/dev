//! Proxy to Hanzo Engine (mistral.rs) for real inference

use anyhow::Result;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::process::{Child, Command};
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{info, warn, error};

/// Configuration for the Hanzo Engine
#[derive(Debug, Clone)]
pub struct EngineConfig {
    pub engine_path: String,
    pub port: u16,
    pub model_type: String,
    pub model_id: String,
    pub use_metal: bool,
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            engine_path: "/Users/z/work/hanzo/engine/target/release/hanzoai".to_string(),
            port: 36900,
            model_type: "plain".to_string(),
            model_id: "microsoft/Phi-3.5-mini-instruct".to_string(),
            use_metal: cfg!(target_os = "macos"),
        }
    }
}

/// Manager for the Hanzo Engine process
pub struct EngineManager {
    config: EngineConfig,
    process: Arc<RwLock<Option<Child>>>,
    client: Client,
}

impl EngineManager {
    pub fn new(config: EngineConfig) -> Self {
        Self {
            config,
            process: Arc::new(RwLock::new(None)),
            client: Client::new(),
        }
    }

    /// Start the Hanzo Engine if not already running
    pub async fn ensure_running(&self) -> Result<()> {
        // Check if already running
        if self.is_running().await {
            return Ok(());
        }

        // Check if another instance is already using the port
        let url = format!("http://localhost:{}/health", self.config.port);
        if let Ok(response) = self.client.get(&url).send().await {
            if response.status().is_success() {
                info!("Hanzo Engine already running on port {}", self.config.port);
                return Ok(());
            }
        }

        // Start the engine
        self.start_engine().await
    }

    /// Start the Hanzo Engine process
    async fn start_engine(&self) -> Result<()> {
        let mut process = self.process.write().await;

        if process.is_some() {
            return Ok(());
        }

        info!("Starting Hanzo Engine with Metal/MLX support...");

        let mut cmd = Command::new(&self.config.engine_path);

        // Configure for OpenAI API compatibility
        cmd.arg("--port").arg(self.config.port.to_string());

        // Add the command type (plain, gguf, etc.)
        cmd.arg(&self.config.model_type);

        // Model configuration
        cmd.arg("--model-id").arg(&self.config.model_id);

        // Enable Metal on macOS
        if self.config.use_metal {
            cmd.env("METAL_SHADER_CACHE", "1");
            cmd.env("METAL_DEVICE_MEMORY_LIMIT", "0");
        }

        // Additional optimizations
        cmd.arg("--max-seq-len").arg("4096");

        info!("Starting engine with command: {:?}", cmd);

        let child = cmd.spawn()?;
        *process = Some(child);

        // Wait for engine to be ready
        tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;

        // Verify it's running
        let health_url = format!("http://localhost:{}/health", self.config.port);
        for _ in 0..10 {
            if let Ok(response) = self.client.get(&health_url).send().await {
                if response.status().is_success() {
                    info!("Hanzo Engine started successfully on port {}", self.config.port);
                    return Ok(());
                }
            }
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
        }

        warn!("Engine started but health check failed");
        Ok(())
    }

    /// Check if the engine process is running
    pub async fn is_running(&self) -> bool {
        let mut process = self.process.write().await;
        if let Some(ref mut child) = *process {
            match child.try_wait() {
                Ok(Some(_)) => {
                    // Process has exited
                    *process = None;
                    false
                }
                Ok(None) => {
                    // Still running
                    true
                }
                Err(_) => false,
            }
        } else {
            false
        }
    }

    /// Stop the engine
    pub async fn stop(&self) -> Result<()> {
        let mut process = self.process.write().await;
        if let Some(mut child) = process.take() {
            child.kill()?;
            child.wait()?;
            info!("Hanzo Engine stopped");
        }
        Ok(())
    }

    /// Get the base URL for the engine
    pub fn base_url(&self) -> String {
        format!("http://localhost:{}", self.config.port)
    }
}

/// Proxy for OpenAI-compatible inference
pub struct InferenceProxy {
    engine: Arc<EngineManager>,
    client: Client,
}

impl InferenceProxy {
    pub fn new(engine: Arc<EngineManager>) -> Self {
        Self {
            engine,
            client: Client::new(),
        }
    }

    /// Handle chat completion request
    pub async fn chat_completion(&self, request: Value) -> Result<Value> {
        // Ensure engine is running
        self.engine.ensure_running().await?;

        let url = format!("{}/v1/chat/completions", self.engine.base_url());

        let response = self.client
            .post(&url)
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let error_text = response.text().await?;
            error!("Engine returned error: {}", error_text);
            anyhow::bail!("Inference failed: {}", error_text);
        }

        Ok(response.json().await?)
    }

    /// Handle text completion request
    pub async fn completion(&self, request: Value) -> Result<Value> {
        // Ensure engine is running
        self.engine.ensure_running().await?;

        let url = format!("{}/v1/completions", self.engine.base_url());

        let response = self.client
            .post(&url)
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let error_text = response.text().await?;
            error!("Engine returned error: {}", error_text);
            anyhow::bail!("Completion failed: {}", error_text);
        }

        Ok(response.json().await?)
    }

    /// Handle embedding request - proxies to real Hanzo Engine
    pub async fn embeddings(&self, request: Value) -> Result<Value> {
        // Ensure engine is running
        self.engine.ensure_running().await?;

        let url = format!("{}/v1/embeddings", self.engine.base_url());

        let response = self.client
            .post(&url)
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let error_text = response.text().await?;
            error!("Engine returned error for embeddings: {}", error_text);
            anyhow::bail!("Embeddings failed: {}", error_text);
        }

        Ok(response.json().await?)
    }

    /// Get available models
    pub async fn models(&self) -> Result<Value> {
        // Ensure engine is running
        self.engine.ensure_running().await?;

        let url = format!("{}/v1/models", self.engine.base_url());

        match self.client.get(&url).send().await {
            Ok(response) if response.status().is_success() => {
                Ok(response.json().await?)
            }
            _ => {
                // Return default model list if endpoint not available
                Ok(serde_json::json!({
                    "data": [{
                        "id": self.engine.config.model_id.clone(),
                        "object": "model",
                        "created": 1677649963,
                        "owned_by": "hanzo"
                    }],
                    "object": "list"
                }))
            }
        }
    }
}

/// Create OpenAI-compatible error response
pub fn create_error_response(message: &str, error_type: &str) -> Value {
    serde_json::json!({
        "error": {
            "message": message,
            "type": error_type,
            "param": null,
            "code": null
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_engine_config_default() {
        let config = EngineConfig::default();
        assert_eq!(config.port, 36900);
        assert_eq!(config.model_type, "plain");
        #[cfg(target_os = "macos")]
        assert!(config.use_metal);
        #[cfg(not(target_os = "macos"))]
        assert!(!config.use_metal);
    }
}