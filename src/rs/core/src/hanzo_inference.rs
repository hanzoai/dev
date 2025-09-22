use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::process::Command;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Configuration for Hanzo local inference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HanzoInferenceConfig {
    /// Path to hanzod binary
    pub hanzod_path: Option<String>,
    /// Port for local inference server
    pub inference_port: u16,
    /// Enable local embeddings
    pub enable_embeddings: bool,
    /// Enable vector search
    pub enable_vector_search: bool,
    /// Model to use for inference
    pub model: String,
}

impl Default for HanzoInferenceConfig {
    fn default() -> Self {
        Self {
            hanzod_path: Some("/Users/z/work/hanzo/node/target/release/hanzod".to_string()),
            inference_port: 11434,
            enable_embeddings: true,
            enable_vector_search: true,
            model: "gpt-oss:20b".to_string(),
        }
    }
}

/// Manager for Hanzo local inference
pub struct HanzoInferenceManager {
    config: Arc<RwLock<HanzoInferenceConfig>>,
    hanzod_process: Arc<RwLock<Option<std::process::Child>>>,
}

impl HanzoInferenceManager {
    pub fn new(config: HanzoInferenceConfig) -> Self {
        Self {
            config: Arc::new(RwLock::new(config)),
            hanzod_process: Arc::new(RwLock::new(None)),
        }
    }

    /// Start the hanzod process for local inference
    pub async fn start(&self) -> Result<()> {
        let config = self.config.read().await;

        // Check if hanzod is already running
        if self.hanzod_process.read().await.is_some() {
            return Ok(());
        }

        let hanzod_path = config.hanzod_path.clone().unwrap_or_else(|| {
            // Try to find hanzod in PATH or use default location
            "hanzod".to_string()
        });

        // Start hanzod with appropriate arguments
        let mut cmd = Command::new(&hanzod_path);
        cmd.arg("--port").arg(config.inference_port.to_string());

        if config.enable_embeddings {
            cmd.arg("--enable-embeddings");
        }

        if config.enable_vector_search {
            cmd.arg("--enable-vector-search");
        }

        cmd.arg("--model").arg(&config.model);

        let child = cmd.spawn()?;
        *self.hanzod_process.write().await = Some(child);

        // Wait for server to be ready
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        Ok(())
    }

    /// Stop the hanzod process
    pub async fn stop(&self) -> Result<()> {
        if let Some(mut child) = self.hanzod_process.write().await.take() {
            child.kill()?;
            child.wait()?;
        }
        Ok(())
    }

    /// Get the inference URL
    pub async fn get_inference_url(&self) -> String {
        let config = self.config.read().await;
        format!("http://localhost:{}/v1", config.inference_port)
    }

    /// Check if local inference is available
    pub async fn is_available(&self) -> bool {
        let url = format!("{}/health", self.get_inference_url().await);
        match reqwest::get(&url).await {
            Ok(response) => response.status().is_success(),
            Err(_) => false,
        }
    }

    /// Perform inference using local hanzod
    pub async fn inference(&self, prompt: String) -> Result<String> {
        let url = format!("{}/chat/completions", self.get_inference_url().await);
        let config = self.config.read().await;

        let request = serde_json::json!({
            "model": config.model,
            "messages": [
                {
                    "role": "user",
                    "content": prompt
                }
            ]
        });

        let client = reqwest::Client::new();
        let response = client
            .post(&url)
            .json(&request)
            .send()
            .await?;

        let json: serde_json::Value = response.json().await?;
        let content = json["choices"][0]["message"]["content"]
            .as_str()
            .unwrap_or("")
            .to_string();

        Ok(content)
    }

    /// Generate embeddings locally
    pub async fn embed(&self, text: &str) -> Result<Vec<f32>> {
        let url = format!("{}/embeddings", self.get_inference_url().await);

        let request = serde_json::json!({
            "input": text,
            "model": "text-embedding-ada-002"
        });

        let client = reqwest::Client::new();
        let response = client
            .post(&url)
            .json(&request)
            .send()
            .await?;

        let json: serde_json::Value = response.json().await?;
        let embedding = json["data"][0]["embedding"]
            .as_array()
            .ok_or_else(|| anyhow::anyhow!("No embedding in response"))?
            .iter()
            .filter_map(|v| v.as_f64().map(|f| f as f32))
            .collect();

        Ok(embedding)
    }

    /// Perform vector search
    pub async fn vector_search(&self, query: Vec<f32>, top_k: usize) -> Result<Vec<String>> {
        let url = format!("{}/vector/search", self.get_inference_url().await);

        let request = serde_json::json!({
            "query": query,
            "top_k": top_k
        });

        let client = reqwest::Client::new();
        let response = client
            .post(&url)
            .json(&request)
            .send()
            .await?;

        let json: serde_json::Value = response.json().await?;
        let results = json["results"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|v| v["text"].as_str().map(|s| s.to_string()))
            .collect();

        Ok(results)
    }
}

impl Drop for HanzoInferenceManager {
    fn drop(&mut self) {
        // Stop hanzod when manager is dropped
        let hanzod_process = self.hanzod_process.clone();
        tokio::spawn(async move {
            if let Some(mut child) = hanzod_process.write().await.take() {
                let _ = child.kill();
                let _ = child.wait();
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_hanzo_inference_config() {
        let config = HanzoInferenceConfig::default();
        assert_eq!(config.inference_port, 11434);
        assert!(config.enable_embeddings);
        assert!(config.enable_vector_search);
        assert_eq!(config.model, "gpt-oss:20b");
    }

    #[tokio::test]
    async fn test_inference_manager() {
        let config = HanzoInferenceConfig::default();
        let manager = HanzoInferenceManager::new(config);

        // Test URL generation
        let url = manager.get_inference_url().await;
        assert_eq!(url, "http://localhost:11434/v1");
    }
}