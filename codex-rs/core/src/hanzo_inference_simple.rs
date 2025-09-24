use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;

// ===== Configuration =====

/// Simplified configuration for containerized inference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HanzoConfig {
    /// Container inference URL
    pub inference_url: String,
    /// Model name
    pub model: String,
    /// Enable services
    pub enable_inference: bool,
    pub enable_embeddings: bool,
}

impl Default for HanzoConfig {
    fn default() -> Self {
        Self {
            inference_url: "http://localhost:8080".to_string(),
            model: "Qwen/Qwen2.5-3B-Instruct".to_string(),
            enable_inference: true,
            enable_embeddings: true,
        }
    }
}

// ===== Inference Client =====

/// Client for containerized inference service
pub struct HanzoInferenceClient {
    config: Arc<RwLock<HanzoConfig>>,
}

impl HanzoInferenceClient {
    pub fn new(config: HanzoConfig) -> Self {
        Self {
            config: Arc::new(RwLock::new(config)),
        }
    }

    /// Check if inference container is healthy
    pub async fn health_check(&self) -> Result<bool> {
        let config = self.config.read().await;
        let url = format!("{}/health", config.inference_url);

        let response = reqwest::get(&url).await?;
        Ok(response.status().is_success())
    }

    /// Simple inference
    pub async fn inference(&self, prompt: String, temperature: Option<f32>, max_tokens: Option<u32>) -> Result<String> {
        let config = self.config.read().await;
        let url = format!("{}/v1/inference", config.inference_url);

        let request = serde_json::json!({
            "model": config.model,
            "prompt": prompt,
            "temperature": temperature.unwrap_or(0.7),
            "max_tokens": max_tokens.unwrap_or(512),
            "stream": false
        });

        let client = reqwest::Client::new();
        let response = client
            .post(&url)
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let error_text = response.text().await?;
            anyhow::bail!("Inference failed: {}", error_text);
        }

        let json: serde_json::Value = response.json().await?;
        Ok(json["response"].as_str().unwrap_or("").to_string())
    }

    /// Chat completions (OpenAI compatible)
    pub async fn chat_completions(&self, messages: Vec<ChatMessage>) -> Result<ChatResponse> {
        let config = self.config.read().await;
        let url = format!("{}/v1/chat/completions", config.inference_url);

        let request = serde_json::json!({
            "model": config.model,
            "messages": messages,
            "temperature": 0.7,
            "max_tokens": 512,
            "stream": false
        });

        let client = reqwest::Client::new();
        let response = client
            .post(&url)
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let error_text = response.text().await?;
            anyhow::bail!("Chat completion failed: {}", error_text);
        }

        let json: serde_json::Value = response.json().await?;

        Ok(ChatResponse {
            content: json["choices"][0]["message"]["content"]
                .as_str()
                .unwrap_or("")
                .to_string(),
            model: config.model.clone(),
            usage: Usage {
                prompt_tokens: json["usage"]["prompt_tokens"].as_u64().unwrap_or(0) as u32,
                completion_tokens: json["usage"]["completion_tokens"].as_u64().unwrap_or(0) as u32,
                total_tokens: json["usage"]["total_tokens"].as_u64().unwrap_or(0) as u32,
            },
        })
    }

    /// Generate embeddings
    pub async fn embeddings(&self, text: String) -> Result<Vec<f32>> {
        let config = self.config.read().await;
        let url = format!("{}/v1/embeddings", config.inference_url);

        let request = serde_json::json!({
            "text": text
        });

        let client = reqwest::Client::new();
        let response = client
            .post(&url)
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let error_text = response.text().await?;
            anyhow::bail!("Embeddings failed: {}", error_text);
        }

        let json: serde_json::Value = response.json().await?;
        let embedding = json["embedding"]
            .as_array()
            .ok_or_else(|| anyhow::anyhow!("No embedding in response"))?
            .iter()
            .filter_map(|v| v.as_f64().map(|f| f as f32))
            .collect();

        Ok(embedding)
    }
}

// ===== Data Models =====

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatMessage {
    pub role: String,
    pub content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatResponse {
    pub content: String,
    pub model: String,
    pub usage: Usage,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Usage {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub total_tokens: u32,
}

// ===== Tests =====

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_client_creation() {
        let config = HanzoConfig::default();
        let client = HanzoInferenceClient::new(config);

        // This will fail if container isn't running, that's ok for unit test
        let health = client.health_check().await;
        println!("Health check result: {:?}", health);
    }

    #[tokio::test]
    async fn test_inference() {
        let client = HanzoInferenceClient::new(HanzoConfig::default());

        // Only run if container is available
        if client.health_check().await.unwrap_or(false) {
            let result = client.inference(
                "What is 2+2?".to_string(),
                Some(0.7),
                Some(50)
            ).await;

            match result {
                Ok(response) => println!("Response: {}", response),
                Err(e) => println!("Error: {}", e),
            }
        }
    }
}