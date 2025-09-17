// Hanzo AI integration for local engine and node support
use anyhow::{Context, Result};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::env;
use std::time::Duration;

// Port configurations
pub const HANZO_ENGINE_PORT: u16 = 36900;
pub const HANZO_NODE_PORT: u16 = 3690;
pub const HANZO_NODE_ALT_PORT: u16 = 3691; // Alternative for node itself

#[derive(Debug, Clone)]
pub struct HanzoConfig {
    pub engine_url: String,
    pub node_url: String,
    pub enable_qwen3: bool,
    pub enable_reranker: bool,
    pub fallback_enabled: bool,
}

impl Default for HanzoConfig {
    fn default() -> Self {
        Self {
            engine_url: format!("http://localhost:{}", HANZO_ENGINE_PORT),
            node_url: format!("http://localhost:{}", HANZO_NODE_PORT),
            enable_qwen3: true,
            enable_reranker: true,
            fallback_enabled: true,
        }
    }
}

impl HanzoConfig {
    pub fn from_env() -> Self {
        let mut config = Self::default();
        
        if let Ok(url) = env::var("HANZO_ENGINE_URL") {
            config.engine_url = url;
        }
        if let Ok(url) = env::var("HANZO_NODE_URL") {
            config.node_url = url;
        }
        if let Ok(val) = env::var("HANZO_ENABLE_QWEN3") {
            config.enable_qwen3 = val.parse().unwrap_or(true);
        }
        if let Ok(val) = env::var("HANZO_ENABLE_RERANKER") {
            config.enable_reranker = val.parse().unwrap_or(true);
        }
        
        config
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Qwen3Request {
    pub model: String,
    pub messages: Vec<Message>,
    pub max_tokens: Option<u32>,
    pub temperature: Option<f32>,
    pub enable_thinking: Option<bool>,
    pub enable_reranker: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Message {
    pub role: String,
    pub content: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Qwen3Response {
    pub choices: Vec<Choice>,
    pub usage: Option<Usage>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Choice {
    pub message: Message,
    pub finish_reason: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Usage {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub total_tokens: u32,
}

pub struct HanzoClient {
    config: HanzoConfig,
    client: Client,
}

impl HanzoClient {
    pub fn new() -> Self {
        Self::with_config(HanzoConfig::from_env())
    }
    
    pub fn with_config(config: HanzoConfig) -> Self {
        let client = Client::builder()
            .timeout(Duration::from_secs(120))
            .build()
            .unwrap_or_default();
            
        Self { config, client }
    }
    
    /// Check if Hanzo engine is running
    pub async fn check_engine_health(&self) -> bool {
        let url = format!("{}/health", self.config.engine_url);
        self.client
            .get(&url)
            .send()
            .await
            .map(|r| r.status().is_success())
            .unwrap_or(false)
    }
    
    /// Check if Hanzo node is running
    pub async fn check_node_health(&self) -> bool {
        let url = format!("{}/health", self.config.node_url);
        self.client
            .get(&url)
            .send()
            .await
            .map(|r| r.status().is_success())
            .unwrap_or(false)
    }
    
    /// Send a request to Qwen3 model
    pub async fn qwen3_completion(&self, request: Qwen3Request) -> Result<Qwen3Response> {
        let url = format!("{}/v1/chat/completions", self.config.engine_url);
        
        let response = self.client
            .post(&url)
            .json(&request)
            .send()
            .await
            .context("Failed to send request to Hanzo engine")?;
            
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            anyhow::bail!("Hanzo engine returned error {}: {}", status, text);
        }
        
        response
            .json()
            .await
            .context("Failed to parse Qwen3 response")
    }
    
    /// Rerank results using Qwen3 reranker
    pub async fn rerank(&self, query: &str, documents: Vec<String>) -> Result<Vec<(usize, f32)>> {
        #[derive(Serialize)]
        struct RerankRequest {
            model: String,
            query: String,
            documents: Vec<String>,
        }
        
        #[derive(Deserialize)]
        struct RerankResponse {
            results: Vec<RerankResult>,
        }
        
        #[derive(Deserialize)]
        struct RerankResult {
            index: usize,
            score: f32,
        }
        
        let url = format!("{}/v1/rerank", self.config.engine_url);
        let request = RerankRequest {
            model: "Qwen/Qwen3-Reranker".to_string(),
            query: query.to_string(),
            documents,
        };
        
        let response = self.client
            .post(&url)
            .json(&request)
            .send()
            .await
            .context("Failed to send rerank request")?;
            
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            anyhow::bail!("Reranker returned error {}: {}", status, text);
        }
        
        let result: RerankResponse = response
            .json()
            .await
            .context("Failed to parse rerank response")?;
            
        Ok(result.results.into_iter().map(|r| (r.index, r.score)).collect())
    }
}

/// Provider detection and fallback chain
pub enum LLMProvider {
    HanzoLocal,
    OpenAI,
    Claude,
    ApiKey(String),
}

impl LLMProvider {
    /// Detect available providers with fallback chain
    pub async fn detect() -> Vec<Self> {
        let mut providers = Vec::new();
        
        // Check local Hanzo services
        let hanzo_client = HanzoClient::new();
        if hanzo_client.check_engine_health().await || hanzo_client.check_node_health().await {
            providers.push(LLMProvider::HanzoLocal);
        }
        
        // Check for OpenAI config
        if env::var("OPENAI_API_KEY").is_ok() {
            providers.push(LLMProvider::OpenAI);
        }
        
        // Check for Claude config
        if env::var("ANTHROPIC_API_KEY").is_ok() {
            providers.push(LLMProvider::Claude);
        }
        
        // Check for other API keys
        for (key, value) in env::vars() {
            if key.ends_with("_API_KEY") && !key.starts_with("OPENAI") && !key.starts_with("ANTHROPIC") {
                providers.push(LLMProvider::ApiKey(value));
            }
        }
        
        providers
    }
    
    pub fn name(&self) -> &str {
        match self {
            LLMProvider::HanzoLocal => "Hanzo Local (Qwen3)",
            LLMProvider::OpenAI => "OpenAI",
            LLMProvider::Claude => "Claude",
            LLMProvider::ApiKey(key) => {
                if key.contains("qwen") || key.contains("dashscope") {
                    "Qwen/DashScope"
                } else {
                    "Custom API"
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_provider_detection() {
        let providers = LLMProvider::detect().await;
        assert!(!providers.is_empty(), "Should detect at least one provider");
        
        for provider in &providers {
            println!("Detected provider: {}", provider.name());
        }
    }
    
    #[tokio::test]
    async fn test_hanzo_health_check() {
        let client = HanzoClient::new();
        
        let engine_healthy = client.check_engine_health().await;
        let node_healthy = client.check_node_health().await;
        
        println!("Hanzo Engine (port {}): {}", HANZO_ENGINE_PORT, if engine_healthy { "✓" } else { "✗" });
        println!("Hanzo Node (port {}): {}", HANZO_NODE_PORT, if node_healthy { "✓" } else { "✗" });
    }
}