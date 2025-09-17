use anyhow::{Context, Result};
use reqwest::{Client, Response};
use serde::{Deserialize, Serialize};
use std::time::Duration;
use tokio::time::timeout;

const DEFAULT_NODE_API_URL: &str = "http://localhost:3690";
const DEFAULT_TIMEOUT_SECS: u64 = 30;

/// Hanzo Node API Client with Qwen3 support
pub struct NodeClient {
    client: Client,
    base_url: String,
    api_key: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ChatRequest {
    pub model: String,
    pub messages: Vec<Message>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temperature: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_tokens: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stream: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thinking: Option<bool>, // Enable Qwen3 thinking mode
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Message {
    pub role: String,
    pub content: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ChatResponse {
    pub id: String,
    pub object: String,
    pub created: u64,
    pub model: String,
    pub choices: Vec<Choice>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub usage: Option<Usage>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thinking_tokens: Option<u32>, // Qwen3 thinking tokens
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Choice {
    pub index: u32,
    pub message: Message,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub finish_reason: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Usage {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub total_tokens: u32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EmbeddingRequest {
    pub model: String,
    pub input: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dimensions: Option<u32>, // Qwen3-Embedding supports 4096 dims
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EmbeddingResponse {
    pub object: String,
    pub data: Vec<EmbeddingData>,
    pub model: String,
    pub usage: Usage,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EmbeddingData {
    pub object: String,
    pub embedding: Vec<f32>,
    pub index: u32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RerankRequest {
    pub model: String,
    pub query: String,
    pub documents: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub top_n: Option<u32>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RerankResponse {
    pub object: String,
    pub data: Vec<RerankResult>,
    pub model: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RerankResult {
    pub index: u32,
    pub score: f32,
    pub document: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ModelInfo {
    pub id: String,
    pub object: String,
    pub created: u64,
    pub owned_by: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<ModelCapabilities>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ModelCapabilities {
    pub max_tokens: u32,
    pub supports_thinking: bool,
    pub supports_vision: bool,
    pub supports_function_calling: bool,
    pub embedding_dimensions: Option<u32>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ModelsResponse {
    pub object: String,
    pub data: Vec<ModelInfo>,
}

impl NodeClient {
    /// Create a new Node API client
    pub fn new(base_url: Option<String>, api_key: Option<String>) -> Result<Self> {
        let base_url = base_url.unwrap_or_else(|| {
            std::env::var("HANZO_NODE_API_URL")
                .unwrap_or_else(|_| DEFAULT_NODE_API_URL.to_string())
        });

        let client = Client::builder()
            .timeout(Duration::from_secs(DEFAULT_TIMEOUT_SECS))
            .build()
            .context("Failed to create HTTP client")?;

        Ok(Self {
            client,
            base_url,
            api_key,
        })
    }

    /// Check if the Node API is healthy
    pub async fn health_check(&self) -> Result<bool> {
        let url = format!("{}/health", self.base_url);
        let response = timeout(
            Duration::from_secs(5),
            self.client.get(&url).send()
        ).await
            .context("Health check timeout")?
            .context("Health check failed")?;

        Ok(response.status().is_success())
    }

    /// List available models
    pub async fn list_models(&self) -> Result<ModelsResponse> {
        let url = format!("{}/v1/models", self.base_url);
        let mut request = self.client.get(&url);
        
        if let Some(api_key) = &self.api_key {
            request = request.header("X-API-Key", api_key);
        }

        let response = request.send().await.context("Failed to list models")?;
        
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            anyhow::bail!("Failed to list models: {} - {}", status, text);
        }

        response.json().await.context("Failed to parse models response")
    }

    /// Send a chat completion request
    pub async fn chat_completion(&self, request: ChatRequest) -> Result<ChatResponse> {
        let url = format!("{}/v1/chat/completions", self.base_url);
        let mut req = self.client.post(&url).json(&request);
        
        if let Some(api_key) = &self.api_key {
            req = req.header("X-API-Key", api_key);
        }

        let response = req.send().await.context("Failed to send chat request")?;
        
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            anyhow::bail!("Chat completion failed: {} - {}", status, text);
        }

        response.json().await.context("Failed to parse chat response")
    }

    /// Generate embeddings using Qwen3-Embedding-8B
    pub async fn create_embeddings(&self, request: EmbeddingRequest) -> Result<EmbeddingResponse> {
        let url = format!("{}/v1/embeddings", self.base_url);
        let mut req = self.client.post(&url).json(&request);
        
        if let Some(api_key) = &self.api_key {
            req = req.header("X-API-Key", api_key);
        }

        let response = req.send().await.context("Failed to create embeddings")?;
        
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            anyhow::bail!("Embedding creation failed: {} - {}", status, text);
        }

        response.json().await.context("Failed to parse embeddings response")
    }

    /// Rerank documents using Qwen3-Reranker-4B
    pub async fn rerank_documents(&self, request: RerankRequest) -> Result<RerankResponse> {
        let url = format!("{}/v1/rerank", self.base_url);
        let mut req = self.client.post(&url).json(&request);
        
        if let Some(api_key) = &self.api_key {
            req = req.header("X-API-Key", api_key);
        }

        let response = req.send().await.context("Failed to rerank documents")?;
        
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            anyhow::bail!("Reranking failed: {} - {}", status, text);
        }

        response.json().await.context("Failed to parse rerank response")
    }

    /// Check if a specific Qwen3 model is available
    pub async fn is_qwen3_available(&self, model: &str) -> Result<bool> {
        let models = self.list_models().await?;
        Ok(models.data.iter().any(|m| m.id == model))
    }

    /// Get Swagger UI URL
    pub fn swagger_ui_url(&self) -> String {
        format!("{}/v2/swagger-ui/", self.base_url)
    }

    /// Create a simple chat message
    pub fn create_message(role: &str, content: &str) -> Message {
        Message {
            role: role.to_string(),
            content: content.to_string(),
        }
    }
}

/// Helper function to create a Qwen3 chat request with thinking enabled
pub fn create_qwen3_request(
    model: &str,
    messages: Vec<Message>,
    enable_thinking: bool,
) -> ChatRequest {
    ChatRequest {
        model: model.to_string(),
        messages,
        temperature: Some(0.7),
        max_tokens: Some(4096),
        stream: Some(false),
        thinking: Some(enable_thinking),
    }
}

/// Helper function to create an embedding request with Qwen3-Embedding-8B
pub fn create_qwen3_embedding_request(texts: Vec<String>) -> EmbeddingRequest {
    EmbeddingRequest {
        model: "qwen3-embedding-8b".to_string(),
        input: texts,
        dimensions: Some(4096),
    }
}

/// Helper function to create a rerank request with Qwen3-Reranker-4B
pub fn create_qwen3_rerank_request(
    query: String,
    documents: Vec<String>,
    top_n: u32,
) -> RerankRequest {
    RerankRequest {
        model: "qwen3-reranker-4b".to_string(),
        query,
        documents,
        top_n: Some(top_n),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_node_client_creation() {
        let client = NodeClient::new(None, None).unwrap();
        assert_eq!(client.base_url, DEFAULT_NODE_API_URL);
    }

    #[tokio::test]
    async fn test_message_creation() {
        let msg = NodeClient::create_message("user", "Hello");
        assert_eq!(msg.role, "user");
        assert_eq!(msg.content, "Hello");
    }

    #[tokio::test]
    async fn test_qwen3_request_creation() {
        let messages = vec![NodeClient::create_message("user", "Test")];
        let request = create_qwen3_request("qwen3-8b", messages, true);
        assert_eq!(request.model, "qwen3-8b");
        assert_eq!(request.thinking, Some(true));
    }
}