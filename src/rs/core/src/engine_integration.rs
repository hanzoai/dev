use anyhow::{Context, Result};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::process::{Command, Stdio};
use std::time::Duration;
use tokio::time::timeout;

const ENGINE_PORT: u16 = 36900;
const ENGINE_HOST: &str = "127.0.0.1";
const ENGINE_TIMEOUT_SECS: u64 = 120;

/// Native Hanzo Engine integration for Qwen models
pub struct HanzoEngine {
    client: Client,
    base_url: String,
    engine_process: Option<std::process::Child>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct QwenRequest {
    pub model: String,
    pub messages: Vec<Message>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_tokens: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temperature: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub top_p: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stream: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Message {
    pub role: String,
    pub content: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct QwenResponse {
    pub id: String,
    pub object: String,
    pub created: u64,
    pub model: String,
    pub choices: Vec<Choice>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Choice {
    pub index: u32,
    pub message: Message,
    pub finish_reason: Option<String>,
}

impl HanzoEngine {
    /// Create a new Hanzo Engine instance
    pub fn new() -> Result<Self> {
        let client = Client::builder()
            .timeout(Duration::from_secs(ENGINE_TIMEOUT_SECS))
            .build()
            .context("Failed to create HTTP client")?;

        let base_url = format!("http://{}:{}", ENGINE_HOST, ENGINE_PORT);

        Ok(Self {
            client,
            base_url,
            engine_process: None,
        })
    }

    /// Start the native Hanzo engine process
    pub fn start_engine(&mut self) -> Result<()> {
        println!("Starting native Hanzo Engine...");

        // Check if engine is already running
        if self.is_running().unwrap_or(false) {
            println!("Engine already running on port {}", ENGINE_PORT);
            return Ok(());
        }

        // Find engine binary
        let engine_paths = vec![
            "~/work/hanzo/engine/target/release/mistralrs-server",
            "~/work/hanzo/engine/target/release/hanzo-engine",
            "/usr/local/bin/hanzo-engine",
            "./hanzo-engine",
        ];

        let mut engine_path = None;
        for path in &engine_paths {
            let expanded = shellexpand::tilde(path);
            if std::path::Path::new(expanded.as_ref()).exists() {
                engine_path = Some(expanded.to_string());
                break;
            }
        }

        let engine_bin = engine_path
            .ok_or_else(|| anyhow::anyhow!("Hanzo engine binary not found"))?;

        println!("Found engine at: {}", engine_bin);

        // Start the engine with Qwen model
        let child = Command::new(&engine_bin)
            .args(&[
                "--port", &ENGINE_PORT.to_string(),
                "--model-id", "Qwen/Qwen2.5-7B-Instruct",
                "--arch", "qwen2",
                "--device", "metal",
                "--dtype", "auto",
                "--from-huggingface",
            ])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to start engine")?;

        self.engine_process = Some(child);

        // Wait for engine to be ready
        for _ in 0..30 {
            std::thread::sleep(Duration::from_secs(2));
            if self.is_running().unwrap_or(false) {
                println!("✓ Engine started successfully on port {}", ENGINE_PORT);
                return Ok(());
            }
        }

        Err(anyhow::anyhow!("Engine failed to start within timeout"))
    }

    /// Check if the engine is running
    pub fn is_running(&self) -> Result<bool> {
        let url = format!("{}/health", self.base_url);
        match self.client.get(&url).send() {
            Ok(resp) => Ok(resp.status().is_success()),
            Err(_) => Ok(false),
        }
    }

    /// Send a completion request to the engine
    pub async fn completion(&self, request: QwenRequest) -> Result<QwenResponse> {
        let url = format!("{}/v1/chat/completions", self.base_url);
        
        let response = self.client
            .post(&url)
            .json(&request)
            .send()
            .await
            .context("Failed to send request to engine")?;

        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            anyhow::bail!("Engine request failed: {} - {}", status, text);
        }

        response.json().await
            .context("Failed to parse engine response")
    }

    /// Download a Qwen model natively
    pub async fn download_model(&self, model_id: &str) -> Result<()> {
        println!("Downloading model {} natively...", model_id);
        
        // Use Python script to download from Hugging Face
        let script = format!(r#"
import os
from huggingface_hub import snapshot_download

model_id = "{}"
cache_dir = os.path.expanduser("~/.cache/huggingface/hub")

print(f"Downloading {{model_id}}...")
path = snapshot_download(
    repo_id=model_id,
    cache_dir=cache_dir,
    ignore_patterns=["*.bin", "*.pth"],
    resume_download=True
)
print(f"Downloaded to: {{path}}")
"#, model_id);

        let output = Command::new("python3")
            .arg("-c")
            .arg(&script)
            .output()
            .context("Failed to run download script")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Model download failed: {}", stderr);
        }

        println!("✓ Model downloaded successfully");
        Ok(())
    }

    /// List available models
    pub async fn list_models(&self) -> Result<Vec<String>> {
        let url = format!("{}/v1/models", self.base_url);
        
        #[derive(Deserialize)]
        struct ModelsResponse {
            data: Vec<ModelInfo>,
        }
        
        #[derive(Deserialize)]
        struct ModelInfo {
            id: String,
        }

        let response = self.client
            .get(&url)
            .send()
            .await
            .context("Failed to list models")?;

        if !response.status().is_success() {
            return Ok(vec!["qwen2.5-7b".to_string()]); // Default model
        }

        let models: ModelsResponse = response.json().await?;
        Ok(models.data.into_iter().map(|m| m.id).collect())
    }

    /// Stop the engine if it was started by us
    pub fn stop_engine(&mut self) {
        if let Some(mut child) = self.engine_process.take() {
            println!("Stopping Hanzo Engine...");
            let _ = child.kill();
            let _ = child.wait();
            println!("✓ Engine stopped");
        }
    }
}

impl Drop for HanzoEngine {
    fn drop(&mut self) {
        self.stop_engine();
    }
}

/// Create a Qwen chat request
pub fn create_qwen_request(
    model: &str,
    messages: Vec<(String, String)>,
) -> QwenRequest {
    QwenRequest {
        model: model.to_string(),
        messages: messages
            .into_iter()
            .map(|(role, content)| Message { role, content })
            .collect(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(0.95),
        stream: Some(false),
    }
}

/// Native engine provider for the dev CLI
pub struct NativeEngineProvider {
    engine: HanzoEngine,
}

impl NativeEngineProvider {
    pub fn new() -> Result<Self> {
        let mut engine = HanzoEngine::new()?;
        
        // Try to start the engine
        if let Err(e) = engine.start_engine() {
            eprintln!("Warning: Could not start native engine: {}", e);
            eprintln!("Falling back to external providers");
        }

        Ok(Self { engine })
    }

    pub async fn chat(&self, prompt: &str) -> Result<String> {
        let request = create_qwen_request(
            "qwen2.5-7b",
            vec![("user".to_string(), prompt.to_string())],
        );

        let response = self.engine.completion(request).await?;
        
        Ok(response.choices
            .first()
            .map(|c| c.message.content.clone())
            .unwrap_or_default())
    }

    pub fn is_available(&self) -> bool {
        self.engine.is_running().unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_engine_creation() {
        let engine = HanzoEngine::new();
        assert!(engine.is_ok());
    }

    #[test]
    fn test_request_creation() {
        let request = create_qwen_request(
            "qwen2.5-7b",
            vec![("user".to_string(), "Hello".to_string())],
        );
        assert_eq!(request.model, "qwen2.5-7b");
        assert_eq!(request.messages.len(), 1);
    }
}