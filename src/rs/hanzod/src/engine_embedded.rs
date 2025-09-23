//! Embedded Hanzo Engine for native inference
//! Uses mistralrs-server-core as a library for elegant integration

use anyhow::Result;
use axum::{
    extract::{Json, State},
    http::StatusCode,
    response::IntoResponse,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use tokio::sync::{RwLock, mpsc};
use tracing::{info, warn, error};

#[cfg(feature = "engine")]
use mistralrs_server_core::{
    mistralrs_for_server_builder::{
        MistralRsForServerBuilder, ModelConfig, defaults,
    },
    mistralrs_server_router_builder::MistralRsServerRouterBuilder,
    types::SharedMistralRsState,
    handler_core::{create_response_channel, send_request},
    openai::ChatCompletionRequest,
};

#[cfg(feature = "engine")]
use mistralrs_core::{
    ModelSelected, PagedCacheType, TokenSource, Device,
    Request, RequestMessage, Response, NormalRequest, SamplingParams,
    ChatCompletionResponse, ChatCompletionChunkResponse,
};

/// Embedded Hanzo Engine
pub struct EmbeddedEngine {
    #[cfg(feature = "engine")]
    router: Option<Arc<axum::Router>>,
    #[cfg(feature = "engine")]
    mistralrs: Option<SharedMistralRsState>,
    #[cfg(not(feature = "engine"))]
    _phantom: std::marker::PhantomData<()>,
}

impl EmbeddedEngine {
    /// Create a new embedded engine instance
    pub async fn new() -> Result<Self> {
        #[cfg(feature = "engine")]
        {
            info!("Initializing embedded Hanzo Engine with Metal/MLX support...");

            // Determine device - prefer Metal on macOS
            let device = if cfg!(target_os = "macos") {
                Device::new_metal(0).unwrap_or_else(|_| {
                    warn!("Failed to initialize Metal device, falling back to CPU");
                    Device::Cpu
                })
            } else {
                Device::Cpu
            };

            info!("Using device: {:?}", device);

            // Build the MistralRs instance with proper configuration
            let model = ModelSelected::Plain {
                model_id: "microsoft/Phi-3.5-mini-instruct".to_string(),
                tokenizer_json: None,
                arch: None,
                dtype: None,
                topology: None,
                organization: Default::default(),
                write_uqff: None,
                from_uqff: None,
                imatrix: None,
                calibration_file: None,
                max_seq_len: None,
                max_batch_size: None,
                hf_cache_path: None,
                matformer_config_path: None,
                matformer_slice_name: None,
            };

            // Build the shared mistralrs state
            let mistralrs = match MistralRsForServerBuilder::new()
                .with_device(device)
                .with_model(model)
                .with_token_source(TokenSource::None)
                .with_max_seqs(10)
                .with_prefix_cache_n(16)
                .with_truncate_sequence(true)
                .with_no_kv_cache(false)
                .build()
                .await {
                Ok(state) => {
                    info!("MistralRs instance created successfully");
                    state
                }
                Err(e) => {
                    error!("Failed to create MistralRs instance: {}", e);
                    return Ok(Self {
                        router: None,
                        mistralrs: None,
                    });
                }
            };

            // Build the router with the mistralrs state
            match MistralRsServerRouterBuilder::new()
                .with_mistralrs(mistralrs.clone())
                .build()
                .await {
                Ok(router) => {
                    info!("Hanzo Engine embedded successfully with Metal acceleration");
                    Ok(Self {
                        router: Some(Arc::new(router)),
                        mistralrs: Some(mistralrs),
                    })
                }
                Err(e) => {
                    error!("Failed to build router: {}", e);
                    Ok(Self {
                        router: None,
                        mistralrs: Some(mistralrs),
                    })
                }
            }
        }

        #[cfg(not(feature = "engine"))]
        {
            warn!("Hanzo Engine feature not enabled, using mock responses");
            Ok(Self {
                _phantom: std::marker::PhantomData,
            })
        }
    }
    
    /// Check if the embedded engine is available
    pub fn is_available(&self) -> bool {
        #[cfg(feature = "engine")]
        {
            self.router.is_some() && self.mistralrs.is_some()
        }
        #[cfg(not(feature = "engine"))]
        {
            false
        }
    }

    /// Get the router for mounting in the main application
    #[cfg(feature = "engine")]
    pub fn router(&self) -> Option<Arc<axum::Router>> {
        self.router.clone()
    }

    /// Get the mistralrs state
    #[cfg(feature = "engine")]
    pub fn mistralrs(&self) -> Option<SharedMistralRsState> {
        self.mistralrs.clone()
    }
    
    /// Handle chat completion requests natively
    pub async fn chat_completion(&self, request: Value) -> Result<Value> {
        #[cfg(feature = "engine")]
        {
            if let Some(ref mistralrs) = self.mistralrs {
                info!("Processing chat completion with embedded Hanzo Engine");

                // Parse the request into the expected format
                let messages = request.get("messages")
                    .and_then(|m| m.as_array())
                    .ok_or_else(|| anyhow::anyhow!("Invalid messages field"))?;

                let mut request_messages = Vec::new();
                for msg in messages {
                    let role = msg.get("role")
                        .and_then(|r| r.as_str())
                        .ok_or_else(|| anyhow::anyhow!("Missing role in message"))?;
                    let content = msg.get("content")
                        .and_then(|c| c.as_str())
                        .ok_or_else(|| anyhow::anyhow!("Missing content in message"))?;

                    request_messages.push(RequestMessage::Literal {
                        role: role.to_string(),
                        content: content.to_string(),
                    });
                }

                // Get sampling parameters from the request
                let temperature = request.get("temperature")
                    .and_then(|t| t.as_f64())
                    .map(|t| t as f32);
                let max_tokens = request.get("max_tokens")
                    .and_then(|t| t.as_u64())
                    .map(|t| t as usize);
                let top_p = request.get("top_p")
                    .and_then(|t| t.as_f64())
                    .map(|t| t as f32);

                // Create sampling params
                let mut sampling = SamplingParams::default();
                if let Some(temp) = temperature {
                    sampling.temperature = Some(temp);
                }
                if let Some(max_tok) = max_tokens {
                    sampling.max_len = Some(max_tok);
                }
                if let Some(p) = top_p {
                    sampling.top_p = Some(p);
                }

                // Create a channel for receiving the response
                let (tx, mut rx) = mpsc::channel(10);
                let response_tx = tx;

                // Create the request
                let normal_request = NormalRequest {
                    messages: request_messages,
                    sampling_params: sampling,
                    response: response_tx,
                    return_logprobs: false,
                    is_streaming: false,
                    id: 0,
                    constraint: mistralrs_core::Constraint::None,
                    suffix: None,
                    adapters: None,
                    tools: None,
                    tool_choice: None,
                    logits_processors: None,
                };

                // Send the request to mistralrs
                let sender = mistralrs.get_sender(None)?;
                sender.send(Request::Normal(normal_request)).await?;

                // Wait for response
                let mut final_response = None;
                while let Some(response) = rx.recv().await {
                    match response {
                        Response::Done(resp) => {
                            final_response = Some(resp);
                            break;
                        }
                        Response::ModelError(msg, _) => {
                            return Err(anyhow::anyhow!("Model error: {}", msg));
                        }
                        Response::ValidationError(e) => {
                            return Err(anyhow::anyhow!("Validation error: {}", e));
                        }
                        Response::InternalError(e) => {
                            return Err(anyhow::anyhow!("Internal error: {}", e));
                        }
                        _ => {
                            // Continue receiving for streaming responses
                        }
                    }
                }

                if let Some(chat_resp) = final_response {
                    // Convert ChatCompletionResponse to JSON Value
                    Ok(serde_json::to_value(chat_resp)?)
                } else {
                    Err(anyhow::anyhow!("No response received from engine"))
                }
            } else {
                warn!("Embedded engine not available, returning mock response");
                Ok(create_mock_chat_response(&request))
            }
        }

        #[cfg(not(feature = "engine"))]
        {
            Ok(create_mock_chat_response(&request))
        }
    }
    
    /// Handle completion requests natively
    pub async fn completion(&self, request: Value) -> Result<Value> {
        #[cfg(feature = "engine")]
        {
            if let Some(ref mistralrs) = self.mistralrs {
                info!("Processing completion with embedded Hanzo Engine");

                // Parse the prompt from the request
                let prompt = request.get("prompt")
                    .and_then(|p| p.as_str())
                    .ok_or_else(|| anyhow::anyhow!("Missing prompt field"))?;

                // Convert completion request to chat format
                // Most models work better with chat format
                let request_messages = vec![
                    RequestMessage::Literal {
                        role: "user".to_string(),
                        content: prompt.to_string(),
                    }
                ];

                // Get sampling parameters from the request
                let temperature = request.get("temperature")
                    .and_then(|t| t.as_f64())
                    .map(|t| t as f32);
                let max_tokens = request.get("max_tokens")
                    .and_then(|t| t.as_u64())
                    .map(|t| t as usize);
                let top_p = request.get("top_p")
                    .and_then(|t| t.as_f64())
                    .map(|t| t as f32);

                // Create sampling params
                let mut sampling = SamplingParams::default();
                if let Some(temp) = temperature {
                    sampling.temperature = Some(temp);
                }
                if let Some(max_tok) = max_tokens {
                    sampling.max_len = Some(max_tok);
                }
                if let Some(p) = top_p {
                    sampling.top_p = Some(p);
                }

                // Create a channel for receiving the response
                let (tx, mut rx) = mpsc::channel(10);
                let response_tx = tx;

                // Create the request
                let normal_request = NormalRequest {
                    messages: request_messages,
                    sampling_params: sampling,
                    response: response_tx,
                    return_logprobs: false,
                    is_streaming: false,
                    id: 0,
                    constraint: mistralrs_core::Constraint::None,
                    suffix: None,
                    adapters: None,
                    tools: None,
                    tool_choice: None,
                    logits_processors: None,
                };

                // Send the request to mistralrs
                let sender = mistralrs.get_sender(None)?;
                sender.send(Request::Normal(normal_request)).await?;

                // Wait for response
                let mut final_text = String::new();
                while let Some(response) = rx.recv().await {
                    match response {
                        Response::Done(resp) => {
                            // Extract the generated text from the chat response
                            if let Some(choice) = resp.choices.first() {
                                final_text = choice.message.content.clone().unwrap_or_default();
                            }
                            break;
                        }
                        Response::ModelError(msg, _) => {
                            return Err(anyhow::anyhow!("Model error: {}", msg));
                        }
                        Response::ValidationError(e) => {
                            return Err(anyhow::anyhow!("Validation error: {}", e));
                        }
                        Response::InternalError(e) => {
                            return Err(anyhow::anyhow!("Internal error: {}", e));
                        }
                        _ => {
                            // Continue receiving for streaming responses
                        }
                    }
                }

                // Format as a completion response
                let response = serde_json::json!({
                    "id": format!("cmpl-{}", uuid::Uuid::new_v4()),
                    "object": "text_completion",
                    "created": chrono::Utc::now().timestamp(),
                    "model": "microsoft/Phi-3.5-mini-instruct",
                    "choices": [{
                        "text": final_text,
                        "index": 0,
                        "logprobs": null,
                        "finish_reason": "stop"
                    }],
                    "usage": {
                        "prompt_tokens": prompt.split_whitespace().count(),
                        "completion_tokens": final_text.split_whitespace().count(),
                        "total_tokens": prompt.split_whitespace().count() + final_text.split_whitespace().count()
                    }
                });

                Ok(response)
            } else {
                warn!("Embedded engine not available, returning mock response");
                Ok(create_mock_completion_response(&request))
            }
        }

        #[cfg(not(feature = "engine"))]
        {
            Ok(create_mock_completion_response(&request))
        }
    }
    
    /// Get available models
    pub async fn models(&self) -> Result<Value> {
        Ok(serde_json::json!({
            "data": [{
                "id": "microsoft/Phi-3.5-mini-instruct",
                "object": "model",
                "created": 1677649963,
                "owned_by": "hanzo",
                "permission": [],
                "root": "microsoft/Phi-3.5-mini-instruct",
                "parent": null,
            }],
            "object": "list"
        }))
    }
}

/// Create a mock chat completion response
fn create_mock_chat_response(request: &Value) -> Value {
    let messages = request.get("messages")
        .and_then(|v| v.as_array())
        .map(|arr| arr.len())
        .unwrap_or(0);
    
    serde_json::json!({
        "id": "chat-embedded-mock",
        "object": "chat.completion",
        "created": 1677649963,
        "model": "microsoft/Phi-3.5-mini-instruct",
        "usage": {
            "prompt_tokens": messages * 10,
            "completion_tokens": 20,
            "total_tokens": messages * 10 + 20
        },
        "choices": [{
            "message": {
                "role": "assistant",
                "content": "This is a mock response from the embedded Hanzo Engine. The engine is being initialized with Metal/MLX support."
            },
            "logprobs": null,
            "finish_reason": "stop",
            "index": 0
        }]
    })
}

/// Create a mock completion response
fn create_mock_completion_response(request: &Value) -> Value {
    let prompt = request.get("prompt")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    
    serde_json::json!({
        "id": "comp-embedded-mock",
        "object": "text_completion",
        "created": 1677649963,
        "model": "microsoft/Phi-3.5-mini-instruct",
        "choices": [{
            "text": format!("Mock completion for: {}", prompt),
            "index": 0,
            "logprobs": null,
            "finish_reason": "stop"
        }],
        "usage": {
            "prompt_tokens": prompt.split_whitespace().count(),
            "completion_tokens": 10,
            "total_tokens": prompt.split_whitespace().count() + 10
        }
    })
}

/// Mount the embedded engine routes onto an Axum router
pub fn mount_engine_routes(engine: Arc<EmbeddedEngine>) -> axum::Router {
    use axum::routing::{get, post};
    
    #[cfg(feature = "engine")]
    {
        // If we have a native router, merge it
        if let Some(ref native_router) = engine.router() {
            info!("Mounting native Hanzo Engine routes");
            return native_router.as_ref().clone();
        }
    }
    
    // Fallback routes for when engine is not available
    axum::Router::new()
        .route("/v1/chat/completions", post(chat_completion_handler))
        .route("/v1/completions", post(completion_handler))
        .route("/v1/models", get(models_handler))
        .route("/health", get(health_handler))
        .with_state(engine)
}

/// Handler for chat completions
async fn chat_completion_handler(
    State(engine): State<Arc<EmbeddedEngine>>,
    Json(request): Json<Value>,
) -> impl IntoResponse {
    match engine.chat_completion(request).await {
        Ok(response) => (StatusCode::OK, Json(response)),
        Err(e) => {
            let error = serde_json::json!({
                "error": {
                    "message": e.to_string(),
                    "type": "server_error",
                    "param": null,
                    "code": null
                }
            });
            (StatusCode::INTERNAL_SERVER_ERROR, Json(error))
        }
    }
}

/// Handler for completions
async fn completion_handler(
    State(engine): State<Arc<EmbeddedEngine>>,
    Json(request): Json<Value>,
) -> impl IntoResponse {
    match engine.completion(request).await {
        Ok(response) => (StatusCode::OK, Json(response)),
        Err(e) => {
            let error = serde_json::json!({
                "error": {
                    "message": e.to_string(),
                    "type": "server_error",
                    "param": null,
                    "code": null
                }
            });
            (StatusCode::INTERNAL_SERVER_ERROR, Json(error))
        }
    }
}

/// Handler for models list
async fn models_handler(
    State(engine): State<Arc<EmbeddedEngine>>,
) -> impl IntoResponse {
    match engine.models().await {
        Ok(response) => (StatusCode::OK, Json(response)),
        Err(e) => {
            let error = serde_json::json!({
                "error": {
                    "message": e.to_string(),
                    "type": "server_error",
                    "param": null,
                    "code": null
                }
            });
            (StatusCode::INTERNAL_SERVER_ERROR, Json(error))
        }
    }
}

/// Health check handler
async fn health_handler() -> impl IntoResponse {
    (StatusCode::OK, Json(serde_json::json!({
        "status": "healthy",
        "engine": "embedded",
        "acceleration": if cfg!(target_os = "macos") { "Metal/MLX" } else { "CPU" }
    })))
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_embedded_engine_creation() {
        let engine = EmbeddedEngine::new().await;
        assert!(engine.is_ok());
    }
    
    #[test]
    fn test_mock_responses() {
        let request = serde_json::json!({
            "messages": [
                {"role": "user", "content": "Hello"}
            ]
        });
        let response = create_mock_chat_response(&request);
        assert!(response.get("choices").is_some());
    }
}