//! Multi-CLI to OpenAI-compatible API Proxy
//!
//! Supports multiple AI CLI backends (uses subscription/login, NOT API keys):
//! - Claude Code CLI (claude) - Anthropic subscription
//! - Codex CLI (codex) - OpenAI subscription
//! - Gemini CLI (gemini) - Google account
//! - Mistral Vibe CLI (vibe) - Mistral subscription
//! - Qwen CLI (qwen) - Alibaba account
//! - Ollama (ollama) - Local models
//! - Hanzo Engine (hanzo) - Local inference engine (OpenAI-compatible HTTP)

use anyhow::Result;
use axum::http::StatusCode;
use axum::routing::get;
use axum::routing::post;
use axum::Json;
use axum::Router;
use clap::Parser;
use serde::Deserialize;
use serde::Serialize;
use std::process::Stdio;
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
use tower_http::cors::CorsLayer;
use tracing::error;
use tracing::info;
use tracing::warn;

/// Default Hanzo engine URL (can be overridden with HANZO_ENGINE_URL env var)
const DEFAULT_HANZO_ENGINE_URL: &str = "http://localhost:8080";

/// CLI proxy server arguments
#[derive(Parser, Debug)]
#[command(name = "dev-cli-proxy")]
#[command(about = "Hanzo Dev multi-CLI to OpenAI-compatible API proxy")]
struct Args {
    /// Port to listen on
    #[arg(short, long, default_value = "9999")]
    port: u16,
}

/// Backend configuration for a CLI tool
#[derive(Clone)]
struct Backend {
    name: &'static str,
    command: &'static str,
    input_mode: InputMode,
    models: Vec<&'static str>,
    api_keys_to_clean: Vec<&'static str>,
}

#[derive(Clone, Copy, PartialEq)]
enum InputMode {
    Stdin,
    Args,
    Http, // Direct HTTP passthrough to local server
}

impl Backend {
    fn build_args(&self, prompt: &str, model: &str) -> Vec<String> {
        match self.name {
            "claude" => {
                let mut args = vec![
                    "-p".to_string(),
                    "--output-format".to_string(),
                    "text".to_string(),
                ];
                if !model.is_empty() && model != "claude-cli" && model.starts_with("claude-") {
                    args.push("--model".to_string());
                    args.push(model.to_string());
                }
                args
            }
            "codex" => {
                let mut args = vec!["exec".to_string(), "--full-auto".to_string()];
                if !model.is_empty() && model != "codex" && model.starts_with("codex-") {
                    args.push("-m".to_string());
                    args.push(model.to_string());
                }
                args.push(prompt.to_string());
                args
            }
            "gemini" => {
                let mut args = vec!["-p".to_string(), prompt.to_string(), "-y".to_string()];
                if !model.is_empty() && model != "gemini" && model.starts_with("gemini-") {
                    args.push("-m".to_string());
                    args.push(model.to_string());
                }
                args
            }
            "vibe" => {
                vec![
                    "-p".to_string(),
                    prompt.to_string(),
                    "--output".to_string(),
                    "text".to_string(),
                    "--auto-approve".to_string(),
                ]
            }
            "qwen" => {
                vec!["-p".to_string(), prompt.to_string()]
            }
            "ollama" => {
                let model_to_use = if model.is_empty() { "llama3.2" } else { model };
                vec![
                    "run".to_string(),
                    model_to_use.to_string(),
                    prompt.to_string(),
                ]
            }
            "hanzo" => {
                // Hanzo uses HTTP passthrough, not CLI args
                vec![]
            }
            "copilot" => {
                vec![
                    "copilot".to_string(),
                    "suggest".to_string(),
                    "--type".to_string(),
                    "shell".to_string(),
                    prompt.to_string(),
                ]
            }
            _ => vec![prompt.to_string()],
        }
    }
}

/// All supported backends
fn get_backends() -> Vec<Backend> {
    vec![
        Backend {
            name: "claude",
            command: "claude",
            input_mode: InputMode::Stdin,
            models: vec!["claude-cli", "claude-opus", "claude-sonnet", "claude-haiku"],
            api_keys_to_clean: vec!["ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN"],
        },
        Backend {
            name: "codex",
            command: "codex",
            input_mode: InputMode::Args,
            models: vec![
                "codex",
                "codex-mini",
                "codex-mini-latest",
                "gpt-4o",
                "gpt-4o-mini",
                "o1",
                "o3-mini",
            ],
            api_keys_to_clean: vec!["OPENAI_API_KEY", "OPENAI_ORG_ID"],
        },
        Backend {
            name: "gemini",
            command: "gemini",
            input_mode: InputMode::Args,
            models: vec![
                "gemini",
                "gemini-2.5-pro",
                "gemini-2.0-flash",
                "gemini-1.5-pro",
            ],
            api_keys_to_clean: vec!["GOOGLE_API_KEY", "GEMINI_API_KEY"],
        },
        Backend {
            name: "vibe",
            command: "vibe",
            input_mode: InputMode::Args,
            models: vec!["vibe", "mistral", "mistral-large", "mistral-small"],
            api_keys_to_clean: vec!["MISTRAL_API_KEY"],
        },
        Backend {
            name: "qwen",
            command: "qwen",
            input_mode: InputMode::Args,
            models: vec!["qwen", "qwen-cli", "qwen-plus", "qwen-turbo", "qwen3"],
            api_keys_to_clean: vec!["DASHSCOPE_API_KEY", "QWEN_API_KEY"],
        },
        Backend {
            name: "ollama",
            command: "ollama",
            input_mode: InputMode::Args,
            models: vec![
                "ollama",
                "llama3.2",
                "llama3.1",
                "codellama",
                "mixtral",
                "phi3",
            ],
            api_keys_to_clean: vec![],
        },
        Backend {
            name: "hanzo",
            command: "", // Not used - hanzo uses HTTP passthrough
            input_mode: InputMode::Http,
            models: vec!["hanzo", "hanzo-engine", "hanzo-local", "default"],
            api_keys_to_clean: vec![],
        },
        Backend {
            name: "copilot",
            command: "gh",
            input_mode: InputMode::Args,
            models: vec!["copilot", "gh-copilot", "github-copilot"],
            api_keys_to_clean: vec!["GITHUB_TOKEN"],
        },
    ]
}

/// Find the right backend for a model
fn get_backend_for_model(model: &str) -> Backend {
    let backends = get_backends();

    // Direct match
    for backend in &backends {
        if backend.models.contains(&model) {
            return backend.clone();
        }
    }

    // Prefix matching
    if model.starts_with("claude") {
        return backends.into_iter().find(|b| b.name == "claude").unwrap();
    }
    if model.starts_with("codex")
        || model.starts_with("gpt")
        || model.starts_with("o1")
        || model.starts_with("o3")
    {
        return backends.into_iter().find(|b| b.name == "codex").unwrap();
    }
    if model.starts_with("gemini") {
        return backends.into_iter().find(|b| b.name == "gemini").unwrap();
    }
    if model.starts_with("vibe") || model.starts_with("mistral") {
        return backends.into_iter().find(|b| b.name == "vibe").unwrap();
    }
    if model.starts_with("qwen") {
        return backends.into_iter().find(|b| b.name == "qwen").unwrap();
    }
    if model.starts_with("llama") || model.starts_with("ollama") || model.starts_with("phi") {
        return backends.into_iter().find(|b| b.name == "ollama").unwrap();
    }
    if model.starts_with("hanzo") {
        return backends.into_iter().find(|b| b.name == "hanzo").unwrap();
    }
    if model.starts_with("copilot") || model.starts_with("gh-copilot") {
        return backends.into_iter().find(|b| b.name == "copilot").unwrap();
    }

    // Default to Claude
    backends.into_iter().find(|b| b.name == "claude").unwrap()
}

/// Extract clean response from CLI output (filter telemetry/JSON)
fn extract_response(output: &str) -> String {
    let mut text_lines = Vec::new();
    let mut in_json = false;
    let mut brace_count = 0;

    for line in output.lines() {
        let trimmed = line.trim();

        // Skip empty lines at start
        if trimmed.is_empty() && text_lines.is_empty() {
            continue;
        }

        // Track JSON blocks
        if trimmed.starts_with('{') {
            in_json = true;
            brace_count = 1;
            continue;
        }

        if in_json {
            brace_count += trimmed.matches('{').count() as i32;
            brace_count -= trimmed.matches('}').count() as i32;
            if brace_count <= 0 {
                in_json = false;
            }
            continue;
        }

        // Skip JSON artifacts
        if trimmed
            .chars()
            .all(|c| matches!(c, '[' | ']' | '{' | '}' | ',' | ' '))
        {
            continue;
        }

        if !trimmed.is_empty() && !trimmed.starts_with('}') {
            text_lines.push(line.to_string());
        }
    }

    let result = text_lines.join("\n").trim().to_string();
    // Clean trailing brackets
    result
        .trim_end_matches(['[', ']', '{', '}', ' '])
        .to_string()
}

/// Invoke Hanzo engine via HTTP passthrough
async fn invoke_hanzo(request: &ChatRequest) -> Result<ChatResponse> {
    let hanzo_url =
        std::env::var("HANZO_ENGINE_URL").unwrap_or_else(|_| DEFAULT_HANZO_ENGINE_URL.to_string());

    // Get the model - use env var default or fallback to "default" (hanzo engine interprets this)
    let default_model =
        std::env::var("HANZO_DEFAULT_MODEL").unwrap_or_else(|_| "default".to_string());

    let endpoint = format!("{}/v1/chat/completions", hanzo_url);

    info!(url = %endpoint, model = %default_model, "Forwarding to Hanzo engine");

    let client = reqwest::Client::new();

    // Build the request body
    let body = serde_json::json!({
        "model": default_model,
        "messages": request.messages.iter().map(|m| {
            serde_json::json!({
                "role": m.role,
                "content": m.content
            })
        }).collect::<Vec<_>>()
    });

    let response = client
        .post(&endpoint)
        .header("Content-Type", "application/json")
        .header("Authorization", "Bearer EMPTY")
        .json(&body)
        .send()
        .await?;

    if !response.status().is_success() {
        let status = response.status();
        let error_text = response.text().await.unwrap_or_default();
        anyhow::bail!("Hanzo engine error ({}): {}", status, error_text);
    }

    let hanzo_response: ChatResponse = response.json().await?;
    Ok(hanzo_response)
}

/// Invoke CLI backend and get response
async fn invoke_cli(prompt: &str, model: &str) -> Result<String> {
    let backend = get_backend_for_model(model);
    let args = backend.build_args(prompt, model);

    info!(
        backend = backend.name,
        command = backend.command,
        "Invoking CLI"
    );

    let mut cmd = Command::new(backend.command);
    cmd.args(&args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    // Explicitly remove API keys to force subscription/login mode
    for key in &backend.api_keys_to_clean {
        cmd.env_remove(*key);
    }
    // Disable OTEL telemetry
    cmd.env("OTEL_SDK_DISABLED", "true");

    // For stdin input mode, we need to pipe stdin
    if matches!(backend.input_mode, InputMode::Stdin) {
        cmd.stdin(Stdio::piped());
    }

    let mut child = cmd.spawn()?;

    // Write prompt to stdin if needed
    if matches!(backend.input_mode, InputMode::Stdin) {
        if let Some(mut stdin) = child.stdin.take() {
            stdin.write_all(prompt.as_bytes()).await?;
            stdin.shutdown().await?;
        }
    }

    let output = child.wait_with_output().await?;

    if !output.status.success() && output.stdout.is_empty() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("{} CLI failed: {}", backend.name, stderr);
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let response = extract_response(&stdout);

    if response.is_empty() {
        Ok(stdout.to_string())
    } else {
        Ok(response)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// API Types
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Deserialize)]
struct ChatRequest {
    model: Option<String>,
    messages: Vec<Message>,
}

#[derive(Deserialize)]
struct Message {
    role: String,
    content: String,
}

#[derive(Serialize, Deserialize)]
struct ChatResponse {
    id: String,
    object: String,
    created: u64,
    model: String,
    choices: Vec<Choice>,
    usage: Usage,
}

#[derive(Serialize, Deserialize)]
struct Choice {
    index: u32,
    message: ResponseMessage,
    finish_reason: String,
}

#[derive(Serialize, Deserialize)]
struct ResponseMessage {
    role: String,
    content: String,
}

#[derive(Serialize, Deserialize)]
struct Usage {
    prompt_tokens: u32,
    completion_tokens: u32,
    total_tokens: u32,
}

#[derive(Serialize)]
struct ModelsResponse {
    object: String,
    data: Vec<ModelInfo>,
}

#[derive(Serialize)]
struct ModelInfo {
    id: String,
    object: String,
    created: u64,
    owned_by: String,
}

#[derive(Serialize)]
struct HealthResponse {
    status: String,
    backends: Vec<String>,
}

#[derive(Serialize)]
struct ErrorResponse {
    error: ErrorDetail,
}

#[derive(Serialize)]
struct ErrorDetail {
    message: String,
    r#type: String,
    code: String,
}

// ─────────────────────────────────────────────────────────────────────────────
// Handlers
// ─────────────────────────────────────────────────────────────────────────────

async fn health_handler() -> Json<HealthResponse> {
    let backends = get_backends().iter().map(|b| b.name.to_string()).collect();
    Json(HealthResponse {
        status: "ok".to_string(),
        backends,
    })
}

async fn models_handler() -> Json<ModelsResponse> {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();

    let mut data = Vec::new();
    for backend in get_backends() {
        for model in backend.models {
            data.push(ModelInfo {
                id: model.to_string(),
                object: "model".to_string(),
                created: now,
                owned_by: backend.name.to_string(),
            });
        }
    }

    Json(ModelsResponse {
        object: "list".to_string(),
        data,
    })
}

async fn chat_completions_handler(
    Json(request): Json<ChatRequest>,
) -> Result<Json<ChatResponse>, (StatusCode, Json<ErrorResponse>)> {
    let model = request
        .model
        .clone()
        .unwrap_or_else(|| "claude-cli".to_string());
    let backend = get_backend_for_model(&model);

    info!(model = %model, backend = backend.name, "Processing chat completion");

    // Use HTTP passthrough for hanzo backend
    if backend.input_mode == InputMode::Http {
        match invoke_hanzo(&request).await {
            Ok(mut response) => {
                // Override model name in response to match requested model
                response.model = model;
                info!("Hanzo engine response received");
                return Ok(Json(response));
            }
            Err(e) => {
                // If hanzo engine is not available, provide helpful error
                warn!(error = %e, "Hanzo engine not available");
                return Err((
                    StatusCode::SERVICE_UNAVAILABLE,
                    Json(ErrorResponse {
                        error: ErrorDetail {
                            message: format!("Hanzo engine not available: {}. Start with: cd ~/work/hanzo/engine && cargo run --release -- --port 8080 plain -m <model>", e),
                            r#type: "server_error".to_string(),
                            code: "hanzo_unavailable".to_string(),
                        },
                    }),
                ));
            }
        }
    }

    // Build prompt from messages for CLI backends
    let prompt: String = request
        .messages
        .iter()
        .map(|m| match m.role.as_str() {
            "system" => format!("[System]: {}", m.content),
            "assistant" => format!("[Assistant]: {}", m.content),
            _ => m.content.clone(),
        })
        .collect::<Vec<_>>()
        .join("\n\n");

    match invoke_cli(&prompt, &model).await {
        Ok(response) => {
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs();

            info!(response_len = response.len(), "Got response");

            Ok(Json(ChatResponse {
                id: format!("chatcmpl-{}", now),
                object: "chat.completion".to_string(),
                created: now,
                model: model.clone(),
                choices: vec![Choice {
                    index: 0,
                    message: ResponseMessage {
                        role: "assistant".to_string(),
                        content: response.clone(),
                    },
                    finish_reason: "stop".to_string(),
                }],
                usage: Usage {
                    prompt_tokens: (prompt.len() / 4) as u32,
                    completion_tokens: (response.len() / 4) as u32,
                    total_tokens: ((prompt.len() + response.len()) / 4) as u32,
                },
            }))
        }
        Err(e) => {
            error!(error = %e, "CLI invocation failed");
            Err((
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ErrorResponse {
                    error: ErrorDetail {
                        message: e.to_string(),
                        r#type: "server_error".to_string(),
                        code: "internal_error".to_string(),
                    },
                }),
            ))
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────────────────

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .init();

    let args = Args::parse();

    // Build router
    let app = Router::new()
        .route("/", get(health_handler))
        .route("/health", get(health_handler))
        .route("/v1/models", get(models_handler))
        .route("/v1/chat/completions", post(chat_completions_handler))
        .layer(CorsLayer::permissive());

    let backends: Vec<_> = get_backends().iter().map(|b| b.name).collect();

    println!(
        r#"
╔══════════════════════════════════════════════════════════════════╗
║     Multi-CLI → OpenAI API Proxy (Subscription Mode)             ║
╠══════════════════════════════════════════════════════════════════╣
║  Listening:  http://localhost:{}                               ║
║  Endpoint:   http://localhost:{}/v1/chat/completions           ║
║  Backends:   {}
╠══════════════════════════════════════════════════════════════════╣
║  All CLIs use subscription/login (API keys are stripped):        ║
║    claude-cli, claude-opus → Claude (Anthropic subscription)     ║
║    codex, gpt-4o, o1       → Codex (OpenAI subscription)         ║
║    gemini, gemini-2.5-pro  → Gemini (Google account)             ║
║    vibe, mistral           → Vibe (Mistral subscription)         ║
║    qwen, qwen-plus         → Qwen (Alibaba account)              ║
║    ollama, llama3.2        → Ollama (local, no keys needed)      ║
║    hanzo, hanzo-engine     → Hanzo Engine (local inference HTTP) ║
╚══════════════════════════════════════════════════════════════════╝
"#,
        args.port,
        args.port,
        backends.join(", ")
    );

    let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:{}", args.port)).await?;
    info!(port = args.port, "Server listening");

    axum::serve(listener, app).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_backend_for_model() {
        assert_eq!(get_backend_for_model("claude-cli").name, "claude");
        assert_eq!(get_backend_for_model("claude-opus").name, "claude");
        assert_eq!(get_backend_for_model("codex").name, "codex");
        assert_eq!(get_backend_for_model("gpt-4o").name, "codex");
        assert_eq!(get_backend_for_model("gemini").name, "gemini");
        assert_eq!(get_backend_for_model("llama3.2").name, "ollama");
        assert_eq!(get_backend_for_model("hanzo").name, "hanzo");
        assert_eq!(get_backend_for_model("hanzo-node").name, "hanzo");
        assert_eq!(get_backend_for_model("unknown").name, "claude"); // default
    }

    #[test]
    fn test_extract_response() {
        let output = r#"{
  "telemetry": "data"
}
Hello, world!
{
  "more": "json"
}
"#;
        assert_eq!(extract_response(output), "Hello, world!");
    }

    #[test]
    fn test_build_args_claude() {
        let backend = get_backend_for_model("claude-cli");
        let args = backend.build_args("test prompt", "claude-opus");
        assert!(args.contains(&"--model".to_string()));
        assert!(args.contains(&"claude-opus".to_string()));
    }
}
