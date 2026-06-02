//! Registry of model providers supported by Codex.
//!
//! Providers can be defined in two places:
//!   1. Built-in defaults compiled into the binary so Codex works out-of-the-box.
//!   2. User-defined entries inside `~/.hanzo/config.toml` under the `model_providers`
//!      table (Hanzo Dev also reads legacy `~/.code/config.toml` / `~/.codex/config.toml`).
//!      key. These override or extend the defaults at runtime.

use crate::CodexAuth;
use crate::error::CodeErr;
use crate::error::EnvVarError;
use hanzo_app_server_protocol::AuthMode;
use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::env::VarError;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::process::Stdio;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::Duration;
const DEFAULT_STREAM_IDLE_TIMEOUT_MS: u64 = 300_000;
pub(crate) const DEFAULT_WEBSOCKET_CONNECT_TIMEOUT_MS: u64 = 15_000;
const DEFAULT_STREAM_MAX_RETRIES: u64 = 5;
const DEFAULT_REQUEST_MAX_RETRIES: u64 = 8;
/// Hard cap for user-configured `stream_max_retries`.
const MAX_STREAM_MAX_RETRIES: u64 = 100;
/// Hard cap for user-configured `request_max_retries`.
const MAX_REQUEST_MAX_RETRIES: u64 = 100;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ProviderAuthCacheKey {
    command: String,
    args: Vec<String>,
    cwd: PathBuf,
}

impl From<&ModelProviderAuthInfo> for ProviderAuthCacheKey {
    fn from(value: &ModelProviderAuthInfo) -> Self {
        Self {
            command: value.command.clone(),
            args: value.args.clone(),
            cwd: value.cwd.as_path().to_path_buf(),
        }
    }
}

#[derive(Debug, Clone)]
struct CachedProviderAuthToken {
    access_token: String,
    fetched_at: Instant,
}

fn provider_auth_cache() -> &'static Mutex<HashMap<ProviderAuthCacheKey, CachedProviderAuthToken>> {
    static CACHE: OnceLock<Mutex<HashMap<ProviderAuthCacheKey, CachedProviderAuthToken>>> =
        OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Wire protocol that the provider speaks. Most third-party services only
/// implement the classic OpenAI Chat Completions JSON schema, whereas OpenAI
/// itself (and a handful of others) additionally expose the more modern
/// *Responses* API. The two protocols use different request/response shapes
/// and *cannot* be auto-detected at runtime, therefore each provider entry
/// must declare which one it expects.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum WireApi {
    /// The Responses API exposed by OpenAI at `/v1/responses`.
    Responses,

    /// Experimental: Responses API over WebSocket transport.
    #[serde(rename = "responses_websocket")]
    ResponsesWebsocket,

    /// Regular Chat Completions compatible with `/v1/chat/completions`.
    #[default]
    Chat,

    /// Native ZAP binary transport over TLS 1.3+PQ.
    /// Connects to zap.hanzo.ai:9999 (or ZAP_ENDPOINT env) and sends
    /// chat completions as MsgType 100 (native cloud service) messages.
    Zap,
}

/// Serializable representation of a provider definition.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct ModelProviderInfo {
    /// Friendly display name.
    pub name: String,
    /// Base URL for the provider's OpenAI-compatible API.
    pub base_url: Option<String>,
    /// Environment variable that stores the user's API key for this provider.
    pub env_key: Option<String>,

    /// Optional instructions to help the user get a valid value for the
    /// variable and set it.
    pub env_key_instructions: Option<String>,

    /// Value to use with `Authorization: Bearer <token>` header. Use of this
    /// config is discouraged in favor of `env_key` for security reasons, but
    /// this may be necessary when using this programmatically.
    pub experimental_bearer_token: Option<String>,

    /// Command-backed bearer-token configuration for this provider.
    pub auth: Option<ModelProviderAuthInfo>,

    /// Which wire protocol this provider expects.
    #[serde(default)]
    pub wire_api: WireApi,

    /// Optional query parameters to append to the base URL.
    pub query_params: Option<HashMap<String, String>>,

    /// Additional HTTP headers to include in requests to this provider where
    /// the (key, value) pairs are the header name and value.
    pub http_headers: Option<HashMap<String, String>>,

    /// Optional HTTP headers to include in requests to this provider where the
    /// (key, value) pairs are the header name and _environment variable_ whose
    /// value should be used. If the environment variable is not set, or the
    /// value is empty, the header will not be included in the request.
    pub env_http_headers: Option<HashMap<String, String>>,

    /// Maximum number of times to retry a failed HTTP request to this provider.
    pub request_max_retries: Option<u64>,

    /// Number of times to retry reconnecting a dropped streaming response before failing.
    pub stream_max_retries: Option<u64>,

    /// Idle timeout (in milliseconds) to wait for activity on a streaming response before treating
    /// the connection as lost.
    pub stream_idle_timeout_ms: Option<u64>,

    /// Timeout (in milliseconds) when establishing a websocket transport connection.
    pub websocket_connect_timeout_ms: Option<u64>,

    /// Whether this provider requires some form of standard authentication (API key, ChatGPT token).
    #[serde(default)]
    pub requires_openai_auth: bool,

    /// Optional OpenRouter-specific configuration for routing preferences and metadata.
    #[serde(default)]
    pub openrouter: Option<OpenRouterConfig>,
}

/// OpenRouter-specific configuration, allowing users to control routing and pricing metadata.
#[derive(Debug, Clone, Default, Deserialize, Serialize, PartialEq)]
#[serde(default)]
pub struct OpenRouterConfig {
    /// Provider-level routing preferences forwarded to OpenRouter.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<OpenRouterProviderConfig>,

    /// Optional `route` payload forwarded as-is to OpenRouter for advanced routing.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub route: Option<Value>,

    /// Additional top-level fields that may be forwarded to OpenRouter as the API evolves.
    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

/// Provider routing preferences supported by OpenRouter.
#[derive(Debug, Clone, Default, Deserialize, Serialize, PartialEq)]
#[serde(default)]
pub struct OpenRouterProviderConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub order: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_fallbacks: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub require_parameters: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data_collection: Option<OpenRouterDataCollectionPolicy>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub zdr: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub only: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ignore: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub quantizations: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sort: Option<OpenRouterProviderSort>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_price: Option<OpenRouterMaxPrice>,

    /// Catch-all for additional provider keys so new OpenRouter features do not break deserialization.
    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
#[derive(Default)]
pub enum OpenRouterDataCollectionPolicy {
    #[default]
    Allow,
    Deny,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
#[derive(Default)]
pub enum OpenRouterProviderSort {
    #[default]
    Price,
    Throughput,
    Latency,
}

/// `max_price` envelope for OpenRouter provider routing controls.
#[derive(Debug, Clone, Default, Deserialize, Serialize, PartialEq)]
#[serde(default)]
pub struct OpenRouterMaxPrice {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub total: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prompt: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub completion: Option<f64>,

    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

impl ModelProviderInfo {
    pub(crate) fn validate(&self) -> std::result::Result<(), String> {
        let Some(auth) = self.auth.as_ref() else {
            return Ok(());
        };

        if auth.command.trim().is_empty() {
            return Err("provider auth.command must not be empty".to_string());
        }

        let mut conflicts = Vec::new();
        if self.env_key.is_some() {
            conflicts.push("env_key");
        }
        if self.experimental_bearer_token.is_some() {
            conflicts.push("experimental_bearer_token");
        }
        if self.requires_openai_auth {
            conflicts.push("requires_openai_auth");
        }

        if conflicts.is_empty() {
            Ok(())
        } else {
            Err(format!(
                "provider auth cannot be combined with {}",
                conflicts.join(", ")
            ))
        }
    }

    pub(crate) fn has_command_auth(&self) -> bool {
        self.auth.is_some()
    }

    pub(crate) fn invalidate_cached_auth_token(&self) {
        if let Some(auth) = self.auth.as_ref() {
            provider_auth_cache()
                .lock()
                .unwrap()
                .remove(&ProviderAuthCacheKey::from(auth));
        }
    }

    /// Construct a `POST` RequestBuilder for the given URL using the provided
    /// reqwest Client applying:
    ///   • provider-specific headers (static + env based)
    ///   • Bearer auth header when an API key is available.
    ///   • Auth token for OAuth.
    ///
    /// If the provider declares an `env_key` but the variable is missing/empty, returns an [`Err`] identical to the
    /// one produced by [`ModelProviderInfo::api_key`].
    pub async fn create_request_builder<'a>(
        &'a self,
        client: &'a reqwest::Client,
        auth: &Option<CodexAuth>,
    ) -> crate::error::Result<reqwest::RequestBuilder> {
        let effective_auth = self.effective_auth(auth).await?;

        self.create_request_builder_with_auth(client, &effective_auth)
            .await
    }

    pub async fn create_request_builder_with_auth<'a>(
        &'a self,
        client: &'a reqwest::Client,
        auth: &Option<CodexAuth>,
    ) -> crate::error::Result<reqwest::RequestBuilder> {
        let url = self.get_full_url(auth);

        let mut builder = client.post(&url);

        if let Some(auth) = auth.as_ref() {
            builder = builder.bearer_auth(auth.get_token().await?);
            if auth.is_fedramp_account() {
                builder = builder.header("X-OpenAI-Fedramp", "true");
            }
        }

        Ok(self.apply_http_headers(builder))
    }

    /// Construct a request builder for an explicit URL using provider-specific
    /// auth and headers.
    pub async fn create_request_builder_for_url<'a>(
        &'a self,
        client: &'a reqwest::Client,
        auth: &Option<CodexAuth>,
        method: reqwest::Method,
        url: reqwest::Url,
    ) -> crate::error::Result<reqwest::RequestBuilder> {
        let effective_auth = self.effective_auth(auth).await?;

        self.create_request_builder_for_url_with_auth(client, &effective_auth, method, url)
            .await
    }

    pub async fn create_request_builder_for_url_with_auth<'a>(
        &'a self,
        client: &'a reqwest::Client,
        auth: &Option<CodexAuth>,
        method: reqwest::Method,
        url: reqwest::Url,
    ) -> crate::error::Result<reqwest::RequestBuilder> {

        let mut builder = client.request(method, url);

        if let Some(auth) = auth.as_ref() {
            builder = builder.bearer_auth(auth.get_token().await?);
            if auth.is_fedramp_account() {
                builder = builder.header("X-OpenAI-Fedramp", "true");
            }
        }

        Ok(self.apply_http_headers(builder))
    }

    pub async fn create_compact_request_builder<'a>(
        &'a self,
        client: &'a reqwest::Client,
        auth: &Option<CodexAuth>,
    ) -> crate::error::Result<reqwest::RequestBuilder> {
        if !matches!(
            self.wire_api,
            WireApi::Responses | WireApi::ResponsesWebsocket
        ) {
            return Err(CodeErr::UnsupportedOperation(
                "Compaction endpoint requires Responses API providers".to_string(),
            ));
        }
        let effective_auth = self.effective_auth(auth)?;
        let url = self.get_compact_url(&effective_auth).ok_or_else(|| {
            CodeErr::UnsupportedOperation(
                "Compaction endpoint requires Responses API providers".to_string(),
            )
        })?;

        let mut builder = client.post(url);
        if let Some(auth) = auth.as_ref() {
            builder = builder.bearer_auth(auth.get_token().await?);
            if auth.is_fedramp_account() {
                builder = builder.header("X-OpenAI-Fedramp", "true");
            }
        }

        Ok(self.apply_http_headers(builder))
    }

    fn effective_auth(&self, auth: &Option<CodexAuth>) -> crate::error::Result<Option<CodexAuth>> {
        if let Some(token) = self
            .experimental_bearer_token
            .as_deref()
            .map(str::trim)
            .filter(|token| !token.is_empty())
        {
            return Ok(Some(CodexAuth::from_api_key(token)));
        }

        if let Some(provider_auth) = self.auth.as_ref() {
            let token = resolve_provider_auth_token(provider_auth)
                .await
                .map_err(|err| CodexErr::UnsupportedOperation(err.to_string()))?;
            return Ok(Some(CodexAuth::from_api_key(&token)));
        }

        match self.api_key() {
            Ok(Some(key)) => Ok(Some(CodexAuth::from_api_key(&key))),
            Ok(None) => Ok(auth.clone()),
            Err(err) => {
                if auth.is_some() {
                    Ok(auth.clone())
                } else {
                    Err(err)
                }
            }
        }
    }

    /// Returns the OpenRouter-specific configuration, if this provider declares one.
    pub fn openrouter_config(&self) -> Option<&OpenRouterConfig> {
        self.openrouter.as_ref()
    }

    fn get_query_string(&self) -> String {
        self.query_params
            .as_ref()
            .map_or_else(String::new, |params| {
                let full_params = params
                    .iter()
                    .map(|(k, v)| format!("{k}={v}"))
                    .collect::<Vec<_>>()
                    .join("&");
                format!("?{full_params}")
            })
    }

    pub(crate) fn get_full_url(&self, auth: &Option<CodexAuth>) -> String {
        let default_base_url = if matches!(
            auth,
            Some(CodexAuth {
                mode: AuthMode::ChatGPT,
                ..
            })
        ) {
            "https://chatgpt.com/backend-api/codex"
        } else {
            "https://api.openai.com/v1"
        };
        let query_string = self.get_query_string();
        let base_url = self
            .base_url
            .clone()
            .unwrap_or(default_base_url.to_string());

        match self.wire_api {
            WireApi::Responses | WireApi::ResponsesWebsocket => {
                format!("{base_url}/responses{query_string}")
            }
            WireApi::Chat => format!("{base_url}/chat/completions{query_string}"),
            WireApi::Zap => {
                // ZAP transport uses a separate endpoint; the URL here is used
                // for fallback/display only. Actual connection goes to ZAP_ENDPOINT.
                format!("{base_url}/chat/completions{query_string}")
            }
        }
    }

    pub(crate) fn get_compact_url(&self, auth: &Option<CodexAuth>) -> Option<String> {
        if !matches!(
            self.wire_api,
            WireApi::Responses | WireApi::ResponsesWebsocket
        ) {
            return None;
        }
        let full = self.get_full_url(auth);
        if let Some((path, query)) = full.split_once('?') {
            Some(format!("{path}/compact?{query}"))
        } else {
            Some(format!("{full}/compact"))
        }
    }

    pub(crate) fn is_azure_responses_endpoint(&self) -> bool {
        if !matches!(
            self.wire_api,
            WireApi::Responses | WireApi::ResponsesWebsocket
        ) {
            return false;
        }

        if self.name.eq_ignore_ascii_case("azure") {
            return true;
        }

        self.base_url
            .as_ref()
            .map(|base| matches_azure_responses_base_url(base))
            .unwrap_or(false)
    }

    pub(crate) fn is_backend_responses_endpoint(&self) -> bool {
        if !matches!(
            self.wire_api,
            WireApi::Responses | WireApi::ResponsesWebsocket
        ) {
            return false;
        }

        if self.name.eq_ignore_ascii_case("backend") {
            return true;
        }

        self.base_url
            .as_ref()
            .is_some_and(|base| base.contains("/backend-api"))
    }

    pub(crate) fn is_public_openai_responses_endpoint(&self) -> bool {
        if !matches!(
            self.wire_api,
            WireApi::Responses | WireApi::ResponsesWebsocket
        ) {
            return false;
        }
        if self.is_backend_responses_endpoint() || self.is_azure_responses_endpoint() {
            return false;
        }

        self.base_url
            .as_ref()
            .and_then(|base| reqwest::Url::parse(base).ok())
            .and_then(|parsed| {
                parsed
                    .host_str()
                    .map(|host| host.eq_ignore_ascii_case("api.openai.com"))
            })
            .unwrap_or(true)
    }

    /// Apply provider-specific HTTP headers (both static and environment-based)
    /// onto an existing `reqwest::RequestBuilder` and return the updated
    /// builder.
    fn apply_http_headers(&self, mut builder: reqwest::RequestBuilder) -> reqwest::RequestBuilder {
        if let Some(extra) = &self.http_headers {
            for (k, v) in extra {
                builder = builder.header(k, v);
            }
        }

        if let Some(env_headers) = &self.env_http_headers {
            for (header, env_var) in env_headers {
                if let Ok(val) = std::env::var(env_var)
                    && !val.trim().is_empty()
                {
                    builder = builder.header(header, val);
                }
            }
        }
        builder
    }

    /// If `env_key` is Some, returns the API key for this provider if present
    /// (and non-empty) in the environment. Falls back to auto-discovered
    /// credentials from Claude Code keychain, Codex CLI, or Hanzo IAM.
    /// If no key can be found, returns an error.
    pub fn api_key(&self) -> crate::error::Result<Option<String>> {
        match &self.env_key {
            Some(env_key) => {
                // 1. Try environment variable first.
                let env_value = std::env::var(env_key);
                if let Ok(v) = &env_value {
                    if !v.trim().is_empty() {
                        return Ok(Some(v.clone()));
                    }
                }

                // 2. Fall back to auto-discovered credentials.
                let discovered = crate::auth::discover_credentials();
                let fallback = match env_key.as_str() {
                    "ANTHROPIC_API_KEY" => discovered.anthropic_api_key,
                    "HANZO_API_KEY" => discovered.hanzo_access_token,
                    "OPENAI_API_KEY" => discovered.openai_api_key,
                    _ => None,
                };
                if let Some(key) = fallback {
                    return Ok(Some(key));
                }

                // 3. No key found — return error.
                Err(crate::error::CodeErr::EnvVar(EnvVarError {
                    var: env_key.clone(),
                    instructions: self.env_key_instructions.clone(),
                }))
            }
            None => Ok(None),
        }
    }

    /// Effective maximum number of request retries for this provider.
    pub fn request_max_retries(&self) -> u64 {
        self.request_max_retries
            .unwrap_or(DEFAULT_REQUEST_MAX_RETRIES)
            .min(MAX_REQUEST_MAX_RETRIES)
    }

    /// Effective maximum number of stream reconnection attempts for this provider.
    pub fn stream_max_retries(&self) -> u64 {
        self.stream_max_retries
            .unwrap_or(DEFAULT_STREAM_MAX_RETRIES)
            .min(MAX_STREAM_MAX_RETRIES)
    }

    /// Effective idle timeout for streaming responses.
    pub fn stream_idle_timeout(&self) -> Duration {
        self.stream_idle_timeout_ms
            .map(Duration::from_millis)
            .unwrap_or(Duration::from_millis(DEFAULT_STREAM_IDLE_TIMEOUT_MS))
    }

    pub fn websocket_connect_timeout(&self) -> Duration {
        self.websocket_connect_timeout_ms
            .map(Duration::from_millis)
            .unwrap_or(Duration::from_millis(DEFAULT_WEBSOCKET_CONNECT_TIMEOUT_MS))
    }

    pub fn base_url_for_probe(&self) -> String {
        self.base_url
            .clone()
            .unwrap_or_else(|| "https://api.openai.com".to_string())
    }
}

async fn resolve_provider_auth_token(config: &ModelProviderAuthInfo) -> io::Result<String> {
    let cache_key = ProviderAuthCacheKey::from(config);
    if let Some(cached_token) = provider_auth_cache().lock().unwrap().get(&cache_key).cloned() {
        let should_use_cached_token = match config.refresh_interval() {
            Some(refresh_interval) => cached_token.fetched_at.elapsed() < refresh_interval,
            None => true,
        };
        if should_use_cached_token {
            return Ok(cached_token.access_token);
        }
    }

    let access_token = run_provider_auth_command(config).await?;
    provider_auth_cache().lock().unwrap().insert(
        cache_key,
        CachedProviderAuthToken {
            access_token: access_token.clone(),
            fetched_at: Instant::now(),
        },
    );
    Ok(access_token)
}

async fn run_provider_auth_command(config: &ModelProviderAuthInfo) -> io::Result<String> {
    let program = resolve_provider_auth_program(&config.command, config.cwd.as_path())?;
    let mut command = Command::new(&program);
    command
        .args(&config.args)
        .current_dir(config.cwd.as_path())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true);

    let output = tokio::time::timeout(config.timeout(), command.output())
        .await
        .map_err(|_| {
            io::Error::other(format!(
                "provider auth command `{}` timed out after {} ms",
                config.command,
                config.timeout_ms.get()
            ))
        })?
        .map_err(|err| {
            io::Error::other(format!(
                "provider auth command `{}` failed to start: {err}",
                config.command
            ))
        })?;

    if !output.status.success() {
        let status = output.status;
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        let stderr_suffix = if stderr.is_empty() {
            String::new()
        } else {
            format!(": {stderr}")
        };
        return Err(io::Error::other(format!(
            "provider auth command `{}` exited with status {status}{stderr_suffix}",
            config.command
        )));
    }

    let stdout = String::from_utf8(output.stdout).map_err(|_| {
        io::Error::other(format!(
            "provider auth command `{}` wrote non-UTF-8 data to stdout",
            config.command
        ))
    })?;
    let access_token = stdout.trim().to_string();
    if access_token.is_empty() {
        return Err(io::Error::other(format!(
            "provider auth command `{}` produced an empty token",
            config.command
        )));
    }

    Ok(access_token)
}

fn resolve_provider_auth_program(command: &str, cwd: &Path) -> io::Result<PathBuf> {
    let path = Path::new(command);
    if path.is_absolute() {
        return Ok(path.to_path_buf());
    }

    if path.components().count() > 1 {
        return Ok(cwd.join(path));
    }

    Ok(PathBuf::from(command))
}

const DEFAULT_OLLAMA_PORT: u32 = 11434;

pub const BUILT_IN_OSS_MODEL_PROVIDER_ID: &str = "oss";

/// Built-in default provider list.
fn wire_api_override_from_env(env_key: &str) -> Option<WireApi> {
    match std::env::var(env_key) {
        Ok(value) => match value.trim().to_ascii_lowercase().as_str() {
            "chat" => Some(WireApi::Chat),
            "responses" => Some(WireApi::Responses),
            "responses_websocket" => Some(WireApi::ResponsesWebsocket),
            "zap" => Some(WireApi::Zap),
            other if !other.is_empty() => {
                tracing::warn!(
                    "Ignoring unknown {env_key} value '{other}'; falling back to default wire API"
                );
                None
            }
            _ => None,
        },
        Err(_) => None,
    }
}

pub fn built_in_model_providers(
    openai_base_url: Option<String>,
) -> HashMap<String, ModelProviderInfo> {
    use ModelProviderInfo as P;

    // Built-in providers for common OpenAI-compatible endpoints.
    // Users can add more via `model_providers` in config.toml.
    [
        // OpenAI (default)
        (
            "openai",
            P {
                name: "OpenAI".into(),
                // Allow users to override the default OpenAI endpoint by
                // exporting `OPENAI_BASE_URL` or passing it via config.
                // This is useful when pointing Codex at a proxy, mock server,
                // or Azure-style deployment without requiring a full TOML
                // override for the built-in OpenAI provider.
                base_url: openai_base_url.or_else(|| std::env::var("OPENAI_BASE_URL")
                    .ok()
                    .filter(|v| !v.trim().is_empty())),
                env_key: None,
                env_key_instructions: None,
                experimental_bearer_token: None,
                auth: None,
                wire_api: wire_api_override_from_env("OPENAI_WIRE_API")
                    .unwrap_or(WireApi::Responses),
                query_params: None,
                http_headers: Some(
                    [("version".to_string(), hanzo_version::version().to_string())]
                        .into_iter()
                        .collect(),
                ),
                env_http_headers: Some(
                    [
                        (
                            "OpenAI-Organization".to_string(),
                            "OPENAI_ORGANIZATION".to_string(),
                        ),
                        ("OpenAI-Project".to_string(), "OPENAI_PROJECT".to_string()),
                    ]
                    .into_iter()
                    .collect(),
                ),
                // Use global defaults for retry/timeout unless overridden in config.toml.
                request_max_retries: None,
                stream_max_retries: None,
                stream_idle_timeout_ms: None,
                websocket_connect_timeout_ms: None,
                requires_openai_auth: true,
                openrouter: None,
            },
        ),
        // Generic local OSS provider (defaults to Ollama port)
        (BUILT_IN_OSS_MODEL_PROVIDER_ID, create_oss_provider()),
        // Ollama - local inference server (port 11434)
        (
            "ollama",
            P {
                name: "Ollama".into(),
                base_url: std::env::var("OLLAMA_BASE_URL")
                    .ok()
                    .filter(|v| !v.trim().is_empty())
                    .or_else(|| Some(format!("http://localhost:{DEFAULT_OLLAMA_PORT}/v1"))),
                env_key: None,
                env_key_instructions: Some(
                    "Ollama runs locally - no API key needed. Start with: ollama serve".into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Chat,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: Some(2),
                stream_max_retries: Some(2),
                stream_idle_timeout_ms: Some(60_000),
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
        // LM Studio - local inference server (port 1234)
        (
            "lmstudio",
            P {
                name: "LM Studio".into(),
                base_url: std::env::var("LMSTUDIO_BASE_URL")
                    .ok()
                    .filter(|v| !v.trim().is_empty())
                    .or_else(|| Some("http://localhost:1234/v1".into())),
                env_key: None,
                env_key_instructions: Some(
                    "LM Studio runs locally - no API key needed. Enable local server in LM Studio."
                        .into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Chat,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: Some(2),
                stream_max_retries: Some(2),
                stream_idle_timeout_ms: Some(60_000),
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
        // Hanzo - Hanzo AI unified gateway (api.hanzo.ai)
        (
            "hanzo",
            P {
                name: "Hanzo".into(),
                base_url: std::env::var("HANZO_BASE_URL")
                    .ok()
                    .filter(|v| !v.trim().is_empty())
                    .or_else(|| Some("https://api.hanzo.ai/v1".into())),
                env_key: Some("HANZO_API_KEY".into()),
                env_key_instructions: Some(
                    "Log in with `dev /login` or set HANZO_API_KEY from https://hanzo.ai".into(),
                ),
                experimental_bearer_token: None,
                wire_api: wire_api_override_from_env("HANZO_WIRE_API")
                    .unwrap_or(WireApi::Chat),
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: Some(2),
                stream_max_retries: Some(2),
                stream_idle_timeout_ms: Some(60_000),
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
        // Hanzo Engine - local private inference (mistral.rs)
        (
            "engine",
            P {
                name: "Hanzo Engine".into(),
                base_url: std::env::var("HANZO_ENGINE_URL")
                    .ok()
                    .filter(|v| !v.trim().is_empty())
                    .or_else(|| Some("http://localhost:36900/v1".into())),
                env_key: None,
                env_key_instructions: Some(
                    "Install Hanzo Engine: cargo install hanzo-engine, then run: hanzo-engine serve".into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Chat,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: Some(1),
                stream_max_retries: Some(1),
                stream_idle_timeout_ms: Some(120_000),
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
        // Hanzo Node - local AI node via native ZAP binary transport (port 3692)
        (
            "hanzo-node",
            P {
                name: "Hanzo Node".into(),
                base_url: std::env::var("HANZO_NODE_ZAP_URL")
                    .ok()
                    .filter(|v| !v.trim().is_empty())
                    .or_else(|| Some("localhost:3692".into())),
                env_key: None,
                env_key_instructions: Some(
                    "Hanzo Node runs locally - start with Hanzo Desktop or: hanzo-node".into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Zap,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: Some(2),
                stream_max_retries: Some(2),
                stream_idle_timeout_ms: Some(120_000),
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
        // OpenRouter - multi-provider router
        (
            "openrouter",
            P {
                name: "OpenRouter".into(),
                base_url: Some("https://openrouter.ai/api/v1".into()),
                env_key: Some("OPENROUTER_API_KEY".into()),
                env_key_instructions: Some(
                    "Get your OpenRouter API key at https://openrouter.ai/keys".into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Chat,
                query_params: None,
                http_headers: Some(
                    [
                        ("HTTP-Referer".to_string(), "https://hanzo.ai".to_string()),
                        ("X-Title".to_string(), "Hanzo Dev".to_string()),
                    ]
                    .into_iter()
                    .collect(),
                ),
                env_http_headers: None,
                request_max_retries: None,
                stream_max_retries: None,
                stream_idle_timeout_ms: None,
                requires_openai_auth: false,
                openrouter: Some(OpenRouterConfig::default()),
            },
        ),
        // Groq - fast inference cloud
        (
            "groq",
            P {
                name: "Groq".into(),
                base_url: Some("https://api.groq.com/openai/v1".into()),
                env_key: Some("GROQ_API_KEY".into()),
                env_key_instructions: Some(
                    "Get your Groq API key at https://console.groq.com/keys".into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Chat,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: None,
                stream_max_retries: None,
                stream_idle_timeout_ms: None,
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
        // Together.ai - inference platform
        (
            "together",
            P {
                name: "Together.ai".into(),
                base_url: Some("https://api.together.xyz/v1".into()),
                env_key: Some("TOGETHER_API_KEY".into()),
                env_key_instructions: Some(
                    "Get your Together API key at https://api.together.xyz/settings/api-keys"
                        .into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Chat,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: None,
                stream_max_retries: None,
                stream_idle_timeout_ms: None,
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
        // Anthropic Claude
        (
            "anthropic",
            P {
                name: "Anthropic".into(),
                base_url: Some("https://api.anthropic.com/v1".into()),
                env_key: Some("ANTHROPIC_API_KEY".into()),
                env_key_instructions: Some(
                    "Get your Anthropic API key at https://console.anthropic.com/settings/keys"
                        .into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Chat,
                query_params: None,
                http_headers: Some(
                    [("anthropic-version".to_string(), "2023-06-01".to_string())]
                        .into_iter()
                        .collect(),
                ),
                env_http_headers: None,
                request_max_retries: None,
                stream_max_retries: None,
                stream_idle_timeout_ms: None,
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
        // Fireworks AI
        (
            "fireworks",
            P {
                name: "Fireworks AI".into(),
                base_url: Some("https://api.fireworks.ai/inference/v1".into()),
                env_key: Some("FIREWORKS_API_KEY".into()),
                env_key_instructions: Some(
                    "Get your Fireworks API key at https://fireworks.ai/account/api-keys".into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Chat,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: None,
                stream_max_retries: None,
                stream_idle_timeout_ms: None,
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
        // Deepseek
        (
            "deepseek",
            P {
                name: "DeepSeek".into(),
                base_url: Some("https://api.deepseek.com/v1".into()),
                env_key: Some("DEEPSEEK_API_KEY".into()),
                env_key_instructions: Some(
                    "Get your DeepSeek API key at https://platform.deepseek.com/api_keys".into(),
                ),
                experimental_bearer_token: None,
                wire_api: WireApi::Chat,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: None,
                stream_max_retries: None,
                stream_idle_timeout_ms: None,
                requires_openai_auth: false,
                openrouter: None,
            },
        ),
    ]
    .into_iter()
    .map(|(k, v)| (k.to_string(), v))
    .collect()
}

pub fn create_oss_provider() -> ModelProviderInfo {
    // These HANZO_OSS_ environment variables are experimental: we may
    // switch to reading values from config.toml instead.
    let code_oss_base_url = match std::env::var("HANZO_OSS_BASE_URL")
        .ok()
        .filter(|v| !v.trim().is_empty())
    {
        Some(url) => url,
        None => format!(
            "http://localhost:{port}/v1",
            port = std::env::var("HANZO_OSS_PORT")
                .ok()
                .filter(|v| !v.trim().is_empty())
                .and_then(|v| v.parse::<u32>().ok())
                .unwrap_or(DEFAULT_OLLAMA_PORT)
        ),
    };

    create_oss_provider_with_base_url(&code_oss_base_url)
}

pub fn create_oss_provider_with_base_url(base_url: &str) -> ModelProviderInfo {
    ModelProviderInfo {
        name: "gpt-oss".into(),
        base_url: Some(base_url.into()),
        env_key: None,
        env_key_instructions: None,
        experimental_bearer_token: None,
        auth: None,
        wire_api: WireApi::Chat,
        query_params: None,
        http_headers: None,
        env_http_headers: None,
        request_max_retries: None,
        stream_max_retries: None,
        stream_idle_timeout_ms: None,
        websocket_connect_timeout_ms: None,
        requires_openai_auth: false,
        openrouter: None,
    }
}

fn matches_azure_responses_base_url(base_url: &str) -> bool {
    let base = base_url.to_ascii_lowercase();
    const AZURE_MARKERS: [&str; 5] = [
        "openai.azure.",
        "cognitiveservices.azure.",
        "aoai.azure.",
        "azure-api.",
        "azurefd.",
    ];
    AZURE_MARKERS.iter().any(|marker| base.contains(marker))
}

#[cfg(test)]
mod tests {
    use super::*;
    use code_utils_absolute_path::AbsolutePathBuf;
    use code_utils_absolute_path::AbsolutePathBufGuard;
    use pretty_assertions::assert_eq;
    use std::num::NonZeroU64;
    use tempfile::tempdir;

    #[test]
    fn test_deserialize_ollama_model_provider_toml() {
        let azure_provider_toml = r#"
name = "Ollama"
base_url = "http://localhost:11434/v1"
        "#;
        let expected_provider = ModelProviderInfo {
            name: "Ollama".into(),
            base_url: Some("http://localhost:11434/v1".into()),
            env_key: None,
            env_key_instructions: None,
            experimental_bearer_token: None,
            auth: None,
            wire_api: WireApi::Chat,
            query_params: None,
            http_headers: None,
            env_http_headers: None,
            request_max_retries: None,
            stream_max_retries: None,
            stream_idle_timeout_ms: None,
            websocket_connect_timeout_ms: None,
            requires_openai_auth: false,
            openrouter: None,
        };

        let provider: ModelProviderInfo = toml::from_str(azure_provider_toml).unwrap();
        assert_eq!(expected_provider, provider);
    }

    #[test]
    fn test_deserialize_azure_model_provider_toml() {
        let azure_provider_toml = r#"
name = "Azure"
base_url = "https://xxxxx.openai.azure.com/openai"
env_key = "AZURE_OPENAI_API_KEY"
query_params = { api-version = "2025-04-01-preview" }
        "#;
        let expected_provider = ModelProviderInfo {
            name: "Azure".into(),
            base_url: Some("https://xxxxx.openai.azure.com/openai".into()),
            env_key: Some("AZURE_OPENAI_API_KEY".into()),
            env_key_instructions: None,
            experimental_bearer_token: None,
            auth: None,
            wire_api: WireApi::Chat,
            query_params: Some(maplit::hashmap! {
                "api-version".to_string() => "2025-04-01-preview".to_string(),
            }),
            http_headers: None,
            env_http_headers: None,
            request_max_retries: None,
            stream_max_retries: None,
            stream_idle_timeout_ms: None,
            websocket_connect_timeout_ms: None,
            requires_openai_auth: false,
            openrouter: None,
        };

        let provider: ModelProviderInfo = toml::from_str(azure_provider_toml).unwrap();
        assert_eq!(expected_provider, provider);
    }

    #[test]
    fn test_deserialize_example_model_provider_toml() {
        let azure_provider_toml = r#"
name = "Example"
base_url = "https://example.com"
env_key = "API_KEY"
http_headers = { "X-Example-Header" = "example-value" }
env_http_headers = { "X-Example-Env-Header" = "EXAMPLE_ENV_VAR" }
        "#;
        let expected_provider = ModelProviderInfo {
            name: "Example".into(),
            base_url: Some("https://example.com".into()),
            env_key: Some("API_KEY".into()),
            env_key_instructions: None,
            experimental_bearer_token: None,
            auth: None,
            wire_api: WireApi::Chat,
            query_params: None,
            http_headers: Some(maplit::hashmap! {
                "X-Example-Header".to_string() => "example-value".to_string(),
            }),
            env_http_headers: Some(maplit::hashmap! {
                "X-Example-Env-Header".to_string() => "EXAMPLE_ENV_VAR".to_string(),
            }),
            request_max_retries: None,
            stream_max_retries: None,
            stream_idle_timeout_ms: None,
            websocket_connect_timeout_ms: None,
            requires_openai_auth: false,
            openrouter: None,
        };

        let provider: ModelProviderInfo = toml::from_str(azure_provider_toml).unwrap();
        assert_eq!(expected_provider, provider);
    }

    #[test]
    fn detects_azure_responses_base_urls() {
        fn provider_for(base_url: &str) -> ModelProviderInfo {
            ModelProviderInfo {
                name: "test".into(),
                base_url: Some(base_url.into()),
                env_key: None,
                env_key_instructions: None,
                experimental_bearer_token: None,
                auth: None,
                wire_api: WireApi::Responses,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: None,
                stream_max_retries: None,
                stream_idle_timeout_ms: None,
                websocket_connect_timeout_ms: None,
                requires_openai_auth: false,
                openrouter: None,
            }
        }

        let positive_cases = [
            "https://foo.openai.azure.com/openai",
            "https://foo.openai.azure.us/openai/deployments/bar",
            "https://foo.cognitiveservices.azure.cn/openai",
            "https://foo.aoai.azure.com/openai",
            "https://foo.openai.azure-api.net/openai",
            "https://foo.z01.azurefd.net/",
        ];
        for base_url in positive_cases {
            let provider = provider_for(base_url);
            assert!(
                provider.is_azure_responses_endpoint(),
                "expected {base_url} to be detected as Azure"
            );
        }

        let named_provider = ModelProviderInfo {
            name: "Azure".into(),
            base_url: Some("https://example.com".into()),
            env_key: None,
            env_key_instructions: None,
            experimental_bearer_token: None,
            auth: None,
            wire_api: WireApi::Responses,
            query_params: None,
            http_headers: None,
            env_http_headers: None,
            request_max_retries: None,
            stream_max_retries: None,
            stream_idle_timeout_ms: None,
            websocket_connect_timeout_ms: None,
            requires_openai_auth: false,
            openrouter: None,
        };
        assert!(named_provider.is_azure_responses_endpoint());

        let negative_cases = [
            "https://api.openai.com/v1",
            "https://example.com/openai",
            "https://myproxy.azurewebsites.net/openai",
        ];
        for base_url in negative_cases {
            let provider = provider_for(base_url);
            assert!(
                !provider.is_azure_responses_endpoint(),
                "expected {base_url} not to be detected as Azure"
            );
        }
    }
}
