use codex_api::SharedAuthProvider;
use reqwest::StatusCode;
use serde::Deserialize;
use serde::de::DeserializeOwned;

use crate::AgentsError;
use crate::Result;
use crate::api::AgentsBackend;
use crate::model_provider_unauthenticated;
use crate::types::Agent;
use crate::types::AgentDetail;
use crate::types::Run;

/// HTTP client for the `/v1/agents` surface. Holds the API base and an auth
/// provider that injects the bearer JWT; the gateway derives `X-Org-Id` from it.
#[derive(Clone)]
pub struct HttpClient {
    base_url: String,
    http: reqwest::Client,
    auth: SharedAuthProvider,
    user_agent: Option<String>,
}

/// The server's error envelope: `{"status": N, "code": "...", "error": "msg"}`.
#[derive(Deserialize)]
struct ErrorEnvelope {
    #[serde(default)]
    error: String,
}

#[derive(Deserialize)]
struct AgentsList {
    #[serde(default)]
    agents: Vec<Agent>,
}

impl HttpClient {
    /// Build a client for `base_url` (e.g. `https://api.hanzo.ai`). Trailing
    /// slashes are trimmed; requests target `{base_url}/v1/agents…`.
    pub fn new(base_url: impl Into<String>) -> Result<Self> {
        let mut base_url = base_url.into();
        while base_url.ends_with('/') {
            base_url.pop();
        }
        let http = reqwest::Client::builder()
            .build()
            .map_err(|e| AgentsError::Http(e.to_string()))?;
        Ok(Self {
            base_url,
            http,
            auth: model_provider_unauthenticated(),
            user_agent: None,
        })
    }

    /// Attach the auth provider whose bearer token authenticates every request.
    pub fn with_auth_provider(mut self, auth: SharedAuthProvider) -> Self {
        self.auth = auth;
        self
    }

    /// Set the `User-Agent` header sent on every request.
    pub fn with_user_agent(mut self, ua: impl Into<String>) -> Self {
        self.user_agent = Some(ua.into());
        self
    }

    fn request(&self, method: reqwest::Method, url: &str) -> reqwest::RequestBuilder {
        let mut req = self.http.request(method, url);
        if let Some(ua) = &self.user_agent {
            req = req.header(reqwest::header::USER_AGENT, ua);
        }
        req.headers(self.auth.to_auth_headers())
    }

    /// Send `req`, returning `(status, body)` regardless of status code.
    async fn send(req: reqwest::RequestBuilder) -> Result<(StatusCode, String)> {
        let resp = req
            .send()
            .await
            .map_err(|e| AgentsError::Http(e.to_string()))?;
        let status = resp.status();
        let body = resp
            .text()
            .await
            .map_err(|e| AgentsError::Http(e.to_string()))?;
        Ok((status, body))
    }

    /// Turn a non-success `(status, body)` into a quoted server error. Prefers
    /// the `{error}` envelope message; falls back to the raw body.
    fn server_error(status: StatusCode, body: &str) -> AgentsError {
        let msg = serde_json::from_str::<ErrorEnvelope>(body)
            .ok()
            .map(|e| e.error)
            .filter(|m| !m.is_empty())
            .unwrap_or_else(|| body.trim().to_string());
        if msg.is_empty() {
            AgentsError::Server(format!("server returned {status}"))
        } else {
            AgentsError::Server(format!("{status}: {msg}"))
        }
    }

    fn decode<T: DeserializeOwned>(body: &str) -> Result<T> {
        serde_json::from_str(body).map_err(|e| AgentsError::Decode(format!("{e}; body={body}")))
    }

    async fn get_json<T: DeserializeOwned>(&self, url: &str) -> Result<T> {
        let (status, body) = Self::send(self.request(reqwest::Method::GET, url)).await?;
        if !status.is_success() {
            return Err(Self::server_error(status, &body));
        }
        Self::decode(&body)
    }

    pub(crate) async fn list(&self) -> Result<Vec<Agent>> {
        let url = format!("{}/v1/agents", self.base_url);
        let list: AgentsList = self.get_json(&url).await?;
        Ok(list.agents)
    }

    pub(crate) async fn get(&self, name: &str) -> Result<AgentDetail> {
        let url = format!("{}/v1/agents/{}", self.base_url, encode_segment(name));
        self.get_json(&url).await
    }

    pub(crate) async fn run(&self, name: &str, input: &str) -> Result<Run> {
        let url = format!("{}/v1/agents/{}/run", self.base_url, encode_segment(name));
        let req = self
            .request(reqwest::Method::POST, &url)
            .json(&serde_json::json!({ "input": input }));
        let (status, body) = Self::send(req).await?;
        // A recorded error run comes back with a Run body (HTTP 502) — surface it
        // honestly rather than as a bare status. Only fall back to the error
        // envelope when the body is not a Run (e.g. 403/404/503).
        match serde_json::from_str::<Run>(&body) {
            Ok(run) => Ok(run),
            Err(decode_err) => {
                if status.is_success() {
                    Err(AgentsError::Decode(format!("{decode_err}; body={body}")))
                } else {
                    Err(Self::server_error(status, &body))
                }
            }
        }
    }
}

/// Percent-encode a path segment's reserved characters. Agent names are already
/// constrained server-side to `^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$`; this is a
/// boundary safeguard against a stray `/` or space corrupting the path.
fn encode_segment(seg: &str) -> String {
    let mut out = String::with_capacity(seg.len());
    for b in seg.bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' => {
                out.push(b as char)
            }
            _ => out.push_str(&format!("%{b:02X}")),
        }
    }
    out
}

impl AgentsBackend for HttpClient {
    fn list_agents(
        &self,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<Vec<Agent>>> + Send + '_>> {
        Box::pin(async move { self.list().await })
    }

    fn get_agent<'a>(
        &'a self,
        name: &'a str,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<AgentDetail>> + Send + 'a>> {
        Box::pin(async move { self.get(name).await })
    }

    fn run_agent<'a>(
        &'a self,
        name: &'a str,
        input: &'a str,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<Run>> + Send + 'a>> {
        Box::pin(async move { self.run(name, input).await })
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use codex_api::AuthProvider;
    use pretty_assertions::assert_eq;
    use reqwest::header::AUTHORIZATION;
    use reqwest::header::HeaderMap;
    use reqwest::header::HeaderValue;
    use serde_json::json;
    use wiremock::Mock;
    use wiremock::MockServer;
    use wiremock::ResponseTemplate;
    use wiremock::matchers::body_json;
    use wiremock::matchers::header;
    use wiremock::matchers::method;
    use wiremock::matchers::path;

    use super::*;

    /// A test auth provider that injects a fixed bearer token — the same shape
    /// the real `BearerAuthProvider` produces from a hanzo.id JWT.
    #[derive(Debug)]
    struct StaticBearer(&'static str);

    impl AuthProvider for StaticBearer {
        fn add_auth_headers(&self, headers: &mut HeaderMap) {
            headers.insert(
                AUTHORIZATION,
                HeaderValue::from_str(&format!("Bearer {}", self.0)).unwrap(),
            );
        }
    }

    fn client(base: &str) -> HttpClient {
        HttpClient::new(base)
            .unwrap()
            .with_auth_provider(Arc::new(StaticBearer("jwt-123")))
    }

    #[test]
    fn encode_segment_passes_valid_names_and_escapes_separators() {
        assert_eq!(encode_segment("helper"), "helper");
        assert_eq!(encode_segment("my.agent-1_x"), "my.agent-1_x");
        assert_eq!(encode_segment("a/b c"), "a%2Fb%20c");
    }

    #[test]
    fn new_trims_trailing_slashes() {
        let c = HttpClient::new("https://api.hanzo.ai///").unwrap();
        assert_eq!(c.base_url, "https://api.hanzo.ai");
    }

    #[tokio::test]
    async fn list_sends_bearer_and_parses_agents_envelope() {
        let server = MockServer::start().await;
        Mock::given(method("GET"))
            .and(path("/v1/agents"))
            .and(header(AUTHORIZATION.as_str(), "Bearer jwt-123"))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "agents": [{
                    "id": "agent_1", "name": "helper", "model": "gpt-4o-mini",
                    "description": "be terse", "tools": ["search"], "status": "ready",
                    "runs": 3, "createdAt": "2026-01-01T00:00:00Z",
                    "updatedAt": "2026-01-02T00:00:00Z"
                }]
            })))
            .expect(1)
            .mount(&server)
            .await;

        let agents = client(&server.uri()).list().await.unwrap();
        assert_eq!(agents.len(), 1);
        assert_eq!(agents[0].name, "helper");
        assert_eq!(agents[0].model, "gpt-4o-mini");
        assert_eq!(agents[0].tools, vec!["search".to_string()]);
        assert_eq!(agents[0].runs, 3);
    }

    #[tokio::test]
    async fn list_forbidden_quotes_the_server_error() {
        let server = MockServer::start().await;
        Mock::given(method("GET"))
            .and(path("/v1/agents"))
            .respond_with(
                ResponseTemplate::new(403)
                    .set_body_json(json!({ "status": 403, "error": "X-Org-Id required" })),
            )
            .mount(&server)
            .await;

        let err = client(&server.uri()).list().await.unwrap_err();
        assert!(matches!(err, AgentsError::Server(_)));
        assert!(
            err.to_string().contains("X-Org-Id required"),
            "error should quote the server: {err}"
        );
    }

    #[tokio::test]
    async fn get_parses_flattened_detail() {
        let server = MockServer::start().await;
        Mock::given(method("GET"))
            .and(path("/v1/agents/helper"))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "id": "agent_1", "name": "helper", "model": "m", "status": "ready",
                "tools": [], "runs": 0, "instructions": "be terse",
                "recentRuns": [{ "id": "run_1", "status": "ok", "model": "m",
                    "input": "hi", "output": "yo", "durationMs": 12 }]
            })))
            .mount(&server)
            .await;

        let detail = client(&server.uri()).get("helper").await.unwrap();
        assert_eq!(detail.agent.name, "helper");
        assert_eq!(detail.instructions, "be terse");
        assert_eq!(detail.recent_runs.len(), 1);
        assert_eq!(detail.recent_runs[0].output, "yo");
    }

    #[tokio::test]
    async fn run_posts_input_and_returns_ok_run() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/agents/helper/run"))
            .and(header(AUTHORIZATION.as_str(), "Bearer jwt-123"))
            .and(body_json(json!({ "input": "hi" })))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "id": "run_1", "status": "ok", "model": "m",
                "input": "hi", "output": "the answer", "durationMs": 42,
                "createdAt": "2026-01-01T00:00:00Z"
            })))
            .expect(1)
            .mount(&server)
            .await;

        let run = client(&server.uri()).run("helper", "hi").await.unwrap();
        assert!(run.is_ok());
        assert_eq!(run.output, "the answer");
        assert_eq!(run.duration_ms, 42);
    }

    #[tokio::test]
    async fn run_recorded_error_502_is_surfaced_as_a_run_not_an_error() {
        // The server records a real failed run and returns it with HTTP 502.
        // The client must surface the recorded run so the caller can report the
        // honest upstream failure — not swallow it as a bare status error.
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/agents/helper/run"))
            .respond_with(ResponseTemplate::new(502).set_body_json(json!({
                "id": "run_2", "status": "error", "model": "m",
                "input": "hi", "error": "upstream model timeout", "durationMs": 5
            })))
            .mount(&server)
            .await;

        let run = client(&server.uri()).run("helper", "hi").await.unwrap();
        assert!(!run.is_ok());
        assert_eq!(run.status, "error");
        assert_eq!(run.error, "upstream model timeout");
    }

    #[tokio::test]
    async fn run_service_unavailable_without_run_body_is_a_server_error() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/agents/helper/run"))
            .respond_with(ResponseTemplate::new(503).set_body_json(
                json!({ "status": 503, "error": "inference is not configured on this deployment" }),
            ))
            .mount(&server)
            .await;

        let err = client(&server.uri()).run("helper", "hi").await.unwrap_err();
        assert!(matches!(err, AgentsError::Server(_)));
        assert!(err.to_string().contains("inference is not configured"));
    }
}
