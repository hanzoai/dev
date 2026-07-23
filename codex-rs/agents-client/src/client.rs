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
use crate::types::ControlBatch;
use crate::types::Run;
use crate::types::Session;
use crate::types::SessionRegister;

/// Cap on the response body we will buffer from any single request. Agent
/// output is text; 16 MiB is far beyond any legitimate run while still bounding
/// memory against a hostile or runaway host.
const MAX_BODY_BYTES: usize = 16 * 1024 * 1024;

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

    /// Send `req`, returning `(status, body)` regardless of status code. The
    /// body is read chunk-by-chunk and capped at [`MAX_BODY_BYTES`] so a hostile
    /// or malfunctioning host cannot exhaust memory by streaming an unbounded
    /// response (a `Content-Length` header would be advisory and can lie, so the
    /// cap is enforced on the bytes actually received).
    async fn send(req: reqwest::RequestBuilder) -> Result<(StatusCode, String)> {
        let mut resp = req
            .send()
            .await
            .map_err(|e| AgentsError::Http(e.to_string()))?;
        let status = resp.status();
        let mut buf: Vec<u8> = Vec::new();
        while let Some(chunk) = resp
            .chunk()
            .await
            .map_err(|e| AgentsError::Http(e.to_string()))?
        {
            if buf.len() + chunk.len() > MAX_BODY_BYTES {
                return Err(AgentsError::Http(format!(
                    "response body exceeds {MAX_BODY_BYTES} bytes"
                )));
            }
            buf.extend_from_slice(&chunk);
        }
        let body = String::from_utf8(buf)
            .map_err(|e| AgentsError::Http(format!("response body was not valid UTF-8: {e}")))?;
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
        validate_name(name)?;
        let url = format!("{}/v1/agents/{}", self.base_url, encode_segment(name));
        self.get_json(&url).await
    }

    pub(crate) async fn run(&self, name: &str, input: &str) -> Result<Run> {
        validate_name(name)?;
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

    // ── session telemetry — make a `hanzo code`/`dev` run watchable in the
    //    hanzo.bot playground fleet (`/v1/agents/sessions`). Every call is
    //    best-effort at the call site: session tracking must never fail a run. ──

    /// Register a session (`POST /v1/agents/sessions`) → the created [`Session`]
    /// whose `id` backs the `hanzo.bot/sessions/<id>` deep-link and every later
    /// event/patch/stop call.
    pub async fn register_session(&self, req: &SessionRegister) -> Result<Session> {
        let url = format!("{}/v1/agents/sessions", self.base_url);
        let (status, body) =
            Self::send(self.request(reqwest::Method::POST, &url).json(req)).await?;
        if !status.is_success() {
            return Err(Self::server_error(status, &body));
        }
        Self::decode(&body)
    }

    /// Append ONE event (`POST /v1/agents/sessions/:id/events`). `kind` is one of
    /// message|tool-call|spawn|log|status|control; `payload` is the render-contract
    /// JSON the playground viewer consumes. Fire-and-observe: returns the server
    /// error but callers ignore it (telemetry is never load-bearing).
    pub async fn append_session_event(
        &self,
        id: &str,
        kind: &str,
        payload: serde_json::Value,
    ) -> Result<()> {
        let url = format!(
            "{}/v1/agents/sessions/{}/events",
            self.base_url,
            encode_segment(id)
        );
        let req = self
            .request(reqwest::Method::POST, &url)
            .json(&serde_json::json!({ "kind": kind, "payload": payload }));
        let (status, body) = Self::send(req).await?;
        if !status.is_success() {
            return Err(Self::server_error(status, &body));
        }
        Ok(())
    }

    /// Update a session's status (`PATCH /v1/agents/sessions/:id`) →
    /// running|paused|done|error. Used to mark a run terminal on exit.
    pub async fn patch_session_status(&self, id: &str, status: &str) -> Result<Session> {
        let url = format!(
            "{}/v1/agents/sessions/{}",
            self.base_url,
            encode_segment(id)
        );
        let req = self
            .request(reqwest::Method::PATCH, &url)
            .json(&serde_json::json!({ "status": status }));
        let (rc, body) = Self::send(req).await?;
        if !rc.is_success() {
            return Err(Self::server_error(rc, &body));
        }
        Self::decode(&body)
    }

    /// Stop a session (`POST /v1/agents/sessions/:id/stop`) — the definitive
    /// terminal transition (also cancels the durable task run server-side).
    pub async fn stop_session(&self, id: &str) -> Result<()> {
        let url = format!(
            "{}/v1/agents/sessions/{}/stop",
            self.base_url,
            encode_segment(id)
        );
        let (status, body) = Self::send(self.request(reqwest::Method::POST, &url)).await?;
        if !status.is_success() {
            return Err(Self::server_error(status, &body));
        }
        Ok(())
    }

    /// Drain steering commands newer than `after` for a session
    /// (`GET /v1/agents/sessions/:id/control?after=N`). Cursor-driven: pass the
    /// returned [`ControlBatch::cursor`] as the next `after` so an applied
    /// command is never seen twice. Owner-scoped server-side by the bearer.
    pub async fn drain_control(&self, id: &str, after: i64) -> Result<ControlBatch> {
        let url = format!(
            "{}/v1/agents/sessions/{}/control?after={after}",
            self.base_url,
            encode_segment(id)
        );
        let (status, body) = Self::send(self.request(reqwest::Method::GET, &url)).await?;
        if !status.is_success() {
            return Err(Self::server_error(status, &body));
        }
        Self::decode(&body)
    }
}

/// Human-readable description of the agent-name grammar the server enforces
/// (`^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$`), quoted in [`AgentsError::InvalidName`].
pub(crate) const NAME_RULE: &str =
    "1-64 chars, starting with a letter or digit, then letters, digits, '.', '-' or '_'";

/// Validate an agent name against the server's grammar *before* it is placed in
/// a URL path. This is the real boundary check: percent-encoding alone does not
/// stop a dot-only segment (`.` / `..`) from being collapsed by URL
/// normalization and rewriting the request to a different endpoint (e.g. `..`
/// would turn `/v1/agents/../run` into `/v1/run`, sending the bearer token to
/// an unintended route). Rejecting anything outside the grammar closes that and
/// gives the caller an honest error instead of a mangled request.
fn validate_name(name: &str) -> Result<()> {
    let ok = name.len() <= 64
        && matches!(name.bytes().next(), Some(b) if b.is_ascii_alphanumeric())
        && name
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'.' | b'-' | b'_'));
    if ok {
        Ok(())
    } else {
        Err(AgentsError::InvalidName(name.to_string()))
    }
}

/// Percent-encode a path segment's reserved characters. Agent names are already
/// constrained by [`validate_name`] to `^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$`; this
/// remains as defense-in-depth so a segment can never carry an unescaped
/// separator into the path.
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
    use wiremock::matchers::query_param;

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
    fn validate_name_accepts_the_server_grammar() {
        for name in ["a", "helper", "my.agent-1_x", "A0", &"z".repeat(64)] {
            assert!(validate_name(name).is_ok(), "should accept {name:?}");
        }
    }

    #[test]
    fn validate_name_rejects_dot_segments_and_separators() {
        // Dot-only / leading-dot names are the traversal vectors: without this
        // check, ".." collapses `/v1/agents/../run` down to `/v1/run`.
        for name in [
            "", ".", "..", "...", ".hidden", "-x", "_x", "a/b", "a b", "a?b", "a#b", "a%2e",
        ] {
            let err = validate_name(name).unwrap_err();
            assert!(
                matches!(err, AgentsError::InvalidName(_)),
                "should reject {name:?}, got {err:?}"
            );
        }
        // Over-length (65 chars) is rejected.
        assert!(matches!(
            validate_name(&"z".repeat(65)).unwrap_err(),
            AgentsError::InvalidName(_)
        ));
    }

    #[test]
    fn invalid_name_error_quotes_the_name_and_the_rule() {
        let err = validate_name("..").unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("\"..\""),
            "should quote the offending name: {msg}"
        );
        assert!(msg.contains(NAME_RULE), "should state the rule: {msg}");
    }

    #[tokio::test]
    async fn run_rejects_traversal_name_before_hitting_network() {
        // No mock is mounted: if the client dialed out this would 404/hang. The
        // request must be refused client-side, so the URL is never built.
        let err = client("http://127.0.0.1:1")
            .run("..", "hi")
            .await
            .unwrap_err();
        assert!(matches!(err, AgentsError::InvalidName(_)), "got {err:?}");
    }

    #[tokio::test]
    async fn oversized_response_body_is_capped() {
        let server = MockServer::start().await;
        // 17 MiB of 'a' — one byte over the 16 MiB cap.
        let big = "a".repeat(super::MAX_BODY_BYTES + 1);
        Mock::given(method("GET"))
            .and(path("/v1/agents"))
            .respond_with(ResponseTemplate::new(200).set_body_string(big))
            .mount(&server)
            .await;

        let err = client(&server.uri()).list().await.unwrap_err();
        assert!(matches!(err, AgentsError::Http(_)), "got {err:?}");
        assert!(
            err.to_string().contains("exceeds"),
            "should report the cap: {err}"
        );
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
    async fn run_502_with_non_run_json_is_a_server_error_not_a_default_run() {
        // Regression guard: a server error whose body is arbitrary JSON (no
        // required Run fields) must NOT deserialize into an all-default Run and
        // masquerade as a silent empty success. `id`/`status`/`model` are
        // required, so the body fails Run decode and surfaces as a server error.
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/agents/helper/run"))
            .respond_with(
                ResponseTemplate::new(502)
                    .set_body_json(json!({ "message": "bad gateway", "upstream": "down" })),
            )
            .mount(&server)
            .await;

        let err = client(&server.uri()).run("helper", "hi").await.unwrap_err();
        assert!(matches!(err, AgentsError::Server(_)), "got {err:?}");
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

    #[tokio::test]
    async fn register_session_posts_and_parses_id() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/agents/sessions"))
            .and(header(AUTHORIZATION.as_str(), "Bearer jwt-123"))
            .and(body_json(
                serde_json::json!({ "agent": "hanzo-code", "host": "mac" }),
            ))
            .respond_with(ResponseTemplate::new(201).set_body_json(serde_json::json!({
                "id": "sess_abc", "agent": "hanzo-code", "status": "running"
            })))
            .mount(&server)
            .await;
        let req = SessionRegister {
            agent: "hanzo-code".into(),
            host: "mac".into(),
            ..Default::default()
        };
        let sess = client(&server.uri()).register_session(&req).await.unwrap();
        assert_eq!(sess.id, "sess_abc");
        assert_eq!(sess.status, "running");
    }

    #[tokio::test]
    async fn drain_control_parses_commands_and_cursor() {
        let server = MockServer::start().await;
        Mock::given(method("GET"))
            .and(path("/v1/agents/sessions/sess_abc/control"))
            .and(query_param("after", "3"))
            .and(header(AUTHORIZATION.as_str(), "Bearer jwt-123"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "commands": [
                    { "seq": 4, "command": "message", "message": "keep going" },
                    { "seq": 5, "command": "stop" }
                ],
                "cursor": 5
            })))
            .mount(&server)
            .await;
        let batch = client(&server.uri())
            .drain_control("sess_abc", 3)
            .await
            .unwrap();
        assert_eq!(batch.cursor, 5);
        assert_eq!(batch.commands.len(), 2);
        assert_eq!(batch.commands[0].command, "message");
        assert_eq!(batch.commands[0].message, "keep going");
        assert_eq!(batch.commands[1].command, "stop");
        assert_eq!(batch.commands[1].seq, 5);
    }

    #[tokio::test]
    async fn append_event_posts_kind_and_payload() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/agents/sessions/sess_abc/events"))
            .and(header(AUTHORIZATION.as_str(), "Bearer jwt-123"))
            .and(body_json(serde_json::json!({
                "kind": "message",
                "payload": { "role": "assistant", "text": "hi" }
            })))
            .respond_with(ResponseTemplate::new(201))
            .mount(&server)
            .await;
        client(&server.uri())
            .append_session_event(
                "sess_abc",
                "message",
                serde_json::json!({ "role": "assistant", "text": "hi" }),
            )
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn patch_status_and_stop() {
        let server = MockServer::start().await;
        Mock::given(method("PATCH"))
            .and(path("/v1/agents/sessions/sess_abc"))
            .and(body_json(serde_json::json!({ "status": "done" })))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "id": "sess_abc", "status": "done"
            })))
            .mount(&server)
            .await;
        Mock::given(method("POST"))
            .and(path("/v1/agents/sessions/sess_abc/stop"))
            .respond_with(ResponseTemplate::new(200))
            .mount(&server)
            .await;
        let c = client(&server.uri());
        assert_eq!(
            c.patch_session_status("sess_abc", "done")
                .await
                .unwrap()
                .status,
            "done"
        );
        c.stop_session("sess_abc").await.unwrap();
    }

    #[tokio::test]
    async fn session_event_surfaces_server_error() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/agents/sessions/sess_x/events"))
            .respond_with(
                ResponseTemplate::new(404)
                    .set_body_json(serde_json::json!({ "error": "session not found" })),
            )
            .mount(&server)
            .await;
        let err = client(&server.uri())
            .append_session_event("sess_x", "status", serde_json::json!({ "status": "done" }))
            .await
            .unwrap_err();
        assert!(matches!(err, AgentsError::Server(_)), "got {err:?}");
        assert!(err.to_string().contains("session not found"));
    }
}
