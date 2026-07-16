//! Content-free reward-signal collection for router training.
//!
//! This crate sends *content-free* feedback signals — an id, an enum, and (only
//! for ratings) a small integer — to the Hanzo gateway so router training gets
//! production feedback. It NEVER transmits any prompt, response, file, or code
//! text; the wire body is exactly `{request_id, signal}` plus an optional
//! `rating`, and nothing else.
//!
//! Separation of concerns: building the request is separate from firing it.
//! [`RewardSignals::send`] builds the body synchronously and spawns a detached
//! task that POSTs and ignores the result. It never blocks the UX and is a
//! silent no-op on any failure — network error, non-2xx, missing id, local
//! opt-out, or a non-Hanzo provider.
//!
//! Privacy note: server-side org/user *training* opt-in is the preferred
//! enforcement point, but it is not present yet. Until it lands, the client
//! sends unconditionally whenever the caller has not locally opted out (env
//! `HANZO_FEEDBACK=0|false|off` or the `reward_signals` config key) and the
//! active provider is the Hanzo gateway.

use std::sync::Arc;
use std::time::Duration;

use serde::Serialize;
use serde::Serializer;
use serde_json::Map;
use serde_json::Value;

/// A content-free reward signal.
///
/// Serializes to the exact wire strings the `/v1/feedback` endpoint expects.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Signal {
    Up,
    Down,
    Regenerate,
    Switch,
    Abandon,
    Accept,
    Revert,
    /// User quality rating. Valid ratings are `1..=3`; `0` is not a rating (use
    /// [`Signal::Dismiss`]). Out-of-range values are clamped into `1..=3`.
    Rating(u8),
    /// User declined to rate ("no opinion"). Carries no `rating` field; it is a
    /// real signal (retained for prompt-fatigue analytics), not a low score.
    Dismiss,
}

impl Signal {
    /// The wire string for this signal's `signal` field.
    pub fn wire(self) -> &'static str {
        match self {
            Signal::Up => "up",
            Signal::Down => "down",
            Signal::Regenerate => "regenerate",
            Signal::Switch => "switch",
            Signal::Abandon => "abandon",
            Signal::Accept => "accept",
            Signal::Revert => "revert",
            Signal::Rating(_) => "rating",
            Signal::Dismiss => "dismiss",
        }
    }

    /// The validated rating for `Rating`, clamped to `1..=3`. `None` for every
    /// other signal, so no `rating` field is ever emitted for them.
    pub fn rating(self) -> Option<u8> {
        match self {
            Signal::Rating(n) => Some(n.clamp(1, 3)),
            _ => None,
        }
    }
}

impl Serialize for Signal {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.wire())
    }
}

/// Build the content-free request body: `request_id` + `signal` and, only for a
/// rating, `rating`. By construction this can never carry prompt/response text.
fn build_body(request_id: &str, signal: Signal) -> Value {
    let mut map = Map::new();
    map.insert("request_id".to_string(), Value::String(request_id.to_string()));
    map.insert("signal".to_string(), Value::String(signal.wire().to_string()));
    if let Some(rating) = signal.rating() {
        map.insert("rating".to_string(), Value::Number(rating.into()));
    }
    Value::Object(map)
}

/// Returns true when `base_url` points at the Hanzo gateway. Sends to any other
/// provider (raw OpenAI, local, ollama, …) are pointless — the `request_id`
/// does not exist in the Hanzo ledger — so they are gated off.
fn is_hanzo_gateway(base_url: &str) -> bool {
    base_url.contains("hanzo.ai")
}

/// Derive the `/v1/feedback` endpoint from a provider base URL.
///
/// The provider base already includes the API version (e.g.
/// `https://api.hanzo.ai/v1`), so we strip a trailing `/v1` (and slashes) to
/// recover the gateway root and then append `/v1/feedback`, matching the
/// endpoint contract `{gateway_base}/v1/feedback`.
fn feedback_url(base_url: &str) -> Option<String> {
    let trimmed = base_url.trim();
    if trimmed.is_empty() {
        return None;
    }
    let root = trimmed.trim_end_matches('/');
    let root = root.strip_suffix("/v1").unwrap_or(root);
    let root = root.trim_end_matches('/');
    Some(format!("{root}/v1/feedback"))
}

struct Inner {
    client: reqwest::Client,
    url: String,
    token: Option<String>,
}

impl Inner {
    async fn post(&self, body: Value) -> bool {
        let mut request = self.client.post(&self.url).json(&body);
        if let Some(token) = &self.token {
            request = request.bearer_auth(token);
        }
        match request.send().await {
            Ok(response) => response.status().is_success(),
            Err(_) => false,
        }
    }
}

/// Fire-and-forget reward-signal client.
///
/// Cheap to clone (shares one [`reqwest::Client`]). A [`RewardSignals::disabled`]
/// client — or one built for a non-Hanzo provider / while opted out — silently
/// drops every send.
#[derive(Clone)]
pub struct RewardSignals {
    inner: Option<Arc<Inner>>,
}

impl std::fmt::Debug for RewardSignals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RewardSignals")
            .field("enabled", &self.is_enabled())
            .finish()
    }
}

impl RewardSignals {
    /// A client that never sends anything.
    pub fn disabled() -> Self {
        Self { inner: None }
    }

    /// Build a client for the active provider.
    ///
    /// Returns a disabled (no-op) client unless all hold:
    /// - `enabled` is true (the caller's local opt-out has already been applied);
    /// - `base_url` is present and points at the Hanzo gateway; and
    /// - a reqwest client can be constructed.
    ///
    /// `token` is the same bearer/api-key the model client uses for this
    /// provider; when absent the POST is still attempted and simply 401s into a
    /// silent no-op.
    pub fn new(base_url: Option<&str>, token: Option<String>, enabled: bool) -> Self {
        if !enabled {
            return Self::disabled();
        }
        let Some(base_url) = base_url else {
            return Self::disabled();
        };
        if !is_hanzo_gateway(base_url) {
            return Self::disabled();
        }
        let Some(url) = feedback_url(base_url) else {
            return Self::disabled();
        };
        let Ok(client) = reqwest::Client::builder()
            .timeout(Duration::from_secs(5))
            .build()
        else {
            return Self::disabled();
        };
        Self {
            inner: Some(Arc::new(Inner { client, url, token })),
        }
    }

    /// True when sends will actually be attempted.
    pub fn is_enabled(&self) -> bool {
        self.inner.is_some()
    }

    /// Fire-and-forget a reward signal for `request_id`.
    ///
    /// Never blocks: builds the body inline, then spawns a detached task to POST
    /// it. Silent no-op when disabled or when `request_id` is empty.
    pub fn send(&self, request_id: impl Into<String>, signal: Signal) {
        let Some(inner) = self.inner.clone() else {
            return;
        };
        let request_id = request_id.into();
        if request_id.is_empty() {
            return;
        }
        let body = build_body(&request_id, signal);
        tokio::spawn(async move {
            let _ = inner.post(body).await;
        });
    }

    /// Send a reward signal and await delivery, bounded by the client timeout.
    ///
    /// Intended for shutdown, where a detached [`RewardSignals::send`] task would
    /// be killed as the process exits before the POST completes. Silent no-op
    /// when disabled or when `request_id` is empty; the result is ignored.
    pub async fn send_now(&self, request_id: impl Into<String>, signal: Signal) {
        let Some(inner) = self.inner.as_ref() else {
            return;
        };
        let request_id = request_id.into();
        if request_id.is_empty() {
            return;
        }
        let _ = inner.post(build_body(&request_id, signal)).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use wiremock::Mock;
    use wiremock::MockServer;
    use wiremock::ResponseTemplate;
    use wiremock::matchers::body_json;
    use wiremock::matchers::header;
    use wiremock::matchers::method;
    use wiremock::matchers::path;

    fn enabled_for(url: &str, token: Option<&str>) -> RewardSignals {
        RewardSignals {
            inner: Some(Arc::new(Inner {
                client: reqwest::Client::new(),
                url: url.to_string(),
                token: token.map(str::to_string),
            })),
        }
    }

    #[test]
    fn signal_serializes_to_exact_wire_strings() {
        let cases = [
            (Signal::Up, "up"),
            (Signal::Down, "down"),
            (Signal::Regenerate, "regenerate"),
            (Signal::Switch, "switch"),
            (Signal::Abandon, "abandon"),
            (Signal::Accept, "accept"),
            (Signal::Revert, "revert"),
            (Signal::Rating(2), "rating"),
            (Signal::Dismiss, "dismiss"),
        ];
        for (signal, wire) in cases {
            assert_eq!(signal.wire(), wire);
            assert_eq!(serde_json::to_value(signal).unwrap(), Value::String(wire.into()));
        }
    }

    #[test]
    fn rating_included_only_for_rating_and_clamped_1_to_3() {
        assert_eq!(Signal::Rating(1).rating(), Some(1));
        assert_eq!(Signal::Rating(2).rating(), Some(2));
        assert_eq!(Signal::Rating(3).rating(), Some(3));
        // Out-of-range clamps into 1..=3 (0 and >3 never reach the wire as-is).
        assert_eq!(Signal::Rating(0).rating(), Some(1));
        assert_eq!(Signal::Rating(9).rating(), Some(3));
        // No other signal carries a rating.
        assert_eq!(Signal::Dismiss.rating(), None);
        assert_eq!(Signal::Regenerate.rating(), None);
        assert_eq!(Signal::Abandon.rating(), None);
    }

    #[test]
    fn body_carries_only_id_signal_and_optional_rating() {
        // Rating body has exactly three keys.
        let rating = build_body("resp_123", Signal::Rating(3));
        let obj = rating.as_object().unwrap();
        let mut keys: Vec<&str> = obj.keys().map(String::as_str).collect();
        keys.sort_unstable();
        assert_eq!(keys, vec!["rating", "request_id", "signal"]);
        assert_eq!(obj["request_id"], Value::String("resp_123".into()));
        assert_eq!(obj["signal"], Value::String("rating".into()));
        assert_eq!(obj["rating"], Value::Number(3u8.into()));

        // Dismiss carries NO rating field: exactly {request_id, signal}.
        let dismiss = build_body("resp_123", Signal::Dismiss);
        let obj = dismiss.as_object().unwrap();
        let mut keys: Vec<&str> = obj.keys().map(String::as_str).collect();
        keys.sort_unstable();
        assert_eq!(keys, vec!["request_id", "signal"]);
        assert_eq!(obj["signal"], Value::String("dismiss".into()));

        // Regenerate / abandon: two keys, no rating.
        for signal in [Signal::Regenerate, Signal::Abandon] {
            let body = build_body("resp_abc", signal);
            let obj = body.as_object().unwrap();
            assert_eq!(obj.len(), 2, "{signal:?} must not carry a rating");
            assert!(obj.contains_key("request_id"));
            assert!(obj.contains_key("signal"));
        }
    }

    #[test]
    fn exact_dismiss_wire_body() {
        assert_eq!(
            build_body("resp_xyz", Signal::Dismiss),
            serde_json::json!({"request_id": "resp_xyz", "signal": "dismiss"}),
        );
    }

    #[test]
    fn feedback_url_normalizes_versioned_and_bare_bases() {
        assert_eq!(
            feedback_url("https://api.hanzo.ai/v1").as_deref(),
            Some("https://api.hanzo.ai/v1/feedback"),
        );
        assert_eq!(
            feedback_url("https://api.hanzo.ai/v1/").as_deref(),
            Some("https://api.hanzo.ai/v1/feedback"),
        );
        assert_eq!(
            feedback_url("https://api.hanzo.ai").as_deref(),
            Some("https://api.hanzo.ai/v1/feedback"),
        );
        assert_eq!(feedback_url("").as_deref(), None);
    }

    #[test]
    fn opt_out_and_non_hanzo_provider_yield_disabled_client() {
        // enabled=false always disables, even on the Hanzo gateway.
        assert!(!RewardSignals::new(Some("https://api.hanzo.ai/v1"), None, false).is_enabled());
        // Non-Hanzo providers are gated off even when enabled.
        assert!(!RewardSignals::new(Some("https://api.openai.com/v1"), None, true).is_enabled());
        assert!(!RewardSignals::new(Some("http://localhost:11434/v1"), None, true).is_enabled());
        assert!(!RewardSignals::new(None, None, true).is_enabled());
        // Hanzo gateway + enabled => live client.
        assert!(RewardSignals::new(Some("https://api.hanzo.ai/v1"), None, true).is_enabled());
    }

    #[test]
    fn disabled_client_send_is_noop() {
        // No panic, no runtime required: disabled send never spawns.
        RewardSignals::disabled().send("resp_1", Signal::Rating(3));
    }

    #[tokio::test]
    async fn posts_to_v1_feedback_with_bearer_and_json_body() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/feedback"))
            .and(header("authorization", "Bearer tok_secret"))
            .and(body_json(
                serde_json::json!({"request_id": "resp_42", "signal": "regenerate"}),
            ))
            .respond_with(ResponseTemplate::new(200))
            .expect(1)
            .mount(&server)
            .await;

        let url = feedback_url(&server.uri()).unwrap();
        let client = enabled_for(&url, Some("tok_secret"));
        // Exercise the same path `send` uses, but await it deterministically.
        assert!(client.inner.as_ref().unwrap().post(build_body("resp_42", Signal::Regenerate)).await);
        // `expect(1)` is verified on drop.
    }

    #[tokio::test]
    async fn rating_body_reaches_server_with_rating_field() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/feedback"))
            .and(body_json(
                serde_json::json!({"request_id": "resp_7", "signal": "rating", "rating": 3}),
            ))
            .respond_with(ResponseTemplate::new(204))
            .expect(1)
            .mount(&server)
            .await;

        let url = feedback_url(&server.uri()).unwrap();
        let inner = Inner {
            client: reqwest::Client::new(),
            url,
            token: None,
        };
        assert!(inner.post(build_body("resp_7", Signal::Rating(3))).await);
    }

    #[tokio::test]
    async fn non_2xx_is_reported_as_failure() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/feedback"))
            .respond_with(ResponseTemplate::new(500))
            .mount(&server)
            .await;

        let url = feedback_url(&server.uri()).unwrap();
        let inner = Inner {
            client: reqwest::Client::new(),
            url,
            token: None,
        };
        assert!(!inner.post(build_body("resp_9", Signal::Abandon)).await);
    }
}
