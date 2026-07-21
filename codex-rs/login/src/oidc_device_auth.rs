//! RFC 8628 OAuth 2.0 Device Authorization Grant for standards-compliant OIDC
//! issuers — i.e. Hanzo IAM (hanzo.id / lux.id / zoolabs.id).
//!
//! This is deliberately separate from [`crate::device_code_auth`], which speaks
//! OpenAI's proprietary `/api/accounts/deviceauth/*` + PKCE scheme. This module
//! is plain RFC 8628 against an OIDC issuer:
//!
//! 1. `POST {issuer}/oauth/device?client_id=…&scope=…`
//!    → `{ device_code, user_code, verification_uri[_complete], expires_in, interval }`
//! 2. `POST {issuer}/oauth/token`
//!    `grant_type=urn:ietf:params:oauth:grant-type:device_code&device_code=…&client_id=…`
//!    → `{ access_token, id_token, refresh_token }`, or an OAuth error
//!    (`authorization_pending` / `slow_down` / `access_denied` / `expired_token`).
//!
//! It is brand-agnostic: the issuer and client_id come from [`ServerOptions`],
//! so the same flow serves every Hanzo-IAM brand. Tokens are persisted through
//! the same store the browser flow uses, so `dev` reads them back unchanged.

use std::io;
use std::time::Duration;
use std::time::Instant;

use serde::Deserialize;

use crate::server::ServerOptions;
use crate::server::persist_tokens_async;
use codex_http_client::build_reqwest_client_with_custom_ca;

const ANSI_BLUE: &str = "\x1b[94m";
const ANSI_GRAY: &str = "\x1b[90m";
const ANSI_RESET: &str = "\x1b[0m";

/// RFC 8628 §3.5 default polling interval when the server omits one.
const DEFAULT_INTERVAL_SECS: u64 = 5;
const MIN_INTERVAL_SECS: u64 = 1;
const MAX_INTERVAL_SECS: u64 = 60;
/// RFC 8628 §3.2 default lifetime when the server omits `expires_in`.
const DEFAULT_EXPIRES_IN_SECS: u64 = 900;
/// Ceiling on a server-provided `expires_in`: bounds polling and prevents a
/// hostile/buggy issuer from overflowing `Instant + Duration` or pinning the
/// CLI in a poll loop for years.
const MAX_EXPIRES_IN_SECS: u64 = 1800;
/// `slow_down` (RFC 8628 §3.5) requires increasing the interval by 5s.
const SLOW_DOWN_BACKOFF_SECS: u64 = 5;

/// The default scope requested for a Hanzo IAM device login.
pub const DEFAULT_SCOPE: &str = "openid profile email";

#[derive(Debug, Clone, Deserialize)]
pub struct DeviceAuthorization {
    pub device_code: String,
    pub user_code: String,
    pub verification_uri: String,
    #[serde(default)]
    pub verification_uri_complete: Option<String>,
    #[serde(default)]
    pub expires_in: u64,
    #[serde(default)]
    pub interval: u64,
}

impl DeviceAuthorization {
    /// Clamp the server's interval into a sane band; fall back to the RFC
    /// default when the server omits it (0).
    fn poll_interval(&self) -> u64 {
        if self.interval == 0 {
            DEFAULT_INTERVAL_SECS
        } else {
            self.interval.clamp(MIN_INTERVAL_SECS, MAX_INTERVAL_SECS)
        }
    }

    fn lifetime(&self) -> Duration {
        let secs = if self.expires_in == 0 {
            DEFAULT_EXPIRES_IN_SECS
        } else {
            self.expires_in
        };
        Duration::from_secs(secs.min(MAX_EXPIRES_IN_SECS))
    }

    /// The link to show the user: prefer `verification_uri_complete` (embeds the
    /// user_code for one-click approval), else the bare `verification_uri`.
    fn approval_link(&self) -> &str {
        self.verification_uri_complete
            .as_deref()
            .filter(|s| !s.is_empty())
            .unwrap_or(&self.verification_uri)
    }
}

#[derive(Debug, Deserialize)]
struct TokenSuccess {
    access_token: String,
    #[serde(default)]
    id_token: Option<String>,
    #[serde(default)]
    refresh_token: Option<String>,
}

#[derive(Debug, Deserialize)]
struct OAuthError {
    error: String,
    #[serde(default)]
    error_description: Option<String>,
}

/// The outcome of one poll of the token endpoint.
#[derive(Debug)]
enum Poll {
    Token(TokenSuccess),
    /// Keep waiting at the current interval (`authorization_pending`).
    Pending,
    /// Keep waiting but back off the interval (`slow_down`).
    SlowDown,
}

/// Classify a single token-endpoint response. Pure (no reqwest types) so the
/// error mapping is unit-testable. A successful token wins; otherwise the body
/// is parsed as an OAuth error and mapped to pending/slow_down (retry) or a
/// terminal `io::Error`.
fn classify_poll(is_success: bool, body: &str) -> io::Result<Poll> {
    if is_success
        && let Ok(tok) = serde_json::from_str::<TokenSuccess>(body)
        && !tok.access_token.is_empty()
    {
        return Ok(Poll::Token(tok));
    }

    let err: OAuthError = serde_json::from_str(body)
        .map_err(|_| io::Error::other(format!("unexpected token response: {body}")))?;

    match err.error.as_str() {
        "authorization_pending" => Ok(Poll::Pending),
        "slow_down" => Ok(Poll::SlowDown),
        "access_denied" => Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            "device authorization was denied",
        )),
        "expired_token" => Err(io::Error::other(
            "the device code expired before approval; run login again",
        )),
        other => Err(io::Error::other(match err.error_description {
            Some(desc) if !desc.is_empty() => {
                format!("device authorization failed: {other} — {desc}")
            }
            _ => format!("device authorization failed: {other}"),
        })),
    }
}

async fn request_device_authorization(
    client: &reqwest::Client,
    issuer: &str,
    client_id: &str,
    scope: &str,
) -> io::Result<DeviceAuthorization> {
    let url = format!("{issuer}/oauth/device");
    let resp = client
        .post(&url)
        // client_id/scope as query params: IAM reads them via Beego Query()
        // (the same shape verified against the live device endpoint).
        .query(&[
            ("client_id", client_id),
            ("scope", scope),
            ("response_type", "device_code"),
        ])
        .header("Content-Type", "application/x-www-form-urlencoded")
        .send()
        .await
        .map_err(io::Error::other)?;

    let status = resp.status();
    let body = resp.text().await.map_err(io::Error::other)?;
    if !status.is_success() {
        return Err(io::Error::other(format!(
            "device authorization request failed ({status}): {body}"
        )));
    }

    let da: DeviceAuthorization = serde_json::from_str(&body).map_err(|e| {
        io::Error::other(format!(
            "invalid device authorization response: {e}: {body}"
        ))
    })?;
    if da.device_code.is_empty() || da.user_code.is_empty() {
        // A JSON 200 that is actually an error envelope (e.g. invalid client_id).
        return Err(io::Error::other(format!(
            "device authorization response missing device_code/user_code: {body}"
        )));
    }
    Ok(da)
}

async fn poll_for_token(
    client: &reqwest::Client,
    issuer: &str,
    client_id: &str,
    da: &DeviceAuthorization,
) -> io::Result<TokenSuccess> {
    let url = format!("{issuer}/oauth/token");
    let deadline = Instant::now() + da.lifetime();
    let mut interval = da.poll_interval();

    loop {
        if Instant::now() >= deadline {
            return Err(io::Error::other(
                "the device code expired before approval; run login again",
            ));
        }

        let resp = client
            .post(&url)
            .header("Content-Type", "application/x-www-form-urlencoded")
            .form(&[
                ("grant_type", "urn:ietf:params:oauth:grant-type:device_code"),
                ("device_code", da.device_code.as_str()),
                ("client_id", client_id),
            ])
            .send()
            .await
            .map_err(io::Error::other)?;

        let is_success = resp.status().is_success();
        let body = resp.text().await.map_err(io::Error::other)?;

        match classify_poll(is_success, &body)? {
            Poll::Token(tok) => return Ok(tok),
            Poll::Pending => {}
            Poll::SlowDown => {
                interval = (interval + SLOW_DOWN_BACKOFF_SECS).min(MAX_INTERVAL_SECS);
            }
        }

        let remaining = deadline.saturating_duration_since(Instant::now());
        tokio::time::sleep(Duration::from_secs(interval).min(remaining)).await;
    }
}

fn print_prompt(da: &DeviceAuthorization) {
    eprintln!(
        "\nSign in with Hanzo ID using device authorization:\n\
\n1. Open this link and sign in:\n   {ANSI_BLUE}{link}{ANSI_RESET}\n\
\n2. Confirm this one-time code matches what the page shows:\n   {ANSI_BLUE}{code}{ANSI_RESET}\n\
\n{ANSI_GRAY}Device codes are a common phishing target. Never share this code.{ANSI_RESET}\n",
        link = da.approval_link(),
        code = da.user_code,
    );
}

/// Run the full RFC 8628 device authorization login against an OIDC issuer and
/// persist the resulting tokens. `opts.issuer` is the issuer base (e.g.
/// `https://hanzo.id/v1/iam`) and `opts.client_id` the public client.
pub async fn run_oidc_device_code_login(opts: ServerOptions, scope: &str) -> io::Result<()> {
    let client = build_reqwest_client_with_custom_ca(reqwest::Client::builder())?;
    let issuer = opts.issuer.trim_end_matches('/').to_string();

    let da = request_device_authorization(&client, &issuer, &opts.client_id, scope).await?;
    print_prompt(&da);
    let tok = poll_for_token(&client, &issuer, &opts.client_id, &da).await?;

    // IAM returns id_token == access_token (a JWT); fall back to it if absent.
    let id_token = tok
        .id_token
        .clone()
        .unwrap_or_else(|| tok.access_token.clone());
    let refresh_token = tok.refresh_token.clone().unwrap_or_default();

    persist_tokens_async(
        &opts.codex_home,
        /* api_key */ None,
        id_token,
        tok.access_token,
        refresh_token,
        opts.cli_auth_credentials_store_mode,
        opts.auth_keyring_backend_kind,
    )
    .await
}

#[cfg(test)]
mod tests {
    use super::*;

    fn da(interval: u64, expires_in: u64) -> DeviceAuthorization {
        DeviceAuthorization {
            device_code: "dc".into(),
            user_code: "WDJB-MJHT".into(),
            verification_uri: "https://hanzo.id/v1/iam/oauth/device".into(),
            verification_uri_complete: None,
            expires_in,
            interval,
        }
    }

    #[test]
    fn poll_interval_defaults_and_clamps() {
        assert_eq!(da(0, 0).poll_interval(), DEFAULT_INTERVAL_SECS);
        assert_eq!(da(3, 0).poll_interval(), 3);
        assert_eq!(da(9999, 0).poll_interval(), MAX_INTERVAL_SECS);
    }

    #[test]
    fn lifetime_defaults_and_clamps() {
        assert_eq!(
            da(5, 0).lifetime(),
            Duration::from_secs(DEFAULT_EXPIRES_IN_SECS)
        );
        assert_eq!(da(5, 120).lifetime(), Duration::from_secs(120));
        // a hostile/huge expires_in is clamped, never overflowing Instant+Duration.
        assert_eq!(
            da(5, 999_999).lifetime(),
            Duration::from_secs(MAX_EXPIRES_IN_SECS)
        );
        assert_eq!(
            da(5, u64::MAX).lifetime(),
            Duration::from_secs(MAX_EXPIRES_IN_SECS)
        );
    }

    #[test]
    fn approval_link_prefers_complete() {
        let mut d = da(5, 900);
        assert_eq!(d.approval_link(), "https://hanzo.id/v1/iam/oauth/device");
        d.verification_uri_complete = Some("https://hanzo.id/v1/iam/oauth/device/WDJB-MJHT".into());
        assert_eq!(
            d.approval_link(),
            "https://hanzo.id/v1/iam/oauth/device/WDJB-MJHT"
        );
        d.verification_uri_complete = Some(String::new());
        assert_eq!(d.approval_link(), "https://hanzo.id/v1/iam/oauth/device");
    }

    #[test]
    fn deserializes_iam_device_response() {
        let body = r#"{"device_code":"abc","user_code":"q92rbp",
            "verification_uri":"https://hanzo.id/v1/iam/oauth/device/q92rbp",
            "verification_uri_complete":"https://hanzo.id/v1/iam/oauth/device/q92rbp",
            "expires_in":900,"interval":5}"#;
        let da: DeviceAuthorization = serde_json::from_str(body).expect("parse");
        assert_eq!(da.device_code, "abc");
        assert_eq!(da.user_code, "q92rbp");
        assert_eq!(da.expires_in, 900);
        assert_eq!(da.poll_interval(), 5);
    }

    #[test]
    fn classify_success_token() {
        let body = r#"{"access_token":"at","id_token":"it","refresh_token":"rt","token_type":"Bearer","expires_in":1209600}"#;
        match classify_poll(true, body).expect("ok") {
            Poll::Token(t) => {
                assert_eq!(t.access_token, "at");
                assert_eq!(t.id_token.as_deref(), Some("it"));
                assert_eq!(t.refresh_token.as_deref(), Some("rt"));
            }
            _ => panic!("expected token"),
        }
    }

    #[test]
    fn classify_pending_and_slow_down_retry() {
        assert!(matches!(
            classify_poll(false, r#"{"error":"authorization_pending"}"#).unwrap(),
            Poll::Pending
        ));
        assert!(matches!(
            classify_poll(false, r#"{"error":"slow_down"}"#).unwrap(),
            Poll::SlowDown
        ));
    }

    #[test]
    fn classify_terminal_errors() {
        let denied = classify_poll(false, r#"{"error":"access_denied"}"#).unwrap_err();
        assert_eq!(denied.kind(), io::ErrorKind::PermissionDenied);

        let expired = classify_poll(false, r#"{"error":"expired_token"}"#).unwrap_err();
        assert!(expired.to_string().contains("expired"));

        let other = classify_poll(
            false,
            r#"{"error":"unsupported_grant_type","error_description":"nope"}"#,
        )
        .unwrap_err();
        assert!(other.to_string().contains("unsupported_grant_type"));
        assert!(other.to_string().contains("nope"));
    }

    #[test]
    fn classify_unparseable_is_error() {
        assert!(classify_poll(false, "<html>502</html>").is_err());
        // A 200 with no access_token and no error shape is also an error.
        assert!(classify_poll(true, r#"{"foo":"bar"}"#).is_err());
    }
}
