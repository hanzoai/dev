//! RFC 8628 OAuth 2.0 Device Authorization Grant against a standards OIDC
//! issuer — Hanzo IAM (hanzo.id / lux.id / zoolabs.id).
//!
//! Deliberately separate from [`crate::device_code_auth`], which speaks
//! OpenAI's proprietary `/api/accounts/deviceauth` + PKCE scheme. Here it is
//! plain RFC 8628:
//!
//! 1. `POST {issuer}/oauth/device` (form: `client_id`, `scope`)
//!    → `{ device_code, user_code, verification_uri[_complete], expires_in, interval }`
//! 2. `POST {issuer}/oauth/token`
//!    `grant_type=urn:ietf:params:oauth:grant-type:device_code&device_code=…&client_id=…`
//!    → `{ access_token, id_token, refresh_token }`, or an OAuth error
//!    (`authorization_pending` / `slow_down` / `access_denied` / `expired_token`).
//!
//! Issuer and client come from [`ServerOptions`], so one flow serves every
//! brand. Tokens are persisted through the store the browser flow already
//! writes, so `dev` reads them back unchanged.

use std::io;
use std::time::Duration;
use std::time::Instant;

use serde::Deserialize;

use crate::server::ServerOptions;
use crate::server::persist_tokens_async;
use code_core::default_client::create_client;

const ANSI_BLUE: &str = "\x1b[94m";
const ANSI_GRAY: &str = "\x1b[90m";
const ANSI_RESET: &str = "\x1b[0m";

/// RFC 8628 §3.5 default polling interval when the issuer omits one.
const DEFAULT_INTERVAL_SECS: u64 = 5;
const MIN_INTERVAL_SECS: u64 = 1;
const MAX_INTERVAL_SECS: u64 = 60;
/// RFC 8628 §3.2 default lifetime when the issuer omits `expires_in`.
const DEFAULT_EXPIRES_IN_SECS: u64 = 900;
/// Ceiling on an issuer-provided `expires_in`: bounds polling and stops a
/// hostile or buggy issuer from overflowing `Instant + Duration` or pinning the
/// CLI in a poll loop for years.
const MAX_EXPIRES_IN_SECS: u64 = 1800;
/// `slow_down` (RFC 8628 §3.5) requires increasing the interval by 5s.
const SLOW_DOWN_BACKOFF_SECS: u64 = 5;

/// `offline_access` earns the refresh token the auth store expects; the rest is
/// the identity every brand's IAM advertises under `scopes_supported`.
const SCOPE: &str = "openid profile email offline_access";

#[derive(Debug, Clone, Deserialize)]
struct DeviceAuth {
    device_code: String,
    user_code: String,
    verification_uri: String,
    #[serde(default)]
    verification_uri_complete: Option<String>,
    #[serde(default)]
    expires_in: u64,
    #[serde(default)]
    interval: u64,
}

impl DeviceAuth {
    /// Clamp the issuer's interval into a sane band; fall back to the RFC
    /// default when the issuer omits it (0).
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

    /// The link to show: prefer `verification_uri_complete` (it embeds the user
    /// code for one-click approval), else the bare `verification_uri`.
    fn link(&self) -> &str {
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
    if is_success {
        if let Ok(tok) = serde_json::from_str::<TokenSuccess>(body) {
            if !tok.access_token.is_empty() {
                return Ok(Poll::Token(tok));
            }
        }
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

async fn request_device_auth(
    client: &reqwest::Client,
    issuer: &str,
    client_id: &str,
) -> io::Result<DeviceAuth> {
    let resp = client
        .post(format!("{issuer}/oauth/device"))
        .form(&[
            ("client_id", client_id),
            ("scope", SCOPE),
            ("response_type", "device_code"),
        ])
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

    let auth: DeviceAuth = serde_json::from_str(&body)
        .map_err(|e| io::Error::other(format!("invalid device authorization response: {e}: {body}")))?;
    if auth.device_code.is_empty() || auth.user_code.is_empty() {
        // A JSON 200 that is really an error envelope (e.g. an invalid client).
        return Err(io::Error::other(format!(
            "device authorization response missing device_code/user_code: {body}"
        )));
    }
    Ok(auth)
}

async fn poll_for_token(
    client: &reqwest::Client,
    issuer: &str,
    client_id: &str,
    auth: &DeviceAuth,
) -> io::Result<TokenSuccess> {
    let url = format!("{issuer}/oauth/token");
    let deadline = Instant::now() + auth.lifetime();
    let mut interval = auth.poll_interval();

    loop {
        if Instant::now() >= deadline {
            return Err(io::Error::other(
                "the device code expired before approval; run login again",
            ));
        }

        let resp = client
            .post(&url)
            .form(&[
                ("grant_type", "urn:ietf:params:oauth:grant-type:device_code"),
                ("device_code", auth.device_code.as_str()),
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

/// An approved-or-not device authorization, in flight. Split so a terminal
/// caller can print the code while a TUI renders it, over one implementation.
pub struct DeviceGrant {
    client: reqwest::Client,
    opts: ServerOptions,
    auth: DeviceAuth,
}

impl DeviceGrant {
    /// Ask the issuer for a device code. `opts.issuer` is the issuer base (e.g.
    /// `https://hanzo.id/v1/iam`) and `opts.client_id` the public client.
    pub async fn start(mut opts: ServerOptions) -> io::Result<Self> {
        opts.issuer = opts.issuer.trim_end_matches('/').to_string();
        let client = create_client(&opts.originator);
        let auth = request_device_auth(&client, &opts.issuer, &opts.client_id).await?;
        Ok(Self { client, opts, auth })
    }

    /// Where the user approves this device.
    pub fn link(&self) -> &str {
        self.auth.link()
    }

    /// The code the user confirms against the page.
    pub fn user_code(&self) -> &str {
        &self.auth.user_code
    }

    /// Poll until the user approves, then persist the tokens.
    pub async fn wait(self) -> io::Result<()> {
        let TokenSuccess {
            access_token,
            id_token,
            refresh_token,
        } = poll_for_token(
            &self.client,
            &self.opts.issuer,
            &self.opts.client_id,
            &self.auth,
        )
        .await?;

        // IAM returns an id_token alongside the access token; fall back to the
        // access token (also a JWT) when the issuer omits it.
        let id_token = id_token.unwrap_or_else(|| access_token.clone());
        persist_tokens_async(
            &self.opts.code_home,
            None,
            id_token,
            access_token,
            refresh_token.unwrap_or_default(),
        )
        .await
    }

    /// The whole flow for a terminal: ask, print, wait.
    pub async fn run(opts: ServerOptions) -> io::Result<()> {
        let grant = Self::start(opts).await?;
        eprintln!(
            "\nSign in to authorize this device:\n\
\n1. Open this link and sign in:\n   {ANSI_BLUE}{link}{ANSI_RESET}\n\
\n2. Confirm this one-time code matches what the page shows:\n   {ANSI_BLUE}{code}{ANSI_RESET}\n\
\n{ANSI_GRAY}Device codes are a common phishing target. Never share this code.{ANSI_RESET}\n",
            link = grant.link(),
            code = grant.user_code(),
        );
        grant.wait().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn auth(interval: u64, expires_in: u64) -> DeviceAuth {
        DeviceAuth {
            device_code: "dc".into(),
            user_code: "WDJB-MJHT".into(),
            verification_uri: "https://hanzo.id/login/oauth/device".into(),
            verification_uri_complete: None,
            expires_in,
            interval,
        }
    }

    #[test]
    fn poll_interval_defaults_and_clamps() {
        assert_eq!(auth(0, 0).poll_interval(), DEFAULT_INTERVAL_SECS);
        assert_eq!(auth(3, 0).poll_interval(), 3);
        assert_eq!(auth(9999, 0).poll_interval(), MAX_INTERVAL_SECS);
    }

    #[test]
    fn lifetime_defaults_and_clamps() {
        assert_eq!(
            auth(5, 0).lifetime(),
            Duration::from_secs(DEFAULT_EXPIRES_IN_SECS)
        );
        assert_eq!(auth(5, 120).lifetime(), Duration::from_secs(120));
        // a hostile or huge expires_in is clamped, never overflowing Instant+Duration.
        assert_eq!(
            auth(5, 999_999).lifetime(),
            Duration::from_secs(MAX_EXPIRES_IN_SECS)
        );
        assert_eq!(
            auth(5, u64::MAX).lifetime(),
            Duration::from_secs(MAX_EXPIRES_IN_SECS)
        );
    }

    #[test]
    fn link_prefers_complete() {
        let mut a = auth(5, 900);
        assert_eq!(a.link(), "https://hanzo.id/login/oauth/device");
        a.verification_uri_complete = Some("https://hanzo.id/login/oauth/device/WDJB-MJHT".into());
        assert_eq!(a.link(), "https://hanzo.id/login/oauth/device/WDJB-MJHT");
        a.verification_uri_complete = Some(String::new());
        assert_eq!(a.link(), "https://hanzo.id/login/oauth/device");
    }

    #[test]
    fn deserializes_iam_device_response() {
        // Verbatim from hanzo.id/v1/iam/oauth/device.
        let body = r#"{"device_code":"CeFNZQ7B6FI7NPfJ2LjkmC6miFdSdtoQVwwzV3gV-ac",
            "user_code":"WAAF4ART",
            "verification_uri":"https://hanzo.id/login/oauth/device",
            "verification_uri_complete":"https://hanzo.id/login/oauth/device/WAAF4ART",
            "expires_in":900,"interval":5}"#;
        let a: DeviceAuth = serde_json::from_str(body).expect("parse");
        assert_eq!(a.device_code, "CeFNZQ7B6FI7NPfJ2LjkmC6miFdSdtoQVwwzV3gV-ac");
        assert_eq!(a.user_code, "WAAF4ART");
        assert_eq!(a.expires_in, 900);
        assert_eq!(a.poll_interval(), 5);
        assert_eq!(a.link(), "https://hanzo.id/login/oauth/device/WAAF4ART");
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
            other => panic!("expected token, got {other:?}"),
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

        // Verbatim from hanzo.id's token endpoint for an unknown device_code.
        let expired = classify_poll(
            false,
            r#"{"error":"expired_token","error_description":"the device code is expired or already redeemed"}"#,
        )
        .unwrap_err();
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
