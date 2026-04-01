//! OAuth token lifecycle: expiry detection and automatic refresh.
//!
//! Ported from claw-code's `api::client` OAuth logic. Provides standalone
//! utilities that work with unix-epoch `expires_at` timestamps (the format
//! used by [`crate::auth`] and stored OAuth credentials).

use std::time::{SystemTime, UNIX_EPOCH};

/// Returns the current wall-clock time as seconds since the Unix epoch.
/// Falls back to `0` if the system clock is before the epoch.
fn now_unix_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_secs())
}

/// Check whether an OAuth token has expired based on its `expires_at`
/// unix-epoch timestamp. Returns `false` when no expiry is recorded
/// (i.e. the token is treated as non-expiring).
#[must_use]
pub fn is_token_expired(expires_at: Option<u64>) -> bool {
    expires_at.is_some_and(|exp| exp <= now_unix_secs())
}

// ---------------------------------------------------------------------------
// Resolve-with-refresh
// ---------------------------------------------------------------------------

/// A successfully resolved token — either the original or a freshly refreshed
/// one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedToken {
    pub access_token: String,
    /// `true` when the returned token came from a refresh exchange rather than
    /// the original credential.
    pub was_refreshed: bool,
}

/// The payload returned by a successful token refresh exchange.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RefreshedToken {
    pub access_token: String,
    pub refresh_token: Option<String>,
    pub expires_at: Option<u64>,
}

/// Errors that can occur when attempting to refresh an OAuth token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenRefreshError {
    /// The token is expired but no refresh token is available.
    NoRefreshToken,
    /// The refresh exchange itself failed.
    Network(String),
}

impl std::fmt::Display for TokenRefreshError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoRefreshToken => write!(f, "token expired and no refresh token available"),
            Self::Network(msg) => write!(f, "token refresh failed: {msg}"),
        }
    }
}

impl std::error::Error for TokenRefreshError {}

/// Resolve an OAuth credential, refreshing transparently when expired.
///
/// - If `expires_at` indicates the token is still valid, returns it directly.
/// - If expired and a `refresh_token` is present, invokes `refresh_fn` to
///   obtain a new access token.
/// - If expired with no refresh token, returns [`TokenRefreshError::NoRefreshToken`].
///
/// The caller-supplied `refresh_fn` receives the refresh token string and
/// should perform the actual HTTP token exchange.
pub async fn resolve_token_with_refresh<F, Fut>(
    access_token: &str,
    refresh_token: Option<&str>,
    expires_at: Option<u64>,
    refresh_fn: F,
) -> Result<ResolvedToken, TokenRefreshError>
where
    F: FnOnce(&str) -> Fut,
    Fut: std::future::Future<Output = Result<RefreshedToken, TokenRefreshError>>,
{
    if !is_token_expired(expires_at) {
        return Ok(ResolvedToken {
            access_token: access_token.to_string(),
            was_refreshed: false,
        });
    }

    let refresh_token = refresh_token.ok_or(TokenRefreshError::NoRefreshToken)?;
    let refreshed = refresh_fn(refresh_token).await?;
    Ok(ResolvedToken {
        access_token: refreshed.access_token,
        was_refreshed: true,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn not_expired_when_no_expiry_set() {
        assert!(!is_token_expired(None));
    }

    #[test]
    fn not_expired_when_far_future() {
        assert!(!is_token_expired(Some(now_unix_secs() + 3600)));
    }

    #[test]
    fn expired_when_in_past() {
        assert!(is_token_expired(Some(1)));
    }

    #[test]
    fn expired_when_exactly_now() {
        // "now" or earlier counts as expired (<=)
        assert!(is_token_expired(Some(now_unix_secs())));
    }

    #[tokio::test]
    async fn returns_original_token_when_not_expired() {
        let result = resolve_token_with_refresh(
            "valid-access",
            Some("refresh"),
            Some(now_unix_secs() + 300),
            |_| async { unreachable!("refresh_fn should not be called") },
        )
        .await
        .expect("should succeed");

        assert_eq!(result.access_token, "valid-access");
        assert!(!result.was_refreshed);
    }

    #[tokio::test]
    async fn errors_when_expired_without_refresh_token() {
        let result = resolve_token_with_refresh(
            "expired-access",
            None,
            Some(1),
            |_| async { unreachable!("refresh_fn should not be called") },
        )
        .await;

        assert_eq!(result.unwrap_err(), TokenRefreshError::NoRefreshToken);
    }

    #[tokio::test]
    async fn calls_refresh_fn_when_expired_with_refresh_token() {
        let result = resolve_token_with_refresh(
            "expired-access",
            Some("my-refresh"),
            Some(1),
            |rt| {
                let rt = rt.to_string();
                async move {
                    assert_eq!(rt, "my-refresh");
                    Ok(RefreshedToken {
                        access_token: "new-access".to_string(),
                        refresh_token: Some("new-refresh".to_string()),
                        expires_at: Some(now_unix_secs() + 600),
                    })
                }
            },
        )
        .await
        .expect("should succeed");

        assert_eq!(result.access_token, "new-access");
        assert!(result.was_refreshed);
    }

    #[tokio::test]
    async fn propagates_refresh_fn_error() {
        let result = resolve_token_with_refresh(
            "expired-access",
            Some("my-refresh"),
            Some(1),
            |_| async { Err(TokenRefreshError::Network("connection refused".to_string())) },
        )
        .await;

        assert_eq!(
            result.unwrap_err(),
            TokenRefreshError::Network("connection refused".to_string())
        );
    }
}
