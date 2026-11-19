use rand::RngCore;
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::Path;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

pub const DESKTOP_BOOTSTRAP_TOKEN_WINDOW_MS: u64 = 12 * 60 * 60 * 1000;

pub fn now_unix_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0))
        .as_millis() as u64
}

/// Standard RFC 2104 HMAC-SHA256 over `sha2::Sha256`.
pub fn hmac_sha256(key: &[u8], message: &[u8]) -> [u8; 32] {
    const BLOCK_SIZE: usize = 64;
    let mut key_block = [0u8; BLOCK_SIZE];
    if key.len() > BLOCK_SIZE {
        let digest = Sha256::digest(key);
        key_block[..32].copy_from_slice(&digest);
    } else {
        key_block[..key.len()].copy_from_slice(key);
    }

    let mut ipad = [0x36u8; BLOCK_SIZE];
    let mut opad = [0x5cu8; BLOCK_SIZE];
    for i in 0..BLOCK_SIZE {
        ipad[i] ^= key_block[i];
        opad[i] ^= key_block[i];
    }

    let mut inner = Sha256::new();
    inner.update(ipad);
    inner.update(message);
    let inner_hash = inner.finalize();

    let mut outer = Sha256::new();
    outer.update(opad);
    outer.update(inner_hash);
    let result = outer.finalize();

    let mut out = [0u8; 32];
    out.copy_from_slice(&result);
    out
}

/// Matches `currentDesktopBootstrapToken` in `packages/shared/src/desktopBootstrapToken.ts`.
pub fn current_desktop_bootstrap_token(secret: &str, now_ms: u64) -> String {
    let window = now_ms / DESKTOP_BOOTSTRAP_TOKEN_WINDOW_MS;
    let payload = format!("hanzo-desktop-bootstrap:{window}");
    hex::encode(hmac_sha256(secret.as_bytes(), payload.as_bytes()))
}

pub fn generate_random_hex(byte_len: usize) -> String {
    let mut bytes = vec![0u8; byte_len];
    rand::thread_rng().fill_bytes(&mut bytes);
    hex::encode(bytes)
}

pub fn read_or_create_host_scope(state_dir: &Path) -> String {
    let scope_path = state_dir.join("desktop-host-scope");
    if let Ok(existing) = fs::read_to_string(&scope_path) {
        let trimmed = existing.trim();
        if trimmed.len() == 32 && trimmed.chars().all(|c| c.is_ascii_hexdigit()) {
            return trimmed.to_ascii_lowercase();
        }
    }
    let generated = generate_random_hex(16);
    let _ = fs::create_dir_all(state_dir);
    let _ = fs::write(&scope_path, &generated);
    generated
}

pub fn client_platform() -> &'static str {
    if cfg!(target_os = "macos") {
        "darwin"
    } else if cfg!(target_os = "windows") {
        "win32"
    } else {
        "linux"
    }
}

#[derive(Deserialize)]
struct OAuthTokenResponse {
    access_token: String,
}

pub async fn exchange_bootstrap_for_bearer_token(
    http_base_url: &str,
    bootstrap_secret: &str,
) -> Result<String, String> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(10))
        .build()
        .map_err(|e| format!("Failed to construct HTTP client: {e}"))?;

    let endpoint = format!("{}/oauth/token", http_base_url.trim_end_matches('/'));
    let started = std::time::Instant::now();
    let max_wait = Duration::from_secs(60);

    loop {
        let credential = current_desktop_bootstrap_token(bootstrap_secret, now_unix_ms());
        let response = client
            .post(&endpoint)
            .form(&[
                ("grant_type", "urn:ietf:params:oauth:grant-type:token-exchange"),
                ("subject_token", credential.as_str()),
                (
                    "subject_token_type",
                    "urn:hanzo:params:oauth:token-type:environment-bootstrap",
                ),
                (
                    "requested_token_type",
                    "urn:ietf:params:oauth:token-type:access_token",
                ),
                ("client_label", "Hanzo Dev Desktop"),
                ("client_device_type", "desktop"),
                ("client_os", client_platform()),
            ])
            .send()
            .await;

        match response {
            Ok(resp) if resp.status().is_success() => {
                let parsed: OAuthTokenResponse = resp
                    .json()
                    .await
                    .map_err(|e| format!("Invalid /oauth/token response JSON: {e}"))?;
                return Ok(parsed.access_token);
            }
            Ok(resp) => {
                let status = resp.status().as_u16();
                let is_transient = matches!(status, 502 | 503 | 504);
                if !is_transient || started.elapsed() >= max_wait {
                    let body = resp.text().await.unwrap_or_default();
                    return Err(format!(
                        "Bearer bootstrap failed with HTTP {status}: {body}"
                    ));
                }
            }
            Err(err) => {
                if started.elapsed() >= max_wait {
                    return Err(format!(
                        "Timed out waiting for local Hanzo Dev backend at {endpoint}: {err}"
                    ));
                }
            }
        }

        tokio::time::sleep(Duration::from_millis(250)).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rfc4231_hmac_sha256_vector_2() {
        let mac = hmac_sha256(b"Jefe", b"what do ya want for nothing?");
        assert_eq!(
            hex::encode(mac),
            "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
        );
    }

    #[test]
    fn bootstrap_token_is_deterministic_within_12h_window() {
        let secret = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
        let t1 = current_desktop_bootstrap_token(secret, 1_795_086_000_000);
        let t2 = current_desktop_bootstrap_token(secret, 1_795_086_000_000 + 60_000);
        assert_eq!(t1, t2);
        assert_eq!(t1.len(), 64);

        let next_window = current_desktop_bootstrap_token(
            secret,
            1_795_086_000_000 + DESKTOP_BOOTSTRAP_TOKEN_WINDOW_MS,
        );
        assert_ne!(t1, next_window);
    }

    #[test]
    fn persists_and_reuses_host_scope() {
        let temp = std::env::temp_dir().join(format!("hanzo-tauri-scope-{}", generate_random_hex(6)));
        let s1 = read_or_create_host_scope(&temp);
        let s2 = read_or_create_host_scope(&temp);
        assert_eq!(s1, s2);
        assert_eq!(s1.len(), 32);
        let _ = fs::remove_dir_all(&temp);
    }
}
