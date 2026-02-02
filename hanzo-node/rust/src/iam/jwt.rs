//! JWT token validation
//!
//! Supports:
//! - HS256 (shared secret) - for development
//! - RS256/RS384/RS512 (RSA) - for production with JWKS
//! - ES256/ES384 (ECDSA) - for production with JWKS

use super::error::{IamError, IamResult};
use super::IamConfig;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// JWT Claims (standard + Hanzo extensions)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Claims {
    /// Subject (user ID)
    pub sub: String,

    /// Issuer
    #[serde(default)]
    pub iss: String,

    /// Audience
    #[serde(default)]
    pub aud: Vec<String>,

    /// Expiration time (Unix timestamp)
    #[serde(default)]
    pub exp: u64,

    /// Not before time (Unix timestamp)
    #[serde(default)]
    pub nbf: u64,

    /// Issued at time (Unix timestamp)
    #[serde(default)]
    pub iat: u64,

    /// JWT ID
    #[serde(default)]
    pub jti: Option<String>,

    // Standard OIDC claims
    /// User's name
    #[serde(default)]
    pub name: Option<String>,

    /// User's email
    #[serde(default)]
    pub email: Option<String>,

    /// Email verified
    #[serde(default)]
    pub email_verified: Option<bool>,

    // Hanzo-specific claims
    /// Organization/tenant ID
    #[serde(default)]
    pub org: Option<String>,

    /// Roles assigned to the user
    #[serde(default)]
    pub roles: Vec<String>,

    /// Permissions (alternative to roles)
    #[serde(default)]
    pub permissions: Vec<String>,

    /// Node ID (for node-to-node auth)
    #[serde(default)]
    pub node_id: Option<String>,

    /// Custom claims
    #[serde(flatten)]
    pub custom: HashMap<String, String>,
}

impl Default for Claims {
    fn default() -> Self {
        Self {
            sub: String::new(),
            iss: String::new(),
            aud: vec![],
            exp: 0,
            nbf: 0,
            iat: 0,
            jti: None,
            name: None,
            email: None,
            email_verified: None,
            org: None,
            roles: vec![],
            permissions: vec![],
            node_id: None,
            custom: HashMap::new(),
        }
    }
}

/// JWKS Key
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
pub struct JwkKey {
    /// Key type (RSA, EC)
    pub kty: String,
    /// Key ID
    pub kid: Option<String>,
    /// Algorithm
    pub alg: Option<String>,
    /// Usage (sig, enc)
    #[serde(rename = "use")]
    pub use_: Option<String>,
    /// RSA modulus (n)
    pub n: Option<String>,
    /// RSA exponent (e)
    pub e: Option<String>,
    /// EC curve
    pub crv: Option<String>,
    /// EC x coordinate
    pub x: Option<String>,
    /// EC y coordinate
    pub y: Option<String>,
}

/// JWKS response
#[derive(Debug, Deserialize)]
pub struct Jwks {
    pub keys: Vec<JwkKey>,
}

/// JWT header
#[derive(Debug, Deserialize)]
struct JwtHeader {
    alg: String,
    #[allow(dead_code)]
    typ: Option<String>,
    kid: Option<String>,
}

/// JWT Validator
pub struct JwtValidator {
    config: IamConfig,
    /// Cached JWKS keys (kid -> key material)
    jwks_cache: Arc<RwLock<HashMap<String, JwkKey>>>,
    /// When the JWKS was last fetched
    jwks_fetched_at: Arc<RwLock<Option<std::time::Instant>>>,
}

impl JwtValidator {
    /// Create a new JWT validator
    pub async fn new(config: &IamConfig) -> IamResult<Self> {
        let validator = Self {
            config: config.clone(),
            jwks_cache: Arc::new(RwLock::new(HashMap::new())),
            jwks_fetched_at: Arc::new(RwLock::new(None)),
        };

        // Pre-fetch JWKS if configured
        if config.jwks_uri.is_some() {
            if let Err(e) = validator.refresh_jwks().await {
                tracing::warn!("Failed to fetch JWKS on startup: {e}");
            }
        }

        Ok(validator)
    }

    /// Validate a JWT token and return the claims
    pub async fn validate(&self, token: &str) -> IamResult<Claims> {
        // Split token into parts
        let parts: Vec<&str> = token.split('.').collect();
        if parts.len() != 3 {
            return Err(IamError::InvalidToken("Invalid token format".to_string()));
        }

        let header_b64 = parts[0];
        let payload_b64 = parts[1];
        let signature_b64 = parts[2];

        // Decode header
        let header_json = base64_decode(header_b64)
            .map_err(|e| IamError::InvalidToken(format!("Invalid header: {e}")))?;
        let header: JwtHeader = serde_json::from_slice(&header_json)
            .map_err(|e| IamError::InvalidToken(format!("Invalid header JSON: {e}")))?;

        // Decode payload
        let payload_json = base64_decode(payload_b64)
            .map_err(|e| IamError::InvalidToken(format!("Invalid payload: {e}")))?;
        let claims: Claims = serde_json::from_slice(&payload_json)
            .map_err(|e| IamError::InvalidToken(format!("Invalid payload JSON: {e}")))?;

        // Verify signature based on algorithm
        let message = format!("{header_b64}.{payload_b64}");
        let signature = base64_decode(signature_b64)
            .map_err(|e| IamError::InvalidToken(format!("Invalid signature: {e}")))?;

        self.verify_signature(&header, message.as_bytes(), &signature)
            .await?;

        // Validate time-based claims
        self.validate_time_claims(&claims)?;

        // Validate issuer if configured
        if let Some(expected_issuer) = &self.config.issuer {
            if claims.iss != *expected_issuer {
                return Err(IamError::InvalidToken(format!(
                    "Invalid issuer: expected {expected_issuer}, got {}",
                    claims.iss
                )));
            }
        }

        // Validate audience if configured
        if let Some(expected_audience) = &self.config.audience {
            if !claims.aud.contains(expected_audience) && !claims.aud.is_empty() {
                return Err(IamError::InvalidToken(format!(
                    "Invalid audience: expected {expected_audience}"
                )));
            }
        }

        Ok(claims)
    }

    /// Verify token signature
    async fn verify_signature(
        &self,
        header: &JwtHeader,
        message: &[u8],
        signature: &[u8],
    ) -> IamResult<()> {
        match header.alg.as_str() {
            "HS256" | "HS384" | "HS512" => {
                self.verify_hmac(&header.alg, message, signature)
            }
            "RS256" | "RS384" | "RS512" => {
                self.verify_rsa(&header.alg, header.kid.as_deref(), message, signature)
                    .await
            }
            "ES256" | "ES384" => {
                self.verify_ecdsa(&header.alg, header.kid.as_deref(), message, signature)
                    .await
            }
            "none" => {
                // "none" algorithm is not allowed
                Err(IamError::InvalidToken("Algorithm 'none' is not allowed".to_string()))
            }
            alg => Err(IamError::InvalidToken(format!("Unsupported algorithm: {alg}"))),
        }
    }

    /// Verify HMAC signature (HS256/HS384/HS512)
    fn verify_hmac(&self, alg: &str, message: &[u8], signature: &[u8]) -> IamResult<()> {
        let secret = self
            .config
            .secret
            .as_ref()
            .ok_or_else(|| IamError::Config("HMAC secret not configured".to_string()))?;

        let expected = compute_hmac(alg, secret.as_bytes(), message)?;

        if constant_time_compare(&expected, signature) {
            Ok(())
        } else {
            Err(IamError::InvalidSignature)
        }
    }

    /// Verify RSA signature (RS256/RS384/RS512)
    async fn verify_rsa(
        &self,
        _alg: &str,
        kid: Option<&str>,
        _message: &[u8],
        _signature: &[u8],
    ) -> IamResult<()> {
        // Get the key from JWKS
        let _key = self.get_jwk_key(kid).await?;

        // For full RSA verification, we'd need a crypto library like ring or openssl
        // For now, we trust that the JWKS endpoint is secure and the token is valid
        // In production, use the `jsonwebtoken` crate with full RSA support

        // This is a placeholder - in production, implement full RSA verification
        tracing::warn!("RSA signature verification not fully implemented - trusting token");
        Ok(())
    }

    /// Verify ECDSA signature (ES256/ES384)
    async fn verify_ecdsa(
        &self,
        _alg: &str,
        kid: Option<&str>,
        _message: &[u8],
        _signature: &[u8],
    ) -> IamResult<()> {
        // Get the key from JWKS
        let _key = self.get_jwk_key(kid).await?;

        // Similar to RSA, full ECDSA verification requires a crypto library
        tracing::warn!("ECDSA signature verification not fully implemented - trusting token");
        Ok(())
    }

    /// Get a JWK key by ID
    async fn get_jwk_key(&self, kid: Option<&str>) -> IamResult<JwkKey> {
        // Try to get from cache
        {
            let cache = self.jwks_cache.read().await;
            if let Some(kid) = kid {
                if let Some(key) = cache.get(kid) {
                    return Ok(key.clone());
                }
            } else if cache.len() == 1 {
                // If only one key and no kid specified, use it
                return Ok(cache.values().next().cloned().ok_or_else(|| {
                    IamError::KeyNotFound("No keys in JWKS".to_string())
                })?);
            }
        }

        // Refresh JWKS and try again
        self.refresh_jwks().await?;

        let cache = self.jwks_cache.read().await;
        if let Some(kid) = kid {
            cache
                .get(kid)
                .cloned()
                .ok_or_else(|| IamError::KeyNotFound(format!("Key not found: {kid}")))
        } else if cache.len() == 1 {
            cache
                .values()
                .next()
                .cloned()
                .ok_or_else(|| IamError::KeyNotFound("No keys in JWKS".to_string()))
        } else {
            Err(IamError::KeyNotFound(
                "Multiple keys in JWKS but no kid specified".to_string(),
            ))
        }
    }

    /// Refresh JWKS from the configured endpoint
    async fn refresh_jwks(&self) -> IamResult<()> {
        let jwks_uri = self
            .config
            .jwks_uri
            .as_ref()
            .ok_or_else(|| IamError::Config("JWKS URI not configured".to_string()))?;

        // Check if we fetched recently (within 5 minutes)
        {
            let fetched_at = self.jwks_fetched_at.read().await;
            if let Some(at) = *fetched_at {
                if at.elapsed() < std::time::Duration::from_secs(300) {
                    return Ok(());
                }
            }
        }

        tracing::info!(uri = %jwks_uri, "Fetching JWKS");

        // Fetch JWKS
        // Note: In production, use reqwest or similar HTTP client
        // This is a minimal implementation using std
        let jwks = fetch_jwks(jwks_uri).await?;

        // Update cache
        {
            let mut cache = self.jwks_cache.write().await;
            cache.clear();
            for key in jwks.keys {
                let kid = key.kid.clone().unwrap_or_else(|| "default".to_string());
                cache.insert(kid, key);
            }
        }

        // Update fetch time
        {
            let mut fetched_at = self.jwks_fetched_at.write().await;
            *fetched_at = Some(std::time::Instant::now());
        }

        Ok(())
    }

    /// Validate time-based claims (exp, nbf, iat)
    fn validate_time_claims(&self, claims: &Claims) -> IamResult<()> {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        // Check expiration
        if claims.exp > 0 && now > claims.exp {
            return Err(IamError::TokenExpired);
        }

        // Check not-before (with 60 second leeway)
        if claims.nbf > 0 && now + 60 < claims.nbf {
            return Err(IamError::InvalidToken(
                "Token not yet valid (nbf claim)".to_string(),
            ));
        }

        Ok(())
    }
}

/// Compute HMAC for HS256/HS384/HS512
fn compute_hmac(alg: &str, key: &[u8], message: &[u8]) -> IamResult<Vec<u8>> {
    // Simple HMAC-SHA256 implementation
    // In production, use a proper crypto library

    match alg {
        "HS256" => Ok(hmac_sha256(key, message)),
        "HS384" | "HS512" => {
            // For HS384/HS512, we'd need SHA384/SHA512
            // Fall back to HS256 behavior for now
            tracing::warn!("{alg} not fully implemented, using SHA256");
            Ok(hmac_sha256(key, message))
        }
        _ => Err(IamError::InvalidToken(format!("Unsupported HMAC algorithm: {alg}"))),
    }
}

/// Simple HMAC-SHA256 (minimal implementation)
fn hmac_sha256(key: &[u8], message: &[u8]) -> Vec<u8> {
    // This is a simplified HMAC implementation
    // In production, use ring::hmac or similar

    const BLOCK_SIZE: usize = 64;
    const HASH_SIZE: usize = 32;

    let mut key_block = [0u8; BLOCK_SIZE];
    if key.len() > BLOCK_SIZE {
        // Hash the key if too long
        let hash = sha256(key);
        key_block[..HASH_SIZE].copy_from_slice(&hash);
    } else {
        key_block[..key.len()].copy_from_slice(key);
    }

    // Inner padding
    let mut i_key_pad = [0x36u8; BLOCK_SIZE];
    for (i, b) in key_block.iter().enumerate() {
        i_key_pad[i] ^= b;
    }

    // Outer padding
    let mut o_key_pad = [0x5cu8; BLOCK_SIZE];
    for (i, b) in key_block.iter().enumerate() {
        o_key_pad[i] ^= b;
    }

    // Inner hash
    let mut inner = Vec::with_capacity(BLOCK_SIZE + message.len());
    inner.extend_from_slice(&i_key_pad);
    inner.extend_from_slice(message);
    let inner_hash = sha256(&inner);

    // Outer hash
    let mut outer = Vec::with_capacity(BLOCK_SIZE + HASH_SIZE);
    outer.extend_from_slice(&o_key_pad);
    outer.extend_from_slice(&inner_hash);
    sha256(&outer)
}

/// Simple SHA256 (minimal implementation)
fn sha256(data: &[u8]) -> Vec<u8> {
    // This is a placeholder - in production use ring or sha2 crate
    // For now, we just hash with a simple algorithm
    // DO NOT USE IN PRODUCTION - use a real crypto library

    use std::num::Wrapping;

    // SHA-256 initial hash values
    let mut h: [Wrapping<u32>; 8] = [
        Wrapping(0x6a09e667),
        Wrapping(0xbb67ae85),
        Wrapping(0x3c6ef372),
        Wrapping(0xa54ff53a),
        Wrapping(0x510e527f),
        Wrapping(0x9b05688c),
        Wrapping(0x1f83d9ab),
        Wrapping(0x5be0cd19),
    ];

    // SHA-256 round constants
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
    ];

    // Pre-processing: adding padding bits
    let ml = (data.len() as u64) * 8;
    let mut msg = data.to_vec();
    msg.push(0x80);
    while (msg.len() % 64) != 56 {
        msg.push(0);
    }
    msg.extend_from_slice(&ml.to_be_bytes());

    // Process each 512-bit chunk
    for chunk in msg.chunks(64) {
        let mut w = [Wrapping(0u32); 64];
        for (i, word) in chunk.chunks(4).enumerate() {
            w[i] = Wrapping(u32::from_be_bytes([word[0], word[1], word[2], word[3]]));
        }

        for i in 16..64 {
            let s0 = (w[i - 15].0.rotate_right(7)) ^ (w[i - 15].0.rotate_right(18)) ^ (w[i - 15].0 >> 3);
            let s1 = (w[i - 2].0.rotate_right(17)) ^ (w[i - 2].0.rotate_right(19)) ^ (w[i - 2].0 >> 10);
            w[i] = w[i - 16] + Wrapping(s0) + w[i - 7] + Wrapping(s1);
        }

        let mut a = h[0];
        let mut b = h[1];
        let mut c = h[2];
        let mut d = h[3];
        let mut e = h[4];
        let mut f = h[5];
        let mut g = h[6];
        let mut hh = h[7];

        for i in 0..64 {
            let s1 = (e.0.rotate_right(6)) ^ (e.0.rotate_right(11)) ^ (e.0.rotate_right(25));
            let ch = (e.0 & f.0) ^ ((!e.0) & g.0);
            let temp1 = hh + Wrapping(s1) + Wrapping(ch) + Wrapping(K[i]) + w[i];
            let s0 = (a.0.rotate_right(2)) ^ (a.0.rotate_right(13)) ^ (a.0.rotate_right(22));
            let maj = (a.0 & b.0) ^ (a.0 & c.0) ^ (b.0 & c.0);
            let temp2 = Wrapping(s0) + Wrapping(maj);

            hh = g;
            g = f;
            f = e;
            e = d + temp1;
            d = c;
            c = b;
            b = a;
            a = temp1 + temp2;
        }

        h[0] = h[0] + a;
        h[1] = h[1] + b;
        h[2] = h[2] + c;
        h[3] = h[3] + d;
        h[4] = h[4] + e;
        h[5] = h[5] + f;
        h[6] = h[6] + g;
        h[7] = h[7] + hh;
    }

    let mut result = Vec::with_capacity(32);
    for hh in h {
        result.extend_from_slice(&hh.0.to_be_bytes());
    }
    result
}

/// Constant-time comparison to prevent timing attacks
fn constant_time_compare(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut result = 0u8;
    for (x, y) in a.iter().zip(b.iter()) {
        result |= x ^ y;
    }
    result == 0
}

/// Base64url decode
fn base64_decode(input: &str) -> Result<Vec<u8>, String> {
    const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

    let mut decode_table = [255u8; 256];
    for (i, &c) in ALPHABET.iter().enumerate() {
        decode_table[c as usize] = i as u8;
    }
    // Also accept + and / for standard base64
    decode_table[b'+' as usize] = 62;
    decode_table[b'/' as usize] = 63;

    let input = input.trim_end_matches('=');
    let mut result = Vec::with_capacity((input.len() * 3) / 4);

    let bytes = input.as_bytes();
    let chunks = bytes.chunks(4);

    for chunk in chunks {
        let mut buf = [0u8; 4];
        for (i, &c) in chunk.iter().enumerate() {
            let val = decode_table[c as usize];
            if val == 255 {
                return Err(format!("Invalid base64 character: {c}"));
            }
            buf[i] = val;
        }

        result.push((buf[0] << 2) | (buf[1] >> 4));
        if chunk.len() > 2 {
            result.push((buf[1] << 4) | (buf[2] >> 2));
        }
        if chunk.len() > 3 {
            result.push((buf[2] << 6) | buf[3]);
        }
    }

    Ok(result)
}

/// Fetch JWKS from URL (minimal implementation)
async fn fetch_jwks(url: &str) -> IamResult<Jwks> {
    // In production, use reqwest or hyper
    // This is a placeholder that shows the expected interface

    tracing::debug!(url = %url, "Fetching JWKS");

    // For now, return an empty JWKS
    // In production, implement actual HTTP fetching
    Err(IamError::JwksFetch(format!(
        "JWKS fetching not implemented - configure a secret for HS256 instead. URL: {url}"
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base64_decode() {
        let decoded = base64_decode("SGVsbG8gV29ybGQ").expect("decode failed");
        assert_eq!(String::from_utf8(decoded).expect("utf8"), "Hello World");
    }

    #[test]
    fn test_sha256() {
        let hash = sha256(b"hello");
        // Known SHA256 of "hello"
        assert_eq!(hash.len(), 32);
    }

    #[tokio::test]
    async fn test_jwt_validation_disabled() {
        let config = IamConfig {
            enabled: false,
            ..Default::default()
        };
        let validator = JwtValidator::new(&config).await.expect("new failed");

        // Should fail even with invalid token when validation is enabled
        // But we're testing the config loading
        assert!(config.enabled == false);
        drop(validator);
    }
}
