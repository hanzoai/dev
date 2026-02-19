use crate::error::{Result, ZtError};
use serde::{Deserialize, Serialize};
use tracing::{debug, warn};

/// Billing guard — enforces paid-only access to ZT services.
/// No free tier: every dial/bind requires a positive balance.
pub struct BillingGuard {
    http: reqwest::Client,
    commerce_url: String,
    auth_token: String,
}

impl BillingGuard {
    pub fn new(commerce_url: &str, auth_token: &str) -> Self {
        Self {
            http: reqwest::Client::new(),
            commerce_url: commerce_url.trim_end_matches('/').to_string(),
            auth_token: auth_token.to_string(),
        }
    }

    /// Check that the user has sufficient balance for the service.
    /// Returns Ok(()) if balance is positive, or InsufficientBalance error.
    pub async fn check_balance(&self, service: &str) -> Result<()> {
        let url = format!("{}/v1/billing/balance", self.commerce_url);

        let resp = self
            .http
            .get(&url)
            .query(&[("service", service)])
            .bearer_auth(&self.auth_token)
            .send()
            .await
            .map_err(|e| ZtError::BillingError(format!("balance check failed: {e}")))?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp.text().await.unwrap_or_default();
            return Err(ZtError::BillingError(format!(
                "balance check returned {status}: {text}"
            )));
        }

        let balance: BalanceResponse = resp
            .json()
            .await
            .map_err(|e| ZtError::BillingError(format!("failed to parse balance: {e}")))?;

        if balance.balance <= 0.0 {
            return Err(ZtError::InsufficientBalance(format!(
                "service '{service}' requires a positive balance (current: {:.2})",
                balance.balance
            )));
        }

        debug!(service, balance = balance.balance, "billing check passed");
        Ok(())
    }

    /// Record usage after a session ends
    pub async fn record_usage(&self, record: &UsageRecord) -> Result<()> {
        let url = format!("{}/v1/billing/usage", self.commerce_url);

        let resp = self
            .http
            .post(&url)
            .bearer_auth(&self.auth_token)
            .json(record)
            .send()
            .await
            .map_err(|e| ZtError::BillingError(format!("usage recording failed: {e}")))?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp.text().await.unwrap_or_default();
            warn!(%status, "failed to record usage: {text}");
        }

        Ok(())
    }
}

#[derive(Debug, Deserialize)]
struct BalanceResponse {
    balance: f64,
    #[serde(default)]
    #[allow(dead_code)]
    currency: Option<String>,
}

/// Usage record sent to the commerce API
#[derive(Debug, Serialize)]
pub struct UsageRecord {
    pub service: String,
    pub session_id: String,
    pub bytes_sent: u64,
    pub bytes_received: u64,
    pub duration_ms: u64,
}
