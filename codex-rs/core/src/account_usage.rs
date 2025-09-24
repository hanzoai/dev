use std::fs::{self, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

use chrono::{DateTime, Duration, Utc};
use codex_protocol::protocol::RateLimitSnapshotEvent;
use fs2::FileExt;
use serde::{Deserialize, Serialize};

use crate::protocol::TokenUsage;

const USAGE_VERSION: u32 = 1;
const USAGE_SUBDIR: &str = "usage";

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct TokenTotals {
    #[serde(default)]
    input_tokens: u64,
    #[serde(default)]
    cached_input_tokens: u64,
    #[serde(default)]
    output_tokens: u64,
    #[serde(default)]
    reasoning_output_tokens: u64,
    #[serde(default)]
    total_tokens: u64,
}

impl TokenTotals {
    fn add_usage(&mut self, usage: &TokenUsage) {
        self.input_tokens = self.input_tokens.saturating_add(usage.input_tokens);
        self.cached_input_tokens = self
            .cached_input_tokens
            .saturating_add(usage.cached_input_tokens);
        self.output_tokens = self.output_tokens.saturating_add(usage.output_tokens);
        self.reasoning_output_tokens = self
            .reasoning_output_tokens
            .saturating_add(usage.reasoning_output_tokens);
        self.total_tokens = self.total_tokens.saturating_add(usage.total_tokens);
    }

    fn add_totals(&mut self, other: &TokenTotals) {
        self.input_tokens = self.input_tokens.saturating_add(other.input_tokens);
        self.cached_input_tokens = self
            .cached_input_tokens
            .saturating_add(other.cached_input_tokens);
        self.output_tokens = self.output_tokens.saturating_add(other.output_tokens);
        self.reasoning_output_tokens = self
            .reasoning_output_tokens
            .saturating_add(other.reasoning_output_tokens);
        self.total_tokens = self.total_tokens.saturating_add(other.total_tokens);
    }

    fn from_usage(usage: &TokenUsage) -> Self {
        let mut totals = TokenTotals::default();
        totals.add_usage(usage);
        totals
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TokenWindowEntry {
    timestamp: DateTime<Utc>,
    tokens: TokenTotals,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct RateLimitInfo {
    #[serde(default)]
    snapshot: Option<RateLimitSnapshotEvent>,
    #[serde(default)]
    observed_at: Option<DateTime<Utc>>,
    #[serde(default)]
    next_reset_at: Option<DateTime<Utc>>,
    #[serde(default)]
    last_usage_limit_hit_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AccountUsageData {
    version: u32,
    account_id: String,
    #[serde(default)]
    plan: Option<String>,
    #[serde(default)]
    last_updated: DateTime<Utc>,
    #[serde(default)]
    totals: TokenTotals,
    #[serde(default)]
    hourly_entries: Vec<TokenWindowEntry>,
    #[serde(default)]
    tokens_last_hour: TokenTotals,
    #[serde(default)]
    rate_limit: Option<RateLimitInfo>,
}

impl AccountUsageData {
    fn new(account_id: String) -> Self {
        Self {
            version: USAGE_VERSION,
            account_id,
            plan: None,
            last_updated: Utc::now(),
            totals: TokenTotals::default(),
            hourly_entries: Vec::new(),
            tokens_last_hour: TokenTotals::default(),
            rate_limit: None,
        }
    }

    fn apply_plan(&mut self, plan: Option<&str>) {
        if let Some(plan) = plan {
            if self.plan.as_deref() != Some(plan) {
                self.plan = Some(plan.to_string());
            }
        }
    }

    fn update_last_hour(&mut self, now: DateTime<Utc>) {
        let cutoff = now - Duration::hours(1);
        self.hourly_entries
            .retain(|entry| entry.timestamp >= cutoff);

        let mut totals = TokenTotals::default();
        for entry in &self.hourly_entries {
            totals.add_totals(&entry.tokens);
        }
        self.tokens_last_hour = totals;
    }
}

fn usage_dir(codex_home: &Path) -> PathBuf {
    codex_home.join(USAGE_SUBDIR)
}

fn usage_file_path(codex_home: &Path, account_id: &str) -> PathBuf {
    usage_dir(codex_home).join(format!("{account_id}.json"))
}

fn with_usage_file<F>(
    codex_home: &Path,
    account_id: &str,
    plan: Option<&str>,
    mut update: F,
) -> std::io::Result<()>
where
    F: FnMut(&mut AccountUsageData),
{
    let usage_dir = usage_dir(codex_home);
    fs::create_dir_all(&usage_dir)?;

    let path = usage_file_path(codex_home, account_id);
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(&path)?;

    file.lock_exclusive()?;

    let mut data = if file.metadata()?.len() == 0 {
        AccountUsageData::new(account_id.to_string())
    } else {
        let mut contents = String::new();
        file.seek(SeekFrom::Start(0))?;
        file.read_to_string(&mut contents)?;
        if contents.trim().is_empty() {
            AccountUsageData::new(account_id.to_string())
        } else {
            match serde_json::from_str::<AccountUsageData>(&contents) {
                Ok(mut parsed) => {
                    if parsed.version != USAGE_VERSION {
                        parsed.version = USAGE_VERSION;
                    }
                    parsed
                }
                Err(_) => AccountUsageData::new(account_id.to_string()),
            }
        }
    };

    data.apply_plan(plan);
    update(&mut data);

    let json = serde_json::to_string_pretty(&data)?;
    file.seek(SeekFrom::Start(0))?;
    file.set_len(0)?;
    file.write_all(json.as_bytes())?;
    file.flush()?;
    file.unlock()?;
    Ok(())
}

pub fn record_token_usage(
    codex_home: &Path,
    account_id: &str,
    plan: Option<&str>,
    usage: &TokenUsage,
    observed_at: DateTime<Utc>,
) -> std::io::Result<()> {
    with_usage_file(codex_home, account_id, plan, |data| {
        data.last_updated = observed_at;
        data.totals.add_usage(usage);
        data.hourly_entries.push(TokenWindowEntry {
            timestamp: observed_at,
            tokens: TokenTotals::from_usage(usage),
        });
        data.update_last_hour(observed_at);
    })
}

pub fn record_rate_limit_snapshot(
    codex_home: &Path,
    account_id: &str,
    plan: Option<&str>,
    snapshot: &RateLimitSnapshotEvent,
    observed_at: DateTime<Utc>,
) -> std::io::Result<()> {
    with_usage_file(codex_home, account_id, plan, |data| {
        data.last_updated = observed_at;
        let mut info = data.rate_limit.take().unwrap_or_default();
        info.snapshot = Some(snapshot.clone());
        info.observed_at = Some(observed_at);
        data.rate_limit = Some(info);
    })
}

pub fn record_usage_limit_hint(
    codex_home: &Path,
    account_id: &str,
    plan: Option<&str>,
    resets_in_seconds: Option<u64>,
    observed_at: DateTime<Utc>,
) -> std::io::Result<()> {
    if resets_in_seconds.is_none() {
        return with_usage_file(codex_home, account_id, plan, |data| {
            data.last_updated = observed_at;
            let mut info = data.rate_limit.take().unwrap_or_default();
            info.last_usage_limit_hit_at = Some(observed_at);
            data.rate_limit = Some(info);
        });
    }

    with_usage_file(codex_home, account_id, plan, |data| {
        data.last_updated = observed_at;
        let mut info = data.rate_limit.take().unwrap_or_default();
        info.last_usage_limit_hit_at = Some(observed_at);
        if let Some(seconds) = resets_in_seconds {
            let reset_at = observed_at + Duration::seconds(seconds as i64);
            info.next_reset_at = Some(reset_at);
        }
        data.rate_limit = Some(info);
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use crate::protocol::TokenUsage;
    use tempfile::TempDir;

    fn sample_usage() -> TokenUsage {
        TokenUsage {
            input_tokens: 120,
            cached_input_tokens: 20,
            output_tokens: 80,
            reasoning_output_tokens: 10,
            total_tokens: 210,
        }
    }

    #[test]
    fn creates_usage_file_and_accumulates_tokens() {
        let home = TempDir::new().expect("tempdir");
        let now = Utc::now();

        record_token_usage(
            home.path(),
            "acct-1",
            Some("Team"),
            &sample_usage(),
            now,
        )
        .expect("record usage");

        let path = usage_file_path(home.path(), "acct-1");
        let mut contents = String::new();
        File::open(path)
            .expect("open usage file")
            .read_to_string(&mut contents)
            .expect("read usage file");

        let parsed: AccountUsageData = serde_json::from_str(&contents).expect("parse usage json");
        assert_eq!(parsed.account_id, "acct-1");
        assert_eq!(parsed.plan.as_deref(), Some("Team"));
        assert_eq!(parsed.totals.input_tokens, 120);
        assert_eq!(parsed.totals.output_tokens, 80);
        assert_eq!(parsed.tokens_last_hour.total_tokens, 210);
        assert_eq!(parsed.hourly_entries.len(), 1);
    }
}
