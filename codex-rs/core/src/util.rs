use std::time::Duration;

use rand::Rng;

const INITIAL_DELAY_MS: u64 = 200;
const BACKOFF_FACTOR: f64 = 2.0;

pub(crate) fn backoff(attempt: u64) -> Duration {
    let exp = BACKOFF_FACTOR.powi(attempt.saturating_sub(1) as i32);
    let base = (INITIAL_DELAY_MS as f64 * exp) as u64;
    let jitter = rand::rng().random_range(0.9..1.1);
    Duration::from_millis((base as f64 * jitter) as u64)
}

/// Strip bash -lc wrapper and escape shell metacharacters for safe execution
pub(crate) fn strip_bash_lc_and_escape(command: &str) -> String {
    // If the command starts with bash -lc, extract the inner command
    if let Some(stripped) = command.strip_prefix("bash -lc ") {
        // Remove surrounding quotes if present
        let inner = if (stripped.starts_with('"') && stripped.ends_with('"'))
            || (stripped.starts_with('\'') && stripped.ends_with('\''))
        {
            &stripped[1..stripped.len() - 1]
        } else {
            stripped
        };
        
        // Basic shell escaping - escape special characters
        inner
            .replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('`', "\\`")
            .replace('$', "\\$")
    } else {
        command.to_string()
    }
}
