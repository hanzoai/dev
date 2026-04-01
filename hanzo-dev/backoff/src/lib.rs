//! Overflow-safe exponential backoff for retry logic.

use std::time::Duration;

/// Configures exponential backoff: initial delay doubles each attempt, capped at max.
pub struct BackoffPolicy {
    initial: Duration,
    max: Duration,
    max_retries: u32,
}

impl BackoffPolicy {
    pub const fn new(initial: Duration, max: Duration, max_retries: u32) -> Self {
        Self {
            initial,
            max,
            max_retries,
        }
    }

    /// Default API policy: 200ms initial, 2s max, 2 retries.
    pub const fn default_api() -> Self {
        Self::new(
            Duration::from_millis(200),
            Duration::from_secs(2),
            2,
        )
    }

    /// Compute delay for the given attempt (1-indexed).
    ///
    /// Returns `Err(BackoffError::Overflow)` if the shift overflows.
    pub fn delay_for_attempt(&self, attempt: u32) -> Result<Duration, BackoffError> {
        let multiplier = 1_u32
            .checked_shl(attempt.saturating_sub(1))
            .ok_or(BackoffError::Overflow {
                attempt,
                base: self.initial,
            })?;
        Ok(self
            .initial
            .checked_mul(multiplier)
            .map_or(self.max, |d| d.min(self.max)))
    }

    pub const fn max_retries(&self) -> u32 {
        self.max_retries
    }
}

/// Errors produced by backoff computation.
#[derive(Debug)]
pub enum BackoffError {
    Overflow { attempt: u32, base: Duration },
}

impl std::fmt::Display for BackoffError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Overflow { attempt, base } => {
                write!(
                    f,
                    "backoff overflow at attempt {attempt} with base {base:?}"
                )
            }
        }
    }
}

impl std::error::Error for BackoffError {}

/// Returns true for HTTP status codes that are safe to retry.
pub const fn is_retryable_status(status: u16) -> bool {
    matches!(status, 408 | 409 | 429 | 500 | 502 | 503 | 504)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backoff_doubles_until_maximum() {
        let policy = BackoffPolicy::new(
            Duration::from_millis(10),
            Duration::from_millis(25),
            3,
        );
        assert_eq!(
            policy.delay_for_attempt(1).expect("attempt 1"),
            Duration::from_millis(10),
        );
        assert_eq!(
            policy.delay_for_attempt(2).expect("attempt 2"),
            Duration::from_millis(20),
        );
        // Capped at max.
        assert_eq!(
            policy.delay_for_attempt(3).expect("attempt 3"),
            Duration::from_millis(25),
        );
    }

    #[test]
    fn overflow_returns_error() {
        let policy = BackoffPolicy::new(
            Duration::from_millis(100),
            Duration::from_secs(10),
            50,
        );
        assert!(policy.delay_for_attempt(33).is_err());
    }

    #[test]
    fn retryable_statuses() {
        assert!(is_retryable_status(429));
        assert!(is_retryable_status(500));
        assert!(is_retryable_status(502));
        assert!(is_retryable_status(503));
        assert!(is_retryable_status(504));
        assert!(is_retryable_status(408));
        assert!(is_retryable_status(409));
        assert!(!is_retryable_status(401));
        assert!(!is_retryable_status(200));
        assert!(!is_retryable_status(404));
    }

    #[test]
    fn default_api_policy() {
        let policy = BackoffPolicy::default_api();
        assert_eq!(policy.max_retries(), 2);
        assert_eq!(
            policy.delay_for_attempt(1).expect("attempt 1"),
            Duration::from_millis(200),
        );
    }
}
