//! Recurring prompt/command runner for the `/loop` slash command.
//!
//! This module provides the parsing and types needed by the TUI to schedule
//! recurring commands. It intentionally does NOT spawn tasks or depend on the
//! TUI -- the caller is responsible for the event loop.

use std::time::Duration;

use tokio_util::sync::CancellationToken;

/// Handle to a running loop that the TUI stores in its state.
pub struct LoopHandle {
    pub id: usize,
    pub interval: Duration,
    pub command: String,
    pub cancel: CancellationToken,
}

/// Parsed result of `/loop [args]`.
#[derive(Debug, PartialEq)]
pub enum LoopAction {
    /// No args -- list active loops.
    List,
    /// `/loop stop [id]`
    Stop(Option<usize>),
    /// `/loop [interval] <command>`
    Start { interval: Duration, command: String },
}

const DEFAULT_INTERVAL: Duration = Duration::from_secs(600);

/// Parse a human-friendly duration: `30s`, `5m`, `1h`, or bare seconds.
pub fn parse_interval(s: &str) -> Option<Duration> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    if let Some(n) = s.strip_suffix('s') {
        n.parse::<u64>().ok().map(Duration::from_secs)
    } else if let Some(n) = s.strip_suffix('m') {
        n.parse::<u64>().ok().map(|m| Duration::from_secs(m * 60))
    } else if let Some(n) = s.strip_suffix('h') {
        n.parse::<u64>().ok().map(|h| Duration::from_secs(h * 3600))
    } else {
        s.parse::<u64>().ok().map(Duration::from_secs)
    }
}

/// Parse the argument string after `/loop`.
pub fn parse_loop_args(input: &str) -> LoopAction {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return LoopAction::List;
    }

    if let Some(rest) = trimmed.strip_prefix("stop") {
        let rest = rest.trim();
        if rest.is_empty() {
            return LoopAction::Stop(None);
        }
        return LoopAction::Stop(rest.parse::<usize>().ok());
    }

    let mut parts = trimmed.splitn(2, char::is_whitespace);
    let first = parts.next().unwrap_or("");
    let rest = parts.next().unwrap_or("").trim();

    if let Some(interval) = parse_interval(first) {
        if rest.is_empty() {
            // Interval but no command -- ambiguous, treat as list.
            LoopAction::List
        } else {
            LoopAction::Start {
                interval,
                command: rest.to_string(),
            }
        }
    } else {
        // No recognisable interval prefix -- use default 10m.
        LoopAction::Start {
            interval: DEFAULT_INTERVAL,
            command: trimmed.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_interval_seconds() {
        assert_eq!(parse_interval("30s"), Some(Duration::from_secs(30)));
    }

    #[test]
    fn parse_interval_minutes() {
        assert_eq!(parse_interval("5m"), Some(Duration::from_secs(300)));
    }

    #[test]
    fn parse_interval_hours() {
        assert_eq!(parse_interval("1h"), Some(Duration::from_secs(3600)));
    }

    #[test]
    fn parse_interval_bare_number() {
        assert_eq!(parse_interval("120"), Some(Duration::from_secs(120)));
    }

    #[test]
    fn parse_interval_empty() {
        assert_eq!(parse_interval(""), None);
    }

    #[test]
    fn parse_interval_invalid() {
        assert_eq!(parse_interval("abc"), None);
        assert_eq!(parse_interval("5x"), None);
    }

    #[test]
    fn args_empty_is_list() {
        assert_eq!(parse_loop_args(""), LoopAction::List);
        assert_eq!(parse_loop_args("  "), LoopAction::List);
    }

    #[test]
    fn args_stop_no_id() {
        assert_eq!(parse_loop_args("stop"), LoopAction::Stop(None));
    }

    #[test]
    fn args_stop_with_id() {
        assert_eq!(parse_loop_args("stop 3"), LoopAction::Stop(Some(3)));
    }

    #[test]
    fn args_interval_and_command() {
        assert_eq!(
            parse_loop_args("5m /status"),
            LoopAction::Start {
                interval: Duration::from_secs(300),
                command: "/status".to_string(),
            }
        );
    }

    #[test]
    fn args_interval_and_freeform() {
        assert_eq!(
            parse_loop_args("10m check the deploy"),
            LoopAction::Start {
                interval: Duration::from_secs(600),
                command: "check the deploy".to_string(),
            }
        );
    }

    #[test]
    fn args_no_interval_uses_default() {
        assert_eq!(
            parse_loop_args("/status"),
            LoopAction::Start {
                interval: DEFAULT_INTERVAL,
                command: "/status".to_string(),
            }
        );
    }

    #[test]
    fn args_interval_only_is_list() {
        assert_eq!(parse_loop_args("5m"), LoopAction::List);
    }
}
