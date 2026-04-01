use std::path::Path;
use std::time::Duration;

use std::sync::Arc;

use hanzo_backoff::BackoffPolicy;
use rand::Rng;
use reqwest;
use shlex::try_join;
use tokio::sync::Notify;
use tracing::debug;

use crate::config::Config;

/// API backoff policy: 200ms initial, 2s cap, 2 retries.
static API_POLICY: BackoffPolicy = BackoffPolicy::default_api();

pub(crate) fn backoff(attempt: u64) -> Duration {
    let base = API_POLICY
        .delay_for_attempt(attempt as u32)
        .unwrap_or(Duration::from_secs(2));
    let jitter = rand::rng().random_range(0.9..1.1);
    Duration::from_millis((base.as_millis() as f64 * jitter) as u64)
}

/// Blocks until the given endpoint responds, pausing between attempts with
/// exponential backoff (capped). Used to pause retries while the user is
/// offline so we resume immediately once connectivity returns.
pub(crate) async fn wait_for_connectivity(probe_url: &str) {
    // Cap individual waits to avoid very long sleeps while still backing off.
    const MAX_DELAY: Duration = Duration::from_secs(30);
    let client = reqwest::Client::builder().use_rustls_tls().build().expect("failed to build reqwest client with rustls");
    let mut attempt: u64 = 1;
    loop {
        // Treat any HTTP response as proof that DNS + TLS + routing are back.
        // Servers like api.openai.com respond 4xx/421 to bare HEADs, so do
        // not gate on status here.
        if client.head(probe_url).send().await.is_ok() {
            return;
        }

        let delay = backoff(attempt).min(MAX_DELAY);
        attempt = attempt.saturating_add(1);
        tokio::time::sleep(delay).await;
    }
}

pub fn escape_command(command: &[String]) -> String {
    try_join(command.iter().map(|s| s.as_str())).unwrap_or_else(|_| command.join(" "))
}

pub fn strip_bash_lc_and_escape(command: &[String]) -> String {
    match command {
        [first, second, third]
            if is_shell_like_executable(first)
                && (second == "-lc" || second == "-c") =>
        {
            third.clone()
        }
        _ => escape_command(command),
    }
}

pub(crate) fn is_shell_like_executable(token: &str) -> bool {
    let trimmed = token.trim_matches('"').trim_matches('\'');
    let name = Path::new(trimmed)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(trimmed)
        .to_ascii_lowercase();
    matches!(
        name.as_str(),
        "bash"
            | "bash.exe"
            | "sh"
            | "sh.exe"
            | "zsh"
            | "zsh.exe"
            | "dash"
            | "dash.exe"
            | "ksh"
            | "ksh.exe"
            | "busybox"
    )
}

#[allow(dead_code)]
pub fn notify_on_sigint() -> Arc<Notify> {
    let notify = Arc::new(Notify::new());

    tokio::spawn({
        let notify = Arc::clone(&notify);
        async move {
            loop {
                tokio::signal::ctrl_c().await.ok();
                debug!("Keyboard interrupt");
                notify.notify_waiters();
            }
        }
    });

    notify
}

#[allow(dead_code)]
pub fn is_inside_git_repo(config: &Config) -> bool {
    let mut dir = config.cwd.to_path_buf();

    loop {
        if dir.join(".git").exists() {
            return true;
        }

        if !dir.pop() {
            break;
        }
    }

    false
}
