//! Persistence layer for the global, append-only task feed file.
//!
//! The feed is stored at `~/.hanzo/tasks.jsonl` with one JSON object per line.
//! Each record captures task lifecycle transitions for local and cloud tracking.

use std::fs::File;
use std::fs::OpenOptions;
use std::io::Result;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

use serde::Serialize;
use tokio::fs;
use tracing::warn;

use crate::git_info::collect_git_info;
use crate::protocol::GitInfo;

#[cfg(unix)]
use std::os::unix::fs::OpenOptionsExt;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

const TASK_FEED_FILENAME: &str = "tasks.jsonl";

const MAX_RETRIES: usize = 10;
const RETRY_SLEEP: Duration = Duration::from_millis(100);
const MAX_MESSAGE_CHARS: usize = 4_000;

#[derive(Clone, Copy, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum TaskFeedEvent {
    TaskStarted,
    TaskCompleted,
    TaskAborted,
}

#[derive(Clone, Copy, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum TaskFeedStatus {
    Running,
    Succeeded,
    Cancelled,
}

#[derive(Clone, Debug)]
pub(crate) struct TaskFeedSeed {
    pub(crate) event: TaskFeedEvent,
    pub(crate) status: TaskFeedStatus,
    pub(crate) session_id: String,
    pub(crate) task_id: String,
    pub(crate) task_kind: String,
    pub(crate) model: String,
    pub(crate) cwd: PathBuf,
    pub(crate) last_agent_message: Option<String>,
    pub(crate) abort_reason: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
struct TaskFeedEntry {
    event: TaskFeedEvent,
    status: TaskFeedStatus,
    ts: u64,
    session_id: String,
    task_id: String,
    task_kind: String,
    model: String,
    cwd: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    git: Option<GitInfo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    last_agent_message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    abort_reason: Option<String>,
}

pub(crate) fn spawn_task_feed_entry(code_home: PathBuf, seed: TaskFeedSeed) {
    tokio::spawn(async move {
        if let Err(err) = record_task_feed_entry(&code_home, seed).await {
            warn!("failed to append task feed entry: {err}");
        }
    });
}

async fn record_task_feed_entry(code_home: &Path, seed: TaskFeedSeed) -> Result<()> {
    let ts = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_err(|e| std::io::Error::other(format!("system clock before Unix epoch: {e}")))?
        .as_secs();
    let git = collect_git_info(&seed.cwd).await;
    let entry = TaskFeedEntry {
        event: seed.event,
        status: seed.status,
        ts,
        session_id: seed.session_id,
        task_id: seed.task_id,
        task_kind: seed.task_kind,
        model: seed.model,
        cwd: seed.cwd.display().to_string(),
        git,
        last_agent_message: seed
            .last_agent_message
            .as_deref()
            .map(|message| truncate_text(message, MAX_MESSAGE_CHARS)),
        abort_reason: seed.abort_reason,
    };

    append_entry(&entry, code_home).await
}

async fn append_entry(entry: &TaskFeedEntry, code_home: &Path) -> Result<()> {
    let path = code_home.join(TASK_FEED_FILENAME);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).await?;
    }

    let mut line = serde_json::to_string(entry)
        .map_err(|e| std::io::Error::other(format!("failed to serialize task feed entry: {e}")))?;
    line.push('\n');

    let mut options = OpenOptions::new();
    options.append(true).read(true).create(true);
    #[cfg(unix)]
    {
        options.mode(0o600);
    }

    let mut task_file = options.open(&path)?;
    ensure_owner_only_permissions(&task_file).await?;

    tokio::task::spawn_blocking(move || -> Result<()> {
        for _ in 0..MAX_RETRIES {
            match fs2::FileExt::try_lock_exclusive(&task_file) {
                Ok(()) => {
                    task_file.write_all(line.as_bytes())?;
                    task_file.flush()?;
                    let _ = fs2::FileExt::unlock(&task_file);
                    return Ok(());
                }
                Err(e) => {
                    if e.kind() == std::io::ErrorKind::WouldBlock {
                        std::thread::sleep(RETRY_SLEEP);
                        continue;
                    }
                    return Err(e);
                }
            }
        }

        Err(std::io::Error::new(
            std::io::ErrorKind::WouldBlock,
            "could not acquire exclusive lock on task feed file after multiple attempts",
        ))
    })
    .await??;

    Ok(())
}

fn truncate_text(text: &str, max_chars: usize) -> String {
    let mut iter = text.chars();
    let mut buf = String::new();

    for _ in 0..max_chars {
        if let Some(ch) = iter.next() {
            buf.push(ch);
        } else {
            return buf;
        }
    }

    buf.push_str("...");
    buf
}

/// On Unix systems ensure the file permissions are `0o600` (rw-------).
#[cfg(unix)]
async fn ensure_owner_only_permissions(file: &File) -> Result<()> {
    let metadata = file.metadata()?;
    let current_mode = metadata.permissions().mode() & 0o777;
    if current_mode != 0o600 {
        let mut perms = metadata.permissions();
        perms.set_mode(0o600);
        let perms_clone = perms.clone();
        let file_clone = file.try_clone()?;
        tokio::task::spawn_blocking(move || file_clone.set_permissions(perms_clone)).await??;
    }
    Ok(())
}

#[cfg(not(unix))]
async fn ensure_owner_only_permissions(_file: &File) -> Result<()> {
    Ok(())
}
