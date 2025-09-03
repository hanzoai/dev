//! Persist Codex session rollouts (.jsonl) so sessions can be replayed or inspected later.

use std::fs::File;
use std::fs::{self};
use std::io::Error as IoError;
use std::path::Path;

use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use time::OffsetDateTime;
use time::format_description::FormatItem;
use time::macros::format_description;
use tokio::io::AsyncWriteExt;
use tokio::sync::mpsc::Sender;
use tokio::sync::mpsc::{self};
use tokio::sync::oneshot;
use tracing::info;
use tracing::warn;
use uuid::Uuid;

use super::SESSIONS_SUBDIR;
use super::list::ConversationsPage;
use super::list::Cursor;
use super::list::get_conversations;
use super::policy::is_persisted_response_item;
use crate::config::Config;
use crate::conversation_manager::InitialHistory;
use codex_protocol::models::ResponseItem;

#[derive(Serialize, Deserialize, Clone, Default)]
pub struct SessionMeta {
    pub id: Uuid,
    pub timestamp: String,
    pub instructions: Option<String>,
}

#[derive(Serialize, Deserialize)]
struct SessionMetaWithGit {
    #[serde(flatten)]
    meta: SessionMeta,
    #[serde(skip_serializing_if = "Option::is_none")] 
    git: Option<serde_json::Value>,
}

#[derive(Serialize, Deserialize, Default, Clone)]
pub struct SessionStateSnapshot {}

#[derive(Serialize, Deserialize, Default, Clone)]
pub struct SavedSession {
    pub session: SessionMeta,
    #[serde(default)]
    pub items: Vec<ResponseItem>,
    #[serde(default)]
    pub state: SessionStateSnapshot,
    pub session_id: Uuid,
}

#[derive(Clone)]
pub struct RolloutRecorder {
    tx: Sender<RolloutCmd>,
}

enum RolloutCmd {
    AddItems(Vec<ResponseItem>),
    UpdateState(SessionStateSnapshot),
    Shutdown { ack: oneshot::Sender<()> },
}

impl RolloutRecorder {
    #[allow(dead_code)]
    pub async fn list_conversations(
        codex_home: &Path,
        page_size: usize,
        cursor: Option<&Cursor>,
    ) -> std::io::Result<ConversationsPage> {
        get_conversations(codex_home, page_size, cursor).await
    }

    pub async fn new(
        config: &Config,
        uuid: Uuid,
        instructions: Option<String>,
    ) -> std::io::Result<Self> {
        let LogFileInfo { file, session_id, timestamp } = create_log_file(config, uuid)?;

        let timestamp_format: &[FormatItem] = format_description!(
            "[year]-[month]-[day]T[hour]:[minute]:[second].[subsecond digits:3]Z"
        );
        let timestamp = timestamp
            .format(timestamp_format)
            .map_err(|e| IoError::other(format!("failed to format timestamp: {e}")))?;

        let (tx, rx) = mpsc::channel::<RolloutCmd>(256);
        tokio::task::spawn(rollout_writer(
            tokio::fs::File::from_std(file),
            rx,
            Some(SessionMeta { timestamp, id: session_id, instructions }),
        ));
        Ok(Self { tx })
    }

    pub async fn resume(path: &Path, _cwd: std::path::PathBuf) -> std::io::Result<(Self, SavedSession)> {
        info!("Resuming rollout from {path:?}");
        let text = tokio::fs::read_to_string(path).await?;
        let mut lines = text.lines();
        let first = lines
            .next()
            .ok_or_else(|| IoError::other("empty session file"))?;
        let meta: SessionMeta = serde_json::from_str::<SessionMetaWithGit>(first)
            .map(|m| m.meta)
            .map_err(|e| IoError::other(format!("failed to parse session header: {e}")))?;

        let mut items = Vec::new();
        for line in lines {
            if line.trim().is_empty() {
                continue;
            }
            let v: Value = match serde_json::from_str(line) {
                Ok(v) => v,
                Err(_) => continue,
            };
            if v.get("record_type")
                .and_then(|rt| rt.as_str())
                .map(|s| s == "state")
                .unwrap_or(false)
            {
                continue;
            }
            if let Ok(item) = serde_json::from_value::<ResponseItem>(v.clone()) {
                items.push(item);
            }
        }

        let file = std::fs::OpenOptions::new().append(true).read(true).open(path)?;
        let (tx, rx) = mpsc::channel::<RolloutCmd>(256);
        tokio::task::spawn(rollout_writer(tokio::fs::File::from_std(file), rx, None));

        let saved = SavedSession {
            session: meta.clone(),
            items: items.clone(),
            state: SessionStateSnapshot::default(),
            session_id: meta.id,
        };
        info!("Resumed rollout successfully from {path:?}");
        Ok((Self { tx }, saved))
    }

    pub(crate) async fn record_items(&self, items: &[ResponseItem]) -> std::io::Result<()> {
        let filtered: Vec<ResponseItem> = items
            .iter()
            .filter(|item| matches!(
                item,
                ResponseItem::Message { .. }
                    | ResponseItem::LocalShellCall { .. }
                    | ResponseItem::FunctionCall { .. }
                    | ResponseItem::FunctionCallOutput { .. }
                    | ResponseItem::CustomToolCall { .. }
                    | ResponseItem::CustomToolCallOutput { .. }
                    | ResponseItem::Reasoning { .. }
            ))
            .cloned()
            .collect();
        if filtered.is_empty() {
            return Ok(());
        }
        self.tx
            .send(RolloutCmd::AddItems(filtered))
            .await
            .map_err(|e| IoError::other(format!("failed to queue rollout items: {e}")))
    }

    pub(crate) async fn record_state(&self, state: SessionStateSnapshot) -> std::io::Result<()> {
        self.tx
            .send(RolloutCmd::UpdateState(state))
            .await
            .map_err(|e| IoError::other(format!("failed to queue rollout state: {e}")))
    }

    pub async fn get_rollout_history(path: &Path) -> std::io::Result<InitialHistory> {
        info!("Resuming rollout from {path:?}");
        let text = tokio::fs::read_to_string(path).await?;
        let mut lines = text.lines();
        let _ = lines
            .next()
            .ok_or_else(|| IoError::other("empty session file"))?;
        let mut items = Vec::new();
        for line in lines {
            if line.trim().is_empty() {
                continue;
            }
            let v: Value = match serde_json::from_str(line) {
                Ok(v) => v,
                Err(_) => continue,
            };
            if v.get("record_type")
                .and_then(|rt| rt.as_str())
                .map(|s| s == "state")
                .unwrap_or(false)
            {
                continue;
            }
            if let Ok(item) = serde_json::from_value::<ResponseItem>(v.clone()) {
                items.push(item);
            }
        }
        if items.is_empty() {
            Ok(InitialHistory::New)
        } else {
            Ok(InitialHistory::Resumed(items))
        }
    }

    pub async fn shutdown(&self) -> std::io::Result<()> {
        let (tx_done, rx_done) = oneshot::channel();
        match self.tx.send(RolloutCmd::Shutdown { ack: tx_done }).await {
            Ok(_) => rx_done
                .await
                .map_err(|e| IoError::other(format!("failed waiting for rollout shutdown: {e}"))),
            Err(e) => {
                warn!("failed to send rollout shutdown command: {e}");
                Err(IoError::other(format!(
                    "failed to send rollout shutdown command: {e}"
                )))
            }
        }
    }
}

struct LogFileInfo {
    file: File,
    session_id: Uuid,
    timestamp: OffsetDateTime,
}

fn create_log_file(config: &Config, session_id: Uuid) -> std::io::Result<LogFileInfo> {
    let timestamp = OffsetDateTime::now_local()
        .map_err(|e| IoError::other(format!("failed to get local time: {e}")))?;
    let mut dir = config.codex_home.clone();
    dir.push(SESSIONS_SUBDIR);
    dir.push(timestamp.year().to_string());
    dir.push(format!("{:02}", u8::from(timestamp.month())));
    dir.push(format!("{:02}", timestamp.day()));
    fs::create_dir_all(&dir)?;
    let format: &[FormatItem] = format_description!("[year]-[month]-[day]T[hour]-[minute]-[second]");
    let date_str = timestamp
        .format(format)
        .map_err(|e| IoError::other(format!("failed to format timestamp: {e}")))?;
    let filename = format!("rollout-{date_str}-{session_id}.jsonl");
    let path = dir.join(filename);
    let file = std::fs::OpenOptions::new().append(true).create(true).open(&path)?;
    Ok(LogFileInfo { file, session_id, timestamp })
}

async fn rollout_writer(
    mut file: tokio::fs::File,
    mut rx: mpsc::Receiver<RolloutCmd>,
    mut meta: Option<SessionMeta>,
) -> std::io::Result<()> {
    if let Some(session_meta) = meta.take() {
        let mut json = serde_json::to_string(&SessionMetaWithGit { meta: session_meta, git: None })?;
        json.push('\n');
        let _ = file.write_all(json.as_bytes()).await;
        file.flush().await?;
    }

    while let Some(cmd) = rx.recv().await {
        match cmd {
            RolloutCmd::AddItems(items) => {
                for item in items {
                    let mut json = serde_json::to_string(&item)?;
                    json.push('\n');
                    let _ = file.write_all(json.as_bytes()).await;
                }
                file.flush().await?;
            }
            RolloutCmd::UpdateState(state) => {
                #[derive(Serialize)]
                struct StateLine<'a> {
                    record_type: &'static str,
                    #[serde(flatten)]
                    state: &'a SessionStateSnapshot,
                }
                let mut json = serde_json::to_string(&StateLine { record_type: "state", state: &state })?;
                json.push('\n');
                let _ = file.write_all(json.as_bytes()).await;
                file.flush().await?;
            }
            RolloutCmd::Shutdown { ack } => { let _ = ack.send(()); }
        }
    }

    Ok(())
}
