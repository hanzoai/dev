use codex_core::plan_tool::StepStatus;
use codex_core::parse_command::ParsedCommand;
use codex_core::protocol::{FileChange, RateLimitSnapshotEvent, TokenUsage};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::{Duration, SystemTime};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum HistoryRecord {
    PlainMessage(PlainMessageState),
    WaitStatus(WaitStatusState),
    Loading(LoadingState),
    RunningTool(RunningToolState),
    ToolCall(ToolCallState),
    PlanUpdate(PlanUpdateState),
    UpgradeNotice(UpgradeNoticeState),
    Reasoning(ReasoningState),
    Exec(ExecRecord),
    AssistantStream(AssistantStreamState),
    AssistantMessage(AssistantMessageState),
    Diff(DiffRecord),
    Image(ImageRecord),
    Explore(ExploreRecord),
    RateLimits(RateLimitsRecord),
    Patch(PatchRecord),
    BackgroundEvent(BackgroundEventRecord),
    Notice(NoticeRecord),
}

#[derive(Clone, Debug, PartialEq)]
pub enum HistoryDomainEvent {
    Insert {
        index: usize,
        record: HistoryDomainRecord,
    },
    Replace {
        index: usize,
        record: HistoryDomainRecord,
    },
    UpdateExecStream {
        index: usize,
        stdout_chunk: Option<ExecStreamChunk>,
        stderr_chunk: Option<ExecStreamChunk>,
    },
    UpsertAssistantStream {
        stream_id: String,
        preview_markdown: String,
        delta: Option<AssistantStreamDelta>,
        metadata: Option<MessageMetadata>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum HistoryDomainRecord {
    Plain(PlainMessageState),
    WaitStatus(WaitStatusState),
    Loading(LoadingState),
    BackgroundEvent(BackgroundEventRecord),
    RateLimits(RateLimitsRecord),
    Exec(ExecRecord),
    AssistantStream(AssistantStreamState),
}

impl From<PlainMessageState> for HistoryDomainRecord {
    fn from(state: PlainMessageState) -> Self {
        HistoryDomainRecord::Plain(state)
    }
}

impl From<WaitStatusState> for HistoryDomainRecord {
    fn from(state: WaitStatusState) -> Self {
        HistoryDomainRecord::WaitStatus(state)
    }
}

impl From<LoadingState> for HistoryDomainRecord {
    fn from(state: LoadingState) -> Self {
        HistoryDomainRecord::Loading(state)
    }
}

impl From<BackgroundEventRecord> for HistoryDomainRecord {
    fn from(state: BackgroundEventRecord) -> Self {
        HistoryDomainRecord::BackgroundEvent(state)
    }
}

impl From<RateLimitsRecord> for HistoryDomainRecord {
    fn from(state: RateLimitsRecord) -> Self {
        HistoryDomainRecord::RateLimits(state)
    }
}

impl From<ExecRecord> for HistoryDomainRecord {
    fn from(state: ExecRecord) -> Self {
        HistoryDomainRecord::Exec(state)
    }
}

impl From<AssistantStreamState> for HistoryDomainRecord {
    fn from(state: AssistantStreamState) -> Self {
        HistoryDomainRecord::AssistantStream(state)
    }
}

impl HistoryDomainRecord {
    fn into_history_record(self) -> HistoryRecord {
        match self {
            HistoryDomainRecord::Plain(mut state) => {
                state.id = HistoryId::ZERO;
                HistoryRecord::PlainMessage(state)
            }
            HistoryDomainRecord::WaitStatus(mut state) => {
                state.id = HistoryId::ZERO;
                HistoryRecord::WaitStatus(state)
            }
            HistoryDomainRecord::Loading(mut state) => {
                state.id = HistoryId::ZERO;
                HistoryRecord::Loading(state)
            }
            HistoryDomainRecord::BackgroundEvent(mut state) => {
                state.id = HistoryId::ZERO;
                HistoryRecord::BackgroundEvent(state)
            }
            HistoryDomainRecord::RateLimits(mut state) => {
                state.id = HistoryId::ZERO;
                HistoryRecord::RateLimits(state)
            }
            HistoryDomainRecord::Exec(mut state) => {
                state.id = HistoryId::ZERO;
                HistoryRecord::Exec(state)
            }
            HistoryDomainRecord::AssistantStream(mut state) => {
                state.id = HistoryId::ZERO;
                HistoryRecord::AssistantStream(state)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlainMessageState {
    pub id: HistoryId,
    pub role: PlainMessageRole,
    pub header: Option<MessageHeader>,
    pub lines: Vec<MessageLine>,
    pub metadata: Option<MessageMetadata>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum PlainMessageRole {
    System,
    User,
    Assistant,
    Tool,
    Error,
    BackgroundEvent,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MessageHeader {
    pub label: String,
    pub badge: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MessageLine {
    pub kind: MessageLineKind,
    pub spans: Vec<InlineSpan>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum MessageLineKind {
    Paragraph,
    Bullet { indent: u8, marker: BulletMarker },
    Code { language: Option<String> },
    Quote,
    Separator,
    Metadata,
    Blank,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum BulletMarker {
    Dash,
    Numbered(u32),
    Custom(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InlineSpan {
    pub text: String,
    pub tone: TextTone,
    pub emphasis: TextEmphasis,
    pub entity: Option<TextEntity>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum TextTone {
    Default,
    Dim,
    Primary,
    Success,
    Warning,
    Error,
    Info,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextEmphasis {
    pub bold: bool,
    pub italic: bool,
    pub dim: bool,
    pub strike: bool,
    pub underline: bool,
}

impl Default for TextEmphasis {
    fn default() -> Self {
        Self {
            bold: false,
            italic: false,
            dim: false,
            strike: false,
            underline: false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum TextEntity {
    Link { href: String },
    Code,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MessageMetadata {
    pub citations: Vec<String>,
    pub token_usage: Option<TokenUsage>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct WaitStatusState {
    pub id: HistoryId,
    pub header: WaitStatusHeader,
    pub details: Vec<WaitStatusDetail>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct WaitStatusHeader {
    pub title: String,
    pub title_tone: TextTone,
    pub summary: Option<String>,
    pub summary_tone: TextTone,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct WaitStatusDetail {
    pub label: String,
    pub value: Option<String>,
    pub tone: TextTone,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoadingState {
    pub id: HistoryId,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ToolCallState {
    pub id: HistoryId,
    pub status: ToolStatus,
    pub title: String,
    pub duration: Option<Duration>,
    pub arguments: Vec<ToolArgument>,
    pub result_preview: Option<ToolResultPreview>,
    pub error_message: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ToolStatus {
    Running,
    Success,
    Failed,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ToolArgument {
    pub name: String,
    pub value: ArgumentValue,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArgumentValue {
    Text(String),
    Json(serde_json::Value),
    Secret,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ToolResultPreview {
    pub lines: Vec<String>,
    pub truncated: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RunningToolState {
    pub id: HistoryId,
    pub title: String,
    pub started_at: SystemTime,
    pub wait_cap_ms: Option<u64>,
    pub wait_has_target: bool,
    pub wait_has_call_id: bool,
    pub arguments: Vec<ToolArgument>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanUpdateState {
    pub id: HistoryId,
    pub name: String,
    pub icon: PlanIcon,
    pub progress: PlanProgress,
    pub steps: Vec<PlanStep>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum PlanIcon {
    LightBulb,
    Rocket,
    Clipboard,
    Custom(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanProgress {
    pub completed: usize,
    pub total: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanStep {
    pub description: String,
    pub status: StepStatus,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct UpgradeNoticeState {
    pub id: HistoryId,
    pub current_version: String,
    pub latest_version: String,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReasoningState {
    pub id: HistoryId,
    pub sections: Vec<ReasoningSection>,
    pub effort: Option<ReasoningEffortLevel>,
    pub in_progress: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReasoningEffortLevel {
    Low,
    Medium,
    High,
    Custom(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReasoningSection {
    /// Optional heading rendered in bold at the top of the section.
    pub heading: Option<String>,
    /// Single-line preview used for collapsed summaries; derived from the first
    /// meaningful block (heading, bullet, paragraph, etc.).
    pub summary: Option<Vec<InlineSpan>>,
    /// Rich collection of blocks that fully describe the reasoning content.
    pub blocks: Vec<ReasoningBlock>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReasoningBlock {
    Paragraph(Vec<InlineSpan>),
    Bullet {
        indent: u8,
        marker: BulletMarker,
        spans: Vec<InlineSpan>,
    },
    Code { language: Option<String>, content: String },
    Quote(Vec<InlineSpan>),
    Separator,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExecRecord {
    pub id: HistoryId,
    pub command: Vec<String>,
    pub parsed: Vec<ParsedCommand>,
    pub action: ExecAction,
    pub status: ExecStatus,
    pub stdout_chunks: Vec<ExecStreamChunk>,
    pub stderr_chunks: Vec<ExecStreamChunk>,
    pub exit_code: Option<i32>,
    pub wait_notes: Vec<ExecWaitNote>,
    pub started_at: SystemTime,
    pub completed_at: Option<SystemTime>,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExecAction {
    Read,
    Search,
    List,
    Run,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExecStatus {
    Running,
    Success,
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExecStreamChunk {
    pub offset: usize,
    pub content: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExecWaitNote {
    pub message: String,
    pub tone: TextTone,
    pub timestamp: SystemTime,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AssistantStreamDelta {
    pub delta: String,
    pub sequence: Option<u64>,
    pub received_at: SystemTime,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AssistantStreamState {
    pub id: HistoryId,
    pub stream_id: String,
    pub preview_markdown: String,
    pub deltas: Vec<AssistantStreamDelta>,
    pub citations: Vec<String>,
    pub metadata: Option<MessageMetadata>,
    pub in_progress: bool,
    pub last_updated_at: SystemTime,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AssistantMessageState {
    pub id: HistoryId,
    pub stream_id: Option<String>,
    pub markdown: String,
    pub citations: Vec<String>,
    pub metadata: Option<MessageMetadata>,
    pub token_usage: Option<TokenUsage>,
    pub created_at: SystemTime,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DiffRecord {
    pub id: HistoryId,
    pub title: String,
    pub hunks: Vec<DiffHunk>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DiffHunk {
    pub header: String,
    pub lines: Vec<DiffLine>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiffLineKind {
    Context,
    Addition,
    Removal,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DiffLine {
    pub kind: DiffLineKind,
    pub content: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImageRecord {
    pub id: HistoryId,
    pub source_path: Option<PathBuf>,
    pub alt_text: Option<String>,
    pub width: u16,
    pub height: u16,
    pub sha256: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExploreRecord {
    pub id: HistoryId,
    pub entries: Vec<ExploreEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExploreEntry {
    pub action: ExecAction,
    pub summary: ExploreSummary,
    pub status: ExploreEntryStatus,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExploreSummary {
    Search {
        query: Option<String>,
        path: Option<String>,
    },
    List {
        path: Option<String>,
    },
    Read {
        display_path: String,
        annotation: Option<String>,
        range: Option<(u32, u32)>,
    },
    Command {
        display: String,
        annotation: Option<String>,
    },
    Fallback {
        text: String,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExploreEntryStatus {
    Running,
    Success,
    NotFound,
    Error { exit_code: Option<i32> },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RateLimitsRecord {
    pub id: HistoryId,
    pub snapshot: RateLimitSnapshotEvent,
    pub legend: Vec<RateLimitLegendEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RateLimitLegendEntry {
    pub label: String,
    pub description: String,
    pub tone: TextTone,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PatchRecord {
    pub id: HistoryId,
    pub patch_type: PatchEventType,
    pub changes: HashMap<PathBuf, FileChange>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum PatchEventType {
    ApprovalRequest,
    ApplyBegin { auto_approved: bool },
    ApplySuccess,
    ApplyFailure,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BackgroundEventRecord {
    pub id: HistoryId,
    pub title: String,
    pub description: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct NoticeRecord {
    pub id: HistoryId,
    pub title: Option<String>,
    pub body: Vec<MessageLine>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct HistoryId(pub u64);

impl HistoryId {
    pub const ZERO: HistoryId = HistoryId(0);
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct HistoryState {
    pub records: Vec<HistoryRecord>,
    pub next_id: u64,
}

#[allow(dead_code)]
impl HistoryState {
    pub fn new() -> Self {
        Self {
            records: Vec::new(),
            next_id: 1,
        }
    }

    pub fn push(&mut self, record: HistoryRecord) -> HistoryId {
        let id = self.next_history_id();
        self.records.push(record.with_id(id));
        id
    }

    pub fn upsert_assistant_stream_state(
        &mut self,
        stream_id: &str,
        preview_markdown: String,
        delta: Option<AssistantStreamDelta>,
        metadata: Option<&MessageMetadata>,
    ) -> HistoryId {
        let event = HistoryDomainEvent::UpsertAssistantStream {
            stream_id: stream_id.to_string(),
            preview_markdown,
            delta,
            metadata: metadata.cloned(),
        };
        match self.apply_domain_event(event) {
            HistoryMutation::Inserted { id, .. }
            | HistoryMutation::Replaced { id, .. } => id,
            _ => HistoryId::ZERO,
        }
    }

    pub fn finalize_assistant_stream_state(
        &mut self,
        stream_id: Option<&str>,
        markdown: String,
        metadata: Option<&MessageMetadata>,
        token_usage: Option<&TokenUsage>,
    ) -> AssistantMessageState {
        let mut carried_citations: Vec<String> = Vec::new();
        let mut carried_metadata: Option<MessageMetadata> = None;
        if let Some(stream_id) = stream_id {
            if let Some(idx) = self.records.iter().position(|record| match record {
                HistoryRecord::AssistantStream(state) => state.stream_id == stream_id,
                _ => false,
            }) {
                if let Some(HistoryRecord::AssistantStream(state)) = self.remove(idx) {
                    if !state.citations.is_empty() {
                        carried_citations = state.citations;
                    }
                    if carried_metadata.is_none() {
                        carried_metadata = state.metadata;
                    }
                }
            }
        }

        let citations = metadata
            .map(|meta| meta.citations.clone())
            .unwrap_or(carried_citations);
        let metadata = metadata.cloned().or(carried_metadata);
        let token_usage = token_usage
            .cloned()
            .or_else(|| metadata.as_ref().and_then(|meta| meta.token_usage.clone()));

        let mut state = AssistantMessageState {
            id: HistoryId::ZERO,
            stream_id: stream_id.map(|s| s.to_string()),
            markdown,
            citations,
            metadata,
            token_usage,
            created_at: SystemTime::now(),
        };
        let id = self.next_history_id();
        state.id = id;
        self.records
            .push(HistoryRecord::AssistantMessage(state.clone()));
        state
    }

    pub fn assistant_stream_state(&self, stream_id: &str) -> Option<&AssistantStreamState> {
        self.records.iter().find_map(|record| match record {
            HistoryRecord::AssistantStream(state) if state.stream_id == stream_id => Some(state),
            _ => None,
        })
    }

    pub fn insert(&mut self, index: usize, record: HistoryRecord) -> HistoryId {
        let id = self.next_history_id();
        self.records.insert(index, record.with_id(id));
        id
    }

    pub fn remove(&mut self, index: usize) -> Option<HistoryRecord> {
        if index < self.records.len() {
            Some(self.records.remove(index))
        } else {
            None
        }
    }

    pub fn get(&self, index: usize) -> Option<&HistoryRecord> {
        self.records.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut HistoryRecord> {
        self.records.get_mut(index)
    }

    pub fn len(&self) -> usize {
        self.records.len()
    }

    pub fn is_empty(&self) -> bool {
        self.records.is_empty()
    }

    fn next_history_id(&mut self) -> HistoryId {
        let id = HistoryId(self.next_id);
        self.next_id = self.next_id.saturating_add(1);
        id
    }

    pub fn apply_event(&mut self, event: HistoryEvent) -> HistoryMutation {
        match event {
            HistoryEvent::Insert { index, record } => {
                let id = self.next_history_id();
                let record = record.with_id(id);
                let idx = index.min(self.records.len());
                self.records.insert(idx, record.clone());
                HistoryMutation::Inserted { index: idx, id, record }
            }
            HistoryEvent::Replace { index, record } => {
                if let Some(existing) = self.records.get(index) {
                    let id = existing.id();
                    let record = record.with_id(id);
                    self.records[index] = record.clone();
                    HistoryMutation::Replaced { index, id, record }
                } else {
                    HistoryMutation::Noop
                }
            }
            HistoryEvent::Remove { index } => {
                if index < self.records.len() {
                    let record = self.records.remove(index);
                    let id = record.id();
                    HistoryMutation::Removed { index, id, record }
                } else {
                    HistoryMutation::Noop
                }
            }
        }
    }

    pub fn apply_domain_event(&mut self, event: HistoryDomainEvent) -> HistoryMutation {
        match event {
            HistoryDomainEvent::Insert { index, record } => {
                let record = record.into_history_record();
                self.apply_event(HistoryEvent::Insert { index, record })
            }
            HistoryDomainEvent::Replace { index, record } => {
                let record = record.into_history_record();
                self.apply_event(HistoryEvent::Replace { index, record })
            }
            HistoryDomainEvent::UpdateExecStream {
                index,
                stdout_chunk,
                stderr_chunk,
            } => {
                if let Some(HistoryRecord::Exec(existing)) = self.records.get(index).cloned() {
                    let mut updated = existing;
                    if let Some(chunk) = stdout_chunk {
                        updated.stdout_chunks.push(chunk);
                    }
                    if let Some(chunk) = stderr_chunk {
                        updated.stderr_chunks.push(chunk);
                    }
                    self.apply_event(HistoryEvent::Replace {
                        index,
                        record: HistoryRecord::Exec(updated),
                    })
                } else {
                    HistoryMutation::Noop
                }
            }
            HistoryDomainEvent::UpsertAssistantStream {
                stream_id,
                preview_markdown,
                delta,
                metadata,
            } => {
                let now = SystemTime::now();
                if let Some(idx) = self.records.iter().position(|record| {
                    matches!(record,
                        HistoryRecord::AssistantStream(state) if state.stream_id == stream_id)
                }) {
                    if let Some(HistoryRecord::AssistantStream(existing)) =
                        self.records.get(idx).cloned()
                    {
                        let mut updated = existing;
                        if let Some(delta) = delta {
                            updated.deltas.push(delta);
                        }
                        updated.preview_markdown = preview_markdown;
                        if let Some(meta) = metadata.clone() {
                            updated.citations = meta.citations.clone();
                            updated.metadata = Some(meta);
                        }
                        updated.in_progress = true;
                        updated.last_updated_at = now;
                        return self.apply_event(HistoryEvent::Replace {
                            index: idx,
                            record: HistoryRecord::AssistantStream(updated),
                        });
                    }
                }

                let mut deltas = Vec::new();
                if let Some(delta) = delta {
                    deltas.push(delta);
                }
                let citations = metadata
                    .as_ref()
                    .map(|meta| meta.citations.clone())
                    .unwrap_or_default();
                let assistant_state = AssistantStreamState {
                    id: HistoryId::ZERO,
                    stream_id,
                    preview_markdown,
                    deltas,
                    citations,
                    metadata,
                    in_progress: true,
                    last_updated_at: now,
                };
                let record = HistoryRecord::AssistantStream(assistant_state);
                self.apply_event(HistoryEvent::Insert {
                    index: self.records.len(),
                    record,
                })
            }
        }
    }
}

#[allow(dead_code)]
trait WithId {
    fn with_id(self, id: HistoryId) -> HistoryRecord;
}

impl WithId for HistoryRecord {
    fn with_id(self, id: HistoryId) -> HistoryRecord {
        match self {
            HistoryRecord::PlainMessage(mut state) => {
                state.id = id;
                HistoryRecord::PlainMessage(state)
            }
            HistoryRecord::WaitStatus(mut state) => {
                state.id = id;
                HistoryRecord::WaitStatus(state)
            }
            HistoryRecord::Loading(mut state) => {
                state.id = id;
                HistoryRecord::Loading(state)
            }
            HistoryRecord::RunningTool(mut state) => {
                state.id = id;
                HistoryRecord::RunningTool(state)
            }
            HistoryRecord::ToolCall(mut state) => {
                state.id = id;
                HistoryRecord::ToolCall(state)
            }
            HistoryRecord::PlanUpdate(mut state) => {
                state.id = id;
                HistoryRecord::PlanUpdate(state)
            }
            HistoryRecord::UpgradeNotice(mut state) => {
                state.id = id;
                HistoryRecord::UpgradeNotice(state)
            }
            HistoryRecord::Reasoning(mut state) => {
                state.id = id;
                HistoryRecord::Reasoning(state)
            }
            HistoryRecord::Exec(mut state) => {
                state.id = id;
                HistoryRecord::Exec(state)
            }
            HistoryRecord::AssistantStream(mut state) => {
                state.id = id;
                HistoryRecord::AssistantStream(state)
            }
            HistoryRecord::AssistantMessage(mut state) => {
                state.id = id;
                HistoryRecord::AssistantMessage(state)
            }
            HistoryRecord::Diff(mut state) => {
                state.id = id;
                HistoryRecord::Diff(state)
            }
            HistoryRecord::Image(mut state) => {
                state.id = id;
                HistoryRecord::Image(state)
            }
            HistoryRecord::Explore(mut state) => {
                state.id = id;
                HistoryRecord::Explore(state)
            }
            HistoryRecord::RateLimits(mut state) => {
                state.id = id;
                HistoryRecord::RateLimits(state)
            }
            HistoryRecord::Patch(mut state) => {
                state.id = id;
                HistoryRecord::Patch(state)
            }
            HistoryRecord::BackgroundEvent(mut state) => {
                state.id = id;
                HistoryRecord::BackgroundEvent(state)
            }
            HistoryRecord::Notice(mut state) => {
                state.id = id;
                HistoryRecord::Notice(state)
            }
        }
    }
}

impl HistoryRecord {
    pub fn id(&self) -> HistoryId {
        match self {
            HistoryRecord::PlainMessage(state) => state.id,
            HistoryRecord::WaitStatus(state) => state.id,
            HistoryRecord::Loading(state) => state.id,
            HistoryRecord::RunningTool(state) => state.id,
            HistoryRecord::ToolCall(state) => state.id,
            HistoryRecord::PlanUpdate(state) => state.id,
            HistoryRecord::UpgradeNotice(state) => state.id,
            HistoryRecord::Reasoning(state) => state.id,
            HistoryRecord::Exec(state) => state.id,
            HistoryRecord::AssistantStream(state) => state.id,
            HistoryRecord::AssistantMessage(state) => state.id,
            HistoryRecord::Diff(state) => state.id,
            HistoryRecord::Image(state) => state.id,
            HistoryRecord::Explore(state) => state.id,
            HistoryRecord::RateLimits(state) => state.id,
            HistoryRecord::Patch(state) => state.id,
            HistoryRecord::BackgroundEvent(state) => state.id,
            HistoryRecord::Notice(state) => state.id,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum HistoryEvent {
    Insert { index: usize, record: HistoryRecord },
    Replace { index: usize, record: HistoryRecord },
    Remove { index: usize },
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum HistoryMutation {
    Inserted { index: usize, id: HistoryId, record: HistoryRecord },
    Replaced { index: usize, id: HistoryId, record: HistoryRecord },
    Removed { index: usize, id: HistoryId, record: HistoryRecord },
    Noop,
}
