use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use once_cell::sync::Lazy;
use std::sync::Arc;
use std::sync::Mutex;

use ratatui::style::{Modifier, Style};

use dev_core::ConversationManager;
use dev_login::{AuthManager, AuthMode};
use dev_core::config::Config;
use dev_core::config_types::ReasoningEffort;
use dev_core::config_types::TextVerbosity;

mod interrupts;
mod streaming;
mod exec_tools;
mod tools;
mod layout_scroll;
mod diff_handlers;
mod perf;
mod diff_ui;
mod message;
use dev_core::parse_command::ParsedCommand;
use dev_core::protocol::AgentMessageDeltaEvent;
use dev_core::protocol::AgentMessageEvent;
use dev_core::protocol::AgentReasoningDeltaEvent;
use dev_core::protocol::AgentReasoningEvent;
use dev_core::protocol::AgentReasoningSectionBreakEvent;
use dev_core::protocol::AgentReasoningRawContentDeltaEvent;
use dev_core::protocol::AgentReasoningRawContentEvent;
use dev_core::protocol::AgentStatusUpdateEvent;
use dev_core::protocol::ApplyPatchApprovalRequestEvent;
use dev_core::protocol::BackgroundEventEvent;
use dev_core::protocol::BrowserScreenshotUpdateEvent;
use dev_core::protocol::CustomToolCallBeginEvent;
use dev_core::protocol::CustomToolCallEndEvent;
use dev_core::protocol::ErrorEvent;
use dev_core::protocol::Event;
use dev_core::protocol::EventMsg;
use dev_core::protocol::SessionConfiguredEvent;
use dev_core::protocol::ExecApprovalRequestEvent;
use dev_core::protocol::ExecCommandBeginEvent;
use dev_core::protocol::ExecCommandEndEvent;
use dev_core::protocol::InputItem;
// MCP tool call handlers moved into chatwidget::tools
use dev_core::protocol::Op;
use dev_core::protocol::PatchApplyBeginEvent;
use dev_core::protocol::PatchApplyEndEvent;
use dev_core::protocol::TaskCompleteEvent;
use dev_core::protocol::TokenUsage;
use dev_core::protocol::TurnDiffEvent;
use crossterm::event::KeyEvent;
use crossterm::event::KeyEventKind;
use image::imageops::FilterType;
use ratatui::buffer::Buffer;
use ratatui::layout::Constraint;
use ratatui::layout::Layout;
use ratatui::layout::Rect;
use ratatui::text::Line;
use ratatui::widgets::Widget;
use ratatui::widgets::WidgetRef;
use ratatui_image::picker::Picker;
use std::cell::RefCell;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::mpsc::unbounded_channel;
use tracing::info;
// use image::GenericImageView;

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::bottom_pane::BottomPane;
use crate::bottom_pane::BottomPaneParams;
use crate::bottom_pane::CancellationEvent;
use crate::bottom_pane::InputResult;
use crate::history_cell;
use crate::history_cell::ExecCell;
use crate::history_cell::HistoryCell;
use dev_protocol::models::{ContentItem, ResponseItem};
use crate::history_cell::PatchEventType;
use crate::live_wrap::RowBuilder;
use crate::user_approval_widget::ApprovalRequest;
use crate::streaming::controller::AppEventHistorySink;
use crate::height_manager::{HeightEvent, HeightManager};
use crate::streaming::StreamKind;
use dev_browser::BrowserManager;
use dev_file_search::FileMatch;
use ratatui::style::Stylize;
use ratatui::text::Text as RtText;
use ratatui::widgets::{Block, Borders, Clear, Paragraph, Scrollbar, ScrollbarState, StatefulWidget};
use ratatui::widgets::ScrollbarOrientation;
use ratatui::symbols::scrollbar as scrollbar_symbols;
use serde::{Deserialize, Serialize};
use dev_core::config::find_dev_home;

#[derive(Debug, Serialize, Deserialize)]
struct CachedConnection {
    port: Option<u16>,
    ws: Option<String>,
}

async fn read_cached_connection() -> Option<(Option<u16>, Option<String>)> {
    let dev_home = find_dev_home().ok()?;
    let path = dev_home.join("cache.json");
    let bytes = tokio::fs::read(path).await.ok()?;
    let parsed: CachedConnection = serde_json::from_slice(&bytes).ok()?;
    Some((parsed.port, parsed.ws))
}

async fn write_cached_connection(port: Option<u16>, ws: Option<String>) -> std::io::Result<()> {
    if port.is_none() && ws.is_none() {
        return Ok(());
    }
    if let Ok(dev_home) = find_dev_home() {
        let path = dev_home.join("cache.json");
        let obj = CachedConnection { port, ws };
        let data = serde_json::to_vec_pretty(&obj).unwrap_or_else(|_| b"{}".to_vec());
        if let Some(dir) = path.parent() { let _ = tokio::fs::create_dir_all(dir).await; }
        tokio::fs::write(path, data).await?;
    }
    Ok(())
}


struct RunningCommand {
    command: Vec<String>,
    parsed: Vec<ParsedCommand>,
    // Index of the in-history Exec cell for this call, if inserted
    history_index: Option<usize>,
}

pub(crate) struct ChatWidget<'a> {
    app_event_tx: AppEventSender,
    dev_op_tx: UnboundedSender<Op>,
    bottom_pane: BottomPane<'a>,
    active_exec_cell: Option<ExecCell>,
    history_cells: Vec<Box<dyn HistoryCell>>, // Store all history in memory
    config: Config,
    initial_user_message: Option<UserMessage>,
    total_token_usage: TokenUsage,
    last_token_usage: TokenUsage,
    content_buffer: String,
    // Buffer for streaming assistant answer text; we do not surface partial
    // We wait for the final AgentMessage event and then emit the full text
    // at once into scrollback so the history contains a single message.
    // Cache of the last finalized assistant message to suppress immediate duplicates
    last_assistant_message: Option<String>,
    // Track the ID of the current streaming message to prevent duplicates
    // Track the ID of the current streaming reasoning to prevent duplicates
    exec: ExecState,
    tools_state: ToolState,
    live_builder: RowBuilder,
    // Store pending image paths keyed by their placeholder text
    pending_images: HashMap<String, PathBuf>,
    // (removed) pending non-image files are no longer tracked; non-image paths remain as plain text
    welcome_shown: bool,
    // Path to the latest browser screenshot and URL for display
    latest_browser_screenshot: Arc<Mutex<Option<(PathBuf, String)>>>,
    // Cached image protocol to avoid recreating every frame (path, area, protocol)
    cached_image_protocol:
        std::cell::RefCell<Option<(PathBuf, Rect, ratatui_image::protocol::Protocol)>>,
    // Cached picker to avoid recreating every frame
    cached_picker: std::cell::RefCell<Option<Picker>>,

    // Cached cell size (width,height) in pixels
    cached_cell_size: std::cell::OnceCell<(u16, u16)>,

    // Terminal information from startup
    terminal_info: crate::tui::TerminalInfo,
    // Agent tracking for multi-agent tasks
    active_agents: Vec<AgentInfo>,
    agents_ready_to_start: bool,
    last_agent_prompt: Option<String>,
    agent_context: Option<String>,
    agent_task: Option<String>,
    overall_task_status: String,
    // Sparkline data for showing agent activity (using RefCell for interior mutability)
    // Each tuple is (value, is_completed) where is_completed indicates if any agent was complete at that time
    sparkline_data: std::cell::RefCell<Vec<(u64, bool)>>,
    last_sparkline_update: std::cell::RefCell<std::time::Instant>,
    // Stream controller for managing streaming content
    stream: crate::streaming::controller::StreamController,
    // Stream lifecycle state (kind, closures, sequencing, cancel)
    stream_state: StreamState,
    // Interrupt manager for handling cancellations
    interrupts: interrupts::InterruptManager,

    // Guard for out-of-order exec events: track call_ids that already ended
    ended_call_ids: HashSet<ExecCallId>,
    /// Exec call_ids that were explicitly cancelled by user interrupt. Used to
    /// drop any late ExecEnd events so we don't render duplicate cells.
    canceled_exec_call_ids: HashSet<ExecCallId>,

    // Accumulated diff/session state
    diffs: DiffsState,

    // Cache for expensive height calculations per cell and width
    height_cache: std::cell::RefCell<std::collections::HashMap<(usize, u16), u16>>,
    // Track last width used to opportunistically clear cache when layout changes
    height_cache_last_width: std::cell::Cell<u16>,
    // Cached visible rows for the diff overlay body to clamp scrolling (kept within diffs)

    // Centralized height manager (always enabled)
    height_manager: RefCell<HeightManager>,

    // Aggregated layout and scroll state
    layout: LayoutState,

    // True when connected to external Chrome via CDP; affects HUD titles
    browser_is_external: bool,

    // Prefix sums of content heights (including spacing) for fast scroll range
    prefix_sums: std::cell::RefCell<Vec<u16>>,
    // Cache key for prefix_sums to avoid rebuilding on pure scroll frames
    last_prefix_width: std::cell::Cell<u16>,
    last_prefix_count: std::cell::Cell<usize>,
    prefix_valid: std::cell::Cell<bool>,

    // Most recent theme snapshot used to retint pre-rendered lines
    last_theme: crate::theme::Theme,

    // Performance tracing (opt-in via /perf)
    perf_state: PerfState,
    // Current session id (from SessionConfigured)
    session_id: Option<uuid::Uuid>,

    // Pending jump-back state (reversible until submit)
    pending_jump_back: Option<PendingJumpBack>,

    // Track active task ids so we don't drop the working status while any
    // agent/sub‑agent is still running (long‑running sessions can interleave).
    active_task_ids: HashSet<String>,

    // --- Queued user message support ---
    // Messages typed while a task is running are kept here and rendered
    // at the bottom as "(queued)" until the next turn begins. At that
    // point we submit one queued message and move its cell into the
    // normal history within the new turn window.
    queued_user_messages: std::collections::VecDeque<UserMessage>,
    // Number of user prompts we pre-pended to history just before starting
    // a new turn; used to anchor the next turn window so assistant output
    // appears after them.
    pending_user_prompts_for_next_turn: usize,

    // Event sequencing to preserve original order across streaming/tool events
    // and stream-related flags moved into stream_state
    
    // Strict global ordering for history: every cell has a required key
    // (req, out, seq). No unordered inserts and no turn windows.
    cell_order_seq: Vec<OrderKey>,
    // Debug: per-cell order info string rendered in the UI to diagnose ordering.
    cell_order_dbg: Vec<Option<String>>,
    // Routing for reasoning stream ids -> existing CollapsibleReasoningCell index
    reasoning_index: HashMap<String, usize>,
    // Stable per-(kind, stream_id) ordering, derived from OrderMeta.
    stream_order_seq: HashMap<(StreamKind, String), OrderKey>,
    // Track last provider request_ordinal seen so internal messages can be
    // assigned request_index = last_seen + 1 (with out = -1).
    last_seen_request_index: u64,
    // Synthetic request index used for internal-only messages; always >= last_seen_request_index
    current_request_index: u64,
    // Monotonic seq for internal messages to keep intra-request order stable
    internal_seq: u64,
    // Show order overlay when true (from --order)
    show_order_overlay: bool,
}

struct PendingJumpBack {
    removed_cells: Vec<Box<dyn HistoryCell>>, // cells removed from the end (from selected user message onward)
}

// ---------- Stable ordering & routing helpers ----------
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct OrderKey {
    req: u64,
    out: i32,
    seq: u64,
}

impl Ord for OrderKey {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.req.cmp(&other.req) {
            std::cmp::Ordering::Equal => match self.out.cmp(&other.out) {
                std::cmp::Ordering::Equal => self.seq.cmp(&other.seq),
                o => o,
            },
            o => o,
        }
    }
}

impl PartialOrd for OrderKey {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// Removed legacy turn-window logic; ordering is strictly global.


// Global guard to prevent overlapping background screenshot captures and to rate-limit them
static BG_SHOT_IN_FLIGHT: Lazy<AtomicBool> = Lazy::new(|| AtomicBool::new(false));
static BG_SHOT_LAST_START_MS: Lazy<AtomicU64> = Lazy::new(|| AtomicU64::new(0));

use self::diff_ui::{DiffBlock, DiffConfirm, DiffOverlay};

use self::message::UserMessage;

use self::perf::PerfStats;

#[derive(Debug, Clone)]
struct AgentInfo {
    name: String,
    status: AgentStatus,
}

#[derive(Debug, Clone, PartialEq)]
enum AgentStatus {
    Pending,
    Running,
    Completed,
    Failed,
}

use self::message::create_initial_user_message;

// Newtype IDs for clarity across exec/tools/streams
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct ExecCallId(pub String);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct ToolCallId(pub String);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct StreamId(pub String);

impl From<String> for ExecCallId { fn from(s: String) -> Self { ExecCallId(s) } }
impl From<&str> for ExecCallId { fn from(s: &str) -> Self { ExecCallId(s.to_string()) } }
impl std::fmt::Display for ExecCallId { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(&self.0) } }
impl AsRef<str> for ExecCallId { fn as_ref(&self) -> &str { &self.0 } }

impl From<String> for ToolCallId { fn from(s: String) -> Self { ToolCallId(s) } }
impl From<&str> for ToolCallId { fn from(s: &str) -> Self { ToolCallId(s.to_string()) } }
impl std::fmt::Display for ToolCallId { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(&self.0) } }
impl AsRef<str> for ToolCallId { fn as_ref(&self) -> &str { &self.0 } }

impl From<String> for StreamId { fn from(s: String) -> Self { StreamId(s) } }
impl From<&str> for StreamId { fn from(s: &str) -> Self { StreamId(s.to_string()) } }
impl std::fmt::Display for StreamId { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(&self.0) } }
impl AsRef<str> for StreamId { fn as_ref(&self) -> &str { &self.0 } }

impl ChatWidget<'_> {
    
    pub(crate) fn enable_perf(&mut self, enable: bool) { self.perf_state.enabled = enable; }
    pub(crate) fn perf_summary(&self) -> String { self.perf_state.stats.borrow().summary() }
    // Build an ordered key from model-provided OrderMeta. Callers must
    // guarantee presence by passing a concrete reference (compile-time guard).
    fn order_key_from_order_meta(om: &dev_core::protocol::OrderMeta) -> OrderKey {
        // sequence_number can be None on some terminal events; treat as 0 for stable placement
        OrderKey { req: om.request_ordinal, out: om.output_index.map(|v| v as i32).unwrap_or(0), seq: om.sequence_number.unwrap_or(0) }
    }

    // Track latest request index observed from provider so internal inserts can anchor to it.
    fn note_order(&mut self, order: Option<&dev_core::protocol::OrderMeta>) {
        if let Some(om) = order { self.last_seen_request_index = self.last_seen_request_index.max(om.request_ordinal); }
    }

    fn debug_fmt_order_key(ok: OrderKey) -> String {
        format!("O:req={} out={} seq={}", ok.req, ok.out, ok.seq)
    }

    // Allocate a new synthetic key for internal (non-LLM) messages at the bottom of the
    // current (active) request: (req = last_seen, out = +∞, seq = monotonic).
    fn next_internal_key(&mut self) -> OrderKey {
        // Anchor to the current provider request if known; otherwise step a synthetic counter.
        let req = if self.last_seen_request_index > 0 {
            self.last_seen_request_index
        } else {
            // Ensure current_request_index always moves forward
            if self.current_request_index < self.last_seen_request_index {
                self.current_request_index = self.last_seen_request_index;
            }
            self.current_request_index = self.current_request_index.saturating_add(1);
            self.current_request_index
        };
        self.internal_seq = self.internal_seq.saturating_add(1);
        // Place internal notices at the end of the current request window by using
        // a maximal out so they sort after any model-provided output_index.
        OrderKey { req, out: i32::MAX, seq: self.internal_seq }
    }

    // Synthetic key for internal content that should appear at the TOP of the NEXT request
    // (e.g., the user’s prompt preceding the model’s output for that turn).
    fn next_req_key_top(&mut self) -> OrderKey {
        let req = self.last_seen_request_index.saturating_add(1);
        self.internal_seq = self.internal_seq.saturating_add(1);
        OrderKey { req, out: i32::MIN, seq: self.internal_seq }
    }

    // Synthetic key for a user prompt that should appear just after banners but
    // still before any model output within the next request.
    fn next_req_key_prompt(&mut self) -> OrderKey {
        let req = self.last_seen_request_index.saturating_add(1);
        self.internal_seq = self.internal_seq.saturating_add(1);
        OrderKey { req, out: i32::MIN + 1, seq: self.internal_seq }
    }

    // Synthetic key for internal notices tied to the upcoming turn that
    // should appear immediately after the user prompt but still before any
    // model output for that turn.
    fn next_req_key_after_prompt(&mut self) -> OrderKey {
        let req = self.last_seen_request_index.saturating_add(1);
        self.internal_seq = self.internal_seq.saturating_add(1);
        OrderKey { req, out: i32::MIN + 2, seq: self.internal_seq }
    }
    /// Hide the bottom spinner/status if the UI is idle (no streams, tools, agents, or tasks).
    fn maybe_hide_spinner(&mut self) {
        let any_tools_running = !self.exec.running_commands.is_empty()
            || !self.tools_state.running_custom_tools.is_empty()
            || !self.tools_state.running_web_search.is_empty();
        let any_streaming = self.stream.is_write_cycle_active();
        let any_agents_active = !self.active_agents.is_empty() || self.agents_ready_to_start;
        let any_tasks_active = !self.active_task_ids.is_empty();
        if !(any_tools_running || any_streaming || any_agents_active || any_tasks_active) {
            self.bottom_pane.set_task_running(false);
        }
    }

    /// Flush any ExecEnd events that arrived before their matching ExecBegin.
    /// We briefly stash such ends to allow natural pairing when the Begin shows up
    /// shortly after. If the pairing window expires, render a fallback completed
    /// Exec cell so users still see the output in history.
    pub(crate) fn flush_pending_exec_ends(&mut self) {
        use std::time::{Duration, Instant};
        let now = Instant::now();
        // Collect keys to avoid holding a mutable borrow while iterating
        let mut ready: Vec<ExecCallId> = Vec::new();
        for (k, (_ev, _order, t0)) in self.exec.pending_exec_ends.iter() {
            if now.saturating_duration_since(*t0) >= Duration::from_millis(110) {
                ready.push(k.clone());
            }
        }
        for key in &ready {
            if let Some((ev, order, _t0)) = self.exec.pending_exec_ends.remove(&key) {
                // Regardless of whether a Begin has arrived by now, handle the End;
                // handle_exec_end_now pairs with a running Exec if present, or falls back.
                self.handle_exec_end_now(ev, &order);
            }
        }
        if !ready.is_empty() {
            self.request_redraw();
        }
    }
    

    fn finalize_all_running_as_interrupted(&mut self) { exec_tools::finalize_all_running_as_interrupted(self); }

    fn finalize_all_running_due_to_answer(&mut self) { exec_tools::finalize_all_running_due_to_answer(self); }
    fn perf_label_for_item(&self, item: &dyn HistoryCell) -> String {
        use crate::history_cell::{ExecKind, ExecStatus, HistoryCellType, PatchKind, ToolStatus};
        match item.kind() {
            HistoryCellType::Plain => "Plain".to_string(),
            HistoryCellType::User => "User".to_string(),
            HistoryCellType::Assistant => "Assistant".to_string(),
            HistoryCellType::Reasoning => "Reasoning".to_string(),
            HistoryCellType::Error => "Error".to_string(),
            HistoryCellType::Exec { kind, status } => {
                let k = match kind {
                    ExecKind::Read => "Read",
                    ExecKind::Search => "Search",
                    ExecKind::List => "List",
                    ExecKind::Run => "Run",
                };
                let s = match status {
                    ExecStatus::Running => "Running",
                    ExecStatus::Success => "Success",
                    ExecStatus::Error => "Error",
                };
                format!("Exec:{}:{}", k, s)
            }
            HistoryCellType::Tool { status } => {
                let s = match status {
                    ToolStatus::Running => "Running",
                    ToolStatus::Success => "Success",
                    ToolStatus::Failed => "Failed",
                };
                format!("Tool:{}", s)
            }
            HistoryCellType::Patch { kind } => {
                let k = match kind {
                    PatchKind::Proposed => "Proposed",
                    PatchKind::ApplyBegin => "ApplyBegin",
                    PatchKind::ApplySuccess => "ApplySuccess",
                    PatchKind::ApplyFailure => "ApplyFailure",
                };
                format!("Patch:{}", k)
            }
            HistoryCellType::PlanUpdate => "PlanUpdate".to_string(),
            HistoryCellType::BackgroundEvent => "BackgroundEvent".to_string(),
            HistoryCellType::Notice => "Notice".to_string(),
            HistoryCellType::Diff => "Diff".to_string(),
            HistoryCellType::Image => "Image".to_string(),
            HistoryCellType::AnimatedWelcome => "AnimatedWelcome".to_string(),
            HistoryCellType::Loading => "Loading".to_string(),
        }
    }

    pub(crate) fn show_resume_picker(&mut self) {
        // Discover candidates
        let cwd = self.config.cwd.clone();
        let dev_home = self.config.dev_home.clone();
        let candidates = crate::resume::discovery::list_sessions_for_cwd(&cwd, &dev_home);
        if candidates.is_empty() {
            let key = self.next_internal_key();
            let _ = self.history_insert_with_key_global(Box::new(crate::history_cell::new_background_event(
                "No past sessions found for this folder".to_string(),
            )), key);
            return;
        }
        // Convert to simple rows with aligned columns and human-friendly times
        fn human_ago(ts: &str) -> String {
            use chrono::{DateTime, Utc};
            if let Ok(dt) = DateTime::parse_from_rfc3339(ts) {
                let now = Utc::now();
                let delta = now.signed_duration_since(dt.with_timezone(&Utc));
                let secs = delta.num_seconds().max(0);
                let mins = secs / 60;
                let hours = mins / 60;
                let days = hours / 24;
                if days >= 7 {
                    // Show date for older entries
                    return dt.format("%Y-%m-%d").to_string();
                }
                if days >= 1 {
                    return format!("{}d ago", days);
                }
                if hours >= 1 {
                    return format!("{}h ago", hours);
                }
                if mins >= 1 {
                    return format!("{}m ago", mins);
                }
                return "just now".to_string();
            }
            ts.to_string()
        }

        let rows: Vec<crate::bottom_pane::resume_selection_view::ResumeRow> = candidates
            .into_iter()
            .map(|c| {
                let modified = human_ago(&c.modified_ts.unwrap_or_default());
                let created = human_ago(&c.created_ts.unwrap_or_default());
                let msgs = format!("{}", c.message_count);
                let branch = c.branch.unwrap_or_else(|| "-".to_string());
                let mut summary = c.snippet.unwrap_or_else(|| c.subtitle.unwrap_or_default());
                const SNIPPET_MAX: usize = 64;
                if summary.chars().count() > SNIPPET_MAX {
                    summary = summary.chars().take(SNIPPET_MAX).collect::<String>() + "…";
                }
                crate::bottom_pane::resume_selection_view::ResumeRow { modified, created, msgs, branch, summary, path: c.path }
            })
            .collect();
        let title = format!("Resume Session — {}", cwd.display());
        let subtitle = Some(String::new());
        self.bottom_pane.show_resume_selection(title, subtitle, rows);
    }

    /// Render a single recorded ResponseItem into history without executing tools
    fn render_replay_item(&mut self, item: ResponseItem) {
        match item {
            ResponseItem::Message { role, content, .. } => {
                let mut text = String::new();
                for c in content {
                    match c {
                        ContentItem::OutputText { text: t }
                        | ContentItem::InputText { text: t } => {
                            if !text.is_empty() { text.push('\n'); }
                            text.push_str(&t);
                        }
                        _ => {}
                    }
                }
                // Show internal system status messages (rendered with markdown) so
                // code blocks and formatting are consistent with assistant output.
                if text.contains("== System Status ==") {
                    use ratatui::text::Line as RLine;
                    let mut lines: Vec<RLine<'static>> = Vec::new();
                    crate::markdown::append_markdown(&text, &mut lines, &self.config);
                    let key = self.next_internal_key();
                    let _ = self.history_insert_with_key_global(Box::new(crate::history_cell::PlainHistoryCell {
                        lines,
                        kind: crate::history_cell::HistoryCellType::Notice,
                    }), key);
                    return;
                }
                if role == "user" {
                    let key = self.next_internal_key();
                    let _ = self.history_insert_with_key_global(Box::new(crate::history_cell::new_user_prompt(text)), key);
                } else {
                    // Build a PlainHistoryCell with Assistant kind; header line hidden by renderer
                    use crate::history_cell::{PlainHistoryCell, HistoryCellType};
                    let mut lines = Vec::new();
                    lines.push(ratatui::text::Line::from("assistant"));
                    for l in text.lines() { lines.push(ratatui::text::Line::from(l.to_string())); }
                    let key = self.next_internal_key();
                    let _ = self.history_insert_with_key_global(Box::new(PlainHistoryCell { lines, kind: HistoryCellType::Assistant }), key);
                }
            }
            ResponseItem::Reasoning { summary, .. } => {
                for s in summary {
                    let dev_protocol::models::ReasoningItemReasoningSummary::SummaryText { text } = s;
                    // Reasoning cell – use the existing reasoning output styling
                    let sink = crate::streaming::controller::AppEventHistorySink(self.app_event_tx.clone());
                    streaming::begin(self, StreamKind::Reasoning, None);
                    let _ = self.stream.apply_final_reasoning(&text, &sink);
                    // finalize immediately for static replay
                    self.stream.finalize(crate::streaming::StreamKind::Reasoning, true, &sink);
                }
            }
            ResponseItem::FunctionCallOutput { output, .. } => {
                // Try to unwrap common JSON wrapper {"output": "...", ...}
                let mut content = output.content;
                if let Ok(v) = serde_json::from_str::<serde_json::Value>(&content) {
                    if let Some(s) = v.get("output").and_then(|x| x.as_str()) {
                        content = s.to_string();
                    }
                }
                let key = self.next_internal_key();
                let _ = self.history_insert_with_key_global(Box::new(crate::history_cell::new_background_event(content)), key);
            }
            _ => {
                // Ignore other item kinds for replay (tool calls, etc.)
            }
        }
    }
    /// Trigger fade on the welcome cell when the composer expands (e.g., slash popup).
    pub(crate) fn on_composer_expanded(&mut self) {
        for cell in &self.history_cells {
            cell.trigger_fade();
        }
        self.request_redraw();
    }
    /// If the user is at or near the bottom, keep following new messages.
    /// We treat "near" as within 3 rows, matching our scroll step.
    fn autoscroll_if_near_bottom(&mut self) { layout_scroll::autoscroll_if_near_bottom(self); }

    fn clear_reasoning_in_progress(&mut self) {
        let mut changed = false;
        for cell in &self.history_cells {
            if let Some(reasoning_cell) = cell
                .as_any()
                .downcast_ref::<history_cell::CollapsibleReasoningCell>()
            {
                reasoning_cell.set_in_progress(false);
                changed = true;
            }
        }
        if changed {
            self.invalidate_height_cache();
        }
    }
    
    /// Handle streaming delta for both answer and reasoning
    // Legacy helper removed: streaming now requires explicit sequence numbers.
    // Call sites should invoke `streaming::delta_text(self, kind, id, delta, seq)` directly.

    /// Defer or handle an interrupt based on whether we're streaming
    fn defer_or_handle<F1, F2>(&mut self, defer_fn: F1, handle_fn: F2)
    where
        F1: FnOnce(&mut interrupts::InterruptManager),
        F2: FnOnce(&mut Self),
    {
        if self.is_write_cycle_active() { defer_fn(&mut self.interrupts); } else { handle_fn(self); }
    }

    fn next_sequence(&mut self) -> u64 {
        let s = self.stream_state.next_seq;
        self.stream_state.next_seq = self.stream_state.next_seq.saturating_add(1);
        s
    }

    // Removed order-adjustment helpers; ordering now uses stable order keys on insert.



    /// Mark that the widget needs to be redrawn
    fn mark_needs_redraw(&mut self) {
        // Clean up fully faded cells before redraw. If any are removed,
        // invalidate the height cache since indices shift and our cache is
        // keyed by (idx,width).
        let before_len = self.history_cells.len();
        self.history_cells.retain(|cell| !cell.should_remove());
        if self.history_cells.len() != before_len {
            self.invalidate_height_cache();
        }

        // Send a redraw event to trigger UI update
        self.app_event_tx.send(AppEvent::RequestRedraw);
    }

    /// Clear memoized cell heights (called when history/content changes)
    fn invalidate_height_cache(&mut self) {
        self.height_cache.borrow_mut().clear();
        self.prefix_sums.borrow_mut().clear();
        self.prefix_valid.set(false);
    }


    /// Handle exec approval request immediately
    fn handle_exec_approval_now(&mut self, _id: String, ev: ExecApprovalRequestEvent) {
        // Use call_id as the approval correlation id so responses map to the
        // exact pending approval in core (supports multiple approvals per turn).
        let approval_id = ev.call_id.clone();
        self.bottom_pane.push_approval_request(ApprovalRequest::Exec {
            id: approval_id,
            command: ev.command,
            reason: ev.reason,
        });
    }

    /// Handle apply patch approval request immediately
    fn handle_apply_patch_approval_now(&mut self, _id: String, ev: ApplyPatchApprovalRequestEvent) {
        let ApplyPatchApprovalRequestEvent {
            call_id,
            changes,
            reason,
            grant_root,
        } = ev;
        
        // Clone for session storage before moving into history
        let changes_clone = changes.clone();
        // Surface the patch summary in the main conversation
        let key = self.next_internal_key();
        let _ = self.history_insert_with_key_global(
            Box::new(history_cell::new_patch_event(
                history_cell::PatchEventType::ApprovalRequest,
                changes,
            )),
            key,
        );
        // Record change set for session diff popup (latest last)
        self.diffs.session_patch_sets.push(changes_clone);
        // For any new paths, capture an original baseline snapshot the first time we see them
        if let Some(last) = self.diffs.session_patch_sets.last() {
            for (src_path, chg) in last.iter() {
                match chg {
                    dev_core::protocol::FileChange::Update { move_path: Some(dest_path), .. } => {
                        if let Some(baseline) = self.diffs.baseline_file_contents.get(src_path).cloned() {
                            // Mirror baseline under destination so tabs use the new path
                            self.diffs.baseline_file_contents.entry(dest_path.clone()).or_insert(baseline);
                        } else if !self.diffs.baseline_file_contents.contains_key(dest_path) {
                            // Snapshot from source (pre-apply)
                            let baseline = std::fs::read_to_string(src_path).unwrap_or_default();
                            self.diffs.baseline_file_contents.insert(dest_path.clone(), baseline);
                        }
                    }
                    _ => {
                        if !self.diffs.baseline_file_contents.contains_key(src_path) {
                            let baseline = std::fs::read_to_string(src_path).unwrap_or_default();
                            self.diffs.baseline_file_contents.insert(src_path.clone(), baseline);
                        }
                    }
                }
            }
        }
        // Enable Ctrl+D footer hint now that we have diffs to show
        self.bottom_pane.set_diffs_hint(true);
        
        // Push the approval request to the bottom pane, keyed by call_id
        let request = ApprovalRequest::ApplyPatch { id: call_id, reason, grant_root };
        self.bottom_pane.push_approval_request(request);
    }

    /// Handle exec command begin immediately
    fn handle_exec_begin_now(&mut self, ev: ExecCommandBeginEvent, order: &dev_core::protocol::OrderMeta) {
        exec_tools::handle_exec_begin_now(self, ev, order);
    }

    /// Handle exec command end immediately
    fn handle_exec_end_now(&mut self, ev: ExecCommandEndEvent, order: &dev_core::protocol::OrderMeta) { exec_tools::handle_exec_end_now(self, ev, order); }

    /// If a completed exec cell sits at `idx`, attempt to merge it into the
    /// previous cell when they represent the same action header (e.g., Searched, Read).
    

    // MCP tool call handlers now live in chatwidget::tools

    /// Handle patch apply end immediately
    fn handle_patch_apply_end_now(&mut self, ev: PatchApplyEndEvent) {
        if ev.success {
            // Update the most recent patch cell header from "Updating..." to "Updated"
            // without creating a new history section.
            if let Some(last) = self.history_cells.iter_mut().rev().find(|c| {
                matches!(
                    c.kind(),
                    crate::history_cell::HistoryCellType::Patch { kind: crate::history_cell::PatchKind::ApplyBegin }
                        | crate::history_cell::HistoryCellType::Patch { kind: crate::history_cell::PatchKind::Proposed }
                )
            }) {
                // Case 1: Patch summary cell – update title/kind in-place
                if let Some(summary) = last.as_any_mut().downcast_mut::<history_cell::PatchSummaryCell>() {
                    summary.title = "Updated".to_string();
                    summary.kind = history_cell::PatchKind::ApplySuccess;
                    self.request_redraw();
                    return;
                }
                // Case 2: Plain history cell fallback – adjust first span and kind
                if let Some(plain) = last.as_any_mut().downcast_mut::<history_cell::PlainHistoryCell>() {
                    if let Some(first_line) = plain.lines.first_mut() {
                        if let Some(first_span) = first_line.spans.get_mut(0) {
                            first_span.content = "Updated".into();
                            first_span.style = Style::default()
                                .fg(crate::colors::success())
                                .add_modifier(Modifier::BOLD);
                        }
                    }
                    plain.kind = history_cell::HistoryCellType::Patch {
                        kind: history_cell::PatchKind::ApplySuccess,
                    };
                    self.request_redraw();
                    return;
                }
            }
            // Fallback: if no prior cell found, do nothing (avoid extra section)
        } else {
            let key = self.next_internal_key();
            let _ = self.history_insert_with_key_global(Box::new(history_cell::new_patch_apply_failure(ev.stderr)), key);
        }
        // After patch application completes, re-evaluate idle state
        self.maybe_hide_spinner();
    }

    /// Get or create the global browser manager
    async fn get_browser_manager() -> Arc<BrowserManager> {
        dev_browser::global::get_or_create_browser_manager().await
    }

    pub(crate) fn insert_str(&mut self, s: &str) {
        self.bottom_pane.insert_str(s);
    }

    // Removed: pending insert sequencing is not used under strict ordering.

    pub(crate) fn register_pasted_image(&mut self, placeholder: String, path: std::path::PathBuf) {
        self.pending_images.insert(placeholder, path);
        self.request_redraw();
    }

    fn parse_message_with_images(&mut self, text: String) -> UserMessage {
        use std::path::Path;

        // Common image extensions
        const IMAGE_EXTENSIONS: &[&str] = &[
            ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".webp", ".svg", ".ico", ".tiff", ".tif",
        ];
        // We keep a visible copy of the original (normalized) text for history
        let mut display_text = text.clone();
        let mut ordered_items: Vec<InputItem> = Vec::new();

        // First, handle [image: ...] placeholders from drag-and-drop
        let placeholder_regex = regex_lite::Regex::new(r"\[image: [^\]]+\]").unwrap();
        let mut cursor = 0usize;
        for mat in placeholder_regex.find_iter(&text) {
            // Push preceding text as a text item (if any)
            if mat.start() > cursor {
                let chunk = &text[cursor..mat.start()];
                if !chunk.trim().is_empty() {
                    ordered_items.push(InputItem::Text { text: chunk.to_string() });
                }
            }

            let placeholder = mat.as_str();
            if placeholder.starts_with("[image:") {
                if let Some(path) = self.pending_images.remove(placeholder) {
                    // Emit a small marker followed by the image so the LLM sees placement
                    let filename = path.file_name().and_then(|n| n.to_str()).unwrap_or("image");
                    let marker = format!("[image: {}]", filename);
                    ordered_items.push(InputItem::Text { text: marker });
                    ordered_items.push(InputItem::LocalImage { path });
                } else {
                    // Unknown placeholder: preserve as text
                    ordered_items.push(InputItem::Text { text: placeholder.to_string() });
                }
            } else {
                // Unknown placeholder type; preserve
                ordered_items.push(InputItem::Text { text: placeholder.to_string() });
            }
            cursor = mat.end();
        }
        // Push any remaining trailing text
        if cursor < text.len() {
            let chunk = &text[cursor..];
            if !chunk.trim().is_empty() {
                ordered_items.push(InputItem::Text { text: chunk.to_string() });
            }
        }

        // Then check for direct file paths typed into the message (no placeholder).
        // We conservatively append these at the end to avoid mis-ordering text.
        // This keeps the behavior consistent while still including the image.
        // We do NOT strip them from display_text so the user sees what they typed.
        let words: Vec<String> = text.split_whitespace().map(String::from).collect();
        for word in &words {
            if word.starts_with("[image:") { continue; }
            let is_image_path = IMAGE_EXTENSIONS.iter().any(|ext| word.to_lowercase().ends_with(ext));
            if !is_image_path { continue; }
            let path = Path::new(word);
            if path.exists() {
                // Add a marker then the image so the LLM has contextual placement info
                let filename = path.file_name().and_then(|n| n.to_str()).unwrap_or("image");
                ordered_items.push(InputItem::Text { text: format!("[image: {}]", filename) });
                ordered_items.push(InputItem::LocalImage { path: path.to_path_buf() });
            }
        }

        // Non-image paths are left as-is in the text; the model may choose to read them.

        // Preserve user formatting (retain newlines) but normalize whitespace:
        // - Normalize CRLF -> LF
        // - Trim trailing spaces per line
        // - Remove any completely blank lines at the start and end
        display_text = display_text.replace("\r\n", "\n");
        let mut _lines_tmp: Vec<String> = display_text
            .lines()
            .map(|l| l.trim_end().to_string())
            .collect();
        while _lines_tmp.first().map_or(false, |s| s.trim().is_empty()) {
            _lines_tmp.remove(0);
        }
        while _lines_tmp.last().map_or(false, |s| s.trim().is_empty()) {
            _lines_tmp.pop();
        }
        display_text = _lines_tmp.join("\n");

        UserMessage {
            display_text,
            ordered_items,
        }
    }

    /// Periodic tick to commit at most one queued line to history,
    /// animating the output.
    pub(crate) fn on_commit_tick(&mut self) { streaming::on_commit_tick(self); }
    fn is_write_cycle_active(&self) -> bool { streaming::is_write_cycle_active(self) }

    fn flush_interrupt_queue(&mut self) {
        let mut mgr = std::mem::take(&mut self.interrupts);
        mgr.flush_all(self);
        self.interrupts = mgr;
    }

    fn on_error(&mut self, message: String) {
        let key = self.next_internal_key();
        let _ = self.history_insert_with_key_global(Box::new(history_cell::new_error_event(message)), key);
        self.bottom_pane.set_task_running(false);
        self.exec.running_commands.clear();
        self.stream.clear_all();
        self.stream_state.drop_streaming = false;
        self.agents_ready_to_start = false;
        self.active_task_ids.clear();
        self.maybe_hide_spinner();
        self.mark_needs_redraw();
    }

    fn interrupt_running_task(&mut self) {
        if self.bottom_pane.is_task_running() {
            self.active_exec_cell = None;
            // Finalize any visible running indicators as interrupted (Exec/Web/Custom)
            self.finalize_all_running_as_interrupted();
            self.bottom_pane.clear_ctrl_c_quit_hint();
            // Stop any active UI streams immediately so output ceases at once.
            self.finalize_active_stream();
            self.stream_state.drop_streaming = true;
            // Surface an explicit notice in history so users see confirmation.
            // We add a lightweight background event (not an error) to match prior UX.
            let key = self.next_internal_key();
            let _ = self.history_insert_with_key_global(Box::new(crate::history_cell::new_background_event(
                "Cancelled by user.".to_string(),
            )), key);
            self.submit_op(Op::Interrupt);
            // Immediately drop the running status so the next message can be typed/run,
            // even if backend cleanup (and Error event) arrives slightly later.
            self.bottom_pane.set_task_running(false);
            self.bottom_pane.clear_live_ring();
            // Reset with max width to disable wrapping
            self.live_builder = RowBuilder::new(usize::MAX);
            // Stream state is now managed by StreamController
            self.content_buffer.clear();
            // Defensive: clear transient flags so UI can quiesce
            self.agents_ready_to_start = false;
            self.active_task_ids.clear();
            // Restore any queued messages back into the composer so the user can
            // immediately press Enter to resume the conversation where they left off.
            if !self.queued_user_messages.is_empty() {
                let mut prefill = String::new();
                for (i, qm) in self.queued_user_messages.iter().enumerate() {
                    if i > 0 { prefill.push('\n'); }
                    prefill.push_str(qm.display_text.trim_end());
                }
                self.clear_composer();
                if !prefill.is_empty() { self.insert_str(&prefill); }
                self.queued_user_messages.clear();
                // Clear any sticky status like "queued for next turn" now that we returned text
                self.bottom_pane.update_status_text(String::new());
            }
            self.maybe_hide_spinner();
            self.request_redraw();
        }
    }
    fn layout_areas(&self, area: Rect) -> Vec<Rect> { layout_scroll::layout_areas(self, area) }
    fn finalize_active_stream(&mut self) { streaming::finalize_active_stream(self); }
    // Strict stream order key helpers
    fn seed_stream_order_key(&mut self, kind: StreamKind, id: &str, key: OrderKey) {
        self.stream_order_seq.insert((kind, id.to_string()), key);
    }
    // Try to fetch a seeded stream order key. Callers must handle None.
    fn try_stream_order_key(&self, kind: StreamKind, id: &str) -> Option<OrderKey> {
        self.stream_order_seq.get(&(kind, id.to_string())).copied()
    }
    pub(crate) fn new(
        config: Config,
        app_event_tx: AppEventSender,
        initial_prompt: Option<String>,
        initial_images: Vec<PathBuf>,
        enhanced_keys_supported: bool,
        terminal_info: crate::tui::TerminalInfo,
        show_order_overlay: bool,
    ) -> Self {
        let (dev_op_tx, mut dev_op_rx) = unbounded_channel::<Op>();

        let app_event_tx_clone = app_event_tx.clone();
        // Create the Codex asynchronously so the UI loads as quickly as possible.
        let config_for_agent_loop = config.clone();
        tokio::spawn(async move {
            // Use ConversationManager with an AuthManager (API key by default)
            let conversation_manager = ConversationManager::new(AuthManager::shared(
                config_for_agent_loop.dev_home.clone(),
                AuthMode::ApiKey,
                config_for_agent_loop.responses_originator_header.clone(),
            ));
            let new_conversation = match conversation_manager
                .new_conversation(config_for_agent_loop)
                .await
            {
                Ok(conv) => conv,
                Err(e) => {
                    tracing::error!("failed to initialize conversation: {e}");
                    // Surface a visible background event so users see why nothing starts.
                    let ev = Event {
                        id: "diagnostic".to_string(),
                        event_seq: 0,
                        msg: EventMsg::BackgroundEvent(BackgroundEventEvent {
                            message: format!(
                                "❌ Failed to initialize model session: {}.\n• Ensure an OpenAI API key is set (CODE_OPENAI_API_KEY / OPENAI_API_KEY) or run `code login`.\n• Also verify config.cwd is an absolute path.",
                                e
                            ),
                        }),
                        order: None,
                    };
                    app_event_tx_clone.send(AppEvent::CodexEvent(ev));
                    return;
                }
            };

            // Forward the SessionConfigured event to the UI
            let event = Event { id: new_conversation.conversation_id.to_string(), event_seq: 0, msg: EventMsg::SessionConfigured(new_conversation.session_configured), order: None };
            app_event_tx_clone.send(AppEvent::CodexEvent(event));

            let conversation = new_conversation.conversation;
            let conversation_clone = conversation.clone();
            let app_event_tx_submit = app_event_tx_clone.clone();
            tokio::spawn(async move {
                while let Some(op) = dev_op_rx.recv().await {
                    if let Err(e) = conversation_clone.submit(op).await {
                        tracing::error!("failed to submit op: {e}");
                        let ev = Event { id: "diagnostic".to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent { message: format!("⚠️ Failed to submit Op to core: {}", e) }), order: None };
                        app_event_tx_submit.send(AppEvent::CodexEvent(ev));
                    }
                }
            });

            while let Ok(event) = conversation.next_event().await {
                app_event_tx_clone.send(AppEvent::CodexEvent(event));
            }
            // (debug end notice removed)
        });

        // Browser manager is now handled through the global state
        // The core session will use the same global manager when browser tools are invoked

        // Add initial animated welcome message to history (top of first request)
        let history_cells: Vec<Box<dyn HistoryCell>> = Vec::new();
        // Insert later via history_push_top_next_req once struct is constructed

        // Removed startup tip for /resume; /resume is now shown under "Popular commands".

        // Initialize image protocol for rendering screenshots

        let new_widget = Self {
            app_event_tx: app_event_tx.clone(),
            dev_op_tx,
            bottom_pane: BottomPane::new(BottomPaneParams {
                app_event_tx,
                has_input_focus: true,
                enhanced_keys_supported,
                using_chatgpt_auth: config.using_chatgpt_auth,
            }),
            active_exec_cell: None,
            history_cells,
            config: config.clone(),
            initial_user_message: create_initial_user_message(
                initial_prompt.unwrap_or_default(),
                initial_images,
            ),
            total_token_usage: TokenUsage::default(),
            last_token_usage: TokenUsage::default(),
            content_buffer: String::new(),
            last_assistant_message: None,
    exec: ExecState { running_commands: HashMap::new(), running_read_agg_index: None, pending_exec_ends: HashMap::new() },
    canceled_exec_call_ids: HashSet::new(),
            tools_state: ToolState { running_custom_tools: HashMap::new(), running_web_search: HashMap::new() },
            // Use max width to disable wrapping during streaming
            // Text will be properly wrapped when displayed based on terminal width
            live_builder: RowBuilder::new(usize::MAX),
            pending_images: HashMap::new(),
            welcome_shown: false,
            latest_browser_screenshot: Arc::new(Mutex::new(None)),
            cached_image_protocol: RefCell::new(None),
            cached_picker: RefCell::new(terminal_info.picker.clone()),
            cached_cell_size: std::cell::OnceCell::new(),
            terminal_info,
            active_agents: Vec::new(),
            agents_ready_to_start: false,
            last_agent_prompt: None,
            agent_context: None,
            agent_task: None,
            overall_task_status: "preparing".to_string(),
            sparkline_data: std::cell::RefCell::new(Vec::new()),
            last_sparkline_update: std::cell::RefCell::new(std::time::Instant::now()),
            stream: crate::streaming::controller::StreamController::new(config.clone()),
            stream_state: StreamState { current_kind: None, closed_answer_ids: HashSet::new(), closed_reasoning_ids: HashSet::new(), next_seq: 1, seq_answer_final: None, drop_streaming: false },
            interrupts: interrupts::InterruptManager::new(),
            ended_call_ids: HashSet::new(),
            diffs: DiffsState { session_patch_sets: Vec::new(), baseline_file_contents: HashMap::new(), overlay: None, confirm: None, body_visible_rows: std::cell::Cell::new(0) },
            height_cache: std::cell::RefCell::new(std::collections::HashMap::new()),
            height_cache_last_width: std::cell::Cell::new(0),
            height_manager: RefCell::new(HeightManager::new(
                crate::height_manager::HeightManagerConfig::default(),
            )),
            layout: LayoutState {
                scroll_offset: 0,
                last_max_scroll: std::cell::Cell::new(0),
                last_history_viewport_height: std::cell::Cell::new(0),
                vertical_scrollbar_state: std::cell::RefCell::new(ScrollbarState::default()),
                scrollbar_visible_until: std::cell::Cell::new(None),
                last_hud_present: std::cell::Cell::new(false),
                browser_hud_expanded: false,
                agents_hud_expanded: false,
                last_frame_height: std::cell::Cell::new(0),
            },
            prefix_sums: std::cell::RefCell::new(Vec::new()),
            last_prefix_width: std::cell::Cell::new(0),
            last_prefix_count: std::cell::Cell::new(0),
            prefix_valid: std::cell::Cell::new(false),
            last_theme: crate::theme::current_theme(),
            perf_state: PerfState { enabled: false, stats: std::cell::RefCell::new(PerfStats::default()) },
            session_id: None,
            pending_jump_back: None,
            active_task_ids: HashSet::new(),
            queued_user_messages: std::collections::VecDeque::new(),
            pending_user_prompts_for_next_turn: 0,
            browser_is_external: false,
            // Stable ordering & routing init
            cell_order_seq: vec![OrderKey { req: 0, out: -1, seq: 0 }],
            cell_order_dbg: vec![None; 1],
            reasoning_index: HashMap::new(),
            stream_order_seq: HashMap::new(),
            last_seen_request_index: 0,
            current_request_index: 0,
            internal_seq: 0,
            show_order_overlay,
        };
        // Insert the welcome cell as top-of-first-request so future model output
        // appears below it.
        let mut w = new_widget;
        w.history_push_top_next_req(history_cell::new_animated_welcome()); // tag: prelude
        w
    }

    /// Construct a ChatWidget from an existing conversation (forked session).
    pub(crate) fn new_from_existing(
        config: Config,
        conversation: std::sync::Arc<dev_core::CodexConversation>,
        session_configured: SessionConfiguredEvent,
        app_event_tx: AppEventSender,
        enhanced_keys_supported: bool,
        terminal_info: crate::tui::TerminalInfo,
        show_order_overlay: bool,
    ) -> Self {
        let (dev_op_tx, mut dev_op_rx) = unbounded_channel::<Op>();

        // Forward events from existing conversation
        let app_event_tx_clone = app_event_tx.clone();
        tokio::spawn(async move {
            // Send the provided SessionConfigured to the UI first
            let event = Event { id: "fork".to_string(), event_seq: 0, msg: EventMsg::SessionConfigured(session_configured), order: None };
            app_event_tx_clone.send(AppEvent::CodexEvent(event));

            let conversation_clone = conversation.clone();
            tokio::spawn(async move {
                while let Some(op) = dev_op_rx.recv().await {
                    let id = conversation_clone.submit(op).await;
                    if let Err(e) = id {
                        tracing::error!("failed to submit op: {e}");
                    }
                }
            });

            while let Ok(event) = conversation.next_event().await {
                app_event_tx_clone.send(AppEvent::CodexEvent(event));
            }
        });

        // Basic widget state mirrors `new`
        let history_cells: Vec<Box<dyn HistoryCell>> = Vec::new();

        let mut w = Self {
            app_event_tx: app_event_tx.clone(),
            dev_op_tx,
            bottom_pane: BottomPane::new(BottomPaneParams {
                app_event_tx,
                has_input_focus: true,
                enhanced_keys_supported,
                using_chatgpt_auth: config.using_chatgpt_auth,
            }),
            active_exec_cell: None,
            history_cells,
            config: config.clone(),
            initial_user_message: None,
            total_token_usage: TokenUsage::default(),
            last_token_usage: TokenUsage::default(),
            content_buffer: String::new(),
            last_assistant_message: None,
    exec: ExecState { running_commands: HashMap::new(), running_read_agg_index: None, pending_exec_ends: HashMap::new() },
    canceled_exec_call_ids: HashSet::new(),
            tools_state: ToolState { running_custom_tools: HashMap::new(), running_web_search: HashMap::new() },
            live_builder: RowBuilder::new(usize::MAX),
            pending_images: HashMap::new(),
            welcome_shown: false,
            latest_browser_screenshot: Arc::new(Mutex::new(None)),
            cached_image_protocol: RefCell::new(None),
            cached_picker: RefCell::new(terminal_info.picker.clone()),
            cached_cell_size: std::cell::OnceCell::new(),
            terminal_info,
            active_agents: Vec::new(),
            agents_ready_to_start: false,
            last_agent_prompt: None,
            agent_context: None,
            agent_task: None,
            overall_task_status: "preparing".to_string(),
            sparkline_data: std::cell::RefCell::new(Vec::new()),
            last_sparkline_update: std::cell::RefCell::new(std::time::Instant::now()),
            stream: crate::streaming::controller::StreamController::new(config.clone()),
            stream_state: StreamState { current_kind: None, closed_answer_ids: HashSet::new(), closed_reasoning_ids: HashSet::new(), next_seq: 1, seq_answer_final: None, drop_streaming: false },
            interrupts: interrupts::InterruptManager::new(),
            ended_call_ids: HashSet::new(),
            diffs: DiffsState { session_patch_sets: Vec::new(), baseline_file_contents: HashMap::new(), overlay: None, confirm: None, body_visible_rows: std::cell::Cell::new(0) },
            height_cache: std::cell::RefCell::new(std::collections::HashMap::new()),
            height_cache_last_width: std::cell::Cell::new(0),
            height_manager: RefCell::new(HeightManager::new(
                crate::height_manager::HeightManagerConfig::default(),
            )),
            layout: LayoutState {
                scroll_offset: 0,
                last_max_scroll: std::cell::Cell::new(0),
                last_history_viewport_height: std::cell::Cell::new(0),
                vertical_scrollbar_state: std::cell::RefCell::new(ScrollbarState::default()),
                scrollbar_visible_until: std::cell::Cell::new(None),
                last_hud_present: std::cell::Cell::new(false),
                browser_hud_expanded: false,
                agents_hud_expanded: false,
                last_frame_height: std::cell::Cell::new(0),
            },
            prefix_sums: std::cell::RefCell::new(Vec::new()),
            last_prefix_width: std::cell::Cell::new(0),
            last_prefix_count: std::cell::Cell::new(0),
            prefix_valid: std::cell::Cell::new(false),
            last_theme: crate::theme::current_theme(),
            perf_state: PerfState { enabled: false, stats: std::cell::RefCell::new(PerfStats::default()) },
            session_id: None,
            pending_jump_back: None,
            active_task_ids: HashSet::new(),
            queued_user_messages: std::collections::VecDeque::new(),
            pending_user_prompts_for_next_turn: 0,
            browser_is_external: false,
            // Strict ordering init for forked widget
            cell_order_seq: vec![OrderKey { req: 0, out: -1, seq: 0 }],
            cell_order_dbg: vec![None; 1],
            reasoning_index: HashMap::new(),
            stream_order_seq: HashMap::new(),
            last_seen_request_index: 0,
            current_request_index: 0,
            internal_seq: 0,
            show_order_overlay,
            
        };
        // Welcome at top of first request for forked session too
        w.history_push_top_next_req(history_cell::new_animated_welcome());
        w
    }

    /// Export current user/assistant messages into ResponseItem list for forking.
    pub(crate) fn export_response_items(&self) -> Vec<dev_protocol::models::ResponseItem> {
        use dev_protocol::models::{ContentItem, ResponseItem};
        let mut items = Vec::new();
        for cell in &self.history_cells {
            match cell.kind() {
                crate::history_cell::HistoryCellType::User => {
                    let text = cell.display_lines().iter().map(|l| l.spans.iter().map(|s| s.content.to_string()).collect::<String>()).collect::<Vec<_>>().join("\n");
                    items.push(ResponseItem::Message { id: None, role: "user".to_string(), content: vec![ContentItem::OutputText { text }] });
                }
                crate::history_cell::HistoryCellType::Assistant => {
                    let text = cell.display_lines().iter().map(|l| l.spans.iter().map(|s| s.content.to_string()).collect::<String>()).collect::<Vec<_>>().join("\n");
                    items.push(ResponseItem::Message { id: None, role: "assistant".to_string(), content: vec![ContentItem::OutputText { text }] });
                }
                _ => {}
            }
        }
        items
    }

    pub(crate) fn config_ref(&self) -> &Config { &self.config }

    /// Check if there are any animations and trigger redraw if needed
    pub fn check_for_initial_animations(&mut self) {
        if self.history_cells.iter().any(|cell| cell.is_animating()) {
            tracing::info!("Initial animation detected, scheduling frame");
            // Schedule initial frame for animations to ensure they start properly.
            // Use ScheduleFrameIn to avoid debounce issues with immediate RequestRedraw.
            self.app_event_tx.send(AppEvent::ScheduleFrameIn(std::time::Duration::from_millis(50)));
        }
    }
    
    /// Format model name with proper capitalization (e.g., "gpt-4" -> "GPT-4")
    fn format_model_name(&self, model_name: &str) -> String {
        if model_name.to_lowercase().starts_with("gpt-") {
            format!("GPT{}", &model_name[3..])
        } else {
            model_name.to_string()
        }
    }

    /// Calculate the maximum scroll offset based on current content size
    #[allow(dead_code)]
    fn calculate_max_scroll_offset(&self, content_area_height: u16) -> u16 {
        let mut total_height = 0u16;

        // Calculate total content height (same logic as render method)
        for cell in &self.history_cells {
            let h = cell.desired_height(80); // Use reasonable width for height calculation
            total_height = total_height.saturating_add(h);
        }

        if let Some(ref cell) = self.active_exec_cell {
            let h = cell.desired_height(80);
            total_height = total_height.saturating_add(h);
        }

        // Max scroll is content height minus available height
        total_height.saturating_sub(content_area_height)
    }

    pub(crate) fn handle_key_event(&mut self, key_event: KeyEvent) {
        // Intercept keys for diff overlay when active
        if diff_handlers::handle_diff_key(self, key_event) { return; }
        if key_event.kind == KeyEventKind::Press {
            self.bottom_pane.clear_ctrl_c_quit_hint();
        }

        // Global HUD toggles (avoid conflicting with common editor keys):
        // - Ctrl+B: toggle Browser panel (expand/collapse)
        // - Ctrl+G: toggle Agents panel (expand/collapse)
        if let KeyEvent {
            code: crossterm::event::KeyCode::Char('b'),
            modifiers: crossterm::event::KeyModifiers::CONTROL,
            kind: KeyEventKind::Press | KeyEventKind::Repeat,
            ..
        } = key_event
        {
            self.toggle_browser_hud();
            return;
        }
        if let KeyEvent {
            code: crossterm::event::KeyCode::Char('a'),
            modifiers: crossterm::event::KeyModifiers::CONTROL,
            kind: KeyEventKind::Press | KeyEventKind::Repeat,
            ..
        } = key_event
        {
            self.toggle_agents_hud();
            return;
        }

        // Fast-path PageUp/PageDown to scroll the transcript by a viewport at a time.
        if let crossterm::event::KeyEvent { code: crossterm::event::KeyCode::PageUp, kind: KeyEventKind::Press | KeyEventKind::Repeat, .. } = key_event {
            layout_scroll::page_up(self);
            return;
        }
        if let crossterm::event::KeyEvent { code: crossterm::event::KeyCode::PageDown, kind: KeyEventKind::Press | KeyEventKind::Repeat, .. } = key_event {
            layout_scroll::page_down(self);
            return;
        }

        match self.bottom_pane.handle_key_event(key_event) {
            InputResult::Submitted(text) => {
                // Commit pending jump-back (make trimming permanent) before submission
                if self.pending_jump_back.is_some() {
                    self.pending_jump_back = None;
                }
                let user_message = self.parse_message_with_images(text);
                self.submit_user_message(user_message);
            }
            InputResult::Command(_cmd) => {
                // Command was dispatched at the App layer; request redraw.
                self.app_event_tx.send(AppEvent::RequestRedraw);
            }
            InputResult::ScrollUp => {
                // Only allow Up to navigate command history when the top view
                // cannot be scrolled at all (no scrollback available).
                if self.layout.last_max_scroll.get() == 0 {
                    if self.bottom_pane.try_history_up() { return; }
                }
                // Scroll up in chat history (increase offset, towards older content)
                // Use last_max_scroll computed during the previous render to avoid overshoot
                let new_offset = self
                    .layout.scroll_offset
                    .saturating_add(3)
                    .min(self.layout.last_max_scroll.get());
                self.layout.scroll_offset = new_offset;
                self.flash_scrollbar();
                // Enable compact mode so history can use the spacer line
                if self.layout.scroll_offset > 0 {
                    self.bottom_pane.set_compact_compose(true);
                    self.height_manager
                        .borrow_mut()
                        .record_event(HeightEvent::ComposerModeChange);
                    // Mark that the very next Down should continue scrolling chat (sticky)
                    self.bottom_pane.mark_next_down_scrolls_history();
                }
                self.app_event_tx.send(AppEvent::RequestRedraw);
                self.height_manager
                    .borrow_mut()
                    .record_event(HeightEvent::UserScroll);
            }
            InputResult::ScrollDown => {
                // Only allow Down to navigate command history when the top view
                // cannot be scrolled at all (no scrollback available).
                if self.layout.last_max_scroll.get() == 0 && self.bottom_pane.history_is_browsing() {
                    if self.bottom_pane.try_history_down() { return; }
                }
                // Scroll down in chat history (decrease offset, towards bottom)
                if self.layout.scroll_offset == 0 {
                    // Already at bottom: ensure spacer above input is enabled.
                    self.bottom_pane.set_compact_compose(false);
                    self.app_event_tx.send(AppEvent::RequestRedraw);
                    self.height_manager
                        .borrow_mut()
                        .record_event(HeightEvent::UserScroll);
                    self.height_manager
                        .borrow_mut()
                        .record_event(HeightEvent::ComposerModeChange);
                } else if self.layout.scroll_offset >= 3 {
                    // Move towards bottom but do NOT toggle spacer yet; wait until
                    // the user confirms by pressing Down again at bottom.
                    self.layout.scroll_offset = self.layout.scroll_offset.saturating_sub(3);
                    self.app_event_tx.send(AppEvent::RequestRedraw);
                    self.height_manager
                        .borrow_mut()
                        .record_event(HeightEvent::UserScroll);
                } else if self.layout.scroll_offset > 0 {
                    // Land exactly at bottom without toggling spacer yet; require
                    // a subsequent Down to re-enable the spacer so the input
                    // doesn't move when scrolling into the line above it.
                    self.layout.scroll_offset = 0;
                    self.app_event_tx.send(AppEvent::RequestRedraw);
                    self.height_manager
                        .borrow_mut()
                        .record_event(HeightEvent::UserScroll);
                }
                self.flash_scrollbar();
            }
            InputResult::None => {
                // Trigger redraw so input wrapping/height reflects immediately
                self.app_event_tx.send(AppEvent::RequestRedraw);
            }
        }
    }

    fn toggle_browser_hud(&mut self) { layout_scroll::toggle_browser_hud(self); }

    fn toggle_agents_hud(&mut self) { layout_scroll::toggle_agents_hud(self); }

    // dispatch_command() removed — command routing is handled at the App layer via AppEvent::DispatchCommand

    pub(crate) fn handle_paste(&mut self, text: String) {
        // Check if the pasted text is a file path to an image
        let trimmed = text.trim();

        tracing::info!("Paste received: {:?}", trimmed);

        const IMAGE_EXTENSIONS: &[&str] = &[
            ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".webp", ".svg", ".ico", ".tiff", ".tif",
        ];

        // Check if it looks like a file path
        let is_likely_path = trimmed.starts_with("file://")
            || trimmed.starts_with("/")
            || trimmed.starts_with("~/")
            || trimmed.starts_with("./");

        if is_likely_path {
            // Remove escape backslashes that terminals add for special characters
            let unescaped = trimmed
                .replace("\\ ", " ")
                .replace("\\(", "(")
                .replace("\\)", ")");

            // Handle file:// URLs (common when dragging from Finder)
            let path_str = if unescaped.starts_with("file://") {
                // URL decode to handle spaces and special characters
                // Simple decoding for common cases (spaces as %20, etc.)
                unescaped
                    .strip_prefix("file://")
                    .map(|s| {
                        s.replace("%20", " ")
                            .replace("%28", "(")
                            .replace("%29", ")")
                            .replace("%5B", "[")
                            .replace("%5D", "]")
                            .replace("%2C", ",")
                            .replace("%27", "'")
                            .replace("%26", "&")
                            .replace("%23", "#")
                            .replace("%40", "@")
                            .replace("%2B", "+")
                            .replace("%3D", "=")
                            .replace("%24", "$")
                            .replace("%21", "!")
                            .replace("%2D", "-")
                            .replace("%2E", ".")
                    })
                    .unwrap_or_else(|| unescaped.clone())
            } else {
                unescaped
            };

            tracing::info!("Decoded path: {:?}", path_str);

            // Check if it has an image extension
            let is_image = IMAGE_EXTENSIONS
                .iter()
                .any(|ext| path_str.to_lowercase().ends_with(ext));

            if is_image {
                let path = PathBuf::from(&path_str);
                tracing::info!("Checking if path exists: {:?}", path);
                if path.exists() {
                    tracing::info!("Image file dropped/pasted: {:?}", path);
                    // Get just the filename for display
                    let filename = path.file_name().and_then(|n| n.to_str()).unwrap_or("image");

                    // Add a placeholder to the compose field instead of submitting
                    let placeholder = format!("[image: {}]", filename);

                    // Store the image path for later submission
                    self.pending_images.insert(placeholder.clone(), path);

                    // Add the placeholder text to the compose field
                    self.bottom_pane.handle_paste(placeholder);
                    // Force immediate redraw to reflect input growth/wrap
                    self.request_redraw();
                    return;
                } else {
                    tracing::warn!("Image path does not exist: {:?}", path);
                }
            } else {
                // For non-image files, paste the decoded path as plain text.
                let path = PathBuf::from(&path_str);
                if path.exists() && path.is_file() {
                    self.bottom_pane.handle_paste(path_str);
                    self.request_redraw();
                    return;
                }
            }
        }

        // Otherwise handle as regular text paste
        self.bottom_pane.handle_paste(text);
        // Force immediate redraw so compose height matches new content
        self.request_redraw();
    }

    /// Briefly show the vertical scrollbar and schedule a redraw to hide it.
    fn flash_scrollbar(&self) { layout_scroll::flash_scrollbar(self); }

    fn history_insert_with_key_global(&mut self, cell: Box<dyn HistoryCell>, key: OrderKey) -> usize {
        self.history_insert_with_key_global_tagged(cell, key, "untagged")
    }

    // Internal: same as above but with a short tag for debug overlays.
    fn history_insert_with_key_global_tagged(&mut self, cell: Box<dyn HistoryCell>, key: OrderKey, tag: &'static str) -> usize {
        // Any ordered insert of a non-reasoning cell means reasoning is no longer the
        // bottom-most active block; drop the in-progress ellipsis on collapsed titles.
        let is_reasoning_cell = cell
            .as_any()
            .downcast_ref::<crate::history_cell::CollapsibleReasoningCell>()
            .is_some();
        if !is_reasoning_cell {
            self.clear_reasoning_in_progress();
        }
        // Determine insertion position across the entire history
        let mut pos = self.history_cells.len();
        for i in 0..self.history_cells.len() {
            if let Some(existing) = self.cell_order_seq.get(i) {
                if *existing > key { pos = i; break; }
            }
        }

        // Keep auxiliary order vector in lockstep with history before inserting
        if self.cell_order_seq.len() < self.history_cells.len() {
            let missing = self.history_cells.len() - self.cell_order_seq.len();
            for _ in 0..missing { self.cell_order_seq.push(OrderKey { req: 0, out: -1, seq: 0}); }
        }

        tracing::info!(
            "[order] insert: {} pos={} len_before={} order_len_before={} tag={}",
            Self::debug_fmt_order_key(key),
            pos,
            self.history_cells.len(),
            self.cell_order_seq.len(),
            tag
        );
        // If order overlay is enabled, compute a short, inline debug summary for
        // reasoning titles so we can spot mid‑word character drops quickly.
        // We intentionally do this before inserting so we can attach the
        // composed string alongside the standard order debug info.
        let reasoning_title_dbg: Option<String> = if self.show_order_overlay {
            // CollapsibleReasoningCell shows a collapsed "title" line; extract
            // the first visible line and summarize its raw text/lengths.
            if let Some(rc) = cell
                .as_any()
                .downcast_ref::<crate::history_cell::CollapsibleReasoningCell>()
            {
                let lines = rc.display_lines_trimmed();
                let first = lines.first();
                if let Some(line) = first {
                    // Collect visible text and basic metrics
                    let text: String = line
                        .spans
                        .iter()
                        .map(|s| s.content.as_ref())
                        .collect();
                    let bytes = text.len();
                    let chars = text.chars().count();
                    let width = unicode_width::UnicodeWidthStr::width(text.as_str());
                    let spans = line.spans.len();
                    // Per‑span byte lengths to catch odd splits inside words
                    let span_lens: Vec<usize> = line
                        .spans
                        .iter()
                        .map(|s| s.content.len())
                        .collect();
                    // Truncate preview to avoid overflow in narrow panes
                    let mut preview = text.clone();
                    // Truncate preview by display width, not bytes, to avoid splitting
                    // a multi-byte character at an invalid boundary.
                    {
                        use unicode_width::UnicodeWidthStr as _;
                        let maxw = 120usize;
                        if preview.width() > maxw {
                            preview = format!(
                                "{}…",
                                crate::live_wrap::take_prefix_by_width(&preview, maxw.saturating_sub(1)).0
                            );
                        }
                    }
                    Some(format!(
                        "title='{}' bytes={} chars={} width={} spans={} span_bytes={:?}",
                        preview, bytes, chars, width, spans, span_lens
                    ))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        self.history_cells.insert(pos, cell);
        // Ensure order vector is also long enough for position after cell insert
        if self.cell_order_seq.len() < pos { self.cell_order_seq.resize(pos, OrderKey { req: 0, out: -1, seq: 0}); }
        self.cell_order_seq.insert(pos, key);
        // Insert debug info aligned with cell insert
        let ordered = "ordered";
        let req_dbg = format!("{}", key.req);
        let dbg = if let Some(tdbg) = reasoning_title_dbg {
            format!(
                "insert: {} req={} key={} {} pos={} tag={} | {}",
                ordered,
                req_dbg,
                0,
                Self::debug_fmt_order_key(key),
                pos,
                tag,
                tdbg
            )
        } else {
            format!(
                "insert: {} req={} {} pos={} tag={}",
                ordered,
                req_dbg,
                Self::debug_fmt_order_key(key),
                pos,
                tag
            )
        };
        if self.cell_order_dbg.len() < pos { self.cell_order_dbg.resize(pos, None); }
        self.cell_order_dbg.insert(pos, Some(dbg));
        self.invalidate_height_cache();
        self.autoscroll_if_near_bottom();
        self.bottom_pane.set_has_chat_history(true);
        self.process_animation_cleanup();
        // Maintain input focus when new history arrives
        self.bottom_pane.ensure_input_focus();
        self.app_event_tx.send(AppEvent::RequestRedraw);
        pos
    }

    /// Push a cell using a synthetic global order key at the bottom of the current request.
    fn history_push(&mut self, cell: impl HistoryCell + 'static) { let key = self.next_internal_key(); let _ = self.history_insert_with_key_global_tagged(Box::new(cell), key, "epilogue"); }
    /// Push a cell using a synthetic key at the TOP of the NEXT request.
    fn history_push_top_next_req(&mut self, cell: impl HistoryCell + 'static) { let key = self.next_req_key_top(); let _ = self.history_insert_with_key_global_tagged(Box::new(cell), key, "prelude"); }
    /// Push a user prompt so it appears right under banners and above model output for the next request.
    fn history_push_prompt_next_req(&mut self, cell: impl HistoryCell + 'static) { let key = self.next_req_key_prompt(); let _ = self.history_insert_with_key_global_tagged(Box::new(cell), key, "prompt"); }

    fn history_replace_at(&mut self, idx: usize, cell: Box<dyn HistoryCell>) {
        if idx < self.history_cells.len() {
            self.history_cells[idx] = cell;
            self.invalidate_height_cache();
            self.request_redraw();
            // Keep debug info for this cell index as-is.
        }
    }

    fn history_remove_at(&mut self, idx: usize) {
        if idx < self.history_cells.len() {
            self.history_cells.remove(idx);
            if idx < self.cell_order_seq.len() { self.cell_order_seq.remove(idx); }
            if idx < self.cell_order_dbg.len() { self.cell_order_dbg.remove(idx); }
            self.invalidate_height_cache();
            self.request_redraw();
        }
    }

    fn history_replace_and_maybe_merge(&mut self, idx: usize, cell: Box<dyn HistoryCell>) {
        // Replace at index, then attempt standard exec merge with previous cell.
        self.history_replace_at(idx, cell);
        // Merge only if the new cell is an Exec with output (completed) or a MergedExec.
        crate::chatwidget::exec_tools::try_merge_completed_exec_at(self, idx);
    }

    // Merge adjacent tool cells with the same header (e.g., successive Web Search blocks)
    fn history_maybe_merge_tool_with_previous(&mut self, idx: usize) {
        if idx == 0 || idx >= self.history_cells.len() { return; }
        let new_lines = self.history_cells[idx].display_lines();
        let new_header = new_lines
            .first()
            .and_then(|l| l.spans.get(0))
            .map(|s| s.content.clone().to_string())
            .unwrap_or_default();
        if new_header.is_empty() { return; }
        let prev_lines = self.history_cells[idx - 1].display_lines();
        let prev_header = prev_lines
            .first()
            .and_then(|l| l.spans.get(0))
            .map(|s| s.content.clone().to_string())
            .unwrap_or_default();
        if new_header != prev_header { return; }
        let mut combined = prev_lines.clone();
        while combined
            .last()
            .map(|l| crate::render::line_utils::is_blank_line_trim(l))
            .unwrap_or(false)
        { combined.pop(); }
        let mut body: Vec<ratatui::text::Line<'static>> = new_lines.into_iter().skip(1).collect();
        while body.first().map(|l| crate::render::line_utils::is_blank_line_trim(l)).unwrap_or(false) { body.remove(0); }
        while body.last().map(|l| crate::render::line_utils::is_blank_line_trim(l)).unwrap_or(false) { body.pop(); }
        if let Some(first_line) = body.first_mut() {
            if let Some(first_span) = first_line.spans.get_mut(0) {
                if first_span.content == "  └ " || first_span.content == "└ " {
                    first_span.content = "  ".into();
                }
            }
        }
        combined.extend(body);
        self.history_replace_at(idx - 1, Box::new(crate::history_cell::PlainHistoryCell { lines: combined, kind: crate::history_cell::HistoryCellType::Plain }));
        self.history_remove_at(idx);
    }

    /// Clean up faded-out animation cells
    fn process_animation_cleanup(&mut self) {
        // With trait-based cells, we can't easily detect and clean up specific cell types
        // Animation cleanup is now handled differently
    }

    fn submit_user_message(&mut self, user_message: UserMessage) {
        // Surface a local diagnostic note and anchor it to the NEXT turn,
        // placing it directly after the user prompt so ordering is stable.
        // (debug message removed)
        // Fade the welcome cell only when a user actually posts a message.
        for cell in &self.history_cells { cell.trigger_fade(); }
        let UserMessage { display_text, mut ordered_items } = user_message;
        let original_text = display_text.clone();
        // Build a combined string view of the text-only parts to process slash commands
        let mut text_only = String::new();
        for it in &ordered_items {
            if let InputItem::Text { text } = it { if !text_only.is_empty() { text_only.push('\n'); } text_only.push_str(text); }
        }

        // Save the prompt if it's a multi-agent command
        let original_trimmed = original_text.trim();
        if original_trimmed.starts_with("/plan ")
            || original_trimmed.starts_with("/solve ")
            || original_trimmed.starts_with("/code ")
        {
            self.last_agent_prompt = Some(original_text.clone());
        }

        // Process slash commands and expand them if needed
        let processed = crate::slash_command::process_slash_command_message(&text_only);
        match processed {
            crate::slash_command::ProcessedCommand::ExpandedPrompt(expanded) => {
                // Replace the message with the expanded prompt
                ordered_items.clear();
                ordered_items.push(InputItem::Text { text: expanded });
            }
            crate::slash_command::ProcessedCommand::RegularCommand(cmd, _args) => {
                // This is a regular slash command, dispatch it normally
                self.app_event_tx
                    .send(AppEvent::DispatchCommand(cmd, original_text.clone()));
                return;
            }
            crate::slash_command::ProcessedCommand::Error(error_msg) => {
                // Show error in history
                self.history_push(history_cell::new_error_event(error_msg));
                return;
            }
            crate::slash_command::ProcessedCommand::NotCommand(_) => {
                // Not a slash command, process normally
            }
        }

        let mut items: Vec<InputItem> = Vec::new();

        // Check if browser mode is enabled and capture screenshot
        // IMPORTANT: Always use global browser manager for consistency
        // The global browser manager ensures both TUI and agent tools use the same instance

        // We need to check if browser is enabled first
        // Use a channel to check browser status from async context
        let (status_tx, status_rx) = std::sync::mpsc::channel();
        tokio::spawn(async move {
            let browser_manager = ChatWidget::get_browser_manager().await;
            let enabled = browser_manager.is_enabled().await;
            let _ = status_tx.send(enabled);
        });

        let browser_enabled = status_rx.recv().unwrap_or(false);

        // Start async screenshot capture in background (non-blocking)
        if browser_enabled {
            tracing::info!("Browser is enabled, starting async screenshot capture...");

            // Clone necessary data for the async task
            let latest_browser_screenshot_clone = Arc::clone(&self.latest_browser_screenshot);

            tokio::spawn(async move {
                tracing::info!("Starting background screenshot capture...");

                // Rate-limit: skip if a capture ran very recently (< 4000ms)
                let now_ms = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_millis() as u64;
                let last = BG_SHOT_LAST_START_MS.load(Ordering::Relaxed);
                if now_ms.saturating_sub(last) < 4000 {
                    tracing::info!("Skipping background screenshot: rate-limited");
                    return;
                }

                // Single-flight: skip if another capture is in progress
                if BG_SHOT_IN_FLIGHT.swap(true, Ordering::AcqRel) {
                    tracing::info!("Skipping background screenshot: already in-flight");
                    return;
                }
                BG_SHOT_LAST_START_MS.store(now_ms, Ordering::Relaxed);
                // Ensure we always clear the flag
                struct ShotGuard;
                impl Drop for ShotGuard { fn drop(&mut self) { BG_SHOT_IN_FLIGHT.store(false, Ordering::Release); } }
                let _guard = ShotGuard;

                // Short settle to allow page to reach a stable state; keep it small
                tokio::time::sleep(tokio::time::Duration::from_millis(800)).await;

                let browser_manager = ChatWidget::get_browser_manager().await;

                // Retry screenshot capture with exponential backoff
                // Keep background capture lightweight: single attempt with a modest timeout
                let mut attempts = 0;
                let max_attempts = 1;

                loop {
                    attempts += 1;
                    tracing::info!(
                        "Screenshot capture attempt {} of {}",
                        attempts,
                        max_attempts
                    );

                    // Add timeout to screenshot capture
                    let capture_result = tokio::time::timeout(
                        tokio::time::Duration::from_secs(5),
                        browser_manager.capture_screenshot_with_url(),
                    )
                    .await;

                    match capture_result {
                        Ok(Ok((screenshot_paths, url))) => {
                            tracing::info!(
                                "Background screenshot capture succeeded with {} images on attempt {}",
                                screenshot_paths.len(),
                                attempts
                            );

                            // Save the first screenshot path and URL for display in the TUI
                            if let Some(first_path) = screenshot_paths.first() {
                                if let Ok(mut latest) = latest_browser_screenshot_clone.lock() {
                                    let url_string =
                                        url.clone().unwrap_or_else(|| "Browser".to_string());
                                    *latest = Some((first_path.clone(), url_string));
                                }
                            }

                            // Create screenshot items
                            let mut screenshot_items = Vec::new();
                            for path in screenshot_paths {
                                if path.exists() {
                                    tracing::info!("Adding browser screenshot: {}", path.display());
                                    let timestamp = std::time::SystemTime::now()
                                        .duration_since(std::time::UNIX_EPOCH)
                                        .unwrap_or_default()
                                        .as_secs();
                                    let metadata = format!(
                                        "screenshot:{}:{}",
                                        timestamp,
                                        url.as_deref().unwrap_or("unknown")
                                    );
                                    screenshot_items.push(InputItem::EphemeralImage {
                                        path,
                                        metadata: Some(metadata),
                                    });
                                }
                            }

                            // Do not enqueue screenshots as messages.
                            // They are now injected per-turn by the core session.
                            break; // Success - exit retry loop
                        }
                        Ok(Err(e)) => {
                            tracing::warn!(
                                "Background screenshot capture failed (attempt {}): {}",
                                attempts, e
                            );
                            break;
                        }
                        Err(_timeout_err) => {
                            tracing::warn!(
                                "Background screenshot capture timed out (attempt {})",
                                attempts
                            );
                            break;
                        }
                    }
                }
            });
        } else {
            tracing::info!("Browser is not enabled, skipping screenshot capture");
        }

        // Use the ordered items (text + images interleaved with markers)
        items.extend(ordered_items);

        if items.is_empty() {
            return;
        }

        // Debug logging for what we're sending
        let ephemeral_count = items
            .iter()
            .filter(|item| matches!(item, InputItem::EphemeralImage { .. }))
            .count();
        let total_items = items.len();
        if ephemeral_count > 0 {
            tracing::info!(
                "Sending {} items to model (including {} ephemeral images)",
                total_items,
                ephemeral_count
            );
        }

        // New policy: always send immediately even if a task is running.
        // The core will abort any current task on UserInput and start a new turn.
        // This prevents messages from being stranded in `queued_user_messages`
        // after an interrupt (there is no TaskComplete after Op::Interrupt).
        if self.bottom_pane.is_task_running() || !self.active_task_ids.is_empty() {
            self.bottom_pane.update_status_text("interrupting & starting new turn".to_string());
        }

        // Idle path: append the user cell first so the upcoming TaskStarted
        // window begins after it, ensuring assistant output renders below.
        if !original_text.is_empty() {
            self.history_push_prompt_next_req(history_cell::new_user_prompt(original_text.clone()));
            self.pending_user_prompts_for_next_turn = self.pending_user_prompts_for_next_turn.saturating_add(1);
        }

        // Now send to the agent
        let _ = self.dev_op_tx.send(Op::UserInput { items });
        if !original_text.is_empty() {
            self.dev_op_tx
                .send(Op::AddToHistory { text: original_text.clone() })
                .unwrap_or_else(|e| tracing::error!("failed to send AddHistory op: {e}"));
        }

        // (debug watchdog removed)
    }

    #[allow(dead_code)]
    pub(crate) fn set_mouse_status_message(&mut self, message: &str) {
        self.bottom_pane.update_status_text(message.to_string());
    }

    pub(crate) fn handle_mouse_event(&mut self, mouse_event: crossterm::event::MouseEvent) {
        use crossterm::event::KeyModifiers;
        use crossterm::event::MouseEventKind;

        // Check if Shift is held - if so, let the terminal handle selection
        if mouse_event.modifiers.contains(KeyModifiers::SHIFT) {
            // Don't handle any mouse events when Shift is held
            // This allows the terminal's native text selection to work
            return;
        }

        match mouse_event.kind {
            MouseEventKind::ScrollUp => layout_scroll::mouse_scroll(self, true),
            MouseEventKind::ScrollDown => layout_scroll::mouse_scroll(self, false),
            _ => {
                // Ignore other mouse events for now
            }
        }
    }

    pub(crate) fn handle_dev_event(&mut self, event: Event) {
        tracing::debug!(
            "handle_dev_event({})",
            serde_json::to_string_pretty(&event).unwrap_or_default()
        );
        // Strict ordering: all LLM/tool events must carry OrderMeta; internal events use synthetic keys.
        // Track provider order to anchor internal inserts at the bottom of the active request.
        self.note_order(event.order.as_ref());

        let Event { id, msg, .. } = event.clone();
        match msg {
            EventMsg::SessionConfigured(event) => {
                // Record session id for potential future fork/backtrack features
                self.session_id = Some(event.session_id);
                self.bottom_pane
                    .set_history_metadata(event.history_log_id, event.history_entry_count);
                // Record session information at the top of the conversation.
                // Only show welcome message on first SessionConfigured event
                let is_first = !self.welcome_shown;
                if is_first {
                    self.welcome_shown = true;
                }
                self.history_push_top_next_req(history_cell::new_session_info(&self.config, event, is_first)); // tag: prelude

                if let Some(user_message) = self.initial_user_message.take() {
                    // If the user provided an initial message, add it to the
                    // conversation history.
                    self.submit_user_message(user_message);
                }

                self.request_redraw();
            }
            EventMsg::WebSearchBegin(ev) => {
                // Enforce order presence (tool events should carry it)
                let ok = match event.order.as_ref() {
                    Some(om) => Self::order_key_from_order_meta(om),
                    None => {
                        tracing::warn!("missing OrderMeta on WebSearchBegin; using synthetic key");
                        self.next_internal_key()
                    }
                };
                tracing::info!("[order] WebSearchBegin call_id={} seq={}", ev.call_id, event.event_seq);
                tools::web_search_begin(self, ev.call_id, ev.query, ok)
            },
            EventMsg::AgentMessage(AgentMessageEvent { message }) => {
                // If the user requested an interrupt, ignore late final answers.
                if self.stream_state.drop_streaming {
                    tracing::debug!("Ignoring AgentMessage after interrupt");
                    return;
                }
                self.stream_state.seq_answer_final = Some(event.event_seq);
                // Strict order for the stream id
                let ok = match event.order.as_ref() {
                    Some(om) => Self::order_key_from_order_meta(om),
                    None => {
                        tracing::warn!("missing OrderMeta on AgentMessage; using synthetic key");
                        self.next_internal_key()
                    }
                };
                self.seed_stream_order_key(StreamKind::Answer, &id, ok);

                tracing::debug!(
                    "AgentMessage final id={} bytes={} preview={:?}",
                    id,
                    message.len(),
                    message.chars().take(80).collect::<String>()
                );

                // Close out any running tool/exec indicators before inserting final answer.
                self.finalize_all_running_due_to_answer();

                // Route final message through streaming controller so AppEvent::InsertFinalAnswer
                // is the single source of truth for assistant content.
                let sink = AppEventHistorySink(self.app_event_tx.clone());
                streaming::begin(self, StreamKind::Answer, Some(id.clone()));
                let _ = self.stream.apply_final_answer(&message, &sink);

                // Track last message for potential dedup heuristics.
                self.last_assistant_message = Some(message);
                // Do not mark closed here; the final insert path manages closed ids and replacement.
                self.maybe_hide_spinner();
            }
            EventMsg::ReplayHistory(ev) => {
                // Render prior transcript items statically without executing tools
                for item in ev.items {
                    self.render_replay_item(item);
                }
                self.request_redraw();
            }
            EventMsg::WebSearchComplete(ev) => tools::web_search_complete(self, ev.call_id, ev.query),
            EventMsg::AgentMessageDelta(AgentMessageDeltaEvent { delta }) => {
                tracing::debug!("AgentMessageDelta: {:?}", delta);
                // If the user requested an interrupt, ignore late deltas.
                if self.stream_state.drop_streaming {
                    tracing::debug!("Ignoring Answer delta after interrupt");
                    return;
                }
                // Ignore late deltas for ids that have already finalized in this turn
                if self.stream_state.closed_answer_ids.contains(&StreamId(id.clone())) {
                    tracing::debug!("Ignoring Answer delta for closed id={}", id);
                    return;
                }
                // Seed/refresh order key for this Answer stream id (must have OrderMeta)
                let ok = match event.order.as_ref() {
                    Some(om) => Self::order_key_from_order_meta(om),
                    None => {
                        tracing::warn!("missing OrderMeta on AgentMessageDelta; using synthetic key");
                        self.next_internal_key()
                    }
                };
                self.seed_stream_order_key(StreamKind::Answer, &id, ok);
                // Stream answer delta through StreamController
                streaming::delta_text(
                    self,
                    StreamKind::Answer,
                    id.clone(),
                    delta,
                    event.order.as_ref().and_then(|o| o.sequence_number),
                );
                // Show responding state while assistant streams
                self.bottom_pane.update_status_text("responding".to_string());
            }
            EventMsg::AgentReasoning(AgentReasoningEvent { text }) => {
                // Ignore late reasoning if we've dropped streaming due to interrupt.
                if self.stream_state.drop_streaming {
                    tracing::debug!("Ignoring AgentReasoning after interrupt");
                    return;
                }
                tracing::debug!("AgentReasoning event with text: {:?}...", text.chars().take(100).collect::<String>());
                // Guard duplicates for this id within the task
                if self.stream_state.closed_reasoning_ids.contains(&StreamId(id.clone())) {
                    tracing::warn!("Ignoring duplicate AgentReasoning for closed id={}", id);
                    return;
                }
                // Seed strict order key for this Reasoning stream
                let ok = match event.order.as_ref() {
                    Some(om) => Self::order_key_from_order_meta(om),
                    None => {
                        tracing::warn!("missing OrderMeta on AgentReasoning; using synthetic key");
                        self.next_internal_key()
                    }
                };
                tracing::info!("[order] EventMsg::AgentReasoning id={} key={:?}", id, ok);
                self.seed_stream_order_key(StreamKind::Reasoning, &id, ok);
                // Fallback: if any tools/execs are still marked running, complete them now.
                self.finalize_all_running_due_to_answer();
                // Use StreamController for final reasoning
                let sink = AppEventHistorySink(self.app_event_tx.clone());
                streaming::begin(self, StreamKind::Reasoning, Some(id.clone()));
                
                // The StreamController now properly handles duplicate detection and prevents
                // re-injecting content when we're already finishing a stream
                let _finished = self.stream.apply_final_reasoning(&text, &sink);
                // Stream finishing is handled by StreamController
                // Mark this id closed for further reasoning deltas in this turn
                self.stream_state.closed_reasoning_ids.insert(StreamId(id.clone()));
                // Clear in-progress flags on the most recent reasoning cell(s)
                if let Some(last) = self.history_cells.iter().rposition(|c| c.as_any().downcast_ref::<history_cell::CollapsibleReasoningCell>().is_some()) {
                    if let Some(reason) = self.history_cells[last].as_any().downcast_ref::<history_cell::CollapsibleReasoningCell>() {
                        reason.set_in_progress(false);
                    }
                }
                self.mark_needs_redraw();
            }
            EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent { delta }) => {
                tracing::debug!("AgentReasoningDelta: {:?}", delta);
                if self.stream_state.drop_streaming {
                    tracing::debug!("Ignoring Reasoning delta after interrupt");
                    return;
                }
                // Ignore late deltas for ids that have already finalized in this turn
                if self.stream_state.closed_reasoning_ids.contains(&StreamId(id.clone())) {
                    tracing::debug!("Ignoring Reasoning delta for closed id={}", id);
                    return;
                }
                // Seed strict order key for this Reasoning stream
                let ok = match event.order.as_ref() {
                    Some(om) => Self::order_key_from_order_meta(om),
                    None => {
                        tracing::warn!("missing OrderMeta on AgentReasoningDelta; using synthetic key");
                        self.next_internal_key()
                    }
                };
                tracing::info!("[order] EventMsg::AgentReasoningDelta id={} key={:?}", id, ok);
                self.seed_stream_order_key(StreamKind::Reasoning, &id, ok);
                // Stream reasoning delta through StreamController
                streaming::delta_text(
                    self,
                    StreamKind::Reasoning,
                    id.clone(),
                    delta,
                    event.order.as_ref().and_then(|o| o.sequence_number),
                );
                // Show thinking state while reasoning streams
                self.bottom_pane.update_status_text("thinking".to_string());
            }
            EventMsg::AgentReasoningSectionBreak(AgentReasoningSectionBreakEvent {}) => {
                // Insert section break in reasoning stream
                let sink = AppEventHistorySink(self.app_event_tx.clone());
                self.stream.insert_reasoning_section_break(&sink);
            }
            EventMsg::TaskStarted => {
                // This begins the new turn; clear the pending prompt anchor count
                // so subsequent background events use standard placement.
                self.pending_user_prompts_for_next_turn = 0;
                // Reset stream headers for new turn
                self.stream.reset_headers_for_new_turn();
                self.stream_state.current_kind = None;
                // New turn: clear closed id guards
                self.stream_state.closed_answer_ids.clear();
                self.stream_state.closed_reasoning_ids.clear();
                self.ended_call_ids.clear();
                self.bottom_pane.clear_ctrl_c_quit_hint();
                // Accept streaming again for this turn
                self.stream_state.drop_streaming = false;
                // Mark this task id as active and ensure the status stays visible
                self.active_task_ids.insert(id.clone());
                // Reset per-turn UI indicators; ordering is now global-only
                self.reasoning_index.clear();
                self.bottom_pane.set_task_running(true);
                self.bottom_pane.update_status_text("waiting for model".to_string());
                tracing::info!("[order] EventMsg::TaskStarted id={}", id);
                
                // Don't add loading cell - we have progress in the input area
                // self.add_to_history(history_cell::new_loading_cell("waiting for model".to_string()));
                
                self.mark_needs_redraw();
            }
            EventMsg::TaskComplete(TaskCompleteEvent { last_agent_message: _ }) => {
                // Finalize any active streams
                if self.stream.is_write_cycle_active() {
                    // Finalize both streams via streaming facade
                    streaming::finalize(self, StreamKind::Reasoning, true);
                    streaming::finalize(self, StreamKind::Answer, true);
                }
                // Remove this id from the active set (it may be a sub‑agent)
                self.active_task_ids.remove(&id);
                // Defensive: clear transient agents-preparing state
                self.agents_ready_to_start = false;
                // Convert any lingering running exec/tool cells to completed so the UI doesn't hang
                self.finalize_all_running_due_to_answer();
                // Mark any running web searches as completed
                if !self.tools_state.running_web_search.is_empty() {
                    // Replace each running web search cell in-place with a completed one
                    // Iterate over a snapshot of keys to avoid borrow issues
                    let entries: Vec<(ToolCallId, (usize, Option<String>))> = self
                        .tools_state.running_web_search
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect();
                    for (call_id, (idx, query_opt)) in entries {
                        // Try exact index; if out of bounds or shifted, search nearby from end
                        let mut target_idx = None;
                        if idx < self.history_cells.len() {
                            // Verify this index is still a running web search cell
                            let is_ws = self.history_cells[idx]
                                .as_any()
                                .downcast_ref::<history_cell::RunningToolCallCell>()
                                .is_some_and(|rt| rt.has_title("Web Search..."));
                            if is_ws { target_idx = Some(idx); }
                        }
                        if target_idx.is_none() {
                            for i in (0..self.history_cells.len()).rev() {
                                if let Some(rt) = self.history_cells[i]
                                    .as_any()
                                    .downcast_ref::<history_cell::RunningToolCallCell>()
                                {
                                    if rt.has_title("Web Search...") {
                                        target_idx = Some(i);
                                        break;
                                    }
                                }
                            }
                        }
                        if let Some(i) = target_idx {
                            if let Some(rt) = self.history_cells[i]
                                .as_any()
                                .downcast_ref::<history_cell::RunningToolCallCell>()
                            {
                                let completed = rt.finalize_web_search(true, query_opt);
                                self.history_replace_at(i, Box::new(completed));
                            }
                        }
                        // Remove from running set
                        self.tools_state.running_web_search.remove(&call_id);
                    }
                }
                // Now that streaming is complete, flush any queued interrupts
                self.flush_interrupt_queue();

                // If any user messages were queued while the previous task was
                // running, dispatch exactly one now to start the next turn.
                if let Some(next) = self.queued_user_messages.pop_front() {
                    // Move it from the sticky queue preview into history
                    let UserMessage { display_text, ordered_items } = next;
                    if !display_text.is_empty() {
                        self.history_push_prompt_next_req(history_cell::new_user_prompt(display_text.clone()));
                        self.pending_user_prompts_for_next_turn = self.pending_user_prompts_for_next_turn.saturating_add(1);
                    }
                    self.dev_op_tx
                        .send(Op::UserInput { items: ordered_items })
                        .unwrap_or_else(|e| tracing::error!("failed to send queued message: {e}"));
                    if !display_text.is_empty() {
                        self.dev_op_tx
                            .send(Op::AddToHistory { text: display_text })
                            .unwrap_or_else(|e| tracing::error!("failed to send AddHistory for queued: {e}"));
                    }
                    // Keep the spinner up; TaskStarted will follow.
                    self.bottom_pane.set_task_running(true);
                    self.bottom_pane.update_status_text("waiting for model".to_string());
                    self.request_redraw();
                }

                // Only drop the working status if nothing is actually running.
                let any_tools_running = !self.exec.running_commands.is_empty()
                    || !self.tools_state.running_custom_tools.is_empty()
                    || !self.tools_state.running_web_search.is_empty();
                let any_streaming = self.stream.is_write_cycle_active();
                let any_agents_active = !self.active_agents.is_empty() || self.agents_ready_to_start;
                let any_tasks_active = !self.active_task_ids.is_empty();

                if !(any_tools_running || any_streaming || any_agents_active || any_tasks_active) {
                    self.bottom_pane.set_task_running(false);
                }
                self.stream_state.current_kind = None;
                // Final re-check for idle state
                self.maybe_hide_spinner();
                self.mark_needs_redraw();
            }
            EventMsg::AgentReasoningRawContentDelta(AgentReasoningRawContentDeltaEvent { delta }) => {
                if self.stream_state.drop_streaming {
                    tracing::debug!("Ignoring RawContent delta after interrupt");
                    return;
                }
                // Treat raw reasoning content the same as summarized reasoning
                if self.stream_state.closed_reasoning_ids.contains(&StreamId(id.clone())) {
                    tracing::debug!("Ignoring RawContent delta for closed id={}", id);
                    return;
                }
                // Seed strict order key for this reasoning stream id
                let ok = match event.order.as_ref() {
                    Some(om) => Self::order_key_from_order_meta(om),
                    None => { tracing::warn!("missing OrderMeta on Tools::PlanUpdate; using synthetic key"); self.next_internal_key() }
                };
                self.seed_stream_order_key(StreamKind::Reasoning, &id, ok);

                streaming::delta_text(
                    self,
                    StreamKind::Reasoning,
                    id.clone(),
                    delta,
                    event.order.as_ref().and_then(|o| o.sequence_number),
                );
            }
            EventMsg::AgentReasoningRawContent(AgentReasoningRawContentEvent { text }) => {
                if self.stream_state.drop_streaming {
                    tracing::debug!("Ignoring AgentReasoningRawContent after interrupt");
                    return;
                }
                tracing::debug!("AgentReasoningRawContent event with text: {:?}...", text.chars().take(100).collect::<String>());
                if self.stream_state.closed_reasoning_ids.contains(&StreamId(id.clone())) {
                    tracing::warn!("Ignoring duplicate AgentReasoningRawContent for closed id={}", id);
                    return;
                }
                // Seed strict order key now so upcoming insert uses the correct key.
                let ok = match event.order.as_ref() {
                    Some(om) => Self::order_key_from_order_meta(om),
                    None => { tracing::warn!("missing OrderMeta on Tools::ReasoningBegin; using synthetic key"); self.next_internal_key() }
                };
                self.seed_stream_order_key(StreamKind::Reasoning, &id, ok);
                // Use StreamController for final raw reasoning
                let sink = AppEventHistorySink(self.app_event_tx.clone());
                streaming::begin(self, StreamKind::Reasoning, Some(id.clone()));
                let _finished = self.stream.apply_final_reasoning(&text, &sink);
                // Stream finishing is handled by StreamController
                self.stream_state.closed_reasoning_ids.insert(StreamId(id.clone()));
                if let Some(last) = self.history_cells.iter().rposition(|c| c.as_any().downcast_ref::<history_cell::CollapsibleReasoningCell>().is_some()) {
                    if let Some(reason) = self.history_cells[last].as_any().downcast_ref::<history_cell::CollapsibleReasoningCell>() {
                        reason.set_in_progress(false);
                    }
                }
                self.mark_needs_redraw();
            }
            EventMsg::TokenCount(token_usage) => {
                self.total_token_usage = add_token_usage(&self.total_token_usage, &token_usage);
                self.last_token_usage = token_usage;
                self.bottom_pane.set_token_usage(
                    self.total_token_usage.clone(),
                    self.last_token_usage.clone(),
                    self.config.model_context_window,
                );
            }
            EventMsg::Error(ErrorEvent { message }) => {
                self.on_error(message);
            }
            EventMsg::PlanUpdate(update) => {
                // Avoid interleaving plan updates inside streaming sections.
                // If a stream is active, defer until the stream finalizes so
                // the plan block doesn't split a heading and its content.
                if self.is_write_cycle_active() {
                    let seq = self.next_sequence();
                    self.interrupts.push_plan_update(seq, update, event.order.clone());
                } else {
                    let key = self.next_internal_key();
                    let _ = self.history_insert_with_key_global(Box::new(history_cell::new_plan_update(update)), key);
                }
            }
            EventMsg::ExecApprovalRequest(ev) => {
                let id2 = id.clone();
                let ev2 = ev.clone();
                let seq = event.event_seq;
                self.defer_or_handle(
                    move |interrupts| interrupts.push_exec_approval(seq, id, ev),
                    |this| {
                        this.finalize_active_stream();
                        this.flush_interrupt_queue();
                        this.handle_exec_approval_now(id2, ev2);
                        this.request_redraw();
                    },
                );
            }
            EventMsg::ApplyPatchApprovalRequest(ev) => {
                let id2 = id.clone();
                let ev2 = ev.clone();
                self.defer_or_handle(
                    move |interrupts| interrupts.push_apply_patch_approval(event.event_seq, id, ev),
                    |this| {
                        this.finalize_active_stream();
                        this.flush_interrupt_queue();
                        // Internal message (no OrderMeta in core for approvals): allocate synthetic key
                        let key = this.next_internal_key();
                        let cell = history_cell::new_patch_event(
                            history_cell::PatchEventType::ApprovalRequest,
                            ev2.changes.clone(),
                        );
                        let _ = this.history_insert_with_key_global(Box::new(cell), key);
                        // Push approval UI state to bottom pane
                        this.handle_apply_patch_approval_now(id2, ev2);
                        this.request_redraw();
                    },
                );
            }
            EventMsg::ExecCommandBegin(ev) => {
                let ev2 = ev.clone();
                let seq = event.event_seq;
                let om_begin = event.order.clone().expect("missing OrderMeta for ExecCommandBegin");
                let om_begin_for_handler = om_begin.clone();
                self.defer_or_handle(
                    move |interrupts| interrupts.push_exec_begin(seq, ev, Some(om_begin)),
                    move |this| {
                        // Finalize any active streaming sections, then establish
                        // the running Exec cell before flushing queued interrupts.
                        // This prevents an out‑of‑order ExecCommandEnd from being
                        // applied first (which would fall back to showing call_id).
                        this.finalize_active_stream();
                        tracing::info!("[order] ExecCommandBegin call_id={} seq={}", ev2.call_id, seq);
                        this.handle_exec_begin_now(ev2.clone(), &om_begin_for_handler);
                        // If an ExecEnd for this call_id arrived earlier and is waiting,
                        // apply it immediately now that we have a matching Begin.
                        if let Some((pending_end, order2, _ts)) = this
                            .exec
                            .pending_exec_ends
                            .remove(&ExecCallId(ev2.call_id.clone()))
                        {
                            // Use the same order for the pending end
                            this.handle_exec_end_now(pending_end, &order2);
                        }
                        this.flush_interrupt_queue();
                    },
                );
            }
            EventMsg::ExecCommandOutputDelta(_) => {
                // TODO
            }
            EventMsg::PatchApplyBegin(PatchApplyBeginEvent {
                call_id: _,
                auto_approved,
                changes,
            }) => {
                // Store for session diff popup (clone before moving into history)
                self.diffs.session_patch_sets.push(changes.clone());
                // Capture/adjust baselines, including rename moves
                if let Some(last) = self.diffs.session_patch_sets.last() {
                    for (src_path, chg) in last.iter() {
                        match chg {
                            dev_core::protocol::FileChange::Update { move_path: Some(dest_path), .. } => {
                                // Prefer to carry forward existing baseline from src to dest.
                                if let Some(baseline) = self.diffs.baseline_file_contents.remove(src_path) {
                                    self.diffs.baseline_file_contents.insert(dest_path.clone(), baseline);
                                } else if !self.diffs.baseline_file_contents.contains_key(dest_path) {
                                    // Fallback: snapshot current contents of src (pre-apply) under dest key.
                                    let baseline = std::fs::read_to_string(src_path).unwrap_or_default();
                                    self.diffs.baseline_file_contents.insert(dest_path.clone(), baseline);
                                }
                            }
                            _ => {
                                if !self.diffs.baseline_file_contents.contains_key(src_path) {
                                    let baseline = std::fs::read_to_string(src_path).unwrap_or_default();
                                    self.diffs.baseline_file_contents.insert(src_path.clone(), baseline);
                                }
                            }
                        }
                    }
                }
                // Enable Ctrl+D footer hint now that we have diffs to show
                self.bottom_pane.set_diffs_hint(true);
                // Strict order
                let ok = match event.order.as_ref() {
                    Some(om) => Self::order_key_from_order_meta(om),
                    None => { tracing::warn!("missing OrderMeta on ExecEnd flush; using synthetic key"); self.next_internal_key() }
                };
                let cell = history_cell::new_patch_event(PatchEventType::ApplyBegin { auto_approved }, changes);
                let _ = self.history_insert_with_key_global(Box::new(cell), ok);
            }
            EventMsg::PatchApplyEnd(ev) => {
                let ev2 = ev.clone();
                self.defer_or_handle(
                    move |interrupts| interrupts.push_patch_end(event.event_seq, ev),
                    |this| this.handle_patch_apply_end_now(ev2),
                );
            }
            EventMsg::ExecCommandEnd(ev) => {
                let ev2 = ev.clone();
                let seq = event.event_seq;
                let order_meta_end = event.order.clone().expect("missing OrderMeta for ExecCommandEnd");
                let om_for_send = order_meta_end.clone();
                let om_for_insert = order_meta_end.clone();
                self.defer_or_handle(
                    move |interrupts| interrupts.push_exec_end(seq, ev, Some(om_for_send)),
                    move |this| {
                        tracing::info!("[order] ExecCommandEnd call_id={} seq={}", ev2.call_id, seq);
                        // If we already have a running command for this call_id, finish it now.
                        let has_running = this
                            .exec
                            .running_commands
                            .contains_key(&ExecCallId(ev2.call_id.clone()));
                        if has_running {
                            this.handle_exec_end_now(ev2, &order_meta_end);
                        } else {
                            // Otherwise, stash it briefly and schedule a flush in case the
                            // matching Begin arrives shortly. This avoids rendering a fallback
                            // "call_<id>" cell when events are slightly out of order.
                            this.exec
                                .pending_exec_ends
                                .insert(ExecCallId(ev2.call_id.clone()), (ev2, om_for_insert, std::time::Instant::now()));
                            let tx = this.app_event_tx.clone();
                            std::thread::spawn(move || {
                                std::thread::sleep(std::time::Duration::from_millis(120));
                                tx.send(crate::app_event::AppEvent::FlushPendingExecEnds);
                            });
                        }
                    },
                );
            }
            EventMsg::McpToolCallBegin(ev) => {
                let ev2 = ev.clone();
                let seq = event.event_seq;
                let order_ok = match event.order.as_ref() { Some(om) => Self::order_key_from_order_meta(om), None => { tracing::warn!("missing OrderMeta on McpBegin; using synthetic key"); self.next_internal_key() } };
                self.defer_or_handle(
                    move |interrupts| interrupts.push_mcp_begin(seq, ev, event.order.clone()),
                    |this| {
                        this.finalize_active_stream();
                        this.flush_interrupt_queue();
                        tracing::info!("[order] McpToolCallBegin call_id={} seq={}", ev2.call_id, seq);
                        tools::mcp_begin(this, ev2, order_ok);
                    },
                );
            }
            EventMsg::McpToolCallEnd(ev) => {
                let ev2 = ev.clone();
                let seq = event.event_seq;
                let order_ok = match event.order.as_ref() { Some(om) => Self::order_key_from_order_meta(om), None => { tracing::warn!("missing OrderMeta on McpEnd; using synthetic key"); self.next_internal_key() } };
                self.defer_or_handle(
                    move |interrupts| interrupts.push_mcp_end(seq, ev, event.order.clone()),
                    |this| {
                        tracing::info!("[order] McpToolCallEnd call_id={} seq={}", ev2.call_id, seq);
                        tools::mcp_end(this, ev2, order_ok)
                    },
                );
            }
            EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
                call_id,
                tool_name,
                parameters,
            }) => {
                // Any custom tool invocation should fade out the welcome animation
                for cell in &self.history_cells { cell.trigger_fade(); }
                self.finalize_active_stream();
                // Flush any queued interrupts when streaming ends
                self.flush_interrupt_queue();
                // Show an active entry immediately for all custom tools so the user sees progress
                let params_string = parameters.map(|p| p.to_string());
                // Animated running cell with live timer and formatted args
                let cell = if tool_name.starts_with("browser_") {
                    history_cell::new_running_browser_tool_call(tool_name.clone(), params_string.clone())
                } else {
                    history_cell::new_running_custom_tool_call(tool_name.clone(), params_string.clone())
                };
                // Enforce ordering for custom tool begin
                let ok = match event.order.as_ref() { Some(om) => Self::order_key_from_order_meta(om), None => { tracing::warn!("missing OrderMeta on CustomToolCallBegin; using synthetic key"); self.next_internal_key() } };
                let idx = self.history_insert_with_key_global(Box::new(cell), ok);
                // Track index so we can replace it on completion
                if idx < self.history_cells.len() {
                    self.tools_state.running_custom_tools.insert(ToolCallId(call_id.clone()), idx);
                }

                // Update border status based on tool
                if tool_name.starts_with("browser_") {
                    self.bottom_pane.update_status_text("using browser".to_string());
                } else if tool_name.starts_with("agent_") {
                    self.bottom_pane.update_status_text("agents coordinating".to_string());
                } else {
                    self.bottom_pane
                        .update_status_text(format!("using tool: {}", tool_name));
                }
            }
            EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
                call_id,
                tool_name,
                parameters,
                duration,
                result,
            }) => {
                let ok = match event.order.as_ref() { Some(om) => Self::order_key_from_order_meta(om), None => { tracing::warn!("missing OrderMeta on CustomToolCallEnd; using synthetic key"); self.next_internal_key() } };
                tracing::info!("[order] CustomToolCallEnd call_id={} tool={} seq={}", call_id, tool_name, event.event_seq);
                // Convert parameters to String if present
                let params_string = parameters.map(|p| p.to_string());
                // Determine success and content from Result
                let (success, content) = match result {
                    Ok(content) => (true, content),
                    Err(error) => (false, error),
                };
                // Special-case web_fetch to render returned markdown nicely.
                if tool_name == "web_fetch" {
                    let completed = history_cell::new_completed_web_fetch_tool_call(
                        &self.config,
                        params_string,
                        duration,
                        success,
                        content,
                    );
                    if let Some(idx) = self.tools_state.running_custom_tools.remove(&ToolCallId(call_id)) {
                        if idx < self.history_cells.len() { self.history_replace_at(idx, Box::new(completed)); }
                        else { let _ = self.history_insert_with_key_global(Box::new(completed), ok); }
                    } else {
                        let _ = self.history_insert_with_key_global(Box::new(completed), ok);
                    }

                    // After tool completes, likely transitioning to response
                    self.bottom_pane.update_status_text("responding".to_string());
                    self.maybe_hide_spinner();
                    return;
                }
                let completed = history_cell::new_completed_custom_tool_call(
                    tool_name,
                    params_string,
                    duration,
                    success,
                    content,
                );
                if let Some(idx) = self.tools_state.running_custom_tools.remove(&ToolCallId(call_id)) { if idx < self.history_cells.len() { self.history_replace_at(idx, Box::new(completed)); } else { let _ = self.history_insert_with_key_global(Box::new(completed), ok); } } else { let _ = self.history_insert_with_key_global(Box::new(completed), ok); }

                // After tool completes, likely transitioning to response
                self.bottom_pane.update_status_text("responding".to_string());
                self.maybe_hide_spinner();
            }
            EventMsg::GetHistoryEntryResponse(event) => {
                let dev_core::protocol::GetHistoryEntryResponseEvent {
                    offset,
                    log_id,
                    entry,
                } = event;

                // Inform bottom pane / composer.
                self.bottom_pane
                    .on_history_entry_response(log_id, offset, entry.map(|e| e.text));
            }
            EventMsg::ShutdownComplete => {
                self.history_push(history_cell::new_background_event("🟡 ShutdownComplete".to_string()));
                self.app_event_tx.send(AppEvent::ExitRequest);
            }
            EventMsg::TurnDiff(TurnDiffEvent { unified_diff }) => {
                info!("TurnDiffEvent: {unified_diff}");
            }
            EventMsg::BackgroundEvent(BackgroundEventEvent { message }) => {
                info!("BackgroundEvent: {message}");
                // Surface lightweight background events in the history feed
                // so users see confirmations (e.g., CDP connect success).
                // If a user prompt was just inserted for the upcoming turn,
                // anchor these background events to that next turn so they
                // render below the prompt and above model output.
                if self.pending_user_prompts_for_next_turn > 0 {
                    let key = self.next_req_key_after_prompt();
                    let _ = self.history_insert_with_key_global_tagged(
                        Box::new(history_cell::new_background_event(message.clone())),
                        key,
                        "bg-next-turn",
                    );
                } else {
                    self.history_push(history_cell::new_background_event(message.clone()));
                }

                // Also reflect CDP connect success in the status line.
                if message.starts_with("✅ Connected to Chrome via CDP") {
                    self.bottom_pane
                        .update_status_text("using browser (CDP)".to_string());
                }
            }
            EventMsg::AgentStatusUpdate(AgentStatusUpdateEvent {
                agents,
                context,
                task,
            }) => {
                // Update the active agents list from the event
                self.active_agents.clear();
                for agent in agents {
                    self.active_agents.push(AgentInfo {
                        name: agent.name.clone(),
                        status: match agent.status.as_str() {
                            "pending" => AgentStatus::Pending,
                            "running" => AgentStatus::Running,
                            "completed" => AgentStatus::Completed,
                            "failed" => AgentStatus::Failed,
                            _ => AgentStatus::Pending,
                        },
                    });
                }

                // Store shared context and task
                self.agent_context = context;
                self.agent_task = task;

                // Update overall task status based on agent states
                self.overall_task_status = if self.active_agents.is_empty() {
                    "preparing".to_string()
                } else if self
                    .active_agents
                    .iter()
                    .any(|a| matches!(a.status, AgentStatus::Running))
                {
                    "running".to_string()
                } else if self
                    .active_agents
                    .iter()
                    .all(|a| matches!(a.status, AgentStatus::Completed))
                {
                    "complete".to_string()
                } else if self
                    .active_agents
                    .iter()
                    .any(|a| matches!(a.status, AgentStatus::Failed))
                {
                    "failed".to_string()
                } else {
                    "planning".to_string()
                };

                // Reflect concise agent status in the input border
                let count = self.active_agents.len();
                let msg = match self.overall_task_status.as_str() {
                    "preparing" => format!("agents: preparing ({} ready)", count),
                    "running" => format!("agents: running ({})", count),
                    "complete" => format!("agents: complete ({} ok)", count),
                    "failed" => "agents: failed".to_string(),
                    _ => "agents: planning".to_string(),
                };
                self.bottom_pane.update_status_text(msg);

                // Clear agents HUD when task is complete or failed
                if matches!(self.overall_task_status.as_str(), "complete" | "failed") {
                    self.active_agents.clear();
                    self.agents_ready_to_start = false;
                    self.agent_context = None;
                    self.agent_task = None;
                    self.last_agent_prompt = None;
                    self.sparkline_data.borrow_mut().clear();
                }

                // Reset ready to start flag when we get actual agent updates
                if !self.active_agents.is_empty() {
                    self.agents_ready_to_start = false;
                }
                self.request_redraw();
            }
            EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                screenshot_path,
                url,
            }) => {
                tracing::info!(
                    "Received browser screenshot update: {} at URL: {}",
                    screenshot_path.display(),
                    url
                );

                // Update the latest screenshot and URL for display
                if let Ok(mut latest) = self.latest_browser_screenshot.lock() {
                    let old_url = latest.as_ref().map(|(_, u)| u.clone());
                    *latest = Some((screenshot_path.clone(), url.clone()));
                    if old_url.as_ref() != Some(&url) {
                        tracing::info!("Browser URL changed from {:?} to {}", old_url, url);
                    }
                    tracing::debug!(
                        "Updated browser screenshot display with path: {} and URL: {}",
                        screenshot_path.display(),
                        url
                    );
                } else {
                    tracing::warn!("Failed to acquire lock for browser screenshot update");
                }

                // Request a redraw to update the display immediately
                self.app_event_tx.send(AppEvent::RequestRedraw);
            }
        }
    }

    fn request_redraw(&mut self) {
        self.app_event_tx.send(AppEvent::RequestRedraw);
    }

    pub(crate) fn handle_perf_command(&mut self, args: String) {
        let arg = args.trim().to_lowercase();
        match arg.as_str() {
            "on" => {
                self.perf_state.enabled = true;
                self.add_perf_output("performance tracing: on".to_string());
            }
            "off" => {
                self.perf_state.enabled = false;
                self.add_perf_output("performance tracing: off".to_string());
            }
            "reset" => {
                self.perf_state.stats.borrow_mut().reset();
                self.add_perf_output("performance stats reset".to_string());
            }
            "show" | "" => {
                let summary = self.perf_state.stats.borrow().summary();
                self.add_perf_output(summary);
            }
            _ => {
                self.add_perf_output("usage: /perf on | off | show | reset".to_string());
            }
        }
        self.request_redraw();
    }

    fn add_perf_output(&mut self, text: String) {
        let mut lines: Vec<ratatui::text::Line<'static>> = Vec::new();
        lines.push(ratatui::text::Line::from("performance".dim()));
        for l in text.lines() {
            lines.push(ratatui::text::Line::from(l.to_string()))
        }
        self.history_push(crate::history_cell::PlainHistoryCell {
            lines,
            kind: crate::history_cell::HistoryCellType::Notice,
        });
    }

    pub(crate) fn add_diff_output(&mut self, diff_output: String) {
        self.history_push(history_cell::new_diff_output(diff_output.clone()));
    }

    pub(crate) fn add_status_output(&mut self) {
        self.history_push(history_cell::new_status_output(
            &self.config,
            &self.total_token_usage,
        ));
    }

    pub(crate) fn add_prompts_output(&mut self) {
        self.history_push(history_cell::new_prompts_output());
    }

    pub(crate) fn add_agents_output(&mut self) {
        use ratatui::text::Line;

        // Gather active agents from current UI state
        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::from("/agents").fg(crate::colors::keyword()));
        lines.push(Line::from(""));

        // Platform + environment summary to aid debugging
        lines.push(Line::from(vec!["🖥  ".into(), "Environment".bold()]));
        let os = std::env::consts::OS;
        let arch = std::env::consts::ARCH;
        lines.push(Line::from(format!("  • Platform: {os}-{arch}")));
        lines.push(Line::from(format!("  • CWD: {}", self.config.cwd.display())));
        let in_git = dev_core::git_info::get_git_repo_root(&self.config.cwd).is_some();
        lines.push(Line::from(format!(
            "  • Git repo: {}",
            if in_git { "yes" } else { "no" }
        )));
        // PATH summary
        if let Some(path_os) = std::env::var_os("PATH") {
            let entries: Vec<String> = std::env::split_paths(&path_os)
                .map(|p| p.display().to_string())
                .collect();
            let shown = entries.iter().take(6).cloned().collect::<Vec<_>>().join("; ");
            let suffix = if entries.len() > 6 { format!(" (+{} more)", entries.len() - 6) } else { String::new() };
            lines.push(Line::from(format!("  • PATH ({} entries): {}{}", entries.len(), shown, suffix)));
        }
        #[cfg(target_os = "windows")]
        if let Ok(pathext) = std::env::var("PATHEXT") {
            lines.push(Line::from(format!("  • PATHEXT: {}", pathext)));
        }
        lines.push(Line::from(""));

        // Section: Active agents
        lines.push(Line::from(vec!["🤖 ".into(), "Active Agents".bold()]));
        if self.active_agents.is_empty() {
            if self.agents_ready_to_start {
                lines.push(Line::from("  • preparing agents…"));
            } else {
                lines.push(Line::from("  • No active agents"));
            }
        } else {
            for a in &self.active_agents {
                let status = match a.status {
                    AgentStatus::Pending => "pending",
                    AgentStatus::Running => "running",
                    AgentStatus::Completed => "completed",
                    AgentStatus::Failed => "failed",
                };
                lines.push(Line::from(format!("  • {} — {}", a.name, status)));
            }
        }

        lines.push(Line::from(""));

        // Section: Availability
        lines.push(Line::from(vec!["🧭 ".into(), "Availability".bold()]));

        // Determine which agents to check: configured (enabled) or defaults
        let mut to_check: Vec<(String, String, bool)> = Vec::new();
        if !self.config.agents.is_empty() {
            for a in &self.config.agents {
                if !a.enabled { continue; }
                let name = a.name.clone();
                let cmd = a.command.clone();
                let builtin = matches!(cmd.as_str(), "code" | "codex");
                to_check.push((name, cmd, builtin));
            }
        } else {
            to_check.push(("claude".to_string(), "claude".to_string(), false));
            to_check.push(("gemini".to_string(), "gemini".to_string(), false));
            to_check.push(("code".to_string(), "code".to_string(), true));
        }

        // Helper: PATH presence + resolved path
        let resolve_cmd = |cmd: &str| -> Option<String> { which::which(cmd).ok().map(|p| p.display().to_string()) };

        for (name, cmd, builtin) in to_check {
            if builtin {
                let exe = std::env::current_exe()
                    .ok()
                    .map(|p| p.display().to_string())
                    .unwrap_or_else(|| "(unknown)".to_string());
                lines.push(Line::from(format!("  • {} — available (built-in, exe: {})", name, exe)));
                // Reminder about exec flag ordering for trust bypass
                lines.push(Line::from("      Tip: use `exec --skip-git-repo-check` if needed (after subcommand)"));
            } else if let Some(path) = resolve_cmd(&cmd) {
                lines.push(Line::from(format!("  • {} — available ({} at {})", name, cmd, path)));
            } else {
                lines.push(Line::from(format!("  • {} — not found (command: {})", name, cmd)));
                // Short cross-platform hint
                lines.push(Line::from("      Debug: ensure the CLI is installed and on PATH"));
                lines.push(Line::from("      Windows: run `where <cmd>`; macOS/Linux: `which <cmd>`"));
            }
        }

        // Final helpful notes
        lines.push(Line::from(""));
        lines.push(Line::from("Notes:".bold()));
        lines.push(Line::from("- Built-in 'code' runs even without global shims."));
        lines.push(Line::from("- External CLIs must be in PATH; restart terminal after install."));

        self.history_push(crate::history_cell::PlainHistoryCell {
            lines,
            kind: crate::history_cell::HistoryCellType::Notice,
        });
        self.request_redraw();
    }

    pub(crate) fn show_diffs_popup(&mut self) {
        use crate::diff_render::create_diff_details_only;
        // Build a latest-first unique file list
        let mut order: Vec<PathBuf> = Vec::new();
        let mut seen: std::collections::HashSet<PathBuf> = std::collections::HashSet::new();
        for changes in self.diffs.session_patch_sets.iter().rev() {
            for (path, change) in changes.iter() {
                // If this change represents a move/rename, show the destination path in the tabs
                let display_path: PathBuf = match change {
                    dev_core::protocol::FileChange::Update { move_path: Some(dest), .. } => dest.clone(),
                    _ => path.clone(),
                };
                if seen.insert(display_path.clone()) {
                    order.push(display_path);
                }
            }
        }
        // Build tabs: for each file, create a single unified diff against the original baseline
        let mut tabs: Vec<(String, Vec<DiffBlock>)> = Vec::new();
        for path in order {
            // Resolve baseline (first-seen content) and current (on-disk) content
            let baseline = self
                .diffs.baseline_file_contents
                .get(&path)
                .cloned()
                .unwrap_or_default();
            let current = std::fs::read_to_string(&path).unwrap_or_default();
            // Build a unified diff from baseline -> current
            let unified = diffy::create_patch(&baseline, &current).to_string();
            // Render detailed lines (no header) using our diff renderer helpers
            let mut single = HashMap::new();
            single.insert(
                path.clone(),
                dev_core::protocol::FileChange::Update { unified_diff: unified.clone(), move_path: None },
            );
            let detail = create_diff_details_only(&single);
            let mut blocks: Vec<DiffBlock> = vec![DiffBlock { lines: detail }];

            // Count adds/removes for the header label from the unified diff
            let mut total_added: usize = 0;
            let mut total_removed: usize = 0;
            if let Ok(patch) = diffy::Patch::from_str(&unified) {
                for h in patch.hunks() {
                    for l in h.lines() {
                        match l {
                            diffy::Line::Insert(_) => total_added += 1,
                            diffy::Line::Delete(_) => total_removed += 1,
                            _ => {}
                        }
                    }
                }
            } else {
                for l in unified.lines() {
                    if l.starts_with("+++") || l.starts_with("---") || l.starts_with("@@") { continue; }
                    if let Some(b) = l.as_bytes().first() {
                        if *b == b'+' { total_added += 1; }
                        else if *b == b'-' { total_removed += 1; }
                    }
                }
            }
            // Prepend a header block with the full path and counts
            let header_line = {
                use ratatui::text::{Line as RtLine, Span as RtSpan};
                use ratatui::style::{Style, Modifier};
                let mut spans: Vec<RtSpan<'static>> = Vec::new();
                spans.push(RtSpan::styled(
                    path.display().to_string(),
                    Style::default().fg(crate::colors::text()).add_modifier(Modifier::BOLD),
                ));
                spans.push(RtSpan::raw(" "));
                spans.push(RtSpan::styled(
                    format!("+{}", total_added),
                    Style::default().fg(crate::colors::success()),
                ));
                spans.push(RtSpan::raw(" "));
                spans.push(RtSpan::styled(
                    format!("-{}", total_removed),
                    Style::default().fg(crate::colors::error()),
                ));
                RtLine::from(spans)
            };
            blocks.insert(0, DiffBlock { lines: vec![header_line] });

            // Tab title: file name only
            let title = path
                .file_name()
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| path.display().to_string());
            tabs.push((title, blocks));
        }
        if tabs.is_empty() {
            // Nothing to show — surface a small notice so Ctrl+D feels responsive
            self.bottom_pane.flash_footer_notice("No diffs recorded this session".to_string());
            return;
        }
        self.diffs.overlay = Some(DiffOverlay::new(tabs));
        self.diffs.confirm = None;
        self.request_redraw();
    }

    pub(crate) fn toggle_diffs_popup(&mut self) {
        if self.diffs.overlay.is_some() {
            self.diffs.overlay = None;
            self.request_redraw();
        } else {
            self.show_diffs_popup();
        }
    }

    pub(crate) fn handle_reasoning_command(&mut self, command_args: String) {
        // command_args contains only the arguments after the command (e.g., "high" not "/reasoning high")
        let trimmed = command_args.trim();

        if !trimmed.is_empty() {
            // User specified a level: e.g., "high"
            let new_effort = match trimmed.to_lowercase().as_str() {
                "minimal" | "min" => ReasoningEffort::Minimal,
                "low" => ReasoningEffort::Low,
                "medium" | "med" => ReasoningEffort::Medium,
                "high" => ReasoningEffort::High,
                // Backwards compatibility: map legacy values to minimal.
                "none" | "off" => ReasoningEffort::Minimal,
                _ => {
                    // Invalid parameter, show error and return
                    let message = format!(
                        "Invalid reasoning level: '{}'. Use: minimal, low, medium, or high",
                        trimmed
                    );
                    self.history_push(history_cell::new_error_event(message));
                    return;
                }
            };
            self.set_reasoning_effort(new_effort);
        } else {
            // No parameter - show interactive selection UI
            self.bottom_pane
                .show_reasoning_selection(self.config.model_reasoning_effort);
            return;
        }
    }

    pub(crate) fn handle_verbosity_command(&mut self, command_args: String) {
        // Verbosity is not supported with ChatGPT auth
        if self.config.using_chatgpt_auth {
            let message = "Text verbosity is not available when using Sign in with ChatGPT".to_string();
            self.history_push(history_cell::new_error_event(message));
            return;
        }
        
        // command_args contains only the arguments after the command (e.g., "high" not "/verbosity high")
        let trimmed = command_args.trim();

        if !trimmed.is_empty() {
            // User specified a level: e.g., "high"
            let new_verbosity = match trimmed.to_lowercase().as_str() {
                "low" => TextVerbosity::Low,
                "medium" | "med" => TextVerbosity::Medium,
                "high" => TextVerbosity::High,
                _ => {
                    // Invalid parameter, show error and return
                    let message = format!(
                        "Invalid verbosity level: '{}'. Use: low, medium, or high",
                        trimmed
                    );
                    self.history_push(history_cell::new_error_event(message));
                    return;
                }
            };

            // Update the configuration
            self.config.model_text_verbosity = new_verbosity;

            // Display success message
            let message = format!("Text verbosity set to: {}", new_verbosity);
            self.history_push(history_cell::new_background_event(message));

            // Send the update to the backend
            let op = Op::ConfigureSession {
                provider: self.config.model_provider.clone(),
                model: self.config.model.clone(),
                model_reasoning_effort: self.config.model_reasoning_effort,
                model_reasoning_summary: self.config.model_reasoning_summary,
                model_text_verbosity: self.config.model_text_verbosity,
                user_instructions: self.config.user_instructions.clone(),
                base_instructions: self.config.base_instructions.clone(),
                approval_policy: self.config.approval_policy,
                sandbox_policy: self.config.sandbox_policy.clone(),
                disable_response_storage: self.config.disable_response_storage,
                notify: self.config.notify.clone(),
                cwd: self.config.cwd.clone(),
                resume_path: None,
            };
            let _ = self.dev_op_tx.send(op);
        } else {
            // No parameter specified, show interactive UI
            self.bottom_pane
                .show_verbosity_selection(self.config.model_text_verbosity);
            return;
        }
    }

    pub(crate) fn prepare_agents(&mut self) {
        // Set the flag to show agents are ready to start
        self.agents_ready_to_start = true;

        // Initialize sparkline with some data so it shows immediately
        {
            let mut sparkline_data = self.sparkline_data.borrow_mut();
            if sparkline_data.is_empty() {
                // Add initial low activity data for preparing phase
                for _ in 0..10 {
                    sparkline_data.push((2, false));
                }
                tracing::info!(
                    "Initialized sparkline data with {} points for preparing phase",
                    sparkline_data.len()
                );
            }
        } // Drop the borrow here

        self.request_redraw();
    }

    /// Update sparkline data with randomized activity based on agent count
    fn update_sparkline_data(&self) {
        let now = std::time::Instant::now();

        // Update every 100ms for smooth animation
        if now
            .duration_since(*self.last_sparkline_update.borrow())
            .as_millis()
            < 100
        {
            return;
        }

        *self.last_sparkline_update.borrow_mut() = now;

        // Calculate base height based on number of agents and status
        let agent_count = self.active_agents.len();
        let is_planning = self.overall_task_status == "planning";
        let base_height = if agent_count == 0 && self.agents_ready_to_start {
            2 // Minimal activity when preparing
        } else if is_planning && agent_count > 0 {
            3 // Low activity during planning phase
        } else if agent_count == 1 {
            5 // Low activity for single agent
        } else if agent_count == 2 {
            10 // Medium activity for two agents
        } else if agent_count >= 3 {
            15 // High activity for multiple agents
        } else {
            0 // No activity when no agents
        };

        // Don't generate data if there's no activity
        if base_height == 0 {
            return;
        }

        // Generate random variation
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hash;
        use std::hash::Hasher;
        let mut hasher = DefaultHasher::new();
        now.elapsed().as_nanos().hash(&mut hasher);
        let random_seed = hasher.finish();

        // More variation during planning phase for visibility (+/- 50%)
        // Less variation during running for stability (+/- 30%)
        let variation_percent = if self.agents_ready_to_start && self.active_agents.is_empty() {
            50 // More variation during planning for visibility
        } else {
            30 // Standard variation during running
        };

        let variation_range = variation_percent * 2; // e.g., 100 for +/- 50%
        let variation = ((random_seed % variation_range) as i32 - variation_percent as i32)
            * base_height as i32
            / 100;
        let height = ((base_height as i32 + variation).max(1) as u64).min(20);

        // Check if any agents are completed
        let has_completed = self
            .active_agents
            .iter()
            .any(|a| matches!(a.status, AgentStatus::Completed));

        // Keep a rolling window of 60 data points (about 6 seconds at 100ms intervals)
        let mut sparkline_data = self.sparkline_data.borrow_mut();
        sparkline_data.push((height, has_completed));
        if sparkline_data.len() > 60 {
            sparkline_data.remove(0);
        }
    }

    pub(crate) fn set_reasoning_effort(&mut self, new_effort: ReasoningEffort) {
        // Update the config
        self.config.model_reasoning_effort = new_effort;

        // Send ConfigureSession op to update the backend
        let op = Op::ConfigureSession {
            provider: self.config.model_provider.clone(),
            model: self.config.model.clone(),
            model_reasoning_effort: new_effort,
            model_reasoning_summary: self.config.model_reasoning_summary,
            model_text_verbosity: self.config.model_text_verbosity,
            user_instructions: self.config.user_instructions.clone(),
            base_instructions: self.config.base_instructions.clone(),
            approval_policy: self.config.approval_policy.clone(),
            sandbox_policy: self.config.sandbox_policy.clone(),
            disable_response_storage: self.config.disable_response_storage,
            notify: self.config.notify.clone(),
            cwd: self.config.cwd.clone(),
            resume_path: None,
        };

        self.submit_op(op);

        // Add status message to history
        self.history_push(history_cell::new_reasoning_output(&new_effort));
    }

    pub(crate) fn set_text_verbosity(&mut self, new_verbosity: TextVerbosity) {
        // Update the config
        self.config.model_text_verbosity = new_verbosity;

        // Send ConfigureSession op to update the backend
        let op = Op::ConfigureSession {
            provider: self.config.model_provider.clone(),
            model: self.config.model.clone(),
            model_reasoning_effort: self.config.model_reasoning_effort,
            model_reasoning_summary: self.config.model_reasoning_summary,
            model_text_verbosity: new_verbosity,
            user_instructions: self.config.user_instructions.clone(),
            base_instructions: self.config.base_instructions.clone(),
            approval_policy: self.config.approval_policy.clone(),
            sandbox_policy: self.config.sandbox_policy.clone(),
            disable_response_storage: self.config.disable_response_storage,
            notify: self.config.notify.clone(),
            cwd: self.config.cwd.clone(),
            resume_path: None,
        };

        self.submit_op(op);

        // Add status message to history  
        let message = format!("Text verbosity set to: {}", new_verbosity);
        self.history_push(history_cell::new_background_event(message));
    }

    /// Forward file-search results to the bottom pane.
    pub(crate) fn apply_file_search_result(&mut self, query: String, matches: Vec<FileMatch>) {
        self.bottom_pane.on_file_search_result(query, matches);
    }

    pub(crate) fn show_theme_selection(&mut self) {
        self.bottom_pane
            .show_theme_selection(self.config.tui.theme.name);
    }

    // Ctrl+Y syntax cycling disabled intentionally.

    /// Show a brief debug notice in the footer.
    #[allow(dead_code)]
    pub(crate) fn debug_notice(&mut self, text: String) {
        self.bottom_pane.flash_footer_notice(text);
        self.request_redraw();
    }

    pub(crate) fn set_theme(&mut self, new_theme: dev_core::config_types::ThemeName) {
        // Update the config
        self.config.tui.theme.name = new_theme;

        // Save the theme to config file
        self.save_theme_to_config(new_theme);

        // Retint pre-rendered history cell lines to the new palette
        self.restyle_history_after_theme_change();

        // Add confirmation message to history
        let theme_name = match new_theme {
            // Light themes
            dev_core::config_types::ThemeName::LightPhoton => "Light - Photon",
            dev_core::config_types::ThemeName::LightPrismRainbow => "Light - Prism Rainbow",
            dev_core::config_types::ThemeName::LightVividTriad => "Light - Vivid Triad",
            dev_core::config_types::ThemeName::LightPorcelain => "Light - Porcelain",
            dev_core::config_types::ThemeName::LightSandbar => "Light - Sandbar",
            dev_core::config_types::ThemeName::LightGlacier => "Light - Glacier",
            // Dark themes
            dev_core::config_types::ThemeName::DarkCarbonNight => "Dark - Carbon Night",
            dev_core::config_types::ThemeName::DarkShinobiDusk => "Dark - Shinobi Dusk",
            dev_core::config_types::ThemeName::DarkOledBlackPro => "Dark - OLED Black Pro",
            dev_core::config_types::ThemeName::DarkAmberTerminal => "Dark - Amber Terminal",
            dev_core::config_types::ThemeName::DarkAuroraFlux => "Dark - Aurora Flux",
            dev_core::config_types::ThemeName::DarkCharcoalRainbow => "Dark - Charcoal Rainbow",
            dev_core::config_types::ThemeName::DarkZenGarden => "Dark - Zen Garden",
            dev_core::config_types::ThemeName::DarkPaperLightPro => "Dark - Paper Light Pro",
            dev_core::config_types::ThemeName::Custom => "Custom",
        };
        let message = format!("✓ Theme changed to {}", theme_name);
        self.history_push(history_cell::new_background_event(message));
    }

    fn restyle_history_after_theme_change(&mut self) {
        let old = self.last_theme.clone();
        let new = crate::theme::current_theme();
        if old == new { return; }

        for cell in &mut self.history_cells {
            if let Some(plain) = cell.as_any_mut().downcast_mut::<history_cell::PlainHistoryCell>() {
                history_cell::retint_lines_in_place(&mut plain.lines, &old, &new);
            } else if let Some(tool) = cell.as_any_mut().downcast_mut::<history_cell::ToolCallCell>() {
                tool.retint(&old, &new);
                
            } else if let Some(reason) = cell.as_any_mut().downcast_mut::<history_cell::CollapsibleReasoningCell>() {
                history_cell::retint_lines_in_place(&mut reason.lines, &old, &new);
            } else if let Some(stream) = cell.as_any_mut().downcast_mut::<history_cell::StreamingContentCell>() {
                stream.retint(&old, &new);
            } else if let Some(assist) = cell.as_any_mut().downcast_mut::<history_cell::AssistantMarkdownCell>() {
                // Fully rebuild from raw to apply new theme + syntax highlight
                assist.rebuild(&self.config);
            }
        }

        // Update snapshot and redraw; height caching can remain (colors don't affect wrap)
        self.last_theme = new;
        self.app_event_tx.send(AppEvent::RequestRedraw);
    }

    /// Public-facing hook for preview mode to retint existing history lines
    /// without persisting the theme or adding history events.
    pub(crate) fn retint_history_for_preview(&mut self) {
        self.restyle_history_after_theme_change();
    }

    fn save_theme_to_config(&self, new_theme: dev_core::config_types::ThemeName) {
        // Persist the theme selection to CODE_HOME/CODEX_HOME config.toml
        match dev_core::config::find_dev_home() {
            Ok(home) => {
                if let Err(e) = dev_core::config::set_tui_theme_name(&home, new_theme) {
                    tracing::warn!("Failed to persist theme to config.toml: {}", e);
                } else {
                    tracing::info!("Persisted TUI theme selection to config.toml");
                }
            }
            Err(e) => {
                tracing::warn!("Could not locate Codex home to persist theme: {}", e);
            }
        }
    }

    #[allow(dead_code)]
    pub(crate) fn on_esc(&mut self) -> bool {
        if self.bottom_pane.is_task_running() {
            self.interrupt_running_task();
            return true;
        }
        false
    }

    /// Handle Ctrl-C key press.
    /// Returns CancellationEvent::Handled if the event was consumed by the UI, or
    /// CancellationEvent::Ignored if the caller should handle it (e.g. exit).
    pub(crate) fn on_ctrl_c(&mut self) -> CancellationEvent {
        match self.bottom_pane.on_ctrl_c() {
            CancellationEvent::Handled => return CancellationEvent::Handled,
            CancellationEvent::Ignored => {}
        }
        if self.bottom_pane.is_task_running() {
            self.interrupt_running_task();
            CancellationEvent::Ignored
        } else if self.bottom_pane.ctrl_c_quit_hint_visible() {
            self.submit_op(Op::Shutdown);
            CancellationEvent::Handled
        } else {
            self.bottom_pane.show_ctrl_c_quit_hint();
            CancellationEvent::Ignored
        }
    }

    #[allow(dead_code)]
    pub(crate) fn composer_is_empty(&self) -> bool {
        self.bottom_pane.composer_is_empty()
    }

    // --- Double‑Escape helpers ---
    pub(crate) fn show_esc_backtrack_hint(&mut self) {
        self.bottom_pane
            .flash_footer_notice("Esc edit prev".to_string());
    }

    pub(crate) fn show_edit_previous_picker(&mut self) {
        use crate::bottom_pane::list_selection_view::{ListSelectionView, SelectionItem};
        // Collect recent user prompts (newest first)
        let mut items: Vec<SelectionItem> = Vec::new();
        let mut nth_counter = 0usize;
        for cell in self.history_cells.iter().rev() {
            if cell.kind() == crate::history_cell::HistoryCellType::User {
                nth_counter += 1; // 1-based index for Nth last
                let content_lines = cell.display_lines();
                if content_lines.is_empty() {
                    continue;
                }
                let full_text: String = content_lines
                    .iter()
                    .map(|l| l.spans.iter().map(|s| s.content.to_string()).collect::<String>())
                    .collect::<Vec<_>>()
                    .join("\n");

                // Build a concise name from first line
                let mut first = content_lines[0]
                    .spans
                    .iter()
                    .map(|s| s.content.as_ref())
                    .collect::<String>();
                const MAX: usize = 64;
                if first.chars().count() > MAX {
                    first = first.chars().take(MAX).collect::<String>() + "…";
                }

                let nth = nth_counter;
                let actions: Vec<crate::bottom_pane::list_selection_view::SelectionAction> = vec![
                    Box::new({
                        let text = full_text.clone();
                        move |tx: &crate::app_event_sender::AppEventSender| {
                            tx.send(crate::app_event::AppEvent::JumpBack { nth, prefill: text.clone() });
                        }
                    })
                ];

                items.push(SelectionItem {
                    name: first,
                    description: None,
                    is_current: false,
                    actions,
                });
            }
        }

        if items.is_empty() {
            self.bottom_pane
                .flash_footer_notice("No previous messages to edit".to_string());
            return;
        }

        let view: ListSelectionView = ListSelectionView::new(
            " Jump back to a previous message ".to_string(),
            Some("This will return the conversation to an earlier state".to_string()),
            Some("Esc cancel".to_string()),
            items,
            self.app_event_tx.clone(),
        );
        self.bottom_pane.show_list_selection(
            "Jump back to a previous message".to_string(),
            None,
            None,
            view,
        );
    }

    pub(crate) fn is_task_running(&self) -> bool {
        self.bottom_pane.is_task_running()
    }

    // begin_jump_back no longer used: backend fork handles it.

    pub(crate) fn undo_jump_back(&mut self) {
        if let Some(mut st) = self.pending_jump_back.take() {
            // Restore removed cells in original order
            self.history_cells.extend(st.removed_cells.drain(..));
            // Clear composer (no reliable way to restore prior text)
            self.insert_str("");
            self.request_redraw();
        }
    }

    pub(crate) fn has_pending_jump_back(&self) -> bool { self.pending_jump_back.is_some() }

    /// Clear the composer text and any pending paste placeholders/history cursors.
    pub(crate) fn clear_composer(&mut self) {
        self.bottom_pane.clear_composer();
        // Mark a height change so layout adjusts immediately if the composer shrinks.
        self.height_manager
            .borrow_mut()
            .record_event(crate::height_manager::HeightEvent::ComposerModeChange);
        self.request_redraw();
    }

    pub(crate) fn close_file_popup_if_active(&mut self) -> bool {
        self.bottom_pane.close_file_popup_if_active()
    }

    pub(crate) fn has_active_modal_view(&self) -> bool {
        self.bottom_pane.has_active_modal_view()
    }

    /// Forward an `Op` directly to codex.
    pub(crate) fn submit_op(&self, op: Op) {
        if let Err(e) = self.dev_op_tx.send(op) {
            tracing::error!("failed to submit op: {e}");
        }
    }

    pub(crate) fn insert_history_lines(&mut self, lines: Vec<ratatui::text::Line<'static>>) {
        let kind = self.stream_state.current_kind.unwrap_or(StreamKind::Answer);
        self.insert_history_lines_with_kind(kind, None, lines);
    }

    pub(crate) fn insert_history_lines_with_kind(
        &mut self,
        kind: StreamKind,
        id: Option<String>,
        mut lines: Vec<ratatui::text::Line<'static>>,
    ) {
        // No debug logging: we rely on preserving span modifiers end-to-end.
        // Insert all lines as a single streaming content cell to preserve spacing
        if lines.is_empty() { return; }

        
        if let Some(first_line) = lines.first() {
            let first_line_text: String = first_line
                .spans
                .iter()
                .map(|s| s.content.to_string())
                .collect();
            tracing::debug!("First line content: {:?}", first_line_text);
        }

        match kind {
            StreamKind::Reasoning => {
                // This reasoning block is the bottom-most; show progress indicator here only
                self.clear_reasoning_in_progress();
                // Ensure footer shows Ctrl+R hint when reasoning content is present
                self.bottom_pane.set_reasoning_hint(true);
                // Update footer label to reflect current visibility state
                self.bottom_pane.set_reasoning_state(self.is_reasoning_shown());
                // Route by id when provided to avoid splitting reasoning across cells.
                // Be defensive: the cached index may be stale after inserts/removals; validate it.
                if let Some(ref rid) = id {
                    if let Some(&idx) = self.reasoning_index.get(rid) {
                        if idx < self.history_cells.len() {
                            if let Some(reasoning_cell) = self.history_cells[idx]
                                .as_any_mut()
                                .downcast_mut::<history_cell::CollapsibleReasoningCell>()
                            {
                                tracing::debug!("Appending {} lines to Reasoning(id={})", lines.len(), rid);
                                reasoning_cell.append_lines_dedup(lines);
                                reasoning_cell.set_in_progress(true);
                                self.invalidate_height_cache();
                                self.autoscroll_if_near_bottom();
                                self.request_redraw();
                                return;
                            }
                        }
                        // Cached index was stale or wrong type — try to locate by scanning.
                        if let Some(found_idx) = self.history_cells.iter().rposition(|c| {
                            c.as_any()
                                .downcast_ref::<history_cell::CollapsibleReasoningCell>()
                                .and_then(|rc| rc.id.as_ref())
                                .map(|sid| sid == rid)
                                .unwrap_or(false)
                        }) {
                            if let Some(reasoning_cell) = self.history_cells[found_idx]
                                .as_any_mut()
                                .downcast_mut::<history_cell::CollapsibleReasoningCell>()
                            {
                                // Refresh the cache with the corrected index
                                self.reasoning_index.insert(rid.clone(), found_idx);
                                tracing::debug!("Recovered stale reasoning index; appending at {} for id={}", found_idx, rid);
                                reasoning_cell.append_lines_dedup(lines);
                                reasoning_cell.set_in_progress(true);
                                self.invalidate_height_cache();
                                self.autoscroll_if_near_bottom();
                                self.request_redraw();
                                return;
                            }
                        } else {
                            // No matching cell remains; drop the stale cache entry.
                            self.reasoning_index.remove(rid);
                        }
                    }
                }

                tracing::debug!("Creating new CollapsibleReasoningCell id={:?}", id);
                let cell = history_cell::CollapsibleReasoningCell::new_with_id(lines, id.clone());
                if self.config.tui.show_reasoning { cell.set_collapsed(false); } else { cell.set_collapsed(true); }
                cell.set_in_progress(true);

                // Use pre-seeded key for this stream id when present; otherwise synthesize.
                let key = match id.as_deref() {
                    Some(rid) => self.try_stream_order_key(kind, rid).unwrap_or_else(|| {
                        tracing::warn!("missing stream order key for Reasoning id={}; using synthetic key", rid);
                        self.next_internal_key()
                    }),
                    None => {
                        tracing::warn!("missing stream id for Reasoning; using synthetic key");
                        self.next_internal_key()
                    }
                };
                tracing::info!("[order] insert Reasoning new id={:?} {}", id, Self::debug_fmt_order_key(key));
                let idx = self.history_insert_with_key_global(Box::new(cell), key);
                if let Some(rid) = id { self.reasoning_index.insert(rid, idx); }
            }
            StreamKind::Answer => {
                tracing::debug!(
                    "history.insert Answer id={:?} incoming_lines={}",
                    id,
                    lines.len()
                );
                // Any incoming Answer means reasoning is no longer bottom-most
                self.clear_reasoning_in_progress();
                // Keep a single StreamingContentCell and append to it
                if let Some(last) = self.history_cells.last_mut() {
                    if let Some(stream_cell) = last
                        .as_any_mut()
                        .downcast_mut::<history_cell::StreamingContentCell>()
                    {
                        // If id is specified, only append when ids match
                        if let Some(ref want) = id {
                            if stream_cell.id.as_ref() != Some(want) {
                                // fall through to create/find matching cell below
                            } else {
                                tracing::debug!("history.append -> last StreamingContentCell (id match) lines+={}", lines.len());
                                // Guard against stray header sneaking into a later chunk
                                if lines.first().map(|l| {
                                    l.spans
                                        .iter()
                                        .map(|s| s.content.as_ref())
                                        .collect::<String>()
                                        .trim()
                                        .eq_ignore_ascii_case("codex")
                                }).unwrap_or(false) {
                                    if lines.len() == 1 {
                                        return;
                                    } else {
                                        lines.remove(0);
                                    }
                                }
                                stream_cell.extend_lines(lines);
                                self.invalidate_height_cache();
                                self.autoscroll_if_near_bottom();
                                self.request_redraw();
                                return;
                            }
                        } else {
                            // No id — legacy: append to last
                            tracing::debug!("history.append -> last StreamingContentCell (no id provided) lines+={}", lines.len());
                            if lines.first().map(|l| {
                                l.spans
                                    .iter()
                                    .map(|s| s.content.as_ref())
                                    .collect::<String>()
                                    .trim()
                                    .eq_ignore_ascii_case("codex")
                            }).unwrap_or(false) {
                                if lines.len() == 1 { return; } else { lines.remove(0); }
                            }
                            stream_cell.extend_lines(lines);
                            self.invalidate_height_cache();
                            self.autoscroll_if_near_bottom();
                            self.request_redraw();
                            return;
                        }
                    }
                }

                // If id is specified, try to locate an existing streaming cell with that id
                if let Some(ref want) = id {
                    if let Some(idx) = self.history_cells.iter().rposition(|c| c
                        .as_any()
                        .downcast_ref::<history_cell::StreamingContentCell>()
                        .map(|sc| sc.id.as_ref() == Some(want)).unwrap_or(false))
                    {
                        if let Some(stream_cell) = self.history_cells[idx]
                            .as_any_mut()
                            .downcast_mut::<history_cell::StreamingContentCell>()
                        {
                            tracing::debug!("history.append -> StreamingContentCell by id at idx={} lines+={}", idx, lines.len());
                            if lines.first().map(|l| {
                                l.spans.iter().map(|s| s.content.as_ref()).collect::<String>().trim().eq_ignore_ascii_case("codex")
                            }).unwrap_or(false) {
                                if lines.len() == 1 { return; } else { lines.remove(0); }
                            }
                            stream_cell.extend_lines(lines);
                            self.invalidate_height_cache();
                            self.autoscroll_if_near_bottom();
                            self.request_redraw();
                            return;
                        }
                    }
                }
                
                // Ensure a hidden 'codex' header is present
                let has_header = lines.first().map(|l| {
                    l.spans
                        .iter()
                        .map(|s| s.content.as_ref())
                        .collect::<String>()
                        .trim()
                        .eq_ignore_ascii_case("codex")
                }).unwrap_or(false);
                if !has_header {
                    let mut with_header: Vec<ratatui::text::Line<'static>> =
                        Vec::with_capacity(lines.len() + 1);
                    with_header.push(ratatui::text::Line::from("codex"));
                    with_header.extend(lines);
                    lines = with_header;
                }
                // Use pre-seeded key for this stream id when present; otherwise synthesize.
                let key = match id.as_deref() {
                    Some(rid) => self.try_stream_order_key(kind, rid).unwrap_or_else(|| {
                        tracing::warn!("missing stream order key for Answer id={}; using synthetic key", rid);
                        self.next_internal_key()
                    }),
                    None => {
                        tracing::warn!("missing stream id for Answer; using synthetic key");
                        self.next_internal_key()
                    }
                };
                tracing::info!("[order] insert Answer new id={:?} {}", id, Self::debug_fmt_order_key(key));
                let new_idx = self.history_insert_with_key_global(
                    Box::new(history_cell::new_streaming_content_with_id(id.clone(), lines)),
                    key,
                );
        tracing::debug!("history.new StreamingContentCell at idx={} id={:?}", new_idx, id);
            }
        }

        // Auto-follow if near bottom so new inserts are visible
        self.autoscroll_if_near_bottom();
        self.request_redraw();
    }

    /// Replace the in-progress streaming assistant cell with a final markdown cell that
    /// stores raw markdown for future re-rendering.
    pub(crate) fn insert_final_answer_with_id(
        &mut self,
        id: Option<String>,
        lines: Vec<ratatui::text::Line<'static>>,
        source: String,
    ) {
        tracing::debug!("insert_final_answer_with_id id={:?} source_len={} lines={}", id, source.len(), lines.len());
        tracing::info!("[order] final Answer id={:?}", id);
        // Debug: list last few history cell kinds so we can see what's present
        let tail_kinds: String = self
            .history_cells
            .iter()
            .rev()
            .take(5)
            .map(|c| {
                if c.as_any().downcast_ref::<history_cell::StreamingContentCell>().is_some() {
                    "Streaming".to_string()
                } else if c.as_any().downcast_ref::<history_cell::AssistantMarkdownCell>().is_some() {
                    "AssistantFinal".to_string()
                } else if c.as_any().downcast_ref::<history_cell::CollapsibleReasoningCell>().is_some() {
                    "Reasoning".to_string()
                } else {
                    format!("{:?}", c.kind())
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        tracing::debug!("history.tail kinds(last5) = [{}]", tail_kinds);

        // When we have an id but could not find a streaming cell by id, dump ids
        if id.is_some() {
            let ids: Vec<String> = self.history_cells.iter().enumerate().filter_map(|(i,c)| {
                c.as_any().downcast_ref::<history_cell::StreamingContentCell>().and_then(|sc| {
                    sc.id.as_ref().map(|s| format!("{}:{}", i, s))
                })
            }).collect();
            tracing::debug!("history.streaming ids={}", ids.join(" | "));
        }
        // If we already finalized this id in the current turn with identical content,
        // drop this event to avoid duplicates (belt-and-suspenders against upstream repeats).
        if let Some(ref want) = id {
            if self.stream_state.closed_answer_ids.contains(&StreamId(want.clone())) {
                if let Some(existing_idx) = self
                    .history_cells
                    .iter()
                    .rposition(|c| c
                        .as_any()
                        .downcast_ref::<history_cell::AssistantMarkdownCell>()
                        .map(|amc| amc.id.as_ref() == Some(want))
                        .unwrap_or(false))
                {
                    if let Some(amc) = self.history_cells[existing_idx]
                        .as_any()
                        .downcast_ref::<history_cell::AssistantMarkdownCell>()
                    {
                        let prev = Self::normalize_text(&amc.raw);
                        let newn = Self::normalize_text(&source);
                        if prev == newn {
                            tracing::debug!("InsertFinalAnswer: dropping duplicate final for id={}", want);
                            return;
                        }
                    }
                }
            }
        }
        // Ensure a hidden 'codex' header is present
        let has_header = lines.first().map(|l| {
            l.spans.iter().map(|s| s.content.as_ref()).collect::<String>().trim().eq_ignore_ascii_case("codex")
        }).unwrap_or(false);
        if !has_header {
            // No need to mutate `lines` further since we rebuild from `source` below.
        }

        // Replace the matching StreamingContentCell if one exists for this id; else fallback to most recent.
        // NOTE (dup‑guard): This relies on `StreamingContentCell::as_any()` returning `self`.
        // If that impl is removed, downcast_ref will fail and we won't find the streaming cell,
        // causing the final to append a new Assistant cell (duplicate).
        let streaming_idx = if let Some(ref want) = id {
            // Only replace a streaming cell if its id matches this final.
            self.history_cells
                .iter()
                .rposition(|c| {
                    if let Some(sc) = c.as_any().downcast_ref::<history_cell::StreamingContentCell>() {
                        sc.id.as_ref() == Some(want)
                    } else {
                        false
                    }
                })
        } else {
            None
        };
        if let Some(idx) = streaming_idx {
            tracing::debug!("final-answer: replacing StreamingContentCell at idx={} by id match", idx);
            // Replace the matching streaming cell in-place, preserving the id
            let cell = history_cell::AssistantMarkdownCell::new_with_id(source, id.clone(), &self.config);
            self.history_replace_at(idx, Box::new(cell));
            self.autoscroll_if_near_bottom();
            return;
        }

        // No streaming cell found. First, try to replace a finalized assistant cell
        // that was created for the same stream id (e.g., we already finalized due to
        // a lifecycle event and this InsertFinalAnswer arrived slightly later).
        if let Some(ref want) = id {
            if let Some(idx) = self
                .history_cells
                .iter()
                .rposition(|c| {
                    if let Some(amc) = c.as_any().downcast_ref::<history_cell::AssistantMarkdownCell>() {
                        amc.id.as_ref() == Some(want)
                    } else {
                        false
                    }
                })
            {
                tracing::debug!("final-answer: replacing existing AssistantMarkdownCell at idx={} by id match", idx);
                let cell = history_cell::AssistantMarkdownCell::new_with_id(source, id.clone(), &self.config);
                self.history_replace_at(idx, Box::new(cell));
                self.autoscroll_if_near_bottom();
                return;
            }
        }

        // Otherwise, if a finalized assistant cell exists at the tail,
        // replace it in place to avoid duplicate assistant messages when a second
        // InsertFinalAnswer (e.g., from an AgentMessage event) arrives after we already
        // finalized due to a side event.
        if let Some(idx) = self
            .history_cells
            .iter()
            .rposition(|c| c.as_any().downcast_ref::<history_cell::AssistantMarkdownCell>().is_some())
        {
            // Replace the tail finalized assistant cell if the new content is identical OR
            // a superset revision of the previous content (common provider behavior where
            // a later final slightly extends the earlier one). Otherwise append a new
            // assistant message so distinct messages remain separate.
            let (should_replace, _prev_len, _new_len) = self.history_cells[idx]
                .as_any()
                .downcast_ref::<history_cell::AssistantMarkdownCell>()
                .map(|amc| {
                    let prev = Self::normalize_text(&amc.raw);
                    let newn = Self::normalize_text(&source);
                    let identical = prev == newn;
                    let is_superset = !identical && newn.contains(&prev);
                    // Heuristic: treat as revision when previous is reasonably long to
                    // avoid collapsing very short replies unintentionally.
                    let long_enough = prev.len() >= 80;
                    (identical || (is_superset && long_enough), prev.len(), newn.len())
                })
                .unwrap_or((false, 0, 0));
            if should_replace {
                tracing::debug!("final-answer: replacing tail AssistantMarkdownCell via heuristic identical/superset");
                let cell = history_cell::AssistantMarkdownCell::new_with_id(source, id.clone(), &self.config);
                self.history_replace_at(idx, Box::new(cell));
                self.autoscroll_if_near_bottom();
                return;
            }
        }

        // Fallback: no prior assistant cell found; insert at stable sequence position.
        tracing::debug!("final-answer: ordered insert new AssistantMarkdownCell id={:?}", id);
        let key = match id.as_deref() {
            Some(rid) => self.try_stream_order_key(StreamKind::Answer, rid).unwrap_or_else(|| {
                tracing::warn!("missing stream order key for final Answer id={}; using synthetic key", rid);
                self.next_internal_key()
            }),
            None => {
                tracing::warn!("missing stream id for final Answer; using synthetic key");
                self.next_internal_key()
            }
        };
        tracing::info!("[order] final Answer ordered insert id={:?} {}", id, Self::debug_fmt_order_key(key));
        let cell = history_cell::AssistantMarkdownCell::new_with_id(source, id, &self.config);
        let _ = self.history_insert_with_key_global(Box::new(cell), key);
    }

    // Assign or fetch a stable sequence for a stream kind+id within its originating turn
    // removed legacy ensure_stream_order_key; strict variant is used instead

    /// Normalize text for duplicate detection (trim trailing whitespace and normalize newlines)
    fn normalize_text(s: &str) -> String {
        // 1) Normalize newlines
        let s = s.replace("\r\n", "\n");
        // 2) Trim trailing whitespace per line; collapse repeated blank lines
        let mut out: Vec<String> = Vec::new();
        let mut saw_blank = false;
        for line in s.lines() {
            // Replace common Unicode bullets with ASCII to stabilize equality checks
            let line = line
                .replace('\u{2022}', "-") // •
                .replace('\u{25E6}', "-") // ◦
                .replace('\u{2219}', "-"); // ∙
            let trimmed = line.trim_end();
            if trimmed.chars().all(|c| c.is_whitespace()) {
                if !saw_blank { out.push(String::new()); }
                saw_blank = true;
            } else {
                out.push(trimmed.to_string());
                saw_blank = false;
            }
        }
        // 3) Remove trailing blank lines
        while out.last().is_some_and(|l| l.is_empty()) { out.pop(); }
        out.join("\n")
    }

    pub(crate) fn toggle_reasoning_visibility(&mut self) {
        // Track whether any reasoning cells are found and their new state
        let mut has_reasoning_cells = false;
        let mut new_collapsed_state = false;
        
        // Toggle all CollapsibleReasoningCell instances in history
        for cell in &self.history_cells {
            // Try to downcast to CollapsibleReasoningCell
            if let Some(reasoning_cell) = cell.as_any().downcast_ref::<history_cell::CollapsibleReasoningCell>() {
                reasoning_cell.toggle_collapsed();
                has_reasoning_cells = true;
                new_collapsed_state = reasoning_cell.is_collapsed();
            }
        }
        
        // Update the config to reflect the current state (inverted because collapsed means hidden)
        if has_reasoning_cells {
            self.config.tui.show_reasoning = !new_collapsed_state;
            // Brief status to confirm the toggle to the user
            let status = if self.config.tui.show_reasoning { "Reasoning shown" } else { "Reasoning hidden" };
            self.bottom_pane.update_status_text(status.to_string());
            // Update footer label to reflect current state
            self.bottom_pane.set_reasoning_state(self.config.tui.show_reasoning);
        } else {
            // No reasoning cells exist; inform the user
            self.bottom_pane.update_status_text("No reasoning to toggle".to_string());
        }
        // Collapsed state changes affect heights; clear cache
        self.invalidate_height_cache();
        self.request_redraw();
    }
    
    pub(crate) fn is_reasoning_shown(&self) -> bool {
        // Check if any reasoning cell exists and if it's expanded
        for cell in &self.history_cells {
            if let Some(reasoning_cell) = cell.as_any().downcast_ref::<history_cell::CollapsibleReasoningCell>() {
                return !reasoning_cell.is_collapsed();
            }
        }
        // If no reasoning cells exist, return the config default
        self.config.tui.show_reasoning
    }
    
    pub(crate) fn show_chrome_options(&mut self, port: Option<u16>) {
        self.bottom_pane.show_chrome_selection(port);
    }

    pub(crate) fn handle_chrome_launch_option(
        &mut self,
        option: crate::bottom_pane::chrome_selection_view::ChromeLaunchOption,
        port: Option<u16>,
    ) {
        use crate::bottom_pane::chrome_selection_view::ChromeLaunchOption;

        let launch_port = port.unwrap_or(9222);

        match option {
            ChromeLaunchOption::CloseAndUseProfile => {
                // Kill existing Chrome and launch with user profile
                #[cfg(target_os = "macos")]
                {
                    let _ = std::process::Command::new("pkill")
                        .arg("-f")
                        .arg("Google Chrome")
                        .output();
                    std::thread::sleep(std::time::Duration::from_millis(500));
                }
                #[cfg(target_os = "linux")]
                {
                    let _ = std::process::Command::new("pkill")
                        .arg("-f")
                        .arg("chrome")
                        .output();
                    std::thread::sleep(std::time::Duration::from_millis(500));
                }
                #[cfg(target_os = "windows")]
                {
                    let _ = std::process::Command::new("taskkill")
                        .arg("/F")
                        .arg("/IM")
                        .arg("chrome.exe")
                        .output();
                    std::thread::sleep(std::time::Duration::from_millis(500));
                }
                self.launch_chrome_with_profile(launch_port);
                // Connect to Chrome after launching
                self.connect_to_chrome_after_launch(launch_port);
            }
            ChromeLaunchOption::UseTempProfile => {
                // Launch with temporary profile
                self.launch_chrome_with_temp_profile(launch_port);
                // Connect to Chrome after launching
                self.connect_to_chrome_after_launch(launch_port);
            }
            ChromeLaunchOption::UseInternalBrowser => {
                // Redirect to internal browser command
                self.handle_browser_command(String::new());
            }
            ChromeLaunchOption::Cancel => {
                // Do nothing, just close the dialog
            }
        }
    }

    fn launch_chrome_with_profile(&mut self, port: u16) {
        use ratatui::text::Line;
        use std::process::Stdio;

        #[cfg(target_os = "macos")]
        {
            let log_path = format!("{}/code-chrome.log", std::env::temp_dir().display());
            let mut cmd = std::process::Command::new(
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
            );
            cmd.arg(format!("--remote-debugging-port={}", port))
                .arg("--no-first-run")
                .arg("--no-default-browser-check")
                .arg("--disable-component-extensions-with-background-pages")
                .arg("--disable-background-networking")
                .arg("--silent-debugger-extension-api")
                .arg("--remote-allow-origins=*")
                .arg("--disable-features=ChromeWhatsNewUI,TriggerFirstRunUI")
                .arg("--disable-hang-monitor")
                .arg("--disable-background-timer-throttling")
                .arg("--enable-logging")
                .arg("--log-level=1")
                .arg(format!("--log-file={}", log_path))
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .stdin(Stdio::null());
            let _ = cmd.spawn();
        }

        #[cfg(target_os = "linux")]
        {
            let log_path = format!("{}/code-chrome.log", std::env::temp_dir().display());
            let mut cmd = std::process::Command::new("google-chrome");
            cmd.arg(format!("--remote-debugging-port={}", port))
                .arg("--no-first-run")
                .arg("--no-default-browser-check")
                .arg("--disable-component-extensions-with-background-pages")
                .arg("--disable-background-networking")
                .arg("--silent-debugger-extension-api")
                .arg("--remote-allow-origins=*")
                .arg("--disable-features=ChromeWhatsNewUI,TriggerFirstRunUI")
                .arg("--disable-hang-monitor")
                .arg("--disable-background-timer-throttling")
                .arg("--enable-logging")
                .arg("--log-level=1")
                .arg(format!("--log-file={}", log_path))
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .stdin(Stdio::null());
            let _ = cmd.spawn();
        }

        #[cfg(target_os = "windows")]
        {
            let log_path = format!("{}\\code-chrome.log", std::env::temp_dir().display());
            let chrome_paths = vec![
                "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe".to_string(),
                "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe".to_string(),
                format!(
                    "{}\\AppData\\Local\\Google\\Chrome\\Application\\chrome.exe",
                    std::env::var("USERPROFILE").unwrap_or_default()
                ),
            ];

            for chrome_path in chrome_paths {
                if std::path::Path::new(&chrome_path).exists() {
                    let mut cmd = std::process::Command::new(&chrome_path);
                    cmd.arg(format!("--remote-debugging-port={}", port))
                        .arg("--no-first-run")
                        .arg("--no-default-browser-check")
                        .arg("--disable-component-extensions-with-background-pages")
                        .arg("--disable-background-networking")
                        .arg("--silent-debugger-extension-api")
                        .arg("--remote-allow-origins=*")
                        .arg("--disable-features=ChromeWhatsNewUI,TriggerFirstRunUI")
                        .arg("--disable-hang-monitor")
                        .arg("--disable-background-timer-throttling")
                        .arg("--enable-logging")
                        .arg("--log-level=1")
                        .arg(format!("--log-file={}", log_path))
                        .stdout(Stdio::null())
                        .stderr(Stdio::null())
                        .stdin(Stdio::null());
                    let _ = cmd.spawn();
                    break;
                }
            }
        }

        // Add status message
        self.history_push(history_cell::PlainHistoryCell { 
            lines: vec![Line::from("✅ Chrome launched with user profile")],
            kind: history_cell::HistoryCellType::BackgroundEvent,
        });
        // Show browsing state in input border after launch
        self.bottom_pane.update_status_text("using browser".to_string());
    }

    fn connect_to_chrome_after_launch(&mut self, port: u16) {
        // Wait a moment for Chrome to start, then reuse the existing connection logic
        let app_event_tx = self.app_event_tx.clone();
        let latest_screenshot = self.latest_browser_screenshot.clone();

        tokio::spawn(async move {
            // Wait for Chrome to fully start
            tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

            // Now try to connect using the shared CDP connection logic
            ChatWidget::connect_to_cdp_chrome(None, Some(port), latest_screenshot, app_event_tx).await;
        });
    }

    /// Shared CDP connection logic used by both /chrome command and Chrome launch options
    async fn connect_to_cdp_chrome(
        host: Option<String>,
        port: Option<u16>,
        latest_screenshot: Arc<Mutex<Option<(PathBuf, String)>>>,
        app_event_tx: AppEventSender,
    ) {
        tracing::info!("[cdp] connect_to_cdp_chrome() begin, host={:?}, port={:?}", host, port);
        let browser_manager = ChatWidget::get_browser_manager().await;
        browser_manager.set_enabled_sync(true);

        // Configure for CDP connection (prefer cached ws/port on auto-detect)
        // Track whether we're attempting via cached WS and retain a cached port for fallback.
        let mut attempted_via_cached_ws = false;
        let mut cached_port_for_fallback: Option<u16> = None;
        {
            let mut config = browser_manager.config.write().await;
            config.headless = false;
            config.persist_profile = true;
            config.enabled = true;

            if let Some(p) = port {
                config.connect_ws = None;
                config.connect_host = host.clone();
                config.connect_port = Some(p);
            } else {
                // Load persisted cache from disk (if any), then fall back to in-memory
                let (cached_port, cached_ws) = match read_cached_connection().await {
                    Some(v) => v,
                    None => dev_browser::global::get_last_connection().await,
                };
                cached_port_for_fallback = cached_port;
                if let Some(ws) = cached_ws {
                    tracing::info!("[cdp] using cached Chrome WS endpoint");
                    attempted_via_cached_ws = true;
                    config.connect_ws = Some(ws);
                    config.connect_port = None;
                } else if let Some(p) = cached_port_for_fallback {
                    tracing::info!("[cdp] using cached Chrome debug port: {}", p);
                    config.connect_ws = None;
                    config.connect_host = host.clone();
                    config.connect_port = Some(p);
                } else {
                    config.connect_ws = None;
                    config.connect_host = host.clone();
                    config.connect_port = Some(0); // auto-detect
                }
            }
        }

        // Try to connect to existing Chrome (no fallback to internal browser) with timeout
        tracing::info!("[cdp] calling BrowserManager::connect_to_chrome_only()…");
        // Allow 15s for WS discovery + 5s for connect
        let connect_deadline = tokio::time::Duration::from_secs(20);
        let connect_result = tokio::time::timeout(connect_deadline, browser_manager.connect_to_chrome_only()).await;
        match connect_result {
            Err(_) => {
                tracing::error!("[cdp] connect_to_chrome_only timed out after {:?}", connect_deadline);
                use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                let _ = app_event_tx.send(AppEvent::CodexEvent(Event {
                    id: uuid::Uuid::new_v4().to_string(),
                    event_seq: 0,
                    msg: EventMsg::BackgroundEvent(BackgroundEventEvent {
                        message: format!(
                            "❌ CDP connect timed out after {}s. Ensure Chrome is running with --remote-debugging-port={} and http://127.0.0.1:{}/json/version is reachable",
                            connect_deadline.as_secs(), port.unwrap_or(0), port.unwrap_or(0)
                        ),
                    }),
                    order: None,
                }));
                // Offer launch options popup to help recover quickly
                app_event_tx.send(AppEvent::ShowChromeOptions(port));
                return;
            }
            Ok(result) => match result {
                Ok(_) => {
                    tracing::info!("[cdp] Connected to Chrome via CDP");

                    // Build a detailed success message including CDP port and current URL when available
                    let (detected_port, detected_ws) = dev_browser::global::get_last_connection().await;
                    // Prefer explicit port; otherwise try to parse from ws URL
                    let mut port_num: Option<u16> = detected_port;
                    if port_num.is_none() {
                        if let Some(ws) = &detected_ws {
                            // crude parse: ws://host:port/...
                            if let Some(after_scheme) = ws.split("//").nth(1) {
                                if let Some(hostport) = after_scheme.split('/').next() {
                                    if let Some(pstr) = hostport.split(':').nth(1) {
                                        if let Ok(p) = pstr.parse::<u16>() { port_num = Some(p); }
                                    }
                                }
                            }
                        }
                    }

                    // Try to capture current page URL (best-effort)
                    let current_url = browser_manager.get_current_url().await;

                    let success_msg = match (port_num, current_url) {
                        (Some(p), Some(url)) if !url.is_empty() => {
                            format!("✅ Connected to Chrome via CDP (port {}) to {}", p, url)
                        }
                        (Some(p), _) => format!("✅ Connected to Chrome via CDP (port {})", p),
                        (None, Some(url)) if !url.is_empty() => {
                            format!("✅ Connected to Chrome via CDP to {}", url)
                        }
                        _ => "✅ Connected to Chrome via CDP".to_string(),
                    };

                    // Immediately notify success (do not block on screenshots)
                    use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                    let _ = app_event_tx.send(AppEvent::CodexEvent(Event {
                        id: uuid::Uuid::new_v4().to_string(),
                        event_seq: 0,
                        msg: EventMsg::BackgroundEvent(BackgroundEventEvent {
                            message: success_msg.clone(),
                        }),
                        order: None,
                    }));

                    // Persist last connection cache to disk (best-effort)
                    tokio::spawn(async move {
                        let (p, ws) = dev_browser::global::get_last_connection().await;
                        let _ = write_cached_connection(p, ws).await;
                    });

                    // Set up navigation callback
                    let latest_screenshot_callback = latest_screenshot.clone();
                    let app_event_tx_callback = app_event_tx.clone();

                    browser_manager
                        .set_navigation_callback(move |url| {
                            tracing::info!("CDP Navigation callback triggered for URL: {}", url);
                            let latest_screenshot_inner = latest_screenshot_callback.clone();
                            let app_event_tx_inner = app_event_tx_callback.clone();
                            let url_inner = url.clone();

                            tokio::spawn(async move {
                                tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
                                let browser_manager_inner = ChatWidget::get_browser_manager().await;
                                let mut attempt = 0;
                                let max_attempts = 2;
                                loop {
                                    attempt += 1;
                                    match browser_manager_inner.capture_screenshot_with_url().await {
                                        Ok((paths, _)) => {
                                            if let Some(first_path) = paths.first() {
                                                tracing::info!("[cdp] auto-captured screenshot: {}", first_path.display());

                                            if let Ok(mut latest) = latest_screenshot_inner.lock() {
                                                *latest = Some((first_path.clone(), url_inner.clone()));
                                            }

                                            use dev_core::protocol::{
                                                BrowserScreenshotUpdateEvent, Event, EventMsg,
                                            };
                                            let _ = app_event_tx_inner.send(AppEvent::CodexEvent(Event {
                                                id: uuid::Uuid::new_v4().to_string(),
                                                event_seq: 0,
                                                msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                    screenshot_path: first_path.clone(),
                                                    url: url_inner,
                                                }),
                                                order: None,
                                            }));
                                            break;
                                        }
                                    }
                                    Err(e) => {
                                        tracing::warn!("[cdp] auto-capture failed (attempt {}): {}", attempt, e);
                                        if attempt >= max_attempts { break; }
                                        tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
                                        continue;
                                    }
                                }
                                // end match
                                }
                                // end loop
                            });
                        })
                        .await;

                    // Set as global manager
                    dev_browser::global::set_global_browser_manager(browser_manager.clone()).await;

                    // Capture initial screenshot in background (don't block connect feedback)
                    {
                        let latest_screenshot_bg = latest_screenshot.clone();
                        let app_event_tx_bg = app_event_tx.clone();
                        tokio::spawn(async move {
                            tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
                            let browser_manager = ChatWidget::get_browser_manager().await;
                            let mut attempt = 0;
                            let max_attempts = 2;
                            loop {
                                attempt += 1;
                                match browser_manager.capture_screenshot_with_url().await {
                                    Ok((paths, url)) => {
                                        if let Some(first_path) = paths.first() {
                                            tracing::info!("Initial CDP screenshot captured: {}", first_path.display());
                                            if let Ok(mut latest) = latest_screenshot_bg.lock() {
                                                *latest = Some((
                                                    first_path.clone(),
                                                    url.clone().unwrap_or_else(|| "Chrome".to_string()),
                                                ));
                                            }
                                            use dev_core::protocol::{BrowserScreenshotUpdateEvent, Event, EventMsg};
                                            let _ = app_event_tx_bg.send(AppEvent::CodexEvent(Event {
                                                id: uuid::Uuid::new_v4().to_string(),
                                                event_seq: 0,
                                                msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                    screenshot_path: first_path.clone(),
                                                    url: url.unwrap_or_else(|| "Chrome".to_string()),
                                                }),
                                                order: None,
                                            }));
                                            break;
                                        }
                                    }
                                    Err(e) => {
                                        tracing::warn!("Failed to capture initial CDP screenshot (attempt {}): {}", attempt, e);
                                        if attempt >= max_attempts { break; }
                                        tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
                                    }
                                }
                            }
                        });
                    }
                }
                Err(e) => {
                    let err_msg = format!("{}", e);
                    // If we attempted via a cached WS, clear it and fallback to port-based discovery once.
                    if attempted_via_cached_ws {
                        tracing::warn!("[cdp] cached WS connect failed: {} — clearing WS cache and retrying via port discovery", err_msg);
                        let port_to_keep = cached_port_for_fallback;
                        // Clear WS in-memory and on-disk
                        dev_browser::global::set_last_connection(port_to_keep, None).await;
                        let _ = write_cached_connection(port_to_keep, None).await;

                        // Reconfigure to use port (prefer cached port, else auto-detect)
                        {
                            let mut cfg = browser_manager.config.write().await;
                            cfg.connect_ws = None;
                            cfg.connect_port = Some(port_to_keep.unwrap_or(0));
                        }

                        tracing::info!("[cdp] retrying connect via port discovery after WS failure…");
                        let retry_deadline = tokio::time::Duration::from_secs(20);
                        let retry = tokio::time::timeout(retry_deadline, browser_manager.connect_to_chrome_only()).await;
                        match retry {
                            Ok(Ok(_)) => {
                                tracing::info!("[cdp] Fallback connect succeeded after clearing cached WS");
                                // Emit success event and set up callbacks, mirroring the success path above
                                let (detected_port, detected_ws) = dev_browser::global::get_last_connection().await;
                                let mut port_num: Option<u16> = detected_port;
                                if port_num.is_none() {
                                    if let Some(ws) = &detected_ws {
                                        if let Some(after_scheme) = ws.split("//").nth(1) {
                                            if let Some(hostport) = after_scheme.split('/').next() {
                                                if let Some(pstr) = hostport.split(':').nth(1) {
                                                    if let Ok(p) = pstr.parse::<u16>() { port_num = Some(p); }
                                                }
                                            }
                                        }
                                    }
                                }
                                let current_url = browser_manager.get_current_url().await;
                                let success_msg = match (port_num, current_url) {
                                    (Some(p), Some(url)) if !url.is_empty() => {
                                        format!("✅ Connected to Chrome via CDP (port {}) to {}", p, url)
                                    }
                                    (Some(p), _) => format!("✅ Connected to Chrome via CDP (port {})", p),
                                    (None, Some(url)) if !url.is_empty() => {
                                        format!("✅ Connected to Chrome via CDP to {}", url)
                                    }
                                    _ => "✅ Connected to Chrome via CDP".to_string(),
                                };
                                use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                                let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent { message: success_msg }), order: None }));

                                // Persist last connection cache
                                tokio::spawn(async move {
                                    let (p, ws) = dev_browser::global::get_last_connection().await;
                                    let _ = write_cached_connection(p, ws).await;
                                });

                                // Navigation callback
                                let latest_screenshot_callback = latest_screenshot.clone();
                                let app_event_tx_callback = app_event_tx.clone();
                                browser_manager
                                    .set_navigation_callback(move |url| {
                                        tracing::info!("CDP Navigation callback triggered for URL: {}", url);
                                        let latest_screenshot_inner = latest_screenshot_callback.clone();
                                        let app_event_tx_inner = app_event_tx_callback.clone();
                                        let url_inner = url.clone();
                                        tokio::spawn(async move {
                                            tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
                                            let browser_manager_inner = ChatWidget::get_browser_manager().await;
                                            let mut attempt = 0;
                                            let max_attempts = 2;
                                            loop {
                                                attempt += 1;
                                                match browser_manager_inner.capture_screenshot_with_url().await {
                                                    Ok((paths, _)) => {
                                                        if let Some(first_path) = paths.first() {
                                                            tracing::info!("[cdp] auto-captured screenshot: {}", first_path.display());
                                                            if let Ok(mut latest) = latest_screenshot_inner.lock() {
                                                                *latest = Some((first_path.clone(), url_inner.clone()));
                                                            }
                                                            use dev_core::protocol::{BrowserScreenshotUpdateEvent, Event, EventMsg};
                                                            let _ = app_event_tx_inner.send(AppEvent::CodexEvent(Event {
                                                                id: uuid::Uuid::new_v4().to_string(),
                                                                event_seq: 0,
                                                                msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                                    screenshot_path: first_path.clone(),
                                                                    url: url_inner,
                                                                }),
                                                                order: None,
                                                            }));
                                                            break;
                                                        }
                                                    }
                                                    Err(e) => {
                                                        tracing::warn!("[cdp] auto-capture failed (attempt {}): {}", attempt, e);
                                                        if attempt >= max_attempts { break; }
                                                        tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
                                                    }
                                                }
                                            }
                                        });
                                    })
                                    .await;
                                // Set as global manager like success path
                                dev_browser::global::set_global_browser_manager(browser_manager.clone()).await;

                                // Initial screenshot in background (best-effort)
                                {
                                    let latest_screenshot_bg = latest_screenshot.clone();
                                    let app_event_tx_bg = app_event_tx.clone();
                                    tokio::spawn(async move {
                                        tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
                                        let browser_manager = ChatWidget::get_browser_manager().await;
                                        let mut attempt = 0;
                                        let max_attempts = 2;
                                        loop {
                                            attempt += 1;
                                            match browser_manager.capture_screenshot_with_url().await {
                                                Ok((paths, url)) => {
                                                    if let Some(first_path) = paths.first() {
                                                        tracing::info!("Initial CDP screenshot captured: {}", first_path.display());
                                                        if let Ok(mut latest) = latest_screenshot_bg.lock() {
                                                            *latest = Some((
                                                                first_path.clone(),
                                                                url.clone().unwrap_or_else(|| "Chrome".to_string()),
                                                            ));
                                                        }
                                                        use dev_core::protocol::{BrowserScreenshotUpdateEvent, Event, EventMsg};
                                                        let _ = app_event_tx_bg.send(AppEvent::CodexEvent(Event {
                                                            id: uuid::Uuid::new_v4().to_string(),
                                                            event_seq: 0,
                                                            msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                                screenshot_path: first_path.clone(),
                                                                url: url.unwrap_or_else(|| "Chrome".to_string()),
                                                            }),
                                                            order: None,
                                                        }));
                                                        break;
                                                    }
                                                }
                                                Err(e) => {
                                                    tracing::warn!("Failed to capture initial CDP screenshot (attempt {}): {}", attempt, e);
                                                    if attempt >= max_attempts { break; }
                                                    tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
                                                }
                                            }
                                        }
                                    });
                                }
                                return;
                            }
                            Ok(Err(e2)) => {
                                tracing::error!("[cdp] Fallback connect failed: {}", e2);
                                use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                                let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent {
                                        message: format!(
                                            "❌ Failed to connect to Chrome after WS fallback: {} (original: {})",
                                            e2, err_msg
                                        ),
                                    }), order: None }));
                                // Also surface the Chrome launch options UI to assist the user
                                app_event_tx.send(AppEvent::ShowChromeOptions(port));
                                return;
                            }
                            Err(_) => {
                                tracing::error!("[cdp] Fallback connect timed out after {:?}", retry_deadline);
                                use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                                let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent {
                                        message: format!(
                                            "❌ CDP connect timed out after {}s during fallback. Ensure Chrome is running with --remote-debugging-port and /json/version is reachable",
                                            retry_deadline.as_secs()
                                        ),
                                    }), order: None }));
                                // Also surface the Chrome launch options UI to assist the user
                                app_event_tx.send(AppEvent::ShowChromeOptions(port));
                                return;
                            }
                        }
                    } else {
                        tracing::error!("[cdp] connect_to_chrome_only failed immediately: {}", err_msg);
                        use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                        let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent { message: format!("❌ Failed to connect to Chrome: {}", err_msg) }), order: None }));
                        // Offer launch options popup to help recover quickly
                        app_event_tx.send(AppEvent::ShowChromeOptions(port));
                        return;
                    }
                }
            }
        }
    }

    fn launch_chrome_with_temp_profile(&mut self, port: u16) {
        use ratatui::text::Line;
        use std::process::Stdio;

        let temp_dir = std::env::temp_dir();
        let profile_dir = temp_dir.join(format!("code-chrome-temp-{}", port));

        #[cfg(target_os = "macos")]
        {
            let log_path = format!("{}/code-chrome.log", std::env::temp_dir().display());
            let mut cmd = std::process::Command::new(
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
            );
            cmd.arg(format!("--remote-debugging-port={}", port))
                .arg(format!("--user-data-dir={}", profile_dir.display()))
                .arg("--no-first-run")
                .arg("--no-default-browser-check")
                .arg("--disable-component-extensions-with-background-pages")
                .arg("--disable-background-networking")
                .arg("--silent-debugger-extension-api")
                .arg("--remote-allow-origins=*")
                .arg("--disable-features=ChromeWhatsNewUI,TriggerFirstRunUI")
                .arg("--disable-hang-monitor")
                .arg("--disable-background-timer-throttling")
                .arg("--enable-logging")
                .arg("--log-level=1")
                .arg(format!("--log-file={}", log_path))
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .stdin(Stdio::null());
            let _ = cmd.spawn();
        }

        #[cfg(target_os = "linux")]
        {
            let log_path = format!("{}/code-chrome.log", std::env::temp_dir().display());
            let mut cmd = std::process::Command::new("google-chrome");
            cmd.arg(format!("--remote-debugging-port={}", port))
                .arg(format!("--user-data-dir={}", profile_dir.display()))
                .arg("--no-first-run")
                .arg("--no-default-browser-check")
                .arg("--disable-component-extensions-with-background-pages")
                .arg("--disable-background-networking")
                .arg("--silent-debugger-extension-api")
                .arg("--remote-allow-origins=*")
                .arg("--disable-features=ChromeWhatsNewUI,TriggerFirstRunUI")
                .arg("--disable-hang-monitor")
                .arg("--disable-background-timer-throttling")
                .arg("--enable-logging")
                .arg("--log-level=1")
                .arg(format!("--log-file={}", log_path))
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .stdin(Stdio::null());
            let _ = cmd.spawn();
        }

        #[cfg(target_os = "windows")]
        {
            let log_path = format!("{}\\code-chrome.log", std::env::temp_dir().display());
            let chrome_paths = vec![
                "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe".to_string(),
                "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe".to_string(),
                format!(
                    "{}\\AppData\\Local\\Google\\Chrome\\Application\\chrome.exe",
                    std::env::var("USERPROFILE").unwrap_or_default()
                ),
            ];

            for chrome_path in chrome_paths {
                if std::path::Path::new(&chrome_path).exists() {
                    let mut cmd = std::process::Command::new(&chrome_path);
                    cmd.arg(format!("--remote-debugging-port={}", port))
                        .arg(format!("--user-data-dir={}", profile_dir.display()))
                        .arg("--no-first-run")
                        .arg("--no-default-browser-check")
                        .arg("--disable-component-extensions-with-background-pages")
                        .arg("--disable-background-networking")
                        .arg("--silent-debugger-extension-api")
                        .arg("--remote-allow-origins=*")
                        .arg("--disable-features=ChromeWhatsNewUI,TriggerFirstRunUI")
                        .arg("--disable-hang-monitor")
                        .arg("--disable-background-timer-throttling")
                        .arg("--enable-logging")
                        .arg("--log-level=1")
                        .arg(format!("--log-file={}", log_path))
                        .stdout(Stdio::null())
                        .stderr(Stdio::null())
                        .stdin(Stdio::null());
                    let _ = cmd.spawn();
                    break;
                }
            }
        }

        // Add status message
        self.history_push(history_cell::PlainHistoryCell {
            lines: vec![Line::from(format!(
                "✅ Chrome launched with temporary profile at {}",
                profile_dir.display()
            ))],
            kind: history_cell::HistoryCellType::BackgroundEvent,
        });
    }

    pub(crate) fn handle_browser_command(&mut self, command_text: String) {
        // Parse the browser subcommand
        let trimmed = command_text.trim();

        // Handle the case where just "/browser" was typed
        if trimmed.is_empty() {
            tracing::info!("[/browser] toggling internal browser on/off");

            // Optimistically reflect browsing activity in the input border if we end up enabling
            // (safe even if we later disable; UI will update on event messages)
            self.bottom_pane.update_status_text("using browser".to_string());

            // Toggle asynchronously: if internal browser is active, disable it; otherwise enable and open about:blank
            let app_event_tx = self.app_event_tx.clone();
            tokio::spawn(async move {
                let browser_manager = ChatWidget::get_browser_manager().await;
                // Determine if internal browser is currently active
                let (is_external, status) = {
                    let cfg = browser_manager.config.read().await;
                    let is_external = cfg.connect_port.is_some() || cfg.connect_ws.is_some();
                    drop(cfg);
                    (is_external, browser_manager.get_status().await)
                };

                if !is_external && status.browser_active {
                    // Internal browser active → disable it
                    if let Err(e) = browser_manager.set_enabled(false).await {
                        tracing::warn!("[/browser] failed to disable internal browser: {}", e);
                    }
                    use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                    let _ = app_event_tx.send(AppEvent::CodexEvent(Event {
                        id: uuid::Uuid::new_v4().to_string(),
                        event_seq: 0,
                        msg: EventMsg::BackgroundEvent(BackgroundEventEvent {
                            message: "🔌 Browser disabled".to_string(),
                        }),
                        order: None,
                    }));
                } else {
                    // Not in internal mode → enable internal and open about:blank
                    // Reuse existing helper (ensures config + start + global manager + screenshot)
                    // Then explicitly navigate to about:blank
                    // We fire-and-forget errors to avoid blocking UI
                    {
                        // Configure cleanly for internal mode
                        let mut cfg = browser_manager.config.write().await;
                        cfg.connect_port = None;
                        cfg.connect_ws = None;
                        cfg.enabled = true;
                        cfg.persist_profile = false;
                        cfg.headless = true;
                    }

                    if let Err(e) = browser_manager.start().await {
                        tracing::error!("[/browser] failed to start internal browser: {}", e);
                        use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                        let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent { message: format!("❌ Failed to start internal browser: {}", e) }), order: None }));
                        return;
                    }

                    // Set as global manager so core/session share the same instance
                    dev_browser::global::set_global_browser_manager(browser_manager.clone()).await;

                    // Navigate to about:blank explicitly
                    if let Err(e) = browser_manager.goto("about:blank").await {
                        tracing::warn!("[/browser] failed to open about:blank: {}", e);
                    }

                    // Emit confirmation
                    use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                    let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent { message: "✅ Browser enabled (about:blank)".to_string() }), order: None }));
                }
            });
            return;
        }

        let parts: Vec<&str> = trimmed.split_whitespace().collect();
        let response = if !parts.is_empty() {
            let first_arg = parts[0];

            // Check if the first argument looks like a URL (has a dot or protocol)
            let is_url = first_arg.contains("://") || first_arg.contains(".");

            if is_url {
                // It's a URL - enable browser mode and navigate to it
                let url = parts.join(" ");

            // Ensure URL has protocol
            let full_url = if !url.contains("://") {
                format!("https://{}", url)
            } else {
                url.clone()
            };

                // We are navigating with the internal browser
                self.browser_is_external = false;

                // Navigate to URL and wait for it to load
                let latest_screenshot = self.latest_browser_screenshot.clone();
                let app_event_tx = self.app_event_tx.clone();
                let url_for_goto = full_url.clone();

                // Add status message
                let status_msg = format!("🌐 Opening internal browser: {}", full_url);
                self.history_push(history_cell::PlainHistoryCell {
                    lines: vec![Line::from(status_msg)],
                    kind: history_cell::HistoryCellType::BackgroundEvent,
                });
                // Also reflect browsing activity in the input border
                self.bottom_pane.update_status_text("using browser".to_string());

                // Connect immediately, don't wait for message send
                tokio::spawn(async move {
                    // Get the global browser manager
                    let browser_manager = ChatWidget::get_browser_manager().await;

                    // Enable browser mode and ensure it's using internal browser (not CDP)
                    browser_manager.set_enabled_sync(true);
                    {
                        let mut config = browser_manager.config.write().await;
                        config.headless = false; // Ensure browser is visible when navigating to URL
                        config.connect_port = None; // Ensure we're not trying to connect to CDP
                        config.connect_ws = None; // Ensure we're not trying to connect via WebSocket
                    }

                    // IMPORTANT: Start the browser manager first before navigating
                    if let Err(e) = browser_manager.start().await {
                        tracing::error!("Failed to start TUI browser manager: {}", e);
                        return;
                    }

                    // Set up navigation callback to auto-capture screenshots
                    {
                        let latest_screenshot_callback = latest_screenshot.clone();
                        let app_event_tx_callback = app_event_tx.clone();

                        browser_manager
                            .set_navigation_callback(move |url| {
                                tracing::info!("Navigation callback triggered for URL: {}", url);
                                let latest_screenshot_inner = latest_screenshot_callback.clone();
                                let app_event_tx_inner = app_event_tx_callback.clone();
                                let url_inner = url.clone();

                                tokio::spawn(async move {
                                    // Get browser manager in the inner async block
                                    let browser_manager_inner =
                                        ChatWidget::get_browser_manager().await;
                                    // Capture screenshot after navigation
                                    match browser_manager_inner.capture_screenshot_with_url().await
                                    {
                                        Ok((paths, _)) => {
                                            if let Some(first_path) = paths.first() {
                                                tracing::info!(
                                                    "Auto-captured screenshot after navigation: {}",
                                                    first_path.display()
                                                );

                                                // Update the latest screenshot
                                                if let Ok(mut latest) =
                                                    latest_screenshot_inner.lock()
                                                {
                                                    *latest = Some((
                                                        first_path.clone(),
                                                        url_inner.clone(),
                                                    ));
                                                }

                                                // Send update event
                                                use dev_core::protocol::{
                                                    BrowserScreenshotUpdateEvent, EventMsg,
                                                };
                                                let _ = app_event_tx_inner.send(
                                                    AppEvent::CodexEvent(Event {
                                                        id: uuid::Uuid::new_v4().to_string(),
                                                        event_seq: 0,
                                                        msg: EventMsg::BrowserScreenshotUpdate(
                                                            BrowserScreenshotUpdateEvent {
                                                                screenshot_path: first_path.clone(),
                                                                url: url_inner,
                                                            },
                                                        ),
                                                        order: None,
                                                    }),
                                                );
                                            }
                                        }
                                        Err(e) => {
                                            tracing::error!(
                                                "Failed to auto-capture screenshot: {}",
                                                e
                                            );
                                        }
                                    }
                                });
                            })
                            .await;
                    }

                    // Set the browser manager as the global manager so both TUI and Session use the same instance
                    dev_browser::global::set_global_browser_manager(browser_manager.clone())
                        .await;

                    // Ensure the navigation callback is also set on the global manager
                    let global_manager = dev_browser::global::get_browser_manager().await;
                    if let Some(global_manager) = global_manager {
                        let latest_screenshot_global = latest_screenshot.clone();
                        let app_event_tx_global = app_event_tx.clone();

                        global_manager.set_navigation_callback(move |url| {
                            tracing::info!("Global manager navigation callback triggered for URL: {}", url);
                            let latest_screenshot_inner = latest_screenshot_global.clone();
                            let app_event_tx_inner = app_event_tx_global.clone();
                            let url_inner = url.clone();

                            tokio::spawn(async move {
                                // Wait a moment for the navigation to complete
                                tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;

                                // Capture screenshot after navigation
                                let browser_manager = dev_browser::global::get_browser_manager().await;
                                if let Some(browser_manager) = browser_manager {
                                    match browser_manager.capture_screenshot_with_url().await {
                                        Ok((paths, _url)) => {
                                            if let Some(first_path) = paths.first() {
                                                tracing::info!("Auto-captured screenshot after global navigation: {}", first_path.display());

                                                // Update the latest screenshot
                                                if let Ok(mut latest) = latest_screenshot_inner.lock() {
                                                    *latest = Some((first_path.clone(), url_inner.clone()));
                                                }

                                                // Send update event
                                                use dev_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                                                let _ = app_event_tx_inner.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                        screenshot_path: first_path.clone(),
                                                        url: url_inner,
                                                    }), order: None }));
                                            }
                                        }
                                        Err(e) => {
                                            tracing::error!("Failed to auto-capture screenshot after global navigation: {}", e);
                                        }
                                    }
                                }
                            });
                        }).await;
                    }

                    // Navigate using global manager
                    match browser_manager.goto(&url_for_goto).await {
                        Ok(result) => {
                            tracing::info!(
                                "Browser opened to: {} (title: {:?})",
                                result.url,
                                result.title
                            );

                            // Send success message to chat
                            use dev_core::protocol::{BackgroundEventEvent, EventMsg};
                            let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent { message: format!("✅ Internal browser opened: {}", result.url) }), order: None }));

                            // Capture initial screenshot
                            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                            match browser_manager.capture_screenshot_with_url().await {
                                Ok((paths, url)) => {
                                    if let Some(first_path) = paths.first() {
                                        tracing::info!(
                                            "Initial screenshot captured: {}",
                                            first_path.display()
                                        );

                                        // Update the latest screenshot
                                        if let Ok(mut latest) = latest_screenshot.lock() {
                                            *latest = Some((
                                                first_path.clone(),
                                                url.clone().unwrap_or_else(|| result.url.clone()),
                                            ));
                                        }

                                        // Send update event
                                        use dev_core::protocol::BrowserScreenshotUpdateEvent;
                                        use dev_core::protocol::EventMsg;
                                        let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent { screenshot_path: first_path.clone(), url: url.unwrap_or_else(|| result.url.clone()) }), order: None }));
                                    }
                                }
                                Err(e) => {
                                    tracing::error!("Failed to capture initial screenshot: {}", e);
                                }
                            }
                        }
                        Err(e) => {
                            tracing::error!("Failed to open browser: {}", e);
                        }
                    }
                });

                format!("Browser mode enabled: {}\n", full_url)
            } else {
                // It's a subcommand
                match first_arg {
                    "off" => {
                        // Disable browser mode
                        // Clear the screenshot popup
                        if let Ok(mut screenshot_lock) = self.latest_browser_screenshot.lock() {
                            *screenshot_lock = None;
                        }
                        // Close any open browser
                        tokio::spawn(async move {
                            let browser_manager = ChatWidget::get_browser_manager().await;
                            browser_manager.set_enabled_sync(false);
                            if let Err(e) = browser_manager.close().await {
                                tracing::error!("Failed to close browser: {}", e);
                            }
                        });
                        self.app_event_tx.send(AppEvent::RequestRedraw);
                        "Browser mode disabled.".to_string()
                    }
                    "status" => {
                        // Get status from BrowserManager
                        // Use a channel to get status from async context
                        let (status_tx, status_rx) = std::sync::mpsc::channel();
                        tokio::spawn(async move {
                            let browser_manager = ChatWidget::get_browser_manager().await;
                            let status = browser_manager.get_status_sync();
                            let _ = status_tx.send(status);
                        });
                        status_rx
                            .recv()
                            .unwrap_or_else(|_| "Failed to get browser status.".to_string())
                    }
                    "fullpage" => {
                        if parts.len() > 2 {
                            match parts[2] {
                                "on" => {
                                    // Enable full-page mode
                                    tokio::spawn(async move {
                                        let browser_manager =
                                            ChatWidget::get_browser_manager().await;
                                        browser_manager.set_fullpage_sync(true);
                                    });
                                    "Full-page screenshot mode enabled (max 8 segments)."
                                        .to_string()
                                }
                                "off" => {
                                    // Disable full-page mode
                                    tokio::spawn(async move {
                                        let browser_manager =
                                            ChatWidget::get_browser_manager().await;
                                        browser_manager.set_fullpage_sync(false);
                                    });
                                    "Full-page screenshot mode disabled.".to_string()
                                }
                                _ => "Usage: /browser fullpage [on|off]".to_string(),
                            }
                        } else {
                            "Usage: /browser fullpage [on|off]".to_string()
                        }
                    }
                    "config" => {
                        if parts.len() > 3 {
                            let key = parts[2];
                            let value = parts[3..].join(" ");
                            // Update browser config
                            match key {
                                "viewport" => {
                                    // Parse viewport dimensions like "1920x1080"
                                    if let Some((width_str, height_str)) = value.split_once('x') {
                                        if let (Ok(width), Ok(height)) =
                                            (width_str.parse::<u32>(), height_str.parse::<u32>())
                                        {
                                            tokio::spawn(async move {
                                                let browser_manager =
                                                    ChatWidget::get_browser_manager().await;
                                                browser_manager.set_viewport_sync(width, height);
                                            });
                                            format!(
                                                "Browser viewport updated: {}x{}",
                                                width, height
                                            )
                                        } else {
                                            "Invalid viewport format. Use: /browser config viewport 1920x1080".to_string()
                                        }
                                    } else {
                                        "Invalid viewport format. Use: /browser config viewport 1920x1080".to_string()
                                    }
                                }
                                "segments_max" => {
                                    if let Ok(max) = value.parse::<usize>() {
                                        tokio::spawn(async move {
                                            let browser_manager =
                                                ChatWidget::get_browser_manager().await;
                                            browser_manager.set_segments_max_sync(max);
                                        });
                                        format!("Browser segments_max updated: {}", max)
                                    } else {
                                        "Invalid segments_max value. Use a number.".to_string()
                                    }
                                }
                                _ => format!(
                                    "Unknown config key: {}. Available: viewport, segments_max",
                                    key
                                ),
                            }
                        } else {
                            "Usage: /browser config <key> <value>\nAvailable keys: viewport, segments_max".to_string()
                        }
                    }
                    _ => {
                        format!(
                            "Unknown browser command: '{}'\nUsage: /browser <url> | off | status | fullpage | config",
                            first_arg
                        )
                    }
                }
            }
        } else {
            "Browser commands:\n• /browser <url> - Open URL in internal browser\n• /browser off - Disable browser mode\n• /browser status - Show current status\n• /browser fullpage [on|off] - Toggle full-page mode\n• /browser config <key> <value> - Update configuration\n\nUse /chrome [port] to connect to external Chrome browser".to_string()
        };

        // Add the response to the UI as a background event
        let lines = response
            .lines()
            .map(|line| Line::from(line.to_string()))
            .collect();
        self.history_push(history_cell::PlainHistoryCell { lines, kind: history_cell::HistoryCellType::BackgroundEvent });
    }

    #[allow(dead_code)]
    fn switch_to_internal_browser(&mut self) {
        // Switch to internal browser mode
        self.browser_is_external = false;
        let latest_screenshot = self.latest_browser_screenshot.clone();
        let app_event_tx = self.app_event_tx.clone();

        tokio::spawn(async move {
            let browser_manager = ChatWidget::get_browser_manager().await;

            // First, close any existing Chrome connection
            if browser_manager.is_enabled().await {
                let _ = browser_manager.close().await;
            }

            // Configure for internal browser
            {
                let mut config = browser_manager.config.write().await;
                config.connect_port = None;
                config.connect_ws = None;
                config.headless = true;
                config.persist_profile = false;
                config.enabled = true;
            }

            // Enable internal browser
            browser_manager.set_enabled_sync(true);

            // Explicitly (re)start the internal browser session now
            if let Err(e) = browser_manager.start().await {
                tracing::error!("Failed to start internal browser: {}", e);
                let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent { message: format!("❌ Failed to start internal browser: {}", e) }), order: None }));
                return;
            }

            // Set as global manager so core/session share the same instance
            dev_browser::global::set_global_browser_manager(browser_manager.clone()).await;

            // Notify about successful switch/reconnect
            let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent { message: "✅ Switched to internal browser mode (reconnected)".to_string() }), order: None }));

            // Clear any existing screenshot
            if let Ok(mut screenshot) = latest_screenshot.lock() {
                *screenshot = None;
            }

            // Proactively navigate to about:blank, then capture a first screenshot to populate HUD
            let _ = browser_manager.goto("about:blank").await;
            // Capture an initial screenshot to populate HUD
            tokio::time::sleep(tokio::time::Duration::from_millis(300)).await;
            match browser_manager.capture_screenshot_with_url().await {
                Ok((paths, url)) => {
                    if let Some(first_path) = paths.first() {
                        if let Ok(mut latest) = latest_screenshot.lock() {
                            *latest = Some((
                                first_path.clone(),
                                url.clone().unwrap_or_else(|| "Browser".to_string()),
                            ));
                        }
                        use dev_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                        let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent { screenshot_path: first_path.clone(), url: url.unwrap_or_else(|| "Browser".to_string()) }), order: None }));
                    }
                }
                Err(e) => {
                    tracing::warn!("Failed to capture initial internal browser screenshot: {}", e);
                }
            }
        });
    }

    fn handle_chrome_connection(&mut self, host: Option<String>, port: Option<u16>) {
        tracing::info!("[cdp] handle_chrome_connection begin, host={:?}, port={:?}", host, port);
        self.browser_is_external = true;
        let latest_screenshot = self.latest_browser_screenshot.clone();
        let app_event_tx = self.app_event_tx.clone();
        let port_display = port.map_or("auto-detect".to_string(), |p| p.to_string());
        let host_display = host.clone().unwrap_or_else(|| "127.0.0.1".to_string());

        // Add status message to chat (use BackgroundEvent with header so it renders reliably)
        let status_msg = format!(
            "🔗 Connecting to Chrome DevTools Protocol ({}:{})...",
            host_display,
            port_display
        );
        self.history_push(history_cell::new_background_event(status_msg));

        // Connect in background with a single, unified flow (no double-connect)
        tokio::spawn(async move {
            tracing::info!("[cdp] connect task spawned, host={:?}, port={:?}", host, port);
            // Unified connect flow; emits success/failure messages internally
            ChatWidget::connect_to_cdp_chrome(host, port, latest_screenshot.clone(), app_event_tx.clone()).await;
        });
    }

    pub(crate) fn handle_chrome_command(&mut self, command_text: String) {
        tracing::info!("[cdp] handle_chrome_command start: '{}'", command_text);
        // Parse the chrome command arguments
        let parts: Vec<&str> = command_text.trim().split_whitespace().collect();

        // Handle empty command - just "/chrome"
        if parts.is_empty() || command_text.trim().is_empty() {
            tracing::info!("[cdp] no args provided; toggle connect/disconnect");

            // Toggle behavior: if an external Chrome connection is active, disconnect it.
            // Otherwise, start a connection (auto-detect).
            let (tx, rx) = std::sync::mpsc::channel();
            let app_event_tx = self.app_event_tx.clone();
            tokio::spawn(async move {
                let browser_manager = ChatWidget::get_browser_manager().await;
                // Check if we're currently connected to an external Chrome
                let (is_external, browser_active) = {
                    let cfg = browser_manager.config.read().await;
                    let is_external = cfg.connect_port.is_some() || cfg.connect_ws.is_some();
                    drop(cfg);
                    let status = browser_manager.get_status().await;
                    (is_external, status.browser_active)
                };

                if is_external && browser_active {
                    // Disconnect from external Chrome (do not close Chrome itself)
                    if let Err(e) = browser_manager.stop().await {
                        tracing::warn!("[cdp] failed to stop external Chrome connection: {}", e);
                    }
                    // Notify UI
                    use dev_core::protocol::{BackgroundEventEvent, Event, EventMsg};
                    let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BackgroundEvent(BackgroundEventEvent { message: "🔌 Disconnected from Chrome".to_string() }), order: None }));
                    let _ = tx.send(true);
                } else {
                    // Not connected externally; proceed to connect
                    let _ = tx.send(false);
                }
            });

            // If the async task handled a disconnect, stop here; otherwise connect.
            let handled_disconnect = rx.recv().unwrap_or(false);
            if !handled_disconnect {
                // Switch to external Chrome mode with default/auto-detected port
                self.handle_chrome_connection(None, None);
            } else {
                // We just disconnected; reflect in title immediately
                self.browser_is_external = false;
                self.request_redraw();
            }
            return;
        }

        // Check if it's a status command
        if parts[0] == "status" {
            // Get status from BrowserManager - same as /browser status
            let (status_tx, status_rx) = std::sync::mpsc::channel();
            tokio::spawn(async move {
                let browser_manager = ChatWidget::get_browser_manager().await;
                let status = browser_manager.get_status_sync();
                let _ = status_tx.send(status);
            });
            let status = status_rx
                .recv()
                .unwrap_or_else(|_| "Failed to get browser status.".to_string());

            // Add the response to the UI
            let lines = status
                .lines()
                .map(|line| Line::from(line.to_string()))
                .collect();
            self.history_push(history_cell::PlainHistoryCell { lines, kind: history_cell::HistoryCellType::BackgroundEvent });
            return;
        }

        // Accept several forms:
        //   /chrome 9222
        //   /chrome host:9222
        //   /chrome host 9222
        //   /chrome ws://host:9222/devtools/browser/<id>
        let mut host: Option<String> = None;
        let mut port: Option<u16> = None;
        let first = parts[0];

        if let Some(ws) = first.strip_prefix("ws://").or_else(|| first.strip_prefix("wss://")) {
            // Full WS URL provided: set directly via config and return
            let ws_url = if first.starts_with("ws") { first.to_string() } else { format!("wss://{}", ws) };
            tracing::info!("[cdp] /chrome provided WS endpoint: {}", ws_url);
            // Configure and connect using WS
            self.browser_is_external = true;
            let latest_screenshot = self.latest_browser_screenshot.clone();
            let app_event_tx = self.app_event_tx.clone();
            tokio::spawn(async move {
                let bm = ChatWidget::get_browser_manager().await;
                {
                    let mut cfg = bm.config.write().await;
                    cfg.enabled = true;
                    cfg.headless = false;
                    cfg.persist_profile = true;
                    cfg.connect_ws = Some(ws_url);
                    cfg.connect_port = None;
                    cfg.connect_host = None;
                }
                let _ = bm.connect_to_chrome_only().await;
                // Capture a first screenshot if possible
                tokio::time::sleep(tokio::time::Duration::from_millis(300)).await;
                match bm.capture_screenshot_with_url().await {
                    Ok((paths, url)) => {
                        if let Some(first_path) = paths.first() {
                            if let Ok(mut latest) = latest_screenshot.lock() {
                                *latest = Some((
                                    first_path.clone(),
                                    url.clone().unwrap_or_else(|| "Browser".to_string()),
                                ));
                            }
                            use dev_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                            let _ = app_event_tx.send(AppEvent::CodexEvent(Event { id: uuid::Uuid::new_v4().to_string(), event_seq: 0, msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent { screenshot_path: first_path.clone(), url: url.unwrap_or_else(|| "Browser".to_string()) }), order: None }));
                        }
                    }
                    Err(e) => {
                        tracing::warn!("Failed to capture initial external Chrome screenshot: {}", e);
                    }
                }
            });
            return;
        }

        if let Some((h, p)) = first.rsplit_once(':') {
            if let Ok(pn) = p.parse::<u16>() { host = Some(h.to_string()); port = Some(pn); }
        }
        if host.is_none() && port.is_none() {
            if let Ok(pn) = first.parse::<u16>() { port = Some(pn); }
            else if parts.len() >= 2 { if let Ok(pn) = parts[1].parse::<u16>() { host = Some(first.to_string()); port = Some(pn); } }
        }
        tracing::info!("[cdp] parsed host={:?}, port={:?}", host, port);
        self.handle_chrome_connection(host, port);
    }

    /// Programmatically submit a user text message as if typed in the
    /// composer. The text will be added to conversation history and sent to
    /// the agent. This also handles slash command expansion.
    pub(crate) fn submit_text_message(&mut self, text: String) {
        if text.is_empty() {
            return;
        }
        self.submit_user_message(text.into());
    }

    pub(crate) fn token_usage(&self) -> &TokenUsage {
        &self.total_token_usage
    }

    pub(crate) fn clear_token_usage(&mut self) {
        self.total_token_usage = TokenUsage::default();
        self.bottom_pane.set_token_usage(
            self.total_token_usage.clone(),
            self.last_token_usage.clone(),
            self.config.model_context_window,
        );
    }

    /// Clear the conversation and start fresh with a new welcome animation
    pub(crate) fn new_conversation(&mut self, enhanced_keys_supported: bool) {
        // Clear all history cells
        self.history_cells.clear();
        self.cell_order_seq.clear();
        
        // Reset various state
        self.active_exec_cell = None;
        self.clear_token_usage();
        
        // Add a new animated welcome cell at the top of the next request so
        // upcoming output appears below it.
        self.history_push_top_next_req(history_cell::new_animated_welcome());
        self.reasoning_index.clear();
        self.stream_order_seq.clear();
        
        // Reset the bottom pane with a new composer
        // (This effectively clears the text input)
        self.bottom_pane = BottomPane::new(BottomPaneParams {
            app_event_tx: self.app_event_tx.clone(),
            has_input_focus: true,
            enhanced_keys_supported,
            using_chatgpt_auth: self.config.using_chatgpt_auth,
        });
        
        // Request redraw for the new animation
        self.mark_needs_redraw();
    }

    pub fn cursor_pos(&self, area: Rect) -> Option<(u16, u16)> {
        let layout_areas = self.layout_areas(area);
        let bottom_pane_area = if layout_areas.len() == 4 {
            layout_areas[3]
        } else {
            layout_areas[2]
        };
        self.bottom_pane.cursor_pos(bottom_pane_area)
    }

    fn measured_font_size(&self) -> (u16, u16) {
        *self.cached_cell_size.get_or_init(|| {
            let size = self.terminal_info.font_size;

            // HACK: On macOS Retina displays, terminals often report physical pixels
            // but ratatui-image expects logical pixels. If we detect suspiciously
            // large cell sizes (likely 2x scaled), divide by 2.
            #[cfg(target_os = "macos")]
            {
                if size.0 >= 14 && size.1 >= 28 {
                    // Likely Retina display reporting physical pixels
                    tracing::info!(
                        "Detected likely Retina display, adjusting cell size from {:?} to {:?}",
                        size,
                        (size.0 / 2, size.1 / 2)
                    );
                    return (size.0 / 2, size.1 / 2);
                }
            }

            size
        })
    }

    fn get_git_branch(&self) -> Option<String> {
        use std::fs;
        use std::path::Path;

        // Read .git/HEAD to avoid spawning `git` every frame.
        // Formats:
        //   - "ref: refs/heads/<branch>" when on a named branch
        //   - "<40-hex-SHA>" when in a detached HEAD state
        let head_path = self.config.cwd.join(".git/HEAD");
        let head_contents = fs::read_to_string(&head_path).ok()?;
        let head = head_contents.trim();

        if let Some(rest) = head.strip_prefix("ref: ") {
            // Extract last path segment as branch name
            if let Some(name) = Path::new(rest)
                .file_name()
                .and_then(|s| s.to_str())
                .filter(|s| !s.is_empty())
            {
                return Some(name.to_string());
            }
        }

        // Detached HEAD: display short SHA if it looks like a commit hash
        let is_hex = head.len() >= 7 && head.as_bytes().iter().all(|b| b.is_ascii_hexdigit());
        if is_hex {
            return Some(format!("detached: {}", &head[..7]));
        }

        None
    }

    fn render_status_bar(&self, area: Rect, buf: &mut Buffer) {
        use crate::exec_command::relativize_to_home;
        use ratatui::layout::Margin;
        use ratatui::style::{Modifier, Style};
        use ratatui::text::Line;
        use ratatui::text::Span;
        use ratatui::widgets::Block;
        use ratatui::widgets::Borders;
        use ratatui::widgets::Paragraph;

        // Add same horizontal padding as the Message input (2 chars on each side)
        let horizontal_padding = 1u16;
        let padded_area = Rect {
            x: area.x + horizontal_padding,
            y: area.y,
            width: area.width.saturating_sub(horizontal_padding * 2),
            height: area.height,
        };

        // Get current working directory string
        let cwd_str = match relativize_to_home(&self.config.cwd) {
            Some(rel) if !rel.as_os_str().is_empty() => format!("~/{}", rel.display()),
            Some(_) => "~".to_string(),
            None => self.config.cwd.display().to_string(),
        };

        // Build status line spans with dynamic elision based on width.
        // Removal priority when space is tight:
        //   1) Reasoning level
        //   2) Model
        //   3) Branch
        //   4) Directory
        let branch_opt = self.get_git_branch();

        // Helper to assemble spans based on include flags
        let build_spans = |include_reasoning: bool,
                           include_model: bool,
                           include_branch: bool,
                           include_dir: bool| {
            let mut spans: Vec<Span> = Vec::new();
            // Title follows theme text color
            spans.push(Span::styled(
                "Code",
                Style::default()
                    .fg(crate::colors::text())
                    .add_modifier(Modifier::BOLD),
            ));

            if include_model {
                spans.push(Span::styled("  •  ", Style::default().fg(crate::colors::text_dim())));
                spans.push(Span::styled(
                    "Model: ",
                    Style::default().fg(crate::colors::text_dim()),
                ));
                spans.push(Span::styled(
                    self.format_model_name(&self.config.model),
                    Style::default().fg(crate::colors::info()),
                ));
            }

            if include_reasoning {
                spans.push(Span::styled("  •  ", Style::default().fg(crate::colors::text_dim())));
                spans.push(Span::styled(
                    "Reasoning: ",
                    Style::default().fg(crate::colors::text_dim()),
                ));
                spans.push(Span::styled(
                    format!("{}", self.config.model_reasoning_effort),
                    Style::default().fg(crate::colors::info()),
                ));
            }

            if include_dir {
                spans.push(Span::styled("  •  ", Style::default().fg(crate::colors::text_dim())));
                spans.push(Span::styled(
                    "Directory: ",
                    Style::default().fg(crate::colors::text_dim()),
                ));
                spans.push(Span::styled(cwd_str.clone(), Style::default().fg(crate::colors::info())));
            }

            if include_branch {
                if let Some(branch) = &branch_opt {
                    spans.push(Span::styled("  •  ", Style::default().fg(crate::colors::text_dim())));
                    spans.push(Span::styled(
                        "Branch: ",
                        Style::default().fg(crate::colors::text_dim()),
                    ));
                    spans.push(Span::styled(
                        branch.clone(),
                        Style::default().fg(crate::colors::success_green()),
                    ));
                }
            }

            // Footer already shows the Ctrl+R hint; avoid duplicating it here.

            spans
        };

        // Start with all items
        let mut include_reasoning = true;
        let mut include_model = true;
        let mut include_branch = branch_opt.is_some();
        let mut include_dir = true;
        let mut status_spans = build_spans(include_reasoning, include_model, include_branch, include_dir);

        // Now recompute exact available width inside the border + padding before measuring
        // Render a bordered status block and explicitly fill its background.
        // Without a background fill, some terminals blend with prior frame
        // contents, which is especially noticeable on dark themes as dark
        // "caps" at the edges. Match the app background for consistency.
        let status_block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()))
            .style(Style::default().bg(crate::colors::background()));
        let inner_area = status_block.inner(padded_area);
        let padded_inner = inner_area.inner(Margin::new(1, 0));
        let inner_width = padded_inner.width as usize;

        // Helper to measure current spans width
        let measure = |spans: &Vec<Span>| -> usize {
            spans.iter().map(|s| s.content.chars().count()).sum()
        };

        // Elide items in priority order until content fits
        while measure(&status_spans) > inner_width {
            if include_reasoning {
                include_reasoning = false;
            } else if include_model {
                include_model = false;
            } else if include_branch {
                include_branch = false;
            } else if include_dir {
                include_dir = false;
            } else {
                break;
            }
            status_spans = build_spans(include_reasoning, include_model, include_branch, include_dir);
        }
        
        // Note: The reasoning visibility hint is appended inside `build_spans`
        // so it participates in width measurement and elision. Do not append
        // it again here to avoid overflow that caused corrupted glyph boxes on
        // some terminals.

        let status_line = Line::from(status_spans);

        // Render the block first
        status_block.render(padded_area, buf);

        // Then render the text inside with padding, centered
        let status_widget = Paragraph::new(vec![status_line])
            .alignment(ratatui::layout::Alignment::Center)
            .style(
                Style::default()
                    .bg(crate::colors::background())
                    .fg(crate::colors::text()),
            );
        ratatui::widgets::Widget::render(status_widget, padded_inner, buf);
    }

    fn render_screenshot_highlevel(&self, path: &PathBuf, area: Rect, buf: &mut Buffer) {
        use ratatui::widgets::Widget;
        use ratatui_image::Image;
        use ratatui_image::Resize;
        use ratatui_image::picker::Picker;
        use ratatui_image::picker::ProtocolType;

        // First, cheaply read image dimensions without decoding the full image
        let (img_w, img_h) = match image::image_dimensions(path) {
            Ok(dim) => dim,
            Err(_) => {
                self.render_screenshot_placeholder(path, area, buf);
                return;
            }
        };

        // picker (Retina 2x workaround preserved)
        let mut cached_picker = self.cached_picker.borrow_mut();
        if cached_picker.is_none() {
            // If we didn't get a picker from terminal query at startup, create one from font size
            let (fw, fh) = self.measured_font_size();
            let p = Picker::from_fontsize((fw, fh));

            *cached_picker = Some(p);
        }
        let picker = cached_picker.as_ref().unwrap();

        // quantize step by protocol to avoid rounding bias
        let (_qx, _qy): (u16, u16) = match picker.protocol_type() {
            ProtocolType::Halfblocks => (1, 2), // half-block cell = 1 col x 2 half-rows
            _ => (1, 1),                        // pixel protocols (Kitty/iTerm2/Sixel)
        };

        // terminal cell aspect
        let (cw, ch) = self.measured_font_size();
        let cols = area.width as u32;
        let rows = area.height as u32;
        let cw = cw as u32;
        let ch = ch as u32;

        // fit (floor), then choose limiting dimension
        let mut rows_by_w = (cols * cw * img_h) / (img_w * ch);
        if rows_by_w == 0 {
            rows_by_w = 1;
        }
        let mut cols_by_h = (rows * ch * img_w) / (img_h * cw);
        if cols_by_h == 0 {
            cols_by_h = 1;
        }

        let (_used_cols, _used_rows) = if rows_by_w <= rows {
            (cols, rows_by_w)
        } else {
            (cols_by_h, rows)
        };

        // Compute a centered target rect based on image aspect and font cell size
        let (cell_w, cell_h) = self.measured_font_size();
        let area_px_w = (area.width as u32) * (cell_w as u32);
        let area_px_h = (area.height as u32) * (cell_h as u32);
        // If either dimension is zero, bail to placeholder
        if area.width == 0 || area.height == 0 || area_px_w == 0 || area_px_h == 0 {
            self.render_screenshot_placeholder(path, area, buf);
            return;
        }
        let (img_w, img_h) = match image::image_dimensions(path) {
            Ok(dim) => dim,
            Err(_) => { self.render_screenshot_placeholder(path, area, buf); return; }
        };
        let scale_num_w = area_px_w;
        let scale_num_h = area_px_h;
        let scale_w = scale_num_w as f64 / img_w as f64;
        let scale_h = scale_num_h as f64 / img_h as f64;
        let scale = scale_w.min(scale_h).max(0.0);
        // Compute target size in cells
        let target_w_cells = ((img_w as f64 * scale) / (cell_w as f64)).floor() as u16;
        let target_h_cells = ((img_h as f64 * scale) / (cell_h as f64)).floor() as u16;
        let target_w = target_w_cells.clamp(1, area.width);
        let target_h = target_h_cells.clamp(1, area.height);
        let target_x = area.x + (area.width.saturating_sub(target_w)) / 2;
        let target_y = area.y + (area.height.saturating_sub(target_h)) / 2;
        let target = Rect { x: target_x, y: target_y, width: target_w, height: target_h };

        // cache by (path, target)
        let needs_recreate = {
            let cached = self.cached_image_protocol.borrow();
            match cached.as_ref() {
                Some((cached_path, cached_rect, _)) => {
                    cached_path != path || *cached_rect != target
                }
                None => true,
            }
        };
        if needs_recreate {
            // Only decode when we actually need to (path/target changed)
            let dyn_img = match image::ImageReader::open(path) {
                Ok(r) => match r.decode() {
                    Ok(img) => img,
                    Err(_) => {
                        self.render_screenshot_placeholder(path, area, buf);
                        return;
                    }
                },
                Err(_) => {
                    self.render_screenshot_placeholder(path, area, buf);
                    return;
                }
            };
            match picker.new_protocol(dyn_img, target, Resize::Fit(Some(FilterType::Lanczos3))) {
                Ok(protocol) => {
                    *self.cached_image_protocol.borrow_mut() =
                        Some((path.clone(), target, protocol))
                }
                Err(_) => {
                    self.render_screenshot_placeholder(path, area, buf);
                    return;
                }
            }
        }

        if let Some((_, rect, protocol)) = &*self.cached_image_protocol.borrow() {
            let image = Image::new(protocol);
            Widget::render(image, *rect, buf);
        } else {
            self.render_screenshot_placeholder(path, area, buf);
        }
    }

    fn render_screenshot_placeholder(&self, path: &PathBuf, area: Rect, buf: &mut Buffer) {
        use ratatui::style::{Modifier, Style};
        use ratatui::widgets::Block;
        use ratatui::widgets::Borders;
        use ratatui::widgets::Paragraph;

        // Show a placeholder box with screenshot info
        let filename = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("screenshot");

        let placeholder_text = format!("[Screenshot]\n{}", filename);
        let placeholder_widget = Paragraph::new(placeholder_text)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(crate::colors::info()))
                    .title("Browser"),
            )
            .style(
                Style::default()
                    .fg(crate::colors::text_dim())
                    .add_modifier(Modifier::ITALIC),
            )
            .wrap(ratatui::widgets::Wrap { trim: true });

        placeholder_widget.render(area, buf);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use dev_core::config::{Config, ConfigOverrides, ConfigToml};

    fn test_config() -> Config {
        dev_core::config::Config::load_from_base_config_with_overrides(
            ConfigToml::default(),
            ConfigOverrides::default(),
            std::env::temp_dir(),
        ).expect("cfg")
    }

    fn make_widget() -> ChatWidget<'static> {
        let (tx_raw, _rx) = std::sync::mpsc::channel::<AppEvent>();
        let app_event_tx = AppEventSender::new(tx_raw);
        let cfg = test_config();
        let term = crate::tui::TerminalInfo { picker: None, font_size: (8, 16) };
        ChatWidget::new(cfg, app_event_tx, None, Vec::new(), false, term)
    }

    #[tokio::test(flavor = "current_thread")]
    async fn exec_end_before_begin_yields_completed_cell_once() {
        let mut chat = make_widget();
        chat.handle_dev_event(dev_core::protocol::Event {
            id: "call-x".into(),
            msg: dev_core::protocol::EventMsg::ExecCommandEnd(dev_core::protocol::ExecCommandEndEvent {
                call_id: "call-x".into(), exit_code: 0, duration: std::time::Duration::from_millis(5), stdout: "ok".into(), stderr: String::new()
            })
        });
        chat.handle_dev_event(dev_core::protocol::Event {
            id: "call-x".into(),
            msg: dev_core::protocol::EventMsg::ExecCommandBegin(dev_core::protocol::ExecCommandBeginEvent {
                call_id: "call-x".into(), command: vec!["echo".into(), "ok".into()], cwd: std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from(".")), parsed_cmd: vec![]
            })
        });
        let dump = chat.test_dump_history_text();
        assert!(dump.iter().any(|s| s.contains("ok") || s.contains("Ran")), "dump: {:?}", dump);
    }

    #[tokio::test(flavor = "current_thread")]
    async fn answer_final_then_delta_ignores_late_delta() {
        let mut chat = make_widget();
        chat.handle_dev_event(dev_core::protocol::Event { id: "ans-1".into(),
            event_seq: 0,
            order: 0, msg: dev_core::protocol::EventMsg::AgentMessage(dev_core::protocol::AgentMessageEvent { message: "hello".into() })});
        chat.handle_dev_event(dev_core::protocol::Event { id: "ans-1".into(),
            event_seq: 0,
            order: 0, msg: dev_core::protocol::EventMsg::AgentMessageDelta(dev_core::protocol::AgentMessageDeltaEvent { delta: " world".into() })});
        assert_eq!(chat.last_assistant_message.as_deref(), Some("hello"));
        // Late delta should be ignored; closed set contains the id
        assert!(chat.stream_state.closed_answer_ids.contains(&StreamId("ans-1".into())));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn reasoning_final_then_delta_ignores_late_delta() {
        let mut chat = make_widget();
        chat.handle_dev_event(dev_core::protocol::Event { id: "r-1".into(),
            event_seq: 0,
            order: 0, msg: dev_core::protocol::EventMsg::AgentReasoning(dev_core::protocol::AgentReasoningEvent { text: "think".into() })});
        chat.handle_dev_event(dev_core::protocol::Event { id: "r-1".into(),
            event_seq: 0,
            order: 0, msg: dev_core::protocol::EventMsg::AgentReasoningDelta(dev_core::protocol::AgentReasoningDeltaEvent { delta: " harder".into() })});
        assert!(chat.stream_state.closed_reasoning_ids.contains(&StreamId("r-1".into())));
    }
}

#[cfg(test)]
impl ChatWidget<'_> {
    pub(crate) fn test_dump_history_text(&self) -> Vec<String> {
        self.history_cells
            .iter()
            .map(|c| {
                let lines = c.display_lines();
                let mut s = String::new();
                for l in lines {
                    for sp in l.spans { s.push_str(&sp.content); }
                    s.push('\n');
                }
                s
            })
            .collect()
    }
}

impl ChatWidget<'_> {
    /// Render the combined HUD with browser and/or agent panels (stacked full-width)
    fn render_hud(&self, area: Rect, buf: &mut Buffer) {
        // Check what's active
        let has_browser_screenshot = self
            .latest_browser_screenshot
            .lock()
            .map(|lock| lock.is_some())
            .unwrap_or(false);
        let has_active_agents = !self.active_agents.is_empty() || self.agents_ready_to_start;

        // Add same horizontal padding as the Message input (2 chars on each side)
        let horizontal_padding = 1u16;
        let padded_area = Rect {
            x: area.x + horizontal_padding,
            y: area.y,
            width: area.width.saturating_sub(horizontal_padding * 2),
            height: area.height,
        };

        // Determine layout based on what's active (stacked, full width)
        let header_h: u16 = 3;
        // Expanded target based on full terminal height per spec
        let term_h = self.layout.last_frame_height.get().max(1);
        let thirty = ((term_h as u32) * 30 / 100) as u16;
        let sixty = ((term_h as u32) * 60 / 100) as u16;
        let mut expanded_target = if thirty < 25 { 25.min(sixty) } else { thirty };
        // Make sure expanded chunk includes space for header + spacer
        let min_expanded = header_h.saturating_add(2);
        if expanded_target < min_expanded { expanded_target = min_expanded; }
        match (has_active_agents, has_browser_screenshot) {
            (true, true) => {
                let (top_h, bottom_h) = if self.layout.agents_hud_expanded && !self.layout.browser_hud_expanded {
                    (expanded_target.min(padded_area.height.saturating_sub(0)), header_h)
                } else if self.layout.browser_hud_expanded && !self.layout.agents_hud_expanded {
                    (header_h, expanded_target.min(padded_area.height.saturating_sub(0)))
                } else {
                    let top = header_h.min(padded_area.height);
                    let bottom = padded_area.height.saturating_sub(top).min(header_h);
                    (top, bottom)
                };
                let chunks = Layout::vertical([
                    Constraint::Length(top_h),
                    Constraint::Length(bottom_h),
                ]).areas::<2>(padded_area);

                // Agents on top
                if self.layout.agents_hud_expanded {
                    self.render_agent_panel(chunks[0], buf);
                } else {
                    self.render_agents_header(chunks[0], buf);
                }
                // Browser on bottom
                if self.layout.browser_hud_expanded {
                    self.render_browser_panel(chunks[1], buf);
                } else {
                    self.render_browser_header(chunks[1], buf);
                }
            }
            (true, false) => {
                if self.layout.agents_hud_expanded {
                    let h = expanded_target.min(padded_area.height);
                    let [a] = Layout::vertical([Constraint::Length(h)]).areas::<1>(padded_area);
                    self.render_agent_panel(a, buf);
                }
                else {
                    let h = header_h.min(padded_area.height);
                    let [a] = Layout::vertical([Constraint::Length(h)]).areas::<1>(padded_area);
                    self.render_agents_header(a, buf);
                }
            }
            (false, true) => {
                if self.layout.browser_hud_expanded {
                    let h = expanded_target.min(padded_area.height);
                    let [a] = Layout::vertical([Constraint::Length(h)]).areas::<1>(padded_area);
                    self.render_browser_panel(a, buf);
                }
                else {
                    let h = header_h.min(padded_area.height);
                    let [a] = Layout::vertical([Constraint::Length(h)]).areas::<1>(padded_area);
                    self.render_browser_header(a, buf);
                }
            }
            (false, false) => {}
        }
    }

    /// Render the browser panel (left side when both panels are shown)
    fn render_browser_panel(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::widgets::Block;
        use ratatui::widgets::Borders;
        use ratatui::widgets::Widget;

        if let Ok(screenshot_lock) = self.latest_browser_screenshot.lock() {
            if let Some((screenshot_path, url)) = &*screenshot_lock {
                use ratatui::layout::Margin;
                use ratatui::text::{Line as RLine, Span};
                // Use the full area for the browser preview
                let screenshot_block = Block::default()
                    .borders(Borders::ALL)
                    .title(format!(" {} ", self.browser_title()))
                    .border_style(Style::default().fg(crate::colors::border()));

                let inner = screenshot_block.inner(area);
                screenshot_block.render(area, buf);

                // Render a one-line collapsed header inside (with padding), right hint = Collapse
                let line_area = inner.inner(Margin::new(1, 0));
                let header_line = Rect { x: line_area.x, y: line_area.y, width: line_area.width, height: 1 };
                let key_hint_style = Style::default().fg(crate::colors::function());
                let label_style = Style::default().dim();
                let is_active = true;
                let dot_style = if is_active { Style::default().fg(crate::colors::success_green()) } else { Style::default().fg(crate::colors::text_dim()) };
                let mut left_spans: Vec<Span> = Vec::new();
                left_spans.push(Span::styled("•", dot_style));
                // no status text; dot conveys status
                // Spaces between status and URL; no label
                left_spans.push(Span::raw(" "));
                left_spans.push(Span::raw(url.clone()));
                let right_spans: Vec<Span> = vec![
                    Span::from("Ctrl+B").style(key_hint_style),
                    Span::styled(" collapse", label_style),
                ];
                let measure = |spans: &Vec<Span>| -> usize { spans.iter().map(|s| s.content.chars().count()).sum() };
                let left_len = measure(&left_spans);
                let right_len = measure(&right_spans);
                let total_width = line_area.width as usize;
                if total_width > left_len + right_len {
                    let spacer = " ".repeat(total_width - left_len - right_len);
                    left_spans.push(Span::from(spacer));
                }
                let mut spans = left_spans;
                spans.extend(right_spans);
                Paragraph::new(RLine::from(spans)).render(header_line, buf);

                // Leave one blank spacer line, then render the screenshot
                let body = Rect { x: inner.x, y: inner.y + 2, width: inner.width, height: inner.height.saturating_sub(2) };
                self.render_screenshot_highlevel(screenshot_path, body, buf);
            }
        }
    }

    /// Render a collapsed header for the browser HUD with status (1 line + border)
    fn render_browser_header(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::widgets::{Block, Borders, Paragraph};
        use ratatui::text::{Line as RLine, Span};
        use ratatui::layout::Margin;

        let (url_opt, status_str) = {
            let url = self
                .latest_browser_screenshot
                .lock()
                .ok()
                .and_then(|g| g.as_ref().map(|(_, u)| u.clone()));
            let status = self.get_browser_status_string();
            (url, status)
        };
        let title = format!(" {} ", self.browser_title());
        let is_active = url_opt.is_some();
        let summary = match url_opt {
            Some(u) if !u.is_empty() => format!("{}", u),
            _ => status_str,
        };

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()))
            .title(title);
        let inner = block.inner(area);
        block.render(area, buf);
        let content = inner.inner(Margin::new(1, 0)); // 1 space padding inside border

        let key_hint_style = Style::default().fg(crate::colors::function());
        let label_style = Style::default().dim(); // match top status bar label

        // Left side: status dot + text (no label) and URL
        let mut left_spans: Vec<Span> = Vec::new();
        let dot_style = if is_active { Style::default().fg(crate::colors::success_green()) } else { Style::default().fg(crate::colors::text_dim()) };
        left_spans.push(Span::styled("•", dot_style));
        // Choose status text: Active if we have a URL/screenshot, else Idle
        // no status text; dot conveys status
        // Spaces between status and URL; no label
        left_spans.push(Span::raw(" "));
        left_spans.push(Span::raw(summary));

        // Right side: toggle hint based on state
        let action = if self.layout.browser_hud_expanded { " collapse" } else { " expand" };
        let right_spans: Vec<Span> = vec![
            Span::from("Ctrl+B").style(key_hint_style),
            Span::styled(action, label_style),
        ];

        let measure = |spans: &Vec<Span>| -> usize { spans.iter().map(|s| s.content.chars().count()).sum() };
        let left_len = measure(&left_spans);
        let right_len = measure(&right_spans);
        let total_width = content.width as usize;
        let trailing_pad = 0usize; // Paragraph will draw to edge; we already padded left/right
        if total_width > left_len + right_len + trailing_pad {
            let spacer = " ".repeat(total_width - left_len - right_len - trailing_pad);
            left_spans.push(Span::from(spacer));
        }
        let mut spans = left_spans;
        spans.extend(right_spans);
        Paragraph::new(RLine::from(spans)).render(content, buf);
    }

    /// Render a collapsed header for the agents HUD with counts/list (1 line + border)
    fn render_agents_header(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::widgets::{Block, Borders, Paragraph};
        use ratatui::text::{Line as RLine, Span};
        use ratatui::layout::Margin;

        let count = self.active_agents.len();
        let summary = if count == 0 && self.agents_ready_to_start {
            "preparing context".to_string()
        } else if count == 0 {
            "no active agents".to_string()
        } else {
            let mut parts: Vec<String> = Vec::new();
            for a in self.active_agents.iter().take(3) {
                let s = match a.status { AgentStatus::Pending => "pending", AgentStatus::Running => "running", AgentStatus::Completed => "done", AgentStatus::Failed => "failed" };
                parts.push(format!("{} ({})", a.name, s));
            }
            let extra = if count > 3 { format!(" +{}", count - 3) } else { String::new() };
            format!("{}{}", parts.join(", "), extra)
        };

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()))
            .title(" Agents ");
        let inner = block.inner(area);
        block.render(area, buf);
        let content = inner.inner(Margin::new(1, 0)); // 1 space padding inside border

        let key_hint_style = Style::default().fg(crate::colors::function());
        let label_style = Style::default().dim(); // match top status bar label

        // Left side: status dot + text (no label) and Agents summary
        let mut left_spans: Vec<Span> = Vec::new();
        let is_active = !self.active_agents.is_empty() || self.agents_ready_to_start;
        let dot_style = if is_active { Style::default().fg(crate::colors::success_green()) } else { Style::default().fg(crate::colors::text_dim()) };
        left_spans.push(Span::styled("•", dot_style));
        // no status text; dot conveys status
        // single space between dot and summary; no label/separator
        left_spans.push(Span::raw(" "));
        left_spans.push(Span::raw(summary));

        // Right side: toggle hint based on state (Ctrl+A)
        let action = if self.layout.agents_hud_expanded { " collapse" } else { " expand" };
        let right_spans: Vec<Span> = vec![
            Span::from("Ctrl+A").style(key_hint_style),
            Span::styled(action, label_style),
        ];

        let measure = |spans: &Vec<Span>| -> usize { spans.iter().map(|s| s.content.chars().count()).sum() };
        let left_len = measure(&left_spans);
        let right_len = measure(&right_spans);
        let total_width = content.width as usize;
        let trailing_pad = 0usize;
        if total_width > left_len + right_len + trailing_pad {
            let spacer = " ".repeat(total_width - left_len - right_len - trailing_pad);
            left_spans.push(Span::from(spacer));
        }
        let mut spans = left_spans;
        spans.extend(right_spans);
        Paragraph::new(RLine::from(spans)).render(content, buf);
    }

    fn get_browser_status_string(&self) -> String { "Browser".to_string() }

    fn browser_title(&self) -> &'static str {
        if self.browser_is_external { "Chrome" } else { "Browser" }
    }

    /// Render the agent status panel in the HUD
    fn render_agent_panel(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::text::Line as RLine;
        use ratatui::text::Span;
        use ratatui::text::Text;
        use ratatui::widgets::Block;
        use ratatui::widgets::Borders;
        use ratatui::widgets::Paragraph;
        use ratatui::widgets::Sparkline;
        use ratatui::widgets::SparklineBar;
        use ratatui::widgets::Widget;
        use ratatui::widgets::Wrap;

        // Update sparkline data for animation
        if !self.active_agents.is_empty() || self.agents_ready_to_start {
            self.update_sparkline_data();
        }

        // Agent status block
        let agent_block = Block::default()
            .borders(Borders::ALL)
            .title(" Agents ")
            .border_style(Style::default().fg(crate::colors::border()));

        let inner_agent = agent_block.inner(area);
        agent_block.render(area, buf);
        // Render a one-line collapsed header inside expanded panel
        use ratatui::layout::Margin;
        let header_pad = inner_agent.inner(Margin::new(1, 0));
        let header_line = Rect { x: header_pad.x, y: header_pad.y, width: header_pad.width, height: 1 };
        let key_hint_style = Style::default().fg(crate::colors::function());
        let label_style = Style::default().dim();
        let is_active = !self.active_agents.is_empty() || self.agents_ready_to_start;
        let dot_style = if is_active { Style::default().fg(crate::colors::success_green()) } else { Style::default().fg(crate::colors::text_dim()) };
        // Build summary like collapsed header
        let count = self.active_agents.len();
        let summary = if count == 0 && self.agents_ready_to_start { "preparing context".to_string() } else if count == 0 { "no active agents".to_string() } else {
            let mut parts: Vec<String> = Vec::new();
            for a in self.active_agents.iter().take(3) {
                let s = match a.status { AgentStatus::Pending => "pending", AgentStatus::Running => "running", AgentStatus::Completed => "done", AgentStatus::Failed => "failed" };
                parts.push(format!("{} ({})", a.name, s));
            }
            let extra = if count > 3 { format!(" +{}", count - 3) } else { String::new() };
            format!("{}{}", parts.join(", "), extra)
        };
        let mut left_spans: Vec<Span> = Vec::new();
        left_spans.push(Span::styled("•", dot_style));
        // no status text; dot conveys status
        // single space between dot and summary; no label/separator
        left_spans.push(Span::raw(" "));
        left_spans.push(Span::raw(summary));
        let right_spans: Vec<Span> = vec![
            Span::from("Ctrl+A").style(key_hint_style),
            Span::styled(" collapse", label_style),
        ];
        let measure = |spans: &Vec<Span>| -> usize { spans.iter().map(|s| s.content.chars().count()).sum() };
        let left_len = measure(&left_spans);
        let right_len = measure(&right_spans);
        let total_width = header_line.width as usize;
        if total_width > left_len + right_len { left_spans.push(Span::from(" ".repeat(total_width - left_len - right_len))); }
        let mut spans = left_spans; spans.extend(right_spans);
        Paragraph::new(RLine::from(spans)).render(header_line, buf);

        // Body area excludes the header line and a spacer line
        let inner_agent = Rect { x: inner_agent.x, y: inner_agent.y + 2, width: inner_agent.width, height: inner_agent.height.saturating_sub(2) };

        // Dynamically calculate sparkline height based on agent activity
        // More agents = taller sparkline area
        let agent_count = self.active_agents.len();
        let sparkline_height = if agent_count == 0 && self.agents_ready_to_start {
            1u16 // Minimal height when preparing
        } else if agent_count == 0 {
            0u16 // No sparkline when no agents
        } else {
            (agent_count as u16 + 1).min(4) // 2-4 lines based on agent count
        };

        // Ensure we have enough space for both content and sparkline
        // Reserve at least 3 lines for content (status + blank + message)
        let min_content_height = 3u16;
        let available_height = inner_agent.height;

        let (actual_content_height, actual_sparkline_height) = if sparkline_height > 0 {
            if available_height > min_content_height + sparkline_height {
                // Enough space for both
                (
                    available_height.saturating_sub(sparkline_height),
                    sparkline_height,
                )
            } else if available_height > min_content_height {
                // Limited space - give minimum to content, rest to sparkline
                (
                    min_content_height,
                    available_height
                        .saturating_sub(min_content_height)
                        .min(sparkline_height),
                )
            } else {
                // Very limited space - content only
                (available_height, 0)
            }
        } else {
            // No sparkline needed
            (available_height, 0)
        };

        let content_area = Rect {
            x: inner_agent.x,
            y: inner_agent.y,
            width: inner_agent.width,
            height: actual_content_height,
        };
        let sparkline_area = Rect {
            x: inner_agent.x,
            y: inner_agent.y + actual_content_height,
            width: inner_agent.width,
            height: actual_sparkline_height,
        };

        // Build all content into a single Text structure for proper wrapping
        let mut text_content = vec![];

        // Add blank line at the top
        text_content.push(RLine::from(" "));

        // Add overall task status at the top
        let status_color = match self.overall_task_status.as_str() {
            "planning" => crate::colors::warning(),
            "running" => crate::colors::info(),
            "consolidating" => crate::colors::warning(),
            "complete" => crate::colors::success(),
            "failed" => crate::colors::error(),
            _ => crate::colors::text_dim(),
        };

        text_content.push(RLine::from(vec![
            Span::from(" "),
            Span::styled(
                "Status: ",
                Style::default()
                    .fg(crate::colors::text())
                    .add_modifier(Modifier::BOLD),
            ),
            Span::styled(&self.overall_task_status, Style::default().fg(status_color)),
        ]));

        // Add blank line
        text_content.push(RLine::from(" "));

        // Display agent statuses
        if self.agents_ready_to_start && self.active_agents.is_empty() {
            // Show "Building context..." message when agents are expected
            text_content.push(RLine::from(vec![
                Span::from(" "),
                Span::styled(
                    "Building context...",
                    Style::default()
                        .fg(crate::colors::text_dim())
                        .add_modifier(Modifier::ITALIC),
                ),
            ]));
        } else if self.active_agents.is_empty() {
            text_content.push(RLine::from(vec![
                Span::from(" "),
                Span::styled(
                    "No active agents",
                    Style::default().fg(crate::colors::text_dim()),
                ),
            ]));
        } else {
            // Show agent names/models
            for agent in &self.active_agents {
                let status_color = match agent.status {
                    AgentStatus::Pending => crate::colors::warning(),
                    AgentStatus::Running => crate::colors::info(),
                    AgentStatus::Completed => crate::colors::success(),
                    AgentStatus::Failed => crate::colors::error(),
                };

                let status_text = match agent.status {
                    AgentStatus::Pending => "pending",
                    AgentStatus::Running => "running",
                    AgentStatus::Completed => "completed",
                    AgentStatus::Failed => "failed",
                };

                text_content.push(RLine::from(vec![
                    Span::from(" "),
                    Span::styled(
                        format!("{}: ", agent.name),
                        Style::default()
                            .fg(crate::colors::text())
                            .add_modifier(Modifier::BOLD),
                    ),
                    Span::styled(status_text, Style::default().fg(status_color)),
                ]));
            }
        }

        // Calculate how much vertical space the fixed content takes
        let fixed_content_height = text_content.len() as u16;

        // Create the first paragraph for the fixed content (status and agents) without wrapping
        let fixed_paragraph = Paragraph::new(Text::from(text_content));

        // Render the fixed content first
        let fixed_area = Rect {
            x: content_area.x,
            y: content_area.y,
            width: content_area.width,
            height: fixed_content_height.min(content_area.height),
        };
        fixed_paragraph.render(fixed_area, buf);

        // Calculate remaining area for wrapped content
        let remaining_height = content_area.height.saturating_sub(fixed_content_height);
        if remaining_height > 0 {
            let wrapped_area = Rect {
                x: content_area.x,
                y: content_area.y + fixed_content_height,
                width: content_area.width,
                height: remaining_height,
            };

            // Add context and task sections with proper wrapping in the remaining area
            let mut wrapped_content = vec![];

            if let Some(ref task) = self.agent_task {
                wrapped_content.push(RLine::from(" ")); // Empty line separator
                wrapped_content.push(RLine::from(vec![
                    Span::from(" "),
                    Span::styled(
                        "Task:",
                        Style::default()
                            .fg(crate::colors::text())
                            .add_modifier(Modifier::BOLD),
                    ),
                    Span::from(" "),
                    Span::styled(task, Style::default().fg(crate::colors::text_dim())),
                ]));
            }

            if !wrapped_content.is_empty() {
                // Create paragraph with wrapping enabled for the long text content
                let wrapped_paragraph =
                    Paragraph::new(Text::from(wrapped_content)).wrap(Wrap { trim: false });
                wrapped_paragraph.render(wrapped_area, buf);
            }
        }

        // Render sparkline at the bottom if we have data and agents are active
        let sparkline_data = self.sparkline_data.borrow();

        // Debug logging
        tracing::debug!(
            "Sparkline render check: data_len={}, agents={}, ready={}, height={}, actual_height={}, area={:?}",
            sparkline_data.len(),
            self.active_agents.len(),
            self.agents_ready_to_start,
            sparkline_height,
            actual_sparkline_height,
            sparkline_area
        );

        if !sparkline_data.is_empty()
            && (!self.active_agents.is_empty() || self.agents_ready_to_start)
            && actual_sparkline_height > 0
        {
            // Convert data to SparklineBar with colors based on completion status
            let bars: Vec<SparklineBar> = sparkline_data
                .iter()
                .map(|(value, is_completed)| {
                    let color = if *is_completed {
                        crate::colors::success() // Green for completed
                    } else {
                        crate::colors::border() // Border color for normal activity
                    };
                    SparklineBar::from(*value).style(Style::default().fg(color))
                })
                .collect();

            // Use dynamic max based on the actual data for better visibility
            // During preparing/planning, values are small (2-3), during running they're larger (5-15)
            // For planning phase with single line, use smaller max for better visibility
            let max_value = if self.agents_ready_to_start && self.active_agents.is_empty() {
                // Planning phase - use smaller max for better visibility of 1-3 range
                sparkline_data
                    .iter()
                    .map(|(v, _)| *v)
                    .max()
                    .unwrap_or(4)
                    .max(4)
            } else {
                // Running phase - use larger max
                sparkline_data
                    .iter()
                    .map(|(v, _)| *v)
                    .max()
                    .unwrap_or(10)
                    .max(10)
            };

            let sparkline = Sparkline::default().data(bars).max(max_value); // Dynamic max for better visibility
            sparkline.render(sparkline_area, buf);
        }
    }

}

impl WidgetRef for &ChatWidget<'_> {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        // Top-level widget render timing
        let _perf_widget_start = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };

        // Ensure a consistent background even when individual widgets skip
        // painting unchanged regions. Without this, gutters and inter‑cell
        // spacing can show through after we reduced full clears.
        // Cost: one Block render across the frame (O(area)); acceptable and
        // fixes visual artifacts reported after redraw reductions.
        {
            use ratatui::style::Style;
            use ratatui::widgets::Block;
            let bg = Block::default().style(Style::default().bg(crate::colors::background()));
            bg.render(area, buf);
        }

        // Remember full frame height for HUD sizing logic
        self.layout.last_frame_height.set(area.height);

        let layout_areas = self.layout_areas(area);
        let (status_bar_area, hud_area, history_area, bottom_pane_area) = if layout_areas.len() == 4
        {
            // Browser HUD is present
            (
                layout_areas[0],
                Some(layout_areas[1]),
                layout_areas[2],
                layout_areas[3],
            )
        } else {
            // No browser HUD
            (layout_areas[0], None, layout_areas[1], layout_areas[2])
        };

        // Render status bar
        self.render_status_bar(status_bar_area, buf);

        // Render HUD if present (browser and/or agents)
        if let Some(hud_area) = hud_area {
            self.render_hud(hud_area, buf);
        }

        // Create a unified scrollable container for all chat content
        // Use consistent padding throughout
        let padding = 1u16;
        let content_area = Rect {
            x: history_area.x + padding,
            y: history_area.y,
            width: history_area.width.saturating_sub(padding * 2),
            height: history_area.height,
        };

        // Collect all content items into a single list
        let mut all_content: Vec<&dyn HistoryCell> = Vec::new();
        for cell in self.history_cells.iter() {
            all_content.push(cell);
        }

        // Add active/streaming cell if present
        if let Some(ref cell) = self.active_exec_cell {
            all_content.push(cell as &dyn HistoryCell);
        }

        // Add live streaming content if present
        let streaming_lines = self
            .live_builder
            .display_rows()
            .into_iter()
            .map(|r| ratatui::text::Line::from(r.text))
            .collect::<Vec<_>>();

        let streaming_cell = if !streaming_lines.is_empty() {
            Some(history_cell::new_streaming_content(streaming_lines))
        } else {
            None
        };

        if let Some(ref cell) = streaming_cell {
            all_content.push(cell);
        }

        // Append any queued user messages as sticky preview cells at the very
        // end so they always render at the bottom until they are dispatched.
        let mut queued_preview_cells: Vec<crate::history_cell::PlainHistoryCell> = Vec::new();
        if !self.queued_user_messages.is_empty() {
            for qm in &self.queued_user_messages {
                queued_preview_cells.push(crate::history_cell::new_queued_user_prompt(qm.display_text.clone()));
            }
            for c in &queued_preview_cells { all_content.push(c as &dyn HistoryCell); }
        }

        // Calculate total content height using prefix sums; build if needed
        let spacing = 1u16; // Standard spacing between cells
        const GUTTER_WIDTH: u16 = 2; // Same as in render loop

        // Opportunistically clear height cache if width changed
        if self.height_cache_last_width.get() != content_area.width {
            self.height_cache.borrow_mut().clear();
            self.prefix_sums.borrow_mut().clear();
            self.prefix_valid.set(false);
            self.height_cache_last_width.set(content_area.width);
        }

        // Perf: count a frame
        if self.perf_state.enabled {
            let mut p = self.perf_state.stats.borrow_mut();
            p.frames = p.frames.saturating_add(1);
        }

        // Detect dynamic content that requires per-frame recomputation
        let has_active_animation_early = self.history_cells.iter().any(|cell| cell.is_animating());
        let must_rebuild_prefix = !self.prefix_valid.get()
            || self.last_prefix_width.get() != content_area.width
            || self.last_prefix_count.get() != all_content.len()
            || streaming_cell.is_some()
            || has_active_animation_early;

        let total_height: u16 = if must_rebuild_prefix {
            let perf_enabled = self.perf_state.enabled;
            let total_start = if perf_enabled { Some(std::time::Instant::now()) } else { None };
            let mut ps = self.prefix_sums.borrow_mut();
            ps.clear();
            ps.push(0);
            let mut acc = 0u16;
            if perf_enabled {
                let mut p = self.perf_state.stats.borrow_mut();
                p.prefix_rebuilds = p.prefix_rebuilds.saturating_add(1);
            }
            for (idx, item) in all_content.iter().enumerate() {
                let content_width = content_area.width.saturating_sub(GUTTER_WIDTH);
                // Cache heights for most items. Also allow caching for ExecCell once completed
                // (custom_render but stable), to avoid repeated wrapping/measure.
                let is_stable_exec = item
                    .as_any()
                    .downcast_ref::<crate::history_cell::ExecCell>()
                    .map(|e| e.output.is_some())
                    .unwrap_or(false);
                // Assistant markdown cells are static once built; cache their heights
                let is_assistant_static = item
                    .as_any()
                    .downcast_ref::<crate::history_cell::AssistantMarkdownCell>()
                    .is_some();
                let is_streaming = item
                    .as_any()
                    .downcast_ref::<crate::history_cell::StreamingContentCell>()
                    .is_some();
                let is_cacheable = ((!item.has_custom_render()) || is_stable_exec || is_assistant_static)
                    && !item.is_animating()
                    && !is_streaming;
                let h = if is_cacheable {
                    let key = (idx, content_width);
                    // Take an immutable borrow in a small scope to avoid overlapping with the later mutable borrow
                    let cached_val = {
                        let cache_ref = self.height_cache.borrow();
                        cache_ref.get(&key).copied()
                    };
                    if let Some(cached) = cached_val {
                        if perf_enabled {
                            let mut p = self.perf_state.stats.borrow_mut();
                            p.height_hits_total = p.height_hits_total.saturating_add(1);
                        }
                        cached
                    } else {
                        if perf_enabled {
                            let mut p = self.perf_state.stats.borrow_mut();
                            p.height_misses_total = p.height_misses_total.saturating_add(1);
                        }
                        let label = if perf_enabled { Some(self.perf_label_for_item(*item)) } else { None };
                        let t0 = if perf_enabled { Some(std::time::Instant::now()) } else { None };
                        let computed = item.desired_height(content_width);
                        if let (true, Some(start)) = (perf_enabled, t0) {
                            let dt = start.elapsed().as_nanos();
                            let mut p = self.perf_state.stats.borrow_mut();
                            p.record_total((idx, content_width), label.as_deref().unwrap_or("unknown"), dt);
                        }
                        // Now take a mutable borrow to insert
                        self.height_cache.borrow_mut().insert(key, computed);
                        computed
                    }
                } else {
                    item.desired_height(content_width)
                };
                acc = acc.saturating_add(h);
                // Spacing rule must mirror the render path: no spacer between
                // adjacent collapsed reasoning cells.
                let mut should_add_spacing = idx < all_content.len() - 1 && h > 0;
                if should_add_spacing {
                    let this_is_collapsed_reasoning = item
                        .as_any()
                        .downcast_ref::<crate::history_cell::CollapsibleReasoningCell>()
                        .map(|rc| rc.is_collapsed())
                        .unwrap_or(false);
                    if this_is_collapsed_reasoning {
                        if let Some(next_item) = all_content.get(idx + 1) {
                            let next_is_collapsed_reasoning = next_item
                                .as_any()
                                .downcast_ref::<crate::history_cell::CollapsibleReasoningCell>()
                                .map(|rc| rc.is_collapsed())
                                .unwrap_or(false);
                            if next_is_collapsed_reasoning {
                                should_add_spacing = false;
                            }
                        }
                    }
                }
                if should_add_spacing {
                    acc = acc.saturating_add(spacing);
                }
                ps.push(acc);
            }
            let total = *ps.last().unwrap_or(&0);
            if let Some(start) = total_start {
                if self.perf_state.enabled {
                    let mut p = self.perf_state.stats.borrow_mut();
                    p.ns_total_height = p.ns_total_height.saturating_add(start.elapsed().as_nanos());
                }
            }
            // Update cache keys
            self.last_prefix_width.set(content_area.width);
            self.last_prefix_count.set(all_content.len());
            self.prefix_valid.set(true);
            total
        } else {
            // Use cached prefix sums
            *self.prefix_sums.borrow().last().unwrap_or(&0)
        };

        // Check for active animations using the trait method
        let has_active_animation = self.history_cells.iter().any(|cell| cell.is_animating());

        if has_active_animation {
            tracing::debug!("Active animation detected, scheduling next frame");
            // Lower animation cadence to reduce CPU while remaining smooth in terminals.
            // ~50ms ≈ 20 FPS is typically sufficient.
            self.app_event_tx
                .send(AppEvent::ScheduleFrameIn(std::time::Duration::from_millis(50)));
        }

        // Calculate scroll position and vertical alignment
        // Stabilize viewport when input area height changes while scrolled up.
        let prev_viewport_h = self.layout.last_history_viewport_height.get();
        if prev_viewport_h == 0 {
            // Initialize on first render
            self.layout.last_history_viewport_height.set(content_area.height);
        }

        let (start_y, scroll_pos) = if total_height <= content_area.height {
            // Content fits - always align to bottom so "Popular commands" stays at the bottom
            let start_y = content_area.y + content_area.height.saturating_sub(total_height);
            // Update last_max_scroll cache
            self.layout.last_max_scroll.set(0);
            (start_y, 0u16) // No scrolling needed
        } else {
            // Content overflows - calculate scroll position
            // scroll_offset is measured from the bottom (0 = bottom/newest)
            // Convert to distance from the top for rendering math.
            let max_scroll = total_height.saturating_sub(content_area.height);
            // Update cache and clamp for display only
            self.layout.last_max_scroll.set(max_scroll);
            let clamped_scroll_offset = self.layout.scroll_offset.min(max_scroll);
            let mut scroll_from_top = max_scroll.saturating_sub(clamped_scroll_offset);

            // Viewport stabilization: when user is scrolled up (offset > 0) and the
            // history viewport height changes due to the input area growing/shrinking,
            // adjust the scroll_from_top to keep the top line steady on screen.
            if clamped_scroll_offset > 0 {
                let prev_h = prev_viewport_h as i32;
                let curr_h = content_area.height as i32;
                let delta_h = prev_h - curr_h; // positive if viewport shrank
                if delta_h != 0 {
                    // Adjust in the opposite direction to keep the same top anchor
                    let sft = scroll_from_top as i32 - delta_h;
                    let sft = sft.clamp(0, max_scroll as i32) as u16;
                    scroll_from_top = sft;
                }
            }

            (content_area.y, scroll_from_top)
        };

        // Record current viewport height for the next frame
        self.layout.last_history_viewport_height.set(content_area.height);

        // Targeted clears: only pad stripes and any top gap inside content area.
        let clear_style = Style::default()
            .bg(crate::colors::background())
            .fg(crate::colors::text());
        let _perf_hist_clear_start = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };
        let mut cleared_cells: u64 = 0;
        // Left/right padding stripes
        let left_pad_w = content_area.x.saturating_sub(history_area.x);
        let right_pad_start = content_area.x.saturating_add(content_area.width);
        let right_pad_w = history_area
            .x
            .saturating_add(history_area.width)
            .saturating_sub(right_pad_start);
        if left_pad_w > 0 {
            for y in history_area.y..history_area.y.saturating_add(history_area.height) {
                for x in history_area.x..history_area.x.saturating_add(left_pad_w) {
                    buf[(x, y)].set_char(' ').set_style(clear_style);
                }
            }
            cleared_cells = cleared_cells.saturating_add((left_pad_w as u64) * (history_area.height as u64));
        }
        if right_pad_w > 0 {
            for y in history_area.y..history_area.y.saturating_add(history_area.height) {
                for x in right_pad_start..right_pad_start.saturating_add(right_pad_w) {
                    buf[(x, y)].set_char(' ').set_style(clear_style);
                }
            }
            cleared_cells = cleared_cells.saturating_add((right_pad_w as u64) * (history_area.height as u64));
        }
        // Top gap inside content area when content is bottom-aligned
        if start_y > content_area.y {
            let gap_h = start_y.saturating_sub(content_area.y);
            for y in content_area.y..content_area.y.saturating_add(gap_h) {
                for x in content_area.x..content_area.x.saturating_add(content_area.width) {
                    buf[(x, y)].set_char(' ').set_style(clear_style);
                }
            }
            cleared_cells = cleared_cells.saturating_add((gap_h as u64) * (content_area.width as u64));
        }
        if let Some(t0) = _perf_hist_clear_start {
            let dt = t0.elapsed().as_nanos();
            let mut p = self.perf_state.stats.borrow_mut();
            p.ns_history_clear = p.ns_history_clear.saturating_add(dt);
            p.cells_history_clear = p.cells_history_clear.saturating_add(cleared_cells);
        }

        // Render the scrollable content with spacing using prefix sums
        let mut screen_y = start_y; // Position on screen
        let spacing = 1u16; // Spacing between cells
        let viewport_bottom = scroll_pos.saturating_add(content_area.height);
        let ps = self.prefix_sums.borrow();
        let mut start_idx = match ps.binary_search(&scroll_pos) {
            Ok(i) => i,
            Err(i) => i.saturating_sub(1),
        };
        start_idx = start_idx.min(all_content.len());
        let mut end_idx = match ps.binary_search(&viewport_bottom) {
            Ok(i) => i,
            Err(i) => i,
        };
        // Extend end_idx by one to include the next item when the viewport cuts into spacing
        end_idx = end_idx.saturating_add(1).min(all_content.len());

        let render_loop_start = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };
        for idx in start_idx..end_idx {
            let item = all_content[idx];
            // Calculate height with reduced width due to gutter
            const GUTTER_WIDTH: u16 = 2;
            let content_width = content_area.width.saturating_sub(GUTTER_WIDTH);
            // Height from cache if possible
            // Cache heights for most items. Also allow caching for completed ExecCell (stable).
            let is_stable_exec = item
                .as_any()
                .downcast_ref::<crate::history_cell::ExecCell>()
                .map(|e| e.output.is_some())
                .unwrap_or(false);
            let is_streaming = item
                .as_any()
                .downcast_ref::<crate::history_cell::StreamingContentCell>()
                .is_some();
            let is_cacheable = ((!item.has_custom_render()) || is_stable_exec)
                && !item.is_animating()
                && !is_streaming;
            let item_height = if is_cacheable {
                let key = (idx, content_width);
                if let Some(cached) = self.height_cache.borrow().get(&key).copied() {
                    if self.perf_state.enabled {
                        let mut p = self.perf_state.stats.borrow_mut();
                        p.height_hits_render = p.height_hits_render.saturating_add(1);
                    }
                    cached
                } else {
                    if self.perf_state.enabled {
                        let mut p = self.perf_state.stats.borrow_mut();
                        p.height_misses_render = p.height_misses_render.saturating_add(1);
                    }
                    let label = if self.perf_state.enabled { Some(self.perf_label_for_item(item)) } else { None };
                    let t0 = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };
                    let computed = item.desired_height(content_width);
                    if let (true, Some(start)) = (self.perf_state.enabled, t0) {
                        let dt = start.elapsed().as_nanos();
                        let mut p = self.perf_state.stats.borrow_mut();
                        p.record_render((idx, content_width), label.as_deref().unwrap_or("unknown"), dt);
                    }
                    self.height_cache.borrow_mut().insert(key, computed);
                    computed
                }
            } else {
                item.desired_height(content_width)
            };

            let content_y = ps[idx];

            // Targeted bottom-row spacer compensation:
            // If we're at the very bottom and the last item starts just after the
            // spacer row, nudge the draw cursor down by at most that spacer (1 row).
            // Previously we used the full `gap = content_y - scroll_pos`, which could
            // be many rows and push the cursor past the viewport, making the bottom
            // appear blank. Clamp strictly to the spacer size.
            if viewport_bottom == total_height && idx == end_idx.saturating_sub(1) {
                let gap = content_y.saturating_sub(scroll_pos);
                if gap > 0 && gap <= spacing { // only compensate a single spacer row
                    let remaining = (content_area.y + content_area.height).saturating_sub(screen_y);
                    let shift = spacing.min(remaining);
                    screen_y = screen_y.saturating_add(shift);
                }
            }

            let skip_top = if content_y < scroll_pos { scroll_pos - content_y } else { 0 };

            // Stop if we've gone past the bottom of the screen
            if screen_y >= content_area.y + content_area.height {
                break;
            }

            // Calculate how much height is available for this item
            let available_height = (content_area.y + content_area.height).saturating_sub(screen_y);
            let visible_height = item_height.saturating_sub(skip_top).min(available_height);

            if visible_height > 0 {
                // Define gutter width (2 chars: symbol + space)
                const GUTTER_WIDTH: u16 = 2;
                
                // Split area into gutter and content
                let gutter_area = Rect {
                    x: content_area.x,
                    y: screen_y,
                    width: GUTTER_WIDTH.min(content_area.width),
                    height: visible_height,
                };
                
                let item_area = Rect {
                    x: content_area.x + GUTTER_WIDTH.min(content_area.width),
                    y: screen_y,
                    width: content_area.width.saturating_sub(GUTTER_WIDTH),
                    height: visible_height,
                };

                // Paint gutter background. For Assistant, extend the assistant tint under the
                // gutter and also one extra column to the left (so the • has color on both sides),
                // without changing layout or symbol positions.
                let is_assistant = matches!(item.kind(), crate::history_cell::HistoryCellType::Assistant);
                let gutter_bg = if is_assistant {
                    crate::colors::assistant_bg()
                } else {
                    crate::colors::background()
                };

                // Paint gutter background for assistant cells so the tinted
                // strip appears contiguous with the message body. This avoids
                // the light "hole" seen after we reduced redraws. For other
                // cell types keep the default background (already painted by
                // the frame bg fill above).
                if is_assistant && gutter_area.width > 0 && gutter_area.height > 0 {
                    let _perf_gutter_start = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };
                    let style = Style::default().bg(gutter_bg);
                    for y in gutter_area.y..gutter_area.y.saturating_add(gutter_area.height) {
                        // Only the first column (symbol column) needs tint; the second is spacing to content
                        // but tint both for visual continuity with the assistant block.
                        for x in gutter_area.x..gutter_area.x.saturating_add(gutter_area.width) {
                            buf[(x, y)].set_char(' ').set_style(style);
                        }
                    }
                    // Also tint the single left padding column so the assistant
                    // gutter visually reaches the outer edge. The content area
                    // is inset by a uniform padding; when present, paint that
                    // one column with the same assistant background for the
                    // vertical span of this item.
                    if content_area.x > history_area.x {
                        let left_col_x = content_area.x.saturating_sub(1);
                        for y in gutter_area.y..gutter_area.y.saturating_add(gutter_area.height) {
                            buf[(left_col_x, y)].set_char(' ').set_style(style);
                        }
                    }
                    // Also tint one column immediately to the right of the content area
                    // so the assistant block is visually bookended. This column lives in the
                    // right padding stripe; when the scrollbar is visible it will draw over
                    // the far-right edge, which is fine.
                    let right_col_x = content_area.x.saturating_add(content_area.width);
                    let history_right = history_area.x.saturating_add(history_area.width);
                    if right_col_x < history_right {
                        for y in item_area.y..item_area.y.saturating_add(item_area.height) {
                            buf[(right_col_x, y)].set_char(' ').set_style(style);
                        }
                    }
                    if let Some(t0) = _perf_gutter_start {
                        let dt = t0.elapsed().as_nanos();
                        let mut p = self.perf_state.stats.borrow_mut();
                        p.ns_gutter_paint = p.ns_gutter_paint.saturating_add(dt);
                        // Rough accounting: area of gutter rectangle (clamped to u64)
                        let area_cells: u64 = (gutter_area.width as u64).saturating_mul(gutter_area.height as u64);
                        p.cells_gutter_paint = p.cells_gutter_paint.saturating_add(area_cells);
                    }
                }

                // Render gutter symbol if present
                if let Some(symbol) = item.gutter_symbol() {
                    // Choose color based on symbol/type
                    let color = if symbol == "❯" {
                        // Executed arrow – color reflects exec state
                        if let Some(exec) = item.as_any().downcast_ref::<crate::history_cell::ExecCell>() {
                            match &exec.output {
                                None => crate::colors::info(),              // Running...
                                // On successful completion, turn the gutter arrow solid black
                                Some(o) if o.exit_code == 0 => ratatui::style::Color::Black, // Ran
                                Some(_) => crate::colors::error(),
                            }
                        } else {
                            // Handle merged exec cells (multi-block "Ran") the same as single execs
                            match item.kind() {
                                crate::history_cell::HistoryCellType::Exec { kind: crate::history_cell::ExecKind::Run, status: crate::history_cell::ExecStatus::Success } => ratatui::style::Color::Black,
                                crate::history_cell::HistoryCellType::Exec { kind: crate::history_cell::ExecKind::Run, status: crate::history_cell::ExecStatus::Error } => crate::colors::error(),
                                crate::history_cell::HistoryCellType::Exec { .. } => crate::colors::info(),
                                _ => crate::colors::info(),
                            }
                        }
                    } else if symbol == "↯" {
                        // Patch/Updated arrow color – match the header text color
                        match item.kind() {
                            crate::history_cell::HistoryCellType::Patch { kind: crate::history_cell::PatchKind::ApplySuccess } => crate::colors::success(),
                            crate::history_cell::HistoryCellType::Patch { kind: crate::history_cell::PatchKind::ApplyBegin } => crate::colors::success(),
                            crate::history_cell::HistoryCellType::Patch { kind: crate::history_cell::PatchKind::Proposed } => crate::colors::primary(),
                            crate::history_cell::HistoryCellType::Patch { kind: crate::history_cell::PatchKind::ApplyFailure } => crate::colors::error(),
                            _ => crate::colors::primary(),
                        }
                    } else {
                        match symbol {
                            "›" => crate::colors::text(),        // user
                            "⋮" => crate::colors::primary(),     // thinking
                            "•" => crate::colors::text_bright(),  // codex/agent
                            "⚙" => crate::colors::info(),         // tool working
                            "✔" => crate::colors::success(),      // tool complete
                            "✖" => crate::colors::error(),        // error
                            "★" => crate::colors::text_bright(),  // notice/popular
                            _ => crate::colors::text_dim(),
                        }
                    };

                    // Draw the symbol anchored to the top of the message (not the viewport).
                    // "Top of the message" accounts for any intentional top padding per cell type.
                    // As you scroll past that anchor, the icon scrolls away with the message.
                    if gutter_area.width >= 2 {
                        // Anchor offset counted from the very start of the item's painted area
                        // to the first line of its content that the icon should align with.
                        let anchor_offset: u16 = match item.kind() {
                            // Assistant messages render with one row of top padding so that
                            // the content visually aligns; anchor to that second row.
                            crate::history_cell::HistoryCellType::Assistant => 1,
                            _ => 0,
                        };

                        // If we've scrolled past the anchor line, don't render the icon.
                        if skip_top <= anchor_offset {
                            let rel = anchor_offset - skip_top; // rows from current viewport top
                            let symbol_y = gutter_area.y.saturating_add(rel);
                            if symbol_y < gutter_area.y.saturating_add(gutter_area.height) {
                                let symbol_style = Style::default().fg(color).bg(gutter_bg);
                                buf.set_string(gutter_area.x, symbol_y, symbol, symbol_style);
                            }
                        }
                    }
                }

                // Render only the visible window of the item using vertical skip
                let skip_rows = skip_top;
                
                // Log all cells being rendered
                let is_animating = item.is_animating();
                let has_custom = item.has_custom_render();
                
                
                if is_animating || has_custom {
                    tracing::debug!(
                        ">>> RENDERING ANIMATION Cell[{}]: area={:?}, skip_rows={}",
                        idx, item_area, skip_rows
                    );
                }
                
                // Render the cell content first
                item.render_with_skip(item_area, buf, skip_rows);

                // Debug: overlay order info on the spacing row below (or above if needed).
                if self.show_order_overlay {
                    if let Some(Some(info)) = self.cell_order_dbg.get(idx) {
                        let mut text = format!("⟦{}⟧", info);
                        // Live reasoning diagnostics: append current title detection snapshot
                        if let Some(rc) = item.as_any().downcast_ref::<crate::history_cell::CollapsibleReasoningCell>() {
                            let snap = rc.debug_title_overlay();
                            text.push_str(" | ");
                            text.push_str(&snap);
                        }
                        let style = Style::default().fg(crate::colors::text_dim());
                        // Prefer below the item in the one-row spacing area
                        let below_y = item_area.y.saturating_add(visible_height);
                        let bottom_y = content_area.y.saturating_add(content_area.height);
                        let maxw = item_area.width as usize;
                        // Truncate safely by display width, not by bytes, to avoid
                        // panics on non-UTF-8 boundaries (e.g., emoji/CJK). Use the
                        // same width logic as our live wrap utilities.
                        let draw_text = {
                            use unicode_width::UnicodeWidthStr as _;
                            if text.width() > maxw {
                                crate::live_wrap::take_prefix_by_width(&text, maxw).0
                            } else {
                                text.clone()
                            }
                        };
                        if item_area.width > 0 {
                            if below_y < bottom_y {
                                buf.set_string(item_area.x, below_y, draw_text.clone(), style);
                            } else if item_area.y > content_area.y {
                                // Fall back to above the item if no space below
                                let above_y = item_area.y.saturating_sub(1);
                                buf.set_string(item_area.x, above_y, draw_text.clone(), style);
                            }
                        }
                    }
                }
                screen_y += visible_height;
            }

            // Add spacing only if something was actually rendered for this item.
            // Prevent a stray blank when zero-height, and suppress spacing between
            // consecutive collapsed reasoning titles so they appear as a tight list.
            let mut should_add_spacing = idx < all_content.len() - 1 && visible_height > 0;
            if should_add_spacing {
                // Special-case: two adjacent collapsed reasoning cells → no spacer.
                let this_is_collapsed_reasoning = item
                    .as_any()
                    .downcast_ref::<crate::history_cell::CollapsibleReasoningCell>()
                    .map(|rc| rc.is_collapsed())
                    .unwrap_or(false);
                if this_is_collapsed_reasoning {
                    if let Some(next_item) = all_content.get(idx + 1) {
                        let next_is_collapsed_reasoning = next_item
                            .as_any()
                            .downcast_ref::<crate::history_cell::CollapsibleReasoningCell>()
                            .map(|rc| rc.is_collapsed())
                            .unwrap_or(false);
                        if next_is_collapsed_reasoning {
                            should_add_spacing = false;
                        }
                    }
                }
            }
            if should_add_spacing {
                if screen_y < content_area.y + content_area.height {
                    screen_y += spacing.min((content_area.y + content_area.height).saturating_sub(screen_y));
                }
            }
        }
        if let Some(start) = render_loop_start {
            if self.perf_state.enabled {
                let mut p = self.perf_state.stats.borrow_mut();
                p.ns_render_loop = p.ns_render_loop.saturating_add(start.elapsed().as_nanos());
            }
        }

        // Clear any bottom gap inside the content area that wasn’t covered by items
        if screen_y < content_area.y + content_area.height {
            let _perf_hist_clear2 = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };
            for y in screen_y..content_area.y + content_area.height {
                for x in content_area.x..content_area.x + content_area.width {
                    buf[(x, y)].set_char(' ').set_style(clear_style);
                }
            }
            if let Some(t0) = _perf_hist_clear2 {
                let dt = t0.elapsed().as_nanos();
                let mut p = self.perf_state.stats.borrow_mut();
                p.ns_history_clear = p.ns_history_clear.saturating_add(dt);
                let cells = (content_area.width as u64)
                    * ((content_area.y + content_area.height - screen_y) as u64);
                p.cells_history_clear = p.cells_history_clear.saturating_add(cells);
            }
        }

        // Render vertical scrollbar when content is scrollable and currently visible
        // Auto-hide after a short delay to avoid copying it along with text.
        let now = std::time::Instant::now();
        let show_scrollbar = total_height > content_area.height
            && self
                .layout.scrollbar_visible_until
                .get()
                .map(|t| now < t)
                .unwrap_or(false);
        if show_scrollbar {
            let mut sb_state = self.layout.vertical_scrollbar_state.borrow_mut();
            // Scrollbar expects number of scroll positions, not total rows.
            // For a viewport of H rows and content of N rows, there are
            // max_scroll = N - H positions; valid positions = [0, max_scroll].
            let max_scroll = total_height.saturating_sub(content_area.height);
            let scroll_positions = max_scroll.saturating_add(1).max(1) as usize;
            let pos = scroll_pos.min(max_scroll) as usize;
            *sb_state = sb_state.content_length(scroll_positions).position(pos);
            // Theme-aware scrollbar styling (line + block)
            // Track: thin line using border color; Thumb: block using border_focused.
            let theme = crate::theme::current_theme();
            let sb = Scrollbar::new(ScrollbarOrientation::VerticalRight)
                .symbols(scrollbar_symbols::VERTICAL)
                .begin_symbol(None)
                .end_symbol(None)
                .track_symbol(Some("│"))
                .track_style(Style::default().fg(crate::colors::border()).bg(crate::colors::background()))
                .thumb_symbol("█")
                .thumb_style(Style::default().fg(theme.border_focused).bg(crate::colors::background()));
            // To avoid a small jump at the bottom due to spacer toggling,
            // render the scrollbar in a slightly shorter area (reserve 1 row).
            let sb_area = Rect {
                x: history_area.x,
                y: history_area.y,
                width: history_area.width,
                height: history_area.height.saturating_sub(1),
            };
            StatefulWidget::render(sb, sb_area, buf, &mut sb_state);
        }

        // Render the bottom pane directly without a border for now
        // The composer has its own layout with hints at the bottom
        (&self.bottom_pane).render(bottom_pane_area, buf);

        // Welcome animation is kept as a normal cell in history; no overlay.

        // The welcome animation is no longer rendered as an overlay.

        // Render diff overlay (covering the history area, aligned with padding) if active
        if let Some(overlay) = &self.diffs.overlay {
            // Global scrim: dim the whole background to draw focus to the viewer
            // We intentionally do this across the entire widget area rather than just the
            // history area so the viewer stands out even with browser HUD or status bars.
            let scrim_bg = Style::default()
                .bg(crate::colors::overlay_scrim())
                .fg(crate::colors::text_dim());
            let _perf_scrim_start = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };
            for y in area.y..area.y + area.height {
                for x in area.x..area.x + area.width {
                    // Overwrite with a dimmed style; we don't Clear so existing glyphs remain,
                    // but foreground is muted to reduce visual competition.
                    buf[(x, y)].set_style(scrim_bg);
                }
            }
            if let Some(t0) = _perf_scrim_start {
                let dt = t0.elapsed().as_nanos();
                let mut p = self.perf_state.stats.borrow_mut();
                p.ns_overlay_scrim = p.ns_overlay_scrim.saturating_add(dt);
                let cells = (area.width as u64) * (area.height as u64);
                p.cells_overlay_scrim = p.cells_overlay_scrim.saturating_add(cells);
            }
            // Match the horizontal padding used by status bar and input
            let padding = 1u16;
            let area = Rect {
                x: history_area.x + padding,
                y: history_area.y,
                width: history_area.width.saturating_sub(padding * 2),
                height: history_area.height,
            };

            // Clear and repaint the overlay area with theme scrim background
            Clear.render(area, buf);
            let bg_style = Style::default().bg(crate::colors::overlay_scrim());
            let _perf_overlay_area_bg_start = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };
            for y in area.y..area.y + area.height {
                for x in area.x..area.x + area.width {
                    buf[(x, y)].set_style(bg_style);
                }
            }
            if let Some(t0) = _perf_overlay_area_bg_start {
                let dt = t0.elapsed().as_nanos();
                let mut p = self.perf_state.stats.borrow_mut();
                p.ns_overlay_body_bg = p.ns_overlay_body_bg.saturating_add(dt);
                let cells = (area.width as u64) * (area.height as u64);
                p.cells_overlay_body_bg = p.cells_overlay_body_bg.saturating_add(cells);
            }

            // Build a styled title: keys/icons in normal text color; descriptors and dividers dim
            let t_dim = Style::default().fg(crate::colors::text_dim());
            let t_fg = Style::default().fg(crate::colors::text());
            let has_tabs = overlay.tabs.len() > 1;
            let mut title_spans: Vec<ratatui::text::Span<'static>> = vec![
                ratatui::text::Span::styled(" ", t_dim),
                ratatui::text::Span::styled("Diff viewer", t_fg),
            ];
            if has_tabs {
                title_spans.extend_from_slice(&[
                    ratatui::text::Span::styled(" ——— ", t_dim),
                    ratatui::text::Span::styled("◂ ▸", t_fg),
                    ratatui::text::Span::styled(" change tabs ", t_dim),
                ]);
            }
            title_spans.extend_from_slice(&[
                ratatui::text::Span::styled("——— ", t_dim),
                ratatui::text::Span::styled("e", t_fg),
                ratatui::text::Span::styled(" explain ", t_dim),
                ratatui::text::Span::styled("——— ", t_dim),
                ratatui::text::Span::styled("u", t_fg),
                ratatui::text::Span::styled(" undo ", t_dim),
                ratatui::text::Span::styled("——— ", t_dim),
                ratatui::text::Span::styled("Esc", t_fg),
                ratatui::text::Span::styled(" close ", t_dim),
            ]);
            let block = Block::default()
                .borders(Borders::ALL)
                .title(ratatui::text::Line::from(title_spans))
                // Use normal background for the window itself so it contrasts against the
                // dimmed scrim behind
                .style(Style::default().bg(crate::colors::background()))
                .border_style(
                    Style::default()
                        .fg(crate::colors::border())
                        .bg(crate::colors::background()),
                );
            let inner = block.inner(area);
            block.render(area, buf);

            // Paint inner content background as the normal theme background
            let inner_bg = Style::default().bg(crate::colors::background());
            let _perf_overlay_inner_bg_start = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };
            for y in inner.y..inner.y + inner.height {
                for x in inner.x..inner.x + inner.width {
                    buf[(x, y)].set_style(inner_bg);
                }
            }
            if let Some(t0) = _perf_overlay_inner_bg_start {
                let dt = t0.elapsed().as_nanos();
                let mut p = self.perf_state.stats.borrow_mut();
                p.ns_overlay_body_bg = p.ns_overlay_body_bg.saturating_add(dt);
                let cells = (inner.width as u64) * (inner.height as u64);
                p.cells_overlay_body_bg = p.cells_overlay_body_bg.saturating_add(cells);
            }

            // Split into header tabs and body/footer
            // Add one cell padding around the entire inside of the window
            let padded_inner = inner.inner(ratatui::layout::Margin::new(1, 1));
            let [tabs_area, body_area] = if has_tabs {
                Layout::vertical([Constraint::Length(2), Constraint::Fill(1)]).areas(padded_inner)
            } else {
                // Keep a small header row to show file path and counts
                let [t, b] = Layout::vertical([Constraint::Length(2), Constraint::Fill(1)]).areas(padded_inner);
                [t, b]
            };

            // Render tabs only if we have more than one file
            if has_tabs {
                let labels: Vec<String> = overlay
                    .tabs
                    .iter()
                    .map(|(t, _)| format!("  {}  ", t))
                    .collect();
                let mut constraints: Vec<Constraint> = Vec::new();
                let mut total: u16 = 0;
                for label in &labels {
                    let w = (label.chars().count() as u16).min(tabs_area.width.saturating_sub(total));
                    constraints.push(Constraint::Length(w));
                    total = total.saturating_add(w);
                    if total >= tabs_area.width.saturating_sub(4) { break; }
                }
                constraints.push(Constraint::Fill(1));
                let chunks = Layout::horizontal(constraints).split(tabs_area);
                // Draw a light bottom border across the entire tabs strip
                let tabs_bottom_rule = Block::default()
                    .borders(Borders::BOTTOM)
                    .border_style(Style::default().fg(crate::colors::border()));
                tabs_bottom_rule.render(tabs_area, buf);
                for i in 0..labels.len() { // last chunk is filler; guard below
                    if i >= chunks.len().saturating_sub(1) { break; }
                    let rect = chunks[i];
                    if rect.width == 0 { continue; }
                    let selected = i == overlay.selected;

                    // Both selected and unselected tabs use the normal background
                    let tab_bg = crate::colors::background();
                    let bg_style = Style::default().bg(tab_bg);
                    for y in rect.y..rect.y + rect.height {
                        for x in rect.x..rect.x + rect.width {
                            buf[(x, y)].set_style(bg_style);
                        }
                    }

                    // Render label at the top line, with padding
                    let label_rect = Rect {
                        x: rect.x + 1,
                        y: rect.y,
                        width: rect.width.saturating_sub(2),
                        height: 1,
                    };
                    let label_style = if selected {
                        Style::default().fg(crate::colors::text()).add_modifier(Modifier::BOLD)
                    } else {
                        Style::default().fg(crate::colors::text_dim())
                    };
                    let line = ratatui::text::Line::from(ratatui::text::Span::styled(labels[i].clone(), label_style));
                    Paragraph::new(RtText::from(vec![line]))
                        .wrap(ratatui::widgets::Wrap { trim: true })
                        .render(label_rect, buf);
                    // Selected tab: thin underline using text_bright under the label width
                    if selected {
                        let label_len = labels[i].chars().count() as u16;
                        let accent_w = label_len.min(rect.width.saturating_sub(2)).max(1);
                        let accent_rect = Rect {
                            x: label_rect.x,
                            y: rect.y + rect.height.saturating_sub(1),
                            width: accent_w,
                            height: 1,
                        };
                        let underline = Block::default()
                            .borders(Borders::BOTTOM)
                            .border_style(Style::default().fg(crate::colors::text_bright()));
                        underline.render(accent_rect, buf);
                    }
                }
            } else {
                // Single-file header: show full path with (+adds -dels)
                if let Some((label, _)) = overlay.tabs.get(overlay.selected) {
                    let header_line = ratatui::text::Line::from(ratatui::text::Span::styled(
                        label.clone(),
                        Style::default().fg(crate::colors::text()).add_modifier(Modifier::BOLD),
                    ));
                    let para = Paragraph::new(RtText::from(vec![header_line]))
                        .wrap(ratatui::widgets::Wrap { trim: true });
                    ratatui::widgets::Widget::render(para, tabs_area, buf);
                }
            }

            // Render selected tab with vertical scroll and highlight current diff block
            if let Some((_, blocks)) = overlay.tabs.get(overlay.selected) {
                // Flatten blocks into lines and record block start indices
                let mut all_lines: Vec<ratatui::text::Line<'static>> = Vec::new();
                let mut block_starts: Vec<(usize, usize)> = Vec::new(); // (start_index, len)
                for b in blocks {
                    let start = all_lines.len();
                    block_starts.push((start, b.lines.len()));
                    all_lines.extend(b.lines.clone());
                }

                let raw_skip = overlay.scroll_offsets.get(overlay.selected).copied().unwrap_or(0) as usize;
                let visible_rows = body_area.height as usize;
                // Cache visible rows so key handler can clamp
                self.diffs.body_visible_rows.set(body_area.height);
                let max_off = all_lines.len().saturating_sub(visible_rows.max(1));
                let skip = raw_skip.min(max_off);
                let body_inner = body_area;
                let visible_rows = body_inner.height as usize;

                // Collect visible slice
                let end = (skip + visible_rows).min(all_lines.len());
                let visible = if skip < all_lines.len() { &all_lines[skip..end] } else { &[] };
                // Fill body background with a slightly lighter paper-like background
                let bg = crate::colors::background();
                let paper_color = match bg {
                    ratatui::style::Color::Rgb(r, g, b) => {
                        let alpha = 0.06f32; // subtle lightening toward white
                        let nr = ((r as f32) * (1.0 - alpha) + 255.0 * alpha).round() as u8;
                        let ng = ((g as f32) * (1.0 - alpha) + 255.0 * alpha).round() as u8;
                        let nb = ((b as f32) * (1.0 - alpha) + 255.0 * alpha).round() as u8;
                        ratatui::style::Color::Rgb(nr, ng, nb)
                    }
                    _ => bg,
                };
                let body_bg = Style::default().bg(paper_color);
                let _perf_overlay_body_bg2 = if self.perf_state.enabled { Some(std::time::Instant::now()) } else { None };
                for y in body_inner.y..body_inner.y + body_inner.height {
                    for x in body_inner.x..body_inner.x + body_inner.width {
                        buf[(x, y)].set_style(body_bg);
                    }
                }
                if let Some(t0) = _perf_overlay_body_bg2 {
                    let dt = t0.elapsed().as_nanos();
                    let mut p = self.perf_state.stats.borrow_mut();
                    p.ns_overlay_body_bg = p.ns_overlay_body_bg.saturating_add(dt);
                    let cells = (body_inner.width as u64) * (body_inner.height as u64);
                    p.cells_overlay_body_bg = p.cells_overlay_body_bg.saturating_add(cells);
                }
                let paragraph = Paragraph::new(RtText::from(visible.to_vec())).wrap(ratatui::widgets::Wrap { trim: false });
                ratatui::widgets::Widget::render(paragraph, body_inner, buf);

                // No explicit current-block highlight for a cleaner look

                // Render confirmation dialog if active
                if self.diffs.confirm.is_some() {
                    // Centered small box
                    let w = (body_inner.width as i16 - 10).max(20) as u16;
                    let h = 5u16;
                    let x = body_inner.x + (body_inner.width.saturating_sub(w)) / 2;
                    let y = body_inner.y + (body_inner.height.saturating_sub(h)) / 2;
                    let dialog = Rect { x, y, width: w, height: h };
                    Clear.render(dialog, buf);
                    let dlg_block = Block::default()
                        .borders(Borders::ALL)
                        .title("Confirm Undo")
                        .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
                        .border_style(Style::default().fg(crate::colors::border()));
                    let dlg_inner = dlg_block.inner(dialog);
                    dlg_block.render(dialog, buf);
                    // Fill dialog inner area with theme background for consistent look
                    let dlg_bg = Style::default().bg(crate::colors::background());
                    for y in dlg_inner.y..dlg_inner.y + dlg_inner.height { for x in dlg_inner.x..dlg_inner.x + dlg_inner.width { buf[(x, y)].set_style(dlg_bg); } }
                    let lines = vec![
                        ratatui::text::Line::from("Are you sure you want to undo this diff?"),
                        ratatui::text::Line::from("Press Enter to confirm • Esc to cancel".to_string().dim()),
                    ];
                    let para = Paragraph::new(RtText::from(lines))
                        .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
                        .wrap(ratatui::widgets::Wrap { trim: true });
                    ratatui::widgets::Widget::render(para, dlg_inner, buf);
                }
            }
        }
        // Finalize widget render timing
        if let Some(t0) = _perf_widget_start {
            let dt = t0.elapsed().as_nanos();
            let mut p = self.perf_state.stats.borrow_mut();
            p.ns_widget_render_total = p.ns_widget_render_total.saturating_add(dt);
        }
    }
}

fn add_token_usage(current_usage: &TokenUsage, new_usage: &TokenUsage) -> TokenUsage {
    let cached_input_tokens = match (
        current_usage.cached_input_tokens,
        new_usage.cached_input_tokens,
    ) {
        (Some(current), Some(new)) => Some(current + new),
        (Some(current), None) => Some(current),
        (None, Some(new)) => Some(new),
        (None, None) => None,
    };
    let reasoning_output_tokens = match (
        current_usage.reasoning_output_tokens,
        new_usage.reasoning_output_tokens,
    ) {
        (Some(current), Some(new)) => Some(current + new),
        (Some(current), None) => Some(current),
        (None, Some(new)) => Some(new),
        (None, None) => None,
    };
    TokenUsage {
        input_tokens: current_usage.input_tokens + new_usage.input_tokens,
        cached_input_tokens,
        output_tokens: current_usage.output_tokens + new_usage.output_tokens,
        reasoning_output_tokens,
        total_tokens: current_usage.total_tokens + new_usage.total_tokens,
    }
}

// Coalesce adjacent Read entries of the same file with contiguous ranges in a rendered lines vector.
// Expects the vector to contain a header line at index 0 (e.g., "Read"). Modifies in place.
#[allow(dead_code)]
fn coalesce_read_ranges_in_lines(lines: &mut Vec<ratatui::text::Line<'static>>) {
    use ratatui::style::{Modifier, Style};
    use ratatui::text::{Line, Span};

    if lines.len() <= 1 {
        return;
    }

    // Helper to parse a content line into (filename, start, end, prefix)
    fn parse_read_line(line: &Line<'_>) -> Option<(String, u32, u32, String)> {
        if line.spans.is_empty() {
            return None;
        }
        let prefix = line.spans[0].content.to_string();
        if !(prefix == "└ " || prefix == "  ") {
            return None;
        }
        let rest: String = line
            .spans
            .iter()
            .skip(1)
            .map(|s| s.content.as_ref())
            .collect();
        if let Some(idx) = rest.rfind(" (lines ") {
            let fname = rest[..idx].to_string();
            let tail = &rest[idx + 1..];
            if tail.starts_with("(lines ") && tail.ends_with(")") {
                let inner = &tail[7..tail.len() - 1];
                if let Some((s1, s2)) = inner.split_once(" to ") {
                    if let (Ok(start), Ok(end)) = (s1.trim().parse::<u32>(), s2.trim().parse::<u32>()) {
                        return Some((fname, start, end, prefix));
                    }
                }
            }
        }
        None
    }

    // Merge overlapping or touching ranges for the same file, regardless of adjacency.
    let mut i: usize = 0; // works for vectors with or without a header line
    while i < lines.len() {
        let Some((fname_a, mut a1, mut a2, prefix_a)) = parse_read_line(&lines[i]) else { i += 1; continue; };
        let mut k = i + 1;
        while k < lines.len() {
            if let Some((fname_b, b1, b2, _prefix_b)) = parse_read_line(&lines[k]) {
                if fname_b == fname_a {
                    let touch_or_overlap = b1 <= a2.saturating_add(1) && b2.saturating_add(1) >= a1;
                    if touch_or_overlap {
                        a1 = a1.min(b1);
                        a2 = a2.max(b2);
                        let new_spans: Vec<Span<'static>> = vec![
                            Span::styled(prefix_a.clone(), Style::default().add_modifier(Modifier::DIM)),
                            Span::styled(fname_a.clone(), Style::default().fg(crate::colors::text())),
                            Span::styled(format!(" (lines {} to {})", a1, a2), Style::default().fg(crate::colors::text_dim())),
                        ];
                        lines[i] = Line::from(new_spans);
                        lines.remove(k);
                        continue;
                    }
                }
            }
            k += 1;
        }
        i += 1;
    }
}
#[derive(Default)]
struct ExecState {
    running_commands: HashMap<ExecCallId, RunningCommand>,
    running_read_agg_index: Option<usize>,
    // Pairing map for out-of-order exec events. If an ExecEnd arrives before
    // ExecBegin, we stash it briefly and either pair it when Begin arrives or
    // flush it after a short timeout to show a fallback cell.
    pending_exec_ends: HashMap<ExecCallId, (ExecCommandEndEvent, dev_core::protocol::OrderMeta, std::time::Instant)>,
}

#[derive(Default)]
struct ToolState {
    running_custom_tools: HashMap<ToolCallId, usize>,
    running_web_search: HashMap<ToolCallId, (usize, Option<String>)>,
}
#[derive(Default)]
struct StreamState {
    current_kind: Option<StreamKind>,
    closed_answer_ids: HashSet<StreamId>,
    closed_reasoning_ids: HashSet<StreamId>,
    next_seq: u64,
    seq_answer_final: Option<u64>,
    drop_streaming: bool,
}

#[derive(Default)]
struct LayoutState {
    // Scroll offset from bottom (0 = bottom)
    scroll_offset: u16,
    // Cached max scroll from last render
    last_max_scroll: std::cell::Cell<u16>,
    // Track last viewport height of the history content area
    last_history_viewport_height: std::cell::Cell<u16>,
    // Stateful vertical scrollbar for history view
    vertical_scrollbar_state: std::cell::RefCell<ScrollbarState>,
    // Auto-hide scrollbar timer
    scrollbar_visible_until: std::cell::Cell<Option<std::time::Instant>>,
    // HUD visibility and sizing
    last_hud_present: std::cell::Cell<bool>,
    browser_hud_expanded: bool,
    agents_hud_expanded: bool,
    last_frame_height: std::cell::Cell<u16>,
}

#[derive(Default)]
struct DiffsState {
    session_patch_sets: Vec<HashMap<PathBuf, dev_core::protocol::FileChange>>,
    baseline_file_contents: HashMap<PathBuf, String>,
    overlay: Option<DiffOverlay>,
    confirm: Option<DiffConfirm>,
    body_visible_rows: std::cell::Cell<u16>,
}
#[derive(Default)]
struct PerfState {
    enabled: bool,
    stats: std::cell::RefCell<PerfStats>,
}
