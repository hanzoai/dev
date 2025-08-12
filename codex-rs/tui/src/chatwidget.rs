use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use ratatui::style::Modifier;

use codex_core::codex_wrapper::CodexConversation;
use codex_core::codex_wrapper::init_codex;
use codex_core::config::Config;
use codex_core::config_types::ReasoningEffort;
use codex_core::parse_command::ParsedCommand;
use codex_core::protocol::AgentMessageDeltaEvent;
use codex_core::protocol::AgentMessageEvent;
use codex_core::protocol::AgentReasoningDeltaEvent;
use codex_core::protocol::AgentReasoningEvent;
use codex_core::protocol::AgentReasoningRawContentDeltaEvent;
use codex_core::protocol::AgentReasoningRawContentEvent;
use codex_core::protocol::AgentStatusUpdateEvent;
use codex_core::protocol::ApplyPatchApprovalRequestEvent;
use codex_core::protocol::BackgroundEventEvent;
use codex_core::protocol::BrowserScreenshotUpdateEvent;
use codex_core::protocol::ErrorEvent;
use codex_core::protocol::Event;
use codex_core::protocol::EventMsg;
use codex_core::protocol::ExecApprovalRequestEvent;
use codex_core::protocol::ExecCommandBeginEvent;
use codex_core::protocol::ExecCommandEndEvent;
use codex_core::protocol::InputItem;
use codex_core::protocol::McpToolCallBeginEvent;
use codex_core::protocol::McpToolCallEndEvent;
use codex_core::protocol::Op;
use codex_core::protocol::PatchApplyBeginEvent;
use codex_core::protocol::TaskCompleteEvent;
use codex_core::protocol::TokenUsage;
use codex_core::protocol::TurnDiffEvent;
use crossterm::event::KeyEvent;
use crossterm::event::KeyEventKind;
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
use crate::common::DEFAULT_WRAP_COLS;
use crate::history_cell::CommandOutput;
use crate::history_cell::HistoryCell;
use crate::history_cell::PatchEventType;
use crate::live_wrap::RowBuilder;
use crate::text_block::TextBlock;
use crate::user_approval_widget::ApprovalRequest;
use codex_browser::BrowserManager;
use codex_file_search::FileMatch;
use ratatui::style::Stylize;

struct RunningCommand {
    command: Vec<String>,
    parsed: Vec<ParsedCommand>,
}

pub(crate) struct ChatWidget<'a> {
    app_event_tx: AppEventSender,
    codex_op_tx: UnboundedSender<Op>,
    bottom_pane: BottomPane<'a>,
    active_history_cell: Option<HistoryCell>,
    history_cells: Vec<HistoryCell>, // Store all history in memory
    config: Config,
    initial_user_message: Option<UserMessage>,
    total_token_usage: TokenUsage,
    last_token_usage: TokenUsage,
    reasoning_buffer: String,
    content_buffer: String,
    // Buffer for streaming assistant answer text; we do not surface partial
    // We wait for the final AgentMessage event and then emit the full text
    // at once into scrollback so the history contains a single message.
    answer_buffer: String,
    running_commands: HashMap<String, RunningCommand>,
    live_builder: RowBuilder,
    current_stream: Option<StreamKind>,
    stream_header_emitted: bool,
    live_max_rows: u16,
    // Store pending image paths keyed by their placeholder text
    pending_images: HashMap<String, PathBuf>,
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
    // Scroll offset from bottom (0 = at bottom, positive = scrolled up)
    scroll_offset: u16,
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
}

struct UserMessage {
    text: String,
    image_paths: Vec<PathBuf>,
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StreamKind {
    Answer,
    Reasoning,
}

impl From<String> for UserMessage {
    fn from(text: String) -> Self {
        Self {
            text,
            image_paths: Vec::new(),
        }
    }
}

fn create_initial_user_message(text: String, image_paths: Vec<PathBuf>) -> Option<UserMessage> {
    if text.is_empty() && image_paths.is_empty() {
        None
    } else {
        Some(UserMessage { text, image_paths })
    }
}

impl ChatWidget<'_> {
    /// Get or create the global browser manager
    async fn get_browser_manager() -> Arc<BrowserManager> {
        codex_browser::global::get_or_create_browser_manager().await
    }

    pub(crate) fn insert_str(&mut self, s: &str) {
        self.bottom_pane.insert_str(s);
    }

    fn parse_message_with_images(&mut self, text: String) -> UserMessage {
        use std::path::Path;

        // Common image extensions
        const IMAGE_EXTENSIONS: &[&str] = &[
            ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".webp", ".svg", ".ico", ".tiff", ".tif",
        ];

        let mut image_paths = Vec::new();
        let mut cleaned_text = text.clone();

        // First, handle [image: ...] placeholders from drag-and-drop
        let placeholder_regex = regex_lite::Regex::new(r"\[image: [^\]]+\]").unwrap();
        for mat in placeholder_regex.find_iter(&text) {
            let placeholder = mat.as_str();
            if let Some(path) = self.pending_images.remove(placeholder) {
                image_paths.push(path);
                // Remove the placeholder from the text
                cleaned_text = cleaned_text.replace(placeholder, "");
            }
        }

        // Then check for direct file paths in the text
        let words: Vec<String> = text.split_whitespace().map(String::from).collect();

        for word in &words {
            // Skip placeholders we already handled
            if word.starts_with("[image:") {
                continue;
            }

            // Check if this looks like an image path
            let is_image_path = IMAGE_EXTENSIONS
                .iter()
                .any(|ext| word.to_lowercase().ends_with(ext));

            if is_image_path {
                let path = Path::new(word);

                // Check if it's a relative or absolute path that exists
                if path.exists() {
                    image_paths.push(path.to_path_buf());
                    // Remove the path from the text
                    cleaned_text = cleaned_text.replace(word, "");
                } else {
                    // Try with common relative paths
                    let potential_paths = vec![
                        PathBuf::from(word),
                        PathBuf::from("./").join(word),
                        std::env::current_dir()
                            .ok()
                            .map(|d| d.join(word))
                            .unwrap_or_default(),
                    ];

                    for potential_path in potential_paths {
                        if potential_path.exists() {
                            image_paths.push(potential_path);
                            cleaned_text = cleaned_text.replace(word, "");
                            break;
                        }
                    }
                }
            }
        }

        // Clean up extra whitespace
        cleaned_text = cleaned_text
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");

        UserMessage {
            text: cleaned_text,
            image_paths,
        }
    }

    fn interrupt_running_task(&mut self) {
        if self.bottom_pane.is_task_running() {
            self.active_history_cell = None;
            self.bottom_pane.clear_ctrl_c_quit_hint();
            self.submit_op(Op::Interrupt);
            self.bottom_pane.set_task_running(false);
            self.bottom_pane.clear_live_ring();
            self.live_builder = RowBuilder::new(self.live_builder.width());
            self.current_stream = None;
            self.stream_header_emitted = false;
            self.answer_buffer.clear();
            self.reasoning_buffer.clear();
            self.content_buffer.clear();
            self.request_redraw();
        }
    }
    fn layout_areas(&self, area: Rect) -> Vec<Rect> {
        // Check if browser is active and has a screenshot
        let has_browser_screenshot = self
            .latest_browser_screenshot
            .lock()
            .map(|lock| lock.is_some())
            .unwrap_or(false);

        // Check if there are active agents or if agents are ready to start
        let has_active_agents = !self.active_agents.is_empty() || self.agents_ready_to_start;

        let bottom_height = 6u16
            .max(self.bottom_pane.desired_height(area.width))
            .min(15);

        if has_browser_screenshot || has_active_agents {
            // match HUD padding used in render_browser_hud()
            let horizontal_padding = 2u16;
            let padded_area = Rect {
                x: area.x + horizontal_padding,
                y: area.y,
                width: area.width.saturating_sub(horizontal_padding * 2),
                height: area.height,
            };

            // use the actual 50/50 split
            let [_left, right] =
                Layout::horizontal([Constraint::Percentage(50), Constraint::Percentage(50)])
                    .areas::<2>(padded_area);

            // inner width of the Preview block (remove 1-char borders)
            let inner_cols = right.width.saturating_sub(2);

            // rows = cols * (3/4) * (cell_w/cell_h)
            let (cw, ch) = self.measured_font_size();
            let number = (inner_cols as u32) * 3 * (cw as u32);
            let denom = 4 * (ch as u32);

            // use FLOOR to avoid over-estimating (which creates bottom slack)
            let inner_rows: u16 = (number / denom) as u16;

            // add back the top/bottom borders of the Preview block
            let mut hud_height = inner_rows.saturating_add(2);

            // one-line tighten to kill residual rounding slack
            hud_height = hud_height.saturating_sub(1);

            // keep within vertical budget (status+bottom+≥1 row history)
            let available = area.height.saturating_sub(5) / 3;
            hud_height = hud_height.clamp(4, available);

            return Layout::vertical([
                Constraint::Length(3),
                Constraint::Length(hud_height),
                Constraint::Fill(1),
                Constraint::Length(bottom_height),
            ])
            .areas::<4>(area)
            .to_vec();
        } else {
            // Status bar, history, bottom pane (no HUD)
            Layout::vertical([
                Constraint::Length(3), // Status bar with box border
                Constraint::Fill(1),   // History takes all remaining space
                Constraint::Length(bottom_height),
            ])
            .areas::<3>(area)
            .to_vec()
        }
    }
    fn emit_stream_header(&mut self, kind: StreamKind) {
        use ratatui::text::Line as RLine;
        if self.stream_header_emitted {
            return;
        }
        match kind {
            StreamKind::Reasoning => {
                // Always add space above reasoning (unless history is completely empty)
                if !self.history_cells.is_empty() {
                    self.history_cells.push(HistoryCell::new_text_line(RLine::from("")));
                }
                self.stream_header_emitted = true;
                self.request_redraw();
            }
            StreamKind::Answer => {
                // Add space above GPT-5 header only if the last item isn't already a blank line
                let needs_space = if self.history_cells.is_empty() {
                    false
                } else {
                    // Check if the last item is a blank line (from any cell type that could be empty)
                    match self.history_cells.last() {
                        Some(HistoryCell::BackgroundEvent { view }) 
                        | Some(HistoryCell::StyledText { view })
                        | Some(HistoryCell::DimmedReasoning { view }) 
                            if view.lines.len() == 1 => 
                        {
                            // Check if it's an empty line
                            let line = &view.lines[0];
                            let is_empty = line.spans.is_empty() || 
                                          (line.spans.len() == 1 && line.spans[0].content.is_empty());
                            !is_empty // Need space if the line is NOT empty
                        }
                        _ => true // Always need space for other cell types
                    }
                };
                
                if needs_space {
                    self.history_cells.push(HistoryCell::new_text_line(RLine::from("")));
                }
                
                // Add GPT-5 header with secondary color
                let formatted_model = self.format_model_name(&self.config.model);
                use ratatui::style::{Style, Modifier};
                use ratatui::text::Span;
                self.history_cells.push(HistoryCell::new_styled_text_line(
                    RLine::from(Span::styled(
                        formatted_model,
                        Style::default()
                            .fg(crate::colors::secondary())
                            .add_modifier(Modifier::BOLD)
                    ))
                ));
                
                self.stream_header_emitted = true;
                self.request_redraw();
            }
        }
    }
    fn finalize_active_stream(&mut self) {
        if let Some(kind) = self.current_stream {
            self.finalize_stream(kind);
        }
    }
    pub(crate) fn new(
        config: Config,
        app_event_tx: AppEventSender,
        initial_prompt: Option<String>,
        initial_images: Vec<PathBuf>,
        enhanced_keys_supported: bool,
    ) -> Self {
        let (codex_op_tx, mut codex_op_rx) = unbounded_channel::<Op>();

        let app_event_tx_clone = app_event_tx.clone();
        // Create the Codex asynchronously so the UI loads as quickly as possible.
        let config_for_agent_loop = config.clone();
        tokio::spawn(async move {
            let CodexConversation {
                codex,
                session_configured,
                ..
            } = match init_codex(config_for_agent_loop).await {
                Ok(vals) => vals,
                Err(e) => {
                    // TODO: surface this error to the user.
                    tracing::error!("failed to initialize codex: {e}");
                    return;
                }
            };

            // Forward the captured `SessionInitialized` event that was consumed
            // inside `init_codex()` so it can be rendered in the UI.
            app_event_tx_clone.send(AppEvent::CodexEvent(session_configured.clone()));
            let codex = Arc::new(codex);
            let codex_clone = codex.clone();
            tokio::spawn(async move {
                while let Some(op) = codex_op_rx.recv().await {
                    let id = codex_clone.submit(op).await;
                    if let Err(e) = id {
                        tracing::error!("failed to submit op: {e}");
                    }
                }
            });

            while let Ok(event) = codex.next_event().await {
                app_event_tx_clone.send(AppEvent::CodexEvent(event));
            }
        });

        // Browser manager is now handled through the global state
        // The core session will use the same global manager when browser tools are invoked

        // Add initial animated welcome message to history
        let mut history_cells = Vec::new();
        history_cells.push(HistoryCell::AnimatedWelcome {
            start_time: std::time::Instant::now(),
            completed: std::cell::Cell::new(false),
            fade_start: std::cell::Cell::new(None),
            faded_out: std::cell::Cell::new(false),
        });

        // Initialize image protocol for rendering screenshots

        Self {
            app_event_tx: app_event_tx.clone(),
            codex_op_tx,
            bottom_pane: BottomPane::new(BottomPaneParams {
                app_event_tx,
                has_input_focus: true,
                enhanced_keys_supported,
            }),
            active_history_cell: None,
            history_cells,
            config,
            initial_user_message: create_initial_user_message(
                initial_prompt.unwrap_or_default(),
                initial_images,
            ),
            total_token_usage: TokenUsage::default(),
            last_token_usage: TokenUsage::default(),
            reasoning_buffer: String::new(),
            content_buffer: String::new(),
            answer_buffer: String::new(),
            running_commands: HashMap::new(),
            live_builder: RowBuilder::new(DEFAULT_WRAP_COLS.into()),
            current_stream: None,
            stream_header_emitted: false,
            live_max_rows: 3,
            pending_images: HashMap::new(),
            welcome_shown: false,
            latest_browser_screenshot: Arc::new(Mutex::new(None)),
            cached_image_protocol: RefCell::new(None),
            cached_picker: RefCell::new(None),
            cached_cell_size: std::cell::OnceCell::new(),
            scroll_offset: 0,
            active_agents: Vec::new(),
            agents_ready_to_start: false,
            last_agent_prompt: None,
            agent_context: None,
            agent_task: None,
            overall_task_status: "preparing".to_string(),
            sparkline_data: std::cell::RefCell::new(Vec::new()),
            last_sparkline_update: std::cell::RefCell::new(std::time::Instant::now()),
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
    fn calculate_max_scroll_offset(&self, content_area_height: u16) -> u16 {
        let mut total_height = 0u16;
        
        // Calculate total content height (same logic as render method)
        for cell in &self.history_cells {
            let h = cell.desired_height(80); // Use reasonable width for height calculation
            total_height = total_height.saturating_add(h);
        }
        
        if let Some(ref cell) = self.active_history_cell {
            let h = cell.desired_height(80);
            total_height = total_height.saturating_add(h);
        }
        
        // Max scroll is content height minus available height
        total_height.saturating_sub(content_area_height)
    }

    pub(crate) fn handle_key_event(&mut self, key_event: KeyEvent) {
        if key_event.kind == KeyEventKind::Press {
            self.bottom_pane.clear_ctrl_c_quit_hint();
        }

        match self.bottom_pane.handle_key_event(key_event) {
            InputResult::Submitted(text) => {
                let user_message = self.parse_message_with_images(text);
                self.submit_user_message(user_message);
            }
            InputResult::ScrollUp => {
                // Scroll up in chat history (increase offset, towards older content)
                // Estimate max scroll based on content - use reasonable content area height of 30
                let estimated_max_scroll = self.calculate_max_scroll_offset(30);
                let new_offset = self.scroll_offset.saturating_add(3).min(estimated_max_scroll);
                self.scroll_offset = new_offset;
                self.app_event_tx.send(AppEvent::RequestRedraw);
            }
            InputResult::ScrollDown => {
                // Scroll down in chat history (decrease offset, towards bottom)
                if self.scroll_offset >= 3 {
                    self.scroll_offset = self.scroll_offset.saturating_sub(3);
                    self.app_event_tx.send(AppEvent::RequestRedraw);
                } else if self.scroll_offset > 0 {
                    self.scroll_offset = 0;
                    self.app_event_tx.send(AppEvent::RequestRedraw);
                }
            }
            InputResult::None => {}
        }
    }

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
                    return;
                } else {
                    tracing::warn!("Image path does not exist: {:?}", path);
                }
            }
        }

        // Otherwise handle as regular text paste
        self.bottom_pane.handle_paste(text);
    }

    fn add_to_history(&mut self, cell: HistoryCell) {
        // If this is the first user prompt, start fade-out animation for AnimatedWelcome
        if matches!(cell, HistoryCell::UserPrompt { .. }) {
            let has_existing_user_prompts = self
                .history_cells
                .iter()
                .any(|cell| matches!(cell, HistoryCell::UserPrompt { .. }));

            if !has_existing_user_prompts {
                // This is the first user prompt - start fade-out
                for history_cell in &mut self.history_cells {
                    if let HistoryCell::AnimatedWelcome { fade_start, .. } = history_cell {
                        fade_start.set(Some(std::time::Instant::now()));
                    }
                }
            }
        }

        // Store in memory instead of sending to scrollback
        self.history_cells.push(cell);
        // Auto-scroll to bottom when new content arrives, but only if we're already at the bottom
        // This preserves the user's scroll position if they've manually scrolled up
        if self.scroll_offset == 0 {
            // Already at bottom, stay there
            self.scroll_offset = 0;
        }
        // If user has scrolled up (scroll_offset > 0), don't change their position
        // Check if there's actual conversation history (any user prompts submitted)
        let has_conversation = self
            .history_cells
            .iter()
            .any(|cell| matches!(cell, HistoryCell::UserPrompt { .. }));
        self.bottom_pane.set_has_chat_history(has_conversation);
        // Ensure input focus is maintained when new content arrives
        self.bottom_pane.ensure_input_focus();
        // Clean up any faded-out animations
        self.process_animation_cleanup();
        // Request redraw to show new history
        self.app_event_tx.send(AppEvent::RequestRedraw);
    }

    /// Clean up faded-out animation cells
    fn process_animation_cleanup(&mut self) {
        self.history_cells.retain(|cell| {
            if let HistoryCell::AnimatedWelcome { faded_out, .. } = cell {
                !faded_out.get()
            } else {
                true
            }
        });
    }

    fn submit_user_message(&mut self, user_message: UserMessage) {
        let UserMessage { text, image_paths } = user_message;

        // Keep the original text for display purposes
        let original_text = text.clone();
        let mut actual_text = text;

        // Save the prompt if it's a multi-agent command
        let original_trimmed = original_text.trim();
        if original_trimmed.starts_with("/plan ")
            || original_trimmed.starts_with("/solve ")
            || original_trimmed.starts_with("/code ")
        {
            self.last_agent_prompt = Some(original_text.clone());
        }

        // Process slash commands and expand them if needed
        let processed = crate::slash_command::process_slash_command_message(&original_text);
        match processed {
            crate::slash_command::ProcessedCommand::ExpandedPrompt(expanded) => {
                // Replace the slash command with the expanded prompt for the LLM
                actual_text = expanded;
            }
            crate::slash_command::ProcessedCommand::RegularCommand(cmd, _args) => {
                // This is a regular slash command, dispatch it normally
                self.app_event_tx
                    .send(AppEvent::DispatchCommand(cmd, actual_text.clone()));
                return;
            }
            crate::slash_command::ProcessedCommand::Error(error_msg) => {
                // Show error in history
                self.add_to_history(HistoryCell::new_error_event(error_msg));
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
            let codex_op_tx_clone = self.codex_op_tx.clone();
            let latest_browser_screenshot_clone = Arc::clone(&self.latest_browser_screenshot);
            
            tokio::spawn(async move {
                tracing::info!("Starting background screenshot capture...");
                
                // Wait a bit longer before capturing to ensure page is ready
                tokio::time::sleep(tokio::time::Duration::from_millis(2000)).await;
                
                let browser_manager = ChatWidget::get_browser_manager().await;
                
                // Retry screenshot capture with exponential backoff
                let mut attempts = 0;
                let max_attempts = 3;
                
                loop {
                    attempts += 1;
                    tracing::info!("Screenshot capture attempt {} of {}", attempts, max_attempts);
                    
                    match browser_manager.capture_screenshot_with_url().await {
                        Ok((screenshot_paths, url)) => {
                            tracing::info!("Background screenshot capture succeeded with {} images on attempt {}", screenshot_paths.len(), attempts);
                            
                            // Save the first screenshot path and URL for display in the TUI
                            if let Some(first_path) = screenshot_paths.first() {
                                if let Ok(mut latest) = latest_browser_screenshot_clone.lock() {
                                    let url_string = url.clone().unwrap_or_else(|| "Browser".to_string());
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
                            
                            // Send a follow-up message with just the screenshots
                            if !screenshot_items.is_empty() {
                                let _ = codex_op_tx_clone.send(Op::UserInput {
                                    items: screenshot_items,
                                });
                            }
                            break; // Success - exit retry loop
                        }
                        Err(e) => {
                            if attempts >= max_attempts {
                                tracing::error!("Background screenshot capture failed after {} attempts: {}", attempts, e);
                                break; // Give up after max attempts
                            } else {
                                tracing::warn!("Background screenshot capture failed on attempt {} ({}), retrying...", attempts, e);
                                // Exponential backoff: wait 1s, then 2s, then 4s
                                let wait_time = std::time::Duration::from_millis(1000 * (1 << (attempts - 1)));
                                tokio::time::sleep(wait_time).await;
                            }
                        }
                    }
                }
            });
        } else {
            tracing::info!("Browser is not enabled, skipping screenshot capture");
        }

        if !actual_text.is_empty() {
            items.push(InputItem::Text {
                text: actual_text.clone(),
            });
        }

        // Add user-provided images (these are persistent in history)
        for path in image_paths {
            items.push(InputItem::LocalImage { path });
        }

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

        self.codex_op_tx
            .send(Op::UserInput { items })
            .unwrap_or_else(|e| {
                tracing::error!("failed to send message: {e}");
            });

        // Persist the original text to cross-session message history.
        if !original_text.is_empty() {
            self.codex_op_tx
                .send(Op::AddToHistory {
                    text: original_text.clone(),
                })
                .unwrap_or_else(|e| {
                    tracing::error!("failed to send AddHistory op: {e}");
                });
        }

        if !original_text.is_empty() {
            self.add_to_history(HistoryCell::new_user_prompt(original_text.clone()));
        }
    }

    #[allow(dead_code)]
    pub(crate) fn set_mouse_status_message(&mut self, message: &str) {
        self.bottom_pane.update_status_text(message.to_string());
    }

    pub(crate) fn handle_mouse_event(&mut self, mouse_event: crossterm::event::MouseEvent) {
        use crossterm::event::{KeyModifiers, MouseEventKind};

        // Check if Shift is held - if so, let the terminal handle selection
        if mouse_event.modifiers.contains(KeyModifiers::SHIFT) {
            // Don't handle any mouse events when Shift is held
            // This allows the terminal's native text selection to work
            return;
        }

        match mouse_event.kind {
            MouseEventKind::ScrollUp => {
                // Scroll up with proper bounds checking 
                let estimated_max_scroll = self.calculate_max_scroll_offset(30);
                let new_offset = self.scroll_offset.saturating_add(3).min(estimated_max_scroll);
                self.scroll_offset = new_offset;
                self.app_event_tx.send(AppEvent::RequestRedraw);
            }
            MouseEventKind::ScrollDown => {
                // Scroll down in chat history (decrease offset, towards bottom)
                if self.scroll_offset >= 3 {
                    self.scroll_offset = self.scroll_offset.saturating_sub(3);
                    self.app_event_tx.send(AppEvent::RequestRedraw);
                } else if self.scroll_offset > 0 {
                    self.scroll_offset = 0;
                    self.app_event_tx.send(AppEvent::RequestRedraw);
                }
            }
            _ => {
                // Ignore other mouse events for now
            }
        }
    }

    pub(crate) fn handle_codex_event(&mut self, event: Event) {
        let Event { id, msg } = event;
        match msg {
            EventMsg::SessionConfigured(event) => {
                self.bottom_pane
                    .set_history_metadata(event.history_log_id, event.history_entry_count);
                // Record session information at the top of the conversation.
                // Only show welcome message on first SessionConfigured event
                let is_first = !self.welcome_shown;
                if is_first {
                    self.welcome_shown = true;
                }
                self.add_to_history(HistoryCell::new_session_info(&self.config, event, is_first));

                if let Some(user_message) = self.initial_user_message.take() {
                    // If the user provided an initial message, add it to the
                    // conversation history.
                    self.submit_user_message(user_message);
                }

                self.request_redraw();
            }
            EventMsg::AgentMessage(AgentMessageEvent { message }) => {
                // AgentMessage: if no deltas were streamed, render the final text.
                if self.current_stream != Some(StreamKind::Answer) && !message.is_empty() {
                    self.begin_stream(StreamKind::Answer);
                    self.stream_push_and_maybe_commit(&message);
                }
                self.finalize_stream(StreamKind::Answer);
                self.request_redraw();
            }
            EventMsg::AgentMessageDelta(AgentMessageDeltaEvent { delta }) => {
                self.begin_stream(StreamKind::Answer);
                // Update status to show we're generating a response
                if self.bottom_pane.is_task_running() {
                    self.bottom_pane.update_status_text("responding".to_string());
                }
                self.answer_buffer.push_str(&delta);
                self.stream_push_and_maybe_commit(&delta);
                self.request_redraw();
            }
            EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent { delta }) => {
                // Stream CoT into the live pane; keep input visible and commit
                // overflow rows incrementally to scrollback.
                self.begin_stream(StreamKind::Reasoning);
                // Update status to show we're thinking/reasoning
                if self.bottom_pane.is_task_running() {
                    self.bottom_pane.update_status_text("reasoning".to_string());
                }
                self.reasoning_buffer.push_str(&delta);
                self.stream_push_and_maybe_commit(&delta);
                self.request_redraw();
            }
            EventMsg::AgentReasoning(AgentReasoningEvent { text }) => {
                // Final reasoning: if no deltas were streamed, render the final text.
                if self.current_stream != Some(StreamKind::Reasoning) && !text.is_empty() {
                    self.begin_stream(StreamKind::Reasoning);
                    self.stream_push_and_maybe_commit(&text);
                }
                self.finalize_stream(StreamKind::Reasoning);
                self.request_redraw();
            }
            EventMsg::AgentReasoningRawContentDelta(AgentReasoningRawContentDeltaEvent {
                delta,
            }) => {
                // Treat raw reasoning content the same as summarized reasoning for UI flow.
                self.begin_stream(StreamKind::Reasoning);
                // Update status to show we're thinking/reasoning
                if self.bottom_pane.is_task_running() {
                    self.bottom_pane.update_status_text("reasoning".to_string());
                }
                self.reasoning_buffer.push_str(&delta);
                self.stream_push_and_maybe_commit(&delta);
                self.request_redraw();
            }
            EventMsg::AgentReasoningRawContent(AgentReasoningRawContentEvent { text }) => {
                // Final raw reasoning: if no deltas were streamed, render the final text.
                if self.current_stream != Some(StreamKind::Reasoning) && !text.is_empty() {
                    self.begin_stream(StreamKind::Reasoning);
                    self.stream_push_and_maybe_commit(&text);
                }
                self.finalize_stream(StreamKind::Reasoning);
                self.request_redraw();
            }
            EventMsg::TaskStarted => {
                self.bottom_pane.clear_ctrl_c_quit_hint();
                self.bottom_pane.set_task_running(true);
                // Replace composer with single-line spinner while waiting.
                self.bottom_pane
                    .update_status_text("waiting for model".to_string());
                self.request_redraw();
            }
            EventMsg::TaskComplete(TaskCompleteEvent {
                last_agent_message: _,
            }) => {
                self.bottom_pane.set_task_running(false);
                self.bottom_pane.clear_live_ring();
                // Reset stream state for next conversation
                self.current_stream = None;
                self.stream_header_emitted = false;
                self.request_redraw();
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
                self.add_to_history(HistoryCell::new_error_event(message.clone()));
                self.bottom_pane.set_task_running(false);
                self.bottom_pane.clear_live_ring();
                self.live_builder = RowBuilder::new(self.live_builder.width());
                self.current_stream = None;
                self.stream_header_emitted = false;
                self.answer_buffer.clear();
                self.reasoning_buffer.clear();
                self.content_buffer.clear();
                self.request_redraw();
            }
            EventMsg::PlanUpdate(update) => {
                // Commit plan updates directly to history (no status-line preview).
                self.add_to_history(HistoryCell::new_plan_update(update));
            }
            EventMsg::ExecApprovalRequest(ExecApprovalRequestEvent {
                call_id: _,
                command,
                cwd,
                reason,
            }) => {
                self.finalize_active_stream();
                let request = ApprovalRequest::Exec {
                    id,
                    command,
                    cwd,
                    reason,
                };
                self.bottom_pane.push_approval_request(request);
                self.request_redraw();
            }
            EventMsg::ApplyPatchApprovalRequest(ApplyPatchApprovalRequestEvent {
                call_id: _,
                changes,
                reason,
                grant_root,
            }) => {
                self.finalize_active_stream();
                // ------------------------------------------------------------------
                // Before we even prompt the user for approval we surface the patch
                // summary in the main conversation so that the dialog appears in a
                // sensible chronological order:
                //   (1) codex → proposes patch (HistoryCell::PendingPatch)
                //   (2) UI → asks for approval (BottomPane)
                // This mirrors how command execution is shown (command begins →
                // approval dialog) and avoids surprising the user with a modal
                // prompt before they have seen *what* is being requested.
                // ------------------------------------------------------------------
                self.add_to_history(HistoryCell::new_patch_event(
                    PatchEventType::ApprovalRequest,
                    changes,
                ));

                // Now surface the approval request in the BottomPane as before.
                let request = ApprovalRequest::ApplyPatch {
                    id,
                    reason,
                    grant_root,
                };
                self.bottom_pane.push_approval_request(request);
                self.request_redraw();
            }
            EventMsg::ExecCommandBegin(ExecCommandBeginEvent {
                call_id,
                command,
                cwd: _,
                parsed_cmd,
            }) => {
                self.finalize_active_stream();
                // Ensure the status indicator is visible while the command runs.
                self.bottom_pane
                    .update_status_text("running command".to_string());
                self.running_commands.insert(
                    call_id,
                    RunningCommand {
                        command: command.clone(),
                        parsed: parsed_cmd.clone(),
                    },
                );
                self.active_history_cell =
                    Some(HistoryCell::new_active_exec_command(command, parsed_cmd));
            }
            EventMsg::ExecCommandOutputDelta(_) => {
                // TODO
            }
            EventMsg::PatchApplyBegin(PatchApplyBeginEvent {
                call_id: _,
                auto_approved,
                changes,
            }) => {
                self.add_to_history(HistoryCell::new_patch_event(
                    PatchEventType::ApplyBegin { auto_approved },
                    changes,
                ));
            }
            EventMsg::PatchApplyEnd(event) => {
                if !event.success {
                    self.add_to_history(HistoryCell::new_patch_apply_failure(event.stderr));
                }
            }
            EventMsg::ExecCommandEnd(ExecCommandEndEvent {
                call_id,
                exit_code,
                duration: _,
                stdout,
                stderr,
            }) => {
                // Compute summary before moving stdout into the history cell.
                let cmd = self.running_commands.remove(&call_id);
                self.active_history_cell = None;
                let (command, parsed) = cmd
                    .map(|cmd| (cmd.command, cmd.parsed))
                    .unwrap_or_else(|| (vec![call_id], vec![]));
                self.add_to_history(HistoryCell::new_completed_exec_command(
                    command,
                    parsed,
                    CommandOutput {
                        exit_code,
                        stdout,
                        stderr,
                    },
                ));
            }
            EventMsg::McpToolCallBegin(McpToolCallBeginEvent {
                call_id: _,
                invocation,
            }) => {
                self.finalize_active_stream();
                self.add_to_history(HistoryCell::new_active_mcp_tool_call(invocation));
            }
            EventMsg::McpToolCallEnd(McpToolCallEndEvent {
                call_id: _,
                duration,
                invocation,
                result,
            }) => {
                self.add_to_history(HistoryCell::new_completed_mcp_tool_call(
                    80,
                    invocation,
                    duration,
                    result
                        .as_ref()
                        .map(|r| r.is_error.unwrap_or(false))
                        .unwrap_or(false),
                    result,
                ));
            }
            EventMsg::GetHistoryEntryResponse(event) => {
                let codex_core::protocol::GetHistoryEntryResponseEvent {
                    offset,
                    log_id,
                    entry,
                } = event;

                // Inform bottom pane / composer.
                self.bottom_pane
                    .on_history_entry_response(log_id, offset, entry.map(|e| e.text));
            }
            EventMsg::ShutdownComplete => {
                self.app_event_tx.send(AppEvent::ExitRequest);
            }
            EventMsg::TurnDiff(TurnDiffEvent { unified_diff }) => {
                info!("TurnDiffEvent: {unified_diff}");
            }
            EventMsg::BackgroundEvent(BackgroundEventEvent { message }) => {
                info!("BackgroundEvent: {message}");
            }
            EventMsg::AgentStatusUpdate(AgentStatusUpdateEvent { agents, context, task }) => {
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
                } else if self.active_agents.iter().any(|a| matches!(a.status, AgentStatus::Running)) {
                    "running".to_string()
                } else if self.active_agents.iter().all(|a| matches!(a.status, AgentStatus::Completed)) {
                    "complete".to_string()
                } else if self.active_agents.iter().any(|a| matches!(a.status, AgentStatus::Failed)) {
                    "failed".to_string()
                } else {
                    "planning".to_string()
                };
                
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

    /// Update the live log preview while a task is running.
    pub(crate) fn update_latest_log(&mut self, line: String) {
        if self.bottom_pane.is_task_running() {
            self.bottom_pane.update_status_text(line);
        }
    }

    fn request_redraw(&mut self) {
        self.app_event_tx.send(AppEvent::RequestRedraw);
    }

    pub(crate) fn add_diff_output(&mut self, diff_output: String) {
        self.add_to_history(HistoryCell::new_diff_output(diff_output.clone()));
    }

    pub(crate) fn add_status_output(&mut self) {
        self.add_to_history(HistoryCell::new_status_output(
            &self.config,
            &self.total_token_usage,
        ));
    }

    pub(crate) fn add_prompts_output(&mut self) {
        self.add_to_history(HistoryCell::new_prompts_output());
    }

    pub(crate) fn handle_reasoning_command(&mut self, command_text: String) {
        // Parse the command to extract the parameter
        let parts: Vec<&str> = command_text.trim().split_whitespace().collect();

        if parts.len() > 1 {
            // User specified a level: /reasoning high
            let new_effort = match parts[1].to_lowercase().as_str() {
                "low" => ReasoningEffort::Low,
                "medium" | "med" => ReasoningEffort::Medium,
                "high" => ReasoningEffort::High,
                "none" | "off" => ReasoningEffort::None,
                _ => {
                    // Invalid parameter, show error and return
                    let message = format!(
                        "Invalid reasoning level: '{}'. Use: low, medium, high, or none",
                        parts[1]
                    );
                    self.add_to_history(HistoryCell::new_error_event(message));
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
                tracing::info!("Initialized sparkline data with {} points for preparing phase", sparkline_data.len());
            }
        } // Drop the borrow here
        
        self.request_redraw();
    }

    /// Update sparkline data with randomized activity based on agent count
    fn update_sparkline_data(&self) {
        let now = std::time::Instant::now();
        
        // Update every 100ms for smooth animation
        if now.duration_since(*self.last_sparkline_update.borrow()).as_millis() < 100 {
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
        use std::hash::{Hash, Hasher};
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
        let variation = ((random_seed % variation_range) as i32 - variation_percent as i32) * base_height as i32 / 100;
        let height = ((base_height as i32 + variation).max(1) as u64).min(20);
        
        // Check if any agents are completed
        let has_completed = self.active_agents.iter()
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
        self.add_to_history(HistoryCell::new_reasoning_output(new_effort));
    }

    /// Forward file-search results to the bottom pane.
    pub(crate) fn apply_file_search_result(&mut self, query: String, matches: Vec<FileMatch>) {
        self.bottom_pane.on_file_search_result(query, matches);
    }

    pub(crate) fn show_theme_selection(&mut self) {
        self.bottom_pane
            .show_theme_selection(self.config.tui.theme.name);
    }

    pub(crate) fn set_theme(&mut self, new_theme: codex_core::config_types::ThemeName) {
        // Update the config
        self.config.tui.theme.name = new_theme;

        // Save the theme to config file
        self.save_theme_to_config(new_theme);

        // Add confirmation message to history
        let theme_name = match new_theme {
            // Light themes
            codex_core::config_types::ThemeName::LightPhoton => "Light - Photon",
            codex_core::config_types::ThemeName::LightPrismRainbow => "Light - Prism Rainbow",
            codex_core::config_types::ThemeName::LightVividTriad => "Light - Vivid Triad",
            codex_core::config_types::ThemeName::LightPorcelain => "Light - Porcelain",
            codex_core::config_types::ThemeName::LightSandbar => "Light - Sandbar",
            codex_core::config_types::ThemeName::LightGlacier => "Light - Glacier",
            // Dark themes
            codex_core::config_types::ThemeName::DarkCarbonNight => "Dark - Carbon Night",
            codex_core::config_types::ThemeName::DarkShinobiDusk => "Dark - Shinobi Dusk",
            codex_core::config_types::ThemeName::DarkOledBlackPro => "Dark - OLED Black Pro",
            codex_core::config_types::ThemeName::DarkAmberTerminal => "Dark - Amber Terminal",
            codex_core::config_types::ThemeName::DarkAuroraFlux => "Dark - Aurora Flux",
            codex_core::config_types::ThemeName::DarkCharcoalRainbow => "Dark - Charcoal Rainbow",
            codex_core::config_types::ThemeName::DarkZenGarden => "Dark - Zen Garden",
            codex_core::config_types::ThemeName::DarkPaperLightPro => "Dark - Paper Light Pro",
            codex_core::config_types::ThemeName::Custom => "Custom",
        };
        let message = format!("✓ Theme changed to {}", theme_name);
        self.add_to_history(HistoryCell::new_background_event(message));
    }

    fn save_theme_to_config(&self, new_theme: codex_core::config_types::ThemeName) {
        // For now, just log the theme change - config saving could be implemented
        // using the core config system in a future update
        let theme_str = match new_theme {
            // Light themes
            codex_core::config_types::ThemeName::LightPhoton => "light-photon",
            codex_core::config_types::ThemeName::LightPrismRainbow => "light-prism-rainbow",
            codex_core::config_types::ThemeName::LightVividTriad => "light-vivid-triad",
            codex_core::config_types::ThemeName::LightPorcelain => "light-porcelain",
            codex_core::config_types::ThemeName::LightSandbar => "light-sandbar",
            codex_core::config_types::ThemeName::LightGlacier => "light-glacier",
            // Dark themes
            codex_core::config_types::ThemeName::DarkCarbonNight => "dark-carbon-night",
            codex_core::config_types::ThemeName::DarkShinobiDusk => "dark-shinobi-dusk",
            codex_core::config_types::ThemeName::DarkOledBlackPro => "dark-oled-black-pro",
            codex_core::config_types::ThemeName::DarkAmberTerminal => "dark-amber-terminal",
            codex_core::config_types::ThemeName::DarkAuroraFlux => "dark-aurora-flux",
            codex_core::config_types::ThemeName::DarkCharcoalRainbow => "dark-charcoal-rainbow",
            codex_core::config_types::ThemeName::DarkZenGarden => "dark-zen-garden",
            codex_core::config_types::ThemeName::DarkPaperLightPro => "dark-paper-light-pro",
            codex_core::config_types::ThemeName::Custom => "custom",
        };
        tracing::info!("Theme changed to: {}", theme_str);
        // Note: To persist the theme, add the following to your config.toml:
        // [tui.theme]
        // name = "{}"
    }

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

    pub(crate) fn composer_is_empty(&self) -> bool {
        self.bottom_pane.composer_is_empty()
    }

    /// Forward an `Op` directly to codex.
    pub(crate) fn submit_op(&self, op: Op) {
        if let Err(e) = self.codex_op_tx.send(op) {
            tracing::error!("failed to submit op: {e}");
        }
    }

    pub(crate) fn insert_history_lines(&mut self, lines: Vec<ratatui::text::Line<'static>>) {
        // Insert lines directly into history as text line cells
        for line in lines {
            self.history_cells.push(HistoryCell::new_text_line(line));
        }
        self.request_redraw();
    }

    pub(crate) fn handle_browser_command(&mut self, command_text: String) {
        // Parse the browser subcommand
        let parts: Vec<&str> = command_text.trim().split_whitespace().collect();

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

                // Navigate to URL and wait for it to load
                let latest_screenshot = self.latest_browser_screenshot.clone();
                let app_event_tx = self.app_event_tx.clone();
                let url_for_goto = full_url.clone();
                
                // Connect immediately, don't wait for message send
                tokio::spawn(async move {
                    // Get the global browser manager
                    let browser_manager = ChatWidget::get_browser_manager().await;
                    
                    // Enable browser mode and ensure it's visible (not headless) for URL navigation
                    browser_manager.set_enabled_sync(true);
                    {
                        let mut config = browser_manager.config.write().await;
                        config.headless = false; // Ensure browser is visible when navigating to URL
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
                        
                        browser_manager.set_navigation_callback(move |url| {
                            tracing::info!("Navigation callback triggered for URL: {}", url);
                            let latest_screenshot_inner = latest_screenshot_callback.clone();
                            let app_event_tx_inner = app_event_tx_callback.clone();
                            let url_inner = url.clone();
                            
                            tokio::spawn(async move {
                                // Get browser manager in the inner async block
                                let browser_manager_inner = ChatWidget::get_browser_manager().await;
                                // Capture screenshot after navigation
                                match browser_manager_inner.capture_screenshot_with_url().await {
                                    Ok((paths, _)) => {
                                        if let Some(first_path) = paths.first() {
                                            tracing::info!("Auto-captured screenshot after navigation: {}", first_path.display());
                                            
                                            // Update the latest screenshot
                                            if let Ok(mut latest) = latest_screenshot_inner.lock() {
                                                *latest = Some((first_path.clone(), url_inner.clone()));
                                            }
                                            
                                            // Send update event
                                            use codex_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                                            let _ = app_event_tx_inner.send(AppEvent::CodexEvent(Event {
                                                id: uuid::Uuid::new_v4().to_string(),
                                                msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                    screenshot_path: first_path.clone(),
                                                    url: url_inner,
                                                }),
                                            }));
                                        }
                                    }
                                    Err(e) => {
                                        tracing::error!("Failed to auto-capture screenshot: {}", e);
                                    }
                                }
                            });
                        }).await;
                    }
                    
                    // Set the browser manager as the global manager so both TUI and Session use the same instance
                    codex_browser::global::set_global_browser_manager(browser_manager.clone()).await;
                    
                    // Ensure the navigation callback is also set on the global manager
                    let global_manager = codex_browser::global::get_browser_manager().await;
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
                                let browser_manager = codex_browser::global::get_browser_manager().await;
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
                                                use codex_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                                                let _ = app_event_tx_inner.send(AppEvent::CodexEvent(Event {
                                                    id: uuid::Uuid::new_v4().to_string(),
                                                    msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                        screenshot_path: first_path.clone(),
                                                        url: url_inner,
                                                    }),
                                                }));
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
                            
                            // Capture initial screenshot
                            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                            match browser_manager.capture_screenshot_with_url().await {
                                Ok((paths, url)) => {
                                    if let Some(first_path) = paths.first() {
                                        tracing::info!("Initial screenshot captured: {}", first_path.display());
                                        
                                        // Update the latest screenshot
                                        if let Ok(mut latest) = latest_screenshot.lock() {
                                            *latest = Some((first_path.clone(), url.clone().unwrap_or_else(|| result.url.clone())));
                                        }
                                        
                                        // Send update event
                                        use codex_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                                        let _ = app_event_tx.send(AppEvent::CodexEvent(Event {
                                            id: uuid::Uuid::new_v4().to_string(),
                                            msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                screenshot_path: first_path.clone(),
                                                url: url.unwrap_or_else(|| result.url.clone()),
                                            }),
                                        }));
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

                format!(
                    "Browser mode enabled: {}\n",
                    full_url
                )
            } else {
                // It's a subcommand
                match first_arg {
                    "chrome" => {
                        // Connect to Chrome instance
                        // Check if there's a port specified after "chrome"
                        let port = if parts.len() > 1 {
                            // Try to parse the port number
                            parts[1].parse::<u16>().ok()
                        } else {
                            None
                        };

                        // Update the config to use Chrome connection
                        let latest_screenshot = self.latest_browser_screenshot.clone();
                        let app_event_tx = self.app_event_tx.clone();
                        let _port_display =
                            port.map_or("auto-detected".to_string(), |p| p.to_string());
                        let launch_port = port.unwrap_or(9222);

                        // Connect immediately, don't wait for message send
                        tokio::spawn(async move {
                            // Get the global browser manager
                            let browser_manager = ChatWidget::get_browser_manager().await;
                            browser_manager.set_enabled_sync(true);
                            
                            // Configure the manager for local Chrome connection
                            {
                                let mut config = browser_manager.config.write().await;
                                config.connect_port = Some(port.unwrap_or(0));
                                config.headless = false;
                                config.persist_profile = true;
                                config.enabled = true;
                            }

                            // Connect using the global manager
                            match browser_manager.start().await {
                                Ok(_) => {
                                    tracing::info!(
                                        "Connected to local Chrome instance"
                                    );
                                    
                                    // Set up navigation callback to auto-capture screenshots
                                    {
                                        let latest_screenshot_callback = latest_screenshot.clone();
                                        let app_event_tx_callback = app_event_tx.clone();
                                        
                                        browser_manager.set_navigation_callback(move |url| {
                                            tracing::info!("CDP Navigation callback triggered for URL: {}", url);
                                            let latest_screenshot_inner = latest_screenshot_callback.clone();
                                            let app_event_tx_inner = app_event_tx_callback.clone();
                                            let url_inner = url.clone();
                                            
                                            tokio::spawn(async move {
                                                // Wait a bit for page to stabilize
                                                tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                                                
                                                // Get browser manager in the inner async block
                                                let browser_manager_inner = ChatWidget::get_browser_manager().await;
                                                // Capture screenshot after navigation
                                                match browser_manager_inner.capture_screenshot_with_url().await {
                                                    Ok((paths, _)) => {
                                                        if let Some(first_path) = paths.first() {
                                                            tracing::info!("Auto-captured CDP screenshot: {}", first_path.display());
                                                            
                                                            // Update the latest screenshot
                                                            if let Ok(mut latest) = latest_screenshot_inner.lock() {
                                                                *latest = Some((first_path.clone(), url_inner.clone()));
                                                            }
                                                            
                                                            // Send update event
                                                            use codex_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                                                            let _ = app_event_tx_inner.send(AppEvent::CodexEvent(Event {
                                                                id: uuid::Uuid::new_v4().to_string(),
                                                                msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                                    screenshot_path: first_path.clone(),
                                                                    url: url_inner,
                                                                }),
                                                            }));
                                                        }
                                                    }
                                                    Err(e) => {
                                                        tracing::error!("Failed to auto-capture CDP screenshot: {}", e);
                                                    }
                                                }
                                            });
                                        }).await;
                                    }
                                    
                                    // Set the browser manager as the global manager
                                    codex_browser::global::set_global_browser_manager(browser_manager.clone()).await;
                                    
                                    // Ensure the navigation callback is also set on the global manager
                                    let global_manager = codex_browser::global::get_browser_manager().await;
                                    if let Some(global_manager) = global_manager {
                                        let latest_screenshot_global = latest_screenshot.clone();
                                        let app_event_tx_global = app_event_tx.clone();
                                        
                                        global_manager.set_navigation_callback(move |url| {
                                            tracing::info!("Global CDP navigation callback triggered for URL: {}", url);
                                            let latest_screenshot_inner = latest_screenshot_global.clone();
                                            let app_event_tx_inner = app_event_tx_global.clone();
                                            let url_inner = url.clone();
                                            
                                            tokio::spawn(async move {
                                                // Wait a moment for the navigation to complete
                                                tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
                                                
                                                // Capture screenshot after navigation
                                                let browser_manager = codex_browser::global::get_browser_manager().await;
                                                if let Some(browser_manager) = browser_manager {
                                                    match browser_manager.capture_screenshot_with_url().await {
                                                        Ok((paths, _url)) => {
                                                            if let Some(first_path) = paths.first() {
                                                                tracing::info!("Auto-captured screenshot after global CDP navigation: {}", first_path.display());
                                                                
                                                                // Update the latest screenshot
                                                                if let Ok(mut latest) = latest_screenshot_inner.lock() {
                                                                    *latest = Some((first_path.clone(), url_inner.clone()));
                                                                }
                                                                
                                                                // Send update event
                                                                use codex_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                                                                let _ = app_event_tx_inner.send(AppEvent::CodexEvent(Event {
                                                                    id: uuid::Uuid::new_v4().to_string(),
                                                                    msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                                        screenshot_path: first_path.clone(),
                                                                        url: url_inner,
                                                                    }),
                                                                }));
                                                            }
                                                        }
                                                        Err(e) => {
                                                            tracing::error!("Failed to auto-capture screenshot after global CDP navigation: {}", e);
                                                        }
                                                    }
                                                }
                                            });
                                        }).await;
                                    }
                                    
                                    tracing::info!(
                                        "All browser operations (TUI and LLM tools) will use external Chrome"
                                    );
                                    
                                    // Capture initial screenshot from current tab
                                    tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                                    match browser_manager.capture_screenshot_with_url().await {
                                        Ok((paths, url)) => {
                                            if let Some(first_path) = paths.first() {
                                                tracing::info!("Initial CDP screenshot captured: {}", first_path.display());
                                                
                                                // Update the latest screenshot
                                                if let Ok(mut latest) = latest_screenshot.lock() {
                                                    *latest = Some((first_path.clone(), url.clone().unwrap_or_else(|| "Chrome".to_string())));
                                                }
                                                
                                                // Send update event
                                                use codex_core::protocol::BrowserScreenshotUpdateEvent;
                                                let _ = app_event_tx.send(AppEvent::CodexEvent(Event {
                                                    id: uuid::Uuid::new_v4().to_string(),
                                                    msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                        screenshot_path: first_path.clone(),
                                                        url: url.unwrap_or_else(|| "Chrome".to_string()),
                                                    }),
                                                }));
                                            }
                                        }
                                        Err(e) => {
                                            tracing::error!("Failed to capture initial CDP screenshot: {}", e);
                                        }
                                    }
                                }
                                Err(e) => {
                                    tracing::warn!(
                                        "Failed to connect to local Chrome: {}. Trying to launch Chrome with debug port...",
                                        e
                                    );


                                    // Try launching Chrome with debug port
                                    #[cfg(target_os = "macos")]
                                    {
                                        let _ = tokio::process::Command::new("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
                                            .arg(format!("--remote-debugging-port={}", launch_port))
                                            .arg("--user-data-dir=/tmp/coder-chrome-profile")
                                            .spawn();
                                    }

                                    #[cfg(target_os = "linux")]
                                    {
                                        let _ = tokio::process::Command::new("google-chrome")
                                            .arg(format!("--remote-debugging-port={}", launch_port))
                                            .arg("--user-data-dir=/tmp/coder-chrome-profile")
                                            .spawn();
                                    }

                                    // Wait a bit for Chrome to start
                                    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

                                    // Try connecting again
                                    match browser_manager.start().await {
                                        Ok(_) => {
                                            tracing::info!("Successfully connected to Chrome after launching");
                                            
                                            // Set up navigation callback
                                            {
                                                let latest_screenshot_callback = latest_screenshot.clone();
                                                let app_event_tx_callback = app_event_tx.clone();
                                                
                                                browser_manager.set_navigation_callback(move |url| {
                                                    tracing::info!("CDP Navigation callback (launched) triggered for URL: {}", url);
                                                    let latest_screenshot_inner = latest_screenshot_callback.clone();
                                                    let app_event_tx_inner = app_event_tx_callback.clone();
                                                    let url_inner = url.clone();
                                                    
                                                    tokio::spawn(async move {
                                                        tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                                                        // Get browser manager in the inner async block
                                                        let browser_manager_inner = ChatWidget::get_browser_manager().await;
                                                        match browser_manager_inner.capture_screenshot_with_url().await {
                                                            Ok((paths, _)) => {
                                                                if let Some(first_path) = paths.first() {
                                                                    if let Ok(mut latest) = latest_screenshot_inner.lock() {
                                                                        *latest = Some((first_path.clone(), url_inner.clone()));
                                                                    }
                                                                    use codex_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                                                                    let _ = app_event_tx_inner.send(AppEvent::CodexEvent(Event {
                                                                        id: uuid::Uuid::new_v4().to_string(),
                                                                        msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                                            screenshot_path: first_path.clone(),
                                                                            url: url_inner,
                                                                        }),
                                                                    }));
                                                                }
                                                            }
                                                            Err(e) => {
                                                                tracing::error!("Failed to auto-capture launched CDP screenshot: {}", e);
                                                            }
                                                        }
                                                    });
                                                }).await;
                                            }
                                            
                                            // Set the browser manager as the global manager
                                            codex_browser::global::set_global_browser_manager(browser_manager.clone()).await;
                                            
                                            // Ensure the navigation callback is also set on the global manager
                                            let global_manager = codex_browser::global::get_browser_manager().await;
                                            if let Some(global_manager) = global_manager {
                                                let latest_screenshot_global = latest_screenshot.clone();
                                                let app_event_tx_global = app_event_tx.clone();
                                                
                                                global_manager.set_navigation_callback(move |url| {
                                                    tracing::info!("Global CDP (launched) navigation callback triggered for URL: {}", url);
                                                    let latest_screenshot_inner = latest_screenshot_global.clone();
                                                    let app_event_tx_inner = app_event_tx_global.clone();
                                                    let url_inner = url.clone();
                                                    
                                                    tokio::spawn(async move {
                                                        // Wait a moment for the navigation to complete
                                                        tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
                                                        
                                                        // Capture screenshot after navigation
                                                        let browser_manager = codex_browser::global::get_browser_manager().await;
                                                        if let Some(browser_manager) = browser_manager {
                                                            match browser_manager.capture_screenshot_with_url().await {
                                                                Ok((paths, _url)) => {
                                                                    if let Some(first_path) = paths.first() {
                                                                        tracing::info!("Auto-captured screenshot after global CDP (launched) navigation: {}", first_path.display());
                                                                        
                                                                        // Update the latest screenshot
                                                                        if let Ok(mut latest) = latest_screenshot_inner.lock() {
                                                                            *latest = Some((first_path.clone(), url_inner.clone()));
                                                                        }
                                                                        
                                                                        // Send update event
                                                                        use codex_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                                                                        let _ = app_event_tx_inner.send(AppEvent::CodexEvent(Event {
                                                                            id: uuid::Uuid::new_v4().to_string(),
                                                                            msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                                                screenshot_path: first_path.clone(),
                                                                                url: url_inner,
                                                                            }),
                                                                        }));
                                                                    }
                                                                }
                                                                Err(e) => {
                                                                    tracing::error!("Failed to auto-capture screenshot after global CDP (launched) navigation: {}", e);
                                                                }
                                                            }
                                                        }
                                                    });
                                                }).await;
                                            }
                                            
                                            // Capture initial screenshot
                                            tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
                                            match browser_manager.capture_screenshot_with_url().await {
                                                Ok((paths, url)) => {
                                                    if let Some(first_path) = paths.first() {
                                                        if let Ok(mut latest) = latest_screenshot.lock() {
                                                            *latest = Some((first_path.clone(), url.clone().unwrap_or_else(|| "Chrome".to_string())));
                                                        }
                                                        use codex_core::protocol::{BrowserScreenshotUpdateEvent, EventMsg};
                                                        let _ = app_event_tx.send(AppEvent::CodexEvent(Event {
                                                            id: uuid::Uuid::new_v4().to_string(),
                                                            msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
                                                                screenshot_path: first_path.clone(),
                                                                url: url.unwrap_or_else(|| "Chrome".to_string()),
                                                            }),
                                                        }));
                                                    }
                                                }
                                                Err(e) => {
                                                    tracing::error!("Failed to capture initial launched CDP screenshot: {}", e);
                                                }
                                            }
                                        }
                                        Err(e) => {
                                            tracing::error!(
                                                "Failed to connect to Chrome after launching: {}",
                                                e
                                            );
                                        }
                                    }
                                }
                            }
                        });

                        if port.is_some() {
                            format!(
                                "Connecting to local Chrome instance on port {}...\n",
                                port.unwrap_or(9222)
                            )
                        } else {
                            "Scanning for local Chrome instance with debug port...\n".to_string()
                        }
                    }
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
                        status_rx.recv().unwrap_or_else(|_| "Failed to get browser status.".to_string())
                    }
                    "fullpage" => {
                        if parts.len() > 2 {
                            match parts[2] {
                                "on" => {
                                    // Enable full-page mode
                                    tokio::spawn(async move {
                                        let browser_manager = ChatWidget::get_browser_manager().await;
                                        browser_manager.set_fullpage_sync(true);
                                    });
                                    "Full-page screenshot mode enabled (max 8 segments)."
                                        .to_string()
                                }
                                "off" => {
                                    // Disable full-page mode
                                    tokio::spawn(async move {
                                        let browser_manager = ChatWidget::get_browser_manager().await;
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
                                                let browser_manager = ChatWidget::get_browser_manager().await;
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
                                            let browser_manager = ChatWidget::get_browser_manager().await;
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
                            "Unknown browser command: '{}'\nUsage: /browser <url> | chrome [port] | off | status | fullpage | config\nAlias: /chrome [port]",
                            first_arg
                        )
                    }
                }
            }
        } else {
            "Browser commands:\n• /browser <url> - Open URL and enable browser mode\n• /browser chrome [port] - Connect to Chrome browser (CDP)\n• /chrome [port] - Alias for /browser chrome\n• /browser off - Disable browser mode\n• /browser status - Show current status\n• /browser fullpage [on|off] - Toggle full-page mode\n• /browser config <key> <value> - Update configuration".to_string()
        };

        // Add the response to the UI as a background event
        let lines = response
            .lines()
            .map(|line| Line::from(line.to_string()))
            .collect();
        self.add_to_history(HistoryCell::BackgroundEvent {
            view: TextBlock::new(lines),
        });
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
        let default_guess = if std::env::var("TERM_PROGRAM").unwrap_or_default() == "iTerm.app" {
            (7, 15)
        } else {
            (8, 16)
        };
        *self.cached_cell_size.get_or_init(|| {
            let size = crate::terminal_info::get_cell_size_pixels().unwrap_or(default_guess);

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
        use std::process::Command;

        let output = Command::new("git")
            .arg("rev-parse")
            .arg("--abbrev-ref")
            .arg("HEAD")
            .current_dir(&self.config.cwd)
            .output()
            .ok()?;

        if output.status.success() {
            let branch = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !branch.is_empty() && branch != "HEAD" {
                Some(branch)
            } else {
                // Try to get short commit hash if in detached HEAD state
                let commit_output = Command::new("git")
                    .arg("rev-parse")
                    .arg("--short")
                    .arg("HEAD")
                    .current_dir(&self.config.cwd)
                    .output()
                    .ok()?;

                if commit_output.status.success() {
                    let commit = String::from_utf8_lossy(&commit_output.stdout)
                        .trim()
                        .to_string();
                    if !commit.is_empty() {
                        Some(format!("detached: {}", commit))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        } else {
            None
        }
    }

    fn render_status_bar(&self, area: Rect, buf: &mut Buffer) {
        use crate::exec_command::relativize_to_home;
        use ratatui::layout::Margin;
        use ratatui::style::{Modifier, Style};
        use ratatui::text::{Line, Span};
        use ratatui::widgets::{Block, Borders, Paragraph};

        // Add same horizontal padding as the Message input (2 chars on each side)
        let horizontal_padding = 2u16;
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

        // Build status line spans
        let mut status_spans = vec![
            Span::styled("Coder", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw( " •  "),
            Span::styled("Model: ", Style::default().dim()),
            Span::styled(
                self.format_model_name(&self.config.model),
                Style::default().fg(crate::colors::secondary()),
            ),
            Span::raw("  •  "),
            Span::styled("Reasoning: ", Style::default().dim()),
            Span::styled(
                format!("{}", self.config.model_reasoning_effort),
                Style::default().fg(crate::colors::info()),
            ),
            Span::raw("  •  "),
            Span::styled("Directory: ", Style::default().dim()),
            Span::styled(cwd_str, Style::default().fg(crate::colors::info())),
        ];

        // Add git branch if available
        if let Some(branch) = self.get_git_branch() {
            status_spans.push(Span::raw("  •  "));
            status_spans.push(Span::styled("Branch: ", Style::default().dim()));
            status_spans.push(Span::styled(
                branch,
                Style::default().fg(crate::colors::success_green()),
            ));
        }

        let status_line = Line::from(status_spans);

        // Create box border similar to Message input
        let status_block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()));

        // Add padding inside the box (1 char horizontal only, like Message input)
        let inner_area = status_block.inner(padded_area);
        let padded_inner = inner_area.inner(Margin::new(1, 0));

        // Render the block first
        status_block.render(padded_area, buf);

        // Then render the text inside with padding, centered
        let status_widget = Paragraph::new(vec![status_line])
            .alignment(ratatui::layout::Alignment::Center);
        ratatui::widgets::Widget::render(status_widget, padded_inner, buf);
    }

    fn render_screenshot_highlevel(&self, path: &PathBuf, area: Rect, buf: &mut Buffer) {
        use image::GenericImageView; // for dimensions()
        use ratatui::widgets::Widget;
        use ratatui_image::picker::{Picker, ProtocolType};
        use ratatui_image::{Image, Resize};

        // open + decode
        let reader = match image::ImageReader::open(path) {
            Ok(r) => r,
            Err(_) => {
                self.render_screenshot_placeholder(path, area, buf);
                return;
            }
        };
        let dyn_img = match reader.decode() {
            Ok(img) => img,
            Err(_) => {
                self.render_screenshot_placeholder(path, area, buf);
                return;
            }
        };
        let (img_w, img_h) = dyn_img.dimensions();

        // picker (Retina 2x workaround preserved)
        let mut cached_picker = self.cached_picker.borrow_mut();
        if cached_picker.is_none() {
            let (fw, fh) = self.measured_font_size();
            *cached_picker = Some(Picker::from_fontsize((fw * 2, fh * 2)));
        }
        let picker = cached_picker.as_ref().unwrap();

        // quantize step by protocol to avoid rounding bias
        let (qx, qy): (u16, u16) = match picker.protocol_type() {
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

        let (used_cols, used_rows) = if rows_by_w <= rows {
            (cols, rows_by_w)
        } else {
            (cols_by_h, rows)
        };

        // quantize to protocol grid
        let mut used_cols_u16 = used_cols as u16;
        let mut used_rows_u16 = used_rows as u16;
        if qx > 1 {
            let rem = used_cols_u16 % qx;
            if rem != 0 {
                used_cols_u16 = used_cols_u16.saturating_sub(rem).max(qx);
            }
        }
        if qy > 1 {
            let rem = used_rows_u16 % qy;
            if rem != 0 {
                used_rows_u16 = used_rows_u16.saturating_sub(rem).max(qy);
            }
        }
        used_cols_u16 = used_cols_u16.min(area.width).max(1);
        used_rows_u16 = used_rows_u16.min(area.height).max(1);

        // center both axes
        let hpad = (area.width.saturating_sub(used_cols_u16)) / 2;
        let vpad = (area.height.saturating_sub(used_rows_u16)) / 2;
        let target = Rect {
            x: area.x + hpad,
            y: area.y + vpad,
            width: used_cols_u16,
            height: used_rows_u16,
        };

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
            match picker.new_protocol(dyn_img, target, Resize::Fit(None)) {
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
        use ratatui::widgets::{Block, Borders, Paragraph};

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

impl ChatWidget<'_> {
    /// Render the combined HUD with browser and/or agent panels based on what's active
    fn render_hud(&self, area: Rect, buf: &mut Buffer) {
        // Check what's active
        let has_browser_screenshot = self
            .latest_browser_screenshot
            .lock()
            .map(|lock| lock.is_some())
            .unwrap_or(false);
        let has_active_agents = !self.active_agents.is_empty() || self.agents_ready_to_start;

        // Add same horizontal padding as the Message input (2 chars on each side)
        let horizontal_padding = 2u16;
        let padded_area = Rect {
            x: area.x + horizontal_padding,
            y: area.y,
            width: area.width.saturating_sub(horizontal_padding * 2),
            height: area.height,
        };

        // Determine layout based on what's active
        if has_browser_screenshot && has_active_agents {
            // Both panels: 50/50 split
            let chunks =
                Layout::horizontal([Constraint::Percentage(50), Constraint::Percentage(50)])
                    .areas::<2>(padded_area);

            self.render_browser_panel(chunks[0], buf);
            self.render_agent_panel(chunks[1], buf);
        } else if has_browser_screenshot {
            // Only browser: 50% width on the left side
            let chunks =
                Layout::horizontal([Constraint::Percentage(50), Constraint::Percentage(50)])
                    .areas::<2>(padded_area);
            
            self.render_browser_panel(chunks[0], buf);
            // Right side remains empty
        } else if has_active_agents {
            // Only agents: 50% width on the left side
            let chunks =
                Layout::horizontal([Constraint::Percentage(50), Constraint::Percentage(50)])
                    .areas::<2>(padded_area);
            
            self.render_agent_panel(chunks[0], buf);
            // Right side remains empty
        }
    }

    /// Render the browser panel (left side when both panels are shown)
    fn render_browser_panel(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::style::Style;
        use ratatui::widgets::{Block, Borders, Widget};

        if let Ok(screenshot_lock) = self.latest_browser_screenshot.lock() {
            if let Some((screenshot_path, url)) = &*screenshot_lock {
                // Use the full area for the browser preview
                let screenshot_block = Block::default()
                    .borders(Borders::ALL)
                    .title(format!(" Browser - {} ", url))
                    .border_style(Style::default().fg(crate::colors::border()));

                let inner_screenshot = screenshot_block.inner(area);
                screenshot_block.render(area, buf);

                // Render the screenshot using the full inner area
                self.render_screenshot_highlevel(screenshot_path, inner_screenshot, buf);
            }
        }
    }


    /// Render the agent status panel in the HUD
    fn render_agent_panel(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::style::Style;
        use ratatui::text::{Line as RLine, Span, Text};
        use ratatui::widgets::{Block, Borders, Paragraph, Sparkline, SparklineBar, Widget, Wrap};

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
                (available_height.saturating_sub(sparkline_height), sparkline_height)
            } else if available_height > min_content_height {
                // Limited space - give minimum to content, rest to sparkline
                (min_content_height, available_height.saturating_sub(min_content_height).min(sparkline_height))
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
            Span::styled(
                &self.overall_task_status,
                Style::default().fg(status_color),
            ),
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
                    Span::styled(
                        task,
                        Style::default().fg(crate::colors::text_dim()),
                    ),
                ]));
            }
            
            if !wrapped_content.is_empty() {
                // Create paragraph with wrapping enabled for the long text content
                let wrapped_paragraph = Paragraph::new(Text::from(wrapped_content))
                    .wrap(Wrap { trim: false });
                wrapped_paragraph.render(wrapped_area, buf);
            }
        }

        // Render sparkline at the bottom if we have data and agents are active
        let sparkline_data = self.sparkline_data.borrow();
        
        // Debug logging
        tracing::debug!("Sparkline render check: data_len={}, agents={}, ready={}, height={}, actual_height={}, area={:?}",
            sparkline_data.len(),
            self.active_agents.len(),
            self.agents_ready_to_start,
            sparkline_height,
            actual_sparkline_height,
            sparkline_area
        );
        
        if !sparkline_data.is_empty() && (!self.active_agents.is_empty() || self.agents_ready_to_start) && actual_sparkline_height > 0 {
            // Convert data to SparklineBar with colors based on completion status
            let bars: Vec<SparklineBar> = sparkline_data.iter()
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
                sparkline_data.iter().map(|(v, _)| *v).max().unwrap_or(4).max(4)
            } else {
                // Running phase - use larger max
                sparkline_data.iter().map(|(v, _)| *v).max().unwrap_or(10).max(10)
            };
            
            let sparkline = Sparkline::default()
                .data(bars)
                .max(max_value); // Dynamic max for better visibility
            sparkline.render(sparkline_area, buf);
        }
    }

    fn begin_stream(&mut self, kind: StreamKind) {
        if let Some(current) = self.current_stream {
            if current != kind {
                self.finalize_stream(current);
            }
        }

        if self.current_stream != Some(kind) {
            self.current_stream = Some(kind);
            self.stream_header_emitted = false;
            // Clear any previous live content; we're starting a new stream.
            self.live_builder = RowBuilder::new(self.live_builder.width());
            self.emit_stream_header(kind);
        }
    }

    fn stream_push_and_maybe_commit(&mut self, delta: &str) {
        self.live_builder.push_fragment(delta);

        // Commit overflow rows (small batches) while keeping the last N rows visible.
        let drained = self
            .live_builder
            .drain_commit_ready(self.live_max_rows as usize);
        if !drained.is_empty() {
            let mut lines: Vec<ratatui::text::Line<'static>> = Vec::new();
            for r in drained {
                let line = match self.current_stream {
                    Some(StreamKind::Reasoning) => {
                        // Use dimmer color for reasoning text
                        use ratatui::text::Span;
                        use ratatui::style::Style;
                        ratatui::text::Line::from(Span::styled(r.text, Style::default().fg(crate::colors::text_dim())))
                    }
                    _ => ratatui::text::Line::from(r.text)
                };
                lines.push(line);
            }
            // Add to in-memory history instead of scrollback
            for line in lines {
                // Use dimmed reasoning for thinking to get both markdown and dimming
                if matches!(self.current_stream, Some(StreamKind::Reasoning)) {
                    self.history_cells.push(HistoryCell::new_dimmed_reasoning_line(line));
                } else {
                    self.history_cells.push(HistoryCell::new_text_line(line));
                }
            }
            // Ensure input focus is maintained when adding streamed content
            self.bottom_pane.ensure_input_focus();
            self.app_event_tx.send(AppEvent::RequestRedraw);
        }

        // Clear the live ring since streaming content is now in the main scrollable area
        self.bottom_pane.clear_live_ring();
    }

    fn finalize_stream(&mut self, kind: StreamKind) {
        if self.current_stream != Some(kind) {
            // Nothing to do; either already finalized or not the active stream.
            return;
        }
        // Flush any partial line as a full row, then drain all remaining rows.
        self.live_builder.end_line();
        let remaining = self.live_builder.drain_rows();
        // TODO: Re-add markdown rendering for assistant answers and reasoning.
        // When finalizing, pass the accumulated text through `markdown::append_markdown`
        // to build styled `Line<'static>` entries instead of raw plain text lines.
        if !remaining.is_empty() {
            let mut lines: Vec<ratatui::text::Line<'static>> = Vec::new();
            for r in remaining {
                let line = match kind {
                    StreamKind::Reasoning => {
                        // Use dimmer color for reasoning text
                        use ratatui::text::Span;
                        use ratatui::style::Style;
                        ratatui::text::Line::from(Span::styled(r.text, Style::default().fg(crate::colors::text_dim())))
                    }
                    _ => ratatui::text::Line::from(r.text)
                };
                lines.push(line);
            }
            // Close the block with a blank line for readability (except for reasoning which already has spacing)
            if kind != StreamKind::Reasoning {
                lines.push(ratatui::text::Line::from(""));
            }
            // Add to in-memory history instead of scrollback
            for line in lines {
                // Use dimmed reasoning for thinking to get both markdown and dimming
                if matches!(self.current_stream, Some(StreamKind::Reasoning)) {
                    self.history_cells.push(HistoryCell::new_dimmed_reasoning_line(line));
                } else {
                    self.history_cells.push(HistoryCell::new_text_line(line));
                }
            }
            // Ensure input focus is maintained when adding streamed content
            self.bottom_pane.ensure_input_focus();
            self.app_event_tx.send(AppEvent::RequestRedraw);
        }

        // Clear the live overlay and reset state for the next stream.
        self.live_builder = RowBuilder::new(self.live_builder.width());
        self.bottom_pane.clear_live_ring();
        self.current_stream = None;
        self.stream_header_emitted = false;
    }
}

impl WidgetRef for &ChatWidget<'_> {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::style::Style;

        // Fill entire area with theme background
        let bg_style = Style::default()
            .bg(crate::colors::background())
            .fg(crate::colors::text());
        for y in area.top()..area.bottom() {
            for x in area.left()..area.right() {
                buf[(x, y)].set_style(bg_style);
            }
        }

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
        let padding = 3u16;
        let content_area = Rect {
            x: history_area.x + padding,
            y: history_area.y,
            width: history_area.width.saturating_sub(padding * 2),
            height: history_area.height,
        };

        // Collect all content items into a single list
        let mut all_content: Vec<&HistoryCell> = Vec::new();

        // Add all history cells
        for cell in &self.history_cells {
            all_content.push(cell);
        }

        // Add active/streaming cell if present
        if let Some(cell) = &self.active_history_cell {
            all_content.push(cell);
        }

        // Add live streaming content if present
        let streaming_lines = self
            .live_builder
            .display_rows()
            .into_iter()
            .map(|r| {
                if matches!(self.current_stream, Some(StreamKind::Reasoning)) {
                    // Apply dimming to live reasoning content
                    use ratatui::text::Span;
                    use ratatui::style::Style;
                    ratatui::text::Line::from(Span::styled(r.text, Style::default().fg(crate::colors::text_dim())))
                } else {
                    ratatui::text::Line::from(r.text)
                }
            })
            .collect::<Vec<_>>();

        let streaming_cell = if !streaming_lines.is_empty() {
            Some(HistoryCell::new_streaming_content(streaming_lines))
        } else {
            None
        };

        if let Some(ref cell) = streaming_cell {
            all_content.push(cell);
        }

        // Calculate total content height
        let mut total_height = 0u16;
        let mut item_heights = Vec::new();
        for item in &all_content {
            let h = item.desired_height(content_area.width);
            item_heights.push(h);
            total_height += h;
        }

        // Check for active animations and clean up faded-out cells
        let has_active_animation = self.history_cells.iter().any(|cell| {
            if let HistoryCell::AnimatedWelcome {
                start_time,
                completed,
                fade_start,
                faded_out,
            } = cell
            {
                // Check for initial animation
                if !completed.get() {
                    let elapsed = start_time.elapsed();
                    let animation_duration = std::time::Duration::from_secs(2);
                    return elapsed < animation_duration;
                }

                // Check for fade animation
                if let Some(fade_time) = fade_start.get() {
                    if !faded_out.get() {
                        let fade_elapsed = fade_time.elapsed();
                        let fade_duration = std::time::Duration::from_millis(800);
                        return fade_elapsed < fade_duration;
                    }
                }

                false
            } else {
                false
            }
        });

        // Note: Faded-out AnimatedWelcome cells are cleaned up in process_animation_cleanup

        if has_active_animation {
            self.app_event_tx.send(AppEvent::RequestRedraw);
        }

        // Calculate scroll position and vertical alignment
        let (start_y, scroll_pos) = if total_height <= content_area.height {
            // Content fits - align to bottom of container
            let start_y = content_area.y + content_area.height.saturating_sub(total_height);
            (start_y, 0u16) // No scrolling needed
        } else {
            // Content overflows - calculate scroll position
            // scroll_offset of 0 = show newest at bottom
            // scroll_offset > 0 = scroll up to see older content
            let max_scroll = total_height.saturating_sub(content_area.height);
            // Clamp scroll_offset to valid range (can't mutate in render, so just clamp for display)
            let clamped_scroll_offset = self.scroll_offset.min(max_scroll);
            (content_area.y, clamped_scroll_offset)
        };

        // Render the scrollable content
        let mut content_y = 0u16; // Position within the content
        let mut screen_y = start_y; // Position on screen

        for (idx, item) in all_content.iter().enumerate() {
            let item_height = item_heights[idx];

            // Skip items that are scrolled off the top
            if content_y + item_height <= scroll_pos {
                content_y += item_height;
                continue;
            }

            // Stop if we've gone past the bottom of the screen
            if screen_y >= content_area.y + content_area.height {
                break;
            }

            // Calculate how much of this item to skip from the top
            let skip_top = if content_y < scroll_pos {
                scroll_pos - content_y
            } else {
                0
            };

            // Calculate how much height is available for this item
            let available_height = (content_area.y + content_area.height).saturating_sub(screen_y);
            let visible_height = item_height.saturating_sub(skip_top).min(available_height);

            if visible_height > 0 {
                let item_area = Rect {
                    x: content_area.x,
                    y: screen_y,
                    width: content_area.width,
                    height: visible_height,
                };

                // Render the item
                item.render_ref(item_area, buf);
                screen_y += visible_height;
            }

            content_y += item_height;
        }

        // Render the bottom pane directly without a border for now
        // The composer has its own layout with hints at the bottom
        (&self.bottom_pane).render(bottom_pane_area, buf);
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
