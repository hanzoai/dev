use std::collections::HashSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use chrono::DateTime;
use chrono::Utc;
use code_core::ConversationItem;
use code_core::ConversationsPage;
use code_core::Cursor;
use code_core::RolloutRecorder;
use code_core::INTERACTIVE_SESSION_SOURCES;
use color_eyre::eyre::Result;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyEventKind;
use ratatui::layout::Constraint;
use ratatui::layout::Layout;
use ratatui::layout::Rect;
use ratatui::style::Stylize as _;
use ratatui::text::Line;
use ratatui::text::Span;
use tokio::sync::mpsc;
use tokio_stream::StreamExt;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::text_formatting::truncate_text;
use crate::tui::FrameRequester;
use crate::tui::Tui;
use crate::tui::TuiEvent;
use code_protocol::models::ContentItem;
use code_protocol::models::ResponseItem;
use code_protocol::protocol::InputMessageKind;
use code_protocol::protocol::USER_MESSAGE_BEGIN;

const PAGE_SIZE: usize = 25;
const LOAD_NEAR_THRESHOLD: usize = 5;

#[derive(Debug, Clone)]
pub enum ResumeSelection {
    StartFresh,
    Resume(PathBuf),
    Exit,
}

#[derive(Clone)]
struct PageLoadRequest {
    code_home: PathBuf,
    cursor: Option<Cursor>,
    request_token: usize,
    search_token: Option<usize>,
}

type PageLoader = Arc<dyn Fn(PageLoadRequest) + Send + Sync>;

enum BackgroundEvent {
    PageLoaded {
        request_token: usize,
        search_token: Option<usize>,
        page: std::io::Result<ConversationsPage>,
    },
}

/// Interactive session picker that lists recorded rollout files with simple
/// search and pagination. Shows the first user input as the preview, relative
/// time (e.g., "5 seconds ago"), and the absolute path.
pub async fn run_resume_picker(tui: &mut Tui, code_home: &Path) -> Result<ResumeSelection> {
    let alt = AltScreenGuard::enter(tui);
    let (bg_tx, bg_rx) = mpsc::unbounded_channel();

    let loader_tx = bg_tx.clone();
    let page_loader: PageLoader = Arc::new(move |request: PageLoadRequest| {
        let tx = loader_tx.clone();
        tokio::spawn(async move {
            let page = RolloutRecorder::list_conversations(
                &request.code_home,
                PAGE_SIZE,
                request.cursor.as_ref(),
                INTERACTIVE_SESSION_SOURCES,
            )
            .await;
            let _ = tx.send(BackgroundEvent::PageLoaded {
                request_token: request.request_token,
                search_token: request.search_token,
                page,
            });
        });
    });

    let mut state = PickerState::new(
        code_home.to_path_buf(),
        alt.tui.frame_requester(),
        page_loader,
    );
    state.load_initial_page().await?;
    state.request_frame();

    let mut tui_events = alt.tui.event_stream().fuse();
    let mut background_events = UnboundedReceiverStream::new(bg_rx).fuse();

    loop {
        tokio::select! {
            Some(ev) = tui_events.next() => {
                match ev {
                    TuiEvent::Key(key) => {
                        if matches!(key.kind, KeyEventKind::Release) {
                            continue;
                        }
                        if let Some(sel) = state.handle_key(key).await? {
                            return Ok(sel);
                        }
                    }
                    TuiEvent::Draw => {
                        if let Ok(size) = alt.tui.terminal.size() {
                            let list_height = size.height.saturating_sub(3) as usize;
                            state.update_view_rows(list_height);
                            state.ensure_minimum_rows_for_view(list_height);
                        }
                        draw_picker(alt.tui, &state)?;
                    }
                    _ => {}
                }
            }
            Some(event) = background_events.next() => {
                state.handle_background_event(event)?;
            }
            else => break,
        }
    }

    // Fallback – treat as cancel/new
    Ok(ResumeSelection::StartFresh)
}

/// RAII guard that ensures we leave the alt-screen on scope exit.
struct AltScreenGuard<'a> {
    tui: &'a mut Tui,
}

impl<'a> AltScreenGuard<'a> {
    fn enter(tui: &'a mut Tui) -> Self {
        let _ = tui.enter_alt_screen();
        Self { tui }
    }
}

impl Drop for AltScreenGuard<'_> {
    fn drop(&mut self) {
        let _ = self.tui.leave_alt_screen();
    }
}

struct PickerState {
    code_home: PathBuf,
    requester: FrameRequester,
    pagination: PaginationState,
    all_rows: Vec<Row>,
    filtered_rows: Vec<Row>,
    seen_paths: HashSet<PathBuf>,
    selected: usize,
    scroll_top: usize,
    query: String,
    search_state: SearchState,
    next_request_token: usize,
    next_search_token: usize,
    page_loader: PageLoader,
    view_rows: Option<usize>,
}

struct PaginationState {
    next_cursor: Option<Cursor>,
    num_scanned_files: usize,
    reached_scan_cap: bool,
    loading: LoadingState,
}

#[derive(Clone, Copy, Debug)]
enum LoadingState {
    Idle,
    Pending(PendingLoad),
}

#[derive(Clone, Copy, Debug)]
struct PendingLoad {
    request_token: usize,
    search_token: Option<usize>,
}

#[derive(Clone, Copy, Debug)]
enum SearchState {
    Idle,
    Active { token: usize },
}

enum LoadTrigger {
    Scroll,
    Search { token: usize },
}

impl LoadingState {
    fn is_pending(&self) -> bool {
        matches!(self, LoadingState::Pending(_))
    }
}

impl SearchState {
    fn active_token(&self) -> Option<usize> {
        match self {
            SearchState::Idle => None,
            SearchState::Active { token } => Some(*token),
        }
    }

    fn is_active(&self) -> bool {
        self.active_token().is_some()
    }
}

#[derive(Clone)]
struct Row {
    path: PathBuf,
    preview: String,
    ts: Option<DateTime<Utc>>,
}

impl PickerState {
    fn new(code_home: PathBuf, requester: FrameRequester, page_loader: PageLoader) -> Self {
        Self {
            code_home,
            requester,
            pagination: PaginationState {
                next_cursor: None,
                num_scanned_files: 0,
                reached_scan_cap: false,
                loading: LoadingState::Idle,
            },
            all_rows: Vec::new(),
            filtered_rows: Vec::new(),
            seen_paths: HashSet::new(),
            selected: 0,
            scroll_top: 0,
            query: String::new(),
            search_state: SearchState::Idle,
            next_request_token: 0,
            next_search_token: 0,
            page_loader,
            view_rows: None,
        }
    }

    fn request_frame(&self) {
        self.requester.schedule_frame();
    }

    async fn handle_key(&mut self, key: KeyEvent) -> Result<Option<ResumeSelection>> {
        match key.code {
            KeyCode::Esc => return Ok(Some(ResumeSelection::StartFresh)),
            KeyCode::Char('c')
                if key
                    .modifiers
                    .contains(crossterm::event::KeyModifiers::CONTROL) =>
            {
                return Ok(Some(ResumeSelection::Exit));
            }
            KeyCode::Enter => {
                if let Some(row) = self.filtered_rows.get(self.selected) {
                    return Ok(Some(ResumeSelection::Resume(row.path.clone())));
                }
            }
            KeyCode::Up => {
                if self.selected > 0 {
                    self.selected -= 1;
                    self.ensure_selected_visible();
                }
                self.request_frame();
            }
            KeyCode::Down => {
                if self.selected + 1 < self.filtered_rows.len() {
                    self.selected += 1;
                    self.ensure_selected_visible();
                }
                self.maybe_load_more_for_scroll();
                self.request_frame();
            }
            KeyCode::PageUp => {
                let step = self.view_rows.unwrap_or(10).max(1);
                if self.selected > 0 {
                    self.selected = self.selected.saturating_sub(step);
                    self.ensure_selected_visible();
                    self.request_frame();
                }
            }
            KeyCode::PageDown => {
                if !self.filtered_rows.is_empty() {
                    let step = self.view_rows.unwrap_or(10).max(1);
                    let max_index = self.filtered_rows.len().saturating_sub(1);
                    self.selected = (self.selected + step).min(max_index);
                    self.ensure_selected_visible();
                    self.maybe_load_more_for_scroll();
                    self.request_frame();
                }
            }
            KeyCode::Backspace => {
                let mut new_query = self.query.clone();
                new_query.pop();
                self.set_query(new_query);
            }
            KeyCode::Char(c) => {
                // basic text input for search
                if !key
                    .modifiers
                    .contains(crossterm::event::KeyModifiers::CONTROL)
                    && !key.modifiers.contains(crossterm::event::KeyModifiers::ALT)
                {
                    let mut new_query = self.query.clone();
                    new_query.push(c);
                    self.set_query(new_query);
                }
            }
            _ => {}
        }
        Ok(None)
    }

    async fn load_initial_page(&mut self) -> Result<()> {
        let page = RolloutRecorder::list_conversations(
            &self.code_home,
            PAGE_SIZE,
            None,
            INTERACTIVE_SESSION_SOURCES,
        )
        .await?;
        self.reset_pagination();
        self.all_rows.clear();
        self.filtered_rows.clear();
        self.seen_paths.clear();
        self.search_state = SearchState::Idle;
        self.selected = 0;
        self.ingest_page(page);
        Ok(())
    }

    fn handle_background_event(&mut self, event: BackgroundEvent) -> Result<()> {
        match event {
            BackgroundEvent::PageLoaded {
                request_token,
                search_token,
                page,
            } => {
                let pending = match self.pagination.loading {
                    LoadingState::Pending(pending) => pending,
                    LoadingState::Idle => return Ok(()),
                };
                if pending.request_token != request_token {
                    return Ok(());
                }
                self.pagination.loading = LoadingState::Idle;
                let page = page.map_err(color_eyre::Report::from)?;
                self.ingest_page(page);
                let completed_token = pending.search_token.or(search_token);
                self.continue_search_if_token_matches(completed_token);
            }
        }
        Ok(())
    }

    fn reset_pagination(&mut self) {
        self.pagination.next_cursor = None;
        self.pagination.num_scanned_files = 0;
        self.pagination.reached_scan_cap = false;
        self.pagination.loading = LoadingState::Idle;
    }

    fn ingest_page(&mut self, page: ConversationsPage) {
        if let Some(cursor) = page.next_cursor.clone() {
            self.pagination.next_cursor = Some(cursor);
        } else {
            self.pagination.next_cursor = None;
        }
        self.pagination.num_scanned_files = self
            .pagination
            .num_scanned_files
            .saturating_add(page.num_scanned_files);
        if page.reached_scan_cap {
            self.pagination.reached_scan_cap = true;
        }

        let rows = rows_from_items(page.items);
        for row in rows {
            if self.seen_paths.insert(row.path.clone()) {
                self.all_rows.push(row);
            }
        }

        self.apply_filter();
    }

    fn apply_filter(&mut self) {
        if self.query.is_empty() {
            self.filtered_rows = self.all_rows.clone();
        } else {
            let q = self.query.to_lowercase();
            self.filtered_rows = self
                .all_rows
                .iter()
                .filter(|r| r.preview.to_lowercase().contains(&q))
                .cloned()
                .collect();
        }
        if self.selected >= self.filtered_rows.len() {
            self.selected = self.filtered_rows.len().saturating_sub(1);
        }
        if self.filtered_rows.is_empty() {
            self.scroll_top = 0;
        }
        self.ensure_selected_visible();
        self.request_frame();
    }

    fn set_query(&mut self, new_query: String) {
        if self.query == new_query {
            return;
        }
        self.query = new_query;
        self.selected = 0;
        self.apply_filter();
        if self.query.is_empty() {
            self.search_state = SearchState::Idle;
            return;
        }
        if !self.filtered_rows.is_empty() {
            self.search_state = SearchState::Idle;
            return;
        }
        if self.pagination.reached_scan_cap || self.pagination.next_cursor.is_none() {
            self.search_state = SearchState::Idle;
            return;
        }
        let token = self.allocate_search_token();
        self.search_state = SearchState::Active { token };
        self.load_more_if_needed(LoadTrigger::Search { token });
    }

    fn continue_search_if_needed(&mut self) {
        let Some(token) = self.search_state.active_token() else {
            return;
        };
        if !self.filtered_rows.is_empty() {
            self.search_state = SearchState::Idle;
            return;
        }
        if self.pagination.reached_scan_cap || self.pagination.next_cursor.is_none() {
            self.search_state = SearchState::Idle;
            return;
        }
        self.load_more_if_needed(LoadTrigger::Search { token });
    }

    fn continue_search_if_token_matches(&mut self, completed_token: Option<usize>) {
        let Some(active) = self.search_state.active_token() else {
            return;
        };
        if let Some(token) = completed_token
            && token != active
        {
            return;
        }
        self.continue_search_if_needed();
    }

    fn ensure_selected_visible(&mut self) {
        if self.filtered_rows.is_empty() {
            self.scroll_top = 0;
            return;
        }
        let capacity = self.view_rows.unwrap_or(self.filtered_rows.len()).max(1);

        if self.selected < self.scroll_top {
            self.scroll_top = self.selected;
        } else {
            let last_visible = self.scroll_top.saturating_add(capacity - 1);
            if self.selected > last_visible {
                self.scroll_top = self.selected.saturating_sub(capacity - 1);
            }
        }

        let max_start = self.filtered_rows.len().saturating_sub(capacity);
        if self.scroll_top > max_start {
            self.scroll_top = max_start;
        }
    }

    fn ensure_minimum_rows_for_view(&mut self, minimum_rows: usize) {
        if minimum_rows == 0 {
            return;
        }
        if self.filtered_rows.len() >= minimum_rows {
            return;
        }
        if self.pagination.loading.is_pending() || self.pagination.next_cursor.is_none() {
            return;
        }
        if let Some(token) = self.search_state.active_token() {
            self.load_more_if_needed(LoadTrigger::Search { token });
        } else {
            self.load_more_if_needed(LoadTrigger::Scroll);
        }
    }

    fn update_view_rows(&mut self, rows: usize) {
        self.view_rows = if rows == 0 { None } else { Some(rows) };
        self.ensure_selected_visible();
    }

    fn maybe_load_more_for_scroll(&mut self) {
        if self.pagination.loading.is_pending() {
            return;
        }
        if self.pagination.next_cursor.is_none() {
            return;
        }
        if self.filtered_rows.is_empty() {
            return;
        }
        let remaining = self.filtered_rows.len().saturating_sub(self.selected + 1);
        if remaining <= LOAD_NEAR_THRESHOLD {
            self.load_more_if_needed(LoadTrigger::Scroll);
        }
    }

    fn load_more_if_needed(&mut self, trigger: LoadTrigger) {
        if self.pagination.loading.is_pending() {
            return;
        }
        let Some(cursor) = self.pagination.next_cursor.clone() else {
            return;
        };
        let request_token = self.allocate_request_token();
        let search_token = match trigger {
            LoadTrigger::Scroll => None,
            LoadTrigger::Search { token } => Some(token),
        };
        self.pagination.loading = LoadingState::Pending(PendingLoad {
            request_token,
            search_token,
        });
        self.request_frame();

        (self.page_loader)(PageLoadRequest {
            code_home: self.code_home.clone(),
            cursor: Some(cursor),
            request_token,
            search_token,
        });
    }

    fn allocate_request_token(&mut self) -> usize {
        let token = self.next_request_token;
        self.next_request_token = self.next_request_token.wrapping_add(1);
        token
    }

    fn allocate_search_token(&mut self) -> usize {
        let token = self.next_search_token;
        self.next_search_token = self.next_search_token.wrapping_add(1);
        token
    }
}

fn rows_from_items(items: Vec<ConversationItem>) -> Vec<Row> {
    items.into_iter().map(|item| head_to_row(&item)).collect()
}

fn head_to_row(item: &ConversationItem) -> Row {
    let mut ts: Option<DateTime<Utc>> = None;
    if let Some(first) = item.head.first()
        && let Some(t) = first.get("timestamp").and_then(|v| v.as_str())
        && let Ok(parsed) = chrono::DateTime::parse_from_rfc3339(t)
    {
        ts = Some(parsed.with_timezone(&Utc));
    }

    let preview = preview_from_head(&item.head)
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| String::from("(no message yet)"));

    Row {
        path: item.path.clone(),
        preview,
        ts,
    }
}

fn preview_from_head(head: &[serde_json::Value]) -> Option<String> {
    head.iter()
        .filter_map(|value| serde_json::from_value::<ResponseItem>(value.clone()).ok())
        .find_map(|item| match item {
            ResponseItem::Message { content, .. } => {
                // Find the actual user message (as opposed to user instructions or ide context)
                let preview = content
                    .into_iter()
                    .filter_map(|content| match content {
                        ContentItem::InputText { text }
                            if matches!(
                                InputMessageKind::from(("user", text.as_str())),
                                InputMessageKind::Plain
                            ) =>
                        {
                            // Strip ide context.
                            let text = match text.find(USER_MESSAGE_BEGIN) {
                                Some(idx) => {
                                    text[idx + USER_MESSAGE_BEGIN.len()..].trim().to_string()
                                }
                                None => text,
                            };
                            Some(text)
                        }
                        _ => None,
                    })
                    .collect::<String>();

                if preview.is_empty() {
                    None
                } else {
                    Some(preview)
                }
            }
            _ => None,
        })
}

fn draw_picker(tui: &mut Tui, state: &PickerState) -> std::io::Result<()> {
    // Render full-screen overlay
    let height = tui.terminal.size()?.height;
    tui.draw(height, |frame| {
        let area = frame.area();
        let [header, search, list, hint] = Layout::vertical([
            Constraint::Length(1),
            Constraint::Length(1),
            Constraint::Min(area.height.saturating_sub(3)),
            Constraint::Length(1),
        ])
        .areas(area);

        // Header
        frame.render_widget_ref(
            Line::from(vec!["Resume a previous session".bold().cyan()]),
            header,
        );

        // Search line
        let q = if state.query.is_empty() {
            "Type to search".dim().to_string()
        } else {
            format!("Search: {}", state.query)
        };
        frame.render_widget_ref(Line::from(q), search);

        // List
        render_list(frame, list, state);

        // Hint line
        let hint_line: Line = vec![
            "Enter".bold(),
            " to resume ".into(),
            "• ".dim(),
            "Esc".bold(),
            " to start new ".into(),
            "• ".dim(),
            "Ctrl+C".into(),
            " to quit ".into(),
            "• ".dim(),
            "↑/↓".into(),
            " to browse".dim(),
        ]
        .into();
        frame.render_widget_ref(hint_line, hint);
    })
}

fn render_list(frame: &mut crate::custom_terminal::Frame, area: Rect, state: &PickerState) {
    if area.height == 0 {
        return;
    }

    let rows = &state.filtered_rows;
    if rows.is_empty() {
        let message = render_empty_state_line(state);
        frame.render_widget_ref(message, area);
        return;
    }

    let capacity = area.height as usize;
    let start = state.scroll_top.min(rows.len().saturating_sub(1));
    let end = rows.len().min(start + capacity);
    let mut y = area.y;

    for (idx, row) in rows[start..end].iter().enumerate() {
        let is_sel = start + idx == state.selected;
        let marker = if is_sel { "> ".bold() } else { "  ".into() };
        let ts = row
            .ts
            .map(human_time_ago)
            .unwrap_or_else(|| "".to_string())
            .dim();
        let max_cols = area.width.saturating_sub(6) as usize;
        let preview = truncate_text(&row.preview, max_cols);

        let line: Line = vec![marker, ts, "  ".into(), preview.into()].into();
        let rect = Rect::new(area.x, y, area.width, 1);
        frame.render_widget_ref(line, rect);
        y = y.saturating_add(1);
    }

    if state.pagination.loading.is_pending() && y < area.y.saturating_add(area.height) {
        let loading_line: Line = vec!["  ".into(), "Loading older sessions…".italic().dim()].into();
        let rect = Rect::new(area.x, y, area.width, 1);
        frame.render_widget_ref(loading_line, rect);
    }
}

fn render_empty_state_line(state: &PickerState) -> Line<'static> {
    if !state.query.is_empty() {
        if state.search_state.is_active()
            || (state.pagination.loading.is_pending() && state.pagination.next_cursor.is_some())
        {
            return vec!["Searching…".italic().dim()].into();
        }
        if state.pagination.reached_scan_cap {
            let msg = format!(
                "Search scanned first {} sessions; more may exist",
                state.pagination.num_scanned_files
            );
            return vec![Span::from(msg).italic().dim()].into();
        }
        return vec!["No results for your search".italic().dim()].into();
    }

    if state.all_rows.is_empty() && state.pagination.num_scanned_files == 0 {
        return vec!["No sessions yet".italic().dim()].into();
    }

    if state.pagination.loading.is_pending() {
        return vec!["Loading older sessions…".italic().dim()].into();
    }

    vec!["No sessions yet".italic().dim()].into()
}

fn human_time_ago(ts: DateTime<Utc>) -> String {
    let now = Utc::now();
    let delta = now - ts;
    let secs = delta.num_seconds();
    if secs < 60 {
        let n = secs.max(0);
        if n == 1 {
            format!("{n} second ago")
        } else {
            format!("{n} seconds ago")
        }
    } else if secs < 60 * 60 {
        let m = secs / 60;
        if m == 1 {
            format!("{m} minute ago")
        } else {
            format!("{m} minutes ago")
        }
    } else if secs < 60 * 60 * 24 {
        let h = secs / 3600;
        if h == 1 {
            format!("{h} hour ago")
        } else {
            format!("{h} hours ago")
        }
    } else {
        let d = secs / (60 * 60 * 24);
        if d == 1 {
            format!("{d} day ago")
        } else {
            format!("{d} days ago")
        }
    }
}

