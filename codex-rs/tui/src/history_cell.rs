use crate::diff_render::create_diff_summary_with_width;
use crate::exec_command::strip_bash_lc_and_escape;
use crate::slash_command::SlashCommand;
use crate::text_formatting::format_json_compact;
use base64::Engine;
use codex_ansi_escape::ansi_escape_line;
use codex_common::create_config_summary_entries;
use codex_common::elapsed::format_duration;
use codex_core::config::Config;
use codex_core::config_types::ReasoningEffort;
use codex_core::parse_command::ParsedCommand;
use codex_core::plan_tool::PlanItemArg;
use codex_core::plan_tool::StepStatus;
use codex_core::plan_tool::UpdatePlanArgs;
use codex_core::protocol::FileChange;
use codex_core::protocol::McpInvocation;
use codex_core::protocol::SessionConfiguredEvent;
use codex_core::protocol::TokenUsage;
use image::DynamicImage;
use image::ImageReader;
use mcp_types::EmbeddedResourceResource;
use mcp_types::ResourceLink;
use ratatui::prelude::*;
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::widgets::Block;
use ratatui::widgets::Borders;
use ratatui::widgets::Padding;
use ratatui::widgets::Paragraph;
use ratatui::widgets::WidgetRef;
use ratatui::widgets::Wrap;
use std::collections::HashMap;
use std::io::Cursor;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;
use tracing::error;
use unicode_width::UnicodeWidthChar;

// ==================== Core Types ====================

#[derive(Clone)]
pub(crate) struct CommandOutput {
    pub(crate) exit_code: i32,
    pub(crate) stdout: String,
    pub(crate) stderr: String,
}

#[derive(Clone, Copy)]
pub(crate) enum PatchEventType {
    ApprovalRequest,
    ApplyBegin { auto_approved: bool },
}

// ==================== HistoryCellType ====================

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum HistoryCellType {
    Plain,
    User,
    Assistant,
    Reasoning,
    Error,
    Exec { kind: ExecKind, status: ExecStatus },
    Tool { status: ToolStatus },
    Patch { kind: PatchKind },
    PlanUpdate,
    BackgroundEvent,
    Notice,
    Diff,
    Image,
    AnimatedWelcome,
    Loading,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ExecKind {
    Read,
    Search,
    List,
    Run,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ExecStatus {
    Running,
    Success,
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ToolStatus {
    Running,
    Success,
    Failed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PatchKind {
    Proposed,
    ApplyBegin,
    ApplySuccess,
    ApplyFailure,
}

// ==================== HistoryCell Trait ====================

/// Represents an event to display in the conversation history.
/// Returns its `Vec<Line<'static>>` representation to make it easier
/// to display in a scrollable list.
pub(crate) trait HistoryCell {
    fn display_lines(&self) -> Vec<Line<'static>>;
    /// A required, explicit type descriptor for the history cell.
    fn kind(&self) -> HistoryCellType;

    /// Allow downcasting to concrete types
    fn as_any(&self) -> &dyn std::any::Any {
        // Default implementation that doesn't support downcasting
        // Concrete types that need downcasting should override this
        &() as &dyn std::any::Any
    }
    /// Allow mutable downcasting to concrete types
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any;

    /// Get display lines with empty lines trimmed from beginning and end.
    /// This ensures consistent spacing when cells are rendered together.
    fn display_lines_trimmed(&self) -> Vec<Line<'static>> {
        trim_empty_lines(self.display_lines())
    }

    fn desired_height(&self, width: u16) -> u16 {
        Paragraph::new(Text::from(self.display_lines_trimmed()))
            .wrap(Wrap { trim: false })
            .line_count(width)
            .try_into()
            .unwrap_or(0)
    }

    fn render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        // Check if this cell has custom rendering
        if self.has_custom_render() {
            // Allow custom renders to handle top skipping explicitly
            self.custom_render_with_skip(area, buf, skip_rows);
            return;
        }

        // Default path: render the full text and use Paragraph.scroll to skip
        // vertical rows AFTER wrapping. Slicing lines before wrapping causes
        // incorrect blank space when lines wrap across multiple rows.
        // IMPORTANT: Explicitly clear the entire area first. While some containers
        // clear broader regions, custom widgets that shrink or scroll can otherwise
        // leave residual glyphs to the right of shorter lines or from prior frames.
        // We paint spaces with the current theme background to guarantee a clean slate.
        // Assistant messages use a subtly tinted background: theme background
        // moved 5% toward the theme info color for a gentle distinction.
        let cell_bg = match self.kind() {
            HistoryCellType::Assistant => crate::colors::assistant_bg(),
            _ => crate::colors::background(),
        };
        let bg_style = Style::default().bg(cell_bg).fg(crate::colors::text());
        for y in area.y..area.y.saturating_add(area.height) {
            for x in area.x..area.x.saturating_add(area.width) {
                buf[(x, y)].set_char(' ').set_style(bg_style);
            }
        }

        // Ensure the entire allocated area is painted with the theme background
        // by attaching a background-styled Block to the Paragraph as well.
        let lines = self.display_lines_trimmed();
        let text = Text::from(lines);

        let bg_block = Block::default().style(Style::default().bg(cell_bg));
        Paragraph::new(text)
            .block(bg_block)
            .wrap(Wrap { trim: false })
            .scroll((skip_rows, 0))
            .style(Style::default().bg(cell_bg))
            .render(area, buf);
    }

    /// Returns true if this cell has custom rendering (e.g., animations)
    fn has_custom_render(&self) -> bool {
        false // Default: most cells use display_lines
    }

    /// Custom render implementation for cells that need it
    fn custom_render(&self, _area: Rect, _buf: &mut Buffer) {
        // Default: do nothing (cells with custom rendering will override)
    }
    /// Custom render with support for skipping top rows
    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, _skip_rows: u16) {
        // Default: fall back to non-skipping custom render
        self.custom_render(area, buf);
    }

    /// Returns true if this cell is currently animating and needs redraws
    fn is_animating(&self) -> bool {
        false // Default: most cells don't animate
    }

    /// Returns true if this is a loading cell that should be removed when streaming starts
    #[allow(dead_code)]
    fn is_loading_cell(&self) -> bool {
        false // Default: most cells are not loading cells
    }

    /// Trigger fade-out animation (for AnimatedWelcomeCell)
    fn trigger_fade(&self) {
        // Default: do nothing (only AnimatedWelcomeCell implements this)
    }

    /// Check if this cell should be removed (e.g., fully faded out)
    fn should_remove(&self) -> bool {
        false // Default: most cells should not be removed
    }

    /// Check if this cell is ONLY a title line (no content after it)
    /// Used to avoid spacing between standalone titles and their content
    fn is_title_only(&self) -> bool {
        let lines = self.display_lines_trimmed();
        // Cell is title-only if it has exactly 1 line and that line is a title
        lines.len() == 1 && lines.first().map_or(false, is_title_line)
    }

    /// Returns the gutter symbol for this cell type
    /// Returns None if no symbol should be displayed
    fn gutter_symbol(&self) -> Option<&'static str> {
        match self.kind() {
            HistoryCellType::Plain => None,
            HistoryCellType::User => Some("›"),
            // Restore assistant gutter icon
            HistoryCellType::Assistant => Some("•"),
            HistoryCellType::Reasoning => None,
            HistoryCellType::Error => Some("✖"),
            HistoryCellType::Tool { status } => Some(match status {
                ToolStatus::Running => "⚙",
                ToolStatus::Success => "✔",
                ToolStatus::Failed => "✖",
            }),
            HistoryCellType::Exec { kind, status } => {
                // Show ❯ only for Run executions; hide for read/search/list summaries
                match (kind, status) {
                    (ExecKind::Run, ExecStatus::Error) => Some("✖"),
                    (ExecKind::Run, _) => Some("❯"),
                    _ => None,
                }
            }
            HistoryCellType::Patch { .. } => Some("↯"),
            HistoryCellType::PlanUpdate => Some("◔"), // final glyph will be chosen in header line
            HistoryCellType::BackgroundEvent => Some("»"),
            HistoryCellType::Notice => Some("★"),
            HistoryCellType::Diff => Some("↯"),
            HistoryCellType::Image => None,
            HistoryCellType::AnimatedWelcome => None,
            HistoryCellType::Loading => None,
        }
    }
}

// Allow Box<dyn HistoryCell> to implement HistoryCell
impl HistoryCell for Box<dyn HistoryCell> {
    fn as_any(&self) -> &dyn std::any::Any {
        self.as_ref().as_any()
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self.as_mut().as_any_mut()
    }
    fn kind(&self) -> HistoryCellType {
        self.as_ref().kind()
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        self.as_ref().display_lines()
    }

    fn display_lines_trimmed(&self) -> Vec<Line<'static>> {
        self.as_ref().display_lines_trimmed()
    }

    fn desired_height(&self, width: u16) -> u16 {
        self.as_ref().desired_height(width)
    }

    fn render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        self.as_ref().render_with_skip(area, buf, skip_rows)
    }

    fn has_custom_render(&self) -> bool {
        self.as_ref().has_custom_render()
    }

    fn custom_render(&self, area: Rect, buf: &mut Buffer) {
        self.as_ref().custom_render(area, buf)
    }

    fn is_animating(&self) -> bool {
        self.as_ref().is_animating()
    }

    fn is_loading_cell(&self) -> bool {
        self.as_ref().is_loading_cell()
    }

    fn trigger_fade(&self) {
        self.as_ref().trigger_fade()
    }

    fn should_remove(&self) -> bool {
        self.as_ref().should_remove()
    }

    fn is_title_only(&self) -> bool {
        self.as_ref().is_title_only()
    }

    fn gutter_symbol(&self) -> Option<&'static str> {
        self.as_ref().gutter_symbol()
    }
}

// ==================== PlainHistoryCell ====================
// For simple cells that just store lines

pub(crate) struct PlainHistoryCell {
    pub(crate) lines: Vec<Line<'static>>,
    pub(crate) kind: HistoryCellType,
}

impl HistoryCell for PlainHistoryCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        self.kind
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // If a gutter symbol implies a standard header line (user, assistant, tool, etc.),
        // hide that first title line and show only the content.
        let hide_header = matches!(
            self.kind,
            HistoryCellType::User
                | HistoryCellType::Assistant
                | HistoryCellType::Tool { .. }
                | HistoryCellType::Error
                | HistoryCellType::BackgroundEvent
                | HistoryCellType::Notice
        );
        if hide_header && self.lines.len() > 1 {
            self.lines[1..].to_vec()
        } else if hide_header {
            Vec::new()
        } else {
            self.lines.clone()
        }
    }

    fn has_custom_render(&self) -> bool {
        matches!(self.kind, HistoryCellType::User)
    }

    fn desired_height(&self, width: u16) -> u16 {
        if matches!(self.kind, HistoryCellType::User) {
            // Match input composer wrapping by reserving 2 columns of right padding.
            // Composer content width is pane−6; history content is pane−4 (after gutter).
            // Subtract 2 more so wrapping positions are identical when the message moves
            // from the composer into history.
            let inner_w = width.saturating_sub(2);
            let text = Text::from(self.display_lines_trimmed());
            Paragraph::new(text)
                .wrap(Wrap { trim: false })
                .line_count(inner_w)
                .try_into()
                .unwrap_or(0)
        } else {
            Paragraph::new(Text::from(self.display_lines_trimmed()))
                .wrap(Wrap { trim: false })
                .line_count(width)
                .try_into()
                .unwrap_or(0)
        }
    }

    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        if !matches!(self.kind, HistoryCellType::User) {
            // Fallback to default behavior for non-user cells
            return HistoryCell::custom_render_with_skip(self, area, buf, skip_rows);
        }

        // Render User cells with extra right padding to mirror the composer input padding.
        let cell_bg = crate::colors::background();
        let bg_style = Style::default().bg(cell_bg).fg(crate::colors::text());

        // Clear area
        for y in area.y..area.y.saturating_add(area.height) {
            for x in area.x..area.x.saturating_add(area.width) {
                buf[(x, y)].set_char(' ').set_style(bg_style);
            }
        }

        let lines = self.display_lines_trimmed();
        let text = Text::from(lines);

        // Add Block with padding: reserve 2 columns on the right.
        let block = Block::default().style(bg_style).padding(Padding {
            left: 0,
            right: 2,
            top: 0,
            bottom: 0,
        });

        Paragraph::new(text)
            .block(block)
            .wrap(Wrap { trim: false })
            .scroll((skip_rows, 0))
            .style(bg_style)
            .render(area, buf);
    }
}

// ==================== ExecCell ====================

pub(crate) struct ExecCell {
    pub(crate) command: Vec<String>,
    pub(crate) parsed: Vec<ParsedCommand>,
    pub(crate) output: Option<CommandOutput>,
    pub(crate) start_time: Option<Instant>,
    // Caches to avoid recomputing expensive line construction for completed execs
    cached_display_lines: std::cell::RefCell<Option<Vec<Line<'static>>>>,
    cached_pre_lines: std::cell::RefCell<Option<Vec<Line<'static>>>>,
    cached_out_lines: std::cell::RefCell<Option<Vec<Line<'static>>>>,
}

// ==================== AssistantMarkdownCell ====================
// Stores raw assistant markdown and rebuilds on demand (e.g., theme/syntax changes)

pub(crate) struct AssistantMarkdownCell {
    // Raw markdown used to rebuild when theme/syntax changes
    pub(crate) raw: String,
    // Optional stream/item id that produced this finalized cell
    pub(crate) id: Option<String>,
    // Pre-rendered lines (first line is a hidden "codex" header)
    pub(crate) lines: Vec<Line<'static>>, // includes hidden header "codex"
}

impl AssistantMarkdownCell {
    #[allow(dead_code)]
    pub(crate) fn new(raw: String, cfg: &codex_core::config::Config) -> Self {
        Self::new_with_id(raw, None, cfg)
    }

    pub(crate) fn new_with_id(
        raw: String,
        id: Option<String>,
        cfg: &codex_core::config::Config,
    ) -> Self {
        let mut me = Self {
            raw,
            id,
            lines: Vec::new(),
        };
        me.rebuild(cfg);
        me
    }
    pub(crate) fn rebuild(&mut self, cfg: &codex_core::config::Config) {
        let mut out: Vec<Line<'static>> = Vec::new();
        out.push(Line::from("codex"));
        crate::markdown::append_markdown_with_bold_first(&self.raw, &mut out, cfg);
        // Apply bright text to body like streaming finalize
        let bright = crate::colors::text_bright();
        for line in out.iter_mut().skip(1) {
            line.style = line.style.patch(Style::default().fg(bright));
        }
        self.lines = out;
    }
}

impl HistoryCell for AssistantMarkdownCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Assistant
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // Hide the header line, mirroring PlainHistoryCell behavior for Assistant
        if self.lines.len() > 1 {
            self.lines[1..].to_vec()
        } else {
            Vec::new()
        }
    }
    fn has_custom_render(&self) -> bool {
        true
    }
    fn desired_height(&self, width: u16) -> u16 {
        // Match custom rendering exactly:
        // - Bullet lines are prewrapped with hanging indent and treated as fixed rows
        // - Code blocks render inside a bordered card (add 2 rows for borders)
        // - Other text wraps via Paragraph at the available width
        // - Add top and bottom padding rows
        let text_wrap_width = width;
        let src_lines = self.display_lines_trimmed();

        #[derive(Debug)]
        enum Seg {
            Text(Vec<Line<'static>>),
            Bullet(Vec<Line<'static>>),
            Code(Vec<Line<'static>>),
        }

        let mut segs: Vec<Seg> = Vec::new();
        let mut text_buf: Vec<Line<'static>> = Vec::new();
        let mut _is_first_output_line = true;
        let mut iter = src_lines.into_iter().peekable();
        while let Some(line) = iter.next() {
            if crate::render::line_utils::is_code_block_painted(&line) {
                if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                let mut chunk = vec![line];
                while let Some(n) = iter.peek() {
                    if crate::render::line_utils::is_code_block_painted(n) { chunk.push(iter.next().unwrap()); } else { break; }
                }
                segs.push(Seg::Code(chunk));
                continue;
            }

            if text_wrap_width > 4 && is_horizontal_rule_line(&line) {
                if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                let hr = Line::from(Span::styled(
                    std::iter::repeat('─').take(text_wrap_width as usize).collect::<String>(),
                    Style::default().fg(crate::colors::assistant_hr()),
                ));
                segs.push(Seg::Bullet(vec![hr]));
                _is_first_output_line = false;
                continue;
            }

            if text_wrap_width > 4 {
                if let Some((indent_spaces, bullet_char)) = detect_bullet_prefix(&line) {
                    if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                    // Always use explicit bullet wrapping with hanging indent so
                    // continuation lines align under the start of the content.
                    segs.push(Seg::Bullet(wrap_bullet_line(
                        line,
                        indent_spaces,
                        &bullet_char,
                        text_wrap_width,
                    )));
                    _is_first_output_line = false;
                    continue;
                }
            }

            text_buf.push(line);
            _is_first_output_line = false;
        }
        if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }

        let mut total: u16 = 0;
        for seg in segs {
            match seg {
                Seg::Bullet(lines) => {
                    total = total.saturating_add(lines.len() as u16);
                }
                Seg::Text(lines) => {
                    if lines.is_empty() { continue; }
                    let text = Text::from(lines);
                    let rows: u16 = Paragraph::new(text)
                        .wrap(Wrap { trim: false })
                        .line_count(text_wrap_width)
                        .try_into()
                        .unwrap_or(0);
                    total = total.saturating_add(rows);
                }
                Seg::Code(mut chunk) => {
                    // Remove language sentinel and trim blank padding rows
                    if let Some(first) = chunk.first() {
                        let flat: String = first.spans.iter().map(|s| s.content.as_ref()).collect();
                        if flat.contains("⟦LANG:") { chunk.remove(0); }
                    }
                    while chunk.first().is_some_and(|l| crate::render::line_utils::is_blank_line_spaces_only(l)) { chunk.remove(0); }
                    while chunk.last().is_some_and(|l| crate::render::line_utils::is_blank_line_spaces_only(l)) { chunk.pop(); }
                    total = total.saturating_add(chunk.len() as u16 + 2);
                }
            }
        }

        total.saturating_add(2)
    }
    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        // Mirror StreamingContentCell rendering so finalized assistant cells look
        // identical to streaming ones (gutter alignment, padding, bg tint).
        let cell_bg = crate::colors::assistant_bg();
        let bg_style = Style::default().bg(cell_bg);

        // Clear full area with assistant background
        for y in area.y..area.y.saturating_add(area.height) {
            for x in area.x..area.x.saturating_add(area.width) {
                buf[(x, y)].set_char(' ').set_style(bg_style);
            }
        }

        // Build segments: prewrapped bullets, code (no rewrap), and normal text.
        let text_wrap_width = area.width;
        #[derive(Debug)]
        enum Seg {
            Text(Vec<Line<'static>>),
            Bullet(Vec<Line<'static>>),
            Code(Vec<Line<'static>>),
        }
        let mut segs: Vec<Seg> = Vec::new();
        let mut text_buf: Vec<Line<'static>> = Vec::new();
        let mut _is_first_output_line = true;
        let mut iter = self.display_lines_trimmed().into_iter().peekable();
        while let Some(line) = iter.next() {
            if crate::render::line_utils::is_code_block_painted(&line) {
                if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                let mut chunk = vec![line];
                while let Some(n) = iter.peek() {
                    if crate::render::line_utils::is_code_block_painted(n) { chunk.push(iter.next().unwrap()); } else { break; }
                }
                // Trim padding rows rendered inside the code card background
                while chunk.first().is_some_and(|l| crate::render::line_utils::is_blank_line_spaces_only(l)) { chunk.remove(0); }
                while chunk.last().is_some_and(|l| crate::render::line_utils::is_blank_line_spaces_only(l)) { chunk.pop(); }
                segs.push(Seg::Code(chunk));
                continue;
            }
            if text_wrap_width > 4 && is_horizontal_rule_line(&line) {
                if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                let hr = Line::from(Span::styled(
                    std::iter::repeat('─').take(text_wrap_width as usize).collect::<String>(),
                    Style::default().fg(crate::colors::assistant_hr()),
                ));
                segs.push(Seg::Bullet(vec![hr]));
                _is_first_output_line = false;
                continue;
            }
            if text_wrap_width > 4 {
                if let Some((indent_spaces, bullet_char)) = detect_bullet_prefix(&line) {
                    if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                    segs.push(Seg::Bullet(wrap_bullet_line(
                        line,
                        indent_spaces,
                        &bullet_char,
                        text_wrap_width,
                    )));
                    _is_first_output_line = false;
                    continue;
                }
            }
            text_buf.push(line);
            _is_first_output_line = false;
        }
        if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }

        // Streaming-style top padding row for the entire assistant cell
        let mut remaining_skip = skip_rows;
        let mut cur_y = area.y;
        let end_y = area.y.saturating_add(area.height);
        if remaining_skip == 0 && cur_y < end_y {
            cur_y = cur_y.saturating_add(1);
        }
        remaining_skip = remaining_skip.saturating_sub(1);

        // Helpers
        use unicode_width::UnicodeWidthStr as UW;
        let measure_line =
            |l: &Line<'_>| -> usize { l.spans.iter().map(|s| UW::width(s.content.as_ref())).sum() };
        let mut draw_segment = |seg: &Seg, y: &mut u16, skip: &mut u16| {
            if *y >= end_y {
                return;
            }
            match seg {
                Seg::Text(lines) => {
                    // Measure height with wrap
                    let txt = Text::from(lines.clone());
                    let total: u16 = Paragraph::new(txt.clone())
                        .wrap(Wrap { trim: false })
                        .line_count(text_wrap_width)
                        .try_into()
                        .unwrap_or(0);
                    if *skip >= total {
                        *skip -= total;
                        return;
                    }
                    // Visible height in remaining space
                    let avail = end_y.saturating_sub(*y);
                    let draw_h = (total.saturating_sub(*skip)).min(avail);
                    if draw_h == 0 {
                        return;
                    }
                    let rect = Rect {
                        x: area.x,
                        y: *y,
                        width: area.width,
                        height: draw_h,
                    };
                    Paragraph::new(txt)
                        .block(Block::default().style(bg_style))
                        .wrap(Wrap { trim: false })
                        .scroll((*skip, 0))
                        .style(bg_style)
                        .render(rect, buf);
                    *y = y.saturating_add(draw_h);
                    *skip = 0;
                }
                Seg::Bullet(lines) => {
                    let total = lines.len() as u16;
                    if *skip >= total { *skip -= total; return; }
                    let avail = end_y.saturating_sub(*y);
                    let draw_h = (total.saturating_sub(*skip)).min(avail);
                    if draw_h == 0 { return; }
                    let rect = Rect { x: area.x, y: *y, width: area.width, height: draw_h };
                    let txt = Text::from(lines.clone());
                    Paragraph::new(txt)
                        .block(Block::default().style(bg_style))
                        .scroll((*skip, 0))
                        .style(bg_style)
                        .render(rect, buf);
                    *y = y.saturating_add(draw_h);
                    *skip = 0;
                }
                Seg::Code(lines_in) => {
                    if lines_in.is_empty() {
                        return;
                    }
                    // Extract language sentinel and drop it from visible lines
                    let mut lang_label: Option<String> = None;
                    let mut lines = lines_in.clone();
                    if let Some(first) = lines.first() {
                        let flat: String = first.spans.iter().map(|s| s.content.as_ref()).collect();
                        if let Some(s) = flat.strip_prefix("⟦LANG:") {
                            if let Some(end) = s.find('⟧') {
                                lang_label = Some(s[..end].to_string());
                                lines.remove(0);
                            }
                        }
                    }
                    if lines.is_empty() {
                        return;
                    }
                    // Determine target width for the code card (content width) and add borders (2) + inner pads (left/right = 2 each)
                    let max_w = lines.iter().map(|l| measure_line(l)).max().unwrap_or(0) as u16;
                    let inner_w = max_w.max(1);
                    // Borders (2) + inner horizontal padding (2 left, 2 right) => +6
                    let card_w = inner_w.saturating_add(6).min(area.width.max(6));
                    let total = lines.len() as u16 + 2; // top/bottom border only
                    if *skip >= total {
                        *skip -= total;
                        return;
                    }
                    let avail = end_y.saturating_sub(*y);
                    if avail == 0 {
                        return;
                    }
                    // Compute visible slice (accounting for top/bottom border + inner padding rows)
                    let mut local_skip = *skip;
                    let mut top_border = 1u16;
                    if local_skip > 0 {
                        let drop = local_skip.min(top_border);
                        top_border -= drop;
                        local_skip -= drop;
                    }
                    let code_skip = local_skip.min(lines.len() as u16);
                    local_skip -= code_skip;
                    let mut bottom_border = 1u16;
                    if local_skip > 0 {
                        let drop = local_skip.min(bottom_border);
                        bottom_border -= drop;
                    }
                    // Compute drawable height in this pass
                    let visible = top_border
                        + (lines.len() as u16 - code_skip)
                        + bottom_border;
                    let draw_h = visible.min(avail);
                    if draw_h == 0 {
                        return;
                    }
                    // No outer horizontal padding; align card to content area.
                    let content_x = area.x;
                    let _content_w = area.width;
                    let rect_x = content_x;
                    // Draw bordered block for visible rows
                    let rect = Rect {
                        x: rect_x,
                        y: *y,
                        width: card_w,
                        height: draw_h,
                    };
                    let code_bg = crate::colors::code_block_bg();
                    let mut blk = Block::default()
                        .borders(Borders::ALL)
                        .border_style(Style::default().fg(crate::colors::border()))
                        .style(Style::default().bg(code_bg))
                        .padding(Padding { left: 2, right: 2, top: 0, bottom: 0 });
                    if let Some(lang) = &lang_label {
                        blk = blk.title(Span::styled(
                            format!(" {} ", lang),
                            Style::default().fg(crate::colors::text_dim()),
                        ));
                    }
                    // Clone before render so we can compute inner rect after drawing borders
                    let blk_for_inner = blk.clone();
                    blk.render(rect, buf);
                    // Inner paragraph area (exclude borders)
                    let inner_rect = blk_for_inner.inner(rect);
                    let inner_h = inner_rect.height.min(rect.height);
                    if inner_h > 0 {
                        let slice_start = code_skip as usize;
                        let slice_end = lines.len();
                        let txt = Text::from(lines[slice_start..slice_end].to_vec());
                        Paragraph::new(txt)
                            .style(Style::default().bg(code_bg))
                            .block(Block::default().style(Style::default().bg(code_bg)))
                            .render(inner_rect, buf);
                    }
                    // No outside padding stripes.
                    *y = y.saturating_add(draw_h);
                    *skip = 0;
                }
            }
        };

        for seg in &segs {
            if cur_y >= end_y {
                break;
            }
            draw_segment(seg, &mut cur_y, &mut remaining_skip);
        }
        // Bottom padding row (blank): area is already cleared to bg
        if remaining_skip == 0 && cur_y < end_y {
            cur_y = cur_y.saturating_add(1);
        } else {
            remaining_skip = remaining_skip.saturating_sub(1);
        }
        // Mark as used to satisfy unused_assignments lint
        let _ = (cur_y, remaining_skip);
    }
}

impl HistoryCell for ExecCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        let kind = match action_from_parsed(&self.parsed) {
            "read" => ExecKind::Read,
            "search" => ExecKind::Search,
            "list" => ExecKind::List,
            _ => ExecKind::Run,
        };
        let status = match &self.output {
            None => ExecStatus::Running,
            Some(o) if o.exit_code == 0 => ExecStatus::Success,
            Some(_) => ExecStatus::Error,
        };
        HistoryCellType::Exec { kind, status }
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // Fallback textual representation (used for height measurement only when custom rendering).
        // For completed executions, cache the computed lines since they are immutable.
        if let Some(cached) = self.cached_display_lines.borrow().as_ref() {
            return cached.clone();
        }
        let lines = exec_command_lines(
            &self.command,
            &self.parsed,
            self.output.as_ref(),
            self.start_time,
        );
        if self.output.is_some() {
            *self.cached_display_lines.borrow_mut() = Some(lines.clone());
        }
        lines
    }
    fn has_custom_render(&self) -> bool {
        true
    }
    fn desired_height(&self, width: u16) -> u16 {
        // Measure exactly like custom_render_with_skip: preamble at full width,
        // output inside a left-bordered block with left padding (width - 2).
        let (pre_lines, out_lines) = self.exec_render_parts();
        let pre_text = Text::from(trim_empty_lines(pre_lines));
        let out_text = Text::from(trim_empty_lines(out_lines));
        let pre_wrap_width = width;
        let out_wrap_width = width.saturating_sub(2);
        let pre_total: u16 = Paragraph::new(pre_text)
            .wrap(Wrap { trim: false })
            .line_count(pre_wrap_width)
            .try_into()
            .unwrap_or(0);
        let out_total: u16 = Paragraph::new(out_text)
            .wrap(Wrap { trim: false })
            .line_count(out_wrap_width)
            .try_into()
            .unwrap_or(0);
        pre_total.saturating_add(out_total)
    }
    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        // Render command header/content above and stdout/stderr preview inside a left-bordered block.
        let (pre_lines, out_lines) = self.exec_render_parts();

        // Prepare texts and total heights (after wrapping). Keep the visual prefix
        // (e.g., "└ ") in the preamble to show the connector on the first line.
        let pre_text = Text::from(trim_empty_lines(pre_lines));
        let out_text = Text::from(trim_empty_lines(out_lines));
        // Measure with the same widths we will render with.
        let pre_wrap_width = area.width;
        // Output renders inside a Block with a LEFT border (1 col) and left padding of 1,
        // so the inner text width is reduced accordingly.
        let out_wrap_width = area.width.saturating_sub(2);
        let pre_total: u16 = Paragraph::new(pre_text.clone())
            .wrap(Wrap { trim: false })
            .line_count(pre_wrap_width)
            .try_into()
            .unwrap_or(0);
        let out_total: u16 = Paragraph::new(out_text.clone())
            .wrap(Wrap { trim: false })
            .line_count(out_wrap_width)
            .try_into()
            .unwrap_or(0);

        // Compute how many rows to skip from the preamble, then from the output
        let pre_skip = skip_rows.min(pre_total);
        let out_skip = skip_rows.saturating_sub(pre_total).min(out_total);

        // Compute how much height is available for pre and out segments in this area
        let pre_remaining = pre_total.saturating_sub(pre_skip);
        let pre_height = pre_remaining.min(area.height);
        let out_available = area.height.saturating_sub(pre_height);
        let out_remaining = out_total.saturating_sub(out_skip);
        let out_height = out_available.min(out_remaining);

        // Render preamble (scrolled) if any space. Do not strip or offset the
        // leading "└ ": render at the left edge so the angle is visible.
        if pre_height > 0 {
            let pre_area = Rect {
                x: area.x,
                y: area.y,
                width: area.width,
                height: pre_height,
            };
            // Hard clear: fill pre_area with spaces using theme background. This prevents
            // artifacts when the preamble shrinks or when scrolling reveals previously
            // longer content.
            let bg_style = Style::default()
                .bg(crate::colors::background())
                .fg(crate::colors::text());
            for y in pre_area.y..pre_area.y.saturating_add(pre_area.height) {
                for x in pre_area.x..pre_area.x.saturating_add(pre_area.width) {
                    buf[(x, y)].set_char(' ').set_style(bg_style);
                }
            }
            let pre_block =
                Block::default().style(Style::default().bg(crate::colors::background()));
            Paragraph::new(pre_text)
                .block(pre_block)
                .wrap(Wrap { trim: false })
                .scroll((pre_skip, 0))
                .style(Style::default().bg(crate::colors::background()))
                .render(pre_area, buf);
        }

        // Render output (scrolled) with a left border block if any space
        if out_height > 0 {
            let out_area = Rect {
                x: area.x,
                y: area.y.saturating_add(pre_height),
                width: area.width,
                height: out_height,
            };
            // Hard clear: fill out_area with spaces before drawing the bordered paragraph.
            let bg_style = Style::default()
                .bg(crate::colors::background())
                .fg(crate::colors::text_dim());
            for y in out_area.y..out_area.y.saturating_add(out_area.height) {
                for x in out_area.x..out_area.x.saturating_add(out_area.width) {
                    buf[(x, y)].set_char(' ').set_style(bg_style);
                }
            }
            let block = Block::default()
                .borders(Borders::LEFT)
                .border_style(
                    Style::default()
                        .fg(crate::colors::border_dim())
                        .bg(crate::colors::background()),
                )
                .style(Style::default().bg(crate::colors::background()))
                .padding(Padding {
                    left: 1,
                    right: 0,
                    top: 0,
                    bottom: 0,
                });
            Paragraph::new(out_text)
                .block(block)
                .wrap(Wrap { trim: false })
                // Scroll count is based on the wrapped text rows at out_wrap_width
                .scroll((out_skip, 0))
                .style(
                    Style::default()
                        .bg(crate::colors::background())
                        .fg(crate::colors::text_dim()),
                )
                .render(out_area, buf);
        }
    }
}

impl ExecCell {
    // Build separate segments: (preamble lines, output lines)
    fn exec_render_parts(&self) -> (Vec<Line<'static>>, Vec<Line<'static>>) {
        // For completed executions, cache pre/output segments since they are immutable.
        if let (true, Some(pre), Some(out)) = (
            self.output.is_some(),
            self.cached_pre_lines.borrow().as_ref(),
            self.cached_out_lines.borrow().as_ref(),
        ) {
            return (pre.clone(), out.clone());
        }

        let parts = if self.parsed.is_empty() {
            exec_render_parts_generic(&self.command, self.output.as_ref(), self.start_time)
        } else {
            exec_render_parts_parsed(&self.parsed, self.output.as_ref(), self.start_time)
        };

        if self.output.is_some() {
            let (pre, out) = parts.clone();
            *self.cached_pre_lines.borrow_mut() = Some(pre);
            *self.cached_out_lines.borrow_mut() = Some(out);
        }
        parts
    }
}

// ==================== DiffCell ====================

pub(crate) struct DiffCell {
    pub(crate) lines: Vec<Line<'static>>,
}

impl HistoryCell for DiffCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Diff
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        self.lines.clone()
    }
    fn has_custom_render(&self) -> bool {
        true
    }
    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, mut skip_rows: u16) {
        // Render a two-column diff with a 1-col marker gutter and 1-col padding
        // so wrapped lines hang under their first content column.
        // Hard clear the entire area: write spaces + background so any
        // previously longer content does not bleed into shorter frames.
        let bg = Style::default().bg(crate::colors::background());
        for y in area.y..area.y.saturating_add(area.height) {
            for x in area.x..area.x.saturating_add(area.width) {
                buf[(x, y)].set_char(' ').set_style(bg);
            }
        }

        // Center the sign in the two-column gutter by leaving one leading
        // space and drawing the sign in the second column.
        let marker_col_x = area.x.saturating_add(2); // two spaces then '+'/'-'
        let content_x = area.x.saturating_add(4); // two spaces before sign + one after sign
        let content_w = area.width.saturating_sub(4);
        let mut cur_y = area.y;

        // Helper to classify a line and extract marker and content
        let classify = |l: &Line<'_>| -> (Option<char>, Line<'static>, Style) {
            let text: String = l
                .spans
                .iter()
                .map(|s| s.content.as_ref())
                .collect::<String>();
            let default_style = Style::default().fg(crate::colors::text());
            if text.starts_with("+") && !text.starts_with("+++") {
                let content = text.chars().skip(1).collect::<String>();
                (
                    Some('+'),
                    Line::from(content).style(Style::default().fg(crate::colors::success())),
                    default_style,
                )
            } else if text.starts_with("-") && !text.starts_with("---") {
                let content = text.chars().skip(1).collect::<String>();
                (
                    Some('-'),
                    Line::from(content).style(Style::default().fg(crate::colors::error())),
                    default_style,
                )
            } else if text.starts_with("@@") {
                (
                    None,
                    Line::from(text).style(Style::default().fg(crate::colors::primary())),
                    default_style,
                )
            } else {
                (None, Line::from(text), default_style)
            }
        };

        'outer: for line in &self.lines {
            // Measure this line at wrapped width
            let (_marker, content_line, _sty) = classify(line);
            let content_text = Text::from(vec![content_line.clone()]);
            let rows: u16 = Paragraph::new(content_text.clone())
                .wrap(Wrap { trim: false })
                .line_count(content_w)
                .try_into()
                .unwrap_or(0);

            let mut local_skip = 0u16;
            if skip_rows > 0 {
                if skip_rows >= rows {
                    skip_rows -= rows;
                    continue 'outer;
                } else {
                    local_skip = skip_rows;
                    skip_rows = 0;
                }
            }

            // Remaining height available
            if cur_y >= area.y.saturating_add(area.height) {
                break;
            }
            let avail = area.y.saturating_add(area.height) - cur_y;
            let draw_h = rows.saturating_sub(local_skip).min(avail);
            if draw_h == 0 {
                continue;
            }

            // Draw content with hanging indent (left margin = 2)
            let content_area = Rect {
                x: content_x,
                y: cur_y,
                width: content_w,
                height: draw_h,
            };
            let block = Block::default().style(bg);
            Paragraph::new(content_text)
                .block(block)
                .wrap(Wrap { trim: false })
                .scroll((local_skip, 0))
                .style(bg)
                .render(content_area, buf);

            // Draw marker on the first visible visual row of this logical line
            let (marker, _content_line2, _) = classify(line);
            if let Some(m) = marker {
                if local_skip == 0 && area.width >= 1 {
                    let color = if m == '+' {
                        crate::colors::success()
                    } else {
                        crate::colors::error()
                    };
                    let style = Style::default().fg(color).bg(crate::colors::background());
                    buf.set_string(marker_col_x, cur_y, m.to_string(), style);
                }
            }

            cur_y = cur_y.saturating_add(draw_h);
            if cur_y >= area.y.saturating_add(area.height) {
                break;
            }
        }
    }
}

// ==================== MergedExecCell ====================
// Represents multiple completed exec results merged into one cell while preserving
// the bordered, dimmed output styling for each command's stdout/stderr preview.

pub(crate) struct MergedExecCell {
    // Sequence of (preamble lines, output lines) for each completed exec
    segments: Vec<(Vec<Line<'static>>, Vec<Line<'static>>)>,
    // Choose icon/behavior based on predominant action kind for gutter
    kind: ExecKind,
}

impl MergedExecCell {
    pub(crate) fn exec_kind(&self) -> ExecKind {
        self.kind
    }
    pub(crate) fn from_exec(exec: &ExecCell) -> Self {
        let (pre, out) = exec.exec_render_parts();
        let kind = match action_from_parsed(&exec.parsed) {
            "read" => ExecKind::Read,
            "search" => ExecKind::Search,
            "list" => ExecKind::List,
            _ => ExecKind::Run,
        };
        Self {
            segments: vec![(pre, out)],
            kind,
        }
    }
    pub(crate) fn push_exec(&mut self, exec: &ExecCell) {
        let (pre, out) = exec.exec_render_parts();
        self.segments.push((pre, out));
    }

    // Build an aggregated preamble for Read segments by concatenating
    // all per-exec preambles and coalescing contiguous ranges for the
    // same file. Returns None for non-Read kinds.
    fn aggregated_read_preamble_lines(&self) -> Option<Vec<Line<'static>>> {
        if self.kind != ExecKind::Read {
            return None;
        }
        use ratatui::text::Span;
        // Concatenate per-segment preambles (without their headers), but KEEP ONLY
        // read-like entries. Then normalize the connector so only the very first
        // visible line uses a corner marker and subsequent lines use two spaces.
        // Finally, coalesce contiguous ranges for the same file.

        // Local helper: parse a read range line of the form
        // "└ <file> (lines A to B)" or "  <file> (lines A to B)".
        fn parse_read_line(line: &Line<'_>) -> Option<(String, u32, u32)> {
            if line.spans.is_empty() {
                return None;
            }
            let first = line.spans[0].content.as_ref();
            if !(first == "└ " || first == "  ") {
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
                    let inner = &tail[7..tail.len().saturating_sub(1)];
                    if let Some((a, b)) = inner.split_once(" to ") {
                        if let (Ok(s), Ok(e)) = (a.trim().parse::<u32>(), b.trim().parse::<u32>()) {
                            return Some((fname, s, e));
                        }
                    }
                }
            }
            None
        }

        // Heuristic: identify search-like lines (e.g., "… in dir/" or " (in dir)") so
        // they can be dropped from a Read aggregation if they slipped in.
        fn is_search_like(line: &Line<'_>) -> bool {
            let text: String = line.spans.iter().map(|s| s.content.as_ref()).collect();
            let t = text.trim();
            t.contains(" (in ")
                || t.rsplit_once(" in ")
                    .map(|(_, rhs)| rhs.trim_end().ends_with('/'))
                    .unwrap_or(false)
        }

        let mut kept: Vec<Line<'static>> = Vec::new();
        for (seg_idx, (pre_raw, _)) in self.segments.iter().enumerate() {
            let mut pre = trim_empty_lines(pre_raw.clone());
            if !pre.is_empty() {
                pre.remove(0);
            } // drop per-exec header
            // Filter: keep definite read-range lines; drop obvious search-like lines.
            for l in pre.into_iter() {
                if is_search_like(&l) {
                    continue;
                }
                // Prefer lines that parse as read ranges; otherwise allow if they are not search-like.
                let keep = parse_read_line(&l).is_some() || seg_idx == 0; // be permissive for first segment
                if !keep {
                    continue;
                }
                kept.push(l);
            }
        }

        if kept.is_empty() {
            return Some(kept);
        }

        // Normalize connector: first visible line uses "└ ", later lines use "  ".
        if let Some(first) = kept.first_mut() {
            let flat: String = first.spans.iter().map(|s| s.content.as_ref()).collect();
            let has_connector = flat.trim_start().starts_with("└ ");
            if !has_connector {
                first.spans.insert(
                    0,
                    Span::styled("└ ", Style::default().fg(crate::colors::text_dim())),
                );
            }
        }
        for l in kept.iter_mut().skip(1) {
            if let Some(sp0) = l.spans.get_mut(0) {
                if sp0.content.as_ref() == "└ " {
                    sp0.content = "  ".into();
                    // Keep dim styling for alignment consistency
                    sp0.style = sp0.style.add_modifier(Modifier::DIM);
                }
            }
        }

        // Merge adjacent/overlapping ranges in-place
        coalesce_read_ranges_in_lines_local(&mut kept);
        Some(kept)
    }
}

impl HistoryCell for MergedExecCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Exec {
            kind: self.kind,
            status: ExecStatus::Success,
        }
    }
    fn desired_height(&self, width: u16) -> u16 {
        // Match custom_render_with_skip exactly:
        // - Single shared header row (1)
        // - For each segment:
        //   - Measure preamble after dropping the per-segment header
        //     and normalizing the leading "└ " prefix at full `width`.
        //   - Measure output inside a left-bordered block with left padding,
        //     which reduces the effective wrapping width by 2 columns.
        let mut total: u16 = 1; // shared header (e.g., "Ran", "Read")
        let pre_wrap_width = width;
        let out_wrap_width = width.saturating_sub(2);

        if let Some(agg_pre) = self.aggregated_read_preamble_lines() {
            let pre_rows: u16 = Paragraph::new(Text::from(agg_pre))
                .wrap(Wrap { trim: false })
                .line_count(pre_wrap_width)
                .try_into()
                .unwrap_or(0);
            total = total.saturating_add(pre_rows);
            for (_pre_raw, out_raw) in &self.segments {
                let out = trim_empty_lines(out_raw.clone());
                let out_rows: u16 = Paragraph::new(Text::from(out))
                    .wrap(Wrap { trim: false })
                    .line_count(out_wrap_width)
                    .try_into()
                    .unwrap_or(0);
                total = total.saturating_add(out_rows);
            }
            return total;
        }

        let mut added_corner = false;
        for (pre_raw, out_raw) in &self.segments {
            // Build preamble like the renderer: trim, drop first header line, ensure prefix
            let mut pre = trim_empty_lines(pre_raw.clone());
            if !pre.is_empty() {
                pre.remove(0);
            }
            if let Some(first) = pre.first_mut() {
                let flat: String = first.spans.iter().map(|s| s.content.as_ref()).collect();
                let has_corner = flat.trim_start().starts_with("└ ");
                let has_spaced_corner = flat.trim_start().starts_with("  └ ");
                if !added_corner {
                    if !(has_corner || has_spaced_corner) {
                        first.spans.insert(
                            0,
                            Span::styled("└ ", Style::default().fg(crate::colors::text_dim())),
                        );
                    }
                    added_corner = true;
                } else {
                    // For subsequent segments, ensure no leading corner; use two spaces instead.
                    if let Some(sp0) = first.spans.get_mut(0) {
                        if sp0.content.as_ref() == "└ " {
                            sp0.content = "  ".into();
                            sp0.style = sp0.style.add_modifier(Modifier::DIM);
                        }
                    }
                }
            }
            let out = trim_empty_lines(out_raw.clone());
            let pre_rows: u16 = Paragraph::new(Text::from(pre))
                .wrap(Wrap { trim: false })
                .line_count(pre_wrap_width)
                .try_into()
                .unwrap_or(0);
            let out_rows: u16 = Paragraph::new(Text::from(out))
                .wrap(Wrap { trim: false })
                .line_count(out_wrap_width)
                .try_into()
                .unwrap_or(0);
            total = total.saturating_add(pre_rows).saturating_add(out_rows);
        }

        total
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // Fallback textual form: concatenate all preambles + outputs with blank separators.
        let mut out: Vec<Line<'static>> = Vec::new();
        for (i, (pre, body)) in self.segments.iter().enumerate() {
            if i > 0 {
                out.push(Line::from(""));
            }
            out.extend(trim_empty_lines(pre.clone()));
            out.extend(trim_empty_lines(body.clone()));
        }
        out
    }
    fn has_custom_render(&self) -> bool {
        true
    }
    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, mut skip_rows: u16) {
        // Single shared header (e.g., "Ran") then each segment's command + output.
        let bg = Style::default()
            .bg(crate::colors::background())
            .fg(crate::colors::text());
        // Hard clear area first
        for y in area.y..area.y.saturating_add(area.height) {
            for x in area.x..area.x.saturating_add(area.width) {
                buf[(x, y)].set_char(' ').set_style(bg);
            }
        }

        // Build one header line based on exec kind
        let header_line = match self.kind {
            ExecKind::Read => Line::styled("Read", Style::default().fg(crate::colors::text())),
            ExecKind::Search => {
                Line::styled("Searched", Style::default().fg(crate::colors::text()))
            }
            ExecKind::List => {
                Line::styled("List Files", Style::default().fg(crate::colors::text()))
            }
            ExecKind::Run => Line::styled(
                "Ran",
                Style::default()
                    .fg(crate::colors::text_bright())
                    .add_modifier(Modifier::BOLD),
            ),
        };

        let mut cur_y = area.y;
        let end_y = area.y.saturating_add(area.height);

        // Render or skip header line
        if skip_rows == 0 {
            if cur_y < end_y {
                let txt = Text::from(vec![header_line.clone()]);
                Paragraph::new(txt)
                    .block(Block::default().style(bg))
                    .wrap(Wrap { trim: false })
                    .render(
                        Rect {
                            x: area.x,
                            y: cur_y,
                            width: area.width,
                            height: 1,
                        },
                        buf,
                    );
                cur_y = cur_y.saturating_add(1);
            }
        } else {
            skip_rows = skip_rows.saturating_sub(1);
        }

        // Helper: ensure only the very first preamble line across all segments gets the corner
        let mut added_corner: bool = false;
        let mut ensure_prefix = |lines: &mut Vec<Line<'static>>| {
            if let Some(first) = lines.first_mut() {
                let flat: String = first.spans.iter().map(|s| s.content.as_ref()).collect();
                let has_corner = flat.trim_start().starts_with("└ ");
                let has_spaced_corner = flat.trim_start().starts_with("  └ ");
                if !added_corner {
                    if !(has_corner || has_spaced_corner) {
                        first.spans.insert(
                            0,
                            Span::styled("└ ", Style::default().fg(crate::colors::text_dim())),
                        );
                    }
                    added_corner = true;
                } else {
                    // For subsequent segments, replace any leading corner with two spaces
                    if let Some(sp0) = first.spans.get_mut(0) {
                        if sp0.content.as_ref() == "└ " {
                            sp0.content = "  ".into();
                            sp0.style = sp0.style.add_modifier(Modifier::DIM);
                        }
                    }
                }
            }
        };

        // Special aggregated rendering for Read: collapse file ranges
        if self.kind == ExecKind::Read {
            // Build aggregated preamble once
            let agg_pre = self.aggregated_read_preamble_lines().unwrap_or_else(|| {
                // Fallback: concatenate per-segment preambles
                let mut all: Vec<Line<'static>> = Vec::new();
                for (i, (pre_raw, _)) in self.segments.iter().enumerate() {
                    let mut pre = trim_empty_lines(pre_raw.clone());
                    if !pre.is_empty() {
                        pre.remove(0);
                    }
                    if i == 0 {
                        // ensure leading corner (legacy for Read aggregation)
                        if let Some(first) = pre.first_mut() {
                            let flat: String =
                                first.spans.iter().map(|s| s.content.as_ref()).collect();
                            let already = flat.trim_start().starts_with("└ ")
                                || flat.trim_start().starts_with("  └ ");
                            if !already {
                                first.spans.insert(
                                    0,
                                    Span::styled(
                                        "└ ",
                                        Style::default().fg(crate::colors::text_dim()),
                                    ),
                                );
                            }
                        }
                    }
                    all.extend(pre);
                }
                all
            });

            // Header was already handled above (including skip accounting).
            // Render aggregated preamble next using the current skip_rows.
            let pre_text = Text::from(agg_pre);
            let pre_wrap_width = area.width;
            let pre_total: u16 = Paragraph::new(pre_text.clone())
                .wrap(Wrap { trim: false })
                .line_count(pre_wrap_width)
                .try_into()
                .unwrap_or(0);
            if cur_y < end_y {
                let pre_skip = skip_rows.min(pre_total);
                let pre_remaining = pre_total.saturating_sub(pre_skip);
                let pre_height = pre_remaining.min(end_y.saturating_sub(cur_y));
                if pre_height > 0 {
                    Paragraph::new(pre_text)
                        .block(Block::default().style(bg))
                        .wrap(Wrap { trim: false })
                        .scroll((pre_skip, 0))
                        .style(bg)
                        .render(
                            Rect {
                                x: area.x,
                                y: cur_y,
                                width: area.width,
                                height: pre_height,
                            },
                            buf,
                        );
                    cur_y = cur_y.saturating_add(pre_height);
                }
                skip_rows = skip_rows.saturating_sub(pre_skip);
            }

            // Render each segment's output only
            let out_wrap_width = area.width.saturating_sub(2);
            for (_pre_raw, out_raw) in self.segments.iter() {
                if cur_y >= end_y {
                    break;
                }
                let out = trim_empty_lines(out_raw.clone());
                let out_text = Text::from(out.clone());
                let out_total: u16 = Paragraph::new(out_text.clone())
                    .wrap(Wrap { trim: false })
                    .line_count(out_wrap_width)
                    .try_into()
                    .unwrap_or(0);
                let out_skip = skip_rows.min(out_total);
                let out_remaining = out_total.saturating_sub(out_skip);
                let out_height = out_remaining.min(end_y.saturating_sub(cur_y));
                if out_height > 0 {
                    let out_area = Rect {
                        x: area.x,
                        y: cur_y,
                        width: area.width,
                        height: out_height,
                    };
                    let block = Block::default()
                        .borders(Borders::LEFT)
                        .border_style(
                            Style::default()
                                .fg(crate::colors::border_dim())
                                .bg(crate::colors::background()),
                        )
                        .style(Style::default().bg(crate::colors::background()))
                        .padding(Padding {
                            left: 1,
                            right: 0,
                            top: 0,
                            bottom: 0,
                        });
                    Paragraph::new(out_text)
                        .block(block)
                        .wrap(Wrap { trim: false })
                        .scroll((out_skip, 0))
                        .style(
                            Style::default()
                                .bg(crate::colors::background())
                                .fg(crate::colors::text_dim()),
                        )
                        .render(out_area, buf);
                    cur_y = cur_y.saturating_add(out_height);
                }
                skip_rows = skip_rows.saturating_sub(out_skip);
            }
            return;
        }

        for (pre_raw, out_raw) in self.segments.iter() {
            if cur_y >= end_y {
                break;
            }
            // Drop the per-segment header line (first element)
            let mut pre = trim_empty_lines(pre_raw.clone());
            if !pre.is_empty() {
                pre.remove(0);
            }
            // Normalize command prefix for generic execs (only on the first segment)
            ensure_prefix(&mut pre);

            let out = trim_empty_lines(out_raw.clone());

            // Measure with same widths as ExecCell
            let pre_text = Text::from(pre.clone());
            let out_text = Text::from(out.clone());
            let pre_wrap_width = area.width;
            let out_wrap_width = area.width.saturating_sub(2);
            let pre_total: u16 = Paragraph::new(pre_text.clone())
                .wrap(Wrap { trim: false })
                .line_count(pre_wrap_width)
                .try_into()
                .unwrap_or(0);
            let out_total: u16 = Paragraph::new(out_text.clone())
                .wrap(Wrap { trim: false })
                .line_count(out_wrap_width)
                .try_into()
                .unwrap_or(0);

            // Apply skip to pre, then out
            let pre_skip = skip_rows.min(pre_total);
            let out_skip = skip_rows.saturating_sub(pre_total).min(out_total);

            // Render pre
            let pre_remaining = pre_total.saturating_sub(pre_skip);
            let pre_height = pre_remaining.min(end_y.saturating_sub(cur_y));
            if pre_height > 0 {
                Paragraph::new(pre_text)
                    .block(Block::default().style(bg))
                    .wrap(Wrap { trim: false })
                    .scroll((pre_skip, 0))
                    .style(bg)
                    .render(
                        Rect {
                            x: area.x,
                            y: cur_y,
                            width: area.width,
                            height: pre_height,
                        },
                        buf,
                    );
                cur_y = cur_y.saturating_add(pre_height);
            }

            if cur_y >= end_y {
                break;
            }
            // Render out as bordered, dim block
            let out_remaining = out_total.saturating_sub(out_skip);
            let out_height = out_remaining.min(end_y.saturating_sub(cur_y));
            if out_height > 0 {
                let out_area = Rect {
                    x: area.x,
                    y: cur_y,
                    width: area.width,
                    height: out_height,
                };
                let block = Block::default()
                    .borders(Borders::LEFT)
                    .border_style(
                        Style::default()
                            .fg(crate::colors::border_dim())
                            .bg(crate::colors::background()),
                    )
                    .style(Style::default().bg(crate::colors::background()))
                    .padding(Padding {
                        left: 1,
                        right: 0,
                        top: 0,
                        bottom: 0,
                    });
                Paragraph::new(out_text)
                    .block(block)
                    .wrap(Wrap { trim: false })
                    .scroll((out_skip, 0))
                    .style(
                        Style::default()
                            .bg(crate::colors::background())
                            .fg(crate::colors::text_dim()),
                    )
                    .render(out_area, buf);
                cur_y = cur_y.saturating_add(out_height);
            }

            // Consume skip rows used in this segment
            let consumed = pre_total + out_total;
            skip_rows = skip_rows.saturating_sub(consumed);
        }
    }
}

fn exec_render_parts_generic(
    command: &[String],
    output: Option<&CommandOutput>,
    start_time: Option<Instant>,
) -> (Vec<Line<'static>>, Vec<Line<'static>>) {
    let mut pre: Vec<Line<'static>> = Vec::new();
    let command_escaped = strip_bash_lc_and_escape(command);
    // Highlight the full command as a bash snippet; we will append
    // the running duration (when applicable) to the first visual line.
    let mut highlighted_cmd: Vec<Line<'static>> =
        crate::syntax_highlight::highlight_code_block(&command_escaped, Some("bash"));

    let header_line = match output {
        None => {
            let duration_str = if let Some(start) = start_time {
                let elapsed = start.elapsed();
                format!(" ({})", format_duration(elapsed))
            } else {
                String::new()
            };
            Line::styled(
                "Running...".to_string() + &duration_str,
                Style::default()
                    .fg(crate::colors::info())
                    .add_modifier(Modifier::BOLD),
            )
        }
        Some(o) if o.exit_code == 0 => Line::styled(
            "Ran",
            Style::default()
                .fg(crate::colors::text_bright())
                .add_modifier(Modifier::BOLD),
        ),
        Some(_) => Line::styled(
            "Ran",
            Style::default()
                .fg(crate::colors::text_bright())
                .add_modifier(Modifier::BOLD),
        ),
    };

    // Compute output first so we know whether to draw a downward corner on the command.
    let out = output_lines(output, false, false);
    let has_output = !trim_empty_lines(out.clone()).is_empty();

    pre.push(header_line.clone());
    if let Some(first) = highlighted_cmd.first_mut() {
        // Append duration (dim) to the first highlighted line when running
        if output.is_none() && start_time.is_some() {
            let elapsed = start_time.unwrap().elapsed();
            let duration_str = format!(" ({})", format_duration(elapsed));
            first.spans.push(Span::styled(
                duration_str,
                Style::default().fg(crate::colors::text_dim()),
            ));
        }
        // Corner is added on the last command line, not the first
    }
    if has_output {
        if let Some(last) = highlighted_cmd.last_mut() {
            last.spans.insert(
                0,
                Span::styled("┌ ", Style::default().fg(crate::colors::text_dim())),
            );
        }
    }
    pre.extend(highlighted_cmd);
    // Output: already computed above
    (pre, out)
}

fn exec_render_parts_parsed(
    parsed_commands: &[ParsedCommand],
    output: Option<&CommandOutput>,
    start_time: Option<Instant>,
) -> (Vec<Line<'static>>, Vec<Line<'static>>) {
    let action = action_from_parsed(parsed_commands);
    let ctx_path = first_context_path(parsed_commands);
    let mut pre: Vec<Line<'static>> = vec![match output {
        None => {
            let duration_str = if let Some(start) = start_time {
                let elapsed = start.elapsed();
                format!(" ({})", format_duration(elapsed))
            } else {
                String::new()
            };
            let header = match action {
                "read" => "Read".to_string(),
                "search" => "Searched".to_string(),
                "list" => "List Files".to_string(),
                _ => match &ctx_path {
                    Some(p) => format!("Running... in {}", p),
                    None => "Running...".to_string(),
                },
            };
            if matches!(action, "read" | "search" | "list") {
                Line::styled(
                    header + &duration_str,
                    Style::default().fg(crate::colors::info()),
                )
            } else {
                Line::styled(
                    header + &duration_str,
                    Style::default()
                        .fg(crate::colors::info())
                        .add_modifier(Modifier::BOLD),
                )
            }
        }
        Some(o) if o.exit_code == 0 => {
            let done = match action {
                "read" => "Read".to_string(),
                "search" => "Searched".to_string(),
                "list" => "List Files".to_string(),
                _ => match &ctx_path {
                    Some(p) => format!("Ran in {}", p),
                    None => "Ran".to_string(),
                },
            };
            if matches!(action, "read" | "search" | "list") {
                Line::styled(done, Style::default().fg(crate::colors::text()))
            } else {
                Line::styled(
                    done,
                    Style::default()
                        .fg(crate::colors::text_bright())
                        .add_modifier(Modifier::BOLD),
                )
            }
        }
        Some(_) => {
            let done = match action {
                "read" => "Read".to_string(),
                "search" => "Searched".to_string(),
                "list" => "List Files".to_string(),
                _ => match &ctx_path {
                    Some(p) => format!("Ran in {}", p),
                    None => "Ran".to_string(),
                },
            };
            if matches!(action, "read" | "search" | "list") {
                Line::styled(done, Style::default().fg(crate::colors::text()))
            } else {
                Line::styled(
                    done,
                    Style::default()
                        .fg(crate::colors::text_bright())
                        .add_modifier(Modifier::BOLD),
                )
            }
        }
    }];

    // Reuse the same parsed-content rendering as new_parsed_command
    let mut search_paths: std::collections::HashSet<String> = std::collections::HashSet::new();
    for pc in parsed_commands.iter() {
        if let ParsedCommand::Search { path: Some(p), .. } = pc {
            search_paths.insert(p.to_string());
        }
    }
    // Compute output preview first to know whether to draw the downward corner.
    let show_stdout = action == "run";
    let out = output_lines(output, !show_stdout, false);
    let mut any_content_emitted = false;
    // Determine allowed label(s) for this cell's primary action
    let expected_label: Option<&'static str> = match action {
        "read" => Some("Read"),
        "search" => Some("Search"),
        "list" => Some("List Files"),
        _ => None, // run: allow a set of labels
    };
    for parsed in parsed_commands.iter() {
        let (label, content) = match parsed {
            ParsedCommand::Read { name, cmd, .. } => {
                let mut c = name.clone();
                if let Some(ann) = parse_read_line_annotation(cmd) {
                    c = format!("{} {}", c, ann);
                }
                ("Read".to_string(), c)
            }
            ParsedCommand::ListFiles { cmd: _, path } => match path {
                Some(p) => {
                    if search_paths.contains(p) {
                        (String::new(), String::new())
                    } else {
                        let display_p = if p.ends_with('/') {
                            p.to_string()
                        } else {
                            format!("{}/", p)
                        };
                        ("List Files".to_string(), format!("in {}", display_p))
                    }
                }
                None => ("List Files".to_string(), "in ./".to_string()),
            },
            ParsedCommand::Search { query, path, cmd } => {
                // Make search terms human-readable:
                // - Unescape any backslash-escaped character (e.g., "\?" -> "?")
                // - Close unbalanced pairs for '(' and '{' to avoid dangling text in UI
                let prettify_term = |s: &str| -> String {
                    // General unescape: remove backslashes that escape the next char
                    let mut out = String::with_capacity(s.len());
                    let mut iter = s.chars();
                    while let Some(ch) = iter.next() {
                        if ch == '\\' {
                            if let Some(next) = iter.next() {
                                out.push(next);
                            } else {
                                out.push('\\');
                            }
                        } else {
                            out.push(ch);
                        }
                    }

                    // Balance parentheses
                    let opens_paren = out.matches("(").count();
                    let closes_paren = out.matches(")").count();
                    for _ in 0..opens_paren.saturating_sub(closes_paren) {
                        out.push(')');
                    }

                    // Balance curly braces
                    let opens_curly = out.matches("{").count();
                    let closes_curly = out.matches("}").count();
                    for _ in 0..opens_curly.saturating_sub(closes_curly) {
                        out.push('}');
                    }

                    out
                };
                let fmt_query = |q: &str| -> String {
                    let mut parts: Vec<String> = q
                        .split('|')
                        .map(|s| s.trim())
                        .filter(|s| !s.is_empty())
                        .map(prettify_term)
                        .collect();
                    match parts.len() {
                        0 => String::new(),
                        1 => parts.remove(0),
                        2 => format!("{} and {}", parts[0], parts[1]),
                        _ => {
                            let last = parts.last().cloned().unwrap_or_default();
                            let head = &parts[..parts.len() - 1];
                            format!("{} and {}", head.join(", "), last)
                        }
                    }
                };
                match (query, path) {
                    (Some(q), Some(p)) => {
                        let display_p = if p.ends_with('/') {
                            p.to_string()
                        } else {
                            format!("{}/", p)
                        };
                        (
                            "Search".to_string(),
                            format!("{} in {}", fmt_query(q), display_p),
                        )
                    }
                    (Some(q), None) => ("Search".to_string(), format!("{}", fmt_query(q))),
                    (None, Some(p)) => {
                        let display_p = if p.ends_with('/') {
                            p.to_string()
                        } else {
                            format!("{}/", p)
                        };
                        ("Search".to_string(), format!("in {}", display_p))
                    }
                    (None, None) => ("Search".to_string(), cmd.clone()),
                }
            }
            ParsedCommand::Format { .. } => ("Format".to_string(), String::new()),
            ParsedCommand::Test { cmd } => ("Test".to_string(), cmd.clone()),
            ParsedCommand::Lint { cmd, .. } => ("Lint".to_string(), cmd.clone()),
            ParsedCommand::Unknown { cmd } => {
                // Suppress separator helpers like `echo ---` which are used
                // internally to delimit chunks when reading files.
                let t = cmd.trim();
                let lower = t.to_lowercase();
                if lower.starts_with("echo") && lower.contains("---") {
                    (String::new(), String::new()) // drop from preamble
                } else {
                    ("Run".to_string(), cmd.clone())
                }
            }
            ParsedCommand::Noop { .. } => continue,
        };
        // Enforce per-action grouping: only keep entries matching this cell's action.
        if let Some(exp) = expected_label {
            if label != exp {
                continue;
            }
        } else if !(label == "Run" || label == "Test" || label == "Lint" || label == "Format") {
            // For generic "run" header, keep common run-like labels only.
            continue;
        }
        if label.is_empty() && content.is_empty() {
            continue;
        }
        for line_text in content.lines() {
            if line_text.is_empty() {
                continue;
            }
            let prefix = if !any_content_emitted { "└ " } else { "  " };
            let mut spans: Vec<Span<'static>> = vec![Span::styled(
                prefix,
                Style::default().add_modifier(Modifier::DIM),
            )];
            match label.as_str() {
                "Search" => {
                    let remaining = line_text.to_string();
                    let (terms_part, path_part) = if let Some(idx) = remaining.rfind(" (in ") {
                        (
                            remaining[..idx].to_string(),
                            Some(remaining[idx..].to_string()),
                        )
                    } else if let Some(idx) = remaining.rfind(" in ") {
                        let suffix = &remaining[idx + 1..];
                        if suffix.trim_end().ends_with('/') {
                            (
                                remaining[..idx].to_string(),
                                Some(remaining[idx..].to_string()),
                            )
                        } else {
                            (remaining.clone(), None)
                        }
                    } else {
                        (remaining.clone(), None)
                    };
                    let tmp = terms_part.clone();
                    let chunks: Vec<String> = if tmp.contains(", ") {
                        tmp.split(", ").map(|s| s.to_string()).collect()
                    } else {
                        vec![tmp.clone()]
                    };
                    for (i, chunk) in chunks.iter().enumerate() {
                        if i > 0 {
                            spans.push(Span::styled(
                                ", ",
                                Style::default().fg(crate::colors::text_dim()),
                            ));
                        }
                        if let Some((left, right)) = chunk.rsplit_once(" and ") {
                            if !left.is_empty() {
                                spans.push(Span::styled(
                                    left.to_string(),
                                    Style::default().fg(crate::colors::text()),
                                ));
                                spans.push(Span::styled(
                                    " and ",
                                    Style::default().fg(crate::colors::text_dim()),
                                ));
                                spans.push(Span::styled(
                                    right.to_string(),
                                    Style::default().fg(crate::colors::text()),
                                ));
                            } else {
                                spans.push(Span::styled(
                                    chunk.to_string(),
                                    Style::default().fg(crate::colors::text()),
                                ));
                            }
                        } else {
                            spans.push(Span::styled(
                                chunk.to_string(),
                                Style::default().fg(crate::colors::text()),
                            ));
                        }
                    }
                    if let Some(p) = path_part {
                        spans.push(Span::styled(
                            p,
                            Style::default().fg(crate::colors::text_dim()),
                        ));
                    }
                }
                "Read" => {
                    if let Some(idx) = line_text.find(" (") {
                        let (fname, rest) = line_text.split_at(idx);
                        spans.push(Span::styled(
                            fname.to_string(),
                            Style::default().fg(crate::colors::text()),
                        ));
                        spans.push(Span::styled(
                            rest.to_string(),
                            Style::default().fg(crate::colors::text_dim()),
                        ));
                    } else {
                        spans.push(Span::styled(
                            line_text.to_string(),
                            Style::default().fg(crate::colors::text()),
                        ));
                    }
                }
                "List" => {
                    spans.push(Span::styled(
                        line_text.to_string(),
                        Style::default().fg(crate::colors::text()),
                    ));
                }
                _ => {
                    // Apply shell syntax highlighting to executed command lines.
                    // We highlight the single logical line as bash and append its spans inline.
                    let mut hl =
                        crate::syntax_highlight::highlight_code_block(line_text, Some("bash"));
                    if let Some(mut first) = hl.pop() {
                        // `highlight_code_block` returns exactly one line for single-line input.
                        // Append the highlighted spans inline after the prefix.
                        spans.extend(first.spans.drain(..));
                    } else {
                        // Fallback: plain text if highlighting yields nothing.
                        spans.push(Span::styled(
                            line_text.to_string(),
                            Style::default().fg(crate::colors::text()),
                        ));
                    }
                }
            }
            pre.push(Line::from(spans));
            any_content_emitted = true;
        }
    }

    // Collapse adjacent Read ranges for the same file inside a single exec's preamble
    coalesce_read_ranges_in_lines_local(&mut pre);

    // Output: show stdout only for real run commands; errors always included
    (pre, out)
}

// Local helper: coalesce "<file> (lines A to B)" entries when contiguous.
fn coalesce_read_ranges_in_lines_local(lines: &mut Vec<Line<'static>>) {
    use ratatui::text::Span;
    if lines.len() <= 1 {
        return;
    }
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
                    if let (Ok(a), Ok(b)) = (s1.trim().parse::<u32>(), s2.trim().parse::<u32>()) {
                        return Some((fname, a, b, prefix));
                    }
                }
            }
        }
        None
    }
    // Merge overlapping or touching ranges for the same file, regardless of adjacency.
    let mut i: usize = 0;
    while i < lines.len() {
        let Some((fname_a, mut a1, mut a2, prefix_a)) = parse_read_line(&lines[i]) else {
            i += 1;
            continue;
        };
        let mut k = i + 1;
        while k < lines.len() {
            if let Some((fname_b, b1, b2, _prefix_b)) = parse_read_line(&lines[k]) {
                if fname_b == fname_a {
                    // Merge if overlapping or contiguous
                    let touch_or_overlap = b1 <= a2.saturating_add(1) && b2.saturating_add(1) >= a1;
                    if touch_or_overlap {
                        a1 = a1.min(b1);
                        a2 = a2.max(b2);
                        let new_spans: Vec<Span<'static>> = vec![
                            Span::styled(
                                prefix_a.clone(),
                                Style::default().add_modifier(Modifier::DIM),
                            ),
                            Span::styled(
                                fname_a.clone(),
                                Style::default().fg(crate::colors::text()),
                            ),
                            Span::styled(
                                format!(" (lines {} to {})", a1, a2),
                                Style::default().fg(crate::colors::text_dim()),
                            ),
                        ];
                        lines[i] = Line::from(new_spans);
                        lines.remove(k);
                        continue; // keep checking more occurrences of the same file
                    }
                }
            }
            k += 1;
        }
        i += 1;
    }
}

impl WidgetRef for &ExecCell {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        Paragraph::new(Text::from(self.display_lines_trimmed()))
            .wrap(Wrap { trim: false })
            .style(Style::default().bg(crate::colors::background()))
            .render(area, buf);
    }
}

// ==================== AnimatedWelcomeCell ====================

pub(crate) struct AnimatedWelcomeCell {
    pub(crate) start_time: Instant,
    pub(crate) completed: std::cell::Cell<bool>,
    pub(crate) fade_start: std::cell::Cell<Option<Instant>>,
    pub(crate) faded_out: std::cell::Cell<bool>,
    // Lock the measured height on first layout so it doesn't resize later
    pub(crate) locked_height: std::cell::Cell<Option<u16>>,
    // When true, render nothing but keep reserved height (for stable layout)
    pub(crate) hidden: std::cell::Cell<bool>,
}

impl HistoryCell for AnimatedWelcomeCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::AnimatedWelcome
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // For plain lines, just show a simple welcome message
        vec![
            Line::from(""),
            Line::from("Welcome to Code"),
            Line::from("What can I code for you today?"),
            Line::from(""),
        ]
    }

    fn desired_height(&self, width: u16) -> u16 {
        // On first use, choose a height based on width; then lock it to avoid
        // resizing as the user scrolls or resizes slightly.
        if let Some(h) = self.locked_height.get() {
            return h.saturating_add(3);
        }

        // Word "CODE" uses 4 letters of 5 cols each with 3 gaps: 4*5 + 3 = 23 cols.
        let cols: u16 = 23;
        let base_rows: u16 = 7;
        let max_scale: u16 = 3;
        let scale = if width >= cols {
            (width / cols).min(max_scale).max(1)
        } else {
            1
        };
        let h = base_rows.saturating_mul(scale);
        self.locked_height.set(Some(h));
        // Add a little padding below to give extra spacing
        h.saturating_add(3)
    }

    fn has_custom_render(&self) -> bool {
        true // AnimatedWelcomeCell uses custom rendering for the glitch animation
    }

    fn custom_render(&self, area: Rect, buf: &mut Buffer) {
        // If hidden, draw nothing (area will retain background) to preserve layout height.
        if self.hidden.get() {
            return;
        }
        let _elapsed = self.start_time.elapsed();
        // Top-align within the provided area so scrolling simply crops the top.
        // Limit to our locked height if present to avoid growth/shrink.
        let locked_h = self.locked_height.get().unwrap_or(21);
        let height = locked_h.min(area.height);
        let positioned_area = Rect {
            x: area.x,
            y: area.y,
            width: area.width,
            height,
        };

        let fade_duration = std::time::Duration::from_millis(800);

        // Check if we're in fade-out phase
        if let Some(fade_time) = self.fade_start.get() {
            let fade_elapsed = fade_time.elapsed();
            if fade_elapsed < fade_duration && !self.faded_out.get() {
                // Fade-out animation
                let fade_progress = fade_elapsed.as_secs_f32() / fade_duration.as_secs_f32();
                let alpha = 1.0 - fade_progress; // From 1.0 to 0.0

                crate::glitch_animation::render_intro_animation_with_alpha(
                    positioned_area,
                    buf,
                    1.0, // Full animation progress (static state)
                    alpha,
                );
            } else {
                // Fade-out complete - mark as faded out
                self.faded_out.set(true);
                // Don't render anything (invisible)
                // not rendering
            }
        } else {
            // Normal animation phase
            let elapsed = self.start_time.elapsed();
            let animation_duration = std::time::Duration::from_secs(2);

            if elapsed < animation_duration && !self.completed.get() {
                // Calculate animation progress
                let progress = elapsed.as_secs_f32() / animation_duration.as_secs_f32();

                // Render the animation
                crate::glitch_animation::render_intro_animation(positioned_area, buf, progress);
            } else {
                // Animation complete - mark it and render final static state
                self.completed.set(true);

                // Render the final static state
                crate::glitch_animation::render_intro_animation(positioned_area, buf, 1.0);
            }
        }
    }

    fn is_animating(&self) -> bool {
        // Check for initial animation
        if !self.completed.get() {
            let elapsed = self.start_time.elapsed();
            let animation_duration = std::time::Duration::from_secs(2);
            if elapsed < animation_duration {
                return true;
            }
            // Mark as completed if animation time has passed
            self.completed.set(true);
        }

        // Check for fade-out animation
        if let Some(fade_time) = self.fade_start.get() {
            if !self.faded_out.get() {
                let fade_elapsed = fade_time.elapsed();
                let fade_duration = std::time::Duration::from_millis(800);
                if fade_elapsed < fade_duration {
                    return true;
                }
                // Mark as faded out if fade time has passed
                self.faded_out.set(true);
            }
        }

        false
    }

    fn trigger_fade(&self) {
        // Only trigger fade if not already fading or faded
        if self.fade_start.get().is_none() {
            self.fade_start.set(Some(Instant::now()));
        }
    }

    fn should_remove(&self) -> bool {
        // Remove only after fade-out is complete
        self.faded_out.get()
    }
}

// ==================== LoadingCell ====================

#[allow(dead_code)]
pub(crate) struct LoadingCell {
    #[allow(dead_code)] // May be used for displaying status alongside animation
    pub(crate) message: String,
}

impl HistoryCell for LoadingCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Loading
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("⟳ ", Style::default().fg(crate::colors::info())),
                Span::from("Loading..."),
            ]),
            Line::from(""),
        ]
    }

    fn desired_height(&self, _width: u16) -> u16 {
        3 // Just 3 lines for the loading message
    }

    fn has_custom_render(&self) -> bool {
        false // No custom rendering needed, just use display_lines
    }

    fn is_animating(&self) -> bool {
        false // Not animating - no need for constant redraws
    }

    fn is_loading_cell(&self) -> bool {
        true // This is a loading cell
    }
}
/// Return the emoji followed by a hair space (U+200A) and a normal space.
/// This creates a reasonable gap across different terminals,
/// in particular Terminal.app and iTerm, which render too tightly with just a single normal space.
///
/// Improvements here could be to condition this behavior on terminal,
/// or possibly on emoji.
// Removed unused helpers padded_emoji and padded_emoji_with.

// ==================== ImageOutputCell ====================

pub(crate) struct ImageOutputCell {
    #[allow(dead_code)] // Will be used for terminal image protocol support
    pub(crate) image: DynamicImage,
}

impl HistoryCell for ImageOutputCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Image
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        vec![
            Line::from("tool result (image output omitted)"),
            Line::from(""),
        ]
    }
}

// ==================== ToolCallCell ====================

pub(crate) enum ToolState {
    #[allow(dead_code)]
    Running,
    Success,
    Failed,
}

pub(crate) struct ToolCallCell {
    lines: Vec<Line<'static>>,
    state: ToolState,
}

impl HistoryCell for ToolCallCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Tool {
            status: match self.state {
                ToolState::Running => ToolStatus::Running,
                ToolState::Success => ToolStatus::Success,
                ToolState::Failed => ToolStatus::Failed,
            },
        }
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // Show all lines; header visibility aligns with exec-style sections
        self.lines.clone()
    }
}

impl ToolCallCell {
    pub(crate) fn retint(&mut self, old: &crate::theme::Theme, new: &crate::theme::Theme) {
        retint_lines_in_place(&mut self.lines, old, new);
    }
}

// ==================== RunningToolCallCell (animated) ====================

pub(crate) struct RunningToolCallCell {
    title: String,
    start_time: Instant,
    arg_lines: Vec<Line<'static>>,
}

impl HistoryCell for RunningToolCallCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Tool {
            status: ToolStatus::Running,
        }
    }
    fn is_animating(&self) -> bool {
        true
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        let elapsed = self.start_time.elapsed();
        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::styled(
            format!("{} ({})", self.title, format_duration(elapsed)),
            Style::default()
                .fg(crate::colors::info())
                .add_modifier(Modifier::BOLD),
        ));
        lines.extend(self.arg_lines.clone());
        lines.push(Line::from(""));
        lines
    }
}

impl RunningToolCallCell {
    pub(crate) fn has_title(&self, title: &str) -> bool {
        self.title == title
    }
    /// Finalize a running web search cell into a completed ToolCallCell.
    pub(crate) fn finalize_web_search(&self, success: bool, query: Option<String>) -> ToolCallCell {
        let duration = self.start_time.elapsed();
        let title = if success {
            "Web Search"
        } else {
            "Web Search (failed)"
        };
        let duration = format_duration(duration);

        let title_line = if success {
            Line::from(vec![
                Span::styled(
                    title,
                    Style::default()
                        .fg(crate::colors::success())
                        .add_modifier(Modifier::BOLD),
                ),
                format!(", duration: {duration}").dim(),
            ])
        } else {
            Line::from(vec![
                Span::styled(
                    title,
                    Style::default()
                        .fg(crate::colors::error())
                        .add_modifier(Modifier::BOLD),
                ),
                format!(", duration: {duration}").dim(),
            ])
        };

        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(title_line);
        if let Some(q) = query {
            lines.push(Line::from(vec![
                Span::styled("└ query: ", Style::default().fg(crate::colors::text_dim())),
                Span::styled(q, Style::default().fg(crate::colors::text())),
            ]));
        }
        lines.push(Line::from(""));

        ToolCallCell {
            lines,
            state: if success {
                ToolState::Success
            } else {
                ToolState::Failed
            },
        }
    }
}

// ==================== CollapsibleReasoningCell ====================
// For reasoning content that can be collapsed to show only summary

pub(crate) struct CollapsibleReasoningCell {
    pub(crate) lines: Vec<Line<'static>>,
    pub(crate) collapsed: std::cell::Cell<bool>,
    pub(crate) in_progress: std::cell::Cell<bool>,
}

impl CollapsibleReasoningCell {
    pub fn new(lines: Vec<Line<'static>>) -> Self {
        Self {
            lines,
            collapsed: std::cell::Cell::new(true), // Default to collapsed
            in_progress: std::cell::Cell::new(false),
        }
    }

    pub fn toggle_collapsed(&self) {
        self.collapsed.set(!self.collapsed.get());
    }

    pub fn set_collapsed(&self, collapsed: bool) {
        self.collapsed.set(collapsed);
    }

    #[allow(dead_code)]
    pub fn is_collapsed(&self) -> bool {
        self.collapsed.get()
    }

    pub fn set_in_progress(&self, in_progress: bool) {
        self.in_progress.set(in_progress);
    }

    /// Normalize reasoning content lines by splitting any line that begins
    /// with a bold "section title" followed immediately by regular text.
    /// This produces a separate title line and keeps following text on a new line,
    /// improving section detection and spacing.
    fn normalized_lines(&self) -> Vec<Line<'static>> {
        let mut out: Vec<Line<'static>> = Vec::new();
        for line in &self.lines {
            // Skip unchanged if empty or single span
            if line.spans.len() <= 1 {
                out.push(line.clone());
                continue;
            }

            // Determine length of the leading bold run
            let mut idx = 0usize;
            while idx < line.spans.len() {
                let s = &line.spans[idx];
                // Treat heading-style titles (often bold) as bold too
                let is_bold = s.style.add_modifier.contains(Modifier::BOLD);
                if idx == 0 && s.content.trim().is_empty() {
                    // allow leading spaces in the bold run
                    idx += 1;
                    continue;
                }
                if is_bold {
                    idx += 1;
                    continue;
                }
                break;
            }

            // If no leading bold run or the entire line is bold, keep as-is
            if idx == 0 || idx >= line.spans.len() {
                out.push(line.clone());
                continue;
            }

            // Create a separate title line from the leading bold spans
            let mut title_spans = Vec::new();
            let mut rest_spans = Vec::new();
            for (i, s) in line.spans.iter().enumerate() {
                if i < idx {
                    title_spans.push(s.clone());
                } else {
                    rest_spans.push(s.clone());
                }
            }

            // Push title line
            out.push(Line::from(title_spans));
            // Insert a spacer if the rest is non-empty and not already a blank line
            let rest_is_blank = rest_spans.iter().all(|s| s.content.trim().is_empty());
            if !rest_is_blank {
                out.push(Line::from(rest_spans));
            }
        }
        out
    }

    /// Extracts section titles for collapsed display: any line that appears to
    /// be a heading or starts with a leading bold run. Returns at least the
    /// first non-empty non-header line if no titles found.
    fn extract_section_titles(&self) -> Vec<Line<'static>> {
        let lines = self.normalized_lines();
        let mut titles: Vec<Line<'static>> = Vec::new();
        for (idx, l) in lines.iter().enumerate() {
            // Skip blank lines
            let text: String = l.spans.iter().map(|s| s.content.as_ref()).collect();
            let trimmed = text.trim();
            if trimmed.is_empty() {
                continue;
            }

            // Title heuristics:
            // 1) Entire line bold
            let all_bold = !l.spans.is_empty()
                && l.spans.iter().all(|s| {
                    s.style.add_modifier.contains(Modifier::BOLD) || s.content.trim().is_empty()
                });
            // 2) Starts with one or more bold spans and ends with ':'
            let mut leading_bold = true;
            for s in &l.spans {
                if s.content.trim().is_empty() {
                    continue;
                }
                leading_bold &= s.style.add_modifier.contains(Modifier::BOLD);
                break;
            }
            let ends_colon = trimmed.ends_with(':');

            // 3) Markdown heading (begins with '#') - renderer includes hashes in content
            let is_md_heading = trimmed.starts_with('#');

            // 4) Title-like plain line: reasonably short, not a meta/instructional preamble,
            //    no terminal punctuation (including ':'), and either first line or preceded
            //    by a blank separator.
            let prev_blank = idx == 0
                || lines
                    .get(idx.saturating_sub(1))
                    .map(|pl| pl.spans.iter().all(|s| s.content.trim().is_empty()))
                    .unwrap_or(true);
            let len_ok = trimmed.chars().count() >= 3 && trimmed.chars().count() <= 80;
            // Consider quotes/closing brackets at the end when checking punctuation
            let mut tail = trimmed;
            while let Some(last) = tail.chars().last() {
                if matches!(last, '"' | '\'' | '”' | '’' | ')' | ']' | '}') {
                    tail = &tail[..tail.len()-last.len_utf8()];
                } else { break; }
            }
            let no_terminal_punct = !tail.ends_with('.') && !tail.ends_with('!') && !tail.ends_with('?') && !tail.ends_with(':');
            let lowered = trimmed.to_ascii_lowercase();
            let is_meta_intro = lowered.starts_with("here are ")
                || lowered.starts_with("i need to ")
                || lowered.starts_with("i plan to ")
                || lowered.starts_with("let's ")
                || lowered.starts_with("we should ")
                || lowered.starts_with("i'll ")
                || lowered.starts_with("next, ")
                || lowered.starts_with("now, ");
            let plain_title_like = prev_blank && len_ok && no_terminal_punct && !is_meta_intro;

            if all_bold || (leading_bold && ends_colon) || is_md_heading || plain_title_like {
                titles.push(l.clone());
            }
        }

        if titles.is_empty() {
            // Fallback: first non-empty line as summary
            for l in lines.iter() {
                let text: String = l.spans.iter().map(|s| s.content.as_ref()).collect();
                let trimmed = text.trim();
                if trimmed.is_empty() {
                    continue;
                }
                titles.push(l.clone());
                break;
            }
        }

        // Style collapsed titles consistently dim to match reasoning theme
        let color = crate::colors::text_dim();
        titles
            .into_iter()
            .map(|line| {
                let spans: Vec<Span<'static>> = line
                    .spans
                    .into_iter()
                    .map(|s| s.style(Style::default().fg(color)))
                    .collect();
                Line::from(spans)
            })
            .collect()
    }
}

impl HistoryCell for CollapsibleReasoningCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Reasoning
    }

    // Ensure collapsed reasoning always gets standard spacing after it.
    // Treating it as a title-only cell suppresses the inter-cell spacer,
    // which causes the missing blank line effect users observed.
    fn is_title_only(&self) -> bool {
        false
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        if self.lines.is_empty() {
            return Vec::new();
        }

        // Normalize to improve section splitting and spacing
        let normalized = self.normalized_lines();

        // There is no explicit 'thinking' header; show all lines
        let start_idx = 0;

        if self.collapsed.get() {
            // When collapsed, show extracted section titles (or at least one summary)
            let mut titles = self.extract_section_titles();
            if self.in_progress.get() {
                if let Some(last) = titles.pop() {
                    let mut spans = last.spans;
                    spans.push(Span::styled(
                        "…",
                        Style::default().fg(crate::colors::text_dim()),
                    ));
                    titles.push(Line::from(spans));
                } else {
                    titles.push(Line::from("…"));
                }
            }
            titles
        } else {
            // When expanded, show all lines; append an ellipsis if in progress
            let mut out = normalized[start_idx..].to_vec();
            if self.in_progress.get() {
                out.push(Line::from("…".dim()));
            }
            out
        }
    }

    fn gutter_symbol(&self) -> Option<&'static str> {
        // No gutter icon for reasoning/thinking
        None
    }
}

// ==================== StreamingContentCell ====================
// For live streaming content that's being actively rendered

pub(crate) struct StreamingContentCell {
    pub(crate) id: Option<String>,
    pub(crate) lines: Vec<Line<'static>>,
}

impl HistoryCell for StreamingContentCell {
    // IMPORTANT: We must support immutable downcasting here. The TUI replaces
    // an in‑progress StreamingContentCell with a finalized AssistantMarkdownCell
    // by searching history via `c.as_any().downcast_ref::<StreamingContentCell>()`
    // and matching on the stream `id`. If this returns a dummy type (default impl)
    // instead of `self`, the lookup fails and the final cannot find the streaming
    // cell — leading to duplicates (final gets appended instead of replaced).
    // See: chatwidget.rs::insert_final_answer_with_id and related logs
    // ("final-answer: append new AssistantMarkdownCell (no prior cell)").
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Assistant
    }
    fn has_custom_render(&self) -> bool {
        true
    }
    fn desired_height(&self, width: u16) -> u16 {
        // Match streaming render path with bullet-aware prewrapping and no double wrap.
        let text_wrap_width = width;
        let src_lines = self.display_lines_trimmed();

        #[derive(Debug)]
        enum Seg { Text(Vec<Line<'static>>), Bullet(Vec<Line<'static>>), Code(Vec<Line<'static>>) }

        let mut segs: Vec<Seg> = Vec::new();
        let mut text_buf: Vec<Line<'static>> = Vec::new();
        let mut _is_first_output_line = true;
        let mut iter = src_lines.into_iter().peekable();
        while let Some(line) = iter.next() {
            if crate::render::line_utils::is_code_block_painted(&line) {
                if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                let mut chunk = vec![line];
                while let Some(n) = iter.peek() {
                    if crate::render::line_utils::is_code_block_painted(n) { chunk.push(iter.next().unwrap()); } else { break; }
                }
                segs.push(Seg::Code(chunk));
                continue;
            }
            if text_wrap_width > 4 && is_horizontal_rule_line(&line) {
                if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                let hr = Line::from(Span::styled(
                    std::iter::repeat('─').take(text_wrap_width as usize).collect::<String>(),
                    Style::default().fg(crate::colors::assistant_hr()),
                ));
                segs.push(Seg::Bullet(vec![hr]));
                _is_first_output_line = false;
                continue;
            }
            if text_wrap_width > 4 {
                if let Some((indent_spaces, bullet_char)) = detect_bullet_prefix(&line) {
                    if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                    segs.push(Seg::Bullet(wrap_bullet_line(
                        line,
                        indent_spaces,
                        &bullet_char,
                        text_wrap_width,
                    )));
                    _is_first_output_line = false;
                    continue;
                }
            }
            text_buf.push(line);
            _is_first_output_line = false;
        }
        if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }

        let mut total: u16 = 0;
        for seg in segs {
            match seg {
                Seg::Bullet(lines) => total = total.saturating_add(lines.len() as u16),
                Seg::Text(lines) => {
                    if lines.is_empty() { continue; }
                    let text = Text::from(lines);
                    let rows: u16 = Paragraph::new(text)
                        .wrap(Wrap { trim: false })
                        .line_count(text_wrap_width)
                        .try_into()
                        .unwrap_or(0);
                    total = total.saturating_add(rows);
                }
                Seg::Code(mut chunk) => {
                    if let Some(first) = chunk.first() {
                        let flat: String = first.spans.iter().map(|s| s.content.as_ref()).collect();
                        if flat.contains("⟦LANG:") { chunk.remove(0); }
                    }
                    while chunk.first().is_some_and(|l| crate::render::line_utils::is_blank_line_spaces_only(l)) { chunk.remove(0); }
                    while chunk.last().is_some_and(|l| crate::render::line_utils::is_blank_line_spaces_only(l)) { chunk.pop(); }
                    total = total.saturating_add(chunk.len() as u16 + 2);
                }
            }
        }
        total.saturating_add(2)
    }
    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        // Render with a 1-row top and bottom padding, all using the assistant bg tint.
        let cell_bg = crate::colors::assistant_bg();
        let bg_style = Style::default().bg(cell_bg);

        // Hard clear area with assistant background
        for y in area.y..area.y.saturating_add(area.height) {
            for x in area.x..area.x.saturating_add(area.width) {
                buf[(x, y)].set_char(' ').set_style(bg_style);
            }
        }

        // Build segments: prewrapped bullets, code (no rewrap), and normal text.
        let text_wrap_width = area.width;
        #[derive(Debug)]
        enum Seg { Text(Vec<Line<'static>>), Bullet(Vec<Line<'static>>), Code(Vec<Line<'static>>) }
        let mut segs: Vec<Seg> = Vec::new();
        let mut text_buf: Vec<Line<'static>> = Vec::new();
        let mut _is_first_output_line = true;
        let mut iter = self.display_lines_trimmed().into_iter().peekable();
        while let Some(line) = iter.next() {
            if crate::render::line_utils::is_code_block_painted(&line) {
                if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                let mut chunk = vec![line];
                while let Some(n) = iter.peek() { if crate::render::line_utils::is_code_block_painted(n) { chunk.push(iter.next().unwrap()); } else { break; } }
                // Trim padding rows inside code card
                while chunk.first().is_some_and(|l| crate::render::line_utils::is_blank_line_spaces_only(l)) { chunk.remove(0); }
                while chunk.last().is_some_and(|l| crate::render::line_utils::is_blank_line_spaces_only(l)) { chunk.pop(); }
                segs.push(Seg::Code(chunk));
                continue;
            }
            if text_wrap_width > 4 && is_horizontal_rule_line(&line) {
                if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                let hr = Line::from(Span::styled(
                    std::iter::repeat('─').take(text_wrap_width as usize).collect::<String>(),
                    Style::default().fg(crate::colors::assistant_hr()),
                ));
                segs.push(Seg::Bullet(vec![hr]));
                _is_first_output_line = false;
                continue;
            }
            if text_wrap_width > 4 {
                if let Some((indent_spaces, bullet_char)) = detect_bullet_prefix(&line) {
                    if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }
                    segs.push(Seg::Bullet(wrap_bullet_line(
                        line,
                        indent_spaces,
                        &bullet_char,
                        text_wrap_width,
                    )));
                    _is_first_output_line = false;
                    continue;
                }
            }
            text_buf.push(line);
            _is_first_output_line = false;
        }
        if !text_buf.is_empty() { segs.push(Seg::Text(std::mem::take(&mut text_buf))); }

        // Streaming-style top padding row
        let mut remaining_skip = skip_rows;
        let mut cur_y = area.y;
        let end_y = area.y.saturating_add(area.height);
        if remaining_skip == 0 && cur_y < end_y {
            cur_y = cur_y.saturating_add(1);
        }
        remaining_skip = remaining_skip.saturating_sub(1);

        // Helpers
        use unicode_width::UnicodeWidthStr as UW;
        let measure_line =
            |l: &Line<'_>| -> usize { l.spans.iter().map(|s| UW::width(s.content.as_ref())).sum() };
        let mut draw_segment = |seg: &Seg, y: &mut u16, skip: &mut u16| {
            if *y >= end_y {
                return;
            }
            match seg {
                Seg::Text(lines) => {
                    let txt = Text::from(lines.clone());
                    let total: u16 = Paragraph::new(txt.clone())
                        .wrap(Wrap { trim: false })
                        .line_count(text_wrap_width)
                        .try_into()
                        .unwrap_or(0);
                    if *skip >= total {
                        *skip -= total;
                        return;
                    }
                    let avail = end_y.saturating_sub(*y);
                    let draw_h = (total.saturating_sub(*skip)).min(avail);
                    if draw_h == 0 {
                        return;
                    }
                    let rect = Rect {
                        x: area.x,
                        y: *y,
                        width: area.width,
                        height: draw_h,
                    };
                    Paragraph::new(txt)
                        .block(Block::default().style(bg_style))
                        .wrap(Wrap { trim: false })
                        .scroll((*skip, 0))
                        .style(bg_style)
                        .render(rect, buf);
                    *y = y.saturating_add(draw_h);
                    *skip = 0;
                }
                Seg::Bullet(lines) => {
                    let total = lines.len() as u16;
                    if *skip >= total { *skip -= total; return; }
                    let avail = end_y.saturating_sub(*y);
                    let draw_h = (total.saturating_sub(*skip)).min(avail);
                    if draw_h == 0 { return; }
                    let rect = Rect { x: area.x, y: *y, width: area.width, height: draw_h };
                    let txt = Text::from(lines.clone());
                    Paragraph::new(txt)
                        .block(Block::default().style(bg_style))
                        .scroll((*skip, 0))
                        .style(bg_style)
                        .render(rect, buf);
                    *y = y.saturating_add(draw_h);
                    *skip = 0;
                }
                Seg::Code(lines_in) => {
                    if lines_in.is_empty() {
                        return;
                    }
                    // Extract optional language sentinel and drop it from the content lines
                    let mut lang_label: Option<String> = None;
                    let mut lines = lines_in.clone();
                    if let Some(first) = lines.first() {
                        let flat: String = first.spans.iter().map(|s| s.content.as_ref()).collect();
                        if let Some(s) = flat.strip_prefix("⟦LANG:") {
                            if let Some(end) = s.find('⟧') {
                                lang_label = Some(s[..end].to_string());
                                lines.remove(0);
                            }
                        }
                    }
                    if lines.is_empty() {
                        return;
                    }
                    // Determine target width of the code card (respect content width)
                    let max_w = lines.iter().map(|l| measure_line(l)).max().unwrap_or(0) as u16;
                    let inner_w = max_w.max(1);
                    // Borders (2) + inner left/right padding (4 total for two spaces each)
                    let card_w = inner_w.saturating_add(6).min(area.width.max(6));
                    // Include top/bottom border only (2); no inner vertical padding
                    let total = lines.len() as u16 + 2;
                    if *skip >= total {
                        *skip -= total;
                        return;
                    }
                    let avail = end_y.saturating_sub(*y);
                    if avail == 0 {
                        return;
                    }
                    // Compute visible slice of the card (accounting for inner padding rows)
                    let mut local_skip = *skip;
                    let mut top_border = 1u16;
                    if local_skip > 0 {
                        let drop = local_skip.min(top_border);
                        top_border -= drop;
                        local_skip -= drop;
                    }
                    let code_skip = local_skip.min(lines.len() as u16);
                    local_skip -= code_skip;
                    let mut bottom_border = 1u16;
                    if local_skip > 0 {
                        let drop = local_skip.min(bottom_border);
                        bottom_border -= drop;
                    }
                    let visible = top_border
                        + (lines.len() as u16 - code_skip)
                        + bottom_border;
                    let draw_h = visible.min(avail);
                    if draw_h == 0 {
                        return;
                    }
                    // Align card to content area (no outer left/right stripes)
                    let content_x = area.x;
                    let rect_x = content_x;
                    // Draw bordered block for the visible rows
                    let rect = Rect {
                        x: rect_x,
                        y: *y,
                        width: card_w,
                        height: draw_h,
                    };
                    let code_bg = crate::colors::code_block_bg();
                    let mut blk = Block::default()
                        .borders(Borders::ALL)
                        .border_style(Style::default().fg(crate::colors::border()))
                        .style(Style::default().bg(code_bg))
                        .padding(Padding { left: 2, right: 2, top: 0, bottom: 0 });
                    if let Some(lang) = &lang_label {
                        blk = blk.title(Span::styled(
                            format!(" {} ", lang),
                            Style::default().fg(crate::colors::text_dim()),
                        ));
                    }
                    let blk_for_inner = blk.clone();
                    blk.render(rect, buf);
                    // Inner paragraph area (exclude borders)
                    let inner_rect = blk_for_inner.inner(rect);
                    let inner_h = inner_rect.height.min(rect.height);
                    if inner_h > 0 {
                        let slice_start = code_skip as usize;
                        let txt = Text::from(lines[slice_start..].to_vec());
                        Paragraph::new(txt)
                            .style(Style::default().bg(code_bg))
                            .block(Block::default().style(Style::default().bg(code_bg)))
                            .render(inner_rect, buf);
                    }
                    // No outside padding stripes.
                    *y = y.saturating_add(draw_h);
                    *skip = 0;
                }
            }
        };

        for seg in &segs {
            if cur_y >= end_y {
                break;
            }
            draw_segment(seg, &mut cur_y, &mut remaining_skip);
        }
        // Bottom padding row (blank): area already cleared
        if remaining_skip == 0 && cur_y < end_y {
            cur_y = cur_y.saturating_add(1);
        } else {
            remaining_skip = remaining_skip.saturating_sub(1);
        }
        // Mark as used to satisfy unused_assignments lint
        let _ = (cur_y, remaining_skip);
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // Hide the header line (e.g., "codex") when using a gutter symbol
        if self.gutter_symbol().is_some() {
            if self.lines.len() == 1 {
                // Single-line cell with gutter symbol = just a header, hide it completely
                Vec::new()
            } else {
                // Multi-line cell with gutter symbol = skip the title line
                self.lines[1..].to_vec()
            }
        } else {
            self.lines.clone()
        }
    }
}

// Detect lines that start with a markdown bullet produced by our renderer and return (indent, bullet)
fn detect_bullet_prefix(line: &ratatui::text::Line<'_>) -> Option<(usize, String)> {
    // Treat these as unordered bullets, plus checkbox glyphs for task lists.
    let bullets = ["-", "•", "◦", "·", "∘", "⋅", "☐", "✔"];
    let spans = &line.spans;
    if spans.is_empty() {
        return None;
    }
    // First span may be leading spaces
    let mut idx = 0;
    let mut indent = 0usize;
    if let Some(s) = spans.get(0) {
        let t = s.content.as_ref();
        if !t.is_empty() && t.chars().all(|c| c == ' ') {
            indent = t.chars().count();
            idx = 1;
        }
    }
    // Next must be a bullet-like prefix with an accompanying space. Accept either
    // a separate single-space span after the marker OR a trailing space baked
    // into the bullet span (e.g., checkboxes like "☐ ").
    let bullet_span = spans.get(idx)?;
    let mut bullet_text = bullet_span.content.as_ref().to_string();
    let has_following_space_span = spans
        .get(idx + 1)
        .map(|s| s.content.as_ref() == " ")
        .unwrap_or(false);
    let has_trailing_space_in_bullet = bullet_text.ends_with(' ');
    if !(has_following_space_span || has_trailing_space_in_bullet) {
        return None;
    }
    if has_trailing_space_in_bullet {
        bullet_text.pop();
    }
    if bullets.contains(&bullet_text.as_str()) {
        return Some((indent, bullet_text));
    }
    // Ordered list: e.g., "1.", "12.", etc.
    if bullet_text.len() >= 2
        && bullet_text.ends_with('.')
        && bullet_text[..bullet_text.len() - 1]
            .chars()
            .all(|c| c.is_ascii_digit())
    {
        return Some((indent, bullet_text));
    }
    // Fallback: derive from flattened text if span structure is unexpected.
    // This guards against upstream changes that merge or split the bullet/space spans.
    let flat: String = line
        .spans
        .iter()
        .map(|s| s.content.as_ref())
        .collect();
    let mut chars = flat.chars().peekable();
    let mut indent_count = 0usize;
    while matches!(chars.peek(), Some(' ')) { chars.next(); indent_count += 1; }
    // Capture token up to first whitespace
    let mut token = String::new();
    while let Some(&ch) = chars.peek() {
        if ch.is_whitespace() { break; }
        token.push(ch);
        chars.next();
        // Limit token length to avoid scanning entire lines on odd inputs
        if token.len() > 8 { break; }
    }
    // Require at least one whitespace after the token
    let has_space = matches!(chars.peek(), Some(c) if c.is_whitespace());
    if has_space {
        let bullets = ["-", "•", "◦", "·", "∘", "⋅", "☐", "✔"]; // same set
        if bullets.contains(&token.as_str())
            || (token.len() >= 2
                && token.ends_with('.')
                && token[..token.len()-1].chars().all(|c| c.is_ascii_digit()))
        {
            return Some((indent_count, token));
        }
    }
    None
}


// Wrap a bullet line with a hanging indent so wrapped lines align under the content start.
fn wrap_bullet_line(
    mut line: ratatui::text::Line<'static>,
    indent_spaces: usize,
    bullet: &str,
    width: u16,
) -> Vec<ratatui::text::Line<'static>> {
    use ratatui::style::Style;
    use ratatui::text::Span;
    use unicode_width::UnicodeWidthStr as UWStr;

    // Apply a 1-col safety margin to reduce secondary wraps from Paragraph,
    // which can occur due to terminal-specific width differences (e.g.,
    // ambiguous-width glyphs, grapheme clusters). This keeps our prewrapped
    // bullet lines comfortably within the final render width.
    let width = width.saturating_sub(1) as usize;
    let mut spans = std::mem::take(&mut line.spans);
    // If the line contains OSC 8 hyperlinks (ESC ]8), avoid character-level
    // rewrapping to prevent breaking escape sequences. Fall back to default
    // Paragraph wrapping for this line by returning it unchanged.
    if spans.iter().any(|s| s.content.as_ref().contains('\u{1b}')) {
        line.spans = spans;
        return vec![line];
    }
    let mut i = 0usize;
    // Consume leading spaces span
    if i < spans.len() {
        let t = spans[i].content.as_ref();
        if t.chars().all(|c| c == ' ') {
            i += 1;
        }
    }
    // Consume bullet span and optional following single-space span. Support
    // cases where the bullet span already contains a trailing space (e.g., "☐ ").
    let bullet_style = if i < spans.len() { spans[i].style } else { Style::default() };
    if i < spans.len() {
        let bullet_span_text = spans[i].content.as_ref().to_string();
        i += 1; // consume bullet span
        if !bullet_span_text.ends_with(' ')
            && i < spans.len()
            && spans[i].content.as_ref() == " "
        {
            i += 1; // consume separate following space span
        }
    }

    // Remaining spans comprise the content; build grapheme clusters with style
    use unicode_segmentation::UnicodeSegmentation;
    let rest_spans = spans.drain(i..).collect::<Vec<_>>();
    let mut clusters: Vec<(String, Style)> = Vec::new();
    for sp in &rest_spans {
        let st = sp.style;
        for g in sp.content.as_ref().graphemes(true) {
            clusters.push((g.to_string(), st));
        }
    }

    // Some renderers may leave extra literal spaces between the bullet and the
    // first non-space glyph as part of the content (instead of a distinct
    // single-space span). Detect and incorporate those spaces into the hanging
    // indent, then drop them from the visible content so continuation lines
    // align perfectly under the start of the sentence.
    let mut leading_content_spaces: usize = 0;
    while leading_content_spaces < clusters.len()
        && (clusters[leading_content_spaces].0 == " " || clusters[leading_content_spaces].0 == "\u{3000}")
    {
        leading_content_spaces += 1;
    }

    // Prefix widths (display columns)
    let bullet_cols = UWStr::width(bullet);
    // Use a two-space gap after the bullet for better legibility and to keep
    // continuation lines aligned with the start of the bullet content. This
    // matches typical Markdown list rendering expectations in terminals.
    let gap_after_bullet = 2usize;
    let extra_gap = leading_content_spaces; // absorb any extra content-leading spaces
    let first_prefix = indent_spaces + bullet_cols + gap_after_bullet + extra_gap;
    let cont_prefix = indent_spaces + bullet_cols + gap_after_bullet + extra_gap; // keep continuation aligned

    let mut out: Vec<ratatui::text::Line<'static>> = Vec::new();
    let mut pos = leading_content_spaces;
    let mut first = true;
    while pos < clusters.len() {
        let avail_cols = if first {
            width.saturating_sub(first_prefix)
        } else {
            width.saturating_sub(cont_prefix)
        } as usize;
        let avail_cols = avail_cols.max(1);

        // Greedy take up to avail_cols, preferring to break at a preceding space cluster.
        let mut taken = 0usize; // number of clusters consumed
        let mut cols = 0usize; // display columns consumed
        let mut last_space_idx: Option<usize> = None; // index into clusters
        while pos + taken < clusters.len() {
            let (ref g, _) = clusters[pos + taken];
            let w = UWStr::width(g.as_str());
            if cols.saturating_add(w) > avail_cols {
                break;
            }
            cols += w;
            if g == " " || g == "\u{3000}" { last_space_idx = Some(pos + taken); }
            taken += 1;
            if cols == avail_cols {
                break;
            }
        }

        // Choose cut position:
        // - If the entire remaining content fits into this visual line, do NOT
        //   split at the last space — keep the final word on this line.
        // - Otherwise, prefer breaking at the last space within range; fall back
        //   to a hard break when no space is present (e.g., a long token).
        let (cut_end, next_start) = if pos + taken >= clusters.len() {
            (pos + taken, pos + taken)
        } else if let Some(space_idx) = last_space_idx {
            // Trim any spaces following the break point for next line start
            let mut next = space_idx;
            // cut_end excludes the space
            let mut cut = space_idx;
            // Also trim any trailing spaces before the break in this segment
            while cut > pos && clusters[cut - 1].0 == " " {
                cut -= 1;
            }
            // Advance next past contiguous spaces
            while next < clusters.len() && clusters[next].0 == " " {
                next += 1;
            }
            (cut, next)
        } else {
            // No space seen in range – hard break (very long word or first token longer than width)
            (pos + taken, pos + taken)
        };

        // If cut_end did not advance (e.g., segment starts with spaces), skip spaces and continue
        if cut_end <= pos {
            let mut p = pos;
            while p < clusters.len() && clusters[p].0 == " " {
                p += 1;
            }
            if p == pos {
                // safety: ensure forward progress
                p = pos + 1;
            }
            pos = p;
            continue;
        }

        let slice = &clusters[pos..cut_end];
        let mut seg_spans: Vec<Span<'static>> = Vec::new();
        // Build prefix spans
        if first {
            if indent_spaces > 0 {
                seg_spans.push(Span::raw(" ".repeat(indent_spaces)));
            }
            seg_spans.push(Span::styled(bullet.to_string(), bullet_style));
            // Two-space gap after bullet for readability and hanging indent
            seg_spans.push(Span::raw("  "));
        } else {
            seg_spans.push(Span::raw(" ".repeat(cont_prefix)));
        }
        // Build content spans coalescing same-style runs
        let mut cur_style = None::<Style>;
        let mut buf = String::new();
        for (g, st) in slice.iter() {
            if cur_style.map(|cs| cs == *st).unwrap_or(false) {
                buf.push_str(g);
            } else {
                if !buf.is_empty() { seg_spans.push(Span::styled(std::mem::take(&mut buf), cur_style.unwrap())); }
                cur_style = Some(*st);
                buf.push_str(g);
            }
        }
        if !buf.is_empty() { seg_spans.push(Span::styled(buf, cur_style.unwrap())); }
        out.push(ratatui::text::Line::from(seg_spans));
        pos = next_start;
        first = false;
    }

    if out.is_empty() {
        // Ensure at least prefix-only line (edge case empty content)
        let mut seg_spans: Vec<Span<'static>> = Vec::new();
        if indent_spaces > 0 {
            seg_spans.push(Span::raw(" ".repeat(indent_spaces)));
        }
        seg_spans.push(Span::styled(bullet.to_string(), bullet_style));
        out.push(ratatui::text::Line::from(seg_spans));
    }

    out
}

// Wrap a line with a hanging indent of `indent_spaces + hang_cols` columns, without
// rendering a bullet glyph. This is used for the special case where we suppress the
// initial "-" bullet on the first assistant line, but still want continuation lines
// to align under where the content would begin (i.e., as if there were a bullet +
// two-space gap).

fn is_horizontal_rule_line(line: &ratatui::text::Line<'_>) -> bool {
    let text: String = line.spans.iter().map(|s| s.content.as_ref()).collect();
    let t = text.trim();
    if t.is_empty() {
        return false;
    }
    let chars: Vec<char> = t.chars().collect();
    // Allow optional spaces between characters
    let only = |ch: char| chars.iter().all(|c| *c == ch || c.is_whitespace());
    (only('-') && chars.iter().filter(|c| **c == '-').count() >= 3)
        || (only('*') && chars.iter().filter(|c| **c == '*').count() >= 3)
        || (only('_') && chars.iter().filter(|c| **c == '_').count() >= 3)
}

// Bold the first sentence (up to the first '.', '!' or '?' in the first non-empty line),
// or the entire first non-empty line if no terminator is present. Newlines already split lines.
// removed bold_first_sentence; renderer handles first sentence styling
/*
fn bold_first_sentence(mut lines: Vec<Line<'static>>) -> Vec<Line<'static>> {
    use ratatui::text::Span;
    use ratatui::style::Modifier;

    // Find the first non-empty line index
    let first_idx = match lines.iter().position(|l| {
        let s: String = l.spans.iter().map(|sp| sp.content.as_ref()).collect();
        !s.trim().is_empty()
    }) {
        Some(i) => i,
        None => return lines,
    };

    // Build the plain text of that line
    let line_text: String = lines[first_idx]
        .spans
        .iter()
        .map(|sp| sp.content.as_ref())
        .collect();

    // If the first non-space character is a bullet (•), do not bold.
    if line_text.chars().skip_while(|c| c.is_whitespace()).next() == Some('•') {
        return lines;
    }

    // Heuristic: pick first sentence terminator that is not part of a filename or common
    // abbreviation (e.g., "e.g.", "i.e."). Treat '.', '!' or '?' as terminators when
    // followed by whitespace/end or a closing quote then whitespace/end. Skip when the
    // next character is a letter/number (e.g., within filenames like example.sh).
    let mut boundary: Option<usize> = None; // char index inclusive
    let chars: Vec<char> = line_text.chars().collect();
    let len_chars = chars.len();
    for i in 0..len_chars {
        let ch = chars[i];
        if ch == '.' || ch == '!' || ch == '?' || ch == ':' {
            let next = chars.get(i + 1).copied();
            // Skip if next is alphanumeric (likely filename/identifier like example.sh)
            if matches!(next, Some(c) if c.is_ascii_alphanumeric()) { continue; }
            // Skip common abbreviation endings like "e.g." or "i.e." (match last 4 chars)
            if i >= 3 {
                let tail: String = chars[i - 3..=i].iter().collect::<String>().to_lowercase();
                if tail == "e.g." || tail == "i.e." { continue; }
            }
            // Accept if end of line,
            // or next is whitespace,
            // or next is quote then whitespace/end
            let ok = match next {
                None => true,
                Some(c) if c.is_whitespace() => true,
                Some('"') | Some('\'') => {
                    let n2 = chars.get(i + 2).copied();
                    n2.is_none() || n2.map(|c| c.is_whitespace()).unwrap_or(false)
                }
                _ => false,
            };
            if ok { boundary = Some(i); break; }
        }
    }

    // Bold up to and including the terminator.
    let bold_upto = boundary.map(|i| i + 1);

    // If there's no terminator or there's no additional content after it in the message,
    // do not bold (single-sentence message).
    if let Some(limit) = bold_upto {
        let mut has_more_in_line = false;
        // allow trailing quote right after terminator
        let mut idx = limit;
        if let Some('"') | Some('\'') = chars.get(idx) { idx += 1; }
        if idx < len_chars {
            has_more_in_line = chars[idx..].iter().any(|c| !c.is_whitespace());
        }
        let has_more_below = if !has_more_in_line {
            lines.iter().skip(first_idx + 1).any(|l| {
                let s: String = l.spans.iter().map(|sp| sp.content.as_ref()).collect();
                !s.trim().is_empty()
            })
        } else { true };
        if !has_more_below {
            return lines; // single-sentence message: leave as-is
        }
    } else {
        // No terminator at all → treat as single sentence; leave as-is
        return lines;
    }

    // Rebuild spans for the line with bold applied up to bold_upto (in chars)
    let mut new_spans: Vec<Span<'static>> = Vec::new();
    let mut consumed_chars: usize = 0;
    for sp in lines[first_idx].spans.drain(..) {
        let content = sp.content.into_owned();
        let len = content.chars().count();
        if bold_upto.is_none() {
            // Entire line bold
            let mut st = sp.style;
            st.add_modifier.insert(Modifier::BOLD);
            st.fg = Some(crate::colors::text_bright());
            new_spans.push(Span::styled(content, st));
            consumed_chars += len;
            continue;
        }
        let limit = bold_upto.unwrap();
        if consumed_chars >= limit {
            // After bold range, preserve original styling (do not strip bold)
            new_spans.push(Span::styled(content, sp.style));
            consumed_chars += len;
        } else if consumed_chars + len <= limit {
            // Entire span within bold range
            let mut st = sp.style;
            st.add_modifier.insert(Modifier::BOLD);
            st.fg = Some(crate::colors::text_bright());
            new_spans.push(Span::styled(content, st));
            consumed_chars += len;
        } else {
            // Split this span at the boundary
            let split_at = limit - consumed_chars; // chars into this span
            let mut iter = content.chars();
            let bold_part: String = iter.by_ref().take(split_at).collect();
            let rest_part: String = iter.collect();
            let mut bold_style = sp.style;
            bold_style.add_modifier.insert(Modifier::BOLD);
            bold_style.fg = Some(crate::colors::text_bright());
            if !bold_part.is_empty() { new_spans.push(Span::styled(bold_part, bold_style)); }
            if !rest_part.is_empty() { new_spans.push(Span::styled(rest_part, sp.style)); }
            consumed_chars += len;
        }
    }
    lines[first_idx].spans = new_spans;

    // Recolor markdown bullet glyphs inside assistant content to text_dim.
    // Applies to common unordered bullets produced by our renderer: •, ◦, ·, ∘, ⋅
    let bullet_set: [&str; 5] = ["•", "◦", "·", "∘", "⋅"];
    for line in lines.iter_mut() {
        let mut updated: Vec<Span<'static>> = Vec::with_capacity(line.spans.len());
        for sp in line.spans.drain(..) {
            let content_ref = sp.content.as_ref();
            if bullet_set.contains(&content_ref) {
                let mut st = sp.style;
                st.fg = Some(crate::colors::text_dim());
                updated.push(Span::styled(sp.content, st));
            } else {
                updated.push(sp);
            }
        }
        line.spans = updated;
    }

    lines
}
*/

// ==================== Helper Functions ====================

// Unified preview format: show first 2 and last 5 non-empty lines with an ellipsis between.
const PREVIEW_HEAD_LINES: usize = 2;
const PREVIEW_TAIL_LINES: usize = 5;

/// Normalize common TTY overwrite sequences within a text block so that
/// progress lines using carriage returns, backspaces, or ESC[K erase behave as
/// expected when rendered in a pure-buffered UI (no cursor movement).
fn normalize_overwrite_sequences(input: &str) -> String {
    // Process per line, but keep CR/BS/CSI semantics within logical lines.
    // Treat "\n" as committing a line and resetting the cursor.
    let mut out = String::with_capacity(input.len());
    let mut line: Vec<char> = Vec::new(); // visible chars only
    let mut cursor: usize = 0; // column in visible chars

    // Helper to flush current line to out
    let flush_line = |line: &mut Vec<char>, cursor: &mut usize, out: &mut String| {
        if !line.is_empty() {
            out.push_str(&line.iter().collect::<String>());
        }
        out.push('\n');
        line.clear();
        *cursor = 0;
    };

    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        let ch = chars[i];
        match ch {
            '\n' => {
                flush_line(&mut line, &mut cursor, &mut out);
                i += 1;
            }
            '\r' => {
                // Carriage return: move cursor to column 0
                cursor = 0;
                i += 1;
            }
            '\u{0008}' => {
                // Backspace: move left one column if possible
                if cursor > 0 {
                    cursor -= 1;
                }
                i += 1;
            }
            '\u{001B}' => {
                // CSI: ESC [ ... <cmd>
                if i + 1 < chars.len() && chars[i + 1] == '[' {
                    // Find final byte (alphabetic)
                    let mut j = i + 2;
                    while j < chars.len() && !chars[j].is_alphabetic() {
                        j += 1;
                    }
                    if j < chars.len() {
                        let cmd = chars[j];
                        // Extract numeric prefix (first parameter only)
                        let num: usize = chars[i + 2..j]
                            .iter()
                            .take_while(|c| c.is_ascii_digit())
                            .collect::<String>()
                            .parse()
                            .unwrap_or(0);

                        match cmd {
                            // Erase in Line: 0/None = cursor..end, 1 = start..cursor, 2 = entire line
                            'K' => {
                                let n = num; // default 0 when absent
                                match n {
                                    0 => {
                                        if cursor < line.len() {
                                            line.truncate(cursor);
                                        }
                                    }
                                    1 => {
                                        // Replace from start to cursor with spaces to keep remaining columns stable
                                        let end = cursor.min(line.len());
                                        for k in 0..end {
                                            line[k] = ' ';
                                        }
                                        // Trim leading spaces if the whole line became spaces
                                        while line.last().map_or(false, |c| *c == ' ') {
                                            line.pop();
                                        }
                                    }
                                    2 => {
                                        line.clear();
                                        cursor = 0;
                                    }
                                    _ => {}
                                }
                                i = j + 1;
                                continue;
                            }
                            // Cursor horizontal absolute (1-based)
                            'G' => {
                                let pos = num.saturating_sub(1);
                                cursor = pos.min(line.len());
                                i = j + 1;
                                continue;
                            }
                            // Cursor forward/backward
                            'C' => {
                                cursor = cursor.saturating_add(num);
                                i = j + 1;
                                continue;
                            }
                            'D' => {
                                cursor = cursor.saturating_sub(num);
                                i = j + 1;
                                continue;
                            }
                            _ => {
                                // Unknown/unsupported CSI (incl. SGR 'm'): keep styling intact by
                                // copying the entire sequence verbatim into the output so ANSI
                                // parsing can apply later, but do not affect cursor position.
                                // First, splice current visible buffer into out to preserve order
                                if !line.is_empty() {
                                    out.push_str(&line.iter().collect::<String>());
                                    line.clear();
                                    cursor = 0;
                                }
                                for k in i..=j {
                                    out.push(chars[k]);
                                }
                                i = j + 1;
                                continue;
                            }
                        }
                    } else {
                        // Malformed CSI: drop it entirely by exiting the loop
                        break;
                    }
                } else {
                    // Other ESC sequences (e.g., OSC): pass through verbatim without affecting cursor
                    // Copy ESC and advance one; do not attempt to parse full OSC payload here.
                    if !line.is_empty() {
                        out.push_str(&line.iter().collect::<String>());
                        line.clear();
                        cursor = 0;
                    }
                    out.push(ch);
                    i += 1;
                }
            }
            _ => {
                // Put visible char at cursor, expanding with spaces if needed
                if cursor < line.len() {
                    line[cursor] = ch;
                } else {
                    while line.len() < cursor {
                        line.push(' ');
                    }
                    line.push(ch);
                }
                cursor += 1;
                i += 1;
            }
        }
    }
    // Flush any remaining visible text
    if !line.is_empty() {
        out.push_str(&line.iter().collect::<String>());
    }
    out
}

fn build_preview_lines(text: &str, _include_left_pipe: bool) -> Vec<Line<'static>> {
    let processed = format_json_compact(text).unwrap_or_else(|| text.to_string());
    let processed = normalize_overwrite_sequences(&processed);
    let processed = expand_tabs_to_spaces(&processed, 4);
    let non_empty: Vec<&str> = processed.lines().filter(|line| !line.is_empty()).collect();

    enum Seg<'a> {
        Line(&'a str),
        Ellipsis,
    }
    let segments: Vec<Seg> = if non_empty.len() <= PREVIEW_HEAD_LINES + PREVIEW_TAIL_LINES {
        non_empty.iter().map(|s| Seg::Line(s)).collect()
    } else {
        let mut v: Vec<Seg> = Vec::with_capacity(PREVIEW_HEAD_LINES + PREVIEW_TAIL_LINES + 1);
        // Head
        for i in 0..PREVIEW_HEAD_LINES {
            v.push(Seg::Line(non_empty[i]));
        }
        v.push(Seg::Ellipsis);
        // Tail
        let start = non_empty.len().saturating_sub(PREVIEW_TAIL_LINES);
        for s in &non_empty[start..] {
            v.push(Seg::Line(s));
        }
        v
    };

    let mut out: Vec<Line<'static>> = Vec::new();
    for seg in segments {
        match seg {
            Seg::Line(line) => {
                // Do not draw manual borders; the caller wraps output in a Block
                // with a left border and padding. Just emit the content line.
                out.push(ansi_escape_line(line));
            }
            Seg::Ellipsis => {
                // Use dots for truncation marker; border comes from Block
                out.push(Line::from("⋮".dim()));
            }
        }
    }
    out
}

fn output_lines(
    output: Option<&CommandOutput>,
    only_err: bool,
    include_angle_pipe: bool,
) -> Vec<Line<'static>> {
    let CommandOutput {
        exit_code,
        stdout,
        stderr,
    } = match output {
        Some(o) => o,
        None => return Vec::new(),
    };

    let mut lines: Vec<Line<'static>> = Vec::new();

    if !only_err && !stdout.is_empty() {
        lines.extend(build_preview_lines(stdout, include_angle_pipe));
    }

    if !stderr.is_empty() && *exit_code != 0 {
        if !lines.is_empty() {
            lines.push(Line::from(""));
        }
        lines.push(Line::styled(
            format!("Error (exit code {})", exit_code),
            Style::default().fg(crate::colors::error()),
        ));
        let stderr_norm = expand_tabs_to_spaces(&normalize_overwrite_sequences(stderr), 4);
        for line in stderr_norm.lines().filter(|line| !line.is_empty()) {
            lines.push(ansi_escape_line(line).style(Style::default().fg(crate::colors::error())));
        }
    }

    if !lines.is_empty() {
        lines.push(Line::from(""));
    }

    lines
}

fn format_mcp_invocation(invocation: McpInvocation) -> Line<'static> {
    let provider_name = pretty_provider_name(&invocation.server);
    let invocation_str = if let Some(args) = invocation.arguments {
        format!("{}.{}({})", provider_name, invocation.tool, args)
    } else {
        format!("{}.{}()", provider_name, invocation.tool)
    };

    Line::styled(
        invocation_str,
        Style::default()
            .fg(crate::colors::text_dim())
            .add_modifier(Modifier::ITALIC),
    )
}

fn pretty_provider_name(id: &str) -> String {
    // Special case common providers with human-friendly names
    match id {
        "brave-search" => "brave",
        "screenshot-website-fast" => "screenshot",
        "read-website-fast" => "readweb",
        "sequential-thinking" => "think",
        "discord-bot" => "discord",
        _ => id,
    }
    .to_string()
}

// ==================== Factory Functions ====================

pub(crate) fn new_background_event(message: String) -> PlainHistoryCell {
    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(Line::from("event".dim()));
    let msg_norm = normalize_overwrite_sequences(&message);
    lines.extend(msg_norm.lines().map(|line| ansi_escape_line(line).dim()));
    // No empty line at end - trimming and spacing handled by renderer
    PlainHistoryCell {
        lines,
        kind: HistoryCellType::BackgroundEvent,
    }
}

pub(crate) fn new_session_info(
    config: &Config,
    event: SessionConfiguredEvent,
    is_first_event: bool,
) -> PlainHistoryCell {
    let SessionConfiguredEvent {
        model,
        session_id: _,
        history_log_id: _,
        history_entry_count: _,
    } = event;

    if is_first_event {
        let lines: Vec<Line<'static>> = vec![
            Line::from("notice".dim()),
            Line::styled(
                "Popular commands:",
                Style::default().fg(crate::colors::text_bright()),
            ),
            Line::from(vec![
                Span::styled("/chrome", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Chrome.description())
                    .style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled(
                    "/browser <url>",
                    Style::default().fg(crate::colors::primary()),
                ),
                Span::from(" - "),
                Span::from(SlashCommand::Browser.description())
                    .style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/plan", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Plan.description())
                    .style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/solve", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Solve.description())
                    .style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/code", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Code.description())
                    .style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/reasoning", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Reasoning.description())
                    .style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/resume", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Resume.description())
                    .style(Style::default().add_modifier(Modifier::DIM)),
            ]),
        ];
        PlainHistoryCell {
            lines,
            kind: HistoryCellType::Notice,
        }
    } else if config.model == model {
        PlainHistoryCell {
            lines: Vec::new(),
            kind: HistoryCellType::Notice,
        }
    } else {
        let lines = vec![
            Line::from("model changed:".magenta().bold()),
            Line::from(format!("requested: {}", config.model)),
            Line::from(format!("used: {model}")),
            // No empty line at end - trimming and spacing handled by renderer
        ];
        PlainHistoryCell {
            lines,
            kind: HistoryCellType::Notice,
        }
    }
}

pub(crate) fn new_user_prompt(message: String) -> PlainHistoryCell {
    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(Line::from("user"));
    // Sanitize user-provided text for terminal safety and stable layout:
    // - Normalize common TTY overwrite sequences (\r, \x08, ESC[K)
    // - Expand tabs to spaces with a fixed tab stop so wrapping is deterministic
    // - Parse ANSI sequences into spans so we never emit raw control bytes
    let normalized = normalize_overwrite_sequences(&message);
    let expanded = expand_tabs_to_spaces(&normalized, 4);
    // Build content lines with ANSI converted to styled spans
    let content: Vec<Line<'static>> = expanded.lines().map(|l| ansi_escape_line(l)).collect();
    let content = trim_empty_lines(content);
    lines.extend(content);
    // No empty line at end - trimming and spacing handled by renderer
    PlainHistoryCell {
        lines,
        kind: HistoryCellType::User,
    }
}

/// Expand horizontal tabs to spaces using a fixed tab stop.
/// This prevents terminals from applying their own tab expansion after
/// ratatui has computed layout, which can otherwise cause glyphs to appear
/// to "hang" or smear until overwritten.
fn expand_tabs_to_spaces(input: &str, tabstop: usize) -> String {
    let ts = tabstop.max(1);
    let mut out = String::with_capacity(input.len());
    for line in input.split_inclusive('\n') {
        let mut col = 0usize; // display columns in this logical line
        for ch in line.chars() {
            match ch {
                '\t' => {
                    let spaces = ts - (col % ts);
                    out.extend(std::iter::repeat(' ').take(spaces));
                    col += spaces;
                }
                '\n' => {
                    out.push('\n');
                    col = 0;
                }
                _ => {
                    out.push(ch);
                    // Use Unicode width to advance columns for wide glyphs
                    col += UnicodeWidthChar::width(ch).unwrap_or(1).max(1);
                }
            }
        }
        // If the line did not end with a newline, `split_inclusive` ensures we
        // won't add one here (preserve exact trailing newline semantics).
    }
    out
}

#[allow(dead_code)]
pub(crate) fn new_text_line(line: Line<'static>) -> PlainHistoryCell {
    PlainHistoryCell {
        lines: vec![line],
        kind: HistoryCellType::Notice,
    }
}

pub(crate) fn new_streaming_content(lines: Vec<Line<'static>>) -> StreamingContentCell {
    StreamingContentCell { id: None, lines }
}

pub(crate) fn new_streaming_content_with_id(
    id: Option<String>,
    lines: Vec<Line<'static>>,
) -> StreamingContentCell {
    StreamingContentCell { id, lines }
}

pub(crate) fn new_animated_welcome() -> AnimatedWelcomeCell {
    AnimatedWelcomeCell {
        start_time: Instant::now(),
        completed: std::cell::Cell::new(false),
        fade_start: std::cell::Cell::new(None),
        faded_out: std::cell::Cell::new(false),
        locked_height: std::cell::Cell::new(None),
        hidden: std::cell::Cell::new(false),
    }
}

#[allow(dead_code)]
pub(crate) fn new_loading_cell(message: String) -> LoadingCell {
    LoadingCell { message }
}

pub(crate) fn new_active_exec_command(
    command: Vec<String>,
    parsed: Vec<ParsedCommand>,
) -> ExecCell {
    new_exec_cell(command, parsed, None)
}

pub(crate) fn new_completed_exec_command(
    command: Vec<String>,
    parsed: Vec<ParsedCommand>,
    output: CommandOutput,
) -> ExecCell {
    new_exec_cell(command, parsed, Some(output))
}

fn new_exec_cell(
    command: Vec<String>,
    parsed: Vec<ParsedCommand>,
    output: Option<CommandOutput>,
) -> ExecCell {
    let start_time = if output.is_none() {
        Some(Instant::now())
    } else {
        None
    };
    ExecCell {
        command,
        parsed,
        output,
        start_time,
        cached_display_lines: std::cell::RefCell::new(None),
        cached_pre_lines: std::cell::RefCell::new(None),
        cached_out_lines: std::cell::RefCell::new(None),
    }
}

fn exec_command_lines(
    command: &[String],
    parsed: &[ParsedCommand],
    output: Option<&CommandOutput>,
    start_time: Option<Instant>,
) -> Vec<Line<'static>> {
    match parsed.is_empty() {
        true => new_exec_command_generic(command, output, start_time),
        false => new_parsed_command(parsed, output, start_time),
    }
}

pub(crate) fn action_from_parsed(parsed_commands: &[ParsedCommand]) -> &'static str {
    for parsed in parsed_commands.iter() {
        match parsed {
            ParsedCommand::Search { .. } => return "search",
            ParsedCommand::Read { .. } => return "read",
            ParsedCommand::ListFiles { .. } => return "list",
            ParsedCommand::Noop { .. } => continue,
            _ => return "run",
        }
    }
    "run"
}

fn first_context_path(parsed_commands: &[ParsedCommand]) -> Option<String> {
    for parsed in parsed_commands.iter() {
        match parsed {
            ParsedCommand::ListFiles { path, .. } => {
                if let Some(p) = path {
                    return Some(p.clone());
                }
            }
            ParsedCommand::Search { path, .. } => {
                if let Some(p) = path {
                    return Some(p.clone());
                }
            }
            _ => {}
        }
    }
    None
}

fn parse_read_line_annotation(cmd: &str) -> Option<String> {
    let lower = cmd.to_lowercase();
    // Try sed -n '<start>,<end>p'
    if lower.contains("sed") && lower.contains("-n") {
        // Look for a token like 123,456p possibly quoted
        for raw in cmd.split(|c: char| c.is_whitespace() || c == '"' || c == '\'') {
            let token = raw.trim();
            if token.ends_with('p') {
                let core = &token[..token.len().saturating_sub(1)];
                if let Some((a, b)) = core.split_once(',') {
                    if let (Ok(start), Ok(end)) = (a.trim().parse::<u32>(), b.trim().parse::<u32>())
                    {
                        return Some(format!("(lines {} to {})", start, end));
                    }
                }
            }
        }
    }
    // head -n N => lines 1..N
    if lower.contains("head") && lower.contains("-n") {
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        for i in 0..parts.len() {
            if parts[i] == "-n" && i + 1 < parts.len() {
                if let Ok(n) = parts[i + 1]
                    .trim_matches('"')
                    .trim_matches('\'')
                    .parse::<u32>()
                {
                    return Some(format!("(lines 1 to {})", n));
                }
            }
        }
    }
    // tail -n +K => from K to end; tail -n N => last N lines
    if lower.contains("tail") && lower.contains("-n") {
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        for i in 0..parts.len() {
            if parts[i] == "-n" && i + 1 < parts.len() {
                let val = parts[i + 1].trim_matches('"').trim_matches('\'');
                if let Some(rest) = val.strip_prefix('+') {
                    if let Ok(k) = rest.parse::<u32>() {
                        return Some(format!("(from {} to end)", k));
                    }
                } else if let Ok(n) = val.parse::<u32>() {
                    return Some(format!("(last {} lines)", n));
                }
            }
        }
    }
    None
}

fn new_parsed_command(
    parsed_commands: &[ParsedCommand],
    output: Option<&CommandOutput>,
    start_time: Option<Instant>,
) -> Vec<Line<'static>> {
    let action = action_from_parsed(parsed_commands);
    let ctx_path = first_context_path(parsed_commands);
    let mut lines: Vec<Line> = vec![match output {
        None => {
            let duration_str = if let Some(start) = start_time {
                let elapsed = start.elapsed();
                format!(" ({})", format_duration(elapsed))
            } else {
                String::new()
            };
            // Running state per action
            let header = match action {
                "read" => "Read".to_string(),
                "search" => "Searched".to_string(),
                "list" => "List Files".to_string(),
                _ => match &ctx_path {
                    Some(p) => format!("Running... in {p}"),
                    None => "Running...".to_string(),
                },
            };
            // Use non-bold styling for informational actions; use info color
            if matches!(action, "read" | "search" | "list") {
                Line::styled(
                    format!("{header}{duration_str}"),
                    Style::default().fg(crate::colors::info()),
                )
            } else {
                Line::styled(
                    format!("{header}{duration_str}"),
                    Style::default()
                        .fg(crate::colors::info())
                        .add_modifier(Modifier::BOLD),
                )
            }
        }
        Some(o) if o.exit_code == 0 => {
            let done = match action {
                "read" => "Read".to_string(),
                "search" => "Searched".to_string(),
                "list" => "List Files".to_string(),
                _ => match &ctx_path {
                    Some(p) => format!("Ran in {p}"),
                    None => "Ran".to_string(),
                },
            };
            // Color by action: informational (Read/Search/List) use normal text; execution uses primary
            if matches!(action, "read" | "search" | "list") {
                Line::styled(done, Style::default().fg(crate::colors::text()))
            } else {
                Line::styled(
                    done,
                    Style::default()
                        .fg(crate::colors::text_bright())
                        .add_modifier(Modifier::BOLD),
                )
            }
        }
        Some(_o) => {
            // Preserve the action header (e.g., "Searched") on error so users
            // can still see what operation was attempted. Error details are
            // rendered below via `output_lines`.
            let done = match action {
                "read" => "Read".to_string(),
                "search" => "Searched".to_string(),
                "list" => "List Files".to_string(),
                _ => match &ctx_path {
                    Some(p) => format!("Ran in {p}"),
                    None => "Ran".to_string(),
                },
            };
            // Use the same styling as success to keep headers stable/recognizable.
            if matches!(action, "read" | "search" | "list") {
                Line::styled(done, Style::default().fg(crate::colors::text()))
            } else {
                Line::styled(
                    done,
                    Style::default()
                        .fg(crate::colors::text_bright())
                        .add_modifier(Modifier::BOLD),
                )
            }
        }
    }];

    // Collect any paths referenced by search commands to suppress redundant directory lines
    let mut search_paths: std::collections::HashSet<String> = std::collections::HashSet::new();
    for pc in parsed_commands.iter() {
        if let ParsedCommand::Search { path: Some(p), .. } = pc {
            search_paths.insert(p.to_string());
        }
    }

    // We'll emit only content lines here; the header above already communicates the action.
    // Use a single leading "└ " for the very first content line, then indent subsequent ones.
    let mut any_content_emitted = false;

    // Restrict displayed entries to the primary action for this cell.
    // For the generic "run" header, allow Run/Test/Lint/Format entries.
    let expected_label: Option<&'static str> = match action {
        "read" => Some("Read"),
        "search" => Some("Search"),
        "list" => Some("List Files"),
        _ => None,
    };

    for parsed in parsed_commands.iter() {
        // Produce a logical label and content string without icons
        let (label, content) = match parsed {
            ParsedCommand::Read { name, cmd, .. } => {
                let mut c = name.clone();
                if let Some(ann) = parse_read_line_annotation(cmd) {
                    c = format!("{c} {ann}");
                }
                ("Read".to_string(), c)
            }
            ParsedCommand::ListFiles { cmd: _, path } => match path {
                Some(p) => {
                    if search_paths.contains(p) {
                        (String::new(), String::new()) // suppressed
                    } else {
                        let display_p = if p.ends_with('/') {
                            p.to_string()
                        } else {
                            format!("{p}/")
                        };
                        ("List Files".to_string(), format!("in {display_p}"))
                    }
                }
                None => ("List Files".to_string(), "in ./".to_string()),
            },
            ParsedCommand::Search { query, path, cmd } => {
                // Format query for display: unescape backslash-escapes and close common unbalanced delimiters
                let prettify_term = |s: &str| -> String {
                    // General unescape: turn "\X" into "X" for any X
                    let mut out = String::with_capacity(s.len());
                    let mut iter = s.chars();
                    while let Some(ch) = iter.next() {
                        if ch == '\\' {
                            if let Some(next) = iter.next() {
                                out.push(next);
                            } else {
                                out.push('\\');
                            }
                        } else {
                            out.push(ch);
                        }
                    }
                    // Balance parentheses
                    let opens_paren = out.matches("(").count();
                    let closes_paren = out.matches(")").count();
                    for _ in 0..opens_paren.saturating_sub(closes_paren) {
                        out.push(')');
                    }
                    // Balance curly braces
                    let opens_curly = out.matches("{").count();
                    let closes_curly = out.matches("}").count();
                    for _ in 0..opens_curly.saturating_sub(closes_curly) {
                        out.push('}');
                    }
                    out
                };
                let fmt_query = |q: &str| -> String {
                    let mut parts: Vec<String> = q
                        .split('|')
                        .map(|s| s.trim())
                        .filter(|s| !s.is_empty())
                        .map(prettify_term)
                        .collect();
                    match parts.len() {
                        0 => String::new(),
                        1 => parts.remove(0),
                        2 => format!("{} and {}", parts[0], parts[1]),
                        _ => {
                            let last = parts.last().cloned().unwrap_or_default();
                            let head = &parts[..parts.len() - 1];
                            format!("{} and {}", head.join(", "), last)
                        }
                    }
                };
                match (query, path) {
                    (Some(q), Some(p)) => {
                        let display_p = if p.ends_with('/') {
                            p.to_string()
                        } else {
                            format!("{p}/")
                        };
                        (
                            "Search".to_string(),
                            format!("{} in {}", fmt_query(q), display_p),
                        )
                    }
                    (Some(q), None) => ("Search".to_string(), format!("{}", fmt_query(q))),
                    (None, Some(p)) => {
                        let display_p = if p.ends_with('/') {
                            p.to_string()
                        } else {
                            format!("{p}/")
                        };
                        ("Search".to_string(), format!("in {}", display_p))
                    }
                    (None, None) => ("Search".to_string(), cmd.clone()),
                }
            }
            ParsedCommand::Format { .. } => ("Format".to_string(), String::new()),
            ParsedCommand::Test { cmd } => ("Test".to_string(), cmd.clone()),
            ParsedCommand::Lint { cmd, .. } => ("Lint".to_string(), cmd.clone()),
            ParsedCommand::Unknown { cmd } => {
                let t = cmd.trim();
                let lower = t.to_lowercase();
                if lower.starts_with("echo") && lower.contains("---") {
                    (String::new(), String::new())
                } else {
                    ("Run".to_string(), cmd.clone())
                }
            }
            ParsedCommand::Noop { .. } => continue,
        };

        // Keep only entries that match the primary action grouping.
        if let Some(exp) = expected_label {
            if label != exp {
                continue;
            }
        } else if !(label == "Run" || label == "Test" || label == "Lint" || label == "Format") {
            continue;
        }

        // Skip suppressed entries
        if label.is_empty() && content.is_empty() {
            continue;
        }

        // Split content into lines and push without repeating the action label
        for line_text in content.lines() {
            if line_text.is_empty() {
                continue;
            }
            let prefix = if !any_content_emitted { "└ " } else { "  " };
            let mut spans: Vec<Span<'static>> = vec![Span::styled(
                prefix,
                Style::default().add_modifier(Modifier::DIM),
            )];

            match label.as_str() {
                // Highlight searched terms in normal text color; keep connectors/path dim
                "Search" => {
                    let remaining = line_text.to_string();
                    // Split off optional path suffix. Support both " (in ...)" and " in <dir>/" forms.
                    let (terms_part, path_part) = if let Some(idx) = remaining.rfind(" (in ") {
                        (
                            remaining[..idx].to_string(),
                            Some(remaining[idx..].to_string()),
                        )
                    } else if let Some(idx) = remaining.rfind(" in ") {
                        let suffix = &remaining[idx + 1..]; // keep leading space for styling
                        // Heuristic: treat as path if it ends with '/'
                        if suffix.trim_end().ends_with('/') {
                            (
                                remaining[..idx].to_string(),
                                Some(remaining[idx..].to_string()),
                            )
                        } else {
                            (remaining.clone(), None)
                        }
                    } else {
                        (remaining.clone(), None)
                    };
                    // Tokenize terms by ", " and " and " while preserving separators
                    let tmp = terms_part.clone();
                    // First, split by ", "
                    let chunks: Vec<String> = if tmp.contains(", ") {
                        tmp.split(", ").map(|s| s.to_string()).collect()
                    } else {
                        vec![tmp.clone()]
                    };
                    for (i, chunk) in chunks.iter().enumerate() {
                        if i > 0 {
                            // Add comma separator between items (dim)
                            spans.push(Span::styled(
                                ", ",
                                Style::default().fg(crate::colors::text_dim()),
                            ));
                        }
                        // Within each chunk, if it contains " and ", split into left and right with dimmed " and "
                        if let Some((left, right)) = chunk.rsplit_once(" and ") {
                            if !left.is_empty() {
                                spans.push(Span::styled(
                                    left.to_string(),
                                    Style::default().fg(crate::colors::text()),
                                ));
                                spans.push(Span::styled(
                                    " and ",
                                    Style::default().fg(crate::colors::text_dim()),
                                ));
                                spans.push(Span::styled(
                                    right.to_string(),
                                    Style::default().fg(crate::colors::text()),
                                ));
                            } else {
                                spans.push(Span::styled(
                                    chunk.to_string(),
                                    Style::default().fg(crate::colors::text()),
                                ));
                            }
                        } else {
                            spans.push(Span::styled(
                                chunk.to_string(),
                                Style::default().fg(crate::colors::text()),
                            ));
                        }
                    }
                    if let Some(p) = path_part {
                        // Dim the entire path portion including the " in " or " (in " prefix
                        spans.push(Span::styled(
                            p,
                            Style::default().fg(crate::colors::text_dim()),
                        ));
                    }
                }
                // Highlight filenames in Read; keep line ranges dim
                "Read" => {
                    if let Some(idx) = line_text.find(" (") {
                        let (fname, rest) = line_text.split_at(idx);
                        spans.push(Span::styled(
                            fname.to_string(),
                            Style::default().fg(crate::colors::text()),
                        ));
                        spans.push(Span::styled(
                            rest.to_string(),
                            Style::default().fg(crate::colors::text_dim()),
                        ));
                    } else {
                        spans.push(Span::styled(
                            line_text.to_string(),
                            Style::default().fg(crate::colors::text()),
                        ));
                    }
                }
                // List Files: highlight directory names
                "List Files" => {
                    spans.push(Span::styled(
                        line_text.to_string(),
                        Style::default().fg(crate::colors::text()),
                    ));
                }
                _ => {
                    // For executed commands (Run/Test/Lint/etc.), use shell syntax highlighting.
                    let mut hl =
                        crate::syntax_highlight::highlight_code_block(line_text, Some("bash"));
                    if let Some(mut first) = hl.pop() {
                        // If the exec has completed ("Ran"), render command in a unified
                        // completed color to make the state change clear; otherwise keep
                        // full syntax highlighting while running.
                        if output.is_some() {
                            for s in first.spans.drain(..) {
                                spans.push(Span::styled(
                                    s.content.to_string(),
                                    Style::default().fg(crate::colors::text_bright()),
                                ));
                            }
                        } else {
                            spans.extend(first.spans.drain(..));
                        }
                    } else {
                        spans.push(Span::styled(
                            line_text.to_string(),
                            Style::default().fg(if output.is_some() {
                                crate::colors::text_bright()
                            } else {
                                crate::colors::text()
                            }),
                        ));
                    }
                }
            }

            lines.push(Line::from(spans));
            any_content_emitted = true;
        }
    }

    // Show stdout for real run commands; keep read/search/list concise unless error
    let show_stdout = action == "run";
    let use_angle_pipe = show_stdout; // add "> " prefix for run output
    lines.extend(output_lines(output, !show_stdout, use_angle_pipe));
    lines.push(Line::from(""));
    lines
}

fn new_exec_command_generic(
    command: &[String],
    output: Option<&CommandOutput>,
    start_time: Option<Instant>,
) -> Vec<Line<'static>> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    let command_escaped = strip_bash_lc_and_escape(command);
    // Highlight the command as bash and then append a dimmed duration to the
    // first visual line while running.
    let mut highlighted_cmd =
        crate::syntax_highlight::highlight_code_block(&command_escaped, Some("bash"));

    let header_line = match output {
        None => {
            let duration_str = if let Some(start) = start_time {
                let elapsed = start.elapsed();
                format!(" ({})", format_duration(elapsed))
            } else {
                String::new()
            };
            Line::styled(
                format!("Running...{duration_str}"),
                Style::default()
                    .fg(crate::colors::info())
                    .add_modifier(Modifier::BOLD),
            )
        }
        Some(o) if o.exit_code == 0 => Line::styled(
            "Ran",
            Style::default()
                .fg(crate::colors::text_bright())
                .add_modifier(Modifier::BOLD),
        ),
        Some(_o) => {
            // Preserve the header as "Ran" even on error; detailed error output
            // (including exit code and stderr) will be shown below by `output_lines`.
            Line::styled(
                "Ran",
                Style::default()
                    .fg(crate::colors::text_bright())
                    .add_modifier(Modifier::BOLD),
            )
        }
    };

    lines.push(header_line.clone());
    if let Some(first) = highlighted_cmd.first_mut() {
        if output.is_none() && start_time.is_some() {
            let elapsed = start_time.unwrap().elapsed();
            let duration_str = format!(" ({})", format_duration(elapsed));
            first.spans.push(Span::styled(
                duration_str,
                Style::default().fg(crate::colors::text_dim()),
            ));
        }
    }
    lines.extend(highlighted_cmd);

    lines.extend(output_lines(output, false, true));
    lines
}

#[allow(dead_code)]
pub(crate) fn new_active_mcp_tool_call(invocation: McpInvocation) -> ToolCallCell {
    let title_line = Line::styled("Working", Style::default().fg(crate::colors::info()));
    let lines: Vec<Line> = vec![
        title_line,
        format_mcp_invocation(invocation),
        Line::from(""),
    ];
    ToolCallCell {
        lines,
        state: ToolState::Running,
    }
}

#[allow(dead_code)]
pub(crate) fn new_active_custom_tool_call(tool_name: String, args: Option<String>) -> ToolCallCell {
    let title_line = Line::styled("Working", Style::default().fg(crate::colors::info()));
    let invocation_str = if let Some(args) = args {
        format!("{}({})", tool_name, args)
    } else {
        format!("{}()", tool_name)
    };

    let lines: Vec<Line> = vec![
        title_line,
        Line::styled(
            invocation_str,
            Style::default()
                .fg(crate::colors::text_dim())
                .add_modifier(Modifier::ITALIC),
        ),
        Line::from(""),
    ];
    ToolCallCell {
        lines,
        state: ToolState::Running,
    }
}

// Friendly present-participle titles for running browser tools
fn browser_running_title(tool_name: &str) -> &'static str {
    match tool_name {
        "browser_click" => "Clicking...",
        "browser_type" => "Typing...",
        "browser_key" => "Sending key...",
        "browser_javascript" => "Running JavaScript...",
        "browser_scroll" => "Scrolling...",
        "browser_open" => "Opening...",
        "browser_close" => "Closing...",
        "browser_status" => "Checking status...",
        "browser_history" => "Navigating...",
        "browser_inspect" => "Inspecting...",
        "browser_console" => "Reading console...",
        "browser_move" => "Moving...",
        _ => "Working...",
    }
}

pub(crate) fn new_running_browser_tool_call(
    tool_name: String,
    args: Option<String>,
) -> RunningToolCallCell {
    // Parse args JSON and use compact humanized form when possible
    let mut arg_lines: Vec<Line<'static>> = Vec::new();
    if let Some(args_str) = args {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(&args_str) {
            if let Some(lines) = format_browser_args_humanized(&tool_name, &json) {
                arg_lines.extend(lines);
            } else {
                arg_lines.extend(format_browser_args_line(&json));
            }
        }
    }
    RunningToolCallCell {
        title: browser_running_title(&tool_name).to_string(),
        start_time: Instant::now(),
        arg_lines,
    }
}

fn custom_tool_running_title(tool_name: &str) -> String {
    if tool_name.starts_with("agent_") {
        // Reuse agent title and append ellipsis
        format!("{}...", agent_tool_title(tool_name))
    } else if tool_name.starts_with("browser_") {
        browser_running_title(tool_name).to_string()
    } else {
        // TitleCase from snake_case and append ellipsis
        let pretty = tool_name
            .split('_')
            .filter(|s| !s.is_empty())
            .map(|s| {
                let mut chars = s.chars();
                match chars.next() {
                    Some(f) => format!("{}{}", f.to_uppercase(), chars.as_str()),
                    None => String::new(),
                }
            })
            .collect::<Vec<_>>()
            .join(" ");
        format!("{}...", pretty)
    }
}

pub(crate) fn new_running_custom_tool_call(
    tool_name: String,
    args: Option<String>,
) -> RunningToolCallCell {
    // Parse args JSON and format as key/value lines
    let mut arg_lines: Vec<Line<'static>> = Vec::new();
    if let Some(args_str) = args {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(&args_str) {
            arg_lines.extend(format_browser_args_line(&json));
        } else {
            // Fallback to showing raw args string
            arg_lines.push(Line::from(vec![
                Span::styled("└ args: ", Style::default().fg(crate::colors::text_dim())),
                Span::styled(args_str, Style::default().fg(crate::colors::text())),
            ]));
        }
    }
    RunningToolCallCell {
        title: custom_tool_running_title(&tool_name),
        start_time: Instant::now(),
        arg_lines,
    }
}

/// Running web search call (native Responses web_search)
pub(crate) fn new_running_web_search(query: Option<String>) -> RunningToolCallCell {
    let mut arg_lines: Vec<Line<'static>> = Vec::new();
    if let Some(q) = query {
        arg_lines.push(Line::from(vec![
            Span::styled("└ query: ", Style::default().fg(crate::colors::text_dim())),
            Span::styled(q, Style::default().fg(crate::colors::text())),
        ]));
    }
    RunningToolCallCell {
        title: "Web Search...".to_string(),
        start_time: Instant::now(),
        arg_lines,
    }
}

pub(crate) fn new_running_mcp_tool_call(invocation: McpInvocation) -> RunningToolCallCell {
    // Represent as provider.tool(...) on one dim line beneath a generic running header with timer
    let line = format_mcp_invocation(invocation);
    RunningToolCallCell {
        title: "Working...".to_string(),
        start_time: Instant::now(),
        arg_lines: vec![line],
    }
}

pub(crate) fn new_completed_custom_tool_call(
    tool_name: String,
    args: Option<String>,
    duration: Duration,
    success: bool,
    result: String,
) -> ToolCallCell {
    // Special rendering for browser_* tools
    if tool_name.starts_with("browser_") {
        return new_completed_browser_tool_call(tool_name, args, duration, success, result);
    }
    // Special rendering for agent_* tools
    if tool_name.starts_with("agent_") {
        return new_completed_agent_tool_call(tool_name, args, duration, success, result);
    }
    let duration = format_duration(duration);
    let status_str = if success { "Complete" } else { "Error" };
    let title_line = if success {
        Line::from(vec![
            Span::styled(status_str, Style::default().fg(crate::colors::success())),
            format!(", duration: {duration}").dim(),
        ])
    } else {
        Line::from(vec![
            Span::styled(status_str, Style::default().fg(crate::colors::error())),
            format!(", duration: {duration}").dim(),
        ])
    };

    let invocation_str = if let Some(args) = args {
        format!("{}({})", tool_name, args)
    } else {
        format!("{}()", tool_name)
    };

    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(title_line);
    lines.push(Line::styled(
        invocation_str,
        Style::default()
            .fg(crate::colors::text_dim())
            .add_modifier(Modifier::ITALIC),
    ));

    if !result.is_empty() {
        lines.push(Line::from(""));
        let mut preview = build_preview_lines(&result, true);
        preview = preview
            .into_iter()
            .map(|l| l.style(Style::default().fg(crate::colors::text_dim())))
            .collect();
        lines.extend(preview);
    }

    lines.push(Line::from(""));
    ToolCallCell {
        lines,
        state: if success {
            ToolState::Success
        } else {
            ToolState::Failed
        },
    }
}

// Map `browser_*` tool names to friendly titles
fn browser_tool_title(tool_name: &str) -> &'static str {
    match tool_name {
        "browser_click" => "Browser Click",
        "browser_type" => "Browser Type",
        "browser_key" => "Browser Key",
        "browser_javascript" => "Browser JavaScript",
        "browser_scroll" => "Browser Scroll",
        "browser_open" => "Browser Open",
        "browser_close" => "Browser Close",
        "browser_status" => "Browser Status",
        "browser_history" => "Browser History",
        "browser_inspect" => "Browser Inspect",
        "browser_console" => "Browser Console",
        "browser_cdp" => "Browser CDP",
        "browser_move" => "Browser Move",
        _ => "Browser Tool",
    }
}

fn format_browser_args_line(args: &serde_json::Value) -> Vec<Line<'static>> {
    use serde_json::Value;
    let mut lines: Vec<Line<'static>> = Vec::new();

    let dim = |s: &str| {
        Span::styled(
            s.to_string(),
            Style::default().fg(crate::colors::text_dim()),
        )
    };
    let text = |s: String| Span::styled(s, Style::default().fg(crate::colors::text()));

    // Helper to one-line, truncated representation for values
    fn short(v: &serde_json::Value, key: &str) -> String {
        match v {
            serde_json::Value::String(s) => {
                let one = s.replace('\n', " ");
                let max = if key == "code" { 80 } else { 80 };
                if one.chars().count() > max {
                    let truncated: String = one.chars().take(max).collect();
                    format!("{}…", truncated)
                } else {
                    one
                }
            }
            serde_json::Value::Number(n) => n.to_string(),
            serde_json::Value::Bool(b) => b.to_string(),
            serde_json::Value::Array(a) => format!("[{} items]", a.len()),
            serde_json::Value::Object(o) => format!("{{{} keys}}", o.len()),
            serde_json::Value::Null => "null".to_string(),
        }
    }

    match args {
        Value::Object(map) => {
            // Preserve insertion order (serde_json in this crate preserves order via feature)
            for (k, v) in map {
                let val = short(v, k);
                lines.push(Line::from(vec![
                    dim("└ "),
                    dim(&format!("{}: ", k)),
                    text(val),
                ]));
            }
        }
        Value::Null => {}
        other => {
            lines.push(Line::from(vec![dim("└ args: "), text(other.to_string())]));
        }
    }
    lines
}

// Attempt a compact, humanized one-line summary for browser tools.
// Returns Some(lines) when a concise form is available for the given tool, else None.
fn format_browser_args_humanized(
    tool_name: &str,
    args: &serde_json::Value,
) -> Option<Vec<Line<'static>>> {
    use serde_json::Value;
    let text = |s: String| Span::styled(s, Style::default().fg(crate::colors::text()));

    // Helper: format coordinate pair as integers (pixels)
    let fmt_xy = |x: f64, y: f64| -> String {
        let xi = x.round() as i64;
        let yi = y.round() as i64;
        format!("({xi}, {yi})")
    };

    match (tool_name, args) {
        ("browser_click", Value::Object(map)) => {
            // Expect optional `type`, and x/y for absolute. Only compact when both x and y provided.
            let ty = map
                .get("type")
                .and_then(|v| v.as_str())
                .unwrap_or("click")
                .to_lowercase();
            let (x, y) = match (
                map.get("x").and_then(|v| v.as_f64()),
                map.get("y").and_then(|v| v.as_f64()),
            ) {
                (Some(x), Some(y)) => (x, y),
                _ => return None,
            };
            let msg = format!("└ {ty} at {}", fmt_xy(x, y));
            Some(vec![Line::from(text(msg))])
        }
        ("browser_move", Value::Object(map)) => {
            // Prefer absolute x/y → "to (x, y)"; otherwise relative dx/dy → "by (dx, dy)".
            if let (Some(x), Some(y)) = (
                map.get("x").and_then(|v| v.as_f64()),
                map.get("y").and_then(|v| v.as_f64()),
            ) {
                let msg = format!("└ to {}", fmt_xy(x, y));
                return Some(vec![Line::from(text(msg))]);
            }
            if let (Some(dx), Some(dy)) = (
                map.get("dx").and_then(|v| v.as_f64()),
                map.get("dy").and_then(|v| v.as_f64()),
            ) {
                let msg = format!("└ by {}", fmt_xy(dx, dy));
                return Some(vec![Line::from(text(msg))]);
            }
            None
        }
        _ => None,
    }
}

fn new_completed_browser_tool_call(
    tool_name: String,
    args: Option<String>,
    duration: Duration,
    success: bool,
    result: String,
) -> ToolCallCell {
    let title = browser_tool_title(&tool_name);
    let duration = format_duration(duration);

    // Title styled by status with duration dimmed
    let title_line = if success {
        Line::from(vec![
            Span::styled(
                title,
                Style::default()
                    .fg(crate::colors::success())
                    .add_modifier(Modifier::BOLD),
            ),
            format!(", duration: {duration}").dim(),
        ])
    } else {
        Line::from(vec![
            Span::styled(
                title,
                Style::default()
                    .fg(crate::colors::error())
                    .add_modifier(Modifier::BOLD),
            ),
            format!(", duration: {duration}").dim(),
        ])
    };

    // Parse args JSON (if provided)
    let mut arg_lines: Vec<Line<'static>> = Vec::new();
    if let Some(args_str) = args {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(&args_str) {
            if let Some(lines) = format_browser_args_humanized(&tool_name, &json) {
                arg_lines.extend(lines);
            } else {
                arg_lines.extend(format_browser_args_line(&json));
            }
        }
    }

    // Result lines (preview format)
    let mut result_lines: Vec<Line<'static>> = Vec::new();
    if !result.is_empty() {
        let preview = build_preview_lines(&result, true)
            .into_iter()
            .map(|l| l.style(Style::default().fg(crate::colors::text_dim())))
            .collect::<Vec<_>>();
        result_lines.extend(preview);
    }

    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(title_line);
    lines.extend(arg_lines);
    if !result_lines.is_empty() {
        lines.push(Line::from(""));
        lines.extend(result_lines);
    }
    lines.push(Line::from(""));

    ToolCallCell {
        lines,
        state: if success {
            ToolState::Success
        } else {
            ToolState::Failed
        },
    }
}

// Map `agent_*` tool names to friendly titles
fn agent_tool_title(tool_name: &str) -> String {
    match tool_name {
        "agent_run" => "Agent Run".to_string(),
        "agent_check" => "Agent Check".to_string(),
        "agent_result" => "Agent Result".to_string(),
        "agent_cancel" => "Agent Cancel".to_string(),
        "agent_wait" => "Agent Wait".to_string(),
        "agent_list" => "Agent List".to_string(),
        other => {
            // Fallback: pretty-print unknown agent_* tools as "Agent <TitleCase>"
            if let Some(rest) = other.strip_prefix("agent_") {
                let title = rest
                    .split('_')
                    .filter(|s| !s.is_empty())
                    .map(|s| {
                        let mut chars = s.chars();
                        match chars.next() {
                            Some(first) => format!("{}{}", first.to_uppercase(), chars.as_str()),
                            None => String::new(),
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("Agent {}", title)
            } else {
                "Agent Tool".to_string()
            }
        }
    }
}

fn new_completed_agent_tool_call(
    tool_name: String,
    args: Option<String>,
    duration: Duration,
    success: bool,
    result: String,
) -> ToolCallCell {
    let title = agent_tool_title(&tool_name);
    let duration = format_duration(duration);

    // Title styled by status with duration dimmed
    let title_line = if success {
        Line::from(vec![
            Span::styled(
                title,
                Style::default()
                    .fg(crate::colors::success())
                    .add_modifier(Modifier::BOLD),
            ),
            format!(", duration: {duration}").dim(),
        ])
    } else {
        Line::from(vec![
            Span::styled(
                title,
                Style::default()
                    .fg(crate::colors::error())
                    .add_modifier(Modifier::BOLD),
            ),
            format!(", duration: {duration}").dim(),
        ])
    };

    // Parse args JSON (if provided)
    let mut arg_lines: Vec<Line<'static>> = Vec::new();
    if let Some(args_str) = args {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(&args_str) {
            arg_lines.extend(format_browser_args_line(&json));
        }
    }

    // Result lines (preview format)
    let mut result_lines: Vec<Line<'static>> = Vec::new();
    if !result.is_empty() {
        let preview = build_preview_lines(&result, true)
            .into_iter()
            .map(|l| l.style(Style::default().fg(crate::colors::text_dim())))
            .collect::<Vec<_>>();
        result_lines.extend(preview);
    }

    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(title_line);
    lines.extend(arg_lines);
    if !result_lines.is_empty() {
        lines.push(Line::from(""));
        lines.extend(result_lines);
    }
    lines.push(Line::from(""));

    ToolCallCell {
        lines,
        state: if success {
            ToolState::Success
        } else {
            ToolState::Failed
        },
    }
}

// Try to create an image cell if the MCP result contains an image
fn try_new_completed_mcp_tool_call_with_image_output(
    result: &Result<mcp_types::CallToolResult, String>,
) -> Option<ImageOutputCell> {
    match result {
        Ok(mcp_types::CallToolResult { content, .. }) => {
            if let Some(mcp_types::ContentBlock::ImageContent(image)) = content.first() {
                let raw_data = match base64::engine::general_purpose::STANDARD.decode(&image.data) {
                    Ok(data) => data,
                    Err(e) => {
                        error!("Failed to decode image data: {e}");
                        return None;
                    }
                };
                let reader = match ImageReader::new(Cursor::new(raw_data)).with_guessed_format() {
                    Ok(reader) => reader,
                    Err(e) => {
                        error!("Failed to guess image format: {e}");
                        return None;
                    }
                };

                let image = match reader.decode() {
                    Ok(image) => image,
                    Err(e) => {
                        error!("Image decoding failed: {e}");
                        return None;
                    }
                };

                Some(ImageOutputCell { image })
            } else {
                None
            }
        }
        _ => None,
    }
}

pub(crate) fn new_completed_mcp_tool_call(
    _num_cols: usize,
    invocation: McpInvocation,
    duration: Duration,
    success: bool,
    result: Result<mcp_types::CallToolResult, String>,
) -> Box<dyn HistoryCell> {
    if let Some(cell) = try_new_completed_mcp_tool_call_with_image_output(&result) {
        return Box::new(cell);
    }

    let duration = format_duration(duration);
    let status_str = if success { "Complete" } else { "Error" };
    let title_line = if success {
        Line::from(vec![
            Span::styled(status_str, Style::default().fg(crate::colors::success())),
            format!(", duration: {duration}").dim(),
        ])
    } else {
        Line::from(vec![
            Span::styled(status_str, Style::default().fg(crate::colors::error())),
            format!(", duration: {duration}").dim(),
        ])
    };

    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(title_line);
    lines.push(format_mcp_invocation(invocation));

    match result {
        Ok(mcp_types::CallToolResult { content, .. }) => {
            if !content.is_empty() {
                lines.push(Line::from(""));

                for tool_call_result in content {
                    match tool_call_result {
                        mcp_types::ContentBlock::TextContent(text) => {
                            let mut preview = build_preview_lines(&text.text, true);
                            preview = preview
                                .into_iter()
                                .map(|l| l.style(Style::default().fg(crate::colors::text_dim())))
                                .collect();
                            lines.extend(preview);
                        }
                        mcp_types::ContentBlock::ImageContent(_) => {
                            lines.push(Line::from("<image content>".to_string()))
                        }
                        mcp_types::ContentBlock::AudioContent(_) => {
                            lines.push(Line::from("<audio content>".to_string()))
                        }
                        mcp_types::ContentBlock::EmbeddedResource(resource) => {
                            let uri = match resource.resource {
                                EmbeddedResourceResource::TextResourceContents(text) => text.uri,
                                EmbeddedResourceResource::BlobResourceContents(blob) => blob.uri,
                            };
                            lines.push(Line::from(format!("embedded resource: {uri}")));
                        }
                        mcp_types::ContentBlock::ResourceLink(ResourceLink { uri, .. }) => {
                            lines.push(Line::from(format!("link: {uri}")));
                        }
                    }
                }
            }

            lines.push(Line::from(""));
        }
        Err(e) => {
            lines.push(Line::from(vec![
                Span::styled(
                    "Error: ",
                    Style::default()
                        .fg(crate::colors::error())
                        .add_modifier(Modifier::BOLD),
                ),
                Span::styled(e, Style::default().fg(crate::colors::error())),
            ]));
            lines.push(Line::from(""));
        }
    }

    Box::new(ToolCallCell {
        lines,
        state: if success {
            ToolState::Success
        } else {
            ToolState::Failed
        },
    })
}

pub(crate) fn new_error_event(message: String) -> PlainHistoryCell {
    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(Line::styled(
        "error",
        Style::default()
            .fg(crate::colors::error())
            .add_modifier(Modifier::BOLD),
    ));
    let msg_norm = normalize_overwrite_sequences(&message);
    lines.extend(
        msg_norm
            .lines()
            .map(|line| ansi_escape_line(line).style(Style::default().fg(crate::colors::error()))),
    );
    // No empty line at end - trimming and spacing handled by renderer
    PlainHistoryCell {
        lines,
        kind: HistoryCellType::Error,
    }
}

pub(crate) fn new_diff_output(diff_output: String) -> DiffCell {
    // Parse the diff output into lines
    let mut lines = vec![Line::from("/diff".magenta())];
    for line in diff_output.lines() {
        if line.starts_with('+') && !line.starts_with("+++") {
            lines.push(Line::from(line.to_string().green()));
        } else if line.starts_with('-') && !line.starts_with("---") {
            lines.push(Line::from(line.to_string().red()));
        } else if line.starts_with("@@") {
            lines.push(Line::from(line.to_string().cyan()));
        } else {
            lines.push(Line::from(line.to_string()));
        }
    }
    lines.push(Line::from(""));
    DiffCell { lines }
}

pub(crate) fn new_reasoning_output(reasoning_effort: &ReasoningEffort) -> PlainHistoryCell {
    let lines = vec![
        Line::from(""),
        Line::from("Reasoning Effort".magenta().bold()),
        Line::from(format!("Value: {}", reasoning_effort)),
    ];
    PlainHistoryCell {
        lines,
        kind: HistoryCellType::Notice,
    }
}

// Continue with more factory functions...
// I'll add the rest in the next part to keep this manageable
pub(crate) fn new_status_output(config: &Config, usage: &TokenUsage) -> PlainHistoryCell {
    let mut lines: Vec<Line<'static>> = Vec::new();

    lines.push(Line::from("/status".magenta()));
    lines.push(Line::from(""));

    // 🔧 Configuration
    lines.push(Line::from(vec!["🔧 ".into(), "Configuration".bold()]));

    // Prepare config summary with custom prettification
    let summary_entries = create_config_summary_entries(config);
    let summary_map: HashMap<String, String> = summary_entries
        .iter()
        .map(|(key, value)| (key.to_string(), value.clone()))
        .collect();

    let lookup = |key: &str| -> String { summary_map.get(key).unwrap_or(&String::new()).clone() };
    let title_case = |s: &str| -> String {
        s.split_whitespace()
            .map(|w| {
                let mut chars = w.chars();
                match chars.next() {
                    None => String::new(),
                    Some(f) => f.to_uppercase().collect::<String>() + chars.as_str(),
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    };

    // Format model name with proper capitalization
    let formatted_model = if config.model.to_lowercase().starts_with("gpt-") {
        format!("GPT{}", &config.model[3..])
    } else {
        config.model.clone()
    };
    lines.push(Line::from(vec![
        "  • Name: ".into(),
        formatted_model.into(),
    ]));
    let provider_disp = pretty_provider_name(&config.model_provider_id);
    lines.push(Line::from(vec![
        "  • Provider: ".into(),
        provider_disp.into(),
    ]));

    // Only show Reasoning fields if present in config summary
    let reff = lookup("reasoning effort");
    if !reff.is_empty() {
        lines.push(Line::from(vec![
            "  • Reasoning Effort: ".into(),
            title_case(&reff).into(),
        ]));
    }
    let rsum = lookup("reasoning summaries");
    if !rsum.is_empty() {
        lines.push(Line::from(vec![
            "  • Reasoning Summaries: ".into(),
            title_case(&rsum).into(),
        ]));
    }

    lines.push(Line::from(""));

    // 📊 Token Usage
    lines.push(Line::from(vec!["📊 ".into(), "Token Usage".bold()]));
    // Input: <input> [+ <cached> cached]
    let mut input_line_spans: Vec<Span<'static>> = vec![
        "  • Input: ".into(),
        usage.non_cached_input().to_string().into(),
    ];
    if let Some(cached) = usage.cached_input_tokens {
        if cached > 0 {
            input_line_spans.push(format!(" (+ {cached} cached)").into());
        }
    }
    lines.push(Line::from(input_line_spans));
    // Output: <output>
    lines.push(Line::from(vec![
        "  • Output: ".into(),
        usage.output_tokens.to_string().into(),
    ]));
    // Total: <total>
    lines.push(Line::from(vec![
        "  • Total: ".into(),
        usage.blended_total().to_string().into(),
    ]));

    PlainHistoryCell {
        lines,
        kind: HistoryCellType::Notice,
    }
}

pub(crate) fn new_prompts_output() -> PlainHistoryCell {
    let lines: Vec<Line<'static>> = vec![
        Line::from("/prompts".magenta()),
        Line::from(""),
        Line::from(" 1. Explain this codebase"),
        Line::from(" 2. Summarize recent commits"),
        Line::from(" 3. Implement {feature}"),
        Line::from(" 4. Find and fix a bug in @filename"),
        Line::from(" 5. Write tests for @filename"),
        Line::from(" 6. Improve documentation in @filename"),
        Line::from(""),
    ];
    PlainHistoryCell {
        lines,
        kind: HistoryCellType::Notice,
    }
}

pub(crate) fn new_plan_update(update: UpdatePlanArgs) -> PlainHistoryCell {
    let UpdatePlanArgs { explanation, plan } = update;

    let mut lines: Vec<Line<'static>> = Vec::new();
    // Header with progress summary
    let total = plan.len();
    let completed = plan
        .iter()
        .filter(|p| matches!(p.status, StepStatus::Completed))
        .count();

    let width: usize = 10;
    let filled = if total > 0 {
        (completed * width + total / 2) / total
    } else {
        0
    };
    let empty = width.saturating_sub(filled);

    // Build header without leading icon; icon will render in the gutter
    let mut header: Vec<Span> = Vec::new();
    let total = plan.len();
    let completed = plan
        .iter()
        .filter(|p| matches!(p.status, StepStatus::Completed))
        .count();
    header.push(Span::styled(
        " Update plan",
        Style::default()
            .fg(crate::colors::primary())
            .add_modifier(Modifier::BOLD),
    ));
    header.push(Span::raw(" ["));
    if filled > 0 {
        header.push(Span::styled(
            "█".repeat(filled),
            Style::default().fg(crate::colors::success()),
        ));
    }
    if empty > 0 {
        header.push(Span::styled(
            "░".repeat(empty),
            Style::default().add_modifier(Modifier::DIM),
        ));
    }
    header.push(Span::raw("] "));
    header.push(Span::raw(format!("{completed}/{total}")));
    lines.push(Line::from(header));

    // Optional explanation/note from the model
    if let Some(expl) = explanation.and_then(|s| {
        let t = s.trim().to_string();
        if t.is_empty() { None } else { Some(t) }
    }) {
        lines.push(Line::from("note".dim().italic()));
        for l in expl.lines() {
            lines.push(Line::from(l.to_string()).dim());
        }
    }

    // Steps styled as checkbox items
    if plan.is_empty() {
        lines.push(Line::from("(no steps provided)".dim().italic()));
    } else {
        for (idx, PlanItemArg { step, status }) in plan.into_iter().enumerate() {
            let (box_span, text_span) = match status {
                StepStatus::Completed => (
                    Span::styled("✔", Style::default().fg(crate::colors::success())),
                    Span::styled(
                        step,
                        Style::default().add_modifier(Modifier::CROSSED_OUT | Modifier::DIM),
                    ),
                ),
                StepStatus::InProgress => (
                    Span::raw("□"),
                    Span::styled(
                        step,
                        Style::default()
                            .fg(crate::colors::info())
                            .add_modifier(Modifier::BOLD),
                    ),
                ),
                StepStatus::Pending => (
                    Span::raw("□"),
                    Span::styled(step, Style::default().add_modifier(Modifier::DIM)),
                ),
            };
            let prefix = if idx == 0 {
                Span::raw("└ ")
            } else {
                Span::raw("  ")
            };
            lines.push(Line::from(vec![
                prefix,
                box_span,
                Span::raw(" "),
                text_span,
            ]));
        }
    }

    PlainHistoryCell {
        lines,
        kind: HistoryCellType::PlanUpdate,
    }
}

pub(crate) fn new_patch_event(
    event_type: PatchEventType,
    changes: HashMap<PathBuf, FileChange>,
) -> PatchSummaryCell {
    let title = match event_type {
        PatchEventType::ApprovalRequest => "proposed patch".to_string(),
        PatchEventType::ApplyBegin { .. } => "Updating...".to_string(),
    };
    let kind = match event_type {
        PatchEventType::ApprovalRequest => PatchKind::Proposed,
        PatchEventType::ApplyBegin { .. } => PatchKind::ApplyBegin,
    };
    PatchSummaryCell {
        title,
        changes,
        event_type,
        kind,
    }
}

pub(crate) fn new_patch_apply_failure(stderr: String) -> PlainHistoryCell {
    let mut lines: Vec<Line<'static>> = vec![
        Line::from("❌ Patch application failed".red().bold()),
        Line::from(""),
    ];

    let norm = normalize_overwrite_sequences(&stderr);
    let norm = expand_tabs_to_spaces(&norm, 4);
    for line in norm.lines() {
        if !line.is_empty() {
            lines.push(ansi_escape_line(line).red());
        }
    }

    lines.push(Line::from(""));
    PlainHistoryCell {
        lines,
        kind: HistoryCellType::Patch {
            kind: PatchKind::ApplyFailure,
        },
    }
}

// ==================== PatchSummaryCell ====================
// Renders patch summary + details with width-aware hanging indents so wrapped
// diff lines align under their code indentation.

pub(crate) struct PatchSummaryCell {
    pub(crate) title: String,
    pub(crate) changes: HashMap<PathBuf, FileChange>,
    pub(crate) event_type: PatchEventType,
    pub(crate) kind: PatchKind,
}

impl HistoryCell for PatchSummaryCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Patch { kind: self.kind }
    }

    // We compute lines based on width at render time; provide a conservative
    // default for non-width callers (not normally used in our pipeline).
    fn display_lines(&self) -> Vec<Line<'static>> {
        create_diff_summary_with_width(&self.title, &self.changes, self.event_type, Some(80))
            .into_iter()
            .collect()
    }

    fn has_custom_render(&self) -> bool {
        true
    }

    fn desired_height(&self, width: u16) -> u16 {
        let lines: Vec<Line<'static>> = create_diff_summary_with_width(
            &self.title,
            &self.changes,
            self.event_type,
            Some(width as usize),
        )
        .into_iter()
        .collect();
        Paragraph::new(Text::from(lines))
            .wrap(Wrap { trim: false })
            .line_count(width)
            .try_into()
            .unwrap_or(0)
    }

    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        let lines: Vec<Line<'static>> = create_diff_summary_with_width(
            &self.title,
            &self.changes,
            self.event_type,
            Some(area.width as usize),
        )
        .into_iter()
        .collect();

        let text = Text::from(lines);
        let bg_block = Block::default().style(Style::default().bg(crate::colors::background()));
        Paragraph::new(text)
            .block(bg_block)
            .wrap(Wrap { trim: false })
            .scroll((skip_rows, 0))
            .style(Style::default().bg(crate::colors::background()))
            .render(area, buf);
    }
}

// new_patch_apply_success was removed in favor of in-place header mutation and type update in chatwidget

// ==================== Spacing Helper ====================

/// Check if a line appears to be a title/header (like "codex", "user", "thinking", etc.)
fn is_title_line(line: &Line) -> bool {
    // Check if the line has special formatting that indicates it's a title
    if line.spans.is_empty() {
        return false;
    }

    // Get the text content of the line
    let text: String = line
        .spans
        .iter()
        .map(|span| span.content.as_ref())
        .collect::<String>()
        .trim()
        .to_lowercase();

    // Check for common title patterns (fallback heuristic only; primary logic uses explicit cell types)
    matches!(
        text.as_str(),
        "codex"
            | "user"
            | "thinking"
            | "event"
            | "tool"
            | "/diff"
            | "/status"
            | "/prompts"
            | "reasoning effort"
            | "error"
    ) || text.starts_with("⚡")
        || text.starts_with("⚙")
        || text.starts_with("✓")
        || text.starts_with("✗")
        || text.starts_with("↯")
        || text.starts_with("proposed patch")
        || text.starts_with("applying patch")
        || text.starts_with("updating")
        || text.starts_with("updated")
}

/// Check if a line is empty (no content or just whitespace)
fn is_empty_line(line: &Line) -> bool {
    if line.spans.is_empty() {
        return true;
    }
    // Consider a line empty when all spans have only whitespace
    line.spans
        .iter()
        .all(|s| s.content.as_ref().trim().is_empty())
}

/// Trim empty lines from the beginning and end of a Vec<Line>.
/// Also normalizes internal spacing - no more than 1 empty line between content.
/// This ensures consistent spacing when cells are rendered together.
pub(crate) fn trim_empty_lines(mut lines: Vec<Line<'static>>) -> Vec<Line<'static>> {
    // Remove ALL leading empty lines
    while lines.first().map_or(false, is_empty_line) {
        lines.remove(0);
    }

    // Remove ALL trailing empty lines
    while lines.last().map_or(false, is_empty_line) {
        lines.pop();
    }

    // Normalize internal spacing - no more than 1 empty line in a row
    let mut result = Vec::new();
    let mut prev_was_empty = false;

    for line in lines {
        let is_empty = is_empty_line(&line);

        // Skip consecutive empty lines
        if is_empty && prev_was_empty {
            continue;
        }

        // Special case: If this is an empty line right after a title, skip it
        if is_empty && result.len() == 1 && result.first().map_or(false, is_title_line) {
            continue;
        }

        result.push(line);
        prev_was_empty = is_empty;
    }

    result
}

/// Retint a set of pre-rendered lines by mapping colors from the previous
/// theme palette to the new one. This pragmatically applies a theme change
/// to already materialized `Line` structures without rebuilding them from
/// semantic sources.
pub(crate) fn retint_lines_in_place(
    lines: &mut Vec<Line<'static>>,
    old: &crate::theme::Theme,
    new: &crate::theme::Theme,
) {
    use ratatui::style::Color;
    fn map_color(c: Color, old: &crate::theme::Theme, new: &crate::theme::Theme) -> Color {
        // Map prior theme-resolved colors to new theme.
        if c == old.text {
            return new.text;
        }
        if c == old.text_dim {
            return new.text_dim;
        }
        if c == old.text_bright {
            return new.text_bright;
        }
        if c == old.primary {
            return new.primary;
        }
        if c == old.success {
            return new.success;
        }
        if c == old.error {
            return new.error;
        }
        if c == old.info {
            return new.info;
        }
        if c == old.border {
            return new.border;
        }
        if c == old.foreground {
            return new.foreground;
        }
        if c == old.background {
            return new.background;
        }

        // Map named ANSI colors to semantic theme colors for dynamic theme switches
        match c {
            Color::White => return new.text_bright,
            Color::Gray | Color::DarkGray => return new.text_dim,
            Color::Black => return new.text, // ensure visible on dark backgrounds
            Color::Red | Color::LightRed => return new.error,
            Color::Green | Color::LightGreen => return new.success,
            Color::Yellow | Color::LightYellow => return new.warning,
            Color::Blue | Color::LightBlue | Color::Cyan | Color::LightCyan => return new.info,
            Color::Magenta | Color::LightMagenta => return new.primary,
            _ => {}
        }

        c
    }

    for line in lines.iter_mut() {
        // First retint the line-level style so lines that rely on a global
        // foreground/background (with span-level colors unset) still update.
        {
            let mut st = line.style;
            if let Some(fg) = st.fg {
                st.fg = Some(map_color(fg, old, new));
            }
            if let Some(bg) = st.bg {
                st.bg = Some(map_color(bg, old, new));
            }
            if let Some(uc) = st.underline_color {
                st.underline_color = Some(map_color(uc, old, new));
            }
            line.style = st;
        }

        // Then retint any explicit span-level colors.
        let mut new_spans: Vec<Span<'static>> = Vec::with_capacity(line.spans.len());
        for s in line.spans.drain(..) {
            let mut st = s.style;
            if let Some(fg) = st.fg {
                st.fg = Some(map_color(fg, old, new));
            }
            if let Some(bg) = st.bg {
                st.bg = Some(map_color(bg, old, new));
            }
            if let Some(uc) = st.underline_color {
                st.underline_color = Some(map_color(uc, old, new));
            }
            new_spans.push(Span::styled(s.content, st));
        }
        line.spans = new_spans;
    }
}
