use crate::exec_command::strip_bash_lc_and_escape;
use crate::slash_command::SlashCommand;
use crate::text_formatting::format_and_truncate_tool_result;
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
use crate::diff_render::create_diff_summary;
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
use ratatui::widgets::Paragraph;
use ratatui::widgets::WidgetRef;
use ratatui::widgets::Wrap;
use std::collections::HashMap;
use std::io::Cursor;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;
use tracing::error;

// ==================== Core Types ====================

#[derive(Clone)]
pub(crate) struct CommandOutput {
    pub(crate) exit_code: i32,
    pub(crate) stdout: String,
    pub(crate) stderr: String,
}

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
pub(crate) enum ExecKind { Read, Search, List, Run }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ExecStatus { Running, Success, Error }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ToolStatus { Running, Success, Failed }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PatchKind { Proposed, ApplyBegin, ApplySuccess, ApplyFailure }

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
            // Custom renders (like animations) need to render fully - they can't be skipped
            // The area is already adjusted for the visible portion
            self.custom_render(area, buf);
            return;
        }

        // Default path: render the full text and use Paragraph.scroll to skip
        // vertical rows AFTER wrapping. Slicing lines before wrapping causes
        // incorrect blank space when lines wrap across multiple rows.
        let lines = self.display_lines_trimmed();
        let text = Text::from(lines);

        Paragraph::new(text)
            .wrap(Wrap { trim: false })
            .scroll((skip_rows, 0))
            .style(Style::default().bg(crate::colors::background()))
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
            HistoryCellType::Assistant => Some("•"),
            HistoryCellType::Reasoning => None,
            HistoryCellType::Error => Some("✖"),
            HistoryCellType::Tool { status } => Some(match status {
                ToolStatus::Running => "⚙",
                ToolStatus::Success => "✔",
                ToolStatus::Failed => "✖",
            }),
            HistoryCellType::Exec { kind, status } => {
                // Show ➤ only for Run executions; hide for read/search/list summaries
                match (kind, status) {
                    (ExecKind::Run, ExecStatus::Error) => Some("✖"),
                    (ExecKind::Run, _) => Some("➤"),
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
    fn kind(&self) -> HistoryCellType { self.as_ref().kind() }
    
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
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn kind(&self) -> HistoryCellType { self.kind }
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
}

// ==================== ExecCell ====================

pub(crate) struct ExecCell {
    pub(crate) command: Vec<String>,
    pub(crate) parsed: Vec<ParsedCommand>,
    pub(crate) output: Option<CommandOutput>,
    pub(crate) start_time: Option<Instant>,
}

impl HistoryCell for ExecCell {
    fn as_any(&self) -> &dyn std::any::Any { self }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
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
        exec_command_lines(&self.command, &self.parsed, self.output.as_ref(), self.start_time)
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
    fn as_any(&self) -> &dyn std::any::Any { self }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn kind(&self) -> HistoryCellType { HistoryCellType::AnimatedWelcome }
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
        if let Some(h) = self.locked_height.get() { return h.saturating_add(3); }

        // Word "CODE" uses 4 letters of 5 cols each with 3 gaps: 4*5 + 3 = 23 cols.
        let cols: u16 = 23;
        let base_rows: u16 = 7;
        let max_scale: u16 = 3;
        let scale = if width >= cols { (width / cols).min(max_scale).max(1) } else { 1 };
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
        let positioned_area = Rect { x: area.x, y: area.y, width: area.width, height };
        
        let fade_duration = std::time::Duration::from_millis(800);
        
        // Check if we're in fade-out phase
        if let Some(fade_time) = self.fade_start.get() {
            let fade_elapsed = fade_time.elapsed();
            if fade_elapsed < fade_duration && !self.faded_out.get() {
                // Fade-out animation
                let fade_progress = fade_elapsed.as_secs_f32() / fade_duration.as_secs_f32();
                let alpha = 1.0 - fade_progress; // From 1.0 to 0.0
                
                crate::glitch_animation::render_intro_animation_with_alpha(
                    positioned_area, buf, 1.0, // Full animation progress (static state)
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
            if elapsed < animation_duration { return true; }
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
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn kind(&self) -> HistoryCellType { HistoryCellType::Loading }
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

// ==================== ImageOutputCell ====================

pub(crate) struct ImageOutputCell {
    #[allow(dead_code)] // Will be used for terminal image protocol support
    pub(crate) image: DynamicImage,
}

impl HistoryCell for ImageOutputCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn kind(&self) -> HistoryCellType { HistoryCellType::Image }
    fn display_lines(&self) -> Vec<Line<'static>> {
        vec![
            Line::from("tool result (image output omitted)"),
            Line::from(""),
        ]
    }
}

// ==================== ToolCallCell ====================

pub(crate) enum ToolState {
    Running,
    Success,
    Failed,
}

pub(crate) struct ToolCallCell {
    lines: Vec<Line<'static>>,
    state: ToolState,
}

impl HistoryCell for ToolCallCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn as_any(&self) -> &dyn std::any::Any { self }
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
    fn as_any(&self) -> &dyn std::any::Any { self }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn kind(&self) -> HistoryCellType { HistoryCellType::Tool { status: ToolStatus::Running } }
    fn is_animating(&self) -> bool { true }
    fn display_lines(&self) -> Vec<Line<'static>> {
        let elapsed = self.start_time.elapsed();
        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::styled(
            format!("{} ({})", self.title, format_duration(elapsed)),
            Style::default().fg(crate::colors::primary()).add_modifier(Modifier::BOLD),
        ));
        lines.extend(self.arg_lines.clone());
        lines.push(Line::from(""));
        lines
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
            if trimmed.is_empty() { continue; }

            // Title heuristics:
            // 1) Entire line bold
            let all_bold = !l.spans.is_empty() && l.spans.iter().all(|s| s.style.add_modifier.contains(Modifier::BOLD) || s.content.trim().is_empty());
            // 2) Starts with one or more bold spans and ends with ':'
            let mut leading_bold = true;
            for s in &l.spans {
                if s.content.trim().is_empty() { continue; }
                leading_bold &= s.style.add_modifier.contains(Modifier::BOLD);
                break;
            }
            let ends_colon = trimmed.ends_with(':');

            // 3) Markdown heading (begins with '#') - renderer includes hashes in content
            let is_md_heading = trimmed.starts_with('#');

            // 4) Title-like plain line: reasonably short, no terminal punctuation, and
            //    either first line or preceded by a blank separator.
            let prev_blank = idx == 0 || lines.get(idx.saturating_sub(1)).map(|pl| {
                pl.spans.iter().all(|s| s.content.trim().is_empty())
            }).unwrap_or(true);
            let len_ok = trimmed.chars().count() >= 3 && trimmed.chars().count() <= 80;
            let no_terminal_punct = !trimmed.ends_with('.') && !trimmed.ends_with('!') && !trimmed.ends_with('?');
            let plain_title_like = prev_blank && len_ok && no_terminal_punct;

            if all_bold || (leading_bold && ends_colon) || is_md_heading || plain_title_like {
                titles.push(l.clone());
            }
        }

        if titles.is_empty() {
            // Fallback: first non-empty line as summary
            for l in lines.iter() {
                let text: String = l.spans.iter().map(|s| s.content.as_ref()).collect();
                let trimmed = text.trim();
                if trimmed.is_empty() { continue; }
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
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType { HistoryCellType::Reasoning }
    
    // Ensure collapsed reasoning always gets standard spacing after it.
    // Treating it as a title-only cell suppresses the inter-cell spacer,
    // which causes the missing blank line effect users observed.
    fn is_title_only(&self) -> bool { false }
    
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
    pub(crate) lines: Vec<Line<'static>>,
}

impl HistoryCell for StreamingContentCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn kind(&self) -> HistoryCellType { HistoryCellType::Assistant }
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

// ==================== Helper Functions ====================

const TOOL_CALL_MAX_LINES: usize = 5;


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
        // Truncate stdout to 6 lines: first 1, ellipsis, last 5 when needed
        let non_empty: Vec<&str> = stdout.lines().filter(|line| !line.is_empty()).collect();
        enum Seg<'a> { Line(&'a str), Ellipsis }
        let segments: Vec<Seg> = if non_empty.len() <= 6 {
            non_empty.iter().map(|s| Seg::Line(s)).collect()
        } else {
            let mut v: Vec<Seg> = Vec::with_capacity(7);
            v.push(Seg::Line(non_empty[0]));
            v.push(Seg::Ellipsis);
            let start = non_empty.len() - 5;
            for s in &non_empty[start..] { v.push(Seg::Line(s)); }
            v
        };

        let start_time = Instant::now();
        for seg in segments {
            match seg {
                Seg::Line(line) => {
                    let elapsed = start_time.elapsed();
                    if elapsed > Duration::from_millis(100) {
                        tracing::warn!("Slow ansi_escape_line took {:?}", elapsed);
                    }
                    if include_angle_pipe {
                        let mut line_spans = vec![
                            Span::styled(
                                "> ",
                                Style::default()
                                    .add_modifier(Modifier::DIM)
                                    .fg(crate::colors::text_dim()),
                            ),
                        ];
                        let escaped_line = ansi_escape_line(line);
                        line_spans.extend(escaped_line.spans);
                        lines.push(Line::from(line_spans));
                    } else {
                        lines.push(ansi_escape_line(line));
                    }
                }
                Seg::Ellipsis => lines.push(Line::from("…".dim())),
            }
        }
    }

    if !stderr.is_empty() && *exit_code != 0 {
        if !lines.is_empty() {
            lines.push(Line::from(""));
        }
        lines.push(Line::styled(
            format!("Error (exit code {})", exit_code),
            Style::default().fg(crate::colors::error()),
        ));
        for line in stderr.lines().filter(|line| !line.is_empty()) {
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
    lines.extend(message.lines().map(|line| ansi_escape_line(line).dim()));
    // No empty line at end - trimming and spacing handled by renderer
    PlainHistoryCell { lines, kind: HistoryCellType::BackgroundEvent }
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
                Span::from(SlashCommand::Chrome.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/browser <url>", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Browser.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/plan", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Plan.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/solve", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Solve.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/code", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Code.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/reasoning", Style::default().fg(crate::colors::primary())),
                Span::from(" - "),
                Span::from(SlashCommand::Reasoning.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
        ];
        PlainHistoryCell { lines, kind: HistoryCellType::Notice }
    } else if config.model == model {
        PlainHistoryCell { lines: Vec::new(), kind: HistoryCellType::Notice }
    } else {
        let lines = vec![
            Line::from("model changed:".magenta().bold()),
            Line::from(format!("requested: {}", config.model)),
            Line::from(format!("used: {model}")),
            // No empty line at end - trimming and spacing handled by renderer
        ];
        PlainHistoryCell { lines, kind: HistoryCellType::Notice }
    }
}

pub(crate) fn new_user_prompt(message: String) -> PlainHistoryCell {
    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(Line::from("user"));
    // Build content lines, then trim trailing/leading empties and normalize spacing
    let content: Vec<Line<'static>> = message
        .lines()
        .map(|l| Line::from(l.to_string()))
        .collect();
    let content = trim_empty_lines(content);
    lines.extend(content);
    // No empty line at end - trimming and spacing handled by renderer
    PlainHistoryCell { lines, kind: HistoryCellType::User }
}

#[allow(dead_code)]
pub(crate) fn new_text_line(line: Line<'static>) -> PlainHistoryCell {
    PlainHistoryCell { lines: vec![line], kind: HistoryCellType::Notice }
}


pub(crate) fn new_streaming_content(lines: Vec<Line<'static>>) -> StreamingContentCell {
    StreamingContentCell { lines }
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
    LoadingCell {
        message,
    }
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
    let start_time = if output.is_none() { Some(Instant::now()) } else { None };
    ExecCell {
        command,
        parsed,
        output,
        start_time,
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
                    if let (Ok(start), Ok(end)) = (a.trim().parse::<u32>(), b.trim().parse::<u32>()) {
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
                if let Ok(n) = parts[i + 1].trim_matches('"').trim_matches('\'').parse::<u32>() {
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
                "read" => "Reading...".to_string(),
                "search" => "Searching...".to_string(),
                "list" => "Listing files...".to_string(),
                _ => match &ctx_path {
                    Some(p) => format!("Running... in {p}"),
                    None => "Running...".to_string(),
                },
            };
            // Use non-bold styling for informational actions; keep primary color
            if matches!(action, "read" | "search" | "list") {
                Line::styled(
                    format!("{}{}", header, duration_str),
                    Style::default().fg(crate::colors::primary()),
                )
            } else {
                Line::styled(
                    format!("{}{}", header, duration_str),
                    Style::default()
                        .fg(crate::colors::primary())
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
                Line::styled(
                    done,
                    Style::default().fg(crate::colors::text()),
                )
            } else {
                Line::styled(
                    done,
                    Style::default().fg(crate::colors::text_bright()).add_modifier(Modifier::BOLD),
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
                Line::styled(
                    done,
                    Style::default().fg(crate::colors::text()),
                )
            } else {
                Line::styled(
                    done,
                    Style::default().fg(crate::colors::text_bright()).add_modifier(Modifier::BOLD),
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
            ParsedCommand::ListFiles { cmd, path } => match path {
                Some(p) => {
                    if search_paths.contains(p) {
                        (String::new(), String::new()) // suppressed
                    } else {
                        let display_p = if p.ends_with('/') { p.to_string() } else { format!("{p}/") };
                        ("List".to_string(), display_p)
                    }
                }
                None => ("List".to_string(), cmd.clone()),
            },
            ParsedCommand::Search { query, path, cmd } => {
                // Format query: split on '|' and join with commas + 'and' for readability
                // Also pretty-print common shell-escaped characters for display purposes.
                let prettify_term = |s: &str| -> String {
                    let mut out = s
                        .replace("\\(", "(")
                        .replace("\\)", ")")
                        .replace("\\.", ".");
                    // Balance a single unmatched opening paren common in function-name searches
                    let opens = out.matches('(').count();
                    let closes = out.matches(')').count();
                    if opens > closes {
                        out.push(')');
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
                        let display_p = if p.ends_with('/') { p.to_string() } else { format!("{p}/") };
                        ("Search".to_string(), format!("{} in {}", fmt_query(q), display_p))
                    }
                    (Some(q), None) => ("Search".to_string(), format!("{}", fmt_query(q))),
                    (None, Some(p)) => {
                        let display_p = if p.ends_with('/') { p.to_string() } else { format!("{p}/") };
                        ("Search".to_string(), format!("in {}", display_p))
                    }
                    (None, None) => ("Search".to_string(), cmd.clone()),
                }
            },
            ParsedCommand::Format { .. } => ("Format".to_string(), String::new()),
            ParsedCommand::Test { cmd } => ("Test".to_string(), cmd.clone()),
            ParsedCommand::Lint { cmd, .. } => ("Lint".to_string(), cmd.clone()),
            ParsedCommand::Unknown { cmd } => ("Run".to_string(), cmd.clone()),
            ParsedCommand::Noop { .. } => continue,
        };

        // Skip suppressed entries
        if label.is_empty() && content.is_empty() {
            continue;
        }

        // Split content into lines and push without repeating the action label
        for line_text in content.lines() {
            if line_text.is_empty() { continue; }
            let prefix = if !any_content_emitted { "└ " } else { "  " };
            let mut spans: Vec<Span<'static>> = vec![
                Span::styled(prefix, Style::default().add_modifier(Modifier::DIM)),
            ];

            match label.as_str() {
                // Highlight searched terms in normal text color; keep connectors/path dim
                "Search" => {
                    let remaining = line_text.to_string();
                    // Split off optional path suffix. Support both " (in ...)" and " in <dir>/" forms.
                    let (terms_part, path_part) = if let Some(idx) = remaining.rfind(" (in ") {
                        (remaining[..idx].to_string(), Some(remaining[idx..].to_string()))
                    } else if let Some(idx) = remaining.rfind(" in ") {
                        let suffix = &remaining[idx + 1..]; // keep leading space for styling
                        // Heuristic: treat as path if it ends with '/'
                        if suffix.trim_end().ends_with('/') {
                            (remaining[..idx].to_string(), Some(remaining[idx..].to_string()))
                        } else {
                            (remaining.clone(), None)
                        }
                    } else {
                        (remaining.clone(), None)
                    };
                    // Tokenize terms by ", " and " and " while preserving separators
                    let tmp = terms_part.clone();
                    // First, split by ", "
                    let chunks: Vec<String> = if tmp.contains(", ") { tmp.split(", ").map(|s| s.to_string()).collect() } else { vec![tmp.clone()] };
                    for (i, chunk) in chunks.iter().enumerate() {
                        if i > 0 {
                            // Add comma separator between items (dim)
                            spans.push(Span::styled(", ", Style::default().fg(crate::colors::text_dim())));
                        }
                        // Within each chunk, if it contains " and ", split into left and right with dimmed " and "
                        if let Some((left, right)) = chunk.rsplit_once(" and ") {
                            if !left.is_empty() {
                                spans.push(Span::styled(left.to_string(), Style::default().fg(crate::colors::text())));
                                spans.push(Span::styled(" and ", Style::default().fg(crate::colors::text_dim())));
                                spans.push(Span::styled(right.to_string(), Style::default().fg(crate::colors::text())));
                            } else {
                                spans.push(Span::styled(chunk.to_string(), Style::default().fg(crate::colors::text())));
                            }
                        } else {
                            spans.push(Span::styled(chunk.to_string(), Style::default().fg(crate::colors::text())));
                        }
                    }
                    if let Some(p) = path_part {
                        // Dim the entire path portion including the " in " or " (in " prefix
                        spans.push(Span::styled(p, Style::default().fg(crate::colors::text_dim())));
                    }
                }
                // Highlight filenames in Read; keep line ranges dim
                "Read" => {
                    if let Some(idx) = line_text.find(" (") {
                        let (fname, rest) = line_text.split_at(idx);
                        spans.push(Span::styled(fname.to_string(), Style::default().fg(crate::colors::text())));
                        spans.push(Span::styled(rest.to_string(), Style::default().fg(crate::colors::text_dim())));
                    } else {
                        spans.push(Span::styled(line_text.to_string(), Style::default().fg(crate::colors::text())));
                    }
                }
                // List Files: highlight directory names
                "List" => {
                    spans.push(Span::styled(line_text.to_string(), Style::default().fg(crate::colors::text())));
                }
                _ => {
                    // For executed commands (Run/Test/Lint/etc.), show the command text
                    // in normal text color rather than dimmed.
                    spans.push(Span::styled(
                        line_text.to_string(),
                        Style::default().fg(crate::colors::text()),
                    ));
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
    let mut cmd_lines = command_escaped.lines();
    
    let header_line = match output {
        None => {
            let duration_str = if let Some(start) = start_time {
                let elapsed = start.elapsed();
                format!(" ({})", format_duration(elapsed))
            } else {
                String::new()
            };
            Line::styled(
                format!("Running...{}", duration_str),
                Style::default().fg(crate::colors::primary()).add_modifier(Modifier::BOLD),
            )
        }
        Some(o) if o.exit_code == 0 => Line::styled(
            "Ran",
            Style::default().fg(crate::colors::text_bright()).add_modifier(Modifier::BOLD),
        ),
        Some(_o) => {
            // Preserve the header as "Ran" even on error; detailed error output
            // (including exit code and stderr) will be shown below by `output_lines`.
            Line::styled(
                "Ran",
                Style::default().fg(crate::colors::text_bright()).add_modifier(Modifier::BOLD),
            )
        },
    };

    if let Some(first) = cmd_lines.next() {
        let duration_str = if output.is_none() && start_time.is_some() {
            let elapsed = start_time.unwrap().elapsed();
            format!(" ({})", format_duration(elapsed))
        } else {
            String::new()
        };

        lines.push(header_line.clone());
        // Show the command itself in standard text color; keep the duration dimmed
        lines.push(Line::from(vec![
            Span::styled(first.to_string(), Style::default().fg(crate::colors::text())),
            duration_str.dim(),
        ]));
    } else {
        lines.push(header_line);
    }
    
    for cont in cmd_lines {
        lines.push(Line::styled(
            cont.to_string(),
            Style::default().fg(crate::colors::text()),
        ));
    }

    lines.extend(output_lines(output, false, true));
    lines
}

pub(crate) fn new_active_mcp_tool_call(invocation: McpInvocation) -> ToolCallCell {
    let title_line = Line::styled("Working", Style::default().fg(crate::colors::primary()));
    let lines: Vec<Line> = vec![
        title_line,
        format_mcp_invocation(invocation),
        Line::from(""),
    ];
    ToolCallCell { lines, state: ToolState::Running }
}

pub(crate) fn new_active_custom_tool_call(tool_name: String, args: Option<String>) -> ToolCallCell {
    let title_line = Line::styled("Working", Style::default().fg(crate::colors::primary()));
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
    ToolCallCell { lines, state: ToolState::Running }
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

pub(crate) fn new_running_browser_tool_call(tool_name: String, args: Option<String>) -> RunningToolCallCell {
    // Parse args JSON and format like completed cells
    let mut arg_lines: Vec<Line<'static>> = Vec::new();
    if let Some(args_str) = args {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(&args_str) {
            arg_lines.extend(format_browser_args_line(&json));
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

pub(crate) fn new_running_custom_tool_call(tool_name: String, args: Option<String>) -> RunningToolCallCell {
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
        // Truncate result if needed
        let truncated = format_and_truncate_tool_result(&result, TOOL_CALL_MAX_LINES, 80);
        lines.push(Line::styled(
            truncated,
            Style::default().fg(crate::colors::text_dim()),
        ));
    }

    lines.push(Line::from(""));
    ToolCallCell { lines, state: if success { ToolState::Success } else { ToolState::Failed } }
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

    let dim = |s: &str| Span::styled(s.to_string(), Style::default().fg(crate::colors::text_dim()));
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
            Span::styled(title, Style::default().fg(crate::colors::success()).add_modifier(Modifier::BOLD)),
            format!(", duration: {duration}").dim(),
        ])
    } else {
        Line::from(vec![
            Span::styled(title, Style::default().fg(crate::colors::error()).add_modifier(Modifier::BOLD)),
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

    // Result line (truncated)
    let mut result_lines: Vec<Line<'static>> = Vec::new();
    if !result.is_empty() {
        let truncated = format_and_truncate_tool_result(&result, TOOL_CALL_MAX_LINES, 80);
        result_lines.push(Line::from(vec![
            Span::styled("Result: ", Style::default().fg(crate::colors::text_dim()).add_modifier(Modifier::BOLD)),
            Span::styled(truncated, Style::default().fg(crate::colors::text())),
        ]));
    }

    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(title_line);
    lines.extend(arg_lines);
    if !result_lines.is_empty() {
        lines.push(Line::from(""));
        lines.extend(result_lines);
    }
    lines.push(Line::from(""));

    ToolCallCell { lines, state: if success { ToolState::Success } else { ToolState::Failed } }
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
            Span::styled(title, Style::default().fg(crate::colors::success()).add_modifier(Modifier::BOLD)),
            format!(", duration: {duration}").dim(),
        ])
    } else {
        Line::from(vec![
            Span::styled(title, Style::default().fg(crate::colors::error()).add_modifier(Modifier::BOLD)),
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

    // Result line (truncated)
    let mut result_lines: Vec<Line<'static>> = Vec::new();
    if !result.is_empty() {
        let truncated = format_and_truncate_tool_result(&result, TOOL_CALL_MAX_LINES, 80);
        result_lines.push(Line::from(vec![
            Span::styled("Result: ", Style::default().fg(crate::colors::text_dim()).add_modifier(Modifier::BOLD)),
            Span::styled(truncated, Style::default().fg(crate::colors::text())),
        ]));
    }

    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(title_line);
    lines.extend(arg_lines);
    if !result_lines.is_empty() {
        lines.push(Line::from(""));
        lines.extend(result_lines);
    }
    lines.push(Line::from(""));

    ToolCallCell { lines, state: if success { ToolState::Success } else { ToolState::Failed } }
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
    num_cols: usize,
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
                    let line_text = match tool_call_result {
                        mcp_types::ContentBlock::TextContent(text) => {
                            format_and_truncate_tool_result(
                                &text.text,
                                TOOL_CALL_MAX_LINES,
                                num_cols,
                            )
                        }
                        mcp_types::ContentBlock::ImageContent(_) => {
                            "<image content>".to_string()
                        }
                        mcp_types::ContentBlock::AudioContent(_) => "<audio content>".to_string(),
                        mcp_types::ContentBlock::EmbeddedResource(resource) => {
                            let uri = match resource.resource {
                                EmbeddedResourceResource::TextResourceContents(text) => text.uri,
                                EmbeddedResourceResource::BlobResourceContents(blob) => blob.uri,
                            };
                            format!("embedded resource: {uri}")
                        }
                        mcp_types::ContentBlock::ResourceLink(ResourceLink { uri, .. }) => {
                            format!("link: {uri}")
                        }
                    };
                    lines.push(Line::styled(
                        line_text,
                        Style::default().fg(crate::colors::text_dim()),
                    ));
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

    Box::new(ToolCallCell { lines, state: if success { ToolState::Success } else { ToolState::Failed } })
}

pub(crate) fn new_error_event(message: String) -> PlainHistoryCell {
    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(Line::styled(
        "error",
        Style::default()
            .fg(crate::colors::error())
            .add_modifier(Modifier::BOLD),
    ));
    lines.extend(
        message
            .lines()
            .map(|line| ansi_escape_line(line).style(Style::default().fg(crate::colors::error()))),
    );
    // No empty line at end - trimming and spacing handled by renderer
    PlainHistoryCell { lines, kind: HistoryCellType::Error }
}

pub(crate) fn new_diff_output(diff_output: String) -> PlainHistoryCell {
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
    PlainHistoryCell { lines, kind: HistoryCellType::Diff }
}

pub(crate) fn new_reasoning_output(reasoning_effort: &ReasoningEffort) -> PlainHistoryCell {
    let lines = vec![
        Line::from(""),
        Line::from("Reasoning Effort".magenta().bold()),
        Line::from(format!("Value: {}", reasoning_effort)),
    ];
    PlainHistoryCell { lines, kind: HistoryCellType::Notice }
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
    
    PlainHistoryCell { lines, kind: HistoryCellType::Notice }
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
    PlainHistoryCell { lines, kind: HistoryCellType::Notice }
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
            let prefix = if idx == 0 { Span::raw("└ ") } else { Span::raw("  ") };
            lines.push(Line::from(vec![
                prefix,
                box_span,
                Span::raw(" "),
                text_span,
            ]));
        }
    }
    
    PlainHistoryCell { lines, kind: HistoryCellType::PlanUpdate }
}

pub(crate) fn new_patch_event(
    event_type: PatchEventType,
    changes: HashMap<PathBuf, FileChange>,
) -> PlainHistoryCell {
    let title = match event_type {
        PatchEventType::ApprovalRequest => "proposed patch",
        PatchEventType::ApplyBegin { auto_approved: true } => "Updating...",
        PatchEventType::ApplyBegin { auto_approved: false } => "Updating...",
    };

    let lines: Vec<Line<'static>> = create_diff_summary(title, &changes, event_type)
        .into_iter()
        .collect();
    let kind = match title {
        "proposed patch" => HistoryCellType::Patch { kind: PatchKind::Proposed },
        _ => HistoryCellType::Patch { kind: PatchKind::ApplyBegin },
    };
    PlainHistoryCell { lines, kind }
}

pub(crate) fn new_patch_apply_failure(stderr: String) -> PlainHistoryCell {
    let mut lines: Vec<Line<'static>> = vec![
        Line::from("❌ Patch application failed".red().bold()),
        Line::from(""),
    ];
    
    for line in stderr.lines() {
        lines.push(ansi_escape_line(line).red());
    }
    
    lines.push(Line::from(""));
    PlainHistoryCell { lines, kind: HistoryCellType::Patch { kind: PatchKind::ApplyFailure } }
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
    let text: String = line.spans.iter()
        .map(|span| span.content.as_ref())
        .collect::<String>()
        .trim()
        .to_lowercase();
    
    // Check for common title patterns (fallback heuristic only; primary logic uses explicit cell types)
    matches!(text.as_str(), 
        "codex" | "user" | "thinking" | "event" | 
        "tool" | "/diff" | "/status" | "/prompts" |
        "reasoning effort" | "error"
    ) || text.starts_with("⚡") || text.starts_with("⚙") || text.starts_with("✓") || text.starts_with("✗") ||
        text.starts_with("↯") || text.starts_with("proposed patch") || text.starts_with("applying patch") || text.starts_with("updating") || text.starts_with("updated")
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
){
    use ratatui::style::Color;
    fn map_color(c: Color, old: &crate::theme::Theme, new: &crate::theme::Theme) -> Color {
        // Map prior theme-resolved colors to new theme.
        if c == old.text { return new.text; }
        if c == old.text_dim { return new.text_dim; }
        if c == old.text_bright { return new.text_bright; }
        if c == old.primary { return new.primary; }
        if c == old.success { return new.success; }
        if c == old.error { return new.error; }
        if c == old.info { return new.info; }
        if c == old.border { return new.border; }
        if c == old.foreground { return new.foreground; }
        if c == old.background { return new.background; }

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
            if let Some(fg) = st.fg { st.fg = Some(map_color(fg, old, new)); }
            if let Some(bg) = st.bg { st.bg = Some(map_color(bg, old, new)); }
            if let Some(uc) = st.underline_color { st.underline_color = Some(map_color(uc, old, new)); }
            line.style = st;
        }

        // Then retint any explicit span-level colors.
        let mut new_spans: Vec<Span<'static>> = Vec::with_capacity(line.spans.len());
        for s in line.spans.drain(..) {
            let mut st = s.style;
            if let Some(fg) = st.fg { st.fg = Some(map_color(fg, old, new)); }
            if let Some(bg) = st.bg { st.bg = Some(map_color(bg, old, new)); }
            if let Some(uc) = st.underline_color { st.underline_color = Some(map_color(uc, old, new)); }
            new_spans.push(Span::styled(s.content, st));
        }
        line.spans = new_spans;
    }
}
