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

// ==================== HistoryCell Trait ====================

/// Represents an event to display in the conversation history.
/// Returns its `Vec<Line<'static>>` representation to make it easier 
/// to display in a scrollable list.
pub(crate) trait HistoryCell {
    fn display_lines(&self) -> Vec<Line<'static>>;
    
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
        } else {
            // Default: render using display_lines_trimmed for consistent spacing
            let lines = self.display_lines_trimmed();
            
            // Skip the specified number of rows
            let visible_lines: Vec<Line<'static>> = lines
                .into_iter()
                .skip(skip_rows as usize)
                .take(area.height as usize)
                .collect();
            
            if !visible_lines.is_empty() {
                let text = Text::from(visible_lines);
                Paragraph::new(text)
                    .wrap(Wrap { trim: false })
                    .render(area, buf);
            }
        }
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
        None // Default: no symbol
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
}

impl HistoryCell for PlainHistoryCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // If we have a gutter symbol, handle title line appropriately
        if let Some(_) = self.gutter_symbol() {
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
    
    fn gutter_symbol(&self) -> Option<&'static str> {
        // Detect type from first line content
        if let Some(first_line) = self.lines.first() {
            let text: String = first_line.spans.iter()
                .map(|span| span.content.as_ref())
                .collect::<String>()
                .trim()
                .to_lowercase();
            
            match text.as_str() {
                "user" => Some("›"),
                "codex" => Some("•"),
                "thinking" | "thinking..." => Some("⋮"),
                "tool" => Some("⚙"),
                "error" => Some("✖"),
                "event" => Some("»"),
                "notice" => Some("★"),
                _ => None,
            }
        } else {
            None
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
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn display_lines(&self) -> Vec<Line<'static>> {
        exec_command_lines(&self.command, &self.parsed, self.output.as_ref(), self.start_time)
    }
    
    fn gutter_symbol(&self) -> Option<&'static str> {
        // Dynamic gutter: working=⚙, success=✔, error=✖
        match &self.output {
            None => Some("⚙"),
            Some(o) if o.exit_code == 0 => Some("✔"),
            Some(_) => Some("✖"),
        }
    }
}

impl WidgetRef for &ExecCell {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        Paragraph::new(Text::from(self.display_lines_trimmed()))
            .wrap(Wrap { trim: false })
            .render(area, buf);
    }
}

// ==================== AnimatedWelcomeCell ====================

pub(crate) struct AnimatedWelcomeCell {
    pub(crate) start_time: Instant,
    pub(crate) completed: std::cell::Cell<bool>,
    pub(crate) fade_start: std::cell::Cell<Option<Instant>>,
    pub(crate) faded_out: std::cell::Cell<bool>,
}

impl HistoryCell for AnimatedWelcomeCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // For plain lines, just show a simple welcome message
        vec![
            Line::from(""),
            Line::from("Welcome to Coder"),
            Line::from(""),
        ]
    }
    
    fn desired_height(&self, _width: u16) -> u16 {
        // With scale of 3, we need 7 * 3 = 21 rows
        21
    }
    
    fn has_custom_render(&self) -> bool {
        true // AnimatedWelcomeCell uses custom rendering for the glitch animation
    }
    
    fn custom_render(&self, area: Rect, buf: &mut Buffer) {
        let elapsed = self.start_time.elapsed();
        tracing::debug!("AnimatedWelcomeCell::custom_render called, area: {:?}, elapsed: {:?}, completed: {}", 
                      area, elapsed, self.completed.get());
        
        // Position the animation at the bottom of the available area
        // The actual animation needs 21 rows
        let animation_height = 21u16;
        let positioned_area = if area.height > animation_height {
            // Position at bottom with a small margin
            let top_offset = area.height.saturating_sub(animation_height + 2); // 2 rows margin from bottom
            Rect {
                x: area.x,
                y: area.y + top_offset,
                width: area.width,
                height: animation_height,
            }
        } else {
            area
        };
        
        let fade_duration = std::time::Duration::from_millis(800);
        
        // Check if we're in fade-out phase
        if let Some(fade_time) = self.fade_start.get() {
            let fade_elapsed = fade_time.elapsed();
            if fade_elapsed < fade_duration && !self.faded_out.get() {
                // Fade-out animation
                let fade_progress = fade_elapsed.as_secs_f32() / fade_duration.as_secs_f32();
                let alpha = 1.0 - fade_progress; // From 1.0 to 0.0
                
                tracing::debug!("Rendering fade-out animation, alpha: {}", alpha);
                crate::glitch_animation::render_intro_animation_with_alpha(
                    positioned_area, buf, 1.0, // Full animation progress (static state)
                    alpha,
                );
            } else {
                // Fade-out complete - mark as faded out
                self.faded_out.set(true);
                // Don't render anything (invisible)
                tracing::debug!("Fade-out complete, not rendering");
            }
        } else {
            // Normal animation phase
            let elapsed = self.start_time.elapsed();
            let animation_duration = std::time::Duration::from_secs(2);
            
            if elapsed < animation_duration && !self.completed.get() {
                // Calculate animation progress
                let progress = elapsed.as_secs_f32() / animation_duration.as_secs_f32();
                
                // Render the animation
                tracing::debug!("Rendering animation, progress: {}", progress);
                crate::glitch_animation::render_intro_animation(positioned_area, buf, progress);
            } else {
                // Animation complete - mark it and render final static state
                self.completed.set(true);
                
                // Render the final static state
                tracing::debug!("Animation complete, rendering static state");
                crate::glitch_animation::render_intro_animation(
                    positioned_area, buf, 1.0, // Full progress = static final state
                );
            }
        }
    }
    
    fn is_animating(&self) -> bool {
        // Check for initial animation
        if !self.completed.get() {
            let elapsed = self.start_time.elapsed();
            let animation_duration = std::time::Duration::from_secs(2);
            if elapsed < animation_duration {
                tracing::debug!("AnimatedWelcomeCell is animating, elapsed: {:?}", elapsed);
                return true;
            }
            // Mark as completed if animation time has passed
            tracing::info!("AnimatedWelcomeCell animation complete after {:?}", elapsed);
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
            tracing::info!("Triggering fade-out animation for AnimatedWelcomeCell");
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
    fn display_lines(&self) -> Vec<Line<'static>> {
        vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("⟳ ", Style::default().fg(Color::Cyan)),
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
    fn display_lines(&self) -> Vec<Line<'static>> {
        // Hide header line if gutter is present
        if self.lines.len() > 1 {
            self.lines[1..].to_vec()
        } else {
            Vec::new()
        }
    }
    fn gutter_symbol(&self) -> Option<&'static str> {
        Some(match self.state {
            ToolState::Running => "⚙",
            ToolState::Success => "✔",
            ToolState::Failed => "✖",
        })
    }
}

// ==================== CollapsibleReasoningCell ====================
// For reasoning content that can be collapsed to show only summary

pub(crate) struct CollapsibleReasoningCell {
    pub(crate) lines: Vec<Line<'static>>,
    pub(crate) collapsed: std::cell::Cell<bool>,
}

impl CollapsibleReasoningCell {
    pub fn new(lines: Vec<Line<'static>>) -> Self {
        Self {
            lines,
            collapsed: std::cell::Cell::new(true), // Default to collapsed
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
        for l in lines.iter() {
            // Skip the "thinking" header and blank lines
            let text: String = l.spans.iter().map(|s| s.content.as_ref()).collect();
            let trimmed = text.trim();
            if trimmed.is_empty() { continue; }
            if trimmed.eq_ignore_ascii_case("thinking") { continue; }

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

            if all_bold || (leading_bold && ends_colon) || is_md_heading {
                titles.push(l.clone());
            }
        }

        if titles.is_empty() {
            // Fallback: first non-empty non-header line as summary
            for l in lines.iter() {
                let text: String = l.spans.iter().map(|s| s.content.as_ref()).collect();
                let trimmed = text.trim();
                if trimmed.is_empty() { continue; }
                if trimmed.eq_ignore_ascii_case("thinking") { continue; }
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
    
    fn display_lines(&self) -> Vec<Line<'static>> {
        if self.lines.is_empty() {
            return Vec::new();
        }
        
        // Normalize to improve section splitting and spacing
        let normalized = self.normalized_lines();
        
        // Skip the "thinking" header if present
        let start_idx = if normalized.first()
            .and_then(|l| l.spans.first())
            .map(|s| s.content.to_lowercase() == "thinking")
            .unwrap_or(false) {
            1
        } else {
            0
        };
        
        if self.collapsed.get() {
            // When collapsed, show extracted section titles (or at least one summary)
            return self.extract_section_titles();
        } else {
            // When expanded, show all lines except the header
            normalized[start_idx..].to_vec()
        }
    }
    
    fn gutter_symbol(&self) -> Option<&'static str> {
        Some("⋮")
    }
}

// ==================== StreamingContentCell ====================
// For live streaming content that's being actively rendered

pub(crate) struct StreamingContentCell {
    pub(crate) lines: Vec<Line<'static>>,
}

impl HistoryCell for StreamingContentCell {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // If we have a gutter symbol, handle title line appropriately
        if let Some(_) = self.gutter_symbol() {
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
    
    fn gutter_symbol(&self) -> Option<&'static str> {
        // Detect type from first line content (same as PlainHistoryCell)
        if let Some(first_line) = self.lines.first() {
            let text: String = first_line.spans.iter()
                .map(|span| span.content.as_ref())
                .collect::<String>()
                .trim()
                .to_lowercase();
            
            match text.as_str() {
                "user" => Some("›"),
                "codex" => Some("•"),
                "thinking" | "thinking..." => Some("⋮"),
                "tool" => Some("⚙"),
                "error" => Some("✖"),
                "event" => Some("»"),
                "notice" => Some("★"),
                _ => None,
            }
        } else {
            None
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
        let start_time = Instant::now();
        let output_lines = stdout.lines().filter(|line| !line.is_empty());
        for line in output_lines {
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
                            .fg(Color::Gray),
                    ),
                ];
                let escaped_line = ansi_escape_line(line);
                line_spans.extend(escaped_line.spans);
                lines.push(Line::from(line_spans));
            } else {
                lines.push(ansi_escape_line(line));
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
    PlainHistoryCell { lines }
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
                Style::default().fg(crate::colors::primary()),
            ),
            Line::from(vec![
                Span::styled("/chrome", Style::default().fg(crate::colors::function())),
                Span::from(" - "),
                Span::from(SlashCommand::Chrome.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/browser <url>", Style::default().fg(crate::colors::function())),
                Span::from(" - "),
                Span::from(SlashCommand::Browser.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/plan", Style::default().fg(crate::colors::function())),
                Span::from(" - "),
                Span::from(SlashCommand::Plan.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/solve", Style::default().fg(crate::colors::function())),
                Span::from(" - "),
                Span::from(SlashCommand::Solve.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/code", Style::default().fg(crate::colors::function())),
                Span::from(" - "),
                Span::from(SlashCommand::Code.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
            Line::from(vec![
                Span::styled("/reasoning", Style::default().fg(crate::colors::function())),
                Span::from(" - "),
                Span::from(SlashCommand::Reasoning.description()).style(Style::default().add_modifier(Modifier::DIM)),
            ]),
        ];
        PlainHistoryCell { lines }
    } else if config.model == model {
        PlainHistoryCell { lines: Vec::new() }
    } else {
        let lines = vec![
            Line::from("model changed:".magenta().bold()),
            Line::from(format!("requested: {}", config.model)),
            Line::from(format!("used: {model}")),
            // No empty line at end - trimming and spacing handled by renderer
        ];
        PlainHistoryCell { lines }
    }
}

pub(crate) fn new_user_prompt(message: String) -> PlainHistoryCell {
    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(Line::from("user"));
    lines.extend(message.lines().map(|l| Line::from(l.to_string())));
    // No empty line at end - trimming and spacing handled by renderer
    PlainHistoryCell { lines }
}

#[allow(dead_code)]
pub(crate) fn new_text_line(line: Line<'static>) -> PlainHistoryCell {
    PlainHistoryCell { lines: vec![line] }
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

fn action_from_parsed(parsed_commands: &[ParsedCommand]) -> &'static str {
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
                "list" => "Listing...".to_string(),
                _ => match &ctx_path {
                    Some(p) => format!("Running... in {p}"),
                    None => "Running...".to_string(),
                },
            };
            Line::styled(
                format!("{}{}", header, duration_str),
                Style::default().fg(crate::colors::primary()).add_modifier(Modifier::BOLD),
            )
        }
        Some(o) if o.exit_code == 0 => {
            let done = match action {
                "read" => "Read".to_string(),
                "search" => "Searched".to_string(),
                "list" => "Listed".to_string(),
                _ => match &ctx_path {
                    Some(p) => format!("Ran in {p}"),
                    None => "Ran".to_string(),
                },
            };
            Line::styled(
                done,
                Style::default().fg(crate::colors::success()).add_modifier(Modifier::BOLD),
            )
        }
        Some(o) => Line::styled(
            format!("Error (exit {})", o.exit_code),
            Style::default().fg(crate::colors::error()).add_modifier(Modifier::BOLD),
        ),
    }];

    for (i, parsed) in parsed_commands.iter().enumerate() {
        let text = match parsed {
            ParsedCommand::Read { name, cmd, .. } => {
                if let Some(ann) = parse_read_line_annotation(cmd) {
                    format!("📖 {name} {ann}")
                } else {
                    format!("📖 {name}")
                }
            }
            ParsedCommand::ListFiles { cmd, path } => match path {
                Some(p) => format!("📂 {p}"),
                None => format!("📂 {}", cmd),
            },
            ParsedCommand::Search { query, path, cmd } => match (query, path) {
                (Some(q), Some(p)) => format!("🔎 {q} in {p}"),
                (Some(q), None) => format!("🔎 {q}"),
                (None, Some(p)) => format!("🔎 {p}"),
                (None, None) => format!("🔎 {}", cmd),
            },
            ParsedCommand::Format { .. } => "✨ Formatting".to_string(),
            ParsedCommand::Test { cmd } => format!("🧪 {}", cmd),
            ParsedCommand::Lint { cmd, .. } => format!("🧹 {}", cmd),
            ParsedCommand::Unknown { cmd } => format!("⌨️ {}", cmd),
            ParsedCommand::Noop { .. } => continue, // Skip noop commands
        };

        let first_prefix = if i == 0 { "  └ " } else { "    " };
        for (j, line_text) in text.lines().enumerate() {
            let prefix = if j == 0 { first_prefix } else { "    " };
            lines.push(Line::from(vec![
                Span::styled(prefix, Style::default().add_modifier(Modifier::DIM)),
                Span::styled(line_text.to_string(), Style::default().fg(crate::colors::text_dim())),
            ]));
        }
    }

    lines.extend(output_lines(output, true, false));
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
            Style::default().fg(crate::colors::success()).add_modifier(Modifier::BOLD),
        ),
        Some(o) => Line::styled(
            format!("Error (exit {})", o.exit_code),
            Style::default().fg(crate::colors::error()).add_modifier(Modifier::BOLD),
        ),
    };

    if let Some(first) = cmd_lines.next() {
        let duration_str = if output.is_none() && start_time.is_some() {
            let elapsed = start_time.unwrap().elapsed();
            format!(" ({})", format_duration(elapsed))
        } else {
            String::new()
        };

        lines.push(header_line.clone());
        lines.push(Line::from(vec![first.to_string().into(), duration_str.dim()]));
    } else {
        lines.push(header_line);
    }
    
    for cont in cmd_lines {
        lines.push(Line::from(cont.to_string()));
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

pub(crate) fn new_completed_custom_tool_call(
    tool_name: String,
    args: Option<String>,
    duration: Duration,
    success: bool,
    result: String,
) -> ToolCallCell {
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
    PlainHistoryCell { lines }
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
    PlainHistoryCell { lines }
}

pub(crate) fn new_reasoning_output(reasoning_effort: &ReasoningEffort) -> PlainHistoryCell {
    let lines = vec![
        Line::from(""),
        Line::from("Reasoning Effort".magenta().bold()),
        Line::from(format!("Current: {}", reasoning_effort)),
        Line::from(""),
    ];
    PlainHistoryCell { lines }
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
    
    PlainHistoryCell { lines }
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
    PlainHistoryCell { lines }
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
    
    let mut header: Vec<Span> = Vec::new();
    header.push(Span::raw("📋"));
    header.push(Span::styled(
        " Update plan",
        Style::default().add_modifier(Modifier::BOLD).magenta(),
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
                Span::raw("  └ ")
            } else {
                Span::raw("    ")
            };
            lines.push(Line::from(vec![
                prefix,
                box_span,
                Span::raw(" "),
                text_span,
            ]));
        }
    }
    
    PlainHistoryCell { lines }
}

pub(crate) fn new_patch_event(
    event_type: PatchEventType,
    changes: HashMap<PathBuf, FileChange>,
) -> PlainHistoryCell {
    let title = match event_type {
        PatchEventType::ApprovalRequest => "proposed patch",
        PatchEventType::ApplyBegin { auto_approved: true } => "✏️ Applying patch",
        PatchEventType::ApplyBegin { auto_approved: false } => "✏️ Applying approved patch",
    };

    let lines: Vec<Line<'static>> = create_diff_summary(title, &changes, event_type)
        .into_iter()
        .collect();
    PlainHistoryCell { lines }
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
    PlainHistoryCell { lines }
}

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
    
    // Check for common title patterns
    matches!(text.as_str(), 
        "codex" | "user" | "thinking" | "event" | 
        "tool" | "/diff" | "/status" | "/prompts" |
        "reasoning effort" | "error"
    ) || text.starts_with("⚡") || text.starts_with("⚙") || text.starts_with("✓") || text.starts_with("✗") ||
        text.starts_with("📋") || text.starts_with("proposed patch") || text.starts_with("✏️")
}

/// Check if a line is empty (no content or just whitespace)
fn is_empty_line(line: &Line) -> bool {
    line.spans.is_empty() || 
    (line.spans.len() == 1 && line.spans[0].content.trim().is_empty())
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
