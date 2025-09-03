use crossterm::terminal;
// Color type is already in scope at the top of this module
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::style::Stylize;
use ratatui::text::Line as RtLine;
use ratatui::text::Span as RtSpan;
use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use codex_core::protocol::FileChange;

use crate::history_cell::PatchEventType;
use codex_core::git_info::get_git_repo_root;
use codex_core::protocol::FileChange;

// Sanitize diff content so tabs and control characters don’t break terminal layout.
// Mirrors the behavior we use for user input and command output:
// - Expand tabs to spaces using a fixed tab stop (4)
// - Remove ASCII control characters (including ESC/CSI sequences) that could
//   confuse terminal rendering; keep plain text only
fn expand_tabs_to_spaces(input: &str, tabstop: usize) -> String {
    let ts = tabstop.max(1);
    let mut out = String::with_capacity(input.len());
    let mut col = 0usize;
    for ch in input.chars() {
        match ch {
            '\t' => {
                let spaces = ts - (col % ts);
                out.extend(std::iter::repeat(' ').take(spaces));
                col += spaces;
            }
            _ => {
                // Treat all other chars as width 1 for our fixed-width wrapping pre-pass.
                // The ratatui layer will handle wide glyphs.
                out.push(ch);
                col += 1;
            }
        }
    }
    out
}

fn strip_control_sequences(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\u{001B}' {
            // Skip a simple ESC [...] <alpha> CSI sequence, or generic ESC-seq until letter
            if matches!(chars.peek(), Some('[')) {
                // consume '['
                let _ = chars.next();
                // consume params until we hit an alphabetic final byte or end
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphabetic() { chars.next(); break; }
                    let _ = chars.next();
                }
            } else {
                // Consume until an alphabetic; best‑effort strip of non‑CSI
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphabetic() { let _ = chars.next(); break; }
                    let _ = chars.next();
                }
            }
            continue;
        }
        // Drop other ASCII control characters (0x00..0x1F, 0x7F)
        if (ch as u32) < 0x20 || ch == '\u{007F}' {
            continue;
        }
        out.push(ch);
    }
    out
}

#[inline]
fn sanitize_diff_text(s: &str) -> String {
    // Order: first expand tabs (so control stripping doesn’t accidentally
    // touch spaces we insert), then remove control sequences.
    let expanded = expand_tabs_to_spaces(s, 4);
    strip_control_sequences(&expanded)
}

#[allow(dead_code)]
// Keep one space between the line number and the sign column for typical
// 4‑digit line numbers (e.g., "1235 + "). This value is the total target
// width for "<ln><gap>", so with 4 digits we get 1 space gap.
const SPACES_AFTER_LINE_NUMBER: usize = 6;

// Internal representation for diff line rendering
#[allow(dead_code)]
enum DiffLineType {
    Insert,
    Delete,
    Context,
}

#[allow(dead_code)]
pub(super) fn create_diff_summary(
    title: &str,
    changes: &HashMap<PathBuf, FileChange>,
    event_type: PatchEventType,
    cwd: &Path,
    wrap_cols: usize,
) -> Vec<RtLine<'static>> {
    create_diff_summary_with_width(title, changes, event_type, None)
}

/// Same as `create_diff_summary` but allows specifying a target content width in columns.
/// When `width_cols` is provided, wrapping for detailed diff lines uses that width to
/// ensure hanging indents align within the caller’s render area.
pub(super) fn create_diff_summary_with_width(
    title: &str,
    changes: &HashMap<PathBuf, FileChange>,
    event_type: PatchEventType,
    width_cols: Option<usize>,
) -> Vec<RtLine<'static>> {
    struct FileSummary {
        display_path: String,
        added: usize,
        removed: usize,
    }

    let count_from_unified = |diff: &str| -> (usize, usize) {
        if let Ok(patch) = diffy::Patch::from_str(diff) {
            patch
                .hunks()
                .iter()
                .flat_map(|h| h.lines())
                .fold((0, 0), |(a, d), l| match l {
                    diffy::Line::Insert(_) => (a + 1, d),
                    diffy::Line::Delete(_) => (a, d + 1),
                    _ => (a, d),
                })
        } else {
            // Fallback: manual scan to preserve counts even for unparsable diffs
            let mut adds = 0usize;
            let mut dels = 0usize;
            for l in diff.lines() {
                if l.starts_with("+++") || l.starts_with("---") || l.starts_with("@@") {
                    continue;
                }
                match l.as_bytes().first() {
                    Some(b'+') => adds += 1,
                    Some(b'-') => dels += 1,
                    _ => {}
                }
            }
        }
        PatchEventType::ApprovalRequest => HeaderKind::ProposedChange,
    };
    render_changes_block(rows, wrap_cols, header_kind, cwd)
}

// Shared row for per-file presentation
#[derive(Clone)]
struct Row {
    #[allow(dead_code)]
    path: PathBuf,
    move_path: Option<PathBuf>,
    added: usize,
    removed: usize,
    change: FileChange,
}

fn collect_rows(changes: &HashMap<PathBuf, FileChange>) -> Vec<Row> {
    let mut rows: Vec<Row> = Vec::new();
    for (path, change) in changes.iter() {
        let (added, removed) = match change {
            FileChange::Add { content } => (content.lines().count(), 0),
            FileChange::Delete { content } => (0, content.lines().count()),
            FileChange::Update { unified_diff, .. } => calculate_add_remove_from_diff(unified_diff),
        };
        let move_path = match change {
            FileChange::Update {
                move_path: Some(new),
                ..
            } => Some(new.clone()),
            _ => None,
        };
        rows.push(Row {
            path: path.clone(),
            move_path,
            added,
            removed,
            change: change.clone(),
        });
    }
    rows.sort_by_key(|r| r.path.clone());
    rows
}

enum HeaderKind {
    ProposedChange,
    Edited,
    ChangeApproved,
}

fn render_changes_block(
    rows: Vec<Row>,
    wrap_cols: usize,
    header_kind: HeaderKind,
    cwd: &Path,
) -> Vec<RtLine<'static>> {
    let mut out: Vec<RtLine<'static>> = Vec::new();
    let term_cols = wrap_cols;

    fn render_line_count_summary(added: usize, removed: usize) -> Vec<RtSpan<'static>> {
        let mut spans = Vec::new();
        spans.push("(".into());
        spans.push(format!("+{added}").green());
        spans.push(" ".into());
        spans.push(format!("-{removed}").red());
        spans.push(")".into());
        spans
    }

    // Header
    let mut header_spans: Vec<RtSpan<'static>> = Vec::new();
    // Colorize title: success for apply events, keep primary for approval requests
    let title_style = match event_type {
        PatchEventType::ApplyBegin { .. } => Style::default()
            .fg(crate::colors::success())
            .add_modifier(Modifier::BOLD),
        PatchEventType::ApprovalRequest => Style::default()
            .fg(crate::colors::primary())
            .add_modifier(Modifier::BOLD),
    };
    header_spans.push(RtSpan::styled(title.to_owned(), title_style));
    // Only include aggregate counts in header for approval requests; omit for apply/updated.
    if matches!(event_type, PatchEventType::ApprovalRequest) {
        header_spans.push(RtSpan::raw(" "));
        header_spans.push(RtSpan::raw(format!("{file_count} {noun} ")));
        header_spans.push(RtSpan::raw("("));
        header_spans.push(RtSpan::styled(
            format!("+{total_added}"),
            Style::default().fg(crate::colors::success()),
        ));
        header_spans.push(RtSpan::raw(" "));
        header_spans.push(RtSpan::styled(
            format!("-{total_removed}"),
            Style::default().fg(crate::colors::error()),
        ));
        header_spans.push(RtSpan::raw(")"));
    }
    out.push(RtLine::from(header_spans));

    // Per-file lines with prefix
    for (idx, f) in files.iter().enumerate() {
        let mut spans: Vec<RtSpan<'static>> = Vec::new();
        // Prefix
        let prefix = if idx == 0 { "└ " } else { "  " };
        spans.push(RtSpan::styled(
            prefix.to_string(),
            Style::default().add_modifier(Modifier::DIM),
        ));
        // File path
        spans.push(RtSpan::styled(
            f.display_path.clone(),
            Style::default().fg(crate::colors::text_dim()),
        ));
        // Per-file counts shown inline in chat summary
        spans.push(RtSpan::styled(" (".to_string(), Style::default().fg(crate::colors::text_dim())));
        spans.push(RtSpan::styled(
            format!("+{}", f.added),
            Style::default().fg(crate::colors::success()),
        ));
        spans.push(RtSpan::raw(" "));
        spans.push(RtSpan::styled(
            format!("-{}", f.removed),
            Style::default().fg(crate::colors::error()),
        ));
        spans.push(RtSpan::styled(")".to_string(), Style::default().fg(crate::colors::text_dim())));
        out.push(RtLine::from(spans));
    }
    out.push(RtLine::from(header_spans));

    let show_details = matches!(
        event_type,
        PatchEventType::ApplyBegin {
            auto_approved: true
        } | PatchEventType::ApprovalRequest
    );

    if show_details {
        out.extend(render_patch_details_with_width(changes, width_cols));
    }

    out
}

#[allow(dead_code)]
pub(super) fn render_patch_details(changes: &HashMap<PathBuf, FileChange>) -> Vec<RtLine<'static>> {
    render_patch_details_with_width(changes, None)
}

#[allow(dead_code)]
fn render_patch_details_with_width(
    changes: &HashMap<PathBuf, FileChange>,
    width_cols: Option<usize>,
) -> Vec<RtLine<'static>> {
    let mut out: Vec<RtLine<'static>> = Vec::new();
    // Use caller-provided width or fall back to a conservative estimate based on terminal width.
    // Subtract a gutter safety margin so our pre-wrapping rarely exceeds the
    // actual chat content width (prevents secondary wrapping that breaks hanging indents).
    let term_cols: usize = if let Some(w) = width_cols {
        w as usize
    } else {
        let full = terminal::size().map(|(w, _)| w as usize).unwrap_or(120);
        full.saturating_sub(20).max(40)
    };

    let total_files = changes.len();
    for (index, (path, change)) in changes.iter().enumerate() {
        let is_first_file = index == 0;
        // Add separator only between files (not at the very start)
        if !is_first_file {
            out.push(RtLine::from(vec![
                RtSpan::raw("    "),
                RtSpan::styled("...", style_dim()),
            ]));
        }
        // File header line (skip when single-file header already shows the name)
        let skip_file_header =
            matches!(header_kind, HeaderKind::ProposedChange | HeaderKind::Edited)
                && file_count == 1;
        if !skip_file_header {
            let mut header: Vec<RtSpan<'static>> = Vec::new();
            header.push("  └ ".dim());
            header.extend(render_path(&r));
            header.push(" ".into());
            header.extend(render_line_count_summary(r.added, r.removed));
            out.push(RtLine::from(header));
        }

        match r.change {
            FileChange::Add { content } => {
                for (i, raw) in content.lines().enumerate() {
                    let ln = i + 1;
                    let cleaned = sanitize_diff_text(raw);
                    out.extend(push_wrapped_diff_line_with_width(
                        ln,
                        DiffLineType::Insert,
                        &cleaned,
                        term_cols,
                    ));
                }
            }
            FileChange::Delete => {
                let original = std::fs::read_to_string(path).unwrap_or_default();
                for (i, raw) in original.lines().enumerate() {
                    let ln = i + 1;
                    let cleaned = sanitize_diff_text(raw);
                    out.extend(push_wrapped_diff_line_with_width(
                        ln,
                        DiffLineType::Delete,
                        &cleaned,
                        term_cols,
                    ));
                }
            }
            FileChange::Update { unified_diff, .. } => {
                if let Ok(patch) = diffy::Patch::from_str(&unified_diff) {
                    let mut is_first_hunk = true;
                    for h in patch.hunks() {
                        if !is_first_hunk {
                            out.push(RtLine::from(vec!["    ".into(), "⋮".dim()]));
                        }
                        is_first_hunk = false;

                        let mut old_ln = h.old_range().start();
                        let mut new_ln = h.new_range().start();
                        for l in h.lines() {
                            match l {
                                diffy::Line::Insert(text) => {
                                    let s = sanitize_diff_text(text.trim_end_matches('\n'));
                    out.extend(push_wrapped_diff_line_with_width(
                        new_ln,
                        DiffLineType::Insert,
                        &s,
                        term_cols,
                    ));
                                    new_ln += 1;
                                }
                                diffy::Line::Delete(text) => {
                                    let s = sanitize_diff_text(text.trim_end_matches('\n'));
                    out.extend(push_wrapped_diff_line_with_width(
                        old_ln,
                        DiffLineType::Delete,
                        &s,
                        term_cols,
                    ));
                                    old_ln += 1;
                                }
                                diffy::Line::Context(text) => {
                                    let s = sanitize_diff_text(text.trim_end_matches('\n'));
                    out.extend(push_wrapped_diff_line_with_width(
                        new_ln,
                        DiffLineType::Context,
                        &s,
                        term_cols,
                    ));
                                    old_ln += 1;
                                    new_ln += 1;
                                }
                            }
                        }
                    }
                }
            }
        }

        // Avoid trailing blank line at the very end; only add spacing
        // when there are more files following.
        if index + 1 < total_files {
            out.push(RtLine::from(RtSpan::raw("")));
        }
    }

    out
}

/// Produce only the detailed diff lines without any file-level headers/summaries.
/// Used by the Diff Viewer overlay where surrounding chrome already conveys context.
#[allow(dead_code)]
pub(super) fn create_diff_details_only(
    changes: &HashMap<PathBuf, FileChange>,
) -> Vec<RtLine<'static>> {
    render_patch_details(changes)
}

#[allow(dead_code)]
fn push_wrapped_diff_line_with_width(
    line_number: usize,
    kind: DiffLineType,
    text: &str,
    term_cols: usize,
) -> Vec<RtLine<'static>> {
    // Slightly smaller left padding so line numbers sit a couple of spaces left
    let indent = "  ";
    let ln_str = line_number.to_string();
    let mut remaining_text: &str = text;

    // Reserve a fixed number of spaces after the line number so that content starts
    // at a consistent column. Always include a 1‑char diff sign ("+"/"-" or space)
    // at the start of the content so gutters align across wrapped lines.
    let gap_after_ln = SPACES_AFTER_LINE_NUMBER.saturating_sub(ln_str.len());
    let prefix_cols = indent.len() + ln_str.len() + gap_after_ln;

    let mut first = true;
    // Continuation hanging indent equals the leading spaces of the content
    // (after the diff sign). This keeps wrapped rows aligned under the code
    // indentation.
    let continuation_indent: usize = text.chars().take_while(|c| *c == ' ').count();
    let (sign_opt, line_style) = match kind {
        DiffLineType::Insert => (Some('+'), Some(style_add())),
        DiffLineType::Delete => (Some('-'), Some(style_del())),
        DiffLineType::Context => (None, None),
    };
    let mut lines: Vec<RtLine<'static>> = Vec::new();

    loop {
        // Fit the content for the current terminal row:
        // compute how many columns are available after the prefix, then split
        // at a UTF-8 character boundary so this row's chunk fits exactly.
        // First line includes a visible sign plus a trailing space after it.
        // Continuation lines include only the hanging space (no sign).
        // First line reserves 1 col for the sign ('+'/'-') and 1 space after it.
        // Continuation lines must reserve BOTH columns as well (sign column + its trailing space)
        // before applying the hanging indent equal to the content's leading spaces.
        let base_prefix = if first { prefix_cols + 2 } else { prefix_cols + 2 + continuation_indent };
        let available_content_cols = term_cols
            .saturating_sub(base_prefix)
            .max(1);
        let split_at_byte_index = remaining_text
            .char_indices()
            .nth(available_content_cols)
            .map(|(i, _)| i)
            .unwrap_or_else(|| remaining_text.len());
        let (chunk, rest) = remaining_text.split_at(split_at_byte_index);
        remaining_text = rest;

        if first {
            let mut spans: Vec<RtSpan<'static>> = Vec::new();
            spans.push(RtSpan::raw(indent));
            spans.push(RtSpan::styled(ln_str.clone(), style_dim()));
            spans.push(RtSpan::raw(" ".repeat(gap_after_ln)));

            // Always prefix the content with a sign char for consistent gutters
            let sign_char = sign_opt.unwrap_or(' ');
            // Add a space after the sign so it sits centered in the sign column
            // and content starts one cell to the right: "+ <content>".
            let display_chunk = format!("{sign_char} {chunk}");

            let content_span = match line_style {
                Some(style) => RtSpan::styled(display_chunk, style),
                None => RtSpan::raw(display_chunk),
            };
            spans.push(content_span);
            let mut line = RtLine::from(spans);
            if let Some(style) = line_style {
                line.style = line.style.patch(style);
            }
            // Apply themed tinted background for added/removed lines
            if matches!(kind, DiffLineType::Insert | DiffLineType::Delete) {
                let tint = match kind {
                    DiffLineType::Insert => success_tint(),
                    DiffLineType::Delete => error_tint(),
                    DiffLineType::Context => crate::colors::background(),
                };
                line.style = line.style.bg(tint);
            }
            lines.push(line);
            first = false;
        } else {
            // Continuation lines keep a space for the sign column so content aligns
            let hang_prefix = format!(
                "{indent}{}{}  {}",
                " ".repeat(ln_str.len()),
                " ".repeat(gap_after_ln),
                " ".repeat(continuation_indent)
            );
            let content_span = match line_style {
                Some(style) => RtSpan::styled(chunk.to_string(), style),
                None => RtSpan::raw(chunk.to_string()),
            };
            let mut line = RtLine::from(vec![RtSpan::raw(hang_prefix), content_span]);
            if let Some(style) = line_style {
                line.style = line.style.patch(style);
            }
            if matches!(kind, DiffLineType::Insert | DiffLineType::Delete) {
                let tint = match kind {
                    DiffLineType::Insert => success_tint(),
                    DiffLineType::Delete => error_tint(),
                    DiffLineType::Context => crate::colors::background(),
                };
                line.style = line.style.bg(tint);
            }
            lines.push(line);
        }
        if remaining_text.is_empty() {
            break;
        }
    }
    lines
}

#[allow(dead_code)]
fn style_dim() -> Style {
    Style::default().add_modifier(Modifier::DIM)
}

#[allow(dead_code)]
fn style_add() -> Style {
    // Use theme success color for additions so it adapts to light/dark themes
    Style::default().fg(crate::colors::success())
}

#[allow(dead_code)]
fn style_del() -> Style {
    // Use theme error color for deletions so it adapts to light/dark themes
    Style::default().fg(crate::colors::error())
}

// --- Very light tinted backgrounds for insert/delete lines ------------------
use ratatui::style::Color;

fn color_to_rgb(c: Color) -> (u8, u8, u8) {
    match c {
        Color::Rgb(r, g, b) => (r, g, b),
        Color::Black => (0, 0, 0),
        Color::White => (255, 255, 255),
        Color::Gray => (192, 192, 192),
        Color::DarkGray => (128, 128, 128),
        Color::Red => (205, 49, 49),
        Color::Green => (13, 188, 121),
        Color::Yellow => (229, 229, 16),
        Color::Blue => (36, 114, 200),
        Color::Magenta => (188, 63, 188),
        Color::Cyan => (17, 168, 205),
        Color::LightRed => (255, 102, 102),
        Color::LightGreen => (102, 255, 178),
        Color::LightYellow => (255, 255, 102),
        Color::LightBlue => (102, 153, 255),
        Color::LightMagenta => (255, 102, 255),
        Color::LightCyan => (102, 255, 255),
        Color::Indexed(i) => (i, i, i),
        Color::Reset => color_to_rgb(crate::colors::background()),
    }
}

fn blend(bg: (u8, u8, u8), fg: (u8, u8, u8), alpha: f32) -> (u8, u8, u8) {
    let inv = 1.0 - alpha;
    let r = (bg.0 as f32 * inv + fg.0 as f32 * alpha).round() as u8;
    let g = (bg.1 as f32 * inv + fg.1 as f32 * alpha).round() as u8;
    let b = (bg.2 as f32 * inv + fg.2 as f32 * alpha).round() as u8;
    (r, g, b)
}

fn is_dark(rgb: (u8, u8, u8)) -> bool {
    let l = (0.2126 * rgb.0 as f32 + 0.7152 * rgb.1 as f32 + 0.0722 * rgb.2 as f32) / 255.0;
    l < 0.55
}

fn tinted_bg_toward(accent: Color) -> Color {
    let bg = color_to_rgb(crate::colors::background());
    let fg = color_to_rgb(accent);
    // Slightly stronger tint on dark themes, lighter on light themes
    let alpha = if is_dark(bg) { 0.20 } else { 0.10 };
    let (r, g, b) = blend(bg, fg, alpha);
    Color::Rgb(r, g, b)
}

fn success_tint() -> Color { tinted_bg_toward(crate::colors::success()) }
fn error_tint() -> Color { tinted_bg_toward(crate::colors::error()) }

// Removed per-line tinted backgrounds per design feedback

#[allow(clippy::expect_used)]
#[cfg(all(test, feature = "legacy_tests"))]
mod tests {
    use super::*;
    use insta::assert_snapshot;
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    use ratatui::text::Text;
    use ratatui::widgets::Paragraph;
    use ratatui::widgets::WidgetRef;
    use ratatui::widgets::Wrap;
    fn diff_summary_for_tests(
        changes: &HashMap<PathBuf, FileChange>,
        event_type: PatchEventType,
    ) -> Vec<RtLine<'static>> {
        create_diff_summary(changes, event_type, &PathBuf::from("/"), 80)
    }

    fn snapshot_lines(name: &str, lines: Vec<RtLine<'static>>, width: u16, height: u16) {
        let mut terminal = Terminal::new(TestBackend::new(width, height)).expect("terminal");
        terminal
            .draw(|f| {
                Paragraph::new(Text::from(lines))
                    .wrap(Wrap { trim: false })
                    .render_ref(f.area(), f.buffer_mut())
            })
            .expect("draw");
        assert_snapshot!(name, terminal.backend());
    }

    fn snapshot_lines_text(name: &str, lines: &[RtLine<'static>]) {
        // Convert Lines to plain text rows and trim trailing spaces so it's
        // easier to validate indentation visually in snapshots.
        let text = lines
            .iter()
            .map(|l| {
                l.spans
                    .iter()
                    .map(|s| s.content.as_ref())
                    .collect::<String>()
            })
            .map(|s| s.trim_end().to_string())
            .collect::<Vec<_>>()
            .join("\n");
        assert_snapshot!(name, text);
    }

    #[test]
    fn ui_snapshot_add_details() {
        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            PathBuf::from("README.md"),
            FileChange::Add {
                content: "first line\nsecond line\n".to_string(),
            },
        );

        let lines = diff_summary_for_tests(&changes, PatchEventType::ApprovalRequest);

        snapshot_lines("add_details", lines, 80, 10);
    }

    #[test]
    fn ui_snapshot_update_details_with_rename() {
        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();

        let original = "line one\nline two\nline three\n";
        let modified = "line one\nline two changed\nline three\n";
        let patch = diffy::create_patch(original, modified).to_string();

        changes.insert(
            PathBuf::from("src/lib.rs"),
            FileChange::Update {
                unified_diff: patch,
                move_path: Some(PathBuf::from("src/lib_new.rs")),
            },
        );

        let lines = diff_summary_for_tests(&changes, PatchEventType::ApprovalRequest);

        snapshot_lines("update_details_with_rename", lines, 80, 12);
    }

    #[test]
    fn ui_snapshot_wrap_behavior_insert() {
        // Narrow width to force wrapping within our diff line rendering
        let long_line = "this is a very long line that should wrap across multiple terminal columns and continue";

        // Call the wrapping function directly so we can precisely control the width
        // Use a fixed width for testing wrapping behavior
        const TEST_WRAP_WIDTH: usize = 80;
        let lines = push_wrapped_diff_line_with_width(1, DiffLineType::Insert, long_line, TEST_WRAP_WIDTH);

        // Render into a small terminal to capture the visual layout
        snapshot_lines(
            "wrap_behavior_insert",
            lines,
            (TEST_WRAP_WIDTH + 10) as u16,
            8,
        );
    }

    #[test]
    fn ui_snapshot_single_line_replacement_counts() {
        // Reproduce: one deleted line replaced by one inserted line, no extra context
        let original = "# Codex CLI (Rust Implementation)\n";
        let modified = "# Codex CLI (Rust Implementation) banana\n";
        let patch = diffy::create_patch(original, modified).to_string();

        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            PathBuf::from("README.md"),
            FileChange::Update {
                unified_diff: patch,
                move_path: None,
            },
        );

        let lines = diff_summary_for_tests(&changes, PatchEventType::ApprovalRequest);

        snapshot_lines("single_line_replacement_counts", lines, 80, 8);
    }

    #[test]
    fn ui_snapshot_blank_context_line() {
        // Ensure a hunk that includes a blank context line at the beginning is rendered visibly
        let original = "\nY\n";
        let modified = "\nY changed\n";
        let patch = diffy::create_patch(original, modified).to_string();

        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            PathBuf::from("example.txt"),
            FileChange::Update {
                unified_diff: patch,
                move_path: None,
            },
        );

        let lines = diff_summary_for_tests(&changes, PatchEventType::ApprovalRequest);

        snapshot_lines("blank_context_line", lines, 80, 10);
    }

    #[test]
    fn ui_snapshot_vertical_ellipsis_between_hunks() {
        // Create a patch with two separate hunks to ensure we render the vertical ellipsis (⋮)
        let original =
            "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9\nline 10\n";
        let modified = "line 1\nline two changed\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline nine changed\nline 10\n";
        let patch = diffy::create_patch(original, modified).to_string();

        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            PathBuf::from("example.txt"),
            FileChange::Update {
                unified_diff: patch,
                move_path: None,
            },
        );

        let lines = diff_summary_for_tests(&changes, PatchEventType::ApprovalRequest);

        // Height is large enough to show both hunks and the separator
        snapshot_lines("vertical_ellipsis_between_hunks", lines, 80, 16);
    }

    #[test]
    fn ui_snapshot_apply_update_block() {
        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        let original = "line one\nline two\nline three\n";
        let modified = "line one\nline two changed\nline three\n";
        let patch = diffy::create_patch(original, modified).to_string();

        changes.insert(
            PathBuf::from("example.txt"),
            FileChange::Update {
                unified_diff: patch,
                move_path: None,
            },
        );

        for (name, auto_approved) in [
            ("apply_update_block", true),
            ("apply_update_block_manual", false),
        ] {
            let lines =
                diff_summary_for_tests(&changes, PatchEventType::ApplyBegin { auto_approved });

            snapshot_lines(name, lines, 80, 12);
        }
    }

    #[test]
    fn ui_snapshot_apply_update_with_rename_block() {
        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        let original = "A\nB\nC\n";
        let modified = "A\nB changed\nC\n";
        let patch = diffy::create_patch(original, modified).to_string();

        changes.insert(
            PathBuf::from("old_name.rs"),
            FileChange::Update {
                unified_diff: patch,
                move_path: Some(PathBuf::from("new_name.rs")),
            },
        );

        let lines = diff_summary_for_tests(
            &changes,
            PatchEventType::ApplyBegin {
                auto_approved: true,
            },
        );

        snapshot_lines("apply_update_with_rename_block", lines, 80, 12);
    }

    #[test]
    fn ui_snapshot_apply_multiple_files_block() {
        // Two files: one update and one add, to exercise combined header and per-file rows
        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();

        // File a.txt: single-line replacement (one delete, one insert)
        let patch_a = diffy::create_patch("one\n", "one changed\n").to_string();
        changes.insert(
            PathBuf::from("a.txt"),
            FileChange::Update {
                unified_diff: patch_a,
                move_path: None,
            },
        );

        // File b.txt: newly added with one line
        changes.insert(
            PathBuf::from("b.txt"),
            FileChange::Add {
                content: "new\n".to_string(),
            },
        );

        let lines = diff_summary_for_tests(
            &changes,
            PatchEventType::ApplyBegin {
                auto_approved: true,
            },
        );

        snapshot_lines("apply_multiple_files_block", lines, 80, 14);
    }

    #[test]
    fn ui_snapshot_apply_add_block() {
        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            PathBuf::from("new_file.txt"),
            FileChange::Add {
                content: "alpha\nbeta\n".to_string(),
            },
        );

        let lines = diff_summary_for_tests(
            &changes,
            PatchEventType::ApplyBegin {
                auto_approved: true,
            },
        );

        snapshot_lines("apply_add_block", lines, 80, 10);
    }

    #[test]
    fn ui_snapshot_apply_delete_block() {
        // Write a temporary file so the delete renderer can read original content
        let tmp_path = PathBuf::from("tmp_delete_example.txt");
        std::fs::write(&tmp_path, "first\nsecond\nthird\n").expect("write tmp file");

        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            tmp_path.clone(),
            FileChange::Delete {
                content: "first\nsecond\nthird\n".to_string(),
            },
        );

        let lines = diff_summary_for_tests(
            &changes,
            PatchEventType::ApplyBegin {
                auto_approved: true,
            },
        );

        // Cleanup best-effort; rendering has already read the file
        let _ = std::fs::remove_file(&tmp_path);

        snapshot_lines("apply_delete_block", lines, 80, 12);
    }

    #[test]
    fn ui_snapshot_apply_update_block_wraps_long_lines() {
        // Create a patch with a long modified line to force wrapping
        let original = "line 1\nshort\nline 3\n";
        let modified = "line 1\nshort this_is_a_very_long_modified_line_that_should_wrap_across_multiple_terminal_columns_and_continue_even_further_beyond_eighty_columns_to_force_multiple_wraps\nline 3\n";
        let patch = diffy::create_patch(original, modified).to_string();

        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            PathBuf::from("long_example.txt"),
            FileChange::Update {
                unified_diff: patch,
                move_path: None,
            },
        );

        let lines = create_diff_summary(
            &changes,
            PatchEventType::ApplyBegin {
                auto_approved: true,
            },
            &PathBuf::from("/"),
            72,
        );

        // Render with backend width wider than wrap width to avoid Paragraph auto-wrap.
        snapshot_lines("apply_update_block_wraps_long_lines", lines, 80, 12);
    }

    #[test]
    fn ui_snapshot_apply_update_block_wraps_long_lines_text() {
        // This mirrors the desired layout example: sign only on first inserted line,
        // subsequent wrapped pieces start aligned under the line number gutter.
        let original = "1\n2\n3\n4\n";
        let modified = "1\nadded long line which wraps and_if_there_is_a_long_token_it_will_be_broken\n3\n4 context line which also wraps across\n";
        let patch = diffy::create_patch(original, modified).to_string();

        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            PathBuf::from("wrap_demo.txt"),
            FileChange::Update {
                unified_diff: patch,
                move_path: None,
            },
        );

        let mut lines = create_diff_summary(
            &changes,
            PatchEventType::ApplyBegin {
                auto_approved: true,
            },
            &PathBuf::from("/"),
            28,
        );
        // Drop the combined header for this text-only snapshot
        if !lines.is_empty() {
            lines.remove(0);
        }
        snapshot_lines_text("apply_update_block_wraps_long_lines_text", &lines);
    }

    #[test]
    fn ui_snapshot_apply_update_block_relativizes_path() {
        let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));
        let abs_old = cwd.join("abs_old.rs");
        let abs_new = cwd.join("abs_new.rs");

        let original = "X\nY\n";
        let modified = "X changed\nY\n";
        let patch = diffy::create_patch(original, modified).to_string();

        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            abs_old.clone(),
            FileChange::Update {
                unified_diff: patch,
                move_path: Some(abs_new.clone()),
            },
        );

        let lines = create_diff_summary(
            &changes,
            PatchEventType::ApplyBegin {
                auto_approved: true,
            },
            &cwd,
            80,
        );

        snapshot_lines("apply_update_block_relativizes_path", lines, 80, 10);
    }
}
