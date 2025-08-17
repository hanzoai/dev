use crossterm::terminal;
// Color type is already in scope at the top of this module
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::text::Line as RtLine;
use ratatui::text::Span as RtSpan;
use std::collections::HashMap;
use std::path::PathBuf;

use codex_core::protocol::FileChange;

use crate::history_cell::PatchEventType;

#[allow(dead_code)]
const SPACES_AFTER_LINE_NUMBER: usize = 4;

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
            (adds, dels)
        }
    };

    let mut files: Vec<FileSummary> = Vec::new();
    for (path, change) in changes.iter() {
        match change {
            FileChange::Add { content } => files.push(FileSummary {
                display_path: path.display().to_string(),
                added: content.lines().count(),
                removed: 0,
            }),
            FileChange::Delete => files.push(FileSummary {
                display_path: path.display().to_string(),
                added: 0,
                removed: std::fs::read_to_string(path)
                    .ok()
                    .map(|s| s.lines().count())
                    .unwrap_or(0),
            }),
            FileChange::Update {
                unified_diff,
                move_path,
            } => {
                let (added, removed) = count_from_unified(unified_diff);
                let display_path = if let Some(new_path) = move_path {
                    format!("{} → {}", path.display(), new_path.display())
                } else {
                    path.display().to_string()
                };
                files.push(FileSummary {
                    display_path,
                    added,
                    removed,
                });
            }
        }
    }

    let file_count = files.len();
    let total_added: usize = files.iter().map(|f| f.added).sum();
    let total_removed: usize = files.iter().map(|f| f.removed).sum();
    let noun = if file_count == 1 { "file" } else { "files" };

    let mut out: Vec<RtLine<'static>> = Vec::new();

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
        // Counts
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

    let show_details = matches!(
        event_type,
        PatchEventType::ApplyBegin {
            auto_approved: true
        } | PatchEventType::ApprovalRequest
    );

    if show_details {
        out.extend(render_patch_details(changes));
    }

    out
}

#[allow(dead_code)]
fn render_patch_details(changes: &HashMap<PathBuf, FileChange>) -> Vec<RtLine<'static>> {
    let mut out: Vec<RtLine<'static>> = Vec::new();
    // Use terminal width or a reasonable fallback
    let term_cols: usize = terminal::size().map(|(w, _)| w as usize).unwrap_or(120);

    for (index, (path, change)) in changes.iter().enumerate() {
        let is_first_file = index == 0;
        // Add separator only between files (not at the very start)
        if !is_first_file {
            out.push(RtLine::from(vec![
                RtSpan::raw("    "),
                RtSpan::styled("...", style_dim()),
            ]));
        }
        match change {
            FileChange::Add { content } => {
                for (i, raw) in content.lines().enumerate() {
                    let ln = i + 1;
                    out.extend(push_wrapped_diff_line(
                        ln,
                        DiffLineType::Insert,
                        raw,
                        term_cols,
                    ));
                }
            }
            FileChange::Delete => {
                let original = std::fs::read_to_string(path).unwrap_or_default();
                for (i, raw) in original.lines().enumerate() {
                    let ln = i + 1;
                    out.extend(push_wrapped_diff_line(
                        ln,
                        DiffLineType::Delete,
                        raw,
                        term_cols,
                    ));
                }
            }
            FileChange::Update {
                unified_diff,
                move_path: _,
            } => {
                if let Ok(patch) = diffy::Patch::from_str(unified_diff) {
                    for h in patch.hunks() {
                        let mut old_ln = h.old_range().start();
                        let mut new_ln = h.new_range().start();
                        for l in h.lines() {
                            match l {
                                diffy::Line::Insert(text) => {
                                    let s = text.trim_end_matches('\n');
                                    out.extend(push_wrapped_diff_line(
                                        new_ln,
                                        DiffLineType::Insert,
                                        s,
                                        term_cols,
                                    ));
                                    new_ln += 1;
                                }
                                diffy::Line::Delete(text) => {
                                    let s = text.trim_end_matches('\n');
                                    out.extend(push_wrapped_diff_line(
                                        old_ln,
                                        DiffLineType::Delete,
                                        s,
                                        term_cols,
                                    ));
                                    old_ln += 1;
                                }
                                diffy::Line::Context(text) => {
                                    let s = text.trim_end_matches('\n');
                                    out.extend(push_wrapped_diff_line(
                                        new_ln,
                                        DiffLineType::Context,
                                        s,
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

        out.push(RtLine::from(RtSpan::raw("")));
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
fn push_wrapped_diff_line(
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
    // at a consistent column. The sign ("+"/"-") is rendered as part of the content
    // and colored with the same foreground as the edited text, not as a separate
    // dimmed column.
    let gap_after_ln = SPACES_AFTER_LINE_NUMBER.saturating_sub(ln_str.len());
    let first_prefix_cols = indent.len() + ln_str.len() + gap_after_ln;
    let cont_prefix_cols = indent.len() + ln_str.len() + gap_after_ln;

    let mut first = true;
    let (sign_opt, line_style) = match kind { 
        DiffLineType::Insert => (Some('+'), Some(style_add())),
        DiffLineType::Delete => (Some('-'), Some(style_del())),
        DiffLineType::Context => (None, None),
    };
    let mut lines: Vec<RtLine<'static>> = Vec::new();
    while !remaining_text.is_empty() {
        let prefix_cols = if first {
            first_prefix_cols
        } else {
            cont_prefix_cols
        };
        // Fit the content for the current terminal row:
        // compute how many columns are available after the prefix, then split
        // at a UTF-8 character boundary so this row's chunk fits exactly.
        let available_content_cols = term_cols.saturating_sub(prefix_cols).max(1);
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

            // Prefix the content with the sign if it is an insertion or deletion, and color
            // the sign and content with the same foreground as the edited text.
            let display_chunk = match sign_opt {
                Some(sign_char) => format!("{sign_char}{chunk}"),
                None => chunk.to_string(),
            };

            let content_span = match line_style {
                Some(style) => RtSpan::styled(display_chunk, style),
                None => RtSpan::raw(display_chunk),
            };
            spans.push(content_span);
            let mut line = RtLine::from(spans);
            if let Some(style) = line_style {
                line.style = line.style.patch(style);
            }
            // Apply a very light background tint for added/removed lines for readability
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
            let hang_prefix = format!(
                "{indent}{}{}",
                " ".repeat(ln_str.len()),
                " ".repeat(gap_after_ln)
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
#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    use ratatui::text::Text;
    use ratatui::widgets::Paragraph;
    use ratatui::widgets::WidgetRef;
    use ratatui::widgets::Wrap;

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

    #[test]
    fn ui_snapshot_add_details() {
        let mut changes: HashMap<PathBuf, FileChange> = HashMap::new();
        changes.insert(
            PathBuf::from("README.md"),
            FileChange::Add {
                content: "first line\nsecond line\n".to_string(),
            },
        );

        let lines =
            create_diff_summary("proposed patch", &changes, PatchEventType::ApprovalRequest);

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

        let lines =
            create_diff_summary("proposed patch", &changes, PatchEventType::ApprovalRequest);

        snapshot_lines("update_details_with_rename", lines, 80, 12);
    }

    #[test]
    fn ui_snapshot_wrap_behavior_insert() {
        // Narrow width to force wrapping within our diff line rendering
        let long_line = "this is a very long line that should wrap across multiple terminal columns and continue";

        // Call the wrapping function directly so we can precisely control the width
        // Use a fixed width for testing wrapping behavior
        const TEST_WRAP_WIDTH: usize = 80;
        let lines = push_wrapped_diff_line(1, DiffLineType::Insert, long_line, TEST_WRAP_WIDTH);

        // Render into a small terminal to capture the visual layout
        snapshot_lines(
            "wrap_behavior_insert",
            lines,
            (TEST_WRAP_WIDTH + 10) as u16,
            8,
        );
    }
}
