use super::*;
use crate::history::state::{DiffHunk, DiffLine, DiffLineKind, DiffRecord, HistoryId};

pub(crate) struct DiffCell {
    _record: DiffRecord,
    lines: Vec<Line<'static>>,
}

impl DiffCell {
    pub(crate) fn new(record: DiffRecord) -> Self {
        let lines = build_lines(&record);
        Self { _record: record, lines }
    }
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
        let bg = Style::default().bg(crate::colors::background());
        fill_rect(buf, area, Some(' '), bg);

        let marker_col_x = area.x.saturating_add(2);
        let content_x = area.x.saturating_add(4);
        let content_w = area.width.saturating_sub(4);
        let mut cur_y = area.y;
        let end_y = area.y.saturating_add(area.height);

        let classify = |line: &Line<'static>| -> (Option<char>, Line<'static>) {
            if line.spans.is_empty() {
                return (None, line.clone());
            }
            let mut iter = line
                .spans
                .iter()
                .flat_map(|s| s.content.chars())
                .peekable();
            match iter.peek().copied() {
                Some('+') => {
                    let text = line
                        .spans
                        .iter()
                        .map(|s| s.content.as_ref())
                        .collect::<String>();
                    let content = text.chars().skip(1).collect::<String>();
                    let styled = Line::from(content).style(Style::default().fg(crate::colors::success()));
                    (Some('+'), styled)
                }
                Some('-') => {
                    let text = line
                        .spans
                        .iter()
                        .map(|s| s.content.as_ref())
                        .collect::<String>();
                    let content = text.chars().skip(1).collect::<String>();
                    let styled = Line::from(content).style(Style::default().fg(crate::colors::error()));
                    (Some('-'), styled)
                }
                _ => (None, line.clone()),
            }
        };

        'outer: for line in &self.lines {
            let (marker, content_line) = classify(line);
            let text = Text::from(vec![content_line.clone()]);
            let rows: u16 = Paragraph::new(text.clone())
                .wrap(Wrap { trim: false })
                .line_count(content_w)
                .try_into()
                .unwrap_or(0);

            let mut local_skip = 0u16;
            if skip_rows > 0 {
                if skip_rows >= rows {
                    skip_rows -= rows;
                    continue 'outer;
                }
                local_skip = skip_rows;
                skip_rows = 0;
            }

            if cur_y >= end_y {
                break;
            }
            let avail = end_y.saturating_sub(cur_y);
            let draw_h = rows.saturating_sub(local_skip).min(avail);
            if draw_h == 0 {
                continue;
            }

            let content_area = Rect {
                x: content_x,
                y: cur_y,
                width: content_w,
                height: draw_h,
            };
            Paragraph::new(text)
                .block(Block::default().style(bg))
                .wrap(Wrap { trim: false })
                .scroll((local_skip, 0))
                .style(bg)
                .render(content_area, buf);

            if let Some(m) = marker {
                if local_skip == 0 && area.width > 0 {
                    let style = Style::default().fg(if m == '+' {
                        crate::colors::success()
                    } else {
                        crate::colors::error()
                    });
                    buf.set_string(marker_col_x, cur_y, m.to_string(), style);
                }
            }

            cur_y = cur_y.saturating_add(draw_h);
            if cur_y >= end_y {
                break;
            }
        }
    }
}

fn build_lines(record: &DiffRecord) -> Vec<Line<'static>> {
    let mut lines: Vec<Line<'static>> = Vec::new();

    if !record.title.is_empty() {
        lines.push(Line::from(record.title.clone()).fg(crate::colors::primary()));
    }

    for hunk in &record.hunks {
        lines.push(Line::from(hunk.header.clone()).fg(crate::colors::primary()));
        for line in &hunk.lines {
            lines.push(line_to_history(line));
        }
    }

    lines
}

fn line_to_history(line: &DiffLine) -> Line<'static> {
    match line.kind {
        DiffLineKind::Addition => Line::from(format!("+{}", line.content))
            .fg(crate::colors::success()),
        DiffLineKind::Removal => Line::from(format!("-{}", line.content))
            .fg(crate::colors::error()),
        DiffLineKind::Context => Line::from(line.content.clone()),
    }
}

pub(crate) fn diff_record_from_string(title: String, diff: &str) -> DiffRecord {
    let mut hunks: Vec<DiffHunk> = Vec::new();
    let mut current_header: Option<String> = None;
    let mut current_lines: Vec<DiffLine> = Vec::new();

    let flush_hunk = |header: Option<String>, lines: Vec<DiffLine>, hunks: &mut Vec<DiffHunk>| {
        if let Some(header) = header {
            hunks.push(DiffHunk { header, lines });
        } else if !lines.is_empty() {
            hunks.push(DiffHunk {
                header: String::new(),
                lines,
            });
        }
    };

    for raw_line in diff.lines() {
        if raw_line.starts_with("@@") {
            let prev_lines = std::mem::take(&mut current_lines);
            flush_hunk(current_header.take(), prev_lines, &mut hunks);
            current_header = Some(raw_line.to_string());
            continue;
        }

        let (kind, content) = if let Some(rest) = raw_line.strip_prefix('+') {
            (DiffLineKind::Addition, rest.to_string())
        } else if let Some(rest) = raw_line.strip_prefix('-') {
            (DiffLineKind::Removal, rest.to_string())
        } else {
            (DiffLineKind::Context, raw_line.to_string())
        };
        current_lines.push(DiffLine { kind, content });
    }

    flush_hunk(current_header.take(), current_lines, &mut hunks);

    DiffRecord {
        id: HistoryId::ZERO,
        title,
        hunks,
    }
}

pub(crate) fn new_diff_cell_from_string(diff_output: String) -> DiffCell {
    let record = diff_record_from_string(String::new(), &diff_output);
    DiffCell::new(record)
}
