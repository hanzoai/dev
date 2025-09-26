use super::*;
use crate::history::state::{DiffHunk, DiffLine, DiffLineKind, DiffRecord, HistoryId};
use crate::insert_history::word_wrap_lines;

pub(crate) struct DiffCell {
    record: DiffRecord,
    layout: std::cell::RefCell<Option<DiffLayoutCache>>,
}

impl DiffCell {
    pub(crate) fn from_record(record: DiffRecord) -> Self {
        Self {
            record,
            layout: std::cell::RefCell::new(None),
        }
    }

    pub(crate) fn record(&self) -> &DiffRecord {
        &self.record
    }

    pub(crate) fn record_mut(&mut self) -> &mut DiffRecord {
        &mut self.record
    }

    pub(crate) fn rebuild_with_theme(&self) {
        self.layout.borrow_mut().take();
    }

    fn ensure_layout(&self, width: u16) -> DiffLayoutCache {
        if let Some(cache) = self.layout.borrow().as_ref() {
            if cache.width == width {
                return cache.clone();
            }
        }

        let cache = DiffLayoutCache::build(&self.record, width);
        *self.layout.borrow_mut() = Some(cache.clone());
        cache
    }

    fn display_lines_from_record(&self) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();

        if !self.record.title.is_empty() {
            lines.push(Line::from(self.record.title.clone()).fg(crate::colors::primary()));
        }

        for hunk in &self.record.hunks {
            if !hunk.header.is_empty() {
                lines.push(Line::from(hunk.header.clone()).fg(crate::colors::primary()));
            }
            for diff_line in &hunk.lines {
                let prefix = match diff_line.kind {
                    DiffLineKind::Addition => '+',
                    DiffLineKind::Removal => '-',
                    DiffLineKind::Context => ' ',
                };
                let content = format!("{}{}", prefix, diff_line.content);
                let styled = match diff_line.kind {
                    DiffLineKind::Addition => {
                        Line::from(content).fg(crate::colors::success())
                    }
                    DiffLineKind::Removal => {
                        Line::from(content).fg(crate::colors::error())
                    }
                    DiffLineKind::Context => Line::from(content),
                };
                lines.push(styled);
            }
        }

        lines
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
        self.display_lines_from_record()
    }

    fn has_custom_render(&self) -> bool {
        true
    }

    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, mut skip_rows: u16) {
        let bg = Style::default().bg(crate::colors::background());
        fill_rect(buf, area, Some(' '), bg);

        if area.width == 0 || area.height == 0 {
            return;
        }

        let plan = self.ensure_layout(area.width);
        let marker_col_x = area.x.saturating_add(plan.marker_offset);
        let content_x = area.x.saturating_add(plan.content_offset);
        let content_w = plan.content_width;
        let mut cur_y = area.y;
        let end_y = area.y.saturating_add(area.height);

        for (segment, rows) in plan.segments.iter().zip(plan.segment_heights.iter()) {
            if skip_rows >= *rows {
                skip_rows -= *rows;
                continue;
            }

            let local_skip = skip_rows;
            skip_rows = 0;

            if cur_y >= end_y {
                break;
            }

            let avail = end_y.saturating_sub(cur_y);
            if avail == 0 {
                break;
            }

            let first_visible = usize::from(local_skip);
            let remaining = (*rows).saturating_sub(local_skip);
            let draw_rows = remaining.min(avail);
            if draw_rows == 0 {
                continue;
            }

            for (idx, line) in segment
                .lines
                .iter()
                .enumerate()
                .skip(first_visible)
                .take(draw_rows as usize)
            {
                if cur_y >= end_y {
                    break;
                }

                write_line(buf, content_x, cur_y, content_w, line, bg);

                if idx == first_visible {
                    if let Some(marker) = segment.marker {
                        let marker_style = match marker {
                            '+' => Style::default().fg(crate::colors::success()),
                            '-' => Style::default().fg(crate::colors::error()),
                            _ => Style::default().fg(crate::colors::text_dim()),
                        };
                        if area.width > 0 {
                            buf.set_string(marker_col_x, cur_y, marker.to_string(), marker_style);
                        }
                    }
                }

                cur_y = cur_y.saturating_add(1);
                if cur_y >= end_y {
                    break;
                }
            }
        }
    }

    fn desired_height(&self, width: u16) -> u16 {
        self.ensure_layout(width).total_rows
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

        let (kind, content) = if raw_line.starts_with("+++") || raw_line.starts_with("---") {
            (DiffLineKind::Context, raw_line.to_string())
        } else if let Some(rest) = raw_line.strip_prefix('+') {
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

#[allow(dead_code)]
pub(crate) fn new_diff_cell_from_string(diff_output: String) -> DiffCell {
    let record = diff_record_from_string(String::new(), &diff_output);
    DiffCell::from_record(record)
}

#[derive(Clone)]
struct DiffRenderedSegment {
    marker: Option<char>,
    lines: Vec<Line<'static>>,
}

#[derive(Clone)]
struct DiffLayoutCache {
    width: u16,
    marker_offset: u16,
    content_offset: u16,
    content_width: u16,
    segments: Vec<DiffRenderedSegment>,
    segment_heights: Vec<u16>,
    total_rows: u16,
}

impl DiffLayoutCache {
    fn build(record: &DiffRecord, width: u16) -> Self {
        let marker_offset = if width >= 3 { 2 } else { width.saturating_sub(1) };
        let content_offset = marker_offset.saturating_add(2);
        let content_width = width.saturating_sub(content_offset).max(1);

        let mut segments: Vec<DiffRenderedSegment> = Vec::new();

        if !record.title.is_empty() {
            let line = Line::from(record.title.clone()).fg(crate::colors::primary());
            let wrapped = word_wrap_lines(&[line], content_width);
            segments.push(DiffRenderedSegment {
                marker: None,
                lines: wrapped,
            });
        }

        for hunk in &record.hunks {
            if !hunk.header.is_empty() {
                let header = Line::from(hunk.header.clone()).fg(crate::colors::primary());
                let wrapped = word_wrap_lines(&[header], content_width);
                segments.push(DiffRenderedSegment {
                    marker: None,
                    lines: wrapped,
                });
            }

            for diff_line in &hunk.lines {
                let (marker, styled_line) = match diff_line.kind {
                    DiffLineKind::Addition => (
                        Some('+'),
                        Line::from(diff_line.content.clone()).fg(crate::colors::success()),
                    ),
                    DiffLineKind::Removal => (
                        Some('-'),
                        Line::from(diff_line.content.clone()).fg(crate::colors::error()),
                    ),
                    DiffLineKind::Context => (None, Line::from(diff_line.content.clone())),
                };
                let wrapped = word_wrap_lines(&[styled_line], content_width);
                segments.push(DiffRenderedSegment { marker, lines: wrapped });
            }
        }

        let mut segment_heights: Vec<u16> = Vec::with_capacity(segments.len());
        let mut total_rows: u16 = 0;
        for segment in &segments {
            let rows = segment.lines.len() as u16;
            segment_heights.push(rows);
            total_rows = total_rows.saturating_add(rows);
        }

        Self {
            width,
            marker_offset,
            content_offset,
            content_width,
            segments,
            segment_heights,
            total_rows,
        }
    }
}
