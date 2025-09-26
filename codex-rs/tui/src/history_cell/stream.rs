use super::*;
use super::assistant::{AssistantLayoutCache, AssistantMarkdownCell, AssistantSeg};
use crate::history::state::AssistantStreamState;
use codex_core::config::Config;

// ==================== StreamingContentCell ====================
// Renders in-progress assistant answers backed by `AssistantStreamState`.

pub(crate) struct StreamingContentCell {
    pub(crate) id: Option<String>,
    state: AssistantStreamState,
    assistant: AssistantMarkdownCell,
    pub(crate) show_ellipsis: bool,
}

impl HistoryCell for StreamingContentCell {
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
        let plan = self.ensure_stream_layout(width);
        let mut total = plan.total_rows_with_padding;
        if self.show_ellipsis {
            total = total.saturating_add(1);
        }
        total
    }

    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        let cell_bg = crate::colors::assistant_bg();
        let bg_style = Style::default().bg(cell_bg);
        fill_rect(buf, area, Some(' '), bg_style);

        if area.width == 0 || area.height == 0 {
            return;
        }

        let text_wrap_width = area.width;
        let plan = self.ensure_stream_layout(text_wrap_width);
        let mut segs = plan.segs.clone();
        let mut seg_rows = plan.seg_rows.clone();

        if self.show_ellipsis {
            const FRAMES: [&str; 5] = ["...", "·..", ".·.", "..·", "..."];
            let frame_idx = (SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis()
                / 200) as usize
                % FRAMES.len();
            let frame = FRAMES[frame_idx];
            let ellipsis_line = Line::styled(
                frame.to_string(),
                Style::default().fg(crate::colors::text_dim()),
            );
            let wrapped = word_wrap_lines(&[ellipsis_line], text_wrap_width);
            seg_rows.push(wrapped.len() as u16);
            segs.push(AssistantSeg::Text(wrapped));
        }

        let mut remaining_skip = skip_rows;
        let mut cur_y = area.y;
        let end_y = area.y.saturating_add(area.height);

        if remaining_skip == 0 && cur_y < end_y {
            cur_y = cur_y.saturating_add(1);
        }
        remaining_skip = remaining_skip.saturating_sub(1);

        #[derive(Clone)]
        enum Seg {
            Text(Vec<Line<'static>>),
            Bullet(Vec<Line<'static>>),
            Code {
                lines: Vec<Line<'static>>,
                lang_label: Option<String>,
                max_line_width: u16,
            },
        }

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
                    let txt = Text::from(lines.clone());
                    Paragraph::new(txt)
                        .block(Block::default().style(bg_style))
                        .scroll((*skip, 0))
                        .style(bg_style)
                        .render(rect, buf);
                    *y = y.saturating_add(draw_h);
                    *skip = 0;
                }
                Seg::Code {
                    lines,
                    lang_label,
                    max_line_width,
                } => {
                    if lines.is_empty() {
                        return;
                    }
                    let inner_w = (*max_line_width).max(1);
                    let card_w = inner_w.saturating_add(6).min(area.width.max(6));
                    let total = lines.len() as u16 + 2; // borders
                    if *skip >= total {
                        *skip -= total;
                        return;
                    }
                    let avail = end_y.saturating_sub(*y);
                    if avail == 0 {
                        return;
                    }
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
                    let visible = top_border + (lines.len() as u16 - code_skip) + bottom_border;
                    let draw_h = visible.min(avail);
                    if draw_h == 0 {
                        return;
                    }
                    let rect = Rect {
                        x: area.x,
                        y: *y,
                        width: card_w,
                        height: draw_h,
                    };
                    let code_bg = crate::colors::code_block_bg();
                    let mut blk = Block::default()
                        .borders(Borders::ALL)
                        .border_style(Style::default().fg(crate::colors::border()))
                        .style(Style::default().bg(code_bg))
                        .padding(Padding {
                            left: 2,
                            right: 2,
                            top: 0,
                            bottom: 0,
                        });
                    if let Some(lang) = lang_label {
                        blk = blk.title(Span::styled(
                            format!(" {} ", lang),
                            Style::default().fg(crate::colors::text_dim()),
                        ));
                    }
                    let blk_for_inner = blk.clone();
                    blk.render(rect, buf);
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
                    *y = y.saturating_add(draw_h);
                    *skip = 0;
                }
            }
        };

        for (idx, seg) in segs.iter().enumerate() {
            if cur_y >= end_y {
                break;
            }
            let rows = seg_rows.get(idx).copied().unwrap_or(0);
            if remaining_skip >= rows {
                remaining_skip -= rows;
                continue;
            }
            let seg_draw = match seg {
                AssistantSeg::Text(lines) => Seg::Text(lines.clone()),
                AssistantSeg::Bullet(lines) => Seg::Bullet(lines.clone()),
                AssistantSeg::Code {
                    lines,
                    lang_label,
                    max_line_width,
                } => Seg::Code {
                    lines: lines.clone(),
                    lang_label: lang_label.clone(),
                    max_line_width: *max_line_width,
                },
            };
            draw_segment(&seg_draw, &mut cur_y, &mut remaining_skip);
        }

        if remaining_skip == 0 && cur_y < end_y {
            cur_y = cur_y.saturating_add(1);
        } else {
            remaining_skip = remaining_skip.saturating_sub(1);
        }
        let _ = (cur_y, remaining_skip);
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        self.assistant.display_lines()
    }
}

impl StreamingContentCell {
    pub(crate) fn from_state(state: AssistantStreamState, cfg: &Config) -> Self {
        let assistant = AssistantMarkdownCell::from_stream_state(&state, cfg);
        let show_ellipsis = state.in_progress;
        Self {
            id: Some(state.stream_id.clone()),
            state,
            assistant,
            show_ellipsis,
        }
    }

    pub(crate) fn update_from_state(&mut self, state: AssistantStreamState, cfg: &Config) {
        self.state = state;
        self.id = Some(self.state.stream_id.clone());
        self.assistant = AssistantMarkdownCell::from_stream_state(&self.state, cfg);
        self.show_ellipsis = self.state.in_progress;
    }

    pub(crate) fn rebuild_with_config(&mut self, cfg: &Config) {
        let current = self.assistant.state().clone();
        self.assistant.update_state(current, cfg);
    }

    pub(crate) fn matches_id(&self, want: &str) -> bool {
        self.id.as_deref() == Some(want) || self.state.stream_id == want
    }

    fn ensure_stream_layout(&self, width: u16) -> AssistantLayoutCache {
        self.assistant.ensure_layout(width)
    }

    pub(crate) fn state(&self) -> &AssistantStreamState {
        &self.state
    }

    pub(crate) fn state_mut(&mut self) -> &mut AssistantStreamState {
        &mut self.state
    }
}
