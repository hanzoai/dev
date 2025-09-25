use super::semantic::{lines_from_ratatui, lines_to_ratatui, SemanticLine};
use super::*;
use crate::theme::current_theme;

#[derive(Debug)]
pub(crate) struct LimitsCellState {
    pub summary_lines: Vec<SemanticLine>,
    pub legend_lines: Vec<SemanticLine>,
    pub footer_lines: Vec<SemanticLine>,
    pub grid_state: Option<crate::rate_limits_view::GridState>,
    pub grid: crate::rate_limits_view::GridConfig,
}

impl LimitsCellState {
    pub(crate) fn new(view: LimitsView) -> Self {
        Self {
            summary_lines: lines_from_ratatui(view.summary_lines.clone()),
            legend_lines: lines_from_ratatui(view.legend_lines.clone()),
            footer_lines: lines_from_ratatui(view.footer_lines.clone()),
            grid_state: view.grid_state(),
            grid: view.grid_config(),
        }
    }
}

pub(crate) struct LimitsHistoryCell {
    state: LimitsCellState,
}

impl LimitsHistoryCell {
    const TRANSCRIPT_WIDTH: u16 = 80;

    pub(crate) fn new(view: LimitsView) -> Self {
        Self {
            state: LimitsCellState::new(view),
        }
    }

    fn lines_for_width(&self, width: u16) -> Vec<Line<'static>> {
        let theme = current_theme();
        let mut lines = lines_to_ratatui(&self.state.summary_lines, &theme);
        if let Some(state) = self.state.grid_state {
            lines.extend(crate::rate_limits_view::render_limit_grid(state, self.state.grid, width));
        }
        lines.extend(lines_to_ratatui(&self.state.legend_lines, &theme));
        lines.extend(lines_to_ratatui(&self.state.footer_lines, &theme));
        lines
    }
}

impl HistoryCell for LimitsHistoryCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Notice
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        self.lines_for_width(Self::TRANSCRIPT_WIDTH)
    }

    fn has_custom_render(&self) -> bool {
        true
    }

    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        let width = if area.width == 0 { 1 } else { area.width };
        let lines = self.lines_for_width(width);
        let text = Text::from(lines);

        let cell_bg = crate::colors::background();

        Paragraph::new(text)
            .wrap(Wrap { trim: false })
            .scroll((skip_rows, 0))
            .block(Block::default().style(Style::default().bg(cell_bg)))
            .style(Style::default().bg(cell_bg))
            .render(area, buf);
    }
}
