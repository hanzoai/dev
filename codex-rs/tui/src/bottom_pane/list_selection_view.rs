#![allow(dead_code)]
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyModifiers;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::text::Line;
use ratatui::text::Span;
use ratatui::widgets::{Paragraph, Block, Borders, Clear};
use ratatui::layout::Alignment;
use ratatui::widgets::Widget;

use crate::app_event_sender::AppEventSender;

use super::BottomPane;
use super::CancellationEvent;
use super::bottom_pane_view::BottomPaneView;
use super::scroll_state::ScrollState;
use super::selection_popup_common::GenericDisplayRow;
use super::selection_popup_common::render_rows;

/// One selectable item in the generic selection list.
pub(crate) type SelectionAction = Box<dyn Fn(&AppEventSender) + Send + Sync>;

pub(crate) struct SelectionItem {
    pub name: String,
    pub description: Option<String>,
    pub is_current: bool,
    pub actions: Vec<SelectionAction>,
}

pub(crate) struct ListSelectionView {
    title: String,
    subtitle: Option<String>,
    footer_hint: Option<String>,
    items: Vec<SelectionItem>,
    state: ScrollState,
    complete: bool,
    app_event_tx: AppEventSender,
    max_rows: usize,
}

impl ListSelectionView {
    fn dim_prefix_span() -> Span<'static> {
        Span::styled("▌ ", Style::default().add_modifier(Modifier::DIM))
    }

    fn render_dim_prefix_line(area: Rect, buf: &mut Buffer) {
        // Render a simple blank spacer line (no glyphs like '▌')
        let para = Paragraph::new(Line::from(""));
        para.render(area, buf);
    }
    
    fn wrapped_lines_for(text: &str, width: u16) -> u16 {
        if text.is_empty() || width == 0 { return 0; }
        let w = width as usize;
        let mut lines: u16 = 0;
        for part in text.split('\n') {
            let len = part.chars().count();
            if len == 0 { lines = lines.saturating_add(1); continue; }
            let mut l = (len / w) as u16;
            if len % w != 0 { l = l.saturating_add(1); }
            if l == 0 { l = 1; }
            lines = lines.saturating_add(l);
        }
        lines
    }
    pub fn new(
        title: String,
        subtitle: Option<String>,
        footer_hint: Option<String>,
        items: Vec<SelectionItem>,
        app_event_tx: AppEventSender,
        max_rows: usize,
    ) -> Self {
        let mut s = Self {
            title,
            subtitle,
            footer_hint,
            items,
            state: ScrollState::new(),
            complete: false,
            app_event_tx,
            max_rows,
        };
        let len = s.items.len();
        if let Some(idx) = s.items.iter().position(|it| it.is_current) {
            s.state.selected_idx = Some(idx);
        }
        s.state.clamp_selection(len);
        s.state.ensure_visible(len, s.max_rows.min(len));
        s
    }

    fn move_up(&mut self) {
        let len = self.items.len();
        self.state.move_up_wrap(len);
        self.state.ensure_visible(len, self.max_rows.min(len));
    }

    fn move_down(&mut self) {
        let len = self.items.len();
        self.state.move_down_wrap(len);
        self.state.ensure_visible(len, self.max_rows.min(len));
    }

    fn accept(&mut self) {
        if let Some(idx) = self.state.selected_idx {
            if let Some(item) = self.items.get(idx) {
                for act in &item.actions {
                    act(&self.app_event_tx);
                }
                self.complete = true;
            }
        } else {
            self.complete = true;
        }
    }

    fn cancel(&mut self) {
        // Close the popup without performing any actions.
        self.complete = true;
    }
}

impl BottomPaneView<'_> for ListSelectionView {
    fn handle_key_event(&mut self, _pane: &mut BottomPane<'_>, key_event: KeyEvent) {
        match key_event {
            KeyEvent {
                code: KeyCode::Up, ..
            } => self.move_up(),
            KeyEvent {
                code: KeyCode::Down,
                ..
            } => self.move_down(),
            KeyEvent {
                code: KeyCode::Esc, ..
            } => self.cancel(),
            KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.accept(),
            _ => {}
        }
    }

    fn is_complete(&self) -> bool {
        self.complete
    }

    fn on_ctrl_c(&mut self, _pane: &mut BottomPane<'_>) -> CancellationEvent {
        self.complete = true;
        CancellationEvent::Handled
    }

    fn desired_height(&self, width: u16) -> u16 {
        let rows = (self.items.len()).clamp(1, self.max_rows);
        // Title row always 1
        let mut height = rows as u16 + 1;
        // If subtitle exists, estimate wrapped height + one spacer line
        if let Some(sub) = &self.subtitle {
            // inner width = total width - borders(2) - left padding(1)
            let inner_w = width.saturating_sub(3);
            let sub_rows = Self::wrapped_lines_for(sub, inner_w).max(1);
            height = height.saturating_add(sub_rows).saturating_add(1);
        }
        // Footer takes 2 lines (spacer + footer)
        if self.footer_hint.is_some() {
            height = height.saturating_add(2);
        }
        height
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        if area.height == 0 || area.width == 0 {
            return;
        }

        // Clear and draw a bordered block matching other slash popups
        Clear.render(area, buf);
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()))
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .title(self.title.clone())
            .title_alignment(Alignment::Center);
        let inner = block.inner(area);
        block.render(area, buf);

        // Layout inside the block: optional subtitle header, rows, footer
        let mut next_y = inner.y;
        if let Some(sub) = &self.subtitle {
            // Left pad by one column inside the inner area
            let subtitle_rows = Self::wrapped_lines_for(sub, inner.width.saturating_sub(1));
            let sub_h = subtitle_rows.max(1);
            let subtitle_area = Rect { x: inner.x.saturating_add(1), y: next_y, width: inner.width.saturating_sub(1), height: sub_h };
            Paragraph::new(sub.clone())
                .style(Style::default().fg(crate::colors::text_dim()))
                .render(subtitle_area, buf);
            next_y = next_y.saturating_add(sub_h);

            // Render a visual spacer line between subtitle and the list
            let spacer_area = Rect { x: inner.x.saturating_add(1), y: next_y, width: inner.width.saturating_sub(1), height: 1 };
            Self::render_dim_prefix_line(spacer_area, buf);
            next_y = next_y.saturating_add(1);
        }

        // Reserve a single footer row when present
        let footer_reserved = if self.footer_hint.is_some() { 1 } else { 0 };
        let rows_area = Rect {
            // Left pad by one column
            x: inner.x.saturating_add(1),
            y: next_y,
            width: inner.width.saturating_sub(1),
            height: inner.height.saturating_sub(next_y.saturating_sub(inner.y)).saturating_sub(footer_reserved),
        };

        let rows: Vec<GenericDisplayRow> = self
            .items
            .iter()
            .enumerate()
            .map(|(i, it)| {
                let is_selected = self.state.selected_idx == Some(i);
                // Use a nicer selector: '›' when selected, otherwise space
                let prefix = if is_selected { '›' } else { ' ' };
                let name_with_marker = if it.is_current {
                    format!("{} (current)", it.name)
                } else {
                    it.name.clone()
                };
                let display_name = format!("{} {}. {}", prefix, i + 1, name_with_marker);
                GenericDisplayRow {
                    name: display_name,
                    match_indices: None,
                    is_current: it.is_current,
                    description: if is_selected { it.description.clone() } else { None },
                    name_color: None,
                }
            })
            .collect();
        if rows_area.height > 0 {
            render_rows(rows_area, buf, &rows, &self.state, self.max_rows, true);
        }

        if self.footer_hint.is_some() {
            // Left pad footer by one column and render it on the last line
            let footer_area = Rect { x: inner.x.saturating_add(1), y: inner.y + inner.height - 1, width: inner.width.saturating_sub(1), height: 1 };
            let line = Line::from(vec![
                Span::styled("↑↓", Style::default().fg(crate::colors::function())),
                Span::styled(" Navigate  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("Enter", Style::default().fg(crate::colors::success())),
                Span::styled(" Select  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("Esc", Style::default().fg(crate::colors::error())),
                Span::styled(" Cancel", Style::default().fg(crate::colors::text_dim())),
            ]);
            Paragraph::new(line)
                .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
                .render(footer_area, buf);
        }
    }
}
