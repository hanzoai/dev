use ratatui::buffer::Buffer;
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::{Block, Borders, Clear, Paragraph, Tabs, Widget};
use crossterm::event::{KeyCode, KeyEvent, KeyEventKind};

use super::{BottomPane, BottomPaneView, CancellationEvent};

#[allow(dead_code)]
pub(crate) struct DiffPopupView {
    tabs: Vec<(String, Vec<Line<'static>>)>,
    selected: usize,
    complete: bool,
}

#[allow(dead_code)]
impl DiffPopupView {
    pub fn new(tabs: Vec<(String, Vec<Line<'static>>)>) -> Self {
        Self { tabs, selected: 0, complete: false }
    }
}

impl<'a> BottomPaneView<'a> for DiffPopupView {
    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        if key_event.kind == KeyEventKind::Press || key_event.kind == KeyEventKind::Repeat {
            match key_event.code {
                KeyCode::Left => {
                    if self.selected > 0 { self.selected -= 1; }
                }
                KeyCode::Right => {
                    if self.selected + 1 < self.tabs.len() { self.selected += 1; }
                }
                KeyCode::Esc => {
                    self.complete = true;
                }
                _ => {}
            }
        }
    }

    fn is_complete(&self) -> bool { self.complete }

    fn on_ctrl_c(&mut self, _pane: &mut BottomPane<'a>) -> CancellationEvent {
        self.complete = true;
        CancellationEvent::Handled
    }

    fn desired_height(&self, _width: u16) -> u16 {
        // Reasonable height for a diff popup within bottom pane
        20
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        // Reserve a small margin inside area
        let inner = Rect { x: area.x, y: area.y, width: area.width, height: area.height };

        // Draw background clear and border
        Clear.render(inner, buf);
        let block = Block::default()
            .borders(Borders::ALL)
            .title("Diffs – Esc to close, ◄ ► to change file")
            .border_style(Style::default().fg(crate::colors::border()));
        block.clone().render(inner, buf);
        let content = block.inner(inner);

        // Split into tabs header and content
        let [tabs_area, body_area] = Layout::vertical([Constraint::Length(1), Constraint::Fill(1)]).areas(content);

        // Render tabs
        let titles = self
            .tabs
            .iter()
            .map(|(t, _)| Line::from(Span::raw(format!(" {t} "))))
            .collect::<Vec<_>>();
        let tabs = Tabs::new(titles)
            .select(self.selected)
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .divider(" ");
        Widget::render(tabs, tabs_area, buf);

        // Render selected tab content
        if let Some((_, lines)) = self.tabs.get(self.selected) {
            let paragraph = Paragraph::new(Text::from(lines.clone()))
                .wrap(ratatui::widgets::Wrap { trim: false });
            Widget::render(paragraph, body_area, buf);
        }
    }
}
