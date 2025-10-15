use crossterm::event::KeyEvent;
use ratatui::buffer::Buffer;
use ratatui::layout::{Constraint, Layout, Margin, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, List, ListItem, Paragraph, Widget};

use crate::bottom_pane::{
    BottomPaneView,
    McpSettingsView,
    ModelSelectionView,
    NotificationsSettingsView,
    SettingsSection,
    ThemeSelectionView,
};

pub(crate) trait SettingsContent {
    fn render(&self, area: Rect, buf: &mut Buffer);
    fn handle_key(&mut self, key: KeyEvent) -> bool;
    fn is_complete(&self) -> bool;
    fn on_close(&mut self) {}
}

pub(crate) struct ModelSettingsContent {
    view: ModelSelectionView,
}

impl ModelSettingsContent {
    pub(crate) fn new(view: ModelSelectionView) -> Self {
        Self { view }
    }
}

impl SettingsContent for ModelSettingsContent {
    fn render(&self, area: Rect, buf: &mut Buffer) {
        self.view.render(area, buf);
    }

    fn handle_key(&mut self, key: KeyEvent) -> bool {
        self.view.handle_key_event_direct(key)
    }

    fn is_complete(&self) -> bool {
        self.view.is_complete()
    }
}

pub(crate) struct ThemeSettingsContent {
    view: ThemeSelectionView,
}

impl ThemeSettingsContent {
    pub(crate) fn new(view: ThemeSelectionView) -> Self {
        Self { view }
    }
}

impl SettingsContent for ThemeSettingsContent {
    fn render(&self, area: Rect, buf: &mut Buffer) {
        self.view.render(area, buf);
    }

    fn handle_key(&mut self, key: KeyEvent) -> bool {
        self.view.handle_key_event_direct(key)
    }

    fn is_complete(&self) -> bool {
        self.view.is_complete()
    }
}

pub(crate) struct NotificationsSettingsContent {
    view: NotificationsSettingsView,
}

impl NotificationsSettingsContent {
    pub(crate) fn new(view: NotificationsSettingsView) -> Self {
        Self { view }
    }
}

impl SettingsContent for NotificationsSettingsContent {
    fn render(&self, area: Rect, buf: &mut Buffer) {
        self.view.render(area, buf);
    }

    fn handle_key(&mut self, key: KeyEvent) -> bool {
        self.view.handle_key_event_direct(key)
    }

    fn is_complete(&self) -> bool {
        self.view.is_complete()
    }
}

pub(crate) struct McpSettingsContent {
    view: McpSettingsView,
}

impl McpSettingsContent {
    pub(crate) fn new(view: McpSettingsView) -> Self {
        Self { view }
    }
}

impl SettingsContent for McpSettingsContent {
    fn render(&self, area: Rect, buf: &mut Buffer) {
        self.view.render(area, buf);
    }

    fn handle_key(&mut self, key: KeyEvent) -> bool {
        self.view.handle_key_event_direct(key)
    }

    fn is_complete(&self) -> bool {
        self.view.is_complete()
    }
}

/// Full-screen settings overlay rendered by the chat widget.
pub(crate) struct SettingsOverlayView {
    active_section: SettingsSection,
    model_content: Option<ModelSettingsContent>,
    theme_content: Option<ThemeSettingsContent>,
    notifications_content: Option<NotificationsSettingsContent>,
    mcp_content: Option<McpSettingsContent>,
}

impl SettingsOverlayView {
    pub(crate) fn new(section: SettingsSection) -> Self {
        Self {
            active_section: section,
            model_content: None,
            theme_content: None,
            notifications_content: None,
            mcp_content: None,
        }
    }

    pub(crate) fn active_section(&self) -> SettingsSection {
        self.active_section
    }

    pub(crate) fn set_model_content(&mut self, content: ModelSettingsContent) {
        self.model_content = Some(content);
    }

    pub(crate) fn set_theme_content(&mut self, content: ThemeSettingsContent) {
        self.theme_content = Some(content);
    }

    pub(crate) fn set_notifications_content(&mut self, content: NotificationsSettingsContent) {
        self.notifications_content = Some(content);
    }

    pub(crate) fn set_mcp_content(&mut self, content: McpSettingsContent) {
        self.mcp_content = Some(content);
    }

    pub(crate) fn set_section(&mut self, section: SettingsSection) -> bool {
        if self.active_section == section {
            return false;
        }
        self.active_section = section;
        true
    }

    pub(crate) fn select_next(&mut self) -> bool {
        let mut idx = self.index_of(self.active_section);
        idx = (idx + 1) % SettingsSection::ALL.len();
        self.set_section(SettingsSection::ALL[idx])
    }

    pub(crate) fn select_previous(&mut self) -> bool {
        let mut idx = self.index_of(self.active_section);
        idx = idx.checked_sub(1).unwrap_or(SettingsSection::ALL.len() - 1);
        self.set_section(SettingsSection::ALL[idx])
    }

    pub(crate) fn select_by_shortcut(&mut self, ch: char) -> bool {
        let needle = ch.to_ascii_lowercase();
        if let Some(section) = SettingsSection::ALL
            .iter()
            .copied()
            .find(|section| section.shortcut().map(|s| s == needle).unwrap_or(false))
        {
            return self.set_section(section);
        }
        false
    }

    fn index_of(&self, section: SettingsSection) -> usize {
        SettingsSection::ALL
            .iter()
            .position(|s| *s == section)
            .unwrap_or(0)
    }

    pub(crate) fn render(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        let dim = Style::default().fg(crate::colors::text_dim());
        let fg = Style::default().fg(crate::colors::text());

        let title = Line::from(vec![
            Span::styled(" ", dim),
            Span::styled("Settings", fg),
            Span::styled(" ——— ", dim),
            Span::styled("Esc", fg),
            Span::styled(" close ", dim),
        ]);

        let block = Block::default()
            .borders(Borders::ALL)
            .title(title)
            .style(Style::default().bg(crate::colors::background()))
            .border_style(
                Style::default()
                    .fg(crate::colors::border())
                    .bg(crate::colors::background()),
            );
        let inner = block.inner(area);
        block.render(area, buf);

        // Paint inner background for a clean canvas
        let bg = Style::default().bg(crate::colors::background());
        for y in inner.y..inner.y.saturating_add(inner.height) {
            for x in inner.x..inner.x.saturating_add(inner.width) {
                buf[(x, y)].set_style(bg);
            }
        }

        let content = inner.inner(Margin::new(1, 1));
        if content.width == 0 || content.height == 0 {
            return;
        }

        let [header_area, body_area] = Layout::vertical([
            Constraint::Length(1),
            Constraint::Fill(1),
        ])
        .areas(content);

        self.render_header(header_area, buf);
        self.render_body(body_area, buf);
    }

    fn render_header(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }
        let mut spans: Vec<Span<'static>> = Vec::new();
        spans.push(Span::styled("↑/↓", Style::default().fg(crate::colors::text())));
        spans.push(Span::styled(" navigate  ", Style::default().fg(crate::colors::text_dim())));
        spans.push(Span::styled("m/t/a/l/c/p/n", Style::default().fg(crate::colors::text())));
        spans.push(Span::styled(" jump  ", Style::default().fg(crate::colors::text_dim())));
        spans.push(Span::styled("Enter", Style::default().fg(crate::colors::text())));
        spans.push(Span::styled(" select", Style::default().fg(crate::colors::text_dim())));

        let line = Line::from(spans);
        Paragraph::new(line)
            .style(Style::default().bg(crate::colors::background()))
            .render(area, buf);
    }

    fn render_body(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        let [sidebar, main] = Layout::horizontal([
            Constraint::Length(22),
            Constraint::Fill(1),
        ])
        .areas(area);

        self.render_sidebar(sidebar, buf);
        self.render_content(main, buf);
    }

    fn render_sidebar(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }
        let items: Vec<ListItem> = SettingsSection::ALL
            .iter()
            .map(|section| {
                let is_active = *section == self.active_section;
                let mut spans: Vec<Span<'static>> = Vec::new();
                let prefix = if is_active { "›" } else { " " };
                spans.push(Span::styled(prefix, Style::default().fg(crate::colors::text())));
                spans.push(Span::raw(" "));
                spans.push(Span::styled(
                    section.label(),
                    if is_active {
                        Style::default()
                            .fg(crate::colors::text())
                            .add_modifier(Modifier::BOLD)
                    } else {
                        Style::default().fg(crate::colors::text_dim())
                    },
                ));
                if let Some(shortcut) = section.shortcut() {
                    spans.push(Span::raw("  "));
                    spans.push(Span::styled(
                        format!("({})", shortcut.to_ascii_uppercase()),
                        Style::default().fg(crate::colors::text_dim()),
                    ));
                }
                ListItem::new(Line::from(spans))
            })
            .collect();

        let list = List::new(items)
            .block(Block::default().borders(Borders::RIGHT))
            .highlight_style(
                Style::default()
                    .fg(crate::colors::primary())
                    .add_modifier(Modifier::BOLD),
            );
        list.render(area, buf);
    }

    fn render_content(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }
        match self.active_section {
            SettingsSection::Model => {
                if let Some(content) = self.model_content.as_ref() {
                    content.render(area, buf);
                    return;
                }
                self.render_placeholder(area, buf, SettingsSection::Model.placeholder());
            }
            SettingsSection::Theme => {
                if let Some(content) = self.theme_content.as_ref() {
                    content.render(area, buf);
                    return;
                }
                self.render_placeholder(area, buf, SettingsSection::Theme.placeholder());
            }
            SettingsSection::Notifications => {
                if let Some(content) = self.notifications_content.as_ref() {
                    content.render(area, buf);
                    return;
                }
                self.render_placeholder(area, buf, SettingsSection::Notifications.placeholder());
            }
            SettingsSection::Mcp => {
                if let Some(content) = self.mcp_content.as_ref() {
                    content.render(area, buf);
                    return;
                }
                self.render_placeholder(area, buf, SettingsSection::Mcp.placeholder());
            }
            section => self.render_placeholder(area, buf, section.placeholder()),
        }
    }

    fn render_placeholder(&self, area: Rect, buf: &mut Buffer, text: &'static str) {
        let paragraph = Paragraph::new(text)
            .wrap(ratatui::widgets::Wrap { trim: true })
            .style(Style::default().fg(crate::colors::text_dim()));
        paragraph.render(area, buf);
    }

    pub(crate) fn active_content_mut(&mut self) -> Option<&mut dyn SettingsContent> {
        match self.active_section {
            SettingsSection::Model => self
                .model_content
                .as_mut()
                .map(|content| content as &mut dyn SettingsContent),
            SettingsSection::Theme => self
                .theme_content
                .as_mut()
                .map(|content| content as &mut dyn SettingsContent),
            SettingsSection::Notifications => self
                .notifications_content
                .as_mut()
                .map(|content| content as &mut dyn SettingsContent),
            SettingsSection::Mcp => self
                .mcp_content
                .as_mut()
                .map(|content| content as &mut dyn SettingsContent),
            _ => None,
        }
    }

    pub(crate) fn notify_close(&mut self) {
        match self.active_section {
            SettingsSection::Model => {
                if let Some(content) = self.model_content.as_mut() {
                    content.on_close();
                }
            }
            SettingsSection::Theme => {
                if let Some(content) = self.theme_content.as_mut() {
                    content.on_close();
                }
            }
            SettingsSection::Notifications => {
                if let Some(content) = self.notifications_content.as_mut() {
                    content.on_close();
                }
            }
            SettingsSection::Mcp => {
                if let Some(content) = self.mcp_content.as_mut() {
                    content.on_close();
                }
            }
            _ => {}
        }
    }
}
