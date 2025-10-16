use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Constraint, Layout, Margin, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, List, ListItem, ListState, Paragraph, Widget};

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::bottom_pane::{
    BottomPaneView,
    McpSettingsView,
    ModelSelectionView,
    NotificationsSettingsView,
    SettingsSection,
    ThemeSelectionView,
    agent_editor_view::AgentEditorView,
    agents_settings_view::SubagentEditorView,
};
use crate::chrome_launch::{ChromeLaunchOption, CHROME_LAUNCH_CHOICES};
use super::limits_overlay::{LimitsOverlay, LimitsOverlayContent};

pub(crate) trait SettingsContent {
    fn render(&self, area: Rect, buf: &mut Buffer);
    fn handle_key(&mut self, key: KeyEvent) -> bool;
    fn is_complete(&self) -> bool;
    fn on_close(&mut self) {}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct MenuState {
    selected: SettingsSection,
}

impl MenuState {
    fn new(selected: SettingsSection) -> Self {
        Self { selected }
    }

    fn selected(&self) -> SettingsSection {
        self.selected
    }

    fn set_selected(&mut self, section: SettingsSection) {
        self.selected = section;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct SectionState {
    active: SettingsSection,
}

impl SectionState {
    fn new(active: SettingsSection) -> Self {
        Self { active }
    }

    fn active(&self) -> SettingsSection {
        self.active
    }

    fn set_active(&mut self, section: SettingsSection) {
        self.active = section;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum SettingsOverlayMode {
    Menu(MenuState),
    Section(SectionState),
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

#[derive(Clone, Debug)]
pub(crate) struct AgentOverviewRow {
    pub(crate) name: String,
    pub(crate) enabled: bool,
    pub(crate) installed: bool,
}

#[derive(Default)]
struct AgentsOverviewState {
    rows: Vec<AgentOverviewRow>,
    commands: Vec<String>,
    selected: usize,
}

impl AgentsOverviewState {
    fn total_rows(&self) -> usize {
        self.rows.len().saturating_add(self.commands.len()).saturating_add(1)
    }

    fn clamp_selection(&mut self) {
        let total = self.total_rows();
        if total == 0 {
            self.selected = 0;
        } else if self.selected >= total {
            self.selected = total - 1;
        }
    }
}

enum AgentsPane {
    Overview(AgentsOverviewState),
    Subagent(SubagentEditorView),
    Agent(AgentEditorView),
}

pub(crate) struct AgentsSettingsContent {
    pane: AgentsPane,
    app_event_tx: AppEventSender,
}

impl AgentsSettingsContent {
    pub(crate) fn new_overview(
        rows: Vec<AgentOverviewRow>,
        commands: Vec<String>,
        selected: usize,
        app_event_tx: AppEventSender,
    ) -> Self {
        let mut overview = AgentsOverviewState { rows, commands, selected };
        overview.clamp_selection();
        Self { pane: AgentsPane::Overview(overview), app_event_tx }
    }

    pub(crate) fn set_overview(
        &mut self,
        rows: Vec<AgentOverviewRow>,
        commands: Vec<String>,
        selected: usize,
    ) {
        let mut overview = AgentsOverviewState { rows, commands, selected };
        overview.clamp_selection();
        self.pane = AgentsPane::Overview(overview);
    }

    pub(crate) fn set_editor(&mut self, editor: SubagentEditorView) {
        self.pane = AgentsPane::Subagent(editor);
    }

    pub(crate) fn set_overview_selection(&mut self, selected: usize) {
        if let AgentsPane::Overview(state) = &mut self.pane {
            state.selected = selected;
            state.clamp_selection();
        }
    }

    pub(crate) fn set_agent_editor(&mut self, editor: AgentEditorView) {
        self.pane = AgentsPane::Agent(editor);
    }

    #[cfg_attr(not(any(test, feature = "test-helpers")), allow(dead_code))]
    pub(crate) fn is_agent_editor_active(&self) -> bool {
        matches!(self.pane, AgentsPane::Agent(_))
    }

    fn render_overview(&self, area: Rect, buf: &mut Buffer, state: &AgentsOverviewState) {
        use ratatui::widgets::Paragraph;

        let mut lines: Vec<Line<'static>> = Vec::new();

        lines.push(Line::from(Span::styled(
            "Agents",
            Style::default().add_modifier(Modifier::BOLD),
        )));

        let max_name = state
            .rows
            .iter()
            .map(|row| row.name.chars().count())
            .max()
            .unwrap_or(0);

        for (idx, row) in state.rows.iter().enumerate() {
            let selected = idx == state.selected;
            let status = if !row.enabled {
                ("disabled", crate::colors::error())
            } else if !row.installed {
                ("not installed", crate::colors::warning())
            } else {
                ("enabled", crate::colors::success())
            };

            let mut spans = Vec::new();
            spans.push(Span::styled(
                if selected { "› " } else { "  " },
                if selected {
                    Style::default().fg(crate::colors::primary())
                } else {
                    Style::default()
                },
            ));
            spans.push(Span::styled(
                format!("{:<width$}", row.name, width = max_name),
                if selected {
                    Style::default()
                        .fg(crate::colors::primary())
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default()
                },
            ));
            spans.push(Span::raw("  "));
            spans.push(Span::styled("•", Style::default().fg(status.1)));
            spans.push(Span::raw(" "));
            spans.push(Span::styled(status.0.to_string(), Style::default().fg(status.1)));

            if selected {
                spans.push(Span::raw("  "));
                let hint = if !row.installed {
                    "Enter to install"
                } else {
                    "Enter to configure"
                };
                spans.push(Span::styled(hint, Style::default().fg(crate::colors::text_dim())));
            }

            lines.push(Line::from(spans));
        }

        lines.push(Line::from(""));
        lines.push(Line::from(Span::styled(
            "Commands",
            Style::default().add_modifier(Modifier::BOLD),
        )));

        for (offset, cmd) in state.commands.iter().enumerate() {
            let idx = state.rows.len() + offset;
            let selected = idx == state.selected;
            let mut spans = Vec::new();
            spans.push(Span::styled(
                if selected { "› " } else { "  " },
                if selected {
                    Style::default().fg(crate::colors::primary())
                } else {
                    Style::default()
                },
            ));
            spans.push(Span::styled(
                format!("/{}", cmd),
                if selected {
                    Style::default()
                        .fg(crate::colors::primary())
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default()
                },
            ));
            if selected {
                spans.push(Span::raw("  "));
                spans.push(Span::styled(
                    "Enter to configure",
                    Style::default().fg(crate::colors::text_dim()),
                ));
            }
            lines.push(Line::from(spans));
        }

        let add_idx = state.rows.len() + state.commands.len();
        let add_selected = add_idx == state.selected;
        let mut add_spans = Vec::new();
        add_spans.push(Span::styled(
            if add_selected { "› " } else { "  " },
            if add_selected {
                Style::default().fg(crate::colors::primary())
            } else {
                Style::default()
            },
        ));
        add_spans.push(Span::styled(
            "Add new…",
            if add_selected {
                Style::default()
                    .fg(crate::colors::primary())
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            },
        ));
        if add_selected {
            add_spans.push(Span::raw("  "));
            add_spans.push(Span::styled(
                "Enter to create",
                Style::default().fg(crate::colors::text_dim()),
            ));
        }
        lines.push(Line::from(add_spans));

        lines.push(Line::from(""));
        lines.push(Line::from(vec![
            Span::styled("↑↓", Style::default().fg(crate::colors::function())),
            Span::styled(" Navigate  ", Style::default().fg(crate::colors::text_dim())),
            Span::styled("Enter", Style::default().fg(crate::colors::success())),
            Span::styled(" Open", Style::default().fg(crate::colors::text_dim())),
            Span::styled("  Esc", Style::default().fg(crate::colors::error())),
            Span::styled(" Close", Style::default().fg(crate::colors::text_dim())),
        ]));

        Paragraph::new(lines)
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .render(area, buf);
    }

    fn handle_overview_key(
        state: &mut AgentsOverviewState,
        key: KeyEvent,
        app_event_tx: &AppEventSender,
    ) -> bool {
        match key.code {
            KeyCode::Up => {
                if state.total_rows() == 0 {
                    return true;
                }
                if state.selected == 0 {
                    state.selected = state.total_rows().saturating_sub(1);
                } else {
                    state.selected -= 1;
                }
                app_event_tx
                    .send(AppEvent::AgentsOverviewSelectionChanged { index: state.selected });
                true
            }
            KeyCode::Down => {
                let total = state.total_rows();
                if total == 0 {
                    return true;
                }
                state.selected = (state.selected + 1) % total;
                app_event_tx
                    .send(AppEvent::AgentsOverviewSelectionChanged { index: state.selected });
                true
            }
            KeyCode::Enter => {
                let idx = state.selected;
                if idx < state.rows.len() {
                    let row = &state.rows[idx];
                    if !row.installed {
                        app_event_tx
                            .send(AppEvent::RequestAgentInstall { name: row.name.clone(), selected_index: idx });
                    } else {
                        app_event_tx
                            .send(AppEvent::ShowAgentEditor { name: row.name.clone() });
                    }
                } else {
                    let cmd_idx = idx.saturating_sub(state.rows.len());
                    if cmd_idx < state.commands.len() {
                        if let Some(name) = state.commands.get(cmd_idx) {
                            app_event_tx
                                .send(AppEvent::ShowSubagentEditorForName { name: name.clone() });
                        }
                    } else {
                        app_event_tx.send(AppEvent::ShowSubagentEditorNew);
                    }
                }
                true
            }
            _ => false,
        }
    }
}

impl SettingsContent for AgentsSettingsContent {
    fn render(&self, area: Rect, buf: &mut Buffer) {
        match &self.pane {
            AgentsPane::Overview(state) => {
                self.render_overview(area, buf, state);
            }
            AgentsPane::Subagent(view) => {
                view.render(area, buf);
            }
            AgentsPane::Agent(view) => {
                view.render(area, buf);
            }
        }
    }

    fn handle_key(&mut self, key: KeyEvent) -> bool {
        match &mut self.pane {
            AgentsPane::Overview(state) => {
                Self::handle_overview_key(state, key, &self.app_event_tx)
            }
            AgentsPane::Subagent(view) => view.handle_key_event_direct(key),
            AgentsPane::Agent(view) => view.handle_key_event_direct(key),
        }
    }

    fn is_complete(&self) -> bool {
        false
    }
}

pub(crate) struct LimitsSettingsContent {
    overlay: LimitsOverlay,
}

impl LimitsSettingsContent {
    pub(crate) fn new(content: LimitsOverlayContent) -> Self {
        Self { overlay: LimitsOverlay::new(content) }
    }

    pub(crate) fn set_content(&mut self, content: LimitsOverlayContent) {
        self.overlay.set_content(content);
    }

    fn render_tabs(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::widgets::Paragraph;

        if area.width == 0 || area.height == 0 {
            return;
        }

        if let Some(tabs) = self.overlay.tabs() {
            let mut spans = Vec::new();
            for (idx, tab) in tabs.iter().enumerate() {
                let selected = idx == self.overlay.selected_tab();
                let style = if selected {
                    Style::default()
                        .fg(crate::colors::text())
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default().fg(crate::colors::text_dim())
                };
                spans.push(Span::styled(format!(" {} ", tab.title), style));
                spans.push(Span::raw(" "));
            }
            Paragraph::new(Line::from(spans))
                .style(Style::default().bg(crate::colors::background()))
                .render(area, buf);
        }
    }

    fn render_body(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::widgets::Paragraph;
        use ratatui::widgets::Wrap;

        if area.width == 0 || area.height == 0 {
            self.overlay.set_visible_rows(0);
            self.overlay.set_max_scroll(0);
            return;
        }

        self.overlay.set_visible_rows(area.height);

        let lines = self.overlay.lines_for_width(area.width);
        let max_scroll = lines.len().saturating_sub(area.height as usize) as u16;
        self.overlay.set_max_scroll(max_scroll);

        let start = self.overlay.scroll() as usize;
        let end = (start + area.height as usize).min(lines.len());
        let viewport = if start < end {
            lines[start..end].to_vec()
        } else {
            Vec::new()
        };

        Paragraph::new(viewport)
            .wrap(Wrap { trim: true })
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .render(area, buf);
    }
}

impl SettingsContent for LimitsSettingsContent {
    fn render(&self, area: Rect, buf: &mut Buffer) {
        use ratatui::widgets::Block;
        use ratatui::widgets::Borders;

        let block = Block::default()
            .borders(Borders::ALL)
            .title(Line::from(vec![
                Span::styled(" Rate limits ", Style::default().fg(crate::colors::text())),
                Span::styled("——— ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("↑↓", Style::default().fg(crate::colors::function())),
                Span::styled(" scroll  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("◂ ▸", Style::default().fg(crate::colors::function())),
                Span::styled(" change", Style::default().fg(crate::colors::text_dim())),
            ]))
            .style(Style::default().bg(crate::colors::background()))
            .border_style(Style::default().fg(crate::colors::border()));
        let inner = block.inner(area);
        block.render(area, buf);

        if inner.width == 0 || inner.height == 0 {
            return;
        }

        let (tabs_area, body_area) = if self.overlay.tab_count() > 1 {
            let [tabs_area, body_area] =
                Layout::vertical([Constraint::Length(2), Constraint::Fill(1)]).areas(inner);
            (Some(tabs_area), body_area)
        } else {
            (None, inner)
        };

        if let Some(tabs_rect) = tabs_area {
            self.render_tabs(tabs_rect, buf);
        }

        self.render_body(body_area.inner(Margin::new(1, 1)), buf);
    }

    fn handle_key(&mut self, key: KeyEvent) -> bool {
        match key.code {
            KeyCode::Up => {
                let current = self.overlay.scroll();
                if current > 0 {
                    self.overlay.set_scroll(current - 1);
                }
                true
            }
            KeyCode::Down => {
                let current = self.overlay.scroll();
                let next = current.saturating_add(1).min(self.overlay.max_scroll());
                self.overlay.set_scroll(next);
                true
            }
            KeyCode::PageUp => {
                let step = self.overlay.visible_rows().max(1);
                let current = self.overlay.scroll();
                self.overlay.set_scroll(current.saturating_sub(step));
                true
            }
            KeyCode::PageDown | KeyCode::Char(' ') => {
                let step = self.overlay.visible_rows().max(1);
                let current = self.overlay.scroll();
                let next = current.saturating_add(step).min(self.overlay.max_scroll());
                self.overlay.set_scroll(next);
                true
            }
            KeyCode::Home => {
                self.overlay.set_scroll(0);
                true
            }
            KeyCode::End => {
                self.overlay.set_scroll(self.overlay.max_scroll());
                true
            }
            KeyCode::Left | KeyCode::Char('[') => self.overlay.select_prev_tab(),
            KeyCode::Right | KeyCode::Char(']') => self.overlay.select_next_tab(),
            KeyCode::Tab => {
                if key.modifiers.contains(KeyModifiers::SHIFT) {
                    self.overlay.select_prev_tab()
                } else {
                    self.overlay.select_next_tab()
                }
            }
            KeyCode::BackTab => self.overlay.select_prev_tab(),
            _ => false,
        }
    }

    fn is_complete(&self) -> bool {
        false
    }
}

pub(crate) struct ChromeSettingsContent {
    selected_index: usize,
    app_event_tx: AppEventSender,
    port: Option<u16>,
    is_complete: bool,
}

impl ChromeSettingsContent {
    pub(crate) fn new(app_event_tx: AppEventSender, port: Option<u16>) -> Self {
        Self {
            selected_index: 0,
            app_event_tx,
            port,
            is_complete: false,
        }
    }

    fn options() -> &'static [(ChromeLaunchOption, &'static str, &'static str)] {
        CHROME_LAUNCH_CHOICES
    }

    fn move_up(&mut self) {
        let len = Self::options().len();
        if self.selected_index == 0 {
            self.selected_index = len.saturating_sub(1);
        } else {
            self.selected_index -= 1;
        }
    }

    fn move_down(&mut self) {
        let len = Self::options().len();
        if len > 0 {
            self.selected_index = (self.selected_index + 1) % len;
        }
    }

    fn confirm(&mut self) {
        if let Some((option, _, _)) = Self::options().get(self.selected_index) {
            let _ = self
                .app_event_tx
                .send(AppEvent::ChromeLaunchOptionSelected(*option, self.port));
            self.is_complete = true;
        }
    }

    fn cancel(&mut self) {
        let _ = self.app_event_tx.send(AppEvent::ChromeLaunchOptionSelected(
            ChromeLaunchOption::Cancel,
            self.port,
        ));
        self.is_complete = true;
    }
}

impl SettingsContent for ChromeSettingsContent {
    fn render(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        Clear.render(area, buf);

        let block = Block::default()
            .borders(Borders::ALL)
            .title(Line::from(" Chrome Launch Options "))
            .title_alignment(Alignment::Center)
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .border_style(Style::default().fg(crate::colors::border()));
        let inner = block.inner(area);
        block.render(area, buf);

        if inner.width == 0 || inner.height == 0 {
            return;
        }

        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::from(vec![Span::styled(
            "Chrome is already running or CDP connection failed",
            Style::default()
                .fg(crate::colors::warning())
                .add_modifier(Modifier::BOLD),
        )]));
        lines.push(Line::from(""));
        lines.push(Line::from("Select an option:"));
        lines.push(Line::from(""));

        for (idx, (_, label, description)) in Self::options().iter().enumerate() {
            let selected = idx == self.selected_index;
            if selected {
                lines.push(Line::from(vec![Span::styled(
                    format!("› {}", label),
                    Style::default()
                        .fg(crate::colors::success())
                        .add_modifier(Modifier::BOLD),
                )]));
                lines.push(Line::from(vec![Span::styled(
                    format!("  {}", description),
                    Style::default().fg(crate::colors::secondary()),
                )]));
            } else {
                lines.push(Line::from(vec![Span::styled(
                    format!("  {}", label),
                    Style::default().fg(crate::colors::text()),
                )]));
                lines.push(Line::from(vec![Span::styled(
                    format!("  {}", description),
                    Style::default().fg(crate::colors::text_dim()),
                )]));
            }
            lines.push(Line::from(""));
        }

        lines.push(Line::from(vec![
            Span::styled("↑↓/jk", Style::default().fg(crate::colors::function())),
            Span::styled(" move  ", Style::default().fg(crate::colors::text_dim())),
            Span::styled("Enter", Style::default().fg(crate::colors::function())),
            Span::styled(" select  ", Style::default().fg(crate::colors::text_dim())),
            Span::styled("Esc/q", Style::default().fg(crate::colors::function())),
            Span::styled(" cancel", Style::default().fg(crate::colors::text_dim())),
        ]));

        let content_area = inner.inner(Margin::new(1, 1));
        if content_area.width == 0 || content_area.height == 0 {
            return;
        }

        Paragraph::new(lines)
            .alignment(Alignment::Left)
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .render(content_area, buf);
    }

    fn handle_key(&mut self, key: KeyEvent) -> bool {
        match key.code {
            KeyCode::Up | KeyCode::Char('k') => {
                self.move_up();
                true
            }
            KeyCode::Down | KeyCode::Char('j') => {
                self.move_down();
                true
            }
            KeyCode::Enter => {
                self.confirm();
                true
            }
            KeyCode::Esc | KeyCode::Char('q') => {
                self.cancel();
                true
            }
            _ => false,
        }
    }

    fn is_complete(&self) -> bool {
        self.is_complete
    }
}

/// Full-screen settings overlay rendered by the chat widget.
pub(crate) struct SettingsOverlayView {
    mode: SettingsOverlayMode,
    last_section: SettingsSection,
    model_content: Option<ModelSettingsContent>,
    theme_content: Option<ThemeSettingsContent>,
    notifications_content: Option<NotificationsSettingsContent>,
    mcp_content: Option<McpSettingsContent>,
    agents_content: Option<AgentsSettingsContent>,
    limits_content: Option<LimitsSettingsContent>,
    chrome_content: Option<ChromeSettingsContent>,
}

impl SettingsOverlayView {
    pub(crate) fn new(section: SettingsSection) -> Self {
        let section_state = SectionState::new(section);
        Self {
            mode: SettingsOverlayMode::Section(section_state),
            last_section: section,
            model_content: None,
            theme_content: None,
            notifications_content: None,
            mcp_content: None,
            agents_content: None,
            limits_content: None,
            chrome_content: None,
        }
    }

    pub(crate) fn active_section(&self) -> SettingsSection {
        match self.mode {
            SettingsOverlayMode::Menu(state) => state.selected(),
            SettingsOverlayMode::Section(state) => state.active(),
        }
    }

    pub(crate) fn is_menu_active(&self) -> bool {
        matches!(self.mode, SettingsOverlayMode::Menu(_))
    }

    pub(crate) fn set_mode_menu(&mut self, selected: Option<SettingsSection>) {
        let section = selected.unwrap_or(self.last_section);
        self.mode = SettingsOverlayMode::Menu(MenuState::new(section));
    }

    pub(crate) fn set_mode_section(&mut self, section: SettingsSection) {
        self.mode = SettingsOverlayMode::Section(SectionState::new(section));
        self.last_section = section;
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

    pub(crate) fn set_agents_content(&mut self, content: AgentsSettingsContent) {
        self.agents_content = Some(content);
    }

    pub(crate) fn set_limits_content(&mut self, content: LimitsSettingsContent) {
        self.limits_content = Some(content);
    }

    pub(crate) fn set_chrome_content(&mut self, content: ChromeSettingsContent) {
        self.chrome_content = Some(content);
    }

    #[cfg_attr(not(any(test, feature = "test-helpers")), allow(dead_code))]
    pub(crate) fn agents_content(&self) -> Option<&AgentsSettingsContent> {
        self.agents_content.as_ref()
    }

    pub(crate) fn agents_content_mut(&mut self) -> Option<&mut AgentsSettingsContent> {
        self.agents_content.as_mut()
    }

    pub(crate) fn limits_content_mut(&mut self) -> Option<&mut LimitsSettingsContent> {
        self.limits_content.as_mut()
    }

    pub(crate) fn set_section(&mut self, section: SettingsSection) -> bool {
        if self.active_section() == section {
            return false;
        }
        self.last_section = section;
        match &mut self.mode {
            SettingsOverlayMode::Menu(state) => state.set_selected(section),
            SettingsOverlayMode::Section(state) => state.set_active(section),
        }
        true
    }

    pub(crate) fn select_next(&mut self) -> bool {
        let mut idx = self.index_of(self.active_section());
        idx = (idx + 1) % SettingsSection::ALL.len();
        self.set_section(SettingsSection::ALL[idx])
    }

    pub(crate) fn select_previous(&mut self) -> bool {
        let mut idx = self.index_of(self.active_section());
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
            Constraint::Length(2),
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
        let [crumb_area, hint_area] = Layout::vertical([Constraint::Length(1), Constraint::Fill(1)]).areas(area);

        let crumb = {
            let mut spans: Vec<Span<'static>> = Vec::new();
            spans.push(Span::styled("Settings", Style::default().fg(crate::colors::text())));
            spans.push(Span::styled(" ▸ ", Style::default().fg(crate::colors::text_dim())));
            let section_label = if self.is_menu_active() {
                "Menu".to_string()
            } else {
                self.active_section().label().to_string()
            };
            spans.push(Span::styled(section_label, Style::default().fg(crate::colors::text())));
            Line::from(spans)
        };
        Paragraph::new(crumb)
            .style(Style::default().bg(crate::colors::background()))
            .render(crumb_area, buf);

        if hint_area.height == 0 {
            return;
        }

        let hints = if self.is_menu_active() {
            Line::from(vec![
                Span::styled("↑↓", Style::default().fg(crate::colors::text())),
                Span::styled(" move  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("Enter", Style::default().fg(crate::colors::text())),
                Span::styled(" open  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("Esc", Style::default().fg(crate::colors::text())),
                Span::styled(" close", Style::default().fg(crate::colors::text_dim())),
            ])
        } else {
            Line::from(vec![
                Span::styled("←", Style::default().fg(crate::colors::text())),
                Span::styled(" back  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("Esc", Style::default().fg(crate::colors::text())),
                Span::styled(" menu  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("↑↓", Style::default().fg(crate::colors::text())),
                Span::styled(" navigate", Style::default().fg(crate::colors::text_dim())),
            ])
        };

        Paragraph::new(hints)
            .style(Style::default().bg(crate::colors::background()))
            .render(hint_area, buf);
    }

    fn render_body(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        if self.is_menu_active() {
            self.render_menu(area, buf);
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

    fn render_menu(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        let items: Vec<ListItem> = SettingsSection::ALL
            .iter()
            .map(|section| {
                let is_active = *section == self.active_section();
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

        let list = List::new(items).block(
            Block::default()
                .borders(Borders::ALL)
                .title(Span::styled("Sections", Style::default().fg(crate::colors::text()))),
        );

        let mut state = ListState::default();
        let selected_idx = SettingsSection::ALL
            .iter()
            .position(|section| *section == self.active_section());
        state.select(selected_idx);
        ratatui::widgets::StatefulWidget::render(list, area, buf, &mut state);
    }

    fn render_sidebar(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }
        let items: Vec<ListItem> = SettingsSection::ALL
            .iter()
            .map(|section| {
                let is_active = *section == self.active_section();
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
        let mut state = ListState::default();
        let selected_idx = SettingsSection::ALL
            .iter()
            .position(|section| *section == self.active_section());
        state.select(selected_idx);
        ratatui::widgets::StatefulWidget::render(list, area, buf, &mut state);
    }

    fn render_content(&self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }
        match self.active_section() {
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
            SettingsSection::Agents => {
                if let Some(content) = self.agents_content.as_ref() {
                    content.render(area, buf);
                    return;
                }
                self.render_placeholder(area, buf, SettingsSection::Agents.placeholder());
            }
            SettingsSection::Limits => {
                if let Some(content) = self.limits_content.as_ref() {
                    content.render(area, buf);
                    return;
                }
                self.render_placeholder(area, buf, SettingsSection::Limits.placeholder());
            }
            SettingsSection::Chrome => {
                if let Some(content) = self.chrome_content.as_ref() {
                    content.render(area, buf);
                    return;
                }
                self.render_placeholder(area, buf, SettingsSection::Chrome.placeholder());
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
        }
    }

    fn render_placeholder(&self, area: Rect, buf: &mut Buffer, text: &'static str) {
        let paragraph = Paragraph::new(text)
            .wrap(ratatui::widgets::Wrap { trim: true })
            .style(Style::default().fg(crate::colors::text_dim()));
        paragraph.render(area, buf);
    }

    pub(crate) fn active_content_mut(&mut self) -> Option<&mut dyn SettingsContent> {
        if self.is_menu_active() {
            return None;
        }

        match self.active_section() {
            SettingsSection::Model => self
                .model_content
                .as_mut()
                .map(|content| content as &mut dyn SettingsContent),
            SettingsSection::Theme => self
                .theme_content
                .as_mut()
                .map(|content| content as &mut dyn SettingsContent),
            SettingsSection::Agents => self
                .agents_content
                .as_mut()
                .map(|content| content as &mut dyn SettingsContent),
            SettingsSection::Limits => self
                .limits_content
                .as_mut()
                .map(|content| content as &mut dyn SettingsContent),
            SettingsSection::Chrome => self
                .chrome_content
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
        }
    }

    pub(crate) fn notify_close(&mut self) {
        match self.active_section() {
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
            SettingsSection::Chrome => {
                if let Some(content) = self.chrome_content.as_mut() {
                    content.on_close();
                }
            }
            _ => {}
        }
    }
}
