use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph, Widget};

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;

use super::bottom_pane_view::BottomPaneView;
use super::list_selection_view::{ListSelectionView, SelectionAction, SelectionItem};
use super::BottomPane;

#[derive(Clone, Debug)]
pub struct AgentsSettingsView {
    builtins: Vec<String>,
    custom: Vec<String>,
    existing: Vec<codex_core::config_types::SubagentCommandConfig>,
    available_agents: Vec<String>,
    app_event_tx: AppEventSender,
}

impl AgentsSettingsView {
    pub fn new(
        builtins: Vec<String>,
        custom: Vec<String>,
        existing: Vec<codex_core::config_types::SubagentCommandConfig>,
        available_agents: Vec<String>,
        app_event_tx: AppEventSender,
    ) -> Self {
        Self { builtins, custom, existing, available_agents, app_event_tx }
    }

    pub fn into_list_view(self) -> ListSelectionView {
        // Build items: built-ins first, then custom, then Add New…
        let mut items: Vec<SelectionItem> = Vec::new();
        let make_actions = |name: String| -> Vec<SelectionAction> {
            let name_clone = name.clone();
            let view_clone = self.clone();
            vec![Box::new(move |tx: &AppEventSender| {
                tx.send(AppEvent::ShowSubagentEditor { name: name_clone.clone(), available_agents: view_clone.available_agents.clone(), existing: view_clone.existing.clone(), is_new: false });
            })]
        };

        // Built-ins
        for name in &self.builtins {
            let desc = Some("(press Enter to configure)".to_string());
            items.push(SelectionItem { name: format!("/{}", name), description: desc, is_current: false, actions: make_actions(name.clone()) });
        }
        // Custom
        for name in &self.custom {
            let desc = Some("(press Enter to configure)".to_string());
            items.push(SelectionItem { name: format!("/{}", name), description: desc, is_current: false, actions: make_actions(name.clone()) });
        }
        // Add New…
        {
            let view_clone = self.clone();
            let actions: Vec<SelectionAction> = vec![Box::new(move |tx: &AppEventSender| {
                tx.send(AppEvent::ShowSubagentEditor { name: String::new(), available_agents: view_clone.available_agents.clone(), existing: view_clone.existing.clone(), is_new: true });
            })];
            items.push(SelectionItem { name: "Add new…".to_string(), description: None, is_current: false, actions });
        }

        // Show all built-ins and Add new…; avoid wrapping constraints
        let max_rows = items.len().max(4);
        ListSelectionView::new(
            " Agent Commands ".to_string(),
            None,
            Some("Esc cancel".to_string()),
            items,
            self.app_event_tx.clone(),
            max_rows,
        )
    }
}

#[derive(Clone, Debug)]
pub struct SubagentEditorView {
    name: String,
    read_only: bool,
    selected_agent_indices: Vec<usize>,
    agent_cursor: usize,
    orchestrator: String,
    agent: String,
    available_agents: Vec<String>,
    is_new: bool,
    field: usize, // 0 name, 1 mode, 2 agents, 3 orch, 4 agent, 5 save, 6 cancel
    is_complete: bool,
    app_event_tx: AppEventSender,
}

impl SubagentEditorView {
    pub fn new(root: &AgentsSettingsView, name: &str) -> Self {
        let mut me = Self {
            name: name.to_string(),
            read_only: matches!(name, "plan" | "solve"),
            selected_agent_indices: Vec::new(),
            agent_cursor: 0,
            orchestrator: String::new(),
            agent: String::new(),
            available_agents: root.available_agents.clone(),
            is_new: name.is_empty(),
            field: 0,
            is_complete: false,
            app_event_tx: root.app_event_tx.clone(),
        };
        // Seed from existing config if present
        if let Some(cfg) = root.existing.iter().find(|c| c.name.eq_ignore_ascii_case(name)) {
            me.name = cfg.name.clone();
            me.read_only = cfg.read_only;
            me.orchestrator = cfg.orchestrator_instructions.clone().unwrap_or_default();
            me.agent = cfg.agent_instructions.clone().unwrap_or_default();
            let set: std::collections::HashSet<String> = cfg.agents.iter().cloned().collect();
            for (idx, a) in me.available_agents.iter().enumerate() {
                if set.contains(a) { me.selected_agent_indices.push(idx); }
            }
        }
        me
    }

    pub fn new_with_data(
        name: String,
        available_agents: Vec<String>,
        existing: Vec<codex_core::config_types::SubagentCommandConfig>,
        is_new: bool,
        app_event_tx: AppEventSender,
    ) -> Self {
        let root = AgentsSettingsView { builtins: vec![], custom: vec![], existing, available_agents, app_event_tx };
        let mut s = Self::new(&root, &name);
        s.is_new = is_new;
        s
    }

    fn toggle_agent_at(&mut self, idx: usize) {
        if let Some(pos) = self.selected_agent_indices.iter().position(|i| *i == idx) {
            self.selected_agent_indices.remove(pos);
        } else {
            self.selected_agent_indices.push(idx);
        }
    }

    fn save(&mut self) {
        let agents: Vec<String> = if self.selected_agent_indices.is_empty() {
            Vec::new()
        } else {
            self.selected_agent_indices.iter().filter_map(|i| self.available_agents.get(*i).cloned()).collect()
        };
        let cfg = codex_core::config_types::SubagentCommandConfig {
            name: self.name.clone(),
            read_only: self.read_only,
            agents,
            orchestrator_instructions: if self.orchestrator.trim().is_empty() { None } else { Some(self.orchestrator.clone()) },
            agent_instructions: if self.agent.trim().is_empty() { None } else { Some(self.agent.clone()) },
        };
        // Persist to disk
        if let Ok(home) = codex_core::config::find_codex_home() {
            let rt = tokio::runtime::Handle::current();
            let _ = rt.block_on(codex_core::config_edit::upsert_subagent_command(&home, &cfg));
        }
        // Update in-memory config
        self.app_event_tx.send(AppEvent::UpdateSubagentCommand(cfg));
    }
}

impl<'a> BottomPaneView<'a> for SubagentEditorView {
    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        match key_event {
            KeyEvent { code: KeyCode::Esc, .. } => { self.is_complete = true; }
            KeyEvent { code: KeyCode::Tab, .. } => { self.field = (self.field + 1).min(6); }
            KeyEvent { code: KeyCode::BackTab, .. } => { if self.field > 0 { self.field -= 1; } }
            KeyEvent { code: KeyCode::Up, .. } => { if self.field > 0 { self.field -= 1; } }
            KeyEvent { code: KeyCode::Down, .. } => { self.field = (self.field + 1).min(6); }
            KeyEvent { code: KeyCode::Left, .. } if self.field == 1 => { self.read_only = !self.read_only; }
            KeyEvent { code: KeyCode::Right, .. } if self.field == 1 => { self.read_only = !self.read_only; }
            KeyEvent { code: KeyCode::Enter, .. } if self.field == 1 => { self.read_only = !self.read_only; }
            KeyEvent { code: KeyCode::Left, .. } if self.field == 2 => { if self.agent_cursor > 0 { self.agent_cursor -= 1; } }
            KeyEvent { code: KeyCode::Right, .. } if self.field == 2 => { if self.agent_cursor + 1 < self.available_agents.len() { self.agent_cursor += 1; } }
            KeyEvent { code: KeyCode::Char(' '), .. } if self.field == 2 => {
                let idx = self.agent_cursor.min(self.available_agents.len().saturating_sub(1));
                self.toggle_agent_at(idx);
            }
            KeyEvent { code: KeyCode::Enter, .. } if self.field == 2 => {
                let idx = self.agent_cursor.min(self.available_agents.len().saturating_sub(1));
                self.toggle_agent_at(idx);
            }
            KeyEvent { code: KeyCode::Enter, .. } if self.field == 5 => { self.save(); }
            KeyEvent { code: KeyCode::Enter, .. } if self.field == 6 => { self.is_complete = true; }
            KeyEvent { code: KeyCode::Char(c), modifiers: KeyModifiers::NONE, .. } => {
                match self.field {
                    0 => self.name.push(c),
                    3 => self.orchestrator.push(c),
                    4 => self.agent.push(c),
                    _ => {}
                }
            }
            KeyEvent { code: KeyCode::Backspace, .. } => {
                match self.field {
                    0 => { self.name.pop(); },
                    3 => { self.orchestrator.pop(); },
                    4 => { self.agent.pop(); },
                    _ => {}
                }
            }
            _ => {}
        }
    }

    fn is_complete(&self) -> bool { self.is_complete }

    fn desired_height(&self, width: u16) -> u16 {
        // Approximate inner width: block has a border and we render with +1 left pad and -2 width
        let inner_w = width.saturating_sub(4).max(10) as usize;
        // Count wrapped lines for a given string and width.
        fn wrapped_lines(s: &str, w: usize) -> u16 {
            if s.is_empty() { return 1; }
            let mut lines: u16 = 0;
            for part in s.split('\n') {
                let len = part.chars().count();
                let mut l = (len / w) as u16;
                if len % w != 0 { l += 1; }
                if l == 0 { l = 1; }
                lines = lines.saturating_add(l);
            }
            lines.max(1)
        }

        // Static rows: Name(1), Mode(1), Agents label(1), buttons(1)
        let static_rows: u16 = 4;
        // Agents row typically one line; make it at least 1
        let agents_row: u16 = 1;
        let orch_rows = wrapped_lines(&self.orchestrator, inner_w);
        let agent_rows = wrapped_lines(&self.agent, inner_w);
        // Sum + a tiny breathing room
        static_rows
            .saturating_add(agents_row)
            .saturating_add(orch_rows)
            .saturating_add(agent_rows)
            .saturating_add(1) // spacer
            .clamp(8, 40)
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        Clear.render(area, buf);
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()))
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .title(" Configure Agent Command ")
            .title_alignment(Alignment::Center);
        let inner = block.inner(area);
        block.render(area, buf);

        let mut lines: Vec<Line<'static>> = Vec::new();
        let sel = |idx: usize| if self.field == idx { Style::default().bg(crate::colors::selection()).add_modifier(Modifier::BOLD) } else { Style::default() };
        let label = |idx: usize| if self.field == idx { Style::default().fg(crate::colors::primary()).add_modifier(Modifier::BOLD) } else { Style::default() };

        // Name with cursor
        let mut name_render = self.name.clone();
        if self.field == 0 { name_render.push('▌'); }
        lines.push(Line::from(vec![Span::styled("Name: ", label(0)), Span::styled(name_render, sel(0))]));
        let mode_str = if self.read_only { "read-only" } else { "write" };
        lines.push(Line::from(vec![Span::styled("Mode: ", label(1)), Span::styled(mode_str.to_string(), sel(1))]));

        // Agents selection with cursor highlight
        let mut spans: Vec<Span> = Vec::new();
        for (idx, a) in self.available_agents.iter().enumerate() {
            let checked = if self.selected_agent_indices.contains(&idx) { "[x]" } else { "[ ]" };
            let mut style = sel(2);
            if idx == self.agent_cursor { style = style.fg(crate::colors::primary()).add_modifier(Modifier::BOLD); }
            spans.push(Span::styled(format!("{} {}", checked, a), style));
            spans.push(Span::raw("  "));
        }
        lines.push(Line::from(Span::styled("Agents:", label(2))));
        lines.push(Line::from(spans));

        // Orchestrator with cursor
        let mut orch_render = self.orchestrator.clone();
        if self.field == 3 { orch_render.push('▌'); }
        lines.push(Line::from(Span::styled("Instructions to Code (orchestrator):", label(3))));
        lines.push(Line::from(Span::styled(orch_render, sel(3))));

        // Agent with cursor
        let mut agent_render = self.agent.clone();
        if self.field == 4 { agent_render.push('▌'); }
        lines.push(Line::from(Span::styled("Instructions to each agent:", label(4))));
        lines.push(Line::from(Span::styled(agent_render, sel(4))));

        let save_style = sel(5).fg(crate::colors::success());
        let cancel_style = sel(6).fg(crate::colors::error());
        lines.push(Line::from(vec![
            Span::styled("[ Save ]  ", save_style),
            Span::styled("[ Cancel ]", cancel_style),
        ]));

        let paragraph = Paragraph::new(lines)
            .alignment(Alignment::Left)
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()));
        paragraph.render(Rect { x: inner.x.saturating_add(1), y: inner.y, width: inner.width.saturating_sub(2), height: inner.height }, buf);
    }
}
