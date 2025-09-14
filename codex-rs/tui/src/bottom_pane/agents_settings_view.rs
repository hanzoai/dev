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

        ListSelectionView::new(
            " Agent Commands ".to_string(),
            Some("Press Enter to configure".to_string()),
            Some("Esc cancel".to_string()),
            items,
            self.app_event_tx.clone(),
        )
    }
}

#[derive(Clone, Debug)]
pub struct SubagentEditorView {
    name: String,
    read_only: bool,
    selected_agent_indices: Vec<usize>,
    orchestrator: String,
    agent: String,
    available_agents: Vec<String>,
    is_new: bool,
    field: usize, // 0 name, 1 mode, 2 agents, 3 orch, 4 agent, 5 save, 6 cancel
    app_event_tx: AppEventSender,
}

impl SubagentEditorView {
    pub fn new(root: &AgentsSettingsView, name: &str) -> Self {
        let mut me = Self {
            name: name.to_string(),
            read_only: matches!(name, "plan" | "solve"),
            selected_agent_indices: Vec::new(),
            orchestrator: String::new(),
            agent: String::new(),
            available_agents: root.available_agents.clone(),
            is_new: name.is_empty(),
            field: 0,
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
            KeyEvent { code: KeyCode::Esc, .. } => { /* close handled by parent */ }
            KeyEvent { code: KeyCode::Tab, .. } => { self.field = (self.field + 1).min(6); }
            KeyEvent { code: KeyCode::BackTab, .. } => { if self.field > 0 { self.field -= 1; } }
            KeyEvent { code: KeyCode::Left | KeyCode::Right, .. } if self.field == 1 => { self.read_only = !self.read_only; }
            KeyEvent { code: KeyCode::Char(' '), .. } if self.field == 2 => {
                // toggle currently highlighted agent (first one for simplicity)
                self.toggle_agent_at(0);
            }
            KeyEvent { code: KeyCode::Enter, .. } if self.field == 5 => { self.save(); }
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

    fn is_complete(&self) -> bool { false }

    fn desired_height(&self, _width: u16) -> u16 { 20 }

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

        lines.push(Line::from(vec![Span::styled("Name: ", Style::default()), Span::styled(self.name.clone(), sel(0))]));
        let mode_str = if self.read_only { "read-only" } else { "write" };
        lines.push(Line::from(vec![Span::styled("Mode: ", Style::default()), Span::styled(mode_str.to_string(), sel(1))]));

        // Agents selection (simple summary; toggle behavior simplified)
        let mut sel_names: Vec<String> = Vec::new();
        for (idx, a) in self.available_agents.iter().enumerate() {
            let checked = if self.selected_agent_indices.contains(&idx) { "[x]" } else { "[ ]" };
            sel_names.push(format!("{} {}", checked, a));
        }
        lines.push(Line::from("Agents:"));
        lines.push(Line::from(vec![Span::styled(sel_names.join(", "), sel(2))]));

        lines.push(Line::from(""));
        lines.push(Line::from("Instructions to Code (orchestrator):"));
        lines.push(Line::from(Span::styled(self.orchestrator.clone(), sel(3))));
        lines.push(Line::from(""));
        lines.push(Line::from("Instructions to each agent:"));
        lines.push(Line::from(Span::styled(self.agent.clone(), sel(4))));
        lines.push(Line::from(""));

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
