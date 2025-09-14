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
use super::form_text_field::FormTextField;

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
        let subtitle = "Configure which agents run for each command. Press Enter to configure.".to_string();
        ListSelectionView::new(
            " Agent Commands ".to_string(),
            Some(subtitle),
            Some("Esc cancel".to_string()),
            items,
            self.app_event_tx.clone(),
            max_rows,
        )
    }
}

#[derive(Debug)]
pub struct SubagentEditorView {
    name_field: FormTextField,
    read_only: bool,
    selected_agent_indices: Vec<usize>,
    agent_cursor: usize,
    orch_field: FormTextField,
    agent_field: FormTextField,
    available_agents: Vec<String>,
    is_new: bool,
    field: usize, // 0 name, 1 mode, 2 agents, 3 orch, 4 agent, 5 save, 6 cancel
    is_complete: bool,
    app_event_tx: AppEventSender,
}

impl SubagentEditorView {
    pub fn new(root: &AgentsSettingsView, name: &str) -> Self {
        let mut me = Self {
            name_field: FormTextField::new_single_line(),
            read_only: matches!(name, "plan" | "solve"),
            selected_agent_indices: Vec::new(),
            agent_cursor: 0,
            orch_field: FormTextField::new_multi_line(),
            agent_field: FormTextField::new_multi_line(),
            available_agents: root.available_agents.clone(),
            is_new: name.is_empty(),
            field: 0,
            is_complete: false,
            app_event_tx: root.app_event_tx.clone(),
        };
        // Seed from existing config if present
        if let Some(cfg) = root.existing.iter().find(|c| c.name.eq_ignore_ascii_case(name)) {
            me.name_field.set_text(&cfg.name);
            me.read_only = cfg.read_only;
            me.orch_field.set_text(&cfg.orchestrator_instructions.clone().unwrap_or_default());
            me.agent_field.set_text(&cfg.agent_instructions.clone().unwrap_or_default());
            let set: std::collections::HashSet<String> = cfg.agents.iter().cloned().collect();
            for (idx, a) in me.available_agents.iter().enumerate() {
                if set.contains(a) { me.selected_agent_indices.push(idx); }
            }
        } else {
            // No user config yet; provide sensible defaults for built-ins so users can edit them
            match name.to_lowercase().as_str() {
                "plan" => {
                    me.read_only = true;
                    me.orch_field.set_text("Plan a multi-agent approach. Research the repo structure, enumerate tasks, dependencies, risks, and milestones. Use multiple agents in read-only mode and synthesize a single, actionable plan.");
                    me.agent_field.set_text("Study the codebase, cite files you read, list assumptions. Propose concrete steps and call out risks or unknowns.");
                }
                "solve" => {
                    me.read_only = true;
                    me.orch_field.set_text("Coordinate multiple agents to propose competing solutions. Keep all agents read-only. Compare proposals, pick one, and outline verification steps.");
                    me.agent_field.set_text("Propose a fix with exact steps to validate. Be explicit about tests to run and edge cases to check.");
                }
                "code" => {
                    me.read_only = false;
                    me.orch_field.set_text("Coordinate write-mode agents to implement the task in isolated worktrees. Surface worktree_path and branch_name after completion.");
                    me.agent_field.set_text("Make minimal, focused changes with clear rationale. Include tests or validation steps where possible.");
                }
                _ => { me.name_field.set_text(name); }
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
            name: self.name_field.text().to_string(),
            read_only: self.read_only,
            agents,
            orchestrator_instructions: {
                let t = self.orch_field.text().trim().to_string();
                if t.is_empty() { None } else { Some(t) }
            },
            agent_instructions: {
                let t = self.agent_field.text().trim().to_string();
                if t.is_empty() { None } else { Some(t) }
            },
        };
        // Persist to disk asynchronously to avoid blocking the TUI runtime
        if let Ok(home) = codex_core::config::find_codex_home() {
            let cfg_clone = cfg.clone();
            tokio::spawn(async move {
                let _ = codex_core::config_edit::upsert_subagent_command(&home, &cfg_clone).await;
            });
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
            KeyEvent { code: KeyCode::Up, modifiers, .. } => {
                if (self.field == 3 || self.field == 4) && modifiers.contains(KeyModifiers::SHIFT) {
                    let _ = match self.field { 3 => self.orch_field.handle_key(key_event), 4 => self.agent_field.handle_key(key_event), _ => false };
                } else if self.field > 0 { self.field -= 1; }
            }
            KeyEvent { code: KeyCode::Down, modifiers, .. } => {
                if (self.field == 3 || self.field == 4) && modifiers.contains(KeyModifiers::SHIFT) {
                    let _ = match self.field { 3 => self.orch_field.handle_key(key_event), 4 => self.agent_field.handle_key(key_event), _ => false };
                } else { self.field = (self.field + 1).min(6); }
            }
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
            // Delegate input to focused text fields (handles Shift‑chars, Enter/newline, undo, etc.)
            ev @ KeyEvent { .. } if self.field == 0 => { let _ = self.name_field.handle_key(ev); }
            ev @ KeyEvent { .. } if self.field == 3 => { let _ = self.orch_field.handle_key(ev); }
            ev @ KeyEvent { .. } if self.field == 4 => { let _ = self.agent_field.handle_key(ev); }
            KeyEvent { code: KeyCode::Enter, .. } if self.field == 5 => { self.save(); }
            KeyEvent { code: KeyCode::Enter, .. } if self.field == 6 => { self.is_complete = true; }
            _ => {}
        }
    }

    fn is_complete(&self) -> bool { self.is_complete }

    fn handle_paste(&mut self, text: String) -> super::bottom_pane_view::ConditionalUpdate {
        match self.field {
            0 => self.name_field.handle_paste(text),
            3 => self.orch_field.handle_paste(text),
            4 => self.agent_field.handle_paste(text),
            _ => {}
        }
        super::bottom_pane_view::ConditionalUpdate::NeedsRedraw
    }

    fn desired_height(&self, width: u16) -> u16 {
        // Compute content width consistent with render: inner = width-2; content = inner-1
        let inner_w = width.saturating_sub(2);
        let content_w = inner_w.saturating_sub(1).max(10) as usize;
        // Static rows (content lines):
        // Name(1), Mode(1), Agents label(1), Agents line(1),
        // Orchestrator label(1), Agent label(1), Buttons(1)
        let static_rows: u16 = 7;
        // Content rows for the two instruction bodies (wrapped)
        let orch_rows = self.orch_field.desired_height(content_w as u16);
        let agent_rows = self.agent_field.desired_height(content_w as u16);
        // Sum and add borders (2)
        let content_rows = static_rows
            .saturating_add(orch_rows)
            .saturating_add(agent_rows);
        (content_rows + 2).clamp(8, 50)
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

        // Name label (input will be drawn in-place to its right)
        lines.push(Line::from(vec![Span::styled("Name: ", label(0))]));
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

        // Orchestrator label (content drawn below)
        lines.push(Line::from(Span::styled("Instructions to Code (orchestrator):", label(3))));
        lines.push(Line::from(""));

        // Agent label (content drawn below)
        lines.push(Line::from(Span::styled("Instructions to each agent:", label(4))));
        lines.push(Line::from(""));

        let save_style = sel(5).fg(crate::colors::success());
        let cancel_style = sel(6).fg(crate::colors::error());
        lines.push(Line::from(vec![
            Span::styled("[ Save ]  ", save_style),
            Span::styled("[ Cancel ]", cancel_style),
        ]));

        let paragraph = Paragraph::new(lines)
            .alignment(Alignment::Left)
            .wrap(ratatui::widgets::Wrap { trim: false })
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()));
        let content_rect = Rect { x: inner.x.saturating_add(1), y: inner.y, width: inner.width.saturating_sub(1), height: inner.height };
        paragraph.render(content_rect, buf);

        // Draw text fields over the paragraph using the same content rect
        let content_w = content_rect.width;
        let mut y = content_rect.y;

        // Row 0: Name input on the same line as label, to the right of "Name: "
        let name_label = "Name: ";
        let name_label_w = name_label.chars().count() as u16;
        let name_field_rect = Rect { x: content_rect.x + name_label_w, y, width: content_w.saturating_sub(name_label_w), height: 1 };
        self.name_field.render(name_field_rect, buf, self.field == 0);

        // Row 1: Mode; Row 2: Agents label; Row 3: Agents list
        y = y.saturating_add(3);

        // Row 4: Orchestrator label; draw field starting next row
        y = y.saturating_add(1);
        let orch_h = self.orch_field.desired_height(content_w);
        self.orch_field.render(Rect { x: content_rect.x, y, width: content_w, height: orch_h }, buf, self.field == 3);
        y = y.saturating_add(orch_h);

        // Next row is Agent label; draw field starting the row after
        y = y.saturating_add(1);
        let agent_h = self.agent_field.desired_height(content_w);
        self.agent_field.render(Rect { x: content_rect.x, y, width: content_w, height: agent_h }, buf, self.field == 4);
    }
}

impl SubagentEditorView {}

// (handle_paste implemented in BottomPaneView impl below)
