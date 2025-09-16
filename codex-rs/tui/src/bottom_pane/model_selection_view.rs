use codex_common::model_presets::ModelPreset;
use codex_core::config_types::ReasoningEffort;
use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use super::bottom_pane_view::BottomPaneView;
use super::BottomPane;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyModifiers;
use ratatui::buffer::Buffer;
use ratatui::layout::Alignment;
use ratatui::layout::Rect;
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::text::Line;
use ratatui::text::Span;
use ratatui::widgets::Block;
use ratatui::widgets::Borders;
use ratatui::widgets::Clear;
use ratatui::widgets::Paragraph;
use ratatui::widgets::Widget;

pub(crate) struct ModelSelectionView {
    presets: Vec<ModelPreset>,
    selected_index: usize,
    current_model: String,
    current_effort: ReasoningEffort,
    app_event_tx: AppEventSender,
    is_complete: bool,
}

impl ModelSelectionView {
    pub fn new(
        presets: Vec<ModelPreset>,
        current_model: String,
        current_effort: ReasoningEffort,
        app_event_tx: AppEventSender,
    ) -> Self {
        let initial_index = Self::initial_selection(&presets, &current_model, current_effort);
        Self {
            presets,
            selected_index: initial_index,
            current_model,
            current_effort,
            app_event_tx,
            is_complete: false,
        }
    }

    fn initial_selection(
        presets: &[ModelPreset],
        current_model: &str,
        current_effort: ReasoningEffort,
    ) -> usize {
        // Prefer an exact match on model + effort, fall back to first model match, then first entry.
        if let Some((idx, _)) = presets.iter().enumerate().find(|(_, preset)| {
            preset.model.eq_ignore_ascii_case(current_model)
                && Self::preset_effort(preset) == current_effort
        }) {
            return idx;
        }

        if let Some((idx, _)) = presets
            .iter()
            .enumerate()
            .find(|(_, preset)| preset.model.eq_ignore_ascii_case(current_model))
        {
            return idx;
        }

        0
    }

    fn preset_effort(preset: &ModelPreset) -> ReasoningEffort {
        preset
            .effort
            .map(ReasoningEffort::from)
            .unwrap_or(ReasoningEffort::Medium)
    }

    fn format_model_header(model: &str) -> String {
        if let Some((prefix, rest)) = model.split_once('-') {
            format!("{}-{}", prefix.to_ascii_uppercase(), rest)
        } else {
            let mut chars = model.chars();
            match chars.next() {
                Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
                None => String::new(),
            }
        }
    }

    fn move_selection_up(&mut self) {
        if self.presets.is_empty() {
            return;
        }
        self.selected_index = if self.selected_index == 0 {
            self.presets.len() - 1
        } else {
            self.selected_index - 1
        };
    }

    fn move_selection_down(&mut self) {
        if self.presets.is_empty() {
            return;
        }
        self.selected_index = (self.selected_index + 1) % self.presets.len();
    }

    fn confirm_selection(&mut self) {
        if let Some(preset) = self.presets.get(self.selected_index) {
            let effort = Self::preset_effort(preset);
            let _ = self.app_event_tx.send(AppEvent::UpdateModelSelection {
                model: preset.model.to_string(),
                effort: Some(effort),
            });
        }
        self.is_complete = true;
    }
}

impl<'a> BottomPaneView<'a> for ModelSelectionView {
    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        match key_event {
            KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.move_selection_up();
            }
            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.move_selection_down();
            }
            KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.confirm_selection();
            }
            KeyEvent {
                code: KeyCode::Esc,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.is_complete = true;
            }
            _ => {}
        }
    }

    fn is_complete(&self) -> bool {
        self.is_complete
    }

    fn desired_height(&self, _width: u16) -> u16 {
        // Title + current selection + spacing + presets + footer
        (self.presets.len() as u16 + 6).max(9)
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        Clear.render(area, buf);

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()))
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .title(" Select Model & Reasoning ")
            .title_alignment(Alignment::Center);

        let inner_area = block.inner(area);
        block.render(area, buf);

        let mut lines: Vec<Line> = Vec::new();
        lines.push(Line::from(vec![
            Span::styled("Current model: ", Style::default().fg(crate::colors::text_dim())),
            Span::styled(
                self.current_model.clone(),
                Style::default()
                    .fg(crate::colors::warning())
                    .add_modifier(Modifier::BOLD),
            ),
        ]));
        lines.push(Line::from(vec![
            Span::styled("Reasoning effort: ", Style::default().fg(crate::colors::text_dim())),
            Span::styled(
                format!("{}", self.current_effort),
                Style::default()
                    .fg(crate::colors::warning())
                    .add_modifier(Modifier::BOLD),
            ),
        ]));
        lines.push(Line::from(""));

        let mut previous_model: Option<&str> = None;

        for (idx, preset) in self.presets.iter().enumerate() {
            if previous_model
                .map(|m| !m.eq_ignore_ascii_case(&preset.model))
                .unwrap_or(true)
            {
                if !lines.is_empty() {
                    lines.push(Line::from(""));
                }
                lines.push(Line::from(vec![Span::styled(
                    Self::format_model_header(&preset.model),
                    Style::default()
                        .fg(crate::colors::text_bright())
                        .add_modifier(Modifier::BOLD),
                )]));
                previous_model = Some(&preset.model);
            }

            let is_selected = idx == self.selected_index;
            let preset_effort = Self::preset_effort(preset);
            let is_current = preset.model.eq_ignore_ascii_case(&self.current_model)
                && preset_effort == self.current_effort;
            let is_default_effort = preset.effort.is_none();

            let mut style = Style::default().fg(crate::colors::text());
            if is_selected {
                style = style
                    .bg(crate::colors::selection())
                    .add_modifier(Modifier::BOLD);
            }
            if is_current {
                style = style.fg(crate::colors::success());
            }

            let prefix = if is_selected { "› " } else { "  " };
            let mut spans = vec![
                Span::raw("  "),
                Span::raw(prefix),
                Span::styled(preset.label.to_string(), style),
            ];

            let detail = if is_default_effort {
                format!("({} · default)", preset_effort)
            } else {
                format!("({})", preset_effort)
            };
            spans.push(Span::raw(" "));
            spans.push(Span::styled(
                detail,
                Style::default().fg(crate::colors::text_dim()),
            ));

            if !preset.description.is_empty() {
                spans.push(Span::raw(" "));
                spans.push(Span::styled(
                    preset.description,
                    Style::default().fg(crate::colors::dim()),
                ));
            }

            lines.push(Line::from(spans));
        }

        lines.push(Line::from(""));
        lines.push(Line::from(vec![
            Span::styled("↑↓", Style::default().fg(crate::colors::light_blue())),
            Span::raw(" Navigate  "),
            Span::styled("Enter", Style::default().fg(crate::colors::success())),
            Span::raw(" Select  "),
            Span::styled("Esc", Style::default().fg(crate::colors::error())),
            Span::raw(" Cancel"),
        ]));

        let padded = Rect {
            x: inner_area.x.saturating_add(1),
            y: inner_area.y,
            width: inner_area.width.saturating_sub(1),
            height: inner_area.height,
        };

        let paragraph = Paragraph::new(lines)
            .alignment(Alignment::Left)
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()));
        paragraph.render(padded, buf);
    }
}
