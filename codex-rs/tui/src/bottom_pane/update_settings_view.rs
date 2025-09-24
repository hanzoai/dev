use std::sync::{Arc, Mutex};

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::colors;
use super::bottom_pane_view::BottomPaneView;
use super::bottom_pane_view::ConditionalUpdate;
use super::BottomPane;
use crossterm::event::{KeyCode, KeyEvent};
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph, Wrap, Widget};

#[derive(Debug, Clone, Default)]
pub struct UpdateSharedState {
    pub checking: bool,
    pub latest_version: Option<String>,
    pub error: Option<String>,
}

pub(crate) struct UpdateSettingsView {
    app_event_tx: AppEventSender,
    field: usize,
    is_complete: bool,
    auto_enabled: bool,
    shared: Arc<Mutex<UpdateSharedState>>,
    current_version: String,
    command: Option<Vec<String>>,
    command_display: Option<String>,
    manual_instructions: Option<String>,
}

impl UpdateSettingsView {
    pub fn new(
        app_event_tx: AppEventSender,
        current_version: String,
        auto_enabled: bool,
        command: Option<Vec<String>>,
        command_display: Option<String>,
        manual_instructions: Option<String>,
        shared: Arc<Mutex<UpdateSharedState>>,
    ) -> Self {
        Self {
            app_event_tx,
            field: 0,
            is_complete: false,
            auto_enabled,
            shared,
            current_version,
            command,
            command_display,
            manual_instructions,
        }
    }

    fn toggle_auto(&mut self) {
        self.auto_enabled = !self.auto_enabled;
        self.app_event_tx
            .send(AppEvent::SetAutoUpgradeEnabled(self.auto_enabled));
    }

    fn invoke_run_upgrade(&mut self) {
        let state = self
            .shared
            .lock()
            .expect("update shared state poisoned")
            .clone();

        if self.command.is_none() {
            if let Some(instructions) = &self.manual_instructions {
                self.app_event_tx
                    .send_background_event(instructions.clone());
            }
            return;
        }

        if state.checking {
            self.app_event_tx
                .send_background_event("Still checking for updates…".to_string());
            return;
        }
        if let Some(err) = &state.error {
            self.app_event_tx
                .send_background_event(format!("❌ /update failed: {err}"));
            return;
        }
        let Some(latest) = state.latest_version.clone() else {
            self.app_event_tx
                .send_background_event("✅ Code is already up to date.".to_string());
            return;
        };

        let command = self.command.clone().expect("command checked above");
        let display = self
            .command_display
            .clone()
            .unwrap_or_else(|| command.join(" "));

        self.app_event_tx.send_background_event(format!(
            "⬆️ Update available: {} → {}. Running `{}`...",
            self.current_version, latest, display
        ));
        self.app_event_tx.send(AppEvent::RunUpdateCommand {
            command,
            display: display.clone(),
            latest_version: Some(latest.clone()),
        });
        self.app_event_tx.send_background_event_late(format!(
            "↻ Restart Code after `{}` completes to finish upgrading to {}.",
            display, latest
        ));
        self.is_complete = true;
    }

    fn build_lines(&self) -> Vec<Line<'static>> {
        let state = self
            .shared
            .lock()
            .expect("update shared state poisoned")
            .clone();

        let mut lines: Vec<Line<'static>> = Vec::new();

        // Header
        lines.push(Line::from(Span::styled(
            "Update",
            Style::default().add_modifier(Modifier::BOLD),
        )));
        lines.push(Line::from(""));

        // Run Upgrade row
        let run_selected = self.field == 0;
        let indicator_style = if run_selected {
            Style::default().fg(colors::primary())
        } else {
            Style::default()
        };
        let mut run_spans = vec![
            Span::styled(if run_selected { "› " } else { "  " }, indicator_style),
        ];
        let run_enabled = self.command.is_some()
            && state.error.is_none()
            && !state.checking
            && state.latest_version.is_some();
        let base_style = if run_selected {
            Style::default()
                .fg(colors::primary())
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default()
        };
        let disabled_style = Style::default().fg(colors::text_dim());
        run_spans.push(Span::styled(
            "Run Upgrade",
            if run_enabled { base_style } else { disabled_style },
        ));

        if state.checking {
            run_spans.push(Span::raw("  "));
            run_spans.push(Span::styled(
                "checking…",
                Style::default().fg(colors::text_dim()),
            ));
        } else if let Some(err) = &state.error {
            run_spans.push(Span::raw("  "));
            run_spans.push(Span::styled(
                err.clone(),
                Style::default().fg(colors::error()),
            ));
        } else if let Some(latest) = &state.latest_version {
            run_spans.push(Span::raw("  "));
            run_spans.push(Span::raw(format!(
                "{} → {}",
                self.current_version, latest
            )));
        } else {
            run_spans.push(Span::raw("  "));
            run_spans.push(Span::styled(
                format!("Current version: {}", self.current_version),
                Style::default().fg(colors::text_dim()),
            ));
        }
        lines.push(Line::from(run_spans));

        if let Some(instructions) = &self.manual_instructions {
            lines.push(Line::from(""));
            lines.push(Line::from(vec![Span::styled(
                instructions.clone(),
                Style::default().fg(colors::text_dim()),
            )]));
        }

        lines.push(Line::from(""));
        lines.push(Line::from(Span::styled(
            "Automatic Upgrades",
            Style::default().add_modifier(Modifier::BOLD),
        )));

        let toggle_selected = self.field == 1;
        let toggle_indicator_style = if toggle_selected {
            Style::default().fg(colors::primary())
        } else {
            Style::default()
        };
        let enabled_style = if self.auto_enabled {
            Style::default().fg(colors::success()).add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(colors::text_dim())
        };
        let disabled_style = if self.auto_enabled {
            Style::default().fg(colors::text_dim())
        } else {
            Style::default().fg(colors::success()).add_modifier(Modifier::BOLD)
        };
        lines.push(Line::from(vec![
            Span::styled(
                if toggle_selected { "› " } else { "  " },
                toggle_indicator_style,
            ),
            Span::styled(
                format!("[{}] Enabled", if self.auto_enabled { "x" } else { " " }),
                enabled_style,
            ),
            Span::raw("   "),
            Span::styled(
                format!("[{}] Disabled", if self.auto_enabled { " " } else { "x" }),
                disabled_style,
            ),
        ]));
        lines.push(Line::from(vec![
            Span::raw("  "),
            Span::styled("Toggle with Enter/Space", Style::default().fg(colors::text_dim())),
        ]));

        lines.push(Line::from(""));
        lines.push(Line::from(vec![
            Span::styled("Enter", Style::default().fg(colors::success())),
            Span::styled(" Run  ", Style::default().fg(colors::text_dim())),
            Span::styled("Space", Style::default().fg(colors::success())),
            Span::styled(" Toggle  ", Style::default().fg(colors::text_dim())),
            Span::styled("Esc", Style::default().fg(colors::error())),
            Span::styled(" Close", Style::default().fg(colors::text_dim())),
        ]));

        lines
    }

    fn can_run_upgrade(&self, state: &UpdateSharedState) -> bool {
        self.command.is_some()
            && state.error.is_none()
            && !state.checking
            && state.latest_version.is_some()
    }
}

impl<'a> BottomPaneView<'a> for UpdateSettingsView {
    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        match key_event.code {
            KeyCode::Esc => self.is_complete = true,
            KeyCode::Tab | KeyCode::Down => {
                self.field = (self.field + 1) % 2;
            }
            KeyCode::BackTab | KeyCode::Up => {
                if self.field == 0 {
                    self.field = 1;
                } else {
                    self.field = 0;
                }
            }
            KeyCode::Left | KeyCode::Right | KeyCode::Char(' ') if self.field == 1 => {
                self.toggle_auto();
            }
            KeyCode::Enter => {
                if self.field == 0 {
                    self.invoke_run_upgrade();
                } else {
                    self.toggle_auto();
                }
            }
            _ => {}
        }
        self.app_event_tx.send(AppEvent::RequestRedraw);
    }

    fn is_complete(&self) -> bool {
        self.is_complete
    }

    fn desired_height(&self, _width: u16) -> u16 {
        self.build_lines().len().saturating_add(2) as u16
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        Clear.render(area, buf);
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(colors::border()))
            .title(" Update ")
            .title_alignment(Alignment::Center);
        let inner = block.inner(area);
        block.render(area, buf);

        let lines = self.build_lines();
        Paragraph::new(lines)
            .alignment(Alignment::Left)
            .style(Style::default().bg(colors::background()).fg(colors::text()))
            .wrap(Wrap { trim: true })
            .render(inner, buf);
    }

    fn handle_paste(&mut self, _text: String) -> ConditionalUpdate {
        ConditionalUpdate::NoRedraw
    }
}
