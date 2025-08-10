use codex_core::config_types::ThemeName;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph, Widget};

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;

use super::bottom_pane_view::BottomPaneView;
use super::BottomPane;

/// Interactive UI for selecting theme
pub(crate) struct ThemeSelectionView {
    current_theme: ThemeName,
    selected_theme: ThemeName,
    selected_index: usize,
    app_event_tx: AppEventSender,
    is_complete: bool,
}

impl ThemeSelectionView {
    pub fn new(current_theme: ThemeName, app_event_tx: AppEventSender) -> Self {
        let themes = Self::get_theme_options();
        let selected_index = themes
            .iter()
            .position(|(t, _, _)| *t == current_theme)
            .unwrap_or(0);
        
        Self {
            current_theme,
            selected_theme: current_theme,
            selected_index,
            app_event_tx,
            is_complete: false,
        }
    }

    fn get_theme_options() -> Vec<(ThemeName, &'static str, &'static str)> {
        vec![
            (ThemeName::CarbonNight, "Carbon Night", "Sleek modern dark theme"),
            (ThemeName::PhotonLight, "Photon Light", "Clean professional light theme"),
            (ThemeName::ShinobiDusk, "Shinobi Dusk", "Japanese-inspired twilight"),
            (ThemeName::OledBlackPro, "OLED Black Pro", "True black for OLED displays"),
            (ThemeName::AmberTerminal, "Amber Terminal", "Retro amber CRT aesthetic"),
            (ThemeName::AuroraFlux, "Aurora Flux", "Northern lights inspired"),
            (ThemeName::CharcoalRainbow, "Charcoal Rainbow", "High-contrast accessible"),
            (ThemeName::ZenGarden, "Zen Garden", "Calm and peaceful"),
            (ThemeName::PaperLightPro, "Paper Light Pro", "Premium paper-like light"),
        ]
    }

    fn move_selection_up(&mut self) {
        let options = Self::get_theme_options();
        if self.selected_index == 0 {
            self.selected_index = options.len() - 1;
        } else {
            self.selected_index -= 1;
        }
        self.selected_theme = options[self.selected_index].0;
    }

    fn move_selection_down(&mut self) {
        let options = Self::get_theme_options();
        self.selected_index = (self.selected_index + 1) % options.len();
        self.selected_theme = options[self.selected_index].0;
    }

    fn confirm_selection(&self) {
        // Send event to update theme
        self.app_event_tx.send(AppEvent::UpdateTheme(self.selected_theme));
    }
}

impl<'a> BottomPaneView<'a> for ThemeSelectionView {
    fn desired_height(&self, _width: u16) -> u16 {
        // Return height needed for the popup
        15
    }
    
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
                self.is_complete = true;
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

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let options = Self::get_theme_options();
        
        // Create content
        let mut lines = vec![
            Line::from(vec![
                Span::raw("Select Theme"),
            ]).style(Style::default().add_modifier(Modifier::BOLD)),
            Line::from(""),
        ];

        for (i, (theme, name, description)) in options.iter().enumerate() {
            let is_selected = i == self.selected_index;
            let is_current = *theme == self.current_theme;
            
            let prefix = if is_selected { "> " } else { "  " };
            let suffix = if is_current { " (current)" } else { "" };
            
            let spans = vec![
                Span::raw(prefix),
                Span::raw(*name),
                Span::raw(suffix),
                Span::raw(" - "),
                Span::styled(
                    *description,
                    Style::default().fg(crate::colors::dim()),
                ),
            ];
            
            let line = Line::from(spans);
            let styled_line = if is_selected {
                line.style(
                    Style::default()
                        .fg(crate::colors::light_blue())
                        .add_modifier(Modifier::BOLD),
                )
            } else {
                line
            };
            
            lines.push(styled_line);
        }
        
        lines.push(Line::from(""));
        lines.push(Line::from(vec![
            Span::raw("Use "),
            Span::styled("↑↓", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(" to navigate, "),
            Span::styled("Enter", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(" to select, "),
            Span::styled("Esc", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(" to cancel"),
        ]).style(Style::default().fg(Color::DarkGray)));

        // Create the popup
        let popup_width = 60;
        let popup_height = (lines.len() + 2) as u16;
        
        let popup_x = area.width.saturating_sub(popup_width) / 2;
        let popup_y = area.height.saturating_sub(popup_height) / 2;
        
        let popup_area = Rect {
            x: area.x + popup_x,
            y: area.y + popup_y,
            width: popup_width.min(area.width),
            height: popup_height.min(area.height),
        };

        // Clear the area
        Clear.render(popup_area, buf);

        // Render the popup
        let block = Block::default()
            .borders(Borders::ALL)
            .style(Style::default().fg(crate::colors::text()));

        let paragraph = Paragraph::new(lines)
            .block(block)
            .alignment(Alignment::Left);

        paragraph.render(popup_area, buf);
    }
}