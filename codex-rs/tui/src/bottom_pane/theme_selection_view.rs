use codex_core::config_types::ThemeName;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyModifiers;
use ratatui::buffer::Buffer;
use ratatui::layout::Alignment;
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::text::Line;
use ratatui::text::Span;
use ratatui::widgets::Block;
use ratatui::widgets::Borders;
use ratatui::widgets::Paragraph;
use ratatui::widgets::Tabs;
use ratatui::widgets::Clear;
use ratatui::widgets::Widget;

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;

use super::BottomPane;
use super::bottom_pane_view::BottomPaneView;

/// Interactive UI for selecting appearance (Theme & Spinner)
pub(crate) struct ThemeSelectionView {
    original_theme: ThemeName, // Theme to restore on cancel
    current_theme: ThemeName,  // Currently displayed theme
    selected_theme_index: usize,
    // Spinner tab state
    original_spinner: String,
    current_spinner: String,
    selected_spinner_index: usize,
    // Tab selection
    active_tab: Tab,
    app_event_tx: AppEventSender,
    is_complete: bool,
}

impl ThemeSelectionView {
    pub fn new(current_theme: ThemeName, app_event_tx: AppEventSender) -> Self {
        let themes = Self::get_theme_options();
        let selected_theme_index = themes
            .iter()
            .position(|(t, _, _)| *t == current_theme)
            .unwrap_or(0);

        // Initialize spinner selection from current runtime spinner
        let spinner_names = crate::spinner::spinner_names();
        let current_spinner_name = crate::spinner::current_spinner().name.to_string();
        let selected_spinner_index = spinner_names
            .iter()
            .position(|n| *n == current_spinner_name)
            .unwrap_or(0);

        Self {
            original_theme: current_theme,
            current_theme,
            selected_theme_index,
            original_spinner: current_spinner_name.clone(),
            current_spinner: current_spinner_name,
            selected_spinner_index,
            active_tab: Tab::Themes,
            app_event_tx,
            is_complete: false,
        }
    }

    fn get_theme_options() -> Vec<(ThemeName, &'static str, &'static str)> {
        vec![
            // Light themes (at top)
            (
                ThemeName::LightPhoton,
                "Light - Photon",
                "Clean professional light theme",
            ),
            (
                ThemeName::LightPrismRainbow,
                "Light - Prism Rainbow",
                "Vibrant rainbow accents",
            ),
            (
                ThemeName::LightVividTriad,
                "Light - Vivid Triad",
                "Cyan, pink, amber triad",
            ),
            (
                ThemeName::LightPorcelain,
                "Light - Porcelain",
                "Refined porcelain tones",
            ),
            (
                ThemeName::LightSandbar,
                "Light - Sandbar",
                "Warm sandy beach colors",
            ),
            (
                ThemeName::LightGlacier,
                "Light - Glacier",
                "Cool glacier blues",
            ),
            (
                ThemeName::DarkPaperLightPro,
                "Light - Paper Pro",
                "Premium paper-like",
            ),
            // Dark themes (below)
            (
                ThemeName::DarkCarbonNight,
                "Dark - Carbon Night",
                "Sleek modern dark theme",
            ),
            (
                ThemeName::DarkShinobiDusk,
                "Dark - Shinobi Dusk",
                "Japanese-inspired twilight",
            ),
            (
                ThemeName::DarkOledBlackPro,
                "Dark - OLED Black Pro",
                "True black for OLED displays",
            ),
            (
                ThemeName::DarkAmberTerminal,
                "Dark - Amber Terminal",
                "Retro amber CRT aesthetic",
            ),
            (
                ThemeName::DarkAuroraFlux,
                "Dark - Aurora Flux",
                "Northern lights inspired",
            ),
            (
                ThemeName::DarkCharcoalRainbow,
                "Dark - Charcoal Rainbow",
                "High-contrast accessible",
            ),
            (
                ThemeName::DarkZenGarden,
                "Dark - Zen Garden",
                "Calm and peaceful",
            ),
        ]
    }

    fn move_selection_up(&mut self) {
        let options = Self::get_theme_options();
        if matches!(self.active_tab, Tab::Themes) {
            if self.selected_theme_index == 0 {
                self.selected_theme_index = options.len() - 1;
            } else {
                self.selected_theme_index -= 1;
            }
            self.current_theme = options[self.selected_theme_index].0;
            // Live preview - update theme immediately (no history event)
            self.app_event_tx
                .send(AppEvent::PreviewTheme(self.current_theme));
        } else {
            let names = crate::spinner::spinner_names();
            if self.selected_spinner_index == 0 {
                self.selected_spinner_index = names.len().saturating_sub(1);
            } else {
                self.selected_spinner_index -= 1;
            }
            if let Some(name) = names.get(self.selected_spinner_index) {
                self.current_spinner = (*name).to_string();
                self.app_event_tx
                    .send(AppEvent::PreviewSpinner(self.current_spinner.clone()));
            }
        }
    }

    fn move_selection_down(&mut self) {
        if matches!(self.active_tab, Tab::Themes) {
            let options = Self::get_theme_options();
            self.selected_theme_index = (self.selected_theme_index + 1) % options.len();
            self.current_theme = options[self.selected_theme_index].0;
            // Live preview - update theme immediately (no history event)
            self.app_event_tx
                .send(AppEvent::PreviewTheme(self.current_theme));
        } else {
            let names = crate::spinner::spinner_names();
            if !names.is_empty() {
                self.selected_spinner_index = (self.selected_spinner_index + 1) % names.len();
                if let Some(name) = names.get(self.selected_spinner_index) {
                    self.current_spinner = (*name).to_string();
                    self.app_event_tx
                        .send(AppEvent::PreviewSpinner(self.current_spinner.clone()));
                }
            }
        }
    }

    fn confirm_selection(&self) {
        // Confirm the selection - this will add it to history
        self.app_event_tx
            .send(AppEvent::UpdateTheme(self.current_theme));
        self.app_event_tx
            .send(AppEvent::UpdateSpinner(self.current_spinner.clone()));
    }

    fn cancel_selection(&mut self) {
        // Restore original selections on cancel (no history event)
        if self.current_theme != self.original_theme {
            self.app_event_tx
                .send(AppEvent::PreviewTheme(self.original_theme));
        }
        if self.current_spinner != self.original_spinner {
            self.app_event_tx
                .send(AppEvent::PreviewSpinner(self.original_spinner.clone()));
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Tab { Themes, Spinner }

impl<'a> BottomPaneView<'a> for ThemeSelectionView {
    fn desired_height(&self, _width: u16) -> u16 {
        // Use most of the available screen for better scrolling
        // But cap it at the number of themes + header/footer
        let theme_count = Self::get_theme_options().len() as u16;
        // Leave room for header/tabs/footer
        (theme_count.max(crate::spinner::spinner_names().len() as u16) + 6).min(22)
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
            KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::NONE, .. } => {
                self.active_tab = Tab::Themes;
                // Schedule a near-future redraw so spinner previews continue animating
                self.app_event_tx
                    .send(AppEvent::ScheduleFrameIn(std::time::Duration::from_millis(120)));
            }
            KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::NONE, .. } => {
                self.active_tab = Tab::Spinner;
                self.app_event_tx
                    .send(AppEvent::ScheduleFrameIn(std::time::Duration::from_millis(120)));
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
                self.cancel_selection();
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
        let theme = crate::theme::current_theme();

        // Use full width for better integration and draw an outer border
        let render_area = Rect { x: area.x, y: area.y, width: area.width, height: area.height };

        // Clear then fill outer area similar to Diff Viewer (selection background)
        Clear.render(render_area, buf);

        let outer = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()))
            .style(Style::default().bg(crate::colors::selection()).fg(theme.text));
        outer.clone().render(render_area, buf);
        let inner = outer.inner(render_area);

        // Split inner into a 3-row tabs header and the body (like Diff Viewer)
        let [tabs_area_all, body_area] = Layout::vertical([Constraint::Length(3), Constraint::Fill(1)]).areas(inner);

        // Fill inner content with selection so inactive tabs blend in (Diff Viewer parity)
        let inner_bg = Block::default().style(Style::default().bg(crate::colors::selection()));
        inner_bg.render(tabs_area_all, buf);

        // Center the tabs within the 3-row header
        let [_, tabs_area, _] = Layout::vertical([Constraint::Length(1), Constraint::Length(1), Constraint::Length(1)])
            .areas(tabs_area_all);

        // Build Tabs titles and styles to match Diff Viewer popup
        let titles = vec![
            Line::from(Span::raw(" Themes ")),
            Line::from(Span::raw(" Spinner ")),
        ];
        let selected_idx = if matches!(self.active_tab, Tab::Themes) { 0 } else { 1 };
        let tabs = Tabs::new(titles)
            .select(selected_idx)
            .style(Style::default().bg(crate::colors::selection()).fg(crate::colors::text()))
            .highlight_style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .divider(" ");
        Widget::render(tabs, tabs_area, buf);

        // Body background uses normal background (like Diff Viewer)
        let body_bg = Block::default().style(Style::default().bg(crate::colors::background()));
        body_bg.render(body_area, buf);

        // Calculate available list height inside body, accounting for header lines below
        let base_header_lines = 2u16; // "Appearance" + spacer line
        let available_height = body_area
            .height
            .saturating_sub(base_header_lines)
            as usize;

        // Calculate scroll offset to keep selected item visible
        let scroll_offset = if available_height >= options.len() {
            0
        } else if self.selected_theme_index < available_height / 2 {
            0
        } else if self.selected_theme_index >= options.len() - available_height / 2 {
            options.len().saturating_sub(available_height)
        } else {
            self.selected_theme_index.saturating_sub(available_height / 2)
        };

        // Create body content
        let mut lines = vec![
            Line::from(vec![
                Span::raw(" "),
                Span::styled(
                    "Appearance",
                    Style::default()
                        .fg(theme.text_bright)
                        .add_modifier(Modifier::BOLD),
                ),
            ]),
            Line::from(" "),
        ];

        if matches!(self.active_tab, Tab::Themes) {
            // Add visible themes based on scroll offset
            let visible_end = (scroll_offset + available_height).min(options.len());
            for i in scroll_offset..visible_end {
                let (theme_enum, name, description) = &options[i];
                let is_selected = i == self.selected_theme_index;
                let is_original = *theme_enum == self.original_theme;

                let prefix = if is_selected { "› " } else { "  " };
                let suffix = if is_original { " (original)" } else { "" };

                let mut spans = vec![Span::raw(" "), Span::raw(prefix)];

                if is_selected {
                    spans.push(Span::styled(
                        *name,
                        Style::default()
                            .fg(theme.primary)
                            .add_modifier(Modifier::BOLD),
                    ));
                } else {
                    spans.push(Span::styled(*name, Style::default().fg(theme.text)));
                }

                spans.push(Span::styled(suffix, Style::default().fg(theme.text_dim)));

                if !suffix.is_empty() {
                    spans.push(Span::raw(" "));
                } else {
                    spans.push(Span::raw("  "));
                }

                spans.push(Span::styled(
                    *description,
                    Style::default().fg(theme.text_dim),
                ));

                lines.push(Line::from(spans));
            }
        } else {
            // Spinner tab: list spinner names with a live frame preview
            use std::time::{SystemTime, UNIX_EPOCH};
            let now_ms = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis();
            let names = crate::spinner::spinner_names();
            let visible_end = (scroll_offset + available_height).min(names.len());
            for i in scroll_offset..visible_end {
                let name = names[i];
                let is_selected = i == self.selected_spinner_index;
                let is_original = name == self.original_spinner;
                let def = crate::spinner::find_spinner_by_name(name).unwrap_or(crate::spinner::current_spinner());
                let frame = crate::spinner::frame_at_time(def, now_ms);

                let prefix = if is_selected { "› " } else { "  " };
                let suffix = if is_original { " (original)" } else { "" };

                let mut spans = vec![Span::raw(" "), Span::raw(prefix)];

                // Show preview frame and name
                let preview = format!("{} ", frame);
                spans.push(Span::styled(preview, Style::default().fg(theme.info)));

                if is_selected {
                    spans.push(Span::styled(
                        name,
                        Style::default().fg(theme.primary).add_modifier(Modifier::BOLD),
                    ));
                } else {
                    spans.push(Span::styled(name, Style::default().fg(theme.text)));
                }

                spans.push(Span::styled(suffix, Style::default().fg(theme.text_dim)));
                lines.push(Line::from(spans));
            }
            // Keep preview animating while the tab is active
            self.app_event_tx
                .send(AppEvent::ScheduleFrameIn(std::time::Duration::from_millis(100)));
        }

        // Add scroll indicators if needed
        if matches!(self.active_tab, Tab::Themes) && (scroll_offset > 0 || (scroll_offset + available_height).min(options.len()) < options.len()) {
            lines.push(Line::from(" "));
            let scroll_info = format!("[{}/{}]", self.selected_theme_index + 1, options.len());
            lines.push(Line::from(vec![Span::raw(" "), Span::styled(
                scroll_info,
                Style::default().fg(theme.text_dim),
            )]));
        } else {
            lines.push(Line::from(" "));
        }

        lines.push(Line::from(vec![
            Span::styled("←→", Style::default().fg(theme.keyword).add_modifier(Modifier::BOLD)),
            Span::styled(" tabs • ", Style::default().fg(theme.text_dim)),
            Span::styled("↑↓", Style::default().fg(theme.keyword).add_modifier(Modifier::BOLD)),
            Span::styled(" select • ", Style::default().fg(theme.text_dim)),
            Span::styled(
                "Enter",
                Style::default()
                    .fg(theme.keyword)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::styled(" confirm • ", Style::default().fg(theme.text_dim)),
            Span::styled(
                "Esc",
                Style::default()
                    .fg(theme.keyword)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::styled(" cancel", Style::default().fg(theme.text_dim)),
        ]));

        // Render the body content paragraph inside body area
        let paragraph = Paragraph::new(lines).alignment(Alignment::Left);
        paragraph.render(body_area, buf);
    }
}
