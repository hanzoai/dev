use codex_core::config_types::ThemeName;
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
use ratatui::widgets::Paragraph;
// Table-based approach was replaced by manual two-column layout; keep imports minimal
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
    // UI mode/state
    mode: Mode,
    overview_selected_index: usize, // 0 = Theme, 1 = Spinner
    // Revert points when backing out of detail views
    revert_theme_on_back: ThemeName,
    revert_spinner_on_back: String,
    // One-shot flags to show selection at top on first render of detail views
    just_entered_themes: bool,
    just_entered_spinner: bool,
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
        let current_spinner_name = crate::spinner::current_spinner().name.clone();
        let selected_spinner_index = spinner_names
            .iter()
            .position(|n| *n == current_spinner_name)
            .unwrap_or(0);

        Self {
            original_theme: current_theme,
            current_theme,
            selected_theme_index,
            original_spinner: current_spinner_name.clone(),
            current_spinner: current_spinner_name.clone(),
            selected_spinner_index,
            mode: Mode::Overview,
            overview_selected_index: 0,
            revert_theme_on_back: current_theme,
            revert_spinner_on_back: current_spinner_name,
            just_entered_themes: false,
            just_entered_spinner: false,
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
        if matches!(self.mode, Mode::Themes) {
            let options = Self::get_theme_options();
                if self.selected_theme_index > 0 {
                    self.selected_theme_index -= 1;
                    self.current_theme = options[self.selected_theme_index].0;
                    self.app_event_tx.send(AppEvent::PreviewTheme(self.current_theme));
                }
        } else {
            let names = crate::spinner::spinner_names();
            if self.selected_spinner_index > 0 {
                self.selected_spinner_index -= 1;
                if let Some(name) = names.get(self.selected_spinner_index) {
                    self.current_spinner = name.clone();
                    self.app_event_tx
                        .send(AppEvent::PreviewSpinner(self.current_spinner.clone()));
                }
            }
        }
    }

    fn move_selection_down(&mut self) {
        if matches!(self.mode, Mode::Themes) {
            let options = Self::get_theme_options();
            if self.selected_theme_index + 1 < options.len() {
                self.selected_theme_index += 1;
                self.current_theme = options[self.selected_theme_index].0;
                self.app_event_tx
                    .send(AppEvent::PreviewTheme(self.current_theme));
            }
        } else {
            let names = crate::spinner::spinner_names();
            if self.selected_spinner_index + 1 < names.len() {
                self.selected_spinner_index += 1;
                if let Some(name) = names.get(self.selected_spinner_index) {
                    self.current_spinner = name.clone();
                    self.app_event_tx
                        .send(AppEvent::PreviewSpinner(self.current_spinner.clone()));
                }
            }
        }
    }

    fn confirm_theme(&mut self) {
        self.app_event_tx.send(AppEvent::UpdateTheme(self.current_theme));
        self.revert_theme_on_back = self.current_theme;
        self.mode = Mode::Overview;
    }

    fn confirm_spinner(&mut self) {
        self.app_event_tx
            .send(AppEvent::UpdateSpinner(self.current_spinner.clone()));
        self.revert_spinner_on_back = self.current_spinner.clone();
        self.mode = Mode::Overview;
    }

    fn cancel_detail(&mut self) {
        match self.mode {
            Mode::Themes => {
                if self.current_theme != self.revert_theme_on_back {
                    self.current_theme = self.revert_theme_on_back;
                    self.app_event_tx
                        .send(AppEvent::PreviewTheme(self.current_theme));
                }
            }
            Mode::Spinner => {
                if self.current_spinner != self.revert_spinner_on_back {
                    self.current_spinner = self.revert_spinner_on_back.clone();
                    self.app_event_tx
                        .send(AppEvent::PreviewSpinner(self.current_spinner.clone()));
                }
            }
            Mode::Overview => {}
        }
        self.mode = Mode::Overview;
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Mode { Overview, Themes, Spinner }

impl<'a> BottomPaneView<'a> for ThemeSelectionView {
    fn desired_height(&self, _width: u16) -> u16 {
        match self.mode {
            // Border (2) + inner padding (2) + 2 content rows = 6
            Mode::Overview => 6,
            // Detail lists: fixed 9 visible rows (max), shrink if fewer
            Mode::Themes => {
                let n = Self::get_theme_options().len() as u16;
                4 + n.min(9)
            }
            Mode::Spinner => {
                let n = crate::spinner::spinner_names().len() as u16;
                4 + n.min(9)
            }
        }
    }

    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        match key_event {
            KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                match self.mode {
                    Mode::Overview => {
                        self.overview_selected_index = self.overview_selected_index.saturating_sub(1) % 2;
                    }
                    _ => self.move_selection_up(),
                }
            }
            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                match self.mode {
                    Mode::Overview => {
                        self.overview_selected_index = (self.overview_selected_index + 1) % 2;
                    }
                    _ => self.move_selection_down(),
                }
            }
            KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::NONE, .. } => {}
            KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::NONE, .. } => {}
            KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                match self.mode {
                    Mode::Overview => {
                        if self.overview_selected_index == 0 {
                            self.revert_theme_on_back = self.current_theme;
                            self.mode = Mode::Themes;
                            self.just_entered_themes = true;
                        } else {
                            self.revert_spinner_on_back = self.current_spinner.clone();
                            self.mode = Mode::Spinner;
                            self.app_event_tx
                                .send(AppEvent::ScheduleFrameIn(std::time::Duration::from_millis(120)));
                            self.just_entered_spinner = true;
                        }
                    }
                    Mode::Themes => self.confirm_theme(),
                    Mode::Spinner => self.confirm_spinner(),
                }
            }
            KeyEvent {
                code: KeyCode::Esc,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                match self.mode {
                    Mode::Overview => self.is_complete = true,
                    _ => self.cancel_detail(),
                }
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

        // Use full width and draw an outer window styled like the Diff overlay
        let render_area = Rect { x: area.x, y: area.y, width: area.width, height: area.height };
        Clear.render(render_area, buf);

        // Add one row of padding above the top border (clear + background)
        if render_area.y > 0 {
            let pad = Rect { x: render_area.x, y: render_area.y - 1, width: render_area.width, height: 1 };
            Clear.render(pad, buf);
            let pad_bg = Block::default().style(Style::default().bg(crate::colors::background()));
            pad_bg.render(pad, buf);
        }

        // Build a styled title with concise hints
        let t_dim = Style::default().fg(crate::colors::text_dim());
        let t_fg = Style::default().fg(crate::colors::text());
        let mut title_spans = vec![Span::styled(" ", t_dim), Span::styled("/theme", t_fg)];
        title_spans.extend_from_slice(&[
            Span::styled(" ——— ", t_dim),
            Span::styled("▲ ▼", t_fg),
            Span::styled(" select ", t_dim),
            Span::styled("——— ", t_dim),
            Span::styled("Enter", t_fg),
            Span::styled(" choose ", t_dim),
            Span::styled("——— ", t_dim),
            Span::styled("Esc", t_fg),
        ]);
        if matches!(self.mode, Mode::Overview) {
            title_spans.push(Span::styled(" close ", t_dim));
        } else {
            title_spans.push(Span::styled(" back ", t_dim));
        }

        let outer = Block::default()
            .borders(Borders::ALL)
            .title(Line::from(title_spans))
            .style(Style::default().bg(crate::colors::background()))
            .border_style(Style::default().fg(crate::colors::border()).bg(crate::colors::background()));
        let inner = outer.inner(render_area);
        outer.render(render_area, buf);

        // Paint inner content background as the normal theme background
        let inner_bg_style = Style::default().bg(crate::colors::background());
        for y in inner.y..inner.y + inner.height {
            for x in inner.x..inner.x + inner.width {
                buf[(x, y)].set_style(inner_bg_style);
            }
        }

        // Add one cell padding around the inside; body occupies full padded area
        let padded = inner.inner(ratatui::layout::Margin::new(1, 1));
        let body_area = padded;

        // Visible rows = available body height (already sized to ≤10)
        let available_height = body_area.height as usize;

        // Create body content
        let mut lines = Vec::new();
        if matches!(self.mode, Mode::Overview) {
            // Overview: two clear actions, also show current values
            let theme_label = Self::get_theme_options()
                .iter()
                .find(|(t, _, _)| *t == self.current_theme)
                .map(|(_, name, _)| *name)
                .unwrap_or("Theme");
            let spinner_label = self.current_spinner.as_str();
            let items = vec![("Change theme", theme_label), ("Change spinner", spinner_label)];
            for (i, (k, v)) in items.iter().enumerate() {
                let selected = i == self.overview_selected_index;
                let mut spans = vec![Span::raw(" ")];
                if selected {
                    spans.push(Span::styled("› ", Style::default().fg(theme.keyword)));
                } else {
                    spans.push(Span::raw("  "));
                }
                if selected {
                    spans.push(Span::styled(*k, Style::default().fg(theme.primary).add_modifier(Modifier::BOLD)));
                } else {
                    spans.push(Span::styled(*k, Style::default().fg(theme.text)));
                }
                spans.push(Span::raw(" — "));
                spans.push(Span::styled(*v, Style::default().fg(theme.text_dim)));
                lines.push(Line::from(spans));
            }
        } else if matches!(self.mode, Mode::Themes) {
            // Compute anchored window: top until middle, then center; bottom shows end
            let count = options.len();
            let visible = available_height.min(9).max(1);
            let (start, _vis, _mid) = crate::util::list_window::anchored_window(
                self.selected_theme_index,
                count,
                visible,
            );
            let end = (start + visible).min(count);
            for i in start..end {
                let (theme_enum, name, description) = &options[i];
                let is_selected = i == self.selected_theme_index;
                let is_original = *theme_enum == self.original_theme;

                let prefix_selected = is_selected;
                let suffix = if is_original { " (original)" } else { "" };

                let mut spans = vec![Span::raw(" ")];
                if prefix_selected {
                    spans.push(Span::styled("› ", Style::default().fg(theme.keyword)));
                } else {
                    spans.push(Span::raw("  "));
                }

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
            // Spinner: render a two-column view [Name | Preview] with the
            // preview styled like the composer title (centered line with spinner and text).
            use std::time::{SystemTime, UNIX_EPOCH};
            let now_ms = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis();
            let names = crate::spinner::spinner_names();
            let count = names.len();
            let visible = available_height.min(9).max(1);
            let (start, _vis, _mid) = crate::util::list_window::anchored_window(
                self.selected_spinner_index,
                count,
                visible,
            );
            let end = (start + visible).min(count);

            // Compute column rects once
            let left_w = (body_area.width as f32 * 0.35) as u16;
            let right_w = body_area.width.saturating_sub(left_w);
            for (row_idx, i) in (start..end).enumerate() {
                let y = body_area.y + row_idx as u16;
                if y >= body_area.y + body_area.height { break; }

                // Left column rect
                let left_rect = Rect { x: body_area.x, y, width: left_w, height: 1 };
                // Right column rect
                let right_rect = Rect { x: body_area.x + left_w, y, width: right_w, height: 1 };

                let name = names[i].clone();
                let is_selected = i == self.selected_spinner_index;
                let is_original = name == self.original_spinner;
                let def = crate::spinner::find_spinner_by_name(&name).unwrap_or(crate::spinner::current_spinner());
                let frame = crate::spinner::frame_at_time(def, now_ms);

                // Render left cell (selector + name + optional tag)
                let mut left_spans = vec![Span::raw(" ")];
                if is_selected { left_spans.push(Span::styled("› ", Style::default().fg(theme.keyword))); } else { left_spans.push(Span::raw("  ")); }
                if is_selected {
                    left_spans.push(Span::styled(name.clone(), Style::default().fg(theme.primary).add_modifier(Modifier::BOLD)));
                } else {
                    left_spans.push(Span::styled(name.clone(), Style::default().fg(theme.text)));
                }
                if is_original { left_spans.push(Span::styled(" (original)", Style::default().fg(theme.text_dim))); }
                Paragraph::new(Line::from(left_spans)).alignment(Alignment::Left).render(left_rect, buf);

                // Render right cell: centered preview with border-colored rules
                let info = Style::default().fg(crate::colors::info());
                let border = Style::default().fg(crate::colors::border());
                let content = format!("{} Thinking...", frame);
                let content_len = content.chars().count() as u16 + 2; // spaces around
                let total = right_rect.width;
                let rule_total = total.saturating_sub(content_len);
                let left_rule = rule_total / 2;
                let right_rule = rule_total.saturating_sub(left_rule);
                let mut right_spans: Vec<Span> = Vec::new();
                right_spans.push(Span::styled("─".repeat(left_rule as usize), border));
                right_spans.push(Span::raw(" "));
                right_spans.push(Span::styled(content, info));
                right_spans.push(Span::raw(" "));
                right_spans.push(Span::styled("─".repeat(right_rule as usize), border));
                Paragraph::new(Line::from(right_spans)).alignment(Alignment::Left).render(right_rect, buf);
            }

            // Animate spinner previews while open
            self.app_event_tx
                .send(AppEvent::ScheduleFrameIn(std::time::Duration::from_millis(100)));

            // Done rendering spinners
            return;
        }

        // No explicit scroll info; list height is fixed to show boundaries naturally

        // Render the body content paragraph inside body area
        let paragraph = Paragraph::new(lines).alignment(Alignment::Left);
        paragraph.render(body_area, buf);
    }
}
