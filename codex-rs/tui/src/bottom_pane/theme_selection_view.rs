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
    _original_spinner: String,
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
            _original_spinner: current_spinner_name.clone(),
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
            // Allow moving onto the extra pseudo-row (Create your own…)
            if self.selected_spinner_index + 1 <= names.len() {
                self.selected_spinner_index += 1;
                if self.selected_spinner_index < names.len() {
                    if let Some(name) = names.get(self.selected_spinner_index) {
                        self.current_spinner = name.clone();
                        self.app_event_tx
                            .send(AppEvent::PreviewSpinner(self.current_spinner.clone()));
                    }
                } else {
                    // On the pseudo-row: do not change current spinner preview
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
            Mode::CreateSpinner(_) => {}
        }
        self.mode = Mode::Overview;
    }
}

#[derive(Clone, PartialEq)]
enum Mode { Overview, Themes, Spinner, CreateSpinner(CreateState) }

#[derive(Clone, PartialEq)]
struct CreateState {
    step: CreateStep,
    interval: String,
    frames: String,
    action_idx: usize, // 0 = Save, 1 = Cancel
}

#[derive(Clone, PartialEq)]
enum CreateStep { Interval, Frames, Action }

impl<'a> BottomPaneView<'a> for ThemeSelectionView {
    fn desired_height(&self, _width: u16) -> u16 {
        match &self.mode {
            // Border (2) + inner padding (2) + 2 content rows = 6
            Mode::Overview => 6,
            // Detail lists: fixed 9 visible rows (max), shrink if fewer
            Mode::Themes => {
                let n = Self::get_theme_options().len() as u16;
                // Border(2) + padding(2) + title(1)+space(1) + list
                6 + n.min(9)
            }
            Mode::Spinner => {
                // +1 for the "Create your own…" pseudo-row
                let n = (crate::spinner::spinner_names().len() as u16) + 1;
                // Border(2) + padding(2) + title(1)+space(1) + list
                6 + n.min(9)
            }
            // Title + spacer + 2 fields + buttons + help = 6 content rows
            // plus border(2) + padding(2) = 10; add 2 rows headroom for small terminals
            Mode::CreateSpinner(_) => 12,
        }
    }

    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        match key_event {
            KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                // In create form, Up navigates fields/buttons
                if let Mode::CreateSpinner(ref mut s) = self.mode {
                    s.step = match s.step {
                        CreateStep::Interval => CreateStep::Action,
                        CreateStep::Frames => CreateStep::Interval,
                        CreateStep::Action => { if s.action_idx > 0 { s.action_idx -= 1; } CreateStep::Action }
                    };
                } else {
                    match self.mode.clone() {
                        Mode::Overview => {
                            self.overview_selected_index = self.overview_selected_index.saturating_sub(1) % 2;
                        }
                        _ => self.move_selection_up(),
                    }
                }
            }
            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                // In create form, Down navigates fields/buttons
                if let Mode::CreateSpinner(ref mut s) = self.mode {
                    s.step = match s.step {
                        CreateStep::Interval => CreateStep::Frames,
                        CreateStep::Frames => CreateStep::Action,
                        CreateStep::Action => { if s.action_idx < 1 { s.action_idx += 1; } CreateStep::Action }
                    };
                } else {
                    match &self.mode.clone() {
                        Mode::Overview => {
                            self.overview_selected_index = (self.overview_selected_index + 1) % 2;
                        }
                        _ => self.move_selection_down(),
                    }
                }
            }
            KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::NONE, .. } => {}
            KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::NONE, .. } => {}
            KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                // Take ownership of mode to avoid borrowing self while we may assign to self.mode
                let current_mode = std::mem::replace(&mut self.mode, Mode::Overview);
                match current_mode {
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
                    Mode::Themes => {
                        // confirm_theme sets self.mode back to Overview
                        self.confirm_theme()
                    }
                    Mode::Spinner => {
                        // If tail row selected (Create your own…), open create form
                        let names = crate::spinner::spinner_names();
                        if self.selected_spinner_index >= names.len() {
                            self.mode = Mode::CreateSpinner(CreateState { step: CreateStep::Interval, interval: String::from("120"), frames: String::new(), action_idx: 0 });
                        } else {
                            self.confirm_spinner()
                        }
                    }
                    Mode::CreateSpinner(mut s) => {
                        let mut go_overview = false;
                        match s.step {
                            CreateStep::Interval => { s.step = CreateStep::Frames; }
                            CreateStep::Frames => { s.step = CreateStep::Action; }
                            CreateStep::Action => {
                                if s.action_idx == 0 {
                                    let interval = s.interval.trim().parse::<u64>().unwrap_or(120);
                                    let frames: Vec<String> = s.frames.split(',').map(|f| f.trim().to_string()).filter(|f| !f.is_empty()).collect();
                                    if !frames.is_empty() {
                                        if let Ok(home) = codex_core::config::find_codex_home() { let _ = codex_core::config::set_custom_spinner(&home, "custom", interval, &frames); }
                                        crate::spinner::add_custom_spinner("custom".to_string(), "Custom".to_string(), interval, frames);
                                        crate::spinner::switch_spinner("custom");
                                        self.current_spinner = "custom".to_string();
                                        self.revert_spinner_on_back = self.current_spinner.clone();
                                        self.app_event_tx.send(AppEvent::InsertBackgroundEventEarly("✓ Custom spinner saved".to_string()));
                                        go_overview = true;
                                    } else {
                                        self.app_event_tx.send(AppEvent::InsertBackgroundEventEarly("Provide at least one frame (comma-separated)".to_string()));
                                    }
                                } else { /* Cancel → return to overview */ go_overview = true; }
                            }
                        }
                        if go_overview { self.mode = Mode::Overview; } else { self.mode = Mode::CreateSpinner(s); }
                    }
                }
            }
            KeyEvent {
                code: KeyCode::Esc,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                match self.mode {
                    Mode::Overview => self.is_complete = true,
                    Mode::CreateSpinner(_) => { self.mode = Mode::Spinner; }
                    _ => self.cancel_detail(),
                }
            }
            KeyEvent { code: KeyCode::Char(c), modifiers: KeyModifiers::NONE, .. } => {
                if let Mode::CreateSpinner(ref mut s) = self.mode {
                    match s.step { CreateStep::Interval => s.interval.push(c), CreateStep::Frames => s.frames.push(c), CreateStep::Action => {} }
                }
            }
            KeyEvent { code: KeyCode::Backspace, .. } => {
                if let Mode::CreateSpinner(ref mut s) = self.mode {
                    let tgt = match s.step { CreateStep::Interval => &mut s.interval, CreateStep::Frames => &mut s.frames, CreateStep::Action => { return; } };
                    tgt.pop();
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
            // Header: Choose Theme
            lines.push(Line::from(Span::styled(
                "Choose Theme",
                Style::default().fg(theme.text_bright).add_modifier(Modifier::BOLD),
            )));
            // Compute anchored window: top until middle, then center; bottom shows end
            let count = options.len();
            let visible = available_height.saturating_sub(1).min(9).max(1);
            let (start, _vis, _mid) = crate::util::list_window::anchored_window(
                self.selected_theme_index,
                count,
                visible,
            );
            let end = (start + visible).min(count + 1); // +1 for "Create your own…"
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
        } else if matches!(self.mode, Mode::CreateSpinner(_)) {
            // Inline form for custom spinner with visible selection & caret
            let theme = crate::theme::current_theme();
            if let Mode::CreateSpinner(s) = &self.mode {
                let mut form_lines = Vec::new();
                form_lines.push(Line::from(Span::styled(
                    "Overview » Change Spinner » Create Custom",
                    Style::default().fg(theme.text_bright).add_modifier(Modifier::BOLD),
                )));
                form_lines.push(Line::from(" "));

                let caret = Span::styled("▏", Style::default().fg(theme.info));
                // Interval line
                {
                    let active = matches!(s.step, CreateStep::Interval);
                    let mut spans: Vec<Span> = Vec::new();
                    spans.push(Span::styled(if active { "› " } else { "  " }, Style::default().fg(theme.keyword)));
                    spans.push(Span::styled("Interval (ms): ", Style::default().fg(theme.keyword)));
                    if s.interval.is_empty() && active { spans.push(Span::styled("(e.g., 120)", Style::default().fg(theme.text_dim))); spans.push(caret.clone()); }
                    else if active { spans.push(Span::raw(s.interval.clone())); spans.push(caret.clone()); }
                    else { spans.push(Span::raw(s.interval.clone())); }
                    form_lines.push(Line::from(spans));
                }
                // Frames line
                {
                    let active = matches!(s.step, CreateStep::Frames);
                    let mut spans: Vec<Span> = Vec::new();
                    spans.push(Span::styled(if active { "› " } else { "  " }, Style::default().fg(theme.keyword)));
                    spans.push(Span::styled("Frames (comma-separated): ", Style::default().fg(theme.keyword)));
                    if s.frames.is_empty() && active { spans.push(Span::styled("(e.g., ., .., …)", Style::default().fg(theme.text_dim))); spans.push(caret.clone()); }
                    else if active { spans.push(Span::raw(s.frames.clone())); spans.push(caret.clone()); }
                    else { spans.push(Span::raw(s.frames.clone())); }
                    form_lines.push(Line::from(spans));
                }

                // Action buttons (Save / Cancel)
                {
                    let mut spans: Vec<Span> = Vec::new();
                    let active = matches!(s.step, CreateStep::Action);
                    let save_selected = active && s.action_idx == 0;
                    let cancel_selected = active && s.action_idx == 1;
                    let sel = |b: bool| if b { Style::default().fg(theme.primary).add_modifier(Modifier::BOLD) } else { Style::default().fg(theme.text) };
                    spans.push(Span::raw("  "));
                    spans.push(Span::styled("[ Save ]", sel(save_selected)));
                    spans.push(Span::raw("  "));
                    spans.push(Span::styled("[ Cancel ]", sel(cancel_selected)));
                    form_lines.push(Line::from(spans));
                }

                Paragraph::new(form_lines).alignment(Alignment::Left).render(body_area, buf);
            }
            return;
        } else {
            // Spinner: render one centered preview row per spinner, matching the composer title
            use std::time::{SystemTime, UNIX_EPOCH};
            let now_ms = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis();
            let names = crate::spinner::spinner_names();
            // Include an extra pseudo-row for "Create your own…"
            let count = names.len() + 1;
            // Reserve two rows (header + spacer)
            let visible = available_height.saturating_sub(2).min(9).max(1);
            let (start, _vis, _mid) = crate::util::list_window::anchored_window(
                self.selected_spinner_index,
                count,
                visible,
            );
            let end = (start + visible).min(count);

            // Compute fixed column widths globally so rows never jump when scrolling
            let max_frame_len: u16 = crate::spinner::global_max_frame_len() as u16;
            let mut max_label_len: u16 = 0;
            for name in names.iter() {
                let label = crate::spinner::spinner_label_for(name);
                max_label_len = max_label_len.max(label.chars().count() as u16);
            }

            // Render header (left-aligned) and spacer row
            let header_rect = Rect { x: body_area.x, y: body_area.y, width: body_area.width, height: 1 };
            let header = Line::from(Span::styled(
                "Overview » Change Spinner",
                Style::default().fg(theme.text_bright).add_modifier(Modifier::BOLD),
            ));
            Paragraph::new(header).alignment(Alignment::Left).render(header_rect, buf);
            if header_rect.y + 1 < body_area.y + body_area.height {
                let spacer = Rect { x: body_area.x, y: body_area.y + 1, width: body_area.width, height: 1 };
                Paragraph::new(Line::from(" ")).render(spacer, buf);
            }

            for row_idx in 0..(end - start) {
                let i = start + row_idx;
                // rows start two below (header + spacer)
                let y = body_area.y + 2 + row_idx as u16;
                if y >= body_area.y + body_area.height { break; }

                let row_rect = Rect { x: body_area.x, y, width: body_area.width, height: 1 };
                if i >= names.len() {
                    let mut spans = Vec::new();
                    let is_selected = i == self.selected_spinner_index;
                    spans.push(Span::styled(if is_selected { "› " } else { "  " }.to_string(), Style::default().fg(if is_selected { theme.keyword } else { theme.text } )));
                    spans.push(Span::styled("Create your own…", Style::default().fg(theme.primary).add_modifier(Modifier::BOLD)));
                    Paragraph::new(Line::from(spans)).render(row_rect, buf);
                    continue;
                }
                let name = &names[i];
                let is_selected = i == self.selected_spinner_index;
                let def = crate::spinner::find_spinner_by_name(name).unwrap_or(crate::spinner::current_spinner());
                let frame = crate::spinner::frame_at_time(def, now_ms);

                // Aligned columns (centered block):
                // selector (2) | left_rule | space | spinner (right‑aligned to max) | space | label (padded to max) | space | right_rule
                let border = if is_selected { Style::default().fg(crate::colors::border()) } else { Style::default().fg(theme.text_dim).add_modifier(Modifier::DIM) };
                let fg = if is_selected { Style::default().fg(crate::colors::info()) } else { Style::default().fg(theme.text_dim).add_modifier(Modifier::DIM) };
                let label = crate::spinner::spinner_label_for(name);

                // Use border-based alignment per spec
                let spinner_len = frame.chars().count() as u16;
                let text_len = (label.chars().count() as u16).saturating_add(3); // label + "..."
                let x: u16 = max_frame_len.saturating_add(5);
                let left_rule = x.saturating_sub(spinner_len);
                let right_rule = x.saturating_sub(text_len);

                let mut spans: Vec<Span> = Vec::new();
                // selector
                spans.push(Span::styled(if is_selected { "› " } else { "  " }.to_string(), Style::default().fg(if is_selected { theme.keyword } else { theme.text } )));
                // left rule
                spans.push(Span::styled("─".repeat(left_rule as usize), border));
                // single space between left border and spinner
                spans.push(Span::raw(" "));
                // spinner
                spans.push(Span::styled(frame, fg));
                spans.push(Span::raw(" "));
                // label with dots
                spans.push(Span::styled(format!("{}...", label), fg));
                // right rule (match left border logic: x - text_len)
                spans.push(Span::styled("─".repeat(right_rule as usize), border));
                Paragraph::new(Line::from(spans)).alignment(Alignment::Left).render(row_rect, buf);
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
