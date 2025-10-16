use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::auto_drive_strings;
use crate::auto_drive_style::{AutoDriveStyle, AutoDriveVariant, FrameStyle};
use crate::glitch_animation::{gradient_multi, mix_rgb};
use crate::colors;
use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Constraint, Direction, Layout, Rect};
use ratatui::prelude::Widget;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Paragraph, WidgetRef, Wrap};
use std::borrow::Cow;
use unicode_width::UnicodeWidthStr;
use std::time::{Duration, Instant};

use super::{
    bottom_pane_view::{BottomPaneView, ConditionalUpdate},
    chat_composer::ChatComposer,
    BottomPane,
};

#[derive(Clone, Debug)]
pub(crate) struct CountdownState {
    pub remaining: u8,
}

#[derive(Clone, Debug)]
pub(crate) struct AutoCoordinatorButton {
    pub label: String,
    pub enabled: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct AutoActiveViewModel {
    #[allow(dead_code)]
    pub goal: Option<String>,
    pub status_lines: Vec<String>,
    pub cli_prompt: Option<String>,
    pub cli_context: Option<String>,
    pub show_composer: bool,
    pub awaiting_submission: bool,
    pub waiting_for_response: bool,
    pub waiting_for_review: bool,
    pub countdown: Option<CountdownState>,
    pub button: Option<AutoCoordinatorButton>,
    pub manual_hint: Option<String>,
    pub ctrl_switch_hint: String,
    pub cli_running: bool,
    pub review_enabled: bool,
    pub agents_enabled: bool,
    pub turns_completed: usize,
    pub started_at: Option<Instant>,
    pub elapsed: Option<Duration>,
    pub progress_past: Option<String>,
    pub progress_current: Option<String>,
    pub intro_started_at: Option<Instant>,
    pub intro_reduced_motion: bool,
}

#[derive(Clone, Debug)]
pub(crate) enum AutoCoordinatorViewModel {
    Active(AutoActiveViewModel),
}

struct ButtonContext {
    label: String,
    enabled: bool,
}

struct VariantContext {
    button: Option<ButtonContext>,
    ctrl_hint: String,
    manual_hint: Option<String>,
}

struct IntroState<'a> {
    header_text: Cow<'a, str>,
    body_visible: bool,
    schedule_next_in: Option<Duration>,
}

pub(crate) struct AutoCoordinatorView {
    model: AutoCoordinatorViewModel,
    app_event_tx: AppEventSender,
    status_message: Option<String>,
    style: AutoDriveStyle,
}

impl AutoCoordinatorView {
    const MIN_COMPOSER_VIEWPORT: u16 = 3;
    const HEADER_HEIGHT: u16 = 1;

    pub fn new(
        model: AutoCoordinatorViewModel,
        app_event_tx: AppEventSender,
        style: AutoDriveStyle,
    ) -> Self {
        Self {
            model,
            app_event_tx,
            status_message: None,
            style,
        }
    }

    pub fn update_model(&mut self, model: AutoCoordinatorViewModel) {
        self.model = model;
    }

    pub fn set_style(&mut self, style: AutoDriveStyle) {
        self.style = style;
    }

    #[allow(dead_code)]
    pub(crate) fn desired_height_with_composer(&self, width: u16, composer: &ChatComposer) -> u16 {
        let AutoCoordinatorViewModel::Active(model) = &self.model;
        let ctx = Self::build_context(model);
        // The framed Auto Drive view introduces an extra border (2 cols) plus a
        // dedicated left padding column before the embedded composer. When the
        // composer renders, it subtracts an additional 4 columns (border + inner
        // padding) from the area we hand it. To keep the measured height in sync
        // with the final render width, subtract those 3 exterior columns before
        // delegating to `ChatComposer::desired_height`.
        let composer_width = width.saturating_sub(3);
        let composer_height = if model.show_composer {
            composer.desired_height(composer_width)
        } else {
            0
        };
        self.estimated_height_active(width, &ctx, model, composer_height)
    }

    fn intro_state<'a>(header_text: &'a str, model: &AutoActiveViewModel) -> IntroState<'a> {
        const LETTER_INTERVAL_MS: u64 = 32;
        const BODY_DELAY_MS: u64 = 90;
        const MIN_FRAME_MS: u64 = 16;

        if header_text.is_empty() || model.intro_reduced_motion {
            return IntroState {
                header_text: Cow::Borrowed(header_text),
                body_visible: true,
                schedule_next_in: None,
            };
        }

        let Some(started) = model.intro_started_at else {
            return IntroState {
                header_text: Cow::Borrowed(header_text),
                body_visible: true,
                schedule_next_in: None,
            };
        };

        let total_chars = header_text.chars().count();
        if total_chars == 0 {
            return IntroState {
                header_text: Cow::Borrowed(header_text),
                body_visible: true,
                schedule_next_in: None,
            };
        }

        let now = Instant::now();
        let elapsed = now.saturating_duration_since(started);
        let interval_ms = LETTER_INTERVAL_MS as u128;
        let stage = (elapsed.as_millis() / interval_ms) as usize;
        let mut visible = stage.saturating_add(1);
        if visible > total_chars {
            visible = total_chars;
        }

        let header_completion_ms = if total_chars <= 1 {
            0
        } else {
            LETTER_INTERVAL_MS * (total_chars as u64 - 1)
        };
        let header_completion = Duration::from_millis(header_completion_ms);
        let body_delay = Duration::from_millis(BODY_DELAY_MS);
        let header_done = elapsed >= header_completion;
        let body_visible = header_done && elapsed >= header_completion + body_delay;

        let header_text = if visible >= total_chars {
            Cow::Borrowed(header_text)
        } else {
            Cow::Owned(header_text.chars().take(visible).collect())
        };

        let mut schedule_next_in = None;
        if !body_visible {
            let next_target = if visible < total_chars {
                Duration::from_millis(LETTER_INTERVAL_MS * visible as u64)
            } else {
                header_completion + body_delay
            };

            let mut remaining = if next_target > elapsed {
                next_target - elapsed
            } else {
                Duration::from_millis(0)
            };

            if remaining == Duration::from_millis(0) {
                remaining = Duration::from_millis(MIN_FRAME_MS);
            }

            let min_delay = Duration::from_millis(MIN_FRAME_MS);
            schedule_next_in = Some(remaining.max(min_delay));
        }

        IntroState {
            header_text,
            body_visible,
            schedule_next_in,
        }
    }

    fn build_context(model: &AutoActiveViewModel) -> VariantContext {
        let button = model.button.as_ref().map(|btn| ButtonContext {
            label: btn.label.clone(),
            enabled: btn.enabled,
        });
        VariantContext {
            button,
            ctrl_hint: model.ctrl_switch_hint.clone(),
            manual_hint: model.manual_hint.clone(),
        }
    }

    fn normalize_status_message(message: &str) -> Option<String> {
        let mapped = ChatComposer::map_status_message(message);
        let trimmed = mapped.trim();
        if trimmed.is_empty() {
            None
        } else {
            Some(trimmed.to_string())
        }
    }

    fn update_status_message(&mut self, message: String) -> bool {
        let new_value = Self::normalize_status_message(&message);
        if self.status_message.as_deref() == new_value.as_deref() {
            return false;
        }
        self.status_message = new_value;
        true
    }

    pub(crate) fn handle_active_key_event(
        &mut self,
        _pane: &mut BottomPane<'_>,
        key_event: KeyEvent,
    ) -> bool {
        if !matches!(key_event.kind, KeyEventKind::Press | KeyEventKind::Repeat) {
            return false;
        }

        if key_event
            .modifiers
            .contains(KeyModifiers::CONTROL)
            && matches!(key_event.code, KeyCode::Char('s') | KeyCode::Char('S'))
        {
            self.app_event_tx.send(AppEvent::ShowAutoDriveSettings);
            return true;
        }

        let awaiting_without_input = matches!(
            &self.model,
            AutoCoordinatorViewModel::Active(model)
                if model.awaiting_submission && !model.show_composer
        );
        if awaiting_without_input {
            // Allow approval keys to bubble so ChatWidget handles them.
            let allow_passthrough = matches!(
                key_event.code,
                KeyCode::Esc | KeyCode::Enter | KeyCode::Char(' ') | KeyCode::Char('e') | KeyCode::Char('E')
            );
            if !allow_passthrough {
                return true;
            }
        }

        matches!(key_event.code, KeyCode::Up | KeyCode::Down)
    }


    fn frame_style_for_model(&self, model: &AutoActiveViewModel) -> FrameStyle {
        let mut style = self.style.frame.clone();
        if self.style.variant == AutoDriveVariant::Beacon {
            if let Some(accent) = style.accent.as_mut() {
                accent.style = if model.awaiting_submission {
                    Style::default()
                        .fg(colors::warning())
                        .add_modifier(Modifier::BOLD)
                } else if model.waiting_for_review {
                    Style::default()
                        .fg(colors::info())
                        .add_modifier(Modifier::BOLD)
                } else if model.cli_running || model.waiting_for_response {
                    Style::default()
                        .fg(colors::primary())
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default()
                        .fg(colors::success())
                        .add_modifier(Modifier::BOLD)
                };
            }
        }
        style
    }

    fn effective_elapsed(model: &AutoActiveViewModel) -> Option<Duration> {
        if let Some(duration) = model.elapsed {
            Some(duration)
        } else {
            model
                .started_at
                .map(|started| Instant::now().saturating_duration_since(started))
        }
    }

    fn status_label(model: &AutoActiveViewModel) -> &'static str {
        if model.waiting_for_review {
            "Awaiting review"
        } else if model.waiting_for_response || model.cli_running {
            "Running"
        } else if model.awaiting_submission {
            "Awaiting input"
        } else if model.started_at.is_some() {
            "Running"
        } else if model.elapsed.is_some() && model.started_at.is_none() {
            "Stopped"
        } else {
            "Ready"
        }
    }

    fn is_generic_status_message(message: &str) -> bool {
        matches!(message, "Auto Drive" | "Auto Drive Goal")
    }

    fn resolve_display_message(&self, model: &AutoActiveViewModel) -> String {
        if let Some(message) = self
            .status_message
            .as_ref()
            .map(|msg| msg.trim())
            .filter(|msg| !msg.is_empty())
            .filter(|msg| !Self::is_generic_status_message(msg))
        {
            return message.to_string();
        }

        if let Some(current) = model.progress_current.as_ref() {
            let trimmed = current.trim();
            if !trimmed.is_empty() {
                return trimmed.to_string();
            }
        }

        if model.awaiting_submission {
            if let Some(countdown) = &model.countdown {
                return format!("Awaiting confirmation ({}s)", countdown.remaining);
            }
            if let Some(button) = &model.button {
                let trimmed = button.label.trim();
                if !trimmed.is_empty() {
                    return trimmed.to_string();
                }
            }
        }

        for status in &model.status_lines {
            let trimmed = status.trim();
            if !trimmed.is_empty() {
                return trimmed.to_string();
            }
        }

        auto_drive_strings::next_auto_drive_phrase().to_string()
    }

    fn runtime_text(&self, model: &AutoActiveViewModel) -> String {
        let label = Self::status_label(model);
        let mut details: Vec<String> = Vec::new();
        if let Some(duration) = Self::effective_elapsed(model) {
            if duration.as_secs() > 0 {
                details.push(Self::format_elapsed(duration));
            }
        }
        details.push(Self::format_turns(model.turns_completed));
        format!("{} ({})", label, details.join(", "))
    }

    fn render_header(
        &self,
        area: Rect,
        buf: &mut Buffer,
        model: &AutoActiveViewModel,
        frame_style: &FrameStyle,
        display_message: &str,
        header_label: &str,
        full_title: &str,
        intro: &IntroState<'_>,
    ) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        let animating = intro.schedule_next_in.is_some() && !model.intro_reduced_motion;
        let mut left_spans: Vec<Span<'static>> = Vec::new();
        left_spans.push(Span::raw(" "));

        let fallback_color = frame_style
            .border_style
            .fg
            .or(frame_style.title_style.fg)
            .unwrap_or_else(colors::primary);

        if animating {
            let total_chars = full_title.chars().count().max(1);
            let visible_chars: Vec<char> = header_label.chars().collect();
            if !visible_chars.is_empty() {
                for (idx, ch) in visible_chars.iter().enumerate() {
                    let gradient_position = if total_chars > 1 {
                        idx as f32 / (total_chars as f32 - 1.0)
                    } else {
                        0.0
                    };
                    let mut color = gradient_multi(gradient_position);
                    if visible_chars.len() == total_chars {
                        color = mix_rgb(color, fallback_color, 0.65);
                    } else if idx == visible_chars.len().saturating_sub(1) {
                        color = mix_rgb(color, Color::Rgb(255, 255, 255), 0.35);
                    }
                    left_spans.push(Span::styled(
                        ch.to_string(),
                        Style::default()
                            .fg(color)
                            .add_modifier(Modifier::BOLD),
                    ));
                }
            }
        } else {
            let mut title_style = frame_style.title_style.clone();
            title_style.fg = Some(fallback_color);
            title_style = title_style.add_modifier(Modifier::BOLD);
            left_spans.push(Span::styled(header_label.to_string(), title_style));
        }

        left_spans.push(Span::styled(
            " > ",
            Style::default().fg(colors::text_dim()),
        ));
        left_spans.push(Span::styled(
            display_message.to_string(),
            Style::default().fg(colors::text()),
        ));
        let left_line = Line::from(left_spans);

        let runtime = self.runtime_text(model);
        let runtime_display = if runtime.is_empty() {
            String::new()
        } else {
            format!(" {} ", runtime)
        };
        let right_width = UnicodeWidthStr::width(runtime_display.as_str()).min(area.width as usize) as u16;
        let constraints = if right_width == 0 {
            vec![Constraint::Fill(1)]
        } else {
            vec![Constraint::Fill(1), Constraint::Length(right_width)]
        };
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(constraints)
            .split(area);

        Paragraph::new(left_line).render(chunks[0], buf);

        if right_width > 0 {
            Paragraph::new(Line::from(Span::styled(
                runtime_display,
                self.style.summary_style.clone(),
            )))
            .alignment(Alignment::Right)
            .render(chunks[chunks.len() - 1], buf);
        }
    }

    fn status_message_line(&self, display_message: &str) -> Option<Line<'static>> {
        let message = self.status_message.as_ref()?;
        let trimmed = message.trim();
        if trimmed.is_empty() {
            return None;
        }
        if trimmed.eq_ignore_ascii_case("auto drive") {
            return None;
        }
        if trimmed == display_message {
            return None;
        }

        let style = Style::default()
            .fg(colors::info())
            .add_modifier(Modifier::ITALIC);

        Some(Line::from(vec![
            Span::raw("   "),
            Span::styled(trimmed.to_string(), style),
        ]))
    }

    fn status_message_wrap_count(&self, width: u16, display_message: &str) -> usize {
        if width == 0 {
            return 0;
        }
        let Some(message) = self.status_message.as_ref() else {
            return 0;
        };
        let trimmed = message.trim();
        if trimmed.is_empty() {
            return 0;
        }
        if trimmed.eq_ignore_ascii_case("auto drive") {
            return 0;
        }
        if trimmed == display_message {
            return 0;
        }
        let display = format!("   {trimmed}");
        Self::wrap_count(display.as_str(), width)
    }

    fn cli_prompt_lines(&self, model: &AutoActiveViewModel) -> Option<Vec<Line<'static>>> {
        let prompt = model
            .cli_prompt
            .as_ref()
            .map(|value| value.trim_end())
            .filter(|value| !value.is_empty());
        let context = model
            .cli_context
            .as_ref()
            .map(|value| value.trim())
            .filter(|value| !value.is_empty());

        if prompt.is_none() && context.is_none() {
            return None;
        }

        let header_style = Style::default()
            .fg(colors::text())
            .add_modifier(Modifier::BOLD);
        let context_label_style = Style::default()
            .fg(colors::text_dim())
            .add_modifier(Modifier::BOLD);
        let context_body_style = Style::default()
            .fg(colors::text_dim())
            .add_modifier(Modifier::ITALIC);
        let prompt_label_style = Style::default()
            .fg(colors::info())
            .add_modifier(Modifier::BOLD);
        let prompt_body_style = Style::default().fg(colors::text());

        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::from(vec![
            Span::raw("   "),
            Span::styled("Auto Drive will send:", header_style),
        ]));

        if let Some(value) = context {
            lines.push(Line::from(vec![
                Span::raw("     "),
                Span::styled("Preface:", context_label_style),
            ]));
            for line in value.lines() {
                let trimmed = line.trim_end();
                if trimmed.is_empty() {
                    lines.push(Line::default());
                } else {
                    lines.push(Line::from(vec![
                        Span::raw("       "),
                        Span::styled(trimmed.to_string(), context_body_style),
                    ]));
                }
            }
        }

        if context.is_some() && prompt.is_some() {
            lines.push(Line::default());
        }

        if let Some(value) = prompt {
            lines.push(Line::from(vec![
                Span::raw("     "),
                Span::styled("Prompt:", prompt_label_style),
            ]));
            for line in value.lines() {
                let trimmed = line.trim_end();
                if trimmed.is_empty() {
                    lines.push(Line::default());
                } else {
                    lines.push(Line::from(vec![
                        Span::raw("       "),
                        Span::styled(trimmed.to_string(), prompt_body_style),
                    ]));
                }
            }
        } else {
            lines.push(Line::from(vec![
                Span::raw("     "),
                Span::styled("Prompt:", prompt_label_style),
            ]));
            lines.push(Line::from(vec![
                Span::raw("       "),
                Span::styled(
                    "(Coordinator did not supply a prompt)".to_string(),
                    prompt_body_style.add_modifier(Modifier::ITALIC),
                ),
            ]));
        }

        Some(lines)
    }

    fn manual_hint_line(&self, ctx: &VariantContext) -> Option<Line<'static>> {
        ctx.manual_hint.as_ref().map(|hint| {
            Line::from(Span::styled(
                hint.clone(),
                Style::default()
                    .fg(colors::info())
                    .add_modifier(Modifier::ITALIC),
            ))
        })
    }

    fn button_block_lines(&self, ctx: &VariantContext) -> Option<Vec<Line<'static>>> {
        let button = ctx.button.as_ref()?;
        let label = button.label.trim();
        if label.is_empty() {
            return None;
        }

        let glyphs = self.style.button.glyphs;
        let inner = format!(" {label} ");
        let inner_width = UnicodeWidthStr::width(inner.as_str());
        let horizontal = glyphs.horizontal.to_string().repeat(inner_width);
        let top = format!(
            "{}{}{}",
            glyphs.top_left, horizontal, glyphs.top_right
        );
        let middle = format!(
            "{}{}{}",
            glyphs.vertical, inner, glyphs.vertical
        );
        let bottom = format!(
            "{}{}{}",
            glyphs.bottom_left, horizontal, glyphs.bottom_right
        );

        let button_style = if button.enabled {
            self.style.button.enabled_style.clone()
        } else {
            self.style.button.disabled_style.clone()
        };

        let mut lines = Vec::with_capacity(3);
        lines.push(Line::from(Span::styled(top, button_style.clone())));

        let mut middle_spans: Vec<Span<'static>> = vec![Span::styled(middle, button_style.clone())];
        if let Some(mut hint_spans) = Self::ctrl_hint_spans(ctx.ctrl_hint.as_str()) {
            if !hint_spans.is_empty() {
                middle_spans.push(Span::raw("   "));
                middle_spans.append(&mut hint_spans);
            }
        }
        lines.push(Line::from(middle_spans));

        lines.push(Line::from(Span::styled(bottom, button_style)));
        Some(lines)
    }

    fn ctrl_hint_spans(hint: &str) -> Option<Vec<Span<'static>>> {
        let trimmed = hint.trim();
        if trimmed.is_empty() {
            return None;
        }

        let normal_style = Style::default().fg(colors::text());
        let bold_style = Style::default()
            .fg(colors::text())
            .add_modifier(Modifier::BOLD);

        let lower = trimmed.to_ascii_lowercase();
        if lower.starts_with("esc") {
            let rest = &trimmed[3..];
            let mut use_prefix = rest.is_empty();
            if let Some(ch) = rest.chars().next() {
                if ch.is_whitespace() || matches!(ch, ':' | '-' | ',' | ';') {
                    use_prefix = true;
                }
            }

            if use_prefix {
                let prefix = &trimmed[..3];
                let mut spans = Vec::new();
                spans.push(Span::styled(prefix.to_string(), bold_style));
                if !rest.is_empty() {
                    spans.push(Span::styled(rest.to_string(), normal_style));
                }
                return Some(spans);
            }
        }

        Some(vec![Span::styled(trimmed.to_string(), normal_style)])
    }

    fn ctrl_hint_line(&self, ctx: &VariantContext) -> Option<Line<'static>> {
        if ctx.button.is_some() {
            return None;
        }
        Self::ctrl_hint_spans(ctx.ctrl_hint.as_str()).map(Line::from)
    }

    fn inner_width(&self, width: u16) -> u16 {
        width.max(1)
    }

    fn wrap_count(text: &str, width: u16) -> usize {
        if width == 0 {
            return text.lines().count().max(1);
        }
        let max_width = width as usize;
        text.lines()
            .map(|line| {
                let trimmed = line.trim_end();
                let w = UnicodeWidthStr::width(trimmed);
                let lines = if w == 0 {
                    1
                } else {
                    (w + max_width - 1) / max_width
                };
                lines.max(1)
            })
            .sum()
    }

    fn estimated_height_active(
        &self,
        width: u16,
        ctx: &VariantContext,
        model: &AutoActiveViewModel,
        composer_height: u16,
    ) -> u16 {
        let mut total = 1usize // blank spacer row
            .saturating_add(Self::HEADER_HEIGHT as usize);

        if !model.awaiting_submission {
            return total.min(u16::MAX as usize) as u16;
        }

        let inner_width = self.inner_width(width);
        let button_height = if ctx.button.is_some() { 3 } else { 0 };
        let hint_height = ctx
            .manual_hint
            .as_ref()
            .map(|text| Self::wrap_count(text, inner_width))
            .unwrap_or(0);
        let ctrl_hint = ctx.ctrl_hint.trim();
        let ctrl_height = if ctx.button.is_some() {
            0
        } else if ctrl_hint.is_empty() {
            0
        } else {
            Self::wrap_count(ctrl_hint, inner_width)
        };

        let display_message = self.resolve_display_message(model);
        total = total.saturating_add(self.status_message_wrap_count(inner_width, &display_message));

        if let Some(prompt_lines) = self.cli_prompt_lines(model) {
            let prompt_height = Self::lines_height(&prompt_lines, inner_width) as usize;
            total += prompt_height;
            if prompt_height > 0 && ctx.button.is_some() {
                total += 1; // spacer before button
            }
        }
        if ctx.button.is_some() {
            total += button_height;
        }
        if ctx.manual_hint.is_some() {
            total += hint_height.max(1);
        }
        if ctrl_height > 0 {
            total += 1; // spacer before ctrl hint
            total += ctrl_height.max(1);
        }

        let composer_block = usize::from(composer_height);
        if composer_block > 0 {
            total = total.saturating_add(composer_block);
        }

        if self.build_status_summary(model).is_some() {
            total = total.saturating_add(1);
        }

        total.min(u16::MAX as usize) as u16
    }

    fn render_active(
        &self,
        area: Rect,
        buf: &mut Buffer,
        model: &AutoActiveViewModel,
        composer: Option<&ChatComposer>,
    ) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        let frame_style = self.frame_style_for_model(model);
        let display_message = self.resolve_display_message(model);
        let intro = Self::intro_state(frame_style.title_text, model);
        if let Some(delay) = intro.schedule_next_in {
            self.app_event_tx.send(AppEvent::ScheduleFrameIn(delay));
        }

        // Draw spacer row to match composer spacing.
        let spacer_row = Rect {
            x: area.x,
            y: area.y,
            width: area.width,
            height: 1,
        };
        Self::clear_row(spacer_row, buf);

        if area.height <= 1 {
            return;
        }

        let header_height = Self::HEADER_HEIGHT.min(area.height.saturating_sub(1));
        let header_area = Rect {
            x: area.x,
            y: area.y + 1,
            width: area.width,
            height: header_height,
        };
        let header_label = intro.header_text.as_ref();
        self.render_header(
            header_area,
            buf,
            model,
            &frame_style,
            &display_message,
            header_label,
            frame_style.title_text,
            &intro,
        );

        if area.height <= 1 + Self::HEADER_HEIGHT {
            return;
        }

        let inner = Rect {
            x: area.x,
            y: area.y + 1 + Self::HEADER_HEIGHT,
            width: area.width,
            height: area
                .height
                .saturating_sub(1)
                .saturating_sub(Self::HEADER_HEIGHT),
        };
        if inner.height == 0 || inner.width == 0 {
            return;
        }

        if !model.awaiting_submission {
            return;
        }

        if !intro.body_visible {
            Self::clear_rect(inner, buf);
            return;
        }

        let ctx = Self::build_context(model);
        let mut top_lines: Vec<Line<'static>> = Vec::new();
        let mut after_lines: Vec<Line<'static>> = Vec::new();

        if let Some(mut prompt_lines) = self.cli_prompt_lines(model) {
            top_lines.append(&mut prompt_lines);
        }

        if let Some(button_block) = self.button_block_lines(&ctx) {
            if !top_lines.is_empty() {
                top_lines.push(Line::default());
            }
            top_lines.extend(button_block);
        }

        if let Some(hint_line) = self.manual_hint_line(&ctx) {
            if !after_lines.is_empty() {
                after_lines.push(Line::default());
            }
            after_lines.push(hint_line);
        }

        if let Some(ctrl_hint_line) = self.ctrl_hint_line(&ctx) {
            if !after_lines.is_empty() {
                after_lines.push(Line::default());
            }
            after_lines.push(ctrl_hint_line);
        }

        if let Some(progress_text) = Self::compose_progress_line(model) {
            let line = Line::from(Span::styled(
                progress_text,
                Style::default().fg(colors::text()),
            ));
            if top_lines.is_empty() {
                top_lines.push(line);
            } else {
                top_lines.insert(0, line);
            }
        }

        if let Some(line) = self.status_message_line(&display_message) {
            if top_lines.is_empty() {
                top_lines.push(line);
            } else {
                top_lines.insert(0, line);
            }
        }

        let summary_line = self.build_status_summary(model);
        let mut summary_height: u16 = if summary_line.is_some() { 1 } else { 0 };

        let mut top_height = Self::lines_height(&top_lines, inner.width);
        let mut after_height = Self::lines_height(&after_lines, inner.width);

        // `ChatComposer::render_ref` expects to operate on a region that is two
        // columns wider than the tight composer rectangle. Reconstruct that width
        // so height estimation matches render-time wrapping exactly.
        let mut composer_block: u16 = if model.show_composer {
            if let Some(composer) = composer {
                let measurement_width = inner.width.saturating_add(2);
                let mut desired_block = composer.desired_height(measurement_width);
                if desired_block < Self::MIN_COMPOSER_VIEWPORT {
                    desired_block = Self::MIN_COMPOSER_VIEWPORT;
                }
                desired_block
            } else {
                0
            }
        } else {
            0
        };

        let total_needed = top_height as usize
            + after_height as usize
            + summary_height as usize
            + composer_block as usize;

        if total_needed > inner.height as usize {
            let mut deficit = total_needed - inner.height as usize;

            let reduce_after = usize::from(after_height).min(deficit);
            after_height = after_height.saturating_sub(reduce_after as u16);
            deficit -= reduce_after;

            let reduce_top = usize::from(top_height).min(deficit);
            top_height = top_height.saturating_sub(reduce_top as u16);
            deficit -= reduce_top;

            if deficit > 0 && summary_height > 0 {
                let reduce_summary = usize::from(summary_height).min(deficit);
                summary_height = summary_height.saturating_sub(reduce_summary as u16);
                deficit -= reduce_summary;
            }

            if deficit > 0 && model.show_composer {
                let reducible = composer_block.saturating_sub(Self::MIN_COMPOSER_VIEWPORT);
                let reduce_composer = usize::from(reducible).min(deficit);
                composer_block = composer_block.saturating_sub(reduce_composer as u16);
            }
        }

        let composer_height = if model.show_composer && composer.is_some() {
            let max_space_for_composer = inner
                .height
                .saturating_sub(top_height)
                .saturating_sub(after_height)
                .saturating_sub(summary_height);

            if max_space_for_composer == 0 {
                1
            } else {
                composer_block.min(max_space_for_composer).max(1)
            }
        } else {
            0
        };

        let mut cursor_y = inner.y;
        if top_height > 0 {
            let max_height = inner.y + inner.height - cursor_y;
            let rect_height = top_height.min(max_height);
            if rect_height > 0 {
                let top_rect = Rect {
                    x: inner.x,
                    y: cursor_y,
                    width: inner.width,
                    height: rect_height,
                };
                Paragraph::new(top_lines.clone())
                    .wrap(Wrap { trim: true })
                    .render(top_rect, buf);
                cursor_y = cursor_y.saturating_add(rect_height);
            }
        }

        if composer_height > 0 && cursor_y < inner.y + inner.height {
            if let Some(composer) = composer {
                let max_height = inner.y + inner.height - cursor_y;
                let rect_height = composer_height.min(max_height);
                if rect_height > 0 {
                    let composer_rect = Rect {
                        x: inner.x,
                        y: cursor_y,
                        width: inner.width,
                        height: rect_height,
                    };
                    composer.render_ref(composer_rect, buf);
                    cursor_y = cursor_y.saturating_add(rect_height);
                }
            }
        }

        if after_height > 0 && cursor_y < inner.y + inner.height {
            let max_height = inner
                .y
                .saturating_add(inner.height)
                .saturating_sub(cursor_y)
                .saturating_sub(summary_height);
            let rect_height = after_height.min(max_height);
            if rect_height > 0 {
                let after_rect = Rect {
                    x: inner.x,
                    y: cursor_y,
                    width: inner.width,
                    height: rect_height,
                };
                Paragraph::new(after_lines.clone())
                    .wrap(Wrap { trim: true })
                    .render(after_rect, buf);
                cursor_y = cursor_y.saturating_add(rect_height);
            }
        }

        if let Some(line) = summary_line {
            if cursor_y < inner.y + inner.height {
                let rect_height = (inner.y + inner.height - cursor_y).max(1);
                let summary_rect = Rect {
                    x: inner.x,
                    y: cursor_y,
                    width: inner.width,
                    height: rect_height,
                };
                Paragraph::new(line).render(summary_rect, buf);
            }
        }
    }

    fn clear_row(area: Rect, buf: &mut Buffer) {
        if area.height == 0 {
            return;
        }
        for x in area.x..area.x.saturating_add(area.width) {
            let cell = &mut buf[(x, area.y)];
            cell.set_symbol(" ");
            cell.set_style(Style::default().fg(colors::text()).bg(colors::background()));
        }
    }

    fn clear_rect(area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }
        for offset in 0..area.height {
            let row = Rect {
                x: area.x,
                y: area.y + offset,
                width: area.width,
                height: 1,
            };
            Self::clear_row(row, buf);
        }
    }

    fn lines_height(lines: &[Line<'static>], width: u16) -> u16 {
        if lines.is_empty() {
            return 0;
        }
        if width == 0 {
            return lines.len() as u16;
        }
        lines.iter().fold(0u16, |acc, line| {
            let line_width = line.width() as u16;
            let segments = if line_width == 0 {
                1
            } else {
                (line_width + width - 1) / width
            };
            acc.saturating_add(segments.max(1))
        })
    }

    fn build_status_summary(&self, model: &AutoActiveViewModel) -> Option<Line<'static>> {
        let mut spans: Vec<Span<'static>> = Vec::new();
        spans.push(Span::styled(
            self.runtime_text(model),
            self.style.summary_style.clone(),
        ));

        let secondary_style = Style::default().fg(colors::text_dim());
        let separator = self.style.footer_separator.to_string();

        let agents_text = if model.agents_enabled {
            "Agents Enabled"
        } else {
            "Agents Disabled"
        };
        let review_text = if model.review_enabled {
            "Review Enabled"
        } else {
            "Review Disabled"
        };

        spans.push(Span::styled(separator.clone(), secondary_style.clone()));
        spans.push(Span::styled(agents_text.to_string(), secondary_style));
        spans.push(Span::styled(
            separator,
            Style::default().fg(colors::text_dim()),
        ));
        spans.push(Span::styled(
            review_text.to_string(),
            Style::default().fg(colors::text_dim()),
        ));

        Some(Line::from(spans))
    }

    fn format_elapsed(duration: Duration) -> String {
        let total_seconds = duration.as_secs();
        let hours = total_seconds / 3600;
        let minutes = (total_seconds % 3600) / 60;
        let seconds = total_seconds % 60;

        if hours > 0 {
            if minutes > 0 {
                format!("{}h {:02}m", hours, minutes)
            } else {
                format!("{}h", hours)
            }
        } else if minutes > 0 {
            if seconds > 0 {
                format!("{}m {:02}s", minutes, seconds)
            } else {
                format!("{}m", minutes)
            }
        } else {
            format!("{}s", seconds)
        }
    }

    fn format_turns(turns: usize) -> String {
        let label = if turns == 1 { "turn" } else { "turns" };
        format!("{} {}", turns, label)
    }

    fn compose_progress_line(model: &AutoActiveViewModel) -> Option<String> {
        let mut parts: Vec<String> = Vec::new();
        if let Some(past) = model.progress_past.as_ref() {
            let trimmed = past.trim();
            if !trimmed.is_empty() {
                parts.push(trimmed.to_string());
            }
        }
        if let Some(current) = model.progress_current.as_ref() {
            let trimmed = current.trim();
            if !trimmed.is_empty() {
                parts.push(trimmed.to_string());
            }
        }
        if parts.is_empty() {
            None
        } else {
            Some(parts.join(" "))
        }
    }

}

impl<'a> BottomPaneView<'a> for AutoCoordinatorView {
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        Some(self)
    }

    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        if !matches!(key_event.kind, KeyEventKind::Press | KeyEventKind::Repeat) {
            return;
        }

        if key_event
            .modifiers
            .contains(KeyModifiers::CONTROL)
            && matches!(key_event.code, KeyCode::Char('s') | KeyCode::Char('S'))
        {
            self.app_event_tx.send(AppEvent::ShowAutoDriveSettings);
        }
    }

    fn desired_height(&self, width: u16) -> u16 {
        let AutoCoordinatorViewModel::Active(model) = &self.model;
        let ctx = Self::build_context(model);
        self.estimated_height_active(width, &ctx, model, 0)
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        if area.height == 0 {
            return;
        }

        let AutoCoordinatorViewModel::Active(model) = &self.model;
        self.render_active(area, buf, model, None);
    }

    fn render_with_composer(
        &self,
        area: Rect,
        buf: &mut Buffer,
        composer: &ChatComposer,
    ) {
        if area.height == 0 {
            return;
        }

        let AutoCoordinatorViewModel::Active(model) = &self.model;
        self.render_active(area, buf, model, Some(composer));
    }

    fn update_status_text(&mut self, text: String) -> ConditionalUpdate {
        if self.update_status_message(text) {
            ConditionalUpdate::NeedsRedraw
        } else {
            ConditionalUpdate::NoRedraw
        }
    }

    fn handle_paste_with_composer(
        &mut self,
        composer: &mut ChatComposer,
        pasted: String,
    ) -> ConditionalUpdate {
        if composer.handle_paste(pasted) {
            ConditionalUpdate::NeedsRedraw
        } else {
            ConditionalUpdate::NoRedraw
        }
    }

    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        Some(self)
    }
}
