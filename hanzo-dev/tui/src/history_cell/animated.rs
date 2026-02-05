use super::*;
use ratatui::style::Stylize;
use std::cell::Cell;
use std::cell::RefCell;
use std::time::Duration;
use std::time::Instant;

/// Simplified welcome cell that renders a Codex-style bordered card
/// with version and model info instead of complex glitch animations.
pub(crate) struct AnimatedWelcomeCell {
    start_time: Instant,
    completed: Cell<bool>,
    fade_start: RefCell<Option<Instant>>,
    faded_out: Cell<bool>,
    version_label: String,
    hidden: Cell<bool>,
}

impl AnimatedWelcomeCell {
    pub(crate) fn new() -> Self {
        Self {
            start_time: Instant::now(),
            completed: Cell::new(false),
            fade_start: RefCell::new(None),
            faded_out: Cell::new(false),
            version_label: format!("v{}", env!("CARGO_PKG_VERSION")),
            hidden: Cell::new(false),
        }
    }

    pub(crate) fn set_available_height(&self, _height: u16) {
        // No longer needed for simplified rendering
    }

    fn fade_start(&self) -> Option<Instant> {
        *self.fade_start.borrow()
    }

    fn set_fade_start(&self) {
        let mut slot = self.fade_start.borrow_mut();
        if slot.is_none() {
            *slot = Some(Instant::now());
        }
    }

    pub(crate) fn begin_fade(&self) {
        self.set_fade_start();
    }

    pub(crate) fn should_remove(&self) -> bool {
        self.faded_out.get()
    }
}

impl HistoryCell for AnimatedWelcomeCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn kind(&self) -> HistoryCellType {
        HistoryCellType::AnimatedWelcome
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        // Codex-style simple bordered card with version info
        // Format: ">_ Hanzo Dev (vX.X.X)"
        let title_line = Line::from(vec![
            Span::from(">_ ").dim(),
            Span::from("hanzo dev").bold(),
            Span::from(" ").dim(),
            Span::from(format!("({})", self.version_label)).dim(),
        ]);

        let greeting_line = Line::from(vec![
            Span::from("   "), // Indent to align with text after ">_ "
            Span::from(crate::greeting::greeting_placeholder()),
        ]);

        vec![title_line, Line::from(""), greeting_line]
    }

    fn desired_height(&self, _width: u16) -> u16 {
        // Simple 3-line card: title, blank, greeting
        3
    }

    fn has_custom_render(&self) -> bool {
        // Use simple text rendering, no custom glitch animation
        false
    }

    fn custom_render(&self, _area: Rect, _buf: &mut Buffer) {
        // Not used - has_custom_render returns false
    }

    fn custom_render_with_skip(&self, _area: Rect, _buf: &mut Buffer, _skip_rows: u16) {
        // Not used - has_custom_render returns false
    }

    fn is_animating(&self) -> bool {
        // Simple fade animation for removal
        let animation_duration = Duration::from_secs(1);
        if !self.completed.get() {
            if self.start_time.elapsed() < animation_duration {
                return true;
            }
            self.completed.set(true);
        }

        if let Some(fade_time) = self.fade_start() {
            if !self.faded_out.get() {
                if fade_time.elapsed() < Duration::from_millis(500) {
                    return true;
                }
                self.faded_out.set(true);
            }
        }

        false
    }

    fn trigger_fade(&self) {
        AnimatedWelcomeCell::begin_fade(self);
    }

    fn should_remove(&self) -> bool {
        AnimatedWelcomeCell::should_remove(self)
    }
}

pub(crate) fn new_animated_welcome() -> AnimatedWelcomeCell {
    AnimatedWelcomeCell::new()
}
