use super::*;
use std::cell::{Cell, RefCell};
use std::time::Instant;

pub(crate) struct AnimatedWelcomeCell {
    fade_start: RefCell<Option<Instant>>,
    faded_out: Cell<bool>,
    version_label: String,
}

impl AnimatedWelcomeCell {
    pub(crate) fn new() -> Self {
        Self {
            fade_start: RefCell::new(None),
            faded_out: Cell::new(false),
            version_label: format!("v{}", code_version::version()),
        }
    }

    pub(crate) fn set_available_height(&self, _height: u16) {
        // No-op: simple text layout does not need height hints.
    }

    pub(crate) fn begin_fade(&self) {
        let mut slot = self.fade_start.borrow_mut();
        if slot.is_none() {
            *slot = Some(Instant::now());
        }
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
        use ratatui::style::{Modifier, Style};
        use ratatui::text::Span;
        vec![
            Line::from(""),
            Line::from(vec![
                Span::raw("  >_ "),
                Span::styled(
                    "hanzo dev",
                    Style::default().add_modifier(Modifier::BOLD),
                ),
                Span::raw(format!(" ({})", self.version_label)),
            ]),
            Line::from(format!("      {}", crate::greeting::greeting_placeholder())),
            Line::from(""),
        ]
    }

    fn desired_height(&self, _width: u16) -> u16 {
        4
    }

    fn has_custom_render(&self) -> bool {
        false
    }

    fn is_animating(&self) -> bool {
        if let Some(fade_time) = *self.fade_start.borrow() {
            if !self.faded_out.get() {
                if fade_time.elapsed() < std::time::Duration::from_millis(400) {
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
