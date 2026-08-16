use super::*;
use ratatui::style::Stylize;
use std::cell::{Cell, RefCell};
use std::time::{Duration, Instant};

/// The first thing on screen: the product name, its version, and the question.
/// Plain text, laid down once; it fades when the first turn begins.
pub(crate) struct AnimatedWelcomeCell {
    start_time: Instant,
    completed: Cell<bool>,
    fade_start: RefCell<Option<Instant>>,
    faded_out: Cell<bool>,
    version_label: String,
}

impl AnimatedWelcomeCell {
    pub(crate) fn new() -> Self {
        Self {
            start_time: Instant::now(),
            completed: Cell::new(false),
            fade_start: RefCell::new(None),
            faded_out: Cell::new(false),
            version_label: format!("v{}", code_version::version()),
        }
    }

    fn fade_start(&self) -> Option<Instant> {
        *self.fade_start.borrow()
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
        let title = Line::from(vec![
            Span::from(">_ ").dim(),
            Span::from("hanzo dev").bold(),
            Span::from(format!(" ({})", self.version_label)).dim(),
        ]);
        vec![
            title,
            Line::from(""),
            Line::from(crate::greeting::greeting_placeholder()),
        ]
    }

    fn desired_height(&self, _width: u16) -> u16 {
        3
    }

    fn is_animating(&self) -> bool {
        if !self.completed.get() {
            if self.start_time.elapsed() < Duration::from_secs(1) {
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
