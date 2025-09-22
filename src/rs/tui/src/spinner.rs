//! Spinner animation for TUI

use std::time::{Duration, Instant};

pub struct Spinner {
    frames: Vec<String>,
    current_frame: usize,
    last_update: Instant,
    interval: Duration,
}

impl Spinner {
    pub fn new() -> Self {
        Self::dots()
    }

    pub fn dots() -> Self {
        Self {
            frames: vec![
                "".to_string(),
                "".to_string(),
                "9".to_string(),
                "8".to_string(),
                "<".to_string(),
                "4".to_string(),
                "&".to_string(),
                "'".to_string(),
                "".to_string(),
                "".to_string(),
            ],
            current_frame: 0,
            last_update: Instant::now(),
            interval: Duration::from_millis(80),
        }
    }

    pub fn tick(&mut self) -> &str {
        let now = Instant::now();
        if now.duration_since(self.last_update) >= self.interval {
            self.current_frame = (self.current_frame + 1) % self.frames.len();
            self.last_update = now;
        }
        &self.frames[self.current_frame]
    }

    pub fn current(&self) -> &str {
        &self.frames[self.current_frame]
    }
}

impl Default for Spinner {
    fn default() -> Self {
        Self::new()
    }
}

pub fn current_spinner() -> &'static str {
    "⠋"
}

pub fn frame_at_time(_time: Duration) -> &'static str {
    "⠋"
}

pub fn spinner_names() -> Vec<&'static str> {
    vec!["dots", "line", "dots2", "dots3"]
}