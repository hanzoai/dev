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

pub fn frame_at_time(_def: SpinnerDefinition, _time_ms: u64) -> &'static str {
    "⠋"
}

pub struct SpinnerDefinition {
    pub name: &'static str,
    pub frames: &'static [&'static str],
    pub interval: u64,
}

pub fn find_spinner_by_name(name: &str) -> Option<SpinnerDefinition> {
    match name {
        "dots" => Some(SpinnerDefinition {
            name: "dots",
            frames: &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"],
            interval: 80,
        }),
        _ => None,
    }
}

pub fn current_spinner() -> SpinnerDefinition {
    SpinnerDefinition {
        name: "dots",
        frames: &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"],
        interval: 80,
    }
}

pub fn spinner_names() -> Vec<&'static str> {
    vec!["dots", "line", "dots2", "dots3"]
}

pub fn add_custom_spinner(_name: String, _frames: Vec<String>, _interval: u64) {
    // Placeholder for custom spinner registration
}

pub fn switch_spinner(_name: &str) {
    // Placeholder for switching spinner
}

pub fn spinner_label_for(name: &str) -> String {
    name.to_string()
}

pub fn global_max_frame_len() -> usize {
    1 // Maximum frame length for alignment
}