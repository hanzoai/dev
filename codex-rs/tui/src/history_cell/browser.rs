use super::HistoryCell;
use ratatui::prelude::Style;
use ratatui::style::Stylize;
use ratatui::text::Line;
use std::path::PathBuf;
use std::time::Duration;
use unicode_width::UnicodeWidthChar;
use url::Url;

const MAX_ACTIONS: usize = 24;
const MAX_CONSOLE: usize = 12;
const MAX_SCREENSHOT_HISTORY: usize = 24;

/// Format a duration in digital clock style (MM:SS or HH:MM:SS).
fn format_duration_digital(duration: Duration) -> String {
    let total_seconds = duration.as_secs();
    let hours = total_seconds / 3_600;
    let minutes = (total_seconds % 3_600) / 60;
    let seconds = total_seconds % 60;

    if hours == 0 {
        return format!("{minutes:02}:{seconds:02}");
    }

    format!("{hours:02}:{minutes:02}:{seconds:02}")
}

#[derive(Clone, Debug)]
pub(crate) struct BrowserScreenshotRecord {
    pub path: PathBuf,
    pub url: Option<String>,
    pub timestamp: Duration,
}

#[derive(Clone, Debug)]
pub(crate) struct BrowserSessionCell {
    url: Option<String>,
    title: Option<String>,
    actions: Vec<BrowserAction>,
    console_messages: Vec<String>,
    screenshot_path: Option<String>,
    screenshot_history: Vec<BrowserScreenshotRecord>,
    total_duration: Duration,
    completed: bool,
    cell_key: Option<String>,
    headless: Option<bool>,
    status_code: Option<String>,
}

impl Default for BrowserSessionCell {
    fn default() -> Self {
        Self {
            url: None,
            title: None,
            actions: Vec::new(),
            console_messages: Vec::new(),
            screenshot_path: None,
            screenshot_history: Vec::new(),
            total_duration: Duration::ZERO,
            completed: false,
            cell_key: None,
            headless: None,
            status_code: None,
        }
    }
}

#[derive(Clone, Debug)]
struct BrowserAction {
    action: String,
    target: Option<String>,
    value: Option<String>,
    outcome: Option<String>,
    timestamp: Duration,
}

impl BrowserSessionCell {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn set_url(&mut self, url: impl Into<String>) {
        self.url = Some(url.into());
    }

    pub(crate) fn summary_label(&self) -> String {
        self.display_label()
    }

    pub(crate) fn current_url(&self) -> Option<&str> {
        self.url.as_deref()
    }

    pub(crate) fn record_action(
        &mut self,
        timestamp: Duration,
        duration: Duration,
        action: String,
        target: Option<String>,
        value: Option<String>,
        outcome: Option<String>,
    ) {
        if self.actions.last().map_or(false, |last| {
            last.action == action
                && last.target == target
                && last.value == value
                && last.outcome == outcome
        }) {
            return;
        }
        let action_entry = BrowserAction {
            action,
            target,
            value,
            outcome: outcome.clone(),
            timestamp,
        };
        self.actions.push(action_entry);
        if self.actions.len() > MAX_ACTIONS {
            let overflow = self.actions.len() - MAX_ACTIONS;
            self.actions.drain(0..overflow);
        }
        let finish = timestamp.saturating_add(duration);
        if finish > self.total_duration {
            self.total_duration = finish;
        }
        if let Some(outcome) = outcome {
            if let Some(code) = extract_status_code(&outcome) {
                self.status_code = Some(code);
            }
        }
    }

    pub(crate) fn add_console_message(&mut self, message: String) {
        if self
            .console_messages
            .last()
            .map_or(false, |last| last == &message)
        {
            return;
        }
        self.console_messages.push(message);
        if self.console_messages.len() > MAX_CONSOLE {
            let overflow = self.console_messages.len() - MAX_CONSOLE;
            self.console_messages.drain(0..overflow);
        }
    }

    pub(crate) fn record_screenshot(
        &mut self,
        timestamp: Duration,
        path: PathBuf,
        url: Option<String>,
    ) {
        let display_path = path.display().to_string();
        self.screenshot_path = Some(display_path);
        self.screenshot_history.push(BrowserScreenshotRecord {
            path,
            url,
            timestamp,
        });
        if self.screenshot_history.len() > MAX_SCREENSHOT_HISTORY {
            let overflow = self.screenshot_history.len() - MAX_SCREENSHOT_HISTORY;
            self.screenshot_history.drain(0..overflow);
        }
    }

    pub(crate) fn set_headless(&mut self, headless: Option<bool>) {
        self.headless = headless;
    }

    pub(crate) fn set_status_code(&mut self, code: Option<String>) {
        self.status_code = code;
    }

    pub(crate) fn set_cell_key(&mut self, key: Option<String>) {
        self.cell_key = key;
    }

    pub(crate) fn cell_key(&self) -> Option<&str> {
        self.cell_key.as_deref()
    }

    pub(crate) fn screenshot_history(&self) -> &[BrowserScreenshotRecord] {
        &self.screenshot_history
    }

    pub(crate) fn total_duration(&self) -> Duration {
        self.total_duration
    }

    pub(crate) fn full_action_entries(&self) -> Vec<(String, String, String)> {
        let show_minutes = self.total_duration.as_secs() >= 60;
        let mut entries: Vec<(String, String, String)> = Vec::new();
        if self.actions.is_empty() {
            if let Some(url) = self.url.as_ref() {
                let time_label = format!(
                    " {}",
                    Self::format_elapsed_label(Duration::ZERO, show_minutes)
                );
                entries.push((time_label, "Opened".to_string(), url.clone()));
            }
            return entries;
        }

        for action in &self.actions {
            let time_label = format!(
                " {}",
                Self::format_elapsed_label(action.timestamp, show_minutes)
            );
            let entry = format_action_entry(action, time_label);
            entries.push((entry.time_label, entry.label, entry.detail));
        }

        entries
    }

    fn normalized_title(&self) -> Option<String> {
        self.title
            .as_ref()
            .map(|value| value.trim())
            .filter(|value| !value.is_empty() && !value.eq_ignore_ascii_case("(pending)"))
            .map(|value| value.to_string())
    }

    fn display_host(&self) -> Option<String> {
        self.url
            .as_ref()
            .and_then(|url| Url::parse(url).ok())
            .and_then(|parsed| parsed.host_str().map(|host| host.to_string()))
    }

    fn display_label(&self) -> String {
        if let Some(title) = self.normalized_title() {
            return title;
        }
        if let Some(host) = self.display_host() {
            return host;
        }
        self.url
            .as_ref()
            .cloned()
            .unwrap_or_else(|| "Browser Session".to_string())
    }

    pub(crate) fn format_elapsed_label(duration: Duration, _show_minutes: bool) -> String {
        format_duration_digital(duration)
    }

    fn build_plain_lines(&self, width: u16) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();

        // Header
        let label = if self.headless.unwrap_or(true) {
            "Browser (headless)"
        } else {
            "Browser"
        };
        let mut header = format!("{}: {}", label, self.display_label());
        if let Some(code) = &self.status_code {
            header.push_str(&format!(" [{}]", code));
        }
        let status = if self.completed { "done" } else { "running" };
        header.push_str(&format!(" ({})", status));
        lines.push(Line::from(header).style(Style::new().bold()));

        // URL
        if let Some(url) = &self.url {
            lines.push(Line::from(format!("  URL: {}", url)));
        }

        // Actions (last few)
        let show_minutes = self.total_duration.as_secs() >= 60;
        let display_actions: Vec<&BrowserAction> = if self.actions.len() > 8 {
            self.actions.iter().rev().take(8).collect::<Vec<_>>().into_iter().rev().collect()
        } else {
            self.actions.iter().collect()
        };
        if !display_actions.is_empty() {
            lines.push(Line::from("  Actions:"));
            for action in display_actions {
                let time = Self::format_elapsed_label(action.timestamp, show_minutes);
                let summary = format_action_summary(action);
                let available = (width as usize).saturating_sub(8);
                let text = if summary.len() > available && available > 3 {
                    format!("    {} {}", time, truncate_str(&summary, available))
                } else {
                    format!("    {} {}", time, summary)
                };
                lines.push(Line::from(text));
            }
        }

        // Console
        if let Some(last) = self.console_messages.last() {
            lines.push(Line::from(format!("  Console: {}", last)));
        }

        // Screenshot
        if let Some(path) = &self.screenshot_path {
            lines.push(Line::from(format!("  Screenshot: {}", path)));
        }

        lines
    }
}

impl HistoryCell for BrowserSessionCell {
    fn display_lines(&self, width: u16) -> Vec<Line<'static>> {
        self.build_plain_lines(width)
    }
}

// ---------------------------------------------------------------------------
// Action formatting helpers
// ---------------------------------------------------------------------------

#[derive(Clone)]
struct ActionEntry {
    label: String,
    detail: String,
    time_label: String,
}

fn format_action_summary(action: &BrowserAction) -> String {
    match (&action.target, &action.value, &action.outcome) {
        (Some(target), Some(value), Some(outcome)) => {
            format!(
                "{} {} \u{2192} {}",
                action.action,
                target,
                outcome_for_display(outcome, value)
            )
        }
        (Some(target), Some(value), None) => {
            format!("{} {} = {}", action.action, target, value)
        }
        (Some(target), None, Some(outcome)) => {
            format!("{} {} \u{2192} {}", action.action, target, outcome)
        }
        (Some(target), None, None) => format!("{} {}", action.action, target),
        (None, Some(value), Some(outcome)) => {
            format!("{} {} \u{2192} {}", action.action, value, outcome)
        }
        (None, Some(value), None) => format!("{} {}", action.action, value),
        (None, None, Some(outcome)) => format!("{} \u{2192} {}", action.action, outcome),
        _ => action.action.clone(),
    }
}

fn format_action_entry(action: &BrowserAction, time_label: String) -> ActionEntry {
    let action_lower = action.action.to_ascii_lowercase();
    match action_lower.as_str() {
        "click" | "mouse_click" => {
            let target = action.target.as_deref().unwrap_or("").trim();
            let detail = if target.starts_with('(') && target.ends_with(')') {
                format!("at {}", target)
            } else if !target.is_empty() {
                target.to_string()
            } else if let Some(value) = action.value.as_deref() {
                value.trim().to_string()
            } else if let Some(outcome) = action.outcome.as_deref() {
                outcome.trim().to_string()
            } else {
                String::new()
            };
            ActionEntry {
                label: "Clicked".to_string(),
                detail,
                time_label,
            }
        }
        "press_key" | "key" | "press" => {
            let key_raw = action
                .value
                .as_deref()
                .or(action.outcome.as_deref())
                .or(action.target.as_deref())
                .unwrap_or("?")
                .trim();
            let key = sanitize_pressed_detail(key_raw);
            ActionEntry {
                label: "Pressed".to_string(),
                detail: key,
                time_label,
            }
        }
        "type" | "input" | "enter_text" | "fill" | "insert_text" => {
            let typed = action
                .value
                .as_deref()
                .or(action.outcome.as_deref())
                .unwrap_or("?")
                .trim()
                .to_string();
            ActionEntry {
                label: "Typed".to_string(),
                detail: typed,
                time_label,
            }
        }
        "navigate" | "open" | "nav" => {
            let dest = action
                .target
                .as_deref()
                .map(sanitize_nav_text)
                .filter(|s| !s.is_empty())
                .or_else(|| {
                    action
                        .value
                        .as_deref()
                        .map(sanitize_nav_text)
                        .filter(|s| !s.is_empty())
                })
                .or_else(|| {
                    action
                        .outcome
                        .as_deref()
                        .map(sanitize_nav_text)
                        .filter(|s| !s.is_empty())
                })
                .unwrap_or_default();
            ActionEntry {
                label: "Opened".to_string(),
                detail: dest,
                time_label,
            }
        }
        other if other.starts_with("scroll") => {
            let detail = action
                .value
                .as_deref()
                .filter(|v| !v.trim().is_empty())
                .map(|v| v.trim().to_string())
                .or_else(|| {
                    action
                        .outcome
                        .as_deref()
                        .filter(|o| !o.trim().is_empty())
                        .map(|o| o.trim().to_string())
                })
                .or_else(|| {
                    action
                        .target
                        .as_deref()
                        .filter(|t| !t.trim().is_empty())
                        .map(|t| t.trim().to_string())
                })
                .unwrap_or_else(|| {
                    let summary = format_action_summary(action);
                    summary
                        .strip_prefix(other)
                        .map(|suffix| {
                            suffix
                                .trim_start_matches(|c| c == ' ' || c == ':' || c == '-')
                                .to_string()
                        })
                        .filter(|suffix| !suffix.is_empty())
                        .unwrap_or(summary)
                });
            ActionEntry {
                label: "Scrolled".to_string(),
                detail,
                time_label,
            }
        }
        _ => {
            let summary = format_action_summary(action);
            let label = titleize_action(action.action.as_str());
            let trimmed = summary
                .strip_prefix(action.action.as_str())
                .map(|suffix| {
                    suffix
                        .trim_start_matches(|c| c == ' ' || c == ':' || c == '-')
                        .to_string()
                })
                .filter(|suffix| !suffix.is_empty())
                .unwrap_or_else(|| summary.clone());
            ActionEntry {
                label,
                detail: trimmed,
                time_label,
            }
        }
    }
}

fn titleize_action(raw: &str) -> String {
    let mut words: Vec<String> = Vec::new();
    for segment in raw.split(['_', '-']).filter(|part| !part.is_empty()) {
        let mut chars = segment.chars();
        if let Some(first) = chars.next() {
            let first_upper = first.to_uppercase().collect::<String>();
            let rest = chars.as_str().to_ascii_lowercase();
            words.push(format!("{}{}", first_upper, rest));
        }
    }
    if words.is_empty() {
        raw.to_string()
    } else {
        words.join(" ")
    }
}

fn sanitize_pressed_detail(raw: &str) -> String {
    let mut candidate = raw;
    const PREFIXES: &[&str] = &["pressed key:", "press key:", "key pressed:", "key:"];
    for prefix in PREFIXES {
        if let Some(rest) = strip_prefix_ignore_case(candidate, prefix) {
            candidate = rest;
            break;
        }
    }
    let cleaned = candidate.trim();
    if cleaned.is_empty() {
        raw.trim().to_string()
    } else {
        cleaned.to_string()
    }
}

fn sanitize_nav_text(raw: &str) -> String {
    let mut candidate = raw;
    const PREFIXES: &[&str] = &[
        "browser opened to:",
        "opened to:",
        "navigated to",
        "nav to:",
        "opened:",
    ];
    for prefix in PREFIXES {
        if let Some(rest) = strip_prefix_ignore_case(candidate, prefix) {
            candidate = rest;
            break;
        }
    }
    candidate.trim().trim_start_matches(':').trim().to_string()
}

fn strip_prefix_ignore_case<'a>(text: &'a str, prefix: &str) -> Option<&'a str> {
    let text_bytes = text.as_bytes();
    let prefix_bytes = prefix.as_bytes();
    if text_bytes.len() < prefix_bytes.len() {
        return None;
    }
    for (idx, prefix_byte) in prefix_bytes.iter().enumerate() {
        if text_bytes[idx].to_ascii_lowercase() != prefix_byte.to_ascii_lowercase() {
            return None;
        }
    }
    Some(text.get(prefix.len()..)?.trim_start())
}

fn outcome_for_display(outcome: &str, value: &str) -> String {
    if outcome == "value set" {
        value.to_string()
    } else {
        outcome.to_string()
    }
}

fn extract_status_code(outcome: &str) -> Option<String> {
    let trimmed = outcome.trim();
    if trimmed.len() < 3 {
        return None;
    }
    let code: String = trimmed
        .chars()
        .take_while(|c| c.is_ascii_digit())
        .collect();
    if code.len() == 3 {
        Some(code)
    } else {
        None
    }
}

fn truncate_str(input: &str, max: usize) -> String {
    if input.chars().count() <= max {
        input.to_string()
    } else {
        let truncated: String = input.chars().take(max.saturating_sub(1)).collect();
        format!("{}\u{2026}", truncated)
    }
}

#[allow(dead_code)]
fn string_display_width(text: &str) -> usize {
    text.chars()
        .map(|ch| UnicodeWidthChar::width(ch).unwrap_or(0))
        .sum()
}
