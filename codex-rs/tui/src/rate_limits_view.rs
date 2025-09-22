use crate::colors;
use chrono::{DateTime, Local, Utc};
use codex_common::elapsed::format_duration;
use codex_core::protocol::RateLimitSnapshotEvent;
use ratatui::prelude::*;
use ratatui::style::Stylize;

const WEEKLY_CELL: &str = "▇▇";
const HOURLY_CELL: &str = "▓▓";
const UNUSED_CELL: &str = "░░";
const BAR_SLOTS: usize = 20;
const BAR_FILLED: &str = "▰";
const BAR_EMPTY: &str = "▱";
const SECTION_INDENT: &str = "  ";
const CHART_INDENT: &str = "     ";

/// Aggregated output used by the `/limits` command.
/// It contains the rendered summary lines, optional legend,
/// and the precomputed gauge state when one can be shown.
#[derive(Debug)]
pub(crate) struct LimitsView {
    pub(crate) summary_lines: Vec<Line<'static>>,
    pub(crate) legend_lines: Vec<Line<'static>>,
    pub(crate) footer_lines: Vec<Line<'static>>,
    grid_state: Option<GridState>,
    grid: GridConfig,
}

impl LimitsView {
    /// Render the gauge for the provided width if the data supports it.
    pub(crate) fn gauge_lines(&self, width: u16) -> Vec<Line<'static>> {
        match self.grid_state {
            Some(state) => render_limit_grid(state, self.grid, width),
            None => Vec::new(),
        }
    }
}

/// Configuration for the simple grid gauge rendered by `/limits`.
#[derive(Clone, Copy, Debug)]
pub(crate) struct GridConfig {
    pub(crate) weekly_slots: usize,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct RateLimitResetInfo {
    pub(crate) primary_last_reset: Option<DateTime<Utc>>,
    pub(crate) weekly_last_reset: Option<DateTime<Utc>>,
}

/// Default gauge configuration used by the TUI.
pub(crate) const DEFAULT_GRID_CONFIG: GridConfig = GridConfig {
    weekly_slots: 100,
};

/// Build the lines and optional gauge used by the `/limits` view.
pub(crate) fn build_limits_view(
    snapshot: &RateLimitSnapshotEvent,
    reset_info: RateLimitResetInfo,
    grid_config: GridConfig,
) -> LimitsView {
    let metrics = RateLimitMetrics::from_snapshot(snapshot);
    let grid_state = extract_capacity_fraction(snapshot)
        .and_then(|fraction| compute_grid_state(&metrics, fraction))
        .map(|state| scale_grid_state(state, grid_config));

    LimitsView {
        summary_lines: build_summary_lines(&metrics, snapshot, reset_info),
        legend_lines: build_legend_lines(grid_state.is_some()),
        footer_lines: build_footer_lines(&metrics),
        grid_state,
        grid: grid_config,
    }
}

#[derive(Debug)]
struct RateLimitMetrics {
    hourly_used: f64,
    weekly_used: f64,
    hourly_remaining: f64,
    weekly_remaining: f64,
    primary_window_minutes: u64,
    weekly_window_minutes: u64,
    primary_to_weekly_ratio_percent: Option<f64>,
}

impl RateLimitMetrics {
    fn from_snapshot(snapshot: &RateLimitSnapshotEvent) -> Self {
        let hourly_used = snapshot.primary_used_percent.clamp(0.0, 100.0);
        let weekly_used = snapshot.weekly_used_percent.clamp(0.0, 100.0);
        let ratio = if snapshot.primary_to_weekly_ratio_percent.is_finite() {
            Some(snapshot.primary_to_weekly_ratio_percent.clamp(0.0, 100.0))
        } else {
            None
        };
        Self {
            hourly_used,
            weekly_used,
            hourly_remaining: (100.0 - hourly_used).max(0.0),
            weekly_remaining: (100.0 - weekly_used).max(0.0),
            primary_window_minutes: snapshot.primary_window_minutes,
            weekly_window_minutes: snapshot.weekly_window_minutes,
            primary_to_weekly_ratio_percent: ratio,
        }
    }

    fn hourly_exhausted(&self) -> bool {
        self.hourly_remaining <= 0.0
    }

    fn weekly_exhausted(&self) -> bool {
        self.weekly_remaining <= 0.0
    }
}

#[derive(Clone, Copy, Debug)]
struct GridState {
    weekly_used_ratio: f64,
    hourly_remaining_ratio: f64,
}

fn build_summary_lines(
    metrics: &RateLimitMetrics,
    snapshot: &RateLimitSnapshotEvent,
    reset_info: RateLimitResetInfo,
) -> Vec<Line<'static>> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push("/limits".magenta().into());
    lines.push("".into());

    lines.push(section_header("Hourly Limit"));
    lines.push(build_bar_line(
        "Used",
        metrics.hourly_used,
        " used",
        Style::default()
            .fg(colors::text())
            .add_modifier(Modifier::BOLD),
    ));
    lines.push(build_hourly_window_line(metrics));
    lines.push(build_hourly_reset_line(
        snapshot.primary_window_minutes,
        reset_info.primary_last_reset,
    ));

    lines.push("".into());

    lines.push(section_header("Weekly Limit"));
    lines.push(build_bar_line(
        "Usage",
        metrics.weekly_used,
        "",
        Style::default()
            .fg(colors::text())
            .add_modifier(Modifier::BOLD),
    ));
    lines.push(build_weekly_window_line(
        metrics.weekly_window_minutes,
        reset_info.weekly_last_reset,
    ));
    lines.push(build_weekly_reset_line(
        snapshot.weekly_window_minutes,
        reset_info.weekly_last_reset,
    ));

    lines.push("".into());
    lines.push(section_header("Chart"));
    lines.push("".into());
    lines
}

fn build_footer_lines(metrics: &RateLimitMetrics) -> Vec<Line<'static>> {
    vec!["".into(), build_status_line(metrics)]
}

fn section_header(title: &str) -> Line<'static> {
    Line::from(vec![Span::styled(
        format!("{SECTION_INDENT}{title}"),
        Style::default().add_modifier(Modifier::BOLD),
    )])
}

fn build_bar_line(label: &str, percent: f64, suffix: &str, style: Style) -> Line<'static> {
    let mut spans: Vec<Span<'static>> = Vec::new();
    spans.push(Span::raw(field_prefix(label)));
    spans.push(Span::styled(render_percent_bar(percent), style));
    let mut text = format_percent(percent);
    if !suffix.is_empty() {
        text.push_str(suffix);
    }
    spans.push(Span::raw(format!(" {text}")));
    Line::from(spans)
}

fn build_hourly_window_line(metrics: &RateLimitMetrics) -> Line<'static> {
    let mut spans: Vec<Span<'static>> = Vec::new();
    spans.push(Span::raw(field_prefix("Window")));
    let ratio = metrics
        .primary_to_weekly_ratio_percent
        .unwrap_or(0.0)
        .clamp(0.0, 100.0);
    spans.push(Span::styled(
        render_percent_bar(ratio),
        Style::default()
            .fg(colors::info())
            .add_modifier(Modifier::BOLD),
    ));

    let primary = format_minutes_short(metrics.primary_window_minutes);
    let weekly = format_minutes_short(metrics.weekly_window_minutes);
    let detail = metrics
        .primary_to_weekly_ratio_percent
        .map(|value| format!("{} ({primary} / {weekly})", format_percent(value)))
        .unwrap_or_else(|| format!("({primary} / {weekly})"));
    spans.push(Span::raw(format!(" {detail}")));
    Line::from(spans)
}

fn build_hourly_reset_line(
    window_minutes: u64,
    last_reset: Option<DateTime<Utc>>,
) -> Line<'static> {
    let text = if let (Some(last), true) = (last_reset, window_minutes > 0) {
        if let Some((remaining, timestamp)) = compute_reset_eta(window_minutes, last) {
            format!("{SECTION_INDENT}Resets in ≈{remaining} @ {timestamp}")
        } else {
            format!("{SECTION_INDENT}Reset timing updating…")
        }
    } else {
        format!("{SECTION_INDENT}Reset shown once next window detected")
    };
    Line::from(vec![Span::raw(text)])
}

fn build_weekly_window_line(
    weekly_minutes: u64,
    last_reset: Option<DateTime<Utc>>,
) -> Line<'static> {
    let prefix = field_prefix("Window");
    let since = last_reset
        .and_then(|last| Utc::now().signed_duration_since(last).to_std().ok())
        .map(format_duration)
        .unwrap_or_else(|| "unknown".to_string());
    let window = format_minutes_short(weekly_minutes);
    Line::from(vec![Span::raw(prefix), Span::raw(format!("({since} / {window})"))])
}

fn build_weekly_reset_line(
    window_minutes: u64,
    last_reset: Option<DateTime<Utc>>,
) -> Line<'static> {
    let text = if let (Some(last), true) = (last_reset, window_minutes > 0) {
        if let Some((remaining, timestamp)) = compute_reset_eta(window_minutes, last) {
            format!("{SECTION_INDENT}Resets in ≈{remaining} @ {timestamp}")
        } else {
            format!("{SECTION_INDENT}Reset timing updating…")
        }
    } else {
        format!("{SECTION_INDENT}Reset shown once next window detected")
    };
    Line::from(vec![Span::raw(text)])
}

fn compute_reset_eta(
    window_minutes: u64,
    last_reset: DateTime<Utc>,
) -> Option<(String, String)> {
    let window_seconds = (window_minutes as i64).checked_mul(60)?;
    if window_seconds <= 0 {
        return None;
    }
    let now = Utc::now();
    let elapsed = now.signed_duration_since(last_reset);
    let mut periods = 0i64;
    if elapsed.num_seconds() > 0 {
        periods = elapsed.num_seconds() / window_seconds;
    }
    let mut next_reset = last_reset + chrono::Duration::seconds(window_seconds);
    if periods > 0 {
        next_reset = last_reset + chrono::Duration::seconds(window_seconds * (periods + 1));
    }
    if next_reset <= now {
        next_reset = now + chrono::Duration::seconds(window_seconds);
    }
    let remaining = next_reset.signed_duration_since(now).to_std().ok()?;
    let local_time = next_reset.with_timezone(&Local).format("%I:%M%P").to_string();
    Some((format_duration(remaining), local_time))
}

fn build_status_line(metrics: &RateLimitMetrics) -> Line<'static> {
    if metrics.weekly_exhausted() || metrics.hourly_exhausted() {
        let reason = match (metrics.hourly_exhausted(), metrics.weekly_exhausted()) {
            (true, true) => "weekly and hourly windows exhausted",
            (true, false) => "hourly window exhausted",
            (false, true) => "weekly window exhausted",
            (false, false) => unreachable!(),
        };
        Line::from(vec![
            Span::raw(SECTION_INDENT),
            Span::styled(
                format!("✕ Rate limited: {reason}"),
                Style::default().fg(colors::error()),
            ),
        ])
    } else {
        Line::from(vec![
            Span::raw(SECTION_INDENT),
            Span::styled(
                "✓ Within current limits".to_string(),
                Style::default().fg(colors::success()),
            ),
        ])
    }
}

fn build_legend_lines(show_gauge: bool) -> Vec<Line<'static>> {
    if !show_gauge {
        return Vec::new();
    }

    let mut lines = Vec::new();
    lines.push(Line::from(vec![
        Span::raw(CHART_INDENT),
        Span::styled(
            WEEKLY_CELL.to_string(),
            Style::default()
                .fg(colors::text())
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw(" weekly usage"),
    ]));
    lines.push(Line::from(vec![
        Span::raw(CHART_INDENT),
        Span::styled(
            HOURLY_CELL.to_string(),
            Style::default()
                .fg(colors::info())
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw(" hourly headroom"),
    ]));
    lines.push(Line::from(vec![
        Span::raw(CHART_INDENT),
        Span::styled(
            UNUSED_CELL.to_string(),
            Style::default()
                .fg(colors::success())
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw(" unused weekly"),
    ]));
    lines
}

fn field_prefix(label: &str) -> String {
    let target_width = 7usize;
    let padding = 2 + target_width.saturating_sub(label.len());
    let spaces = " ".repeat(padding);
    format!("{SECTION_INDENT}{label}:{spaces}")
}

fn render_percent_bar(percent: f64) -> String {
    let clamped = percent.clamp(0.0, 100.0);
    let mut filled = ((clamped / 100.0) * BAR_SLOTS as f64).round() as usize;
    if clamped > 0.0 && filled == 0 {
        filled = 1;
    }
    let filled = filled.min(BAR_SLOTS);
    let empty = BAR_SLOTS.saturating_sub(filled);
    let mut bar = String::with_capacity(BAR_SLOTS * BAR_FILLED.len());
    for _ in 0..filled {
        bar.push_str(BAR_FILLED);
    }
    for _ in 0..empty {
        bar.push_str(BAR_EMPTY);
    }
    bar
}

fn format_percent(percent: f64) -> String {
    let clamped = percent.clamp(0.0, 100.0);
    if clamped == 0.0 || clamped >= 1.0 {
        format!("{clamped:.0}%")
    } else {
        format!("{clamped:.1}%")
    }
}

fn format_minutes_short(minutes: u64) -> String {
    if minutes == 0 {
        return "0m".to_string();
    }
    if minutes % 10_080 == 0 {
        let weeks = minutes / 10_080;
        return format!("{weeks} {}", if weeks == 1 { "week" } else { "weeks" });
    }
    if minutes % 1_440 == 0 {
        let days = minutes / 1_440;
        return format!("{days} {}", if days == 1 { "day" } else { "days" });
    }
    if minutes % 60 == 0 {
        let hours = minutes / 60;
        return format!("{hours} {}", if hours == 1 { "hour" } else { "hours" });
    }
    if minutes > 60 {
        let hours = minutes / 60;
        let mins = minutes % 60;
        if mins == 0 {
            return format!("{hours}h");
        }
        return format!("{hours}h {mins}m");
    }
    format!("{minutes}m")
}

fn extract_capacity_fraction(snapshot: &RateLimitSnapshotEvent) -> Option<f64> {
    let ratio = snapshot.primary_to_weekly_ratio_percent;
    if ratio.is_finite() {
        Some((ratio / 100.0).clamp(0.0, 1.0))
    } else {
        None
    }
}

fn compute_grid_state(metrics: &RateLimitMetrics, capacity_fraction: f64) -> Option<GridState> {
    if capacity_fraction <= 0.0 {
        return None;
    }

    let weekly_used_ratio = (metrics.weekly_used / 100.0).clamp(0.0, 1.0);
    let weekly_remaining_ratio = (1.0 - weekly_used_ratio).max(0.0);

    let hourly_used_ratio = (metrics.hourly_used / 100.0).clamp(0.0, 1.0);
    let hourly_used_within_capacity =
        (hourly_used_ratio * capacity_fraction).min(capacity_fraction);
    let hourly_remaining_within_capacity =
        (capacity_fraction - hourly_used_within_capacity).max(0.0);

    let hourly_remaining_ratio = hourly_remaining_within_capacity.min(weekly_remaining_ratio);

    Some(GridState {
        weekly_used_ratio,
        hourly_remaining_ratio,
    })
}

fn scale_grid_state(state: GridState, grid: GridConfig) -> GridState {
    if grid.weekly_slots == 0 {
        return GridState {
            weekly_used_ratio: 0.0,
            hourly_remaining_ratio: 0.0,
        };
    }
    state
}

/// Convert the grid state to rendered lines for the TUI.
fn render_limit_grid(state: GridState, grid_config: GridConfig, width: u16) -> Vec<Line<'static>> {
    GridLayout::new(grid_config, width)
        .map(|layout| layout.render(state))
        .unwrap_or_default()
}

/// Precomputed layout information for the usage grid.
struct GridLayout {
    size: usize,
}

impl GridLayout {
    const MIN_SIDE: usize = 4;
    const MAX_SIDE: usize = 12;

    fn new(config: GridConfig, width: u16) -> Option<Self> {
        if config.weekly_slots == 0 {
            return None;
        }
        let cell_width = WEEKLY_CELL.chars().count();

        let indent_width = CHART_INDENT.chars().count() as u16;
        let available_inner = width.saturating_sub(indent_width) as usize;
        if available_inner < cell_width {
            return None;
        }

        let base_side = (config.weekly_slots as f64)
            .sqrt()
            .round()
            .clamp(1.0, Self::MAX_SIDE as f64) as usize;
        let width_limited_side =
            ((available_inner + 1) / (cell_width + 1)).clamp(1, Self::MAX_SIDE);

        let mut side = base_side.min(width_limited_side);
        if width_limited_side >= Self::MIN_SIDE {
            side = side.max(Self::MIN_SIDE.min(width_limited_side));
        }
        side = side.clamp(1, width_limited_side);
        while side > 1 && (side * cell_width + side.saturating_sub(1)) > available_inner {
            side -= 1;
        }
        if side == 0 {
            return None;
        }

        Some(Self { size: side })
    }

    /// Render the grid into styled lines for the history cell.
    fn render(&self, state: GridState) -> Vec<Line<'static>> {
        let counts = self.cell_counts(state);
        let mut lines = Vec::new();
        let mut cell_index = 0isize;
        for _ in 0..self.size {
            let mut spans: Vec<Span<'static>> = Vec::new();
            spans.push(Span::raw(CHART_INDENT));

            for col in 0..self.size {
                if col > 0 {
                    spans.push(" ".into());
                }
                let span = if cell_index < counts.dark_cells {
                    Span::styled(
                        WEEKLY_CELL.to_string(),
                        Style::default().fg(colors::text()),
                    )
                } else if cell_index < counts.dark_cells + counts.green_cells {
                    Span::styled(
                        HOURLY_CELL.to_string(),
                        Style::default().fg(colors::info()),
                    )
                } else {
                    Span::styled(
                        UNUSED_CELL.to_string(),
                        Style::default().fg(colors::success()),
                    )
                };
                spans.push(span);
                cell_index += 1;
            }
            lines.push(Line::from(spans));
        }
        lines.push("".into());
        lines
    }

    /// Translate usage ratios into the number of coloured cells.
    fn cell_counts(&self, state: GridState) -> GridCellCounts {
        let total_cells = self.size * self.size;
        let mut dark_cells = (state.weekly_used_ratio * total_cells as f64).round() as isize;
        dark_cells = dark_cells.clamp(0, total_cells as isize);
        let mut green_cells = (state.hourly_remaining_ratio * total_cells as f64).round() as isize;
        if dark_cells + green_cells > total_cells as isize {
            green_cells = (total_cells as isize - dark_cells).max(0);
        }
        GridCellCounts {
            dark_cells,
            green_cells,
        }
    }
}

/// Number of weekly (dark) and hourly (green) cells; remaining slots imply unused weekly capacity.
struct GridCellCounts {
    dark_cells: isize,
    green_cells: isize,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn snapshot() -> RateLimitSnapshotEvent {
        RateLimitSnapshotEvent {
            primary_used_percent: 30.0,
            weekly_used_percent: 60.0,
            primary_to_weekly_ratio_percent: 40.0,
            primary_window_minutes: 300,
            weekly_window_minutes: 10_080,
        }
    }

    #[test]
    fn build_display_constructs_summary_and_gauge() {
        let display = build_limits_view(
            &snapshot(),
            RateLimitResetInfo::default(),
            DEFAULT_GRID_CONFIG,
        );
        let summary_text: Vec<String> = display
            .summary_lines
            .iter()
            .map(|line| {
                line.spans
                    .iter()
                    .map(|span| span.content.as_ref())
                    .collect::<String>()
            })
            .collect();
        assert!(summary_text
            .iter()
            .any(|line| line.contains("Hourly Limit")));
        assert!(summary_text
            .iter()
            .any(|line| line.contains("Weekly Limit")));
        assert!(!display.gauge_lines(80).is_empty());
    }

    #[test]
    fn hourly_and_weekly_percentages_are_not_swapped() {
        let display = build_limits_view(
            &snapshot(),
            RateLimitResetInfo::default(),
            DEFAULT_GRID_CONFIG,
        );
        let summary = display
            .summary_lines
            .iter()
            .map(|line| {
                line.spans
                    .iter()
                    .map(|span| span.content.as_ref())
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join("\n");

        let used_line = summary
            .split('\n')
            .find(|line| line.contains("Used"))
            .expect("expected hourly used line");
        assert!(used_line.contains("30%"));

        let weekly_line = summary
            .split('\n')
            .find(|line| line.contains("Usage"))
            .expect("expected weekly usage line");
        assert!(weekly_line.contains("60%"));
    }

    #[test]
    fn build_display_without_ratio_skips_gauge() {
        let mut s = snapshot();
        s.primary_to_weekly_ratio_percent = f64::NAN;
        let display = build_limits_view(&s, RateLimitResetInfo::default(), DEFAULT_GRID_CONFIG);
        assert!(display.gauge_lines(80).is_empty());
        assert!(display.legend_lines.is_empty());
    }
}
