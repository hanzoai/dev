use super::*;
use crate::history::state::{RateLimitLegendEntry, RateLimitsRecord, TextTone};
use codex_common::elapsed::format_duration;
use codex_core::protocol::RateLimitSnapshotEvent;
use ratatui::style::Color;

pub(crate) struct RateLimitsCell {
    record: RateLimitsRecord,
}

impl RateLimitsCell {
    pub(crate) fn from_record(record: RateLimitsRecord) -> Self {
        Self { record }
    }

    pub(crate) fn record(&self) -> &RateLimitsRecord {
        &self.record
    }

    pub(crate) fn record_mut(&mut self) -> &mut RateLimitsRecord {
        &mut self.record
    }
}

impl HistoryCell for RateLimitsCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Notice
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(
            Line::styled(
                "Rate limits update",
                Style::default()
                    .fg(crate::colors::warning())
                    .add_modifier(Modifier::BOLD),
            ),
        );

        let snapshot = &self.record.snapshot;
        lines.extend(snapshot_summary_lines(snapshot));

        if !self.record.legend.is_empty() {
            lines.push(Line::default());
            lines.push(Line::styled(
                "Warnings",
                Style::default()
                    .fg(crate::colors::warning())
                    .add_modifier(Modifier::BOLD),
            ));
            for entry in &self.record.legend {
                lines.extend(legend_lines(entry));
            }
        }

        lines
    }

    fn desired_height(&self, width: u16) -> u16 {
        Paragraph::new(Text::from(self.display_lines_trimmed()))
            .wrap(Wrap { trim: false })
            .line_count(width)
            .try_into()
            .unwrap_or(0)
    }
}

fn snapshot_summary_lines(snapshot: &RateLimitSnapshotEvent) -> Vec<Line<'static>> {
    let mut lines = Vec::new();

    let hourly_line = format!(
        "└ Hourly usage: {:.0}% of {} min window",
        snapshot.primary_used_percent,
        snapshot.primary_window_minutes,
    );
    lines.push(Line::from(vec![Span::styled(
        hourly_line,
        Style::default().fg(crate::colors::text()),
    )]));

    if let Some(seconds) = snapshot.primary_reset_after_seconds {
        let reset = format_duration(std::time::Duration::from_secs(seconds));
        lines.push(Line::from(vec![Span::styled(
            format!("   • resets in {reset}"),
            Style::default().fg(crate::colors::text_dim()),
        )]));
    }

    let weekly_line = format!(
        "└ Weekly usage: {:.0}% of {} min window",
        snapshot.secondary_used_percent,
        snapshot.secondary_window_minutes,
    );
    lines.push(Line::from(vec![Span::styled(
        weekly_line,
        Style::default().fg(crate::colors::text()),
    )]));

    if let Some(seconds) = snapshot.secondary_reset_after_seconds {
        let reset = format_duration(std::time::Duration::from_secs(seconds));
        lines.push(Line::from(vec![Span::styled(
            format!("   • resets in {reset}"),
            Style::default().fg(crate::colors::text_dim()),
        )]));
    }

    lines
}

fn legend_lines(entry: &RateLimitLegendEntry) -> Vec<Line<'static>> {
    let tone_style = Style::default().fg(color_for_tone(entry.tone));
    let mut lines = Vec::new();
    lines.push(Line::from(vec![
        Span::styled("  • ", tone_style),
        Span::styled(entry.label.clone(), tone_style.add_modifier(Modifier::BOLD)),
    ]));

    if !entry.description.is_empty() {
        lines.push(Line::from(vec![Span::styled(
            format!("    {}", entry.description),
            Style::default().fg(crate::colors::text()),
        )]));
    }

    lines
}

fn color_for_tone(tone: TextTone) -> Color {
    match tone {
        TextTone::Default => crate::colors::text(),
        TextTone::Dim => crate::colors::text_dim(),
        TextTone::Primary => crate::colors::primary(),
        TextTone::Success => crate::colors::success(),
        TextTone::Warning => crate::colors::warning(),
        TextTone::Error => crate::colors::error(),
        TextTone::Info => crate::colors::info(),
    }
}
