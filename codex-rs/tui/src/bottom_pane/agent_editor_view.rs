use crossterm::event::{KeyCode, KeyEvent};
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Rect, Margin};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph, Widget};

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;

use super::bottom_pane_view::BottomPaneView;
use super::form_text_field::FormTextField;
use super::BottomPane;

#[derive(Debug)]
pub(crate) struct AgentEditorView {
    name: String,
    enabled: bool,
    params_ro: FormTextField,
    params_wr: FormTextField,
    instr: FormTextField,
    field: usize, // 0 toggle, 1 ro, 2 wr, 3 instr, 4 save, 5 cancel
    complete: bool,
    app_event_tx: AppEventSender,
    installed: bool,
    install_hint: String,
}

impl AgentEditorView {
    pub fn new(
        name: String,
        enabled: bool,
        args_read_only: Option<Vec<String>>,
        args_write: Option<Vec<String>>,
        instructions: Option<String>,
        command: String,
        app_event_tx: AppEventSender,
    ) -> Self {
        // Simple PATH check similar to the core executor’s logic
        fn command_exists(cmd: &str) -> bool {
            if cmd.contains(std::path::MAIN_SEPARATOR) || cmd.contains('/') || cmd.contains('\\') {
                return std::fs::metadata(cmd).map(|m| m.is_file()).unwrap_or(false);
            }
            #[cfg(target_os = "windows")]
            {
                if let Ok(p) = which::which(cmd) {
                    if !p.is_file() { return false; }
                    match p.extension().and_then(|e| e.to_str()).map(|s| s.to_ascii_lowercase()) {
                        Some(ext) if matches!(ext.as_str(), "exe" | "com" | "cmd" | "bat") => true,
                        _ => false,
                    }
                } else { false }
            }
            #[cfg(not(target_os = "windows"))]
            {
                use std::os::unix::fs::PermissionsExt;
                let Some(path_os) = std::env::var_os("PATH") else { return false; };
                for dir in std::env::split_paths(&path_os) {
                    if dir.as_os_str().is_empty() { continue; }
                    let candidate = dir.join(cmd);
                    if let Ok(meta) = std::fs::metadata(&candidate) {
                        if meta.is_file() {
                            if meta.permissions().mode() & 0o111 != 0 { return true; }
                        }
                    }
                }
                false
            }
        }

        let mut v = Self {
            name,
            enabled,
            params_ro: FormTextField::new_single_line(),
            params_wr: FormTextField::new_single_line(),
            instr: FormTextField::new_multi_line(),
            field: 0,
            complete: false,
            app_event_tx,
            installed: command_exists(&command),
            install_hint: String::new(),
        };

        if let Some(ro) = args_read_only { v.params_ro.set_text(&ro.join(" ")); }
        if let Some(wr) = args_write { v.params_wr.set_text(&wr.join(" ")); }
        if let Some(s) = instructions { v.instr.set_text(&s); v.instr.move_cursor_to_start(); }

        // OS-specific short hint
        #[cfg(target_os = "macos")]
        { v.install_hint = format!("'{}' not found. On macOS, try Homebrew (brew install {}) or consult the agent's docs.", command, command); }
        #[cfg(target_os = "linux")]
        { v.install_hint = format!("'{}' not found. On Linux, install via your package manager or consult the agent's docs.", command); }
        #[cfg(target_os = "windows")]
        { v.install_hint = format!("'{}' not found. On Windows, install the CLI from the vendor site and ensure it’s on PATH.", command); }

        v
    }
}

impl<'a> BottomPaneView<'a> for AgentEditorView {
    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        if !self.installed {
            match key_event {
                KeyEvent { code: KeyCode::Esc, .. } | KeyEvent { code: KeyCode::Enter, .. } => { self.complete = true; }
                _ => {}
            }
            return;
        }
        match key_event {
            KeyEvent { code: KeyCode::Esc, .. } => { self.complete = true; self.app_event_tx.send(AppEvent::ShowAgentsOverview); },
            KeyEvent { code: KeyCode::Up, .. } => { if self.field > 0 { self.field -= 1; } },
            KeyEvent { code: KeyCode::Down, .. } => { self.field = (self.field + 1).min(5); },
            KeyEvent { code: KeyCode::Left | KeyCode::Right, .. } if self.field == 0 => { self.enabled = !self.enabled; },
            KeyEvent { code: KeyCode::Char(' '), .. } if self.field == 0 => { self.enabled = !self.enabled; },
            ev @ KeyEvent { .. } if self.field == 1 => { let _ = self.params_ro.handle_key(ev); },
            ev @ KeyEvent { .. } if self.field == 2 => { let _ = self.params_wr.handle_key(ev); },
            ev @ KeyEvent { .. } if self.field == 3 => { let _ = self.instr.handle_key(ev); },
            KeyEvent { code: KeyCode::Enter, .. } if self.field == 4 => {
                // Save: split params by whitespace; empty -> None
                let ro = self.params_ro.text().split_whitespace().map(|s| s.to_string()).collect::<Vec<_>>();
                let wr = self.params_wr.text().split_whitespace().map(|s| s.to_string()).collect::<Vec<_>>();
                let ro_opt = if ro.is_empty() { None } else { Some(ro) };
                let wr_opt = if wr.is_empty() { None } else { Some(wr) };
                let instr_opt = { let t = self.instr.text().trim().to_string(); if t.is_empty() { None } else { Some(t) } };
                self.app_event_tx.send(AppEvent::UpdateAgentConfig {
                    name: self.name.clone(),
                    enabled: self.enabled,
                    args_read_only: ro_opt,
                    args_write: wr_opt,
                    instructions: instr_opt,
                });
                self.complete = true;
                self.app_event_tx.send(AppEvent::ShowAgentsOverview);
            }
            KeyEvent { code: KeyCode::Enter, .. } if self.field == 5 => { self.complete = true; self.app_event_tx.send(AppEvent::ShowAgentsOverview); }
            _ => {}
        }
    }

    fn is_complete(&self) -> bool { self.complete }

    fn desired_height(&self, width: u16) -> u16 {
        // Match the layout math used in the Command editor so buttons never clip.
        // inner(width) = width-2 (borders); content = inner-1 (left pad used by Paragraph)
        let inner_w = width.saturating_sub(2);
        let content_w = inner_w.saturating_sub(1).max(10);
        // Single‑line input boxes are 3 rows each (inner 1 + borders 2)
        let single_box_h: u16 = 3;
        // Instructions box: compute desired inner height from the field, cap to 8 rows visible
        let instr_inner_w = content_w.saturating_sub(4); // borders(2) + padding(2)
        let desired_instr_inner = self.instr.desired_height(instr_inner_w);
        let instr_box_h = desired_instr_inner.min(8).saturating_add(2);
        // Total content rows including consistent spacing above/below each section
        let content_rows: u16 = 1  // top spacer
            + 1  // title
            + 1  // spacer after title
            + 1  // Enabled row
            + 1  // spacer
            + 1  // RO label
            + single_box_h
            + 1  // spacer
            + 1  // WR label
            + single_box_h
            + 1  // spacer
            + 1  // Instructions label
            + instr_box_h
            + 1  // spacer
            + 1  // buttons row
            + 1; // bottom spacer
        // Add the outer block borders (already counted by area height), but clamp to a sane range
        content_rows.clamp(12, 60)
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        Clear.render(area, buf);
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()))
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .title(" Configure Agent ")
            .title_alignment(Alignment::Center);
        let inner = block.inner(area);
        block.render(area, buf);

        let content = Rect { x: inner.x.saturating_add(1), y: inner.y, width: inner.width.saturating_sub(2), height: inner.height };

        let sel = |idx: usize| if self.field == idx { Style::default().bg(crate::colors::selection()).add_modifier(Modifier::BOLD) } else { Style::default() };
        let label = |idx: usize| if self.field == idx { Style::default().fg(crate::colors::primary()).add_modifier(Modifier::BOLD) } else { Style::default() };

        let mut lines: Vec<Line<'static>> = Vec::new();
        // Compute a responsive layout that guarantees the buttons are visible
        // even on small terminals (e.g., 80x24). We cap the instructions box
        // height to whatever fits after accounting for all fixed rows.
        let top_block: u16 = 3;        // spacer + title + spacer
        let enabled_block: u16 = 2;    // row + spacer
        let ro_label: u16 = 1;
        let ro_box: u16 = 3;           // single-line input box
        let ro_spacer: u16 = 1;
        let wr_label: u16 = 1;
        let wr_box: u16 = 3;
        let wr_spacer: u16 = 1;
        let instr_label: u16 = 1;
        let spacer_before_buttons: u16 = 1;
        let buttons_block: u16 = 1;
        // Footer (optional): blank + key hints; drop if there isn't room.
        let footer_lines_default: u16 = 2;
        // Base rows up to (but not including) the instructions box
        let base_fixed_top = top_block + enabled_block + ro_label + ro_box + ro_spacer + wr_label + wr_box + wr_spacer + instr_label;
        // Desired instructions inner height (without borders)
        let instr_inner_width = content.width.saturating_sub(4);
        let desired_instr_inner = self.instr.desired_height(instr_inner_width).min(8);
        let desired_instr_box_h = desired_instr_inner.saturating_add(2); // add borders

        // Compute how many rows remain and choose the largest box height that keeps buttons visible.
        let mut footer_lines = footer_lines_default;
        let mut ih = desired_instr_box_h;
        let fixed_after_box = spacer_before_buttons + buttons_block + footer_lines;
        if base_fixed_top.saturating_add(ih).saturating_add(fixed_after_box) > content.height {
            // First, try dropping footer entirely
            footer_lines = 0;
        }
        let fixed_after_box = spacer_before_buttons + buttons_block + footer_lines;
        if base_fixed_top.saturating_add(ih).saturating_add(fixed_after_box) > content.height {
            // Reduce the instructions box height as needed, with a minimum of 3 (borders + 1 line)
            let min_ih: u16 = 3;
            let available_for_box = content
                .height
                .saturating_sub(base_fixed_top)
                .saturating_sub(fixed_after_box);
            ih = ih.min(available_for_box).max(min_ih);
        }
        // As a last resort, if even min layout doesn't fit, clamp ih and spacer to keep buttons visible.
        let mut spacer_before_buttons_actual = spacer_before_buttons;
        if base_fixed_top.saturating_add(ih).saturating_add(spacer_before_buttons_actual + buttons_block + footer_lines) > content.height {
            spacer_before_buttons_actual = 0;
        }

        // Top spacer then bold breadcrumb‑style title like the command editor
        lines.push(Line::from(""));
        lines.push(Line::from(Span::styled(format!("Agents » Edit Agent » {}", self.name), Style::default().add_modifier(Modifier::BOLD))));
        lines.push(Line::from(""));

        if !self.installed {
            lines.push(Line::from(Span::styled("Not installed", Style::default().fg(crate::colors::warning()).add_modifier(Modifier::BOLD))));
            lines.push(Line::from(Span::styled(self.install_hint.clone(), Style::default().fg(crate::colors::text_dim()))));
            lines.push(Line::from(""));
            lines.push(Line::from(vec![Span::styled("[ Close ]", Style::default().bg(crate::colors::selection()).add_modifier(Modifier::BOLD))]));
            lines.push(Line::from(""));
            lines.push(Line::from(vec![
                Span::styled("Enter", Style::default().fg(crate::colors::success())),
                Span::styled(" Close  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("Esc", Style::default().fg(crate::colors::error())),
                Span::styled(" Cancel", Style::default().fg(crate::colors::text_dim())),
            ]));

            Paragraph::new(lines)
                .alignment(Alignment::Left)
                .wrap(ratatui::widgets::Wrap { trim: false })
                .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
                .render(content, buf);
            return;
        }

        // Enabled toggle
        let chk = if self.enabled { "[on ]" } else { "[off]" };
        lines.push(Line::from(vec![Span::styled("Enabled:", label(0)), Span::raw("  "), Span::styled(chk, sel(0))]));
        lines.push(Line::from(""));

        // Reserve single-line boxes for params (3 rows each) with one blank spacer between groups
        let single_box_h: u16 = 3;
        lines.push(Line::from(Span::styled("Read-only Params", label(1))));
        for _ in 0..single_box_h { lines.push(Line::from("")); }
        lines.push(Line::from(""));
        lines.push(Line::from(Span::styled("Write Params", label(2))));
        for _ in 0..single_box_h { lines.push(Line::from("")); }
        lines.push(Line::from(""));
        lines.push(Line::from(Span::styled("Instructions", label(3))));
        // Reserve lines for the instructions box based on computed `ih`
        for _ in 0..ih { lines.push(Line::from("")); }
        if spacer_before_buttons_actual > 0 { lines.push(Line::from("")); }

        // Buttons
        let save_style = sel(4).fg(crate::colors::success());
        let cancel_style = sel(5).fg(crate::colors::text());
        lines.push(Line::from(vec![Span::styled("[ Save ]", save_style), Span::raw("  "), Span::styled("[ Cancel ]", cancel_style)]));

        // Footer (optional, only when room exists)
        if footer_lines > 0 {
            lines.push(Line::from(""));
            lines.push(Line::from(vec![
                Span::styled("↑↓", Style::default().fg(crate::colors::function())),
                Span::styled(" Navigate  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("Enter", Style::default().fg(crate::colors::success())),
                Span::styled(" Save/Close  ", Style::default().fg(crate::colors::text_dim())),
                Span::styled("Esc", Style::default().fg(crate::colors::error())),
                Span::styled(" Cancel", Style::default().fg(crate::colors::text_dim())),
            ]));
        }

        Paragraph::new(lines)
            .alignment(Alignment::Left)
            .wrap(ratatui::widgets::Wrap { trim: false })
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .render(content, buf);

        // Draw input boxes at the same y offsets we reserved above
        let mut y = content.y; // start at top of content
        // Skip top spacer + title + spacer
        y = y.saturating_add(top_block);
        // Enabled row + spacer
        y = y.saturating_add(enabled_block);

        // RO params box (3 rows)
        let ro_rect = Rect { x: content.x, y, width: content.width, height: single_box_h };
        let ro_block = Block::default()
            .borders(Borders::ALL)
            .title(Line::from(" Read-only Params "))
            .border_style(if self.field == 1 { Style::default().fg(crate::colors::primary()).add_modifier(Modifier::BOLD) } else { Style::default().fg(crate::colors::border()) });
        let ro_inner = ro_block.inner(ro_rect).inner(Margin::new(1, 0));
        ro_block.render(ro_rect, buf);
        self.params_ro.render(ro_inner, buf, self.field == 1);
        // After RO box + spacer
        y = y.saturating_add(single_box_h + 1);

        // WR params box (3 rows)
        let wr_rect = Rect { x: content.x, y, width: content.width, height: single_box_h };
        let wr_block = Block::default()
            .borders(Borders::ALL)
            .title(Line::from(" Write Params "))
            .border_style(if self.field == 2 { Style::default().fg(crate::colors::primary()).add_modifier(Modifier::BOLD) } else { Style::default().fg(crate::colors::border()) });
        let wr_inner = wr_block.inner(wr_rect).inner(Margin::new(1, 0));
        wr_block.render(wr_rect, buf);
        self.params_wr.render(wr_inner, buf, self.field == 2);
        // After WR box + spacer
        y = y.saturating_add(single_box_h + 1);

        // Instructions (multi-line; height consistent with reserved space above)
        let instr_rect = Rect { x: content.x, y, width: content.width, height: ih };
        let instr_block = Block::default().borders(Borders::ALL).border_style(if self.field == 3 { Style::default().fg(crate::colors::primary()).add_modifier(Modifier::BOLD) } else { Style::default().fg(crate::colors::border()) });
        let instr_block = instr_block.title(Line::from(" Instructions "));
        let instr_inner = instr_block.inner(instr_rect).inner(Margin::new(1, 0));
        instr_block.render(instr_rect, buf);
        self.instr.render(instr_inner, buf, self.field == 3);
    }
}
