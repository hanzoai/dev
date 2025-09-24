use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph, Widget};

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;

use super::bottom_pane_view::BottomPaneView;
use super::BottomPane;

#[derive(Clone, Debug)]
pub(crate) struct ToolStatus {
    pub name: &'static str,
    pub description: &'static str,
    pub installed: bool,
    pub install_hint: String,
}

pub(crate) struct ValidationSettingsView {
    patch_harness: bool,
    tools: Vec<(ToolStatus, bool)>,
    app_event_tx: AppEventSender,
    is_complete: bool,
    selected_row: usize,
}

impl ValidationSettingsView {
    pub fn new(
        patch_harness: bool,
        tools: Vec<(ToolStatus, bool)>,
        app_event_tx: AppEventSender,
    ) -> Self {
        Self {
            patch_harness,
            tools,
            app_event_tx,
            is_complete: false,
            selected_row: 0,
        }
    }

    fn toggle_patch_harness(&mut self) {
        self.patch_harness = !self.patch_harness;
        self.app_event_tx
            .send(AppEvent::UpdateValidationPatchHarness(self.patch_harness));
    }

    fn toggle_tool(&mut self, idx: usize) {
        if let Some((status, enabled)) = self.tools.get_mut(idx) {
            if !status.installed {
                return;
            }
            let new_value = !*enabled;
            *enabled = new_value;
            self.app_event_tx.send(AppEvent::UpdateValidationTool {
                name: status.name.to_string(),
                enable: new_value,
            });
        }
    }
}

impl<'a> BottomPaneView<'a> for ValidationSettingsView {
    fn handle_key_event(&mut self, pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        let tool_rows = self.tools.len();
        let max_row = tool_rows + 1; // include close row
        match key_event {
            KeyEvent { code: KeyCode::Up, modifiers: KeyModifiers::NONE, .. } => {
                if self.selected_row > 0 {
                    self.selected_row -= 1;
                }
            }
            KeyEvent { code: KeyCode::Down, modifiers: KeyModifiers::NONE, .. } => {
                if self.selected_row < max_row {
                    self.selected_row += 1;
                }
            }
            KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::NONE, .. }
            | KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::NONE, .. } => {
                if self.selected_row == 0 {
                    self.toggle_patch_harness();
                } else if (1..=tool_rows).contains(&self.selected_row) {
                    self.toggle_tool(self.selected_row - 1);
                }
            }
            KeyEvent { code: KeyCode::Enter, modifiers: KeyModifiers::NONE, .. } => {
                if self.selected_row == 0 {
                    self.toggle_patch_harness();
                } else if self.selected_row == max_row {
                    self.is_complete = true;
                } else if let Some((status, _)) = self.tools.get(self.selected_row - 1) {
                    if !status.installed {
                        let command = status.install_hint.trim();
                        if command.is_empty() {
                            pane.flash_footer_notice(format!(
                                "No install command available for {}",
                                status.name
                            ));
                        } else {
                            pane.flash_footer_notice(format!(
                                "Opening terminal to install {}",
                                status.name
                            ));
                            self.is_complete = true;
                            self.app_event_tx.send(AppEvent::RequestValidationToolInstall {
                                name: status.name.to_string(),
                                command: command.to_string(),
                            });
                        }
                    } else {
                        self.toggle_tool(self.selected_row - 1);
                    }
                }
            }
            KeyEvent { code: KeyCode::Char(' '), modifiers: KeyModifiers::NONE, .. } => {
                if self.selected_row == 0 {
                    self.toggle_patch_harness();
                } else if (1..=tool_rows).contains(&self.selected_row) {
                    self.toggle_tool(self.selected_row - 1);
                }
            }
            KeyEvent { code: KeyCode::Esc, .. } => {
                self.is_complete = true;
            }
            _ => {}
        }
    }

    fn is_complete(&self) -> bool {
        self.is_complete
    }

    fn desired_height(&self, _width: u16) -> u16 {
        let rows = 9 + (self.tools.len() as u16 * 2);
        rows.min(24)
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        Clear.render(area, buf);
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()))
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()))
            .title(" Validation Settings ")
            .title_alignment(Alignment::Center);
        let inner = block.inner(area);
        block.render(area, buf);

        let mut lines: Vec<Line> = Vec::new();
        let max_row = self.tools.len().saturating_add(1);
        lines.push(Line::from(Span::styled(
            "Validation runs before patches apply; toggles affect which checks run.",
            Style::default().fg(crate::colors::text_dim()),
        )));
        lines.push(Line::from(""));

        let mut status_style = Style::default().fg(crate::colors::text());
        if self.selected_row == 0 {
            status_style = status_style.bg(crate::colors::selection()).add_modifier(Modifier::BOLD);
        }
        let mut header_spans = vec![
            Span::styled("Validate New Code: ", Style::default().fg(crate::colors::text_dim())),
            Span::styled(
                if self.patch_harness { "Enabled" } else { "Disabled" },
                status_style,
            ),
        ];
        if self.selected_row == 0 {
            header_spans.push(Span::styled("  ", Style::default().bg(crate::colors::selection())));
            header_spans.push(Span::styled(
                if self.patch_harness { "(press Enter to disable)" } else { "(press Enter to enable)" },
                Style::default().fg(crate::colors::text_dim()).bg(crate::colors::selection()),
            ));
        }
        lines.push(Line::from(header_spans));

        lines.push(Line::from(""));
        lines.push(Line::from(Span::styled("Tools:", Style::default().fg(crate::colors::text_dim()))));
        for (index, (status, enabled)) in self.tools.iter().enumerate() {
            let selected = self.selected_row == index + 1;
            let mut name_style = if status.installed {
                Style::default().fg(crate::colors::success())
            } else {
                Style::default().fg(crate::colors::warning())
            };
            if selected {
                name_style = name_style.bg(crate::colors::selection()).add_modifier(Modifier::BOLD);
            }
            let mut state_style = Style::default().fg(crate::colors::dim());
            if selected {
                state_style = state_style.bg(crate::colors::selection()).add_modifier(Modifier::BOLD);
            }
            let mut missing_style = Style::default().fg(crate::colors::warning());
            if selected {
                missing_style = missing_style.bg(crate::colors::selection()).add_modifier(Modifier::BOLD);
            }
            let mut main_spans = vec![
                Span::raw("• "),
                Span::styled(status.name, name_style),
                Span::raw(" — "),
                Span::styled(if *enabled { "enabled" } else { "disabled" }, state_style),
            ];
            if !status.installed {
                main_spans.push(Span::raw("  "));
                main_spans.push(Span::styled("not installed", missing_style));
            }
            if selected {
                main_spans.push(Span::styled("  ", Style::default().bg(crate::colors::selection())));
                let hint = if !status.installed {
                    "(press Enter to install)"
                } else if *enabled {
                    "(press Enter to disable)"
                } else {
                    "(press Enter to enable)"
                };
                main_spans.push(Span::styled(
                    hint,
                    Style::default().fg(crate::colors::text_dim()).bg(crate::colors::selection()),
                ));
            }
            lines.push(Line::from(main_spans));

            let mut desc_style = Style::default().fg(crate::colors::text_dim());
            if selected {
                desc_style = desc_style.bg(crate::colors::selection());
            }
            lines.push(Line::from(vec![
                Span::styled("   ", desc_style),
                Span::styled(status.description, desc_style),
            ]));
            lines.push(Line::from(""));
        }

        lines.push(Line::from(""));
        let close_selected = self.selected_row == max_row;
        let close_style = if close_selected {
            Style::default().bg(crate::colors::selection()).add_modifier(Modifier::BOLD)
        } else {
            Style::default()
        };
        lines.push(Line::from(vec![Span::styled(
            if close_selected { "› Close" } else { "  Close" },
            close_style,
        )]));

        lines.push(Line::from(""));
        lines.push(Line::from(vec![
            Span::styled("↑↓", Style::default().fg(crate::colors::light_blue())),
            Span::raw(" Navigate  "),
            Span::styled("←→/Space", Style::default().fg(crate::colors::success())),
            Span::raw(" Toggle  "),
            Span::styled("Enter", Style::default().fg(crate::colors::success())),
            Span::raw(" Toggle / Install  "),
            Span::styled("Esc", Style::default().fg(crate::colors::error())),
            Span::raw(" Close"),
        ]));

        let paragraph = Paragraph::new(lines)
            .alignment(Alignment::Left)
            .style(Style::default().bg(crate::colors::background()).fg(crate::colors::text()));
        paragraph.render(
            Rect {
                x: inner.x.saturating_add(1),
                y: inner.y,
                width: inner.width.saturating_sub(2),
                height: inner.height,
            },
            buf,
        );
    }
}

pub(crate) fn detect_tools() -> Vec<ToolStatus> {
    vec![
        ToolStatus {
            name: "actionlint",
            description: "Lint GitHub Actions workflows for syntax and logic issues.",
            installed: has("actionlint"),
            install_hint: actionlint_hint(),
        },
        ToolStatus {
            name: "shellcheck",
            description: "Analyze shell scripts for bugs and common pitfalls.",
            installed: has("shellcheck"),
            install_hint: shellcheck_hint(),
        },
        ToolStatus {
            name: "markdownlint",
            description: "Lint Markdown content for style and formatting problems.",
            installed: has("markdownlint"),
            install_hint: markdownlint_hint(),
        },
        ToolStatus {
            name: "hadolint",
            description: "Lint Dockerfiles for best practices and mistakes.",
            installed: has("hadolint"),
            install_hint: hadolint_hint(),
        },
        ToolStatus {
            name: "yamllint",
            description: "Validate YAML files for syntax and style issues.",
            installed: has("yamllint"),
            install_hint: yamllint_hint(),
        },
        ToolStatus {
            name: "cargo-check",
            description: "Run `cargo check` to catch Rust compilation errors quickly.",
            installed: has("cargo"),
            install_hint: cargo_check_hint(),
        },
        ToolStatus {
            name: "shfmt",
            description: "Format shell scripts consistently with shfmt.",
            installed: has("shfmt"),
            install_hint: shfmt_hint(),
        },
        ToolStatus {
            name: "prettier",
            description: "Format web assets (JS/TS/JSON/MD) with Prettier.",
            installed: has("prettier"),
            install_hint: prettier_hint(),
        },
    ]
}

fn which(exe: &str) -> Option<std::path::PathBuf> {
    let name = std::ffi::OsStr::new(exe);
    let paths: Vec<std::path::PathBuf> = std::env::var_os("PATH")
        .map(|paths| std::env::split_paths(&paths).collect())
        .unwrap_or_else(Vec::new);
    for dir in paths {
        let candidate = dir.join(name);
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

fn has(cmd: &str) -> bool {
    which(cmd).is_some()
}

fn is_macos() -> bool {
    cfg!(target_os = "macos")
}

pub fn actionlint_hint() -> String {
    if is_macos() && has("brew") {
        return "brew install actionlint".to_string();
    }
    if has("brew") {
        return "brew install actionlint".to_string();
    }
    "See: https://github.com/rhysd/actionlint#installation".to_string()
}

pub fn shellcheck_hint() -> String {
    if is_macos() && has("brew") {
        return "brew install shellcheck".to_string();
    }
    if has("apt-get") {
        return "sudo apt-get update && sudo apt-get install -y shellcheck".to_string();
    }
    if has("dnf") {
        return "sudo dnf install -y ShellCheck".to_string();
    }
    if has("yum") {
        return "sudo yum install -y ShellCheck".to_string();
    }
    if has("brew") {
        return "brew install shellcheck".to_string();
    }
    "https://www.shellcheck.net/".to_string()
}

pub fn markdownlint_hint() -> String {
    if has("npm") {
        return "npm i -g markdownlint-cli2".to_string();
    }
    if is_macos() && has("brew") {
        return "brew install markdownlint-cli2".to_string();
    }
    "npm i -g markdownlint-cli2".to_string()
}

pub fn hadolint_hint() -> String {
    if is_macos() && has("brew") {
        return "brew install hadolint".to_string();
    }
    if has("apt-get") {
        return "sudo apt-get update && sudo apt-get install -y hadolint".to_string();
    }
    if has("dnf") {
        return "sudo dnf install -y hadolint".to_string();
    }
    if has("yum") {
        return "sudo yum install -y hadolint".to_string();
    }
    if has("brew") {
        return "brew install hadolint".to_string();
    }
    "https://github.com/hadolint/hadolint".to_string()
}

pub fn yamllint_hint() -> String {
    if is_macos() && has("brew") {
        return "brew install yamllint".to_string();
    }
    if has("apt-get") {
        return "sudo apt-get update && sudo apt-get install -y yamllint".to_string();
    }
    if has("dnf") {
        return "sudo dnf install -y yamllint".to_string();
    }
    if has("yum") {
        return "sudo yum install -y yamllint".to_string();
    }
    if has("brew") {
        return "brew install yamllint".to_string();
    }
    "https://yamllint.readthedocs.io/".to_string()
}

pub fn cargo_check_hint() -> String {
    if has("cargo") {
        return "cargo check --all-targets".to_string();
    }
    "Install Rust (https://rustup.rs) to enable cargo check".to_string()
}

pub fn shfmt_hint() -> String {
    if is_macos() && has("brew") {
        return "brew install shfmt".to_string();
    }
    if has("apt-get") {
        return "sudo apt-get update && sudo apt-get install -y shfmt".to_string();
    }
    if has("dnf") {
        return "sudo dnf install -y shfmt".to_string();
    }
    if has("yum") {
        return "sudo yum install -y shfmt".to_string();
    }
    if has("brew") {
        return "brew install shfmt".to_string();
    }
    "https://github.com/mvdan/sh".to_string()
}

pub fn prettier_hint() -> String {
    if has("npm") {
        return "npx --yes prettier --write <path>".to_string();
    }
    if is_macos() && has("brew") {
        return "brew install prettier".to_string();
    }
    "npm install --global prettier".to_string()
}
