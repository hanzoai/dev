use std::env;

use ratatui::style::{Modifier, Style};
use ratatui::widgets::BorderType;

use crate::colors;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AutoDriveVariant {
    Sentinel,
    Whisper,
    Beacon,
    Horizon,
    Pulse,
}

impl AutoDriveVariant {
    const ALL: [Self; 5] = [
        Self::Sentinel,
        Self::Whisper,
        Self::Beacon,
        Self::Horizon,
        Self::Pulse,
    ];

    pub fn default() -> Self {
        Self::Sentinel
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Sentinel => "Sentinel",
            Self::Whisper => "Whisper",
            Self::Beacon => "Beacon",
            Self::Horizon => "Horizon",
            Self::Pulse => "Pulse",
        }
    }

    pub fn index(self) -> usize {
        match self {
            Self::Sentinel => 0,
            Self::Whisper => 1,
            Self::Beacon => 2,
            Self::Horizon => 3,
            Self::Pulse => 4,
        }
    }

    pub fn from_index(index: usize) -> Self {
        let clamped = index % Self::ALL.len();
        Self::ALL[clamped]
    }

    pub fn from_env() -> Self {
        env::var("CODEX_AUTO_DRIVE_VARIANT")
            .ok()
            .and_then(|raw| raw.trim().parse::<usize>().ok())
            .map(Self::from_index)
            .unwrap_or_else(Self::default)
    }

    pub fn next(self) -> Self {
        let idx = self.index();
        let next = (idx + 1) % Self::ALL.len();
        Self::from_index(next)
    }

    pub fn style(self) -> AutoDriveStyle {
        match self {
            Self::Sentinel => sentinel_style(),
            Self::Whisper => whisper_style(),
            Self::Beacon => beacon_style(),
            Self::Horizon => horizon_style(),
            Self::Pulse => pulse_style(),
        }
    }
}

#[derive(Clone)]
pub struct AutoDriveStyle {
    pub variant: AutoDriveVariant,
    pub frame: FrameStyle,
    pub button: ButtonStyle,
    pub composer: ComposerStyle,
    pub footer_separator: &'static str,
    pub summary_style: Style,
}

#[derive(Clone)]
pub struct FrameStyle {
    pub title_prefix: &'static str,
    pub title_text: &'static str,
    pub title_suffix: &'static str,
    pub title_style: Style,
    pub border_style: Style,
    pub border_type: BorderType,
    pub accent: Option<AccentStyle>,
}

#[derive(Clone)]
pub struct AccentStyle {
    pub symbol: char,
    pub style: Style,
    pub width: u16,
}

#[derive(Clone)]
pub struct ButtonStyle {
    pub glyphs: ButtonGlyphs,
    pub enabled_style: Style,
    pub disabled_style: Style,
}

#[derive(Clone, Copy)]
pub struct ButtonGlyphs {
    pub top_left: char,
    pub top_right: char,
    pub bottom_left: char,
    pub bottom_right: char,
    pub horizontal: char,
    pub vertical: char,
}

impl ButtonGlyphs {
    pub const fn heavy() -> Self {
        Self {
            top_left: '╭',
            top_right: '╮',
            bottom_left: '╰',
            bottom_right: '╯',
            horizontal: '─',
            vertical: '│',
        }
    }

    pub const fn light() -> Self {
        Self {
            top_left: '+',
            top_right: '+',
            bottom_left: '+',
            bottom_right: '+',
            horizontal: '-',
            vertical: '|',
        }
    }

    pub const fn bold() -> Self {
        Self {
            top_left: '┏',
            top_right: '┓',
            bottom_left: '┗',
            bottom_right: '┛',
            horizontal: '━',
            vertical: '┃',
        }
    }

    pub const fn double() -> Self {
        Self {
            top_left: '╔',
            top_right: '╗',
            bottom_left: '╚',
            bottom_right: '╝',
            horizontal: '═',
            vertical: '║',
        }
    }
}

#[derive(Clone)]
pub struct ComposerStyle {
    pub border_style: Style,
    pub border_type: BorderType,
    pub background_style: Style,
    pub auto_title_prefix: &'static str,
    pub auto_title_suffix: &'static str,
    pub goal_title_prefix: &'static str,
    pub goal_title_suffix: &'static str,
    pub title_style: Style,
}

fn sentinel_style() -> AutoDriveStyle {
    let primary = colors::primary();
    AutoDriveStyle {
        variant: AutoDriveVariant::Sentinel,
        frame: FrameStyle {
            title_prefix: " ▶ ",
            title_text: "Auto Drive",
            title_suffix: "",
            title_style: Style::default()
                .fg(colors::text())
                .add_modifier(Modifier::BOLD),
            border_style: Style::default()
                .fg(primary)
                .add_modifier(Modifier::BOLD),
            border_type: BorderType::Rounded,
            accent: None,
        },
        button: ButtonStyle {
            glyphs: ButtonGlyphs::heavy(),
            enabled_style: Style::default()
                .fg(primary)
                .add_modifier(Modifier::BOLD),
            disabled_style: Style::default().fg(colors::text_dim()),
        },
        composer: ComposerStyle {
            border_style: Style::default().fg(primary),
            border_type: BorderType::Rounded,
            background_style: Style::default().bg(colors::background()),
            auto_title_prefix: " ▶ ",
            auto_title_suffix: " ",
            goal_title_prefix: " ▶ Goal ",
            goal_title_suffix: " ",
            title_style: Style::default()
                .fg(primary)
                .add_modifier(Modifier::BOLD),
        },
        footer_separator: "  •  ",
        summary_style: Style::default()
            .fg(primary)
            .add_modifier(Modifier::BOLD),
    }
}

fn whisper_style() -> AutoDriveStyle {
    let border = colors::border_dim();
    AutoDriveStyle {
        variant: AutoDriveVariant::Whisper,
        frame: FrameStyle {
            title_prefix: " ∙ ",
            title_text: "Auto Drive",
            title_suffix: " ∙",
            title_style: Style::default()
                .fg(colors::text_dim())
                .add_modifier(Modifier::ITALIC),
            border_style: Style::default().fg(border),
            border_type: BorderType::Plain,
            accent: None,
        },
        button: ButtonStyle {
            glyphs: ButtonGlyphs::light(),
            enabled_style: Style::default().fg(colors::text_dim()),
            disabled_style: Style::default()
                .fg(colors::text_dim())
                .add_modifier(Modifier::DIM),
        },
        composer: ComposerStyle {
            border_style: Style::default().fg(border),
            border_type: BorderType::Plain,
            background_style: Style::default().bg(colors::background()),
            auto_title_prefix: " ∙ ",
            auto_title_suffix: " ∙",
            goal_title_prefix: " ∙ Goal ",
            goal_title_suffix: " ∙",
            title_style: Style::default()
                .fg(colors::text_dim())
                .add_modifier(Modifier::ITALIC),
        },
        footer_separator: "  ∙  ",
        summary_style: Style::default()
            .fg(colors::text_dim())
            .add_modifier(Modifier::ITALIC),
    }
}

fn beacon_style() -> AutoDriveStyle {
    AutoDriveStyle {
        variant: AutoDriveVariant::Beacon,
        frame: FrameStyle {
            title_prefix: "",
            title_text: "Auto Drive",
            title_suffix: "",
            title_style: Style::default()
                .fg(colors::keyword())
                .add_modifier(Modifier::BOLD),
            border_style: Style::default().fg(colors::border()),
            border_type: BorderType::Plain,
            accent: Some(AccentStyle {
                symbol: '█',
                style: Style::default()
                    .fg(colors::primary())
                    .add_modifier(Modifier::BOLD),
                width: 1,
            }),
        },
        button: ButtonStyle {
            glyphs: ButtonGlyphs::heavy(),
            enabled_style: Style::default()
                .fg(colors::warning())
                .add_modifier(Modifier::BOLD),
            disabled_style: Style::default().fg(colors::text_dim()),
        },
        composer: ComposerStyle {
            border_style: Style::default()
                .fg(colors::primary())
                .add_modifier(Modifier::BOLD),
            border_type: BorderType::Thick,
            background_style: Style::default().bg(colors::background()),
            auto_title_prefix: " █ ",
            auto_title_suffix: " ",
            goal_title_prefix: " █ Goal ",
            goal_title_suffix: " ",
            title_style: Style::default()
                .fg(colors::keyword())
                .add_modifier(Modifier::BOLD),
        },
        footer_separator: "  |  ",
        summary_style: Style::default()
            .fg(colors::warning())
            .add_modifier(Modifier::BOLD),
    }
}

fn horizon_style() -> AutoDriveStyle {
    let info = colors::info();
    AutoDriveStyle {
        variant: AutoDriveVariant::Horizon,
        frame: FrameStyle {
            title_prefix: "━━ ",
            title_text: "Auto Drive",
            title_suffix: " ━━",
            title_style: Style::default()
                .fg(info)
                .add_modifier(Modifier::BOLD),
            border_style: Style::default()
                .fg(info)
                .add_modifier(Modifier::BOLD),
            border_type: BorderType::Double,
            accent: None,
        },
        button: ButtonStyle {
            glyphs: ButtonGlyphs::double(),
            enabled_style: Style::default()
                .fg(info)
                .add_modifier(Modifier::BOLD),
            disabled_style: Style::default().fg(colors::text_dim()),
        },
        composer: ComposerStyle {
            border_style: Style::default()
                .fg(info)
                .add_modifier(Modifier::BOLD),
            border_type: BorderType::Double,
            background_style: Style::default().bg(colors::assistant_bg()),
            auto_title_prefix: " ═ ",
            auto_title_suffix: " ═",
            goal_title_prefix: " ═ Goal ",
            goal_title_suffix: " ═",
            title_style: Style::default()
                .fg(info)
                .add_modifier(Modifier::BOLD),
        },
        footer_separator: "  ≡  ",
        summary_style: Style::default()
            .fg(info)
            .add_modifier(Modifier::BOLD),
    }
}

fn pulse_style() -> AutoDriveStyle {
    let success = colors::success();
    AutoDriveStyle {
        variant: AutoDriveVariant::Pulse,
        frame: FrameStyle {
            title_prefix: " ◆ ",
            title_text: "Auto Drive",
            title_suffix: " ◆",
            title_style: Style::default()
                .fg(success)
                .add_modifier(Modifier::BOLD),
            border_style: Style::default()
                .fg(success)
                .add_modifier(Modifier::BOLD),
            border_type: BorderType::Thick,
            accent: None,
        },
        button: ButtonStyle {
            glyphs: ButtonGlyphs::bold(),
            enabled_style: Style::default()
                .fg(success)
                .add_modifier(Modifier::BOLD),
            disabled_style: Style::default().fg(colors::text_dim()),
        },
        composer: ComposerStyle {
            border_style: Style::default()
                .fg(success)
                .add_modifier(Modifier::BOLD),
            border_type: BorderType::Rounded,
            background_style: Style::default().bg(colors::background()),
            auto_title_prefix: " ◆ ",
            auto_title_suffix: " ◆",
            goal_title_prefix: " ◆ Goal ",
            goal_title_suffix: " ◆",
            title_style: Style::default()
                .fg(success)
                .add_modifier(Modifier::BOLD),
        },
        footer_separator: "  ✶  ",
        summary_style: Style::default()
            .fg(success)
            .add_modifier(Modifier::BOLD),
    }
}
