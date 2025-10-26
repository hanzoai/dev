use ratatui::prelude::Color;
use std::time::Duration;

#[derive(Clone, Copy, Debug)]
pub struct GradientSpec {
    pub left: Color,
    pub right: Color,
    pub bias: f32,
}

#[derive(Clone, Copy, Debug)]
pub struct PaletteSpec {
    pub border: Color,
    pub text: Color,
    pub title: Color,
    pub footer: Color,
}

#[derive(Clone, Copy, Debug)]
pub struct RevealConfig {
    pub duration: Duration,
    pub variant: RevealVariant,
}

#[derive(Clone, Copy, Debug)]
pub enum RevealVariant {
    GlitchSweep,
    VertDrift,
    DiagonalPulse,
    ChromaticScan,
    SparkleFade,
    RainbowBloom,
    AuroraBridge,
    PrismRise,
    NeonRoad,
    HorizonRush,
    LightBloom,
}

#[derive(Clone, Copy, Debug)]
pub struct CardTheme {
    pub gradient: GradientSpec,
    pub palette: PaletteSpec,
    pub reveal: Option<RevealConfig>,
}

#[derive(Clone, Copy, Debug)]
pub struct CardThemeDefinition {
    pub name: &'static str,
    pub theme: CardTheme,
}

#[derive(Clone, Copy, Debug)]
pub struct CardPreviewSpec {
    pub name: &'static str,
    pub body: &'static [&'static str],
    pub theme: CardTheme,
}

impl CardThemeDefinition {
    pub fn preview(&self, body: &'static [&'static str]) -> CardPreviewSpec {
        CardPreviewSpec {
            name: self.name,
            body,
            theme: self.theme,
        }
    }
}

fn rgb(r: u8, g: u8, b: u8) -> Color {
    Color::Rgb(r, g, b)
}

fn gradient(left: (u8, u8, u8), right: (u8, u8, u8), bias: f32) -> GradientSpec {
    GradientSpec {
        left: rgb(left.0, left.1, left.2),
        right: rgb(right.0, right.1, right.2),
        bias,
    }
}

fn palette(
    border: (u8, u8, u8),
    text: (u8, u8, u8),
    title: (u8, u8, u8),
    footer: (u8, u8, u8),
) -> PaletteSpec {
    PaletteSpec {
        border: rgb(border.0, border.1, border.2),
        text: rgb(text.0, text.1, text.2),
        title: rgb(title.0, title.1, title.2),
        footer: rgb(footer.0, footer.1, footer.2),
    }
}

fn reveal(duration_ms: u64, variant: RevealVariant) -> Option<RevealConfig> {
    Some(RevealConfig {
        duration: Duration::from_millis(duration_ms),
        variant,
    })
}

pub fn search_dark_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Search Dark",
        theme: CardTheme {
            gradient: gradient((48, 12, 96), (210, 120, 255), -0.05),
            palette: palette((210, 170, 245), (242, 234, 255), (248, 240, 255), (232, 220, 250)),
            reveal: None,
        },
    }
}

pub fn auto_drive_dark_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Auto Drive Dark",
        theme: CardTheme {
            gradient: gradient((50, 70, 200), (190, 220, 255), -0.05),
            palette: palette((180, 210, 248), (232, 242, 254), (240, 248, 255), (220, 234, 248)),
            reveal: reveal(720, RevealVariant::RainbowBloom),
        },
    }
}

pub fn agent_orange_dark_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Agent Orange Dark",
        theme: CardTheme {
            gradient: gradient((150, 60, 20), (235, 120, 50), -0.05),
            palette: palette((226, 168, 120), (248, 230, 216), (254, 240, 228), (236, 212, 200)),
            reveal: None,
        },
    }
}

pub fn browser_dark_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Browser Dark",
        theme: CardTheme {
            gradient: gradient((18, 32, 72), (96, 140, 210), -0.05),
            palette: palette((150, 200, 255), (220, 236, 255), (204, 224, 252), (192, 212, 240)),
            reveal: None,
        },
    }
}

pub fn agent_green_dark_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Agent Green Dark",
        theme: CardTheme {
            gradient: gradient((20, 70, 36), (160, 240, 120), -0.05),
            palette: palette((168, 236, 186), (226, 250, 224), (210, 248, 210), (200, 240, 202)),
            reveal: None,
        },
    }
}

pub fn search_light_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Search Light",
        theme: CardTheme {
            gradient: gradient((248, 228, 255), (216, 190, 255), -0.05),
            palette: palette((82, 48, 164), (20, 12, 54), (30, 18, 72), (36, 22, 86)),
            reveal: None,
        },
    }
}

pub fn auto_drive_light_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Auto Drive Light",
        theme: CardTheme {
            gradient: gradient((216, 238, 255), (160, 196, 232), -0.05),
            palette: palette((32, 96, 140), (12, 34, 52), (18, 48, 70), (24, 60, 86)),
            reveal: reveal(720, RevealVariant::LightBloom),
        },
    }
}

pub fn agent_orange_light_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Agent Orange Light",
        theme: CardTheme {
            gradient: gradient((255, 220, 200), (232, 160, 120), -0.05),
            palette: palette((140, 64, 30), (78, 26, 4), (92, 34, 6), (104, 40, 10)),
            reveal: None,
        },
    }
}

pub fn browser_light_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Browser Light",
        theme: CardTheme {
            gradient: gradient((200, 220, 242), (104, 132, 180), -0.05),
            palette: palette((40, 70, 120), (16, 22, 44), (26, 34, 70), (32, 42, 82)),
            reveal: None,
        },
    }
}

pub fn agent_green_light_theme() -> CardThemeDefinition {
    CardThemeDefinition {
        name: "Agent Green Light",
        theme: CardTheme {
            gradient: gradient((220, 255, 230), (182, 228, 188), -0.05),
            palette: palette((48, 102, 82), (18, 40, 32), (26, 52, 40), (34, 64, 50)),
            reveal: None,
        },
    }
}

pub fn dark_theme_catalog() -> Vec<CardThemeDefinition> {
    vec![
        search_dark_theme(),
        auto_drive_dark_theme(),
        agent_orange_dark_theme(),
        browser_dark_theme(),
        agent_green_dark_theme(),
    ]
}

pub fn light_theme_catalog() -> Vec<CardThemeDefinition> {
    vec![
        search_light_theme(),
        auto_drive_light_theme(),
        agent_orange_light_theme(),
        browser_light_theme(),
        agent_green_light_theme(),
    ]
}

pub fn theme_catalog() -> Vec<CardThemeDefinition> {
    let mut themes = dark_theme_catalog();
    themes.extend(light_theme_catalog());
    themes
}

pub fn auto_drive_theme_catalog() -> Vec<CardThemeDefinition> {
    vec![auto_drive_dark_theme(), auto_drive_light_theme()]
}

pub fn agent_read_only_dark_theme() -> CardThemeDefinition {
    agent_orange_dark_theme()
}

pub fn agent_read_only_light_theme() -> CardThemeDefinition {
    agent_orange_light_theme()
}

pub fn agent_write_dark_theme() -> CardThemeDefinition {
    agent_green_dark_theme()
}

pub fn agent_write_light_theme() -> CardThemeDefinition {
    agent_green_light_theme()
}
