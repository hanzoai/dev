use codex_core::config_types::{ThemeColors, ThemeConfig, ThemeName};
use lazy_static::lazy_static;
use ratatui::style::Color;
use std::sync::RwLock;

lazy_static! {
    static ref CURRENT_THEME: RwLock<Theme> = RwLock::new(Theme::default());
}

/// Represents a complete theme with all colors resolved
#[derive(Debug, Clone)]
pub struct Theme {
    // Primary colors
    pub primary: Color,
    pub secondary: Color,
    pub background: Color,
    pub foreground: Color,
    
    // UI elements
    pub border: Color,
    pub border_focused: Color,
    pub selection: Color,
    pub cursor: Color,
    
    // Status colors
    pub success: Color,
    pub warning: Color,
    pub error: Color,
    pub info: Color,
    
    // Text colors
    pub text: Color,
    pub text_dim: Color,
    pub text_bright: Color,
    
    // Syntax/special colors
    pub keyword: Color,
    pub string: Color,
    pub comment: Color,
    pub function: Color,
    
    // Animation colors
    pub spinner: Color,
    pub progress: Color,
}

impl Default for Theme {
    fn default() -> Self {
        get_predefined_theme(ThemeName::PhotonLight)
    }
}

/// Initialize the global theme from configuration
pub fn init_theme(config: &ThemeConfig) {
    let mut theme = get_predefined_theme(config.name);
    apply_custom_colors(&mut theme, &config.colors);
    
    let mut current = CURRENT_THEME.write().unwrap();
    *current = theme;
}

/// Get the current theme
pub fn current_theme() -> Theme {
    CURRENT_THEME.read().unwrap().clone()
}

/// Switch to a different predefined theme
pub fn switch_theme(theme_name: ThemeName) {
    let theme = get_predefined_theme(theme_name);
    let mut current = CURRENT_THEME.write().unwrap();
    *current = theme;
}

/// Get list of available theme names for display
pub fn available_themes() -> Vec<(&'static str, &'static str)> {
    vec![
        ("carbon-night", "Sleek modern dark theme"),
        ("photon-light", "Clean professional light theme"),
        ("shinobi-dusk", "Japanese-inspired twilight"),
        ("oled-black-pro", "True black for OLED displays"),
        ("amber-terminal", "Retro amber CRT aesthetic"),
        ("aurora-flux", "Northern lights inspired"),
        ("charcoal-rainbow", "High-contrast accessible"),
        ("zen-garden", "Calm and peaceful"),
        ("paper-light-pro", "Premium paper-like light"),
    ]
}

/// Parse a color string (hex or named color)
fn parse_color(color_str: &str) -> Option<Color> {
    if let Some(hex) = color_str.strip_prefix('#') {
        if hex.len() == 6 {
            if let (Ok(r), Ok(g), Ok(b)) = (
                u8::from_str_radix(&hex[0..2], 16),
                u8::from_str_radix(&hex[2..4], 16),
                u8::from_str_radix(&hex[4..6], 16),
            ) {
                return Some(Color::Rgb(r, g, b));
            }
        }
    }
    
    // Named colors
    match color_str.to_lowercase().as_str() {
        "black" => Some(Color::Black),
        "red" => Some(Color::Red),
        "green" => Some(Color::Green),
        "yellow" => Some(Color::Yellow),
        "blue" => Some(Color::Blue),
        "magenta" => Some(Color::Magenta),
        "cyan" => Some(Color::Cyan),
        "gray" | "grey" => Some(Color::Gray),
        "darkgray" | "darkgrey" => Some(Color::DarkGray),
        "lightred" => Some(Color::LightRed),
        "lightgreen" => Some(Color::LightGreen),
        "lightyellow" => Some(Color::LightYellow),
        "lightblue" => Some(Color::LightBlue),
        "lightmagenta" => Some(Color::LightMagenta),
        "lightcyan" => Some(Color::LightCyan),
        "white" => Some(Color::White),
        _ => None,
    }
}

/// Apply custom color overrides to a theme
fn apply_custom_colors(theme: &mut Theme, colors: &ThemeColors) {
    if let Some(ref c) = colors.primary {
        if let Some(color) = parse_color(c) {
            theme.primary = color;
        }
    }
    if let Some(ref c) = colors.secondary {
        if let Some(color) = parse_color(c) {
            theme.secondary = color;
        }
    }
    if let Some(ref c) = colors.background {
        if let Some(color) = parse_color(c) {
            theme.background = color;
        }
    }
    if let Some(ref c) = colors.foreground {
        if let Some(color) = parse_color(c) {
            theme.foreground = color;
        }
    }
    if let Some(ref c) = colors.border {
        if let Some(color) = parse_color(c) {
            theme.border = color;
        }
    }
    if let Some(ref c) = colors.border_focused {
        if let Some(color) = parse_color(c) {
            theme.border_focused = color;
        }
    }
    if let Some(ref c) = colors.selection {
        if let Some(color) = parse_color(c) {
            theme.selection = color;
        }
    }
    if let Some(ref c) = colors.cursor {
        if let Some(color) = parse_color(c) {
            theme.cursor = color;
        }
    }
    if let Some(ref c) = colors.success {
        if let Some(color) = parse_color(c) {
            theme.success = color;
        }
    }
    if let Some(ref c) = colors.warning {
        if let Some(color) = parse_color(c) {
            theme.warning = color;
        }
    }
    if let Some(ref c) = colors.error {
        if let Some(color) = parse_color(c) {
            theme.error = color;
        }
    }
    if let Some(ref c) = colors.info {
        if let Some(color) = parse_color(c) {
            theme.info = color;
        }
    }
    if let Some(ref c) = colors.text {
        if let Some(color) = parse_color(c) {
            theme.text = color;
        }
    }
    if let Some(ref c) = colors.text_dim {
        if let Some(color) = parse_color(c) {
            theme.text_dim = color;
        }
    }
    if let Some(ref c) = colors.text_bright {
        if let Some(color) = parse_color(c) {
            theme.text_bright = color;
        }
    }
    if let Some(ref c) = colors.keyword {
        if let Some(color) = parse_color(c) {
            theme.keyword = color;
        }
    }
    if let Some(ref c) = colors.string {
        if let Some(color) = parse_color(c) {
            theme.string = color;
        }
    }
    if let Some(ref c) = colors.comment {
        if let Some(color) = parse_color(c) {
            theme.comment = color;
        }
    }
    if let Some(ref c) = colors.function {
        if let Some(color) = parse_color(c) {
            theme.function = color;
        }
    }
    if let Some(ref c) = colors.spinner {
        if let Some(color) = parse_color(c) {
            theme.spinner = color;
        }
    }
    if let Some(ref c) = colors.progress {
        if let Some(color) = parse_color(c) {
            theme.progress = color;
        }
    }
}

/// Get a predefined theme by name
fn get_predefined_theme(name: ThemeName) -> Theme {
    match name {
        ThemeName::CarbonNight => Theme {
            // Dark default - sleek modern dark theme
            primary: Color::Rgb(37, 194, 255),      // #25c2ff
            secondary: Color::Rgb(179, 146, 240),    // #b392f0
            background: Color::Rgb(11, 13, 16),      // #0b0d10
            foreground: Color::Rgb(230, 237, 243),   // #e6edf3
            border: Color::Rgb(17, 22, 28),          // #11161c
            border_focused: Color::Rgb(37, 194, 255),
            selection: Color::Rgb(23, 32, 42),       // #17202a
            cursor: Color::Rgb(230, 237, 243),
            success: Color::Rgb(63, 185, 80),        // #3fb950
            warning: Color::Rgb(210, 153, 34),       // #d29922
            error: Color::Rgb(248, 81, 73),          // #f85149
            info: Color::Rgb(121, 192, 255),         // #79c0ff
            text: Color::Rgb(230, 237, 243),
            text_dim: Color::Rgb(139, 148, 158),     // #8b949e
            text_bright: Color::White,
            keyword: Color::Rgb(179, 146, 240),
            string: Color::Rgb(165, 214, 255),       // #a5d6ff
            comment: Color::Rgb(110, 118, 129),      // #6e7681
            function: Color::Rgb(126, 231, 135),     // #7ee787
            spinner: Color::Rgb(59, 67, 79),         // #3b434f
            progress: Color::Rgb(37, 194, 255),
        },
        
        ThemeName::PhotonLight => Theme {
            // Light default - clean professional light theme
            primary: Color::Rgb(0, 95, 204),         // #005FCC
            secondary: Color::Rgb(110, 89, 203),      // #6E59CB
            background: Color::Rgb(250, 250, 250),   // #FAFAFA
            foreground: Color::Rgb(31, 35, 40),      // #1F2328
            border: Color::Rgb(208, 215, 222),       // #D0D7DE
            border_focused: Color::Rgb(0, 95, 204),
            selection: Color::Rgb(234, 242, 255),    // #EAF2FF
            cursor: Color::Rgb(31, 35, 40),
            success: Color::Rgb(26, 127, 55),        // #1A7F37
            warning: Color::Rgb(154, 103, 0),        // #9A6700
            error: Color::Rgb(207, 34, 46),          // #CF222E
            info: Color::Rgb(9, 105, 218),           // #0969DA
            text: Color::Rgb(31, 35, 40),
            text_dim: Color::Rgb(102, 112, 133),     // #667085
            text_bright: Color::Black,
            keyword: Color::Rgb(110, 89, 203),
            string: Color::Rgb(11, 125, 105),        // #0B7D69
            comment: Color::Rgb(100, 115, 132),      // #647384
            function: Color::Rgb(0, 95, 204),
            spinner: Color::Rgb(156, 163, 175),      // #9CA3AF
            progress: Color::Rgb(0, 95, 204),
        },
        
        ThemeName::ShinobiDusk => Theme {
            // Japanese-inspired twilight theme
            primary: Color::Rgb(122, 162, 247),      // #7aa2f7
            secondary: Color::Rgb(158, 206, 106),    // #9ece6a
            background: Color::Rgb(15, 20, 25),      // #0f1419
            foreground: Color::Rgb(192, 202, 245),   // #c0caf5
            border: Color::Rgb(27, 35, 48),          // #1b2330
            border_focused: Color::Rgb(122, 162, 247),
            selection: Color::Rgb(26, 33, 48),       // #1a2130
            cursor: Color::Rgb(192, 202, 245),
            success: Color::Rgb(158, 206, 106),
            warning: Color::Rgb(224, 175, 104),      // #e0af68
            error: Color::Rgb(247, 118, 142),        // #f7768e
            info: Color::Rgb(125, 207, 255),         // #7dcfff
            text: Color::Rgb(192, 202, 245),
            text_dim: Color::Rgb(122, 136, 160),     // #7a88a0
            text_bright: Color::White,
            keyword: Color::Rgb(187, 154, 247),      // #bb9af7
            string: Color::Rgb(158, 206, 106),
            comment: Color::Rgb(91, 100, 121),       // #5b6479
            function: Color::Rgb(122, 162, 247),
            spinner: Color::Rgb(42, 49, 64),         // #2a3140
            progress: Color::Rgb(122, 162, 247),
        },
        
        ThemeName::OledBlackPro => Theme {
            // True black for OLED displays with vibrant accents
            primary: Color::Rgb(0, 209, 255),        // #00d1ff
            secondary: Color::Rgb(255, 116, 208),    // #ff74d0
            background: Color::Black,
            foreground: Color::Rgb(218, 218, 218),   // #dadada
            border: Color::Rgb(26, 26, 26),          // #1a1a1a
            border_focused: Color::Rgb(0, 209, 255),
            selection: Color::Rgb(13, 13, 13),       // #0d0d0d
            cursor: Color::Rgb(218, 218, 218),
            success: Color::Rgb(33, 243, 114),       // #21f372
            warning: Color::Rgb(255, 209, 102),      // #ffd166
            error: Color::Rgb(255, 59, 48),          // #ff3b30
            info: Color::Rgb(37, 194, 255),          // #25c2ff
            text: Color::Rgb(208, 208, 208),         // #d0d0d0
            text_dim: Color::Rgb(128, 128, 128),     // #808080
            text_bright: Color::White,
            keyword: Color::Rgb(255, 116, 208),
            string: Color::Rgb(186, 255, 128),       // #baff80
            comment: Color::Rgb(102, 102, 102),      // #666666
            function: Color::Rgb(37, 194, 255),
            spinner: Color::Rgb(45, 45, 45),         // #2d2d2d
            progress: Color::Rgb(0, 209, 255),
        },
        
        ThemeName::AmberTerminal => Theme {
            // Retro amber CRT monitor aesthetic
            primary: Color::Rgb(255, 176, 0),        // #ffb000
            secondary: Color::Rgb(255, 209, 138),    // #ffd18a
            background: Color::Rgb(12, 12, 8),       // #0c0c08
            foreground: Color::Rgb(255, 209, 138),
            border: Color::Rgb(48, 42, 26),          // #302a1a
            border_focused: Color::Rgb(255, 176, 0),
            selection: Color::Rgb(26, 20, 8),        // #1a1408
            cursor: Color::Rgb(255, 209, 138),
            success: Color::Rgb(255, 207, 51),       // #ffcf33
            warning: Color::Rgb(255, 158, 0),        // #ff9e00
            error: Color::Rgb(255, 94, 58),          // #ff5e3a
            info: Color::Rgb(255, 184, 77),          // #ffb84d
            text: Color::Rgb(255, 209, 138),
            text_dim: Color::Rgb(163, 131, 77),      // #a3834d
            text_bright: Color::Rgb(255, 241, 194),  // #fff1c2
            keyword: Color::Rgb(255, 193, 77),       // #ffc14d
            string: Color::Rgb(255, 224, 138),       // #ffe08a
            comment: Color::Rgb(156, 124, 63),       // #9c7c3f
            function: Color::Rgb(255, 176, 0),
            spinner: Color::Rgb(58, 45, 23),         // #3a2d17
            progress: Color::Rgb(255, 176, 0),
        },
        
        ThemeName::AuroraFlux => Theme {
            // Northern lights inspired with cool tones
            primary: Color::Rgb(142, 202, 255),      // #8ecaff
            secondary: Color::Rgb(158, 228, 147),    // #9ee493
            background: Color::Rgb(11, 16, 32),      // #0b1020
            foreground: Color::Rgb(230, 241, 255),   // #e6f1ff
            border: Color::Rgb(18, 26, 46),          // #121a2e
            border_focused: Color::Rgb(142, 202, 255),
            selection: Color::Rgb(19, 26, 44),       // #131a2c
            cursor: Color::Rgb(230, 241, 255),
            success: Color::Rgb(158, 228, 147),
            warning: Color::Rgb(255, 212, 121),      // #ffd479
            error: Color::Rgb(255, 107, 129),        // #ff6b81
            info: Color::Rgb(142, 202, 255),
            text: Color::Rgb(217, 230, 255),         // #d9e6ff
            text_dim: Color::Rgb(127, 140, 168),     // #7f8ca8
            text_bright: Color::White,
            keyword: Color::Rgb(194, 153, 255),      // #c299ff
            string: Color::Rgb(160, 255, 179),       // #a0ffb3
            comment: Color::Rgb(95, 106, 130),       // #5f6a82
            function: Color::Rgb(142, 202, 255),
            spinner: Color::Rgb(37, 48, 74),         // #25304a
            progress: Color::Rgb(142, 202, 255),
        },
        
        ThemeName::CharcoalRainbow => Theme {
            // Accessible high-contrast with rainbow accents
            primary: Color::Rgb(26, 209, 255),       // #1ad1ff
            secondary: Color::Rgb(255, 138, 0),      // #ff8a00
            background: Color::Rgb(18, 18, 18),      // #121212
            foreground: Color::Rgb(232, 232, 232),   // #e8e8e8
            border: Color::Rgb(31, 31, 31),          // #1f1f1f
            border_focused: Color::Rgb(26, 209, 255),
            selection: Color::Rgb(26, 26, 26),       // #1a1a1a
            cursor: Color::Rgb(232, 232, 232),
            success: Color::Rgb(0, 194, 168),        // #00c2a8
            warning: Color::Rgb(255, 160, 0),        // #ffa000
            error: Color::Rgb(255, 77, 109),         // #ff4d6d
            info: Color::Rgb(26, 209, 255),
            text: Color::Rgb(232, 232, 232),
            text_dim: Color::Rgb(154, 154, 154),     // #9a9a9a
            text_bright: Color::White,
            keyword: Color::Rgb(255, 138, 0),
            string: Color::Rgb(0, 229, 255),         // #00e5ff
            comment: Color::Rgb(108, 108, 108),      // #6c6c6c
            function: Color::Rgb(179, 136, 255),     // #b388ff
            spinner: Color::Rgb(42, 42, 42),         // #2a2a2a
            progress: Color::Rgb(26, 209, 255),
        },
        
        ThemeName::ZenGarden => Theme {
            // Calm, peaceful theme with mint and lavender
            primary: Color::Rgb(148, 226, 213),      // #94e2d5
            secondary: Color::Rgb(242, 205, 205),    // #f2cdcd
            background: Color::Rgb(16, 20, 23),      // #101417
            foreground: Color::Rgb(220, 227, 234),   // #dce3ea
            border: Color::Rgb(26, 33, 38),          // #1a2126
            border_focused: Color::Rgb(148, 226, 213),
            selection: Color::Rgb(23, 32, 38),       // #172026
            cursor: Color::Rgb(220, 227, 234),
            success: Color::Rgb(166, 227, 161),      // #a6e3a1
            warning: Color::Rgb(249, 226, 175),      // #f9e2af
            error: Color::Rgb(243, 139, 168),        // #f38ba8
            info: Color::Rgb(137, 220, 235),         // #89dceb
            text: Color::Rgb(220, 227, 234),
            text_dim: Color::Rgb(139, 155, 170),     // #8b9baa
            text_bright: Color::White,
            keyword: Color::Rgb(203, 166, 247),      // #cba6f7
            string: Color::Rgb(166, 227, 161),
            comment: Color::Rgb(108, 122, 136),      // #6c7a88
            function: Color::Rgb(137, 220, 235),
            spinner: Color::Rgb(37, 49, 58),         // #25313a
            progress: Color::Rgb(148, 226, 213),
        },
        
        ThemeName::PaperLightPro => Theme {
            // Premium paper-like light theme
            primary: Color::Rgb(0, 95, 204),         // #005fcc
            secondary: Color::Rgb(111, 66, 193),      // #6f42c1
            background: Color::Rgb(247, 247, 245),   // #f7f7f5
            foreground: Color::Rgb(27, 31, 35),      // #1b1f23
            border: Color::Rgb(208, 215, 222),       // #d0d7de
            border_focused: Color::Rgb(0, 95, 204),
            selection: Color::Rgb(231, 237, 243),    // #e7edf3
            cursor: Color::Rgb(27, 31, 35),
            success: Color::Rgb(26, 127, 55),        // #1a7f37
            warning: Color::Rgb(154, 103, 0),        // #9a6700
            error: Color::Rgb(207, 34, 46),          // #cf222e
            info: Color::Rgb(9, 105, 218),           // #0969da
            text: Color::Rgb(36, 41, 47),            // #24292f
            text_dim: Color::Rgb(87, 96, 106),       // #57606a
            text_bright: Color::Black,
            keyword: Color::Rgb(111, 66, 193),
            string: Color::Rgb(11, 125, 105),        // #0b7d69
            comment: Color::Rgb(140, 149, 159),      // #8c959f
            function: Color::Rgb(0, 95, 204),
            spinner: Color::Rgb(140, 149, 159),
            progress: Color::Rgb(0, 95, 204),
        },
        
        ThemeName::Custom => {
            // Use CarbonNight (dark default) as base for custom
            get_predefined_theme(ThemeName::CarbonNight)
        }
    }
}