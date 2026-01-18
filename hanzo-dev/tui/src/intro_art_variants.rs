//! Multiple intro art variants for Hanzo Dev
//! Randomly selected on each launch for variety

use once_cell::sync::Lazy;
use rand::Rng;

/// Art variant style
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ArtVariant {
    Classic,      // Clean HANZO DEV
    Matrix,       // Matrix-style hex rain
    Cyberpunk,    // Neon cyberpunk style
    Minimal,      // Minimal square H logo
    Glitch,       // Glitchy effect
    Robot,        // Robot/AI themed
    Terminal,     // Old-school terminal
    Gradient,     // Gradient blocks
    Circuit,      // Circuit board style
    Hologram,     // Holographic style
}

impl ArtVariant {
    pub fn random() -> Self {
        let mut rng = rand::rng();
        match rng.random_range(0..10) {
            0 => ArtVariant::Classic,
            1 => ArtVariant::Matrix,
            2 => ArtVariant::Cyberpunk,
            3 => ArtVariant::Minimal,
            4 => ArtVariant::Glitch,
            5 => ArtVariant::Robot,
            6 => ArtVariant::Terminal,
            7 => ArtVariant::Gradient,
            8 => ArtVariant::Circuit,
            _ => ArtVariant::Hologram,
        }
    }
}

/// Selected variant for this session (randomized once at startup)
pub static SESSION_VARIANT: Lazy<ArtVariant> = Lazy::new(ArtVariant::random);

/// Square H logo (Hanzo brand)
pub const SQUARE_H_LOGO: [&str; 7] = [
    "┌─────────────┐",
    "│ ██╗   ██╗   │",
    "│ ██║   ██║   │",
    "│ ███████║   │",
    "│ ██╔══██║   │",
    "│ ██║   ██║   │",
    "└─────────────┘",
];

/// Classic clean HANZO DEV
pub fn classic_art(version: &str) -> Vec<String> {
    vec![
        "".to_string(),
        "   ██╗  ██╗ █████╗ ███╗   ██╗███████╗ ██████╗ ".to_string(),
        "   ██║  ██║██╔══██╗████╗  ██║╚══███╔╝██╔═══██╗".to_string(),
        "   ███████║███████║██╔██╗ ██║  ███╔╝ ██║   ██║".to_string(),
        "   ██╔══██║██╔══██║██║╚██╗██║ ███╔╝  ██║   ██║".to_string(),
        "   ██║  ██║██║  ██║██║ ╚████║███████╗╚██████╔╝".to_string(),
        "   ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝╚══════╝ ╚═════╝ ".to_string(),
        "".to_string(),
        "   ██████╗ ███████╗██╗   ██╗".to_string(),
        "   ██╔══██╗██╔════╝██║   ██║".to_string(),
        "   ██║  ██║█████╗  ██║   ██║".to_string(),
        "   ██║  ██║██╔══╝  ╚██╗ ██╔╝".to_string(),
        "   ██████╔╝███████╗ ╚████╔╝ ".to_string(),
        format!("   ╚═════╝ ╚══════╝  ╚═══╝   {}", version),
        "".to_string(),
    ]
}

/// Matrix-style with hex characters
pub fn matrix_art(version: &str) -> Vec<String> {
    vec![
        "   ░▒▓ 48 41 4E 5A 4F ▓▒░  ░▒▓ 44 45 56 ▓▒░".to_string(),
        "".to_string(),
        "   ╦ ╦╔═╗╔╗╔╔═╗╔═╗  ╔╦╗╔═╗╦  ╦".to_string(),
        "   ╠═╣╠═╣║║║╔═╝║ ║   ║║║╣ ╚╗╔╝".to_string(),
        "   ╩ ╩╩ ╩╝╚╝╚═╝╚═╝  ═╩╝╚═╝ ╚╝ ".to_string(),
        "".to_string(),
        "   ┌──────────────────────────────────────┐".to_string(),
        "   │ 01001000 01000001 01001110 01011010  │".to_string(),
        "   │ 01001111 00100000 01000100 01000101  │".to_string(),
        "   │ 01010110 00100000 ████████████████  │".to_string(),
        "   └──────────────────────────────────────┘".to_string(),
        format!("                                    {}", version),
        "".to_string(),
    ]
}

/// Cyberpunk neon style
pub fn cyberpunk_art(version: &str) -> Vec<String> {
    vec![
        "   ╔═══════════════════════════════════════╗".to_string(),
        "   ║  ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄  ║".to_string(),
        "   ║  █  HANZO  █████████  DEV  █  ║".to_string(),
        "   ║  ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀  ║".to_string(),
        "   ╠═══════════════════════════════════════╣".to_string(),
        "   ║  ◢◤ AI-POWERED DEVELOPMENT ◥◣  ║".to_string(),
        "   ║  ░▒▓█ NEURAL NETWORK ACTIVE █▓▒░  ║".to_string(),
        "   ╚═══════════════════════════════════════╝".to_string(),
        format!("   [ SYSTEM {} ONLINE ]", version),
        "".to_string(),
    ]
}

/// Minimal square H logo style
pub fn minimal_art(version: &str) -> Vec<String> {
    vec![
        "".to_string(),
        "      ┏━━━━━━━━━━━━━━━━━┓".to_string(),
        "      ┃  █   █         ┃".to_string(),
        "      ┃  █   █  HANZO  ┃".to_string(),
        "      ┃  █████         ┃".to_string(),
        "      ┃  █   █   DEV   ┃".to_string(),
        "      ┃  █   █         ┃".to_string(),
        "      ┗━━━━━━━━━━━━━━━━━┛".to_string(),
        format!("           {}", version),
        "".to_string(),
    ]
}

/// Glitchy effect
pub fn glitch_art(version: &str) -> Vec<String> {
    vec![
        "   H̷̢A̴͜N̸̡Z̷͠O̴̧ ̵̛D̶̢E̷̛V̴͝".to_string(),
        "".to_string(),
        "   ██╗  ██╗▒▒▒░░ ███╗   ██╗███████╗ ██████╗ ".to_string(),
        "   ██║▓▓██║░░▒▒▒ ████╗  ██║╚══███╔╝██╔═══██╗".to_string(),
        "   █████▒▒║▓▓░░░ ██╔██╗ ██║  ███╔╝ ██║   ██║".to_string(),
        "   ██╔══██║░░▓▓▓ ██║╚██╗██║ ███╔╝  ██║   ██║".to_string(),
        "   ██║  ██║▒▒▒░░ ██║ ╚████║███████╗╚██████╔╝".to_string(),
        "   ╚═╝  ╚═╝      ╚═╝  ╚═══╝╚══════╝ ╚═════╝ ".to_string(),
        "".to_string(),
        "   ▓▓▓ D̶͝E̷̕V̴̛ ░░░ CORRUPTED_REALITY ▒▒▒".to_string(),
        format!("   ░░░ {} ▓▓▓", version),
        "".to_string(),
    ]
}

/// Robot/AI themed
pub fn robot_art(version: &str) -> Vec<String> {
    vec![
        "        ┌───────────┐".to_string(),
        "        │  ◉     ◉  │  HANZO DEV".to_string(),
        "        │    ═══    │  AI Assistant".to_string(),
        "        │  ╔═════╗  │".to_string(),
        "        └──╫─────╫──┘".to_string(),
        "           ║     ║".to_string(),
        "      ┌────╫─────╫────┐".to_string(),
        "      │    ║ ◈◈◈ ║    │".to_string(),
        "      │ ◈◈ ╠═════╣ ◈◈ │".to_string(),
        "      └────╫─────╫────┘".to_string(),
        "           ╨     ╨".to_string(),
        format!("      [ {} ]", version),
        "".to_string(),
    ]
}

/// Old-school terminal style
pub fn terminal_art(version: &str) -> Vec<String> {
    vec![
        "   ╔══════════════════════════════════════════╗".to_string(),
        "   ║  HANZO DEVELOPMENT ENVIRONMENT           ║".to_string(),
        "   ╠══════════════════════════════════════════╣".to_string(),
        "   ║  > INITIALIZING AI SUBSYSTEM...          ║".to_string(),
        "   ║  > LOADING NEURAL NETWORKS... [OK]       ║".to_string(),
        "   ║  > CONNECTING TO BACKEND... [OK]         ║".to_string(),
        "   ║  > READY FOR INPUT                       ║".to_string(),
        "   ║                                          ║".to_string(),
        format!("   ║  VERSION: {:30}  ║", version),
        "   ╚══════════════════════════════════════════╝".to_string(),
        "".to_string(),
    ]
}

/// Gradient blocks style
pub fn gradient_art(version: &str) -> Vec<String> {
    vec![
        "   ░░░░▒▒▒▒▓▓▓▓████████▓▓▓▓▒▒▒▒░░░░".to_string(),
        "   ░░▒▒▓▓██  HANZO DEV  ██▓▓▒▒░░".to_string(),
        "   ░░░░▒▒▒▒▓▓▓▓████████▓▓▓▓▒▒▒▒░░░░".to_string(),
        "".to_string(),
        "   █▓▒░ H ░▒▓█  █▓▒░ A ░▒▓█  █▓▒░ N ░▒▓█".to_string(),
        "   █▓▒░ Z ░▒▓█  █▓▒░ O ░▒▓█".to_string(),
        "".to_string(),
        "   ░▒▓█ D █▓▒░  ░▒▓█ E █▓▒░  ░▒▓█ V █▓▒░".to_string(),
        format!("                              {}", version),
        "".to_string(),
    ]
}

/// Circuit board style
pub fn circuit_art(version: &str) -> Vec<String> {
    vec![
        "   ┌─●──────●───────●──────●─┐".to_string(),
        "   │ ╔═════╗ ╔═════╗ ╔═════╗ │".to_string(),
        "   ●─╢ CPU ╟─╢ GPU ╟─╢ NPU ╟─●".to_string(),
        "   │ ╚══╤══╝ ╚══╤══╝ ╚══╤══╝ │".to_string(),
        "   │    └───────┴───────┘    │".to_string(),
        "   │  ══════════════════════ │".to_string(),
        "   │    H A N Z O   D E V    │".to_string(),
        "   │  ══════════════════════ │".to_string(),
        "   └─●──────●───────●──────●─┘".to_string(),
        format!("          {}", version),
        "".to_string(),
    ]
}

/// Holographic style
pub fn hologram_art(version: &str) -> Vec<String> {
    vec![
        "   ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  ·".to_string(),
        "   ·  ╭──────────────────────╮  ·".to_string(),
        "   ·  │   ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄   │  ·".to_string(),
        "   ·  │   █ HANZO  DEV █   │  ·".to_string(),
        "   ·  │   ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀   │  ·".to_string(),
        "   ·  ╰──────────────────────╯  ·".to_string(),
        "   ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  ·".to_string(),
        "   ╱╲ HOLOGRAPHIC INTERFACE ╱╲".to_string(),
        format!("        {}", version),
        "".to_string(),
    ]
}

/// Get lines for the current session's art variant
pub fn get_session_art(version: &str) -> Vec<String> {
    get_art_for_variant(*SESSION_VARIANT, version)
}

/// Get lines for a specific art variant
pub fn get_art_for_variant(variant: ArtVariant, version: &str) -> Vec<String> {
    match variant {
        ArtVariant::Classic => classic_art(version),
        ArtVariant::Matrix => matrix_art(version),
        ArtVariant::Cyberpunk => cyberpunk_art(version),
        ArtVariant::Minimal => minimal_art(version),
        ArtVariant::Glitch => glitch_art(version),
        ArtVariant::Robot => robot_art(version),
        ArtVariant::Terminal => terminal_art(version),
        ArtVariant::Gradient => gradient_art(version),
        ArtVariant::Circuit => circuit_art(version),
        ArtVariant::Hologram => hologram_art(version),
    }
}

/// Get the height for the current session's art
pub fn get_session_art_height() -> u16 {
    match *SESSION_VARIANT {
        ArtVariant::Classic => 15,
        ArtVariant::Matrix => 13,
        ArtVariant::Cyberpunk => 10,
        ArtVariant::Minimal => 10,
        ArtVariant::Glitch => 12,
        ArtVariant::Robot => 13,
        ArtVariant::Terminal => 11,
        ArtVariant::Gradient => 10,
        ArtVariant::Circuit => 11,
        ArtVariant::Hologram => 10,
    }
}

/// Matrix rain characters for animation
pub const MATRIX_CHARS: &[char] = &[
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F', 'ア', 'イ', 'ウ', 'エ', 'オ',
    'カ', 'キ', 'ク', 'ケ', 'コ', '░', '▒', '▓', '█',
];

/// Get a random matrix character
pub fn random_matrix_char() -> char {
    let mut rng = rand::rng();
    MATRIX_CHARS[rng.random_range(0..MATRIX_CHARS.len())]
}

/// Color gradient for matrix effect (green -> yellow -> white)
pub fn matrix_gradient_color(intensity: f32) -> (u8, u8, u8) {
    let intensity = intensity.clamp(0.0, 1.0);
    if intensity < 0.5 {
        // Green to yellow
        let t = intensity * 2.0;
        let r = (0.0 + t * 255.0) as u8;
        let g = 255;
        let b = 0;
        (r, g, b)
    } else {
        // Yellow to white
        let t = (intensity - 0.5) * 2.0;
        let r = 255;
        let g = 255;
        let b = (0.0 + t * 255.0) as u8;
        (r, g, b)
    }
}
