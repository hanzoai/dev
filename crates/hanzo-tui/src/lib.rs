//! Hanzo-owned presentation; the upstream TUI owns interaction and animation.

pub const PRODUCT_NAME: &str = "Hanzo Dev";
pub const INPUT_PLACEHOLDER: &str = "Ask Hanzo Dev to do anything";
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const STARTUP_TIP: &str = "Use **/mcp** to inspect connected tools. Hanzo MCP provides cloud controls and local development tools.";

/// Keep light terminals readable and dark input bars much subtler than Codex.
pub fn input_background(background: (u8, u8, u8), light: bool) -> (u8, u8, u8) {
    let (target, alpha) = if light { (0.0, 0.04) } else { (255.0, 0.055) };
    let mix = |channel: u8| (channel as f32 * (1.0 - alpha) + target * alpha).round() as u8;
    (mix(background.0), mix(background.1), mix(background.2))
}
