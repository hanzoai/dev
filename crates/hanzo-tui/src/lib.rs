//! Hanzo-owned presentation; the upstream TUI owns interaction and animation.

pub mod account;
pub mod model;

pub const PRODUCT_NAME: &str = "Hanzo Dev";
pub const INPUT_PLACEHOLDER: &str = "Ask Hanzo Dev to do anything";
/// Dev checks no one else's releases. The check upstream makes compares this build
/// with another product's latest release and offers that product's installer.
pub const UPDATE_CHECK: bool = false;
/// Dev runs its server inside the process. Upstream's shared background server
/// starts from an installer's package directory, which Dev does not ship.
pub const BACKGROUND_SERVER: bool = false;
/// Dev draws in the terminal's own foreground and background: no hue and no grey,
/// with weight and reverse video for emphasis.
pub const MONOCHROME: bool = true;
/// One line under the input: the status line takes the row the shortcut hint would.
pub const SEPARATE_STATUS_LINE: bool = false;
pub const STARTUP_TIP: &str = "Use **/mcp** to inspect connected tools. Hanzo MCP provides cloud controls and local development tools.";

/// Keep light terminals readable and dark input bars much subtler than the upstream default.
pub fn input_background(background: (u8, u8, u8), light: bool) -> (u8, u8, u8) {
    let (target, alpha) = if light { (0.0, 0.04) } else { (255.0, 0.055) };
    let mix = |channel: u8| (channel as f32 * (1.0 - alpha) + target * alpha).round() as u8;
    (mix(background.0), mix(background.1), mix(background.2))
}
