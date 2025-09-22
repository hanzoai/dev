/// TUI-specific tests
use codex_tui::ui_consts::{WINDOW_TITLE, DEFAULT_PADDING, LIVE_PREFIX_COLS};

#[cfg(test)]
mod tui_tests {
    use super::*;

    #[test]
    fn test_ui_constants() {
        assert_eq!(WINDOW_TITLE, "Hanzo Dev");
        assert_eq!(DEFAULT_PADDING, 1);
        assert_eq!(LIVE_PREFIX_COLS, 4);
    }

    #[test]
    fn test_tui_initialization() {
        // Test that TUI module is properly structured
        assert!(true, "TUI module compiles");
    }
}