use std::io::Result;
use std::io::Stdout;
use std::io::stdout;
use std::io::BufWriter;

use dev_core::config::Config;
use crossterm::cursor::MoveTo;
use crossterm::event::DisableBracketedPaste;
use crossterm::event::DisableMouseCapture;
use crossterm::event::DisableFocusChange;
use crossterm::event::EnableBracketedPaste;
use crossterm::event::EnableFocusChange;
use crossterm::event::KeyboardEnhancementFlags;
use crossterm::event::PopKeyboardEnhancementFlags;
use crossterm::event::PushKeyboardEnhancementFlags;
use crossterm::style::SetColors;
use crossterm::style::{Color as CtColor, SetBackgroundColor, SetForegroundColor};
use crossterm::style::Print;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;
use ratatui::crossterm::execute;
use ratatui::crossterm::terminal::disable_raw_mode;
use ratatui::crossterm::terminal::enable_raw_mode;
use ratatui_image::picker::Picker;

/// A type alias for the terminal type used in this application
pub type Tui = Terminal<CrosstermBackend<BufWriter<Stdout>>>;

/// Terminal information queried at startup
#[derive(Clone)]
pub struct TerminalInfo {
    /// The image picker with detected capabilities
    pub picker: Option<Picker>,
    /// Measured font size (width, height) in pixels
    pub font_size: (u16, u16),
}

impl std::fmt::Debug for TerminalInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TerminalInfo")
            .field("picker", &self.picker.is_some())
            .field("font_size", &self.font_size)
            .finish()
    }
}

/// Initialize the terminal (full screen mode with alternate screen)
pub fn init(config: &Config) -> Result<(Tui, TerminalInfo)> {
    // Initialize the theme based on config
    crate::theme::init_theme(&config.tui.theme);
    // Initialize syntax highlighting preference from config
    crate::syntax_highlight::init_highlight_from_config(&config.tui.highlight);

    execute!(stdout(), EnableBracketedPaste)?;
    // Enable focus change events so we can detect when the terminal window/tab
    // regains focus and proactively repaint the UI (helps terminals that clear
    // their alt‑screen buffer while unfocused).
    let _ = execute!(stdout(), EnableFocusChange);

    // Enter alternate screen mode for full screen TUI
    execute!(stdout(), crossterm::terminal::EnterAlternateScreen)?;

    // Query terminal capabilities and font size after entering alternate screen
    // but before enabling raw mode
    let terminal_info = query_terminal_info();

    enable_raw_mode()?;
    // Enable keyboard enhancement flags so modifiers for keys like Enter are disambiguated.
    // chat_composer.rs is using a keyboard event listener to enter for any modified keys
    // to create a new line that require this.
    // Some terminals (notably legacy Windows consoles) do not support
    // keyboard enhancement flags. Attempt to enable them, but continue
    // gracefully if unsupported.
    let _ = execute!(
        stdout(),
        PushKeyboardEnhancementFlags(
            KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
                | KeyboardEnhancementFlags::REPORT_EVENT_TYPES
                | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS
        )
    );
    set_panic_hook();

    // Clear screen with theme background color
    let theme_bg = crate::colors::background();
    let theme_fg = crate::colors::text();
    execute!(
        stdout(),
        SetColors(crossterm::style::Colors::new(
            theme_fg.into(),
            theme_bg.into()
        )),
        Clear(ClearType::All),
        MoveTo(0, 0),
        crossterm::terminal::SetTitle("Code"),
        crossterm::terminal::EnableLineWrap
    )?;

    // Some terminals (notably macOS Terminal.app with certain profiles)
    // clear to the terminal's default background color instead of the
    // currently set background attribute. Proactively painting the full
    // screen with our theme bg fixes that — but doing so on Windows Terminal
    // has been reported to cause broken colors/animation for some users.
    //
    // Restrict the explicit paint to terminals that benefit from it and skip
    // it on Windows Terminal (TERM_PROGRAM=Windows_Terminal or WT_SESSION set).
    let term_program = std::env::var("TERM_PROGRAM").unwrap_or_default();
    let is_windows_terminal = term_program == "Windows_Terminal" || std::env::var("WT_SESSION").is_ok();
    let should_paint_bg = if term_program == "Apple_Terminal" {
        true
    } else if is_windows_terminal {
        false
    } else {
        // For other terminals, be conservative and skip unless a user opts in
        // via CODE_FORCE_FULL_BG_PAINT=1.
        std::env::var("CODE_FORCE_FULL_BG_PAINT").map(|v| v == "1").unwrap_or(false)
    };

    if should_paint_bg {
        if let Ok((cols, rows)) = crossterm::terminal::size() {
            // Build a single line of spaces once to reduce allocations.
            let blank = " ".repeat(cols as usize);
            // Set explicit fg/bg to the theme's colors while painting.
            execute!(stdout(), SetForegroundColor(CtColor::from(theme_fg)), SetBackgroundColor(CtColor::from(theme_bg)))?;
            for y in 0..rows {
                execute!(stdout(), MoveTo(0, y), Print(&blank))?;
            }
            // Restore cursor to home and keep our colors configured for subsequent drawing.
            // Avoid ResetColor here to prevent some terminals from flashing to their
            // profile default background (e.g., white) between frames.
            execute!(stdout(), MoveTo(0, 0), SetColors(crossterm::style::Colors::new(theme_fg.into(), theme_bg.into())))?;
        }
    }

    // Wrap stdout in a larger BufWriter to reduce syscalls and flushes.
    // A larger buffer significantly helps during heavy scrolling where many cells change.
    let backend = CrosstermBackend::new(BufWriter::with_capacity(512 * 1024, stdout()));
    let tui = Terminal::new(backend)?;
    Ok((tui, terminal_info))
}

/// Query terminal capabilities before entering raw mode
fn query_terminal_info() -> TerminalInfo {
    // Try to query using ratatui_image's picker
    let picker = match Picker::from_query_stdio() {
        Ok(p) => {
            tracing::info!("Successfully queried terminal capabilities via Picker");
            Some(p)
        }
        Err(e) => {
            tracing::warn!("Failed to query terminal via Picker: {}", e);
            None
        }
    };

    // Get font size from picker if available, otherwise fall back to terminal_info query
    let font_size = if let Some(ref p) = picker {
        // The picker has font size information
        let (w, h) = p.font_size();
        tracing::info!("Got font size from Picker: {}x{}", w, h);
        (w, h)
    } else {
        // Fall back to our own terminal query
        crate::terminal_info::get_cell_size_pixels().unwrap_or_else(|| {
            tracing::warn!("Failed to get cell size, using defaults");
            if std::env::var("TERM_PROGRAM").unwrap_or_default() == "iTerm.app" {
                (7, 15)
            } else {
                (8, 16)
            }
        })
    };

    TerminalInfo { picker, font_size }
}

fn set_panic_hook() {
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        let _ = restore(); // ignore any errors as we are already failing
        hook(panic_info);
    }));
}

/// Restore the terminal to its original state
pub fn restore() -> Result<()> {
    // Pop may fail on platforms that didn't support the push; ignore errors.
    let _ = execute!(stdout(), PopKeyboardEnhancementFlags);
    execute!(stdout(), DisableBracketedPaste)?;
    // Best‑effort: disable focus change notifications if supported.
    let _ = execute!(stdout(), DisableFocusChange);
    execute!(stdout(), DisableMouseCapture)?;
    disable_raw_mode()?;
    // Leave alternate screen mode
    execute!(stdout(), crossterm::terminal::LeaveAlternateScreen)?;
    Ok(())
}
