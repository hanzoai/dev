use std::io::Result;
use std::io::Stdout;
use std::io::stdout;

use codex_core::config::Config;
use crossterm::cursor::MoveTo;
use crossterm::event::DisableBracketedPaste;
use crossterm::event::EnableBracketedPaste;
use crossterm::event::DisableMouseCapture;
use crossterm::event::EnableMouseCapture;
use crossterm::event::KeyboardEnhancementFlags;
use crossterm::event::PopKeyboardEnhancementFlags;
use crossterm::event::PushKeyboardEnhancementFlags;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use crossterm::style::SetColors;
use ratatui::backend::CrosstermBackend;
use ratatui::crossterm::execute;
use ratatui::crossterm::terminal::disable_raw_mode;
use ratatui::crossterm::terminal::enable_raw_mode;
use ratatui::Terminal;

/// A type alias for the terminal type used in this application
pub type Tui = Terminal<CrosstermBackend<Stdout>>;

/// Initialize the terminal (full screen mode with alternate screen)
pub fn init(config: &Config) -> Result<Tui> {
    // Initialize the theme based on config
    crate::theme::init_theme(&config.tui.theme);
    
    execute!(stdout(), EnableBracketedPaste)?;
    execute!(stdout(), EnableMouseCapture)?;
    
    // Enter alternate screen mode for full screen TUI
    execute!(stdout(), crossterm::terminal::EnterAlternateScreen)?;

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
        SetColors(crossterm::style::Colors::new(theme_fg.into(), theme_bg.into())),
        Clear(ClearType::All), 
        MoveTo(0, 0),
        crossterm::terminal::SetTitle("Coder"),
        crossterm::terminal::EnableLineWrap
    )?;

    let backend = CrosstermBackend::new(stdout());
    let tui = Terminal::new(backend)?;
    Ok(tui)
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
    execute!(stdout(), DisableMouseCapture)?;
    disable_raw_mode()?;
    // Leave alternate screen mode
    execute!(stdout(), crossterm::terminal::LeaveAlternateScreen)?;
    Ok(())
}
