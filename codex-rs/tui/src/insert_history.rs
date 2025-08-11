use std::fmt;
use std::io;
use std::io::Write;

use crossterm::Command;
use crossterm::cursor::MoveTo;
use crossterm::queue;
use crossterm::style::Colors;
use crossterm::style::Print;
use crossterm::style::SetAttribute;
use crossterm::style::SetColors;
use ratatui::style::Modifier;
use ratatui::text::Line;
use ratatui::text::Span;

/// Insert `lines` above the viewport.
/// Writes ANSI to the provided writer. This
/// is intended for testing where a capture buffer is used instead of stdout.
/// NOTE: Simplified for full-screen terminal mode - no viewport manipulation needed
pub fn insert_history_lines_to_writer<B, W>(
    terminal: &mut ratatui::Terminal<B>,
    writer: &mut W,
    lines: Vec<Line>,
) where
    B: ratatui::backend::Backend,
    W: Write,
{
    // In full screen mode, we just write lines at the current position
    // This is mainly used for tests now
    
    // Set theme colors for the newlines and any unstyled content
    let theme_fg = crate::colors::text();
    let theme_bg = crate::colors::background();
    queue!(
        writer,
        SetColors(Colors::new(theme_fg.into(), theme_bg.into()))
    ).ok();

    for line in lines {
        // Fill entire line with background before writing content
        queue!(writer, Print("\r\n")).ok();
        queue!(writer, Print("\x1b[K")).ok(); // Clear to end of line with current bg
        write_spans(writer, line.iter()).ok();
        queue!(writer, Print("\x1b[K")).ok(); // Clear remainder of line after content
    }

    // Restore cursor position if we had one
    if let Ok(pos) = terminal.get_cursor_position() {
        queue!(writer, MoveTo(pos.x, pos.y)).ok();
    }
    
    writer.flush().ok();
}

/// Insert `lines` above the viewport.
pub fn insert_history_lines<B>(
    terminal: &mut ratatui::Terminal<B>,
    lines: Vec<Line>,
) where
    B: ratatui::backend::Backend,
{
    let mut writer = std::io::stdout();
    insert_history_lines_to_writer(terminal, &mut writer, lines);
}

struct SetScrollRegion(std::ops::Range<u16>);
impl Command for SetScrollRegion {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        // CSI Ps ; Ps r  (DECSTBM)
        // Set Scrolling Region [top;bottom] (default = full size of window)
        // 1-based line numbers
        write!(f, "\x1b[{};{}r", self.0.start, self.0.end)
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> io::Result<()> {
        // Windows doesn't support scroll regions in the same way
        // This is a no-op on Windows
        Ok(())
    }
}

struct ResetScrollRegion;
impl Command for ResetScrollRegion {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        // CSI r  (DECSTBM)
        // Reset Scrolling Region to full screen
        write!(f, "\x1b[r")
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> io::Result<()> {
        // Windows doesn't support scroll regions in the same way
        // This is a no-op on Windows
        Ok(())
    }
}

/// Write the spans to the writer with the correct styling
fn write_spans<'a, W: io::Write>(
    writer: &mut W,
    spans: impl Iterator<Item = &'a Span<'a>>,
) -> io::Result<()> {
    let mut prev_fg = None;
    let mut prev_bg = None;
    let mut prev_underline_color = None;
    let mut prev_modifier = Modifier::empty();
    
    for span in spans {
        let Span { content, style } = span;
        let fg = style.fg;
        let bg = style.bg;
        let underline_color = style.underline_color;
        let modifier = style.add_modifier;
        
        if fg != prev_fg || bg != prev_bg {
            let fg = fg.unwrap_or(crate::colors::text());
            let bg = bg.unwrap_or(crate::colors::background());
            queue!(writer, SetColors(Colors::new(fg.into(), bg.into())))?;
        }
        
        if underline_color != prev_underline_color {
            if let Some(color) = underline_color {
                queue!(writer, SetUnderlineColor(color.into()))?;
            }
        }
        
        if modifier != prev_modifier {
            ModifierDiff {
                from: prev_modifier,
                to: modifier,
            }
            .queue(&mut *writer)?;
        }
        
        prev_fg = fg;
        prev_bg = bg;
        prev_underline_color = underline_color;
        prev_modifier = modifier;
        
        queue!(writer, Print(content.as_ref()))?;
    }
    
    Ok(())
}

struct SetUnderlineColor(crossterm::style::Color);
impl Command for SetUnderlineColor {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        // Set underline color using SGR 58
        // Always try to write it, terminals that don't support it will ignore
        write!(f, "\x1b[58:5:{}m", ansi_color_code_from_ratatui_color(self.0))
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> io::Result<()> {
        // Windows terminal may not support underline colors
        // This is a no-op on Windows
        Ok(())
    }
}

struct ModifierDiff {
    pub from: Modifier,
    pub to: Modifier,
}

impl ModifierDiff {
    fn queue<W>(self, mut w: W) -> io::Result<()>
    where
        W: io::Write,
    {
        use crossterm::style::Attribute as CAttribute;
        let removed = self.from - self.to;
        if removed.contains(Modifier::REVERSED) {
            queue!(w, SetAttribute(CAttribute::NoReverse))?;
        }
        if removed.contains(Modifier::BOLD) {
            queue!(w, SetAttribute(CAttribute::NormalIntensity))?;
            if self.to.contains(Modifier::DIM) {
                queue!(w, SetAttribute(CAttribute::Dim))?;
            }
        }
        if removed.contains(Modifier::ITALIC) {
            queue!(w, SetAttribute(CAttribute::NoItalic))?;
        }
        if removed.contains(Modifier::UNDERLINED) {
            queue!(w, SetAttribute(CAttribute::NoUnderline))?;
        }
        if removed.contains(Modifier::DIM) {
            queue!(w, SetAttribute(CAttribute::NormalIntensity))?;
        }
        if removed.contains(Modifier::CROSSED_OUT) {
            queue!(w, SetAttribute(CAttribute::NotCrossedOut))?;
        }
        if removed.contains(Modifier::SLOW_BLINK) || removed.contains(Modifier::RAPID_BLINK) {
            queue!(w, SetAttribute(CAttribute::NoBlink))?;
        }

        let added = self.to - self.from;
        if added.contains(Modifier::REVERSED) {
            queue!(w, SetAttribute(CAttribute::Reverse))?;
        }
        if added.contains(Modifier::BOLD) {
            queue!(w, SetAttribute(CAttribute::Bold))?;
        }
        if added.contains(Modifier::ITALIC) {
            queue!(w, SetAttribute(CAttribute::Italic))?;
        }
        if added.contains(Modifier::UNDERLINED) {
            queue!(w, SetAttribute(CAttribute::Underlined))?;
        }
        if added.contains(Modifier::DIM) {
            queue!(w, SetAttribute(CAttribute::Dim))?;
        }
        if added.contains(Modifier::CROSSED_OUT) {
            queue!(w, SetAttribute(CAttribute::CrossedOut))?;
        }
        if added.contains(Modifier::SLOW_BLINK) {
            queue!(w, SetAttribute(CAttribute::SlowBlink))?;
        }
        if added.contains(Modifier::RAPID_BLINK) {
            queue!(w, SetAttribute(CAttribute::RapidBlink))?;
        }

        Ok(())
    }
}

/// Count the number of terminal lines required to render these Lines
/// accounting for line wrapping.
fn wrapped_line_count(lines: &[Line], width: u16) -> u16 {
    lines
        .iter()
        .map(|line| {
            let line_width = line.width() as u16;
            if line_width == 0 {
                1
            } else {
                (line_width + width - 1) / width
            }
        })
        .sum()
}

/// Convert ratatui::style::Color to ANSI color code
fn ansi_color_code_from_ratatui_color(color: crossterm::style::Color) -> u8 {
    use crossterm::style::Color;
    match color {
        Color::Black => 0,
        Color::DarkRed => 1,
        Color::DarkGreen => 2,
        Color::DarkYellow => 3,
        Color::DarkBlue => 4,
        Color::DarkMagenta => 5,
        Color::DarkCyan => 6,
        Color::Grey => 7,
        Color::DarkGrey => 8,
        Color::Red => 9,
        Color::Green => 10,
        Color::Yellow => 11,
        Color::Blue => 12,
        Color::Magenta => 13,
        Color::Cyan => 14,
        Color::White => 15,
        Color::Rgb { r, g, b } => {
            // Map RGB to closest 256 color
            // This is a simplified mapping
            if r == g && g == b {
                // Grayscale
                232 + ((r as u16 * 23) / 255) as u8
            } else {
                // Color cube
                16 + (36 * (r as u16 * 5 / 255) as u8)
                    + (6 * (g as u16 * 5 / 255) as u8)
                    + (b as u16 * 5 / 255) as u8
            }
        }
        Color::AnsiValue(v) => v,
        _ => 7, // Default to grey for unknown
    }
}