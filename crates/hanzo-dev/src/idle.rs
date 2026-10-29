//! Standalone "dev idle" / "dev --idle" preview of the Hanzo 3D logo.
//! It does not start a model or another process, and it restores terminal
//! state on both normal exit and Ctrl+C.

use std::io;
use std::io::IsTerminal;
use std::io::Write;
use std::time::Duration;
use std::time::Instant;

use anyhow::Result;
use crossterm::cursor;
use crossterm::event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyEventKind};
use crossterm::execute;
use crossterm::terminal::{self, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen};

use crate::ascii;

struct RestoreTerminal;

impl Drop for RestoreTerminal {
    fn drop(&mut self) {
        let mut output = io::stdout();
        let _ = execute!(
            output,
            DisableMouseCapture,
            cursor::Show,
            LeaveAlternateScreen
        );
        let _ = terminal::disable_raw_mode();
    }
}

fn stage_size(columns: u16, rows: u16) -> (u16, u16) {
    let max_width = columns.saturating_sub(4).min(ascii::MAX_WIDTH);
    let max_height = rows.saturating_sub(4).min(ascii::MAX_HEIGHT);
    let width = max_width.min((u32::from(max_height) * 60 / 21) as u16);
    let height = width * 21 / 60;
    (width, height)
}

fn plain_frame(columns: u16, rows: u16) -> String {
    let frame = ascii::render(columns, rows, 0.0);
    frame
        .into_iter()
        .map(|row| row.into_iter().map(|cell| cell.glyph).collect::<String>())
        .collect::<Vec<_>>()
        .join("\n")
}

fn encode_frame(
    output: &mut impl Write,
    columns: u16,
    rows: u16,
    phase: f64,
    truecolor: bool,
    ansi256: bool,
) -> Result<()> {
    let (width, height) = stage_size(columns, rows);
    execute!(output, cursor::MoveTo(0, 0), Clear(ClearType::All))?;
    if width < 32 || height < 12 {
        execute!(output, cursor::MoveTo(0, 0))?;
        output.write_all(b"Hanzo Dev\n")?;
        output.flush()?;
        return Ok(());
    }

    let x = columns.saturating_sub(width) / 2;
    let y = rows.saturating_sub(height) / 2;
    for (index, line) in ascii::render(width, height, phase).iter().enumerate() {
        execute!(output, cursor::MoveTo(x, y + index as u16))?;
        let mut previous: Option<u32> = None;
        for cell in line {
            if cell.glyph != ' ' && (truecolor || ansi256) && previous != Some(cell.rgb) {
                let [_, r, g, b] = cell.rgb.to_be_bytes();
                let shade =
                    ((u16::from(r) + u16::from(g) + u16::from(b)) / 3).saturating_sub(8) / 10;
                if truecolor {
                    write!(output, "\x1b[38;2;{r};{g};{b}m")?;
                } else {
                    write!(output, "\x1b[38;5;{}m", 232 + shade.min(23))?;
                }
                previous = Some(cell.rgb);
            }
            write!(output, "{}", cell.glyph)?;
        }
        if truecolor || ansi256 {
            output.write_all(b"\x1b[0m")?;
        }
    }
    output.flush()?;
    Ok(())
}

// Encode before touching stdout: clearing the live screen before ray casting
// left a visible blank interval on every frame, especially on taller terminals.
fn render_frame(
    output: &mut impl Write,
    columns: u16,
    rows: u16,
    phase: f64,
    truecolor: bool,
    ansi256: bool,
) -> Result<()> {
    let mut frame = Vec::new();
    frame.extend_from_slice(b"\x1b[?2026h");
    encode_frame(&mut frame, columns, rows, phase, truecolor, ansi256)?;
    frame.extend_from_slice(b"\x1b[?2026l");
    output.write_all(&frame)?;
    output.flush()?;
    Ok(())
}

pub(crate) fn play() -> Result<()> {
    let stdout = io::stdout();
    let term = std::env::var("TERM").unwrap_or_default();
    if !stdout.is_terminal() || !io::stdin().is_terminal() || term == "dumb" {
        // Pipes, CI, shell completions and captured output never receive
        // escape sequences or an unbounded animation.
        println!("{}", plain_frame(40, 14));
        return Ok(());
    }
    let no_color = std::env::var_os("NO_COLOR").is_some() || term == "dumb";
    let color_term = std::env::var("COLORTERM")
        .unwrap_or_default()
        .to_lowercase();
    let truecolor = !no_color && (color_term.contains("truecolor") || color_term.contains("24bit"));
    let ansi256 = !no_color && !truecolor && term.contains("256color");
    let reduced = std::env::var_os("HANZO_REDUCED_MOTION").is_some() || term == "dumb";

    terminal::enable_raw_mode()?;
    let _restore = RestoreTerminal;
    let mut output = io::stdout();
    execute!(
        output,
        EnterAlternateScreen,
        EnableMouseCapture,
        cursor::Hide
    )?;
    let started = Instant::now();

    loop {
        let (cols, rows) = terminal::size()?;
        let phase = if reduced {
            0.0
        } else {
            (started.elapsed().as_secs_f64() / hanzo::logo::LOOP_SECONDS).fract()
        };
        render_frame(&mut output, cols, rows, phase, truecolor, ansi256)?;
        if reduced {
            // Static fallback is still interruptible.
            if event::poll(Duration::from_millis(250))? {
                match event::read()? {
                    Event::Resize(_, _) | Event::FocusGained | Event::FocusLost => {}
                    _ => break,
                }
            }
            continue;
        }
        if event::poll(Duration::from_millis(50))? {
            match event::read()? {
                Event::Resize(_, _) | Event::FocusGained | Event::FocusLost => {}
                Event::Key(key) if key.kind != KeyEventKind::Press => {}
                _ => break,
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_color_output_retains_front_and_side_shading() -> Result<()> {
        let mut output = Vec::new();
        render_frame(&mut output, 80, 40, 0.125, false, false)?;
        let text = String::from_utf8(output)?;
        assert!(text.contains('█') && text.contains('▒'));
        assert!(!text.contains("\x1b[38;"));
        Ok(())
    }

    #[test]
    fn stage_never_overflows_80_columns() {
        assert_eq!(stage_size(80, 40), (60, 21));
        let (cols, rows) = stage_size(80, 24);
        assert!(cols <= 60);
        assert!(rows <= 20);
        assert!(stage_size(30, 10).1 < 12);
    }

    #[test]
    fn non_interactive_has_no_terminal_codes() {
        assert!(!plain_frame(40, 14).contains('\x1b'));
    }
}
