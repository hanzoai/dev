use std::io::{self, Write};

use crate::colors;
use crossterm::cursor::MoveTo;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use crossterm::queue;
use crossterm::style::{Color as CtColor, Print, ResetColor, SetForegroundColor};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType};
use crossterm::ExecutableCommand;

pub(crate) enum ModelMigrationOutcome {
    Accepted,
    Rejected,
    Exit,
}

const HEADING: &str = "A new model is available";
const CONTENT: &[&str] = &[
    "The model you are on has a successor in the catalog.",
    "Choose how you'd like Hanzo Dev to proceed.",
];

pub(crate) fn run_model_migration_prompt() -> io::Result<ModelMigrationOutcome> {
    struct RawModeGuard;
    impl RawModeGuard {
        fn new() -> io::Result<Self> {
            enable_raw_mode()?;
            Ok(Self)
        }
    }
    impl Drop for RawModeGuard {
        fn drop(&mut self) {
            let _ = disable_raw_mode();
        }
    }

    let _guard = RawModeGuard::new()?;

    let mut stdout = io::stdout();
    let mut highlighted = 0usize;
    render_prompt(&mut stdout, highlighted)?;

    loop {
        let event = event::read()?;
        if let Event::Key(KeyEvent { code, modifiers, kind, .. }) = event {
            if matches!(kind, KeyEventKind::Release) {
                continue;
            }

            if modifiers.contains(KeyModifiers::CONTROL)
                && matches!(code, KeyCode::Char('c') | KeyCode::Char('d'))
            {
                return Ok(ModelMigrationOutcome::Exit);
            }

            match code {
                KeyCode::Up | KeyCode::Char('k') => {
                    highlighted = 0;
                    render_prompt(&mut stdout, highlighted)?;
                }
                KeyCode::Down | KeyCode::Char('j') => {
                    highlighted = 1;
                    render_prompt(&mut stdout, highlighted)?;
                }
                KeyCode::Char('1') => return Ok(ModelMigrationOutcome::Accepted),
                KeyCode::Char('2') => return Ok(ModelMigrationOutcome::Rejected),
                KeyCode::Enter => {
                    return if highlighted == 0 {
                        Ok(ModelMigrationOutcome::Accepted)
                    } else {
                        Ok(ModelMigrationOutcome::Rejected)
                    };
                }
                KeyCode::Esc => return Ok(ModelMigrationOutcome::Rejected),
                KeyCode::Char('q') => return Ok(ModelMigrationOutcome::Exit),
                _ => {}
            }
        }
    }
}

fn render_prompt(stdout: &mut io::Stdout, highlighted: usize) -> io::Result<()> {
    stdout.execute(Clear(ClearType::All))?;
    stdout.execute(MoveTo(0, 0))?;

    write_line(stdout, HEADING)?;
    write_blank(stdout)?;
    for line in CONTENT {
        write_line(stdout, line)?;
    }

    write_blank(stdout)?;
    let primary_fg = CtColor::from(colors::primary());
    for (idx, label) in ["Try new model (recommended)", "Use existing model"].iter().enumerate() {
        if idx == highlighted {
            queue!(stdout, SetForegroundColor(primary_fg), Print("> "), Print(*label), ResetColor, Print("\r\n"))?;
        } else {
            queue!(stdout, Print("  "), Print(*label), Print("\r\n"))?;
        }
    }
    write_blank(stdout)?;
    write_key_tip_line(stdout)?;

    stdout.flush()
}

fn write_line(stdout: &mut io::Stdout, line: &str) -> io::Result<()> {
    stdout.write_all(line.as_bytes())?;
    stdout.write_all(b"\r\n")
}

fn write_key_tip_line(stdout: &mut io::Stdout) -> io::Result<()> {
    let tip_fg = CtColor::from(colors::function());
    queue!(
        stdout,
        Print("Use "),
        SetForegroundColor(tip_fg),
        Print("↑/↓"),
        ResetColor,
        Print(" to move, "),
        SetForegroundColor(tip_fg),
        Print("Enter"),
        ResetColor,
        Print(" to confirm, "),
        SetForegroundColor(tip_fg),
        Print("Esc"),
        ResetColor,
        Print(" to keep current model.\r\n")
    )?;
    Ok(())
}

fn write_blank(stdout: &mut io::Stdout) -> io::Result<()> {
    stdout.write_all(b"\r\n")
}
