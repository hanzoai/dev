// External editor support for opening user's $EDITOR.

use std::fmt;

#[derive(Debug)]
pub enum ExternalEditorError {
    LaunchFailed(String),
    IoError(std::io::Error),
}

impl fmt::Display for ExternalEditorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LaunchFailed(msg) => write!(f, "Failed to launch editor: {msg}"),
            Self::IoError(err) => write!(f, "I/O error: {err}"),
        }
    }
}

impl std::error::Error for ExternalEditorError {}

impl From<std::io::Error> for ExternalEditorError {
    fn from(err: std::io::Error) -> Self {
        Self::IoError(err)
    }
}

/// Open the user's `$EDITOR` (or `$VISUAL`) with `initial` text pre-filled,
/// then return whatever the user saved.
pub fn run_editor(initial: &str) -> Result<String, ExternalEditorError> {
    let editor = std::env::var("VISUAL")
        .or_else(|_| std::env::var("EDITOR"))
        .unwrap_or_else(|_| "vi".to_string());

    let mut tmp = tempfile::Builder::new()
        .prefix("hanzo-edit-")
        .suffix(".md")
        .tempfile()
        .map_err(ExternalEditorError::IoError)?;

    use std::io::Write;
    tmp.write_all(initial.as_bytes())?;
    tmp.flush()?;

    let path = tmp.path().to_owned();
    let status = std::process::Command::new(&editor)
        .arg(&path)
        .status()
        .map_err(|e| ExternalEditorError::LaunchFailed(format!("{editor}: {e}")))?;

    if !status.success() {
        return Err(ExternalEditorError::LaunchFailed(format!(
            "{editor} exited with status {status}"
        )));
    }

    std::fs::read_to_string(&path).map_err(ExternalEditorError::IoError)
}
