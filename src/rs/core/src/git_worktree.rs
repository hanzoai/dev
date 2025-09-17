use std::path::PathBuf;

pub struct GitWorktree {
    pub path: PathBuf,
}

impl GitWorktree {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}

pub fn sanitize_ref_component(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_alphanumeric() || c == '-' || c == '_' { c } else { '_' })
        .collect()
}

pub fn setup_worktree(_path: &PathBuf, _branch: &str) -> Result<(), std::io::Error> {
    Ok(())
}