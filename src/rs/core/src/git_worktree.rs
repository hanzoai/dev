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

pub fn setup_worktree(path: &PathBuf, branch: &str) -> Result<(PathBuf, String), std::io::Error> {
    // For now, return the same path and branch
    // In a real implementation, this would create a git worktree
    Ok((path.clone(), branch.to_string()))
}