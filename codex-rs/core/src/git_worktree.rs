use chrono::Utc;
use std::path::{Path, PathBuf};
use tokio::process::Command;

/// Sanitize a string to be used as a single git refname component.
///
/// Converts to lowercase, keeps [a-z0-9-], collapses other runs into '-'.
pub fn sanitize_ref_component(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut last_dash = false;
    for ch in s.chars() {
        let c = ch.to_ascii_lowercase();
        let valid = c.is_ascii_alphanumeric() || c == '-';
        if valid {
            out.push(c);
            last_dash = c == '-';
        } else if !last_dash {
            out.push('-');
            last_dash = true;
        }
    }
    let out = out.trim_matches('-').to_string();
    if out.is_empty() { "branch".to_string() } else { out }
}

/// Generate a branch name for a generic task. If `task` is None or cannot
/// produce a meaningful slug, fall back to a timestamp.
pub fn generate_branch_name_from_task(task: Option<&str>) -> String {
    if let Some(task) = task {
        let stop = ["the", "and", "for", "with", "from", "into", "goal"];
        let words: Vec<&str> = task
            .split_whitespace()
            .filter(|w| w.len() > 2 && !stop.contains(&w.to_ascii_lowercase().as_str()))
            .take(4)
            .collect();
        if !words.is_empty() {
            let mut slug = sanitize_ref_component(&words.join("-"));
            if slug.len() > 48 {
                slug.truncate(48);
                slug = slug.trim_matches('-').to_string();
                if slug.is_empty() { slug = "branch".to_string(); }
            }
            return format!("code-branch-{}", slug);
        }
    }
    // Fallback: timestamped id
    let ts = Utc::now().format("%Y%m%d-%H%M%S");
    format!("code-branch-{}", ts)
}

/// Resolve the git repository root (top-level) for the given cwd.
pub async fn get_git_root_from(cwd: &Path) -> Result<PathBuf, String> {
    let output = Command::new("git")
        .arg("rev-parse")
        .arg("--show-toplevel")
        .current_dir(cwd)
        .output()
        .await
        .map_err(|e| format!("Git not installed or not in a git repository: {}", e))?;

    if output.status.success() {
        let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
        Ok(PathBuf::from(path))
    } else {
        Err("Not in a git repository".to_string())
    }
}

/// Create a new worktree for `branch_id` under `<git_root>/.code/branches/<branch_id>`.
/// If a previous worktree directory exists, remove it first.
pub async fn setup_worktree(git_root: &Path, branch_id: &str) -> Result<PathBuf, String> {
    let code_dir = git_root.join(".code").join("branches");
    tokio::fs::create_dir_all(&code_dir)
        .await
        .map_err(|e| format!("Failed to create .code/branches directory: {}", e))?;

    let worktree_path = code_dir.join(branch_id);
    if worktree_path.exists() {
        let _ = Command::new("git")
            .arg("worktree")
            .arg("remove")
            .arg(worktree_path.to_str().unwrap())
            .arg("--force")
            .current_dir(git_root)
            .output()
            .await; // ignore errors
    }

    let output = Command::new("git")
        .current_dir(git_root)
        .args(["worktree", "add", "-b", branch_id, worktree_path.to_str().unwrap()])
        .output()
        .await
        .map_err(|e| format!("Failed to create git worktree: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Failed to create worktree: {}", stderr));
    }

    Ok(worktree_path)
}

/// Copy uncommitted (modified + untracked) files from `src_root` into the `worktree_path`.
/// Returns the number of files copied.
pub async fn copy_uncommitted_to_worktree(src_root: &Path, worktree_path: &Path) -> Result<usize, String> {
    // List modified and other (untracked) files relative to repo root
    let output = Command::new("git")
        .current_dir(src_root)
        .args(["ls-files", "-om", "--exclude-standard", "-z"])
        .output()
        .await
        .map_err(|e| format!("Failed to list changes: {}", e))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("git ls-files failed: {}", stderr));
    }
    let bytes = output.stdout;
    let mut count = 0usize;
    for path_bytes in bytes.split(|b| *b == 0) {
        if path_bytes.is_empty() { continue; }
        let rel = match String::from_utf8(path_bytes.to_vec()) { Ok(s) => s, Err(_) => continue };
        // Avoid copying .git files explicitly
        if rel.starts_with(".git/") { continue; }
        let from = src_root.join(&rel);
        let to = worktree_path.join(&rel);
        if let Some(parent) = to.parent() { tokio::fs::create_dir_all(parent).await.map_err(|e| format!("Failed to create dir {}: {}", parent.display(), e))?; }
        // Use copy for files; skip if it's a directory (shouldn't appear from ls-files)
        match tokio::fs::copy(&from, &to).await {
            Ok(_) => count += 1,
            Err(e) => return Err(format!("Failed to copy {} -> {}: {}", from.display(), to.display(), e)),
        }
    }
    Ok(count)
}

/// Determine repository default branch. Prefers `origin/HEAD` symbolic ref, then local `main`/`master`.
pub async fn detect_default_branch(cwd: &Path) -> Option<String> {
    // Try origin/HEAD first
    let sym = Command::new("git")
        .current_dir(cwd)
        .args(["symbolic-ref", "--quiet", "refs/remotes/origin/HEAD"])
        .output()
        .await
        .ok()?;
    if sym.status.success() {
        if let Ok(s) = String::from_utf8(sym.stdout) {
            if let Some((_, name)) = s.trim().rsplit_once('/') { return Some(name.to_string()); }
        }
    }
    // Fallback to local main/master
    for candidate in ["main", "master"] {
        let out = Command::new("git")
            .current_dir(cwd)
            .args(["rev-parse", "--verify", "--quiet", &format!("refs/heads/{candidate}")])
            .output()
            .await
            .ok()?;
        if out.status.success() { return Some(candidate.to_string()); }
    }
    None
}

