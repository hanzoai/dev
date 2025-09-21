use codex_apply_patch::{ApplyPatchAction, ApplyPatchFileChange};
use crate::config_types::GithubConfig;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

fn is_workflow_path(path: &Path, cwd: &Path) -> bool {
    let absolute = if path.is_absolute() { path.to_path_buf() } else { cwd.join(path) };
    let mut saw_dot_github = false;
    let mut saw_workflows = false;
    for component in absolute.components() {
        let segment = component.as_os_str().to_string_lossy();
        if !saw_dot_github && segment == ".github" {
            saw_dot_github = true;
            continue;
        }
        if saw_dot_github && !saw_workflows && segment == "workflows" {
            saw_workflows = true;
            continue;
        }
        if saw_dot_github && saw_workflows {
            return matches!(
                absolute.extension().and_then(|ext| ext.to_str()),
                Some("yml" | "yaml")
            );
        }
    }
    false
}

/// Run `actionlint` against a temporary workspace containing the proposed
/// workflow changes. Returns diagnostic lines when the tool is available and
/// produced output.
pub fn maybe_run_actionlint(
    action: &ApplyPatchAction,
    cwd: &Path,
    config: &GithubConfig,
) -> Option<Vec<String>> {
    if !config.actionlint_on_patch {
        return None;
    }

    let exe = config
        .actionlint_path
        .clone()
        .unwrap_or_else(|| PathBuf::from("actionlint"));
    let executable = if exe.is_absolute() {
        exe.exists().then_some(exe)
    } else {
        which(&exe)
    };
    let Some(executable) = executable else {
        return None;
    };

    let touches_workflow = action
        .changes()
        .keys()
        .any(|path| is_workflow_path(path, cwd));
    if !touches_workflow {
        return None;
    }

    let temp = TempDir::new().ok()?;
    let temp_root = temp.path();
    let temp_workflows = temp_root.join(".github/workflows");
    let _ = fs::create_dir_all(&temp_workflows);

    let source_workflows = cwd.join(".github/workflows");
    if source_workflows.exists() {
        let _ = copy_dir_flat(&source_workflows, &temp_workflows);
    }

    for (path, change) in action.changes() {
        if !is_workflow_path(path, cwd) {
            continue;
        }
        let relative = path.strip_prefix(cwd).unwrap_or(path);
        let destination = temp_root.join(relative);
        let _ = fs::create_dir_all(destination.parent().unwrap_or(&temp_workflows));
        match change {
            ApplyPatchFileChange::Add { content } | ApplyPatchFileChange::Update { new_content: content, .. } => {
                if let Ok(mut file) = fs::File::create(&destination) {
                    let _ = file.write_all(content.as_bytes());
                }
            }
            ApplyPatchFileChange::Delete { .. } => {
                let _ = fs::remove_file(&destination);
            }
        }
    }

    let output = std::process::Command::new(executable)
        .arg("-color")
        .arg("never")
        .current_dir(temp_root)
        .output()
        .ok()?;

    let mut lines: Vec<String> = Vec::new();
    if !output.stdout.is_empty() {
        lines.extend(String::from_utf8_lossy(&output.stdout).lines().map(|s| s.to_string()));
    }
    if !output.stderr.is_empty() {
        lines.extend(String::from_utf8_lossy(&output.stderr).lines().map(|s| s.to_string()));
    }
    if lines.is_empty() {
        None
    } else {
        Some(lines)
    }
}

fn copy_dir_flat(src: &Path, dst: &Path) -> std::io::Result<()> {
    if !dst.exists() {
        fs::create_dir_all(dst)?;
    }
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        if entry.file_type()?.is_file() {
            let target = dst.join(entry.file_name());
            let _ = fs::copy(entry.path(), target);
        }
    }
    Ok(())
}

fn which(exe: &Path) -> Option<PathBuf> {
    let name = exe.as_os_str();
    let paths: Vec<PathBuf> = std::env::var_os("PATH")
        .map(|paths| std::env::split_paths(&paths).collect())
        .unwrap_or_else(Vec::new);
    for dir in paths {
        let candidate = dir.join(name);
        if candidate.is_file() && is_executable(&candidate) {
            return Some(candidate);
        }
    }
    None
}

#[cfg(unix)]
fn is_executable(path: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;
    fs::metadata(path)
        .map(|meta| meta.permissions().mode() & 0o111 != 0)
        .unwrap_or(false)
}

#[cfg(not(unix))]
fn is_executable(path: &Path) -> bool {
    path.is_file()
}
