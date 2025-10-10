use base64::Engine;
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::collections::{HashSet, VecDeque};
use std::fs as stdfs;
use std::io::{Error as IoError, ErrorKind};
use std::path::{Component, Path, PathBuf};
use std::process::Stdio;
use std::sync::OnceLock;
use tokio::fs::OpenOptions;
use tokio::process::Command;
use tokio::task;
use toml::Value as TomlValue;

use crate::config::find_code_home;

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

pub const LOCAL_DEFAULT_REMOTE: &str = "local-default";
const BRANCH_METADATA_DIR: &str = "_branch-meta";
const DEFAULT_BRANCH_CACHE_DIRS: &[&str] = &["node_modules"];

fn branch_copy_cache_dirs_enabled() -> bool {
    const TOGGLES: &[&str] = &["CODE_BRANCH_COPY_CACHES", "CODEX_BRANCH_COPY_CACHES"];
    for var in TOGGLES {
        if let Ok(value) = std::env::var(var) {
            let value = value.to_ascii_lowercase();
            return matches!(value.as_str(), "1" | "true" | "yes");
        }
    }
    false
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum CacheMode {
    Auto,
    Manual,
    Off,
}

#[derive(Clone, Debug)]
struct CacheSettings {
    mode: CacheMode,
    explicit_paths: Vec<PathBuf>,
}

impl Default for CacheSettings {
    fn default() -> Self {
        Self {
            mode: CacheMode::Auto,
            explicit_paths: Vec::new(),
        }
    }
}

fn cache_settings() -> CacheSettings {
    static SETTINGS: OnceLock<CacheSettings> = OnceLock::new();
    SETTINGS.get_or_init(CacheSettings::from_config).clone()
}

fn branch_target_cache_enabled() -> bool {
    const VARS: &[&str] = &["CODE_BRANCH_TARGET_CACHE", "CODEX_BRANCH_TARGET_CACHE"];
    for var in VARS {
        if let Ok(value) = std::env::var(var) {
            let normalized = value.trim().to_ascii_lowercase();
            if normalized.is_empty() || matches!(normalized.as_str(), "1" | "true" | "yes" | "on") {
                return true;
            }
            if matches!(normalized.as_str(), "0" | "false" | "no" | "off" | "disabled") {
                return false;
            }
        }
    }
    true
}

fn target_cache_root(repo_name: &str) -> PathBuf {
    find_code_home()
        .map(|mut home| {
            home.push("working");
            home.push("_target-cache");
            home.push(repo_name);
            home
        })
        .unwrap_or_else(|_| {
            dirs::home_dir()
                .unwrap_or_else(|| PathBuf::from("."))
                .join(".code")
                .join("working")
                .join("_target-cache")
                .join(repo_name)
        })
}

fn cache_target_path(worktree_path: &Path, rel_path: &Path) -> Option<PathBuf> {
    let repo_root = resolve_repo_root(worktree_path)?;
    let repo_name = repo_root.file_name()?.to_str()?;
    Some(target_cache_root(repo_name).join(rel_path))
}

fn is_target_directory(rel_path: &Path) -> bool {
    rel_path
        .file_name()
        .and_then(|segment| segment.to_str())
        .map(|segment| segment == "target")
        .unwrap_or(false)
}

fn collect_cache_candidates(worktree_path: &Path, settings: &CacheSettings) -> Vec<PathBuf> {
    let mut candidates: Vec<PathBuf> = match settings.mode {
        CacheMode::Manual => settings
            .explicit_paths
            .iter()
            .filter_map(|path| {
                if path.is_absolute() {
                    tracing::warn!(
                        target = %path.display(),
                        "build_cache.paths entries must be relative to the worktree; skipping absolute entry"
                    );
                    None
                } else {
                    Some(normalize_relative(path))
                }
            })
            .collect(),
        CacheMode::Auto => auto_cache_candidates(worktree_path),
        CacheMode::Off => Vec::new(),
    };

    candidates.sort();
    candidates.dedup();
    candidates
}

impl CacheSettings {
    fn from_config() -> Self {
        let mut settings = CacheSettings::default();

        let codex_home = match find_code_home() {
            Ok(path) => path,
            Err(_) => return settings,
        };

        let config_path = codex_home.join("config.toml");
        let contents = match stdfs::read_to_string(config_path) {
            Ok(value) => value,
            Err(_) => return settings,
        };

        let root: TomlValue = match contents.parse() {
            Ok(value) => value,
            Err(_) => return settings,
        };

        let table = match root.get("build_cache") {
            Some(TomlValue::Table(table)) => table,
            Some(_) | None => return settings,
        };

        if let Some(mode_str) = table.get("mode").and_then(|value| value.as_str()) {
            match mode_str.trim().to_ascii_lowercase().as_str() {
                "off" | "disabled" | "disable" => settings.mode = CacheMode::Off,
                "manual" | "custom" => settings.mode = CacheMode::Manual,
                "auto" | "automatic" | "default" => settings.mode = CacheMode::Auto,
                _ => {}
            }
        }

        if let Some(paths_value) = table.get("paths") {
            let mut collected = Vec::new();
            match paths_value {
                TomlValue::Array(arr) => {
                    for entry in arr {
                        if let Some(path_str) = entry.as_str() {
                            if !path_str.trim().is_empty() {
                                collected.push(PathBuf::from(path_str));
                            }
                        }
                    }
                }
                TomlValue::String(value) => {
                    if !value.trim().is_empty() {
                        collected.push(PathBuf::from(value));
                    }
                }
                _ => {}
            }
            settings.explicit_paths = collected;
        }

        if matches!(settings.mode, CacheMode::Manual) && settings.explicit_paths.is_empty() {
            settings.mode = CacheMode::Off;
        }

        settings
    }
}

fn auto_cache_candidates(worktree_path: &Path) -> Vec<PathBuf> {
    const MAX_DEPTH: usize = 3;
    const SKIP_DIR_NAMES: &[&str] = &[
        ".git",
        ".code",
        "target",
        "node_modules",
        "tmp",
        "temp",
        "__pycache__",
        "venv",
        ".venv",
    ];

    let mut out: HashSet<PathBuf> = HashSet::new();
    let mut queue: VecDeque<(PathBuf, usize)> = VecDeque::new();
    queue.push_back((worktree_path.to_path_buf(), 0));

    while let Some((dir, depth)) = queue.pop_front() {
        let cargo_toml = dir.join("Cargo.toml");
        if cargo_toml.is_file() {
            let target_dir = dir.join("target");
            if let Some(rel) = relative_to_worktree(&target_dir, worktree_path) {
                out.insert(rel);
            }
        }

        let package_json = dir.join("package.json");
        if package_json.is_file() {
            let node_modules = dir.join("node_modules");
            if let Some(rel) = relative_to_worktree(&node_modules, worktree_path) {
                out.insert(rel);
            }

            for default in DEFAULT_BRANCH_CACHE_DIRS {
                let candidate = dir.join(default);
                if let Some(rel) = relative_to_worktree(&candidate, worktree_path) {
                    out.insert(rel);
                }
            }
        }

        if depth >= MAX_DEPTH {
            continue;
        }

        let entries = match stdfs::read_dir(&dir) {
            Ok(entries) => entries,
            Err(_) => continue,
        };
        for entry in entries.flatten() {
            let Ok(file_type) = entry.file_type() else { continue };
            if !file_type.is_dir() {
                continue;
            }
            let name = entry.file_name();
            if let Some(name_str) = name.to_str() {
                if SKIP_DIR_NAMES.contains(&name_str) {
                    continue;
                }
            }
            queue.push_back((entry.path(), depth + 1));
        }
    }

    let mut collected: Vec<PathBuf> = out.into_iter().collect();
    collected.sort();
    collected
}

fn relative_to_worktree(path: &Path, worktree_path: &Path) -> Option<PathBuf> {
    path
        .strip_prefix(worktree_path)
        .ok()
        .map(normalize_relative)
}

fn normalize_relative(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                normalized.pop();
            }
            Component::Normal(seg) => normalized.push(seg),
            Component::Prefix(_) | Component::RootDir => {}
        }
    }
    normalized
}

fn resolve_repo_root(worktree_path: &Path) -> Option<PathBuf> {
    let mut ancestors = worktree_path.ancestors();
    while let Some(candidate) = ancestors.next() {
        if candidate.ends_with("branches") {
            return candidate.parent().map(Path::to_path_buf);
        }
    }
    worktree_path.parent().map(Path::to_path_buf)
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct BranchMetadata {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub base_branch: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub remote_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub remote_ref: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub remote_url: Option<String>,
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
pub async fn setup_worktree(git_root: &Path, branch_id: &str) -> Result<(PathBuf, String), String> {
    // Global location: ~/.code/working/<repo_name>/branches
    let repo_name = git_root
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("repo");
    let mut code_dir = dirs::home_dir().unwrap_or_else(|| PathBuf::from("."));
    code_dir = code_dir
        .join(".code")
        .join("working")
        .join(repo_name)
        .join("branches");
    tokio::fs::create_dir_all(&code_dir)
        .await
        .map_err(|e| format!("Failed to create .code/branches directory: {}", e))?;

    let mut effective_branch = branch_id.to_string();
    let mut worktree_path = code_dir.join(&effective_branch);
    if worktree_path.exists() {
        // If the worktree directory already exists, re-use it to avoid the cost
        // of removing and re-adding a worktree. This makes repeated agent runs
        // start much faster.
        record_worktree_in_session(git_root, &worktree_path).await;
        if let Err(err) = ensure_target_cache_links(&worktree_path).await {
            tracing::warn!(
                target = %worktree_path.display(),
                error = %err,
                "failed to prepare target cache link"
            );
        }
        if let Err(err) = seed_branch_targets(&worktree_path).await {
            tracing::warn!(
                target = %worktree_path.display(),
                error = %err,
                "failed to seed cached build artifacts"
            );
        }
        return Ok((worktree_path, effective_branch));
    }

    let output = Command::new("git")
        .current_dir(git_root)
        .args(["worktree", "add", "-b", &effective_branch, worktree_path.to_str().unwrap()])
        .output()
        .await
        .map_err(|e| format!("Failed to create git worktree: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // If the branch already exists, generate a unique name and retry once.
        if stderr.contains("already exists") {
            effective_branch = format!("{}-{}", effective_branch, Utc::now().format("%Y%m%d-%H%M%S"));
            worktree_path = code_dir.join(&effective_branch);
            // Ensure target path is clean
            if worktree_path.exists() {
                let _ = Command::new("git")
                    .arg("worktree")
                    .arg("remove")
                    .arg(worktree_path.to_str().unwrap())
                    .arg("--force")
                    .current_dir(git_root)
                    .output()
                    .await;
            }
            let retry = Command::new("git")
                .current_dir(git_root)
                .args(["worktree", "add", "-b", &effective_branch, worktree_path.to_str().unwrap()])
                .output()
                .await
                .map_err(|e| format!("Failed to create git worktree (retry): {}", e))?;
            if !retry.status.success() {
                let retry_err = String::from_utf8_lossy(&retry.stderr);
                return Err(format!("Failed to create worktree: {}", retry_err));
            }
            record_worktree_in_session(git_root, &worktree_path).await;
        } else {
            return Err(format!("Failed to create worktree: {}", stderr));
        }
    }

    // Skip remote alias setup for speed; we don't need it during agent runs.

    // Record created worktree for this process; best-effort.
    record_worktree_in_session(git_root, &worktree_path).await;
    if let Err(err) = ensure_target_cache_links(&worktree_path).await {
        tracing::warn!(
            target = %worktree_path.display(),
            error = %err,
            "failed to prepare target cache link"
        );
    }
    if let Err(err) = seed_branch_targets(&worktree_path).await {
        tracing::warn!(
            target = %worktree_path.display(),
            error = %err,
            "failed to seed cached build artifacts"
        );
    }

    Ok((worktree_path, effective_branch))
}

/// Append the created worktree to a per-process session file so the TUI can
/// clean it up on exit without touching worktrees from other processes.
async fn record_worktree_in_session(git_root: &Path, worktree_path: &Path) {
    let pid = std::process::id();
    let mut base = dirs::home_dir().unwrap_or_else(|| PathBuf::from("."));
    // Global session registry: ~/.code/working/_session
    base = base.join(".code").join("working").join("_session");
    if let Err(_e) = tokio::fs::create_dir_all(&base).await { return; }
    let file = base.join(format!("pid-{}.txt", pid));
    // Store git_root and worktree_path separated by a tab; one entry per line.
    if let Ok(mut f) = OpenOptions::new().create(true).append(true).open(&file).await {
        let line = format!("{}\t{}\n", git_root.display(), worktree_path.display());
        let _ = tokio::io::AsyncWriteExt::write_all(&mut f, line.as_bytes()).await;
    }
}

async fn seed_branch_targets(worktree_path: &Path) -> Result<(), String> {
    const DISABLE_ENV: &str = "CODEX_DISABLE_TARGET_SEED";
    if std::env::var(DISABLE_ENV)
        .map(|value| matches!(value.trim(), "1" | "true" | "TRUE" | "True"))
        .unwrap_or(false)
    {
        return Ok(());
    }

    let settings = cache_settings();
    if matches!(settings.mode, CacheMode::Off) {
        return Ok(());
    }

    let Some(repo_root) = resolve_repo_root(worktree_path) else {
        return Ok(());
    };
    let Some(repo_name) = repo_root
        .file_name()
        .and_then(|name| name.to_str())
        .map(|value| value.to_string())
    else {
        return Ok(());
    };

    let cache_root = target_cache_root(&repo_name);

    let target_cache_enabled = branch_target_cache_enabled();

    let candidates = collect_cache_candidates(worktree_path, &settings);

    if candidates.is_empty() {
        return Ok(());
    }

    for rel_path in candidates {
        if rel_path.as_os_str().is_empty() {
            continue;
        }

        let branch_target = worktree_path.join(&rel_path);
        let cache_target = cache_root.join(&rel_path);
        if branch_target.exists() {
            if target_cache_enabled && is_target_directory(&rel_path) {
                continue;
            }
            continue;
        }

        if target_cache_enabled && is_target_directory(&rel_path) {
            if let Err(err) = symlink_branch_to_cache(branch_target.clone(), cache_target.clone()).await {
                tracing::warn!(
                    target = %branch_target.display(),
                    cache = %cache_target.display(),
                    error = %err,
                    "failed to link shared target cache; falling back to copy"
                );
            } else {
                continue;
            }
        }

        if !cache_target.is_dir() {
            continue;
        }

        if let Some(parent) = branch_target.parent() {
            if let Err(err) = tokio::fs::create_dir_all(parent).await {
                tracing::warn!(
                    parent = %parent.display(),
                    error = %err,
                    "failed to create parent directory for cached build artifacts"
                );
                continue;
            }
        }

        if let Err(err) = clone_target_from_cache(&cache_target, &branch_target).await {
            tracing::warn!(
                cache = %cache_target.display(),
                target = %branch_target.display(),
                error = %err,
                "failed to seed cached build artifacts"
            );
        }
    }

    Ok(())
}

async fn ensure_target_cache_links(worktree_path: &Path) -> Result<(), String> {
    if !branch_target_cache_enabled() {
        return Ok(());
    }

    const DISABLE_ENV: &str = "CODEX_DISABLE_TARGET_SEED";
    if std::env::var(DISABLE_ENV)
        .map(|value| matches!(value.trim(), "1" | "true" | "TRUE" | "True"))
        .unwrap_or(false)
    {
        return Ok(());
    }

    let settings = cache_settings();
    if matches!(settings.mode, CacheMode::Off) {
        return Ok(());
    }

    let Some(repo_root) = resolve_repo_root(worktree_path) else {
        return Ok(());
    };
    let Some(repo_name) = repo_root
        .file_name()
        .and_then(|name| name.to_str())
        .map(|value| value.to_string())
    else {
        return Ok(());
    };

    let cache_root = target_cache_root(&repo_name);
    let candidates = collect_cache_candidates(worktree_path, &settings);

    for rel_path in candidates {
        if !is_target_directory(&rel_path) {
            continue;
        }
        if rel_path.as_os_str().is_empty() {
            continue;
        }

        let branch_target = worktree_path.join(&rel_path);
        let cache_target = cache_root.join(&rel_path);
        symlink_branch_to_cache(branch_target, cache_target).await?;
    }

    Ok(())
}

async fn clone_target_from_cache(cache_target: &Path, branch_target: &Path) -> Result<(), String> {
    tokio::fs::create_dir_all(branch_target)
        .await
        .map_err(|e| format!("failed to create target directory {}: {e}", branch_target.display()))?;

    let err_copy_clone = match run_rsync(cache_target, branch_target, true).await {
        Ok(()) => return Ok(()),
        Err(err) => err,
    };
    let _ = tokio::fs::remove_dir_all(branch_target).await;

    tokio::fs::create_dir_all(branch_target)
        .await
        .map_err(|e| format!("failed to recreate target directory {}: {e}", branch_target.display()))?;
    let err_cp_clone = match run_cp(cache_target, branch_target, true).await {
        Ok(()) => return Ok(()),
        Err(err) => err,
    };
    let _ = tokio::fs::remove_dir_all(branch_target).await;

    tokio::fs::create_dir_all(branch_target)
        .await
        .map_err(|e| format!("failed to recreate target directory {}: {e}", branch_target.display()))?;
    let err_cp_plain = match run_cp(cache_target, branch_target, false).await {
        Ok(()) => return Ok(()),
        Err(err) => err,
    };
    let _ = tokio::fs::remove_dir_all(branch_target).await;

    Err(format!(
        "rsync --copy-as=clone failed: {err_copy_clone}; \"cp -cR\" failed: {err_cp_clone}; \"cp\" fallback failed: {err_cp_plain}"
    ))
}

async fn run_rsync(
    cache_target: &Path,
    branch_target: &Path,
    copy_as_clone: bool,
) -> Result<(), String> {
    if copy_as_clone && !rsync_supports_copy_as_clone() {
        return Err("rsync --copy-as=clone not supported on this platform".to_string());
    }

    let mut cmd = Command::new("rsync");
    cmd.arg("-a");
    cmd.arg("--delete");
    if copy_as_clone {
        cmd.arg("--copy-as=clone");
    }
    cmd.arg("--quiet");
    cmd.stdout(Stdio::null());
    cmd.stderr(Stdio::null());

    let mut src_arg = cache_target.to_string_lossy().to_string();
    if !src_arg.ends_with('/') {
        src_arg.push('/');
    }
    let mut dst_arg = branch_target.to_string_lossy().to_string();
    if !dst_arg.ends_with('/') {
        dst_arg.push('/');
    }

    cmd.arg(src_arg);
    cmd.arg(dst_arg);

    let status = cmd
        .status()
        .await
        .map_err(|e| format!("failed to launch rsync: {e}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("rsync exited with status {status}"))
    }
}

fn rsync_supports_copy_as_clone() -> bool {
    use std::sync::OnceLock;

    static SUPPORT: OnceLock<bool> = OnceLock::new();
    *SUPPORT.get_or_init(|| {
        let Ok(output) = std::process::Command::new("rsync")
            .arg("--help")
            .output()
        else {
            return false;
        };
        let mut text = String::new();
        text.push_str(&String::from_utf8_lossy(&output.stdout));
        text.push_str(&String::from_utf8_lossy(&output.stderr));
        text.contains("--copy-as=clone")
    })
}

async fn run_cp(cache_target: &Path, branch_target: &Path, prefer_clone: bool) -> Result<(), String> {
    #[cfg(target_os = "macos")]
    {
        let mut cmd = Command::new("cp");
        if prefer_clone {
            cmd.arg("-cR");
        } else {
            cmd.arg("-R");
        }
        cmd.arg(cache_target.join("."));
        cmd.arg(branch_target);

        cmd.stdout(Stdio::null());
        cmd.stderr(Stdio::null());

        let status = cmd
            .status()
            .await
            .map_err(|e| format!("failed to launch cp: {e}"))?;
        if status.success() {
            Ok(())
        } else if prefer_clone {
            Err(format!("cp -cR exited with status {status}"))
        } else {
            Err(format!("cp exited with status {status}"))
        }
    }

    #[cfg(not(target_os = "macos"))]
    {
        if prefer_clone {
            let mut clone_cmd = Command::new("cp");
            clone_cmd.arg("-a");
            clone_cmd.arg("--reflink=auto");
            clone_cmd.arg(cache_target.join("."));
            clone_cmd.arg(branch_target);

            clone_cmd.stdout(Stdio::null());
            clone_cmd.stderr(Stdio::null());

            match clone_cmd.status().await {
                Ok(status) if status.success() => {
                    return Ok(());
                }
                Ok(status) => {
                    tracing::debug!(
                        status = %status,
                        "cp --reflink fallback failed; retrying with full copy"
                    );
                }
                Err(err) => {
                    tracing::debug!(error = %err, "cp --reflink invocation failed; retrying with full copy");
                }
            }
        }

        let mut cmd = Command::new("cp");
        cmd.arg("-a");
        cmd.arg(cache_target.join("."));
        cmd.arg(branch_target);
        cmd.stdout(Stdio::null());
        cmd.stderr(Stdio::null());

        let status = cmd
            .status()
            .await
            .map_err(|e| format!("failed to launch cp: {e}"))?;
        if status.success() {
            Ok(())
        } else {
            Err(format!("cp exited with status {status}"))
        }
    }
}

pub async fn ensure_local_default_remote(
    git_root: &Path,
    base_branch: Option<&str>,
) -> Result<Option<BranchMetadata>, String> {
    let remote_name = LOCAL_DEFAULT_REMOTE;
    let canonical_root = tokio::fs::canonicalize(git_root)
        .await
        .unwrap_or_else(|_| git_root.to_path_buf());
    let remote_url = canonical_root.to_string_lossy().to_string();

    let remote_check = Command::new("git")
        .current_dir(git_root)
        .args(["remote", "get-url", remote_name])
        .output()
        .await;

    match remote_check {
        Ok(out) if out.status.success() => {
            let existing = String::from_utf8_lossy(&out.stdout).trim().to_string();
            if existing != remote_url {
                let update = Command::new("git")
                    .current_dir(git_root)
                    .args(["remote", "set-url", remote_name, &remote_url])
                    .output()
                    .await
                    .map_err(|e| format!("Failed to set {remote_name} URL: {e}"))?;
                if !update.status.success() {
                    let stderr = String::from_utf8_lossy(&update.stderr).trim().to_string();
                    return Err(format!("Failed to set {remote_name} URL: {stderr}"));
                }
            }
        }
        _ => {
            let add = Command::new("git")
                .current_dir(git_root)
                .args(["remote", "add", remote_name, &remote_url])
                .output()
                .await
                .map_err(|e| format!("Failed to add {remote_name}: {e}"))?;
            if !add.status.success() {
                let stderr = String::from_utf8_lossy(&add.stderr).trim().to_string();
                return Err(format!("Failed to add {remote_name}: {stderr}"));
            }
        }
    }

    let base_branch_clean = base_branch
        .map(|s| s.trim())
        .filter(|s| !s.is_empty() && *s != "HEAD")
        .map(|s| s.to_string());
    let base_branch_clean = match base_branch_clean {
        Some(value) => Some(value),
        None => detect_default_branch(git_root).await,
    };

    let mut metadata = BranchMetadata {
        base_branch: base_branch_clean.clone(),
        remote_name: Some(remote_name.to_string()),
        remote_ref: None,
        remote_url: Some(remote_url),
    };

    if let Some(base) = base_branch_clean {
        let commit = Command::new("git")
            .current_dir(git_root)
            .args(["rev-parse", "--verify", &base])
            .output()
            .await
            .map_err(|e| format!("Failed to resolve base branch {base}: {e}"))?;
        if commit.status.success() {
            let sha = String::from_utf8_lossy(&commit.stdout).trim().to_string();
            if !sha.is_empty() {
                let remote_ref = format!("refs/remotes/{remote_name}/{base}");
                let update = Command::new("git")
                    .current_dir(git_root)
                    .args(["update-ref", &remote_ref, &sha])
                    .output()
                    .await
                    .map_err(|e| format!("Failed to update {remote_ref}: {e}"))?;
                if update.status.success() {
                    metadata.remote_ref = Some(format!("{remote_name}/{base}"));
                }
            }
        }
    }

    Ok(Some(metadata))
}

fn canonical_worktree_path(worktree_path: &Path) -> Option<PathBuf> {
    stdfs::canonicalize(worktree_path)
        .ok()
        .or_else(|| Some(worktree_path.to_path_buf()))
}

fn metadata_file_path(worktree_path: &Path) -> Option<PathBuf> {
    let canonical = canonical_worktree_path(worktree_path)?;
    let mut base = dirs::home_dir()?;
    base = base
        .join(".code")
        .join("working")
        .join(BRANCH_METADATA_DIR);
    let key = canonical.to_string_lossy();
    let encoded = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(key.as_bytes());
    Some(base.join(encoded).with_extension("json"))
}

pub async fn write_branch_metadata(
    worktree_path: &Path,
    metadata: &BranchMetadata,
) -> Result<(), String> {
    let Some(path) = metadata_file_path(worktree_path) else { return Ok(()); };
    if let Some(parent) = path.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .map_err(|e| format!("Failed to prepare branch metadata directory: {e}"))?;
    }
    let serialised = serde_json::to_vec_pretty(metadata)
        .map_err(|e| format!("Failed to serialise branch metadata: {e}"))?;
    let legacy_path = worktree_path.join(".codex-branch.json");
    let _ = tokio::fs::remove_file(&legacy_path).await;
    tokio::fs::write(&path, serialised)
        .await
        .map_err(|e| format!("Failed to write branch metadata: {e}"))
}

pub fn load_branch_metadata(worktree_path: &Path) -> Option<BranchMetadata> {
    if let Some(path) = metadata_file_path(worktree_path) {
        if let Ok(bytes) = stdfs::read(&path) {
            if let Ok(parsed) = serde_json::from_slice(&bytes) {
                return Some(parsed);
            }
        }
    }
    let legacy_path = worktree_path.join(".codex-branch.json");
    let bytes = stdfs::read(legacy_path).ok()?;
    serde_json::from_slice(&bytes).ok()
}

/// Ensure a remote named `origin` exists. If it's missing, choose a likely
/// writable remote (prefer `fork`, then `upstream-push`, then the first push
/// URL we find) and alias it as `origin`. Finally, set the remote HEAD so
/// `origin/HEAD` points at the default branch.
async fn _ensure_origin_remote(git_root: &Path) -> Result<(), String> {
    // Check existing remotes
    let remotes_out = Command::new("git")
        .current_dir(git_root)
        .args(["remote"])
        .output()
        .await
        .map_err(|e| format!("git remote failed: {}", e))?;
    if !remotes_out.status.success() {
        return Err("git remote returned error".to_string());
    }
    let remotes: Vec<String> = String::from_utf8_lossy(&remotes_out.stdout)
        .lines()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();
    if remotes.iter().any(|r| r == "origin") {
        // Make sure origin/HEAD is set; ignore errors
        let _ = Command::new("git")
            .current_dir(git_root)
            .args(["remote", "set-head", "origin", "-a"])
            .output()
            .await;
        return Ok(());
    }

    // Prefer candidates in this order
    let mut candidates = vec!["fork", "upstream-push", "upstream"]; // typical setups
    // Append any other remotes as fallbacks
    for r in &remotes {
        if !candidates.contains(&r.as_str()) { candidates.push(r); }
    }

    // Find a candidate with a URL
    for cand in candidates {
        let url_out = Command::new("git")
            .current_dir(git_root)
            .args(["remote", "get-url", cand])
            .output()
            .await;
        if let Ok(out) = url_out {
            if out.status.success() {
                let url = String::from_utf8_lossy(&out.stdout).trim().to_string();
                if !url.is_empty() {
                    // Add origin pointing to this URL
                    let add = Command::new("git")
                        .current_dir(git_root)
                        .args(["remote", "add", "origin", &url])
                        .output()
                        .await
                        .map_err(|e| format!("git remote add origin failed: {}", e))?;
                    if !add.status.success() {
                        return Err("failed to add origin".to_string());
                    }
                    let _ = Command::new("git")
                        .current_dir(git_root)
                        .args(["remote", "set-head", "origin", "-a"])
                        .output()
                        .await;
                    return Ok(());
                }
            }
        }
    }
    // No usable remote found; leave as-is
    Err("no suitable remote to alias as origin".to_string())
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
        let meta = match tokio::fs::metadata(&from).await { Ok(m) => m, Err(_) => continue };
        if !meta.is_file() { continue; }
        if let Some(parent) = to.parent() { tokio::fs::create_dir_all(parent).await.map_err(|e| format!("Failed to create dir {}: {}", parent.display(), e))?; }
        // Use copy for files; skip if it's a directory (shouldn't appear from ls-files)
        match tokio::fs::copy(&from, &to).await {
            Ok(_) => count += 1,
            Err(e) => return Err(format!("Failed to copy {} -> {}: {}", from.display(), to.display(), e)),
        }
    }

    // Opt-in: mirror modified submodule pointers into the worktree index (no checkout/network).
    // Enable via CODEX_BRANCH_INCLUDE_SUBMODULES=1|true|yes.
    let include_submods = std::env::var("CODEX_BRANCH_INCLUDE_SUBMODULES")
        .ok()
        .map(|v| v.to_ascii_lowercase())
        .map(|v| v == "1" || v == "true" || v == "yes")
        .unwrap_or(false);
    if include_submods {
        if let Ok(out) = Command::new("git")
            .current_dir(src_root)
            .args(["submodule", "status", "--recursive"])
            .output()
            .await
        {
            if out.status.success() {
                let text = String::from_utf8_lossy(&out.stdout);
                for line in text.lines() {
                    let line = line.trim();
                    if !line.starts_with('+') { continue; }
                    let rest = &line[1..];
                    let mut parts = rest.split_whitespace();
                    let sha = match parts.next() { Some(s) => s, None => continue };
                    let path = match parts.next() { Some(p) => p, None => continue };
                    let spec = format!("160000,{},{}", sha, path);
                    let _ = Command::new("git")
                        .current_dir(worktree_path)
                        .args(["update-index", "--add", "--cacheinfo", &spec])
                        .output()
                        .await;
                }
            }
        }
    }
    let cache_count = copy_branch_cache_dirs(src_root, worktree_path).await?;
    Ok(count + cache_count)
}

async fn copy_branch_cache_dirs(src_root: &Path, worktree_path: &Path) -> Result<usize, String> {
    let target_cache_enabled = branch_target_cache_enabled();
    let candidates = gather_branch_cache_candidates(src_root);
    let mut seen = HashSet::new();
    let mut total = 0usize;

    for candidate in candidates {
        let Some(rel) = sanitize_relative_path(&candidate) else { continue };
        if !seen.insert(rel.clone()) { continue; }

        let src_dir = src_root.join(&rel);
        let metadata = match tokio::fs::metadata(&src_dir).await {
            Ok(meta) => meta,
            Err(err) if err.kind() == ErrorKind::NotFound => continue,
            Err(err) => {
                return Err(format!("Failed to inspect {}: {}", src_dir.display(), err));
            }
        };
        if !metadata.is_dir() { continue; }

        let mut dst_dir = worktree_path.join(&rel);

        if target_cache_enabled && is_target_directory(&rel) {
            if let Some(cache_target) = cache_target_path(worktree_path, &rel) {
                let branch_target = dst_dir.clone();
                if let Err(err) = symlink_branch_to_cache(branch_target, cache_target.clone()).await {
                    tracing::warn!(
                        target = %worktree_path.join(&rel).display(),
                        error = %err,
                        "failed to link shared target cache; falling back to copy"
                    );
                } else {
                    dst_dir = cache_target;
                }
            }
        }

        let label = rel.to_string_lossy().to_string();
        let src_clone = src_dir.clone();
        let dst_clone = dst_dir.clone();

        let copied = task::spawn_blocking(move || copy_dir_recursive_blocking(&src_clone, &dst_clone))
            .await
            .map_err(|err| format!("Failed to mirror cached directory {label}: {err}"))?
            .map_err(|err| format!("Failed to mirror cached directory {label}: {err}"))?;

        total += copied;
    }

    Ok(total)
}

fn gather_branch_cache_candidates(src_root: &Path) -> Vec<PathBuf> {
    let mut out: Vec<PathBuf> = Vec::new();

    if branch_copy_cache_dirs_enabled() {
        out.extend(DEFAULT_BRANCH_CACHE_DIRS.iter().map(PathBuf::from));
        append_cargo_targets(src_root, &mut out);
        if let Some(raw) = std::env::var_os("CARGO_TARGET_DIR") {
            let path = PathBuf::from(raw);
            if let Some(rel) = relative_candidate_path(&path, src_root) {
                out.push(rel);
            }
        }
    }

    if let Some(raw) = std::env::var_os("CODE_BRANCH_COPY_DIRS") {
        out.extend(std::env::split_paths(&raw));
    }

    if let Some(raw) = std::env::var_os("CODEX_BRANCH_COPY_DIRS") {
        out.extend(std::env::split_paths(&raw));
    }

    out
}

async fn symlink_branch_to_cache(branch_target: PathBuf, cache_target: PathBuf) -> Result<(), String> {
    if let Some(parent) = branch_target.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .map_err(|err| format!("failed to prepare branch cache parent {}: {err}", parent.display()))?;
    }

    tokio::fs::create_dir_all(&cache_target)
        .await
        .map_err(|err| format!("failed to ensure cache directory {}: {err}", cache_target.display()))?;

    let branch_clone = branch_target.clone();
    let cache_clone = cache_target.clone();
    let result = task::spawn_blocking(move || -> Result<(), IoError> {
        if let Ok(existing) = stdfs::read_link(&branch_clone) {
            if existing == cache_clone {
                return Ok(());
            }
        }

        remove_existing_path(&branch_clone)?;

        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(&cache_clone, &branch_clone)?;
        }

        #[cfg(windows)]
        {
            use std::os::windows::fs::symlink_dir;
            symlink_dir(&cache_clone, &branch_clone)?;
        }

        Ok(())
    })
    .await
    .map_err(|err| format!("failed to join symlink task: {err}"))?;

    result.map_err(|err| {
        format!(
            "failed to create shared cache link {} -> {}: {err}",
            branch_target.display(),
            cache_target.display()
        )
    })
}

fn append_cargo_targets(src_root: &Path, out: &mut Vec<PathBuf>) {
    if manifest_sits_here(src_root) {
        out.push(PathBuf::from("target"));
    }

    if let Ok(entries) = stdfs::read_dir(src_root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() { continue; }
            if !manifest_sits_here(&path) { continue; }
            if let Ok(relative) = path.strip_prefix(src_root) {
                out.push(relative.join("target"));
            }
        }
    }
}

fn manifest_sits_here(dir: &Path) -> bool {
    dir.join("Cargo.toml").is_file()
}

fn relative_candidate_path(path: &Path, repo_root: &Path) -> Option<PathBuf> {
    if path.as_os_str().is_empty() { return None; }
    if path.is_relative() { return Some(path.to_path_buf()); }
    if let Ok(stripped) = path.strip_prefix(repo_root) {
        return Some(stripped.to_path_buf());
    }
    None
}

fn sanitize_relative_path(path: &Path) -> Option<PathBuf> {
    let mut clean = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => continue,
            Component::Normal(part) => clean.push(part),
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => return None,
        }
    }
    if clean.as_os_str().is_empty() { None } else { Some(clean) }
}

fn copy_dir_recursive_blocking(src: &Path, dst: &Path) -> Result<usize, IoError> {
    let mut count = 0usize;
    copy_dir_recursive_inner(src, dst, &mut count)?;
    Ok(count)
}

fn copy_dir_recursive_inner(src: &Path, dst: &Path, count: &mut usize) -> Result<(), IoError> {
    stdfs::create_dir_all(dst)?;
    let mut entries = stdfs::read_dir(src)?;
    while let Some(entry) = entries.next() {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if file_type.is_dir() {
            copy_dir_recursive_inner(&from, &to, count)?;
        } else if file_type.is_file() {
            if let Some(parent) = to.parent() { stdfs::create_dir_all(parent)?; }
            stdfs::copy(&from, &to)?;
            *count += 1;
        } else if file_type.is_symlink() {
            recreate_symlink_blocking(&from, &to)?;
            *count += 1;
        }
    }
    Ok(())
}

fn recreate_symlink_blocking(src: &Path, dst: &Path) -> Result<(), IoError> {
    if let Some(parent) = dst.parent() { stdfs::create_dir_all(parent)?; }
    let target = stdfs::read_link(src)?;
    remove_existing_path(dst)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::symlink;
        symlink(&target, dst)?;
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::FileTypeExt;
        use std::os::windows::fs::{symlink_dir, symlink_file};
        let meta = stdfs::symlink_metadata(src)?;
        if meta.file_type().is_symlink_dir() {
            symlink_dir(&target, dst)?;
        } else {
            symlink_file(&target, dst)?;
        }
    }
    Ok(())
}

fn remove_existing_path(path: &Path) -> Result<(), IoError> {
    match stdfs::remove_file(path) {
        Ok(_) => return Ok(()),
        Err(err) if err.kind() == ErrorKind::NotFound => return Ok(()),
        Err(err) if err.kind() == ErrorKind::IsADirectory => {}
        Err(err) => return Err(err),
    }
    match stdfs::remove_dir_all(path) {
        Ok(_) => Ok(()),
        Err(err) if err.kind() == ErrorKind::NotFound => Ok(()),
        Err(err) => Err(err),
    }
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
