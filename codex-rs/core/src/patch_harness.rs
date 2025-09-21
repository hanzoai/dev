use crate::config_types::{GithubConfig, ValidationConfig};
use crate::workflow_validation::maybe_run_actionlint;
use codex_apply_patch::{ApplyPatchAction, ApplyPatchFileChange};
use serde_json as json;
use std::collections::{BTreeSet, HashSet};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::ExitStatus;
use tempfile::TempDir;

#[derive(Debug, Clone)]
pub struct HarnessFinding {
    pub tool: String,
    pub file: Option<PathBuf>,
    pub message: String,
}

/// Run fast validations on the files touched by a patch. Returns `None` when the
/// harness is disabled and no checks were executed.
pub fn run_patch_harness(
    action: &ApplyPatchAction,
    cwd: &Path,
    cfg: &ValidationConfig,
    github: &GithubConfig,
) -> Option<(Vec<HarnessFinding>, Vec<String>)> {
    if !cfg.patch_harness {
        return None;
    }

    let mut findings: Vec<HarnessFinding> = Vec::new();
    let mut ran: Vec<String> = Vec::new();
    let mut record_ran = |name: &str| {
        if !ran.iter().any(|existing| existing == name) {
            ran.push(name.to_string());
        }
    };

    // 1) Built-in structural parses (JSON/TOML/YAML).
    for (path, change) in action.changes() {
        let (analysis_path, contents_opt) = match change {
            ApplyPatchFileChange::Add { content } => (path.as_path(), Some(content)),
            ApplyPatchFileChange::Update { new_content, move_path, .. } => (
                move_path.as_ref().map_or(path.as_path(), |dest| dest.as_path()),
                Some(new_content),
            ),
            ApplyPatchFileChange::Delete { .. } => (path.as_path(), None),
        };

        let Some(contents) = contents_opt else { continue };
        match analysis_path.extension().and_then(|e| e.to_str()).unwrap_or("") {
            "json" => {
                record_ran("json-parse");
                if let Err(err) = json::from_str::<json::Value>(contents) {
                    findings.push(HarnessFinding {
                        tool: "json-parse".to_string(),
                        file: Some(analysis_path.to_path_buf()),
                        message: format!("invalid JSON: {err}"),
                    });
                }
            }
            "toml" => {
                record_ran("toml-parse");
                if let Err(err) = toml::from_str::<toml::Value>(contents) {
                    findings.push(HarnessFinding {
                        tool: "toml-parse".to_string(),
                        file: Some(analysis_path.to_path_buf()),
                        message: format!("invalid TOML: {err}"),
                    });
                }
            }
            "yml" | "yaml" => {
                record_ran("yaml-parse");
                if let Err(err) = serde_yaml::from_str::<serde_yaml::Value>(contents) {
                    findings.push(HarnessFinding {
                        tool: "yaml-parse".to_string(),
                        file: Some(analysis_path.to_path_buf()),
                        message: format!("invalid YAML: {err}"),
                    });
                }
            }
            _ => {}
        }
    }

    // 2) Workflow checks (actionlint plugin).
    if let Some(lines) = maybe_run_actionlint(action, cwd, github) {
        if !lines.is_empty() {
            record_ran("actionlint");
            for line in lines.into_iter().take(24) {
                findings.push(HarnessFinding { tool: "actionlint".to_string(), file: None, message: line });
            }
        }
    }

    // 3) External tools (shellcheck, markdownlint, etc.).
    let allow = cfg.tools_allowlist.clone().unwrap_or_default();
    let timeout = cfg.timeout_seconds.unwrap_or(6);

    // Stage touched files into a temporary workspace so external tools can run safely.
    let temp = TempDir::new().ok()?;
    let staged_root = temp.path();

    let mut changed_paths: Vec<PathBuf> = Vec::new();
    for (path, change) in action.changes() {
        match change {
            ApplyPatchFileChange::Add { content } => {
                if let Some(rel) = stage_file(staged_root, cwd, path, content) {
                    changed_paths.push(rel);
                }
            }
            ApplyPatchFileChange::Update { new_content, move_path, .. } => {
                let dest_path = move_path.as_ref().unwrap_or(path);
                if let Some(rel) = stage_file(staged_root, cwd, dest_path, new_content) {
                    changed_paths.push(rel);
                }
                if move_path.is_some() && move_path.as_ref().map(|p| p.as_path()) != Some(path.as_path()) {
                    remove_staged_file(staged_root, cwd, path);
                }
            }
            ApplyPatchFileChange::Delete { .. } => {
                remove_staged_file(staged_root, cwd, path);
            }
        }
    }

    changed_paths.sort();
    changed_paths.dedup();

    let is_allowed = |tool: &str| allow.is_empty() || allow.iter().any(|entry| entry == tool);
    let run_tool = |tool: &str, args: &[&str], files: &[PathBuf]| -> Vec<HarnessFinding> {
        if files.is_empty() || !is_allowed(tool) {
            return Vec::new();
        }
        let Some(exe) = which(Path::new(tool)) else { return Vec::new() };
        let mut cmd = std::process::Command::new(exe);
        cmd.current_dir(staged_root);
        cmd.args(args);
        cmd.args(files);
        match run_with_timeout(cmd, timeout) {
            Some(output) => collect_output_lines(&output.stdout, &output.stderr)
                .into_iter()
                .map(|message| HarnessFinding { tool: tool.to_string(), file: None, message })
                .collect(),
            None => vec![HarnessFinding {
                tool: tool.to_string(),
                file: None,
                message: format!("{tool} timed out after {timeout} second(s)"),
            }],
        }
    };

    let shell_scripts: Vec<PathBuf> = changed_paths
        .iter()
        .filter(|path| is_shell_script(staged_root, path))
        .cloned()
        .collect();
    if cfg.tools.shellcheck.unwrap_or(true) && !shell_scripts.is_empty() {
        if which(Path::new("shellcheck")).is_some() {
            record_ran("shellcheck");
        }
        findings.extend(run_tool("shellcheck", &["-f", "gcc"], &shell_scripts));
    }

    let markdown_files: Vec<PathBuf> = changed_paths
        .iter()
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("md"))
        .cloned()
        .collect();
    if cfg.tools.markdownlint.unwrap_or(true) && !markdown_files.is_empty() {
        if which(Path::new("markdownlint")).is_some() || which(Path::new("markdownlint-cli2")).is_some() {
            record_ran("markdownlint");
        }
        let mut lines = run_tool("markdownlint", &[], &markdown_files);
        if lines.is_empty() {
            lines = run_tool("markdownlint-cli2", &[], &markdown_files);
        }
        findings.extend(lines);
    }

    let docker_files: Vec<PathBuf> = changed_paths
        .iter()
        .filter(|path| is_dockerfile(path))
        .cloned()
        .collect();
    if cfg.tools.hadolint.unwrap_or(true) && !docker_files.is_empty() {
        if which(Path::new("hadolint")).is_some() {
            record_ran("hadolint");
        }
        findings.extend(run_tool("hadolint", &[], &docker_files));
    }

    let yaml_files: Vec<PathBuf> = changed_paths
        .iter()
        .filter(|path| matches!(path.extension().and_then(|ext| ext.to_str()), Some("yml" | "yaml")))
        .cloned()
        .collect();
    if cfg.tools.yamllint.unwrap_or(true) && !yaml_files.is_empty() {
        if which(Path::new("yamllint")).is_some() {
            record_ran("yamllint");
        }
        findings.extend(run_tool("yamllint", &["-f", "parsable"], &yaml_files));
    }

    let rust_files: Vec<PathBuf> = changed_paths
        .iter()
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("rs"))
        .cloned()
        .collect();

    if cfg.tools.shfmt.unwrap_or(true) && !shell_scripts.is_empty() {
        if which(Path::new("shfmt")).is_some() {
            record_ran("shfmt");
        }
        findings.extend(run_tool("shfmt", &["-d"], &shell_scripts));
    }

    let prettier_exts = [
        "js", "jsx", "ts", "tsx", "json", "css", "scss", "less", "html", "yml", "yaml",
    ];
    let prettier_files: Vec<PathBuf> = changed_paths
        .iter()
        .filter(|path| path
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| prettier_exts.contains(&ext))
            .unwrap_or(false))
        .cloned()
        .collect();
    if cfg.tools.prettier.unwrap_or(true) && !prettier_files.is_empty() {
        if which(Path::new("prettier")).is_some() {
            record_ran("prettier");
        }
        findings.extend(run_tool("prettier", &["--check"], &prettier_files));
    }

    if cfg.tools.cargo_check.unwrap_or(true) && !rust_files.is_empty() {
        if which(Path::new("cargo")).is_none() {
            findings.push(HarnessFinding {
                tool: "cargo-check".to_string(),
                file: None,
                message: "cargo executable not found; install the Rust toolchain".to_string(),
            });
        } else {
            match WorkspaceOverlay::apply(action) {
            Ok(overlay) => {
                let manifests = collect_rust_manifests(cwd, &rust_files);
                let rust_timeout = timeout.max(30);
                for manifest in manifests {
                    let label = manifest
                        .strip_prefix(cwd)
                        .unwrap_or(&manifest)
                        .display()
                        .to_string();
                    let mut cmd = std::process::Command::new("cargo");
                    cmd.current_dir(cwd);
                    cmd.arg("check");
                    cmd.arg("--quiet");
                    cmd.arg("--all-targets");
                    cmd.arg("--manifest-path");
                    cmd.arg(manifest.to_string_lossy().to_string());
                    cmd.env("RUSTFLAGS", "-Dwarnings");

                    match run_with_timeout(cmd, rust_timeout) {
                        Some(output) => {
                            record_ran(&format!("cargo-check({label})"));
                            if output.status.map_or(true, |status| !status.success()) {
                                let mut lines = collect_output_lines(&output.stdout, &output.stderr);
                                if lines.is_empty() {
                                    lines.push("cargo check failed (no output)".to_string());
                                }
                                for line in lines.into_iter().take(24) {
                                    findings.push(HarnessFinding {
                                        tool: format!("cargo-check({label})"),
                                        file: None,
                                        message: line,
                                    });
                                }
                            }
                        }
                        None => {
                            findings.push(HarnessFinding {
                                tool: format!("cargo-check({label})"),
                                file: None,
                                message: format!(
                                    "cargo check timed out after {rust_timeout} second(s)"
                                ),
                            });
                        }
                    }
                }
                drop(overlay);
            }
            Err(err) => {
                findings.push(HarnessFinding {
                    tool: "cargo-check".to_string(),
                    file: None,
                    message: format!("failed to stage workspace for cargo check: {err}"),
                });
            }
        }
        }
    }

    if findings.is_empty() && ran.is_empty() {
        None
    } else {
        Some((findings, ran))
    }
}

fn is_shell_script(staged_root: &Path, relative: &Path) -> bool {
    match relative.extension().and_then(|ext| ext.to_str()) {
        Some("sh") => true,
        _ => {
            let staged = staged_root.join(relative);
            std::fs::read(staged)
                .ok()
                .and_then(|bytes| String::from_utf8(bytes).ok())
                .map(|contents| contents.starts_with("#!/"))
                .unwrap_or(false)
        }
    }
}

fn is_dockerfile(path: &Path) -> bool {
    let Some(name) = path.file_name().and_then(|n| n.to_str()) else { return false };
    name.eq_ignore_ascii_case("Dockerfile") || name.starts_with("Dockerfile.")
}

fn which(exe: &Path) -> Option<PathBuf> {
    if exe.is_absolute() {
        return exe.exists().then(|| exe.to_path_buf());
    }
    let name = exe.as_os_str();
    let paths: Vec<PathBuf> = std::env::var_os("PATH")
        .map(|p| std::env::split_paths(&p).collect())
        .unwrap_or_else(Vec::new);
    for dir in paths {
        let candidate = dir.join(name);
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

fn run_with_timeout(mut cmd: std::process::Command, timeout_secs: u64) -> Option<CommandCapture> {
    use std::process::Stdio;
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
    let Ok(mut child) = cmd.spawn() else { return None };

    let start = std::time::Instant::now();
    loop {
        if let Some(status) = child.try_wait().ok().flatten() {
            let mut stdout = Vec::new();
            let mut stderr = Vec::new();
            if let Some(mut out) = child.stdout.take() {
                let _ = std::io::Read::read_to_end(&mut out, &mut stdout);
            }
            if let Some(mut err) = child.stderr.take() {
                let _ = std::io::Read::read_to_end(&mut err, &mut stderr);
            }
            return Some(CommandCapture { status: Some(status), stdout, stderr });
        }

        if start.elapsed().as_secs() >= timeout_secs {
            let _ = child.kill();
            return None;
        }

        std::thread::sleep(std::time::Duration::from_millis(40));
    }
}

fn stage_file(staged_root: &Path, cwd: &Path, path: &Path, contents: &str) -> Option<PathBuf> {
    let relative = path.strip_prefix(cwd).ok()?;
    let dest = staged_root.join(relative);
    if let Some(parent) = dest.parent() {
        let _ = fs::create_dir_all(parent);
    }
    if let Ok(mut file) = fs::File::create(&dest) {
        if file.write_all(contents.as_bytes()).is_ok() {
            return Some(relative.to_path_buf());
        }
    }
    None
}

fn remove_staged_file(staged_root: &Path, cwd: &Path, path: &Path) {
    if let Ok(relative) = path.strip_prefix(cwd) {
        let dest = staged_root.join(relative);
        let _ = fs::remove_file(dest);
    }
}

fn collect_output_lines(stdout: &[u8], stderr: &[u8]) -> Vec<String> {
    let mut lines: Vec<String> = Vec::new();
    if !stdout.is_empty() {
        lines.extend(String::from_utf8_lossy(stdout).lines().map(|s| s.to_string()));
    }
    if !stderr.is_empty() {
        lines.extend(String::from_utf8_lossy(stderr).lines().map(|s| s.to_string()));
    }
    lines.retain(|line| !line.trim().is_empty());
    lines
}

fn collect_rust_manifests(cwd: &Path, rust_files: &[PathBuf]) -> Vec<PathBuf> {
    let mut manifests = BTreeSet::new();
    for relative in rust_files {
        if let Some(manifest) = find_manifest(cwd, relative) {
            manifests.insert(manifest);
        }
    }
    manifests.into_iter().collect()
}

fn find_manifest(cwd: &Path, relative: &Path) -> Option<PathBuf> {
    let absolute = cwd.join(relative);
    let mut current = absolute.parent()?;
    loop {
        let candidate = current.join("Cargo.toml");
        if candidate.exists() {
            return Some(candidate);
        }
        if current == cwd {
            break;
        }
        current = current.parent()?;
    }
    None
}

struct WorkspaceOverlay {
    backups: Vec<(PathBuf, Option<Vec<u8>>)>,
    created_dirs: Vec<PathBuf>,
}

impl WorkspaceOverlay {
    fn apply(action: &ApplyPatchAction) -> std::io::Result<Self> {
        let mut overlay = WorkspaceOverlay { backups: Vec::new(), created_dirs: Vec::new() };
        let mut seen: HashSet<PathBuf> = HashSet::new();

        for (path, change) in action.changes() {
            match change {
                ApplyPatchFileChange::Add { content } => {
                    overlay.write_file(path, content, &mut seen)?;
                }
                ApplyPatchFileChange::Update { new_content, move_path, .. } => {
                    if let Some(dest) = move_path {
                        overlay.write_file(dest, new_content, &mut seen)?;
                        if dest != path {
                            overlay.remove_file(path, &mut seen)?;
                        }
                    } else {
                        overlay.write_file(path, new_content, &mut seen)?;
                    }
                }
                ApplyPatchFileChange::Delete { .. } => {
                    overlay.remove_file(path, &mut seen)?;
                }
            }
        }

        Ok(overlay)
    }

    fn write_file(
        &mut self,
        path: &Path,
        contents: &str,
        seen: &mut HashSet<PathBuf>,
    ) -> std::io::Result<()> {
        self.backup_if_needed(path, seen)?;
        if let Some(parent) = path.parent() {
            self.ensure_dir(parent)?;
        }
        let mut file = fs::File::create(path)?;
        file.write_all(contents.as_bytes())?;
        Ok(())
    }

    fn remove_file(&mut self, path: &Path, seen: &mut HashSet<PathBuf>) -> std::io::Result<()> {
        self.backup_if_needed(path, seen)?;
        if let Err(err) = fs::remove_file(path) {
            if err.kind() != std::io::ErrorKind::NotFound {
                return Err(err);
            }
        }
        Ok(())
    }

    fn backup_if_needed(&mut self, path: &Path, seen: &mut HashSet<PathBuf>) -> std::io::Result<()> {
        if !seen.insert(path.to_path_buf()) {
            return Ok(());
        }
        let original = match fs::read(path) {
            Ok(bytes) => Some(bytes),
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => None,
            Err(err) => return Err(err),
        };
        self.backups.push((path.to_path_buf(), original));
        Ok(())
    }

    fn ensure_dir(&mut self, dir: &Path) -> std::io::Result<()> {
        if dir.exists() {
            return Ok(());
        }
        let mut to_create: Vec<PathBuf> = Vec::new();
        let mut current = dir.to_path_buf();
        while !current.exists() {
            to_create.push(current.clone());
            if let Some(parent) = current.parent() {
                current = parent.to_path_buf();
            } else {
                break;
            }
        }
        for path in to_create.iter().rev() {
            fs::create_dir(path)?;
            self.created_dirs.push(path.clone());
        }
        Ok(())
    }
}

impl Drop for WorkspaceOverlay {
    fn drop(&mut self) {
        for (path, original) in self.backups.iter().rev() {
            match original {
                Some(bytes) => {
                    if let Some(parent) = path.parent() {
                        let _ = fs::create_dir_all(parent);
                    }
                    let _ = fs::File::create(path).and_then(|mut file| file.write_all(bytes));
                }
                None => {
                    let _ = fs::remove_file(path);
                }
            }
        }

        for dir in self.created_dirs.iter().rev() {
            let _ = fs::remove_dir(dir);
        }
    }
}

struct CommandCapture {
    status: Option<ExitStatus>,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}
