use crate::config_types::{GithubConfig, ValidationConfig};
use crate::workflow_validation::maybe_run_actionlint;
use codex_apply_patch::{ApplyPatchAction, ApplyPatchFileChange};
use serde_json as json;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
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
        let (ext, contents_opt) = match change {
            ApplyPatchFileChange::Add { content } => (path.extension().and_then(|e| e.to_str()), Some(content)),
            ApplyPatchFileChange::Update { new_content, .. } => (path.extension().and_then(|e| e.to_str()), Some(new_content)),
            ApplyPatchFileChange::Delete { .. } => (path.extension().and_then(|e| e.to_str()), None),
        };

        let Some(contents) = contents_opt else { continue };
        match ext.unwrap_or("") {
            "json" => {
                record_ran("json-parse");
                if let Err(err) = json::from_str::<json::Value>(contents) {
                    findings.push(HarnessFinding {
                        tool: "json-parse".to_string(),
                        file: Some(path.clone()),
                        message: format!("invalid JSON: {err}"),
                    });
                }
            }
            "toml" => {
                record_ran("toml-parse");
                if let Err(err) = toml::from_str::<toml::Value>(contents) {
                    findings.push(HarnessFinding {
                        tool: "toml-parse".to_string(),
                        file: Some(path.clone()),
                        message: format!("invalid TOML: {err}"),
                    });
                }
            }
            "yml" | "yaml" => {
                record_ran("yaml-parse");
                if let Err(err) = serde_yaml::from_str::<serde_yaml::Value>(contents) {
                    findings.push(HarnessFinding {
                        tool: "yaml-parse".to_string(),
                        file: Some(path.clone()),
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

    for (path, change) in action.changes() {
        let rel = path.strip_prefix(cwd).unwrap_or(path);
        let dest = staged_root.join(rel);
        let _ = fs::create_dir_all(dest.parent().unwrap_or(staged_root));
        match change {
            ApplyPatchFileChange::Add { content } | ApplyPatchFileChange::Update { new_content: content, .. } => {
                if let Ok(mut file) = fs::File::create(&dest) {
                    let _ = file.write_all(content.as_bytes());
                }
            }
            ApplyPatchFileChange::Delete { .. } => {
                let _ = fs::remove_file(&dest);
            }
        }
    }

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
        let output = run_with_timeout(cmd, timeout);
        output
            .into_iter()
            .flat_map(|(stdout, stderr)| {
                let mut lines: Vec<String> = Vec::new();
                if !stdout.is_empty() {
                    lines.extend(String::from_utf8_lossy(&stdout).lines().map(|s| s.to_string()));
                }
                if !stderr.is_empty() {
                    lines.extend(String::from_utf8_lossy(&stderr).lines().map(|s| s.to_string()));
                }
                lines
            })
            .map(|message| HarnessFinding { tool: tool.to_string(), file: None, message })
            .collect()
    };

    let changed_paths: Vec<&Path> = action
        .changes()
        .iter()
        .filter_map(|(path, change)| matches!(change, ApplyPatchFileChange::Add { .. } | ApplyPatchFileChange::Update { .. }).then_some(path.as_path()))
        .collect();

    let shell_scripts: Vec<PathBuf> = changed_paths
        .iter()
        .filter(|path| is_shell_script(path))
        .map(|path| path.strip_prefix(cwd).unwrap_or(path).to_path_buf())
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
        .map(|path| path.strip_prefix(cwd).unwrap_or(path).to_path_buf())
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
        .map(|path| path.strip_prefix(cwd).unwrap_or(path).to_path_buf())
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
        .map(|path| path.strip_prefix(cwd).unwrap_or(path).to_path_buf())
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
        .map(|path| path.strip_prefix(cwd).unwrap_or(path).to_path_buf())
        .collect();
    if cfg.tools.rustfmt.unwrap_or(true) && !rust_files.is_empty() {
        if which(Path::new("rustfmt")).is_some() {
            record_ran("rustfmt");
        }
        findings.extend(run_tool("rustfmt", &["--check"], &rust_files));
    }

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
        .map(|path| path.strip_prefix(cwd).unwrap_or(path).to_path_buf())
        .collect();
    if cfg.tools.prettier.unwrap_or(true) && !prettier_files.is_empty() {
        if which(Path::new("prettier")).is_some() {
            record_ran("prettier");
        }
        findings.extend(run_tool("prettier", &["--check"], &prettier_files));
    }

    if findings.is_empty() && ran.is_empty() {
        None
    } else {
        Some((findings, ran))
    }
}

fn is_shell_script(path: &Path) -> bool {
    match path.extension().and_then(|ext| ext.to_str()) {
        Some("sh") => true,
        _ => {
            std::fs::read(path)
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

fn run_with_timeout(mut cmd: std::process::Command, timeout_secs: u64) -> Option<(Vec<u8>, Vec<u8>)> {
    use std::process::Stdio;
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
    let Ok(mut child) = cmd.spawn() else { return None };

    let start = std::time::Instant::now();
    loop {
        if let Some(status) = child.try_wait().ok().flatten() {
            let _ = status; // success/failure surfaces through captured output
            let mut stdout = Vec::new();
            let mut stderr = Vec::new();
            if let Some(mut out) = child.stdout.take() {
                let _ = std::io::Read::read_to_end(&mut out, &mut stdout);
            }
            if let Some(mut err) = child.stderr.take() {
                let _ = std::io::Read::read_to_end(&mut err, &mut stderr);
            }
            return Some((stdout, stderr));
        }

        if start.elapsed().as_secs() >= timeout_secs {
            let _ = child.kill();
            return None;
        }

        std::thread::sleep(std::time::Duration::from_millis(40));
    }
}
