use serde::Deserialize;
use serde_json::Value;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

/// One candidate session for the picker
pub struct ResumeCandidate {
    pub path: PathBuf,
    pub title: String,
    pub subtitle: Option<String>,
    pub sort_key: String,
}

#[derive(Deserialize, Default)]
struct MetaLine {
    #[serde(default)]
    id: Option<String>,
    #[serde(default)]
    timestamp: Option<String>,
    #[serde(default)]
    cwd: Option<String>,
    #[serde(default)]
    model: Option<String>,
}

/// Return rollout files under ~/.codex/sessions matching the provided cwd.
/// Reads only the first line of each file to avoid heavy IO.
pub fn list_sessions_for_cwd(cwd: &Path, codex_home: &Path) -> Vec<ResumeCandidate> {
    let mut out: Vec<ResumeCandidate> = Vec::new();
    let mut sessions_dir = codex_home.to_path_buf();
    sessions_dir.push("sessions");
    let target = match cwd.canonicalize() { Ok(p) => p, Err(_) => cwd.to_path_buf() };

    let paths = walk_sessions(&sessions_dir);
    for path in paths {
        if let Some(candidate) = read_first_line(&path).and_then(|v| build_candidate(&path, v, &target)) {
            out.push(candidate);
        }
    }

    // Sort by timestamp descending (string compare works for RFC3339-like format)
    out.sort_by(|a, b| b.sort_key.cmp(&a.sort_key));
    out.into_iter().take(200).collect()
}

fn walk_sessions(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if !root.exists() { return files; }
    let years = match fs::read_dir(root) { Ok(r) => r, Err(_) => return files };
    for y in years.flatten() {
        if !y.file_type().map(|t| t.is_dir()).unwrap_or(false) { continue; }
        let months = match fs::read_dir(y.path()) { Ok(r) => r, Err(_) => continue };
        for m in months.flatten() {
            if !m.file_type().map(|t| t.is_dir()).unwrap_or(false) { continue; }
            let days = match fs::read_dir(m.path()) { Ok(r) => r, Err(_) => continue };
            for d in days.flatten() {
                if !d.file_type().map(|t| t.is_dir()).unwrap_or(false) { continue; }
                let entries = match fs::read_dir(d.path()) { Ok(r) => r, Err(_) => continue };
                for e in entries.flatten() {
                    if let Some(name) = e.file_name().to_str() {
                        if name.starts_with("rollout-") && name.ends_with(".jsonl") {
                            files.push(e.path());
                        }
                    }
                }
            }
        }
    }
    files
}

fn read_first_line(path: &Path) -> Option<Value> {
    let f = fs::File::open(path).ok()?;
    let mut reader = BufReader::new(f);
    let mut first = String::new();
    let _ = reader.read_line(&mut first).ok()?;
    serde_json::from_str::<Value>(&first).ok()
}

fn build_candidate(path: &Path, v: Value, target_cwd: &Path) -> Option<ResumeCandidate> {
    // The first line is SessionMetaWithGit – meta fields flattened, plus optional git.
    let meta: MetaLine = serde_json::from_value(v.clone()).ok()?;
    // Filter by cwd (abs).
    if let Some(cwd_str) = meta.cwd.as_ref() {
        let same_cwd = Path::new(cwd_str).canonicalize().ok().as_deref() == Some(target_cwd);
        if !same_cwd { return None; }
    } else {
        // If cwd missing, skip (old file). Could include later with relaxed rules.
        return None;
    }

    // Build fields for UI (title + subtitle)
    let ts = meta.timestamp.unwrap_or_default();
    let model = meta.model.unwrap_or_default();
    let id = meta.id.unwrap_or_default();

    let title = format!("{}  •  {}", ts, model);
    let subtitle = Some(truncate_middle(&id, 12));
    Some(ResumeCandidate { path: path.to_path_buf(), title, subtitle, sort_key: ts })
}

fn truncate_middle(s: &str, max: usize) -> String {
    if s.len() <= max { return s.to_string(); }
    let half = max / 2;
    format!("{}…{}", &s[..half], &s[s.len()-half..])
}
