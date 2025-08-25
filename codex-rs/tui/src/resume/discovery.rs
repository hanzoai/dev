use serde::Deserialize;
use serde_json::Value;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

/// One candidate session for the picker
pub struct ResumeCandidate {
    pub path: PathBuf,
    pub subtitle: Option<String>,
    pub sort_key: String,
    pub created_ts: Option<String>,
    pub modified_ts: Option<String>,
    pub message_count: usize,
    pub branch: Option<String>,
    pub snippet: Option<String>,
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
    // First: try per-directory index
    if let Some(mut v) = read_dir_index(codex_home, cwd) {
        v.sort_by(|a, b| b.sort_key.cmp(&a.sort_key));
        return v.into_iter().take(200).collect();
    }

    // Fallback: scan rollouts
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
    out.sort_by(|a, b| b.sort_key.cmp(&a.sort_key));
    out.into_iter().take(200).collect()
}

#[derive(Deserialize)]
struct DirIndexLine {
    record_type: String,
    cwd: String,
    session_file: String,
    created_ts: Option<String>,
    modified_ts: Option<String>,
    message_count_delta: Option<usize>,
    model: Option<String>,
    branch: Option<String>,
    last_user_snippet: Option<String>,
}

fn read_dir_index(codex_home: &Path, cwd: &Path) -> Option<Vec<ResumeCandidate>> {
    let index_path = super_sanitize_dir_index_path(codex_home, cwd);
    let f = fs::File::open(index_path).ok()?;
    let reader = BufReader::new(f);
    use std::collections::HashMap;
    struct Accum {
        created: Option<String>,
        modified: Option<String>,
        count: usize,
        model: Option<String>,
        branch: Option<String>,
        snippet: Option<String>,
    }
    let mut map: HashMap<String, Accum> = HashMap::new();
    for line in reader.lines() {
        let Ok(l) = line else { continue };
        if l.trim().is_empty() { continue; }
        let Ok(v) = serde_json::from_str::<DirIndexLine>(&l) else { continue };
        if v.record_type != "dir_index" { continue; }
        if v.cwd.is_empty() { continue; }
        let e = map.entry(v.session_file.clone()).or_insert(Accum {
            created: v.created_ts.clone(),
            modified: v.modified_ts.clone(),
            count: 0,
            model: v.model.clone(),
            branch: v.branch.clone(),
            snippet: None,
        });
        if e.created.is_none() { e.created = v.created_ts.clone(); }
        e.modified = v.modified_ts.clone().or(e.modified.take());
        e.count = e.count.saturating_add(v.message_count_delta.unwrap_or(0));
        if let Some(s) = v.last_user_snippet { if !s.is_empty() { e.snippet = Some(s); } }
        if e.model.is_none() { e.model = v.model.clone(); }
        if e.branch.is_none() { e.branch = v.branch.clone(); }
    }
    let mut out = Vec::new();
    for (path, a) in map.into_iter() {
        if a.count == 0 { continue; }
        let subtitle = a.snippet.clone();
        out.push(ResumeCandidate {
            path: PathBuf::from(path),
            subtitle: subtitle.clone(),
            sort_key: a.modified.clone().unwrap_or_default(),
            created_ts: a.created,
            modified_ts: a.modified,
            message_count: a.count,
            branch: a.branch,
            snippet: subtitle,
        });
    }
    Some(out)
}

fn super_sanitize_dir_index_path(codex_home: &Path, cwd: &Path) -> PathBuf {
    let mut name = cwd.to_string_lossy().to_string();
    name = name.chars().map(|c| if c.is_ascii_alphanumeric() { c } else { '_' }).collect();
    if name.len() > 160 { name.truncate(160); }
    let mut p = codex_home.to_path_buf();
    p.push("sessions");
    p.push("index");
    p.push("by-dir");
    p.push(format!("{}.jsonl", name));
    p
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
    let _model = meta.model.unwrap_or_default();
    let id = meta.id.unwrap_or_default();

    let subtitle = Some(truncate_middle(&id, 12));
    Some(ResumeCandidate { path: path.to_path_buf(), subtitle, sort_key: ts.clone(), created_ts: Some(ts), modified_ts: None, message_count: 0, branch: None, snippet: None })
}

fn truncate_middle(s: &str, max: usize) -> String {
    if s.len() <= max { return s.to_string(); }
    let half = max / 2;
    format!("{}…{}", &s[..half], &s[s.len()-half..])
}
