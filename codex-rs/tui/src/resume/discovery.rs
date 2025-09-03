use serde::Deserialize;
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

// No fallback scan: meta parsing for rollout headers no longer needed here.

/// Return rollout files under ~/.codex/sessions matching the provided cwd.
/// Reads only the first line of each file to avoid heavy IO.
pub fn list_sessions_for_cwd(cwd: &Path, codex_home: &Path) -> Vec<ResumeCandidate> {
    // First: try per-directory index
    if let Some(mut v) = read_dir_index(codex_home, cwd) {
        v.sort_by(|a, b| b.sort_key.cmp(&a.sort_key));
        return v.into_iter().take(200).collect();
    }

    // No index found for this directory
    Vec::new()
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

// Removed fallback slow scan; the fast per-directory index is authoritative.
