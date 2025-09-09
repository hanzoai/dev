use lazy_static::lazy_static;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::sync::RwLock;

#[derive(Debug, Clone)]
pub struct Spinner {
    pub name: String,
    pub interval_ms: u64,
    pub frames: Vec<String>,
}

#[derive(Deserialize)]
struct SpinnerJson {
    interval: u64,
    frames: Vec<String>,
}

// The full upstream set (commit pinned) with our classic "diamond" added.
// Stored as JSON text and parsed on startup; avoids massive generated code.
const SPINNERS_JSON: &str = include_str!("../assets/spinners.json");

lazy_static! {
    static ref ALL_SPINNERS: Vec<Spinner> = {
        let map: BTreeMap<String, SpinnerJson> = serde_json::from_str(SPINNERS_JSON).unwrap_or_default();
        let mut v: Vec<Spinner> = map
            .into_iter()
            .map(|(name, sj)| Spinner { name, interval_ms: sj.interval, frames: sj.frames })
            .collect();
        // Ensure our default "diamond" exists
        if !v.iter().any(|s| s.name == "diamond") {
            v.push(Spinner { name: "diamond".to_string(), interval_ms: 120, frames: vec!["◇".into(), "◆".into()] });
        }
        // Stable sort by name for predictable order
        v.sort_by(|a, b| a.name.cmp(&b.name));
        v
    };
    static ref CURRENT_INDEX: RwLock<usize> = RwLock::new(
        ALL_SPINNERS.iter().position(|s| s.name == "diamond").unwrap_or(0)
    );
}

pub fn init_spinner(name: &str) { switch_spinner(name); }

pub fn switch_spinner(name: &str) {
    let needle = name.trim().to_ascii_lowercase();
    let idx = ALL_SPINNERS
        .iter()
        .position(|s| s.name == needle)
        .unwrap_or_else(|| ALL_SPINNERS.iter().position(|s| s.name == "diamond").unwrap_or(0));
    *CURRENT_INDEX.write().unwrap() = idx;
}

pub fn current_spinner() -> &'static Spinner { &ALL_SPINNERS[*CURRENT_INDEX.read().unwrap()] }

pub fn find_spinner_by_name(name: &str) -> Option<&'static Spinner> {
    let needle = name.trim().to_ascii_lowercase();
    ALL_SPINNERS.iter().find(|s| s.name == needle)
}

pub fn spinner_names() -> Vec<String> { ALL_SPINNERS.iter().map(|s| s.name.clone()).collect() }

pub fn frame_at_time(def: &Spinner, now_ms: u128) -> String {
    if def.frames.is_empty() { return String::new(); }
    let idx = ((now_ms as u64 / def.interval_ms) as usize) % def.frames.len();
    def.frames[idx].clone()
}
