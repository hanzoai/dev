use lazy_static::lazy_static;
use std::sync::RwLock;

/// Definition of a spinner: human name, frame interval (ms), and frames.
pub struct SpinnerDef {
    pub name: &'static str,
    pub interval_ms: u64,
    pub frames: &'static [&'static str],
}

// A curated set of spinners. Names align with sindresorhus/cli-spinners where possible.
// Default is "diamond" to preserve the current look-and-feel.
const SPINNER_DIAMOND: SpinnerDef = SpinnerDef { name: "diamond", interval_ms: 120, frames: &["◇", "◆"] };
const SPINNER_DOTS: SpinnerDef = SpinnerDef { name: "dots", interval_ms: 80, frames: &["⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏"] };
const SPINNER_LINE: SpinnerDef = SpinnerDef { name: "line", interval_ms: 130, frames: &["-","\\","|","/"] };
const SPINNER_PIPE: SpinnerDef = SpinnerDef { name: "pipe", interval_ms: 100, frames: &["┤","┘","┴","└","├","┌","┬","┐"] };
const SPINNER_STAR: SpinnerDef = SpinnerDef { name: "star", interval_ms: 100, frames: &["✶","✵","✹","✺","✹","✷"] };
const SPINNER_ARROW: SpinnerDef = SpinnerDef { name: "arrow", interval_ms: 100, frames: &["←","↖","↑","↗","→","↘","↓","↙"] };
const SPINNER_GROW_VERTICAL: SpinnerDef = SpinnerDef { name: "grow-vertical", interval_ms: 120, frames: &["▁","▃","▄","▅","▆","▇","▆","▅","▄","▃"] };
const SPINNER_BOUNCING_BAR: SpinnerDef = SpinnerDef { name: "bouncing-bar", interval_ms: 80, frames: &["[    ]","[   =]","[  ==]","[ ===]","[====]","[=== ]","[ == ]","[  = ]","[    ]","[=   ]","[==  ]","[=== ]","[====]","[ ===]","[  ==]","[   =]"] };
const SPINNER_FLIP: SpinnerDef = SpinnerDef { name: "flip", interval_ms: 70, frames: &["_","-","‾","-" ] };
const SPINNER_TRIANGLE: SpinnerDef = SpinnerDef { name: "triangle", interval_ms: 80, frames: &["◢","◣","◤","◥"] };
const SPINNER_MONKEY: SpinnerDef = SpinnerDef { name: "monkey", interval_ms: 300, frames: &["🙈","🙉","🙊"] };

const ALL_SPINNERS: &[&SpinnerDef] = &[
    &SPINNER_DIAMOND,
    &SPINNER_DOTS,
    &SPINNER_LINE,
    &SPINNER_PIPE,
    &SPINNER_STAR,
    &SPINNER_ARROW,
    &SPINNER_GROW_VERTICAL,
    &SPINNER_BOUNCING_BAR,
    &SPINNER_FLIP,
    &SPINNER_TRIANGLE,
    &SPINNER_MONKEY,
];

lazy_static! {
    static ref CURRENT_SPINNER: RwLock<&'static SpinnerDef> = RwLock::new(&SPINNER_DIAMOND);
}

/// Initialize the global spinner by name (kebab-case). Unknown names fall back to "diamond".
pub fn init_spinner(name: &str) {
    let def = find_spinner_by_name(name).unwrap_or(&SPINNER_DIAMOND);
    let mut cur = CURRENT_SPINNER.write().unwrap();
    *cur = def;
}

/// Switch to a different spinner by name.
pub fn switch_spinner(name: &str) {
    init_spinner(name);
}

/// Return the current spinner definition.
pub fn current_spinner() -> &'static SpinnerDef {
    *CURRENT_SPINNER.read().unwrap()
}

/// Get a spinner definition by name.
pub fn find_spinner_by_name(name: &str) -> Option<&'static SpinnerDef> {
    let needle = name.trim().to_ascii_lowercase();
    ALL_SPINNERS.iter().copied().find(|s| s.name == needle)
}

/// Enumerate available spinner names in display order.
pub fn spinner_names() -> Vec<&'static str> {
    ALL_SPINNERS.iter().map(|s| s.name).collect()
}

/// Compute the current frame index given `now` in milliseconds.
pub fn frame_at_time(def: &SpinnerDef, now_ms: u128) -> &'static str {
    if def.frames.is_empty() { return ""; }
    let idx = ((now_ms as u64 / def.interval_ms) as usize) % def.frames.len();
    def.frames[idx]
}

