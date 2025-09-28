use std::sync::atomic::{AtomicU64, Ordering};

const AUTO_DRIVE_PHRASES: [&str; 60] = [
    "Plotting course…",
    "Tracing route…",
    "Laying waypoints…",
    "Calculating path…",
    "Re-routing…",
    "Charting map…",
    "Scanning terrain…",
    "Drawing route…",
    "Pathfinding…",
    "Optimizing path…",
    "Choosing lane…",
    "Picking route…",
    "Selecting turn…",
    "Weighing options…",
    "Locking coordinates…",
    "Projecting path…",
    "Computing detour…",
    "Marking destination…",
    "Resolving junction…",
    "Mapping strategy…",
    "Running path solver…",
    "Syncing nav data…",
    "Routing packets…",
    "Graph search in progress…",
    "Expanding nodes…",
    "Traversing graph…",
    "Shortest path check…",
    "Optimizing network…",
    "Waypoint solver active…",
    "Next hop pending…",
    "Projecting options…",
    "Evaluating routes…",
    "Simulating paths…",
    "Exploring courses…",
    "Assessing directions…",
    "Weighing paths…",
    "Considering routes…",
    "Analyzing choices…",
    "Surveying map…",
    "Exploring networks…",
    "Balancing routes…",
    "Charting courses…",
    "Scanning options…",
    "Mapping futures…",
    "Forecasting paths…",
    "Outlining routes…",
    "Estimating journeys…",
    "Scanning directions…",
    "Comparing routes…",
    "Exploring graphs…",
    "Visualizing options…",
    "Sketching paths…",
    "Contemplating routes…",
    "Projecting journeys…",
    "Exploring scenarios…",
    "Balancing outcomes…",
    "Mapping choices…",
    "Simulating outcomes…",
    "Assessing pathways…",
    "Envisioning next move…",
];

static PHRASE_COUNTER: AtomicU64 = AtomicU64::new(0);

pub fn next_auto_drive_phrase() -> &'static str {
    let idx = (PHRASE_COUNTER.fetch_add(1, Ordering::Relaxed) as usize)
        % AUTO_DRIVE_PHRASES.len();
    AUTO_DRIVE_PHRASES[idx]
}

pub fn is_auto_drive_phrase(candidate: &str) -> bool {
    let trimmed = candidate.trim();
    AUTO_DRIVE_PHRASES.iter().any(|phrase| phrase == &trimmed)
}
