use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use serde_json::Value;

/// One recorded provider replay.
#[derive(Debug, Clone, PartialEq)]
pub struct Fixture {
    pub name: String,
    pub driver: String,
    /// Header record first, then the recorded wire entries.
    pub transcript: Vec<Value>,
    pub input: Value,
    /// `None` where the TS replay races and the order is not yet decided.
    pub events: Option<Vec<Value>>,
}

/// The shared fixtures directory.
#[must_use]
pub fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures")
}

fn invalid(path: &Path, why: impl std::fmt::Display) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!("{}: {why}", path.display()),
    )
}

fn read_json(path: &Path) -> io::Result<Value> {
    serde_json::from_slice(&fs::read(path)?).map_err(|e| invalid(path, e))
}

fn read_ndjson(path: &Path) -> io::Result<Vec<Value>> {
    fs::read_to_string(path)?
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| serde_json::from_str(line).map_err(|e| invalid(path, e)))
        .collect()
}

/// Loads every `<fixture>/<driver>.*` triple under `dir`, sorted by name then driver.
pub fn load_all(dir: &Path) -> io::Result<Vec<Fixture>> {
    let mut out = Vec::new();
    for fixture in fs::read_dir(dir)? {
        let fixture = fixture?.path();
        if !fixture.is_dir() {
            continue;
        }
        let name = fixture
            .file_name()
            .and_then(|n| n.to_str())
            .ok_or_else(|| invalid(&fixture, "name is not utf-8"))?
            .to_owned();
        for entry in fs::read_dir(&fixture)? {
            let path = entry?.path();
            let file = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or_default();
            let Some(driver) = file.strip_suffix(".transcript.ndjson") else {
                continue;
            };
            let events_path = fixture.join(format!("{driver}.events.json"));
            let events = if events_path.exists() {
                match read_json(&events_path)? {
                    Value::Array(events) => Some(events),
                    _ => return Err(invalid(&events_path, "not an array")),
                }
            } else {
                None
            };
            out.push(Fixture {
                name: name.clone(),
                driver: driver.to_owned(),
                transcript: read_ndjson(&path)?,
                input: read_json(&fixture.join(format!("{driver}.input.json")))?,
                events,
            });
        }
    }
    out.sort_by(|a, b| (&a.name, &a.driver).cmp(&(&b.name, &b.driver)));
    Ok(out)
}
