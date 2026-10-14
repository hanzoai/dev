//! Hits as text, the way `rg -n --no-heading` prints them, or as JSON lines.

use std::collections::BTreeMap;
use std::fmt::Write;

use crate::Hit;

/// How [`render`] prints hits.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Format {
    /// `path:line:text`, context as `path-line-text`, `--` between groups.
    #[default]
    Lines,
    /// `path:text`, without line numbers.
    Bare,
    /// Each file with a hit, once, in rank order.
    Files,
    /// `path:N`, the hits in each file, in rank order.
    Count,
    /// One JSON object per hit.
    Json,
}

/// The printed form of a hit's location before the line: the path, then each
/// archive member behind `!`, then the place in the document behind `:`.
pub fn locate(hit: &Hit) -> String {
    let mut out = hit.path.display().to_string();
    if let Some(entry) = &hit.entry {
        out.push('!');
        out.push_str(entry);
    }
    if let Some(place) = &hit.place {
        out.push(':');
        out.push_str(place);
    }
    out
}

/// Prints `hits` in `format`. Every line ends with `\n`.
pub fn render(hits: &[Hit], format: Format) -> String {
    let mut out = String::new();
    match format {
        Format::Json => {
            for hit in hits {
                if let Ok(line) = serde_json::to_string(hit) {
                    out.push_str(&line);
                    out.push('\n');
                }
            }
        }
        Format::Files | Format::Count => {
            let mut files: Vec<(String, usize)> = Vec::new();
            for hit in hits {
                let file = locate_file(hit);
                match files.iter_mut().find(|(name, _)| *name == file) {
                    Some((_, count)) => *count += 1,
                    None => files.push((file, 1)),
                }
            }
            for (file, count) in files {
                if format == Format::Count {
                    let _ = writeln!(out, "{file}:{count}");
                } else {
                    let _ = writeln!(out, "{file}");
                }
            }
        }
        Format::Lines | Format::Bare => {
            // Hits of one location are adjacent in rank order. Each run prints its
            // lines once, in order, a match as `:` over a context line as `-`; `--`
            // separates runs and gaps when there is context, as rg does.
            let context = hits
                .iter()
                .any(|hit| !hit.before.is_empty() || !hit.after.is_empty());
            let mut started = false;
            let mut i = 0;
            while i < hits.len() {
                let at = locate(&hits[i]);
                let mut j = i;
                let mut lines: BTreeMap<u64, (bool, &str)> = BTreeMap::new();
                while j < hits.len() && locate(&hits[j]) == at {
                    let hit = &hits[j];
                    match hit.line {
                        None => {
                            let _ = writeln!(out, "{at}");
                        }
                        Some(line) => {
                            let first = line.saturating_sub(hit.before.len() as u64);
                            for (k, text) in hit.before.iter().enumerate() {
                                lines.entry(first + k as u64).or_insert((false, text));
                            }
                            lines.insert(line, (true, &hit.excerpt));
                            for (k, text) in hit.after.iter().enumerate() {
                                lines.entry(line + 1 + k as u64).or_insert((false, text));
                            }
                        }
                    }
                    j += 1;
                }
                let mut previous: Option<u64> = None;
                for (number, (matched, text)) in lines {
                    if context && started && previous.is_none_or(|p| number > p + 1) {
                        out.push_str("--\n");
                    }
                    started = true;
                    previous = Some(number);
                    let separator = if matched { ':' } else { '-' };
                    if format == Format::Bare {
                        let _ = writeln!(out, "{at}{separator}{text}");
                    } else {
                        let _ = writeln!(out, "{at}{separator}{number}{separator}{text}");
                    }
                }
                i = j;
            }
        }
    }
    out
}

/// The file a hit is in, with its archive members: what `-l` and `-c` print.
fn locate_file(hit: &Hit) -> String {
    let mut out = hit.path.display().to_string();
    if let Some(entry) = &hit.entry {
        out.push('!');
        out.push_str(entry);
    }
    out
}
