//! Skill discovery and Kai-driven automatic skill selection for Hanzo Dev.
//!
//! Discovers skills from `<cwd>/.hanzo/dev/skills/*/SKILL.md`, `<cwd>/.agents/skills/*/SKILL.md`,
//! `<home>/skills/*/SKILL.md`, and the built-in `hanzo_config::DEFAULT_SKILLS` (`code-review`,
//! `dev`, `ui`). Builds the `skills.select@1` choice program (`state: {task}`) that `kai` was
//! trained on and combines Kai's selection with explicit skill/alias mentions.

use crate::decide::cut;
use crate::program::{Kind, Program, Question};
use indexmap::IndexMap;
use serde_json::{Map, Value};
use std::collections::HashSet;
use std::path::Path;

pub const PROGRAM_ID: &str = "skills.select@1";
pub const CALIBRATION: &str = "cal_c6ffe2e7b0c8819a";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Skill {
    pub name: String,
    pub summary: String,
    pub body: String,
}

/// Discovers all available skills for a turn in `cwd`, with repo-local skills taking precedence
/// over profile-home skills, and `hanzo_config::DEFAULT_SKILLS` providing a hermetic baseline.
pub fn discover(cwd: &Path, home: Option<&Path>) -> Vec<Skill> {
    let mut seen = HashSet::new();
    let mut out = Vec::new();

    let mut dirs = vec![
        cwd.join(".hanzo/dev/skills"),
        cwd.join(".agents/skills"),
    ];
    if let Some(h) = home {
        dirs.push(h.join("skills"));
    }

    for root in dirs {
        let Ok(entries) = std::fs::read_dir(&root) else {
            continue;
        };
        let mut paths: Vec<_> = entries
            .filter_map(Result::ok)
            .map(|e| e.path())
            .filter(|p| p.is_dir())
            .collect();
        paths.sort();
        for dir in paths {
            let Some(fallback_name) = dir.file_name().and_then(|n| n.to_str()) else {
                continue;
            };
            let skill_md = dir.join("SKILL.md");
            let Ok(raw) = std::fs::read_to_string(&skill_md) else {
                continue;
            };
            if let Some(skill) = parse_skill_md(fallback_name, &raw)
                && seen.insert(skill.name.clone())
            {
                out.push(skill);
            }
        }
    }

    for (name, raw) in hanzo_config::DEFAULT_SKILLS {
        if !seen.contains(*name)
            && let Some(skill) = parse_skill_md(name, raw)
            && seen.insert(skill.name.clone())
        {
            out.push(skill);
        }
    }

    out
}

/// Parses a `SKILL.md` file with optional YAML frontmatter (`name`, `description`).
pub fn parse_skill_md(fallback_name: &str, raw: &str) -> Option<Skill> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    let mut name = fallback_name.to_string();
    let mut description = String::new();
    let mut body = trimmed;

    if let Some(after_open) = trimmed.strip_prefix("---")
        && let Some(end) = after_open.find("\n---")
    {
        let front = &after_open[..end];
        let rest = &after_open[end + 4..];
        body = rest.trim();
        let mut in_desc_block = false;
        let mut desc_lines = Vec::new();
        for line in front.lines() {
            let trimmed_line = line.trim();
            if in_desc_block {
                if line.starts_with(' ') || line.starts_with('\t') {
                    if !trimmed_line.is_empty() {
                        desc_lines.push(trimmed_line);
                    }
                    continue;
                }
                in_desc_block = false;
                if description.is_empty() && !desc_lines.is_empty() {
                    description = desc_lines.join(" ");
                }
            }
            if let Some(v) = trimmed_line.strip_prefix("name:") {
                let v = unquote(v.trim());
                if !v.is_empty() {
                    name = v.to_string();
                }
            } else if let Some(v) = trimmed_line.strip_prefix("description:") {
                let v = unquote(v.trim());
                if matches!(v, ">" | "|" | ">-" | "|-") {
                    in_desc_block = true;
                } else if !v.is_empty() {
                    description = v.to_string();
                }
            }
        }
        if in_desc_block && description.is_empty() && !desc_lines.is_empty() {
            description = desc_lines.join(" ");
        }
    }

    if body.is_empty() {
        return None;
    }
    if description.is_empty() {
        description = body
            .lines()
            .find(|l| {
                let t = l.trim();
                !t.is_empty() && !t.starts_with('#')
            })
            .unwrap_or(&name)
            .trim()
            .to_string();
    }

    Some(Skill {
        name,
        summary: cut(&description, 160),
        body: body.to_string(),
    })
}

fn unquote(s: &str) -> &str {
    s.trim_matches('"').trim_matches('\'')
}

/// Matches explicit skill invocations (`$name`, `/name`, `<name> skill`) and high-signal
/// domain aliases (such as `ui-port`, `design skill`, `visual parity` -> `ui`, or
/// `code-review`, `/dev review` -> `code-review`).
pub fn explicit_matches(text: &str, skills: &[Skill]) -> Vec<String> {
    let lower = text.to_lowercase();
    let available: HashSet<&str> = skills.iter().map(|s| s.name.as_str()).collect();
    let mut matched = Vec::new();
    let mut push = |name: &str| {
        if available.contains(name) && !matched.iter().any(|m| m == name) {
            matched.push(name.to_string());
        }
    };

    for skill in skills {
        let n = skill.name.to_lowercase();
        if lower.contains(&format!("${n}"))
            || lower.contains(&format!("/{n}"))
            || lower.contains(&format!("{n} skill"))
            || lower.contains(&format!("'{n}' skill"))
            || lower.contains(&format!("\"{n}\" skill"))
            || (n.contains('-') && lower.contains(&n))
        {
            push(&skill.name);
        }
    }

    if lower.contains("ui-port")
        || lower.contains("ui port")
        || lower.contains("design skill")
        || lower.contains("'design' skill")
        || lower.contains("\"design\" skill")
        || lower.contains("visual parity")
        || lower.contains("rounded corners")
        || lower.contains("look and feel")
        || lower.contains("@hanzo/ui")
        || lower.contains("@hanzo/gui")
    {
        push("ui");
    }

    if lower.contains("code review")
        || lower.contains("/dev review")
        || lower.contains("/dev audit")
    {
        push("code-review");
    }

    if lower.contains("dev mode") || lower.contains("/dev ") || lower == "/dev" {
        push("dev");
    }

    matched
}

/// Builds the `skills.select@1` choice program that Kai was trained on (`state: {task}`).
pub fn program(skills: &[Skill]) -> Option<Program> {
    if skills.len() < 2 {
        return None;
    }
    let criteria: Map<String, Value> = skills
        .iter()
        .map(|s| (s.name.clone(), Value::String(s.summary.clone())))
        .collect();
    let mut questions = IndexMap::new();
    questions.insert(
        "skill".to_string(),
        Question {
            kind: Kind::Choice,
            instructions: "Which skill does this task need?".to_string(),
            criteria: Value::Object(criteria),
        },
    );
    let threshold = (1.25 / (skills.len() as f64)).clamp(0.16, 0.35);
    let mut thresholds = IndexMap::new();
    thresholds.insert("skill".to_string(), threshold);
    Some(Program {
        id: PROGRAM_ID.to_string(),
        description: "Which skill does this task need? State: {task}.".to_string(),
        calibration: Some(CALIBRATION.to_string()),
        questions,
        thresholds,
        verdict: None,
    })
}

/// Formats the selected skills' `SKILL.md` instructions for injection into the turn input.
pub fn format_selected(skills: &[Skill], names: &[String]) -> Option<String> {
    let selected: Vec<&Skill> = names
        .iter()
        .filter_map(|name| skills.iter().find(|s| &s.name == name))
        .collect();
    if selected.is_empty() {
        return None;
    }
    let list: Vec<&str> = selected.iter().map(|s| s.name.as_str()).collect();
    let bodies: Vec<String> = selected
        .iter()
        .map(|s| format!("<skill name=\"{}\">\n{}\n</skill>", s.name, s.body))
        .collect();
    Some(format!(
        "Kai: enabled skills for this request ({}):\n\n{}",
        list.join(", "),
        bodies.join("\n\n")
    ))
}
