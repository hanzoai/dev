//! Custom Agent Loader
//!
//! Loads custom agent definitions from `~/.hanzo/agents/` and `~/.claude/agents/`
//! directories. Supports both `.md` (markdown with YAML frontmatter) and `.json`
//! agent configuration formats.
//!
//! Users can invoke custom agents via `/use <agent>` slash command or configure
//! them in subagent workflows.

use crate::config::find_code_home;
use crate::config_types::AgentConfig;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use tokio::fs;

/// Custom agent definition loaded from a file
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CustomAgentDef {
    /// Agent name (required)
    pub name: String,
    /// Description of the agent's purpose
    #[serde(default)]
    pub description: Option<String>,
    /// Model to use (e.g., "opus", "sonnet", "gemini-pro")
    #[serde(default)]
    pub model: Option<String>,
    /// CLI command (defaults to "claude" if model is claude-based)
    #[serde(default)]
    pub command: Option<String>,
    /// Additional arguments
    #[serde(default)]
    pub args: Vec<String>,
    /// Tools the agent can use
    #[serde(default)]
    pub tools: Vec<String>,
    /// Capabilities configuration
    #[serde(default)]
    pub capabilities: HashMap<String, bool>,
    /// Custom system instructions (from markdown content)
    #[serde(default)]
    pub instructions: Option<String>,
    /// Environment variables for the agent
    #[serde(default)]
    pub env: Option<HashMap<String, String>>,
    /// Whether the agent is read-only by default
    #[serde(default)]
    pub read_only: bool,
    /// Whether the agent is enabled
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Source file path
    #[serde(skip)]
    pub source_path: Option<PathBuf>,
}

fn default_true() -> bool {
    true
}

impl From<CustomAgentDef> for AgentConfig {
    fn from(def: CustomAgentDef) -> Self {
        // Determine CLI command based on model or explicit command
        let command = def.command.unwrap_or_else(|| {
            if let Some(ref model) = def.model {
                let model_lower = model.to_ascii_lowercase();
                if model_lower.contains("opus")
                    || model_lower.contains("sonnet")
                    || model_lower.contains("haiku")
                    || model_lower.contains("claude")
                {
                    "claude".to_string()
                } else if model_lower.contains("gemini") {
                    "gemini".to_string()
                } else if model_lower.contains("qwen") {
                    "qwen".to_string()
                } else if model_lower.contains("gpt") || model_lower.contains("codex") {
                    "coder".to_string()
                } else if model_lower.contains("mistral") || model_lower.contains("vibe") {
                    "vibe".to_string()
                } else {
                    // Default to claude for unknown models
                    "claude".to_string()
                }
            } else {
                "claude".to_string()
            }
        });

        // Build args including model selection
        let mut args = def.args.clone();
        if let Some(ref model) = def.model {
            if !args.iter().any(|a| a == "--model" || a == "-m") {
                args.push("--model".to_string());
                args.push(model.clone());
            }
        }

        AgentConfig {
            name: def.name,
            command,
            args,
            read_only: def.read_only,
            enabled: def.enabled,
            description: def.description,
            env: def.env,
            args_read_only: None,
            args_write: None,
            instructions: def.instructions,
        }
    }
}

/// Parse YAML frontmatter from a markdown file
fn parse_frontmatter(content: &str) -> Option<(String, String)> {
    let content = content.trim();
    if !content.starts_with("---") {
        return None;
    }

    let rest = &content[3..];
    let end = rest.find("\n---")?;
    let frontmatter = &rest[..end];
    let body = rest[end + 4..].trim();

    Some((frontmatter.to_string(), body.to_string()))
}

/// Load a custom agent from a markdown file with YAML frontmatter
async fn load_agent_from_md(path: &Path) -> Option<CustomAgentDef> {
    let content = fs::read_to_string(path).await.ok()?;

    let (frontmatter, body) = parse_frontmatter(&content)?;

    // Parse the YAML frontmatter
    let mut def: CustomAgentDef = serde_yaml::from_str(&frontmatter).ok().or_else(|| {
        // Fallback: try to extract just name from frontmatter
        let name = path.file_stem()?.to_str()?.to_string();
        Some(CustomAgentDef {
            name,
            description: None,
            model: None,
            command: None,
            args: Vec::new(),
            tools: Vec::new(),
            capabilities: HashMap::new(),
            instructions: None,
            env: None,
            read_only: false,
            enabled: true,
            source_path: None,
        })
    })?;

    // Set instructions from markdown body if not empty
    if !body.is_empty() {
        def.instructions = Some(body);
    }

    def.source_path = Some(path.to_path_buf());
    Some(def)
}

/// Load a custom agent from a JSON file
async fn load_agent_from_json(path: &Path) -> Option<CustomAgentDef> {
    let content = fs::read_to_string(path).await.ok()?;
    let mut def: CustomAgentDef = serde_json::from_str(&content).ok()?;
    def.source_path = Some(path.to_path_buf());

    // If there's a companion .md file, load instructions from it
    let md_path = path.with_extension("md");
    if md_path.exists() {
        if let Ok(md_content) = fs::read_to_string(&md_path).await {
            if let Some((_, body)) = parse_frontmatter(&md_content) {
                if !body.is_empty() {
                    def.instructions = Some(body);
                }
            } else {
                // No frontmatter, use entire content as instructions
                def.instructions = Some(md_content);
            }
        }
    }

    Some(def)
}

/// Load all custom agents from a directory
async fn load_agents_from_dir(dir: &Path) -> Vec<CustomAgentDef> {
    let mut agents = Vec::new();

    let mut entries = match fs::read_dir(dir).await {
        Ok(entries) => entries,
        Err(_) => return agents,
    };

    let mut seen_names: std::collections::HashSet<String> = std::collections::HashSet::new();

    while let Ok(Some(entry)) = entries.next_entry().await {
        let path = entry.path();
        let is_file = entry
            .file_type()
            .await
            .map(|ft| ft.is_file())
            .unwrap_or(false);

        if !is_file {
            continue;
        }

        let ext = path
            .extension()
            .and_then(|s| s.to_str())
            .map(|s| s.to_ascii_lowercase());

        let agent = match ext.as_deref() {
            Some("md") => load_agent_from_md(&path).await,
            Some("json") => load_agent_from_json(&path).await,
            _ => None,
        };

        if let Some(agent) = agent {
            // Avoid duplicates (prefer .json over .md if both exist)
            if !seen_names.contains(&agent.name) {
                seen_names.insert(agent.name.clone());
                agents.push(agent);
            }
        }
    }

    agents.sort_by(|a, b| a.name.cmp(&b.name));
    agents
}

/// Default agents directory paths
fn default_agents_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();

    // HANZO_AGENTS_PATH env var - colon-separated list of directories
    if let Ok(agents_path) = std::env::var("HANZO_AGENTS_PATH") {
        for path in agents_path.split(':') {
            let p = PathBuf::from(path);
            if p.exists() {
                dirs.push(p);
            }
        }
    }

    // Primary: ~/.hanzo/agents
    if let Ok(home) = find_code_home() {
        dirs.push(home.join("agents"));
    }

    // Legacy/Claude compatible: ~/.claude/agents
    if let Some(home) = dirs::home_dir() {
        let claude_agents = home.join(".claude").join("agents");
        dirs.push(claude_agents);
    }

    // Development repo agents: ~/work/hanzo/agents/agents
    if let Some(home) = dirs::home_dir() {
        let repo_agents = home
            .join("work")
            .join("hanzo")
            .join("agents")
            .join("agents");
        if repo_agents.exists() {
            dirs.push(repo_agents);
        }
    }

    dirs
}

/// Load all custom agents from default directories
pub async fn load_custom_agents() -> Vec<CustomAgentDef> {
    let mut all_agents = Vec::new();
    let mut seen_names: std::collections::HashSet<String> = std::collections::HashSet::new();

    for dir in default_agents_dirs() {
        let agents = load_agents_from_dir(&dir).await;
        for agent in agents {
            if !seen_names.contains(&agent.name) {
                seen_names.insert(agent.name.clone());
                all_agents.push(agent);
            }
        }
    }

    all_agents
}

/// Load custom agents and convert to AgentConfig
pub async fn load_custom_agent_configs() -> Vec<AgentConfig> {
    load_custom_agents()
        .await
        .into_iter()
        .map(AgentConfig::from)
        .collect()
}

/// Find a specific custom agent by name
pub async fn find_custom_agent(name: &str) -> Option<CustomAgentDef> {
    let name_lower = name.to_ascii_lowercase();
    load_custom_agents()
        .await
        .into_iter()
        .find(|a| a.name.to_ascii_lowercase() == name_lower)
}

/// List available custom agent names
pub async fn list_custom_agent_names() -> Vec<String> {
    load_custom_agents()
        .await
        .into_iter()
        .map(|a| a.name)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_parse_frontmatter() {
        let content = r#"---
name: test-agent
description: A test agent
model: opus
---

You are a test agent.
"#;
        let (frontmatter, body) = parse_frontmatter(content).unwrap();
        assert!(frontmatter.contains("name: test-agent"));
        assert!(body.contains("You are a test agent"));
    }

    #[tokio::test]
    async fn test_load_agents_from_empty_dir() {
        let tmp = tempdir().unwrap();
        let agents = load_agents_from_dir(tmp.path()).await;
        assert!(agents.is_empty());
    }

    #[tokio::test]
    async fn test_load_agent_from_md() {
        let tmp = tempdir().unwrap();
        let path = tmp.path().join("cto.md");
        std::fs::write(
            &path,
            r#"---
name: cto
description: CTO agent
model: opus
---

You are the CTO.
"#,
        )
        .unwrap();

        let agent = load_agent_from_md(&path).await.unwrap();
        assert_eq!(agent.name, "cto");
        assert_eq!(agent.model, Some("opus".to_string()));
        assert!(agent.instructions.unwrap().contains("You are the CTO"));
    }
}
