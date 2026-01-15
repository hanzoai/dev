//! GitHub Copilot CLI Integration for Hanzo Dev
//! 
//! This module provides comprehensive GitHub Copilot integration as a CLI editing/agent backend,
//! including code suggestions, chat interactions, and workflow automation.

use crate::config::Config;
use crate::error::{Result, CodexErr};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tokio::process::Command as AsyncCommand;

/// GitHub Copilot CLI integration configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopilotConfig {
    /// Enable/disable Copilot integration
    pub enabled: bool,
    /// Copilot CLI binary path (defaults to 'gh copilot')
    pub cli_path: Option<String>,
    /// Auto-suggest on typing (for CLI context)
    pub auto_suggest: bool,
    /// Use Copilot for code review and suggestions
    pub code_review: bool,
    /// Use Copilot chat for interactive assistance
    pub chat_enabled: bool,
    /// Model preference (gpt-4, gpt-3.5-turbo)
    pub model: String,
    /// Maximum tokens for responses
    pub max_tokens: u32,
    /// Context window size for code analysis
    pub context_window: usize,
}

impl Default for CopilotConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            cli_path: None,
            auto_suggest: false,
            code_review: true,
            chat_enabled: true,
            model: "gpt-4".to_string(),
            max_tokens: 4096,
            context_window: 10000,
        }
    }
}

/// GitHub Copilot suggestion response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopilotSuggestion {
    pub text: String,
    pub confidence: f64,
    pub reasoning: Option<String>,
    pub alternatives: Vec<String>,
}

/// GitHub Copilot chat message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopilotMessage {
    pub role: String, // "user", "assistant", "system"
    pub content: String,
    pub context: Option<HashMap<String, String>>,
}

/// GitHub Copilot CLI integration manager
pub struct CopilotIntegration {
    config: CopilotConfig,
    cli_path: String,
}

impl CopilotIntegration {
    /// Create a new Copilot integration instance
    pub fn new(config: CopilotConfig) -> Result<Self> {
        let cli_path = config.cli_path.clone()
            .unwrap_or_else(|| "gh".to_string());
        
        Ok(Self {
            config,
            cli_path,
        })
    }

    /// Check if GitHub CLI and Copilot extension are available
    pub async fn check_availability(&self) -> bool {
        // Check if GitHub CLI is installed
        if !self.check_gh_cli().await {
            return false;
        }

        // Check if Copilot extension is installed
        self.check_copilot_extension().await
    }

    /// Check if GitHub CLI is installed and authenticated
    pub async fn check_gh_cli(&self) -> bool {
        let output = AsyncCommand::new(&self.cli_path)
            .args(["auth", "status"])
            .output()
            .await;

        match output {
            Ok(output) => output.status.success(),
            Err(_) => false,
        }
    }

    /// Check if GitHub Copilot extension is installed
    pub async fn check_copilot_extension(&self) -> bool {
        let output = AsyncCommand::new(&self.cli_path)
            .args(["extension", "list"])
            .output()
            .await;

        match output {
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                stdout.contains("github/copilot") || stdout.contains("github/gh-copilot")
            }
            Err(_) => false,
        }
    }

    /// Get code suggestions for a given context
    pub async fn get_code_suggestions(
        &self,
        context: &str,
        file_type: Option<&str>,
        _cursor_position: Option<usize>,
    ) -> Result<Vec<CopilotSuggestion>> {
        if !self.config.enabled {
            return Ok(vec![]);
        }

        let mut cmd = AsyncCommand::new(&self.cli_path);
        cmd.args(["copilot", "suggest"]);

        // Add context as prompt
        cmd.arg("--type").arg("shell");
        
        if let Some(ft) = file_type {
            cmd.arg("--language").arg(ft);
        }

        let output = cmd
            .arg(context)
            .output()
            .await?;

        if !output.status.success() {
            return Err(CodexErr::UnsupportedOperation(format!("Copilot command failed: {}", 
                String::from_utf8_lossy(&output.stderr))));
        }

        let response = String::from_utf8_lossy(&output.stdout);
        
        // Parse Copilot CLI output (simplified)
        let suggestions = vec![CopilotSuggestion {
            text: response.trim().to_string(),
            confidence: 0.8, // CLI doesn't provide confidence scores
            reasoning: None,
            alternatives: vec![],
        }];

        Ok(suggestions)
    }

    /// Start an interactive chat session with Copilot
    pub async fn start_chat_session(
        &self,
        initial_message: &str,
        context: Option<HashMap<String, String>>,
    ) -> Result<String> {
        if !self.config.enabled || !self.config.chat_enabled {
            return Err(CodexErr::UnsupportedOperation("Copilot chat is disabled".to_string()));
        }

        let mut cmd = AsyncCommand::new(&self.cli_path);
        cmd.args(["copilot", "chat"]);

        // Add context if provided
        if let Some(ctx) = context {
            if let Some(file_path) = ctx.get("file_path") {
                cmd.arg("--file").arg(file_path);
            }
        }

        let output = cmd
            .arg(initial_message)
            .output()
            .await?;

        if !output.status.success() {
            return Err(CodexErr::UnsupportedOperation(format!("Copilot chat failed: {}", 
                String::from_utf8_lossy(&output.stderr))));
        }

        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    }

    /// Get code review suggestions for a file or diff
    pub async fn review_code(
        &self,
        file_path: &str,
        diff: Option<&str>,
    ) -> Result<String> {
        if !self.config.enabled || !self.config.code_review {
            return Err(CodexErr::UnsupportedOperation("Copilot code review is disabled".to_string()));
        }

        let prompt = if let Some(d) = diff {
            format!("Review this code diff and provide suggestions:\n\n{}", d)
        } else {
            format!("Review the code in file {} and provide suggestions", file_path)
        };

        let mut context = HashMap::new();
        context.insert("file_path".to_string(), file_path.to_string());

        self.start_chat_session(&prompt, Some(context)).await
    }

    /// Generate documentation for code
    pub async fn generate_documentation(
        &self,
        code: &str,
        doc_type: &str, // "comment", "readme", "api"
    ) -> Result<String> {
        if !self.config.enabled {
            return Err(CodexErr::UnsupportedOperation("Copilot is disabled".to_string()));
        }

        let prompt = format!(
            "Generate {} documentation for this code:\n\n{}",
            doc_type, code
        );

        self.start_chat_session(&prompt, None).await
    }

    /// Explain code functionality
    pub async fn explain_code(&self, code: &str) -> Result<String> {
        if !self.config.enabled {
            return Err(CodexErr::UnsupportedOperation("Copilot is disabled".to_string()));
        }

        let prompt = format!(
            "Explain what this code does and how it works:\n\n{}",
            code
        );

        self.start_chat_session(&prompt, None).await
    }

    /// Generate shell commands for a given task
    pub async fn suggest_commands(&self, task_description: &str) -> Result<Vec<String>> {
        if !self.config.enabled {
            return Ok(vec![]);
        }

        let mut cmd = AsyncCommand::new(&self.cli_path);
        cmd.args(["copilot", "suggest", "--type", "shell", task_description]);

        let output = cmd.output().await?;

        if !output.status.success() {
            return Err(CodexErr::UnsupportedOperation(format!("Copilot command suggestion failed: {}", 
                String::from_utf8_lossy(&output.stderr))));
        }

        let response = String::from_utf8_lossy(&output.stdout);
        
        // Parse multiple command suggestions
        let commands = response
            .lines()
            .filter(|line| !line.trim().is_empty() && !line.starts_with('#'))
            .map(|line| line.trim().to_string())
            .collect();

        Ok(commands)
    }

    /// Generate Git commit messages
    pub async fn generate_commit_message(&self, diff: &str) -> Result<String> {
        if !self.config.enabled {
            return Err(CodexErr::UnsupportedOperation("Copilot is disabled".to_string()));
        }

        let prompt = format!(
            "Generate a concise, descriptive Git commit message for these changes:\n\n{}",
            diff
        );

        self.start_chat_session(&prompt, None).await
    }

    /// Auto-complete code based on context
    pub async fn auto_complete(
        &self,
        prefix: &str,
        file_type: Option<&str>,
        context_lines: Vec<String>,
    ) -> Result<Vec<String>> {
        if !self.config.enabled || !self.config.auto_suggest {
            return Ok(vec![]);
        }

        let context = format!(
            "Context:\n{}\n\nComplete this code: {}",
            context_lines.join("\n"),
            prefix
        );

        let suggestions = self.get_code_suggestions(&context, file_type, None).await?;
        Ok(suggestions.into_iter().map(|s| s.text).collect())
    }

    /// Install GitHub Copilot CLI extension if not present
    pub async fn install_copilot_extension(&self) -> Result<()> {
        if self.check_copilot_extension().await {
            return Ok(());
        }

        let output = AsyncCommand::new(&self.cli_path)
            .args(["extension", "install", "github/copilot"])
            .output()
            .await?;

        if !output.status.success() {
            return Err(CodexErr::UnsupportedOperation(format!("Failed to install Copilot extension: {}", 
                String::from_utf8_lossy(&output.stderr))));
        }

        Ok(())
    }

    /// Configure Copilot settings
    pub async fn configure_copilot(&self) -> Result<()> {
        // Set up Copilot authentication and preferences
        let output = AsyncCommand::new(&self.cli_path)
            .args(["copilot", "auth", "login"])
            .output()
            .await?;

        if !output.status.success() {
            return Err(CodexErr::UnsupportedOperation(format!("Failed to configure Copilot: {}", 
                String::from_utf8_lossy(&output.stderr))));
        }

        Ok(())
    }
}

/// Integration with Hanzo Dev's config system
impl From<&Config> for CopilotConfig {
    fn from(_config: &Config) -> Self {
        // Extract Copilot configuration from main config
        // This would integrate with the existing config structure
        CopilotConfig::default()
    }
}

/// Utility functions for Copilot integration
pub mod utils {
    use super::*;

    /// Extract code context around a cursor position
    pub fn extract_code_context(
        content: &str,
        cursor_pos: usize,
        window_size: usize,
    ) -> (String, String, String) {
        let lines: Vec<&str> = content.lines().collect();
        let cursor_line = content[..cursor_pos].matches('\n').count();
        
        let start = cursor_line.saturating_sub(window_size / 2);
        let end = std::cmp::min(lines.len(), cursor_line + window_size / 2);
        
        let before = lines[start..cursor_line].join("\n");
        let current = lines.get(cursor_line).unwrap_or(&"").to_string();
        let after = lines[cursor_line + 1..end].join("\n");
        
        (before, current, after)
    }

    /// Detect programming language from file extension
    pub fn detect_language(file_path: &str) -> Option<String> {
        let extension = std::path::Path::new(file_path)
            .extension()?
            .to_str()?;
            
        let language = match extension {
            "rs" => "rust",
            "py" => "python",
            "js" | "jsx" => "javascript",
            "ts" | "tsx" => "typescript",
            "go" => "go",
            "c" => "c",
            "cpp" | "cc" | "cxx" => "cpp",
            "java" => "java",
            "php" => "php",
            "rb" => "ruby",
            "sh" | "bash" => "shell",
            "md" => "markdown",
            "json" => "json",
            "yaml" | "yml" => "yaml",
            "toml" => "toml",
            "xml" => "xml",
            "html" => "html",
            "css" => "css",
            _ => return None,
        };
        
        Some(language.to_string())
    }

    /// Format code suggestions for display
    pub fn format_suggestions(suggestions: &[CopilotSuggestion]) -> String {
        let mut output = String::new();
        
        for (i, suggestion) in suggestions.iter().enumerate() {
            output.push_str(&format!("Suggestion {}:\n", i + 1));
            output.push_str(&suggestion.text);
            output.push('\n');
            
            if let Some(reasoning) = &suggestion.reasoning {
                output.push_str(&format!("Reasoning: {}\n", reasoning));
            }
            
            if !suggestion.alternatives.is_empty() {
                output.push_str("Alternatives:\n");
                for alt in &suggestion.alternatives {
                    output.push_str(&format!("  - {}\n", alt));
                }
            }
            
            output.push('\n');
        }
        
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_copilot_config_default() {
        let config = CopilotConfig::default();
        assert!(config.enabled);
        assert!(config.code_review);
        assert_eq!(config.model, "gpt-4");
    }

    #[test]
    fn test_detect_language() {
        use crate::copilot_integration::utils::detect_language;
        
        assert_eq!(detect_language("main.rs"), Some("rust".to_string()));
        assert_eq!(detect_language("script.py"), Some("python".to_string()));
        assert_eq!(detect_language("app.js"), Some("javascript".to_string()));
        assert_eq!(detect_language("unknown.xyz"), None);
    }

    #[test]
    fn test_extract_code_context() {
        use crate::copilot_integration::utils::extract_code_context;
        
        let content = "line 1\nline 2\nline 3\nline 4\nline 5";
        let cursor_pos = 14; // Position in "line 3"
        let (before, current, after) = extract_code_context(content, cursor_pos, 4);
        
        assert_eq!(before, "line 1\nline 2");
        assert_eq!(current, "line 3");
        assert_eq!(after, "line 4\nline 5");
    }
}