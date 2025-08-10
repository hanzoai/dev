use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use tokio::process::Command;

use crate::openai_tools::{JsonSchema, OpenAiTool, ResponsesApiTool};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentCallParams {
    pub prompt: String,
    pub agents: Vec<String>,
}

pub fn create_agent_tool() -> OpenAiTool {
    let mut properties = BTreeMap::new();
    
    properties.insert(
        "prompt".to_string(),
        JsonSchema::String {
            description: Some("The prompt to send to the external LLM agents".to_string()),
        },
    );
    
    properties.insert(
        "agents".to_string(),
        JsonSchema::Array {
            items: Box::new(JsonSchema::String {
                description: Some("Agent name: 'claude', 'gemini', or 'codex'".to_string()),
            }),
            description: Some("List of agents to call (claude, gemini, codex)".to_string()),
        },
    );

    OpenAiTool::Function(ResponsesApiTool {
        name: "agent".to_string(),
        description: "Call external LLM agents (claude, gemini, codex) via command line to get their responses for a given prompt".to_string(),
        strict: false,
        parameters: JsonSchema::Object {
            properties,
            required: Some(vec!["prompt".to_string(), "agents".to_string()]),
            additional_properties: Some(false),
        },
    })
}

pub async fn execute_agent_call(params: AgentCallParams) -> Result<String, String> {
    let mut results = Vec::new();
    
    for agent in params.agents {
        let result = match agent.to_lowercase().as_str() {
            "claude" => {
                execute_command("claude", vec!["-p", &params.prompt]).await
            }
            "gemini" => {
                execute_command("gemini", vec!["-p", &params.prompt]).await
            }
            "codex" => {
                execute_command("codex", vec!["exec", &params.prompt]).await
            }
            _ => {
                Err(format!("Unknown agent: {}", agent))
            }
        };
        
        match result {
            Ok(output) => {
                results.push(format!("=== {} response ===\n{}", agent, output));
            }
            Err(e) => {
                results.push(format!("=== {} error ===\n{}", agent, e));
            }
        }
    }
    
    Ok(results.join("\n\n"))
}

async fn execute_command(command: &str, args: Vec<&str>) -> Result<String, String> {
    let output = Command::new(command)
        .args(&args)
        .output()
        .await
        .map_err(|e| format!("Failed to execute {}: {}", command, e))?;
    
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!("Command failed: {}", stderr))
    }
}