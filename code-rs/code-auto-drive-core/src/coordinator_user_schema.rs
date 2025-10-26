//! JSON schema helper for coordinator user-turn responses.

use anyhow::Context;
use serde_json::Value;

pub fn user_turn_schema() -> Value {
    serde_json::json!({
        "type": "object",
        "additionalProperties": false,
        "properties": {
            "user_response": {
                "type": ["string", "null"],
                "maxLength": 400,
                "description": "Short message to respond the USER immediately."
            },
            "cli_command": {
                "type": ["string", "null"],
                "maxLength": 400,
                "description": "Shell command to execute in the CLI this turn. Use null when no CLI action is required."
            }
        },
        "required": ["user_response", "cli_command"]
    })
}

pub fn parse_user_turn_reply(raw: &str) -> anyhow::Result<(Option<String>, Option<String>)> {
    let value: Value = serde_json::from_str(raw)
        .context("parsing coordinator user turn JSON")?;
    let obj = value
        .as_object()
        .ok_or_else(|| anyhow::anyhow!("coordinator response was not a JSON object"))?;

    let extract = |name: &str| -> anyhow::Result<Option<String>> {
        let field = obj
            .get(name)
            .ok_or_else(|| anyhow::anyhow!("coordinator response missing required field '{name}'"))?;
        if field.is_null() {
            return Ok(None);
        }
        let Some(text) = field.as_str() else {
            return Err(anyhow::anyhow!("coordinator field '{name}' must be string or null"));
        };
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return Ok(None);
        }
        if trimmed.chars().count() > 400 {
            return Err(anyhow::anyhow!("coordinator field '{name}' exceeded 400 characters"));
        }
        Ok(Some(trimmed.to_string()))
    };

    Ok((extract("user_response")?, extract("cli_command")?))
}
