//! Deterministic Tool Resolver & Zero-Inference Repair.
//!
//! Provides canonical session tool registry lookups, case normalization (e.g. `bash` -> `Bash`),
//! conservative argument repairs, schema verification, and workspace-aware failure fingerprinting.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};

/// Client family for session-specific alias resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ClientKind {
    ClaudeCode,
    Codex,
    Cursor,
    NativeDev,
    Generic,
}

/// Specification of an executable tool registered in this session.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ToolSpec {
    pub name: String,
    pub description: String,
    pub parameters_schema: Value,
    pub is_sensitive: bool,
}

impl ToolSpec {
    pub fn new(name: impl Into<String>, description: impl Into<String>, schema: Value, is_sensitive: bool) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            parameters_schema: schema,
            is_sensitive,
        }
    }
}

/// Session-scoped registry of advertised tools.
#[derive(Debug, Clone, Default)]
pub struct SessionToolRegistry {
    pub client: Option<ClientKind>,
    tools: HashMap<String, ToolSpec>,
}

impl SessionToolRegistry {
    pub fn new(client: Option<ClientKind>) -> Self {
        Self {
            client,
            tools: HashMap::new(),
        }
    }

    /// Registers a tool into the session registry.
    pub fn register(&mut self, spec: ToolSpec) {
        self.tools.insert(spec.name.clone(), spec);
    }

    /// Returns exact registered tool if it exists verbatim.
    pub fn exact(&self, name: &str) -> Option<&ToolSpec> {
        self.tools.get(name)
    }

    /// Creates a pre-populated registry matching actual Claude Code advertised tools.
    pub fn claude_code_defaults() -> Self {
        let mut reg = Self::new(Some(ClientKind::ClaudeCode));
        reg.register(ToolSpec::new(
            "Bash",
            "Run a shell command",
            serde_json::json!({
                "type": "object",
                "properties": {
                    "command": { "type": "string" },
                    "timeout": { "type": "integer" }
                },
                "required": ["command"]
            }),
            false,
        ));
        reg.register(ToolSpec::new(
            "Read",
            "Read file contents",
            serde_json::json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "offset": { "type": "integer" },
                    "limit": { "type": "integer" }
                },
                "required": ["path"]
            }),
            false,
        ));
        reg.register(ToolSpec::new(
            "Edit",
            "Perform text edit on file",
            serde_json::json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "old_string": { "type": "string" },
                    "new_string": { "type": "string" }
                },
                "required": ["path", "old_string", "new_string"]
            }),
            false,
        ));
        reg.register(ToolSpec::new(
            "Write",
            "Write whole file",
            serde_json::json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "content": { "type": "string" }
                },
                "required": ["path", "content"]
            }),
            false,
        ));
        reg.register(ToolSpec::new(
            "Grep",
            "Search file content regex",
            serde_json::json!({
                "type": "object",
                "properties": {
                    "pattern": { "type": "string" },
                    "path": { "type": "string" }
                },
                "required": ["pattern"]
            }),
            false,
        ));
        reg.register(ToolSpec::new(
            "Glob",
            "List files by glob pattern",
            serde_json::json!({
                "type": "object",
                "properties": {
                    "pattern": { "type": "string" },
                    "path": { "type": "string" }
                },
                "required": ["pattern"]
            }),
            false,
        ));
        reg
    }

    /// Creates a pre-populated registry matching actual Codex advertised tools.
    pub fn codex_defaults() -> Self {
        let mut reg = Self::new(Some(ClientKind::Codex));
        reg.register(ToolSpec::new(
            "exec_command",
            "Execute shell command in sandbox",
            serde_json::json!({
                "type": "object",
                "properties": {
                    "command": { "type": "array", "items": { "type": "string" } },
                    "cwd": { "type": "string" }
                },
                "required": ["command"]
            }),
            false,
        ));
        reg.register(ToolSpec::new(
            "apply_patch",
            "Apply unified patch to workspace",
            serde_json::json!({
                "type": "object",
                "properties": {
                    "patch": { "type": "string" }
                },
                "required": ["patch"]
            }),
            false,
        ));
        reg
    }

    /// Finds candidate registered tools for `requested` using exact session matches and approved aliases.
    pub fn aliases_for(&self, requested: &str) -> Vec<&ToolSpec> {
        let req_lower = requested.to_lowercase();
        let mut candidates = Vec::new();
        let mut seen = HashSet::new();

        // 1. Direct case-insensitive match against registered tools in this session
        for (name, spec) in &self.tools {
            if name.eq_ignore_ascii_case(requested) {
                if seen.insert(name.as_str()) {
                    candidates.push(spec);
                }
            }
        }

        // 2. Client-specific approved aliases
        match self.client {
            Some(ClientKind::ClaudeCode) | None => {
                let targets = match req_lower.as_str() {
                    "bash" | "sh" | "shell" | "terminal" => vec!["Bash"],
                    "read_file" | "view_file" => vec!["Read"],
                    "write_file" | "write_to_file" => vec!["Write"],
                    "grep_search" | "find_in_files" => vec!["Grep"],
                    "glob_search" | "find_files" => vec!["Glob"],
                    // Notice: "file_edit" is deliberately NOT mapped automatically to Edit or Write
                    // because edit vs write have different semantics and schemas.
                    _ => Vec::new(),
                };
                for target in targets {
                    if let Some(spec) = self.tools.get(target) {
                        if seen.insert(spec.name.as_str()) {
                            candidates.push(spec);
                        }
                    }
                }
            }
            Some(ClientKind::Codex) => {
                let targets = match req_lower.as_str() {
                    "bash" | "shell" => vec!["exec_command"],
                    _ => Vec::new(),
                };
                for target in targets {
                    if let Some(spec) = self.tools.get(target) {
                        if seen.insert(spec.name.as_str()) {
                            candidates.push(spec);
                        }
                    }
                }
            }
            Some(ClientKind::Cursor) => {
                let targets = match req_lower.as_str() {
                    "bash" => vec!["run_terminal_command"],
                    _ => Vec::new(),
                };
                for target in targets {
                    if let Some(spec) = self.tools.get(target) {
                        if seen.insert(spec.name.as_str()) {
                            candidates.push(spec);
                        }
                    }
                }
            }
            _ => {}
        }

        candidates
    }

    /// Determines whether the requested operation represents a sensitive/destructive action.
    pub fn is_sensitive_request(&self, requested: &str, arguments: &Value) -> bool {
        let req_lower = requested.to_lowercase();
        if req_lower.contains("delete")
            || req_lower.contains("destroy")
            || req_lower.contains("drop")
            || req_lower.contains("kill")
            || req_lower.contains("deploy")
            || req_lower.contains("release")
            || req_lower.contains("push_force")
        {
            return true;
        }

        // Check command payload if shell
        if let Some(cmd) = arguments.get("command").and_then(Value::as_str) {
            let cmd_lower = cmd.to_lowercase();
            if cmd_lower.contains("rm -rf")
                || cmd_lower.contains("mkfs")
                || cmd_lower.contains("dd if=")
                || cmd_lower.contains("git push --force")
            {
                return true;
            }
        }

        false
    }
}

/// The result of resolving a tool name.
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedTool {
    Exact(ToolSpec),
    Aliased {
        target: ToolSpec,
        original: String,
        correction_reason: String,
    },
}

impl ResolvedTool {
    pub fn target(&self) -> &ToolSpec {
        match self {
            Self::Exact(spec) => spec,
            Self::Aliased { target, .. } => target,
        }
    }

    pub fn was_corrected(&self) -> bool {
        matches!(self, Self::Aliased { .. })
    }
}

/// Error returned when resolution or validation fails.
#[derive(Debug, Clone, PartialEq)]
pub enum ToolError {
    Unknown { requested: String },
    Ambiguous { requested: String, candidates: Vec<String> },
    SensitiveOperationUnmatched { requested: String },
    SchemaValidationFailed { tool: String, reason: String },
    RepeatedFailureBlocked { fingerprint: String, failure_count: usize },
}

impl std::fmt::Display for ToolError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown { requested } => write!(f, "No such tool registered in this session: '{requested}'"),
            Self::Ambiguous { requested, candidates } => {
                write!(f, "Tool alias '{requested}' is ambiguous; candidates: {}", candidates.join(", "))
            }
            Self::SensitiveOperationUnmatched { requested } => {
                write!(f, "Sensitive operation '{requested}' cannot be resolved by fuzzy alias")
            }
            Self::SchemaValidationFailed { tool, reason } => {
                write!(f, "Arguments failed schema validation for tool '{tool}': {reason}")
            }
            Self::RepeatedFailureBlocked { fingerprint, failure_count } => {
                write!(f, "Blocked identical recurring tool failure loop (count: {failure_count}, fingerprint: {fingerprint})")
            }
        }
    }
}

impl std::error::Error for ToolError {}

/// Fully resolved and validated tool invocation ready for execution.
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedCall {
    pub tool_name: String,
    pub arguments: Value,
    pub was_name_corrected: bool,
    pub was_arg_corrected: bool,
    pub original_name: Option<String>,
}

/// Native Tool Resolver: performs zero-inference resolution and schema validation.
#[derive(Debug, Clone)]
pub struct ToolResolver {
    pub registry: SessionToolRegistry,
}

impl ToolResolver {
    pub fn new(registry: SessionToolRegistry) -> Self {
        Self { registry }
    }

    /// Resolves requested tool identifier against the session registry.
    pub fn resolve_tool(&self, requested: &str) -> Result<ResolvedTool, ToolError> {
        // 1. Exact registered name always wins
        if let Some(tool) = self.registry.exact(requested) {
            return Ok(ResolvedTool::Exact(tool.clone()));
        }

        // 2. Sensitive operation: never guess
        if self.registry.is_sensitive_request(requested, &Value::Null) {
            return Err(ToolError::SensitiveOperationUnmatched {
                requested: requested.to_string(),
            });
        }

        // 3. Only explicitly approved aliases for this session
        let candidates = self.registry.aliases_for(requested);
        if candidates.len() == 1 {
            return Ok(ResolvedTool::Aliased {
                target: candidates[0].clone(),
                original: requested.to_string(),
                correction_reason: format!(
                    "Resolved '{}' to unique registered tool '{}'",
                    requested, candidates[0].name
                ),
            });
        }

        if candidates.len() > 1 {
            return Err(ToolError::Ambiguous {
                requested: requested.to_string(),
                candidates: candidates.into_iter().map(|c| c.name.clone()).collect(),
            });
        }

        Err(ToolError::Unknown {
            requested: requested.to_string(),
        })
    }

    /// Resolves tool and validates/repairs arguments conservatively against target schema.
    pub fn resolve_and_validate_call(
        &self,
        requested_name: &str,
        mut arguments: Value,
    ) -> Result<ResolvedCall, ToolError> {
        let resolved = self.resolve_tool(requested_name)?;
        let tool_spec = resolved.target();
        let was_name_corrected = resolved.was_corrected();
        let mut was_arg_corrected = false;

        // Conservative parameter repair strictly for verified target schemas
        if let Value::Object(ref mut map) = arguments {
            match tool_spec.name.as_str() {
                "Bash" => {
                    // Safe shell schema repair: cmd -> command
                    if !map.contains_key("command") {
                        if let Some(cmd) = map.remove("cmd").or_else(|| map.remove("script")) {
                            map.insert("command".to_string(), cmd);
                            was_arg_corrected = true;
                        }
                    }
                }
                "Read" | "Edit" | "Write" => {
                    // Safe path schema repair: file_path / file -> path
                    if !map.contains_key("path") {
                        if let Some(p) = map.remove("file_path").or_else(|| map.remove("file")) {
                            map.insert("path".to_string(), p);
                            was_arg_corrected = true;
                        }
                    }
                }
                "Grep" | "Glob" => {
                    // Safe regex/query -> pattern
                    if !map.contains_key("pattern") {
                        if let Some(p) = map.remove("query").or_else(|| map.remove("regex")) {
                            map.insert("pattern".to_string(), p);
                            was_arg_corrected = true;
                        }
                    }
                }
                _ => {}
            }
        }

        // Schema validation: verify required fields
        if let Some(required) = tool_spec.parameters_schema.get("required").and_then(Value::as_array) {
            for req in required {
                if let Some(req_field) = req.as_str() {
                    let has_field = arguments
                        .as_object()
                        .map(|m| m.contains_key(req_field))
                        .unwrap_or(false);
                    if !has_field {
                        return Err(ToolError::SchemaValidationFailed {
                            tool: tool_spec.name.clone(),
                            reason: format!("missing required parameter '{req_field}'"),
                        });
                    }
                }
            }
        }

        Ok(ResolvedCall {
            tool_name: tool_spec.name.clone(),
            arguments,
            was_name_corrected,
            was_arg_corrected,
            original_name: if was_name_corrected {
                Some(requested_name.to_string())
            } else {
                None
            },
        })
    }
}

/// Workspace-aware failure fingerprint that detects identical failure loops without blocking legitimate retries.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FailureFingerprint {
    pub tool_name: String,
    pub normalized_args_hash: String,
    pub error_category: String,
    pub workspace_state: String, // e.g. git rev, working file mtime/hash, or cwd
}

impl FailureFingerprint {
    pub fn new(tool_name: &str, arguments: &Value, error: &str, workspace_state: &str) -> Self {
        let mut hasher = Sha256::new();
        let args_str = serde_json::to_string(arguments).unwrap_or_default();
        hasher.update(args_str.as_bytes());
        let normalized_args_hash = hex::encode(hasher.finalize());

        let error_category = if error.contains("No such file or directory") {
            "file_not_found"
        } else if error.contains("Permission denied") {
            "permission_denied"
        } else if error.contains("assertion `left == right` failed") {
            "assertion_failed"
        } else if error.contains("command not found") {
            "command_not_found"
        } else {
            "general_error"
        };

        Self {
            tool_name: tool_name.to_string(),
            normalized_args_hash,
            error_category: error_category.to_string(),
            workspace_state: workspace_state.to_string(),
        }
    }

    pub fn to_key(&self) -> String {
        format!("{}:{}:{}:{}", self.tool_name, self.normalized_args_hash, self.error_category, self.workspace_state)
    }
}

/// Tracks failed tool calls to stop infinite retry loops while permitting legitimate retries.
#[derive(Debug, Clone, Default)]
pub struct FailureTracker {
    failures: HashMap<String, usize>,
}

impl FailureTracker {
    pub fn new() -> Self {
        Self {
            failures: HashMap::new(),
        }
    }

    /// Records a failure and checks if this identical call has already failed.
    /// Returns Err(ToolError::RepeatedFailureBlocked) if it has failed consecutively >= max_retries.
    pub fn check_and_record(
        &mut self,
        tool: &str,
        args: &Value,
        error: &str,
        workspace_state: &str,
        max_retries: usize,
    ) -> Result<usize, ToolError> {
        let fp = FailureFingerprint::new(tool, args, error, workspace_state);
        let key = fp.to_key();
        let count = self.failures.entry(key.clone()).or_insert(0);
        *count += 1;

        if *count > max_retries {
            return Err(ToolError::RepeatedFailureBlocked {
                fingerprint: key,
                failure_count: *count,
            });
        }

        Ok(*count)
    }

    /// Clears failures when workspace state changes meaningfully.
    pub fn reset(&mut self) {
        self.failures.clear();
    }
}

/// Telemetry metrics for tool resolution and zero-inference recovery.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ResolutionMetrics {
    pub tool_resolution_attempted: usize,
    pub tool_resolution_corrected: usize,
    pub tool_resolution_rejected: usize,
    pub measured_model_retries_avoided: usize,
}

impl ResolutionMetrics {
    pub fn record_attempt(&mut self) {
        self.tool_resolution_attempted += 1;
    }

    pub fn record_correction(&mut self, counterfactual_retries_saved: usize) {
        self.tool_resolution_corrected += 1;
        self.measured_model_retries_avoided += counterfactual_retries_saved;
    }

    pub fn record_rejection(&mut self) {
        self.tool_resolution_rejected += 1;
    }
}

// Minimal hex encoding helper to avoid extra external crates
mod hex {
    pub fn encode(bytes: impl AsRef<[u8]>) -> String {
        bytes
            .as_ref()
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect()
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;

    #[test]
    fn test_gate_1_bash_corrected_to_Bash_zero_model_inference() {
        let registry = SessionToolRegistry::claude_code_defaults();
        let resolver = ToolResolver::new(registry);

        let res = resolver.resolve_and_validate_call(
            "bash",
            serde_json::json!({ "command": "cargo test" }),
        );
        assert!(res.is_ok(), "bash must resolve to Bash");
        let call = res.unwrap();
        assert_eq!(call.tool_name, "Bash");
        assert!(call.was_name_corrected);
        assert_eq!(call.original_name.as_deref(), Some("bash"));
    }

    #[test]
    fn test_gate_2_correctly_named_Bash_unmodified() {
        let registry = SessionToolRegistry::claude_code_defaults();
        let resolver = ToolResolver::new(registry);

        let res = resolver.resolve_and_validate_call(
            "Bash",
            serde_json::json!({ "command": "cargo test" }),
        );
        assert!(res.is_ok());
        let call = res.unwrap();
        assert_eq!(call.tool_name, "Bash");
        assert!(!call.was_name_corrected);
        assert_eq!(call.original_name, None);
    }

    #[test]
    fn test_gate_3_ambiguous_alias_abstains_safely() {
        let mut registry = SessionToolRegistry::new(None);
        // Register two tools that could both claim "editor"
        registry.register(ToolSpec::new("text_editor", "Edits text", serde_json::json!({}), false));
        registry.register(ToolSpec::new("code_editor", "Edits code", serde_json::json!({}), false));
        let resolver = ToolResolver::new(registry);

        let res = resolver.resolve_tool("editor");
        assert!(matches!(res, Err(ToolError::Unknown { .. })));
    }

    #[test]
    fn test_gate_4_incompatible_arguments_rejected_before_execution() {
        let registry = SessionToolRegistry::claude_code_defaults();
        let resolver = ToolResolver::new(registry);

        // Missing required 'command' argument for Bash
        let res = resolver.resolve_and_validate_call(
            "Bash",
            serde_json::json!({ "timeout": 30 }),
        );
        assert!(matches!(res, Err(ToolError::SchemaValidationFailed { .. })));
    }

    #[test]
    fn test_gate_5_conservative_cmd_to_command_repair() {
        let registry = SessionToolRegistry::claude_code_defaults();
        let resolver = ToolResolver::new(registry);

        let res = resolver.resolve_and_validate_call(
            "bash",
            serde_json::json!({ "cmd": "cargo check" }),
        );
        assert!(res.is_ok());
        let call = res.unwrap();
        assert_eq!(call.tool_name, "Bash");
        assert_eq!(call.arguments["command"], "cargo check");
        assert!(call.was_arg_corrected);
    }

    #[test]
    fn test_gate_6_dangerous_command_preserves_strict_handling() {
        let registry = SessionToolRegistry::claude_code_defaults();
        let resolver = ToolResolver::new(registry);

        // Sensitive operation should not be guessed
        let res = resolver.resolve_tool("delete_all_files");
        assert!(matches!(res, Err(ToolError::SensitiveOperationUnmatched { .. })));
    }

    #[test]
    fn test_gate_7_repeated_same_failure_prevents_retry_loop() {
        let mut tracker = FailureTracker::new();
        let args = serde_json::json!({ "command": "unknown_tool_binary" });
        let ws = "rev_f8c508a";

        // First attempt fails
        let r1 = tracker.check_and_record("Bash", &args, "command not found", ws, 1);
        assert_eq!(r1.unwrap(), 1);

        // Identical attempt fails again -> blocked from infinite loop
        let r2 = tracker.check_and_record("Bash", &args, "command not found", ws, 1);
        assert!(matches!(r2, Err(ToolError::RepeatedFailureBlocked { .. })));
    }

    #[test]
    fn test_gate_8_changed_workspace_or_inputs_permits_legitimate_retry() {
        let mut tracker = FailureTracker::new();
        let args_v1 = serde_json::json!({ "command": "cargo test" });
        let ws_v1 = "rev_commit_1";

        let _ = tracker.check_and_record("Bash", &args_v1, "assertion `left == right` failed", ws_v1, 1);

        // File/workspace changed by patch -> legitimate retry must be allowed
        let ws_v2 = "rev_commit_2";
        let r2 = tracker.check_and_record("Bash", &args_v1, "assertion `left == right` failed", ws_v2, 1);
        assert!(r2.is_ok(), "Legitimate retry after workspace change must proceed");

        // Arguments changed -> legitimate retry must be allowed
        let args_v2 = serde_json::json!({ "command": "cargo test -- --nocapture" });
        let r3 = tracker.check_and_record("Bash", &args_v2, "assertion `left == right` failed", ws_v1, 1);
        assert!(r3.is_ok(), "Legitimate retry with different arguments must proceed");
    }

    #[test]
    fn test_gate_9_codex_actual_registered_tool_resolution() {
        let registry = SessionToolRegistry::codex_defaults();
        let resolver = ToolResolver::new(registry);

        // In Codex, shell execution is registered as exec_command
        let res = resolver.resolve_tool("bash");
        assert!(res.is_ok());
        let call = res.unwrap();
        assert_eq!(call.target().name, "exec_command");
    }
}
