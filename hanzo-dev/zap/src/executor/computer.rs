//! Computer/OS tool executor.
//!
//! Provides process execution and system operations.

use crate::error::{Error, Result};
use crate::message::ToolResult;
use crate::tools::{process, ToolCategory};
use crate::executor::{ExecutorContext, ToolExecutor};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;
use std::process::Stdio;
use std::time::Instant;
use tokio::process::Command;
use tokio::time::{timeout, Duration};

/// Computer executor for process and system operations
pub struct ComputerExecutor {
    /// Default timeout for command execution (ms)
    default_timeout_ms: u64,
    /// Maximum output size (bytes)
    max_output_size: usize,
}

impl ComputerExecutor {
    pub fn new() -> Self {
        Self {
            default_timeout_ms: 30_000,
            max_output_size: 1024 * 1024, // 1MB
        }
    }

    fn result(content: impl serde::Serialize, error: Option<String>) -> Result<ToolResult> {
        Ok(ToolResult {
            id: String::new(),
            content: serde_json::to_value(content).unwrap_or(Value::Null),
            error,
            metadata: Default::default(),
        })
    }

    async fn exec_command(&self, args: process::ExecArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let timeout_ms = args.timeout_ms
            .or(ctx.timeout_ms)
            .unwrap_or(self.default_timeout_ms);

        let cwd = args.cwd
            .as_deref()
            .or(ctx.cwd.as_deref())
            .unwrap_or(".");

        let mut cmd = Command::new(&args.command);
        cmd.args(args.args.as_ref().unwrap_or(&vec![]))
            .current_dir(cwd)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        // Set environment variables
        let mut env_map: HashMap<String, String> = ctx.env.clone();
        if let Some(env) = args.env {
            env_map.extend(env);
        }
        for (k, v) in &env_map {
            cmd.env(k, v);
        }

        // Provide stdin if specified
        if args.stdin.is_some() {
            cmd.stdin(Stdio::piped());
        }

        let start = Instant::now();

        let child = cmd.spawn()
            .map_err(|e| Error::Tool(format!("Failed to spawn {}: {}", args.command, e)))?;

        // Execute with timeout
        let result = timeout(Duration::from_millis(timeout_ms), async {
            let output = child.wait_with_output().await
                .map_err(|e| Error::Tool(format!("Command failed: {}", e)))?;
            Ok::<_, Error>(output)
        }).await;

        let duration_ms = start.elapsed().as_millis() as u64;

        match result {
            Ok(Ok(output)) => {
                let mut stdout = String::from_utf8_lossy(&output.stdout).to_string();
                let mut stderr = String::from_utf8_lossy(&output.stderr).to_string();

                // Truncate if needed
                if stdout.len() > self.max_output_size {
                    stdout.truncate(self.max_output_size);
                    stdout.push_str("\n... [truncated]");
                }
                if stderr.len() > self.max_output_size {
                    stderr.truncate(self.max_output_size);
                    stderr.push_str("\n... [truncated]");
                }

                let exec_result = process::ExecResult {
                    exit_code: output.status.code().unwrap_or(-1),
                    stdout,
                    stderr,
                    duration_ms,
                };

                let error_msg = if !output.status.success() {
                    Some(format!("Command exited with code {}", output.status.code().unwrap_or(-1)))
                } else {
                    None
                };

                Self::result(exec_result, error_msg)
            }
            Ok(Err(e)) => Err(e),
            Err(_) => Err(Error::Tool(format!(
                "Command timed out after {}ms",
                timeout_ms
            ))),
        }
    }

    async fn list_processes(&self, _ctx: &ExecutorContext) -> Result<ToolResult> {
        // Use `ps` command for cross-platform basic support
        let output = Command::new("ps")
            .args(["-eo", "pid,ppid,user,%cpu,%mem,command"])
            .output()
            .await
            .map_err(|e| Error::Tool(format!("Failed to list processes: {}", e)))?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let processes: Vec<process::ProcessInfo> = stdout
            .lines()
            .skip(1) // Skip header
            .filter_map(|line| {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 6 {
                    Some(process::ProcessInfo {
                        pid: parts[0].parse().unwrap_or(0),
                        ppid: parts[1].parse().ok(),
                        name: parts[5..].join(" "),
                        cpu_percent: parts[3].parse().ok(),
                        memory_percent: parts[4].parse().ok(),
                        status: None,
                    })
                } else {
                    None
                }
            })
            .collect();

        Self::result(processes, None)
    }

    async fn kill_process(&self, args: process::KillProcessArgs, _ctx: &ExecutorContext) -> Result<ToolResult> {
        let signal = args.signal.unwrap_or(15); // SIGTERM default

        let output = Command::new("kill")
            .args([&format!("-{}", signal), &args.pid.to_string()])
            .output()
            .await
            .map_err(|e| Error::Tool(format!("Failed to kill process: {}", e)))?;

        if output.status.success() {
            Self::result(
                serde_json::json!({"pid": args.pid, "signal": signal}),
                None
            )
        } else {
            Err(Error::Tool(format!(
                "Failed to kill process {}: {}",
                args.pid,
                String::from_utf8_lossy(&output.stderr)
            )))
        }
    }

    async fn get_env(&self, args: process::GetEnvArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        match &args.name {
            Some(name) => {
                // Get specific env var
                let value = ctx.env.get(name)
                    .cloned()
                    .or_else(|| std::env::var(name).ok());

                match value {
                    Some(val) => Self::result(val, None),
                    None => Err(Error::Tool(format!("Environment variable {} not found", name)))
                }
            }
            None => {
                // Return all env vars
                let env_map: HashMap<String, String> = std::env::vars().collect();
                Self::result(env_map, None)
            }
        }
    }
}

impl Default for ComputerExecutor {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ToolExecutor for ComputerExecutor {
    async fn execute(&self, name: &str, args: Value, ctx: &ExecutorContext) -> Result<ToolResult> {
        match name {
            "exec" => {
                let args: process::ExecArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.exec_command(args, ctx).await
            }
            "list_processes" => {
                self.list_processes(ctx).await
            }
            "kill_process" => {
                let args: process::KillProcessArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.kill_process(args, ctx).await
            }
            "get_env" => {
                let args: process::GetEnvArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.get_env(args, ctx).await
            }
            _ => Err(Error::ToolNotFound(name.to_string())),
        }
    }

    fn tools(&self) -> Vec<&'static str> {
        vec!["exec", "list_processes", "kill_process", "get_env"]
    }

    fn category(&self) -> ToolCategory {
        ToolCategory::Computer
    }
}
