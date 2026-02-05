//! VCS (Version Control System) tool executor.
//!
//! Provides git operations: status, diff, commit, log, etc.

use crate::error::{Error, Result};
use crate::executor::{ExecutorContext, ToolExecutor};
use crate::message::ToolResult;
use crate::tools::{ToolCategory, vcs};
use async_trait::async_trait;
use serde_json::Value;
use tokio::process::Command;

/// VCS executor for git operations
pub struct VcsExecutor;

impl VcsExecutor {
    pub fn new() -> Self {
        Self
    }

    fn result(content: impl serde::Serialize, error: Option<String>) -> Result<ToolResult> {
        Ok(ToolResult {
            id: String::new(),
            content: serde_json::to_value(content).unwrap_or(Value::Null),
            error,
            metadata: Default::default(),
        })
    }

    async fn git_status(&self, args: vcs::StatusArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = args.path.as_deref().or(ctx.cwd.as_deref()).unwrap_or(".");

        let output = Command::new("git")
            .args(["status", "--porcelain=v2", "--branch"])
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("git status failed: {}", e)))?;

        if !output.status.success() {
            return Err(Error::Tool(format!(
                "git status failed: {}",
                String::from_utf8_lossy(&output.stderr)
            )));
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut branch = String::new();
        let mut staged: Vec<String> = Vec::new();
        let mut unstaged: Vec<String> = Vec::new();
        let mut untracked: Vec<String> = Vec::new();

        for line in stdout.lines() {
            if line.starts_with("# branch.head") {
                branch = line.split_whitespace().last().unwrap_or("").to_string();
            } else if line.starts_with('1') || line.starts_with('2') {
                // Changed entries
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 9 {
                    let xy = parts[1];
                    let path = parts[8..].join(" ");
                    if !xy.starts_with('.') {
                        staged.push(path.clone());
                    }
                    if !xy.ends_with('.') {
                        unstaged.push(path);
                    }
                }
            } else if line.starts_with('?') {
                // Untracked
                let path = line[2..].to_string();
                untracked.push(path);
            }
        }

        let status = vcs::StatusResult {
            branch,
            staged,
            unstaged,
            untracked,
            ahead: None,
            behind: None,
        };

        Self::result(status, None)
    }

    async fn git_diff(&self, args: vcs::DiffArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = ctx.cwd.as_deref().unwrap_or(".");

        let mut cmd_args = vec!["diff"];
        if args.staged.unwrap_or(false) {
            cmd_args.push("--staged");
        }
        if let Some(ref commit) = args.commit {
            cmd_args.push(commit);
        }
        if let Some(ref path) = args.path {
            cmd_args.push("--");
            cmd_args.push(path);
        }

        let output = Command::new("git")
            .args(&cmd_args)
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("git diff failed: {}", e)))?;

        let diff = String::from_utf8_lossy(&output.stdout).to_string();

        Self::result(diff, None)
    }

    async fn git_commit(&self, args: vcs::CommitArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = ctx.cwd.as_deref().unwrap_or(".");

        // Stage files if specified
        if let Some(files) = &args.files {
            let mut add_args = vec!["add"];
            add_args.extend(files.iter().map(String::as_str));

            let add_output = Command::new("git")
                .args(&add_args)
                .current_dir(cwd)
                .output()
                .await
                .map_err(|e| Error::Tool(format!("git add failed: {}", e)))?;

            if !add_output.status.success() {
                return Err(Error::Tool(format!(
                    "git add failed: {}",
                    String::from_utf8_lossy(&add_output.stderr)
                )));
            }
        }

        let mut cmd_args = vec!["commit", "-m", &args.message];
        if args.amend.unwrap_or(false) {
            cmd_args.push("--amend");
        }

        let output = Command::new("git")
            .args(&cmd_args)
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("git commit failed: {}", e)))?;

        if !output.status.success() {
            return Err(Error::Tool(format!(
                "git commit failed: {}",
                String::from_utf8_lossy(&output.stderr)
            )));
        }

        // Get the commit hash
        let hash_output = Command::new("git")
            .args(["rev-parse", "HEAD"])
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("git rev-parse failed: {}", e)))?;

        let commit_hash = String::from_utf8_lossy(&hash_output.stdout)
            .trim()
            .to_string();

        let result = vcs::CommitResult {
            commit: commit_hash,
            message: args.message.clone(),
        };

        Self::result(result, None)
    }

    async fn git_log(&self, args: vcs::LogArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = ctx.cwd.as_deref().unwrap_or(".");
        let limit = args.limit.unwrap_or(10);

        let format = "--format=%H|%an|%ae|%at|%s";
        let limit_arg = format!("-{}", limit);
        let mut cmd_args = vec!["log", format, &limit_arg];

        if let Some(ref path) = args.path {
            cmd_args.push("--");
            cmd_args.push(path);
        }

        let output = Command::new("git")
            .args(&cmd_args)
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("git log failed: {}", e)))?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let commits: Vec<vcs::LogEntry> = stdout
            .lines()
            .filter_map(|line| {
                let parts: Vec<&str> = line.splitn(5, '|').collect();
                if parts.len() >= 5 {
                    Some(vcs::LogEntry {
                        commit: parts[0].to_string(),
                        author: parts[1].to_string(),
                        email: parts[2].to_string(),
                        timestamp: parts[3].to_string(),
                        message: parts[4].to_string(),
                    })
                } else {
                    None
                }
            })
            .collect();

        Self::result(commits, None)
    }

    async fn git_blame(&self, args: vcs::BlameArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = ctx.cwd.as_deref().unwrap_or(".");

        let mut cmd_args = vec!["blame", "--line-porcelain"];

        let range_arg;
        if let Some(start) = args.start_line {
            if let Some(end) = args.end_line {
                range_arg = format!("{},{}", start, end);
                cmd_args.push("-L");
                cmd_args.push(&range_arg);
            }
        }
        cmd_args.push(&args.path);

        let output = Command::new("git")
            .args(&cmd_args)
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("git blame failed: {}", e)))?;

        // Return raw blame output for now - can be parsed further if needed
        let blame = String::from_utf8_lossy(&output.stdout).to_string();

        Self::result(blame, None)
    }

    async fn git_branch(&self, args: vcs::BranchArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = ctx.cwd.as_deref().unwrap_or(".");

        match args.action {
            vcs::BranchAction::List => {
                let output = Command::new("git")
                    .args([
                        "branch",
                        "-a",
                        "--format=%(refname:short)|%(upstream:short)|%(HEAD)",
                    ])
                    .current_dir(cwd)
                    .output()
                    .await
                    .map_err(|e| Error::Tool(format!("git branch failed: {}", e)))?;

                let stdout = String::from_utf8_lossy(&output.stdout);
                let branches: Vec<vcs::BranchInfo> = stdout
                    .lines()
                    .map(|line| {
                        let parts: Vec<&str> = line.splitn(3, '|').collect();
                        vcs::BranchInfo {
                            name: parts.first().unwrap_or(&"").to_string(),
                            upstream: parts
                                .get(1)
                                .filter(|s| !s.is_empty())
                                .map(|s| s.to_string()),
                            is_current: parts.get(2).map(|s| s.contains('*')).unwrap_or(false),
                        }
                    })
                    .collect();

                Self::result(branches, None)
            }
            vcs::BranchAction::Create => {
                let name = args
                    .name
                    .ok_or_else(|| Error::Tool("Branch name required".into()))?;
                let mut cmd_args = vec!["checkout", "-b", &name];
                if let Some(ref from) = args.from {
                    cmd_args.push(from);
                }

                let output = Command::new("git")
                    .args(&cmd_args)
                    .current_dir(cwd)
                    .output()
                    .await
                    .map_err(|e| Error::Tool(format!("git checkout -b failed: {}", e)))?;

                if !output.status.success() {
                    return Err(Error::Tool(format!(
                        "git checkout -b failed: {}",
                        String::from_utf8_lossy(&output.stderr)
                    )));
                }

                Self::result(
                    serde_json::json!({"action": "created", "branch": name}),
                    None,
                )
            }
            vcs::BranchAction::Delete => {
                let name = args
                    .name
                    .ok_or_else(|| Error::Tool("Branch name required".into()))?;
                let flag = if args.force.unwrap_or(false) {
                    "-D"
                } else {
                    "-d"
                };

                let output = Command::new("git")
                    .args(["branch", flag, &name])
                    .current_dir(cwd)
                    .output()
                    .await
                    .map_err(|e| Error::Tool(format!("git branch delete failed: {}", e)))?;

                if !output.status.success() {
                    return Err(Error::Tool(format!(
                        "git branch delete failed: {}",
                        String::from_utf8_lossy(&output.stderr)
                    )));
                }

                Self::result(
                    serde_json::json!({"action": "deleted", "branch": name}),
                    None,
                )
            }
            vcs::BranchAction::Switch => {
                let name = args
                    .name
                    .ok_or_else(|| Error::Tool("Branch name required".into()))?;

                let output = Command::new("git")
                    .args(["checkout", &name])
                    .current_dir(cwd)
                    .output()
                    .await
                    .map_err(|e| Error::Tool(format!("git checkout failed: {}", e)))?;

                if !output.status.success() {
                    return Err(Error::Tool(format!(
                        "git checkout failed: {}",
                        String::from_utf8_lossy(&output.stderr)
                    )));
                }

                Self::result(
                    serde_json::json!({"action": "switched", "branch": name}),
                    None,
                )
            }
        }
    }
}

impl Default for VcsExecutor {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ToolExecutor for VcsExecutor {
    async fn execute(&self, name: &str, args: Value, ctx: &ExecutorContext) -> Result<ToolResult> {
        match name {
            "git_status" => {
                let args: vcs::StatusArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.git_status(args, ctx).await
            }
            "git_diff" => {
                let args: vcs::DiffArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.git_diff(args, ctx).await
            }
            "git_commit" => {
                let args: vcs::CommitArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.git_commit(args, ctx).await
            }
            "git_log" => {
                let args: vcs::LogArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.git_log(args, ctx).await
            }
            "git_blame" => {
                let args: vcs::BlameArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.git_blame(args, ctx).await
            }
            "git_branch" => {
                let args: vcs::BranchArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.git_branch(args, ctx).await
            }
            _ => Err(Error::ToolNotFound(name.to_string())),
        }
    }

    fn tools(&self) -> Vec<&'static str> {
        vec![
            "git_status",
            "git_diff",
            "git_commit",
            "git_log",
            "git_blame",
            "git_branch",
        ]
    }

    fn category(&self) -> ToolCategory {
        ToolCategory::Vcs
    }
}
