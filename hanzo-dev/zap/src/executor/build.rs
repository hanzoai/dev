//! Build/Test tool executor.
//!
//! Provides build, test, lint, and validation operations.

use crate::error::{Error, Result};
use crate::message::ToolResult;
use crate::tools::{build, ToolCategory};
use crate::executor::{ExecutorContext, ToolExecutor};
use async_trait::async_trait;
use serde_json::Value;
use std::time::Instant;
use tokio::process::Command;

/// Build executor for compilation and testing
pub struct BuildExecutor;

impl BuildExecutor {
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

    async fn build(&self, args: build::BuildArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = ctx.cwd.as_deref().unwrap_or(".");

        // Detect build system
        let (cmd, cmd_args) = self.detect_build_command(cwd, &args).await?;

        let start = Instant::now();
        let output = Command::new(&cmd)
            .args(&cmd_args)
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("Build failed: {}", e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;
        let success = output.status.success();

        let result = build::BuildResult {
            success,
            duration_ms,
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            artifacts: vec![], // Would need to parse build output for artifacts
        };

        let error = if success { None } else { Some("Build failed".to_string()) };
        Self::result(result, error)
    }

    async fn detect_build_command(&self, cwd: &str, args: &build::BuildArgs) -> Result<(String, Vec<String>)> {
        use tokio::fs;

        // Check for various build files
        if fs::metadata(format!("{}/Cargo.toml", cwd)).await.is_ok() {
            let mut cmd_args = vec!["build".to_string()];
            if args.release.unwrap_or(false) {
                cmd_args.push("--release".to_string());
            }
            if let Some(ref target) = args.target {
                cmd_args.push("--target".to_string());
                cmd_args.push(target.clone());
            }
            return Ok(("cargo".to_string(), cmd_args));
        }

        if fs::metadata(format!("{}/package.json", cwd)).await.is_ok() {
            let script = args.target.as_deref().unwrap_or("build");
            return Ok(("npm".to_string(), vec!["run".to_string(), script.to_string()]));
        }

        if fs::metadata(format!("{}/go.mod", cwd)).await.is_ok() {
            let mut cmd_args = vec!["build".to_string()];
            if let Some(ref target) = args.target {
                cmd_args.push(target.clone());
            } else {
                cmd_args.push("./...".to_string());
            }
            return Ok(("go".to_string(), cmd_args));
        }

        if fs::metadata(format!("{}/Makefile", cwd)).await.is_ok() {
            let target = args.target.clone().unwrap_or_default();
            return Ok(("make".to_string(), if target.is_empty() { vec![] } else { vec![target] }));
        }

        Err(Error::Tool("Could not detect build system".to_string()))
    }

    async fn test(&self, args: build::TestArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = ctx.cwd.as_deref().unwrap_or(".");

        // Detect test command
        let (cmd, cmd_args) = self.detect_test_command(cwd, &args).await?;

        let start = Instant::now();
        let output = Command::new(&cmd)
            .args(&cmd_args)
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("Test failed: {}", e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;
        let success = output.status.success();

        let result = build::TestResult {
            success,
            duration_ms,
            passed: 0,  // Would need to parse output
            failed: 0,
            skipped: 0,
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        };

        let error = if success { None } else { Some("Tests failed".to_string()) };
        Self::result(result, error)
    }

    async fn detect_test_command(&self, cwd: &str, args: &build::TestArgs) -> Result<(String, Vec<String>)> {
        use tokio::fs;

        if fs::metadata(format!("{}/Cargo.toml", cwd)).await.is_ok() {
            let mut cmd_args = vec!["test".to_string()];
            if let Some(ref filter) = args.filter {
                cmd_args.push(filter.clone());
            }
            if args.verbose.unwrap_or(false) {
                cmd_args.push("--".to_string());
                cmd_args.push("--nocapture".to_string());
            }
            return Ok(("cargo".to_string(), cmd_args));
        }

        if fs::metadata(format!("{}/package.json", cwd)).await.is_ok() {
            return Ok(("npm".to_string(), vec!["test".to_string()]));
        }

        if fs::metadata(format!("{}/go.mod", cwd)).await.is_ok() {
            let mut cmd_args = vec!["test".to_string()];
            if args.verbose.unwrap_or(false) {
                cmd_args.push("-v".to_string());
            }
            cmd_args.push("./...".to_string());
            return Ok(("go".to_string(), cmd_args));
        }

        if fs::metadata(format!("{}/pyproject.toml", cwd)).await.is_ok()
            || fs::metadata(format!("{}/pytest.ini", cwd)).await.is_ok()
        {
            let mut cmd_args = vec!["-m".to_string(), "pytest".to_string()];
            if args.verbose.unwrap_or(false) {
                cmd_args.push("-v".to_string());
            }
            if let Some(ref filter) = args.filter {
                cmd_args.push("-k".to_string());
                cmd_args.push(filter.clone());
            }
            return Ok(("python".to_string(), cmd_args));
        }

        Err(Error::Tool("Could not detect test framework".to_string()))
    }

    async fn lint(&self, args: build::LintArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = ctx.cwd.as_deref().unwrap_or(".");

        // Detect lint command
        let (cmd, cmd_args) = self.detect_lint_command(cwd, &args).await?;

        let output = Command::new(&cmd)
            .args(&cmd_args)
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("Lint failed: {}", e)))?;

        let success = output.status.success();

        let result = build::LintResult {
            success,
            warnings: vec![], // Would parse from output
            errors: vec![],
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        };

        let error = if success { None } else { Some("Lint errors found".to_string()) };
        Self::result(result, error)
    }

    async fn detect_lint_command(&self, cwd: &str, args: &build::LintArgs) -> Result<(String, Vec<String>)> {
        use tokio::fs;

        if fs::metadata(format!("{}/Cargo.toml", cwd)).await.is_ok() {
            let mut cmd_args = vec!["clippy".to_string()];
            if args.fix.unwrap_or(false) {
                cmd_args.push("--fix".to_string());
            }
            return Ok(("cargo".to_string(), cmd_args));
        }

        if fs::metadata(format!("{}/package.json", cwd)).await.is_ok() {
            let mut cmd_args = vec!["run".to_string(), "lint".to_string()];
            if args.fix.unwrap_or(false) {
                cmd_args.push("--".to_string());
                cmd_args.push("--fix".to_string());
            }
            return Ok(("npm".to_string(), cmd_args));
        }

        if fs::metadata(format!("{}/go.mod", cwd)).await.is_ok() {
            return Ok(("golangci-lint".to_string(), vec!["run".to_string()]));
        }

        if fs::metadata(format!("{}/pyproject.toml", cwd)).await.is_ok() {
            let mut cmd_args = vec!["check".to_string(), ".".to_string()];
            if args.fix.unwrap_or(false) {
                cmd_args = vec!["format".to_string(), ".".to_string()];
            }
            return Ok(("ruff".to_string(), cmd_args));
        }

        Err(Error::Tool("Could not detect linter".to_string()))
    }

    async fn typecheck(&self, _args: build::TypecheckArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let cwd = ctx.cwd.as_deref().unwrap_or(".");
        use tokio::fs;

        // Detect typecheck command
        let (cmd, cmd_args) = if fs::metadata(format!("{}/tsconfig.json", cwd)).await.is_ok() {
            ("npx".to_string(), vec!["tsc".to_string(), "--noEmit".to_string()])
        } else if fs::metadata(format!("{}/pyproject.toml", cwd)).await.is_ok() {
            ("mypy".to_string(), vec![".".to_string()])
        } else if fs::metadata(format!("{}/go.mod", cwd)).await.is_ok() {
            ("go".to_string(), vec!["vet".to_string(), "./...".to_string()])
        } else {
            return Err(Error::Tool("Could not detect type checker".to_string()));
        };

        let output = Command::new(&cmd)
            .args(&cmd_args)
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| Error::Tool(format!("Typecheck failed: {}", e)))?;

        let success = output.status.success();

        let result = build::TypecheckResult {
            success,
            errors: vec![], // Would parse from output
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        };

        let error = if success { None } else { Some("Type errors found".to_string()) };
        Self::result(result, error)
    }
}

impl Default for BuildExecutor {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ToolExecutor for BuildExecutor {
    async fn execute(&self, name: &str, args: Value, ctx: &ExecutorContext) -> Result<ToolResult> {
        match name {
            "build" => {
                let args: build::BuildArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.build(args, ctx).await
            }
            "test" => {
                let args: build::TestArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.test(args, ctx).await
            }
            "lint" => {
                let args: build::LintArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.lint(args, ctx).await
            }
            "typecheck" => {
                let args: build::TypecheckArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.typecheck(args, ctx).await
            }
            _ => Err(Error::ToolNotFound(name.to_string())),
        }
    }

    fn tools(&self) -> Vec<&'static str> {
        vec!["build", "test", "lint", "typecheck"]
    }

    fn category(&self) -> ToolCategory {
        ToolCategory::Build
    }
}
