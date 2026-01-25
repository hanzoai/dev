//! Filesystem tool executor.
//!
//! Provides file operations: read, write, edit, glob, grep, etc.

use crate::error::{Error, Result};
use crate::message::ToolResult;
use crate::tools::{filesystem, ToolCategory};
use crate::executor::{ExecutorContext, ToolExecutor};
use async_trait::async_trait;
use serde_json::Value;
use std::path::Path;
use tokio::fs;

/// Filesystem executor for file operations
pub struct FilesystemExecutor {
    /// Maximum file size to read (default 10MB)
    max_read_size: usize,
}

impl FilesystemExecutor {
    pub fn new() -> Self {
        Self {
            max_read_size: 10 * 1024 * 1024,
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

    async fn read_file(&self, args: filesystem::ReadFileArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let path = self.resolve_path(&args.path, ctx);

        let metadata = fs::metadata(&path).await
            .map_err(|e| Error::Tool(format!("Cannot access {}: {}", path, e)))?;

        if metadata.len() > self.max_read_size as u64 {
            return Err(Error::Tool(format!(
                "File too large: {} bytes (max {})",
                metadata.len(),
                self.max_read_size
            )));
        }

        let content = fs::read_to_string(&path).await
            .map_err(|e| Error::Tool(format!("Failed to read {}: {}", path, e)))?;

        // Apply offset/limit if specified
        let lines: Vec<&str> = content.lines().collect();
        let offset = args.offset.unwrap_or(0) as usize;
        let limit = args.limit.unwrap_or(lines.len() as u64) as usize;

        let selected: Vec<&str> = lines
            .into_iter()
            .skip(offset)
            .take(limit)
            .collect();

        Self::result(selected.join("\n"), None)
    }

    async fn write_file(&self, args: filesystem::WriteFileArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let path = self.resolve_path(&args.path, ctx);

        // Create parent directories if needed
        if let Some(parent) = Path::new(&path).parent() {
            fs::create_dir_all(parent).await
                .map_err(|e| Error::Tool(format!("Failed to create directories: {}", e)))?;
        }

        fs::write(&path, &args.content).await
            .map_err(|e| Error::Tool(format!("Failed to write {}: {}", path, e)))?;

        Self::result(
            serde_json::json!({"bytes_written": args.content.len(), "path": path}),
            None
        )
    }

    async fn edit_file(&self, args: filesystem::EditFileArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let path = self.resolve_path(&args.path, ctx);

        let content = fs::read_to_string(&path).await
            .map_err(|e| Error::Tool(format!("Failed to read {}: {}", path, e)))?;

        let mut new_content = content.clone();
        let mut changes = 0;

        for edit in &args.edits {
            if new_content.contains(&edit.old_text) {
                new_content = new_content.replacen(&edit.old_text, &edit.new_text, 1);
                changes += 1;
            } else {
                return Err(Error::Tool(format!(
                    "old_text not found in file: {:?}",
                    edit.old_text.chars().take(50).collect::<String>()
                )));
            }
        }

        fs::write(&path, &new_content).await
            .map_err(|e| Error::Tool(format!("Failed to write {}: {}", path, e)))?;

        Self::result(serde_json::json!({"edits_applied": changes, "path": path}), None)
    }

    async fn glob_files(&self, args: filesystem::GlobArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let base = args.base_path.as_deref()
            .map(|p| self.resolve_path(p, ctx))
            .unwrap_or_else(|| ctx.cwd.clone().unwrap_or_else(|| ".".to_string()));

        let pattern = format!("{}/{}", base, args.pattern);
        let max = args.max_results.unwrap_or(1000) as usize;

        let mut matches = Vec::new();
        for entry in glob::glob(&pattern).map_err(|e| Error::Tool(format!("Invalid glob: {}", e)))? {
            if matches.len() >= max {
                break;
            }
            if let Ok(path) = entry {
                matches.push(path.display().to_string());
            }
        }

        Self::result(matches, None)
    }

    async fn grep_files(&self, args: filesystem::GrepArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let path = self.resolve_path(&args.path, ctx);
        let regex = regex::Regex::new(&args.pattern)
            .map_err(|e| Error::Tool(format!("Invalid regex: {}", e)))?;

        let max = args.max_results.unwrap_or(100) as usize;
        let context_lines = args.context_lines.unwrap_or(0) as usize;

        let mut results: Vec<filesystem::GrepMatch> = Vec::new();

        // Collect files to search
        let files: Vec<String> = if Path::new(&path).is_dir() {
            if args.recursive.unwrap_or(true) {
                walkdir::WalkDir::new(&path)
                    .into_iter()
                    .filter_map(|e| e.ok())
                    .filter(|e| e.file_type().is_file())
                    .map(|e| e.path().display().to_string())
                    .collect()
            } else {
                std::fs::read_dir(&path)
                    .map_err(|e| Error::Tool(format!("Cannot read dir: {}", e)))?
                    .filter_map(|e| e.ok())
                    .filter(|e| e.file_type().map(|t| t.is_file()).unwrap_or(false))
                    .map(|e| e.path().display().to_string())
                    .collect()
            }
        } else {
            vec![path.clone()]
        };

        'outer: for file_path in files {
            let content = match fs::read_to_string(&file_path).await {
                Ok(c) => c,
                Err(_) => continue, // Skip binary/unreadable files
            };

            let lines: Vec<&str> = content.lines().collect();
            for (i, line) in lines.iter().enumerate() {
                if regex.is_match(line) {
                    let context_before: Vec<String> = lines
                        .iter()
                        .skip(i.saturating_sub(context_lines))
                        .take(context_lines)
                        .map(|s| s.to_string())
                        .collect();

                    let context_after: Vec<String> = lines
                        .iter()
                        .skip(i + 1)
                        .take(context_lines)
                        .map(|s| s.to_string())
                        .collect();

                    results.push(filesystem::GrepMatch {
                        path: file_path.clone(),
                        line_number: (i + 1) as u32,
                        line: line.to_string(),
                        context_before: if context_before.is_empty() { None } else { Some(context_before) },
                        context_after: if context_after.is_empty() { None } else { Some(context_after) },
                    });

                    if results.len() >= max {
                        break 'outer;
                    }
                }
            }
        }

        Self::result(results, None)
    }

    async fn list_dir(&self, args: filesystem::ListDirArgs, ctx: &ExecutorContext) -> Result<ToolResult> {
        let path = self.resolve_path(&args.path, ctx);
        let show_hidden = args.show_hidden.unwrap_or(false);
        let recursive = args.recursive.unwrap_or(false);

        let mut entries: Vec<filesystem::DirEntry> = Vec::new();

        if recursive {
            for entry in walkdir::WalkDir::new(&path).max_depth(args.max_depth.unwrap_or(10) as usize) {
                if let Ok(e) = entry {
                    let name = e.file_name().to_string_lossy().to_string();
                    if !show_hidden && name.starts_with('.') {
                        continue;
                    }
                    let metadata = e.metadata().ok();
                    entries.push(filesystem::DirEntry {
                        name,
                        path: e.path().display().to_string(),
                        is_dir: e.file_type().is_dir(),
                        size: metadata.as_ref().map(|m| m.len()),
                        modified: None, // Would need chrono for proper formatting
                    });
                }
            }
        } else {
            let mut read_dir = fs::read_dir(&path).await
                .map_err(|e| Error::Tool(format!("Cannot read dir: {}", e)))?;

            while let Some(entry) = read_dir.next_entry().await
                .map_err(|e| Error::Tool(format!("Error reading entry: {}", e)))?
            {
                let name = entry.file_name().to_string_lossy().to_string();
                if !show_hidden && name.starts_with('.') {
                    continue;
                }
                let metadata = entry.metadata().await.ok();
                let file_type = entry.file_type().await.ok();
                entries.push(filesystem::DirEntry {
                    name,
                    path: entry.path().display().to_string(),
                    is_dir: file_type.map(|t| t.is_dir()).unwrap_or(false),
                    size: metadata.as_ref().map(|m| m.len()),
                    modified: None,
                });
            }
        }

        Self::result(entries, None)
    }

    fn resolve_path(&self, path: &str, ctx: &ExecutorContext) -> String {
        if Path::new(path).is_absolute() {
            path.to_string()
        } else if let Some(cwd) = &ctx.cwd {
            format!("{}/{}", cwd, path)
        } else {
            path.to_string()
        }
    }
}

impl Default for FilesystemExecutor {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ToolExecutor for FilesystemExecutor {
    async fn execute(&self, name: &str, args: Value, ctx: &ExecutorContext) -> Result<ToolResult> {
        match name {
            "read_file" => {
                let args: filesystem::ReadFileArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.read_file(args, ctx).await
            }
            "write_file" => {
                let args: filesystem::WriteFileArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.write_file(args, ctx).await
            }
            "edit_file" => {
                let args: filesystem::EditFileArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.edit_file(args, ctx).await
            }
            "glob" => {
                let args: filesystem::GlobArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.glob_files(args, ctx).await
            }
            "grep" => {
                let args: filesystem::GrepArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.grep_files(args, ctx).await
            }
            "list_dir" => {
                let args: filesystem::ListDirArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.list_dir(args, ctx).await
            }
            _ => Err(Error::ToolNotFound(name.to_string())),
        }
    }

    fn tools(&self) -> Vec<&'static str> {
        vec!["read_file", "write_file", "edit_file", "glob", "grep", "list_dir"]
    }

    fn category(&self) -> ToolCategory {
        ToolCategory::Computer
    }
}
