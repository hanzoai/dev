//! Planning/Orchestration tool executor.
//!
//! Provides intent creation, routing, DAG composition, and execution.

use crate::error::{Error, Result};
use crate::executor::{ExecutorContext, ToolExecutor};
use crate::message::ToolResult;
use crate::tools::{ToolCategory, plan};
use async_trait::async_trait;
use serde_json::Value;
use std::sync::atomic::{AtomicU64, Ordering};

/// Plan executor for orchestration operations
pub struct PlanExecutor {
    /// Counter for generating IDs
    id_counter: AtomicU64,
}

impl PlanExecutor {
    pub fn new() -> Self {
        Self {
            id_counter: AtomicU64::new(1),
        }
    }

    fn next_id(&self, prefix: &str) -> String {
        let id = self.id_counter.fetch_add(1, Ordering::SeqCst);
        format!("{}-{}", prefix, id)
    }

    fn result(content: impl serde::Serialize, error: Option<String>) -> Result<ToolResult> {
        Ok(ToolResult {
            id: String::new(),
            content: serde_json::to_value(content).unwrap_or(Value::Null),
            error,
            metadata: Default::default(),
        })
    }

    async fn create_intent(
        &self,
        args: plan::IntentArgs,
        _ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        // Parse the description to extract goals
        let goals: Vec<plan::Goal> = args
            .description
            .split(|c| c == '.' || c == ';' || c == '\n')
            .filter(|s| !s.trim().is_empty())
            .enumerate()
            .map(|(i, s)| plan::Goal {
                id: format!("goal-{}", i + 1),
                description: s.trim().to_string(),
                priority: 1,
                constraints: args.constraints.clone(),
            })
            .collect();

        let intent = plan::Intent {
            id: self.next_id("intent"),
            description: args.description,
            goals,
            context: args.context.unwrap_or_default(),
            created_at: chrono::Utc::now().to_rfc3339(),
        };

        Self::result(intent, None)
    }

    async fn create_route(
        &self,
        args: plan::RouteArgs,
        _ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        let intent: plan::Intent = serde_json::from_value(args.intent)
            .map_err(|e| Error::Tool(format!("Invalid intent: {}", e)))?;

        // Create steps from goals
        let steps: Vec<plan::Step> = intent
            .goals
            .iter()
            .enumerate()
            .map(|(i, goal)| plan::Step {
                id: format!("step-{}", i + 1),
                tool: self.suggest_tool_for_goal(&goal.description, &args.available_tools),
                args: serde_json::json!({}),
                depends_on: if i > 0 {
                    vec![format!("step-{}", i)]
                } else {
                    vec![]
                },
                description: goal.description.clone(),
            })
            .collect();

        let route = plan::Route {
            id: self.next_id("route"),
            intent_id: intent.id,
            steps,
            estimated_duration_ms: None,
        };

        Self::result(route, None)
    }

    fn suggest_tool_for_goal(
        &self,
        description: &str,
        available_tools: &Option<Vec<String>>,
    ) -> String {
        let desc_lower = description.to_lowercase();

        // Simple keyword matching for tool suggestion
        let suggested = if desc_lower.contains("read")
            || desc_lower.contains("file")
            || desc_lower.contains("open")
        {
            "read_file"
        } else if desc_lower.contains("write")
            || desc_lower.contains("save")
            || desc_lower.contains("create file")
        {
            "write_file"
        } else if desc_lower.contains("edit")
            || desc_lower.contains("modify")
            || desc_lower.contains("change")
        {
            "edit_file"
        } else if desc_lower.contains("search")
            || desc_lower.contains("find")
            || desc_lower.contains("grep")
        {
            "grep"
        } else if desc_lower.contains("list")
            || desc_lower.contains("dir")
            || desc_lower.contains("folder")
        {
            "list_dir"
        } else if desc_lower.contains("run")
            || desc_lower.contains("execute")
            || desc_lower.contains("command")
        {
            "exec"
        } else if desc_lower.contains("git") || desc_lower.contains("commit") {
            "git_commit"
        } else if desc_lower.contains("test") {
            "test"
        } else if desc_lower.contains("build") || desc_lower.contains("compile") {
            "build"
        } else if desc_lower.contains("http")
            || desc_lower.contains("request")
            || desc_lower.contains("api")
        {
            "http_request"
        } else {
            "exec"
        };

        // Verify tool is available
        if let Some(tools) = available_tools {
            if tools.contains(&suggested.to_string()) {
                return suggested.to_string();
            }
            // Return first available tool as fallback
            return tools
                .first()
                .cloned()
                .unwrap_or_else(|| suggested.to_string());
        }

        suggested.to_string()
    }

    async fn compose_dag(
        &self,
        args: plan::ComposeArgs,
        _ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        let routes: Vec<plan::Route> = serde_json::from_value(args.routes)
            .map_err(|e| Error::Tool(format!("Invalid routes: {}", e)))?;

        let mut nodes: Vec<plan::DagNode> = Vec::new();
        let mut edges: Vec<plan::DagEdge> = Vec::new();

        for route in &routes {
            for step in &route.steps {
                nodes.push(plan::DagNode {
                    id: step.id.clone(),
                    tool: step.tool.clone(),
                    args: step.args.clone(),
                    route_id: route.id.clone(),
                });

                for dep in &step.depends_on {
                    edges.push(plan::DagEdge {
                        from: dep.clone(),
                        to: step.id.clone(),
                    });
                }
            }
        }

        let dag = plan::Dag {
            id: self.next_id("dag"),
            nodes,
            edges,
            parallel: args.parallel.unwrap_or(true),
        };

        Self::result(dag, None)
    }

    async fn cache_lookup(
        &self,
        args: plan::CacheLookupArgs,
        _ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        // Placeholder - would integrate with actual cache store
        let result = plan::CacheResult {
            hit: false,
            key: args.key,
            value: None,
            ttl_remaining_ms: None,
        };

        Self::result(result, None)
    }

    async fn audit_log(
        &self,
        args: plan::AuditLogArgs,
        _ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        // Log the audit event (would persist to actual audit store)
        let entry = plan::AuditEntry {
            id: self.next_id("audit"),
            timestamp: chrono::Utc::now().to_rfc3339(),
            action: args.action,
            tool: args.tool,
            args: args.args,
            result: args.result,
            user: args.user,
            session_id: args.session_id,
        };

        // In production, would persist this entry
        tracing::info!(
            audit_id = %entry.id,
            action = %entry.action,
            tool = ?entry.tool,
            "Audit log entry created"
        );

        Self::result(entry, None)
    }
}

impl Default for PlanExecutor {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ToolExecutor for PlanExecutor {
    async fn execute(&self, name: &str, args: Value, ctx: &ExecutorContext) -> Result<ToolResult> {
        match name {
            "plan_intent" => {
                let args: plan::IntentArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.create_intent(args, ctx).await
            }
            "plan_route" => {
                let args: plan::RouteArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.create_route(args, ctx).await
            }
            "plan_compose" => {
                let args: plan::ComposeArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.compose_dag(args, ctx).await
            }
            "cache_lookup" => {
                let args: plan::CacheLookupArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.cache_lookup(args, ctx).await
            }
            "audit_log" => {
                let args: plan::AuditLogArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.audit_log(args, ctx).await
            }
            _ => Err(Error::ToolNotFound(name.to_string())),
        }
    }

    fn tools(&self) -> Vec<&'static str> {
        vec![
            "plan_intent",
            "plan_route",
            "plan_compose",
            "cache_lookup",
            "audit_log",
        ]
    }

    fn category(&self) -> ToolCategory {
        ToolCategory::Plan
    }
}
