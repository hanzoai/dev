//! Distributed compute cluster - matches Python's cluster module

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use crate::Result;

/// Compute node in the cluster
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Node {
    pub id: String,
    pub address: String,
    pub capabilities: Vec<String>,
    pub status: NodeStatus,
    pub resources: Resources,
}

/// Node status
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum NodeStatus {
    Online,
    Offline,
    Busy,
    Maintenance,
}

/// Available resources on a node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resources {
    pub cpu_cores: u32,
    pub memory_gb: f32,
    pub gpu_count: u32,
    pub gpu_memory_gb: Option<f32>,
}

/// Job to be executed on the cluster
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Job {
    pub id: String,
    pub task_type: String,
    pub payload: serde_json::Value,
    pub requirements: JobRequirements,
}

/// Requirements for job execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobRequirements {
    pub min_cpu_cores: Option<u32>,
    pub min_memory_gb: Option<f32>,
    pub requires_gpu: bool,
    pub preferred_nodes: Option<Vec<String>>,
}

/// Result of job execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobResult {
    pub job_id: String,
    pub status: JobStatus,
    pub output: Option<serde_json::Value>,
    pub error: Option<String>,
    pub execution_time_ms: u64,
}

/// Job execution status
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum JobStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// Cluster manager for distributed compute
pub struct Cluster {
    nodes: HashMap<String, Node>,
    jobs: HashMap<String, Job>,
}

impl Cluster {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            jobs: HashMap::new(),
        }
    }

    /// Register a compute node
    pub fn register_node(&mut self, node: Node) {
        self.nodes.insert(node.id.clone(), node);
    }

    /// Remove a node from the cluster
    pub fn unregister_node(&mut self, node_id: &str) -> Option<Node> {
        self.nodes.remove(node_id)
    }

    /// Submit a job to the cluster
    pub async fn submit_job(&mut self, job: Job) -> Result<String> {
        let job_id = job.id.clone();
        self.jobs.insert(job_id.clone(), job);
        Ok(job_id)
    }

    /// Get job status
    pub fn get_job_status(&self, _job_id: &str) -> Option<JobStatus> {
        // In real impl, would check actual job status
        Some(JobStatus::Pending)
    }

    /// List available nodes
    pub fn list_nodes(&self) -> Vec<&Node> {
        self.nodes.values().collect()
    }

    /// Find best node for job
    pub fn select_node(&self, requirements: &JobRequirements) -> Option<&Node> {
        self.nodes.values().find(|node| {
            if node.status != NodeStatus::Online {
                return false;
            }

            if let Some(min_cpu) = requirements.min_cpu_cores {
                if node.resources.cpu_cores < min_cpu {
                    return false;
                }
            }

            if let Some(min_mem) = requirements.min_memory_gb {
                if node.resources.memory_gb < min_mem {
                    return false;
                }
            }

            if requirements.requires_gpu && node.resources.gpu_count == 0 {
                return false;
            }

            true
        })
    }

    /// Execute job on selected node
    pub async fn execute_job(&self, job_id: &str) -> Result<JobResult> {
        let job = self.jobs
            .get(job_id)
            .ok_or_else(|| crate::HanzoError::InvalidRequest(format!("Job {} not found", job_id)))?;

        let node = self.select_node(&job.requirements)
            .ok_or_else(|| crate::HanzoError::InvalidRequest("No suitable node available".into()))?;

        // In real impl, would dispatch to node and await result
        Ok(JobResult {
            job_id: job_id.to_string(),
            status: JobStatus::Completed,
            output: Some(serde_json::json!({"result": "success"})),
            error: None,
            execution_time_ms: 100,
        })
    }
}

impl Default for Cluster {
    fn default() -> Self {
        Self::new()
    }
}

/// Scheduler for job distribution
#[async_trait]
pub trait Scheduler: Send + Sync {
    /// Schedule a job to a node
    async fn schedule(&self, job: &Job, nodes: &[Node]) -> Result<String>;

    /// Rebalance jobs across nodes
    async fn rebalance(&self, jobs: &[Job], nodes: &[Node]) -> Result<HashMap<String, String>>;
}

/// Simple round-robin scheduler
pub struct RoundRobinScheduler {
    current_index: std::sync::atomic::AtomicUsize,
}

impl RoundRobinScheduler {
    pub fn new() -> Self {
        Self {
            current_index: std::sync::atomic::AtomicUsize::new(0),
        }
    }
}

#[async_trait]
impl Scheduler for RoundRobinScheduler {
    async fn schedule(&self, _job: &Job, nodes: &[Node]) -> Result<String> {
        if nodes.is_empty() {
            return Err(crate::HanzoError::InvalidRequest("No nodes available".into()));
        }

        let index = self.current_index.fetch_add(1, std::sync::atomic::Ordering::Relaxed) % nodes.len();
        Ok(nodes[index].id.clone())
    }

    async fn rebalance(&self, jobs: &[Job], nodes: &[Node]) -> Result<HashMap<String, String>> {
        let mut assignments = HashMap::new();

        for (i, job) in jobs.iter().enumerate() {
            if !nodes.is_empty() {
                let node_index = i % nodes.len();
                assignments.insert(job.id.clone(), nodes[node_index].id.clone());
            }
        }

        Ok(assignments)
    }
}