// Kuasar-inspired sandboxing layer for hanzod
// Provides isolation and resource management for workloads

use anyhow::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::path::PathBuf;
use tokio::sync::RwLock;
use uuid::Uuid;
use chrono::{DateTime, Utc};

use crate::runtime_detection::{RuntimeProvider, ContainerConfig};

/// Sandboxer types supported by hanzod
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum SandboxerType {
    /// MicroVM-based isolation (like Firecracker/Kata)
    MicroVM,
    /// Container runtime sandboxing (Docker/Containerd)
    Container,
    /// WebAssembly-based sandboxing (Wasmtime/WasmEdge)
    Wasm,
    /// Process-based isolation (gVisor/User namespaces)
    Process,
    /// Quark container (Rust-based microkernel)
    Quark,
}

/// Sandbox configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SandboxConfig {
    /// Type of sandboxer to use
    pub sandboxer_type: SandboxerType,
    /// Resource limits
    pub resources: ResourceLimits,
    /// Network configuration
    pub network: NetworkConfig,
    /// Security policies
    pub security: SecurityPolicy,
    /// Mount points
    pub mounts: Vec<MountPoint>,
    /// Environment variables
    pub env: HashMap<String, String>,
}

/// Resource limits for a sandbox
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceLimits {
    /// CPU cores (fractional allowed)
    pub cpu_cores: f32,
    /// Memory in MB
    pub memory_mb: u64,
    /// Disk quota in MB
    pub disk_mb: Option<u64>,
    /// GPU devices
    pub gpu_devices: Vec<String>,
    /// Network bandwidth in Mbps
    pub bandwidth_mbps: Option<u32>,
}

/// Network configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkConfig {
    /// Enable networking
    pub enabled: bool,
    /// Network mode (bridge, host, none)
    pub mode: String,
    /// Port mappings
    pub ports: Vec<PortMapping>,
    /// DNS servers
    pub dns: Vec<String>,
}

/// Port mapping configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortMapping {
    pub host_port: u16,
    pub container_port: u16,
    pub protocol: String,
}

/// Security policy for sandbox
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityPolicy {
    /// Enable seccomp filters
    pub seccomp: bool,
    /// Enable AppArmor/SELinux
    pub mac: bool,
    /// Read-only rootfs
    pub readonly_rootfs: bool,
    /// No new privileges
    pub no_new_privileges: bool,
    /// Capabilities to drop
    pub drop_capabilities: Vec<String>,
    /// Capabilities to add
    pub add_capabilities: Vec<String>,
}

/// Mount point configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MountPoint {
    pub source: String,
    pub target: String,
    pub readonly: bool,
    pub mount_type: String, // bind, volume, tmpfs
}

/// Sandbox instance information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Sandbox {
    pub id: String,
    pub sandboxer_type: SandboxerType,
    pub container_id: Option<String>,
    pub runtime_id: String,
    pub config: SandboxConfig,
    pub status: SandboxStatus,
    pub created_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub stopped_at: Option<DateTime<Utc>>,
    pub metrics: SandboxMetrics,
}

/// Sandbox status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SandboxStatus {
    Creating,
    Created,
    Starting,
    Running,
    Stopping,
    Stopped,
    Failed(String),
}

/// Sandbox runtime metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SandboxMetrics {
    pub cpu_usage: f32,
    pub memory_usage_mb: u64,
    pub network_rx_bytes: u64,
    pub network_tx_bytes: u64,
    pub disk_read_bytes: u64,
    pub disk_write_bytes: u64,
}

impl Default for SandboxMetrics {
    fn default() -> Self {
        Self {
            cpu_usage: 0.0,
            memory_usage_mb: 0,
            network_rx_bytes: 0,
            network_tx_bytes: 0,
            disk_read_bytes: 0,
            disk_write_bytes: 0,
        }
    }
}

/// Sandboxer trait - implemented by different sandboxing backends
#[async_trait]
pub trait Sandboxer: Send + Sync {
    /// Get sandboxer type
    fn sandboxer_type(&self) -> SandboxerType;

    /// Check if sandboxer is available
    async fn is_available(&self) -> Result<bool>;

    /// Create a new sandbox
    async fn create_sandbox(
        &self,
        image: &str,
        config: SandboxConfig,
    ) -> Result<String>;

    /// Start a sandbox
    async fn start_sandbox(&self, id: &str) -> Result<()>;

    /// Stop a sandbox
    async fn stop_sandbox(&self, id: &str) -> Result<()>;

    /// Delete a sandbox
    #[allow(dead_code)]
    async fn delete_sandbox(&self, id: &str) -> Result<()>;

    /// Get sandbox status
    #[allow(dead_code)]
    async fn get_sandbox_status(&self, id: &str) -> Result<SandboxStatus>;

    /// Execute command in sandbox
    async fn exec_in_sandbox(
        &self,
        id: &str,
        command: Vec<String>,
    ) -> Result<String>;

    /// Get sandbox metrics
    #[allow(dead_code)]
    async fn get_metrics(&self, id: &str) -> Result<SandboxMetrics>;
}

/// Container-based sandboxer using existing runtime providers
pub struct ContainerSandboxer {
    runtime_provider: Arc<dyn RuntimeProvider>,
}

impl ContainerSandboxer {
    pub fn new(runtime_provider: Arc<dyn RuntimeProvider>) -> Self {
        Self { runtime_provider }
    }

    /// Parse bytes string with units (e.g., "1.5MB", "500kB", "2GB")
    fn parse_bytes_string(s: &str) -> u64 {
        let s = s.trim();
        if s == "0B" || s == "--" {
            return 0;
        }

        // Extract number and unit
        let (num_str, unit) = if let Some(pos) = s.rfind(|c: char| c.is_ascii_digit() || c == '.') {
            let split_pos = pos + 1;
            (&s[..split_pos], &s[split_pos..])
        } else {
            return 0;
        };

        let num = num_str.parse::<f64>().unwrap_or(0.0);

        // Convert to bytes based on unit
        let multiplier = match unit.to_uppercase().as_str() {
            "B" => 1.0,
            "KB" | "KIB" => 1024.0,
            "MB" | "MIB" => 1024.0 * 1024.0,
            "GB" | "GIB" => 1024.0 * 1024.0 * 1024.0,
            "TB" | "TIB" => 1024.0 * 1024.0 * 1024.0 * 1024.0,
            _ => 1.0,
        };

        (num * multiplier) as u64
    }

    /// Fallback method to get metrics using individual Docker commands
    async fn get_metrics_fallback(&self, id: &str) -> Result<SandboxMetrics> {
        // Get CPU and memory stats using docker stats with specific format
        let cpu_output = tokio::process::Command::new("docker")
            .args(&["stats", "--no-stream", "--format", "{{.CPUPerc}}", id])
            .output()
            .await?;

        let cpu_usage = if cpu_output.status.success() {
            String::from_utf8_lossy(&cpu_output.stdout)
                .trim()
                .trim_end_matches('%')
                .parse::<f32>()
                .unwrap_or(0.0)
        } else {
            0.0
        };

        let mem_output = tokio::process::Command::new("docker")
            .args(&["stats", "--no-stream", "--format", "{{.MemUsage}}", id])
            .output()
            .await?;

        let memory_usage_mb = if mem_output.status.success() {
            let mem_str = String::from_utf8_lossy(&mem_output.stdout);
            if let Some(used_part) = mem_str.split('/').next() {
                let used_trimmed = used_part.trim();
                if used_trimmed.ends_with("MiB") {
                    used_trimmed.trim_end_matches("MiB")
                        .trim()
                        .parse::<f64>()
                        .unwrap_or(0.0) as u64
                } else if used_trimmed.ends_with("GiB") {
                    (used_trimmed.trim_end_matches("GiB")
                        .trim()
                        .parse::<f64>()
                        .unwrap_or(0.0) * 1024.0) as u64
                } else {
                    0
                }
            } else {
                0
            }
        } else {
            0
        };

        // For network and disk I/O, we need more complex inspection
        // These would require maintaining state to track differences over time
        // For now, we'll return zeros or use simplified metrics
        Ok(SandboxMetrics {
            cpu_usage,
            memory_usage_mb,
            network_rx_bytes: 0,  // Would need to track state for accurate delta
            network_tx_bytes: 0,   // Would need to track state for accurate delta
            disk_read_bytes: 0,    // Would need to track state for accurate delta
            disk_write_bytes: 0,   // Would need to track state for accurate delta
        })
    }
}

#[async_trait]
impl Sandboxer for ContainerSandboxer {
    fn sandboxer_type(&self) -> SandboxerType {
        SandboxerType::Container
    }

    async fn is_available(&self) -> Result<bool> {
        self.runtime_provider.health_check().await
    }

    async fn create_sandbox(
        &self,
        image: &str,
        config: SandboxConfig,
    ) -> Result<String> {
        // Convert SandboxConfig to ContainerConfig
        let name = format!("sandbox-{}", Uuid::new_v4());
        let container_config = ContainerConfig {
            env: config.env,
            mounts: config.mounts.iter().map(|m| {
                crate::runtime_detection::Mount {
                    source: PathBuf::from(&m.source),
                    target: PathBuf::from(&m.target),
                    readonly: m.readonly,
                }
            }).collect(),
            memory_limit: Some(config.resources.memory_mb),
            cpu_limit: Some(config.resources.cpu_cores as f64),
            sandbox_type: None,
        };

        let container_id = self.runtime_provider
            .create_container(image, &name, container_config)
            .await?;

        Ok(container_id)
    }

    async fn start_sandbox(&self, id: &str) -> Result<()> {
        self.runtime_provider.start_container(id).await
    }

    async fn stop_sandbox(&self, id: &str) -> Result<()> {
        self.runtime_provider.stop_container(id).await
    }

    async fn delete_sandbox(&self, id: &str) -> Result<()> {
        self.runtime_provider.remove_container(id).await
    }

    async fn get_sandbox_status(&self, id: &str) -> Result<SandboxStatus> {
        // Get container status using Docker inspect
        let output = tokio::process::Command::new("docker")
            .args(&["inspect", "--format", "{{.State.Status}}", id])
            .output()
            .await?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            if stderr.contains("No such object") {
                return Err(anyhow::anyhow!("Container not found: {}", id));
            }
            return Err(anyhow::anyhow!("Failed to get container status: {}", stderr));
        }

        let status_str = String::from_utf8_lossy(&output.stdout).trim().to_string();

        // Map Docker status to SandboxStatus
        let sandbox_status = match status_str.as_str() {
            "created" => SandboxStatus::Created,
            "running" => SandboxStatus::Running,
            "paused" => SandboxStatus::Stopping,  // Map paused to stopping
            "restarting" => SandboxStatus::Starting,
            "removing" => SandboxStatus::Stopping,
            "exited" | "dead" => {
                // Get exit code for failed status
                let exit_code_output = tokio::process::Command::new("docker")
                    .args(&["inspect", "--format", "{{.State.ExitCode}}", id])
                    .output()
                    .await?;

                if exit_code_output.status.success() {
                    let exit_code = String::from_utf8_lossy(&exit_code_output.stdout)
                        .trim()
                        .parse::<i32>()
                        .unwrap_or(-1);

                    if exit_code != 0 {
                        SandboxStatus::Failed(format!("Container exited with code {}", exit_code))
                    } else {
                        SandboxStatus::Stopped
                    }
                } else {
                    SandboxStatus::Stopped
                }
            }
            unknown => SandboxStatus::Failed(format!("Unknown status: {}", unknown)),
        };

        Ok(sandbox_status)
    }

    async fn exec_in_sandbox(
        &self,
        id: &str,
        command: Vec<String>,
    ) -> Result<String> {
        self.runtime_provider.exec_in_container(id, command).await
    }

    async fn get_metrics(&self, id: &str) -> Result<SandboxMetrics> {
        // Get container stats using Docker stats command
        let output = tokio::process::Command::new("docker")
            .args(&["stats", "--no-stream", "--format", "json", id])
            .output()
            .await?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow::anyhow!("Failed to get container metrics: {}", stderr));
        }

        let stats_json = String::from_utf8_lossy(&output.stdout);

        // Parse the JSON stats
        if let Ok(stats) = serde_json::from_str::<serde_json::Value>(stats_json.trim()) {
            // Parse CPU usage
            let cpu_usage = if let Some(cpu_str) = stats["CPUPerc"].as_str() {
                cpu_str.trim_end_matches('%')
                    .parse::<f32>()
                    .unwrap_or(0.0)
            } else {
                0.0
            };

            // Parse memory usage
            let memory_usage_mb = if let Some(mem_str) = stats["MemUsage"].as_str() {
                // Format is "XXXMiB / YYYMiB" or "XXXGiB / YYYGiB"
                if let Some(used_part) = mem_str.split('/').next() {
                    let used_trimmed = used_part.trim();
                    if used_trimmed.ends_with("MiB") {
                        used_trimmed.trim_end_matches("MiB")
                            .trim()
                            .parse::<f64>()
                            .unwrap_or(0.0) as u64
                    } else if used_trimmed.ends_with("GiB") {
                        (used_trimmed.trim_end_matches("GiB")
                            .trim()
                            .parse::<f64>()
                            .unwrap_or(0.0) * 1024.0) as u64
                    } else if used_trimmed.ends_with("KiB") {
                        (used_trimmed.trim_end_matches("KiB")
                            .trim()
                            .parse::<f64>()
                            .unwrap_or(0.0) / 1024.0) as u64
                    } else {
                        0
                    }
                } else {
                    0
                }
            } else {
                0
            };

            // Parse network I/O
            let (network_rx_bytes, network_tx_bytes) = if let Some(net_str) = stats["NetIO"].as_str() {
                // Format is "XXX / YYY" (RX / TX)
                let parts: Vec<&str> = net_str.split('/').collect();
                if parts.len() == 2 {
                    let rx = Self::parse_bytes_string(parts[0].trim());
                    let tx = Self::parse_bytes_string(parts[1].trim());
                    (rx, tx)
                } else {
                    (0, 0)
                }
            } else {
                (0, 0)
            };

            // Parse disk I/O
            let (disk_read_bytes, disk_write_bytes) = if let Some(disk_str) = stats["BlockIO"].as_str() {
                // Format is "XXX / YYY" (Read / Write)
                let parts: Vec<&str> = disk_str.split('/').collect();
                if parts.len() == 2 {
                    let read = Self::parse_bytes_string(parts[0].trim());
                    let write = Self::parse_bytes_string(parts[1].trim());
                    (read, write)
                } else {
                    (0, 0)
                }
            } else {
                (0, 0)
            };

            Ok(SandboxMetrics {
                cpu_usage,
                memory_usage_mb,
                network_rx_bytes,
                network_tx_bytes,
                disk_read_bytes,
                disk_write_bytes,
            })
        } else {
            // If JSON parsing fails, try to get basic metrics using alternative method
            self.get_metrics_fallback(id).await
        }
    }
}

/// Sandbox manager - orchestrates sandboxers
pub struct SandboxManager {
    sandboxers: HashMap<SandboxerType, Arc<dyn Sandboxer>>,
    sandboxes: Arc<RwLock<HashMap<String, Sandbox>>>,
}

impl SandboxManager {
    pub fn new() -> Self {
        Self {
            sandboxers: HashMap::new(),
            sandboxes: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a sandboxer
    pub fn register_sandboxer(&mut self, sandboxer: Arc<dyn Sandboxer>) {
        let sandboxer_type = sandboxer.sandboxer_type();
        self.sandboxers.insert(sandboxer_type, sandboxer);
    }

    /// Refresh all sandbox statuses and metrics
    pub async fn refresh_all_statuses(&self) -> Result<()> {
        let sandboxes = self.sandboxes.read().await;
        let sandbox_ids: Vec<String> = sandboxes.keys().cloned().collect();
        drop(sandboxes);

        for sandbox_id in sandbox_ids {
            if let Err(e) = self.refresh_sandbox_status(&sandbox_id).await {
                tracing::warn!("Failed to refresh status for sandbox {}: {}", sandbox_id, e);
            }
        }

        Ok(())
    }

    /// Refresh status and metrics for a specific sandbox
    async fn refresh_sandbox_status(&self, id: &str) -> Result<()> {
        let sandboxes = self.sandboxes.read().await;
        let sandbox = sandboxes.get(id)
            .ok_or_else(|| anyhow::anyhow!("Sandbox not found: {}", id))?;

        let sandboxer = self.sandboxers
            .get(&sandbox.sandboxer_type)
            .ok_or_else(|| anyhow::anyhow!("Sandboxer not available"))?;

        let container_id = sandbox.container_id.clone()
            .ok_or_else(|| anyhow::anyhow!("No container ID for sandbox {}", id))?;

        drop(sandboxes);

        // Get fresh status
        let actual_status = sandboxer.get_sandbox_status(&container_id).await?;

        // Get fresh metrics if running
        let metrics = if actual_status == SandboxStatus::Running {
            sandboxer.get_metrics(&container_id).await.unwrap_or_default()
        } else {
            SandboxMetrics::default()
        };

        // Update cached data
        let mut sandboxes = self.sandboxes.write().await;
        if let Some(sandbox) = sandboxes.get_mut(id) {
            let old_status = sandbox.status.clone();
            sandbox.status = actual_status.clone();
            sandbox.metrics = metrics;

            // Update timestamps based on status transitions
            match (&old_status, &actual_status) {
                (_, SandboxStatus::Running) if !matches!(old_status, SandboxStatus::Running) => {
                    sandbox.started_at = Some(Utc::now());
                }
                (_, SandboxStatus::Stopped | SandboxStatus::Failed(_))
                    if !matches!(old_status, SandboxStatus::Stopped | SandboxStatus::Failed(_)) => {
                    sandbox.stopped_at = Some(Utc::now());
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// Clean up stopped or failed sandboxes
    pub async fn cleanup_stopped_sandboxes(&self, remove_containers: bool) -> Result<Vec<String>> {
        let sandboxes = self.sandboxes.read().await;
        let stopped_sandboxes: Vec<(String, String, SandboxerType)> = sandboxes
            .iter()
            .filter_map(|(id, sandbox)| {
                match &sandbox.status {
                    SandboxStatus::Stopped | SandboxStatus::Failed(_) => {
                        sandbox.container_id.as_ref().map(|container_id| {
                            (id.clone(), container_id.clone(), sandbox.sandboxer_type.clone())
                        })
                    }
                    _ => None
                }
            })
            .collect();
        drop(sandboxes);

        let mut removed_ids = Vec::new();

        for (sandbox_id, container_id, sandboxer_type) in stopped_sandboxes {
            if remove_containers {
                // Remove the container
                if let Some(sandboxer) = self.sandboxers.get(&sandboxer_type) {
                    if let Err(e) = sandboxer.delete_sandbox(&container_id).await {
                        tracing::warn!("Failed to remove container {} for sandbox {}: {}",
                            container_id, sandbox_id, e);
                        continue;
                    }
                }
            }

            // Remove from our tracking
            let mut sandboxes = self.sandboxes.write().await;
            if sandboxes.remove(&sandbox_id).is_some() {
                removed_ids.push(sandbox_id);
            }
        }

        Ok(removed_ids)
    }

    /// Get resource usage summary across all sandboxes
    pub async fn get_resource_summary(&self) -> Result<ResourceSummary> {
        let sandboxes = self.sandboxes.read().await;
        let running_sandboxes: Vec<_> = sandboxes
            .values()
            .filter(|s| s.status == SandboxStatus::Running)
            .cloned()
            .collect();
        drop(sandboxes);

        let mut total_cpu = 0.0;
        let mut total_memory_mb = 0;
        let mut total_network_rx = 0;
        let mut total_network_tx = 0;
        let mut total_disk_read = 0;
        let mut total_disk_write = 0;
        let mut sandbox_count = 0;

        for sandbox in running_sandboxes {
            if let Some(container_id) = &sandbox.container_id {
                if let Some(sandboxer) = self.sandboxers.get(&sandbox.sandboxer_type) {
                    if let Ok(metrics) = sandboxer.get_metrics(container_id).await {
                        total_cpu += metrics.cpu_usage;
                        total_memory_mb += metrics.memory_usage_mb;
                        total_network_rx += metrics.network_rx_bytes;
                        total_network_tx += metrics.network_tx_bytes;
                        total_disk_read += metrics.disk_read_bytes;
                        total_disk_write += metrics.disk_write_bytes;
                        sandbox_count += 1;
                    }
                }
            }
        }

        Ok(ResourceSummary {
            total_sandboxes: self.sandboxes.read().await.len(),
            running_sandboxes: sandbox_count,
            total_cpu_usage: total_cpu,
            total_memory_mb,
            total_network_rx_bytes: total_network_rx,
            total_network_tx_bytes: total_network_tx,
            total_disk_read_bytes: total_disk_read,
            total_disk_write_bytes: total_disk_write,
        })
    }

    /// Create a sandbox with automatic sandboxer selection
    pub async fn create_sandbox(
        &self,
        image: &str,
        config: SandboxConfig,
    ) -> Result<String> {
        // Get the appropriate sandboxer
        let sandboxer = self.sandboxers
            .get(&config.sandboxer_type)
            .ok_or_else(|| anyhow::anyhow!(
                "Sandboxer {:?} not available",
                config.sandboxer_type
            ))?;

        // Check if sandboxer is available
        if !sandboxer.is_available().await? {
            return Err(anyhow::anyhow!(
                "Sandboxer {:?} is not available",
                config.sandboxer_type
            ));
        }

        let sandbox_id = Uuid::new_v4().to_string();

        // Update status to Creating before actually creating
        {
            let mut sandboxes = self.sandboxes.write().await;
            sandboxes.insert(sandbox_id.clone(), Sandbox {
                id: sandbox_id.clone(),
                sandboxer_type: config.sandboxer_type.clone(),
                container_id: None,
                runtime_id: format!("{:?}", config.sandboxer_type),
                config: config.clone(),
                status: SandboxStatus::Creating,
                created_at: Utc::now(),
                started_at: None,
                stopped_at: None,
                metrics: SandboxMetrics::default(),
            });
        }

        // Create the container
        let container_id = match sandboxer.create_sandbox(image, config.clone()).await {
            Ok(id) => id,
            Err(e) => {
                // Update status to Failed if creation fails
                let mut sandboxes = self.sandboxes.write().await;
                if let Some(sandbox) = sandboxes.get_mut(&sandbox_id) {
                    sandbox.status = SandboxStatus::Failed(format!("Creation failed: {}", e));
                }
                return Err(e);
            }
        };

        // Get actual container status
        let actual_status = sandboxer.get_sandbox_status(&container_id).await
            .unwrap_or(SandboxStatus::Created);

        // Update sandbox with container ID and actual status
        let mut sandboxes = self.sandboxes.write().await;
        if let Some(sandbox) = sandboxes.get_mut(&sandbox_id) {
            sandbox.container_id = Some(container_id);
            sandbox.status = actual_status;
        }

        Ok(sandbox_id)
    }

    /// Start a sandbox
    pub async fn start_sandbox(&self, id: &str) -> Result<()> {
        let sandboxes = self.sandboxes.read().await;
        let sandbox = sandboxes.get(id)
            .ok_or_else(|| anyhow::anyhow!("Sandbox not found: {}", id))?;

        let sandboxer = self.sandboxers
            .get(&sandbox.sandboxer_type)
            .ok_or_else(|| anyhow::anyhow!("Sandboxer not available"))?;

        let container_id = sandbox.container_id.clone()
            .ok_or_else(|| anyhow::anyhow!("No container ID for sandbox {}", id))?;

        drop(sandboxes);

        // Update status to Starting before actually starting
        {
            let mut sandboxes = self.sandboxes.write().await;
            if let Some(sandbox) = sandboxes.get_mut(id) {
                sandbox.status = SandboxStatus::Starting;
            }
        }

        // Start the container
        sandboxer.start_sandbox(&container_id).await?;

        // Verify the container is actually running
        let actual_status = sandboxer.get_sandbox_status(&container_id).await?;

        // Update with actual status
        let mut sandboxes = self.sandboxes.write().await;
        if let Some(sandbox) = sandboxes.get_mut(id) {
            sandbox.status = actual_status;
            if sandbox.status == SandboxStatus::Running {
                sandbox.started_at = Some(Utc::now());
            }
        }

        Ok(())
    }

    /// Stop a sandbox
    pub async fn stop_sandbox(&self, id: &str) -> Result<()> {
        let sandboxes = self.sandboxes.read().await;
        let sandbox = sandboxes.get(id)
            .ok_or_else(|| anyhow::anyhow!("Sandbox not found: {}", id))?;

        let sandboxer = self.sandboxers
            .get(&sandbox.sandboxer_type)
            .ok_or_else(|| anyhow::anyhow!("Sandboxer not available"))?;

        let container_id = sandbox.container_id.clone()
            .ok_or_else(|| anyhow::anyhow!("No container ID for sandbox {}", id))?;

        drop(sandboxes);

        // Update status to Stopping before actually stopping
        {
            let mut sandboxes = self.sandboxes.write().await;
            if let Some(sandbox) = sandboxes.get_mut(id) {
                sandbox.status = SandboxStatus::Stopping;
            }
        }

        // Stop the container
        sandboxer.stop_sandbox(&container_id).await?;

        // Verify the container is actually stopped
        let actual_status = sandboxer.get_sandbox_status(&container_id).await
            .unwrap_or(SandboxStatus::Stopped);

        // Update with actual status
        let mut sandboxes = self.sandboxes.write().await;
        if let Some(sandbox) = sandboxes.get_mut(id) {
            sandbox.status = actual_status;
            if matches!(sandbox.status, SandboxStatus::Stopped | SandboxStatus::Failed(_)) {
                sandbox.stopped_at = Some(Utc::now());
            }
        }

        Ok(())
    }

    /// Get sandbox status with fresh metrics
    pub async fn get_sandbox(&self, id: &str) -> Result<Option<Sandbox>> {
        let sandboxes = self.sandboxes.read().await;
        let mut sandbox = sandboxes.get(id).cloned();
        drop(sandboxes);

        // Update with fresh status and metrics if sandbox exists
        if let Some(ref mut sandbox_data) = sandbox {
            if let Some(ref container_id) = sandbox_data.container_id {
                let sandboxer = self.sandboxers
                    .get(&sandbox_data.sandboxer_type)
                    .ok_or_else(|| anyhow::anyhow!("Sandboxer not available"))?;

                // Get fresh status
                if let Ok(status) = sandboxer.get_sandbox_status(container_id).await {
                    sandbox_data.status = status;
                }

                // Get fresh metrics if running
                if sandbox_data.status == SandboxStatus::Running {
                    if let Ok(metrics) = sandboxer.get_metrics(container_id).await {
                        sandbox_data.metrics = metrics;
                    }
                }
            }
        }

        Ok(sandbox)
    }

    /// List all sandboxes with fresh status
    pub async fn list_sandboxes(&self) -> Result<Vec<Sandbox>> {
        let sandboxes = self.sandboxes.read().await;
        let mut sandbox_list: Vec<Sandbox> = sandboxes.values().cloned().collect();
        drop(sandboxes);

        // Update each sandbox with fresh status
        for sandbox in sandbox_list.iter_mut() {
            if let Some(ref container_id) = sandbox.container_id {
                if let Some(sandboxer) = self.sandboxers.get(&sandbox.sandboxer_type) {
                    // Get fresh status
                    if let Ok(status) = sandboxer.get_sandbox_status(container_id).await {
                        sandbox.status = status;
                    }

                    // Get fresh metrics if running
                    if sandbox.status == SandboxStatus::Running {
                        if let Ok(metrics) = sandboxer.get_metrics(container_id).await {
                            sandbox.metrics = metrics;
                        }
                    }
                }
            }
        }

        Ok(sandbox_list)
    }

    /// Execute command in sandbox
    pub async fn exec_in_sandbox(
        &self,
        id: &str,
        command: Vec<String>,
    ) -> Result<String> {
        let sandboxes = self.sandboxes.read().await;
        let sandbox = sandboxes.get(id)
            .ok_or_else(|| anyhow::anyhow!("Sandbox not found: {}", id))?;

        let sandboxer = self.sandboxers
            .get(&sandbox.sandboxer_type)
            .ok_or_else(|| anyhow::anyhow!("Sandboxer not available"))?;

        if let Some(container_id) = &sandbox.container_id {
            sandboxer.exec_in_sandbox(container_id, command).await
        } else {
            Err(anyhow::anyhow!("No container associated with sandbox"))
        }
    }

    /// Get sandbox metrics
    #[allow(dead_code)]
    pub async fn get_metrics(&self, id: &str) -> Result<SandboxMetrics> {
        let sandboxes = self.sandboxes.read().await;
        let sandbox = sandboxes.get(id)
            .ok_or_else(|| anyhow::anyhow!("Sandbox not found: {}", id))?;

        let sandboxer = self.sandboxers
            .get(&sandbox.sandboxer_type)
            .ok_or_else(|| anyhow::anyhow!("Sandboxer not available"))?;

        let container_id = sandbox.container_id.clone()
            .ok_or_else(|| anyhow::anyhow!("No container ID for sandbox {}", id))?;

        drop(sandboxes);

        // Get fresh metrics from the container
        let metrics = sandboxer.get_metrics(&container_id).await?;

        // Update cached metrics
        let mut sandboxes = self.sandboxes.write().await;
        if let Some(sandbox) = sandboxes.get_mut(id) {
            sandbox.metrics = metrics.clone();
        }

        Ok(metrics)
    }
}

/// Resource usage summary across all sandboxes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceSummary {
    pub total_sandboxes: usize,
    pub running_sandboxes: usize,
    pub total_cpu_usage: f32,
    pub total_memory_mb: u64,
    pub total_network_rx_bytes: u64,
    pub total_network_tx_bytes: u64,
    pub total_disk_read_bytes: u64,
    pub total_disk_write_bytes: u64,
}

/// Default sandbox configurations for common workloads
impl SandboxConfig {
    /// Create a default config for inference workloads
    pub fn inference_default() -> Self {
        Self {
            sandboxer_type: SandboxerType::Container,
            resources: ResourceLimits {
                cpu_cores: 1.0,
                memory_mb: 512,  // 512MB default
                disk_mb: Some(1024),
                gpu_devices: vec![],
                bandwidth_mbps: None,
            },
            network: NetworkConfig {
                enabled: true,
                mode: "bridge".to_string(),
                ports: vec![],
                dns: vec!["8.8.8.8".to_string(), "8.8.4.4".to_string()],
            },
            security: SecurityPolicy {
                seccomp: true,
                mac: false,
                readonly_rootfs: false,
                no_new_privileges: true,
                drop_capabilities: vec![
                    "CAP_SYS_ADMIN".to_string(),
                    "CAP_NET_RAW".to_string(),
                ],
                add_capabilities: vec![],
            },
            mounts: vec![],
            env: HashMap::new(),
        }
    }

    /// Create a default config for database workloads
    #[allow(dead_code)]
    pub fn database_default() -> Self {
        Self {
            sandboxer_type: SandboxerType::Container,
            resources: ResourceLimits {
                cpu_cores: 2.0,
                memory_mb: 4096,
                disk_mb: Some(20480),
                gpu_devices: vec![],
                bandwidth_mbps: None,
            },
            network: NetworkConfig {
                enabled: true,
                mode: "bridge".to_string(),
                ports: vec![],
                dns: vec!["8.8.8.8".to_string()],
            },
            security: SecurityPolicy {
                seccomp: true,
                mac: false,
                readonly_rootfs: false,
                no_new_privileges: true,
                drop_capabilities: vec!["CAP_SYS_ADMIN".to_string()],
                add_capabilities: vec![],
            },
            mounts: vec![],
            env: HashMap::new(),
        }
    }

    /// Create a high-security config for untrusted workloads
    pub fn high_security() -> Self {
        Self {
            sandboxer_type: SandboxerType::Process, // Use gVisor-style isolation
            resources: ResourceLimits {
                cpu_cores: 1.0,
                memory_mb: 512,
                disk_mb: Some(1024),
                gpu_devices: vec![],
                bandwidth_mbps: Some(10),
            },
            network: NetworkConfig {
                enabled: false, // No network by default
                mode: "none".to_string(),
                ports: vec![],
                dns: vec![],
            },
            security: SecurityPolicy {
                seccomp: true,
                mac: true,
                readonly_rootfs: true,
                no_new_privileges: true,
                drop_capabilities: vec![
                    "ALL".to_string(), // Drop all capabilities
                ],
                add_capabilities: vec![],
            },
            mounts: vec![],
            env: HashMap::new(),
        }
    }
}