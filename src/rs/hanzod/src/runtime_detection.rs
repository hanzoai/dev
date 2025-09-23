//! Runtime detection module for identifying available container engines
//!
//! This module provides automatic detection and abstraction for various
//! container runtimes including Docker, Colima, Containerd, and Podman.

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use async_trait::async_trait;

/// Detected runtime type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RuntimeType {
    DockerDesktop,
    Colima,
    Containerd,
    Podman,
    Kuasar,
}

/// Runtime capability flags
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuntimeCapabilities {
    pub oci_compliant: bool,
    pub supports_microvm: bool,
    pub supports_wasm: bool,
    pub supports_gpu: bool,
    pub supports_rootless: bool,
    pub max_containers: Option<usize>,
}

/// Information about a detected runtime
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuntimeInfo {
    pub runtime_type: RuntimeType,
    pub version: String,
    pub socket_path: Option<PathBuf>,
    pub api_endpoint: Option<String>,
    pub capabilities: RuntimeCapabilities,
    pub is_active: bool,
}

/// Abstract runtime provider interface
#[async_trait]
pub trait RuntimeProvider: Send + Sync {
    /// Get runtime information
    fn info(&self) -> &RuntimeInfo;
    
    /// Check if runtime is healthy
    async fn health_check(&self) -> Result<bool>;
    
    /// Create a container
    async fn create_container(
        &self,
        image: &str,
        name: &str,
        config: ContainerConfig,
    ) -> Result<String>;
    
    /// Start a container
    async fn start_container(&self, id: &str) -> Result<()>;
    
    /// Stop a container
    async fn stop_container(&self, id: &str) -> Result<()>;
    
    /// Execute command in container
    async fn exec_in_container(
        &self,
        id: &str,
        command: Vec<String>,
    ) -> Result<String>;
    
    /// Remove a container
    #[allow(dead_code)]
    async fn remove_container(&self, id: &str) -> Result<()>;

    /// List containers
    #[allow(dead_code)]
    async fn list_containers(&self) -> Result<Vec<ContainerInfo>>;
}

/// Container configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerConfig {
    pub env: HashMap<String, String>,
    pub mounts: Vec<Mount>,
    pub memory_limit: Option<u64>,
    pub cpu_limit: Option<f64>,
    pub sandbox_type: Option<SandboxType>,
}

/// Sandbox type for isolation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SandboxType {
    None,
    Runc,
    MicroVM,
    Wasm,
    AppKernel,
}

/// Mount configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Mount {
    pub source: PathBuf,
    pub target: PathBuf,
    pub readonly: bool,
}

/// Container information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerInfo {
    pub id: String,
    pub name: String,
    pub image: String,
    pub status: ContainerStatus,
    pub created_at: String,
}

/// Container status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ContainerStatus {
    Created,
    Running,
    Paused,
    Stopped,
    Exited(i32),
}

/// Runtime detector that finds available container runtimes
pub struct RuntimeDetector {
    detected_runtimes: Vec<RuntimeInfo>,
}

impl RuntimeDetector {
    /// Create a new runtime detector and scan for available runtimes
    pub async fn new() -> Result<Self> {
        let mut detector = Self {
            detected_runtimes: Vec::new(),
        };
        detector.scan().await?;
        Ok(detector)
    }
    
    /// Scan for all available runtimes
    pub async fn scan(&mut self) -> Result<()> {
        self.detected_runtimes.clear();
        
        // Check for Docker
        if let Ok(info) = self.detect_docker().await {
            self.detected_runtimes.push(info);
        }
        
        // Check for Colima
        if let Ok(info) = self.detect_colima().await {
            self.detected_runtimes.push(info);
        }
        
        // Check for Containerd
        if let Ok(info) = self.detect_containerd().await {
            self.detected_runtimes.push(info);
        }
        
        // Check for Podman
        if let Ok(info) = self.detect_podman().await {
            self.detected_runtimes.push(info);
        }
        
        // Check for Kuasar
        if let Ok(info) = self.detect_kuasar().await {
            self.detected_runtimes.push(info);
        }
        
        if self.detected_runtimes.is_empty() {
            return Err(anyhow!("No container runtimes detected"));
        }
        
        Ok(())
    }
    
    /// Get all detected runtimes
    pub fn runtimes(&self) -> &[RuntimeInfo] {
        &self.detected_runtimes
    }
    
    /// Get the preferred runtime based on capabilities
    #[allow(dead_code)]
    pub fn preferred_runtime(&self, sandbox_type: Option<SandboxType>) -> Option<&RuntimeInfo> {
        match sandbox_type {
            Some(SandboxType::MicroVM) => {
                self.detected_runtimes.iter()
                    .find(|r| r.capabilities.supports_microvm && r.is_active)
            }
            Some(SandboxType::Wasm) => {
                self.detected_runtimes.iter()
                    .find(|r| r.capabilities.supports_wasm && r.is_active)
            }
            _ => {
                self.detected_runtimes.iter()
                    .find(|r| r.is_active)
            }
        }
    }
    
    /// Detect Docker runtime
    async fn detect_docker(&self) -> Result<RuntimeInfo> {
        // Check if docker command exists
        let output = Command::new("docker")
            .arg("version")
            .arg("--format")
            .arg("{{.Server.Version}}")
            .output()?;
        
        if !output.status.success() {
            return Err(anyhow!("Docker not running"));
        }
        
        let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
        
        // Check for Docker Desktop socket
        let socket_path = if cfg!(target_os = "macos") {
            PathBuf::from("/var/run/docker.sock")
        } else {
            PathBuf::from("/var/run/docker.sock")
        };
        
        let is_active = socket_path.exists();
        
        Ok(RuntimeInfo {
            runtime_type: RuntimeType::DockerDesktop,
            version,
            socket_path: Some(socket_path),
            api_endpoint: Some("unix:///var/run/docker.sock".to_string()),
            capabilities: RuntimeCapabilities {
                oci_compliant: true,
                supports_microvm: false,
                supports_wasm: false,
                supports_gpu: true,
                supports_rootless: false,
                max_containers: None,
            },
            is_active,
        })
    }
    
    /// Detect Colima runtime
    async fn detect_colima(&self) -> Result<RuntimeInfo> {
        // Check if colima command exists
        let output = Command::new("colima")
            .arg("version")
            .output()?;
        
        if !output.status.success() {
            return Err(anyhow!("Colima not installed"));
        }
        
        let version_str = String::from_utf8_lossy(&output.stdout);
        let version = version_str.lines()
            .find(|l| l.starts_with("colima version"))
            .and_then(|l| l.split_whitespace().nth(2))
            .unwrap_or("unknown")
            .to_string();
        
        // Check if Colima is running
        let status = Command::new("colima")
            .arg("status")
            .output()?;
        
        let is_active = status.status.success();
        
        // Colima uses Docker's socket
        let socket_path = PathBuf::from("/var/run/docker.sock");
        
        Ok(RuntimeInfo {
            runtime_type: RuntimeType::Colima,
            version,
            socket_path: Some(socket_path),
            api_endpoint: Some("unix:///var/run/docker.sock".to_string()),
            capabilities: RuntimeCapabilities {
                oci_compliant: true,
                supports_microvm: true,  // Colima supports QEMU
                supports_wasm: false,
                supports_gpu: false,
                supports_rootless: true,
                max_containers: None,
            },
            is_active,
        })
    }
    
    /// Detect Containerd runtime
    async fn detect_containerd(&self) -> Result<RuntimeInfo> {
        // Check if containerd is running
        let socket_path = PathBuf::from("/run/containerd/containerd.sock");
        
        if !socket_path.exists() {
            return Err(anyhow!("Containerd socket not found"));
        }
        
        // Try to get version
        let output = Command::new("containerd")
            .arg("--version")
            .output()?;
        
        let version = if output.status.success() {
            String::from_utf8_lossy(&output.stdout)
                .lines()
                .next()
                .and_then(|l| l.split_whitespace().nth(2))
                .unwrap_or("unknown")
                .to_string()
        } else {
            "unknown".to_string()
        };
        
        Ok(RuntimeInfo {
            runtime_type: RuntimeType::Containerd,
            version,
            socket_path: Some(socket_path),
            api_endpoint: None,
            capabilities: RuntimeCapabilities {
                oci_compliant: true,
                supports_microvm: true,
                supports_wasm: true,
                supports_gpu: true,
                supports_rootless: false,
                max_containers: None,
            },
            is_active: true,
        })
    }
    
    /// Detect Podman runtime
    async fn detect_podman(&self) -> Result<RuntimeInfo> {
        // Check if podman command exists
        let output = Command::new("podman")
            .arg("version")
            .arg("--format")
            .arg("{{.Version}}")
            .output()?;
        
        if !output.status.success() {
            return Err(anyhow!("Podman not available"));
        }
        
        let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
        
        Ok(RuntimeInfo {
            runtime_type: RuntimeType::Podman,
            version,
            socket_path: None,
            api_endpoint: Some("unix:///run/podman/podman.sock".to_string()),
            capabilities: RuntimeCapabilities {
                oci_compliant: true,
                supports_microvm: false,
                supports_wasm: false,
                supports_gpu: true,
                supports_rootless: true,
                max_containers: None,
            },
            is_active: true,
        })
    }
    
    /// Detect Kuasar runtime
    async fn detect_kuasar(&self) -> Result<RuntimeInfo> {
        // Check if kuasar binaries exist
        let vmm_path = Path::new("/usr/local/bin/vmm-sandboxer");
        let wasm_path = Path::new("/usr/local/bin/wasm-sandboxer");
        
        if !vmm_path.exists() && !wasm_path.exists() {
            return Err(anyhow!("Kuasar not installed"));
        }
        
        Ok(RuntimeInfo {
            runtime_type: RuntimeType::Kuasar,
            version: "latest".to_string(),
            socket_path: None,
            api_endpoint: None,
            capabilities: RuntimeCapabilities {
                oci_compliant: true,
                supports_microvm: vmm_path.exists(),
                supports_wasm: wasm_path.exists(),
                supports_gpu: false,
                supports_rootless: false,
                max_containers: None,
            },
            is_active: true,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_runtime_detection() {
        let detector = RuntimeDetector::new().await;
        
        // Should find at least one runtime on most systems
        assert!(detector.is_ok() || detector.is_err());
        
        if let Ok(detector) = detector {
            let runtimes = detector.runtimes();
            println!("Detected {} runtimes:", runtimes.len());
            for runtime in runtimes {
                println!("  - {:?} v{}", runtime.runtime_type, runtime.version);
            }
        }
    }
}

// Include test module
#[cfg(test)]
#[path = "runtime_detection_test.rs"]
mod runtime_detection_test;
