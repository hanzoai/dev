//! Docker runtime provider implementation

use anyhow::{Result, anyhow};
use async_trait::async_trait;

use crate::runtime_detection::{
    RuntimeProvider, RuntimeInfo, ContainerConfig, ContainerInfo, 
    ContainerStatus, SandboxType,
};

/// Docker runtime provider
pub struct DockerProvider {
    info: RuntimeInfo,
    #[allow(dead_code)]
    client: reqwest::Client,
}

impl DockerProvider {
    /// Create a new Docker provider
    pub fn new(info: RuntimeInfo) -> Self {
        Self {
            info,
            client: reqwest::Client::new(),
        }
    }
    
    /// Make a Docker API request
    #[allow(dead_code)]
    async fn docker_request(
        &self,
        method: reqwest::Method,
        endpoint: &str,
        body: Option<serde_json::Value>,
    ) -> Result<reqwest::Response> {
        let url = if let Some(api) = &self.info.api_endpoint {
            if api.starts_with("unix://") {
                // For Unix socket, we need to use a different approach
                // For now, we'll use the Docker CLI
                return Err(anyhow!("Unix socket support not yet implemented"));
            }
            format!("{}{}", api, endpoint)
        } else {
            format!("http://localhost:2375{}", endpoint)
        };
        
        let mut req = self.client.request(method, &url);
        
        if let Some(body) = body {
            req = req.json(&body);
        }
        
        let resp = req.send().await?;
        
        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp.text().await?;
            return Err(anyhow!("Docker API error {}: {}", status, text));
        }
        
        Ok(resp)
    }
    
    /// Use Docker CLI as fallback for Unix socket
    async fn docker_cli(
        &self,
        args: Vec<&str>,
    ) -> Result<String> {
        let output = tokio::process::Command::new("docker")
            .args(&args)
            .output()
            .await?;
        
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("Docker command failed: {}", stderr));
        }
        
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }
}

#[async_trait]
impl RuntimeProvider for DockerProvider {
    fn info(&self) -> &RuntimeInfo {
        &self.info
    }
    
    async fn health_check(&self) -> Result<bool> {
        // Use Docker CLI for health check
        match self.docker_cli(vec!["info", "--format", "{{.ServerVersion}}"]).await {
            Ok(_) => Ok(true),
            Err(_) => Ok(false),
        }
    }
    
    async fn create_container(
        &self,
        image: &str,
        name: &str,
        config: ContainerConfig,
    ) -> Result<String> {
        // Build Docker CLI arguments - store all formatted strings
        let mut args_owned = Vec::new();
        let mut args = vec!["create", "--name", name];

        // Add environment variables
        for (key, value) in &config.env {
            args.push("-e");
            args_owned.push(format!("{}={}", key, value));
        }

        // Add mounts
        for mount in &config.mounts {
            let mount_str = if mount.readonly {
                format!("{}:{}:ro", mount.source.display(), mount.target.display())
            } else {
                format!("{}:{}", mount.source.display(), mount.target.display())
            };
            args.push("-v");
            args_owned.push(mount_str);
        }

        // Add resource limits
        if let Some(memory) = config.memory_limit {
            args.push("-m");
            args_owned.push(format!("{}m", memory));  // Add 'm' for megabytes
        }

        if let Some(cpu) = config.cpu_limit {
            args.push("--cpus");
            args_owned.push(format!("{}", cpu));
        }

        // Now add all owned strings to args
        let _final_args = args.clone();
        let _owned_idx = 0;

        // Re-build args properly
        let mut final_args = vec!["create", "--name", name];

        for (key, value) in &config.env {
            final_args.push("-e");
            let env_str = format!("{}={}", key, value);
            args_owned.push(env_str);
        }

        for mount in &config.mounts {
            let mount_str = if mount.readonly {
                format!("{}:{}:ro", mount.source.display(), mount.target.display())
            } else {
                format!("{}:{}", mount.source.display(), mount.target.display())
            };
            final_args.push("-v");
            args_owned.push(mount_str);
        }

        if let Some(memory) = config.memory_limit {
            final_args.push("-m");
            args_owned.push(format!("{}m", memory));  // Add 'm' for megabytes
        }

        if let Some(cpu) = config.cpu_limit {
            final_args.push("--cpus");
            args_owned.push(format!("{}", cpu));
        }

        // Handle sandbox type
        match config.sandbox_type {
            Some(SandboxType::None) | None => {
                // Standard Docker container
            }
            Some(SandboxType::Runc) => {
                final_args.push("--runtime");
                final_args.push("runc");
            }
            Some(SandboxType::MicroVM) | Some(SandboxType::Wasm) | Some(SandboxType::AppKernel) => {
                return Err(anyhow!("Docker doesn't support {:?} sandbox type", config.sandbox_type));
            }
        }

        final_args.push(image);

        // Add owned strings as references
        let mut cli_args = Vec::new();
        let mut owned_iter = args_owned.iter();

        cli_args.push("create");
        cli_args.push("--name");
        cli_args.push(name);

        for (_key, _) in &config.env {
            cli_args.push("-e");
            if let Some(env_str) = owned_iter.next() {
                cli_args.push(env_str.as_str());
            }
        }

        for _ in &config.mounts {
            cli_args.push("-v");
            if let Some(mount_str) = owned_iter.next() {
                cli_args.push(mount_str.as_str());
            }
        }

        if config.memory_limit.is_some() {
            cli_args.push("-m");
            if let Some(mem_str) = owned_iter.next() {
                cli_args.push(mem_str.as_str());
            }
        }

        if config.cpu_limit.is_some() {
            cli_args.push("--cpus");
            if let Some(cpu_str) = owned_iter.next() {
                cli_args.push(cpu_str.as_str());
            }
        }

        // Handle sandbox type
        match config.sandbox_type {
            Some(SandboxType::None) | None => {
                // Standard Docker container
            }
            Some(SandboxType::Runc) => {
                cli_args.push("--runtime");
                cli_args.push("runc");
            }
            Some(SandboxType::MicroVM) | Some(SandboxType::Wasm) | Some(SandboxType::AppKernel) => {
                return Err(anyhow!("Docker doesn't support {:?} sandbox type", config.sandbox_type));
            }
        }

        cli_args.push(image);

        // Create container using CLI
        let output = self.docker_cli(cli_args).await?;
        let container_id = output.trim().to_string();

        Ok(container_id)
    }
    
    async fn start_container(&self, id: &str) -> Result<()> {
        self.docker_cli(vec!["start", id]).await?;
        Ok(())
    }
    
    async fn stop_container(&self, id: &str) -> Result<()> {
        self.docker_cli(vec!["stop", id]).await?;
        Ok(())
    }
    
    async fn exec_in_container(
        &self,
        id: &str,
        command: Vec<String>,
    ) -> Result<String> {
        let mut args = vec!["exec", id];
        for cmd in command.iter() {
            args.push(cmd.as_str());
        }

        self.docker_cli(args).await
    }
    
    async fn remove_container(&self, id: &str) -> Result<()> {
        self.docker_cli(vec!["rm", "-f", id]).await?;
        Ok(())
    }
    
    async fn list_containers(&self) -> Result<Vec<ContainerInfo>> {
        let output = self.docker_cli(vec![
            "ps", "-a", "--format",
            "{{json .}}"
        ]).await?;
        
        let mut containers = Vec::new();
        
        for line in output.lines() {
            if line.trim().is_empty() {
                continue;
            }
            
            if let Ok(json) = serde_json::from_str::<serde_json::Value>(line) {
                let status_str = json["Status"].as_str().unwrap_or("");
                let status = if status_str.contains("Up") {
                    ContainerStatus::Running
                } else if status_str.contains("Exited") {
                    ContainerStatus::Exited(0)
                } else if status_str.contains("Created") {
                    ContainerStatus::Created
                } else if status_str.contains("Paused") {
                    ContainerStatus::Paused
                } else {
                    ContainerStatus::Stopped
                };
                
                containers.push(ContainerInfo {
                    id: json["ID"].as_str().unwrap_or("").to_string(),
                    name: json["Names"].as_str().unwrap_or("").to_string(),
                    image: json["Image"].as_str().unwrap_or("").to_string(),
                    status,
                    created_at: json["CreatedAt"].as_str().unwrap_or("").to_string(),
                });
            }
        }
        
        Ok(containers)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime_detection::{RuntimeType, RuntimeCapabilities};
    use std::path::PathBuf;
    
    #[tokio::test]
    async fn test_docker_provider() {
        let info = RuntimeInfo {
            runtime_type: RuntimeType::DockerDesktop,
            version: "24.0.0".to_string(),
            socket_path: Some(PathBuf::from("/var/run/docker.sock")),
            api_endpoint: Some("unix:///var/run/docker.sock".to_string()),
            capabilities: RuntimeCapabilities {
                oci_compliant: true,
                supports_microvm: false,
                supports_wasm: false,
                supports_gpu: true,
                supports_rootless: false,
                max_containers: None,
            },
            is_active: true,
        };
        
        let provider = DockerProvider::new(info);
        
        // Test health check
        let health = provider.health_check().await;
        println!("Docker health: {:?}", health);
        
        // Test list containers
        if health.is_ok() && health.unwrap() {
            let containers = provider.list_containers().await;
            println!("Containers: {:?}", containers);
        }
    }
}
