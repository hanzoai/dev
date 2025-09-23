//! Integration tests for hanzod

use hanzod::runtime_detection::{RuntimeDetector, RuntimeType};
use hanzod::sandboxer::{SandboxConfig, SandboxerType, SandboxManager, ContainerSandboxer};
use hanzod::workload_manager::{WorkloadManager, WorkloadRequest, WorkloadType, ResourceRequirements};
use std::sync::Arc;

#[tokio::test]
async fn test_runtime_detection() {
    // Test runtime detection
    let detector = RuntimeDetector::new().await.expect("Failed to create detector");
    let runtimes = detector.runtimes();

    // Should detect at least one runtime (Docker Desktop or Colima)
    assert!(!runtimes.is_empty(), "No container runtimes detected");

    // Check for Docker Desktop or Colima
    let has_docker = runtimes.iter().any(|r| matches!(r.runtime_type, RuntimeType::DockerDesktop));
    let has_colima = runtimes.iter().any(|r| matches!(r.runtime_type, RuntimeType::Colima));

    assert!(has_docker || has_colima, "Neither Docker Desktop nor Colima detected");
}

#[tokio::test]
async fn test_colima_detection() {
    // Specifically test Colima detection
    let detector = RuntimeDetector::new().await.expect("Failed to create detector");
    let runtimes = detector.runtimes();

    // Check if Colima is installed
    let colima_runtime = runtimes.iter()
        .find(|r| matches!(r.runtime_type, RuntimeType::Colima));

    if let Some(runtime) = colima_runtime {
        println!("Colima detected: v{} (active: {})", runtime.version, runtime.is_active);

        // If Colima is active, it should have a valid socket path
        if runtime.is_active {
            assert!(runtime.socket_path.is_some(), "Active Colima should have socket path");
            let socket_path = runtime.socket_path.as_ref().unwrap();
            assert!(socket_path.exists(), "Colima socket path doesn't exist: {:?}", socket_path);
        }
    }
}

#[tokio::test]
async fn test_sandbox_config_defaults() {
    // Test default configurations
    let inference_config = SandboxConfig::inference_default();
    assert_eq!(inference_config.sandboxer_type, SandboxerType::Container);
    assert_eq!(inference_config.resources.memory_mb, 512);
    assert_eq!(inference_config.resources.cpu_cores, 1.0);

    let database_config = SandboxConfig::database_default();
    assert_eq!(database_config.sandboxer_type, SandboxerType::Container);
    assert_eq!(database_config.resources.memory_mb, 4096);

    let security_config = SandboxConfig::high_security();
    assert_eq!(security_config.sandboxer_type, SandboxerType::Process);
    assert_eq!(security_config.resources.memory_mb, 512);
    assert!(!security_config.network.enabled); // Network should be disabled for high security
}

#[tokio::test]
async fn test_sandbox_manager() {
    // Test sandbox manager creation
    let sandbox_manager = SandboxManager::new();

    // List sandboxes (should be empty initially)
    let sandboxes = sandbox_manager.list_sandboxes().await
        .expect("Failed to list sandboxes");
    assert_eq!(sandboxes.len(), 0, "Should start with no sandboxes");
}

#[tokio::test]
async fn test_workload_types() {
    // Test different workload types
    let inference = WorkloadType::Inference {
        model: "llama3".to_string(),
        engine: "ollama".to_string(),
    };

    let embedding = WorkloadType::Embedding {
        model: "all-minilm".to_string(),
    };

    let vector_db = WorkloadType::VectorDB {
        engine: "qdrant".to_string(),
    };

    let blockchain = WorkloadType::Blockchain {
        chain: "lux".to_string(),
    };

    let compute = WorkloadType::Compute {
        image: "nginx:alpine".to_string(),
        command: vec!["nginx".to_string(), "-g".to_string(), "daemon off;".to_string()],
    };

    // Verify enum variants can be created
    match inference {
        WorkloadType::Inference { model, engine } => {
            assert_eq!(model, "llama3");
            assert_eq!(engine, "ollama");
        }
        _ => panic!("Wrong type"),
    }
}

#[tokio::test]
async fn test_resource_requirements() {
    // Test resource requirements
    let requirements = ResourceRequirements {
        min_memory_mb: 512,
        min_cpu_cores: 1.0,
        gpu_required: false,
        network_required: true,
    };

    assert_eq!(requirements.min_memory_mb, 512);
    assert!(!requirements.gpu_required);
    assert!(requirements.network_required);
}

#[cfg(test)]
mod docker_tests {
    use super::*;
    use hanzod::docker_provider::DockerProvider;
    use hanzod::runtime_detection::{RuntimeInfo, RuntimeProvider, ContainerConfig};
    use std::collections::HashMap;

    #[tokio::test]
    async fn test_docker_provider_creation() {
        // Create a mock runtime info
        let runtime_info = RuntimeInfo {
            runtime_type: RuntimeType::DockerDesktop,
            version: "28.0.1".to_string(),
            socket_path: Some(std::path::PathBuf::from("/var/run/docker.sock")),
            is_active: false, // Set to false so we don't try to connect
            capabilities: vec![],
        };

        let provider = DockerProvider::new(runtime_info.clone());
        let info = provider.info();
        assert_eq!(info.runtime_type, RuntimeType::DockerDesktop);
    }

}

#[cfg(test)]
mod api_tests {
    use axum::http::StatusCode;
    use serde_json::json;

    #[tokio::test]
    async fn test_health_endpoint() {
        // Server should be running from cargo run
        let client = reqwest::Client::new();

        // Try multiple times in case server is starting
        for _ in 0..10 {
            if let Ok(response) = client.get("http://localhost:3690/health").send().await {
                if response.status() == StatusCode::OK {
                    let body: serde_json::Value = response.json().await.expect("Failed to parse JSON");
                    assert_eq!(body["status"], "healthy");
                    return;
                }
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
        }

        panic!("Health endpoint not responding");
    }

    #[tokio::test]
    async fn test_runtimes_endpoint() {
        let client = reqwest::Client::new();

        // Try with retries
        for _ in 0..10 {
            if let Ok(response) = client.get("http://localhost:3690/runtimes").send().await {
                if response.status() == StatusCode::OK {
                    let body: serde_json::Value = response.json().await.expect("Failed to parse JSON");
                    assert!(body["runtimes"].is_array());
                    return;
                }
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
        }
        panic!("Runtimes endpoint not responding");
    }

    #[tokio::test]
    #[ignore] // Requires server and Docker
    async fn test_sandbox_creation() {
        let client = reqwest::Client::new();
        let payload = json!({
            "image": "alpine:latest",
            "sandboxer_type": "container"
        });

        let response = client.post("http://localhost:3690/v1/sandboxes")
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");

        let body: serde_json::Value = response.json().await.expect("Failed to parse JSON");

        // If Docker is running, it should succeed
        if body["success"] == true {
            assert!(body["sandbox_id"].is_string());
        } else {
            // Otherwise, we should get a meaningful error
            assert!(body["error"].is_string());
        }
    }
}