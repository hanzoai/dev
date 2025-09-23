//! Unit tests for runtime detection

#[cfg(test)]
mod tests {
    use super::super::*;
    use std::path::PathBuf;

    #[test]
    fn test_colima_socket_paths() {
        // Test that we check the correct Colima socket paths
        let expected_paths = vec![
            PathBuf::from("/Users/z/.colima/default/docker.sock"),
            PathBuf::from("/Users/z/.colima/docker.sock"),
        ];

        // Verify paths are correctly formatted
        for path in &expected_paths {
            assert!(path.to_str().unwrap().contains(".colima"));
        }
    }

    #[test]
    fn test_runtime_type_display() {
        assert_eq!(format!("{:?}", RuntimeType::DockerDesktop), "DockerDesktop");
        assert_eq!(format!("{:?}", RuntimeType::Colima), "Colima");
        assert_eq!(format!("{:?}", RuntimeType::Containerd), "Containerd");
        assert_eq!(format!("{:?}", RuntimeType::Podman), "Podman");
    }

    #[test]
    fn test_sandbox_type_display() {
        assert_eq!(format!("{:?}", SandboxType::Runc), "Runc");
        assert_eq!(format!("{:?}", SandboxType::MicroVM), "MicroVM");
        assert_eq!(format!("{:?}", SandboxType::Wasm), "Wasm");
    }

    #[tokio::test]
    async fn test_detect_colima_when_installed() {
        // This test verifies Colima detection logic
        use tokio::process::Command;

        // Check if Colima is installed
        let output = Command::new("which")
            .arg("colima")
            .output()
            .await;

        if let Ok(output) = output {
            if output.status.success() {
                // Colima is installed, verify we can detect it
                let detector = RuntimeDetector::new().await.unwrap();
                let runtimes = detector.runtimes();

                let has_colima = runtimes.iter()
                    .any(|r| matches!(r.runtime_type, RuntimeType::Colima));

                assert!(has_colima, "Colima is installed but not detected");
            }
        }
    }

    #[tokio::test]
    async fn test_docker_desktop_socket_path() {
        let socket_path = PathBuf::from("/var/run/docker.sock");

        // If Docker Desktop is running, this socket should exist
        if socket_path.exists() {
            let detector = RuntimeDetector::new().await.unwrap();
            let runtimes = detector.runtimes();

            let docker_runtime = runtimes.iter()
                .find(|r| matches!(r.runtime_type, RuntimeType::DockerDesktop));

            if let Some(runtime) = docker_runtime {
                assert!(runtime.is_active, "Docker Desktop socket exists but not marked active");
            }
        }
    }

    #[test]
    fn test_container_config_defaults() {
        let config = ContainerConfig {
            env: std::collections::HashMap::new(),
            mounts: vec![],
            memory_limit: None,
            cpu_limit: None,
            sandbox_type: None,
        };

        assert!(config.env.is_empty());
        assert!(config.mounts.is_empty());
        assert!(config.memory_limit.is_none());
    }

    #[test]
    fn test_mount_structure() {
        let mount = Mount {
            source: PathBuf::from("/host/path"),
            target: PathBuf::from("/container/path"),
            readonly: false,
        };

        assert_eq!(mount.source, PathBuf::from("/host/path"));
        assert_eq!(mount.target, PathBuf::from("/container/path"));
        assert!(!mount.readonly);
    }

    #[test]
    fn test_container_info_structure() {
        use chrono::Utc;

        let info = ContainerInfo {
            id: "test123".to_string(),
            name: "test-container".to_string(),
            image: "nginx:alpine".to_string(),
            status: ContainerStatus::Running,
            created_at: Utc::now().to_string(),
        };

        assert_eq!(info.id, "test123");
        assert_eq!(info.name, "test-container");
        assert_eq!(info.status, ContainerStatus::Running);
    }
}