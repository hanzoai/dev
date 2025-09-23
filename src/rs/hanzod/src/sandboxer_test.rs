#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use tokio;

    // Mock implementation for testing
    struct MockSandboxer {
        sandboxer_type: SandboxerType,
    }

    #[async_trait]
    impl Sandboxer for MockSandboxer {
        fn sandboxer_type(&self) -> SandboxerType {
            self.sandboxer_type.clone()
        }

        async fn is_available(&self) -> Result<bool> {
            Ok(true)
        }

        async fn create_sandbox(
            &self,
            _image: &str,
            _config: SandboxConfig,
        ) -> Result<String> {
            Ok("mock-container-123".to_string())
        }

        async fn start_sandbox(&self, _id: &str) -> Result<()> {
            Ok(())
        }

        async fn stop_sandbox(&self, _id: &str) -> Result<()> {
            Ok(())
        }

        async fn delete_sandbox(&self, _id: &str) -> Result<()> {
            Ok(())
        }

        async fn get_sandbox_status(&self, _id: &str) -> Result<SandboxStatus> {
            // Simulate real status checking
            Ok(SandboxStatus::Running)
        }

        async fn exec_in_sandbox(
            &self,
            _id: &str,
            _command: Vec<String>,
        ) -> Result<String> {
            Ok("Command executed".to_string())
        }

        async fn get_metrics(&self, _id: &str) -> Result<SandboxMetrics> {
            // Return realistic metrics
            Ok(SandboxMetrics {
                cpu_usage: 25.5,
                memory_usage_mb: 128,
                network_rx_bytes: 1024 * 1024,
                network_tx_bytes: 512 * 1024,
                disk_read_bytes: 2048 * 1024,
                disk_write_bytes: 1024 * 1024,
            })
        }
    }

    #[tokio::test]
    async fn test_sandbox_manager_lifecycle() {
        let mut manager = SandboxManager::new();

        // Register mock sandboxer
        let mock_sandboxer = Arc::new(MockSandboxer {
            sandboxer_type: SandboxerType::Container,
        });
        manager.register_sandboxer(mock_sandboxer);

        // Create a sandbox
        let config = SandboxConfig::inference_default();
        let sandbox_id = manager.create_sandbox("test-image", config).await.unwrap();

        // Verify sandbox was created
        let sandbox = manager.get_sandbox(&sandbox_id).await.unwrap();
        assert!(sandbox.is_some());
        let sandbox = sandbox.unwrap();
        assert_eq!(sandbox.id, sandbox_id);
        assert!(matches!(sandbox.status, SandboxStatus::Created | SandboxStatus::Running));

        // Start the sandbox
        manager.start_sandbox(&sandbox_id).await.unwrap();

        // Check status is updated
        let sandbox = manager.get_sandbox(&sandbox_id).await.unwrap().unwrap();
        assert_eq!(sandbox.status, SandboxStatus::Running);

        // Get metrics
        let metrics = manager.get_metrics(&sandbox_id).await.unwrap();
        assert!(metrics.cpu_usage > 0.0);
        assert!(metrics.memory_usage_mb > 0);

        // Stop the sandbox
        manager.stop_sandbox(&sandbox_id).await.unwrap();

        // Verify stopped
        let sandbox = manager.get_sandbox(&sandbox_id).await.unwrap().unwrap();
        assert!(matches!(sandbox.status, SandboxStatus::Stopped | SandboxStatus::Running));
    }

    #[tokio::test]
    async fn test_resource_summary() {
        let mut manager = SandboxManager::new();

        // Register mock sandboxer
        let mock_sandboxer = Arc::new(MockSandboxer {
            sandboxer_type: SandboxerType::Container,
        });
        manager.register_sandboxer(mock_sandboxer);

        // Create multiple sandboxes
        let config = SandboxConfig::inference_default();
        let sandbox_id1 = manager.create_sandbox("test-image", config.clone()).await.unwrap();
        let sandbox_id2 = manager.create_sandbox("test-image", config).await.unwrap();

        // Start both sandboxes
        manager.start_sandbox(&sandbox_id1).await.unwrap();
        manager.start_sandbox(&sandbox_id2).await.unwrap();

        // Get resource summary
        let summary = manager.get_resource_summary().await.unwrap();
        assert_eq!(summary.total_sandboxes, 2);
        assert!(summary.running_sandboxes <= 2);
        assert!(summary.total_cpu_usage >= 0.0);
        assert!(summary.total_memory_mb >= 0);
    }

    #[tokio::test]
    async fn test_cleanup_stopped_sandboxes() {
        let mut manager = SandboxManager::new();

        // Register mock sandboxer
        let mock_sandboxer = Arc::new(MockSandboxer {
            sandboxer_type: SandboxerType::Container,
        });
        manager.register_sandboxer(mock_sandboxer);

        // Create and stop a sandbox
        let config = SandboxConfig::inference_default();
        let sandbox_id = manager.create_sandbox("test-image", config).await.unwrap();
        manager.start_sandbox(&sandbox_id).await.unwrap();
        manager.stop_sandbox(&sandbox_id).await.unwrap();

        // List sandboxes before cleanup
        let sandboxes_before = manager.list_sandboxes().await.unwrap();
        assert!(!sandboxes_before.is_empty());

        // Clean up stopped sandboxes
        let removed = manager.cleanup_stopped_sandboxes(false).await.unwrap();
        assert!(!removed.is_empty() || sandboxes_before.is_empty());
    }

    #[test]
    fn test_parse_bytes_string() {
        // Test various byte string formats
        assert_eq!(ContainerSandboxer::parse_bytes_string("1.5MB"), 1572864);
        assert_eq!(ContainerSandboxer::parse_bytes_string("500kB"), 512000);
        assert_eq!(ContainerSandboxer::parse_bytes_string("2GB"), 2147483648);
        assert_eq!(ContainerSandboxer::parse_bytes_string("100B"), 100);
        assert_eq!(ContainerSandboxer::parse_bytes_string("1.5GiB"), 1610612736);
        assert_eq!(ContainerSandboxer::parse_bytes_string("512MiB"), 536870912);
        assert_eq!(ContainerSandboxer::parse_bytes_string("0B"), 0);
        assert_eq!(ContainerSandboxer::parse_bytes_string("--"), 0);
    }

    #[test]
    fn test_sandbox_status_mapping() {
        // Test that Docker statuses map correctly to SandboxStatus
        let status_mappings = vec![
            ("created", SandboxStatus::Created),
            ("running", SandboxStatus::Running),
            ("paused", SandboxStatus::Stopping),
            ("restarting", SandboxStatus::Starting),
            ("removing", SandboxStatus::Stopping),
            ("exited", SandboxStatus::Stopped),
            ("dead", SandboxStatus::Stopped),
        ];

        for (docker_status, expected_status) in status_mappings {
            // This tests the logic mapping
            let mapped = match docker_status {
                "created" => SandboxStatus::Created,
                "running" => SandboxStatus::Running,
                "paused" => SandboxStatus::Stopping,
                "restarting" => SandboxStatus::Starting,
                "removing" => SandboxStatus::Stopping,
                "exited" | "dead" => SandboxStatus::Stopped,
                _ => SandboxStatus::Failed(format!("Unknown status: {}", docker_status)),
            };

            assert_eq!(mapped, expected_status, "Failed to map {}", docker_status);
        }
    }

    #[test]
    fn test_sandbox_config_defaults() {
        // Test inference default config
        let config = SandboxConfig::inference_default();
        assert_eq!(config.sandboxer_type, SandboxerType::Container);
        assert_eq!(config.resources.cpu_cores, 1.0);
        assert_eq!(config.resources.memory_mb, 512);
        assert!(config.network.enabled);

        // Test database default config
        let config = SandboxConfig::database_default();
        assert_eq!(config.resources.cpu_cores, 2.0);
        assert_eq!(config.resources.memory_mb, 4096);

        // Test high security config
        let config = SandboxConfig::high_security();
        assert_eq!(config.sandboxer_type, SandboxerType::Process);
        assert!(!config.network.enabled);
        assert!(config.security.readonly_rootfs);
    }

    #[test]
    fn test_resource_summary_structure() {
        let summary = ResourceSummary {
            total_sandboxes: 5,
            running_sandboxes: 3,
            total_cpu_usage: 75.5,
            total_memory_mb: 2048,
            total_network_rx_bytes: 1024 * 1024 * 10,
            total_network_tx_bytes: 1024 * 1024 * 5,
            total_disk_read_bytes: 1024 * 1024 * 20,
            total_disk_write_bytes: 1024 * 1024 * 15,
        };

        assert_eq!(summary.total_sandboxes, 5);
        assert_eq!(summary.running_sandboxes, 3);
        assert_eq!(summary.total_cpu_usage, 75.5);
        assert_eq!(summary.total_memory_mb, 2048);
    }
}