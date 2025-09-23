//! Unit tests for workload manager module

#[cfg(test)]
mod tests {
    use crate::workload_manager::*;
    use std::time::Duration;

    #[test]
    fn test_workload_manager_creation() {
        let manager = WorkloadManager::new();
        assert_eq!(manager.active_workloads(), 0);
        assert_eq!(manager.queued_workloads(), 0);
    }

    #[test]
    fn test_submit_workload() {
        let mut manager = WorkloadManager::new();

        let workload = Workload {
            id: "wl-001".to_string(),
            name: "Training Job".to_string(),
            workload_type: WorkloadType::Training,
            resource_requirements: ResourceRequirements {
                gpu_count: 2,
                cpu_count: 8,
                memory: 32 * 1024 * 1024 * 1024,
                storage: 100 * 1024 * 1024 * 1024,
            },
            priority: Priority::High,
            estimated_duration: Duration::from_secs(3600),
        };

        let result = manager.submit_workload(workload);
        assert!(result.is_ok());
        assert_eq!(manager.queued_workloads(), 1);
    }

    #[test]
    fn test_workload_scheduling() {
        let mut manager = WorkloadManager::new();

        // Submit multiple workloads with different priorities
        let high_priority = Workload {
            id: "high-1".to_string(),
            name: "Critical Job".to_string(),
            workload_type: WorkloadType::Inference,
            resource_requirements: ResourceRequirements {
                gpu_count: 1,
                cpu_count: 4,
                memory: 16 * 1024 * 1024 * 1024,
                storage: 50 * 1024 * 1024 * 1024,
            },
            priority: Priority::Critical,
            estimated_duration: Duration::from_secs(600),
        };

        let low_priority = Workload {
            id: "low-1".to_string(),
            name: "Background Job".to_string(),
            workload_type: WorkloadType::BatchProcessing,
            resource_requirements: ResourceRequirements {
                gpu_count: 0,
                cpu_count: 2,
                memory: 8 * 1024 * 1024 * 1024,
                storage: 20 * 1024 * 1024 * 1024,
            },
            priority: Priority::Low,
            estimated_duration: Duration::from_secs(1800),
        };

        manager.submit_workload(low_priority).unwrap();
        manager.submit_workload(high_priority).unwrap();

        // High priority should be scheduled first
        let next = manager.next_workload();
        assert!(next.is_some());
        assert_eq!(next.unwrap().id, "high-1");
    }

    #[test]
    fn test_workload_lifecycle() {
        let mut manager = WorkloadManager::new();

        let workload = Workload {
            id: "lifecycle-1".to_string(),
            name: "Test Job".to_string(),
            workload_type: WorkloadType::Development,
            resource_requirements: ResourceRequirements {
                gpu_count: 0,
                cpu_count: 1,
                memory: 4 * 1024 * 1024 * 1024,
                storage: 10 * 1024 * 1024 * 1024,
            },
            priority: Priority::Normal,
            estimated_duration: Duration::from_secs(300),
        };

        // Submit
        manager.submit_workload(workload).unwrap();
        assert_eq!(manager.get_workload_status("lifecycle-1"), WorkloadStatus::Queued);

        // Start
        manager.start_workload("lifecycle-1").unwrap();
        assert_eq!(manager.get_workload_status("lifecycle-1"), WorkloadStatus::Running);
        assert_eq!(manager.active_workloads(), 1);

        // Complete
        manager.complete_workload("lifecycle-1").unwrap();
        assert_eq!(manager.get_workload_status("lifecycle-1"), WorkloadStatus::Completed);
        assert_eq!(manager.active_workloads(), 0);
    }

    #[test]
    fn test_workload_cancellation() {
        let mut manager = WorkloadManager::new();

        let workload = Workload {
            id: "cancel-1".to_string(),
            name: "Cancellable Job".to_string(),
            workload_type: WorkloadType::Training,
            resource_requirements: ResourceRequirements {
                gpu_count: 4,
                cpu_count: 16,
                memory: 64 * 1024 * 1024 * 1024,
                storage: 200 * 1024 * 1024 * 1024,
            },
            priority: Priority::Normal,
            estimated_duration: Duration::from_secs(7200),
        };

        manager.submit_workload(workload).unwrap();
        manager.start_workload("cancel-1").unwrap();

        // Cancel running workload
        let result = manager.cancel_workload("cancel-1");
        assert!(result.is_ok());
        assert_eq!(manager.get_workload_status("cancel-1"), WorkloadStatus::Cancelled);
        assert_eq!(manager.active_workloads(), 0);
    }

    #[test]
    fn test_resource_allocation() {
        let manager = WorkloadManager::with_capacity(
            ResourceCapacity {
                total_gpu: 8,
                total_cpu: 32,
                total_memory: 128 * 1024 * 1024 * 1024,
                total_storage: 1024 * 1024 * 1024 * 1024,
            }
        );

        let requirements = ResourceRequirements {
            gpu_count: 4,
            cpu_count: 16,
            memory: 64 * 1024 * 1024 * 1024,
            storage: 500 * 1024 * 1024 * 1024,
        };

        assert!(manager.can_allocate(&requirements));

        let excessive = ResourceRequirements {
            gpu_count: 10, // More than available
            cpu_count: 40,
            memory: 256 * 1024 * 1024 * 1024,
            storage: 2048 * 1024 * 1024 * 1024,
        };

        assert!(!manager.can_allocate(&excessive));
    }

    #[test]
    fn test_workload_metrics() {
        let mut manager = WorkloadManager::new();

        // Submit and complete several workloads
        for i in 0..5 {
            let workload = Workload {
                id: format!("metric-{}", i),
                name: format!("Metric Job {}", i),
                workload_type: WorkloadType::Inference,
                resource_requirements: ResourceRequirements {
                    gpu_count: 1,
                    cpu_count: 2,
                    memory: 8 * 1024 * 1024 * 1024,
                    storage: 20 * 1024 * 1024 * 1024,
                },
                priority: Priority::Normal,
                estimated_duration: Duration::from_secs(60),
            };

            manager.submit_workload(workload).unwrap();
            manager.start_workload(&format!("metric-{}", i)).unwrap();
            manager.complete_workload(&format!("metric-{}", i)).unwrap();
        }

        let metrics = manager.get_metrics();
        assert_eq!(metrics.total_completed, 5);
        assert_eq!(metrics.total_submitted, 5);
        assert!(metrics.average_duration > Duration::from_secs(0));
    }

    #[test]
    fn test_workload_failure_handling() {
        let mut manager = WorkloadManager::new();

        let workload = Workload {
            id: "fail-1".to_string(),
            name: "Failing Job".to_string(),
            workload_type: WorkloadType::Training,
            resource_requirements: ResourceRequirements {
                gpu_count: 2,
                cpu_count: 8,
                memory: 32 * 1024 * 1024 * 1024,
                storage: 100 * 1024 * 1024 * 1024,
            },
            priority: Priority::High,
            estimated_duration: Duration::from_secs(3600),
        };

        manager.submit_workload(workload).unwrap();
        manager.start_workload("fail-1").unwrap();

        // Mark as failed
        manager.fail_workload("fail-1", "Out of memory").unwrap();
        assert_eq!(manager.get_workload_status("fail-1"), WorkloadStatus::Failed);

        // Should be able to retry
        assert!(manager.can_retry("fail-1"));
        manager.retry_workload("fail-1").unwrap();
        assert_eq!(manager.get_workload_status("fail-1"), WorkloadStatus::Queued);
    }
}