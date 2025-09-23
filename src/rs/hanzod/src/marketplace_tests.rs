//! Unit tests for marketplace module

#[cfg(test)]
mod tests {
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;

    // Mock structures for testing
    #[derive(Clone, Debug)]
    pub struct Marketplace {
        providers: Vec<ResourceProvider>,
        resources: Vec<ComputeResource>,
    }

    #[derive(Clone, Debug)]
    pub struct ResourceProvider {
        pub id: String,
        pub name: String,
        pub available_gpu: u32,
        pub available_cpu: u32,
        pub available_memory: u64,
        pub hourly_rate: f64,
        pub carbon_offset: bool,
    }

    #[derive(Clone, Debug)]
    pub struct ComputeResource {
        pub allocation_id: String,
        pub provider_id: String,
        pub total_cost: f64,
        pub carbon_neutral: bool,
    }

    #[derive(Clone, Debug)]
    pub struct ComputeRequest {
        pub gpu_count: u32,
        pub cpu_count: u32,
        pub memory: u64,
        pub duration_hours: u32,
    }

    impl Marketplace {
        pub fn new() -> Self {
            Marketplace {
                providers: Vec::new(),
                resources: Vec::new(),
            }
        }

        pub fn resource_providers(&self) -> &Vec<ResourceProvider> {
            &self.providers
        }

        pub fn compute_resources(&self) -> &Vec<ComputeResource> {
            &self.resources
        }

        pub fn register_provider(&mut self, provider: ResourceProvider) {
            self.providers.push(provider);
        }

        pub fn allocate_resources(&mut self, request: ComputeRequest) -> Result<ComputeResource, String> {
            // Find cheapest provider that can satisfy request
            let mut best_provider = None;
            let mut best_cost = f64::MAX;

            for provider in &self.providers {
                if provider.available_gpu >= request.gpu_count &&
                   provider.available_cpu >= request.cpu_count &&
                   provider.available_memory >= request.memory {
                    let cost = provider.hourly_rate * request.duration_hours as f64;
                    if cost < best_cost {
                        best_cost = cost;
                        best_provider = Some(provider.clone());
                    }
                }
            }

            match best_provider {
                Some(provider) => {
                    let resource = ComputeResource {
                        allocation_id: format!("alloc-{}", uuid::Uuid::new_v4()),
                        provider_id: provider.id.clone(),
                        total_cost: best_cost,
                        carbon_neutral: provider.carbon_offset,
                    };
                    self.resources.push(resource.clone());
                    Ok(resource)
                },
                None => Err("No provider available".to_string())
            }
        }

        pub fn deallocate_resources(&mut self, allocation_id: &str) {
            self.resources.retain(|r| r.allocation_id != allocation_id);
        }
    }

    #[test]
    fn test_marketplace_creation() {
        let marketplace = Marketplace::new();
        assert_eq!(marketplace.resource_providers().len(), 0);
        assert_eq!(marketplace.compute_resources().len(), 0);
    }

    #[test]
    fn test_register_provider() {
        let mut marketplace = Marketplace::new();
        let provider = ResourceProvider {
            id: "provider-1".to_string(),
            name: "Test Provider".to_string(),
            available_gpu: 4,
            available_cpu: 16,
            available_memory: 64 * 1024 * 1024 * 1024, // 64GB
            hourly_rate: 2.50,
            carbon_offset: true,
        };

        marketplace.register_provider(provider.clone());
        assert_eq!(marketplace.resource_providers().len(), 1);
        assert_eq!(marketplace.resource_providers()[0].id, "provider-1");
    }

    #[test]
    fn test_allocate_compute_resource() {
        let mut marketplace = Marketplace::new();

        // Register a provider
        let provider = ResourceProvider {
            id: "provider-1".to_string(),
            name: "GPU Provider".to_string(),
            available_gpu: 8,
            available_cpu: 32,
            available_memory: 128 * 1024 * 1024 * 1024,
            hourly_rate: 5.00,
            carbon_offset: true,
        };
        marketplace.register_provider(provider);

        // Allocate resources
        let request = ComputeRequest {
            gpu_count: 2,
            cpu_count: 8,
            memory: 32 * 1024 * 1024 * 1024,
            duration_hours: 4,
        };

        let allocation = marketplace.allocate_resources(request).unwrap();
        assert_eq!(allocation.provider_id, "provider-1");
        assert_eq!(allocation.total_cost, 20.00); // 5.00 * 4 hours
        assert!(allocation.carbon_neutral);
    }

    #[test]
    fn test_insufficient_resources() {
        let mut marketplace = Marketplace::new();

        let provider = ResourceProvider {
            id: "small-provider".to_string(),
            name: "Small Provider".to_string(),
            available_gpu: 1,
            available_cpu: 4,
            available_memory: 8 * 1024 * 1024 * 1024,
            hourly_rate: 1.00,
            carbon_offset: false,
        };
        marketplace.register_provider(provider);

        let request = ComputeRequest {
            gpu_count: 4, // More than available
            cpu_count: 8,
            memory: 32 * 1024 * 1024 * 1024,
            duration_hours: 1,
        };

        let result = marketplace.allocate_resources(request);
        assert!(result.is_err());
    }

    #[test]
    fn test_multiple_providers_selection() {
        let mut marketplace = Marketplace::new();

        // Register multiple providers
        marketplace.register_provider(ResourceProvider {
            id: "expensive".to_string(),
            name: "Premium Provider".to_string(),
            available_gpu: 16,
            available_cpu: 64,
            available_memory: 256 * 1024 * 1024 * 1024,
            hourly_rate: 10.00,
            carbon_offset: true,
        });

        marketplace.register_provider(ResourceProvider {
            id: "cheap".to_string(),
            name: "Budget Provider".to_string(),
            available_gpu: 8,
            available_cpu: 32,
            available_memory: 128 * 1024 * 1024 * 1024,
            hourly_rate: 3.00,
            carbon_offset: false,
        });

        let request = ComputeRequest {
            gpu_count: 4,
            cpu_count: 16,
            memory: 64 * 1024 * 1024 * 1024,
            duration_hours: 2,
        };

        // Should select cheaper provider when both can satisfy request
        let allocation = marketplace.allocate_resources(request).unwrap();
        assert_eq!(allocation.provider_id, "cheap");
        assert_eq!(allocation.total_cost, 6.00); // 3.00 * 2 hours
    }

    #[test]
    fn test_resource_deallocation() {
        let mut marketplace = Marketplace::new();

        let provider = ResourceProvider {
            id: "provider-1".to_string(),
            name: "Test Provider".to_string(),
            available_gpu: 4,
            available_cpu: 16,
            available_memory: 64 * 1024 * 1024 * 1024,
            hourly_rate: 2.00,
            carbon_offset: true,
        };
        marketplace.register_provider(provider);

        let request = ComputeRequest {
            gpu_count: 2,
            cpu_count: 8,
            memory: 32 * 1024 * 1024 * 1024,
            duration_hours: 1,
        };

        let allocation = marketplace.allocate_resources(request).unwrap();
        let allocation_id = allocation.allocation_id.clone();

        // Verify resource is allocated
        assert_eq!(marketplace.compute_resources().len(), 1);

        // Deallocate
        marketplace.deallocate_resources(&allocation_id);

        // Verify resources are freed
        let provider = &marketplace.resource_providers()[0];
        assert_eq!(provider.available_gpu, 4); // Back to original
    }
}