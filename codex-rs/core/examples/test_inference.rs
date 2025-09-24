// Simple example to verify the hanzo_inference refactoring
use codex_core::hanzo_inference::{
    HanzoConfig, HanzoManager, HanzoEndpoint,
    ChatMessage, ComputeJob, ContainerSpec, ResourceRequirements
};

#[tokio::main]
async fn main() {
    println!("Testing hanzo_inference refactoring...\n");

    // Test 1: Config defaults
    let config = HanzoConfig::default();
    assert_eq!(config.http_port, 8080);
    assert_eq!(config.grpc_port, 50051);
    assert!(config.enable_inference);
    assert!(config.enable_compute);
    println!("✓ Config defaults are correct");

    // Test 2: Manager creation
    let manager = HanzoManager::new(config);
    println!("✓ Manager created successfully");

    // Test 3: Check base URLs
    let inference_url = manager.inference.base_url().await;
    let compute_url = manager.compute.base_url().await;
    assert_eq!(inference_url, "http://localhost:8080");
    assert_eq!(compute_url, "http://localhost:8080");
    println!("✓ Base URLs are correct");

    // Test 4: Health check
    let health = manager.health_check().await;
    println!("Health status: inference={}, compute={}, embedding={}, vector={}", 
             health.inference, health.compute, health.embedding, health.vector);
    // Note: If hanzod is already running on port 8080, health checks may return true
    println!("✓ Health check completed");

    // Test 5: Create sample data structures
    let _msg = ChatMessage {
        role: "user".to_string(),
        content: "Hello".to_string(),
    };
    println!("✓ ChatMessage created");

    let _job = ComputeJob {
        name: "test-job".to_string(),
        image: "ubuntu:latest".to_string(),
        command: vec!["echo".to_string(), "hello".to_string()],
        resources: ResourceRequirements {
            cpu: "1".to_string(),
            memory: "512Mi".to_string(),
            gpu: None,
        },
        env: None,
    };
    println!("✓ ComputeJob created");

    let _container = ContainerSpec {
        name: "test-container".to_string(),
        image: "nginx:latest".to_string(),
        command: vec!["nginx".to_string()],
        args: vec!["-g".to_string(), "daemon off;".to_string()],
        env: None,
        resources: ResourceRequirements {
            cpu: "0.5".to_string(),
            memory: "256Mi".to_string(),
            gpu: None,
        },
    };
    println!("✓ ContainerSpec created");

    println!("\nAll tests passed! The refactoring is successful.");
    println!("\nArchitecture summary:");
    println!("- HanzoInferenceEndpoint: Handles LLM text generation (completions, chat)");
    println!("- HanzoComputeEndpoint: Manages container workloads and distributed compute");
    println!("- HanzoEmbeddingEndpoint: Generates text embeddings");
    println!("- HanzoVectorEndpoint: Performs vector search operations");
    println!("- HanzoManager: Coordinates all endpoints");
}