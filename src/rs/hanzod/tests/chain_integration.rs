//! Integration tests for KuzuDB-powered AI Blockchain
//! Tests the complete pipeline: embedding → model → search in single process

use reqwest;
use serde_json::{json, Value};
use std::time::Duration;
use tokio;

const BASE_URL: &str = "http://localhost:3690/v1";
const API_KEY: &str = "test-key-123";

/// Test client for making API requests
struct TestClient {
    client: reqwest::Client,
    base_url: String,
}

impl TestClient {
    fn new() -> Self {
        Self {
            client: reqwest::Client::builder()
                .timeout(Duration::from_secs(10))
                .build()
                .unwrap(),
            base_url: BASE_URL.to_string(),
        }
    }

    async fn get(&self, endpoint: &str) -> Result<Value, reqwest::Error> {
        let url = format!("{}/{}", self.base_url, endpoint);
        let response = self
            .client
            .get(&url)
            .header("Authorization", format!("Bearer {}", API_KEY))
            .send()
            .await?
            .json()
            .await?;
        Ok(response)
    }

    async fn post(&self, endpoint: &str, body: Value) -> Result<Value, reqwest::Error> {
        let url = format!("{}/{}", self.base_url, endpoint);
        let response = self
            .client
            .post(&url)
            .header("Authorization", format!("Bearer {}", API_KEY))
            .json(&body)
            .send()
            .await?
            .json()
            .await?;
        Ok(response)
    }
}

#[tokio::test]
#[ignore] // Run with --ignored flag when hanzod is running
async fn test_health_check() {
    let client = TestClient::new();
    let result = client.get("health").await;

    assert!(result.is_ok(), "Health check should succeed");
    let health = result.unwrap();
    assert_eq!(health["status"], "healthy");
}

#[tokio::test]
#[ignore]
async fn test_embedding_generation() {
    let client = TestClient::new();

    let request = json!({
        "text": "Hanzo AI blockchain enables decentralized artificial intelligence.",
        "model": "snowflake-arctic-embed-l"
    });

    let result = client.post("embeddings", request).await;
    assert!(result.is_ok(), "Embedding generation should succeed");

    let response = result.unwrap();
    assert!(response["embedding"].is_array());
    assert_eq!(response["embedding"].as_array().unwrap().len(), 1536);
}

#[tokio::test]
#[ignore]
async fn test_vector_storage_and_search() {
    let client = TestClient::new();

    // Store multiple vectors
    let texts = vec![
        "Artificial intelligence is revolutionizing blockchain technology",
        "Machine learning models can be deployed on decentralized networks",
        "Vector databases enable semantic search capabilities",
    ];

    for (i, text) in texts.iter().enumerate() {
        let request = json!({
            "text": text,
            "model": "snowflake-arctic-embed-l",
            "metadata": {
                "doc_id": format!("doc_{}", i + 1),
                "category": "AI"
            }
        });

        let result = client.post("embeddings", request).await;
        assert!(result.is_ok(), "Vector storage should succeed");
    }

    // Search for similar vectors
    let search_request = json!({
        "query": "artificial intelligence blockchain",
        "k": 5,
        "threshold": 0.7
    });

    let search_result = client.post("vector_search", search_request).await;
    assert!(search_result.is_ok(), "Vector search should succeed");

    let results = search_result.unwrap();
    assert!(results["results"].is_array());
    assert!(!results["results"].as_array().unwrap().is_empty());
}

#[tokio::test]
#[ignore]
async fn test_inference_pipeline() {
    let client = TestClient::new();

    let request = json!({
        "prompt": "Explain how Hanzo AI blockchain works",
        "model": "default",
        "max_tokens": 100
    });

    let result = client.post("inference", request).await;
    assert!(result.is_ok(), "Inference should succeed");

    let response = result.unwrap();
    assert!(response["response"].is_string());
    assert!(!response["response"].as_str().unwrap().is_empty());
}

#[tokio::test]
#[ignore]
async fn test_complete_ai_blockchain_pipeline() {
    let client = TestClient::new();

    let request = json!({
        "text": "What are the benefits of combining AI with blockchain technology?",
        "pipeline": {
            "embed": true,
            "search": true,
            "inference": true
        },
        "search_k": 3,
        "model": "default",
        "max_tokens": 150
    });

    let result = client.post("ai_blockchain/process", request).await;
    assert!(result.is_ok(), "Complete pipeline should succeed");

    let response = result.unwrap();
    assert!(response["embedding"].is_array());
    assert!(response["search_results"].is_array());
    assert!(response["inference"].is_string());
}

#[tokio::test]
#[ignore]
async fn test_api_gateway_billing() {
    let client = TestClient::new();

    // Create API key
    let create_key_request = json!({
        "tier": "pro",
        "name": "Test Application",
        "rate_limit": 1000
    });

    let key_result = client.post("api_keys", create_key_request).await;
    assert!(key_result.is_ok(), "API key creation should succeed");

    let key_response = key_result.unwrap();
    assert!(key_response["api_key"].is_string());
    assert_eq!(key_response["tier"], "pro");

    // Check usage
    let usage_result = client.get("usage").await;
    assert!(usage_result.is_ok(), "Usage check should succeed");

    let usage = usage_result.unwrap();
    assert!(usage["total_requests"].is_number());
    assert!(usage["total_tokens"].is_number());
}

#[tokio::test]
#[ignore]
async fn test_privacy_modes_tee() {
    let client = TestClient::new();

    let request = json!({
        "text": "Private medical data that should be encrypted",
        "privacy_mode": "TEE",
        "model": "default"
    });

    let result = client.post("inference", request).await;
    assert!(result.is_ok(), "TEE-protected inference should succeed");

    let response = result.unwrap();
    assert_eq!(response["privacy_mode"], "TEE");
    assert!(response["encrypted"].as_bool().unwrap_or(false));
}

#[tokio::test]
#[ignore]
async fn test_blockchain_checkpoints() {
    let client = TestClient::new();

    // Get blockchain height
    let height_result = client.get("blockchain/height").await;
    assert!(height_result.is_ok(), "Getting blockchain height should succeed");

    let height = height_result.unwrap();
    assert!(height["height"].is_number());

    // Create checkpoint
    let checkpoint_request = json!({
        "create_checkpoint": true
    });

    let checkpoint_result = client.post("blockchain/checkpoint", checkpoint_request).await;
    assert!(checkpoint_result.is_ok(), "Creating checkpoint should succeed");

    let checkpoint = checkpoint_result.unwrap();
    assert!(checkpoint["checkpoint_id"].is_string());
    assert!(checkpoint["merkle_root"].is_string());
}

#[tokio::test]
#[ignore]
async fn test_multi_tenant_data_isolation() {
    let client = TestClient::new();

    // Store data for tenant 1
    let tenant1_request = json!({
        "text": "Tenant 1 private data",
        "tenant_id": "tenant-1",
        "model": "default"
    });

    let tenant1_result = client.post("embeddings", tenant1_request).await;
    assert!(tenant1_result.is_ok(), "Tenant 1 storage should succeed");

    // Store data for tenant 2
    let tenant2_request = json!({
        "text": "Tenant 2 private data",
        "tenant_id": "tenant-2",
        "model": "default"
    });

    let tenant2_result = client.post("embeddings", tenant2_request).await;
    assert!(tenant2_result.is_ok(), "Tenant 2 storage should succeed");

    // Search tenant 1 namespace
    let search_request = json!({
        "query": "private data",
        "tenant_id": "tenant-1",
        "k": 10
    });

    let search_result = client.post("vector_search", search_request).await;
    assert!(search_result.is_ok(), "Tenant search should succeed");

    let results = search_result.unwrap();
    let results_array = results["results"].as_array().unwrap();

    // Verify isolation - should only see tenant-1 data
    for result in results_array {
        assert_eq!(result["tenant_id"], "tenant-1");
    }
}

#[tokio::test]
#[ignore]
async fn test_concurrent_request_handling() {
    use futures::future::join_all;

    let client = Arc::new(TestClient::new());
    let mut handles = vec![];

    for i in 0..10 {
        let client_clone = client.clone();
        let handle = tokio::spawn(async move {
            let request = json!({
                "prompt": format!("Quick test {}", i),
                "model": "default",
                "max_tokens": 10
            });
            client_clone.post("inference", request).await
        });
        handles.push(handle);
    }

    let results = join_all(handles).await;
    let success_count = results.iter().filter(|r| r.is_ok()).count();

    assert!(success_count >= 8, "At least 80% of concurrent requests should succeed");
}

use std::sync::Arc;

/// Unit tests for database operations
#[cfg(test)]
mod database_unit_tests {
    use super::*;
    use hanzod::database::{DatabaseConfig, DatabaseBackend, DatabaseManager};
    use hanzod::kuzu_ledger::KuzuLedger;
    use hanzod::ai_blockchain::AIBlockchain;

    #[tokio::test]
    async fn test_database_manager_creation() {
        let config = DatabaseConfig {
            backend: DatabaseBackend::KuzuDB,
            path: "/tmp/test_kuzu".to_string(),
            connection_string: None,
            cache_size_mb: 128,
            compression: true,
            enable_wal: true,
            batch_size: 100,
        };

        let manager = DatabaseManager::new(config).await;
        assert!(manager.is_ok(), "Database manager should be created");
    }

    #[tokio::test]
    async fn test_kuzu_ledger_operations() {
        let ledger = KuzuLedger::new("/tmp/test_kuzu_ledger", None).await;
        assert!(ledger.is_ok(), "KuzuLedger should be created");

        let ledger = ledger.unwrap();

        // Test basic KV operations
        let result = ledger.set("test_key", b"test_value").await;
        assert!(result.is_ok(), "Set operation should succeed");

        let value = ledger.get("test_key").await;
        assert!(value.is_ok(), "Get operation should succeed");
        assert_eq!(value.unwrap(), Some(b"test_value".to_vec()));
    }

    #[tokio::test]
    async fn test_ai_blockchain_initialization() {
        let blockchain = AIBlockchain::new().await;
        assert!(blockchain.is_ok(), "AI blockchain should initialize");

        let blockchain = blockchain.unwrap();

        // Test text processing
        let result = blockchain
            .process_text("Test text for AI blockchain", false, false)
            .await;
        assert!(result.is_ok(), "Text processing should succeed");
    }
}

/// Performance benchmarks
#[cfg(test)]
mod benchmarks {
    use super::*;
    use std::time::Instant;

    #[tokio::test]
    #[ignore] // Run with cargo test --ignored --release
    async fn bench_embedding_generation() {
        let client = TestClient::new();
        let iterations = 100;
        let start = Instant::now();

        for i in 0..iterations {
            let request = json!({
                "text": format!("Benchmark text {}", i),
                "model": "snowflake-arctic-embed-l"
            });

            let _ = client.post("embeddings", request).await;
        }

        let duration = start.elapsed();
        let avg_ms = duration.as_millis() / iterations;

        println!("Average embedding generation time: {}ms", avg_ms);
        assert!(avg_ms < 100, "Embedding generation should be fast");
    }

    #[tokio::test]
    #[ignore]
    async fn bench_vector_search() {
        let client = TestClient::new();
        let iterations = 100;
        let start = Instant::now();

        for _ in 0..iterations {
            let request = json!({
                "query": "benchmark query",
                "k": 10,
                "threshold": 0.8
            });

            let _ = client.post("vector_search", request).await;
        }

        let duration = start.elapsed();
        let avg_ms = duration.as_millis() / iterations;

        println!("Average vector search time: {}ms", avg_ms);
        assert!(avg_ms < 50, "Vector search should be fast");
    }
}