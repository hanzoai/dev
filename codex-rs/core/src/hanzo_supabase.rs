//! Hanzo-Supabase integration with sled recording
//!
//! This module provides integration between hanzod and Supabase,
//! recording all operations in a sled database for persistence.

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::sync::Arc;
use tokio::sync::RwLock;
use sled::Db;
use chrono::{DateTime, Utc};
use reqwest::Client;

/// Operation types for recording
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OperationType {
    Inference,
    Embedding,
    VectorSearch,
    DatabaseQuery,
    StorageAccess,
}

/// Operation record stored in sled
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperationRecord {
    pub id: String,
    pub operation_type: OperationType,
    pub timestamp: DateTime<Utc>,
    pub request_data: serde_json::Value,
    pub response_data: Option<serde_json::Value>,
    pub duration_ms: u64,
    pub status: String,
    pub error: Option<String>,
}

/// Supabase configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupabaseConfig {
    pub url: String,
    pub anon_key: String,
    pub service_key: Option<String>,
    pub postgres_url: String,
}

impl Default for SupabaseConfig {
    fn default() -> Self {
        Self {
            url: "http://localhost:54321".to_string(),
            anon_key: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZS1kZW1vIiwicm9sZSI6ImFub24iLCJleHAiOjE5ODM4MTI5OTZ9.CRXP1A7WOeoJeXxjNni43kdQwgnWNReilDMblYTn_I0".to_string(),
            service_key: None,
            postgres_url: "postgresql://postgres:your-super-secret-and-long-postgres-password@localhost:54322/postgres".to_string(),
        }
    }
}

/// Main Hanzo-Supabase manager
pub struct HanzoSupabaseManager {
    config: Arc<RwLock<SupabaseConfig>>,
    client: Client,
    sled_db: Db,
}

impl HanzoSupabaseManager {
    /// Create a new manager instance
    pub async fn new(config: SupabaseConfig, sled_path: &str) -> Result<Self> {
        let sled_db = sled::open(sled_path)?;
        
        Ok(Self {
            config: Arc::new(RwLock::new(config)),
            client: Client::new(),
            sled_db,
        })
    }

    /// Record an operation in sled
    pub async fn record_operation(
        &self,
        operation: OperationRecord,
    ) -> Result<()> {
        let key = format!("op_{}_{}", operation.timestamp.timestamp_millis(), operation.id);
        let value = serde_json::to_vec(&operation)?;
        
        self.sled_db.insert(key.as_bytes(), value)?;
        self.sled_db.flush()?;
        
        Ok(())
    }

    /// Query Supabase database and record operation
    pub async fn query_database(
        &self,
        table: &str,
        query: serde_json::Value,
    ) -> Result<serde_json::Value> {
        let start = std::time::Instant::now();
        let operation_id = uuid::Uuid::new_v4().to_string();
        
        let config = self.config.read().await;
        let url = format!("{}/rest/v1/{}", config.url, table);
        
        let response = self.client
            .get(&url)
            .header("apikey", &config.anon_key)
            .header("Authorization", format!("Bearer {}", &config.anon_key))
            .json(&query)
            .send()
            .await?;
        
        let status = response.status();
        let response_data = response.json::<serde_json::Value>().await?;
        let duration = start.elapsed().as_millis() as u64;
        
        // Record operation in sled
        let record = OperationRecord {
            id: operation_id,
            operation_type: OperationType::DatabaseQuery,
            timestamp: Utc::now(),
            request_data: json!({
                "table": table,
                "query": query,
            }),
            response_data: Some(response_data.clone()),
            duration_ms: duration,
            status: status.to_string(),
            error: None,
        };
        
        self.record_operation(record).await?;
        
        Ok(response_data)
    }

    /// Store embedding in Supabase and record operation
    pub async fn store_embedding(
        &self,
        text: &str,
        embedding: Vec<f32>,
    ) -> Result<String> {
        let start = std::time::Instant::now();
        let operation_id = uuid::Uuid::new_v4().to_string();
        
        let config = self.config.read().await;
        let url = format!("{}/rest/v1/embeddings", config.url);
        
        let payload = json!({
            "id": operation_id.clone(),
            "text": text,
            "embedding": embedding,
            "created_at": Utc::now().to_rfc3339(),
        });
        
        let response = self.client
            .post(&url)
            .header("apikey", &config.anon_key)
            .header("Authorization", format!("Bearer {}", &config.anon_key))
            .header("Content-Type", "application/json")
            .header("Prefer", "return=minimal")
            .json(&payload)
            .send()
            .await?;
        
        let status = response.status();
        let duration = start.elapsed().as_millis() as u64;
        
        // Record operation in sled
        let record = OperationRecord {
            id: operation_id.clone(),
            operation_type: OperationType::Embedding,
            timestamp: Utc::now(),
            request_data: json!({
                "text_length": text.len(),
                "embedding_dims": embedding.len(),
            }),
            response_data: None,
            duration_ms: duration,
            status: status.to_string(),
            error: if !status.is_success() {
                Some(format!("Failed to store embedding: {}", status))
            } else {
                None
            },
        };
        
        self.record_operation(record).await?;
        
        Ok(operation_id)
    }

    /// Vector search in Supabase and record operation
    pub async fn vector_search(
        &self,
        embedding: Vec<f32>,
        limit: usize,
        threshold: f32,
    ) -> Result<Vec<serde_json::Value>> {
        let start = std::time::Instant::now();
        let operation_id = uuid::Uuid::new_v4().to_string();
        
        let config = self.config.read().await;
        let url = format!("{}/rest/v1/rpc/vector_search", config.url);
        
        let payload = json!({
            "query_embedding": embedding,
            "match_count": limit,
            "match_threshold": threshold,
        });
        
        let response = self.client
            .post(&url)
            .header("apikey", &config.anon_key)
            .header("Authorization", format!("Bearer {}", &config.anon_key))
            .json(&payload)
            .send()
            .await?;
        
        let status = response.status();
        let results = response.json::<Vec<serde_json::Value>>().await?;
        let duration = start.elapsed().as_millis() as u64;
        
        // Record operation in sled
        let record = OperationRecord {
            id: operation_id,
            operation_type: OperationType::VectorSearch,
            timestamp: Utc::now(),
            request_data: json!({
                "embedding_dims": embedding.len(),
                "limit": limit,
                "threshold": threshold,
            }),
            response_data: Some(json!({
                "results_count": results.len(),
            })),
            duration_ms: duration,
            status: status.to_string(),
            error: None,
        };
        
        self.record_operation(record).await?;
        
        Ok(results)
    }

    /// Get operation history from sled
    pub async fn get_operation_history(
        &self,
        limit: usize,
    ) -> Result<Vec<OperationRecord>> {
        let mut operations = Vec::new();
        
        for result in self.sled_db.iter().rev().take(limit) {
            let (_, value) = result?;
            let operation: OperationRecord = serde_json::from_slice(&value)?;
            operations.push(operation);
        }
        
        Ok(operations)
    }

    /// Get operation statistics from sled
    pub async fn get_operation_stats(&self) -> Result<serde_json::Value> {
        let mut total_count = 0;
        let mut type_counts = std::collections::HashMap::new();
        let mut total_duration = 0u64;
        let mut error_count = 0;
        
        for result in self.sled_db.iter() {
            let (_, value) = result?;
            let operation: OperationRecord = serde_json::from_slice(&value)?;
            
            total_count += 1;
            total_duration += operation.duration_ms;
            
            if operation.error.is_some() {
                error_count += 1;
            }
            
            let type_name = format!("{:?}", operation.operation_type);
            *type_counts.entry(type_name).or_insert(0) += 1;
        }
        
        let avg_duration = if total_count > 0 {
            total_duration / total_count as u64
        } else {
            0
        };
        
        Ok(json!({
            "total_operations": total_count,
            "operation_types": type_counts,
            "average_duration_ms": avg_duration,
            "total_duration_ms": total_duration,
            "error_count": error_count,
            "success_rate": if total_count > 0 {
                ((total_count - error_count) as f64 / total_count as f64) * 100.0
            } else {
                0.0
            },
        }))
    }

    /// Clear all operation records from sled
    pub async fn clear_history(&self) -> Result<()> {
        self.sled_db.clear()?;
        self.sled_db.flush()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_record_operation() {
        let temp_dir = TempDir::new().unwrap();
        let sled_path = temp_dir.path().join("test.db");
        
        let config = SupabaseConfig::default();
        let manager = HanzoSupabaseManager::new(config, sled_path.to_str().unwrap())
            .await
            .unwrap();
        
        let record = OperationRecord {
            id: "test-123".to_string(),
            operation_type: OperationType::Inference,
            timestamp: Utc::now(),
            request_data: json!({"test": "data"}),
            response_data: None,
            duration_ms: 100,
            status: "200".to_string(),
            error: None,
        };
        
        manager.record_operation(record).await.unwrap();
        
        let history = manager.get_operation_history(1).await.unwrap();
        assert_eq!(history.len(), 1);
        assert_eq!(history[0].id, "test-123");
    }

    #[tokio::test]
    async fn test_operation_stats() {
        let temp_dir = TempDir::new().unwrap();
        let sled_path = temp_dir.path().join("test.db");
        
        let config = SupabaseConfig::default();
        let manager = HanzoSupabaseManager::new(config, sled_path.to_str().unwrap())
            .await
            .unwrap();
        
        // Record multiple operations
        for i in 0..5 {
            let record = OperationRecord {
                id: format!("test-{}", i),
                operation_type: if i % 2 == 0 {
                    OperationType::Inference
                } else {
                    OperationType::Embedding
                },
                timestamp: Utc::now(),
                request_data: json!({}),
                response_data: None,
                duration_ms: 100 * (i as u64 + 1),
                status: "200".to_string(),
                error: if i == 4 { Some("Error".to_string()) } else { None },
            };
            manager.record_operation(record).await.unwrap();
        }
        
        let stats = manager.get_operation_stats().await.unwrap();
        assert_eq!(stats["total_operations"], 5);
        assert_eq!(stats["error_count"], 1);
        assert_eq!(stats["average_duration_ms"], 300); // (100+200+300+400+500)/5
    }
}
