//! Example: Run the Chain with native KuzuDB ledger

use anyhow::Result;
use hanzod::chain::{Chain, ChainConfig};
use hanzod::database::{DatabaseManager, DatabaseConfig, DatabaseBackend};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();

    println!("🚀 Starting Chain with native KuzuDB ledger...");

    // Configure KuzuDB as the database backend
    let db_config = DatabaseConfig {
        backend: DatabaseBackend::KuzuDB,
        path: Some("/tmp/hanzo_chain_kuzu".into()),
        cache_size: 100_000_000, // 100MB cache
        enable_wal: true,
        encryption_key: None,
    };

    // Initialize database manager
    let db_manager = Arc::new(DatabaseManager::new(db_config).await?);
    println!("✅ KuzuDB initialized at /tmp/hanzo_chain_kuzu");

    // Create chain configuration
    let chain_config = ChainConfig {
        chain_id: "hanzo-mainnet".to_string(),
        block_time_ms: 1000,
        max_block_size: 1_000_000,
        enable_checkpoints: true,
        checkpoint_interval: 100,
    };

    // Initialize the chain
    let chain = Arc::new(Chain::new(chain_config, db_manager.clone()).await?);
    println!("✅ Chain initialized with ID: hanzo-mainnet");

    // Process a test operation
    println!("\n📝 Processing test AI operation...");
    let result = chain.process_ai_operation(
        "Hello, this is a test of the Hanzo Chain with KuzuDB!",
        true, // Store the operation
        false, // Skip inference for now
    ).await?;

    println!("✅ Operation processed:");
    println!("   - Doc ID: {}", result.doc_id);
    println!("   - Embedding size: {} dimensions", result.embedding.len());
    println!("   - Block height: {}", result.block_height);
    println!("   - Processing time: {:?}", result.latency);

    // Query the graph database
    println!("\n🔍 Querying KuzuDB graph...");
    let graph_result = chain.graph_query(
        "MATCH (n) RETURN count(n) as node_count"
    ).await?;
    println!("✅ Graph query result: {}", serde_json::to_string_pretty(&graph_result)?);

    // Show blockchain stats
    let stats = chain.get_stats().await?;
    println!("\n📊 Chain Statistics:");
    println!("   - Total blocks: {}", stats.total_blocks);
    println!("   - Total transactions: {}", stats.total_transactions);
    println!("   - Total documents: {}", stats.total_documents);
    println!("   - Database backend: KuzuDB (native)");

    println!("\n✨ Chain with KuzuDB is running successfully!");

    Ok(())
}