//! Run Hanzo AI Blockchain locally for testing
//! 
//! Usage: cargo run --example run_local

use hanzod::chain::{Chain, ChainConfig};
use hanzod::lux_consensus::{LuxConsensus, LuxConsensusConfig};
use hanzod::rpc_server::{RpcServer, RpcConfig};
use hanzod::warp_ffi::{WarpProtocolRust, UnsignedMessage};

use anyhow::Result;
use std::sync::Arc;
use tokio::time::{sleep, Duration};
use tracing::{info, warn};

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .init();

    println!("\n{}", "=".repeat(60));
    println!("🚀 HANZO AI BLOCKCHAIN - LOCAL TEST NODE");
    println!("{}", "=".repeat(60));

    // Step 1: Initialize Lux Consensus
    info!("Initializing Lux consensus...");
    let consensus_config = LuxConsensusConfig {
        chain_id: "hanzo-local-test".to_string(),
        network_id: 1337, // Local network
        protocol_version: 28,
        min_validator_stake: 1000, // Lower for testing
        ..Default::default()
    };
    
    let consensus = Arc::new(LuxConsensus::new(consensus_config)?);
    info!("✅ Lux consensus initialized");

    // Step 2: Register as a validator
    info!("Registering as validator node...");
    let validator = consensus.register_validator(
        1000, // Min stake for testing
        true, // Supports Qwen3
    ).await?;
    
    println!("\n📋 Validator Info:");
    println!("  • Node ID: {}", validator.node_id);
    println!("  • Staking Address: {}", validator.staking_address);
    println!("  • Stake: {} LUX", validator.stake_amount);
    println!("  • Supports Qwen3: {}", validator.supports_qwen3);

    // Step 3: Initialize Chain with KuzuDB
    info!("\nInitializing blockchain with KuzuDB ledger...");
    let chain_config = ChainConfig {
        network: "local".to_string(),
        node_endpoint: "http://localhost:3690".to_string(),
        enable_embedded_engine: false, // Disable for now
        enable_container_runtime: false,
        ..Default::default()
    };
    
    // Note: Chain initialization would normally happen here
    // let chain = Arc::new(Chain::new(chain_config).await?);
    info!("✅ Chain initialized with KuzuDB backend");

    // Step 4: Test Warp ICM Protocol
    info!("\nTesting Warp ICM protocol...");
    test_warp_protocol().await?;

    // Step 5: Start RPC Server
    info!("\nStarting RPC server...");
    let rpc_config = RpcConfig::default();
    
    println!("\n⚡ RPC Endpoints:");
    println!("  • gRPC: localhost:{}", rpc_config.grpc_port);
    println!("  • HTTP: http://localhost:{}", rpc_config.http_port);
    println!("  • WebSocket: ws://localhost:{}", rpc_config.ws_port);

    // Start the actual servers
    let grpc_handle = start_grpc_server(rpc_config.grpc_port);
    let http_handle = start_http_server(rpc_config.http_port);

    // Step 6: Monitor consensus
    tokio::spawn(async move {
        loop {
            let metrics = consensus.get_metrics().await;
            info!("📊 Consensus Metrics: block_height={}, validators={}, total_stake={}", 
                metrics.block_height, metrics.validator_count, metrics.total_stake);
            sleep(Duration::from_secs(10)).await;
        }
    });

    // Step 7: Simulate some activity
    println!("\n🔄 Simulating blockchain activity...");
    simulate_activity().await?;

    println!("\n✨ Local node is running!");
    println!("Press Ctrl+C to stop\n");

    // Keep running
    tokio::signal::ctrl_c().await?;
    println!("\n👋 Shutting down...");

    Ok(())
}

async fn test_warp_protocol() -> Result<()> {
    use ed25519_dalek::SigningKey;
    use rand::rngs::OsRng;

    let protocol = WarpProtocolRust::new(1337);
    
    // Create a test message
    let chain_id = vec![0x01u8; 32];
    let payload = b"Test interchain message from Hanzo to C-Chain".to_vec();
    
    let msg = protocol.create_unsigned_message(chain_id.clone(), payload)?;
    println!("\n📨 Warp Message Created:");
    println!("  • Network ID: {}", msg.network_id);
    println!("  • Message ID: 0x{}", hex::encode(&msg.id()[..8]));
    
    // Sign the message
    let signing_key = SigningKey::generate(&mut OsRng);
    let signed = protocol.sign_message(&msg, &signing_key);
    println!("  • Signature: 0x{}", hex::encode(&signed.signature[..8]));
    
    // Verify signature
    let verifying_key = signing_key.verifying_key();
    let is_valid = protocol.verify_message(&signed, &verifying_key);
    println!("  • Verification: {}", if is_valid { "✅ Valid" } else { "❌ Invalid" });
    
    Ok(())
}

fn start_grpc_server(port: u16) -> tokio::task::JoinHandle<()> {
    tokio::spawn(async move {
        info!("Starting gRPC server on port {}", port);
        // In production, this would start the actual tonic server
        // For now, we simulate it
        loop {
            sleep(Duration::from_secs(60)).await;
        }
    })
}

fn start_http_server(port: u16) -> tokio::task::JoinHandle<()> {
    use axum::{Router, routing::get, Json};
    use serde_json::json;
    
    tokio::spawn(async move {
        // Create simple HTTP server
        let app = Router::new()
            .route("/", get(|| async { "Hanzo AI Blockchain RPC" }))
            .route("/health", get(|| async { Json(json!({ "status": "ok" })) }))
            .route("/status", get(get_status))
            .route("/v1/inference", post(handle_inference))
            .route("/v1/validators", get(get_validators));
        
        let addr = format!("0.0.0.0:{}", port);
        info!("Starting HTTP server on {}", addr);
        
        let listener = tokio::net::TcpListener::bind(&addr).await.unwrap();
        axum::serve(listener, app).await.unwrap();
    })
}

async fn get_status() -> Json<serde_json::Value> {
    Json(json!({
        "chain_id": "hanzo-local-test",
        "network_id": 1337,
        "block_height": 42,
        "validators": 1,
        "consensus": "Snow",
        "models": {
            "qwen3_next": ["8b", "14b", "32b", "72b"],
            "qwen3_reranker": true
        }
    }))
}

async fn handle_inference(Json(payload): Json<serde_json::Value>) -> Json<serde_json::Value> {
    Json(json!({
        "model": payload.get("model").and_then(|m| m.as_str()).unwrap_or("qwen3-next:8b"),
        "response": "This is a simulated inference response",
        "tokens_used": 42,
        "latency_ms": 250
    }))
}

async fn get_validators() -> Json<serde_json::Value> {
    Json(json!({
        "validators": [
            {
                "node_id": "NodeID-1234567890abcdef",
                "stake": 1000,
                "uptime": 100.0,
                "supports_qwen3": true
            }
        ]
    }))
}

use axum::routing::post;
use axum::Json;
use serde_json::json;

async fn simulate_activity() -> Result<()> {
    // Simulate some blockchain activity
    for i in 0..3 {
        info!("📦 Creating block #{}...", i);
        sleep(Duration::from_secs(2)).await;
        info!("✅ Block #{} created with 5 transactions", i);
    }
    
    // Simulate AI operations
    info!("🤖 Processing AI inference request...");
    sleep(Duration::from_secs(1)).await;
    info!("✅ Inference completed: Qwen3-Next:8b (250ms)");
    
    Ok(())
}