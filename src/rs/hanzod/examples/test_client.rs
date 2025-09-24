//! Test client for Hanzo AI Blockchain
//! 
//! Usage: cargo run --example test_client

use anyhow::Result;
use reqwest;
use serde_json::json;

#[tokio::main]
async fn main() -> Result<()> {
    println!("\n{}", "=".repeat(60));
    println!("🧪 HANZO AI BLOCKCHAIN - TEST CLIENT");
    println!("{}", "=".repeat(60));

    let base_url = "http://localhost:8545";
    let client = reqwest::Client::new();

    // Test 1: Check health
    println!("\n1️⃣ Testing /health endpoint...");
    match client.get(format!("{}/health", base_url)).send().await {
        Ok(resp) => {
            let status = resp.status();
            let body: serde_json::Value = resp.json().await?;
            println!("   ✅ Health check: {} - {}", status, body);
        }
        Err(e) => {
            println!("   ❌ Server not running: {}", e);
            println!("   💡 Start the server with: cargo run --example run_local");
            return Ok(());
        }
    }

    // Test 2: Get chain status
    println!("\n2️⃣ Testing /status endpoint...");
    let resp = client.get(format!("{}/status", base_url)).send().await?;
    let status: serde_json::Value = resp.json().await?;
    println!("   Chain Status:");
    println!("   • Chain ID: {}", status["chain_id"]);
    println!("   • Network ID: {}", status["network_id"]);
    println!("   • Block Height: {}", status["block_height"]);
    println!("   • Validators: {}", status["validators"]);

    // Test 3: Get validators
    println!("\n3️⃣ Testing /v1/validators endpoint...");
    let resp = client.get(format!("{}/v1/validators", base_url)).send().await?;
    let validators: serde_json::Value = resp.json().await?;
    println!("   Validators: {}", serde_json::to_string_pretty(&validators)?);

    // Test 4: Test inference
    println!("\n4️⃣ Testing /v1/inference endpoint...");
    let inference_request = json!({
        "model": "qwen3-next:8b",
        "prompt": "Explain blockchain consensus",
        "max_tokens": 100
    });
    
    let resp = client
        .post(format!("{}/v1/inference", base_url))
        .json(&inference_request)
        .send()
        .await?;
    
    let result: serde_json::Value = resp.json().await?;
    println!("   Inference Result:");
    println!("   • Model: {}", result["model"]);
    println!("   • Response: {}", result["response"]);
    println!("   • Tokens: {}", result["tokens_used"]);
    println!("   • Latency: {}ms", result["latency_ms"]);

    // Test 5: Test JSON-RPC
    println!("\n5️⃣ Testing JSON-RPC interface...");
    test_json_rpc(&client, base_url).await?;

    println!("\n✅ All tests completed!");
    println!("{}", "=".repeat(60));

    Ok(())
}

async fn test_json_rpc(client: &reqwest::Client, base_url: &str) -> Result<()> {
    // Test eth_blockNumber
    let rpc_request = json!({
        "jsonrpc": "2.0",
        "method": "eth_blockNumber",
        "params": [],
        "id": 1
    });
    
    match client
        .post(base_url)
        .json(&rpc_request)
        .send()
        .await
    {
        Ok(resp) => {
            if resp.status().is_success() {
                let result: serde_json::Value = resp.json().await?;
                println!("   JSON-RPC Response: {}", serde_json::to_string_pretty(&result)?);
            } else {
                println!("   ⚠️ JSON-RPC not fully implemented (status: {})", resp.status());
            }
        }
        Err(_) => {
            println!("   ⚠️ JSON-RPC endpoint not available");
        }
    }
    
    Ok(())
}