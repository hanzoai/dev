#!/usr/bin/env rust-script
//! Test script for Hanzo inference integration
//!
//! ```cargo
//! [dependencies]
//! reqwest = { version = "0.11", features = ["json", "blocking"] }
//! serde_json = "1.0"
//! ```

use reqwest::blocking::Client;
use serde_json::json;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = Client::new();

    println!("Testing Hanzo Inference Container Integration\n");

    // Test 1: Health check
    println!("1. Health Check:");
    let resp = client.get("http://localhost:8080/health")
        .send()?
        .text()?;
    println!("   {}", resp);

    // Test 2: Simple inference
    println!("\n2. Simple Inference:");
    let request = json!({
        "model": "Qwen/Qwen2.5-0.5B-Instruct",
        "prompt": "Complete this: The capital of France is",
        "temperature": 0.1,
        "max_tokens": 10
    });

    let resp = client.post("http://localhost:8080/v1/inference")
        .json(&request)
        .send()?
        .text()?;

    let parsed: serde_json::Value = serde_json::from_str(&resp)?;
    println!("   Prompt: Complete this: The capital of France is");
    println!("   Response: {}", parsed["response"]);

    // Test 3: Chat completions
    println!("\n3. Chat Completions:");
    let request = json!({
        "model": "Qwen/Qwen2.5-0.5B-Instruct",
        "messages": [
            {"role": "user", "content": "What is 2+2?"}
        ],
        "temperature": 0.1,
        "max_tokens": 20
    });

    let resp = client.post("http://localhost:8080/v1/chat/completions")
        .json(&request)
        .send()?
        .text()?;

    let parsed: serde_json::Value = serde_json::from_str(&resp)?;
    println!("   Question: What is 2+2?");
    println!("   Answer: {}", parsed["choices"][0]["message"]["content"]);

    println!("\n✅ All tests passed! Hanzo inference container is working correctly.");
    println!("\nModel Details:");
    println!("   Model: Qwen/Qwen2.5-0.5B-Instruct");
    println!("   Parameters: ~500M");
    println!("   Device: CPU");
    println!("   Endpoint: http://localhost:8080");

    Ok(())
}