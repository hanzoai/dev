use reqwest;
use serde_json::json;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Testing Hanzod Integration...\n");

    // Test health endpoint
    println!("1. Testing health endpoint:");
    let health_response = reqwest::get("http://localhost:8080/health").await?;
    let health_text = health_response.text().await?;
    println!("   Health: {}", health_text);

    // Test inference endpoint
    println!("\n2. Testing inference endpoint:");
    let client = reqwest::Client::new();
    let inference_request = json!({
        "model": "qwen3-4b-thinking-2507",
        "prompt": "What is the capital of France?",
        "temperature": 0.7,
        "max_tokens": 50,
        "stream": false
    });

    let inference_response = client
        .post("http://localhost:8080/v1/inference")
        .json(&inference_request)
        .send()
        .await?;

    let status = inference_response.status();
    let response_text = inference_response.text().await?;
    println!("   Status: {}", status);
    println!("   Response: {}", response_text);

    // Test gRPC health check
    println!("\n3. Testing gRPC endpoint (port 50051):");
    // Note: This would require a gRPC client, showing connection test only
    match std::net::TcpStream::connect("127.0.0.1:50051") {
        Ok(_) => println!("   gRPC port is open and listening"),
        Err(e) => println!("   gRPC port error: {}", e),
    }

    println!("\n✅ Hanzod integration test complete!");

    Ok(())
}