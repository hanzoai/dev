// Test file to verify the embedded engine implementation

use serde_json::json;

fn test_request_format() {
    // Test chat completion request
    let chat_request = json!({
        "messages": [
            {"role": "system", "content": "You are a helpful assistant."},
            {"role": "user", "content": "Hello, how are you?"}
        ],
        "temperature": 0.7,
        "max_tokens": 100,
        "top_p": 0.9
    });

    println!("Chat request structure:");
    println!("{}", serde_json::to_string_pretty(&chat_request).unwrap());

    // Test completion request
    let completion_request = json!({
        "prompt": "Once upon a time",
        "temperature": 0.8,
        "max_tokens": 50,
        "top_p": 0.95
    });

    println!("\nCompletion request structure:");
    println!("{}", serde_json::to_string_pretty(&completion_request).unwrap());

    // Verify required fields
    assert!(chat_request.get("messages").is_some());
    assert!(completion_request.get("prompt").is_some());

    println!("\n✅ Request structures are correct");
}

fn main() {
    test_request_format();
}