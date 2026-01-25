# RPC Protocol

ZAP provides a simple yet powerful RPC protocol for agent-tool communication. This document covers request/response patterns, streaming, pipelining, and error handling.

## Connection Lifecycle

```
Client                              Server
   |                                   |
   |-------- Init -------------------->|
   |<------- InitAck -----------------|
   |                                   |
   |-------- Request 1 --------------->|
   |<------- Response 1 ---------------|
   |                                   |
   |-------- Request 2 --------------->|
   |-------- Request 3 --------------->|  (pipelined)
   |<------- Response 2 ---------------|
   |<------- Response 3 ---------------|
   |                                   |
   |-------- Close ------------------->|
   |                                   |
```

## Handshake

Every connection begins with a handshake:

### Client Init

```json
{
  "name": "my-agent",
  "version": "1.0.0"
}
```

### Server InitAck

```json
{
  "name": "hanzo-zap-gateway",
  "version": "0.1.0",
  "capabilities": {
    "tools": true,
    "resources": true,
    "prompts": true,
    "logging": true
  }
}
```

The `capabilities` object indicates which features the server supports:
- `tools`: Tool listing and execution
- `resources`: Resource listing and reading
- `prompts`: Prompt templates
- `logging`: Server-side logging

## Request/Response Pattern

### Listing Tools

**Request** (MsgType: `0x10`):
```
Empty payload
```

**Response** (MsgType: `0x11`):
```json
[
  {
    "name": "read_file",
    "description": "Read contents of a file",
    "inputSchema": {
      "type": "object",
      "required": ["path"],
      "properties": {
        "path": { "type": "string" },
        "offset": { "type": "integer" },
        "limit": { "type": "integer" }
      }
    }
  },
  {
    "name": "write_file",
    "description": "Write contents to a file",
    "inputSchema": { ... }
  }
]
```

### Calling a Tool

**Request** (MsgType: `0x12`):
```json
{
  "id": "req-1",
  "name": "read_file",
  "arguments": {
    "path": "/src/main.rs"
  },
  "metadata": {
    "session_id": "agent-001"
  }
}
```

**Response** (MsgType: `0x13`):
```json
{
  "id": "req-1",
  "content": "fn main() {\n    println!(\"Hello\");\n}",
  "metadata": {
    "mime_type": "text/x-rust",
    "size": "42"
  }
}
```

### Error Response

**Response** (MsgType: `0xFE`):
```json
{
  "code": -32601,
  "message": "Tool not found: unknown_tool"
}
```

## Pipelining

ZAP supports request pipelining - sending multiple requests without waiting for responses:

```
Client                              Server
   |                                   |
   |-------- CallTool(A) ------------->|
   |-------- CallTool(B) ------------->|  <- Sent immediately
   |-------- CallTool(C) ------------->|
   |                                   |
   |<------- Result(A) ----------------|  <- Responses in order
   |<------- Result(B) ----------------|
   |<------- Result(C) ----------------|
```

### Request IDs

Each request has a unique ID that ties it to its response:

```rust
async fn pipeline_example(client: &mut Client) -> Result<Vec<ToolResult>> {
    // Send all requests without waiting
    let futures = vec![
        client.call_tool("read_file", json!({"path": "a.txt"})),
        client.call_tool("read_file", json!({"path": "b.txt"})),
        client.call_tool("read_file", json!({"path": "c.txt"})),
    ];

    // Wait for all responses
    let results = futures::future::join_all(futures).await;
    results.into_iter().collect()
}
```

### Out-of-Order Responses

While requests are processed in order by default, servers may return responses out of order for concurrent execution:

```
Client                              Server
   |                                   |
   |-------- CallTool(A, slow) ------->|
   |-------- CallTool(B, fast) ------->|
   |                                   |
   |<------- Result(B) ----------------|  <- B finishes first
   |<------- Result(A) ----------------|  <- A finishes later
```

Match responses by `id`, not order:

```rust
fn match_response(request_id: &str, response: &ToolResult) -> bool {
    response.id == request_id
}
```

## Streaming

ZAP supports streaming for long-running operations (future extension):

### Server-Sent Events Pattern

```
Client                              Server
   |                                   |
   |-------- CallTool(exec) ---------> |
   |<------- StreamStart(id) ---------|
   |<------- StreamChunk(stdout) -----|
   |<------- StreamChunk(stdout) -----|
   |<------- StreamChunk(stderr) -----|
   |<------- StreamEnd(result) -------|
```

### Streaming Implementation (Future)

```rust
// Subscribe to streaming responses
let (result_rx, final_rx) = client.call_tool_streaming("exec", json!({
    "command": "cargo build"
})).await?;

// Process streaming chunks
while let Some(chunk) = result_rx.recv().await {
    match chunk {
        StreamChunk::Stdout(data) => print!("{}", data),
        StreamChunk::Stderr(data) => eprint!("{}", data),
        StreamChunk::Progress(pct) => update_progress(pct),
    }
}

// Get final result
let result = final_rx.await?;
```

## Cancellation

Clients can cancel in-flight requests:

### Cancel Request (MsgType: `0xF0`)

```json
{
  "request_id": "req-42"
}
```

### Cancel Acknowledgment (MsgType: `0xF1`)

```json
{
  "request_id": "req-42",
  "cancelled": true
}
```

### Implementation

```rust
let handle = client.call_tool_async("exec", json!({
    "command": "cargo build --release"
}));

// Cancel after timeout
tokio::select! {
    result = handle.await => {
        println!("Completed: {:?}", result);
    }
    _ = tokio::time::sleep(Duration::from_secs(60)) => {
        handle.cancel().await?;
        println!("Cancelled due to timeout");
    }
}
```

## Batching

Multiple tool calls can be batched into a single request:

### Batch Request (MsgType: `0x14`)

```json
{
  "requests": [
    {"id": "1", "name": "read_file", "arguments": {"path": "a.txt"}},
    {"id": "2", "name": "read_file", "arguments": {"path": "b.txt"}},
    {"id": "3", "name": "git_status", "arguments": {}}
  ]
}
```

### Batch Response (MsgType: `0x15`)

```json
{
  "responses": [
    {"id": "1", "content": "contents of a.txt"},
    {"id": "2", "content": "contents of b.txt"},
    {"id": "3", "content": {"branch": "main", "staged": []}}
  ]
}
```

### When to Batch

- **Batch**: Multiple independent reads
- **Don't batch**: Operations with dependencies
- **Don't batch**: Operations that might fail and block others

## Error Handling

### Error Codes

| Code | Name | Description |
|------|------|-------------|
| -32700 | Parse error | Invalid JSON in request |
| -32600 | Invalid request | Request structure invalid |
| -32601 | Method not found | Tool does not exist |
| -32602 | Invalid params | Arguments invalid |
| -32603 | Internal error | Server-side error |
| -32000 | Server error | Generic server error |
| -32001 | Timeout | Request timed out |
| -32002 | Permission denied | Not authorized |
| -32003 | Cancelled | Request was cancelled |

### Error Response Format

```json
{
  "code": -32602,
  "message": "Invalid params: path is required",
  "data": {
    "field": "path",
    "type": "missing"
  }
}
```

### Handling Errors

```rust
match client.call_tool("read_file", args).await {
    Ok(result) => {
        if let Some(error) = result.error {
            // Tool-level error (file not found, etc.)
            eprintln!("Tool error: {}", error);
        } else {
            // Success
            process_content(result.content);
        }
    }
    Err(Error::Protocol(msg)) => {
        // Protocol error (connection issues, parse errors)
        eprintln!("Protocol error: {}", msg);
    }
    Err(Error::Timeout) => {
        // Request timed out
        eprintln!("Request timed out");
    }
    Err(e) => {
        // Other errors
        eprintln!("Error: {}", e);
    }
}
```

### Partial Success (Batching)

With batched requests, some may succeed while others fail:

```json
{
  "responses": [
    {"id": "1", "content": "success"},
    {"id": "2", "error": "File not found: b.txt"},
    {"id": "3", "content": {"branch": "main"}}
  ]
}
```

## Timeouts

### Request Timeout

Set per-request timeout:

```rust
let result = client.call_tool_with_timeout(
    "exec",
    json!({"command": "long_build"}),
    Duration::from_secs(300)  // 5 minutes
).await?;
```

### Connection Timeout

Set connection-level timeout:

```rust
let client = Client::connect_with_options("zap://localhost:9999", Options {
    connect_timeout: Duration::from_secs(10),
    request_timeout: Duration::from_secs(30),
}).await?;
```

### Server-Side Timeout

Servers may enforce their own timeouts:

```json
{
  "code": -32001,
  "message": "Request timed out after 30000ms"
}
```

## Keep-Alive

ZAP connections support keep-alive pings:

### Ping (MsgType: `0xE0`)

```
Empty payload
```

### Pong (MsgType: `0xE1`)

```
Empty payload
```

### Implementation

```rust
// Client sends periodic pings
let ping_interval = Duration::from_secs(30);
tokio::spawn(async move {
    loop {
        tokio::time::sleep(ping_interval).await;
        if client.ping().await.is_err() {
            // Connection lost
            break;
        }
    }
});
```

## Reconnection

Clients should handle disconnections gracefully:

```rust
async fn resilient_client() -> Result<Client> {
    let mut attempts = 0;
    loop {
        match Client::connect("zap://localhost:9999").await {
            Ok(client) => return Ok(client),
            Err(e) => {
                attempts += 1;
                if attempts >= 5 {
                    return Err(e);
                }
                let backoff = Duration::from_millis(100 * 2u64.pow(attempts));
                tokio::time::sleep(backoff).await;
            }
        }
    }
}
```

## Protocol Version Negotiation

Future versions may negotiate capabilities during handshake:

```json
// Init with version preferences
{
  "name": "my-agent",
  "version": "1.0.0",
  "protocol_versions": ["1.0", "1.1"],
  "features": ["streaming", "batching"]
}

// InitAck with negotiated version
{
  "name": "zap-gateway",
  "version": "0.2.0",
  "protocol_version": "1.1",
  "features": ["streaming"]  // Only streaming supported
}
```

## Best Practices

### 1. Always Check Response IDs

```rust
// Good
let response = client.recv_response().await?;
assert_eq!(response.id, request.id);

// Bad - assuming order
let response = client.recv_response().await?;
// What if responses are out of order?
```

### 2. Handle Partial Failures

```rust
let batch_result = client.batch(requests).await?;
for response in batch_result.responses {
    if let Some(error) = response.error {
        log::warn!("Request {} failed: {}", response.id, error);
    }
}
```

### 3. Use Appropriate Timeouts

```rust
// Short timeout for simple operations
let status = client.call_tool_with_timeout(
    "git_status", json!({}),
    Duration::from_secs(5)
).await?;

// Long timeout for builds
let build = client.call_tool_with_timeout(
    "build", json!({"release": true}),
    Duration::from_secs(600)
).await?;
```

### 4. Pipeline Independent Requests

```rust
// Good - parallel file reads
let files = ["a.rs", "b.rs", "c.rs"];
let futures: Vec<_> = files.iter()
    .map(|f| client.call_tool("read_file", json!({"path": f})))
    .collect();
let results = futures::future::join_all(futures).await;

// Bad - sequential reads
for file in files {
    let result = client.call_tool("read_file", json!({"path": file})).await?;
}
```

## Next Steps

- [Security](security.md): Permission checking and sandboxing
- [Tool Reference](tools.md): Complete API documentation
