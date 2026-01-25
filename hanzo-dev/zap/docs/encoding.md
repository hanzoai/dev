# Wire Format Specification

ZAP uses a simple, efficient binary wire format designed for zero-copy parsing and minimal overhead.

## Design Goals

1. **Zero-copy parsing**: Read data directly from network buffers
2. **Minimal framing**: Simple length-prefixed messages
3. **Streaming friendly**: Process messages before fully received
4. **Language agnostic**: Easy to implement in any language

## Message Framing

Every ZAP message consists of a fixed header followed by a variable-length payload:

```
+----------+----------+------------------+
| Length   | MsgType  | Payload          |
| (4 bytes)| (1 byte) | (variable)       |
| LE u32   |          |                  |
+----------+----------+------------------+
```

### Field Descriptions

| Field | Size | Type | Description |
|-------|------|------|-------------|
| Length | 4 bytes | Little-endian u32 | Total size of MsgType + Payload |
| MsgType | 1 byte | u8 | Message type code |
| Payload | Variable | bytes | JSON or binary payload |

### Example: ListTools Request

```
Hex:  01 00 00 00 10
      |________|  |
      Length=1    MsgType=0x10 (ListTools)

No payload for ListTools request.
```

### Example: CallTool Request

```
Hex:  2A 00 00 00 12 7B 22 6E 61 6D 65 22 3A 22 72 65 61 64 5F 66 69 6C 65 22 2C 22 61 72 67 73 22 3A 7B 22 70 61 74 68 22 3A 22 2E 22 7D 7D
      |________|  |  |_______________________________________________________________________________________________________________|
      Length=42  MT  JSON payload: {"name":"read_file","args":{"path":"."}}
```

## Message Types

| Type | Code | Direction | Description |
|------|------|-----------|-------------|
| `Init` | `0x01` | C->S | Client handshake |
| `InitAck` | `0x02` | S->C | Server handshake response |
| `ListTools` | `0x10` | C->S | Request available tools |
| `ListToolsResponse` | `0x11` | S->C | Tool list |
| `CallTool` | `0x12` | C->S | Execute a tool |
| `CallToolResponse` | `0x13` | S->C | Tool result |
| `ListResources` | `0x20` | C->S | Request resources |
| `ListResourcesResponse` | `0x21` | S->C | Resource list |
| `ReadResource` | `0x22` | C->S | Read a resource |
| `ReadResourceResponse` | `0x23` | S->C | Resource content |
| `ListPrompts` | `0x30` | C->S | Request prompts |
| `ListPromptsResponse` | `0x31` | S->C | Prompt list |
| `GetPrompt` | `0x32` | C->S | Get a prompt |
| `GetPromptResponse` | `0x33` | S->C | Prompt content |
| `AddServer` | `0x40` | C->S | Add MCP server (gateway) |
| `AddServerResponse` | `0x41` | S->C | Server ID |
| `RemoveServer` | `0x42` | C->S | Remove MCP server |
| `RemoveServerResponse` | `0x43` | S->C | Success flag |
| `ListServers` | `0x44` | C->S | List connected servers |
| `ListServersResponse` | `0x45` | S->C | Server list |
| `Error` | `0xFE` | S->C | Error response |
| `Close` | `0xFF` | Both | Close connection |

## Payload Encoding

Payloads use JSON encoding with the following conventions:

### String Encoding

Strings are UTF-8 encoded JSON strings:

```json
{"path": "/home/user/file.txt"}
```

### Binary Data

Binary data is base64-encoded in JSON:

```json
{"content": "SGVsbG8gV29ybGQh"}
```

For large binary transfers, consider using resources instead of tool calls.

### Numbers

- Integers: JSON numbers (no quotes)
- Floats: JSON numbers with decimal point
- Large integers (>53 bits): JSON strings to preserve precision

```json
{
  "count": 42,
  "ratio": 3.14159,
  "big_id": "9007199254740993"
}
```

### Timestamps

ISO 8601 format in UTC:

```json
{"created_at": "2025-01-15T10:30:00Z"}
```

### Null vs Absent

- `null`: Field is explicitly empty
- Absent: Field uses default value

```json
// Explicit null - offset is definitely 0
{"path": "/file.txt", "offset": null}

// Absent - offset uses default (also 0, but semantically different)
{"path": "/file.txt"}
```

## Zero-Copy Reading

ZAP's wire format enables zero-copy parsing. The reader maintains a position in the original buffer:

```rust
pub struct Reader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    /// Read bytes without copying - returns slice into original buffer
    pub fn read_bytes(&mut self) -> Result<&'a [u8]> {
        let len = self.read_u32_le()? as usize;
        let bytes = &self.data[self.pos..self.pos + len];
        self.pos += len;
        Ok(bytes)  // Zero-copy: points into original data
    }

    /// Read string without copying
    pub fn read_string(&mut self) -> Result<&'a str> {
        let bytes = self.read_bytes()?;
        std::str::from_utf8(bytes)
    }
}
```

## Primitive Encoding

For binary payloads (not JSON), ZAP uses these formats:

| Type | Size | Format |
|------|------|--------|
| `u8` | 1 | Unsigned byte |
| `u16` | 2 | Little-endian |
| `u32` | 4 | Little-endian |
| `u64` | 8 | Little-endian |
| `i32` | 4 | Little-endian, two's complement |
| `i64` | 8 | Little-endian, two's complement |
| `f32` | 4 | IEEE 754 single |
| `f64` | 8 | IEEE 754 double |
| `bool` | 1 | 0x00 = false, 0x01 = true |
| `bytes` | 4 + n | Length-prefixed (u32 LE + data) |
| `string` | 4 + n | Length-prefixed UTF-8 |

### Example: Length-Prefixed String

```
"hello" encoded:
  05 00 00 00 68 65 6C 6C 6F
  |________|  |____________|
  len=5       "hello"
```

## Message Size Limits

| Limit | Value | Notes |
|-------|-------|-------|
| Maximum message size | 16 MB | Configurable |
| Maximum header size | 5 bytes | Fixed |
| Minimum message size | 5 bytes | Header only |

Messages exceeding the size limit result in an error:

```json
{
  "code": -32600,
  "message": "Message too large: 17825792 bytes exceeds limit of 16777216"
}
```

## Handshake Protocol

### Init Message

```json
{
  "name": "hanzo-agent",
  "version": "1.0.0"
}
```

### InitAck Message

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

## Error Message Format

```json
{
  "code": -32601,
  "message": "Tool not found: unknown_tool"
}
```

### Standard Error Codes

| Code | Name | Description |
|------|------|-------------|
| -32700 | Parse error | Invalid JSON |
| -32600 | Invalid request | Malformed request |
| -32601 | Method not found | Tool/resource not found |
| -32602 | Invalid params | Invalid arguments |
| -32603 | Internal error | Server error |
| -32000 | Server error | Generic server error |
| -32001 | Timeout | Request timed out |
| -32002 | Permission denied | Not authorized |

## Implementation Guide

### Reading a Message (Rust)

```rust
use std::io::Read;

fn read_message<R: Read>(reader: &mut R) -> Result<(u8, Vec<u8>)> {
    // Read length header
    let mut header = [0u8; 4];
    reader.read_exact(&mut header)?;
    let length = u32::from_le_bytes(header) as usize;

    // Validate size
    if length > MAX_MESSAGE_SIZE {
        return Err(Error::MessageTooLarge);
    }

    // Read message body
    let mut body = vec![0u8; length];
    reader.read_exact(&mut body)?;

    // Extract message type and payload
    let msg_type = body[0];
    let payload = body[1..].to_vec();

    Ok((msg_type, payload))
}
```

### Writing a Message (Rust)

```rust
use std::io::Write;

fn write_message<W: Write>(writer: &mut W, msg_type: u8, payload: &[u8]) -> Result<()> {
    // Calculate total length
    let total_len = 1 + payload.len();

    // Write length header
    writer.write_all(&(total_len as u32).to_le_bytes())?;

    // Write message type
    writer.write_all(&[msg_type])?;

    // Write payload
    writer.write_all(payload)?;

    Ok(())
}
```

### Reading a Message (Python)

```python
import struct

def read_message(sock):
    # Read length header
    header = sock.recv(4)
    if len(header) < 4:
        raise ConnectionError("Connection closed")

    length = struct.unpack('<I', header)[0]

    # Validate size
    if length > MAX_MESSAGE_SIZE:
        raise ValueError(f"Message too large: {length}")

    # Read message body
    body = b''
    while len(body) < length:
        chunk = sock.recv(length - len(body))
        if not chunk:
            raise ConnectionError("Connection closed")
        body += chunk

    # Extract message type and payload
    msg_type = body[0]
    payload = body[1:]

    return msg_type, payload
```

### Reading a Message (Go)

```go
func readMessage(conn net.Conn) (byte, []byte, error) {
    // Read length header
    header := make([]byte, 4)
    if _, err := io.ReadFull(conn, header); err != nil {
        return 0, nil, err
    }

    length := binary.LittleEndian.Uint32(header)

    // Validate size
    if length > MaxMessageSize {
        return 0, nil, fmt.Errorf("message too large: %d", length)
    }

    // Read message body
    body := make([]byte, length)
    if _, err := io.ReadFull(conn, body); err != nil {
        return 0, nil, err
    }

    // Extract message type and payload
    msgType := body[0]
    payload := body[1:]

    return msgType, payload, nil
}
```

## Compression (Optional)

ZAP supports optional payload compression for large messages:

| Flag | Algorithm | Notes |
|------|-----------|-------|
| `0x80` | None | Default, no compression |
| `0x81` | LZ4 | Fast compression |
| `0x82` | Zstd | Better ratio |

The compression flag is OR'd with the message type:

```
MsgType = 0x13 (CallToolResponse) | 0x81 (LZ4) = 0x94
```

Decompression:
```rust
let compression = msg_type & 0xF0;
let actual_type = msg_type & 0x0F;

let payload = match compression {
    0x80 => raw_payload,           // No compression
    0x81 => lz4_decompress(raw_payload)?,
    0x82 => zstd_decompress(raw_payload)?,
    _ => return Err(Error::UnknownCompression),
};
```

## Next Steps

- [RPC Protocol](rpc.md): Request/response patterns and streaming
- [Security](security.md): Permission checking and sandboxing
