//! luxfi/zap wire protocol implementation (Rust).
//!
//! Compatible with the Go `github.com/luxfi/zap` v0.2.0 binary format.
//! Implements builder, parser, and frame I/O for the Zero-copy Application
//! Protocol used by Hanzo cloud services.
//!
//! Wire format:
//!   Frame: [4-byte LE length][message bytes]
//!   Message header (16 bytes): magic(4) + version(2) + flags(2) + root_offset(4) + size(4)
//!   Object fields: inline primitives, (relOffset:u32 + length:u32) for text/bytes

use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

// ── Constants ───────────────────────────────────────────────────────────

pub const ZAP_MAGIC: [u8; 4] = *b"ZAP\x00";
pub const HEADER_SIZE: usize = 16;
pub const VERSION: u16 = 1;
pub const ALIGNMENT: usize = 8;
pub const MAX_MESSAGE_SIZE: usize = 10 * 1024 * 1024; // 10 MB

/// Cloud service (native binary RPC).
pub const MSG_TYPE_CLOUD: u16 = 100;

// ── Cloud request field byte offsets ────────────────────────────────────
// Layout: method(0:Text) + auth(8:Text) + body(16:Bytes)
// Each Text/Bytes field is 8 bytes (4 relOffset + 4 length).

pub const CLOUD_REQ_METHOD: usize = 0;
pub const CLOUD_REQ_AUTH: usize = 8;
pub const CLOUD_REQ_BODY: usize = 16;
pub const CLOUD_REQ_FIXED_SIZE: usize = 24;

// ── Cloud response field byte offsets ───────────────────────────────────
// Layout: status(0:Uint32) + body(4:Bytes) + error(12:Text)

pub const CLOUD_RESP_STATUS: usize = 0;
pub const CLOUD_RESP_BODY: usize = 4;
pub const CLOUD_RESP_ERROR: usize = 12;

// ── Call correlation ────────────────────────────────────────────────────

pub const REQ_FLAG_REQ: u32 = 1;
pub const REQ_FLAG_RESP: u32 = 2;

// ── Handshake ───────────────────────────────────────────────────────────

pub const HANDSHAKE_OBJ_SIZE: usize = 64;
pub const HANDSHAKE_ID_MAX: usize = 60;
pub const HANDSHAKE_ID_LEN_OFFSET: usize = 60;

// ── Message (parsed, owns bytes) ────────────────────────────────────────

/// A parsed ZAP message that owns its byte buffer.
pub struct Message {
    data: Vec<u8>,
}

impl Message {
    /// Parse a ZAP message from raw bytes. Validates magic and version.
    pub fn parse(data: Vec<u8>) -> Result<Self, &'static str> {
        if data.len() < HEADER_SIZE {
            return Err("buffer too small for ZAP header");
        }
        if data[0..4] != ZAP_MAGIC {
            return Err("invalid ZAP magic bytes");
        }
        let version = u16::from_le_bytes([data[4], data[5]]);
        if version != VERSION {
            return Err("unsupported ZAP version");
        }
        Ok(Self { data })
    }

    /// Raw message bytes.
    pub fn bytes(&self) -> &[u8] {
        &self.data
    }

    /// Message flags (bits 8-15 = message type, bits 0-7 = compression etc).
    pub fn flags(&self) -> u16 {
        u16::from_le_bytes([self.data[6], self.data[7]])
    }

    /// Message type extracted from flags (upper 8 bits).
    pub fn msg_type(&self) -> u16 {
        self.flags() >> 8
    }

    /// Root object of this message.
    pub fn root(&self) -> Object<'_> {
        let offset = u32::from_le_bytes([
            self.data[8],
            self.data[9],
            self.data[10],
            self.data[11],
        ]) as usize;
        Object {
            data: &self.data,
            offset,
        }
    }
}

// ── Object (zero-copy reader) ───────────────────────────────────────────

/// Zero-copy view into a ZAP object's fields.
pub struct Object<'a> {
    data: &'a [u8],
    offset: usize,
}

impl<'a> Object<'a> {
    /// Read a uint8 at the given byte offset within this object.
    pub fn uint8(&self, field_offset: usize) -> u8 {
        let pos = self.offset + field_offset;
        if pos >= self.data.len() {
            return 0;
        }
        self.data[pos]
    }

    /// Read a uint32 at the given byte offset within this object.
    pub fn uint32(&self, field_offset: usize) -> u32 {
        let pos = self.offset + field_offset;
        if pos + 4 > self.data.len() {
            return 0;
        }
        u32::from_le_bytes([
            self.data[pos],
            self.data[pos + 1],
            self.data[pos + 2],
            self.data[pos + 3],
        ])
    }

    /// Read a bytes/text field at the given byte offset.
    /// Field layout: relOffset(4) + length(4) = 8 bytes total.
    /// Data lives at (field_pos + relOffset) for `length` bytes.
    pub fn bytes_field(&self, field_offset: usize) -> &'a [u8] {
        let pos = self.offset + field_offset;
        if pos + 4 > self.data.len() {
            return &[];
        }
        let rel_offset = i32::from_le_bytes([
            self.data[pos],
            self.data[pos + 1],
            self.data[pos + 2],
            self.data[pos + 3],
        ]);
        if rel_offset == 0 {
            return &[];
        }
        let len_pos = pos + 4;
        if len_pos + 4 > self.data.len() {
            return &[];
        }
        let length = u32::from_le_bytes([
            self.data[len_pos],
            self.data[len_pos + 1],
            self.data[len_pos + 2],
            self.data[len_pos + 3],
        ]) as usize;
        let abs_pos = (pos as i64 + rel_offset as i64) as usize;
        if abs_pos + length > self.data.len() {
            return &[];
        }
        &self.data[abs_pos..abs_pos + length]
    }

    /// Read a text field at the given byte offset (UTF-8).
    pub fn text(&self, field_offset: usize) -> &'a str {
        let b = self.bytes_field(field_offset);
        std::str::from_utf8(b).unwrap_or("")
    }
}

// ── Builder ─────────────────────────────────────────────────────────────

/// Builds ZAP messages compatible with the Go luxfi/zap library.
pub struct Builder {
    buf: Vec<u8>,
    pos: usize,
    root_offset: usize,
}

impl Builder {
    /// Create a new builder with the given capacity hint.
    pub fn new(capacity: usize) -> Self {
        let cap = capacity.max(256);
        let mut buf = vec![0u8; cap];
        buf[0..4].copy_from_slice(&ZAP_MAGIC);
        buf[4..6].copy_from_slice(&VERSION.to_le_bytes());
        Self {
            buf,
            pos: HEADER_SIZE,
            root_offset: 0,
        }
    }

    fn grow(&mut self, n: usize) {
        let needed = self.pos + n;
        if needed <= self.buf.len() {
            return;
        }
        let new_cap = (self.buf.len() * 2).max(needed);
        self.buf.resize(new_cap, 0);
    }

    fn align(&mut self, alignment: usize) {
        let padding = (alignment - (self.pos % alignment)) % alignment;
        self.grow(padding);
        for _ in 0..padding {
            self.buf[self.pos] = 0;
            self.pos += 1;
        }
    }

    /// Start building an object with the given fixed-field data size.
    pub fn start_object(&mut self, data_size: usize) -> ObjectBuilder<'_> {
        self.align(ALIGNMENT);
        ObjectBuilder {
            start_pos: self.pos,
            data_size,
            deferred: Vec::new(),
            builder: self,
        }
    }

    /// Finalize the message, writing root offset and size into the header.
    /// Returns the complete message bytes.
    pub fn finish(mut self) -> Vec<u8> {
        self.buf[8..12].copy_from_slice(&(self.root_offset as u32).to_le_bytes());
        self.buf[12..16].copy_from_slice(&(self.pos as u32).to_le_bytes());
        self.buf.truncate(self.pos);
        self.buf
    }

    /// Finalize with specific flags (e.g., `MSG_TYPE_CLOUD << 8`).
    pub fn finish_with_flags(mut self, flags: u16) -> Vec<u8> {
        self.buf[6..8].copy_from_slice(&flags.to_le_bytes());
        self.finish()
    }
}

struct DeferredWrite {
    field_offset: usize,
    data: Vec<u8>,
}

/// Builder for a single ZAP object (struct).
pub struct ObjectBuilder<'a> {
    builder: &'a mut Builder,
    start_pos: usize,
    data_size: usize,
    deferred: Vec<DeferredWrite>,
}

impl<'a> ObjectBuilder<'a> {
    fn ensure_field(&mut self, end_offset: usize) {
        let needed = self.start_pos + end_offset;
        if needed > self.builder.pos {
            self.builder.grow(needed - self.builder.pos);
            for i in self.builder.pos..needed {
                self.builder.buf[i] = 0;
            }
            self.builder.pos = needed;
        }
    }

    /// Set a uint8 field at the given byte offset.
    pub fn set_uint8(&mut self, field_offset: usize, v: u8) {
        self.ensure_field(field_offset + 1);
        self.builder.buf[self.start_pos + field_offset] = v;
    }

    /// Set a uint32 field at the given byte offset.
    pub fn set_uint32(&mut self, field_offset: usize, v: u32) {
        self.ensure_field(field_offset + 4);
        let pos = self.start_pos + field_offset;
        self.builder.buf[pos..pos + 4].copy_from_slice(&v.to_le_bytes());
    }

    /// Set a bytes field at the given byte offset.
    /// Writes length immediately; defers data write to `finish()`.
    pub fn set_bytes(&mut self, field_offset: usize, data: &[u8]) {
        self.ensure_field(field_offset + 8);
        let pos = self.start_pos + field_offset;
        if data.is_empty() {
            self.builder.buf[pos..pos + 4].copy_from_slice(&0u32.to_le_bytes());
            self.builder.buf[pos + 4..pos + 8].copy_from_slice(&0u32.to_le_bytes());
            return;
        }
        // Write length now
        self.builder.buf[pos + 4..pos + 8]
            .copy_from_slice(&(data.len() as u32).to_le_bytes());
        // Defer the actual data write + offset patch
        self.deferred.push(DeferredWrite {
            field_offset,
            data: data.to_vec(),
        });
    }

    /// Set a text (string) field at the given byte offset.
    pub fn set_text(&mut self, field_offset: usize, text: &str) {
        self.set_bytes(field_offset, text.as_bytes());
    }

    /// Finalize this object: write deferred data, patch relative offsets.
    fn do_finish(&mut self) {
        // Ensure minimum fixed size
        self.ensure_field(self.data_size);
        // Write deferred text/bytes data after the fixed section
        for entry in self.deferred.drain(..) {
            let data_pos = self.builder.pos;
            self.builder.grow(entry.data.len());
            let start = self.builder.pos;
            self.builder.buf[start..start + entry.data.len()].copy_from_slice(&entry.data);
            self.builder.pos += entry.data.len();
            // Patch relative offset: relOffset = dataPos - fieldAbsPos
            let field_abs_pos = self.start_pos + entry.field_offset;
            let rel_offset = data_pos as i32 - field_abs_pos as i32;
            self.builder.buf[field_abs_pos..field_abs_pos + 4]
                .copy_from_slice(&(rel_offset as u32).to_le_bytes());
        }
    }

    /// Finalize and set as the message root object.
    pub fn finish_as_root(mut self) {
        self.do_finish();
        self.builder.root_offset = self.start_pos;
    }
}

// ── Frame I/O (async) ───────────────────────────────────────────────────

/// Write a length-prefixed frame: [4-byte LE length][data].
pub async fn write_frame<W: AsyncWrite + Unpin>(w: &mut W, data: &[u8]) -> std::io::Result<()> {
    let len_buf = (data.len() as u32).to_le_bytes();
    w.write_all(&len_buf).await?;
    w.write_all(data).await?;
    w.flush().await?;
    Ok(())
}

/// Read a length-prefixed frame. Returns the raw message bytes (no length header).
pub async fn read_frame<R: AsyncRead + Unpin>(r: &mut R) -> std::io::Result<Vec<u8>> {
    let mut len_buf = [0u8; 4];
    r.read_exact(&mut len_buf).await?;
    let length = u32::from_le_bytes(len_buf) as usize;
    if length > MAX_MESSAGE_SIZE {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("ZAP message too large: {} bytes", length),
        ));
    }
    let mut data = vec![0u8; length];
    r.read_exact(&mut data).await?;
    Ok(data)
}

// ── Handshake helpers ───────────────────────────────────────────────────

/// Build a ZAP handshake message containing our node ID.
/// Format: 64-byte object with ID bytes at offsets 0-59, length at offset 60.
pub fn build_handshake(node_id: &str) -> Vec<u8> {
    let mut b = Builder::new(128);
    let mut obj = b.start_object(HANDSHAKE_OBJ_SIZE);
    let id_bytes = node_id.as_bytes();
    for (i, &byte) in id_bytes.iter().enumerate() {
        if i >= HANDSHAKE_ID_MAX {
            break;
        }
        obj.set_uint8(i, byte);
    }
    obj.set_uint32(
        HANDSHAKE_ID_LEN_OFFSET,
        id_bytes.len().min(HANDSHAKE_ID_MAX) as u32,
    );
    obj.finish_as_root();
    b.finish()
}

/// Parse a peer's node ID from a handshake message.
pub fn parse_handshake(msg: &Message) -> String {
    let root = msg.root();
    let id_len = root.uint32(HANDSHAKE_ID_LEN_OFFSET) as usize;
    let id_len = id_len.min(HANDSHAKE_ID_MAX);
    let mut id = Vec::with_capacity(id_len);
    for i in 0..id_len {
        id.push(root.uint8(i));
    }
    String::from_utf8_lossy(&id).into_owned()
}

// ── Cloud message builders ──────────────────────────────────────────────

/// Build a MsgType 100 cloud service request.
/// Layout: method(0:Text) + auth(8:Text) + body(16:Bytes)
pub fn build_cloud_request(method: &str, auth: &str, body: &[u8]) -> Vec<u8> {
    let mut b = Builder::new(body.len() + method.len() + auth.len() + 128);
    let mut obj = b.start_object(CLOUD_REQ_FIXED_SIZE);
    obj.set_text(CLOUD_REQ_METHOD, method);
    obj.set_text(CLOUD_REQ_AUTH, auth);
    obj.set_bytes(CLOUD_REQ_BODY, body);
    obj.finish_as_root();
    b.finish_with_flags(MSG_TYPE_CLOUD << 8)
}

/// Parse a MsgType 100 cloud service response.
/// Returns (status, body_bytes, error_string).
pub fn parse_cloud_response(msg: &Message) -> (u32, Vec<u8>, String) {
    let root = msg.root();
    let status = root.uint32(CLOUD_RESP_STATUS);
    let body = root.bytes_field(CLOUD_RESP_BODY).to_vec();
    let error = root.text(CLOUD_RESP_ERROR).to_string();
    (status, body, error)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_and_parse_handshake() {
        let msg_bytes = build_handshake("dev-client");
        let msg = Message::parse(msg_bytes).expect("parse handshake");
        let peer_id = parse_handshake(&msg);
        assert_eq!(peer_id, "dev-client");
    }

    #[test]
    fn test_build_and_parse_cloud_request() {
        let body = br#"{"model":"claude-sonnet-4-20250514","messages":[{"role":"user","content":"hello"}]}"#;
        let msg_bytes = build_cloud_request("chat.completions", "Bearer sk-test", body);
        let msg = Message::parse(msg_bytes).expect("parse cloud request");

        assert_eq!(msg.msg_type(), MSG_TYPE_CLOUD);

        let root = msg.root();
        assert_eq!(root.text(CLOUD_REQ_METHOD), "chat.completions");
        assert_eq!(root.text(CLOUD_REQ_AUTH), "Bearer sk-test");
        assert_eq!(root.bytes_field(CLOUD_REQ_BODY), body.as_slice());
    }

    #[test]
    fn test_build_and_parse_cloud_response() {
        // Simulate a response built by the Go server
        let resp_body = br#"{"id":"chatcmpl-123","choices":[{"message":{"content":"hi"}}]}"#;
        let mut b = Builder::new(256);
        let mut obj = b.start_object(20);
        obj.set_uint32(CLOUD_RESP_STATUS, 200);
        obj.set_bytes(CLOUD_RESP_BODY, resp_body);
        // error field left empty (null)
        obj.finish_as_root();
        let data = b.finish_with_flags(MSG_TYPE_CLOUD << 8);

        let msg = Message::parse(data).expect("parse response");
        let (status, body, error) = parse_cloud_response(&msg);
        assert_eq!(status, 200);
        assert_eq!(body, resp_body);
        assert!(error.is_empty());
    }

    #[test]
    fn test_cloud_response_with_error() {
        let mut b = Builder::new(128);
        let mut obj = b.start_object(20);
        obj.set_uint32(CLOUD_RESP_STATUS, 401);
        // body left empty
        obj.set_text(CLOUD_RESP_ERROR, "auth token required");
        obj.finish_as_root();
        let data = b.finish_with_flags(MSG_TYPE_CLOUD << 8);

        let msg = Message::parse(data).expect("parse error response");
        let (status, body, error) = parse_cloud_response(&msg);
        assert_eq!(status, 401);
        assert!(body.is_empty());
        assert_eq!(error, "auth token required");
    }
}
