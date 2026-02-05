//! Wire protocol for ZAP messages.
//!
//! ZAP uses a simple length-prefixed binary format:
//!
//! ```text
//! +----------+----------+------------------+
//! | Length   | MsgType  | Payload          |
//! | (4 bytes)| (1 byte) | (variable)       |
//! | LE u32   |          |                  |
//! +----------+----------+------------------+
//! ```

use crate::buffer::Buffer;
use crate::error::{Error, Result};
use crate::message::MessageType;

/// Maximum message size (16MB).
pub const MAX_MESSAGE_SIZE: usize = 16 * 1024 * 1024;

/// Zero-copy reader for ZAP wire format.
#[derive(Debug)]
pub struct Reader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    /// Create a new reader from bytes.
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    /// Get remaining bytes.
    pub fn remaining(&self) -> usize {
        self.data.len() - self.pos
    }

    /// Check if at end of data.
    pub fn is_empty(&self) -> bool {
        self.pos >= self.data.len()
    }

    /// Read a u8.
    pub fn read_u8(&mut self) -> Result<u8> {
        if self.remaining() < 1 {
            return Err(Error::Protocol("unexpected end of data".to_string()));
        }
        let value = self.data[self.pos];
        self.pos += 1;
        Ok(value)
    }

    /// Read a little-endian u32.
    pub fn read_u32_le(&mut self) -> Result<u32> {
        if self.remaining() < 4 {
            return Err(Error::Protocol("unexpected end of data".to_string()));
        }
        let bytes: [u8; 4] = self.data[self.pos..self.pos + 4]
            .try_into()
            .map_err(|_| Error::Protocol("failed to read u32".to_string()))?;
        self.pos += 4;
        Ok(u32::from_le_bytes(bytes))
    }

    /// Read a little-endian u64.
    pub fn read_u64_le(&mut self) -> Result<u64> {
        if self.remaining() < 8 {
            return Err(Error::Protocol("unexpected end of data".to_string()));
        }
        let bytes: [u8; 8] = self.data[self.pos..self.pos + 8]
            .try_into()
            .map_err(|_| Error::Protocol("failed to read u64".to_string()))?;
        self.pos += 8;
        Ok(u64::from_le_bytes(bytes))
    }

    /// Read a little-endian i64.
    pub fn read_i64_le(&mut self) -> Result<i64> {
        if self.remaining() < 8 {
            return Err(Error::Protocol("unexpected end of data".to_string()));
        }
        let bytes: [u8; 8] = self.data[self.pos..self.pos + 8]
            .try_into()
            .map_err(|_| Error::Protocol("failed to read i64".to_string()))?;
        self.pos += 8;
        Ok(i64::from_le_bytes(bytes))
    }

    /// Read bytes with length prefix (zero-copy).
    pub fn read_bytes(&mut self) -> Result<&'a [u8]> {
        let len = self.read_u32_le()? as usize;
        if self.remaining() < len {
            return Err(Error::Protocol(format!(
                "unexpected end of data: need {} bytes, have {}",
                len,
                self.remaining()
            )));
        }
        let bytes = &self.data[self.pos..self.pos + len];
        self.pos += len;
        Ok(bytes)
    }

    /// Read a string with length prefix (zero-copy).
    pub fn read_string(&mut self) -> Result<&'a str> {
        let bytes = self.read_bytes()?;
        std::str::from_utf8(bytes).map_err(|e| Error::Protocol(format!("invalid UTF-8: {}", e)))
    }

    /// Read fixed-size bytes (zero-copy).
    pub fn read_fixed<const N: usize>(&mut self) -> Result<&'a [u8; N]> {
        if self.remaining() < N {
            return Err(Error::Protocol(format!(
                "unexpected end of data: need {} bytes, have {}",
                N,
                self.remaining()
            )));
        }
        let bytes: &[u8; N] = self.data[self.pos..self.pos + N]
            .try_into()
            .map_err(|_| Error::Protocol("failed to read fixed bytes".to_string()))?;
        self.pos += N;
        Ok(bytes)
    }
}

/// Writer for ZAP wire format.
#[derive(Debug)]
pub struct Writer {
    buffer: Buffer,
}

impl Writer {
    /// Create a new writer.
    pub fn new() -> Self {
        Self {
            buffer: Buffer::new(),
        }
    }

    /// Create a new writer with a buffer.
    pub fn with_buffer(buffer: Buffer) -> Self {
        Self { buffer }
    }

    /// Get the written bytes.
    pub fn bytes(&self) -> &[u8] {
        self.buffer.bytes()
    }

    /// Take the buffer.
    pub fn into_buffer(self) -> Buffer {
        self.buffer
    }

    /// Clear the writer for reuse.
    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    /// Write a message header.
    pub fn write_header(&mut self, msg_type: MessageType, payload_len: u32) {
        // Total length = 1 (msg type) + payload
        let total_len = 1 + payload_len;
        self.buffer.write_u32_le(total_len);
        self.buffer.write_u8(msg_type as u8);
    }

    /// Write a complete message with framing.
    pub fn write_message(&mut self, msg_type: MessageType, payload: &[u8]) {
        self.write_header(msg_type, payload.len() as u32);
        self.buffer.extend_from_slice(payload);
    }

    /// Write a u8.
    pub fn write_u8(&mut self, value: u8) {
        self.buffer.write_u8(value);
    }

    /// Write a little-endian u32.
    pub fn write_u32_le(&mut self, value: u32) {
        self.buffer.write_u32_le(value);
    }

    /// Write a little-endian u64.
    pub fn write_u64_le(&mut self, value: u64) {
        self.buffer.write_u64_le(value);
    }

    /// Write a little-endian i64.
    pub fn write_i64_le(&mut self, value: i64) {
        self.buffer.write_i64_le(value);
    }

    /// Write bytes with length prefix.
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.buffer.write_bytes(bytes);
    }

    /// Write a string with length prefix.
    pub fn write_string(&mut self, s: &str) {
        self.buffer.write_string(s);
    }
}

impl Default for Writer {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse a framed message from bytes.
///
/// Returns the message type and payload (zero-copy into input).
pub fn parse_frame(data: &[u8]) -> Result<(MessageType, &[u8])> {
    if data.len() < 5 {
        return Err(Error::Protocol("message too short".to_string()));
    }

    let mut reader = Reader::new(data);
    let length = reader.read_u32_le()? as usize;

    if length > MAX_MESSAGE_SIZE {
        return Err(Error::MessageTooLarge {
            size: length,
            max: MAX_MESSAGE_SIZE,
        });
    }

    if data.len() < 4 + length {
        return Err(Error::Protocol(format!(
            "incomplete message: expected {} bytes, have {}",
            4 + length,
            data.len()
        )));
    }

    let msg_type_byte = reader.read_u8()?;
    let msg_type = MessageType::try_from(msg_type_byte)?;
    let payload = &data[5..4 + length];

    Ok((msg_type, payload))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip() {
        let mut writer = Writer::new();
        writer.write_message(MessageType::ListTools, b"test payload");

        let (msg_type, payload) = parse_frame(writer.bytes()).unwrap();
        assert_eq!(msg_type, MessageType::ListTools);
        assert_eq!(payload, b"test payload");
    }

    #[test]
    fn test_zero_copy_read() {
        let data = b"\x05\x00\x00\x00hello";
        let mut reader = Reader::new(data);
        let bytes = reader.read_bytes().unwrap();

        // Verify zero-copy: pointer should be into original data
        assert!(std::ptr::eq(bytes.as_ptr(), data[4..].as_ptr()));
    }
}
