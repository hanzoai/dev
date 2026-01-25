//! Buffer pooling for zero-allocation message handling.
//!
//! ZAP uses buffer pools to eliminate memory allocations on hot paths.
//! Buffers are reused across messages, reducing GC pressure.

use std::sync::Arc;
use tokio::sync::Mutex;

/// Default buffer capacity (64KB).
const DEFAULT_CAPACITY: usize = 64 * 1024;

/// Maximum buffer capacity (16MB).
const MAX_CAPACITY: usize = 16 * 1024 * 1024;

/// A reusable buffer for message encoding/decoding.
#[derive(Debug)]
pub struct Buffer {
    data: Vec<u8>,
}

impl Buffer {
    /// Create a new buffer with default capacity.
    pub fn new() -> Self {
        Self {
            data: Vec::with_capacity(DEFAULT_CAPACITY),
        }
    }

    /// Create a new buffer with specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity.min(MAX_CAPACITY)),
        }
    }

    /// Get the buffer data as a slice.
    pub fn as_slice(&self) -> &[u8] {
        &self.data
    }

    /// Get the buffer data as a mutable slice.
    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        &mut self.data
    }

    /// Get the buffer data as bytes.
    pub fn bytes(&self) -> &[u8] {
        &self.data
    }

    /// Get the buffer length.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Clear the buffer for reuse.
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Reset the buffer (clear and optionally shrink).
    pub fn reset(&mut self) {
        self.data.clear();
        // Shrink if grown too large
        if self.data.capacity() > MAX_CAPACITY {
            self.data.shrink_to(DEFAULT_CAPACITY);
        }
    }

    /// Write a byte to the buffer.
    pub fn write_u8(&mut self, value: u8) {
        self.data.push(value);
    }

    /// Write a little-endian u32 to the buffer.
    pub fn write_u32_le(&mut self, value: u32) {
        self.data.extend_from_slice(&value.to_le_bytes());
    }

    /// Write a little-endian u64 to the buffer.
    pub fn write_u64_le(&mut self, value: u64) {
        self.data.extend_from_slice(&value.to_le_bytes());
    }

    /// Write a little-endian i64 to the buffer.
    pub fn write_i64_le(&mut self, value: i64) {
        self.data.extend_from_slice(&value.to_le_bytes());
    }

    /// Write bytes with length prefix.
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.write_u32_le(bytes.len() as u32);
        self.data.extend_from_slice(bytes);
    }

    /// Write a string with length prefix.
    pub fn write_string(&mut self, s: &str) {
        self.write_bytes(s.as_bytes());
    }

    /// Extend from slice.
    pub fn extend_from_slice(&mut self, bytes: &[u8]) {
        self.data.extend_from_slice(bytes);
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Vec<u8>> for Buffer {
    fn from(data: Vec<u8>) -> Self {
        Self { data }
    }
}

impl From<Buffer> for Vec<u8> {
    fn from(buf: Buffer) -> Vec<u8> {
        buf.data
    }
}

/// A pool of reusable buffers.
#[derive(Debug)]
pub struct BufferPool {
    buffers: Arc<Mutex<Vec<Buffer>>>,
    capacity: usize,
}

impl BufferPool {
    /// Create a new buffer pool.
    pub fn new(capacity: usize) -> Self {
        Self {
            buffers: Arc::new(Mutex::new(Vec::with_capacity(capacity))),
            capacity,
        }
    }

    /// Get a buffer from the pool.
    pub async fn get(&self) -> Buffer {
        let mut buffers = self.buffers.lock().await;
        buffers.pop().unwrap_or_else(Buffer::new)
    }

    /// Return a buffer to the pool.
    pub async fn put(&self, mut buffer: Buffer) {
        buffer.reset();
        let mut buffers = self.buffers.lock().await;
        if buffers.len() < self.capacity {
            buffers.push(buffer);
        }
        // Otherwise, drop the buffer
    }
}

impl Default for BufferPool {
    fn default() -> Self {
        Self::new(32) // Default pool size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffer_write() {
        let mut buf = Buffer::new();
        buf.write_u8(0x42);
        buf.write_u32_le(0x12345678);
        buf.write_string("hello");

        assert_eq!(buf.len(), 1 + 4 + 4 + 5); // u8 + u32 + len + "hello"
    }

    #[tokio::test]
    async fn test_buffer_pool() {
        let pool = BufferPool::new(2);

        let buf1 = pool.get().await;
        let buf2 = pool.get().await;

        pool.put(buf1).await;
        pool.put(buf2).await;

        // Should reuse pooled buffers
        let _buf3 = pool.get().await;
        let _buf4 = pool.get().await;
    }
}
