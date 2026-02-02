//! ZAP Wire Protocol Implementation
//!
//! ZAP (Zero-copy Application Protocol) is a binary wire protocol used by
//! Lux nodes for P2P communication. This is the Rust implementation compatible
//! with the Go reference at ~/work/lux/node/message/wire/zap.go
//!
//! Key features:
//! - Tag-based message encoding (uint8 tags identify message types)
//! - Zero-copy buffer operations where possible
//! - Big-endian byte order for multi-byte integers
//! - No protobuf dependency
//!
//! Lux Engine Types:
//! - Chain: Linear blockchain consensus (sequential blocks)
//! - DAG: Directed acyclic graph, UTXO-based
//! - PQ: Post-quantum, Quasar protocol with BLS + ring signatures

use bytes::{Buf, BufMut, Bytes, BytesMut};
use std::io;

/// ZAP message tags (from Lux wire protocol)
pub mod tags {
    pub const PING: u8 = 2;
    pub const PONG: u8 = 3;
    pub const HANDSHAKE: u8 = 8;
    pub const PUT: u8 = 18;
    pub const GET: u8 = 19;
    pub const PUSH_QUERY: u8 = 20;
    pub const PULL_QUERY: u8 = 21;
    pub const CHITS: u8 = 22;
    pub const GOSSIP: u8 = 24;
    pub const BFT: u8 = 25;
}

/// Lux consensus engine types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum EngineType {
    /// Unspecified engine
    Unspecified = 0,
    /// Chain engine - linear blockchain consensus
    Chain = 1,
    /// DAG engine - directed acyclic graph, UTXO-based
    DAG = 2,
    /// PQ engine - post-quantum resistant (Quasar protocol)
    PQ = 3,
}

impl From<u32> for EngineType {
    fn from(v: u32) -> Self {
        match v {
            1 => EngineType::Chain,
            2 => EngineType::DAG,
            3 => EngineType::PQ,
            _ => EngineType::Unspecified,
        }
    }
}

/// Put message for state replication
#[derive(Debug, Clone)]
pub struct Put {
    pub chain_id: Bytes,
    pub request_id: u32,
    pub container: Bytes,
    pub engine_type: EngineType,
}

/// Gossip message for state propagation
#[derive(Debug, Clone)]
pub struct Gossip {
    pub chain_id: Bytes,
    pub container: Bytes,
}

/// Get message for state retrieval
#[derive(Debug, Clone)]
pub struct Get {
    pub chain_id: Bytes,
    pub request_id: u32,
    pub container_id: Bytes,
    pub engine_type: EngineType,
}

/// Ping message for keepalive
#[derive(Debug, Clone)]
pub struct Ping {
    pub uptime: u32,
    pub subnet_uptimes: Vec<SubnetUptime>,
}

/// Pong response
#[derive(Debug, Clone)]
pub struct Pong {
    pub uptime: u32,
    pub subnet_uptimes: Vec<SubnetUptime>,
}

/// Subnet uptime info
#[derive(Debug, Clone)]
pub struct SubnetUptime {
    pub subnet_id: Bytes,
    pub uptime: u32,
}

/// ZAP message envelope
#[derive(Debug, Clone)]
pub enum Message {
    Ping(Ping),
    Pong(Pong),
    Put(Put),
    Get(Get),
    Gossip(Gossip),
}

/// ZAP buffer for zero-copy encoding
pub struct Buffer {
    inner: BytesMut,
}

impl Buffer {
    /// Create a new buffer with the specified capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: BytesMut::with_capacity(capacity),
        }
    }

    /// Write a single byte
    pub fn write_u8(&mut self, v: u8) {
        self.inner.put_u8(v);
    }

    /// Write a big-endian u16
    pub fn write_u16(&mut self, v: u16) {
        self.inner.put_u16(v);
    }

    /// Write a big-endian u32
    pub fn write_u32(&mut self, v: u32) {
        self.inner.put_u32(v);
    }

    /// Write a big-endian u64
    pub fn write_u64(&mut self, v: u64) {
        self.inner.put_u64(v);
    }

    /// Write a length-prefixed byte slice (u32 length prefix)
    pub fn write_bytes(&mut self, v: &[u8]) {
        self.write_u32(v.len() as u32);
        self.inner.put_slice(v);
    }

    /// Write raw bytes without length prefix
    pub fn write_raw(&mut self, v: &[u8]) {
        self.inner.put_slice(v);
    }

    /// Get the encoded bytes
    pub fn into_bytes(self) -> Bytes {
        self.inner.freeze()
    }

    /// Get current length
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

/// ZAP reader for zero-copy decoding
pub struct Reader<'a> {
    inner: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    /// Create a new reader
    pub fn new(data: &'a [u8]) -> Self {
        Self { inner: data, pos: 0 }
    }

    /// Read a single byte
    pub fn read_u8(&mut self) -> io::Result<u8> {
        if self.pos >= self.inner.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "buffer underflow"));
        }
        let v = self.inner[self.pos];
        self.pos += 1;
        Ok(v)
    }

    /// Read a big-endian u16
    pub fn read_u16(&mut self) -> io::Result<u16> {
        if self.pos + 2 > self.inner.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "buffer underflow"));
        }
        let v = (&self.inner[self.pos..]).get_u16();
        self.pos += 2;
        Ok(v)
    }

    /// Read a big-endian u32
    pub fn read_u32(&mut self) -> io::Result<u32> {
        if self.pos + 4 > self.inner.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "buffer underflow"));
        }
        let v = (&self.inner[self.pos..]).get_u32();
        self.pos += 4;
        Ok(v)
    }

    /// Read a big-endian u64
    pub fn read_u64(&mut self) -> io::Result<u64> {
        if self.pos + 8 > self.inner.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "buffer underflow"));
        }
        let v = (&self.inner[self.pos..]).get_u64();
        self.pos += 8;
        Ok(v)
    }

    /// Read a length-prefixed byte slice
    pub fn read_bytes(&mut self) -> io::Result<Bytes> {
        let len = self.read_u32()? as usize;
        if self.pos + len > self.inner.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "buffer underflow"));
        }
        let v = Bytes::copy_from_slice(&self.inner[self.pos..self.pos + len]);
        self.pos += len;
        Ok(v)
    }

    /// Remaining bytes
    pub fn remaining(&self) -> usize {
        self.inner.len() - self.pos
    }
}

/// Marshal a Put message to ZAP wire format
pub fn marshal_put(put: &Put) -> Bytes {
    let mut buf = Buffer::new(256);
    buf.write_u8(tags::PUT);
    buf.write_bytes(&put.chain_id);
    buf.write_u32(put.request_id);
    buf.write_bytes(&put.container);
    buf.write_u32(put.engine_type as u32);
    buf.into_bytes()
}

/// Marshal a Gossip message to ZAP wire format
pub fn marshal_gossip(gossip: &Gossip) -> Bytes {
    let mut buf = Buffer::new(256);
    buf.write_u8(tags::GOSSIP);
    buf.write_bytes(&gossip.chain_id);
    buf.write_bytes(&gossip.container);
    buf.into_bytes()
}

/// Marshal a Get message to ZAP wire format
pub fn marshal_get(get: &Get) -> Bytes {
    let mut buf = Buffer::new(128);
    buf.write_u8(tags::GET);
    buf.write_bytes(&get.chain_id);
    buf.write_u32(get.request_id);
    buf.write_bytes(&get.container_id);
    buf.write_u32(get.engine_type as u32);
    buf.into_bytes()
}

/// Marshal a Ping message to ZAP wire format
pub fn marshal_ping(ping: &Ping) -> Bytes {
    let mut buf = Buffer::new(64);
    buf.write_u8(tags::PING);
    buf.write_u32(ping.uptime);
    buf.write_u32(ping.subnet_uptimes.len() as u32);
    for su in &ping.subnet_uptimes {
        buf.write_bytes(&su.subnet_id);
        buf.write_u32(su.uptime);
    }
    buf.into_bytes()
}

/// Marshal any Message to ZAP wire format
pub fn marshal(msg: &Message) -> Bytes {
    match msg {
        Message::Ping(p) => marshal_ping(p),
        Message::Pong(p) => marshal_ping(&Ping {
            uptime: p.uptime,
            subnet_uptimes: p.subnet_uptimes.clone(),
        }),
        Message::Put(p) => marshal_put(p),
        Message::Get(g) => marshal_get(g),
        Message::Gossip(g) => marshal_gossip(g),
    }
}

/// Unmarshal a message from ZAP wire format
pub fn unmarshal(data: &[u8]) -> io::Result<Message> {
    let mut reader = Reader::new(data);
    let tag = reader.read_u8()?;

    match tag {
        tags::PING => {
            let uptime = reader.read_u32()?;
            let count = reader.read_u32()? as usize;
            let mut subnet_uptimes = Vec::with_capacity(count);
            for _ in 0..count {
                subnet_uptimes.push(SubnetUptime {
                    subnet_id: reader.read_bytes()?,
                    uptime: reader.read_u32()?,
                });
            }
            Ok(Message::Ping(Ping { uptime, subnet_uptimes }))
        }
        tags::PONG => {
            let uptime = reader.read_u32()?;
            let count = reader.read_u32()? as usize;
            let mut subnet_uptimes = Vec::with_capacity(count);
            for _ in 0..count {
                subnet_uptimes.push(SubnetUptime {
                    subnet_id: reader.read_bytes()?,
                    uptime: reader.read_u32()?,
                });
            }
            Ok(Message::Pong(Pong { uptime, subnet_uptimes }))
        }
        tags::PUT => {
            let chain_id = reader.read_bytes()?;
            let request_id = reader.read_u32()?;
            let container = reader.read_bytes()?;
            let engine_type = EngineType::from(reader.read_u32()?);
            Ok(Message::Put(Put {
                chain_id,
                request_id,
                container,
                engine_type,
            }))
        }
        tags::GET => {
            let chain_id = reader.read_bytes()?;
            let request_id = reader.read_u32()?;
            let container_id = reader.read_bytes()?;
            let engine_type = EngineType::from(reader.read_u32()?);
            Ok(Message::Get(Get {
                chain_id,
                request_id,
                container_id,
                engine_type,
            }))
        }
        tags::GOSSIP => {
            let chain_id = reader.read_bytes()?;
            let container = reader.read_bytes()?;
            Ok(Message::Gossip(Gossip { chain_id, container }))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("unknown message tag: {tag}"),
        )),
    }
}

/// Encode a replication payload for Lux SharedMemory
///
/// The container format includes:
/// - Operation type (1 byte): 0=Put, 1=Delete
/// - Tree name (length-prefixed)
/// - Key (length-prefixed)
/// - Value (length-prefixed, only for Put)
/// - Traits count (u32)
/// - Traits (each length-prefixed)
pub fn encode_replication_container(
    tree: &str,
    key: &[u8],
    value: Option<&[u8]>,
    traits: &[Vec<u8>],
) -> Bytes {
    let mut buf = Buffer::new(256);

    // Operation type
    buf.write_u8(if value.is_some() { 0 } else { 1 });

    // Tree name
    buf.write_bytes(tree.as_bytes());

    // Key
    buf.write_bytes(key);

    // Value (only for put operations)
    if let Some(v) = value {
        buf.write_bytes(v);
    }

    // Traits for indexing
    buf.write_u32(traits.len() as u32);
    for t in traits {
        buf.write_bytes(t);
    }

    buf.into_bytes()
}

/// Decode a replication container
pub fn decode_replication_container(data: &[u8]) -> io::Result<(String, Vec<u8>, Option<Vec<u8>>, Vec<Vec<u8>>)> {
    let mut reader = Reader::new(data);

    let op_type = reader.read_u8()?;
    let tree = String::from_utf8(reader.read_bytes()?.to_vec())
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
    let key = reader.read_bytes()?.to_vec();

    let value = if op_type == 0 {
        Some(reader.read_bytes()?.to_vec())
    } else {
        None
    };

    let traits_count = reader.read_u32()? as usize;
    let mut traits = Vec::with_capacity(traits_count);
    for _ in 0..traits_count {
        traits.push(reader.read_bytes()?.to_vec());
    }

    Ok((tree, key, value, traits))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffer_write_read() {
        let mut buf = Buffer::new(64);
        buf.write_u8(42);
        buf.write_u16(1000);
        buf.write_u32(100000);
        buf.write_u64(1_000_000_000);
        buf.write_bytes(b"hello");

        let bytes = buf.into_bytes();
        let mut reader = Reader::new(&bytes);

        assert_eq!(reader.read_u8().unwrap(), 42);
        assert_eq!(reader.read_u16().unwrap(), 1000);
        assert_eq!(reader.read_u32().unwrap(), 100000);
        assert_eq!(reader.read_u64().unwrap(), 1_000_000_000);
        assert_eq!(reader.read_bytes().unwrap().as_ref(), b"hello");
    }

    #[test]
    fn test_marshal_unmarshal_put() {
        let put = Put {
            chain_id: Bytes::from_static(b"chain123"),
            request_id: 42,
            container: Bytes::from_static(b"container_data"),
            engine_type: EngineType::Chain,
        };

        let encoded = marshal_put(&put);
        let decoded = unmarshal(&encoded).unwrap();

        if let Message::Put(p) = decoded {
            assert_eq!(p.chain_id.as_ref(), b"chain123");
            assert_eq!(p.request_id, 42);
            assert_eq!(p.container.as_ref(), b"container_data");
            assert_eq!(p.engine_type, EngineType::Chain);
        } else {
            panic!("Expected Put message");
        }
    }

    #[test]
    fn test_marshal_unmarshal_gossip() {
        let gossip = Gossip {
            chain_id: Bytes::from_static(b"chain456"),
            container: Bytes::from_static(b"gossip_data"),
        };

        let encoded = marshal_gossip(&gossip);
        let decoded = unmarshal(&encoded).unwrap();

        if let Message::Gossip(g) = decoded {
            assert_eq!(g.chain_id.as_ref(), b"chain456");
            assert_eq!(g.container.as_ref(), b"gossip_data");
        } else {
            panic!("Expected Gossip message");
        }
    }

    #[test]
    fn test_marshal_unmarshal_ping() {
        let ping = Ping {
            uptime: 12345,
            subnet_uptimes: vec![
                SubnetUptime {
                    subnet_id: Bytes::from_static(b"subnet1"),
                    uptime: 100,
                },
                SubnetUptime {
                    subnet_id: Bytes::from_static(b"subnet2"),
                    uptime: 200,
                },
            ],
        };

        let encoded = marshal_ping(&ping);
        let decoded = unmarshal(&encoded).unwrap();

        if let Message::Ping(p) = decoded {
            assert_eq!(p.uptime, 12345);
            assert_eq!(p.subnet_uptimes.len(), 2);
            assert_eq!(p.subnet_uptimes[0].subnet_id.as_ref(), b"subnet1");
            assert_eq!(p.subnet_uptimes[0].uptime, 100);
        } else {
            panic!("Expected Ping message");
        }
    }

    #[test]
    fn test_replication_container_put() {
        let encoded = encode_replication_container(
            "kms:keys",
            b"key123",
            Some(b"value456"),
            &[b"kms".to_vec(), b"keys".to_vec()],
        );

        let (tree, key, value, traits) = decode_replication_container(&encoded).unwrap();
        assert_eq!(tree, "kms:keys");
        assert_eq!(key, b"key123");
        assert_eq!(value, Some(b"value456".to_vec()));
        assert_eq!(traits.len(), 2);
        assert_eq!(traits[0], b"kms");
        assert_eq!(traits[1], b"keys");
    }

    #[test]
    fn test_replication_container_delete() {
        let encoded = encode_replication_container(
            "iam:apikeys",
            b"key789",
            None,
            &[b"iam".to_vec(), b"apikeys".to_vec()],
        );

        let (tree, key, value, traits) = decode_replication_container(&encoded).unwrap();
        assert_eq!(tree, "iam:apikeys");
        assert_eq!(key, b"key789");
        assert_eq!(value, None);
        assert_eq!(traits.len(), 2);
    }

    #[test]
    fn test_unknown_tag() {
        let data = [99u8, 0, 0, 0, 0]; // Unknown tag 99
        let result = unmarshal(&data);
        assert!(result.is_err());
    }
}
