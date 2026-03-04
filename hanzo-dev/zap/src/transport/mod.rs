//! Transport layer for ZAP connections.
//!
//! Supports multiple transport protocols:
//! - TCP (`zap://`)
//! - TLS (`zaps://`)
//! - Unix domain socket (`zap+unix://`)
//! - HTTP/SSE (`http://`, `https://`) — default for Hanzo services

pub mod http;
pub use http::HttpTransport;

use crate::buffer::Buffer;
use crate::error::Error;
use crate::error::Result;
use crate::message::MessageType;
use crate::wire::MAX_MESSAGE_SIZE;
use crate::wire::parse_frame;
use std::sync::Arc;
use tokio::io::AsyncRead;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWrite;
use tokio::io::AsyncWriteExt;
use tokio::net::TcpStream;
use tracing::debug;
use tracing::trace;

/// Inner stream — either plain TCP or TLS.
enum StreamInner {
    Tcp(TcpStream),
    Tls(tokio_rustls::client::TlsStream<TcpStream>),
}

impl AsyncRead for StreamInner {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        match self.get_mut() {
            StreamInner::Tcp(s) => std::pin::Pin::new(s).poll_read(cx, buf),
            StreamInner::Tls(s) => std::pin::Pin::new(s).poll_read(cx, buf),
        }
    }
}

impl AsyncWrite for StreamInner {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        match self.get_mut() {
            StreamInner::Tcp(s) => std::pin::Pin::new(s).poll_write(cx, buf),
            StreamInner::Tls(s) => std::pin::Pin::new(s).poll_write(cx, buf),
        }
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        match self.get_mut() {
            StreamInner::Tcp(s) => std::pin::Pin::new(s).poll_flush(cx),
            StreamInner::Tls(s) => std::pin::Pin::new(s).poll_flush(cx),
        }
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        match self.get_mut() {
            StreamInner::Tcp(s) => std::pin::Pin::new(s).poll_shutdown(cx),
            StreamInner::Tls(s) => std::pin::Pin::new(s).poll_shutdown(cx),
        }
    }
}

/// A ZAP transport connection. Supports both plain TCP and TLS 1.3.
pub struct ZapTransport {
    stream: StreamInner,
    read_buf: Vec<u8>,
    write_buf: Buffer,
}

impl ZapTransport {
    /// Connect to a ZAP server.
    ///
    /// URL schemes:
    /// - `zap://host:port` — plain TCP
    /// - `zaps://host:port` — TLS 1.3 (with PQ key exchange when available)
    pub async fn connect(url: &str) -> Result<Self> {
        let (host, port, tls) = parse_url(url)?;

        debug!("connecting to {}:{} (tls={})", host, port, tls);
        let tcp_stream = TcpStream::connect(format!("{}:{}", host, port))
            .await
            .map_err(|e| Error::Connection(e.to_string()))?;
        tcp_stream.set_nodelay(true).ok();

        let stream = if tls {
            let mut root_store = rustls::RootCertStore::empty();
            root_store.extend(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());

            let config = rustls::ClientConfig::builder()
                .with_root_certificates(root_store)
                .with_no_client_auth();

            let connector = tokio_rustls::TlsConnector::from(Arc::new(config));
            let server_name = rustls::pki_types::ServerName::try_from(host.clone())
                .map_err(|e| Error::Connection(format!("invalid server name: {e}")))?;

            let tls_stream = connector
                .connect(server_name, tcp_stream)
                .await
                .map_err(|e| Error::Connection(format!("TLS handshake failed: {e}")))?;

            debug!("TLS 1.3 connection established to {}:{}", host, port);
            StreamInner::Tls(tls_stream)
        } else {
            StreamInner::Tcp(tcp_stream)
        };

        Ok(Self {
            stream,
            read_buf: Vec::with_capacity(64 * 1024),
            write_buf: Buffer::new(),
        })
    }

    /// Send a message.
    pub async fn send(&mut self, msg_type: MessageType, payload: &[u8]) -> Result<()> {
        self.write_buf.clear();

        // Write framed message
        let total_len = 1 + payload.len();
        self.write_buf.write_u32_le(total_len as u32);
        self.write_buf.write_u8(msg_type as u8);
        self.write_buf.extend_from_slice(payload);

        trace!("sending {:?} message ({} bytes)", msg_type, total_len);

        self.stream
            .write_all(self.write_buf.bytes())
            .await
            .map_err(|e| Error::Io(e))?;

        Ok(())
    }

    /// Receive a message.
    ///
    /// Returns the message type and a reference to the payload in the internal buffer.
    /// The payload reference is valid until the next call to `recv`.
    pub async fn recv(&mut self) -> Result<(MessageType, &[u8])> {
        // Read length header
        let mut header = [0u8; 4];
        self.stream
            .read_exact(&mut header)
            .await
            .map_err(|e| Error::Io(e))?;

        let length = u32::from_le_bytes(header) as usize;

        if length > MAX_MESSAGE_SIZE {
            return Err(Error::MessageTooLarge {
                size: length,
                max: MAX_MESSAGE_SIZE,
            });
        }

        // Resize buffer if needed
        if self.read_buf.len() < length {
            self.read_buf.resize(length, 0);
        }

        // Read message body
        self.stream
            .read_exact(&mut self.read_buf[..length])
            .await
            .map_err(|e| Error::Io(e))?;

        // Parse message type
        if length < 1 {
            return Err(Error::Protocol("message too short".to_string()));
        }

        let msg_type = MessageType::try_from(self.read_buf[0])?;
        let payload = &self.read_buf[1..length];

        trace!("received {:?} message ({} bytes)", msg_type, length);

        Ok((msg_type, payload))
    }

    /// Close the connection.
    pub async fn close(mut self) -> Result<()> {
        self.send(MessageType::Close, &[]).await?;
        self.stream.shutdown().await.ok();
        Ok(())
    }
}

/// Parse a ZAP URL.
///
/// Supported schemes:
/// - `zap://host:port` - Plain TCP
/// - `zaps://host:port` - TLS
/// - `zap+unix:///path` - Unix domain socket
fn parse_url(url: &str) -> Result<(String, u16, bool)> {
    if url.starts_with("zap://") {
        let addr = &url[6..];
        let (host, port) = parse_host_port(addr)?;
        Ok((host, port, false))
    } else if url.starts_with("zaps://") {
        let addr = &url[7..];
        let (host, port) = parse_host_port(addr)?;
        Ok((host, port, true))
    } else if url.starts_with("zap+unix://") {
        // Unix socket - return path as host, port 0
        let path = &url[11..];
        Ok((path.to_string(), 0, false))
    } else {
        Err(Error::InvalidScheme(url.to_string()))
    }
}

fn parse_host_port(addr: &str) -> Result<(String, u16)> {
    if let Some(colon_pos) = addr.rfind(':') {
        let host = addr[..colon_pos].to_string();
        let port = addr[colon_pos + 1..]
            .parse()
            .map_err(|_| Error::Connection(format!("invalid port in {}", addr)))?;
        Ok((host, port))
    } else {
        // Default port
        Ok((addr.to_string(), crate::DEFAULT_PORT))
    }
}

/// A framed message received from the wire.
#[derive(Debug)]
pub struct FramedMessage {
    pub msg_type: MessageType,
    pub payload: Vec<u8>,
}

impl FramedMessage {
    /// Parse from wire bytes.
    pub fn from_bytes(data: &[u8]) -> Result<Self> {
        let (msg_type, payload) = parse_frame(data)?;
        Ok(Self {
            msg_type,
            payload: payload.to_vec(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_url() {
        let (host, port, tls) = parse_url("zap://localhost:9999").unwrap();
        assert_eq!(host, "localhost");
        assert_eq!(port, 9999);
        assert!(!tls);

        let (host, port, tls) = parse_url("zaps://api.hanzo.ai:443").unwrap();
        assert_eq!(host, "api.hanzo.ai");
        assert_eq!(port, 443);
        assert!(tls);

        let (host, port, _) = parse_url("zap://localhost").unwrap();
        assert_eq!(host, "localhost");
        assert_eq!(port, crate::DEFAULT_PORT);
    }
}
