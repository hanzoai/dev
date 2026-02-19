use crate::error::{Result, ZtError};
use std::pin::Pin;
use std::task::{Context, Poll};
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};
use tokio::sync::mpsc;

/// A connection to a ZT service, implementing AsyncRead + AsyncWrite.
///
/// Data flows through the ZT fabric (edge routers) transparently.
/// The connection handles framing and multiplexing internally.
pub struct ZtConnection {
    /// Receive channel for incoming data
    rx: mpsc::Receiver<Vec<u8>>,
    /// Send channel for outgoing data
    tx: mpsc::Sender<Vec<u8>>,
    /// Buffered partial read from last recv
    read_buf: Vec<u8>,
    read_offset: usize,
    /// Session info
    session_id: String,
    service_name: String,
    /// Whether the connection is open
    closed: bool,
}

impl ZtConnection {
    pub(crate) fn new(
        rx: mpsc::Receiver<Vec<u8>>,
        tx: mpsc::Sender<Vec<u8>>,
        session_id: String,
        service_name: String,
    ) -> Self {
        Self {
            rx,
            tx,
            read_buf: Vec::new(),
            read_offset: 0,
            session_id,
            service_name,
            closed: false,
        }
    }

    /// Get the session ID for this connection
    pub fn session_id(&self) -> &str {
        &self.session_id
    }

    /// Get the service name this connection is for
    pub fn service_name(&self) -> &str {
        &self.service_name
    }

    /// Send data through the ZT connection
    pub async fn send(&self, data: &[u8]) -> Result<()> {
        if self.closed {
            return Err(ZtError::ConnectionClosed);
        }
        self.tx
            .send(data.to_vec())
            .await
            .map_err(|_| ZtError::ConnectionClosed)
    }

    /// Receive data from the ZT connection
    pub async fn recv(&mut self) -> Result<Vec<u8>> {
        if self.closed {
            return Err(ZtError::ConnectionClosed);
        }
        self.rx
            .recv()
            .await
            .ok_or(ZtError::ConnectionClosed)
    }

    /// Close the connection
    pub async fn close(&mut self) -> Result<()> {
        self.closed = true;
        Ok(())
    }

    /// Check if the connection is still open
    pub fn is_connected(&self) -> bool {
        !self.closed && !self.tx.is_closed()
    }
}

impl AsyncRead for ZtConnection {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        // First drain any buffered data from previous recv
        if self.read_offset < self.read_buf.len() {
            let remaining = &self.read_buf[self.read_offset..];
            let to_copy = remaining.len().min(buf.remaining());
            buf.put_slice(&remaining[..to_copy]);
            self.read_offset += to_copy;
            if self.read_offset >= self.read_buf.len() {
                self.read_buf.clear();
                self.read_offset = 0;
            }
            return Poll::Ready(Ok(()));
        }

        // Try to receive more data
        match self.rx.poll_recv(cx) {
            Poll::Ready(Some(data)) => {
                let to_copy = data.len().min(buf.remaining());
                buf.put_slice(&data[..to_copy]);
                if to_copy < data.len() {
                    self.read_buf = data;
                    self.read_offset = to_copy;
                }
                Poll::Ready(Ok(()))
            }
            Poll::Ready(None) => Poll::Ready(Ok(())), // EOF
            Poll::Pending => Poll::Pending,
        }
    }
}

impl AsyncWrite for ZtConnection {
    fn poll_write(
        self: Pin<&mut Self>,
        _cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        let data = buf.to_vec();
        let len = data.len();
        match self.tx.try_send(data) {
            Ok(()) => Poll::Ready(Ok(len)),
            Err(mpsc::error::TrySendError::Full(_)) => {
                // Channel is full; signal the caller to retry later.
                // NOTE: This does not register a waker — for production use,
                // wrap with tokio_util::sync::PollSender for proper waking.
                Poll::Pending
            }
            Err(mpsc::error::TrySendError::Closed(_)) => {
                Poll::Ready(Err(std::io::Error::new(
                    std::io::ErrorKind::BrokenPipe,
                    "connection closed",
                )))
            }
        }
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        self.closed = true;
        Poll::Ready(Ok(()))
    }
}
