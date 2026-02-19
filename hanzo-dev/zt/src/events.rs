use tokio::sync::broadcast;
use tracing::debug;

/// Events emitted by the ZT context
#[derive(Debug, Clone)]
pub enum ZtEvent {
    /// Successfully authenticated with the controller
    Authenticated { identity_id: String },
    /// Authentication failed
    AuthFailed { reason: String },
    /// A new service became available
    ServiceAvailable { name: String, id: String },
    /// A service was removed
    ServiceRemoved { name: String, id: String },
    /// Connected to an edge router
    RouterConnected { name: String },
    /// Disconnected from an edge router
    RouterDisconnected { name: String },
    /// Session created for a service
    SessionCreated { service: String, session_id: String },
    /// Session closed
    SessionClosed { session_id: String },
    /// Billing check passed
    BillingOk { service: String },
    /// Billing check failed
    BillingFailed { service: String, reason: String },
}

/// Event emitter for ZT lifecycle events
pub struct EventEmitter {
    tx: broadcast::Sender<ZtEvent>,
}

impl EventEmitter {
    pub fn new(capacity: usize) -> Self {
        let (tx, _) = broadcast::channel(capacity);
        Self { tx }
    }

    /// Subscribe to events
    pub fn subscribe(&self) -> broadcast::Receiver<ZtEvent> {
        self.tx.subscribe()
    }

    /// Emit an event
    pub fn emit(&self, event: ZtEvent) {
        debug!(?event, "zt event");
        // Ignore error if no receivers
        let _ = self.tx.send(event);
    }
}

impl Default for EventEmitter {
    fn default() -> Self {
        Self::new(64)
    }
}
