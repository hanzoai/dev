use crate::billing::BillingGuard;
use crate::config::Config;
use crate::connection::ZtConnection;
use crate::controller::ControllerClient;
use crate::error::{Result, ZtError};
use crate::events::{EventEmitter, ZtEvent};
use crate::models::*;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};
use tracing::{debug, info};

/// Main entry point for the ZT SDK.
///
/// Manages authentication, service discovery, and connections through
/// the ZT fabric via the controller REST API.
pub struct ZtContext {
    config: Config,
    controller: Mutex<ControllerClient>,
    events: Arc<EventEmitter>,
    api_session: Mutex<Option<ApiSession>>,
}

impl ZtContext {
    /// Create a new ZT context with the given configuration
    pub async fn new(config: Config) -> Result<Self> {
        let controller = ControllerClient::new(
            &config.controller_url,
            config.request_timeout,
        )?;

        Ok(Self {
            config,
            controller: Mutex::new(controller),
            events: Arc::new(EventEmitter::default()),
            api_session: Mutex::new(None),
        })
    }

    /// Authenticate with the ZT controller using configured credentials
    pub async fn authenticate(&self) -> Result<()> {
        let method = self.config.credentials.auth_method();

        info!(method, creds = %self.config.credentials.display(), "authenticating with ZT controller");

        let session = match method {
            "ext-jwt" => {
                let jwt = self.resolve_jwt_token()?;
                let mut ctrl = self.controller.lock().await;
                ctrl.authenticate_ext_jwt(&jwt).await?
            }
            _ => {
                return Err(ZtError::AuthFailed(format!(
                    "unsupported auth method: {method}"
                )));
            }
        };

        self.events.emit(ZtEvent::Authenticated {
            identity_id: session.identity.id.clone(),
        });

        *self.api_session.lock().await = Some(session);

        info!("ZT authentication successful");
        Ok(())
    }

    /// Dial a service by name, returning a connection
    pub async fn dial(&self, service: &str) -> Result<ZtConnection> {
        self.ensure_authenticated().await?;

        // Billing check (no free tier)
        if self.config.billing_enabled {
            self.check_billing(service).await?;
        }

        let ctrl = self.controller.lock().await;

        // Find the service
        let svc = ctrl.get_service_by_name(service).await?;
        debug!(service_id = %svc.id, service = %svc.name, "found service");

        // Create a dial session
        let session = ctrl.create_dial_session(&svc.id).await?;
        debug!(session_id = %session.id, "created dial session");

        if session.edge_routers.is_empty() {
            return Err(ZtError::NoEdgeRouters(service.to_string()));
        }

        self.events.emit(ZtEvent::SessionCreated {
            service: service.to_string(),
            session_id: session.id.clone(),
        });

        // Create connection channels
        let (tx_out, _rx_out) = mpsc::channel::<Vec<u8>>(256);
        let (tx_in, rx_in) = mpsc::channel::<Vec<u8>>(256);

        // In a real implementation, this would establish a data channel
        // through the edge router using the session token.
        // For now, we create the connection abstraction.
        let conn = ZtConnection::new(
            rx_in,
            tx_out,
            session.id,
            service.to_string(),
        );

        // TODO: Spawn edge router data plane connection task
        // using session.edge_routers[0] and session.token
        let _ = tx_in; // Will be used by the data plane task

        Ok(conn)
    }

    /// Listen (bind) on a service, accepting incoming connections
    pub async fn listen(&self, service: &str) -> Result<ZtListener> {
        self.ensure_authenticated().await?;

        if self.config.billing_enabled {
            self.check_billing(service).await?;
        }

        let ctrl = self.controller.lock().await;
        let svc = ctrl.get_service_by_name(service).await?;
        let session = ctrl.create_bind_session(&svc.id).await?;

        self.events.emit(ZtEvent::SessionCreated {
            service: service.to_string(),
            session_id: session.id.clone(),
        });

        Ok(ZtListener {
            session_id: session.id,
            service_name: service.to_string(),
        })
    }

    /// List all services available to the current identity
    pub async fn services(&self) -> Result<Vec<ServiceDetail>> {
        self.ensure_authenticated().await?;
        let ctrl = self.controller.lock().await;
        let services = ctrl.list_services().await?;
        Ok(services
            .into_iter()
            .map(|s| ServiceDetail {
                id: s.id,
                name: s.name,
                permissions: s.permissions,
                encryption_required: s.encryption_required,
                configs: vec![],
            })
            .collect())
    }

    /// Subscribe to ZT events
    pub fn events(&self) -> tokio::sync::broadcast::Receiver<ZtEvent> {
        self.events.subscribe()
    }

    /// Get the current API session info
    pub async fn session(&self) -> Option<ApiSession> {
        self.api_session.lock().await.clone()
    }

    // --- Internal ---

    fn resolve_jwt_token(&self) -> Result<String> {
        // Extract token from credentials payload or known credential types
        if let Ok(payload) = self.config.credentials.auth_payload() {
            if let Some(token) = payload.get("token").and_then(|t| t.as_str()) {
                return Ok(token.to_string());
            }
        }

        // Try environment as fallback
        if let Ok(key) = std::env::var("HANZO_API_KEY") {
            if !key.is_empty() {
                return Ok(key);
            }
        }

        Err(ZtError::AuthFailed("cannot resolve JWT token".into()))
    }

    async fn ensure_authenticated(&self) -> Result<()> {
        let ctrl = self.controller.lock().await;
        if !ctrl.is_authenticated() {
            return Err(ZtError::NotAuthenticated);
        }
        Ok(())
    }

    async fn check_billing(&self, service: &str) -> Result<()> {
        // Resolve auth token for billing
        let token = self.resolve_jwt_token().unwrap_or_default();
        let guard = BillingGuard::new(&self.config.commerce_url, &token);
        guard.check_balance(service).await?;
        self.events.emit(ZtEvent::BillingOk {
            service: service.to_string(),
        });
        Ok(())
    }
}

/// Listener for incoming connections on a bound service
pub struct ZtListener {
    session_id: String,
    service_name: String,
}

impl ZtListener {
    /// Accept the next incoming connection
    pub async fn accept(&self) -> Result<ZtConnection> {
        // TODO: Implement accept via edge router data plane
        let (_tx, rx) = mpsc::channel(256);
        let (tx2, _rx2) = mpsc::channel(256);
        Ok(ZtConnection::new(
            rx,
            tx2,
            self.session_id.clone(),
            self.service_name.clone(),
        ))
    }

    /// Get the session ID
    pub fn session_id(&self) -> &str {
        &self.session_id
    }

    /// Get the service name
    pub fn service_name(&self) -> &str {
        &self.service_name
    }
}
