use crate::error::{Result, ZtError};
use crate::models::*;
use reqwest::header::{HeaderMap, HeaderValue, CONTENT_TYPE};
use std::time::Duration;
use tracing::{debug, warn};

/// REST client for the ZT controller edge API (`/edge/client/v1`)
pub struct ControllerClient {
    http: reqwest::Client,
    base_url: String,
    session_token: Option<String>,
}

impl ControllerClient {
    /// Create a new controller client
    pub fn new(base_url: &str, timeout: Duration) -> Result<Self> {
        let http = reqwest::Client::builder()
            .timeout(timeout)
            .build()?;

        let base_url = base_url.trim_end_matches('/').to_string();

        Ok(Self {
            http,
            base_url,
            session_token: None,
        })
    }

    /// Authenticate using external JWT (Hanzo IAM)
    pub async fn authenticate_ext_jwt(&mut self, jwt: &str) -> Result<ApiSession> {
        let url = format!("{}/edge/client/v1/authenticate?method=ext-jwt", self.base_url);

        let mut headers = HeaderMap::new();
        headers.insert(
            CONTENT_TYPE,
            HeaderValue::from_static("application/json"),
        );
        headers.insert(
            "Authorization",
            HeaderValue::from_str(&format!("Bearer {jwt}"))
                .map_err(|e| ZtError::AuthFailed(format!("invalid token: {e}")))?,
        );

        let body = serde_json::json!({ "configTypes": [] });

        let resp = self
            .http
            .post(&url)
            .headers(headers)
            .json(&body)
            .send()
            .await?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp.text().await.unwrap_or_default();
            return Err(ZtError::AuthFailed(format!(
                "controller returned {status}: {text}"
            )));
        }

        let envelope: ApiResponse<ApiSession> = resp.json().await?;
        self.session_token = Some(envelope.data.token.clone());
        debug!(identity_id = %envelope.data.identity.id, "authenticated with ZT controller");
        Ok(envelope.data)
    }

    /// Authenticate using username/password (updb)
    pub async fn authenticate_updb(&mut self, username: &str, password: &str) -> Result<ApiSession> {
        let url = format!("{}/edge/client/v1/authenticate?method=password", self.base_url);

        let body = serde_json::json!({
            "username": username,
            "password": password,
            "configTypes": [],
        });

        let resp = self.http.post(&url).json(&body).send().await?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp.text().await.unwrap_or_default();
            return Err(ZtError::AuthFailed(format!(
                "controller returned {status}: {text}"
            )));
        }

        let envelope: ApiResponse<ApiSession> = resp.json().await?;
        self.session_token = Some(envelope.data.token.clone());
        Ok(envelope.data)
    }

    /// List services available to the current identity
    pub async fn list_services(&self) -> Result<Vec<Service>> {
        let data: Vec<Service> = self.get_list("/edge/client/v1/services").await?;
        Ok(data)
    }

    /// Get a specific service by name
    pub async fn get_service_by_name(&self, name: &str) -> Result<Service> {
        let services = self.list_services().await?;
        services
            .into_iter()
            .find(|s| s.name == name)
            .ok_or_else(|| ZtError::ServiceNotFound(name.to_string()))
    }

    /// Create a session to dial a service
    pub async fn create_dial_session(&self, service_id: &str) -> Result<Session> {
        self.create_session(service_id, SessionType::Dial).await
    }

    /// Create a session to bind (listen on) a service
    pub async fn create_bind_session(&self, service_id: &str) -> Result<Session> {
        self.create_session(service_id, SessionType::Bind).await
    }

    async fn create_session(&self, service_id: &str, session_type: SessionType) -> Result<Session> {
        let url = format!("{}/edge/client/v1/sessions", self.base_url);

        let type_str = match session_type {
            SessionType::Dial => "Dial",
            SessionType::Bind => "Bind",
        };

        let body = serde_json::json!({
            "serviceId": service_id,
            "type": type_str,
        });

        let resp = self.authed_request(reqwest::Method::POST, &url)
            .json(&body)
            .send()
            .await?;

        self.handle_response::<Session>(resp).await
    }

    /// Delete (close) a session
    pub async fn delete_session(&self, session_id: &str) -> Result<()> {
        let url = format!("{}/edge/client/v1/sessions/{session_id}", self.base_url);
        let resp = self.authed_request(reqwest::Method::DELETE, &url)
            .send()
            .await?;

        if !resp.status().is_success() {
            let status = resp.status();
            warn!(%status, "failed to delete session {session_id}");
        }
        Ok(())
    }

    /// Get current API session info
    pub async fn get_current_session(&self) -> Result<ApiSession> {
        let data: ApiSession = self.get("/edge/client/v1/current-api-session").await?;
        Ok(data)
    }

    /// Check if we have a valid session token
    pub fn is_authenticated(&self) -> bool {
        self.session_token.is_some()
    }

    /// Get the current session token
    pub fn session_token(&self) -> Option<&str> {
        self.session_token.as_deref()
    }

    // --- Internal helpers ---

    fn authed_request(&self, method: reqwest::Method, url: &str) -> reqwest::RequestBuilder {
        let mut req = self.http.request(method, url);
        if let Some(ref token) = self.session_token {
            req = req.header("zt-session", token);
        }
        req
    }

    async fn get<T: serde::de::DeserializeOwned>(&self, path: &str) -> Result<T> {
        let url = format!("{}{path}", self.base_url);
        let resp = self.authed_request(reqwest::Method::GET, &url)
            .send()
            .await?;
        self.handle_response::<T>(resp).await
    }

    async fn get_list<T: serde::de::DeserializeOwned>(&self, path: &str) -> Result<Vec<T>> {
        let url = format!("{}{path}", self.base_url);
        let resp = self.authed_request(reqwest::Method::GET, &url)
            .send()
            .await?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp.text().await.unwrap_or_default();
            return Err(ZtError::Controller(format!("{status}: {text}")));
        }

        let envelope: ApiListResponse<T> = resp.json().await?;
        Ok(envelope.data)
    }

    async fn handle_response<T: serde::de::DeserializeOwned>(&self, resp: reqwest::Response) -> Result<T> {
        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp.text().await.unwrap_or_default();
            return Err(ZtError::Controller(format!("{status}: {text}")));
        }

        let envelope: ApiResponse<T> = resp.json().await?;
        Ok(envelope.data)
    }
}
