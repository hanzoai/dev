use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// A ZT service that can be dialed or bound
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Service {
    pub id: String,
    pub name: String,
    #[serde(default)]
    pub permissions: Vec<String>,
    #[serde(default)]
    pub posture_queries: Vec<serde_json::Value>,
    #[serde(default)]
    pub configs: Vec<serde_json::Value>,
    #[serde(default)]
    pub encryption_required: bool,
}

/// Detailed service info from the controller
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ServiceDetail {
    pub id: String,
    pub name: String,
    #[serde(default)]
    pub permissions: Vec<String>,
    #[serde(default)]
    pub encryption_required: bool,
    #[serde(default)]
    pub configs: Vec<ServiceConfig>,
}

/// Service configuration attached to a service
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ServiceConfig {
    #[serde(rename = "configTypeId")]
    pub config_type_id: String,
    pub config: serde_json::Value,
}

/// A ZT session for dialing or binding a service
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Session {
    pub id: String,
    pub token: String,
    #[serde(rename = "type")]
    pub session_type: SessionType,
    pub service_id: String,
    #[serde(default)]
    pub edge_routers: Vec<EdgeRouterEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum SessionType {
    Dial,
    Bind,
}

/// An edge router entry from a session
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EdgeRouterEntry {
    pub name: String,
    pub hostname: String,
    pub urls: std::collections::HashMap<String, String>,
}

/// Identity enrolled on the ZT network
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Identity {
    pub id: String,
    pub name: String,
    #[serde(rename = "type")]
    pub identity_type: serde_json::Value,
    #[serde(default)]
    pub is_admin: bool,
}

/// API session returned after authentication
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ApiSession {
    pub id: String,
    pub token: String,
    pub identity: Identity,
    #[serde(default)]
    pub expires_at: Option<DateTime<Utc>>,
}

/// Envelope for controller API responses
#[derive(Debug, Clone, Deserialize)]
pub struct ApiResponse<T> {
    pub data: T,
    pub meta: Option<ApiMeta>,
}

/// Envelope for list responses
#[derive(Debug, Clone, Deserialize)]
pub struct ApiListResponse<T> {
    pub data: Vec<T>,
    pub meta: Option<ApiMeta>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ApiMeta {
    pub pagination: Option<Pagination>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Pagination {
    pub limit: u32,
    pub offset: u32,
    pub total_count: u32,
}

/// Error from the controller API
#[derive(Debug, Clone, Deserialize)]
pub struct ApiError {
    pub error: ApiErrorDetail,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ApiErrorDetail {
    pub code: String,
    pub message: String,
}
