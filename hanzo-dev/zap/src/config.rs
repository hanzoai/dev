//! Configuration types for ZAP gateway and servers.

use serde::{Deserialize, Serialize};

/// Gateway configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GatewayConfig {
    /// Address to listen on (e.g., "0.0.0.0:9999")
    pub listen: String,

    /// TLS certificate path (optional, enables zaps://)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tls_cert: Option<String>,

    /// TLS key path
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tls_key: Option<String>,

    /// Connected MCP/ZAP servers
    #[serde(default)]
    pub servers: Vec<McpServerConfig>,

    /// Maximum message size (default: 16MB)
    #[serde(default = "default_max_message_size")]
    pub max_message_size: usize,

    /// Request timeout in milliseconds (default: 30000)
    #[serde(default = "default_timeout")]
    pub timeout_ms: u64,
}

fn default_max_message_size() -> usize {
    16 * 1024 * 1024 // 16MB
}

fn default_timeout() -> u64 {
    30000 // 30 seconds
}

impl Default for GatewayConfig {
    fn default() -> Self {
        Self {
            listen: "127.0.0.1:9999".to_string(),
            tls_cert: None,
            tls_key: None,
            servers: Vec::new(),
            max_message_size: default_max_message_size(),
            timeout_ms: default_timeout(),
        }
    }
}

/// MCP server configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpServerConfig {
    /// Server name
    pub name: String,

    /// Transport configuration
    pub transport: Transport,

    /// Authentication (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auth: Option<Auth>,

    /// Request timeout override (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout_ms: Option<u64>,
}

/// Transport configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Transport {
    /// Subprocess (stdio) transport - for local MCP servers
    Stdio {
        command: String,
        #[serde(default)]
        args: Vec<String>,
        #[serde(default)]
        env: std::collections::HashMap<String, String>,
    },

    /// HTTP/SSE transport - for remote MCP servers
    Http { url: String },

    /// WebSocket transport - for browser-compatible servers
    WebSocket { url: String },

    /// Native ZAP transport - for ZAP servers
    Zap { url: String },

    /// Unix domain socket - for local high-performance
    Unix { path: String },
}

/// Authentication configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Auth {
    /// No authentication
    None,

    /// Bearer token (JWT or API key)
    Bearer { token: String },

    /// Basic authentication
    Basic { username: String, password: String },

    /// Client certificate (mTLS)
    Certificate { cert: String, key: String },
}

/// Example configuration for Claude Desktop integration.
///
/// ```json
/// {
///   "zapGateway": {
///     "listen": "127.0.0.1:9999",
///     "servers": [
///       {
///         "name": "filesystem",
///         "transport": {
///           "type": "stdio",
///           "command": "mcp-server-filesystem",
///           "args": ["/Users/me"]
///         }
///       },
///       {
///         "name": "search",
///         "transport": {
///           "type": "zap",
///           "url": "zaps://search.hanzo.ai:9999"
///         }
///       },
///       {
///         "name": "github",
///         "transport": {
///           "type": "http",
///           "url": "https://mcp.github.com"
///         },
///         "auth": {
///           "type": "bearer",
///           "token": "ghp_..."
///         }
///       }
///     ]
///   }
/// }
/// ```
pub fn example_config() -> GatewayConfig {
    GatewayConfig {
        listen: "127.0.0.1:9999".to_string(),
        tls_cert: None,
        tls_key: None,
        servers: vec![
            McpServerConfig {
                name: "filesystem".to_string(),
                transport: Transport::Stdio {
                    command: "mcp-server-filesystem".to_string(),
                    args: vec!["/home".to_string()],
                    env: Default::default(),
                },
                auth: None,
                timeout_ms: None,
            },
            McpServerConfig {
                name: "search".to_string(),
                transport: Transport::Zap {
                    url: "zaps://search.hanzo.ai:9999".to_string(),
                },
                auth: None,
                timeout_ms: None,
            },
        ],
        max_message_size: default_max_message_size(),
        timeout_ms: default_timeout(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_serde() {
        let config = example_config();
        let json = serde_json::to_string_pretty(&config).unwrap();
        let parsed: GatewayConfig = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.listen, config.listen);
        assert_eq!(parsed.servers.len(), 2);
    }
}
