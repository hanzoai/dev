//! Network tool executor.
//!
//! Provides HTTP requests, SSH, and network operations.

use crate::error::{Error, Result};
use crate::executor::{ExecutorContext, ToolExecutor};
use crate::message::ToolResult;
use crate::tools::{ToolCategory, network};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;
use std::time::Instant;

/// Network executor for HTTP and network operations
pub struct NetworkExecutor {
    /// HTTP client
    client: reqwest::Client,
}

impl NetworkExecutor {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::builder()
                .timeout(std::time::Duration::from_secs(30))
                .build()
                .unwrap_or_default(),
        }
    }

    fn result(content: impl serde::Serialize, error: Option<String>) -> Result<ToolResult> {
        Ok(ToolResult {
            id: String::new(),
            content: serde_json::to_value(content).unwrap_or(Value::Null),
            error,
            metadata: Default::default(),
        })
    }

    async fn http_request(
        &self,
        args: network::HttpRequestArgs,
        _ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        let method = match args.method.to_uppercase().as_str() {
            "GET" => reqwest::Method::GET,
            "POST" => reqwest::Method::POST,
            "PUT" => reqwest::Method::PUT,
            "DELETE" => reqwest::Method::DELETE,
            "PATCH" => reqwest::Method::PATCH,
            "HEAD" => reqwest::Method::HEAD,
            "OPTIONS" => reqwest::Method::OPTIONS,
            _ => return Err(Error::Tool(format!("Invalid method: {}", args.method))),
        };

        let mut request = self.client.request(method, &args.url);

        // Add headers
        if let Some(headers) = args.headers {
            for (key, value) in headers {
                request = request.header(&key, &value);
            }
        }

        // Add body
        if let Some(body) = args.body {
            request = request.body(body);
        }

        // Add JSON body
        if let Some(json) = args.json {
            request = request.json(&json);
        }

        let start = Instant::now();
        let response = request
            .send()
            .await
            .map_err(|e| Error::Tool(format!("HTTP request failed: {}", e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;
        let status = response.status().as_u16();
        let headers: HashMap<String, String> = response
            .headers()
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_str().unwrap_or("").to_string()))
            .collect();

        let body = response
            .text()
            .await
            .map_err(|e| Error::Tool(format!("Failed to read response body: {}", e)))?;

        let result = network::HttpResponse {
            status,
            headers,
            body,
            duration_ms,
        };

        let error = if status >= 400 {
            Some(format!("HTTP {}", status))
        } else {
            None
        };

        Self::result(result, error)
    }

    async fn fetch_url(
        &self,
        args: network::FetchUrlArgs,
        _ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        let start = Instant::now();

        let mut request = self.client.get(&args.url);
        if let Some(headers) = args.headers {
            for (key, value) in headers {
                request = request.header(&key, &value);
            }
        }

        let response = request
            .send()
            .await
            .map_err(|e| Error::Tool(format!("Fetch failed: {}", e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;
        let status = response.status().as_u16();

        let content = if args.binary.unwrap_or(false) {
            let bytes = response
                .bytes()
                .await
                .map_err(|e| Error::Tool(format!("Failed to read response: {}", e)))?;
            base64::Engine::encode(&base64::engine::general_purpose::STANDARD, &bytes)
        } else {
            response
                .text()
                .await
                .map_err(|e| Error::Tool(format!("Failed to read response: {}", e)))?
        };

        let result = network::FetchResult {
            status,
            content,
            duration_ms,
            content_type: None, // Would get from headers
        };

        let error = if status >= 400 {
            Some(format!("HTTP {}", status))
        } else {
            None
        };

        Self::result(result, error)
    }

    async fn port_check(
        &self,
        args: network::PortCheckArgs,
        _ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        use tokio::net::TcpStream;
        use tokio::time::{Duration, timeout};

        let timeout_ms = args.timeout_ms.unwrap_or(5000);
        let addr = format!("{}:{}", args.host, args.port);

        let is_open = timeout(Duration::from_millis(timeout_ms), TcpStream::connect(&addr))
            .await
            .map(|r| r.is_ok())
            .unwrap_or(false);

        let result = network::PortCheckResult {
            host: args.host,
            port: args.port,
            is_open,
        };

        Self::result(result, None)
    }

    async fn dns_lookup(
        &self,
        args: network::DnsLookupArgs,
        _ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        use tokio::net::lookup_host;

        let host_with_port = format!("{}:0", args.host);
        let addrs: Vec<String> = lookup_host(&host_with_port)
            .await
            .map_err(|e| Error::Tool(format!("DNS lookup failed: {}", e)))?
            .map(|addr| addr.ip().to_string())
            .collect();

        let result = network::DnsLookupResult {
            host: args.host,
            addresses: addrs,
            record_type: args.record_type.unwrap_or_else(|| "A".to_string()),
        };

        Self::result(result, None)
    }
}

impl Default for NetworkExecutor {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ToolExecutor for NetworkExecutor {
    async fn execute(&self, name: &str, args: Value, ctx: &ExecutorContext) -> Result<ToolResult> {
        match name {
            "http_request" => {
                let args: network::HttpRequestArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.http_request(args, ctx).await
            }
            "fetch_url" => {
                let args: network::FetchUrlArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.fetch_url(args, ctx).await
            }
            "port_check" => {
                let args: network::PortCheckArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.port_check(args, ctx).await
            }
            "dns_lookup" => {
                let args: network::DnsLookupArgs = serde_json::from_value(args)
                    .map_err(|e| Error::Tool(format!("Invalid args: {}", e)))?;
                self.dns_lookup(args, ctx).await
            }
            _ => Err(Error::ToolNotFound(name.to_string())),
        }
    }

    fn tools(&self) -> Vec<&'static str> {
        vec!["http_request", "fetch_url", "port_check", "dns_lookup"]
    }

    fn category(&self) -> ToolCategory {
        ToolCategory::Network
    }
}
