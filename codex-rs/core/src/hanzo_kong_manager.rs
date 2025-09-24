use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Kong configuration for managing Hanzo services
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KongConfig {
    /// Kong Admin API URL
    pub admin_url: String,
    /// Kong Proxy URL
    pub proxy_url: String,
    /// Kong Admin API key (if required)
    pub admin_api_key: Option<String>,
}

impl Default for KongConfig {
    fn default() -> Self {
        Self {
            admin_url: "http://localhost:8001".to_string(),
            proxy_url: "http://localhost:8000".to_string(),
            admin_api_key: None,
        }
    }
}

/// Service configuration for Kong
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceConfig {
    pub name: String,
    pub host: String,
    pub port: u16,
    pub path: Option<String>,
    pub protocol: String,
    pub retries: u32,
    pub connect_timeout: u32,
    pub write_timeout: u32,
    pub read_timeout: u32,
}

/// Route configuration for Kong
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteConfig {
    pub name: String,
    pub service_id: String,
    pub paths: Vec<String>,
    pub methods: Option<Vec<String>>,
    pub strip_path: bool,
    pub preserve_host: bool,
}

/// Plugin configuration for Kong
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginConfig {
    pub name: String,
    pub service_id: Option<String>,
    pub route_id: Option<String>,
    pub config: serde_json::Value,
    pub enabled: bool,
}

/// Kong Manager for Hanzo services
pub struct KongManager {
    config: KongConfig,
    client: reqwest::Client,
}

impl KongManager {
    pub fn new(config: KongConfig) -> Self {
        let client = reqwest::Client::builder()
            .timeout(std::time::Duration::from_secs(30))
            .build()
            .unwrap_or_default();

        Self { config, client }
    }

    /// Register Hanzo services with Kong
    pub async fn setup_hanzo_services(&self) -> Result<()> {
        // Register inference service
        self.register_inference_service().await?;

        // Register embedding service
        self.register_embedding_service().await?;

        // Register vector service
        self.register_vector_service().await?;

        // Setup plugins
        self.setup_plugins().await?;

        Ok(())
    }

    /// Register inference service
    async fn register_inference_service(&self) -> Result<()> {
        let service = ServiceConfig {
            name: "hanzo-inference".to_string(),
            host: "hanzo-engine".to_string(),
            port: 3690,
            path: Some("/v1".to_string()),
            protocol: "http".to_string(),
            retries: 3,
            connect_timeout: 60000,
            write_timeout: 60000,
            read_timeout: 60000,
        };

        let service_id = self.create_or_update_service(&service).await?;

        // Create routes for inference
        let route = RouteConfig {
            name: "inference-route".to_string(),
            service_id: service_id.clone(),
            paths: vec!["/inference".to_string(), "/v1/chat/completions".to_string()],
            methods: Some(vec!["POST".to_string(), "GET".to_string()]),
            strip_path: false,
            preserve_host: false,
        };

        self.create_or_update_route(&route).await?;

        // Add rate limiting for inference
        self.add_rate_limit(&service_id, Some("inference-rate-limit".to_string()), 100, "minute").await?;

        Ok(())
    }

    /// Register embedding service
    async fn register_embedding_service(&self) -> Result<()> {
        let service = ServiceConfig {
            name: "hanzo-embedding".to_string(),
            host: "hanzo-node".to_string(),
            port: 3691,
            path: Some("/v1".to_string()),
            protocol: "http".to_string(),
            retries: 3,
            connect_timeout: 30000,
            write_timeout: 30000,
            read_timeout: 30000,
        };

        let service_id = self.create_or_update_service(&service).await?;

        // Create routes for embeddings
        let route = RouteConfig {
            name: "embedding-route".to_string(),
            service_id: service_id.clone(),
            paths: vec!["/embeddings".to_string(), "/v1/embeddings".to_string()],
            methods: Some(vec!["POST".to_string()]),
            strip_path: false,
            preserve_host: false,
        };

        self.create_or_update_route(&route).await?;

        // Add caching for embeddings
        self.add_cache(&service_id, Some("embedding-cache".to_string()), 3600).await?;

        Ok(())
    }

    /// Register vector service
    async fn register_vector_service(&self) -> Result<()> {
        let service = ServiceConfig {
            name: "hanzo-vector".to_string(),
            host: "hanzo-node".to_string(),
            port: 3692,
            path: Some("/v1".to_string()),
            protocol: "http".to_string(),
            retries: 2,
            connect_timeout: 30000,
            write_timeout: 30000,
            read_timeout: 30000,
        };

        let service_id = self.create_or_update_service(&service).await?;

        // Create routes for vector operations
        let route = RouteConfig {
            name: "vector-route".to_string(),
            service_id: service_id.clone(),
            paths: vec![
                "/vector/search".to_string(),
                "/v1/vector/search".to_string(),
                "/vector/add".to_string(),
                "/v1/vector/add".to_string(),
                "/vector/delete".to_string(),
                "/v1/vector/delete".to_string(),
            ],
            methods: Some(vec!["POST".to_string(), "DELETE".to_string()]),
            strip_path: false,
            preserve_host: false,
        };

        self.create_or_update_route(&route).await?;

        Ok(())
    }

    /// Setup common plugins
    async fn setup_plugins(&self) -> Result<()> {
        // Add CORS plugin globally
        self.add_cors_plugin().await?;

        // Add authentication plugin
        self.add_auth_plugin().await?;

        // Add logging plugin
        self.add_logging_plugin().await?;

        // Add Prometheus metrics
        self.add_prometheus_plugin().await?;

        Ok(())
    }

    /// Create or update a service in Kong
    async fn create_or_update_service(&self, service: &ServiceConfig) -> Result<String> {
        let url = format!("{}/services/{}", self.config.admin_url, service.name);

        let body = serde_json::json!({
            "name": service.name,
            "host": service.host,
            "port": service.port,
            "path": service.path,
            "protocol": service.protocol,
            "retries": service.retries,
            "connect_timeout": service.connect_timeout,
            "write_timeout": service.write_timeout,
            "read_timeout": service.read_timeout
        });

        let mut req = self.client.put(&url).json(&body);

        if let Some(api_key) = &self.config.admin_api_key {
            req = req.header("apikey", api_key);
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            // Try creating if update failed
            let create_url = format!("{}/services", self.config.admin_url);
            let mut create_req = self.client.post(&create_url).json(&body);

            if let Some(api_key) = &self.config.admin_api_key {
                create_req = create_req.header("apikey", api_key);
            }

            let create_response = create_req.send().await?;

            if !create_response.status().is_success() {
                anyhow::bail!("Failed to create service: {}", create_response.text().await?);
            }

            let json: serde_json::Value = create_response.json().await?;
            return Ok(json["id"].as_str().unwrap_or(&service.name).to_string());
        }

        let json: serde_json::Value = response.json().await?;
        Ok(json["id"].as_str().unwrap_or(&service.name).to_string())
    }

    /// Create or update a route in Kong
    async fn create_or_update_route(&self, route: &RouteConfig) -> Result<String> {
        let url = format!("{}/services/{}/routes", self.config.admin_url, route.service_id);

        let body = serde_json::json!({
            "name": route.name,
            "paths": route.paths,
            "methods": route.methods,
            "strip_path": route.strip_path,
            "preserve_host": route.preserve_host
        });

        let mut req = self.client.post(&url).json(&body);

        if let Some(api_key) = &self.config.admin_api_key {
            req = req.header("apikey", api_key);
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to create route: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        Ok(json["id"].as_str().unwrap_or(&route.name).to_string())
    }

    /// Add rate limiting plugin
    async fn add_rate_limit(
        &self,
        service_id: &str,
        name: Option<String>,
        limit: u32,
        period: &str,
    ) -> Result<()> {
        let plugin = PluginConfig {
            name: "rate-limiting".to_string(),
            service_id: Some(service_id.to_string()),
            route_id: None,
            config: serde_json::json!({
                "minute": if period == "minute" { Some(limit) } else { None },
                "hour": if period == "hour" { Some(limit) } else { None },
                "day": if period == "day" { Some(limit) } else { None },
                "policy": "local"
            }),
            enabled: true,
        };

        self.add_plugin(&plugin).await
    }

    /// Add caching plugin
    async fn add_cache(&self, service_id: &str, name: Option<String>, ttl: u32) -> Result<()> {
        let plugin = PluginConfig {
            name: "proxy-cache".to_string(),
            service_id: Some(service_id.to_string()),
            route_id: None,
            config: serde_json::json!({
                "strategy": "memory",
                "cache_ttl": ttl,
                "cache_control": true,
                "content_type": ["application/json"]
            }),
            enabled: true,
        };

        self.add_plugin(&plugin).await
    }

    /// Add CORS plugin
    async fn add_cors_plugin(&self) -> Result<()> {
        let plugin = PluginConfig {
            name: "cors".to_string(),
            service_id: None,
            route_id: None,
            config: serde_json::json!({
                "origins": ["*"],
                "methods": ["GET", "POST", "PUT", "DELETE", "OPTIONS", "HEAD", "PATCH"],
                "headers": ["Accept", "Accept-Version", "Content-Length", "Content-MD5", "Content-Type", "Date", "X-Auth-Token", "Authorization"],
                "exposed_headers": ["X-Auth-Token"],
                "credentials": true,
                "max_age": 3600
            }),
            enabled: true,
        };

        self.add_plugin(&plugin).await
    }

    /// Add authentication plugin (key-auth)
    async fn add_auth_plugin(&self) -> Result<()> {
        let plugin = PluginConfig {
            name: "key-auth".to_string(),
            service_id: None,
            route_id: None,
            config: serde_json::json!({
                "key_names": ["api-key", "apikey", "x-api-key"],
                "key_in_body": false,
                "key_in_header": true,
                "key_in_query": true,
                "hide_credentials": false,
                "run_on_preflight": false
            }),
            enabled: true,
        };

        self.add_plugin(&plugin).await
    }

    /// Add logging plugin
    async fn add_logging_plugin(&self) -> Result<()> {
        let plugin = PluginConfig {
            name: "file-log".to_string(),
            service_id: None,
            route_id: None,
            config: serde_json::json!({
                "path": "/var/log/kong/hanzo-access.log",
                "reopen": true
            }),
            enabled: true,
        };

        self.add_plugin(&plugin).await
    }

    /// Add Prometheus metrics plugin
    async fn add_prometheus_plugin(&self) -> Result<()> {
        let plugin = PluginConfig {
            name: "prometheus".to_string(),
            service_id: None,
            route_id: None,
            config: serde_json::json!({}),
            enabled: true,
        };

        self.add_plugin(&plugin).await
    }

    /// Add a plugin to Kong
    async fn add_plugin(&self, plugin: &PluginConfig) -> Result<()> {
        let url = format!("{}/plugins", self.config.admin_url);

        let mut body = serde_json::json!({
            "name": plugin.name,
            "config": plugin.config,
            "enabled": plugin.enabled
        });

        if let Some(service_id) = &plugin.service_id {
            body["service"] = serde_json::json!({"id": service_id});
        }

        if let Some(route_id) = &plugin.route_id {
            body["route"] = serde_json::json!({"id": route_id});
        }

        let mut req = self.client.post(&url).json(&body);

        if let Some(api_key) = &self.config.admin_api_key {
            req = req.header("apikey", api_key);
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to add plugin {}: {}", plugin.name, response.text().await?);
        }

        Ok(())
    }

    /// Create a consumer
    pub async fn create_consumer(&self, username: &str) -> Result<String> {
        let url = format!("{}/consumers", self.config.admin_url);

        let body = serde_json::json!({
            "username": username
        });

        let mut req = self.client.post(&url).json(&body);

        if let Some(api_key) = &self.config.admin_api_key {
            req = req.header("apikey", api_key);
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to create consumer: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        Ok(json["id"].as_str().unwrap_or(username).to_string())
    }

    /// Create API key for consumer
    pub async fn create_api_key(&self, consumer_id: &str) -> Result<String> {
        let url = format!("{}/consumers/{}/key-auth", self.config.admin_url, consumer_id);

        let body = serde_json::json!({});

        let mut req = self.client.post(&url).json(&body);

        if let Some(api_key) = &self.config.admin_api_key {
            req = req.header("apikey", api_key);
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to create API key: {}", response.text().await?);
        }

        let json: serde_json::Value = response.json().await?;
        Ok(json["key"].as_str().unwrap_or("").to_string())
    }

    /// Health check for Kong
    pub async fn health_check(&self) -> Result<bool> {
        let url = format!("{}/status", self.config.admin_url);

        let mut req = self.client.get(&url);

        if let Some(api_key) = &self.config.admin_api_key {
            req = req.header("apikey", api_key);
        }

        let response = req.send().await?;
        Ok(response.status().is_success())
    }

    /// Get service statistics
    pub async fn get_service_stats(&self, service_name: &str) -> Result<serde_json::Value> {
        let url = format!("{}/services/{}", self.config.admin_url, service_name);

        let mut req = self.client.get(&url);

        if let Some(api_key) = &self.config.admin_api_key {
            req = req.header("apikey", api_key);
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to get service stats: {}", response.text().await?);
        }

        Ok(response.json().await?)
    }

    /// Remove a service
    pub async fn remove_service(&self, service_name: &str) -> Result<()> {
        let url = format!("{}/services/{}", self.config.admin_url, service_name);

        let mut req = self.client.delete(&url);

        if let Some(api_key) = &self.config.admin_api_key {
            req = req.header("apikey", api_key);
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to remove service: {}", response.text().await?);
        }

        Ok(())
    }
}

/// Kong setup script for Docker
pub fn generate_kong_docker_compose() -> String {
    r#"version: '3.7'

services:
  kong-database:
    image: postgres:13
    environment:
      POSTGRES_USER: kong
      POSTGRES_DB: kong
      POSTGRES_PASSWORD: kongpass
    volumes:
      - kong-db-data:/var/lib/postgresql/data
    networks:
      - hanzo-network

  kong-migration:
    image: kong:3.4
    command: kong migrations bootstrap
    environment:
      KONG_DATABASE: postgres
      KONG_PG_HOST: kong-database
      KONG_PG_USER: kong
      KONG_PG_PASSWORD: kongpass
      KONG_PG_DATABASE: kong
    networks:
      - hanzo-network
    depends_on:
      - kong-database

  kong:
    image: kong:3.4
    environment:
      KONG_DATABASE: postgres
      KONG_PG_HOST: kong-database
      KONG_PG_USER: kong
      KONG_PG_PASSWORD: kongpass
      KONG_PG_DATABASE: kong
      KONG_PROXY_ACCESS_LOG: /dev/stdout
      KONG_ADMIN_ACCESS_LOG: /dev/stdout
      KONG_PROXY_ERROR_LOG: /dev/stderr
      KONG_ADMIN_ERROR_LOG: /dev/stderr
      KONG_ADMIN_LISTEN: 0.0.0.0:8001
      KONG_PROXY_LISTEN: 0.0.0.0:8000
      KONG_PLUGINS: bundled,prometheus
    ports:
      - "8000:8000"  # Proxy port
      - "8001:8001"  # Admin API port
      - "8443:8443"  # Proxy SSL port
      - "8444:8444"  # Admin API SSL port
    networks:
      - hanzo-network
    depends_on:
      - kong-migration

  hanzo-engine:
    image: hanzo/engine:latest
    ports:
      - "3690:3690"
    networks:
      - hanzo-network
    volumes:
      - ./models:/models
    environment:
      MODEL_PATH: /models

  hanzo-node:
    image: hanzo/node:latest
    ports:
      - "3691:3691"  # Embeddings
      - "3692:3692"  # Vector search
    networks:
      - hanzo-network
    volumes:
      - ./data:/data
    environment:
      DATA_PATH: /data

networks:
  hanzo-network:
    driver: bridge

volumes:
  kong-db-data:
"#.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_kong_config() {
        let config = KongConfig::default();
        assert_eq!(config.admin_url, "http://localhost:8001");
        assert_eq!(config.proxy_url, "http://localhost:8000");
        assert!(config.admin_api_key.is_none());
    }

    #[test]
    fn test_generate_docker_compose() {
        let compose = generate_kong_docker_compose();
        assert!(compose.contains("kong:3.4"));
        assert!(compose.contains("hanzo-engine"));
        assert!(compose.contains("hanzo-node"));
        assert!(compose.contains("8000:8000"));
        assert!(compose.contains("8001:8001"));
    }
}