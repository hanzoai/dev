use std::fs;
use std::path::PathBuf;
use chrono::Local;
use serde_json::Value;

pub struct DebugLogger {
    enabled: bool,
    log_dir: PathBuf,
}

impl DebugLogger {
    pub fn new(enabled: bool) -> Result<Self, std::io::Error> {
        if !enabled {
            return Ok(Self {
                enabled: false,
                log_dir: PathBuf::new(),
            });
        }

        let log_dir = dirs::home_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join(".codex")
            .join("debug_logs");

        fs::create_dir_all(&log_dir)?;

        Ok(Self {
            enabled,
            log_dir,
        })
    }

    pub fn log_request(&self, endpoint: &str, payload: &Value) -> Result<(), std::io::Error> {
        if !self.enabled {
            return Ok(());
        }

        let timestamp = Local::now();
        let filename = format!(
            "{}_request_{}.json",
            timestamp.format("%Y%m%d_%H%M%S%.3f"),
            sanitize_endpoint(endpoint)
        );

        let file_path = self.log_dir.join(filename);
        
        let log_entry = serde_json::json!({
            "timestamp": timestamp.to_rfc3339(),
            "type": "request",
            "endpoint": endpoint,
            "payload": payload
        });

        let formatted = serde_json::to_string_pretty(&log_entry)?;
        fs::write(file_path, formatted)?;

        Ok(())
    }

    pub fn log_response(&self, endpoint: &str, response: &Value) -> Result<(), std::io::Error> {
        if !self.enabled {
            return Ok(());
        }

        let timestamp = Local::now();
        let filename = format!(
            "{}_response_{}.json",
            timestamp.format("%Y%m%d_%H%M%S%.3f"),
            sanitize_endpoint(endpoint)
        );

        let file_path = self.log_dir.join(filename);
        
        let log_entry = serde_json::json!({
            "timestamp": timestamp.to_rfc3339(),
            "type": "response", 
            "endpoint": endpoint,
            "response": response
        });

        let formatted = serde_json::to_string_pretty(&log_entry)?;
        fs::write(file_path, formatted)?;

        Ok(())
    }

    pub fn log_stream_chunk(&self, endpoint: &str, chunk: &str) -> Result<(), std::io::Error> {
        if !self.enabled {
            return Ok(());
        }

        let timestamp = Local::now();
        let filename = format!(
            "{}_stream_{}.txt",
            timestamp.format("%Y%m%d_%H%M%S%.3f"),
            sanitize_endpoint(endpoint)
        );

        let file_path = self.log_dir.join(filename);
        
        let log_entry = format!(
            "=== Stream Chunk at {} ===\nEndpoint: {}\n\n{}\n",
            timestamp.to_rfc3339(),
            endpoint,
            chunk
        );

        fs::write(file_path, log_entry)?;

        Ok(())
    }

    pub fn log_error(&self, endpoint: &str, error: &str) -> Result<(), std::io::Error> {
        if !self.enabled {
            return Ok(());
        }

        let timestamp = Local::now();
        let filename = format!(
            "{}_error_{}.txt",
            timestamp.format("%Y%m%d_%H%M%S%.3f"),
            sanitize_endpoint(endpoint)
        );

        let file_path = self.log_dir.join(filename);
        
        let log_entry = format!(
            "=== Error at {} ===\nEndpoint: {}\n\n{}\n",
            timestamp.to_rfc3339(),
            endpoint,
            error
        );

        fs::write(file_path, log_entry)?;

        Ok(())
    }

    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}

fn sanitize_endpoint(endpoint: &str) -> String {
    endpoint
        .replace('/', "_")
        .replace('\\', "_")
        .replace(':', "_")
        .replace(' ', "_")
        .chars()
        .filter(|c| c.is_alphanumeric() || *c == '_' || *c == '-')
        .collect()
}