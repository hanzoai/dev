use chrono::Local;
use serde_json::Value;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Mutex;
use uuid::Uuid;

#[derive(Debug)]
struct StreamInfo {
    response_file: PathBuf,
    events: Vec<Value>,
}

#[derive(Debug)]
pub struct DebugLogger {
    enabled: bool,
    log_dir: PathBuf,
    // Maps request_id to stream info for collecting events
    active_streams: Mutex<HashMap<String, StreamInfo>>,
}

impl DebugLogger {
    pub fn new(enabled: bool) -> Result<Self, std::io::Error> {
        if !enabled {
            return Ok(Self {
                enabled: false,
                log_dir: PathBuf::new(),
                active_streams: Mutex::new(HashMap::new()),
            });
        }

        let mut log_dir = crate::config::find_code_home()?;
        log_dir.push("debug_logs");

        fs::create_dir_all(&log_dir)?;

        Ok(Self {
            enabled,
            log_dir,
            active_streams: Mutex::new(HashMap::new()),
        })
    }

    /// Start a new request/response log file and return the request ID
    pub fn start_request_log(
        &self,
        endpoint: &str,
        payload: &Value,
    ) -> Result<String, std::io::Error> {
        if !self.enabled {
            return Ok(String::new());
        }

        let timestamp = Local::now();
        let request_id = Uuid::new_v4().to_string();
        let request_id_short = &request_id[..8]; // Use first 8 chars of UUID for brevity

        // Create request file with pretty-printed JSON
        let request_filename = format!(
            "{}_{}_request.json",
            timestamp.format("%Y%m%d_%H%M%S%.3f"),
            request_id_short
        );
        let request_file_path = self.log_dir.join(request_filename);

        // Create request object with metadata
        let request_entry = serde_json::json!({
            "timestamp": timestamp.to_rfc3339(),
            "request_id": request_id,
            "endpoint": endpoint,
            "payload": payload
        });

        // Write pretty-printed JSON to request file
        let formatted_request = serde_json::to_string_pretty(&request_entry)?;
        fs::write(&request_file_path, formatted_request)?;

        // Prepare response file path
        let response_filename = format!(
            "{}_{}_response.json",
            timestamp.format("%Y%m%d_%H%M%S%.3f"),
            request_id_short
        );
        let response_file_path = self.log_dir.join(response_filename);

        // Store the stream info for this request_id
        if let Ok(mut streams) = self.active_streams.lock() {
            streams.insert(
                request_id.clone(),
                StreamInfo {
                    response_file: response_file_path,
                    events: Vec::new(),
                },
            );
        }

        Ok(request_id)
    }

    /// Append a response event to the in-memory event list
    pub fn append_response_event(
        &self,
        request_id: &str,
        event_type: &str,
        data: &Value,
    ) -> Result<(), std::io::Error> {
        if !self.enabled || request_id.is_empty() {
            return Ok(());
        }

        if let Ok(mut streams) = self.active_streams.lock() {
            if let Some(stream_info) = streams.get_mut(request_id) {
                let timestamp = Local::now();
                let event_entry = serde_json::json!({
                    "timestamp": timestamp.to_rfc3339(),
                    "type": event_type,
                    "data": data
                });
                stream_info.events.push(event_entry);
            }
        }

        Ok(())
    }

    /// Mark a stream as completed and write all collected events to response file
    pub fn end_request_log(&self, request_id: &str) -> Result<(), std::io::Error> {
        if !self.enabled || request_id.is_empty() {
            return Ok(());
        }

        if let Ok(mut streams) = self.active_streams.lock() {
            if let Some(stream_info) = streams.remove(request_id) {
                // Create the response object with all events as an array
                let response_data = serde_json::json!({
                    "request_id": request_id,
                    "completed_at": Local::now().to_rfc3339(),
                    "events": stream_info.events
                });

                // Write pretty-printed JSON to response file
                let formatted_response = serde_json::to_string_pretty(&response_data)?;
                fs::write(&stream_info.response_file, formatted_response)?;
            }
        }

        Ok(())
    }

    // Legacy methods for backward compatibility - they now create standalone files
    pub fn log_request(&self, endpoint: &str, payload: &Value) -> Result<(), std::io::Error> {
        if !self.enabled {
            return Ok(());
        }

        let timestamp = Local::now();
        let filename = format!("{}_request.json", timestamp.format("%Y%m%d_%H%M%S%.3f"));

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
        let filename = format!("{}_response.json", timestamp.format("%Y%m%d_%H%M%S%.3f"));

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
        let filename = format!("{}_stream.txt", timestamp.format("%Y%m%d_%H%M%S%.3f"));

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
        let filename = format!("{}_error.txt", timestamp.format("%Y%m%d_%H%M%S%.3f"));

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

    pub fn log_sse_event(&self, endpoint: &str, event_data: &Value) -> Result<(), std::io::Error> {
        // Legacy method - now creates standalone files
        if !self.enabled {
            return Ok(());
        }

        let timestamp = Local::now();
        let filename = format!("{}_sse.json", timestamp.format("%Y%m%d_%H%M%S%.3f"));

        let file_path = self.log_dir.join(filename);

        let log_entry = serde_json::json!({
            "timestamp": timestamp.to_rfc3339(),
            "type": "sse_event",
            "endpoint": endpoint,
            "event": event_data
        });

        let formatted = serde_json::to_string_pretty(&log_entry)?;
        fs::write(file_path, formatted)?;

        Ok(())
    }
}
