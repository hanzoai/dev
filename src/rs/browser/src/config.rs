use serde::Deserialize;
use serde::Serialize;
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BrowserConfig {
    #[serde(default)]
    pub enabled: bool,

    #[serde(default = "default_viewport")]
    pub viewport: ViewportConfig,

    #[serde(default = "default_wait")]
    pub wait: WaitStrategy,

    #[serde(default)]
    pub fullpage: bool,

    #[serde(default = "default_segments_max")]
    pub segments_max: usize,

    #[serde(default = "default_idle_timeout_ms")]
    pub idle_timeout_ms: u64,

    #[serde(default = "default_format")]
    pub format: ImageFormat,

    /// Launch Chrome in headless mode. Prefer headed for fewer false positives.
    #[serde(default)]
    pub headless: bool,

    /// Connect to an already-running Chrome DevTools WS endpoint
    /// e.g. ws://127.0.0.1:9222/devtools/browser/XXXXXXXX
    #[serde(default)]
    pub connect_ws: Option<String>,

    /// Or discover the WS endpoint from a --remote-debugging-port (e.g. 9222).
    #[serde(default)]
    pub connect_port: Option<u16>,

    /// Use a persistent profile instead of temp. If set, we won't delete it.
    #[serde(default)]
    pub user_data_dir: Option<PathBuf>,

    /// If true and `user_data_dir` is Some, never delete on drop.
    #[serde(default = "default_persist_profile")]
    pub persist_profile: bool,

    /// "Human" env hints applied via CDP immediately after page creation.
    #[serde(default)]
    pub locale: Option<String>, // e.g. Some("en-AU".into())

    #[serde(default)]
    pub timezone: Option<String>, // e.g. Some("Australia/Brisbane".into())

    #[serde(default)]
    pub accept_language: Option<String>, // e.g. Some("en-AU,en;q=0.9".into())

    #[serde(default)]
    pub user_agent: Option<String>, // leave None to let Chrome decide

    // --- Connection tuning (CDP attach) ---
    /// Optional host to use when connecting to an external Chrome via
    /// `connect_port`. Defaults to 127.0.0.1 when not set.
    #[serde(default)]
    pub connect_host: Option<String>,
    /// Per-attempt timeout for WS connect to Chrome (milliseconds)
    #[serde(default = "default_connect_attempt_timeout_ms")]
    pub connect_attempt_timeout_ms: u64,

    /// Number of WS connect attempts before giving up
    #[serde(default = "default_connect_attempts")]
    pub connect_attempts: u32,
}

impl Default for BrowserConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            viewport: default_viewport(),
            wait: default_wait(),
            fullpage: false,
            segments_max: default_segments_max(),
            idle_timeout_ms: default_idle_timeout_ms(),
            format: default_format(),
            headless: false, // Prefer headed for fewer false positives
            connect_ws: None,
            connect_port: None,
            connect_host: None,
            user_data_dir: None,
            persist_profile: default_persist_profile(),
            locale: Some("en-AU".into()),
            timezone: Some("Australia/Brisbane".into()),
            accept_language: Some("en-AU,en;q=0.9".into()),
            user_agent: None,
            connect_attempt_timeout_ms: default_connect_attempt_timeout_ms(),
            connect_attempts: default_connect_attempts(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ViewportConfig {
    pub width: u32,
    pub height: u32,

    #[serde(default = "default_device_scale_factor")]
    pub device_scale_factor: f64,

    #[serde(default)]
    pub mobile: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum WaitStrategy {
    Event(String),
    Delay { delay_ms: u64 },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ImageFormat {
    Png,
    Webp,
}

fn default_viewport() -> ViewportConfig {
    ViewportConfig {
        width: 1024,
        height: 768,
        device_scale_factor: 1.0,
        mobile: false,
    }
}

fn default_wait() -> WaitStrategy {
    // "load" is more reliable than a fixed 1s "networkidle" sleep in our
    // navigation implementation and better matches SPA hydration delays.
    WaitStrategy::Event("load".to_string())
}

fn default_segments_max() -> usize {
    8
}

fn default_idle_timeout_ms() -> u64 {
    60000
}

fn default_device_scale_factor() -> f64 {
    1.0
}

fn default_format() -> ImageFormat {
    ImageFormat::Png
}

fn default_persist_profile() -> bool {
    true
}

fn default_connect_attempt_timeout_ms() -> u64 {
    3000
}

fn default_connect_attempts() -> u32 {
    3
}
