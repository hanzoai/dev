use serde::{Deserialize, Serialize};

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
    WaitStrategy::Event("networkidle".to_string())
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