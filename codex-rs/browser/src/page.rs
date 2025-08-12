use crate::BrowserError;
use crate::Result;
use crate::config::BrowserConfig;
use crate::config::ImageFormat;
use crate::config::ViewportConfig;
use crate::config::WaitStrategy;
use chromiumoxide::cdp::browser_protocol::emulation::SetDeviceMetricsOverrideParams;
use chromiumoxide::cdp::browser_protocol::input::DispatchKeyEventParams;
use chromiumoxide::cdp::browser_protocol::input::DispatchKeyEventType;
use chromiumoxide::cdp::browser_protocol::input::DispatchMouseEventParams;
use chromiumoxide::cdp::browser_protocol::input::DispatchMouseEventType;
use chromiumoxide::cdp::browser_protocol::page::CaptureScreenshotFormat;
use chromiumoxide::cdp::browser_protocol::page::CaptureScreenshotParams;
use base64::Engine as _;
use chromiumoxide::page::Page as CdpPage;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::debug;
use tracing::info;

pub struct Page {
    cdp_page: Arc<CdpPage>,
    config: BrowserConfig,
    current_url: Arc<RwLock<Option<String>>>,
    // Cache of the last viewport metrics we explicitly enforced to avoid
    // re-applying the same device metrics (which can cause flicker/focus).
    last_enforced_viewport: Arc<RwLock<Option<(u32, u32, f64, bool)>>>,
}

impl Page {
    pub fn new(cdp_page: CdpPage, config: BrowserConfig) -> Self {
        Self {
            cdp_page: Arc::new(cdp_page),
            config,
            current_url: Arc::new(RwLock::new(None)),
            last_enforced_viewport: Arc::new(RwLock::new(None)),
        }
    }

    /// Returns the current page title, if available.
    pub async fn get_title(&self) -> Option<String> {
        self.cdp_page.get_title().await.ok().flatten()
    }

    pub async fn goto(&self, url: &str, wait: Option<WaitStrategy>) -> Result<GotoResult> {
        info!("Navigating to {}", url);

        let wait_strategy = wait.unwrap_or_else(|| self.config.wait.clone());

        // Navigate to the URL
        self.cdp_page.goto(url).await?;

        // Wait according to the strategy
        match wait_strategy {
            WaitStrategy::Event(event) => match event.as_str() {
                "domcontentloaded" => {
                    // Wait for DOMContentLoaded event
                    self.cdp_page.wait_for_navigation().await?;
                }
                "networkidle" | "networkidle0" => {
                    // Wait for network to be idle
                    tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
                }
                "networkidle2" => {
                    // Wait for network to be mostly idle
                    tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                }
                "load" => {
                    // Wait for load event
                    self.cdp_page.wait_for_navigation().await?;
                    // Add extra delay to ensure page is fully loaded
                    tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                }
                _ => {
                    return Err(BrowserError::ConfigError(format!(
                        "Unknown wait event: {}",
                        event
                    )));
                }
            },
            WaitStrategy::Delay { delay_ms } => {
                tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;
            }
        }

        // Get the final URL and title after navigation completes
        let title = self.cdp_page.get_title().await.ok().flatten();

        // Try to get the URL multiple times in case it's not immediately available
        let mut final_url = None;
        for _ in 0..3 {
            if let Ok(Some(url)) = self.cdp_page.url().await {
                final_url = Some(url);
                break;
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        }

        let final_url = final_url.unwrap_or_else(|| url.to_string());

        let mut current_url = self.current_url.write().await;
        *current_url = Some(final_url.clone());

        Ok(GotoResult {
            url: final_url,
            title,
        })
    }

    pub async fn screenshot(&self, mode: ScreenshotMode) -> Result<Vec<Screenshot>> {
        match mode {
            ScreenshotMode::Viewport => self.screenshot_viewport().await,
            ScreenshotMode::FullPage { segments_max } => {
                self.screenshot_fullpage(segments_max.unwrap_or(self.config.segments_max))
                    .await
            }
            ScreenshotMode::Region(region) => self.screenshot_region(region).await,
        }
    }

    async fn ensure_viewport_if_needed(&self) -> Result<()> {
        // Compare the current effective viewport to the desired. Only call
        // setDeviceMetricsOverride when strictly necessary to avoid flicker
        // and focus-stealing in headed Chrome.
        let desired = &self.config.viewport;

        // Use clientWidth/Height for a more stable reading vs inner{Width,Height}
        let probe = self
            .inject_js(
                "(() => ({ w: (document.documentElement.clientWidth|0), h: (document.documentElement.clientHeight|0), dpr: (window.devicePixelRatio||1) }))()",
            )
            .await
            .ok();

        let mut need_resize = false;
        if let Some(val) = probe {
            let w = val.get("w").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
            let h = val.get("h").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
            let dpr = val.get("dpr").and_then(|v| v.as_f64()).unwrap_or(1.0);
            let dpr_rounded = (dpr * 100.0).round() / 100.0;
            let desired_dpr_rounded = (desired.device_scale_factor * 100.0).round() / 100.0;

            let dims_mismatch = w != desired.width || h != desired.height;
            // Be lenient for DPR to avoid churn due to tiny rounding diffs
            let dpr_mismatch = (dpr_rounded - desired_dpr_rounded).abs() > 0.25;

            need_resize = dims_mismatch || dpr_mismatch;
        } else {
            // If probing fails, don't attempt to resize.
            need_resize = false;
        }

        if !need_resize {
            return Ok(());
        }

        // Skip if we already enforced these exact metrics previously for this page
        let last = { self.last_enforced_viewport.read().await.clone() };
        let current_target = (
            desired.width,
            desired.height,
            desired.device_scale_factor,
            desired.mobile,
        );
        if last.as_ref() == Some(&current_target) {
            return Ok(());
        }

        self.update_viewport(desired.clone()).await?;
        let mut guard = self.last_enforced_viewport.write().await;
        *guard = Some(current_target);
        Ok(())
    }

    pub async fn screenshot_viewport(&self) -> Result<Vec<Screenshot>> {
        // Safe viewport capture: do not change device metrics or viewport.
        // Measure CSS viewport size via JS and capture a clipped image
        // using the compositor without affecting focus.
        debug!("Taking viewport screenshot (safe clip, no resize)");

        let format = match self.config.format {
            ImageFormat::Png => CaptureScreenshotFormat::Png,
            ImageFormat::Webp => CaptureScreenshotFormat::Webp,
        };

        // Probe CSS viewport using Runtime.evaluate to avoid layout_metrics
        let probe = self
            .inject_js(
                "(() => ({ w: (document.documentElement.clientWidth|0), h: (document.documentElement.clientHeight|0) }))()",
            )
            .await
            .unwrap_or(serde_json::Value::Null);

        let doc_w = probe
            .get("w")
            .and_then(|v| v.as_u64())
            .unwrap_or(0) as u32;
        let doc_h = probe
            .get("h")
            .and_then(|v| v.as_u64())
            .unwrap_or(0) as u32;

        // Fall back to configured viewport if probe failed
        let vw = if doc_w > 0 { doc_w } else { self.config.viewport.width };
        let vh = if doc_h > 0 { doc_h } else { self.config.viewport.height };

        // Clamp to configured maximums to keep images small for the LLM
        let target_w = vw.min(self.config.viewport.width);
        let target_h = vh.min(self.config.viewport.height);

        let params = CaptureScreenshotParams::builder()
            .format(format)
            .from_surface(false)
            .capture_beyond_viewport(true)
            .clip(chromiumoxide::cdp::browser_protocol::page::Viewport {
                x: 0.0,
                y: 0.0,
                width: target_w as f64,
                height: target_h as f64,
                scale: 1.0,
            })
            .build();

        // Use raw execute to avoid any helper that might front the tab
        let resp = self.cdp_page.execute(params).await?;
        let data_b64: &str = resp.data.as_ref();
        let data = base64::engine::general_purpose::STANDARD
            .decode(data_b64.as_bytes())
            .map_err(|e| BrowserError::ScreenshotError(format!("base64 decode failed: {}", e)))?;

        Ok(vec![Screenshot {
            data,
            width: target_w,
            height: target_h,
            format: self.config.format,
        }])
    }

    pub async fn screenshot_fullpage(&self, segments_max: usize) -> Result<Vec<Screenshot>> {
        let format = match self.config.format {
            ImageFormat::Png => CaptureScreenshotFormat::Png,
            ImageFormat::Webp => CaptureScreenshotFormat::Webp,
        };

        // 1) Get document dimensions (CSS px)
        let lm = self.cdp_page.layout_metrics().await?;
        let content = lm.css_content_size; // Rect (not Option)
        let doc_w = content.width.ceil() as u32;
        let doc_h = content.height.ceil() as u32;

        // Use your configured viewport width, but never exceed doc width
        let vw = self.config.viewport.width.min(doc_w);
        let vh = self.config.viewport.height;

        // 2) Slice the page by y-offsets WITHOUT scrolling the page
        let mut shots = Vec::new();
        let mut y: u32 = 0;
        let mut taken = 0usize;

        while y < doc_h && taken < segments_max {
            let h = vh.min(doc_h - y); // last slice may be shorter
            let params = CaptureScreenshotParams::builder()
                .format(format.clone())
                .from_surface(false)
                .capture_beyond_viewport(true) // key to avoid scrolling/flash
                .clip(chromiumoxide::cdp::browser_protocol::page::Viewport {
                    x: 0.0,
                    y: y as f64,
                    width: vw as f64,
                    height: h as f64,
                    scale: 1.0,
                })
                .build();

            let resp = self.cdp_page.execute(params).await?;
            let data_b64: &str = resp.data.as_ref();
            let data = base64::engine::general_purpose::STANDARD
                .decode(data_b64.as_bytes())
                .map_err(|e| BrowserError::ScreenshotError(format!("base64 decode failed: {}", e)))?;
            shots.push(Screenshot {
                data,
                width: vw,
                height: h,
                format: self.config.format,
            });

            y += h;
            taken += 1;
        }

        if taken == segments_max && y < doc_h {
            info!("[full page truncated at {} segments]", segments_max);
        }

        Ok(shots)
    }

    pub async fn screenshot_region(&self, region: ScreenshotRegion) -> Result<Vec<Screenshot>> {
        debug!(
            "Taking region screenshot: {}x{} at ({}, {})",
            region.width, region.height, region.x, region.y
        );

        let format = match self.config.format {
            ImageFormat::Png => CaptureScreenshotFormat::Png,
            ImageFormat::Webp => CaptureScreenshotFormat::Webp,
        };

        let params = CaptureScreenshotParams::builder()
            .format(format)
            .from_surface(false)
            .clip(chromiumoxide::cdp::browser_protocol::page::Viewport {
                x: region.x as f64,
                y: region.y as f64,
                width: region.width as f64,
                height: region.height as f64,
                scale: 1.0,
            })
            .build();

        let resp = self.cdp_page.execute(params).await?;
        let data_b64: &str = resp.data.as_ref();
        let data = base64::engine::general_purpose::STANDARD
            .decode(data_b64.as_bytes())
            .map_err(|e| BrowserError::ScreenshotError(format!("base64 decode failed: {}", e)))?;

        let final_width = if region.width > 1024 {
            1024
        } else {
            region.width
        };

        Ok(vec![Screenshot {
            data,
            width: final_width,
            height: region.height,
            format: self.config.format,
        }])
    }

    pub async fn set_viewport(&self, viewport: SetViewportParams) -> Result<ViewportResult> {
        let params = SetDeviceMetricsOverrideParams::builder()
            .width(viewport.width as i64)
            .height(viewport.height as i64)
            .device_scale_factor(viewport.device_scale_factor.unwrap_or(1.0))
            .mobile(viewport.mobile.unwrap_or(false))
            .build()
            .map_err(|e| BrowserError::CdpError(e))?;

        //self.cdp_page.execute(params).await?;

        Ok(ViewportResult {
            width: viewport.width,
            height: viewport.height,
            dpr: viewport.device_scale_factor.unwrap_or(1.0),
        })
    }

    pub async fn inject_js(&self, script: &str) -> Result<serde_json::Value> {
        let result = self.cdp_page.evaluate(script).await?;
        Ok(result.value().cloned().unwrap_or(serde_json::Value::Null))
    }

    pub async fn close(&self) -> Result<()> {
        // Note: chromiumoxide's close() takes ownership, so we can't call it on Arc<Page>
        // The page will be closed when the Arc is dropped
        Ok(())
    }

    pub async fn get_url(&self) -> Result<String> {
        let url_guard = self.current_url.read().await;
        url_guard.clone().ok_or(BrowserError::PageNotLoaded)
    }

    /// Get the current URL directly from the browser (not cached)
    pub async fn get_current_url(&self) -> Result<String> {
        match self.cdp_page.url().await? {
            Some(url) => Ok(url),
            None => Err(BrowserError::PageNotLoaded),
        }
    }

    pub async fn update_viewport(&self, viewport: ViewportConfig) -> Result<()> {
        let params = SetDeviceMetricsOverrideParams::builder()
            .width(viewport.width as i64)
            .height(viewport.height as i64)
            .device_scale_factor(viewport.device_scale_factor)
            .mobile(viewport.mobile)
            .build()
            .map_err(|e| BrowserError::CdpError(e))?;

        //self.cdp_page.execute(params).await?;
        Ok(())
    }

    /// Click at the specified coordinates
    pub async fn click(&self, x: f64, y: f64) -> Result<()> {
        debug!("Clicking at ({}, {})", x, y);

        // Move to position
        let move_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MouseMoved)
            .x(x)
            .y(y)
            .build()
            .map_err(|e| BrowserError::CdpError(e))?;
        self.cdp_page.execute(move_params).await?;

        // Mouse down
        let down_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MousePressed)
            .x(x)
            .y(y)
            .button(chromiumoxide::cdp::browser_protocol::input::MouseButton::Left)
            .click_count(1)
            .build()
            .map_err(|e| BrowserError::CdpError(e))?;
        self.cdp_page.execute(down_params).await?;

        // Mouse up
        let up_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MouseReleased)
            .x(x)
            .y(y)
            .button(chromiumoxide::cdp::browser_protocol::input::MouseButton::Left)
            .click_count(1)
            .build()
            .map_err(|e| BrowserError::CdpError(e))?;
        self.cdp_page.execute(up_params).await?;

        Ok(())
    }

    /// Type text into the currently focused element
    pub async fn type_text(&self, text: &str) -> Result<()> {
        debug!("Typing text: {}", text);

        for ch in text.chars() {
            let params = DispatchKeyEventParams::builder()
                .r#type(DispatchKeyEventType::Char)
                .text(ch.to_string())
                .build()
                .map_err(|e| BrowserError::CdpError(e))?;
            self.cdp_page.execute(params).await?;
        }

        Ok(())
    }

    /// Press a key (e.g., "Enter", "Tab", "Escape", "ArrowDown")
    pub async fn press_key(&self, key: &str) -> Result<()> {
        debug!("Pressing key: {}", key);

        // Key down
        let down_params = DispatchKeyEventParams::builder()
            .r#type(DispatchKeyEventType::KeyDown)
            .key(key.to_string())
            .build()
            .map_err(|e| BrowserError::CdpError(e))?;
        self.cdp_page.execute(down_params).await?;

        // Key up
        let up_params = DispatchKeyEventParams::builder()
            .r#type(DispatchKeyEventType::KeyUp)
            .key(key.to_string())
            .build()
            .map_err(|e| BrowserError::CdpError(e))?;
        self.cdp_page.execute(up_params).await?;

        Ok(())
    }

    /// Execute JavaScript code with enhanced return value handling
    pub async fn execute_javascript(&self, code: &str) -> Result<serde_json::Value> {
        debug!(
            "Executing JavaScript: {}...",
            &code.chars().take(100).collect::<String>()
        );

        // Helper to check if a line looks like an expression that should be returned
        let is_expression_line = |line: &str| -> bool {
            let line = line.trim();
            if line.is_empty() {
                return false;
            }

            // Skip lines that are already returns or start with control keywords
            let control_keywords = [
                "return", "const", "let", "var", "class", "function", "async", "if", "for",
                "while", "switch", "try", "catch", "finally", "throw", "import", "export", "yield",
                "await", "break", "continue", "debugger",
            ];

            for keyword in &control_keywords {
                if line.starts_with(keyword)
                    && (line.len() == keyword.len()
                        || line
                            .chars()
                            .nth(keyword.len())
                            .map_or(false, |c| !c.is_alphanumeric()))
                {
                    return false;
                }
            }

            // Skip lines that end with block closures
            if line.starts_with('}') {
                return false;
            }

            true
        };

        // Analyze the code
        let lines: Vec<&str> = code.split('\n').collect();
        let mut last_expr_line = None;
        let mut has_explicit_return = false;

        // Scan for significant lines from the end
        for (i, line) in lines.iter().enumerate().rev() {
            let line = line.trim();
            if line.is_empty() || line.starts_with("//") {
                continue;
            }

            if line.starts_with("return") {
                has_explicit_return = true;
                break;
            }

            if last_expr_line.is_none() && is_expression_line(line) {
                last_expr_line = Some(i);
                if !line.ends_with(';') && !line.ends_with('}') {
                    break;
                }
            }
        }

        // Build the function body
        let body = if has_explicit_return {
            code.to_string()
        } else if let Some(expr_line_idx) = last_expr_line {
            let prefix = lines[..expr_line_idx].join("\n");
            let expr_line = lines[expr_line_idx].trim();
            let suffix = lines[expr_line_idx + 1..].join("\n");

            // Handle object literals and array literals - wrap in parentheses
            let wrapped_expr = if expr_line.starts_with('{') || expr_line.starts_with('[') {
                format!("({})", expr_line)
            } else {
                expr_line.to_string()
            };

            format!("{}\nreturn {};\n{}", prefix, wrapped_expr, suffix)
        } else {
            // Try to capture the last variable defined
            let var_regex =
                regex::Regex::new(r"(?:const|let|var)\s+([A-Za-z_$][\w$]*)\s*=").unwrap();
            if let Some(captures) = var_regex.captures(code) {
                if let Some(var_name) = captures.get(1) {
                    format!(
                        "{}; return typeof {} !== 'undefined' ? {} : undefined;",
                        code,
                        var_name.as_str(),
                        var_name.as_str()
                    )
                } else {
                    code.to_string()
                }
            } else {
                code.to_string()
            }
        };

        // Wrap in async IIFE with error handling and console.log capture
        let wrapped = format!(
            r#"
(async () => {{
    const __logs = [];
    const __origLog = console.log;
    
    function safe(arg) {{
        if (arg === null || typeof arg !== 'object') return String(arg);
        try {{ return JSON.stringify(arg); }} catch {{ return '[Circular]'; }}
    }}
    
    console.log = function(...args) {{
        __logs.push(args.map(safe).join(' '));
        __origLog.apply(console, args);
    }};
    
    try {{
        const AsyncFunction = Object.getPrototypeOf(async function(){{}}).constructor;
        const __userCode = {};
        const fn = new AsyncFunction(__userCode);
        const value = await fn();
        return {{
            success: true,
            value: (value === undefined && __logs.length > 0) ? __logs[__logs.length-1] : value,
            logs: __logs
        }};
    }} catch (err) {{
        return {{
            success: false,
            error: err.toString() + (err.stack ? '\n' + err.stack : ''),
            logs: __logs
        }};
    }} finally {{
        console.log = __origLog;
    }}
}})()
"#,
            serde_json::to_string(&body).unwrap()
        );

        tracing::debug!("Executing JavaScript code: {}", code);
        tracing::debug!("Wrapped code: {}", wrapped);

        let result = self.cdp_page.evaluate(wrapped).await?;
        let result_value = result.value().cloned().unwrap_or(serde_json::Value::Null);

        tracing::info!("JavaScript execution result: {}", result_value);

        Ok(result_value)
    }
}

#[derive(Debug, Clone)]
pub enum ScreenshotMode {
    Viewport,
    FullPage { segments_max: Option<usize> },
    Region(ScreenshotRegion),
}

#[derive(Debug, Clone)]
pub struct ScreenshotRegion {
    pub x: u32,
    pub y: u32,
    pub width: u32,
    pub height: u32,
}

#[derive(Debug)]
pub struct Screenshot {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
    pub format: ImageFormat,
}

#[derive(Debug, serde::Serialize)]
pub struct GotoResult {
    pub url: String,
    pub title: Option<String>,
}

#[derive(Debug, serde::Deserialize)]
pub struct SetViewportParams {
    pub width: u32,
    pub height: u32,
    pub device_scale_factor: Option<f64>,
    pub mobile: Option<bool>,
}

#[derive(Debug, serde::Serialize)]
pub struct ViewportResult {
    pub width: u32,
    pub height: u32,
    pub dpr: f64,
}
