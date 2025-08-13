use crate::BrowserError;
use crate::Result;
use crate::config::BrowserConfig;
use crate::config::ImageFormat;
use crate::config::ViewportConfig;
use crate::config::WaitStrategy;
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
}

impl Page {
    pub fn new(cdp_page: CdpPage, config: BrowserConfig) -> Self {
        Self {
            cdp_page: Arc::new(cdp_page),
            config,
            current_url: Arc::new(RwLock::new(None)),
        }
    }

    /// Helper function to capture screenshot with retry logic.
    /// First tries with from_surface(false) to avoid flashing.
    /// If that fails, retries with from_surface(true) for when window is not visible.
    async fn capture_screenshot_with_retry(
        &self,
        params_builder: chromiumoxide::cdp::browser_protocol::page::CaptureScreenshotParamsBuilder,
    ) -> Result<chromiumoxide::cdp::browser_protocol::page::CaptureScreenshotReturns> {
        // First try with from_surface(false) to avoid flashing
        let params = params_builder.clone().from_surface(false).build();
        
        match self.cdp_page.execute(params).await {
            Ok(resp) => Ok(resp.result),
            Err(e) => {
                // Log the initial failure
                debug!("Screenshot with from_surface(false) failed: {}. Retrying with from_surface(true)...", e);
                
                // Retry with from_surface(true) - this may cause flashing but will work when window is not visible
                let retry_params = params_builder.from_surface(true).build();
                match self.cdp_page.execute(retry_params).await {
                    Ok(resp) => Ok(resp.result),
                    Err(retry_err) => {
                        debug!("Screenshot retry with from_surface(true) also failed: {}", retry_err);
                        Err(retry_err.into())
                    }
                }
            }
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

        let params_builder = CaptureScreenshotParams::builder()
            .format(format)
            .capture_beyond_viewport(true)
            .clip(chromiumoxide::cdp::browser_protocol::page::Viewport {
                x: 0.0,
                y: 0.0,
                width: target_w as f64,
                height: target_h as f64,
                scale: 1.0,
            });

        // Use our retry logic to handle cases where window is not visible
        let resp = self.capture_screenshot_with_retry(params_builder).await?;
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
            let params_builder = CaptureScreenshotParams::builder()
                .format(format.clone())
                .capture_beyond_viewport(true) // key to avoid scrolling/flash
                .clip(chromiumoxide::cdp::browser_protocol::page::Viewport {
                    x: 0.0,
                    y: y as f64,
                    width: vw as f64,
                    height: h as f64,
                    scale: 1.0,
                });

            let resp = self.capture_screenshot_with_retry(params_builder).await?;
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

        let params_builder = CaptureScreenshotParams::builder()
            .format(format)
            .clip(chromiumoxide::cdp::browser_protocol::page::Viewport {
                x: region.x as f64,
                y: region.y as f64,
                width: region.width as f64,
                height: region.height as f64,
                scale: 1.0,
            });

        let resp = self.capture_screenshot_with_retry(params_builder).await?;
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

    pub async fn update_viewport(&self, _viewport: ViewportConfig) -> Result<()> { Ok(()) }

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
        // Replace em dashes with regular dashes
        let processed_text = text.replace('—', " - ");
        debug!("Typing text: {}", processed_text);

        for ch in processed_text.chars() {
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

        // Create the user code with sourceURL for better debugging
        let user_code_with_source = format!("{}\n//# sourceURL=browser_js_user_code.js", code);

        // Use the improved JavaScript harness
        let wrapped = format!(
            r#"(async () => {{
  const __meta = {{ startTs: Date.now(), urlBefore: location.href }};
  const __logs = [];
  const __errs = [];

  const __orig = {{
    log: console.log, warn: console.warn, error: console.error, debug: console.debug
  }};

  function __normalize(v, d = 0) {{
    const MAX_DEPTH = 3, MAX_STR = 4000;
    if (d > MAX_DEPTH) return {{ __type: 'truncated' }};
    if (v === undefined) return {{ __type: 'undefined' }};
    if (v === null || typeof v === 'number' || typeof v === 'boolean') return v;
    if (typeof v === 'string') return v.length > MAX_STR ? v.slice(0, MAX_STR) + '…' : v;
    if (typeof v === 'bigint') return {{ __type: 'bigint', value: v.toString() + 'n' }};
    if (typeof v === 'symbol') return {{ __type: 'symbol', value: String(v) }};
    if (typeof v === 'function') return {{ __type: 'function', name: v.name || '' }};

    if (typeof Element !== 'undefined' && v instanceof Element) {{
      return {{
        __type: 'element',
        tag: v.tagName, id: v.id || null, class: v.className || null,
        text: (v.textContent || '').trim().slice(0, 200)
      }};
    }}
    try {{ return JSON.parse(JSON.stringify(v)); }} catch {{}}

    if (Array.isArray(v)) return v.slice(0, 50).map(x => __normalize(x, d + 1));

    const out = Object.create(null);
    let n = 0;
    for (const k in v) {{
      if (!Object.prototype.hasOwnProperty.call(v, k)) continue;
      out[k] = __normalize(v[k], d + 1);
      if (++n >= 50) {{ out.__truncated = true; break; }}
    }}
    return out;
  }}

  const __push = (level, args) => {{
    __logs.push({{ level, args: args.map(a => __normalize(a)) }});
  }};
  console.log  = (...a) => {{ __push('log', a);  __orig.log(...a);  }};
  console.warn = (...a) => {{ __push('warn', a); __orig.warn(...a); }};
  console.error= (...a) => {{ __push('error',a); __orig.error(...a); }};
  console.debug= (...a) => {{ __push('debug',a); __orig.debug(...a); }};

  window.addEventListener('error', e => {{
    try {{ __errs.push(String(e.error || e.message || e)); }} catch {{ __errs.push('window.error'); }}
  }});
  window.addEventListener('unhandledrejection', e => {{
    try {{ __errs.push('unhandledrejection: ' + String(e.reason)); }} catch {{ __errs.push('unhandledrejection'); }}
  }});

  try {{
    const AsyncFunction = Object.getPrototypeOf(async function(){{}}).constructor;
    const __userCode = {0};
    const evaluator = new AsyncFunction('__code', '"use strict"; return eval(__code);');
    const raw = await evaluator(__userCode);
    const value = (raw === undefined ? null : __normalize(raw));

    return {{
      success: true,
      value,
      logs: __logs,
      errors: __errs,
      meta: {{
        urlBefore: __meta.urlBefore,
        urlAfter: location.href,
        durationMs: Date.now() - __meta.startTs
      }}
    }};
  }} catch (err) {{
    return {{
      success: false,
      value: null,
      error: String(err),
      stack: (err && err.stack) ? String(err.stack) : '',
      logs: __logs,
      errors: __errs
    }};
  }} finally {{
    console.log = __orig.log;
    console.warn = __orig.warn;
    console.error = __orig.error;
    console.debug = __orig.debug;
  }}
}})()"#,
            serde_json::to_string(&user_code_with_source).unwrap()
        );

        tracing::debug!("Executing JavaScript code: {}", code);
        tracing::debug!("Wrapped code: {}", wrapped);

        // Execute the wrapped code - chromiumoxide's evaluate method handles async functions
        let result = self.cdp_page.evaluate(wrapped).await?;
        let result_value = result.value().cloned().unwrap_or(serde_json::Value::Null);

        tracing::info!("JavaScript execution result: {}", result_value);

        // Give a brief moment for potential navigation triggered by the script
        // (e.g., element.click(), location changes) to take effect before
        // downstream consumers capture screenshots.
        tokio::time::sleep(tokio::time::Duration::from_millis(300)).await;

        Ok(result_value)
    }

    /// Scroll the page by the given delta in pixels
    pub async fn scroll_by(&self, dx: f64, dy: f64) -> Result<()> {
        debug!("Scrolling by ({}, {})", dx, dy);
        let js = format!(
            "(function() {{ window.scrollBy({dx}, {dy}); return {{ x: window.scrollX, y: window.scrollY }}; }})()"
        );
        let _ = self.execute_javascript(&js).await?;
        Ok(())
    }

    /// Navigate browser history backward one entry
    pub async fn go_back(&self) -> Result<()> {
        debug!("History back");
        let _ = self.execute_javascript("history.back();").await?;
        Ok(())
    }

    /// Navigate browser history forward one entry
    pub async fn go_forward(&self) -> Result<()> {
        debug!("History forward");
        let _ = self.execute_javascript("history.forward();").await?;
        Ok(())
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
