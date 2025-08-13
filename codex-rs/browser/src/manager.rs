use crate::BrowserError;
use crate::Result;
use crate::config::BrowserConfig;
use crate::page::Page;
use chromiumoxide::Browser;
use chromiumoxide::BrowserConfig as CdpConfig;
use chromiumoxide::browser::HeadlessMode;
use chromiumoxide::cdp::browser_protocol::emulation;
use chromiumoxide::cdp::browser_protocol::network;
use futures::StreamExt;
use reqwest::Client;
use serde::Deserialize;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::sync::RwLock;
use tokio::time::Duration;
use tokio::time::Instant;
use tokio::time::sleep;
use tracing::debug;
use tracing::info;
use tracing::warn;

#[derive(Deserialize)]
struct JsonVersion {
    #[serde(rename = "webSocketDebuggerUrl")]
    web_socket_debugger_url: String,
}

async fn discover_ws_via_port(port: u16) -> Result<String> {
    let url = format!("http://127.0.0.1:{}/json/version", port);
    debug!("Requesting Chrome version info from: {}", url);

    let client_start = tokio::time::Instant::now();
    let client = Client::builder()
        .timeout(Duration::from_secs(5)) // Add timeout to prevent hanging
        .build()
        .map_err(|e| BrowserError::CdpError(format!("Failed to build HTTP client: {}", e)))?;
    debug!("HTTP client created in {:?}", client_start.elapsed());

    let req_start = tokio::time::Instant::now();
    let resp = client.get(&url).send().await.map_err(|e| {
        BrowserError::CdpError(format!("Failed to connect to Chrome debug port: {}", e))
    })?;
    debug!(
        "HTTP request completed in {:?}, status: {}",
        req_start.elapsed(),
        resp.status()
    );

    if !resp.status().is_success() {
        return Err(BrowserError::CdpError(format!(
            "Chrome /json/version returned {}",
            resp.status()
        )));
    }

    let parse_start = tokio::time::Instant::now();
    let body: JsonVersion = resp.json().await.map_err(|e| {
        BrowserError::CdpError(format!("Failed to parse Chrome debug response: {}", e))
    })?;
    debug!("Response parsed in {:?}", parse_start.elapsed());

    Ok(body.web_socket_debugger_url)
}

/// Scan for Chrome processes with debug ports and verify accessibility
async fn scan_for_chrome_debug_port() -> Option<u16> {
    use std::process::Command;

    // Use ps to find Chrome processes with remote-debugging-port
    let output = Command::new("ps").args(&["aux"]).output().ok()?;

    let ps_output = String::from_utf8_lossy(&output.stdout);

    // Find all Chrome processes with debug ports
    let mut found_ports = Vec::new();
    for line in ps_output.lines() {
        // Look for Chrome/Chromium processes with remote-debugging-port
        if (line.contains("chrome") || line.contains("Chrome") || line.contains("chromium"))
            && line.contains("--remote-debugging-port=")
        {
            // Extract the port number
            if let Some(port_str) = line.split("--remote-debugging-port=").nth(1) {
                // Take everything up to the next space or end of line
                let port_str = port_str.split_whitespace().next().unwrap_or(port_str);

                // Parse the port number
                if let Ok(port) = port_str.parse::<u16>() {
                    // Skip port 0 (means random port, not accessible)
                    if port > 0 {
                        found_ports.push(port);
                    }
                }
            }
        }
    }

    // Remove duplicates
    found_ports.sort_unstable();
    found_ports.dedup();

    info!(
        "Found {} Chrome process(es) with debug ports: {:?}",
        found_ports.len(),
        found_ports
    );

    // Test each found port to see if it's accessible (test in parallel for speed)
    if found_ports.is_empty() {
        return None;
    }

    debug!("Testing {} port(s) for accessibility...", found_ports.len());
    let test_start = tokio::time::Instant::now();

    // Create futures for testing all ports in parallel
    let mut port_tests = Vec::new();
    for port in found_ports {
        let test_future = async move {
            let url = format!("http://127.0.0.1:{}/json/version", port);
            let client = Client::builder()
                .timeout(Duration::from_millis(200)) // Shorter timeout for parallel tests
                .build()
                .ok()?;

            match client.get(&url).send().await {
                Ok(resp) if resp.status().is_success() => {
                    debug!("Chrome port {} is accessible", port);
                    Some(port)
                }
                Ok(resp) => {
                    debug!("Chrome port {} returned status: {}", port, resp.status());
                    None
                }
                Err(_) => {
                    debug!("Could not connect to Chrome port {}", port);
                    None
                }
            }
        };
        port_tests.push(test_future);
    }

    // Test all ports in parallel and return the first accessible one
    let results = futures::future::join_all(port_tests).await;
    debug!(
        "Port accessibility tests completed in {:?}",
        test_start.elapsed()
    );

    for port in results.into_iter().flatten() {
        info!("Verified Chrome debug port at {} is accessible", port);
        return Some(port);
    }

    warn!("No accessible Chrome debug ports found");
    None
}

pub struct BrowserManager {
    pub config: Arc<RwLock<BrowserConfig>>,
    browser: Arc<Mutex<Option<Browser>>>,
    page: Arc<Mutex<Option<Arc<Page>>>>,
    // Dedicated background page for screenshots to prevent focus stealing
    background_page: Arc<Mutex<Option<Arc<Page>>>>,
    last_activity: Arc<Mutex<Instant>>,
    idle_monitor_handle: Arc<Mutex<Option<tokio::task::JoinHandle<()>>>>,
    event_task: Arc<Mutex<Option<tokio::task::JoinHandle<()>>>>,
    assets: Arc<Mutex<Option<Arc<crate::assets::AssetManager>>>>,
    user_data_dir: Arc<Mutex<Option<String>>>,
    cleanup_profile_on_drop: Arc<Mutex<bool>>,
    navigation_callback: Arc<tokio::sync::RwLock<Option<Box<dyn Fn(String) + Send + Sync>>>>,
    navigation_monitor_handle: Arc<Mutex<Option<tokio::task::JoinHandle<()>>>>,
}

impl BrowserManager {
    pub fn new(config: BrowserConfig) -> Self {
        Self {
            config: Arc::new(RwLock::new(config)),
            browser: Arc::new(Mutex::new(None)),
            page: Arc::new(Mutex::new(None)),
            background_page: Arc::new(Mutex::new(None)),
            last_activity: Arc::new(Mutex::new(Instant::now())),
            idle_monitor_handle: Arc::new(Mutex::new(None)),
            event_task: Arc::new(Mutex::new(None)),
            assets: Arc::new(Mutex::new(None)),
            user_data_dir: Arc::new(Mutex::new(None)),
            cleanup_profile_on_drop: Arc::new(Mutex::new(false)),
            navigation_callback: Arc::new(tokio::sync::RwLock::new(None)),
            navigation_monitor_handle: Arc::new(Mutex::new(None)),
        }
    }

    pub async fn start(&self) -> Result<()> {
        let mut browser_guard = self.browser.lock().await;
        if browser_guard.is_some() {
            return Ok(());
        }

        let config = self.config.read().await.clone();

        // 1) Attach to a live Chrome, if requested
        if let Some(ws) = config.connect_ws.clone() {
            info!("Connecting to Chrome via WebSocket: {}", ws);
            let (browser, mut handler) = Browser::connect(ws).await?;
            // Don't fetch targets immediately - can interfere with screenshot capture
            let task = tokio::spawn(async move { while let Some(_evt) = handler.next().await {} });
            *self.event_task.lock().await = Some(task);
            *browser_guard = Some(browser);
            *self.cleanup_profile_on_drop.lock().await = false;
            self.start_idle_monitor().await;
            self.update_activity().await;
            return Ok(());
        }

        if let Some(port) = config.connect_port {
            let actual_port = if port == 0 {
                info!("Auto-scanning for Chrome debug ports...");
                let start = tokio::time::Instant::now();
                let result = scan_for_chrome_debug_port().await.unwrap_or(0);
                info!(
                    "Auto-scan completed in {:?}, found port: {}",
                    start.elapsed(),
                    result
                );
                result
            } else {
                info!("Using specified Chrome debug port: {}", port);
                port
            };

            if actual_port > 0 {
                info!(
                    "Step 1: Discovering Chrome WebSocket URL via port {}...",
                    actual_port
                );
                let discover_start = tokio::time::Instant::now();
                match discover_ws_via_port(actual_port).await {
                    Ok(ws) => {
                        info!(
                            "Step 2: WebSocket URL discovered in {:?}: {}",
                            discover_start.elapsed(),
                            ws
                        );

                        info!("Step 3: Connecting to Chrome via WebSocket...");
                        let connect_start = tokio::time::Instant::now();
                        let (browser, mut handler) = Browser::connect(ws).await?;
                        info!(
                            "Step 4: Connected to Chrome in {:?}",
                            connect_start.elapsed()
                        );

                        // Don't fetch targets immediately - can interfere with screenshot capture
                        let task =
                            tokio::spawn(
                                async move { while let Some(_evt) = handler.next().await {} },
                            );
                        *self.event_task.lock().await = Some(task);
                        *browser_guard = Some(browser);
                        *self.cleanup_profile_on_drop.lock().await = false;

                        info!("Step 5: Starting idle monitor...");
                        self.start_idle_monitor().await;
                        self.update_activity().await;
                        info!("Step 6: Chrome connection complete!");
                        return Ok(());
                    }
                    Err(e) => {
                        warn!(
                            "Failed to discover WebSocket after {:?}: {}. Will launch new instance.",
                            discover_start.elapsed(),
                            e
                        );
                    }
                }
            }
        }

        // 2) Launch a browser
        info!("Launching new browser instance");
        let mut builder = CdpConfig::builder();

        // Profile dir
        let user_data_path = if let Some(dir) = &config.user_data_dir {
            builder = builder.user_data_dir(dir.clone());
            dir.to_string_lossy().to_string()
        } else {
            let timestamp = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis();
            let temp_path = format!("/tmp/coder-browser-{}-{}", std::process::id(), timestamp);
            if tokio::fs::metadata(&temp_path).await.is_ok() {
                let _ = tokio::fs::remove_dir_all(&temp_path).await;
            }
            builder = builder.user_data_dir(&temp_path);
            temp_path
        };

        // Set headless mode based on config (keep original approach for stability)
        if config.headless {
            builder = builder.headless_mode(HeadlessMode::New);
        }

        // Configure viewport (revert to original approach for screenshot stability)
        builder = builder.window_size(config.viewport.width, config.viewport.height);

        // Add browser launch flags (keep minimal set for screenshot functionality)
        builder = builder
            .arg("--disable-blink-features=AutomationControlled")
            .arg("--no-first-run")
            .arg("--no-default-browser-check");

        let browser_config = builder
            .build()
            .map_err(|e| BrowserError::CdpError(e.to_string()))?;
        let (browser, mut handler) = Browser::launch(browser_config).await?;
        // Optionally: browser.fetch_targets().await.ok();

        let task = tokio::spawn(async move {
            while let Some(event) = handler.next().await {
                debug!("Browser event: {:?}", event);
            }
        });
        *self.event_task.lock().await = Some(task);

        *browser_guard = Some(browser);
        *self.user_data_dir.lock().await = Some(user_data_path.clone());

        let should_cleanup = config.user_data_dir.is_none() || !config.persist_profile;
        *self.cleanup_profile_on_drop.lock().await = should_cleanup;

        self.start_idle_monitor().await;
        self.update_activity().await;
        Ok(())
    }

    pub async fn stop(&self) -> Result<()> {
        self.stop_idle_monitor().await;

        // stop event handler task cleanly
        if let Some(task) = self.event_task.lock().await.take() {
            task.abort();
        }

        self.stop_navigation_monitor().await;

        let mut page_guard = self.page.lock().await;
        *page_guard = None;

        // Also cleanup the background page
        let mut background_page_guard = self.background_page.lock().await;
        *background_page_guard = None;

        let config = self.config.read().await;
        let is_external_chrome = config.connect_port.is_some() || config.connect_ws.is_some();
        drop(config);

        let mut browser_guard = self.browser.lock().await;
        if let Some(mut browser) = browser_guard.take() {
            if is_external_chrome {
                info!("Disconnecting from external Chrome (not closing it)");
                // Just drop the connection, don't close the browser
            } else {
                info!("Stopping browser we launched");
                browser.close().await?;
            }
        }

        // When cleaning profiles, respect the flag everywhere:
        let should_cleanup = *self.cleanup_profile_on_drop.lock().await;
        if should_cleanup {
            let mut user_data_guard = self.user_data_dir.lock().await;
            if let Some(user_data_path) = user_data_guard.take() {
                tokio::time::sleep(std::time::Duration::from_millis(500)).await;
                let _ = tokio::fs::remove_dir_all(&user_data_path).await;
                #[cfg(target_os = "macos")]
                {
                    let _ = tokio::process::Command::new("rm")
                        .arg("-rf")
                        .arg(&user_data_path)
                        .output()
                        .await;
                }
            }
        }

        Ok(())
    }

    pub async fn get_or_create_page(&self) -> Result<Arc<Page>> {
        self.ensure_browser().await?;
        self.update_activity().await;

        let mut page_guard = self.page.lock().await;
        if let Some(page) = page_guard.as_ref() {
            return Ok(Arc::clone(page));
        }

        let browser_guard = self.browser.lock().await;
        let browser = browser_guard.as_ref().ok_or(BrowserError::NotInitialized)?;

        let config = self.config.read().await;

        // If we're connected to an existing Chrome (via connect_port or connect_ws),
        // try to use the current active tab instead of creating a new one
        let cdp_page = if config.connect_port.is_some() || config.connect_ws.is_some() {
            // Try to get existing pages
            let mut pages = browser.pages().await?;

            if !pages.is_empty() {
                // Try to find the active/visible tab
                // We'll check each page to see if it's visible/focused
                let mut active_page = None;

                for page in &pages {
                    // Check if this page is visible by evaluating document.visibilityState
                    // and document.hasFocus()
                    let is_visible = page
                        .evaluate(
                            "(() => { 
                            return {
                                visible: document.visibilityState === 'visible',
                                focused: document.hasFocus(),
                                url: window.location.href
                            };
                        })()",
                        )
                        .await;

                    if let Ok(result) = is_visible {
                        if let Ok(obj) = result.into_value::<serde_json::Value>() {
                            let visible = obj
                                .get("visible")
                                .and_then(|v| v.as_bool())
                                .unwrap_or(false);
                            let focused = obj
                                .get("focused")
                                .and_then(|v| v.as_bool())
                                .unwrap_or(false);
                            let url = obj.get("url").and_then(|v| v.as_str()).unwrap_or("unknown");

                            debug!(
                                "Tab check - URL: {}, Visible: {}, Focused: {}",
                                url, visible, focused
                            );

                            // Prefer focused tab, then visible tab
                            if focused {
                                info!("Found focused tab: {}", url);
                                active_page = Some(page.clone());
                                break;
                            } else if visible && active_page.is_none() {
                                info!("Found visible tab: {}", url);
                                active_page = Some(page.clone());
                            }
                        }
                    }
                }

                // Use the active page if found, otherwise fall back to the last page
                // (which is often the most recently used)
                if let Some(page) = active_page {
                    info!("Using active/visible Chrome tab");
                    page
                } else {
                    // Use the last page as it's often the most recent
                    info!("No active tab found, using most recent tab");
                    pages.pop().unwrap()
                }
            } else {
                // No existing pages, create a new one
                info!("No existing tabs found, creating new tab");
                browser.new_page("about:blank").await?
            }
        } else {
            // We launched Chrome ourselves, create a new page
            browser.new_page("about:blank").await?
        };

        // Apply page overrides (UA, locale, timezone, viewport, etc.)
        self.apply_page_overrides(&cdp_page).await?;

        let page = Arc::new(Page::new(cdp_page, config.clone()));
        *page_guard = Some(Arc::clone(&page));

        // Start navigation monitoring for this page
        self.start_navigation_monitor(Arc::clone(&page)).await;

        Ok(page)
    }

    pub async fn close_page(&self) -> Result<()> {
        let mut page_guard = self.page.lock().await;
        if let Some(page) = page_guard.take() {
            page.close().await?;
        }
        Ok(())
    }

    pub async fn is_enabled(&self) -> bool {
        self.config.read().await.enabled
    }

    pub fn is_enabled_sync(&self) -> bool {
        self.config.try_read().map(|c| c.enabled).unwrap_or(false)
    }

    /// Get a description of the browser connection type
    pub async fn get_browser_type(&self) -> String {
        let config = self.config.read().await;
        if config.connect_ws.is_some() || config.connect_port.is_some() {
            "CDP-connected to user's Chrome browser".to_string()
        } else if config.headless {
            "internal headless Chrome browser".to_string()
        } else {
            "internal Chrome browser (headed mode)".to_string()
        }
    }

    pub async fn set_enabled(&self, enabled: bool) -> Result<()> {
        let mut config = self.config.write().await;
        config.enabled = enabled;

        if enabled {
            self.start().await?;
        } else {
            self.stop().await?;
        }

        Ok(())
    }

    pub async fn update_config(&self, updates: impl FnOnce(&mut BrowserConfig)) -> Result<()> {
        let mut config = self.config.write().await;
        updates(&mut config);

        if let Some(page) = self.page.lock().await.as_ref() {
            // Avoid viewport manipulation for external CDP connections to prevent focus/flicker
            let is_external = config.connect_port.is_some() || config.connect_ws.is_some();
            if !is_external {
                page.update_viewport(config.viewport.clone()).await?;
            }
        }

        Ok(())
    }

    pub async fn get_config(&self) -> BrowserConfig {
        self.config.read().await.clone()
    }

    pub async fn get_current_url(&self) -> Option<String> {
        let page_guard = self.page.lock().await;
        if let Some(page) = page_guard.as_ref() {
            page.get_current_url().await.ok()
        } else {
            None
        }
    }

    pub async fn get_status(&self) -> BrowserStatus {
        let config = self.config.read().await;
        let browser_active = self.browser.lock().await.is_some();
        let current_url = self.get_current_url().await;

        BrowserStatus {
            enabled: config.enabled,
            browser_active,
            current_url,
            viewport: config.viewport.clone(),
            fullpage: config.fullpage,
        }
    }

    /// Apply environment overrides on page creation.
    /// - For external CDP connections: set viewport once on connect; skip humanization (UA, locale, etc.).
    /// - For internal (launched) Chrome: apply humanization; skip viewport here (kept minimal).
    pub async fn apply_page_overrides(&self, page: &chromiumoxide::Page) -> Result<()> {
        let config = self.config.read().await;
        let is_external = config.connect_port.is_some() || config.connect_ws.is_some();

        // Always enable Network domain once
        page.execute(network::EnableParams::default()).await?;

        if is_external {
            // External Chrome: set viewport once on connection; skip humanization.
            let viewport_params = emulation::SetDeviceMetricsOverrideParams::builder()
                .width(config.viewport.width as i64)
                .height(config.viewport.height as i64)
                .device_scale_factor(config.viewport.device_scale_factor)
                .mobile(config.viewport.mobile)
                .build()
                .map_err(BrowserError::CdpError)?;
            page.execute(viewport_params).await?;
        } else {
            // Internal (launched) Chrome: apply human settings; do not touch viewport here.
            if let Some(ua) = &config.user_agent {
                let mut b = network::SetUserAgentOverrideParams::builder().user_agent(ua);
                if let Some(al) = &config.accept_language {
                    b = b.accept_language(al);
                }
                page.execute(b.build().map_err(BrowserError::CdpError)?)
                    .await?;
            } else if let Some(al) = &config.accept_language {
                let mut headers_map = std::collections::HashMap::new();
                headers_map.insert(
                    "Accept-Language".to_string(),
                    serde_json::Value::String(al.clone()),
                );
                let headers = network::Headers::new(serde_json::Value::Object(
                    headers_map.into_iter().map(|(k, v)| (k, v)).collect(),
                ));
                let p = network::SetExtraHttpHeadersParams::builder()
                    .headers(headers)
                    .build()
                    .map_err(BrowserError::CdpError)?;
                page.execute(p).await?;
            }

            // Set viewport once on connection for internal Chrome as well
            let viewport_params = emulation::SetDeviceMetricsOverrideParams::builder()
                .width(config.viewport.width as i64)
                .height(config.viewport.height as i64)
                .device_scale_factor(config.viewport.device_scale_factor)
                .mobile(config.viewport.mobile)
                .build()
                .map_err(BrowserError::CdpError)?;
            page.execute(viewport_params).await?;

            if let Some(tz) = &config.timezone {
                page.execute(emulation::SetTimezoneOverrideParams {
                    timezone_id: tz.clone(),
                })
                .await?;
            }
            if let Some(locale) = &config.locale {
                let p = emulation::SetLocaleOverrideParams::builder()
                    .locale(locale)
                    .build();
                page.execute(p).await?;
            }
        }

        Ok(())
    }

    async fn ensure_browser(&self) -> Result<()> {
        let browser_guard = self.browser.lock().await;
        if browser_guard.is_none() {
            drop(browser_guard);
            self.start().await?;
        }
        Ok(())
    }

    async fn update_activity(&self) {
        let mut last_activity = self.last_activity.lock().await;
        *last_activity = Instant::now();
    }

    pub fn set_enabled_sync(&self, enabled: bool) {
        // Try to set immediately if possible, otherwise spawn a task
        if let Ok(mut cfg) = self.config.try_write() {
            cfg.enabled = enabled;
        } else {
            let config = self.config.clone();
            tokio::spawn(async move {
                let mut cfg = config.write().await;
                cfg.enabled = enabled;
            });
        }
    }

    pub fn set_fullpage_sync(&self, fullpage: bool) {
        if let Ok(mut cfg) = self.config.try_write() {
            cfg.fullpage = fullpage;
        } else {
            let config = self.config.clone();
            tokio::spawn(async move {
                let mut cfg = config.write().await;
                cfg.fullpage = fullpage;
            });
        }
    }

    pub fn set_viewport_sync(&self, width: u32, height: u32) {
        if let Ok(mut cfg) = self.config.try_write() {
            cfg.viewport.width = width;
            cfg.viewport.height = height;
        } else {
            let config = self.config.clone();
            tokio::spawn(async move {
                let mut cfg = config.write().await;
                cfg.viewport.width = width;
                cfg.viewport.height = height;
            });
        }
    }

    pub fn set_segments_max_sync(&self, segments_max: usize) {
        if let Ok(mut cfg) = self.config.try_write() {
            cfg.segments_max = segments_max;
        } else {
            let config = self.config.clone();
            tokio::spawn(async move {
                let mut cfg = config.write().await;
                cfg.segments_max = segments_max;
            });
        }
    }

    pub fn get_status_sync(&self) -> String {
        // Use try operations to avoid blocking - return cached/default values if locks are held
        let cfg = self
            .config
            .try_read()
            .map(|c| {
                let enabled = c.enabled;
                let viewport_width = c.viewport.width;
                let viewport_height = c.viewport.height;
                let fullpage = c.fullpage;
                (enabled, viewport_width, viewport_height, fullpage)
            })
            .unwrap_or((false, 1024, 768, false));

        let browser_active = self
            .browser
            .try_lock()
            .map(|b| b.is_some())
            .unwrap_or(false);

        let mode = if cfg.0 { "enabled" } else { "disabled" };
        let fullpage = if cfg.3 { "on" } else { "off" };

        let mut status = format!(
            "Browser status:\n• Mode: {}\n• Viewport: {}×{}\n• Full-page: {}",
            mode, cfg.1, cfg.2, fullpage
        );

        if browser_active {
            status.push_str("\n• Browser: active");
        }

        status
    }

    async fn start_idle_monitor(&self) {
        let config = self.config.read().await;
        let idle_timeout = Duration::from_millis(config.idle_timeout_ms);
        let is_external_chrome = config.connect_port.is_some() || config.connect_ws.is_some();
        let should_cleanup = *self.cleanup_profile_on_drop.lock().await; // <-- respect this
        drop(config);

        if is_external_chrome {
            info!("Skipping idle monitor for external Chrome connection");
            return;
        }

        let browser = Arc::clone(&self.browser);
        let last_activity = Arc::clone(&self.last_activity);
        let user_data_dir = Arc::clone(&self.user_data_dir);

        let handle = tokio::spawn(async move {
            loop {
                sleep(Duration::from_secs(10)).await;
                let last = *last_activity.lock().await;
                if last.elapsed() > idle_timeout {
                    warn!("Browser idle timeout reached, closing");
                    let mut browser_guard = browser.lock().await;
                    if let Some(mut browser) = browser_guard.take() {
                        let _ = browser.close().await;
                    }
                    if should_cleanup {
                        if let Some(user_data_path) = user_data_dir.lock().await.take() {
                            let _ = tokio::fs::remove_dir_all(&user_data_path).await;
                        }
                    }
                    break;
                }
            }
        });

        *self.idle_monitor_handle.lock().await = Some(handle);
    }

    async fn stop_idle_monitor(&self) {
        let mut handle_guard = self.idle_monitor_handle.lock().await;
        if let Some(handle) = handle_guard.take() {
            handle.abort();
        }
    }

    pub async fn goto(&self, url: &str) -> Result<crate::page::GotoResult> {
        // Get or create page
        let page = self.get_or_create_page().await?;

        info!("Navigating to URL: {}", url);
        let config = self.config.read().await;
        let result = page.goto(url, Some(config.wait.clone())).await?;
        info!("Navigation complete to: {}", result.url);

        // Manually trigger navigation callback for immediate response
        if let Some(ref callback) = *self.navigation_callback.read().await {
            debug!("Manually triggering navigation callback after goto");
            callback(result.url.clone());
        }

        self.update_activity().await;
        Ok(result)
    }

    pub async fn capture_screenshot_with_url(
        &self,
    ) -> Result<(Vec<std::path::PathBuf>, Option<String>)> {
        let (paths, url) = self.capture_screenshot_internal().await?;
        Ok((paths, Some(url)))
    }

    pub async fn capture_screenshot(&self) -> Result<Vec<std::path::PathBuf>> {
        let (paths, _) = self.capture_screenshot_internal().await?;
        Ok(paths)
    }

    async fn capture_screenshot_internal(&self) -> Result<(Vec<std::path::PathBuf>, String)> {
        // Always capture from the active page; do not create background tabs.
        self.capture_screenshot_regular().await
    }

    /// Capture screenshot using regular strategy (launched Chrome)
    async fn capture_screenshot_regular(&self) -> Result<(Vec<std::path::PathBuf>, String)> {
        // For launched Chrome, use the regular approach since it's already isolated
        let page = self.get_or_create_page().await?;

        // Initialize assets manager if needed
        let mut assets_guard = self.assets.lock().await;
        if assets_guard.is_none() {
            *assets_guard = Some(Arc::new(crate::assets::AssetManager::new().await?));
        }
        let assets = assets_guard.as_ref().unwrap().clone();
        drop(assets_guard);

        // Get current config
        let config = self.config.read().await;

        // Determine screenshot mode
        let mode = if config.fullpage {
            crate::page::ScreenshotMode::FullPage {
                segments_max: Some(config.segments_max),
            }
        } else {
            crate::page::ScreenshotMode::Viewport
        };

        // Get current URL
        let current_url = page
            .get_current_url()
            .await
            .unwrap_or_else(|_| "about:blank".to_string());

        // Capture screenshots
        let screenshots = page.screenshot(mode).await?;

        // Store screenshots and get paths
        let mut paths = Vec::new();
        for screenshot in screenshots {
            let image_ref = assets
                .store_screenshot(
                    &screenshot.data,
                    screenshot.format,
                    screenshot.width,
                    screenshot.height,
                    300000, // 5 minute TTL
                )
                .await?;
            paths.push(std::path::PathBuf::from(image_ref.path));
        }

        self.update_activity().await;
        Ok((paths, current_url))
    }

    pub async fn close(&self) -> Result<()> {
        // Just delegate to stop() which handles cleanup properly
        self.stop().await
    }

    /// Click at the specified coordinates
    pub async fn click(&self, x: f64, y: f64) -> Result<()> {
        let page = self.get_or_create_page().await?;
        page.click(x, y).await
    }

    /// Type text into the currently focused element
    pub async fn type_text(&self, text: &str) -> Result<()> {
        let page = self.get_or_create_page().await?;
        page.type_text(text).await
    }

    /// Press a key (e.g., "Enter", "Tab", "Escape", "ArrowDown")
    pub async fn press_key(&self, key: &str) -> Result<()> {
        let page = self.get_or_create_page().await?;
        page.press_key(key).await
    }

    /// Execute JavaScript code with enhanced return value handling
    pub async fn execute_javascript(&self, code: &str) -> Result<serde_json::Value> {
        let page = self.get_or_create_page().await?;
        page.execute_javascript(code).await
    }

    /// Scroll the page by the given delta in pixels
    pub async fn scroll_by(&self, dx: f64, dy: f64) -> Result<()> {
        let page = self.get_or_create_page().await?;
        page.scroll_by(dx, dy).await
    }

    /// Navigate browser history backward one entry
    pub async fn history_back(&self) -> Result<()> {
        let page = self.get_or_create_page().await?;
        page.go_back().await
    }

    /// Navigate browser history forward one entry
    pub async fn history_forward(&self) -> Result<()> {
        let page = self.get_or_create_page().await?;
        page.go_forward().await
    }

    /// Set a callback to be called when navigation occurs
    pub async fn set_navigation_callback<F>(&self, callback: F)
    where
        F: Fn(String) + Send + Sync + 'static,
    {
        let mut callback_guard = self.navigation_callback.write().await;
        *callback_guard = Some(Box::new(callback));
    }

    /// Start monitoring for page navigation changes
    async fn start_navigation_monitor(&self, page: Arc<Page>) {
        // Stop any existing monitor
        self.stop_navigation_monitor().await;

        let navigation_callback = Arc::clone(&self.navigation_callback);
        let page_weak = Arc::downgrade(&page);

        let handle = tokio::spawn(async move {
            let mut last_url = String::new();
            let mut check_count = 0;

            loop {
                // Check if page is still alive
                let page = match page_weak.upgrade() {
                    Some(p) => p,
                    None => {
                        debug!("Page dropped, stopping navigation monitor");
                        break;
                    }
                };

                // Get current URL
                if let Ok(current_url) = page.get_current_url().await {
                    // Check if URL changed (ignore about:blank)
                    if current_url != last_url && current_url != "about:blank" {
                        info!(
                            "Navigation detected: {} -> {}",
                            if last_url.is_empty() {
                                "initial"
                            } else {
                                &last_url
                            },
                            current_url
                        );
                        last_url = current_url.clone();

                        // Call the callback if set (immediate)
                        if let Some(ref callback) = *navigation_callback.read().await {
                            debug!("Triggering navigation callback for URL: {}", current_url);
                            callback(current_url.clone());
                        }

                        // Schedule a delayed callback for fully loaded page
                        let navigation_callback_delayed = Arc::clone(&navigation_callback);
                        let current_url_delayed = current_url.clone();
                        tokio::spawn(async move {
                            // Wait for page to fully load
                            tokio::time::sleep(tokio::time::Duration::from_millis(2000)).await;

                            // Call the callback again with a marker that it's fully loaded
                            if let Some(ref callback) = *navigation_callback_delayed.read().await {
                                info!("Page fully loaded callback for: {}", current_url_delayed);
                                callback(current_url_delayed);
                            }
                        });
                    }
                }

                // Also inject JavaScript to detect client-side navigation
                if check_count % 10 == 0 {
                    // Every 10 checks, reinject the navigation detection script
                    let script = r#"
                        (function() {
                            if (!window.__coder_nav_monitor) {
                                window.__coder_nav_monitor = true;
                                let lastUrl = window.location.href;
                                
                                // Monitor for pushState/replaceState
                                const originalPushState = history.pushState;
                                const originalReplaceState = history.replaceState;
                                
                                history.pushState = function() {
                                    originalPushState.apply(history, arguments);
                                    window.__coder_url_changed = true;
                                };
                                
                                history.replaceState = function() {
                                    originalReplaceState.apply(history, arguments);
                                    window.__coder_url_changed = true;
                                };
                                
                                // Monitor popstate event
                                window.addEventListener('popstate', function() {
                                    window.__coder_url_changed = true;
                                });
                                
                                // Monitor for hash changes
                                window.addEventListener('hashchange', function() {
                                    window.__coder_url_changed = true;
                                });
                            }
                            
                            // Check if URL changed
                            const changed = window.__coder_url_changed || false;
                            window.__coder_url_changed = false;
                            return {
                                url: window.location.href,
                                changed: changed
                            };
                        })()
                    "#;

                    if let Ok(result) = page.execute_javascript(script).await {
                        if let Some(changed) = result.get("changed").and_then(|v| v.as_bool()) {
                            if changed {
                                if let Some(url) = result.get("url").and_then(|v| v.as_str()) {
                                    info!("Client-side navigation detected: {}", url);
                                    if let Some(ref callback) = *navigation_callback.read().await {
                                        callback(url.to_string());
                                    }

                                    // Schedule a delayed callback for fully loaded page
                                    let navigation_callback_delayed =
                                        Arc::clone(&navigation_callback);
                                    let url_delayed = url.to_string();
                                    tokio::spawn(async move {
                                        tokio::time::sleep(tokio::time::Duration::from_millis(
                                            2000,
                                        ))
                                        .await;
                                        if let Some(ref callback) =
                                            *navigation_callback_delayed.read().await
                                        {
                                            info!(
                                                "Page fully loaded callback for client-side nav: {}",
                                                url_delayed
                                            );
                                            callback(url_delayed);
                                        }
                                    });
                                }
                            }
                        }
                    }
                }

                check_count += 1;

                // Check every 500ms
                tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
            }
        });

        let mut handle_guard = self.navigation_monitor_handle.lock().await;
        *handle_guard = Some(handle);
    }

    /// Stop navigation monitoring
    async fn stop_navigation_monitor(&self) {
        let mut handle_guard = self.navigation_monitor_handle.lock().await;
        if let Some(handle) = handle_guard.take() {
            handle.abort();
        }
    }
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct BrowserStatus {
    pub enabled: bool,
    pub browser_active: bool,
    pub current_url: Option<String>,
    pub viewport: crate::config::ViewportConfig,
    pub fullpage: bool,
}
