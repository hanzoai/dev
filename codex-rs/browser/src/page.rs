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
// Import MouseButton (New)
use chromiumoxide::cdp::browser_protocol::input::MouseButton;
// Import AddScriptToEvaluateOnNewDocumentParams (New)
use base64::Engine as _;
use chromiumoxide::cdp::browser_protocol::emulation::SetDeviceMetricsOverrideParams;
use chromiumoxide::cdp::browser_protocol::page::AddScriptToEvaluateOnNewDocumentParams;
use chromiumoxide::cdp::browser_protocol::page::CaptureScreenshotFormat;
use chromiumoxide::cdp::browser_protocol::page::CaptureScreenshotParams;
use chromiumoxide::page::Page as CdpPage;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
// Use Mutex for cursor state (New)
use tokio::sync::Mutex;
use tracing::debug;
use tracing::info;
use tracing::warn;

// Define CursorState struct (New)
#[derive(Debug, Clone)]
pub struct CursorState {
    pub x: f64,
    pub y: f64,
    // Include button state, mirroring the TS implementation
    pub button: MouseButton,
    // Track whether mouse button is currently pressed
    pub is_mouse_down: bool,
}

pub struct Page {
    cdp_page: Arc<CdpPage>,
    config: BrowserConfig,
    current_url: Arc<RwLock<Option<String>>>,
    // Add cursor state tracking (New)
    cursor_state: Arc<Mutex<CursorState>>,
}

impl Page {
    pub fn new(cdp_page: CdpPage, config: BrowserConfig) -> Self {
        // Initialize cursor position (Updated)
        let initial_cursor = CursorState {
            x: (config.viewport.width as f64 / 2.0).floor(),
            y: (config.viewport.height as f64 / 4.0).floor(),
            button: MouseButton::None,
            is_mouse_down: false,
        };

        let page = Self {
            cdp_page: Arc::new(cdp_page),
            config,
            current_url: Arc::new(RwLock::new(None)),
            cursor_state: Arc::new(Mutex::new(initial_cursor)),
        };

        // Inject the navigation interception script asynchronously (New)
        let cdp_page_clone = page.cdp_page.clone();
        tokio::spawn(async move {
            if let Err(e) = Self::inject_tab_interception_script(&cdp_page_clone).await {
                warn!("Failed to inject navigation interception script: {}", e);
            }
        });

        page
    }

    /// (NEW) Injects the script to prevent new tabs from opening and redirect them to the current tab.
    async fn inject_tab_interception_script(cdp_page: &Arc<CdpPage>) -> Result<()> {
        // The comprehensive script ported from browser_session.ts
        let script = r#"
            (() => {
                // Hardened window.open override with Proxy
                const originalOpen = window.open;
                const openProxy = new Proxy(originalOpen, {
                    apply(_t, _this, args) {
                        const url = args[0];
                        if (url) location.href = url;
                        return null;
                    }
                });
                // Lock down the property
                Object.defineProperty(window, 'open', {
                    value: openProxy,
                    writable: false,
                    configurable: false
                });

                // Extract URL helper (including specific attributes from TS version)
                const urlFrom = n => n?.href ??
                    n?.getAttribute?.('href') ??
                    n?.getAttribute?.('post-outbound-link') ??
                    n?.dataset?.url ??
                    n?.dataset?.href ?? null;

                // Intercept handler (handles shadow DOM)
                const intercept = e => {
                    const path = e.composedPath?.() ?? [];
                    for (const n of path) {
                        if (!n?.getAttribute) continue;
                        if (n.getAttribute('target') === '_blank') {
                            const url = urlFrom(n);
                            if (url) {
                                e.preventDefault();
                                e.stopImmediatePropagation();
                                location.href = url;
                            }
                            return;
                        }
                    }
                };

                // Attach listeners
                ['pointerdown', 'click', 'auxclick'].forEach(ev =>
                    document.addEventListener(ev, intercept, { capture: true })
                );

                // Handle keyboard navigation
                document.addEventListener('keydown', e => {
                    if ((e.key === 'Enter' || e.key === ' ') &&
                        document.activeElement?.getAttribute?.('target') === '_blank') {
                        e.preventDefault();
                        const url = urlFrom(document.activeElement);
                        if (url) location.href = url;
                    }
                }, { capture: true });

                // Handle form submissions
                document.addEventListener('submit', e => {
                    if (e.target?.target === '_blank') {
                        e.preventDefault();
                        e.target.target = '_self';
                        e.target.submit();
                    }
                }, { capture: true });

                // Helper to attach listeners to shadow roots
                const attach = root =>
                    ['pointerdown', 'click', 'auxclick'].forEach(ev =>
                        root.addEventListener(ev, intercept, { capture: true })
                    );

                // MutationObserver for shadow DOM
                try {
                    const observeTarget = document.documentElement || document;
                    if (observeTarget) {
                        new MutationObserver(muts => {
                            muts.forEach(m =>
                                m.addedNodes.forEach(n => n && n.shadowRoot && attach(n.shadowRoot))
                            );
                        }).observe(observeTarget, { subtree: true, childList: true });
                    }
                } catch (e) {
                    console.warn("BrowserAutomation: Failed to set up MutationObserver for tab blocking", e);
                }
            })();
        "#;

        let params = AddScriptToEvaluateOnNewDocumentParams::new(script);
        cdp_page.execute(params).await?;
        debug!("Tab interception script injected successfully.");
        Ok(())
    }

    /// Helper function to capture screenshot with retry logic.
    /// First tries with from_surface(false) to avoid flashing.
    /// If that fails, retries with from_surface(true) for when window is not visible.
    async fn capture_screenshot_with_retry(
        &self,
        params_builder: chromiumoxide::cdp::browser_protocol::page::CaptureScreenshotParamsBuilder,
    ) -> Result<chromiumoxide::cdp::browser_protocol::page::CaptureScreenshotReturns> {
        // First try with from_surface(false) to avoid flashing, with timeout
        let params = params_builder.clone().from_surface(false).build();

        let first_attempt = tokio::time::timeout(
            tokio::time::Duration::from_secs(8),
            self.cdp_page.execute(params),
        )
        .await;

        match first_attempt {
            Ok(Ok(resp)) => Ok(resp.result),
            Ok(Err(e)) => {
                // Log the initial failure
                debug!(
                    "Screenshot with from_surface(false) failed: {}. Retrying with from_surface(true)...",
                    e
                );

                // Retry with from_surface(true) - this may cause flashing but will work when window is not visible
                let retry_params = params_builder.from_surface(true).build();
                let retry_attempt = tokio::time::timeout(
                    tokio::time::Duration::from_secs(8),
                    self.cdp_page.execute(retry_params),
                )
                .await;

                match retry_attempt {
                    Ok(Ok(resp)) => Ok(resp.result),
                    Ok(Err(retry_err)) => {
                        debug!(
                            "Screenshot retry with from_surface(true) also failed: {}",
                            retry_err
                        );
                        Err(retry_err.into())
                    }
                    Err(_) => Err(BrowserError::ScreenshotError(
                        "Screenshot retry timed out after 8 seconds".to_string(),
                    )),
                }
            }
            Err(_) => {
                // First attempt timed out, try with from_surface(true)
                debug!(
                    "Screenshot with from_surface(false) timed out. Retrying with from_surface(true)..."
                );

                let retry_params = params_builder.from_surface(true).build();
                let retry_attempt = tokio::time::timeout(
                    tokio::time::Duration::from_secs(8),
                    self.cdp_page.execute(retry_params),
                )
                .await;

                match retry_attempt {
                    Ok(Ok(resp)) => Ok(resp.result),
                    Ok(Err(e)) => Err(e.into()),
                    Err(_) => Err(BrowserError::ScreenshotError(
                        "Screenshot timed out after 8 seconds on both attempts".to_string(),
                    )),
                }
            }
        }
    }

    /// Returns the current page title, if available.
    pub async fn get_title(&self) -> Option<String> {
        self.cdp_page.get_title().await.ok().flatten()
    }

    /// Check and fix viewport scaling issues before taking screenshots
    async fn check_and_fix_scaling(&self) -> Result<()> {
        // Check current viewport and scaling
        let check_script = r#"
            (() => {
                const vw = window.innerWidth;
                const vh = window.innerHeight;
                const dpr = window.devicePixelRatio || 1;
                const zoom = Math.round(window.outerWidth / window.innerWidth * 100) / 100;
                
                // Check if viewport matches expected dimensions
                const expectedWidth = %EXPECTED_WIDTH%;
                const expectedHeight = %EXPECTED_HEIGHT%;
                const expectedDpr = %EXPECTED_DPR%;
                
                return {
                    currentWidth: vw,
                    currentHeight: vh,
                    currentDpr: dpr,
                    currentZoom: zoom,
                    expectedWidth: expectedWidth,
                    expectedHeight: expectedHeight,
                    expectedDpr: expectedDpr,
                    needsCorrection: (
                        Math.abs(vw - expectedWidth) > 5 || 
                        Math.abs(vh - expectedHeight) > 5 ||
                        Math.abs(dpr - expectedDpr) > 0.1 ||
                        Math.abs(zoom - 1.0) > 0.1
                    )
                };
            })()
        "#;

        // Replace placeholders with actual expected values
        let script = check_script
            .replace("%EXPECTED_WIDTH%", &self.config.viewport.width.to_string())
            .replace(
                "%EXPECTED_HEIGHT%",
                &self.config.viewport.height.to_string(),
            )
            .replace(
                "%EXPECTED_DPR%",
                &self.config.viewport.device_scale_factor.to_string(),
            );

        let result = self.cdp_page.evaluate(script).await?;

        if let Some(obj) = result.value() {
            let needs_correction = obj
                .get("needsCorrection")
                .and_then(|v| v.as_bool())
                .unwrap_or(false);

            if needs_correction {
                let current_width = obj
                    .get("currentWidth")
                    .and_then(|v| v.as_u64())
                    .unwrap_or(0);
                let current_height = obj
                    .get("currentHeight")
                    .and_then(|v| v.as_u64())
                    .unwrap_or(0);
                let current_dpr = obj
                    .get("currentDpr")
                    .and_then(|v| v.as_f64())
                    .unwrap_or(1.0);
                let current_zoom = obj
                    .get("currentZoom")
                    .and_then(|v| v.as_f64())
                    .unwrap_or(1.0);

                debug!(
                    "Viewport needs correction: {}x{} @ {}x DPR (zoom: {}) -> {}x{} @ {}x DPR",
                    current_width,
                    current_height,
                    current_dpr,
                    current_zoom,
                    self.config.viewport.width,
                    self.config.viewport.height,
                    self.config.viewport.device_scale_factor
                );

                // Use CDP to set the correct viewport metrics
                let params = SetDeviceMetricsOverrideParams::builder()
                    .width(self.config.viewport.width as i64)
                    .height(self.config.viewport.height as i64)
                    .device_scale_factor(self.config.viewport.device_scale_factor)
                    .mobile(self.config.viewport.mobile)
                    .build()
                    .map_err(|e| {
                        BrowserError::CdpError(format!("Failed to build viewport params: {}", e))
                    })?;

                self.cdp_page.execute(params).await?;

                // Also reset zoom if it's not 1.0
                if (current_zoom - 1.0).abs() > 0.1 {
                    debug!("Resetting zoom from {} to 1.0", current_zoom);
                    let reset_zoom_script = r#"
                        // Reset zoom to 100%
                        document.body.style.zoom = '1.0';
                        // Also try to reset CSS transform scale if present
                        document.documentElement.style.transform = 'scale(1)';
                        document.documentElement.style.transformOrigin = '0 0';
                    "#;
                    let _ = self.cdp_page.evaluate(reset_zoom_script).await;
                }

                info!("Viewport scaling corrected");
            }
        }

        Ok(())
    }

    /// (NEW) Injects a virtual cursor element into the page at the current coordinates.
    pub async fn inject_virtual_cursor(&self) -> Result<()> {
        let cursor = self.cursor_state.lock().await.clone();
        let cursor_x = cursor.x;
        let cursor_y = cursor.y;

        // Creates (once) and updates a two-part virtual cursor:
        // - Arrow (instant follow)
        // - Badge (slight lag / offset for a nicer feel)
        //
        // Tunables inside script:
        //   ARROW_SIZE_PX        => overall design width (scales arrow)
        //   BADGE_SIZE_PX        => overall design width (scales badge)
        //   TIP_X/TIP_Y    => pixel nudge so the arrow tip lands exactly on (x,y)
        //   BADGE_OFF_*    => relative offset of the trailing badge
        //   LAG            => how much the badge trails; lower = looser, higher = tighter
        //
        // To remove later: evaluate `window.__vc?.destroy()`
        let script = format!(
            r#"
(function(x, y) {{
  const ns = 'http://www.w3.org/2000/svg';
  const VIEW_W = 40, VIEW_H = 30;       // original viewBox of your assets
  const ARROW_SIZE_PX = 53;             // arrow visual width (px)
  const BADGE_SIZE_PX = 70;             // badge visual width (px)
  const TIP_X = 3, TIP_Y = 3;           // tip calibration (px) — adjust if needed
  const BADGE_OFF_X = -4, BADGE_OFF_Y = -8;

  function ensureRoot() {{
    let root = document.getElementById('__virtualCursorRoot');
    if (!root) {{
      root = document.createElement('div');
      root.id = '__virtualCursorRoot';
      Object.assign(root.style, {{
        position: 'fixed',
        inset: '0',                 // cover viewport -> non-zero paint area
        pointerEvents: 'none',
        zIndex: '2147483647',
        contain: 'layout style',    // avoid 'paint' or you'll clip to root box
        overflow: 'visible',
      }});
      (document.body || document.documentElement).appendChild(root);
    }}
    return root;
  }}

  function createSvg(tag) {{ return document.createElementNS(ns, tag); }}

  // Install once
  if (!window.__vc) {{
    const root = ensureRoot();

    // --- Arrow SVG container ---
    const arrow = createSvg('svg');
    arrow.setAttribute('viewBox', '0 0 44 34');
    arrow.setAttribute('aria-hidden', 'true');
    arrow.style.position = 'absolute';
    arrow.style.transformOrigin = '0 0';
    arrow.style.width = ARROW_SIZE_PX + 'px';
    arrow.style.height = 'auto';

    // defs + drop-shadow filter (your values)
    const defs = createSvg('defs');
    const filt = createSvg('filter');
    filt.setAttribute('id', 'vc-drop-shadow');
    filt.setAttribute('color-interpolation-filters', 'sRGB');
    filt.setAttribute('x', '-50%');
    filt.setAttribute('y', '-50%');
    filt.setAttribute('width', '200%');
    filt.setAttribute('height', '200%');

    const blur = createSvg('feGaussianBlur'); blur.setAttribute('in', 'SourceAlpha'); blur.setAttribute('stdDeviation', '1.5');
    const off  = createSvg('feOffset');       off.setAttribute('dx', '0'); off.setAttribute('dy', '0');
    const ct   = createSvg('feComponentTransfer'); ct.setAttribute('result', 'offsetblur');
    const fa   = createSvg('feFuncA'); fa.setAttribute('type', 'linear'); fa.setAttribute('slope', '0.35'); ct.appendChild(fa);
    const flood = createSvg('feFlood'); flood.setAttribute('flood-color', '#000'); flood.setAttribute('flood-opacity', '0.35');
    const comp  = createSvg('feComposite'); comp.setAttribute('in2', 'offsetblur'); comp.setAttribute('operator', 'in');
    const merge = createSvg('feMerge');
    const m1 = createSvg('feMergeNode');
    const m2 = createSvg('feMergeNode'); m2.setAttribute('in', 'SourceGraphic');

    merge.appendChild(m1); merge.appendChild(m2);
    filt.appendChild(blur); filt.appendChild(off); filt.appendChild(ct); filt.appendChild(flood); filt.appendChild(comp); filt.appendChild(merge);
    defs.appendChild(filt);
    arrow.appendChild(defs);

    const arrowPath = createSvg('path');
    arrowPath.setAttribute('d',
      'M 16.63 12.239 C 16.63 12.239 3.029 2.981 3 3 L 3.841 18.948 C 3.841 19.648 4.641 20.148 5.241 19.748 L 9.518 15.207 L 16.13 13.939 C 16.93 13.839 17.253 12.798 16.63 12.239 Z'
    );
    arrowPath.setAttribute('stroke', 'white');
    arrowPath.setAttribute('stroke-width','1');
    arrowPath.setAttribute('vector-effect','non-scaling-stroke');
    arrowPath.setAttribute('fill', 'rgb(0, 171, 255)');
    arrowPath.style.strokeLinejoin = 'round';
    arrowPath.setAttribute('filter', 'url(#vc-drop-shadow)');
    arrow.appendChild(arrowPath);

    // --- Badge SVG container ---
    const badge = createSvg('svg');
    badge.setAttribute('viewBox', '0 0 44 34');
    badge.setAttribute('aria-hidden', 'true');
    badge.style.position = 'absolute';
    badge.style.transformOrigin = '0 0';
    badge.style.width = BADGE_SIZE_PX + 'px';
    badge.style.height = 'auto';

    const rect = createSvg('rect');
    rect.setAttribute('x','13.119');
    rect.setAttribute('y','20.897');
    rect.setAttribute('width','28.228');
    rect.setAttribute('height','10.691');
    rect.setAttribute('rx','5');
    rect.setAttribute('ry','5');
    rect.setAttribute('fill','rgb(0, 171, 255)');
    rect.setAttribute('stroke', 'white');
    rect.setAttribute('stroke-width','1');
    rect.setAttribute('vector-effect','non-scaling-stroke');
    rect.setAttribute('filter', 'url(#vc-drop-shadow)');
    badge.appendChild(defs.cloneNode(true));

    const glyphs = createSvg('path');
    glyphs.setAttribute('d',
      'M 21.568 26.99 L 22.259 27.165 C 22.114 27.732 21.854 28.165 21.477 28.464 C 21.1 28.762 20.64 28.911 20.095 28.911 C 19.532 28.911 19.074 28.796 18.721 28.567 C 18.368 28.338 18.1 28.006 17.916 27.571 C 17.732 27.136 17.64 26.669 17.64 26.17 C 17.64 25.626 17.744 25.151 17.951 24.746 C 18.159 24.341 18.455 24.033 18.839 23.823 C 19.223 23.612 19.645 23.507 20.106 23.507 C 20.629 23.507 21.068 23.64 21.425 23.907 C 21.782 24.173 22.03 24.547 22.17 25.029 L 21.489 25.19 C 21.368 24.81 21.192 24.533 20.962 24.359 C 20.731 24.186 20.441 24.099 20.092 24.099 C 19.691 24.099 19.355 24.195 19.085 24.388 C 18.815 24.581 18.625 24.839 18.516 25.163 C 18.407 25.488 18.352 25.822 18.352 26.166 C 18.352 26.611 18.417 26.999 18.547 27.33 C 18.676 27.662 18.878 27.91 19.151 28.073 C 19.424 28.237 19.72 28.319 20.038 28.319 C 20.425 28.319 20.753 28.207 21.022 27.984 C 21.291 27.761 21.473 27.429 21.568 26.99 Z M 22.79 26.929 C 22.79 26.228 22.985 25.709 23.375 25.372 C 23.7 25.091 24.097 24.951 24.565 24.951 C 25.086 24.951 25.511 25.122 25.841 25.463 C 26.172 25.804 26.337 26.275 26.337 26.876 C 26.337 27.363 26.264 27.746 26.117 28.025 C 25.971 28.304 25.758 28.521 25.479 28.676 C 25.2 28.831 24.896 28.908 24.565 28.908 C 24.035 28.908 23.607 28.738 23.28 28.398 C 22.953 28.058 22.79 27.568 22.79 26.929 Z M 23.449 26.929 C 23.449 27.414 23.555 27.777 23.767 28.018 C 23.978 28.259 24.244 28.38 24.565 28.38 C 24.884 28.38 25.149 28.259 25.36 28.016 C 25.571 27.774 25.677 27.405 25.677 26.908 C 25.677 26.44 25.571 26.085 25.358 25.844 C 25.145 25.603 24.881 25.482 24.565 25.482 C 24.244 25.482 23.978 25.602 23.767 25.842 C 23.555 26.082 23.449 26.444 23.449 26.929 Z M 29.544 28.822 L 29.544 28.344 C 29.304 28.72 28.951 28.908 28.485 28.908 C 28.184 28.908 27.906 28.825 27.653 28.658 C 27.4 28.492 27.204 28.26 27.065 27.961 C 26.926 27.663 26.856 27.32 26.856 26.933 C 26.856 26.555 26.919 26.212 27.045 25.904 C 27.171 25.597 27.36 25.361 27.612 25.197 C 27.864 25.033 28.146 24.951 28.457 24.951 C 28.685 24.951 28.888 24.999 29.066 25.095 C 29.245 25.192 29.39 25.317 29.501 25.471 L 29.501 23.597 L 30.139 23.597 L 30.139 28.822 L 29.544 28.822 Z M 27.516 26.933 C 27.516 27.418 27.618 27.78 27.822 28.02 C 28.027 28.26 28.268 28.38 28.546 28.38 C 28.826 28.38 29.064 28.265 29.26 28.036 C 29.457 27.807 29.555 27.457 29.555 26.986 C 29.555 26.468 29.455 26.088 29.255 25.846 C 29.056 25.603 28.81 25.482 28.517 25.482 C 28.232 25.482 27.994 25.598 27.803 25.831 C 27.612 26.064 27.516 26.432 27.516 26.933 Z M 33.739 27.603 L 34.402 27.685 C 34.297 28.072 34.104 28.373 33.821 28.587 C 33.538 28.801 33.177 28.908 32.738 28.908 C 32.184 28.908 31.745 28.737 31.421 28.396 C 31.096 28.055 30.934 27.577 30.934 26.961 C 30.934 26.324 31.098 25.83 31.426 25.479 C 31.754 25.127 32.179 24.951 32.702 24.951 C 33.208 24.951 33.621 25.123 33.942 25.468 C 34.263 25.813 34.424 26.297 34.424 26.922 C 34.424 26.96 34.423 27.017 34.42 27.093 L 31.597 27.093 C 31.621 27.509 31.739 27.828 31.95 28.049 C 32.161 28.27 32.425 28.38 32.741 28.38 C 32.976 28.38 33.177 28.318 33.344 28.195 C 33.51 28.071 33.642 27.874 33.739 27.603 Z M 31.633 26.566 L 33.746 26.566 C 33.718 26.247 33.637 26.008 33.504 25.849 C 33.3 25.602 33.035 25.479 32.709 25.479 C 32.414 25.479 32.167 25.577 31.966 25.774 C 31.765 25.971 31.654 26.235 31.633 26.566 Z M 35.201 28.822 L 35.201 25.037 L 35.778 25.037 L 35.778 25.61 C 35.925 25.342 36.061 25.165 36.186 25.079 C 36.311 24.994 36.449 24.951 36.598 24.951 C 36.814 24.951 37.034 25.02 37.258 25.158 L 37.037 25.753 C 36.88 25.66 36.723 25.614 36.566 25.614 C 36.426 25.614 36.3 25.656 36.188 25.741 C 36.077 25.825 35.997 25.942 35.949 26.092 C 35.878 26.32 35.842 26.569 35.842 26.84 L 35.842 28.822 L 35.201 28.822 Z'
    );
    glyphs.setAttribute('fill', 'white');

    badge.appendChild(rect);
    badge.appendChild(glyphs);

    root.appendChild(arrow);
    root.appendChild(badge);

    // Initial state and transforms
    const state = {{
      arrow, badge, root,
      arrowX: Math.round(x - TIP_X),
      arrowY: Math.round(y - TIP_Y),
      badgeX: Math.round(x - TIP_X + BADGE_OFF_X),
      badgeY: Math.round(y - TIP_Y + BADGE_OFF_Y),
      aAnim: null,
      bAnim: null,
    }};

    arrow.style.transform = 'translate3d(' + state.arrowX + 'px,' + state.arrowY + 'px,0)';
    badge.style.transform = 'translate3d(' + state.badgeX + 'px,' + state.badgeY + 'px,0)';
    arrow.style.willChange = 'transform';
    badge.style.willChange = 'transform';

    // Motion configuration (distance-based durations)
    const MOTION = {{
      pxPerSec: 800,                          // base speed
      min: 600,                                // ms clamp
      max: 3000,                                // ms clamp
      easing: 'cubic-bezier(0.25, 1, 0.5, 1)', // easeOutQuart-ish
      badgeScale: 1.25,                         // badge longer than arrow
      badgeDelay: 100,                          // ms
      jitter: 0.08,                             // ±8% random variation
    }};

    function dist(x0, y0, x1, y1) {{
      const dx = x1 - x0, dy = y1 - y0;
      return Math.hypot(dx, dy);
    }}

    function durationForDistance(d) {{
      // raw time from speed
      let ms = (d / Math.max(1, MOTION.pxPerSec)) * 1000;
      // slight variation (apply before clamping so clamps still respected)
      if (MOTION.jitter > 0) {{
        const j = (Math.random() * 2 - 1) * MOTION.jitter; // [-jitter, +jitter]
        ms = ms * (1 + j);
      }}
      // clamp
      ms = Math.min(MOTION.max, Math.max(MOTION.min, ms));
      // respect reduced motion
      if (window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches) {{
        ms = Math.min(80, ms);
      }}
      return Math.round(ms);
    }}

    function commit(ax, ay, bx, by) {{
      state.arrowX = ax; state.arrowY = ay;
      state.badgeX = bx; state.badgeY = by;
    }}

    function moveTo(nx, ny, opts) {{
      const o = Object.assign({{}}, MOTION, opts || {{}});

      const ax1 = Math.round(nx - TIP_X), ay1 = Math.round(ny - TIP_Y);
      const bx1 = ax1 + BADGE_OFF_X,      by1 = ay1 + BADGE_OFF_Y;

      const ax0 = state.arrowX, ay0 = state.arrowY;
      const bx0 = state.badgeX, by0 = state.badgeY;

      const d    = dist(ax0, ay0, ax1, ay1);
      const base = durationForDistance(d);
      const aDur = base;
      const bDur = Math.round(base * o.badgeScale);
      const bDel = o.badgeDelay;

      // Cancel any in-flight animations (discrete moves -> clean restart)
      try {{ state.aAnim && state.aAnim.cancel(); }} catch (e) {{}}
      try {{ state.bAnim && state.bAnim.cancel(); }} catch (e) {{}}

      state.aAnim = arrow.animate(
        [ {{ transform: 'translate3d(' + ax0 + 'px,' + ay0 + 'px,0)' }},
          {{ transform: 'translate3d(' + ax1 + 'px,' + ay1 + 'px,0)' }} ],
        {{ duration: aDur, easing: o.easing, fill: 'forwards' }}
      );

      state.bAnim = badge.animate(
        [ {{ transform: 'translate3d(' + bx0 + 'px,' + by0 + 'px,0)' }},
          {{ transform: 'translate3d(' + bx1 + 'px,' + by1 + 'px,0)' }} ],
        {{ duration: bDur, delay: bDel, easing: o.easing, fill: 'forwards' }}
      );

      // Commit endpoints immediately so subsequent math uses the new base
      commit(ax1, ay1, bx1, by1);
    }}

    // --- Hover-to-dim (distance to tip) ---
    root.style.opacity = '1';
    root.style.transition = 'opacity 160ms ease-out';
    const HOVER = {{ opacity: 0.2, offset: 20, radius: 55, enabled: true }};

    let _mx = 0, _my = 0, _rafHover = 0, _dimmed = false;
    function hoverTick() {{
      _rafHover = 0;
      const tipX = state.arrowX + TIP_X + HOVER.offset;
      const tipY = state.arrowY + TIP_Y + HOVER.offset;
      const dx = _mx - tipX, dy = _my - tipY;
      const over = (dx*dx + dy*dy) <= (HOVER.radius * HOVER.radius);
      const shouldDim = HOVER.enabled && over;
      if (shouldDim !== _dimmed) {{
        _dimmed = shouldDim;
        root.style.opacity = shouldDim ? String(HOVER.opacity) : '1';
      }}
    }}
    function scheduleHover(ev) {{
      _mx = ev.clientX; _my = ev.clientY;
      if (!_rafHover) _rafHover = requestAnimationFrame(hoverTick);
    }}
    window.addEventListener('mousemove', scheduleHover, {{ passive: true }});
    window.addEventListener('mouseleave', function() {{
      if (_dimmed) {{ _dimmed = false; root.style.opacity = '1'; }}
    }}, {{ passive: true }});

    // Public API
    window.__vc = {{
      moveTo: moveTo,                 // preferred
      update: function(nx, ny) {{     // backwards compat
        moveTo(nx, ny);
      }},
      setSize: function(arrowPx, badgePx) {{
        if (arrowPx) arrow.style.width = arrowPx + 'px';
        if (badgePx) badge.style.width = badgePx + 'px';
      }},
      setMotion: function(p) {{ Object.assign(MOTION, p || {{}}); }},
      setHover:  function(p) {{ Object.assign(HOVER,  p || {{}}); }},
      destroy: function() {{
        try {{ state.aAnim && state.aAnim.cancel(); }} catch (e) {{}}
        try {{ state.bAnim && state.bAnim.cancel(); }} catch (e) {{}}
        window.removeEventListener('mousemove', scheduleHover);
        if (root && root.parentNode) root.parentNode.removeChild(root);
        window.__vc = null;
      }},
      _s: state
    }};

    // Go to initial position
    window.__vc.moveTo(x, y);

  }} else {{
    // Already installed; just move to the new position
    window.__vc.moveTo(x, y);
  }}
}})({cursor_x}, {cursor_y});
"#,
            cursor_x = cursor_x,
            cursor_y = cursor_y
        );

        self.cdp_page.evaluate(script).await?;
        Ok(())
    }

    /// (NEW) Ensures an editable element is focused before typing. (Ported from TS implementation)
    async fn ensure_editable_focused(&self) -> Result<bool> {
        let cursor = self.cursor_state.lock().await.clone();
        let cursor_x = cursor.x;
        let cursor_y = cursor.y;

        // The JS logic from browser_session.ts
        let script = format!(
            r#"
            (function(cursorX, cursorY) {{
                const editable = el => el && (
                    // Refined list: exclude input types not meant for direct typing
                    (el.tagName === 'INPUT' && !/^(checkbox|radio|button|submit|reset|file|image|color|hidden|range)$/i.test(el.type)) ||
                    el.tagName === 'TEXTAREA' ||
                    el.isContentEditable
                );

                // 1) Keep current focus if editable
                if (editable(document.activeElement)) return true;

                // 2) Try element at cursor point
                if (Number.isFinite(cursorX) && Number.isFinite(cursorY)) {{
                    let el = document.elementFromPoint(cursorX, cursorY);
                    // Walk up the tree to find an editable parent
                    while (el && !editable(el)) {{
                        el = el.parentElement;
                    }}
                    if (editable(el)) {{
                        if (typeof el.focus === 'function') el.focus();
                        // Verify focus was successful
                        return document.activeElement === el;
                    }}
                }}

                // 3) Find best visible candidate (closest to center, then bigger)
                const cx = window.innerWidth/2, cy = window.innerHeight/2;
                const candidates = [...document.querySelectorAll('input,textarea,[contenteditable],[contenteditable=""],[contenteditable="true"]')]
                    .filter(n => editable(n) &&
                        n.offsetWidth > 0 &&
                        n.offsetHeight > 0 &&
                        getComputedStyle(n).visibility !== 'hidden' &&
                        getComputedStyle(n).display !== 'none')
                    .map(n => {{
                        const r = n.getBoundingClientRect();
                        const dx = r.left + r.width/2 - cx;
                        const dy = r.top + r.height/2 - cy;
                        return {{
                            node: n,
                            dist: dx*dx + dy*dy,
                            area: r.width * r.height
                        }};
                    }})
                    .sort((a, b) => a.dist - b.dist || b.area - a.area);

                if (candidates.length) {{
                    if (typeof candidates[0].node.focus === 'function') candidates[0].node.focus();
                    return document.activeElement === candidates[0].node;
                }}
                return false;
            }})({cursor_x}, {cursor_y})
        "#,
            cursor_x = cursor_x,
            cursor_y = cursor_y
        );

        let result = self.cdp_page.evaluate(script).await?;
        let focused = result.value().and_then(|v| v.as_bool()).unwrap_or(false);

        Ok(focused)
    }

    pub async fn goto(&self, url: &str, wait: Option<WaitStrategy>) -> Result<GotoResult> {
        info!("Navigating to {}", url);

        let wait_strategy = wait.unwrap_or_else(|| self.config.wait.clone());

        // Navigate to the URL with retry on timeout
        let max_retries = 3;
        let mut last_error = None;

        for attempt in 1..=max_retries {
            match self.cdp_page.goto(url).await {
                Ok(_) => {
                    // Navigation succeeded, break out of retry loop
                    break;
                }
                Err(e) => {
                    let error_str = e.to_string();
                    if error_str.contains("Request timed out") || error_str.contains("timeout") {
                        warn!(
                            "Navigation timeout on attempt {}/{}: {}",
                            attempt, max_retries, error_str
                        );
                        last_error = Some(e);

                        if attempt < max_retries {
                            // Wait before retry, increasing delay each time
                            let delay_ms = 1000 * attempt as u64;
                            info!("Retrying navigation after {}ms...", delay_ms);
                            tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;
                            continue;
                        }
                    } else {
                        // Non-timeout error, fail immediately
                        return Err(e.into());
                    }
                }
            }
        }

        // If we exhausted retries, return the last error
        if let Some(e) = last_error {
            return Err(BrowserError::CdpError(format!(
                "Navigation failed after {} retries: {}",
                max_retries, e
            )));
        }

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
        drop(current_url); // Release lock before injecting cursor

        // Inject the virtual cursor after navigation
        debug!("Injecting virtual cursor after navigation");
        if let Err(e) = self.inject_virtual_cursor().await {
            warn!("Failed to inject virtual cursor after navigation: {}", e);
            // Continue even if cursor injection fails
        }

        Ok(GotoResult {
            url: final_url,
            title,
        })
    }

    // (UPDATED) Inject cursor before taking screenshot
    pub async fn screenshot(&self, mode: ScreenshotMode) -> Result<Vec<Screenshot>> {
        // Check if this is an external Chrome connection
        let is_external = self.config.connect_port.is_some() || self.config.connect_ws.is_some();

        // First, check and fix viewport scaling if needed (skip for external Chrome)
        if !is_external {
            if let Err(e) = self.check_and_fix_scaling().await {
                warn!(
                    "Failed to check/fix viewport scaling: {}, continuing anyway",
                    e
                );
            }
        }

        // Inject the virtual cursor before capturing
        if let Err(e) = self.inject_virtual_cursor().await {
            warn!("Failed to inject virtual cursor: {}", e);
            // Continue with screenshot even if cursor injection fails
        }

        // Allow a brief moment for the cursor SVG to render and scaling to apply
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

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

        let doc_w = probe.get("w").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
        let doc_h = probe.get("h").and_then(|v| v.as_u64()).unwrap_or(0) as u32;

        // Fall back to configured viewport if probe failed
        let vw = if doc_w > 0 {
            doc_w
        } else {
            self.config.viewport.width
        };
        let vh = if doc_h > 0 {
            doc_h
        } else {
            self.config.viewport.height
        };

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
                .map_err(|e| {
                    BrowserError::ScreenshotError(format!("base64 decode failed: {}", e))
                })?;
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

        let params_builder = CaptureScreenshotParams::builder().format(format).clip(
            chromiumoxide::cdp::browser_protocol::page::Viewport {
                x: region.x as f64,
                y: region.y as f64,
                width: region.width as f64,
                height: region.height as f64,
                scale: 1.0,
            },
        );

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

    pub async fn update_viewport(&self, _viewport: ViewportConfig) -> Result<()> {
        Ok(())
    }

    // Move the mouse by relative offset from current position
    pub async fn move_mouse_relative(&self, dx: f64, dy: f64) -> Result<(f64, f64)> {
        // Get current position
        let cursor = self.cursor_state.lock().await;
        let current_x = cursor.x;
        let current_y = cursor.y;
        drop(cursor);

        // Calculate new position
        let new_x = current_x + dx;
        let new_y = current_y + dy;

        debug!(
            "Moving mouse relatively by ({}, {}) from ({}, {}) to ({}, {})",
            dx, dy, current_x, current_y, new_x, new_y
        );

        // Use absolute move with the calculated position
        self.move_mouse(new_x, new_y).await?;
        Ok((new_x, new_y))
    }

    // (NEW) Move the mouse to the specified coordinates
    pub async fn move_mouse(&self, x: f64, y: f64) -> Result<()> {
        debug!("Moving mouse to ({}, {})", x, y);

        // Clamp and floor coordinates
        let move_x = x.floor().max(0.0);
        let move_y = y.floor().max(0.0);

        let mut cursor = self.cursor_state.lock().await;

        // Dispatch the mouse move event, including the current button state
        let move_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MouseMoved)
            .x(move_x)
            .y(move_y)
            .button(cursor.button.clone()) // Pass the button state
            .build()
            .map_err(BrowserError::CdpError)?;
        self.cdp_page.execute(move_params).await?;

        // Update cursor position
        cursor.x = move_x;
        cursor.y = move_y;
        drop(cursor); // Release lock before JavaScript evaluation

        // First check if cursor exists, if not inject it
        let check_script = "typeof window.__vc !== 'undefined'";
        let cursor_exists = self
            .cdp_page
            .evaluate(check_script)
            .await
            .ok()
            .and_then(|result| result.value().and_then(|v| v.as_bool()))
            .unwrap_or(false);

        if !cursor_exists {
            debug!("Virtual cursor not found, injecting it now");
            if let Err(e) = self.inject_virtual_cursor().await {
                warn!("Failed to inject virtual cursor: {}", e);
            }
        }

        // Update cursor position
        let _ = self
            .cdp_page
            .evaluate(format!(
                "window.__vc && window.__vc.update({:.0}, {:.0});",
                move_x, move_y
            ))
            .await;

        // Small delay (ported from TS)
        tokio::time::sleep(tokio::time::Duration::from_millis(30)).await;

        Ok(())
    }

    /// (UPDATED) Click at the specified coordinates with visual animation
    pub async fn click(&self, x: f64, y: f64) -> Result<()> {
        debug!("Clicking at ({}, {})", x, y);

        // Use move_mouse to handle movement, clamping, and state update
        self.move_mouse(x, y).await?;

        // Get the final coordinates after potential clamping
        let cursor = self.cursor_state.lock().await;
        let click_x = cursor.x;
        let click_y = cursor.y;
        drop(cursor); // Release lock before async calls

        // Trigger click animation on the virtual cursor
        // The animation scales from 1.0 to 0.7 and back over 3 seconds with transform-origin at top-left
        let animation_script = r#"
            if (window.__vc && window.__vc._s) {
                const arrow = window.__vc._s.arrow;
                const badge = window.__vc._s.badge;
                if (arrow && badge) {
                    // Store original transform to preserve position
                    const originalArrowTransform = arrow.style.transform;
                    const originalBadgeTransform = badge.style.transform;
                    
                    // Add transition for smooth animation (3s total duration)
                    arrow.style.transition = 'transform 1.5s cubic-bezier(0.4, 0, 0.2, 1)';
                    badge.style.transition = 'transform 1.5s cubic-bezier(0.4, 0, 0.2, 1)';
                    
                    // Set transform-origin to top-left (where the cursor tip is)
                    arrow.style.transformOrigin = '0 0';
                    badge.style.transformOrigin = '0 0';
                    
                    // Apply scale down (keeping position)
                    setTimeout(() => {
                        // Parse existing transform to preserve translate3d
                        const arrowMatch = originalArrowTransform.match(/translate3d\([^)]+\)/);
                        const badgeMatch = originalBadgeTransform.match(/translate3d\([^)]+\)/);
                        const arrowTranslate = arrowMatch ? arrowMatch[0] : 'translate3d(0px, 0px, 0)';
                        const badgeTranslate = badgeMatch ? badgeMatch[0] : 'translate3d(0px, 0px, 0)';
                        
                        // Apply scale while preserving position
                        arrow.style.transform = arrowTranslate + ' scale(0.7)';
                        badge.style.transform = badgeTranslate + ' scale(0.7)';
                    }, 10);
                    
                    // Scale back up after 1.5s
                    setTimeout(() => {
                        arrow.style.transform = originalArrowTransform;
                        badge.style.transform = originalBadgeTransform;
                    }, 1500);
                    
                    // Clean up transitions after animation completes
                    setTimeout(() => {
                        arrow.style.transition = '';
                        badge.style.transition = '';
                    }, 3000);
                }
            }
        "#;

        // Start the click animation
        let _ = self.cdp_page.evaluate(animation_script).await;

        // Mouse down
        let down_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MousePressed)
            .x(click_x)
            .y(click_y)
            .button(MouseButton::Left)
            .click_count(1)
            .build()
            .map_err(BrowserError::CdpError)?;
        self.cdp_page.execute(down_params).await?;

        // Add a small delay between press and release (Ported from TS)
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;

        // Mouse up
        let up_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MouseReleased)
            .x(click_x)
            .y(click_y)
            .button(MouseButton::Left)
            .click_count(1)
            .build()
            .map_err(BrowserError::CdpError)?;
        self.cdp_page.execute(up_params).await?;

        // Wait briefly to allow potential event handlers (like navigation) to trigger (ported from TS)
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        Ok(())
    }

    /// Perform mouse down at the current position
    pub async fn mouse_down_at_current(&self) -> Result<(f64, f64)> {
        let cursor = self.cursor_state.lock().await;
        let x = cursor.x;
        let y = cursor.y;
        let is_down = cursor.is_mouse_down;
        drop(cursor);
        
        if is_down {
            debug!("Mouse is already down at ({}, {})", x, y);
            return Ok((x, y));
        }
        
        debug!("Mouse down at current position ({}, {})", x, y);
        
        let down_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MousePressed)
            .x(x)
            .y(y)
            .button(MouseButton::Left)
            .click_count(1)
            .build()
            .map_err(BrowserError::CdpError)?;
        self.cdp_page.execute(down_params).await?;
        
        // Update mouse state
        let mut cursor = self.cursor_state.lock().await;
        cursor.is_mouse_down = true;
        drop(cursor);
        
        Ok((x, y))
    }
    
    /// Perform mouse up at the current position
    pub async fn mouse_up_at_current(&self) -> Result<(f64, f64)> {
        let cursor = self.cursor_state.lock().await;
        let x = cursor.x;
        let y = cursor.y;
        let is_down = cursor.is_mouse_down;
        drop(cursor);
        
        if !is_down {
            debug!("Mouse is already up at ({}, {})", x, y);
            return Ok((x, y));
        }
        
        debug!("Mouse up at current position ({}, {})", x, y);
        
        let up_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MouseReleased)
            .x(x)
            .y(y)
            .button(MouseButton::Left)
            .click_count(1)
            .build()
            .map_err(BrowserError::CdpError)?;
        self.cdp_page.execute(up_params).await?;
        
        // Update mouse state
        let mut cursor = self.cursor_state.lock().await;
        cursor.is_mouse_down = false;
        drop(cursor);
        
        Ok((x, y))
    }

    /// Click at the current mouse position without moving the cursor
    pub async fn click_at_current(&self) -> Result<(f64, f64)> {
        // Get the current cursor position and check if mouse is down
        let cursor = self.cursor_state.lock().await;
        let click_x = cursor.x;
        let click_y = cursor.y;
        let was_down = cursor.is_mouse_down;
        debug!("Clicking at current position ({}, {}), mouse was_down: {}", click_x, click_y, was_down);
        drop(cursor); // Release lock before async calls
        
        // If mouse is already down, release it first
        if was_down {
            debug!("Mouse was down, releasing first before click");
            self.mouse_up_at_current().await?;
            // Small delay between release and new click
            tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
        }

        // Trigger click animation on the virtual cursor
        // The animation scales from 1.0 to 0.7 and back over 3 seconds with transform-origin at top-left
        let animation_script = r#"
            if (window.__vc && window.__vc._s) {
                const arrow = window.__vc._s.arrow;
                const badge = window.__vc._s.badge;
                if (arrow && badge) {
                    // Store original transform to preserve position
                    const originalArrowTransform = arrow.style.transform;
                    const originalBadgeTransform = badge.style.transform;
                    
                    // Add transition for smooth animation (3s total duration)
                    arrow.style.transition = 'transform 1.5s cubic-bezier(0.4, 0, 0.2, 1)';
                    badge.style.transition = 'transform 1.5s cubic-bezier(0.4, 0, 0.2, 1)';
                    
                    // Set transform-origin to top-left (where the cursor tip is)
                    arrow.style.transformOrigin = '0 0';
                    badge.style.transformOrigin = '0 0';
                    
                    // Apply scale down (keeping position)
                    setTimeout(() => {
                        // Parse existing transform to preserve translate3d
                        const arrowMatch = originalArrowTransform.match(/translate3d\([^)]+\)/);
                        const badgeMatch = originalBadgeTransform.match(/translate3d\([^)]+\)/);
                        const arrowTranslate = arrowMatch ? arrowMatch[0] : 'translate3d(0px, 0px, 0)';
                        const badgeTranslate = badgeMatch ? badgeMatch[0] : 'translate3d(0px, 0px, 0)';
                        
                        // Apply scale while preserving position
                        arrow.style.transform = arrowTranslate + ' scale(0.7)';
                        badge.style.transform = badgeTranslate + ' scale(0.7)';
                    }, 10);
                    
                    // Scale back up after 1.5s
                    setTimeout(() => {
                        arrow.style.transform = originalArrowTransform;
                        badge.style.transform = originalBadgeTransform;
                    }, 1500);
                    
                    // Clean up transitions after animation completes
                    setTimeout(() => {
                        arrow.style.transition = '';
                        badge.style.transition = '';
                    }, 3000);
                }
            }
        "#;

        // Start the click animation
        let _ = self.cdp_page.evaluate(animation_script).await;

        // Mouse down
        let down_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MousePressed)
            .x(click_x)
            .y(click_y)
            .button(MouseButton::Left)
            .click_count(1)
            .build()
            .map_err(BrowserError::CdpError)?;
        self.cdp_page.execute(down_params).await?;

        // Add a small delay between press and release (Ported from TS)
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;

        // Mouse up
        let up_params = DispatchMouseEventParams::builder()
            .r#type(DispatchMouseEventType::MouseReleased)
            .x(click_x)
            .y(click_y)
            .button(MouseButton::Left)
            .click_count(1)
            .build()
            .map_err(BrowserError::CdpError)?;
        self.cdp_page.execute(up_params).await?;

        // Wait briefly to allow potential event handlers (like navigation) to trigger (ported from TS)
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        Ok((click_x, click_y))
    }

    /// Type text into the currently focused element with optimized typing strategies
    pub async fn type_text(&self, text: &str) -> Result<()> {
        // Replace em dashes with regular dashes
        let processed_text = text.replace('—', " - ");
        debug!("Typing text: {}", processed_text);

        // Ensure an editable element is focused first
        self.ensure_editable_focused().await?;

        let text_len = processed_text.len();

        if text_len < 20 {
            // Short text: character-by-character with natural delays
            for ch in processed_text.chars() {
                let params = DispatchKeyEventParams::builder()
                    .r#type(DispatchKeyEventType::Char)
                    .text(ch.to_string())
                    .build()
                    .map_err(BrowserError::CdpError)?;
                self.cdp_page.execute(params).await?;

                // Natural typing delay with slight randomization
                let delay = 50 + (rand::random::<u64>() % 30);
                tokio::time::sleep(Duration::from_millis(delay)).await;
            }
        } else if text_len < 100 {
            // Medium text: small chunks with InsertTextParams
            let chunk_size = 3;
            let chars: Vec<char> = processed_text.chars().collect();
            let chunks: Vec<String> = chars
                .chunks(chunk_size)
                .map(|chunk| chunk.iter().collect())
                .collect();

            for chunk in chunks {
                // Type each character in the chunk
                for ch in chunk.chars() {
                    let params = DispatchKeyEventParams::builder()
                        .r#type(DispatchKeyEventType::Char)
                        .text(ch.to_string())
                        .build()
                        .map_err(BrowserError::CdpError)?;
                    self.cdp_page.execute(params).await?;
                }

                // Delay between chunks
                let delay = 100 + (rand::random::<u64>() % 50);
                tokio::time::sleep(Duration::from_millis(delay)).await;
            }
        } else {
            // Long text: larger chunks for efficiency
            let chunk_size = 10;
            let chars: Vec<char> = processed_text.chars().collect();
            let chunks: Vec<String> = chars
                .chunks(chunk_size)
                .map(|chunk| chunk.iter().collect())
                .collect();

            for (i, chunk) in chunks.iter().enumerate() {
                // Type each character in the chunk
                for ch in chunk.chars() {
                    let params = DispatchKeyEventParams::builder()
                        .r#type(DispatchKeyEventType::Char)
                        .text(ch.to_string())
                        .build()
                        .map_err(BrowserError::CdpError)?;
                    self.cdp_page.execute(params).await?;
                }

                // Add occasional longer pauses to simulate thinking
                if i % 5 == 0 && i > 0 {
                    let delay = 300 + (rand::random::<u64>() % 200);
                    tokio::time::sleep(Duration::from_millis(delay)).await;
                } else {
                    let delay = 150 + (rand::random::<u64>() % 100);
                    tokio::time::sleep(Duration::from_millis(delay)).await;
                }
            }
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
            .map_err(BrowserError::CdpError)?;
        self.cdp_page.execute(down_params).await?;

        // Key up
        let up_params = DispatchKeyEventParams::builder()
            .r#type(DispatchKeyEventType::KeyUp)
            .key(key.to_string())
            .build()
            .map_err(BrowserError::CdpError)?;
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
        let user_code_with_source = format!("{code}\n//# sourceURL=browser_js_user_code.js");

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
            serde_json::to_string(&user_code_with_source).expect("Failed to serialize user code")
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

    /// Get the current cursor position
    pub async fn get_cursor_position(&self) -> Result<(f64, f64)> {
        let cursor = self.cursor_state.lock().await;
        Ok((cursor.x, cursor.y))
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
