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

        // Register a unified bootstrap (runs on every new document):
        //  - Blocks _blank/tab opens
        //  - Installs minimal virtual cursor early
        //  - Hooks SPA history to signal route changes
        let cdp_page_boot = page.cdp_page.clone();
        tokio::spawn(async move {
            if let Err(e) = Self::inject_bootstrap_script(&cdp_page_boot).await {
                warn!("Failed to inject unified bootstrap script: {}", e);
            } else {
                debug!("Unified bootstrap script registered for new documents");
            }
        });

        page
    }

    /// Ensure the virtual cursor is present; inject if missing, then update to current position.
    async fn ensure_virtual_cursor(&self) -> Result<bool> {
        // Quick existence check
        let exists = self
            .cdp_page
            .evaluate("typeof window.__vc !== 'undefined'")
            .await
            .ok()
            .and_then(|r| r.value().and_then(|v| v.as_bool()))
            .unwrap_or(false);

        if !exists {
            // Inject if missing
            if let Err(e) = self.inject_virtual_cursor().await {
                warn!("Failed to inject virtual cursor: {}", e);
                return Err(e);
            }
            return Ok(true);
        } else {
            // Update position to current cursor
            let cursor = self.cursor_state.lock().await.clone();
            let _ = self
                .cdp_page
                .evaluate(format!(
                    "window.__vc && window.__vc.update({:.0}, {:.0});",
                    cursor.x, cursor.y
                ))
                .await;
        }

        Ok(false)
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

    /// Injects a unified bootstrap for each new document: tab blocking + cursor bootstrap + SPA hooks
    async fn inject_bootstrap_script(cdp_page: &Arc<CdpPage>) -> Result<()> {
        // This script installs the full virtual cursor on DOM ready for each new document.
        // It also prevents _blank tabs and hooks SPA history changes.
        let script = r#"
(function(){
  // 1) Tab blocking: override window.open + intercept target="_blank"
  try {
    const originalOpen = window.open;
    const openProxy = new Proxy(originalOpen, {
      apply(_t, _this, args) {
        const url = args[0];
        if (url) location.href = url;
        return null;
      }
    });
    Object.defineProperty(window, 'open', { value: openProxy, writable: false, configurable: false });

    const urlFrom = n => n?.href ?? n?.getAttribute?.('href') ?? n?.getAttribute?.('post-outbound-link') ?? n?.dataset?.url ?? n?.dataset?.href ?? null;
    const intercept = e => {
      const path = e.composedPath?.() ?? [];
      for (const n of path) {
        if (!n?.getAttribute) continue;
        if (n.getAttribute('target') === '_blank') {
          const url = urlFrom(n);
          if (url) { e.preventDefault(); e.stopImmediatePropagation(); location.href = url; }
          return;
        }
      }
    };
    ['pointerdown','click','auxclick'].forEach(ev => document.addEventListener(ev, intercept, { capture: true }));
    document.addEventListener('keydown', e => {
      if ((e.key === 'Enter' || e.key === ' ') && document.activeElement?.getAttribute?.('target') === '_blank') {
        e.preventDefault(); const url = urlFrom(document.activeElement); if (url) location.href = url;
      }
    }, { capture: true });
    document.addEventListener('submit', e => {
      if (e.target?.target === '_blank') { e.preventDefault(); e.target.target = '_self'; e.target.submit(); }
    }, { capture: true });
    try {
      const observeTarget = document.documentElement || document;
      if (observeTarget) {
        new MutationObserver(muts => muts.forEach(m => m.addedNodes.forEach(n => n && n.shadowRoot && ['pointerdown','click','auxclick'].forEach(ev => n.shadowRoot.addEventListener(ev, intercept, { capture: true })) ))).observe(observeTarget, { subtree: true, childList: true });
      }
    } catch (e) { console.warn('Tab block MO failed', e); }
  } catch (e) { console.warn('Tab blocking failed', e); }

  // 2) SPA history hooks
  try {
    const dispatch = () => {
      try {
        const ev = new Event('codex:locationchange');
        window.dispatchEvent(ev);
        window.__codex_last_url = location.href;
      } catch {}
    };
    const push = history.pushState.bind(history);
    const repl = history.replaceState.bind(history);
    history.pushState = function(...a){ const r = push(...a); dispatch(); return r; };
    history.replaceState = function(...a){ const r = repl(...a); dispatch(); return r; };
    window.addEventListener('popstate', dispatch, { passive: true });
    dispatch();
  } catch (e) { console.warn('SPA hook failed', e); }

  // 3) No cursor bootstrap here; full cursor is injected by runtime ensure_virtual_cursor
})();
"#;

        let params = AddScriptToEvaluateOnNewDocumentParams::new(script);
        cdp_page.execute(params).await?;
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

    // Install once or upgrade from bootstrap version
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
    rect.setAttribute('x','10.82');
    rect.setAttribute('y','18.564');
    rect.setAttribute('width','25.686');
    rect.setAttribute('height','10.691');
    rect.setAttribute('rx','4');
    rect.setAttribute('ry','4');
    rect.setAttribute('fill','rgb(0, 171, 255)');
    rect.setAttribute('stroke', 'white');
    rect.setAttribute('stroke-width','1');
    rect.setAttribute('vector-effect','non-scaling-stroke');
    rect.setAttribute('filter', 'url(#vc-drop-shadow)');
    badge.appendChild(defs.cloneNode(true));

    const glyphs = createSvg('path');
    glyphs.setAttribute('d',
      'M 19.269 24.657 L 19.96 24.832 C 19.815 25.399 19.555 25.832 19.178 26.131 C 18.801 26.429 18.341 26.578 17.796 26.578 C 17.233 26.578 16.775 26.463 16.422 26.234 C 16.069 26.005 15.801 25.673 15.617 25.238 C 15.433 24.803 15.341 24.336 15.341 23.837 C 15.341 23.293 15.445 22.818 15.652 22.413 C 15.86 22.008 16.156 21.7 16.54 21.49 C 16.924 21.279 17.346 21.174 17.807 21.174 C 18.33 21.174 18.769 21.307 19.126 21.574 C 19.483 21.84 19.731 22.214 19.871 22.696 L 19.19 22.857 C 19.069 22.477 18.893 22.2 18.663 22.026 C 18.432 21.853 18.142 21.766 17.793 21.766 C 17.392 21.766 17.056 21.862 16.786 22.055 C 16.516 22.248 16.326 22.506 16.217 22.83 C 16.108 23.155 16.053 23.489 16.053 23.833 C 16.053 24.278 16.118 24.666 16.248 24.997 C 16.377 25.329 16.579 25.577 16.852 25.74 C 17.125 25.904 17.421 25.986 17.739 25.986 C 18.126 25.986 18.454 25.874 18.723 25.651 C 18.992 25.428 19.174 25.096 19.269 24.657 Z M 20.491 24.596 C 20.491 23.895 20.686 23.376 21.076 23.039 C 21.401 22.758 21.798 22.618 22.266 22.618 C 22.787 22.618 23.212 22.789 23.542 23.13 C 23.873 23.471 24.038 23.942 24.038 24.543 C 24.038 25.03 23.965 25.413 23.818 25.692 C 23.672 25.971 23.459 26.188 23.18 26.343 C 22.901 26.498 22.597 26.575 22.266 26.575 C 21.736 26.575 21.308 26.405 20.981 26.065 C 20.654 25.725 20.491 25.235 20.491 24.596 Z M 21.15 24.596 C 21.15 25.081 21.256 25.444 21.468 25.685 C 21.679 25.926 21.945 26.047 22.266 26.047 C 22.585 26.047 22.85 25.926 23.061 25.683 C 23.272 25.441 23.378 25.072 23.378 24.575 C 23.378 24.107 23.272 23.752 23.059 23.511 C 22.846 23.27 22.582 23.149 22.266 23.149 C 21.945 23.149 21.679 23.269 21.468 23.509 C 21.256 23.749 21.15 24.111 21.15 24.596 Z M 27.245 26.489 L 27.245 26.011 C 27.005 26.387 26.652 26.575 26.186 26.575 C 25.885 26.575 25.607 26.492 25.354 26.325 C 25.101 26.159 24.905 25.927 24.766 25.628 C 24.627 25.33 24.557 24.987 24.557 24.6 C 24.557 24.222 24.62 23.879 24.746 23.571 C 24.872 23.264 25.061 23.028 25.313 22.864 C 25.565 22.7 25.847 22.618 26.158 22.618 C 26.386 22.618 26.589 22.666 26.767 22.762 C 26.946 22.859 27.091 22.984 27.202 23.138 L 27.202 21.264 L 27.84 21.264 L 27.84 26.489 L 27.245 26.489 Z M 25.217 24.6 C 25.217 25.085 25.319 25.447 25.523 25.687 C 25.728 25.927 25.969 26.047 26.247 26.047 C 26.527 26.047 26.765 25.932 26.961 25.703 C 27.158 25.474 27.256 25.124 27.256 24.653 C 27.256 24.135 27.156 23.755 26.956 23.513 C 26.757 23.27 26.511 23.149 26.218 23.149 C 25.933 23.149 25.695 23.265 25.504 23.498 C 25.313 23.731 25.217 24.099 25.217 24.6 Z M 31.44 25.27 L 32.103 25.352 C 31.998 25.739 31.805 26.04 31.522 26.254 C 31.239 26.468 30.878 26.575 30.439 26.575 C 29.885 26.575 29.446 26.404 29.122 26.063 C 28.797 25.722 28.635 25.244 28.635 24.628 C 28.635 23.991 28.799 23.497 29.127 23.146 C 29.455 22.794 29.88 22.618 30.403 22.618 C 30.909 22.618 31.322 22.79 31.643 23.135 C 31.964 23.48 32.125 23.964 32.125 24.589 C 32.125 24.627 32.124 24.684 32.121 24.76 L 29.298 24.76 C 29.322 25.176 29.44 25.495 29.651 25.716 C 29.862 25.937 30.126 26.047 30.442 26.047 C 30.677 26.047 30.878 25.985 31.045 25.862 C 31.211 25.738 31.343 25.541 31.44 25.27 Z M 29.334 24.233 L 31.447 24.233 C 31.419 23.914 31.338 23.675 31.205 23.516 C 31.001 23.269 30.736 23.146 30.41 23.146 C 30.115 23.146 29.868 23.244 29.667 23.441 C 29.466 23.638 29.355 23.902 29.334 24.233 Z'
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
      ignoreHoverUntil: 0,
      curveFlip: false,
    }};

    arrow.style.transform = 'translate3d(' + state.arrowX + 'px,' + state.arrowY + 'px,0)';
    badge.style.transform = 'translate3d(' + state.badgeX + 'px,' + state.badgeY + 'px,0)';
    arrow.style.willChange = 'transform';
    badge.style.willChange = 'transform';
    // For consistent rotation pivoting
    arrow.style.transformOrigin = '0 0';
    badge.style.transformOrigin = '0 0';

    // Motion configuration (distance-based durations)
    const MOTION = {{
      pxPerSec: 120,                           // much slower, relaxed glide
      min: 600,                                // longer minimum duration
      max: 4200,                               // ms cap
      easing: 'cubic-bezier(0.18, 0.9, 0.18, 1)', // very soft ease-out
      // Duration shaping
      arrowScale: 1.50,                        // arrow takes noticeably longer
      badgeScale: 1.70,                        // badge travels longer (smooth trail)
      badgeDelay: 40,                          // tiny delay so it starts almost immediately
      jitter: 0.0,                             // deterministic for consistency
      rotateMaxDeg: 16,                        // cap rotation
      // Rotation tuning
      arrowTilt: 0.85,                         // slightly calmer rotation
      badgeTilt: 0.60,                         // calmer badge rotation
      overshootDeg: 4.0,                       // gentler overshoot
      arrowOvershootScale: 0.5,                // arrow overshoot smaller
      badgeOvershootScale: 0.85,               // badge overshoot gentle
      overshootAt: 0.7,                        // overshoot moment
      // Curve tuning
      curveFactor: 0.25,                       // scales with distance (0..1)
      curveMaxPx: 70,                          // pixel cap for arc height
      curveAlternate: true                     // alternate left/right per move for variety
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

    // Ensure elements use their currently computed transform as the inline baseline
    function pinCurrent(el) {{
      try {{
        const cs = getComputedStyle(el);
        const t  = cs && cs.transform;
        if (t && t !== 'none') {{
          // Set inline to the current computed transform to avoid visual jumps on cancel
          el.style.transform = t;
        }}
      }} catch (e) {{}}
    }}

    function moveTo(nx, ny, opts) {{
      const o = Object.assign({{}}, MOTION, opts || {{}});

      const ax1 = Math.round(nx - TIP_X), ay1 = Math.round(ny - TIP_Y);
      const bx1 = ax1 + BADGE_OFF_X,      by1 = ay1 + BADGE_OFF_Y;

      const ax0 = state.arrowX, ay0 = state.arrowY;
      const bx0 = state.badgeX, by0 = state.badgeY;

      // Helper to coerce to finite numbers
      const _safe = (v, dv=0) => (Number.isFinite(v) ? v : dv);

      const d    = dist(ax0, ay0, ax1, ay1);
      // For tiny moves, snap without animation to avoid visible twitch
      if (d < 1.5) {{
        try {{ state.aAnim && state.aAnim.cancel(); }} catch (e) {{}}
        try {{ state.bAnim && state.bAnim.cancel(); }} catch (e) {{}}
        arrow.style.transform = 'translate3d(' + ax1 + 'px,' + ay1 + 'px,0) rotate(0deg)';
        badge.style.transform = 'translate3d(' + bx1 + 'px,' + by1 + 'px,0) rotate(0deg)';
        commit(ax1, ay1, bx1, by1);
        return;
      }}

      const base = durationForDistance(d);
      const aDur = Math.round(base * o.arrowScale);
      const bDur = Math.round(base * o.badgeScale);
      const bDel = o.badgeDelay;
      const angle = Math.atan2(ay1 - ay0, ax1 - ax0) * 180 / Math.PI; // [-180,180]
      const rot   = Math.max(-o.rotateMaxDeg, Math.min(o.rotateMaxDeg, angle));
      const sign  = rot >= 0 ? 1 : -1; // direction sign
      // Scale rotation and overshoot by distance so tiny moves don't wiggle
      const distNorm = Math.min(1, d / 80); // 0..1 over ~80px
      const aRotTarget = rot * o.arrowTilt * distNorm;
      const bRotTarget = rot * o.badgeTilt * distNorm;
      const overs = o.overshootDeg * distNorm;
      const aOvershoot = -sign * overs * o.arrowOvershootScale;
      const bOvershoot = -sign * overs * o.badgeOvershootScale;

      // Curved path mid-point calculation
      const dx = ax1 - ax0, dy = ay1 - ay0;
      const len = Math.hypot(dx, dy) || 1;
      const nqx = -dy / len, nqy = dx / len; // unit normal (renamed to avoid param shadow)
      if (o.curveAlternate) state.curveFlip = !state.curveFlip;
      const curveSign = state.curveFlip ? 1 : -1;
      const curveMag = Math.min(o.curveMaxPx || 0, Math.max(0, d * (o.curveFactor || 0)));
      const mx = Math.round((ax0 + ax1) / 2 + nqx * curveMag * curveSign);
      const my = Math.round((ay0 + ay1) / 2 + nqy * curveMag * curveSign);
      const angleMid = Math.atan2(my - ay0, mx - ax0) * 180 / Math.PI;
      const aRotMid = Math.max(-o.rotateMaxDeg, Math.min(o.rotateMaxDeg, angleMid)) * (o.arrowTilt * distNorm);
      const bRotMid = Math.max(-o.rotateMaxDeg, Math.min(o.rotateMaxDeg, angleMid)) * (o.badgeTilt * distNorm);

      // Pin current visual state, then cancel any in-flight animations.
      // This prevents elements from snapping back to their old inline transforms.
      pinCurrent(arrow);
      pinCurrent(badge);
      try {{ state.aAnim && state.aAnim.cancel(); }} catch (e) {{}}
      try {{ state.bAnim && state.bAnim.cancel(); }} catch (e) {{}}

      // Arrow and badge animations with robust fallback
      try {{
        const supportsWAAPI = typeof arrow.animate === 'function' && typeof badge.animate === 'function';
        if (!supportsWAAPI) throw new Error('WAAPI not supported');

        const aKF = [
          {{ transform: 'translate3d(' + ax0 + 'px,' + ay0 + 'px,0) rotate(0deg)' }},
          {{ transform: 'translate3d(' + mx  + 'px,' + my  + 'px,0) rotate(' + aRotMid + 'deg)', offset: 0.5 }},
          {{ transform: 'translate3d(' + ax1 + 'px,' + ay1 + 'px,0) rotate(' + aRotTarget + 'deg)', offset: 0.82 }},
          {{ transform: 'translate3d(' + ax1 + 'px,' + ay1 + 'px,0) rotate(' + aOvershoot + 'deg)', offset: Math.min(0.95, Math.max(0.5, _safe(o.overshootAt, 0.7))) }},
          {{ transform: 'translate3d(' + ax1 + 'px,' + ay1 + 'px,0) rotate(0deg)' }}
        ];
        const bKF = [
          {{ transform: 'translate3d(' + bx0 + 'px,' + by0 + 'px,0) rotate(0deg)' }},
          {{ transform: 'translate3d(' + (mx + BADGE_OFF_X) + 'px,' + (my + BADGE_OFF_Y) + 'px,0) rotate(' + bRotMid + 'deg)', offset: 0.5 }},
          {{ transform: 'translate3d(' + bx1 + 'px,' + by1 + 'px,0) rotate(' + bRotTarget + 'deg)', offset: 0.85 }},
          {{ transform: 'translate3d(' + bx1 + 'px,' + by1 + 'px,0) rotate(' + bOvershoot + 'deg)', offset: Math.min(0.98, Math.max(0.55, _safe(o.overshootAt, 0.7) + 0.05)) }},
          {{ transform: 'translate3d(' + bx1 + 'px,' + by1 + 'px,0) rotate(0deg)' }}
        ];

        state.aAnim = arrow.animate(aKF, {{ duration: aDur, easing: o.easing, fill: 'forwards' }});
        state.bAnim = badge.animate(bKF, {{ duration: bDur, delay: bDel, easing: o.easing, fill: 'forwards' }});
      }} catch (e) {{
        // Fallback: set final transforms directly (no animation)
        arrow.style.transform = 'translate3d(' + ax1 + 'px,' + ay1 + 'px,0) rotate(0deg)';
        badge.style.transform = 'translate3d(' + bx1 + 'px,' + by1 + 'px,0) rotate(0deg)';
        state.aAnim = null; state.bAnim = null;
      }}

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
      const now = (window.performance && performance.now) ? performance.now() : Date.now();
      if (now < state.ignoreHoverUntil) {{
        // Ignore hover updates during synthetic/programmatic moves
        return;
      }}
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
      // Click pulse animation: bouncy scale + transient ring ripple at tip
      clickPulse: function(opts) {{
        const scaleDown = (opts && opts.scaleDown) || 0.84;
        const dur       = (opts && opts.duration) || 360; // per half-cycle (slower)
        const easing    = (opts && opts.easing) || 'cubic-bezier(0.16, 1, 0.3, 1)';

        // Cancel any prior click animations
        try {{ state.caAnim && state.caAnim.cancel(); }} catch (e) {{}}
        try {{ state.cbAnim && state.cbAnim.cancel(); }} catch (e) {{}}

        const aBase = 'translate3d(' + state.arrowX + 'px,' + state.arrowY + 'px,0)';
        const bBase = 'translate3d(' + state.badgeX + 'px,' + state.badgeY + 'px,0)';

        // Suppress hover dimming during click pulse too
        try {{
          const nowTS = (window.performance && performance.now) ? performance.now() : Date.now();
          state.ignoreHoverUntil = Math.max(state.ignoreHoverUntil, nowTS + (dur*2 + 40));
        }} catch (e) {{}}

        state.caAnim = arrow.animate(
          [ {{ transform: aBase + ' scale(1)' }},
            {{ transform: aBase + ' scale(' + scaleDown + ')' }},
            {{ transform: aBase + ' scale(1)' }} ],
          {{ duration: dur * 2, easing, fill: 'none' }}
        );
        state.cbAnim = badge.animate(
          [ {{ transform: bBase + ' scale(1)' }},
            {{ transform: bBase + ' scale(' + (scaleDown - 0.04) + ')' }},
            {{ transform: bBase + ' scale(1)' }} ],
          {{ duration: dur * 2 + 40, easing, fill: 'none' }}
        );

        // Transient ring ripple near the tip for visibility
        try {{
          const ring = document.createElement('div');
          ring.className = '__vc_click_ring';
          const tipX = state.arrowX + TIP_X; // approximate tip
          const tipY = state.arrowY + TIP_Y;
          const sz = 18; // ring base size
          Object.assign(ring.style, {{
            position: 'absolute',
            left: (tipX - sz/2) + 'px',
            top: (tipY - sz/2) + 'px',
            width: sz + 'px',
            height: sz + 'px',
            borderRadius: '999px',
            border: '2px solid rgba(255,255,255,0.95)',
            boxShadow: '0 0 0 2px rgba(0,0,0,0.15)',
            opacity: '0.9',
            pointerEvents: 'none',
            transform: 'scale(0.6)',
            transformOrigin: 'center center',
            willChange: 'transform, opacity',
            zIndex: '2147483647'
          }});
          state.root.appendChild(ring);
          const ringAnim = ring.animate([
            {{ transform: 'scale(0.6)', opacity: 0.9 }},
            {{ transform: 'scale(1.8)', opacity: 0.0 }}
          ], {{ duration: 480, easing: 'cubic-bezier(0.22, 1, 0.36, 1)', fill: 'forwards' }});
          ringAnim.onfinish = () => {{ try {{ ring.remove(); }} catch (e) {{}} }};
        }} catch (e) {{}}

        return dur * 2 + 80; // approximate total
      }},
      // Return an estimated remaining time (ms) for any in-flight animations
      getSettleMs: function() {{
        function rem(anim) {{
          if (!anim) return 0;
          try {{
            const t = anim.currentTime || 0;
            const eff = anim.effect && anim.effect.getTiming ? anim.effect.getTiming() : null;
            let d = 0;
            if (eff) {{
              if (typeof eff.duration === 'number') d = eff.duration || 0;
            }}
            const left = Math.max(0, d - t);
            return isFinite(left) ? Math.ceil(left) : 0;
          }} catch (e) {{ return 0; }}
        }}
        return Math.max(rem(state.aAnim), rem(state.bAnim), rem(state.caAnim), rem(state.cbAnim));
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

      // During programmatic movement, suppress hover dimming as CDP will fire mousemove events
      const nowTS = (window.performance && performance.now) ? performance.now() : Date.now();
      const total = Math.max(aDur, bDur + bDel) + 40;
      state.ignoreHoverUntil = nowTS + total;
        window.removeEventListener('mousemove', scheduleHover);
        if (root && root.parentNode) root.parentNode.removeChild(root);
        window.__vc = null;
      }},
      __bootstrap: false,
      __version: 2,
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

        // Navigate to the URL with retry on timeout. If Chrome reports timeouts
        // but the page URL actually updates to a real http(s) page, treat it as success.
        let max_retries = 3;
        let mut last_error = None;
        let mut fallback_navigated = false;

        for attempt in 1..=max_retries {
            // Wrap CDP navigation with a short timeout so we don't block ~30s
            // for sites that load but don't signal expected events.
            let nav_attempt = tokio::time::timeout(
                tokio::time::Duration::from_secs(5),
                self.cdp_page.goto(url),
            )
            .await;

            match nav_attempt {
                Ok(Ok(_)) => {
                    // Navigation reported success
                    last_error = None;
                    break;
                }
                Ok(Err(e)) => {
                    let error_str = e.to_string();
                    if error_str.contains("Request timed out") || error_str.contains("timeout") {
                        warn!(
                            "Navigation timeout on attempt {}/{}: {}",
                            attempt, max_retries, error_str
                        );
                        last_error = Some(e);

                        // Check if the page actually navigated despite the timeout
                        if let Ok(cur_opt) = self.cdp_page.url().await {
                            if let Some(cur) = cur_opt {
                                let looks_loaded = cur.starts_with("http://") || cur.starts_with("https://");
                                if looks_loaded && cur != "about:blank" {
                                    info!(
                                        "Navigation reported timeout, but page URL is now {} — treating as success",
                                        cur
                                    );
                                    fallback_navigated = true;
                                    last_error = None;
                                    break;
                                }
                            }
                        }

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
                Err(_) => {
                    // Our outer timeout fired; fallback to URL check, same as above.
                    warn!(
                        "Navigation attempt {}/{} exceeded 5s timeout; checking current URL...",
                        attempt, max_retries
                    );
                    // Check if the page actually navigated despite the timeout
                    if let Ok(cur_opt) = self.cdp_page.url().await {
                        if let Some(cur) = cur_opt {
                            let looks_loaded = cur.starts_with("http://") || cur.starts_with("https://");
                            if looks_loaded && cur != "about:blank" {
                                info!(
                                    "Navigation exceeded timeout, but page URL is now {} — treating as success",
                                    cur
                                );
                                fallback_navigated = true;
                                last_error = None;
                                break;
                            }
                        }
                    }

                    if attempt < max_retries {
                        let delay_ms = 1000 * attempt as u64;
                        info!("Retrying navigation after {}ms...", delay_ms);
                        tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;
                        continue;
                    }
                    // If this was the last attempt, return a synthetic timeout error
                    return Err(BrowserError::CdpError("Navigation timed out".to_string()));
                }
            }
        }

        // If we exhausted retries and still have an error, bail out
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
                    if fallback_navigated {
                        // Poll document.readyState instead of wait_for_navigation()
                        let script = "document.readyState";
                        let start = std::time::Instant::now();
                        loop {
                            let state = self
                                .cdp_page
                                .evaluate(script)
                                .await
                                .ok()
                                .and_then(|r| r.value().and_then(|v| v.as_str().map(|s| s.to_string())));
                            if matches!(state.as_deref(), Some("interactive") | Some("complete")) {
                                break;
                            }
                            if start.elapsed() > std::time::Duration::from_secs(3) {
                                break;
                            }
                            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
                        }
                    } else {
                        // Wait for DOMContentLoaded event
                        self.cdp_page.wait_for_navigation().await?;
                    }
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
                    if fallback_navigated {
                        // Poll for complete state
                        let script = "document.readyState";
                        let start = std::time::Instant::now();
                        loop {
                            let state = self
                                .cdp_page
                                .evaluate(script)
                                .await
                                .ok()
                                .and_then(|r| r.value().and_then(|v| v.as_str().map(|s| s.to_string())));
                            if matches!(state.as_deref(), Some("complete")) {
                                break;
                            }
                            if start.elapsed() > std::time::Duration::from_secs(4) {
                                break;
                            }
                            tokio::time::sleep(tokio::time::Duration::from_millis(120)).await;
                        }
                        // Small cushion after load
                        tokio::time::sleep(tokio::time::Duration::from_millis(300)).await;
                    } else {
                        // Wait for load event
                        self.cdp_page.wait_for_navigation().await?;
                        // Add extra delay to ensure page is fully loaded
                        tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                    }
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

        // Ensure the virtual cursor after navigation
        debug!("Ensuring virtual cursor after navigation");
        if let Err(e) = self.ensure_virtual_cursor().await {
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
        // Always verify and correct viewport before capturing
        if let Err(e) = self.check_and_fix_scaling().await {
            warn!(
                "Failed to check/fix viewport scaling: {}, continuing anyway",
                e
            );
        }

        // Fast path: ensure the virtual cursor exists before capturing
        let injected = match self.ensure_virtual_cursor().await {
            Ok(injected) => injected,
            Err(e) => {
                warn!("Failed to inject virtual cursor: {}", e);
                // Continue with screenshot even if cursor injection fails
                false
            }
        };

        // Allow a brief moment for the cursor SVG to render only if we injected it now
        if injected {
            tokio::time::sleep(tokio::time::Duration::from_millis(60)).await;
        }

        // Wait for any in-flight cursor animation to settle before capture
        if let Ok(remain) = self
            .cdp_page
            .evaluate("(function(){ return (window.__vc && window.__vc.getSettleMs) ? window.__vc.getSettleMs() : 0; })()")
            .await
            .and_then(|r| Ok(r.value().and_then(|v| v.as_u64()).unwrap_or(0)))
        {
            if remain > 0 {
                // Allow a bit more time so screenshots catch the settled state
                let wait_ms = remain.min(800);
                tokio::time::sleep(tokio::time::Duration::from_millis(wait_ms)).await;
            }
        }

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

        // If target is effectively the same as current, avoid dispatching/animating
        if (cursor.x - move_x).abs() < 0.5 && (cursor.y - move_y).abs() < 0.5 {
            drop(cursor);
            // Ensure cursor is present/updated even if no move
            let _ = self.ensure_virtual_cursor().await;
            return Ok(());
        }

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

        // Update cursor position with animation
        let _ = self
            .cdp_page
            .evaluate(format!(
                "window.__vc && window.__vc.update({:.0}, {:.0});",
                move_x, move_y
            ))
            .await;

        // Optionally wait a tiny amount to reduce flicker in immediate snapshots
        // (movement duration is dynamic; we defer full waiting to screenshot path)

        // If using external CDP, wait for animation to settle so the next screenshot captures final state
        let is_external = self.config.connect_port.is_some() || self.config.connect_ws.is_some();
        if is_external {
            let remain = self
                .cdp_page
                .evaluate("(function(){ return (window.__vc && window.__vc.getSettleMs) ? window.__vc.getSettleMs() : 0; })()")
                .await
                .ok()
                .and_then(|r| r.value().and_then(|v| v.as_u64()))
                .unwrap_or(0);
            if remain > 0 {
                let wait_ms = remain.min(3000);
                tokio::time::sleep(tokio::time::Duration::from_millis(wait_ms)).await;
            }
        } else {
            // Tiny delay for internal to keep interactions smooth
            tokio::time::sleep(tokio::time::Duration::from_millis(20)).await;
        }

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

        // Trigger click pulse animation via virtual cursor API and wait briefly
        let click_ms_val = self
            .cdp_page
            .evaluate(
                "(function(){ if(window.__vc && window.__vc.clickPulse){ return window.__vc.clickPulse(); } return 0; })()",
            )
            .await
            .ok()
            .and_then(|r| r.value().and_then(|v| v.as_u64()))
            .unwrap_or(0) as u64;

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

        // Add a small delay between press and release
        tokio::time::sleep(tokio::time::Duration::from_millis(40)).await;

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

        // Wait for click animation to settle before returning (helps next auto-screenshot)
        let is_external = self.config.connect_port.is_some() || self.config.connect_ws.is_some();
        let settle_ms = if is_external { click_ms_val.max(120) } else { 40 };
        if settle_ms > 0 {
            tokio::time::sleep(tokio::time::Duration::from_millis(settle_ms.min(3000))).await;
        }

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
        
        // Update mouse state (track button for drag moves)
        let mut cursor = self.cursor_state.lock().await;
        cursor.is_mouse_down = true;
        cursor.button = MouseButton::Left;
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
        cursor.button = MouseButton::None;
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
            tokio::time::sleep(tokio::time::Duration::from_millis(40)).await;
        }

        // Trigger click animation through virtual cursor API
        let click_ms_val = self
            .cdp_page
            .evaluate(
                "(function(){ if(window.__vc && window.__vc.clickPulse){ return window.__vc.clickPulse(); } return 0; })()",
            )
            .await
            .ok()
            .and_then(|r| r.value().and_then(|v| v.as_u64()))
            .unwrap_or(0) as u64;

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

        // Add a small delay between press and release
        tokio::time::sleep(tokio::time::Duration::from_millis(40)).await;

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

        // Wait so the next auto-screenshot captures the click pulse
        let is_external = self.config.connect_port.is_some() || self.config.connect_ws.is_some();
        let settle_ms = if is_external { click_ms_val.max(120) } else { 40 };
        if settle_ms > 0 {
            tokio::time::sleep(tokio::time::Duration::from_millis(settle_ms.min(3000))).await;
        }

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
                if ch == '\n' {
                    self.press_key("Enter").await?;
                } else if ch == '\t' {
                    self.press_key("Tab").await?;
                } else {
                    let params = DispatchKeyEventParams::builder()
                        .r#type(DispatchKeyEventType::Char)
                        .text(ch.to_string())
                        .build()
                        .map_err(BrowserError::CdpError)?;
                    self.cdp_page.execute(params).await?;
                }

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
                    if ch == '\n' {
                        self.press_key("Enter").await?;
                    } else if ch == '\t' {
                        self.press_key("Tab").await?;
                    } else {
                        let params = DispatchKeyEventParams::builder()
                            .r#type(DispatchKeyEventType::Char)
                            .text(ch.to_string())
                            .build()
                            .map_err(BrowserError::CdpError)?;
                        self.cdp_page.execute(params).await?;
                    }
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
                    if ch == '\n' {
                        self.press_key("Enter").await?;
                    } else if ch == '\t' {
                        self.press_key("Tab").await?;
                    } else {
                        let params = DispatchKeyEventParams::builder()
                            .r#type(DispatchKeyEventType::Char)
                            .text(ch.to_string())
                            .build()
                            .map_err(BrowserError::CdpError)?;
                        self.cdp_page.execute(params).await?;
                    }
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

        // Map key names to their proper codes and virtual key codes
        let (code, text, windows_virtual_key_code, native_virtual_key_code) = match key {
            "Enter" => ("Enter", Some("\r"), Some(13), Some(13)),
            "Tab" => ("Tab", Some("\t"), Some(9), Some(9)),
            "Escape" => ("Escape", None, Some(27), Some(27)),
            "Backspace" => ("Backspace", None, Some(8), Some(8)),
            "Delete" => ("Delete", None, Some(46), Some(46)),
            "ArrowUp" => ("ArrowUp", None, Some(38), Some(38)),
            "ArrowDown" => ("ArrowDown", None, Some(40), Some(40)),
            "ArrowLeft" => ("ArrowLeft", None, Some(37), Some(37)),
            "ArrowRight" => ("ArrowRight", None, Some(39), Some(39)),
            "Home" => ("Home", None, Some(36), Some(36)),
            "End" => ("End", None, Some(35), Some(35)),
            "PageUp" => ("PageUp", None, Some(33), Some(33)),
            "PageDown" => ("PageDown", None, Some(34), Some(34)),
            "Space" => ("Space", Some(" "), Some(32), Some(32)),
            _ => (key, None, None, None), // Default fallback
        };

        // Key down
        let mut down_builder = DispatchKeyEventParams::builder()
            .r#type(DispatchKeyEventType::KeyDown)
            .key(key.to_string())
            .code(code.to_string());
        
        if let Some(vk) = windows_virtual_key_code {
            down_builder = down_builder.windows_virtual_key_code(vk);
        }
        if let Some(nvk) = native_virtual_key_code {
            down_builder = down_builder.native_virtual_key_code(nvk);
        }
        
        let down_params = down_builder.build().map_err(BrowserError::CdpError)?;
        self.cdp_page.execute(down_params).await?;

        // Send char event for keys that produce text
        if let Some(text_str) = text {
            let char_params = DispatchKeyEventParams::builder()
                .r#type(DispatchKeyEventType::Char)
                .key(key.to_string())
                .code(code.to_string())
                .text(text_str.to_string())
                .build()
                .map_err(BrowserError::CdpError)?;
            self.cdp_page.execute(char_params).await?;
        }

        // Key up
        let mut up_builder = DispatchKeyEventParams::builder()
            .r#type(DispatchKeyEventType::KeyUp)
            .key(key.to_string())
            .code(code.to_string());
        
        if let Some(vk) = windows_virtual_key_code {
            up_builder = up_builder.windows_virtual_key_code(vk);
        }
        if let Some(nvk) = native_virtual_key_code {
            up_builder = up_builder.native_virtual_key_code(nvk);
        }
        
        let up_params = up_builder.build().map_err(BrowserError::CdpError)?;
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

        tracing::debug!("JavaScript execution result: {}", result_value);

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

// Raw CDP command wrapper to allow executing arbitrary methods with JSON params
#[derive(Debug, Clone)]
struct RawCdpCommand {
    method: String,
    params: serde_json::Value,
}

impl RawCdpCommand {
    fn new(method: impl Into<String>, params: serde_json::Value) -> Self {
        Self {
            method: method.into(),
            params,
        }
    }
}

impl serde::Serialize for RawCdpCommand {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Serialize only the params as the Command payload
        self.params.serialize(serializer)
    }
}

impl chromiumoxide_types::Method for RawCdpCommand {
    fn identifier(&self) -> chromiumoxide_types::MethodId {
        self.method.clone().into()
    }
}

impl chromiumoxide_types::Command for RawCdpCommand {
    type Response = serde_json::Value;
}

impl Page {
    /// Execute an arbitrary CDP method with the provided JSON params against this page's session
    pub async fn execute_cdp_raw(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<serde_json::Value> {
        let cmd = RawCdpCommand::new(method, params);
        let resp = self.cdp_page.execute(cmd).await?;
        Ok(resp.result)
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
