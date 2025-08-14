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
  const TIP_X = 1, TIP_Y = 1;           // tip calibration (px) — adjust if needed
  const BADGE_OFF_X = -5, BADGE_OFF_Y = -9;

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
    arrow.setAttribute('viewBox', '0 0 40 30');
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

    const blur = createSvg('feGaussianBlur'); blur.setAttribute('in', 'SourceAlpha'); blur.setAttribute('stdDeviation', '1');
    const off  = createSvg('feOffset');       off.setAttribute('dx', '0'); off.setAttribute('dy', '0');
    const ct   = createSvg('feComponentTransfer'); ct.setAttribute('result', 'offsetblur');
    const fa   = createSvg('feFuncA'); fa.setAttribute('type', 'linear'); fa.setAttribute('slope', '0.8'); ct.appendChild(fa);
    const flood = createSvg('feFlood'); flood.setAttribute('flood-color', '#000'); flood.setAttribute('flood-opacity', '0.3');
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
      'M 14.331 9.906 C 14.331 9.906 0.73 0.648 0.701 0.667 L 1.542 16.615 C 1.542 17.315 2.342 17.815 2.942 17.415 L 7.219 12.874 L 13.831 11.606 C 14.631 11.506 14.954 10.465 14.331 9.906 Z'
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
    badge.setAttribute('viewBox', '0 0 40 30');
    badge.setAttribute('aria-hidden', 'true');
    badge.style.position = 'absolute';
    badge.style.transformOrigin = '0 0';
    badge.style.width = BADGE_SIZE_PX + 'px';
    badge.style.height = 'auto';

    const rect = createSvg('rect');
    rect.setAttribute('x','10.82');
    rect.setAttribute('y','18.564');
    rect.setAttribute('width','28.228');
    rect.setAttribute('height','10.691');
    rect.setAttribute('rx','5.3');
    rect.setAttribute('ry','5.3');
    rect.setAttribute('fill','rgb(0, 171, 255)');
    rect.setAttribute('stroke', 'white');
    rect.setAttribute('stroke-width','1');
    rect.setAttribute('vector-effect','non-scaling-stroke');

    const glyphs = createSvg('path');
    glyphs.setAttribute('d',
      'M 19.269 24.657 L 19.96 24.832 Q 19.743 25.683 19.178 26.131 Q 18.613 26.578 17.796 26.578 Q 16.952 26.578 16.422 26.234 Q 15.893 25.89 15.617 25.238 Q 15.341 24.586 15.341 23.837 Q 15.341 23.021 15.652 22.413 Q 15.964 21.805 16.54 21.49 Q 17.116 21.174 17.807 21.174 Q 18.591 21.174 19.126 21.574 Q 19.661 21.973 19.871 22.696 L 19.19 22.857 Q 19.008 22.287 18.663 22.026 Q 18.317 21.766 17.793 21.766 Q 17.191 21.766 16.786 22.055 Q 16.381 22.344 16.217 22.83 Q 16.053 23.317 16.053 23.833 Q 16.053 24.5 16.248 24.997 Q 16.442 25.495 16.852 25.74 Q 17.262 25.986 17.739 25.986 Q 18.32 25.986 18.723 25.651 Q 19.126 25.316 19.269 24.657 Z M 20.491 24.596 Q 20.491 23.545 21.076 23.039 Q 21.564 22.618 22.266 22.618 Q 23.047 22.618 23.542 23.13 Q 24.038 23.641 24.038 24.543 Q 24.038 25.274 23.818 25.692 Q 23.599 26.111 23.18 26.343 Q 22.762 26.575 22.266 26.575 Q 21.471 26.575 20.981 26.065 Q 20.491 25.555 20.491 24.596 Z M 21.15 24.596 Q 21.15 25.323 21.468 25.685 Q 21.785 26.047 22.266 26.047 Q 22.744 26.047 23.061 25.683 Q 23.378 25.32 23.378 24.575 Q 23.378 23.873 23.059 23.511 Q 22.74 23.149 22.266 23.149 Q 21.785 23.149 21.468 23.509 Q 21.15 23.869 21.15 24.596 Z M 27.245 26.489 L 27.245 26.011 Q 26.885 26.575 26.186 26.575 Q 25.734 26.575 25.354 26.325 Q 24.974 26.076 24.766 25.628 Q 24.557 25.181 24.557 24.6 Q 24.557 24.033 24.746 23.571 Q 24.935 23.11 25.313 22.864 Q 25.691 22.618 26.158 22.618 Q 26.5 22.618 26.767 22.762 Q 27.035 22.907 27.202 23.138 L 27.202 21.264 L 27.84 21.264 L 27.84 26.489 Z M 25.217 24.6 Q 25.217 25.327 25.523 25.687 Q 25.83 26.047 26.247 26.047 Q 26.667 26.047 26.961 25.703 Q 27.256 25.359 27.256 24.653 Q 27.256 23.876 26.956 23.513 Q 26.657 23.149 26.218 23.149 Q 25.791 23.149 25.504 23.498 Q 25.217 23.848 25.217 24.6 Z M 31.44 25.27 L 32.103 25.352 Q 31.946 25.933 31.522 26.254 Q 31.098 26.575 30.439 26.575 Q 29.608 26.575 29.122 26.063 Q 28.635 25.552 28.635 24.628 Q 28.635 23.673 29.127 23.146 Q 29.619 22.618 30.403 22.618 Q 31.162 22.618 31.643 23.135 Q 32.125 23.652 32.125 24.589 Q 32.125 24.646 32.121 24.76 L 29.298 24.76 Q 29.334 25.384 29.651 25.716 Q 29.968 26.047 30.442 26.047 Q 30.795 26.047 31.045 25.862 Q 31.294 25.676 31.44 25.27 Z M 29.334 24.233 L 31.447 24.233 Q 31.405 23.755 31.205 23.516 Q 30.899 23.146 30.41 23.146 Q 29.968 23.146 29.667 23.441 Q 29.366 23.737 29.334 24.233 Z M 32.902 26.489 L 32.902 22.704 L 33.479 22.704 L 33.479 23.277 Q 33.7 22.875 33.887 22.746 Q 34.075 22.618 34.299 22.618 Q 34.623 22.618 34.959 22.825 L 34.738 23.42 Q 34.502 23.281 34.267 23.281 Q 34.057 23.281 33.889 23.408 Q 33.722 23.534 33.65 23.759 Q 33.543 24.101 33.543 24.507 L 33.543 26.489 Z'
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
      pxPerSec: 1400,                          // base speed
      min: 500,                                // ms clamp
      max: 2000,                                // ms clamp
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
    const HOVER = {{ opacity: 0.2, radius: 12, enabled: true }};

    let _mx = 0, _my = 0, _rafHover = 0, _dimmed = false;
    function hoverTick() {{
      _rafHover = 0;
      const tipX = state.arrowX + TIP_X;
      const tipY = state.arrowY + TIP_Y;
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
        // Inject the virtual cursor before capturing
        if let Err(e) = self.inject_virtual_cursor().await {
            warn!("Failed to inject virtual cursor: {}", e);
            // Continue with screenshot even if cursor injection fails
        }

        // Allow a brief moment for the cursor SVG to render
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;

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
            .map_err(|e| BrowserError::CdpError(e))?;
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
            .map_err(|e| BrowserError::CdpError(e))?;
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
            .map_err(|e| BrowserError::CdpError(e))?;
        self.cdp_page.execute(up_params).await?;

        // Wait briefly to allow potential event handlers (like navigation) to trigger (ported from TS)
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        Ok(())
    }

    /// Click at the current mouse position without moving the cursor
    pub async fn click_at_current(&self) -> Result<(f64, f64)> {
        // Get the current cursor position
        let cursor = self.cursor_state.lock().await;
        let click_x = cursor.x;
        let click_y = cursor.y;
        debug!("Clicking at current position ({}, {})", click_x, click_y);
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
            .map_err(|e| BrowserError::CdpError(e))?;
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
            .map_err(|e| BrowserError::CdpError(e))?;
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
                    .map_err(|e| BrowserError::CdpError(e))?;
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
                        .map_err(|e| BrowserError::CdpError(e))?;
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
                        .map_err(|e| BrowserError::CdpError(e))?;
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
