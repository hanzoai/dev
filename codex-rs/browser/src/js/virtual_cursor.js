// Virtual Cursor installer (full original code), externalized for easier iteration.
(function () {
  function __vcInstall(x, y) {
    try {
      const ns = 'http://www.w3.org/2000/svg';
      const VIEW_W = 40, VIEW_H = 30;       // original viewBox of your assets
      const ARROW_SIZE_PX = 53;             // arrow visual width (px)
      const BADGE_SIZE_PX = 70;             // badge visual width (px)
      const TIP_X = 3, TIP_Y = 3;           // tip calibration (px) — adjust if needed
      const BADGE_OFF_X = -4, BADGE_OFF_Y = -8;

      function ensureRoot() {
        let root = document.getElementById('__virtualCursorRoot');
        if (!root) {
          root = document.createElement('div');
          root.id = '__virtualCursorRoot';
          Object.assign(root.style, {
            position: 'fixed',
            inset: '0',                 // cover viewport -> non-zero paint area
            pointerEvents: 'none',
            zIndex: '2147483647',
            contain: 'layout style',    // avoid 'paint' or you'll clip to root box
            overflow: 'visible',
          });
          (document.body || document.documentElement).appendChild(root);
        }
        return root;
      }

      function createSvg(tag) { return document.createElementNS(ns, tag); }

      // --- Debug logging helpers ---
      const DEBUG = true;
      function log() { try { console.debug('[VC]', ...arguments); } catch (e) { } }
      function warn() { try { console.warn('[VC]', ...arguments); } catch (e) { } }
      function info() { try { console.info('[VC]', ...arguments); } catch (e) { } }

      // Install once or upgrade from bootstrap version
      if (!window.__vc) {
        const root = ensureRoot();
        log('init', { v: 11, href: location.href, vis: document.visibilityState, prm: (window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches) });

        // --- Arrow SVG container ---
        const arrow = createSvg('svg');
        arrow.setAttribute('viewBox', '0 0 44 34');
        arrow.setAttribute('aria-hidden', 'true');
        arrow.style.position = 'absolute';
        arrow.style.transformOrigin = '0 0';
        arrow.style.width = ARROW_SIZE_PX + 'px';
        arrow.style.height = 'auto';
        // Ensure no clipping when arrow rotates/translates beyond its viewBox
        try { arrow.style.overflow = 'visible'; } catch (_) { }
        try { arrow.setAttribute('overflow', 'visible'); } catch (_) { }
        try { arrow.style.transformBox = 'view-box'; } catch (_) { }

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
        const off = createSvg('feOffset'); off.setAttribute('dx', '0'); off.setAttribute('dy', '0');
        const ct = createSvg('feComponentTransfer'); ct.setAttribute('result', 'offsetblur');
        const fa = createSvg('feFuncA'); fa.setAttribute('type', 'linear'); fa.setAttribute('slope', '0.35'); ct.appendChild(fa);
        const flood = createSvg('feFlood'); flood.setAttribute('flood-color', '#000'); flood.setAttribute('flood-opacity', '0.35');
        const comp = createSvg('feComposite'); comp.setAttribute('in2', 'offsetblur'); comp.setAttribute('operator', 'in');
        const merge = createSvg('feMerge');
        const m1 = createSvg('feMergeNode');
        const m2 = createSvg('feMergeNode'); m2.setAttribute('in', 'SourceGraphic');

        merge.appendChild(m1); merge.appendChild(m2);
        filt.appendChild(blur); filt.appendChild(off); filt.appendChild(ct); filt.appendChild(flood); filt.appendChild(comp); filt.appendChild(merge);
        defs.appendChild(filt);
        arrow.appendChild(defs);

        const arrowInner = createSvg('g');
        // Ensure rotation pivots around the tip; use SVG CSS transform box
        try {
          arrowInner.style.transformBox = 'fill-box';
          arrowInner.style.transformOrigin = TIP_X + 'px ' + TIP_Y + 'px';
        } catch (_) { }
        const arrowPath = createSvg('path');
        arrowPath.setAttribute('d',
          'M 16.63 12.239 C 16.63 12.239 3.029 2.981 3 3 L 3.841 18.948 C 3.841 19.648 4.641 20.148 5.241 19.748 L 9.518 15.207 L 16.13 13.939 C 16.93 13.839 17.253 12.798 16.63 12.239 Z'
        );
        arrowPath.setAttribute('stroke', 'white');
        arrowPath.setAttribute('stroke-width', '1');
        arrowPath.setAttribute('vector-effect', 'non-scaling-stroke');
        arrowPath.setAttribute('fill', 'rgb(0, 171, 255)');
        arrowPath.style.strokeLinejoin = 'round';
        arrowPath.setAttribute('filter', 'url(#vc-drop-shadow)');
        arrowInner.appendChild(arrowPath);
        arrow.appendChild(arrowInner);
        // Ensure arrow renders above the badge
        try { arrow.style.zIndex = '2'; } catch (_) { }

        // --- Badge SVG container ---
        const badge = createSvg('svg');
        badge.setAttribute('viewBox', '0 0 44 34');
        badge.setAttribute('aria-hidden', 'true');
        badge.style.position = 'absolute';
        badge.style.transformOrigin = '0 0';
        badge.style.width = BADGE_SIZE_PX + 'px';
        badge.style.height = 'auto';

        const badgeInner = createSvg('g');
        const rect = createSvg('rect');
        rect.setAttribute('x', '10.82');
        rect.setAttribute('y', '18.564');
        rect.setAttribute('width', '25.686');
        rect.setAttribute('height', '10.691');
        rect.setAttribute('rx', '4');
        rect.setAttribute('ry', '4');
        rect.setAttribute('fill', 'rgb(0, 171, 255)');
        rect.setAttribute('stroke', 'white');
        rect.setAttribute('stroke-width', '1');
        rect.setAttribute('vector-effect', 'non-scaling-stroke');
        rect.setAttribute('filter', 'url(#vc-drop-shadow)');
        badge.appendChild(defs.cloneNode(true));

        const glyphs = createSvg('path');
        glyphs.setAttribute('d',
          'M 19.269 24.657 L 19.96 24.832 C 19.815 25.399 19.555 25.832 19.178 26.131 C 18.801 26.429 18.341 26.578 17.796 26.578 C 17.233 26.578 16.775 26.463 16.422 26.234 C 16.069 26.005 15.801 25.673 15.617 25.238 C 15.433 24.803 15.341 24.336 15.341 23.837 C 15.341 23.293 15.445 22.818 15.652 22.413 C 15.86 22.008 16.156 21.7 16.54 21.49 C 16.924 21.279 17.346 21.174 17.807 21.174 C 18.33 21.174 18.769 21.307 19.126 21.574 C 19.483 21.84 19.731 22.214 19.871 22.696 L 19.19 22.857 C 19.069 22.477 18.893 22.2 18.663 22.026 C 18.432 21.853 18.142 21.766 17.793 21.766 C 17.392 21.766 17.056 21.862 16.786 22.055 C 16.516 22.248 16.326 22.506 16.217 22.83 C 16.108 23.155 16.053 23.489 16.053 23.833 C 16.053 24.278 16.118 24.666 16.248 24.997 C 16.377 25.329 16.579 25.577 16.852 25.74 C 17.125 25.904 17.421 25.986 17.739 25.986 C 18.126 25.986 18.454 25.874 18.723 25.651 C 18.992 25.428 19.174 25.096 19.269 24.657 Z M 20.491 24.596 C 20.491 23.895 20.686 23.376 21.076 23.039 C 21.401 22.758 21.798 22.618 22.266 22.618 C 22.787 22.618 23.212 22.789 23.542 23.13 C 23.873 23.471 24.038 23.942 24.038 24.543 C 24.038 25.03 23.965 25.413 23.818 25.692 C 23.672 25.971 23.459 26.188 23.18 26.343 C 22.901 26.498 22.597 26.575 22.266 26.575 C 21.736 26.575 21.308 26.405 20.981 26.065 C 20.654 25.725 20.491 25.235 20.491 24.596 Z M 21.15 24.596 C 21.15 25.081 21.256 25.444 21.468 25.685 C 21.679 25.926 21.945 26.047 22.266 26.047 C 22.585 26.047 22.85 25.926 23.061 25.683 C 23.272 25.441 23.378 25.072 23.378 24.575 C 23.378 24.107 23.272 23.752 23.059 23.511 C 22.846 23.27 22.582 23.149 22.266 23.149 C 21.945 23.149 21.679 23.269 21.468 23.509 C 21.256 23.749 21.15 24.111 21.15 24.596 Z M 27.245 26.489 L 27.245 26.011 C 27.005 26.387 26.652 26.575 26.186 26.575 C 25.885 26.575 25.607 26.492 25.354 26.325 C 25.101 26.159 24.905 25.927 24.766 25.628 C 24.627 25.33 24.557 24.987 24.557 24.6 C 24.557 24.222 24.62 23.879 24.746 23.571 C 24.872 23.264 25.061 23.028 25.313 22.864 C 25.565 22.7 25.847 22.618 26.158 22.618 C 26.386 22.618 26.589 22.666 26.767 22.762 C 26.946 22.859 27.091 22.984 27.202 23.138 L 27.202 21.264 L 27.84 21.264 L 27.84 26.489 L 27.245 26.489 Z M 25.217 24.6 C 25.217 25.085 25.319 25.447 25.523 25.687 C 25.728 25.927 25.969 26.047 26.247 26.047 C 26.527 26.047 26.765 25.932 26.961 25.703 C 27.158 25.474 27.256 25.124 27.256 24.653 C 27.256 24.135 27.156 23.755 26.956 23.513 C 26.757 23.27 26.511 23.149 26.218 23.149 C 25.933 23.149 25.695 23.265 25.504 23.498 C 25.313 23.731 25.217 24.099 25.217 24.6 Z M 31.44 25.27 L 32.103 25.352 C 31.998 25.739 31.805 26.04 31.522 26.254 C 31.239 26.468 30.878 26.575 30.439 26.575 C 29.885 26.575 29.446 26.404 29.122 26.063 C 28.797 25.722 28.635 25.244 28.635 24.628 C 28.635 23.991 28.799 23.497 29.127 23.146 C 29.455 22.794 29.88 22.618 30.403 22.618 C 30.909 22.618 31.322 22.79 31.643 23.135 C 31.964 23.48 32.125 23.964 32.125 24.589 C 32.125 24.627 32.124 24.684 32.121 24.76 L 29.298 24.76 C 29.322 25.176 29.44 25.495 29.651 25.716 C 29.862 25.937 30.126 26.047 30.442 26.047 C 30.677 26.047 30.878 25.985 31.045 25.862 C 31.211 25.738 31.343 25.541 31.44 25.27 Z M 29.334 24.233 L 31.447 24.233 C 31.419 23.914 31.338 23.675 31.205 23.516 C 31.001 23.269 30.736 23.146 30.41 23.146 C 30.115 23.146 29.868 23.244 29.667 23.441 C 29.466 23.638 29.355 23.902 29.334 24.233 Z'
        );
        glyphs.setAttribute('fill', 'white');

        badgeInner.appendChild(rect);
        badgeInner.appendChild(glyphs);
        badge.appendChild(badgeInner);
        try { badge.style.zIndex = '1'; } catch (_) { }

        // Unified container: wrap (translation, position = rectangle center), wrapInner (click scale + rotation)
        const wrap = document.createElement('div');
        wrap.style.position = 'absolute';
        wrap.style.left = '0';
        wrap.style.top = '0';
        wrap.style.transformOrigin = '0 0';
        wrap.style.willChange = 'transform';
        const wrapInner = document.createElement('div');
        wrapInner.style.transformOrigin = '0 0';

        // Pivot sits at rectangle center (relative to tip). We use transformOrigin on pivot
        // to rotate/orbit around the rectangle center while keeping wrap translating the tip.
        const pivot = document.createElement('div');
        pivot.style.position = 'absolute';
        pivot.style.left = '0px';
        pivot.style.top = '0px';
        pivot.style.transformOrigin = '0 0'; // we'll set a numeric origin later once RCX/RCY known

        // Paint order: put badge first, then arrow so arrow renders above rectangle
        pivot.appendChild(badge);
        pivot.appendChild(arrow);
        wrapInner.appendChild(pivot);
        wrap.appendChild(wrapInner);
        root.appendChild(wrap);
        // Hide briefly on first install to avoid initial flicker during attach/screenshot
        try {
          root.style.visibility = 'hidden';
          setTimeout(() => { try { root.style.visibility = 'visible'; } catch (_) { } }, 150);
        } catch (_) { }

        // Initial state and transforms
        const state = {
          root, wrap, wrapInner, pivot, arrow, arrowInner, badge,
          styleEl: null,
          wrapX: Math.round(x),
          wrapY: Math.round(y),
          aAnim: null,
          bAnim: null,
          caAnim: null,
          cbAnim: null,
          cssActiveA: false,
          cssActiveB: false,
          lastMoveAt: 0,
          lastAx: Math.round(x),
          lastAy: Math.round(y),
          lastDur: 0,
          cancelLog: [],
          ignoreHoverUntil: 0,
          curveFlip: false,
        };

        // Initial transforms: wrap at tip (0,0), arrow and badge offset relative to tip
        const RECT_CX = 10.82 + 25.686 / 2;
        const RECT_CY = 18.564 + 10.691 / 2;
        // Badge top-left relative to tip
        const BADGE_TLX = (BADGE_OFF_X - TIP_X);
        const BADGE_TLY = (BADGE_OFF_Y - TIP_Y);
        // Rectangle center relative to tip
        const RCX = BADGE_TLX + RECT_CX;
        const RCY = BADGE_TLY + RECT_CY;

        wrap.style.transform = 'translate3d(' + state.wrapX + 'px,' + state.wrapY + 'px,0)';
        // Arrow so its tip is at (0,0)
        arrow.style.transform = 'translate3d(' + (-TIP_X) + 'px,' + (-TIP_Y) + 'px,0)';
        // Badge positioned by its top-left offset from tip
        badge.style.transform = 'translate3d(' + BADGE_TLX + 'px,' + BADGE_TLY + 'px,0)';
        // Rotate wrapInner around rectangle center (relative to tip)
        try {
          wrapInner.style.transformOrigin = RCX + 'px ' + RCY + 'px';
          pivot.style.transformOrigin = RCX + 'px ' + RCY + 'px';
        } catch (_) { }
        // Create style element for dynamic CSS keyframes
        try {
          const st = document.createElement('style');
          st.type = 'text/css';
          st.id = '__vc_css_kf';
          (document.head || document.documentElement || root).appendChild(st);
          state.styleEl = st;
        } catch (_) { }
        arrow.style.willChange = 'transform';
        badge.style.willChange = 'transform';
        arrow.style.transformOrigin = '0 0';
        badge.style.transformOrigin = '0 0';

      // Motion configuration (CSS-only animations)
      const MOTION = {
        min_dist: 300,
        max_dist: 1000,
        min_ms: 600,
        max_ms: 2000,
        easing: 'cubic-bezier(.25,.1,.25,1)', // soft ease-out
        arrowScale: 1.0,
        badgeScale: 1.02,
        badgeDelay: 35,
        cssDurationMs: 0,                         // when >0, force CSS duration (diagnostic override)
        honorReducedMotion: false,
        curveFactor: 0.25,                        // curved path
        curveMaxPx: 70,
        curveAlternate: true,
        rotateMaxDeg: 28,
        arrowTilt: 0.5,
        badgeTilt: 0.3,
        overshootDeg: 10,
        arrowOvershootScale: 1.0,
        badgeOvershootScale: 0.7,
        overshootAt: 0.92,
        orbitMode: 'quad',                        // 'quad' | 'normal' | 'pivot' | 'none'
        orbitBiasDown: 2.0,                       // multiplier for downward orbit
        orbitBasePx: 16,                          // max orbit radius at full-screen distance
          orbitMinPx: 100,                          // min distance (px) before orbit engages
          backBegin: 0.40                          // when to start rotating back (0..1)
      };

        function dist(x0, y0, x1, y1) {
          const dx = x1 - x0, dy = y1 - y0;
          return Math.hypot(dx, dy);
        }

        function durationForDistance(d) {
          // pull values and provide sensible defaults (snappy by default)
          let minDist = Number(MOTION.min_dist ?? 0);
          let maxDist = Number(MOTION.max_dist ?? (minDist + 1)); // avoid zero range
          let minMs   = Number(MOTION.min_ms   ?? 100);
          let maxMs   = Number(MOTION.max_ms   ?? 300);

          // If the user accidentally supplied reversed distances, fix by swapping
          if (minDist > maxDist) {
            [minDist, maxDist] = [maxDist, minDist];
            [minMs,   maxMs]   = [maxMs,   minMs];
          }

          // If distances are equal after defaults, treat as step
          if (minDist === maxDist) return d <= minDist ? minMs : maxMs;

          // normalized t in [0,1]
          const t = Math.max(0, Math.min(1, (d - minDist) / (maxDist - minDist)));

          // linear interpolation
          return Math.round(minMs + t * (maxMs - minMs));
        }

        function commit(wx, wy, _bx, _by) {
          state.wrapX = wx; state.wrapY = wy;
        }

        // Ensure elements use their currently computed transform as the inline baseline
        function pinCurrent(el) {
          try {
            const cs = getComputedStyle(el);
            const t = cs && cs.transform;
            if (t && t !== 'none') {
              // Set inline to the current computed transform to avoid visual jumps on cancel
              el.style.transform = t;
            }
          } catch (e) { }
        }

        // Helper: apply CSS transition from current to target transform
        function cssAnim(el, which, curX, curY, nextX, nextY, durMs, easing, delayMs) {
          try {
            el.style.transition = 'none';
            el.style.transform = 'translate3d(' + Math.round(curX) + 'px,' + Math.round(curY) + 'px,0)';
            void el.offsetWidth; // reflow
            el.style.transition = 'transform ' + durMs + 'ms ' + easing + (delayMs > 0 ? (' ' + (delayMs | 0) + 'ms') : '');
            el.style.transform = 'translate3d(' + Math.round(nextX) + 'px,' + Math.round(nextY) + 'px,0)';
            if (which === 'a') state.cssActiveA = true; else if (which === 'b') state.cssActiveB = true;
            const onEnd = () => { if (which === 'a') state.cssActiveA = false; else state.cssActiveB = false; el.removeEventListener('transitionend', onEnd); };
            el.addEventListener('transitionend', onEnd);
          } catch (e) { warn('cssAnim error', e); }
        }

        // Helpers to inject and run CSS keyframes
        function addKeyframes(name, cssText) {
          try {
            if (!state.styleEl) return false;
            state.styleEl.textContent += "\n@keyframes " + name + " {\n" + cssText + "\n}\n";
            return true;
          } catch (e) { warn('addKeyframes error', e); return false; }
        }
        function playKeyframes(el, name, dur, easing, delay) {
          try {
            el.style.animation = 'none';
            void el.offsetWidth; // reflow
            el.style.animation = name + ' ' + dur + 'ms ' + easing + ' ' + ((delay | 0)) + 'ms forwards';
          } catch (e) { warn('playKeyframes error', e); }
        }

        function moveTo(nx, ny, opts) {
          const o = Object.assign({}, MOTION, opts || {});

          const wx1 = Math.round(nx), wy1 = Math.round(ny);
          const wx0 = state.wrapX, wy0 = state.wrapY;

          const now = (window.performance && performance.now) ? performance.now() : Date.now();
          const sincePrev = now - (state.lastMoveAt || 0);
          const coalesceMs = 80;
          const noOp = (Math.abs(wx1 - wx0) < 0.5) && (Math.abs(wy1 - wy0) < 0.5);
          if (noOp) { log('moveTo no-op', { wx1, wy1 }); return 0; }
          const d = dist(wx0, wy0, wx1, wy1);
          // For tiny moves, snap without animation to avoid visible twitch
          if (d < 1.5) {
            try { state.aAnim && state.aAnim.cancel(); } catch (e) { }
            try { state.bAnim && state.bAnim.cancel(); } catch (e) { }
            wrap.style.transform = 'translate3d(' + wx1 + 'px,' + wy1 + 'px,0)';
            try { arrowInner.style.transform = 'translate(0px,0px) rotate(0deg)'; } catch (_) { }
            commit(wx1, wy1, 0, 0);
            state.lastMoveAt = now; state.lastAx = wx1; state.lastAy = wy1;
            return 0;
          }

          const base = durationForDistance(d);
          let aDur = Math.round(base * o.arrowScale);
          let bDur = 0; const bDel = 0; // unified motion
          log('moveTo', { from: { x: wx0, y: wy0 }, to: { x: wx1, y: wy1 }, d, base, aDur, engine: o.engine, easing: o.easing, waapi: (typeof wrap.animate === 'function'), sincePrev });
          // During programmatic movement, suppress hover dimming so synthetic mousemove doesn't dim the cursor
          try {
            const totalPlan = Math.max(aDur, (bDel | 0) + bDur);
            state.ignoreHoverUntil = Math.max(state.ignoreHoverUntil, now + totalPlan + 40);
            hoverClearUntil = Math.max(hoverClearUntil, now + totalPlan + 40);
          } catch (_) { }

          const dx = wx1 - wx0, dy = wy1 - wy0;
          const distNorm = Math.min(1, d / 80);
          const dir = dx >= 0 ? 1 : -1; // rotation only depends on horizontal direction
          const tiltBase = o.rotateMaxDeg * distNorm;
          const bRotTarget = dir * tiltBase * o.badgeTilt;

          // Pin current visual state.
          pinCurrent(wrap);

          // Orbit midpoints based on selected orbit mode
          let midRot = 0;
          const enableOrbit = d >= (o.orbitMinPx || 150);
          const orbitScale = enableOrbit ? Math.max(0, Math.min(0.6, (d - 100) * 0.0006)) : 0; // 0..0.6 (over 1000)

          // Quadrant-based poses provided by user
          if (dx < 0 && dy > 0) {
            midRot = Math.round(-107 * orbitScale);
          }
          else if (dx > 0 && dy > 0) {
            midRot = Math.round(153 * orbitScale);
          }
          else if (dx > 0 && dy < 0) {
            midRot = Math.round(-107 * orbitScale);
          }
          else { midRot = 0; }
  
          log('orbit', { midRot, orbitScale });

          const bTilt = Math.round(bRotTarget);


          // CSS-only movement and rotation
          const dur = (o.cssDurationMs && o.cssDurationMs > 0) ? (o.cssDurationMs | 0) : aDur;
          const retarget = sincePrev <= coalesceMs;
          if (retarget && (state.cssActiveA || state.cssActiveB)) {
            log('css retarget', { dur, sincePrev, cssActiveA: state.cssActiveA, cssActiveB: state.cssActiveB });
            wrap.style.transform = 'translate3d(' + wx1 + 'px,' + wy1 + 'px,0)';
          } else {
            log('css-mode begin', { dur, easing: o.easing });
            cssAnim(wrap, 'a', wx0, wy0, wx1, wy1, dur, o.easing, 0);
          }
          // CSS-only orbit using injected keyframes on arrowInner/pivot
          try {

            // Unique names per run
            state.seq = (state.seq | 0) + 1;
            const anArrow = '__vc_arrow_' + state.seq;
            const anPivot = '__vc_pivot_' + state.seq;
            const baseA = 'translate3d(' + (-TIP_X) + 'px,' + (-TIP_Y) + 'px,0) ';
            // Quadrant-based transform-origin and rotation (CSS path)
            // Default to rectangle center for non-quad modes
            let ORI_X = 36, ORI_Y = 31;
            let cssDeg = midRot;
            const qScale = orbitScale; // 0..1 based on distance
            if (dx <= 0 && dy <= 0) {
              // top left (no change to arrow orbit)
              ORI_X = 0; ORI_Y = 0; cssDeg = Math.round(0 * qScale);
            } else if (dx <= 0 && dy > 0) {
              // bottom left
              ORI_X = 17; ORI_Y = 29; cssDeg = Math.round(-90 * qScale);
            } else if (dx > 0 && dy <= 0) {
              // top right
              ORI_X = 33; ORI_Y = 40; cssDeg = Math.round(90 * qScale);
            } else {
              // bottom right (use -179deg to force anticlockwise)
              //ORI_X = 32; ORI_Y = 28; cssDeg = Math.round(-179 * qScale);
              // Use top right as bottom right looks weird
              ORI_X = 33; ORI_Y = 40; cssDeg = Math.round(90 * qScale);
            }
            // Arrow keyframes: rotate to quadrant target then return gradually.
            // Reset transform-origin to 0 0 at end so clicks look correct.
            const arrowKF =
              '0%{transform:' + baseA + 'rotate(0deg);transform-origin:' + ORI_X + 'px ' + ORI_Y + 'px}\n' +
              '10%{transform:' + baseA + 'rotate(' + cssDeg + 'deg);transform-origin:' + ORI_X + 'px ' + ORI_Y + 'px}\n' +
              '99%{transform:' + baseA + 'rotate(0deg);transform-origin:' + ORI_X + 'px ' + ORI_Y + 'px}\n' +
              '100%{transform:' + baseA + 'rotate(0deg);transform-origin:0px 0px}';
            const pKFcss =
              '0%{transform:rotate(0deg)}\n' +
              '50%{transform:rotate(' + bTilt + 'deg)}\n' +
              '100%{transform:rotate(0deg)}';
            addKeyframes(anArrow, arrowKF);
            addKeyframes(anPivot, pKFcss);
            playKeyframes(arrow, anArrow, dur, o.easing, 0);
            if (bTilt !== 0) playKeyframes(pivot, anPivot, dur, o.easing, Math.round(dur*0.08));
          } catch (_) { }

          commit(wx1, wy1, 0, 0);
          state.lastMoveAt = now; state.lastAx = wx1; state.lastAy = wy1; state.lastDur = dur;
          if (window.__vc && window.__vc._overlay && window.__vc._overlayUpdate) window.__vc._overlayUpdate();
          return dur;
          
        }

        // --- Hover-to-dim (distance to tip) ---
        root.style.opacity = '1';
        root.style.transition = 'opacity 160ms ease-out';
        // Avoid hover dimming right after install to reduce perceived flicker
        try {
          const nowTS = (window.performance && performance.now) ? performance.now() : Date.now();
          state.ignoreHoverUntil = nowTS + 600;
        } catch (_) { }
        const HOVER = { opacity: 0.2, offset: 20, radius: 55, enabled: true };
        let hoverClearUntil = 0;

        let _mx = 0, _my = 0, _rafHover = 0, _dimmed = false;
        function hoverTick() {
          _rafHover = 0;
          const now = (window.performance && performance.now) ? performance.now() : Date.now();
          if (now < state.ignoreHoverUntil || now < hoverClearUntil) {
            // Ignore hover updates during synthetic/programmatic moves
            if (_dimmed) { _dimmed = false; root.style.opacity = '1'; }
            return;
          }
          const tipX = state.wrapX + HOVER.offset;
          const tipY = state.wrapY + HOVER.offset;
          const dx = _mx - tipX, dy = _my - tipY;
          const over = (dx * dx + dy * dy) <= (HOVER.radius * HOVER.radius);
          const shouldDim = HOVER.enabled && over;
          if (shouldDim !== _dimmed) {
            _dimmed = shouldDim;
            root.style.opacity = shouldDim ? String(HOVER.opacity) : '1';
          }
        }
        function scheduleHover(ev) {
          // Ignore synthetic/injected events and updates during suppression window
          try { if (ev && ev.isTrusted === false) return; } catch (_) { }
          const t = (window.performance && performance.now) ? performance.now() : Date.now();
          if (t < state.ignoreHoverUntil || t < hoverClearUntil) return;
          _mx = ev.clientX; _my = ev.clientY;
          if (!_rafHover) _rafHover = requestAnimationFrame(hoverTick);
        }
        window.addEventListener('mousemove', scheduleHover, { passive: true });
        window.addEventListener('mouseleave', function () {
          if (_dimmed) { _dimmed = false; root.style.opacity = '1'; }
        }, { passive: true });

        // Public API
        window.__vc = {
          moveTo: moveTo,                 // preferred; returns ms duration
          update: function (nx, ny) {     // backwards compat; returns ms duration
            return moveTo(nx, ny);
          },
          // Snap instantly without WAAPI (host-driven stepping can use this)
          snapTo: function (nx, ny) {
            try { state.aAnim && state.aAnim.cancel(); } catch (e) { }
            try { state.bAnim && state.bAnim.cancel(); } catch (e) { }
            try { state.raAnim && state.raAnim.cancel(); } catch (e) { }
            wrap.style.transform = 'translate3d(' + Math.round(nx) + 'px,' + Math.round(ny) + 'px,0)';
            try { arrowInner.style.transform = 'translate(0px,0px) rotate(0deg)'; } catch (_) { }
            commit(Math.round(nx), Math.round(ny), 0, 0);
            return true;
          },
          // Click pulse animation: scale wrapInner + ripple at tip (CSS-only)
          clickPulse: function (opts) {
            const dur = (opts && opts.duration) || 550;
            // Suppress hover dimming during click pulse
            try {
              const nowTS = (window.performance && performance.now) ? performance.now() : Date.now();
              const until = nowTS + (dur + 120);
              state.ignoreHoverUntil = Math.max(state.ignoreHoverUntil, until);
              hoverClearUntil = Math.max(hoverClearUntil, until);
              // proactively clear any dim
              try { root.style.opacity = '1'; } catch (_) { }
            } catch (e) { }
            // Scale on wrapInner from top-left via CSS keyframes
            try {
              state.seq = (state.seq | 0) + 1;
              const aName = '__vc_click_scale_' + state.seq;
              addKeyframes(aName,
                '0%{transform:scale(1); transform-origin:-5px -5px;}\n' +
                '48%{transform:scale(0.8); transform-origin:-5px -5px;}\n' +
                '52%{transform:scale(0.8); transform-origin:-5px -5px;}\n' +
                '99%{transform:scale(1); transform-origin:-5px -5px;}\n' +
                '100%{transform-origin:0px 0px;}');
              playKeyframes(state.wrapInner, aName, dur, 'cubic-bezier(0.16, 1, 0.3, 1)', 0);
            } catch (_) { }
            // Transient ring ripple near the tip for visibility
            try {
              const ring = document.createElement('div');
              ring.className = '__vc_click_ring';
              const tipX = state.wrapX; // tip coincides with wrap origin
              const tipY = state.wrapY;
              const sz = 18; // ring base size
              Object.assign(ring.style, {
                position: 'absolute',
                left: (tipX - sz / 2) + 'px',
                top: (tipY - sz / 2) + 'px',
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
              });
              state.root.appendChild(ring);
              state.seq = (state.seq | 0) + 1;
              const rName = '__vc_click_ring_' + state.seq;
              addKeyframes(rName,
                '0%{transform:scale(0.6);opacity:0.9}\n' +
                '100%{transform:scale(1.8);opacity:0}');
              playKeyframes(ring, rName, 480, 'cubic-bezier(0.22, 1, 0.36, 1)', 0);
              ring.addEventListener('animationend', function onEnd() { try { ring.remove(); } catch (e) { } });
            } catch (e) { }

            return dur * 2 + 80; // approximate total
          },
          // Return an estimated remaining time (ms) for any in-flight animations
          getSettleMs: function () {
            try {
              const nowTS = (window.performance && performance.now) ? performance.now() : Date.now();
              const elapsed = Math.max(0, nowTS - (state.lastMoveAt || 0));
              const left = Math.max(0, (state.lastDur || 0) - elapsed);
              return Math.ceil(left);
            } catch (_) { return 0; }
          },
          setSize: function (arrowPx, badgePx) {
            if (arrowPx) arrow.style.width = arrowPx + 'px';
            if (badgePx) badge.style.width = badgePx + 'px';
          },
          setMotion: function (p) { Object.assign(MOTION, p || {}); if (window.__vc && window.__vc._overlay && window.__vc._overlayUpdate) window.__vc._overlayUpdate(); },
          dump: function () {
            try {
              const tf = (el) => el ? getComputedStyle(el).transform : null;
              console.debug('[VC/dump]', {
                orbitMode: MOTION.orbitMode, orbitMinPx: MOTION.orbitMinPx,
                wrap: tf(state.wrap), wrapInner: tf(state.wrapInner), pivot: tf(state.pivot),
                arrow: tf(state.arrow), badge: tf(state.badge),
                wrapPos: { x: state.wrapX, y: state.wrapY }, lastDur: state.lastDur
              });
              return true;
            } catch (e) { console.warn('[VC/dump] error', e); return false; }
          },
          setHover: function (p) { Object.assign(HOVER, p || {}); },
          setOrbitMode: function (m) { if (m) MOTION.orbitMode = String(m); return MOTION.orbitMode; },
          getOrbitMode: function () { return MOTION.orbitMode; },
          // debugOrbit removed (WAAPI-free)
          setDebug: function (flag) {
            try {
              if (!flag) {
                if (window.__vc && window.__vc._overlay) { window.__vc._overlay.remove(); window.__vc._overlay = null; }
                return true;
              }
              if (!window.__vc._overlay) {
                const el = document.createElement('div');
                el.style.cssText = 'position:fixed;bottom:8px;left:8px;background:rgba(0,0,0,0.6);color:#fff;padding:6px 8px;border-radius:6px;font:12px/1.4 -apple-system,Segoe UI,Arial;z-index:2147483647;pointer-events:none;white-space:pre;';
                window.__vc._overlay = el;
                document.body.appendChild(el);
              }
              window.__vc._overlayUpdate = function () {
                try {
                  const txt = 'orbit: ' + (MOTION.orbitMode || '') + '\n' +
                    'target: (' + state.arrowX + ',' + state.arrowY + ')\n' +
                    'current: cssA:' + (state.cssActiveA ? 1 : 0) + ' cssB:' + (state.cssActiveB ? 1 : 0) + '\n' +
                    'last dur: ' + (state.lastDur || 0) + 'ms\n' +
                    'cancels: ' + state.cancelLog.length + '\n' +
                    'lastMoveAt: +' + Math.round(state.lastMoveAt || 0) + 'ms';
                  window.__vc._overlay.textContent = txt;
                } catch (e) { }
              };
              window.__vc._overlayUpdate();
              return true;
            } catch (e) { return false; }
          },
          // Quick random move helpers for testing
          testMove: function (dx, dy, opts) {
            try {
              const s = window.__vc && window.__vc._s; if (!s) return 0;
              let x = s.wrapX + (typeof dx === 'number' ? dx : 0);
              let y = s.wrapY + (typeof dy === 'number' ? dy : 0);
              if (typeof dx !== 'number' || typeof dy !== 'number') {
                const W = (window.innerWidth || 1024), H = (window.innerHeight || 768);
                x = Math.max(20, Math.min(W - 20, Math.round(Math.random() * W)));
                y = Math.max(20, Math.min(H - 20, Math.round(Math.random() * H)));
              }
              return window.__vc.moveTo(x, y, opts || null);
            } catch (e) { console.warn('[VC/testMove] error', e); return 0; }
          },
          randomWalk: function (count, opts) {
            count = (count | 0) || 4;
            let i = 0;
            function step() {
              if (i++ >= count) return true;
              const ms = window.__vc.testMove(null, null, opts || null) || 800;
              setTimeout(step, Math.max(80, ms + 120));
            }
            step();
            return true;
          },
          destroy: function () {
            // During programmatic movement, suppress hover dimming as CDP will fire mousemove events.
            // Use remaining animation time if available, otherwise a small fixed window.
            try {
              const nowTS = (window.performance && performance.now) ? performance.now() : Date.now();
              const rem = (window.__vc && typeof window.__vc.getSettleMs === 'function') ? window.__vc.getSettleMs() : 0;
              const total = Math.max(160, rem + 40);
              state.ignoreHoverUntil = nowTS + total;
            } catch (e) { }
            window.removeEventListener('mousemove', scheduleHover);
            if (root && root.parentNode) root.parentNode.removeChild(root);
            window.__vc = null;
          },
          __bootstrap: false,
          __version: 11,
          _s: state
        };

        // Go to initial position
        window.__vc.moveTo(x, y);

      } else {
        // Already installed; just move to the new position
        window.__vc.moveTo(x, y);
      }
      return (typeof window.__vc === 'object') ? 'ok' : 'missing';
    } catch (e) {
      try { console.error('[VC] inject error', e); } catch (_) { }
      return 'error:' + (e && (e.message || e))
    }
  }
  try { Object.defineProperty(window, '__vcInstall', { value: __vcInstall, configurable: true, writable: true }); }
  catch (_) { window.__vcInstall = __vcInstall; }
})();
