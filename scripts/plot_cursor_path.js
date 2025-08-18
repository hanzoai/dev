#!/usr/bin/env node
/**
 * Plot the virtual cursor path used in codex-rs/browser/src/page.rs.
 *
 * It mirrors the current algorithm: distance-based duration, optional curved midpoint,
 * rotation-driven overshoot timing (translation holds final after ~82%), and easing.
 *
 * Usage:
 *   node scripts/plot_cursor_path.js --from 100,100 --to 900,520 --out /tmp/cursor_path.svg
 *   node scripts/plot_cursor_path.js --from 200,300 --to 400,350 --no-curve --pxPerSec 120 --min 600 --max 4200
 */

const fs = require('fs');

function parsePoint(s) {
  const [x, y] = String(s).split(',').map(Number);
  if (!Number.isFinite(x) || !Number.isFinite(y)) throw new Error(`Invalid point: ${s}`);
  return { x, y };
}

function parseArgs(argv) {
  const args = {
    from: { x: 120, y: 90 },
    to: { x: 860, y: 520 },
    out: 'cursor_path.svg',
    fps: 60,
    // Mirror MOTION constants from page.rs (current tuning)
    pxPerSec: 120,
    min: 600,
    max: 4200,
    easing: 'cubic-bezier(0.18, 0.9, 0.18, 1)',
    arrowScale: 1.50,
    badgeScale: 1.70,
    badgeDelay: 40,
    jitter: 0,
    rotateMaxDeg: 16,
    arrowTilt: 0.85,
    badgeTilt: 0.60,
    overshootDeg: 4.0,
    arrowOvershootScale: 0.5,
    badgeOvershootScale: 0.85,
    overshootAt: 0.7,
    curveFactor: 0.25,
    curveMaxPx: 70,
    curveAlternate: true,
    tipX: 0,
    tipY: 0,
    badgeOffX: 12,
    badgeOffY: 4,
    noCurve: false,
  };

  for (let i = 2; i < argv.length; i++) {
    const a = argv[i];
    const next = argv[i + 1];
    const setNum = (k, v) => { if (v === undefined) throw new Error(`Missing value for ${k}`); args[k] = Number(v); i++; };
    switch (a) {
      case '--from': args.from = parsePoint(next); i++; break;
      case '--to': args.to = parsePoint(next); i++; break;
      case '--out': args.out = next; i++; break;
      case '--fps': setNum('fps', next); break;
      case '--pxPerSec': setNum('pxPerSec', next); break;
      case '--min': setNum('min', next); break;
      case '--max': setNum('max', next); break;
      case '--no-curve': args.noCurve = true; break;
      default:
        if (a.startsWith('--')) {
          const k = a.slice(2);
          if (k in args) {
            const val = next;
            if (val === undefined) throw new Error(`Missing value for ${a}`);
            const n = Number(val);
            args[k] = Number.isFinite(n) ? n : val;
            i++;
          }
        }
    }
  }

  return args;
}

function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }
function dist(x0, y0, x1, y1) { const dx = x1 - x0, dy = y1 - y0; return Math.hypot(dx, dy); }
function lerp(a, b, t) { return a + (b - a) * t; }
function lerp2(a, b, t) { return { x: lerp(a.x, b.x, t), y: lerp(a.y, b.y, t) }; }

// Evaluate cubic-bezier easing at t; return y for x=t mapping
function cubicBezierAt(p1x, p1y, p2x, p2y, t) {
  // Borrowed from https://github.com/gre/bezier-easing (MIT) inlined minimal.
  function A(aA1, aA2) { return 1.0 - 3.0 * aA2 + 3.0 * aA1; }
  function B(aA1, aA2) { return 3.0 * aA2 - 6.0 * aA1; }
  function C(aA1)      { return 3.0 * aA1; }
  function calcBezier(aT, aA1, aA2) { return ((A(aA1, aA2) * aT + B(aA1, aA2)) * aT + C(aA1)) * aT; }
  function getSlope(aT, aA1, aA2)  { return 3.0 * A(aA1, aA2) * aT * aT + 2.0 * B(aA1, aA2) * aT + C(aA1); }

  function getTforX(aX) {
    // Newton-Raphson
    let aT = aX;
    for (let i = 0; i < 4; i++) {
      const slope = getSlope(aT, p1x, p2x);
      if (slope === 0) return aT;
      const x = calcBezier(aT, p1x, p2x) - aX;
      aT -= x / slope;
    }
    return aT;
  }

  const x = getTforX(t);
  const y = calcBezier(x, p1y, p2y);
  return y;
}

function durationForDistance(d, motion) {
  let ms = (d / Math.max(1, motion.pxPerSec)) * 1000;
  if (motion.jitter > 0) {
    const j = (Math.random() * 2 - 1) * motion.jitter;
    ms = ms * (1 + j);
  }
  ms = Math.min(motion.max, Math.max(motion.min, ms));
  return Math.round(ms);
}

function computePath(args) {
  const o = { ...args };
  const ax0 = Math.round(args.from.x - o.tipX);
  const ay0 = Math.round(args.from.y - o.tipY);
  const ax1 = Math.round(args.to.x   - o.tipX);
  const ay1 = Math.round(args.to.y   - o.tipY);
  const bx0 = ax0 + o.badgeOffX, by0 = ay0 + o.badgeOffY;
  const bx1 = ax1 + o.badgeOffX, by1 = ay1 + o.badgeOffY;

  const d = dist(ax0, ay0, ax1, ay1);
  const base = durationForDistance(d, o);
  const aDur = Math.round(base * o.arrowScale);
  const bDur = Math.round(base * o.badgeScale);

  // Curve midpoint
  let mx = (ax0 + ax1) / 2;
  let my = (ay0 + ay1) / 2;
  if (!o.noCurve) {
    const dx = ax1 - ax0, dy = ay1 - ay0;
    const len = Math.hypot(dx, dy) || 1;
    const nqx = -dy / len, nqy = dx / len;
    const curveMag = Math.min(o.curveMaxPx || 0, Math.max(0, d * (o.curveFactor || 0)));
    const curveSign = 1; // stateless plot (no alternation)
    mx = Math.round(mx + nqx * curveMag * curveSign);
    my = Math.round(my + nqy * curveMag * curveSign);
  }

  const offsets = [0.0, 0.5, 0.82, 0.90, 1.0];
  for (let i = 1; i < offsets.length; i++) {
    if (!(offsets[i] > offsets[i - 1])) throw new Error('Non-increasing keyframe offsets');
  }

  const pts = [ {x: ax0, y: ay0}, {x: mx, y: my}, {x: ax1, y: ay1} ];
  const easing = o.easing;
  const m = easing.match(/cubic-bezier\(([^)]+)\)/);
  const bez = m ? m[1].split(',').map(Number).map(v => clamp(v, -10, 10)) : [0.25, 1, 0.5, 1];

  const samples = [];
  const totalMs = aDur;
  const step = Math.max(1, Math.round(1000 / o.fps));
  for (let tms = 0; tms <= totalMs; tms += step) {
    const t = tms / totalMs; // 0..1
    let segStart = 0, segEnd = 0.5, A = pts[0], B = pts[1];
    if (t >= 0.5 && t < 0.82) { segStart = 0.5; segEnd = 0.82; A = pts[1]; B = pts[2]; }
    else if (t >= 0.82) { segStart = 0.82; segEnd = 1.0; A = pts[2]; B = pts[2]; }
    const local = (t - segStart) / Math.max(1e-6, (segEnd - segStart));
    const eased = cubicBezierAt(bez[0], bez[1], bez[2], bez[3], clamp(local, 0, 1));
    const P = lerp2(A, B, eased);
    samples.push({ tms, x: P.x, y: P.y });
  }

  const fallback = [];
  for (let tms = 0; tms <= totalMs; tms += step) {
    const t = tms / totalMs;
    const eased = cubicBezierAt(0.25, 1, 0.5, 1, t);
    const P = lerp2({x: ax0, y: ay0}, {x: ax1, y: ay1}, eased);
    fallback.push({ tms, x: P.x, y: P.y });
  }

  return { aDur, bDur, pts: { start: {x: ax0, y: ay0}, mid: {x: mx, y: my}, end: {x: ax1, y: ay1} }, samples, fallback };
}

function toSVG(data, width, height, margin = 20) {
  // Fit points into the viewport
  const xs = data.samples.map(p => p.x).concat([data.pts.start.x, data.pts.mid.x, data.pts.end.x]);
  const ys = data.samples.map(p => p.y).concat([data.pts.start.y, data.pts.mid.y, data.pts.end.y]);
  const minX = Math.min(...xs), maxX = Math.max(...xs);
  const minY = Math.min(...ys), maxY = Math.max(...ys);
  const spanX = Math.max(1, maxX - minX);
  const spanY = Math.max(1, maxY - minY);

  const sx = (x) => margin + (x - minX) * (width - 2*margin) / spanX;
  const sy = (y) => margin + (y - minY) * (height - 2*margin) / spanY;

  const path = data.samples.map((p, i) => `${i===0?'M':'L'} ${sx(p.x).toFixed(1)} ${sy(p.y).toFixed(1)}`).join(' ');
  const fbPath = data.fallback.map((p, i) => `${i===0?'M':'L'} ${sx(p.x).toFixed(1)} ${sy(p.y).toFixed(1)}`).join(' ');

  const dots = [data.pts.start, data.pts.mid, data.pts.end]
    .map((p, i) => `<circle cx="${sx(p.x).toFixed(1)}" cy="${sy(p.y).toFixed(1)}" r="3" fill="${['#2a9d8f','#f4a261','#e76f51'][i]}"/>`).join('\n      ');

  return `<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">
  <rect x="0" y="0" width="100%" height="100%" fill="#0b0d10"/>
  <g stroke="#444" stroke-width="1" fill="none" opacity="0.7">
    ${Array.from({length: 10}).map((_,i)=>`<line x1="${margin}" x2="${width-margin}" y1="${margin + i*(height-2*margin)/10}" y2="${margin + i*(height-2*margin)/10}"/>`).join('\n    ')}
  </g>
  <path d="${fbPath}" stroke="#6c757d" stroke-dasharray="4 3" stroke-width="2" fill="none"/>
  <path d="${path}" stroke="#00abff" stroke-width="2.5" fill="none"/>
  ${dots}
  <text x="${margin}" y="${height - margin/2}" fill="#ddd" font-family="monospace" font-size="12">
    curved (blue) vs fallback (gray), duration: ${data.aDur}ms
  </text>
</svg>`;
}

function main() {
  const args = parseArgs(process.argv);
  const data = computePath(args);
  const svg = toSVG(data, 900, 480);
  fs.writeFileSync(args.out, svg, 'utf8');
  console.log(`Wrote ${args.out}`);
  console.log(`Duration (arrow): ${data.aDur} ms  | samples: ${data.samples.length}`);
  console.log('Key points:', data.pts);
  console.log('First 5 samples:', data.samples.slice(0,5));
}

if (require.main === module) {
  try { main(); } catch (e) { console.error('Error:', e.message); process.exit(1); }
}

