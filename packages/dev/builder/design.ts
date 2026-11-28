/**
 * A design system as the composer handles it: drafted by a model, kept as a
 * Studio brand (CSS custom properties, the form Studio's renderer reads), said to the
 * model on every message of a conversation it is attached to, and drawn as a
 * page to look at. Pure, so it is tested without a browser.
 */
import type { Brand } from './api/studio.ts'

/** What a model is asked to answer: the system in plain fields. */
export interface Draft {
  name: string
  colors: Record<string, string>
  fonts: { heading: string; body: string; mono: string }
  /** Type sizes, px, smallest first. */
  scale: number[]
  /** Spacing steps, px. */
  spacing: number[]
  radius: Record<string, number>
  /** One line each: how a button, card, input, nav… looks and behaves. */
  components: Record<string, string>
  voice: { tone: string; do: string[]; dont: string[] }
}

/** The schema the drafting call decodes under. */
export const SCHEMA = {
  type: 'object',
  additionalProperties: false,
  required: ['name', 'colors', 'fonts', 'scale', 'spacing', 'radius', 'components', 'voice'],
  properties: {
    name: { type: 'string' },
    colors: {
      type: 'object',
      additionalProperties: false,
      required: ['primary', 'secondary', 'accent', 'background', 'surface', 'text', 'muted', 'border'],
      properties: Object.fromEntries(['primary', 'secondary', 'accent', 'background', 'surface', 'text', 'muted', 'border'].map((k) => [k, { type: 'string' }])),
    },
    fonts: { type: 'object', additionalProperties: false, required: ['heading', 'body', 'mono'], properties: { heading: { type: 'string' }, body: { type: 'string' }, mono: { type: 'string' } } },
    scale: { type: 'array', items: { type: 'number' } },
    spacing: { type: 'array', items: { type: 'number' } },
    radius: { type: 'object', additionalProperties: false, required: ['sm', 'md', 'lg'], properties: { sm: { type: 'number' }, md: { type: 'number' }, lg: { type: 'number' } } },
    components: {
      type: 'object',
      additionalProperties: false,
      required: ['button', 'card', 'input', 'nav', 'hero'],
      properties: Object.fromEntries(['button', 'card', 'input', 'nav', 'hero'].map((k) => [k, { type: 'string' }])),
    },
    voice: { type: 'object', additionalProperties: false, required: ['tone', 'do', 'dont'], properties: { tone: { type: 'string' }, do: { type: 'array', items: { type: 'string' } }, dont: { type: 'array', items: { type: 'string' } } } },
  },
} as const

const HEX = /^#[0-9a-f]{3}([0-9a-f]{3})?$/i
const str = (v: unknown): string => (typeof v === 'string' ? v.trim() : '')
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' && !Array.isArray(v) ? (v as Record<string, unknown>) : {})
const nums = (v: unknown, lo: number, hi: number): number[] => (Array.isArray(v) ? v.filter((n): n is number => typeof n === 'number' && n >= lo && n <= hi) : [])
const lines = (v: unknown): string[] => (Array.isArray(v) ? v.map(str).filter(Boolean).slice(0, 8) : [])

/** A model's answer as a Draft, or why it is not one. */
export function drafted(raw: unknown): Draft | string {
  const o = obj(raw)
  const name = str(o.name).slice(0, 80)
  if (!name) return 'The design system has no name.'
  const colors = Object.fromEntries(Object.entries(obj(o.colors)).filter(([, v]) => HEX.test(str(v))).map(([k, v]) => [k, str(v)]))
  if (!colors.primary || !colors.background || !colors.text) return 'The design system needs at least a primary, a background and a text colour.'
  const f = obj(o.fonts)
  const fonts = { heading: str(f.heading) || 'Inter', body: str(f.body) || 'Inter', mono: str(f.mono) || 'ui-monospace' }
  const r = obj(o.radius)
  const radius = Object.fromEntries(Object.entries(r).filter(([, v]) => typeof v === 'number' && v >= 0 && v <= 64)) as Record<string, number>
  const v = obj(o.voice)
  return {
    name,
    colors,
    fonts,
    scale: nums(o.scale, 8, 160).slice(0, 10),
    spacing: nums(o.spacing, 0, 256).slice(0, 12),
    radius,
    components: Object.fromEntries(Object.entries(obj(o.components)).map(([k, d]) => [k, str(d)]).filter(([, d]) => d)),
    voice: { tone: str(v.tone), do: lines(v.do), dont: lines(v.dont) },
  }
}

/**
 * A Draft as design tokens: CSS custom properties, the names @hanzo/design uses
 * (--background, --foreground, --primary, --font-sans, …). That is the one form
 * Studio's renderer takes (hanzo/render `tokens`: every key `--name`, every value
 * one declaration), so a deck, a document and a page draw from the same map.
 * The components and the voice ride along as quoted strings under their own names.
 */
export function tokensOf(d: Draft): Record<string, string> {
  // Quoted, with nothing that would make JSON escape it or end the declaration.
  const q = (v: string) => JSON.stringify(v.replace(/["\\;{}<>\u0000-\u001f]|\/\*/g, ' ').slice(0, 280))
  const c = d.colors
  const out: Record<string, string> = {
    '--background': c.background!,
    '--foreground': c.text!,
    '--primary': c.primary!,
  }
  const more: Record<string, string | undefined> = { '--secondary': c.secondary, '--accent': c.accent, '--card': c.surface, '--muted-foreground': c.muted, '--border': c.border }
  for (const [k, v] of Object.entries(more)) if (v) out[k] = v
  out['--font-display'] = d.fonts.heading
  out['--font-sans'] = d.fonts.body
  out['--font-mono'] = d.fonts.mono
  for (const [k, v] of Object.entries(d.radius)) out[`--radius-${k}`] = `${v}px`
  d.scale.forEach((n, i) => (out[`--text-${i}`] = `${n}px`))
  d.spacing.forEach((n, i) => (out[`--space-${i}`] = `${n}px`))
  for (const [k, v] of Object.entries(d.components)) out[`--component-${k.replace(/[^a-z0-9-]/gi, '-').toLowerCase()}`] = q(v)
  if (d.voice.tone) out['--voice-tone'] = q(d.voice.tone)
  if (d.voice.do.length) out['--voice-do'] = q(d.voice.do.join('; ').replace(/;/g, ','))
  if (d.voice.dont.length) out['--voice-dont'] = q(d.voice.dont.join('; ').replace(/;/g, ','))
  return out
}

/** A token's value as written, a quoted string unquoted. */
const value = (v: unknown): string => {
  const s = typeof v === 'string' ? v : ''
  try {
    return s.startsWith('"') ? String(JSON.parse(s)) : s
  } catch {
    return s
  }
}

/** The tokens whose names start `prefix`, as `name value` pairs, the prefix dropped. */
function pairs(tokens: Record<string, unknown>, prefix: string): [string, string][] {
  return Object.entries(tokens)
    .filter(([k]) => k.startsWith(prefix))
    .map(([k, v]) => [k.slice(prefix.length), value(v)])
}

/** The colours by the names a person reads them. */
const COLOURS: [string, string][] = [
  ['primary', '--primary'],
  ['secondary', '--secondary'],
  ['accent', '--accent'],
  ['background', '--background'],
  ['surface', '--card'],
  ['text', '--foreground'],
  ['muted', '--muted-foreground'],
  ['border', '--border'],
]
const colours = (t: Record<string, unknown>): [string, string][] => COLOURS.filter(([, k]) => t[k]).map(([n, k]) => [n, value(t[k])])

/** What a model is told on every message of a conversation the system is attached to. */
export function said(b: Brand): string {
  const t = b.tokens
  const row = (list: [string, string][]) => list.map(([k, v]) => `${k} ${v}`).join(', ')
  const fonts: [string, string][] = [
    ['heading', value(t['--font-display'])],
    ['body', value(t['--font-sans'])],
    ['mono', value(t['--font-mono'])],
  ].filter(([, v]) => v) as [string, string][]
  const comps = pairs(t, '--component-')
  const voice = [value(t['--voice-tone']), value(t['--voice-do']) && `do: ${value(t['--voice-do'])}`, value(t['--voice-dont']) && `don't: ${value(t['--voice-dont'])}`].filter(Boolean)
  const parts = [
    `Design system "${b.name}" is attached to this conversation. Everything you make — pages, apps, components, slides, images and copy — follows it exactly; never invent other colours, fonts or radii.`,
    colours(t).length && `Colours: ${row(colours(t))}.`,
    fonts.length && `Fonts: ${row(fonts)}.`,
    pairs(t, '--text-').length && `Type scale: ${row(pairs(t, '--text-'))}.`,
    pairs(t, '--space-').length && `Spacing: ${row(pairs(t, '--space-'))}.`,
    pairs(t, '--radius-').length && `Radius: ${row(pairs(t, '--radius-'))}.`,
    comps.length && `Components: ${comps.map(([k, v]) => `${k} — ${v}`).join(' ')}`,
    voice.length && `Voice: ${voice.join(' | ')}`,
    `In HTML, declare these exact custom properties on :root and use them: ${Object.keys(t).filter((k) => !k.startsWith('--component-') && !k.startsWith('--voice-')).join(', ')}. Load the fonts from Google Fonts when they are web fonts.`,
  ]
  return parts.filter(Boolean).join('\n')
}

const esc = (s: string): string => s.replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[c]!)

/** The system drawn as a page: its palette, type, a button, a card, an input, its voice. */
export function preview(b: Brand): string {
  const t = b.tokens
  const v = (k: string, d: string) => value(t[k]) || d
  const sizes = pairs(t, '--text-').map(([, x]) => x)
  const comps = pairs(t, '--component-')
  const heading = v('--font-display', 'Inter')
  const body = v('--font-sans', 'Inter')
  const fonts = [heading, body].filter((x) => x && !/^ui-|system|sans-serif|serif$/i.test(x))
  const link = fonts.length ? `<link rel="stylesheet" href="https://fonts.googleapis.com/css2?${fonts.map((x) => `family=${encodeURIComponent(x)}:wght@400;600;700`).join('&')}&display=swap">` : ''
  const swatches = colours(t).map(([k, c]) => `<div class="sw"><span style="background:${esc(c)}"></span><b>${esc(k)}</b><code>${esc(c)}</code></div>`).join('')
  const scale = sizes.map((x) => `<div style="font-size:${esc(x)}">${esc(b.name)} — ${esc(x)}</div>`).join('')
  const root = Object.entries(t).filter(([k]) => !k.startsWith('--component-') && !k.startsWith('--voice-')).map(([k, x]) => `${k}:${value(x)}`).join(';')
  return `<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>${esc(b.name)}</title>${link}
<style>:root{${root}}
body{margin:0;background:var(--background);color:var(--foreground);font-family:'${esc(body)}',system-ui,sans-serif;padding:40px;line-height:1.5}
h1,h2{font-family:'${esc(heading)}',system-ui,sans-serif;margin:0 0 12px}h2{margin-top:40px;font-size:14px;letter-spacing:.08em;text-transform:uppercase;color:var(--muted-foreground,var(--foreground))}
.row{display:flex;flex-wrap:wrap;gap:16px}.sw{display:flex;flex-direction:column;gap:4px;width:120px}.sw span{height:64px;border-radius:var(--radius-md,8px);border:1px solid var(--border,var(--foreground))}.sw code{color:var(--muted-foreground);font-size:12px}
.btn{background:var(--primary);color:var(--background);border:0;border-radius:var(--radius-md,8px);padding:12px 20px;font:inherit;font-weight:600}.btn.ghost{background:transparent;color:var(--foreground);border:1px solid var(--border,var(--foreground))}
.card{background:var(--card,var(--background));border:1px solid var(--border,var(--foreground));border-radius:var(--radius-lg,16px);padding:20px;max-width:320px}input{background:var(--card,var(--background));color:var(--foreground);border:1px solid var(--border,var(--foreground));border-radius:var(--radius-sm,4px);padding:10px 12px;font:inherit;width:260px}
li{color:var(--muted-foreground)}</style></head><body>
<h1 style="font-size:40px">${esc(b.name)}</h1><p style="color:var(--muted-foreground)">${esc(value(t['--voice-tone']))}</p>
<h2>Colour</h2><div class="row">${swatches}</div>
<h2>Type</h2>${scale}
<h2>Components</h2><div class="row" style="align-items:flex-start"><button class="btn">Primary action</button><button class="btn ghost">Secondary</button><input placeholder="An input"><div class="card"><b>A card</b><p style="color:var(--muted-foreground);margin:8px 0 0">${esc(comps.find(([k]) => k === 'card')?.[1] ?? '')}</p></div></div>
<ul>${comps.map(([k, x]) => `<li><b style="color:var(--foreground)">${esc(k)}</b> — ${esc(x)}</li>`).join('')}</ul>
<h2>Voice</h2><p><b>Do</b>: ${esc(value(t['--voice-do']))}</p><p><b>Don't</b>: ${esc(value(t['--voice-dont']))}</p>
</body></html>`
}
