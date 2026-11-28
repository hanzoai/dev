/**
 * An agent drafted from one line: a name ("Marie Curie") or a description ("a
 * skeptical CFO", "my grandmother who taught maths"). A Hanzo model, asked with
 * the person's own bearer, writes the draft; nothing is created until they save it.
 *
 *   POST /v1/chat/completions   Enso, the router every plan grants
 *   GET  commons.wikimedia.org  a public-domain portrait, for a historical figure only
 */
import { ask, json } from './chat.ts'
import { ENSO } from './models.ts'
import type { Target } from './call.ts'

/** What the model drafts. */
export interface Sketch {
  /** One short word the agent is called by. */
  name: string
  role: string
  instructions: string
  /** The first line it says, kept in its instructions. */
  opening: string
  /** One emoji for a role; '' for a person. */
  emoji: string
  /** The full name of a real historical person, '' for anyone or anything else. */
  figure: string
}

export const SYSTEM = [
  'You draft one agent for a person\'s own team from one line they typed: a name or a description.',
  'Reply with one JSON object and nothing else: {"name","role","instructions","opening","emoji","figure"}.',
  'name: one short word the agent goes by, letters and digits only (Curie, Marie, Cfo).',
  'role: two to four words.',
  'instructions: second person, "You are ...", plain and specific: what you do, how you speak, how you work. For a real historical person, speak in their own voice and from what they wrote and did, and say when a question is past what they could have known.',
  'opening: the one short line you say first.',
  'emoji: one emoji that fits a role; "" for a real person.',
  'figure: the full name of a real historical person who died more than seventy years ago when the line names one; "" otherwise.',
].join(' ')

const word = (v: unknown): string => (typeof v === 'string' ? v.trim() : '')

/** A model's answer as a draft: the fields that are text, or null where it named no one to draft. */
export function parse(said: string): Sketch | null {
  const o = json(said)
  if (!o) return null
  const name = word(o.name).replace(/[^\p{L}\p{N}._-]+/gu, '').slice(0, 40)
  const instructions = word(o.instructions)
  if (!name || !instructions) return null
  return { name, role: word(o.role), instructions, opening: word(o.opening), emoji: word(o.emoji).slice(0, 8), figure: word(o.figure) }
}

/**
 * The tokens a draft is allowed: Enso reasons before it answers, and under 4,000 the answer
 * is cut off with nothing said (measured: 1,200 came back empty, 4,000 in full).
 */
export const ROOM = 4000

/** The line they typed, drafted; asked a second time when the first answer was not one. */
export async function sketch(t: Target, line: string, signal?: AbortSignal): Promise<Sketch> {
  for (let tried = 0; tried < 2; tried++) {
    const draft = parse(await ask(t, SYSTEM, line.trim(), signal, ROOM, ENSO))
    if (draft) return draft
  }
  throw new Error('The draft did not come out. Try again.')
}

/** The instructions as saved: what the model wrote, then the first line it says. */
export const saved = (s: Pick<Sketch, 'instructions' | 'opening'>): string => (s.opening ? `${s.instructions.trim()}\n\nBegin a new conversation with: ${s.opening}` : s.instructions.trim())

/** A portrait that is free to use, and where it came from. */
export interface Portrait {
  url: string
  page: string
  licence: string
}

/** A licence the portrait may be used under without asking: public domain or CC0. */
export const free = (licence: string): boolean => /^(public domain|pd\b|cc0)/i.test(licence.trim())

/** The Commons answer for a search over files, as a portrait that is free to use, or null. */
export function portraitOf(body: unknown): Portrait | null {
  const pages = (body as { query?: { pages?: Record<string, unknown> } } | null)?.query?.pages
  if (!pages) return null
  const rows = Object.values(pages) as { index?: number; imageinfo?: { thumburl?: string; url?: string; descriptionurl?: string; extmetadata?: { LicenseShortName?: { value?: string } } }[] }[]
  for (const r of rows.sort((a, b) => (a.index ?? 0) - (b.index ?? 0))) {
    const info = r.imageinfo?.[0]
    const licence = word(info?.extmetadata?.LicenseShortName?.value)
    const url = word(info?.thumburl) || word(info?.url)
    if (info && url.startsWith('https://') && free(licence)) return { url, page: word(info.descriptionurl), licence }
  }
  return null
}

/** A public-domain portrait of a historical figure from Wikimedia Commons, or null; any failure is no portrait. */
export async function portrait(name: string, signal?: AbortSignal): Promise<Portrait | null> {
  const q = new URLSearchParams({
    action: 'query',
    format: 'json',
    origin: '*',
    generator: 'search',
    gsrsearch: `${name} portrait`,
    gsrnamespace: '6',
    gsrlimit: '8',
    prop: 'imageinfo',
    iiprop: 'url|extmetadata',
    iiurlwidth: '256',
  })
  try {
    const r = await fetch(`https://commons.wikimedia.org/w/api.php?${q}`, { signal })
    return r.ok ? portraitOf(await r.json()) : null
  } catch {
    return null
  }
}
