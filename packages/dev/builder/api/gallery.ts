/**
 * The gallery: gallery.hanzo.ai's templates, read live from the one catalogue it
 * publishes (hanzoai/gallery `scripts/gen-templates-json.mjs`).
 *
 *   GET https://gallery.hanzo.ai/templates.json   {version, templates: [row]}
 *
 * Reference content: no bearer, no org. A row is a starter kit — a repository
 * with a capture of its page — and names no kind, so a row with a repository is
 * a site. A row that names a kind is that kind; one this reader does not know,
 * or one missing what its kind is made from, is left out.
 *
 * A site is made in Dev, as a copy of its repository (projects.ts `fork`); an
 * agent in Customize › Agents, as the org's own (agents.ts `use`).
 */
import { phrase } from '@hanzo/i18n'

import { SLUG } from '../route.ts'

export const GALLERY = 'https://gallery.hanzo.ai/templates.json'

export const KINDS = ['site', 'agent'] as const
export type Kind = (typeof KINDS)[number]

/** Each kind's name, English and marked (`phrase`), drawn through `t`. */
export const KIND: Record<Kind, string> = { site: phrase('Site'), agent: phrase('Agent') }

/** Where each kind is made. */
export type App = 'dev' | 'agents'
export const APP: Record<Kind, App> = { site: 'dev', agent: 'agents' }

export interface Template {
  slug: string
  name: string
  kind: Kind
  category: string
  description: string
  /** Its capture, an https address, or ''. */
  preview: string
  /** An agent's instructions and model ('' the deployment's default); '' for a site. */
  instructions: string
  model: string
  /** What a search reads, lower case. */
  words: string
}

/** What the screen says when the catalogue cannot be read, whatever the reason. */
export const UNREAD = phrase('The gallery is not answering right now.')

const str = (v: unknown): string => (typeof v === 'string' ? v.trim() : '')
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
const https = (v: unknown): string => (/^https:\/\/\S+$/.test(str(v)) ? str(v) : '')
const strs = (v: unknown): string[] => (Array.isArray(v) ? v.map(str).filter(Boolean) : [])

/** One row as the gallery serves it, or null for one this Library cannot offer. */
export function template(raw: unknown): Template | null {
  const o = obj(raw)
  const slug = str(o.slug)
  if (!SLUG.test(slug)) return null
  // A row names its kind, or is a site by its repository.
  const kind = (str(o.kind) || (https(o.repo) ? 'site' : '')) as Kind
  if (!KINDS.includes(kind)) return null
  const instructions = kind === 'agent' ? str(o.instructions) : ''
  if (kind === 'agent' && !instructions) return null
  const name = str(o.displayName) || str(o.name) || slug
  const category = str(o.category)
  const description = str(o.description)
  return {
    slug,
    name,
    kind,
    category,
    description,
    preview: o.hasScreenshot === false ? '' : https(o.screenshotUrl),
    instructions,
    model: kind === 'agent' ? str(o.model) : '',
    words: [name, slug, category, description, str(o.useCase), str(o.framework), ...strs(o.features), ...strs(o.intents)].join(' ').toLowerCase(),
  }
}

/** The gallery's templates, in its order, each slug once. Any failure is `UNREAD`; an abort stays an abort. */
export async function catalogue(init?: { signal?: AbortSignal }): Promise<Template[]> {
  let raw: unknown
  try {
    const res = await fetch(GALLERY, { signal: init?.signal, cache: 'no-cache' })
    if (!res.ok) throw new Error(String(res.status))
    raw = await res.json()
  } catch (e) {
    if (init?.signal?.aborted) throw e
    throw new Error(UNREAD)
  }
  const rows = obj(raw).templates
  if (!Array.isArray(rows)) throw new Error(UNREAD)
  const out = new Map<string, Template>()
  for (const r of rows) {
    const x = template(r)
    if (x && !out.has(x.slug)) out.set(x.slug, x)
  }
  return [...out.values()]
}

/** The templates of `kind` ('' every kind) holding every word of `q`. */
export function narrow(list: readonly Template[], kind: Kind | '', q: string): Template[] {
  const words = q.toLowerCase().split(/\s+/).filter(Boolean)
  return list.filter((x) => (!kind || x.kind === kind) && words.every((w) => x.words.includes(w)))
}
