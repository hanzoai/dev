/**
 * The signed-in person's own settings, as the platform keeps them for every
 * Hanzo surface.
 *
 *   GET   /v1/pref → {prefs: {…}, updatedAt}   an empty document for someone who saved nothing
 *   PATCH /v1/pref   {key: value | null}        a shallow merge; null deletes the key
 *
 * The document is one object shared by every surface, keyed per person and per
 * org. This reads only the keys the builder owns and writes only those, so a
 * key another surface saved is left as it was. A value this version does not
 * recognise reads as unset rather than as a choice.
 */
import { kept, type Kept } from '../suggest.ts'
import { call, type Target } from './call.ts'

export type Theme = 'system' | 'light' | 'dark'
export type Text = 'small' | 'medium' | 'large'
export type Motion = 'system' | 'reduced'

/**
 * Where and how a new run starts when New has no choice of its own kept. The
 * model and effort are not here: they are the one choice Chat and Dev share,
 * kept in the browser (mind.tsx).
 */
export interface Code {
  mode?: 'build' | 'plan'
  /** A machine's id, or '' for the sandbox. */
  place?: string
}

/** The side panel beside Chat and Dev, once for both. */
export interface Side {
  /** How wide it is drawn, px, while the middle column shows. */
  width?: number
}

export interface Prefs {
  theme?: Theme
  text?: Text
  motion?: Motion
  /** The dictation language, as voice.ts names it. */
  language?: string
  /** What Hanzo calls the person. */
  callName?: string
  /** What best describes their work. */
  work?: string
  /** Whether the recommended skills for their work were taken up or put off (customize/roles.tsx); unset until asked. */
  roles?: 'done' | 'skipped'
  /** What every coding run is told about how they like to work. */
  instructions?: string
  code?: Code
  side?: Side
  /** The suggestions under the composer: when made, from what, the rows, and those dismissed (suggest.ts). */
  suggest?: Kept
  /**
   * Whether suggestions are made for this person, kept by the account so it follows them across devices and never
   * reaches the next person on a shared browser. Unset is OFF: only a person who turned them on has their activity
   * read and sent (suggestions.tsx `setSuggestions`).
   */
  suggestions?: 'on' | 'off'
  /** What the person calls their devices and shells, by device key (names.ts). */
  deviceNames?: Record<string, string>
}

/** A change: a key set to a value, or to null to delete it. */
export type Patch = { [K in keyof Prefs]?: Prefs[K] | null }

const THEMES: readonly Theme[] = ['system', 'light', 'dark']
const SUGGESTIONS = ['on', 'off'] as const
const TEXTS: readonly Text[] = ['small', 'medium', 'large']
const MOTIONS: readonly Motion[] = ['system', 'reduced']
const MODES = ['build', 'plan'] as const
const ROLES = ['done', 'skipped'] as const

const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' && !Array.isArray(v) ? (v as Record<string, unknown>) : {})
const text = (v: unknown): string | undefined => (typeof v === 'string' && v.trim() ? v : undefined)
const one = <T extends string>(v: unknown, of: readonly T[]): T | undefined => (of.includes(v as T) ? (v as T) : undefined)

/** Drop the keys that say nothing, so a read and a merge compare cleanly. */
function defined<T extends object>(o: T): T {
  return Object.fromEntries(Object.entries(o).filter(([, v]) => v !== undefined)) as T
}

export function code(raw: unknown): Code | undefined {
  const c = obj(raw)
  const out = defined<Code>({
    mode: one(c.mode, MODES),
    place: typeof c.place === 'string' ? c.place : undefined,
  })
  return Object.keys(out).length ? out : undefined
}

/** The longest device or shell name kept, in characters: a menu row's worth. */
export const NAME_MAX = 60

/** A name as kept: trimmed, one line, at most `NAME_MAX` characters; '' for none. */
export function tidyName(name: unknown): string {
  return typeof name === 'string' ? name.replace(/\s+/g, ' ').trim().slice(0, NAME_MAX) : ''
}

/** Device and shell names from a stored value, dropping what is not a name; undefined for none. */
export function deviceNames(raw: unknown): Record<string, string> | undefined {
  const out: Record<string, string> = {}
  for (const [k, v] of Object.entries(obj(raw))) {
    const name = tidyName(v)
    if (k && name) out[k] = name
  }
  return Object.keys(out).length ? out : undefined
}

export function side(raw: unknown): Side | undefined {
  const w = obj(raw).width
  return typeof w === 'number' && Number.isFinite(w) && w > 0 ? { width: Math.round(w) } : undefined
}

/** The builder's keys from a whole preference document. */
export function prefs(raw: unknown): Prefs {
  const p = obj(raw)
  return defined<Prefs>({
    theme: one(p.theme, THEMES),
    text: one(p.text, TEXTS),
    motion: one(p.motion, MOTIONS),
    language: text(p.language),
    callName: text(p.callName),
    work: text(p.work),
    roles: one(p.roles, ROLES),
    instructions: text(p.instructions),
    code: code(p.code),
    side: side(p.side),
    suggest: kept(p.suggest),
    suggestions: one(p.suggestions, SUGGESTIONS),
    deviceNames: deviceNames(p.deviceNames),
  })
}

/** What a document reads as once `patch` is merged into it — the platform's own rule. */
export function merge(base: Prefs, patch: Patch): Prefs {
  const out: Record<string, unknown> = { ...base }
  for (const [k, v] of Object.entries(patch)) {
    if (v === null) delete out[k]
    else if (v !== undefined) out[k] = v
  }
  return prefs(out)
}

export async function read(t: Target): Promise<Prefs> {
  const r = await call<{ prefs?: unknown }>(t, 'GET', '/v1/pref')
  return prefs(r?.prefs)
}

export async function save(t: Target, patch: Patch): Promise<Prefs> {
  const r = await call<{ prefs?: unknown }>(t, 'PATCH', '/v1/pref', patch)
  return prefs(r?.prefs)
}
