/**
 * What a conversation carries beside its words: the skills, connectors and
 * plugins picked from the composer's `+`, the design system, the device, the
 * Research and Memory switches and the mode. Chat and Dev keep it the same way.
 *
 *   GET   /v1/pref   {prefs: {gear, gears}}
 *   PATCH /v1/pref   {gear} — the person's default, what a new conversation opens with
 *                    {gears: {<key>: Gear}} — each conversation's own, newest first, at most `KEEP`
 *
 * Both live in the account, so a choice follows the person to every device. A
 * conversation that has none of its own reads the default; changing one writes
 * that conversation's. A conversation not begun holds its choice on the page
 * (`draft`) and hands it to the conversation once it has an id. What carries
 * over to the next new conversation is only the switches (`STICKY`: Research,
 * Memory, the mode); a skill or a plugin is picked for the conversation it was
 * picked in.
 */
import { phrase } from '@hanzo/i18n'
import { useCallback, useEffect, useMemo, useRef, useState, useSyncExternalStore } from 'react'

import { call, type Target } from './api/call.ts'

/** What a run or a turn may do on its own: the permission mode. */
export type Mode = 'auto' | 'manual' | 'plan'

/**
 * The modes, one line each. Chat offers Auto and Manual; Dev adds Plan. Manual
 * is a real pause: before a plugin runs for a message (a search, a site read, a
 * deck), the person allows it once, always in this conversation, or not, and a
 * Dev run is asked for before it starts. Written in English and marked
 * (`phrase`): the composer draws each word through `t`.
 */
export const MODES: readonly { id: Mode; label: string; note: string }[] = [
  { id: 'auto', label: phrase('Auto'), note: phrase('Runs tools on its own; pauses only for anything risky.') },
  { id: 'manual', label: phrase('Manual'), note: phrase('Asks before each tool, connector or site it opens.') },
  { id: 'plan', label: phrase('Plan'), note: phrase('Writes the plan and waits for your go before changing anything.') },
]

/** The modes each surface offers. */
export const CHAT_MODES: readonly Mode[] = ['auto', 'manual']
export const DEV_MODES: readonly Mode[] = ['auto', 'manual', 'plan']

export interface Gear {
  /** Skill names (`/v1/tool/skills`). */
  skills: string[]
  /** Connector ids (`/v1/tool/mcp/servers`). */
  connectors: string[]
  /** Plugin ids. */
  plugins: string[]
  /** A design system's id, or ''. */
  design: string
  /** A machine's id, or '' for none (Chat) or the sandbox (Dev). */
  device: string
  research: boolean
  memory: boolean
  mode: Mode
  /** In Manual, the plugins allowed for the rest of this conversation. */
  allowed: string[]
}

export const BARE: Gear = {
  skills: [],
  connectors: [],
  plugins: [],
  design: '',
  device: '',
  research: false,
  memory: true,
  mode: 'auto',
  allowed: [],
}

/** What a new conversation carries over from the last: the switches, never a pick. */
const STICKY = ['research', 'memory', 'mode'] as const
const pick = (g: Gear): Pick<Gear, (typeof STICKY)[number]> => ({ research: g.research, memory: g.memory, mode: g.mode })

/** How many conversations keep a choice of their own: the document holds 16 KiB in all. */
export const KEEP = 30

const list = (v: unknown): string[] => (Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string' && x !== '') : [])
const text = (v: unknown): string => (typeof v === 'string' ? v : '')

/** A stored value as a Gear, a field the reader does not know read as unset. */
export function gearOf(raw: unknown, base: Gear = BARE): Gear {
  const o = raw && typeof raw === 'object' && !Array.isArray(raw) ? (raw as Record<string, unknown>) : {}
  return {
    skills: 'skills' in o ? list(o.skills) : base.skills,
    connectors: 'connectors' in o ? list(o.connectors) : base.connectors,
    plugins: 'plugins' in o ? list(o.plugins) : base.plugins,
    design: 'design' in o ? text(o.design) : base.design,
    device: 'device' in o ? text(o.device) : base.device,
    research: typeof o.research === 'boolean' ? o.research : base.research,
    memory: typeof o.memory === 'boolean' ? o.memory : base.memory,
    mode: MODES.some((m) => m.id === o.mode) ? (o.mode as Mode) : base.mode,
    allowed: 'allowed' in o ? list(o.allowed) : base.allowed,
  }
}

/** `gears` with `key`'s entry first, cut to `KEEP`. */
export function shelve(gears: Record<string, Gear>, key: string, g: Gear): Record<string, Gear> {
  const out: Record<string, Gear> = { [key]: g }
  for (const [k, v] of Object.entries(gears)) {
    if (k === key) continue
    if (Object.keys(out).length >= KEEP) break
    out[k] = v
  }
  return out
}

// One document per page, read once per target and shared by every composer on it.
interface Doc {
  gear: Gear
  gears: Record<string, Gear>
  /** The conversation not begun yet, on this page. */
  draft: Gear | null
  /** The dictation language (`language`, the key Settings › General writes), or null for Auto. */
  language: string | null
}
let doc: Doc = { gear: BARE, gears: {}, draft: null, language: null }
let loaded: string | null = null
const listeners = new Set<() => void>()
const emit = () => listeners.forEach((f) => f())
const subscribe = (f: () => void) => {
  listeners.add(f)
  return () => listeners.delete(f)
}

function load(t: Target): void {
  const id = `${t.api}|${t.org ?? ''}|${t.token() ? 1 : 0}`
  if (loaded === id || !t.token()) return
  loaded = id
  void call<{ prefs?: { gear?: unknown; gears?: unknown; language?: unknown } }>(t, 'GET', '/v1/pref')
    .then((r) => {
      const raw = r?.prefs ?? {}
      const gear = gearOf(raw.gear)
      const all = raw.gears && typeof raw.gears === 'object' ? (raw.gears as Record<string, unknown>) : {}
      const language = typeof raw.language === 'string' && raw.language ? raw.language : null
      doc = { gear, gears: Object.fromEntries(Object.entries(all).map(([k, v]) => [k, gearOf(v, gear)])), draft: doc.draft, language }
      emit()
    })
    .catch(() => {
      loaded = null
    })
}

export interface GearState {
  gear: Gear
  /** Change this conversation's choice. */
  set: (patch: Partial<Gear>) => void
  /** Add or take out one id of a list. */
  toggle: (field: 'skills' | 'connectors' | 'plugins', id: string) => void
  /** Make this conversation's choice the person's default. */
  keep: () => void
  /** Whether a save was refused, in the platform's words. */
  error: string | null
}

/**
 * The gear of the conversation `key` names (`''` for one not begun: it reads and
 * writes the default until it has an id). Signed out, it lives for the page.
 */
export function useGear(t: Target, key = ''): GearState {
  useEffect(() => load(t), [t])
  const now = useSyncExternalStore(subscribe, () => doc, () => doc)
  const [error, setError] = useState<string | null>(null)
  // A conversation with no choice of its own starts from the switches alone.
  const fresh = useMemo(() => ({ ...BARE, ...pick(now.gear) }), [now.gear])
  const gear = key ? (now.gears[key] ?? fresh) : (now.draft ?? fresh)

  const save = useCallback(
    (patch: object) => {
      if (!t.token()) return
      call(t, 'PATCH', '/v1/pref', patch).then(
        () => setError(null),
        (e: unknown) => setError(e instanceof Error ? e.message : 'Your choice was not saved.'),
      )
    },
    [t],
  )

  // The conversation just begun takes what was picked before it had an id.
  const was = useRef(key)
  useEffect(() => {
    const from = was.current
    was.current = key
    if (from || !key || !doc.draft || doc.gears[key]) return
    doc = { ...doc, gears: shelve(doc.gears, key, doc.draft), draft: null }
    emit()
    save({ gears: doc.gears })
  }, [key, save])

  const write = useCallback(
    (g: Gear) => {
      if (key) {
        doc = { ...doc, gears: shelve(doc.gears, key, g) }
        emit()
        return save({ gears: doc.gears })
      }
      // Not begun: the page holds it; the switches become the default too.
      const sticky = { ...doc.gear, ...pick(g) }
      const moved = STICKY.some((k) => sticky[k] !== doc.gear[k])
      doc = { ...doc, draft: g, ...(moved ? { gear: sticky } : null) }
      emit()
      if (moved) save({ gear: sticky })
    },
    [key, save],
  )

  return useMemo(
    () => ({
      gear,
      error,
      set: (patch) => write({ ...gear, ...patch }),
      toggle: (field, id) => {
        const has = gear[field].includes(id)
        write({ ...gear, [field]: has ? gear[field].filter((x) => x !== id) : [...gear[field], id] })
      },
      keep: () => {
        doc = { ...doc, gear }
        emit()
        if (t.token()) call(t, 'PATCH', '/v1/pref', { gear }).catch((e: unknown) => setError(e instanceof Error ? e.message : 'Not saved.'))
      },
    }),
    [gear, error, write, t],
  )
}

/** The dictation language the person keeps, null for Auto, and how to change it. */
export function useLanguage(t: Target): readonly [string | null, (id: string | null) => void] {
  useEffect(() => load(t), [t])
  const now = useSyncExternalStore(subscribe, () => doc, () => doc)
  const set = useCallback(
    (id: string | null) => {
      const was = doc.language
      doc = { ...doc, language: id }
      emit()
      // A refused save takes the choice back, so what is shown is what is kept.
      if (t.token())
        call(t, 'PATCH', '/v1/pref', { language: id }).catch(() => {
          doc = { ...doc, language: was }
          emit()
        })
    },
    [t],
  )
  return [now.language, set] as const
}
