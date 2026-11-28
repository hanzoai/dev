/**
 * The side panel's tabs, once, for Chat and Dev: what is open, in what order,
 * which one is chosen, and whether the panel is open at all — kept as it was
 * left. The panel that draws them is `Panel` (panel.tsx).
 *
 *   hanzo.side.<mode>     whether the panel is open in Chat or in Dev
 *   hanzo.side.<scope>    its tabs, their order, and the chosen one
 *
 * A tab is a KIND the surface offers (Artifacts, Preview, Files, Diff,
 * Terminal, Sources, …), one of each, or a PAGE: an address, or the bytes of
 * something an answer wrote, rendered. `+` opens a kind that is not open or a
 * new page; a tab is dragged to reorder (Ctrl/⌘+Shift+←/→ from the keyboard)
 * and shut with its cross (Delete). A page's own bytes are kept with it, so an
 * artifact's tab survives a reload; an object URL does not and is not kept.
 * A page keeps the addresses it went through (`back`, `ahead`), so its bar's
 * Back and Forward walk them as a browser's do.
 *
 * A PINNED tab leads the strip and closes only on purpose: its own close (`shut`)
 * takes it, and nothing that closes tabs in bulk (`clear`) does. Pinned tabs
 * keep their order among themselves, the others theirs; a tab moved stays in
 * its group.
 *
 */
import { useEffect, useRef, useState, useSyncExternalStore } from 'react'

/** One tab. A kind's id is the kind; a page's is minted. */
export interface Tab {
  id: string
  kind: string
  title: string
  /** A page's address. */
  url?: string
  /** A page's own bytes, rendered as `type`. */
  body?: string
  type?: string
  /** The addresses a page came from, oldest first, and those Back stepped over, nearest first. */
  back?: string[]
  ahead?: string[]
  /** Leads the strip, and closes only on purpose. */
  pinned?: boolean
}

/** The tabs in a scope, in order, and the chosen one. */
export interface Tabs {
  tabs: Tab[]
  at: string | null
}

export const PAGE = 'page'

/** Bytes kept with a page tab, at most: a page an answer wrote, not a dump. */
const KEEP = 200_000

const where = (scope: string) => `hanzo.side.${scope}`

function store(): Storage | null {
  try {
    return typeof window === 'undefined' ? null : window.localStorage
  } catch {
    return null
  }
}

// The scopes this document has read, so a switch back costs no parse.
const live = new Map<string, Tabs | null>()
const listeners = new Set<() => void>()
const announce = () => {
  for (const l of listeners) l()
}
const subscribe = (l: () => void) => {
  listeners.add(l)
  return () => {
    listeners.delete(l)
  }
}

const isTab = (t: unknown): t is Tab =>
  !!t && typeof (t as Tab).id === 'string' && typeof (t as Tab).kind === 'string' && typeof (t as Tab).title === 'string'

/** A scope as kept, or null when nothing is: the surface's seed stands in. A bad entry is dropped. */
export function read(scope: string): Tabs | null {
  if (live.has(scope)) return live.get(scope)!
  let held: Tabs | null = null
  try {
    const raw = store()?.getItem(where(scope))
    if (raw) {
      const v = JSON.parse(raw) as Partial<Tabs>
      // An object URL dies with the document that minted it: a page holding only one is not kept.
      const tabs = ranked((Array.isArray(v.tabs) ? v.tabs.filter(isTab) : []).filter((t) => t.kind !== PAGE || t.body !== undefined || !t.url?.startsWith('blob:')))
      held = { tabs, at: tabs.some((t) => t.id === v.at) ? (v.at as string) : (tabs[0]?.id ?? null) }
    }
  } catch {
    held = null
  }
  live.set(scope, held)
  return held
}

function write(scope: string, next: Tabs): void {
  live.set(scope, next)
  try {
    const kept = { ...next, tabs: next.tabs.map((t) => (t.body !== undefined && t.body.length > KEEP ? { ...t, body: undefined } : t)) }
    store()?.setItem(where(scope), JSON.stringify(kept))
  } catch {
    // A browser that refuses storage keeps the tabs for this page.
  }
  announce()
}

/** Pinned tabs first, each group in the order it had. */
function ranked(tabs: Tab[]): Tab[] {
  return [...tabs.filter((t) => t.pinned), ...tabs.filter((t) => !t.pinned)]
}

let minted = 0
const mint = () => `p${Date.now().toString(36)}${(minted++).toString(36)}`

/** A tab opened: a kind already open is chosen, a page already open on the same thing is chosen. */
export function opened(was: Tabs, tab: Omit<Tab, 'id'> & { id?: string }): Tabs {
  const same = was.tabs.find((t) =>
    tab.kind === PAGE ? t.kind === PAGE && ((tab.url && t.url === tab.url) || (tab.body !== undefined && t.body === tab.body && t.title === tab.title)) : t.kind === tab.kind,
  )
  if (same) return { ...was, at: same.id }
  const id = tab.id ?? (tab.kind === PAGE ? mint() : tab.kind)
  return { tabs: ranked([...was.tabs, { ...tab, id }]), at: id }
}

/** A tab shut: the one to its right is chosen, else the one to its left, as a browser does. */
export function shut(was: Tabs, id: string): Tabs {
  const gone = was.tabs.findIndex((t) => t.id === id)
  if (gone < 0) return was
  const tabs = was.tabs.filter((t) => t.id !== id)
  if (was.at !== id) return { ...was, tabs }
  return { tabs, at: (tabs[gone] ?? tabs[gone - 1])?.id ?? null }
}

/** A tab moved to `to`, the others closing up around it, and kept in its group: pinned or not. */
export function moved(was: Tabs, id: string, to: number): Tabs {
  const from = was.tabs.findIndex((t) => t.id === id)
  if (from < 0) return was
  const tabs = was.tabs.slice()
  const [tab] = tabs.splice(from, 1)
  const pins = tabs.filter((t) => t.pinned).length
  const [low, high] = tab!.pinned ? [0, pins] : [pins, tabs.length]
  tabs.splice(Math.max(low, Math.min(high, to)), 0, tab!)
  return { ...was, tabs }
}

/** A tab pinned, or let go: it joins the end of the pinned tabs, or leads the rest. */
export function pinned(was: Tabs, id: string, on: boolean): Tabs {
  const tab = was.tabs.find((t) => t.id === id)
  if (!tab || Boolean(tab.pinned) === on) return was
  const rest = was.tabs.filter((t) => t.id !== id)
  const pins = rest.filter((t) => t.pinned)
  const others = rest.filter((t) => !t.pinned)
  const plain = { ...tab }
  delete plain.pinned
  return { ...was, tabs: on ? [...pins, { ...tab, pinned: true }, ...others] : [...pins, plain, ...others] }
}

/** Every tab closed but the pinned ones and those `keep` names; the chosen one stays if it is left. */
export function cleared(was: Tabs, keep: (t: Tab) => boolean = () => false): Tabs {
  const tabs = was.tabs.filter((t) => t.pinned || keep(t))
  if (tabs.length === was.tabs.length) return was
  return { tabs, at: tabs.some((t) => t.id === was.at) ? was.at : (tabs[0]?.id ?? null) }
}

export interface Deck extends Tabs {
  open: (tab: Omit<Tab, 'id'> & { id?: string }) => void
  pick: (id: string) => void
  shut: (id: string) => void
  move: (id: string, to: number) => void
  /** Pin a tab: it leads the strip and closes only on purpose. */
  pin: (id: string) => void
  unpin: (id: string) => void
  /** Close every tab but the pinned ones and those `keep` names. */
  clear: (keep?: (t: Tab) => boolean) => void
  /** Point a page somewhere else: where it was goes on its Back, and Forward is forgotten. */
  go: (id: string, url: string) => void
  /** A page's Back and Forward. */
  back: (id: string) => void
  forward: (id: string) => void
  /** A page's title, as what it shows calls itself. */
  rename: (id: string, title: string) => void
}

/** How many addresses a page's Back keeps. */
const DEPTH = 50

/** A page pointed at `url`, where it was on its Back. */
export function went(t: Tab, url: string): Tab {
  if (url === t.url) return t
  const back = t.url ? [...(t.back ?? []), t.url].slice(-DEPTH) : (t.back ?? [])
  return { ...t, url, title: name(url), body: undefined, type: undefined, back, ahead: [] }
}

/** A page one step back, or forward: the address it leaves goes the other way. */
export function stepped(t: Tab, way: 'back' | 'forward'): Tab {
  const from = way === 'back' ? (t.back ?? []) : (t.ahead ?? [])
  const url = way === 'back' ? from.at(-1) : from[0]
  if (!url) return t
  const rest = way === 'back' ? from.slice(0, -1) : from.slice(1)
  const here = t.url ? [t.url] : []
  return way === 'back'
    ? { ...t, url, title: name(url), back: rest, ahead: [...here, ...(t.ahead ?? [])] }
    : { ...t, url, title: name(url), ahead: rest, back: [...(t.back ?? []), ...here].slice(-DEPTH) }
}

/** A scope's tabs: as kept, else `seed`, and every change kept. */
export function useDeck(scope: string, seed: Tabs): Deck {
  const kept = useSyncExternalStore(subscribe, () => read(scope), () => null)
  const now = kept ?? seed
  const change = (f: (was: Tabs) => Tabs) => write(scope, f(read(scope) ?? seed))
  return {
    ...now,
    open: (tab) => change((was) => opened(was, tab)),
    pick: (id) => change((was) => ({ ...was, at: id })),
    shut: (id) => change((was) => shut(was, id)),
    move: (id, to) => change((was) => moved(was, id, to)),
    pin: (id) => change((was) => pinned(was, id, true)),
    unpin: (id) => change((was) => pinned(was, id, false)),
    clear: (keep) => change((was) => cleared(was, keep)),
    go: (id, url) => change((was) => ({ ...was, tabs: was.tabs.map((t) => (t.id === id ? went(t, url) : t)) })),
    back: (id) => change((was) => ({ ...was, tabs: was.tabs.map((t) => (t.id === id ? stepped(t, 'back') : t)) })),
    forward: (id) => change((was) => ({ ...was, tabs: was.tabs.map((t) => (t.id === id ? stepped(t, 'forward') : t)) })),
    rename: (id, title) => change((was) => ({ ...was, tabs: was.tabs.map((t) => (t.id === id && title && t.title !== title ? { ...t, title } : t)) })),
  }
}

const shownKey = (mode: string) => where(`${mode}.open`)
const shownLive = new Map<string, boolean>()

function shownOf(mode: string, first: boolean): boolean {
  if (shownLive.has(mode)) return shownLive.get(mode)!
  let v = first
  try {
    const raw = store()?.getItem(shownKey(mode))
    if (raw === '1' || raw === '0') v = raw === '1'
  } catch {
    v = first
  }
  shownLive.set(mode, v)
  return v
}

/** Whether the panel is open in `mode`, kept; `first` before anyone has said. */
export function useShown(mode: string, first: boolean): readonly [boolean, (open: boolean) => void] {
  const open = useSyncExternalStore(subscribe, () => shownOf(mode, first), () => first)
  const set = (next: boolean) => {
    shownLive.set(mode, next)
    try {
      store()?.setItem(shownKey(mode), next ? '1' : '0')
    } catch {
      // Kept for this page.
    }
    announce()
  }
  return [open, set] as const
}

/**
 * Whether the panel is open: kept per mode where it is a column (`wide`), and
 * for this page only where it is a sheet over everything — a phone opens on
 * the conversation, never on a sheet it did not ask for.
 */
export function usePanel(mode: string, first: boolean, wide: boolean): readonly [boolean, (open: boolean) => void] {
  const [kept, keep] = useShown(mode, first)
  const [sheet, setSheet] = useState(false)
  return wide ? ([kept, keep] as const) : ([sheet, setSheet] as const)
}

/** Whether a key went to something the reader types in, where it is theirs. */
const typing = (e: KeyboardEvent): boolean => {
  const el = e.target as HTMLElement | null
  return !!el && (el.isContentEditable || el.tagName === 'INPUT' || el.tagName === 'TEXTAREA' || el.tagName === 'SELECT')
}

/**
 * The panel's keys, from anywhere on the page but a field: Ctrl+Shift+B (⌘⇧B on
 * a Mac), the tabs' own, and ⌘. / Ctrl+., which it had first.
 */
export function useKey(toggle: () => void): void {
  const now = useRef(toggle)
  now.current = toggle
  useEffect(() => {
    const press = (e: KeyboardEvent) => {
      if (!(e.metaKey || e.ctrlKey) || e.altKey || typing(e)) return
      const tabs = e.shiftKey && e.key.toLowerCase() === 'b'
      const dot = !e.shiftKey && e.key === '.'
      if (!tabs && !dot) return
      e.preventDefault()
      now.current()
    }
    window.addEventListener('keydown', press)
    return () => window.removeEventListener('keydown', press)
  }, [])
}

/** The host and path of an address: as much as a title can honestly be. */
export function name(url: string): string {
  try {
    const u = new URL(url)
    return u.host + (u.pathname === '/' ? '' : u.pathname)
  } catch {
    return url
  }
}

/** Where words typed in a page's bar are looked up. */
export const SEARCH = 'https://duckduckgo.com/?q='

/** Whether what was typed in a page's bar is an address rather than words to look up. */
export function located(typed: string): boolean {
  const t = typed.trim()
  if (!t || /\s/.test(t)) return false
  if (/^(localhost|127\.0\.0\.1|\[::1\])(:\d+)?(\/|$)/i.test(t)) return true
  if (/^[a-z][a-z0-9+.-]*:\/\//i.test(t) || /^(about|blob|data):/i.test(t)) return true
  return /^[a-z0-9-]+(\.[a-z0-9-]+)*\.[a-z]{2,}(:\d+)?(\/.*)?$/i.test(t)
}

/** What a page's bar opens for what was typed: the address, or a search for the words. */
export function destination(typed: string): string {
  const t = typed.trim()
  if (!t) return ''
  return located(t) ? address(t) : SEARCH + encodeURIComponent(t)
}

/** An address as typed, made one that opens: https for a host, http for this machine. */
export function address(typed: string): string {
  const t = typed.trim()
  if (!t) return ''
  // This machine first: `localhost:3000` reads as a scheme to the rule below.
  if (/^(localhost|127\.0\.0\.1|\[::1\])(:|\/|$)/i.test(t)) return `http://${t}`
  if (/^[a-z][a-z0-9+.-]*:(?!\d)/i.test(t)) return t
  return `https://${t}`
}

