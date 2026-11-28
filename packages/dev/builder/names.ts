/**
 * What a person calls a device or a shell (hanzo-inc/app issue 311).
 *
 * A machine reports its hostname: `Alexs-MacBook-Pro.local`, and its hardware
 * beside it. A person reads a name: "Alex's MacBook Pro". `friendly` turns the
 * hostname into that name. The hardware, the hostname and the chip stay in the
 * detail (`formatDeviceLabel`).
 *
 * A person can also rename a device or a shell. Their name wins everywhere the
 * device is drawn: Dev's "where to run", Settings → Machines, Tabs and the
 * sidebar. It is kept per person on the account (`/v1/pref`, key
 * `deviceNames`), so it follows them to every surface. It is not kept on the
 * machine's record: `hanzo beat` re-registers the record under its hostname on
 * every heartbeat, which would undo the rename within a minute. A copy is also
 * kept in this browser, so the first frame already has it.
 *
 * A name is kept under the device's key (`deviceKey`). That key is the same
 * string whichever surface draws the device: its hostname without the
 * local-network suffix, lowercased, else its label, else its id. Tabs keys a
 * machine by `host || label || id` too. A shell is kept under
 * `<device key>/<shell name>` (`shellKey`).
 */
import { useEffect, useSyncExternalStore } from 'react'

import type { Target } from './api/call.ts'
import { deviceNames, NAME_MAX, read, save, tidyName } from './api/pref.ts'

export { NAME_MAX, tidyName }

/** Where this browser keeps its copy of the names. */
export const NAMES_KEY = 'hanzo.device.names'

export type Names = Readonly<Record<string, string>>

const SUFFIX = /\.(local|lan|home|internal|localdomain)$/i

/** The key a device's name is kept under: the first of its hostname, label and id that says something, without a local-network suffix, lowercased. */
export function deviceKey(...parts: (string | null | undefined)[]): string {
  const first = parts.map((p) => (p ?? '').trim()).find(Boolean) ?? ''
  return first.replace(SUFFIX, '').toLowerCase()
}

/** The key a shell's name is kept under: its device's key, then its own name. */
export function shellKey(device: string, shell: string): string {
  return `${deviceKey(device)}/${shell.trim().toLowerCase()}`
}

/** The names from a stored value, dropping what is not a name. */
export function names(raw: unknown): Record<string, string> {
  return deviceNames(raw) ?? {}
}

/** Apple's models, as the tail of a hostname spells them and as a person says them. */
const MODELS: readonly [RegExp, string][] = [
  [/^(macbook-?pro|mbp)$/i, 'MacBook Pro'],
  [/^(macbook-?air|mba)$/i, 'MacBook Air'],
  [/^macbook$/i, 'MacBook'],
  [/^imac(-?pro)?$/i, 'iMac'],
  [/^mac-?mini$/i, 'Mac mini'],
  [/^mac-?studio$/i, 'Mac Studio'],
  [/^mac-?pro$/i, 'Mac Pro'],
]

const model = (tail: string): string | undefined => MODELS.find(([re]) => re.test(tail))?.[1]

const capital = (word: string): string => (word ? word.charAt(0).toUpperCase() + word.slice(1) : word)

/**
 * A hostname as a person says it.
 *
 *   Alexs-MacBook-Pro.local   → Alex's MacBook Pro   (macOS drops the apostrophe from "Alex's MacBook Pro")
 *   alexs-mbp-2.local         → Alex's MacBook Pro   (a "-2" is macOS avoiding a name already on the network)
 *   MacBook-Air.local         → MacBook Air
 *   studio-Mac-mini           → Studio Mac mini
 *   DESKTOP-4F7K2QX           → Windows PC           (Windows' own made-up name)
 *   spark-dgx.lan             → spark-dgx            (a name someone chose stays theirs)
 */
export function friendly(host: string): string {
  const tidy = host.trim().replace(SUFFIX, '')
  if (!tidy) return ''
  if (/^desktop-[a-z0-9]{5,}$/i.test(tidy)) return 'Windows PC'
  if (/^laptop-[a-z0-9]{5,}$/i.test(tidy)) return 'Windows laptop'
  const words = tidy.replace(/-\d+$/, '').split(/[-_\s]+/).filter(Boolean)
  for (const take of [2, 1]) {
    if (words.length < take) continue
    const name = model(words.slice(-take).join('-'))
    if (!name) continue
    const owner = words.slice(0, -take)
    if (!owner.length) return name
    const last = owner[owner.length - 1]!
    // "Alexs" is "Alex's" with the apostrophe macOS could not keep.
    if (last.length > 2 && /s$/i.test(last)) owner[owner.length - 1] = `${last.slice(0, -1)}'s`
    return `${owner.map(capital).join(' ')} ${name}`
  }
  return tidy
}

let kept: Record<string, string> = {}
let loaded = false
const listeners = new Set<() => void>()

function load(): void {
  if (loaded) return
  loaded = true
  if (typeof window === 'undefined') return
  try {
    kept = names(JSON.parse(window.localStorage.getItem(NAMES_KEY) ?? '{}'))
  } catch {
    kept = {}
  }
  window.addEventListener('storage', (e) => {
    if (e.key !== NAMES_KEY) return
    try {
      set(names(JSON.parse(e.newValue ?? '{}')), false)
    } catch {
      /* a bad copy is not a name */
    }
  })
}

function set(next: Record<string, string>, store = true): void {
  kept = next
  if (store && typeof window !== 'undefined') {
    try {
      window.localStorage.setItem(NAMES_KEY, JSON.stringify(next))
    } catch {
      /* kept for this page only */
    }
  }
  for (const l of listeners) l()
}

/** The names as this page holds them now. */
export function heldNames(): Names {
  load()
  return kept
}

/** The name a person gave the device or shell under `key`, if any. */
export function nameOf(key: string): string | undefined {
  return heldNames()[key]
}

/**
 * Name the device or shell under `key` (an empty name clears it, back to its own).
 * With a `target`, the names are saved on the account too; that save failing
 * keeps the name in this browser.
 */
export async function rename(key: string, name: string, target?: Target | null): Promise<void> {
  if (!key) return
  const next = { ...heldNames() }
  const clean = tidyName(name)
  if (clean) next[key] = clean
  else delete next[key]
  set(next)
  if (target?.token()) await save(target, { deviceNames: Object.keys(next).length ? next : null })
}

/** The account documents read this page, per credential: the names are adopted once from each. */
const adopted = new Set<string>()

function subscribe(onChange: () => void): () => void {
  listeners.add(onChange)
  return () => listeners.delete(onChange)
}

const EMPTY: Names = {}

/**
 * The names a person gave their devices and shells, kept current: a rename on
 * any surface of this page redraws every one that reads them. With a signed-in
 * `target`, the account's names are read once per page and win over this
 * browser's copy.
 */
export function useDeviceNames(target?: Target | null): Names {
  const held = useSyncExternalStore(subscribe, heldNames, () => EMPTY)
  const token = target?.token() ?? null
  useEffect(() => {
    if (!target || !token) return
    const key = `${target.api} ${target.org ?? ''} ${token}`
    if (adopted.has(key)) return
    adopted.add(key)
    read(target)
      .then((p) => {
        if (p.deviceNames) set({ ...p.deviceNames })
      })
      .catch(() => adopted.delete(key))
    // The target's identity is its api, org and bearer.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [target?.api, target?.org, token])
  return held
}
