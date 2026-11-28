import { phrase } from '@hanzo/i18n'
import type { Say } from '@hanzo/i18n/react'
import { useCallback, useSyncExternalStore } from 'react'

import { friendly } from './names.ts'
import { english } from './say.ts'

export const DEV_MODE_KEY = 'hanzo.dev_mode'
export const DEV_MODE_EVENT = 'hanzo:dev-mode'

/** What Developer mode does, in one line, wherever its switch is drawn (English, marked; drawn through `t`). */
export const DEV_MODE_NOTE = phrase('Shows the process: the terminal, the commands Hanzo runs and each step. Off, Hanzo shows the result.')

/**
 * DEVELOPER MODE IS OFF FOR A NEW PERSON, and the result is what they see
 * (hanzo-inc/app issues 317 and 321). Someone who has worked with the process
 * on this device keeps seeing it: the first time the choice is read where it
 * was never made, it is made once from what this device shows of them, and kept.
 *
 *   - Tabs listed a machine or a cloud workspace of theirs (`hanzo.tabs.fleet`):
 *     they opened a terminal, or linked a computer with the CLI.
 *   - Dev's transcript view is Thinking or Verbose: they asked to see more of it.
 *
 * Anyone else, including someone who has only built with Dev, starts with the
 * result. The switch (Settings › General, the account menu, a run's menu, ⌘⇧D)
 * changes it either way.
 */
export function adopt(store: Pick<Storage, 'getItem'>): boolean {
  try {
    const view = store.getItem('hanzo.build.transcript.view')
    if (view === '"thinking"' || view === '"verbose"') return true
    const fleet = JSON.parse(store.getItem('hanzo.tabs.fleet') || '[]') as unknown
    return Array.isArray(fleet) && fleet.length > 0
  } catch {
    return false
  }
}

export function getDevMode(): boolean {
  if (typeof window === 'undefined') return false
  try {
    const kept = window.localStorage.getItem(DEV_MODE_KEY)
    if (kept !== null) return kept === 'true'
    const chosen = adopt(window.localStorage)
    window.localStorage.setItem(DEV_MODE_KEY, chosen ? 'true' : 'false')
    return chosen
  } catch {
    return false
  }
}

export function setDevMode(next: boolean): void {
  if (typeof window === 'undefined') return
  try {
    window.localStorage.setItem(DEV_MODE_KEY, next ? 'true' : 'false')
  } catch {
    /* ignore storage errors */
  }
  window.dispatchEvent(new CustomEvent(DEV_MODE_EVENT, { detail: { enabled: next } }))
}

function subscribeDevMode(onChange: () => void): () => void {
  if (typeof window === 'undefined') return () => {}
  const handler = () => onChange()
  const onStorage = (e: StorageEvent) => {
    if (e.key === DEV_MODE_KEY) onChange()
  }
  window.addEventListener(DEV_MODE_EVENT, handler)
  window.addEventListener('storage', onStorage)
  return () => {
    window.removeEventListener(DEV_MODE_EVENT, handler)
    window.removeEventListener('storage', onStorage)
  }
}

export function useDevMode(): [boolean, (next: boolean) => void] {
  const enabled = useSyncExternalStore(subscribeDevMode, getDevMode, () => false)
  const update = useCallback((next: boolean) => setDevMode(next), [])
  return [enabled, update]
}

export interface DeviceLabelOptions {
  hardware?: string | null
  host?: string | null
  /** The name the person gave it (names.ts): it wins, for a developer too. */
  name?: string | null
  devMode?: boolean
  /** The reader's `t`; English where none is given. */
  say?: Say
}

function mergeDetail(...parts: (string | null | undefined)[]): string | undefined {
  const seen = new Set<string>()
  const out: string[] = []
  for (const p of parts) {
    for (const seg of (p ?? '').trim().split(/\s*·\s*/)) {
      const key = seg.trim().toLowerCase()
      if (key && !seen.has(key)) {
        seen.add(key)
        out.push(seg.trim())
      }
    }
  }
  return out.length ? out.join(' · ') : undefined
}

/**
 * A device as a person reads it. A name the person gave it wins. Otherwise a
 * developer sees the name the machine reports, and anyone else sees it as a
 * person says it (`Alexs-MacBook-Pro.local` → "Alex's MacBook Pro", names.ts
 * `friendly`), with the hardware and the hostname beneath, and the cloud
 * workspace in plain words.
 */
export function formatDeviceLabel(raw: string | null | undefined, opts: DeviceLabelOptions = {}): { title: string; detail?: string } {
  const say = opts.say ?? english
  const name = (raw ?? '').trim()
  const own = (opts.name ?? '').trim()
  if (own) return { title: own, detail: mergeDetail(opts.hardware, opts.host, name !== own ? name : undefined) }
  if (opts.devMode) return name ? { title: name, detail: mergeDetail(opts.hardware, opts.host && opts.host !== name ? opts.host : undefined) } : { title: 'Cloud' }
  const lower = name.toLowerCase()
  if (!name || /^(hanzo sandbox|sandbox|cloud|cloud workspace)$/.test(lower) || /^cloud-\d+(\s|·|$)/.test(lower)) {
    return { title: say('Cloud workspace'), detail: !name || !lower.startsWith('cloud-') ? say('Managed Hanzo cloud workspace') : name }
  }
  const tidy = name
    .split(/\s*·\s*/)[0]!
    .replace(/\s*\((?:shell|desk|tmux|tty)[^)]*\)\s*$/i, '')
    .trim()
  const title = friendly(tidy) || name
  return { title, detail: mergeDetail(opts.hardware, opts.host, name !== title ? name : undefined) }
}
