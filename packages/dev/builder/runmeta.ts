/**
 * What a person did to a run from their own lists: pinned, archived, deleted, renamed.
 *
 * One store, kept in this browser, for every place that lists or opens a run: the
 * sidebar's Recents (hanzoai/app) and a run's own menu here. A run archived or
 * deleted from either is gone from the other at once, in this window and in any
 * other tab of the browser, because both read and write the same key and say so on
 * the window (`RUNS_EVENT`).
 */
import { useCallback, useSyncExternalStore } from 'react'

export const RUNS_KEY = 'hanzo.dev.runs.meta'
export const RUNS_EVENT = 'hanzo:runs-meta'

/** One run's own marks; absent means none. */
export interface RunMark {
  title?: string
  pinned?: boolean
  archived?: boolean
  deleted?: boolean
}

export type RunMarks = Record<string, RunMark>

const NONE: RunMarks = {}

/** What storage held at the last read: the same object until it changes, as a store snapshot must be. */
let held: { raw: string | null; marks: RunMarks } = { raw: null, marks: NONE }

/** Every run's marks. Storage that throws, or holds what is not a record, reads as none. */
export function marks(): RunMarks {
  if (typeof window === 'undefined') return NONE
  let raw: string | null = null
  try {
    raw = window.localStorage.getItem(RUNS_KEY)
  } catch {
    return held.marks
  }
  if (raw === held.raw) return held.marks
  let parsed: unknown = null
  try {
    parsed = raw === null ? null : JSON.parse(raw)
  } catch {
    parsed = null
  }
  held = { raw, marks: parsed && typeof parsed === 'object' && !Array.isArray(parsed) ? (parsed as RunMarks) : NONE }
  return held.marks
}

/** Mark one run: the keys in `patch` replace its own, the rest stay. Said on the window so every list redraws. */
export function mark(id: string, patch: RunMark): void {
  if (typeof window === 'undefined') return
  const next: RunMarks = { ...marks(), [id]: { ...marks()[id], ...patch } }
  try {
    window.localStorage.setItem(RUNS_KEY, JSON.stringify(next))
  } catch {
    // Kept for this page only: the change still shows, until the page is left.
    held = { raw: held.raw, marks: next }
  }
  window.dispatchEvent(new Event(RUNS_EVENT))
}

function subscribe(changed: () => void): () => void {
  const other = (e: StorageEvent) => {
    if (e.key === RUNS_KEY || e.key === null) changed()
  }
  window.addEventListener(RUNS_EVENT, changed)
  window.addEventListener('storage', other)
  return () => {
    window.removeEventListener(RUNS_EVENT, changed)
    window.removeEventListener('storage', other)
  }
}

/** The runs' marks, and the way to mark one; the component redraws whenever either place changes them. */
export function useMarks(): [RunMarks, (id: string, patch: RunMark) => void] {
  const all = useSyncExternalStore(subscribe, marks, () => NONE)
  return [all, useCallback((id, patch) => mark(id, patch), [])]
}
