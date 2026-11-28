/**
 * Preview URL selection, cache-busting, and freshness tracking.
 *
 * When a coding agent edits files inside a sandbox, the deployed production site
 * (`liveUrl`) remains at its pre-edit snapshot until the user publishes. Preview
 * therefore prefers the live sandbox preview URL whenever one is available,
 * automatically cache-busts/reloads when turns finish or commits/deploys land,
 * and signals when the workspace holds unpublished edits.
 */
import type { Event } from './api/sessions.ts'
import { decode } from './api/turn.ts'

export type PreviewKind = 'sandbox' | 'live' | 'starter' | 'none'

export interface PreviewSource {
  url: string
  kind: PreviewKind
}

export interface PreviewInputs {
  sandboxUrl?: string
  liveUrl?: string
  starterUrl?: string
  /** Explicit user toggle to inspect the live deployed site while a sandbox preview is also up. */
  preferLive?: boolean
}

/**
 * Pick the freshest preview URL:
 *  1. Live sandbox dev-server preview (unless the user explicitly chose Live site and one exists),
 *  2. Deployed project `liveUrl`,
 *  3. Starter template demo URL,
 *  4. Empty (`none`).
 */
export function resolvePreviewSource({ sandboxUrl = '', liveUrl = '', starterUrl = '', preferLive = false }: PreviewInputs): PreviewSource {
  const sbx = sandboxUrl.trim()
  const live = liveUrl.trim()
  const starter = starterUrl.trim()
  if (preferLive && live) return { url: live, kind: 'live' }
  if (sbx) return { url: sbx, kind: 'sandbox' }
  if (live) return { url: live, kind: 'live' }
  if (starter) return { url: starter, kind: 'starter' }
  return { url: '', kind: 'none' }
}

/**
 * Append/update a lightweight cache-busting query parameter on http(s) URLs so
 * manual reloads, turn completions, and fresh publishes never render stale bytes.
 */
export function cacheBustUrl(url: string, rev: number): string {
  const raw = url.trim()
  if (!raw || rev <= 0) return raw
  try {
    const parsed = new URL(raw)
    if (parsed.protocol !== 'http:' && parsed.protocol !== 'https:') return raw
    parsed.searchParams.set('_hzr', String(rev))
    return parsed.toString()
  } catch {
    return raw
  }
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')

/**
 * Compute a monotonic revision key that changes whenever:
 *  - a turn completes (`status: done | completed`),
 *  - a push or file edit lands in the session events,
 *  - a publish completes (`published` seq advances), or
 *  - the session reaches a terminal state.
 */
export function previewRevision(events: readonly Pick<Event, 'kind' | 'payload' | 'seq'>[], status: string, published: number): string {
  let lastEditSeq = 0
  let lastDoneSeq = 0
  for (const e of events) {
    const b = decode(e.payload)
    if (!b || typeof b === 'string') continue
    if (e.kind === 'status') {
      const s = str(b.status)
      if (s === 'done' || s === 'completed') lastDoneSeq = Math.max(lastDoneSeq, e.seq)
      continue
    }
    if (e.kind === 'tool-call') {
      const step = str(b.step)
      const st = str(b.status)
      if ((step === 'push' || step === 'artifacts') && (st === 'ok' || st === 'done')) {
        lastEditSeq = Math.max(lastEditSeq, e.seq)
      }
      continue
    }
    if (e.kind === 'event') {
      const type = str(b.type)
      if (type === 'turn.completed') lastDoneSeq = Math.max(lastDoneSeq, e.seq)
      const item = b.item && typeof b.item === 'object' ? (b.item as Record<string, unknown>) : null
      if (item && str(item.type) === 'file_change' && (type === 'item.completed' || str(item.status) === 'completed')) {
        lastEditSeq = Math.max(lastEditSeq, e.seq)
      }
    }
  }
  const doneFlag = status === 'done' || status === 'completed' ? '1' : '0'
  return `${published}:${lastDoneSeq}:${lastEditSeq}:${doneFlag}`
}

/**
 * Report whether the session has file edits or pushed commits newer than `published`.
 */
export function hasUnpublishedEdits(events: readonly Pick<Event, 'kind' | 'payload' | 'seq'>[], published: number): boolean {
  let latestChange = 0
  for (const e of events) {
    const b = decode(e.payload)
    if (!b || typeof b === 'string') continue
    if (e.kind === 'status' && str(b.status) === 'done' && b.changed === true) {
      latestChange = Math.max(latestChange, e.seq)
    } else if (e.kind === 'tool-call' && str(b.step) === 'push' && (str(b.status) === 'ok' || str(b.status) === 'done')) {
      latestChange = Math.max(latestChange, e.seq)
    } else if (e.kind === 'event') {
      const item = b.item && typeof b.item === 'object' ? (b.item as Record<string, unknown>) : null
      if (item && str(item.type) === 'file_change') {
        latestChange = Math.max(latestChange, e.seq)
      }
    }
  }
  return latestChange > published
}
