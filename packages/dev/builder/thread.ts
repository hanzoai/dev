/**
 * Single-thread session continuity, live-state resolution, and Recents grouping.
 *
 * A coding session can span multiple turns (follow-ups via `after` and mid-flight
 * steers via `POST /v1/agent/coding/{id}/steer`). Each turn has its own backend
 * session id, shares the thread's branch (`agent/<short>`) or sandbox, and
 * belongs to ONE conversation thread for the person using Dev.
 *
 * This module keeps:
 *  - Accurate active/live/terminal status classification (so lifecycle events like
 *    `started`, `routed`, `starting`, `queued`, `resumed` never falsely mark an
 *    active session as finished),
 *  - Client-persisted thread links (`hanzo.dev.session.threads`) synchronized with
 *    server-observed `root`, `parent`, `branch`+`repo`, and `sandbox` chains,
 *  - Recents deduplication (`groupSessions`) so continuing a thread updates its
 *    single entry instead of spamming duplicate rows in the sidebar,
 *  - Clean error normalization so server-scrubbed `[REDACTED-TOKEN]` strings never
 *    leak into user-facing status banners.
 */
import { useSyncExternalStore } from 'react'

import type { Session } from './api/sessions.ts'

export const THREADS_KEY = 'hanzo.dev.session.threads'
export const THREADS_EVENT = 'hanzo:session-threads'

/** Map of child session id -> immediate previous (or root) session id in the same thread. */
export type ThreadLinks = Record<string, string>

/** Statuses indicating a session turn has ended. */
export const TERMINAL_STATUSES = new Set(['done', 'completed', 'error', 'stopped', 'followed'])

/** Statuses indicating a session turn is actively executing (or queued/starting). */
export const ACTIVE_STATUSES = new Set(['running', 'starting', 'started', 'queued', 'routed', 'resumed'])

/** Statuses indicating a session is still live (active or paused, or not yet reported). */
export const LIVE_STATUSES = new Set(['running', 'starting', 'started', 'queued', 'routed', 'resumed', 'paused', ''])

export const isSessionActive = (status: string): boolean => ACTIVE_STATUSES.has(status)

export const isSessionLive = (status: string): boolean => LIVE_STATUSES.has(status)

export const isSessionTerminal = (status: string): boolean => TERMINAL_STATUSES.has(status)

/**
 * Resolve the effective status from the session record and its narrated outcome event.
 *
 * Only a terminal outcome (`done`, `completed`, `error`, `stopped`, `followed`) or
 * `paused` overrides a live record status; intermediate lifecycle events like
 * `started` or `routed` keep the session live instead of flipping it to finished.
 */
export function resolveSessionState(recordStatus: string, outcomeStatus: string): string {
  const rec = recordStatus || ''
  const end = outcomeStatus || ''
  if (end && (TERMINAL_STATUSES.has(end) || end === 'paused') && LIVE_STATUSES.has(rec)) return end
  return rec || end
}

/** Whether a server error indicates the target coding session is still running. */
export function isAlreadyRunningError(err: unknown): boolean {
  const msg = err instanceof Error ? err.message : typeof err === 'string' ? err : ''
  return /is running;\s*stop it or let it finish|already running|already being followed up/i.test(msg)
}

/**
 * Normalize error messages shown in the UI so server-redacted session IDs
 * (`[REDACTED-TOKEN]`) are replaced with readable session language.
 */
export function cleanErrorText(message: string): string {
  if (!message) return ''
  return message
    .replace(/\bthe run \[REDACTED-TOKEN\]/gi, 'this session')
    .replace(/\brun \[REDACTED-TOKEN\]/gi, 'this session')
    .replace(/\[REDACTED-TOKEN\]/g, 'this session')
}

const NONE: ThreadLinks = {}
let held: { raw: string | null; links: ThreadLinks } = { raw: null, links: NONE }

export function readThreadLinks(): ThreadLinks {
  if (typeof window === 'undefined') return held.links
  let raw: string | null = null
  try {
    raw = window.localStorage.getItem(THREADS_KEY)
  } catch {
    return held.links
  }
  if (raw === held.raw) return held.links
  let parsed: unknown = null
  try {
    parsed = raw === null ? null : JSON.parse(raw)
  } catch {
    parsed = null
  }
  const clean: ThreadLinks = {}
  if (parsed && typeof parsed === 'object' && !Array.isArray(parsed)) {
    for (const [k, v] of Object.entries(parsed as Record<string, unknown>)) {
      if (typeof k === 'string' && typeof v === 'string' && k && v && k !== v) clean[k] = v
    }
  }
  held = { raw, links: clean }
  return held.links
}

function writeThreadLinks(next: ThreadLinks): void {
  const raw = JSON.stringify(next)
  held = { raw, links: next }
  if (typeof window === 'undefined') return
  try {
    window.localStorage.setItem(THREADS_KEY, raw)
  } catch {
    /* kept in memory for this page */
  }
  window.dispatchEvent(new Event(THREADS_EVENT))
}

/** Follow parent pointers to the earliest root session id for `id` (cycle-safe). */
export function threadRoot(id: string, links: ThreadLinks = readThreadLinks()): string {
  let cur = id
  const seen = new Set<string>([cur])
  while (links[cur] && !seen.has(links[cur]!)) {
    cur = links[cur]!
    seen.add(cur)
  }
  return cur
}

/**
 * Record that `nextId` continues `previousId` inside the same thread.
 * Returns the thread's root session id.
 */
export function linkThread(previousId: string, nextId: string): string {
  if (!previousId || !nextId || previousId === nextId) return previousId || nextId
  const current = readThreadLinks()
  // Prevent cycles if nextId is already an ancestor of previousId.
  let ancestor = previousId
  const seen = new Set<string>([ancestor])
  while (current[ancestor] && !seen.has(current[ancestor]!)) {
    ancestor = current[ancestor]!
    if (ancestor === nextId) return threadRoot(previousId, current)
    seen.add(ancestor)
  }
  if (current[nextId] === previousId) return threadRoot(previousId, current)
  const next: ThreadLinks = { ...current, [nextId]: previousId }
  writeThreadLinks(next)
  return threadRoot(previousId, next)
}

/**
 * Return the ordered chain of session IDs belonging to `id`'s thread (oldest to newest).
 */
export function threadChain(id: string, links: ThreadLinks = readThreadLinks()): string[] {
  if (!id) return []
  const root = threadRoot(id, links)
  const children = new Map<string, string[]>()
  for (const [child, parent] of Object.entries(links)) {
    const list = children.get(parent) ?? []
    list.push(child)
    children.set(parent, list)
  }
  const order: string[] = []
  const visited = new Set<string>()
  const walk = (node: string) => {
    if (visited.has(node)) return
    visited.add(node)
    order.push(node)
    for (const child of children.get(node) ?? []) walk(child)
  }
  walk(root)
  if (!visited.has(id)) order.push(id)
  return order
}

/** Key identifying sessions that belong to the same coding workstream on the server. */
function workstreamKey(s: Session): string {
  if (s.kind !== 'coding') return ''
  if (s.branch && s.branch.startsWith('agent/')) return `branch:${s.repo.toLowerCase()}:${s.branch}`
  if (s.sandbox && !s.branch) return `sandbox:${s.project.toLowerCase()}:${s.sandbox}`
  return ''
}

/**
 * Discover thread chains from a list of server sessions (via `parent`, `root`,
 * shared `agent/` branch, or shared sandbox) and merge them into the link map.
 */
export function deriveThreadLinks(sessions: readonly Session[], base: ThreadLinks = readThreadLinks()): ThreadLinks {
  const next: ThreadLinks = { ...base }
  const byId = new Map(sessions.map((s) => [s.id, s]))
  const oldestFirst = [...sessions].sort((a, b) => a.createdAt.localeCompare(b.createdAt))

  // 1. Explicit server parent/root relationships.
  for (const s of oldestFirst) {
    const parent = s.parent || (s.root && s.root !== s.id ? s.root : '')
    if (parent && parent !== s.id && (byId.has(parent) || s.parent)) {
      if (!next[s.id]) next[s.id] = parent
    }
  }

  // 2. Chained coding runs sharing the same agent/ branch or repo-less sandbox.
  const streams = new Map<string, Session[]>()
  for (const s of oldestFirst) {
    const key = workstreamKey(s)
    if (!key) continue
    const list = streams.get(key) ?? []
    list.push(s)
    streams.set(key, list)
  }
  for (const group of streams.values()) {
    if (group.length < 2) continue
    for (let i = 1; i < group.length; i++) {
      const prev = group[i - 1]!
      const cur = group[i]!
      if (!next[cur.id] && threadRoot(prev.id, next) !== cur.id) {
        next[cur.id] = prev.id
      }
    }
  }
  return next
}

/** Persist any newly discovered thread chains from a server session list. */
export function syncThreadsFromSessions(sessions: readonly Session[]): ThreadLinks {
  const before = readThreadLinks()
  const after = deriveThreadLinks(sessions, before)
  let changed = false
  for (const [k, v] of Object.entries(after)) {
    if (before[k] !== v) {
      changed = true
      break
    }
  }
  if (changed) writeThreadLinks(after)
  return after
}

/**
 * Collapse chained follow-up turns into a single Recents row per thread:
 *  - Stable `id` = root session id
 *  - `title` = root session title (falling back to latest)
 *  - `status`, `updatedAt`, `endedAt`, `pr`, `branch`, `sandbox` = latest turn's values
 *  - Sorted newest activity first (`updatedAt || createdAt`)
 */
export function groupSessions(sessions: readonly Session[], links: ThreadLinks = readThreadLinks()): Session[] {
  if (sessions.length === 0) return []
  const merged = deriveThreadLinks(sessions, links)
  const groups = new Map<string, Session[]>()
  for (const s of sessions) {
    const root = threadRoot(s.id, merged)
    const list = groups.get(root) ?? []
    list.push(s)
    groups.set(root, list)
  }
  const out: Session[] = []
  for (const members of groups.values()) {
    members.sort((a, b) => a.createdAt.localeCompare(b.createdAt))
    const root = members[0]!
    const latest = members[members.length - 1]!
    const activityAt = latest.updatedAt || latest.createdAt || root.updatedAt || root.createdAt
    out.push({
      ...root,
      status: latest.status || root.status,
      title: root.title || latest.title,
      branch: latest.branch || root.branch,
      pr: latest.pr || root.pr,
      sandbox: latest.sandbox || root.sandbox,
      repo: latest.repo || root.repo,
      project: latest.project || root.project,
      updatedAt: activityAt,
      endedAt: latest.endedAt,
      events: members.reduce((sum, m) => sum + m.events, 0),
    })
  }
  out.sort((a, b) => (b.updatedAt || b.createdAt).localeCompare(a.updatedAt || a.createdAt))
  return out
}

function subscribe(changed: () => void): () => void {
  if (typeof window === 'undefined') return () => {}
  const other = (e: StorageEvent) => {
    if (e.key === THREADS_KEY || e.key === null) changed()
  }
  window.addEventListener(THREADS_EVENT, changed)
  window.addEventListener('storage', other)
  return () => {
    window.removeEventListener(THREADS_EVENT, changed)
    window.removeEventListener('storage', other)
  }
}

/** Reactive subscription to persisted single-thread links. */
export function useThreadLinks(): ThreadLinks {
  return useSyncExternalStore(subscribe, readThreadLinks, () => NONE)
}

/** Test-only helper to reset in-memory thread links between unit tests. */
export function resetThreadLinks(): void {
  held = { raw: null, links: NONE }
  if (typeof window !== 'undefined') {
    try {
      window.localStorage.removeItem(THREADS_KEY)
    } catch {
      /* ignore */
    }
  }
}
