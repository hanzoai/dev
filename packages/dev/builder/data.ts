/**
 * The builder's reads, as hooks: one per thing on screen, each answering
 * `{value, error, loading, reload}` so every pane owes the same three states.
 *
 * A read that fails says so and keeps what it had; a read with no person
 * signed in is not attempted — the platform would refuse it and the refusal
 * would read as an outage.
 */
import { phrase } from '@hanzo/i18n'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'

import type { Target } from './api/call.ts'
import { places, type Place } from './api/places.ts'
import { projects, type Project } from './api/projects.ts'
import { get, list, watch, type Detail, type Event, type Session } from './api/sessions.ts'
import { TALK } from './api/talk.ts'
import { merge } from './api/turn.ts'
import { groupSessions, syncThreadsFromSessions, threadRoot, useThreadLinks } from './thread.ts'

export interface Read<T> {
  value: T
  error: Error | null
  loading: boolean
  reload: () => void
}

/** Run `load` whenever `key` changes (and on `reload`), keeping the last good value. */
export function useRead<T>(load: (() => Promise<T>) | null, initial: T, key: unknown[]): Read<T> {
  const [value, setValue] = useState<T>(initial)
  const [error, setError] = useState<Error | null>(null)
  const [loading, setLoading] = useState(Boolean(load))
  const [tick, setTick] = useState(0)
  const reload = useCallback(() => setTick((n) => n + 1), [])
  useEffect(() => {
    if (!load) {
      setLoading(false)
      return
    }
    let live = true
    setLoading(true)
    load()
      .then((v) => {
        if (!live) return
        setValue(v)
        setError(null)
      })
      // Every read goes through the platform's client, which refuses with an Error.
      .catch((e: unknown) => live && setError(e as Error))
      .finally(() => live && setLoading(false))
    return () => {
      live = false
    }
    // `key` is the dependency list, by contract.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [...key, tick])
  return { value, error, loading, reload }
}

/** How long a read is waited on before the screen says it is taking too long. */
export const WAIT = 15_000

/** A read that did not answer in time. Its message is ours, marked, so a screen drawing it through `t` says it in the reader's language. */
export class Slow extends Error {
  constructor() {
    super(phrase('Hanzo is taking too long to answer.'))
    this.name = 'Slow'
  }
}

/** What this page last read, per key, for the life of the page. */
const seen = new Map<string, unknown>()

/**
 * A read drawn at once from what this page last read under the same `key`, then
 * read again behind it: stale while it revalidates, so moving between tabs is
 * instant and a list never blanks to "reading" on the way back. A read that has
 * not answered after `wait` ms is let go, as `Slow`. A null key reads nothing.
 */
export function useCached<T>(key: string | null, load: (signal: AbortSignal) => Promise<T>, initial: T, wait = WAIT): Read<T> {
  const [value, setValue] = useState<T>(() => (key !== null && seen.has(key) ? (seen.get(key) as T) : initial))
  const [error, setError] = useState<Error | null>(null)
  const [loading, setLoading] = useState(key !== null)
  const [tick, setTick] = useState(0)
  const reload = useCallback(() => setTick((n) => n + 1), [])
  const loader = useRef(load)
  loader.current = load
  const was = useRef(key)
  useEffect(() => {
    // Another key (another org, another search) never shows the last one's answer or failure.
    if (was.current !== key) {
      was.current = key
      setValue(key !== null && seen.has(key) ? (seen.get(key) as T) : initial)
      setError(null)
    }
    if (key === null) {
      setLoading(false)
      return
    }
    const ctl = new AbortController()
    const timer = setTimeout(() => {
      ctl.abort()
      setError(new Slow())
      setLoading(false)
    }, wait)
    setLoading(true)
    loader
      .current(ctl.signal)
      .then((v) => {
        if (ctl.signal.aborted) return
        seen.set(key, v)
        setValue(v)
        setError(null)
        setLoading(false)
      })
      .catch((e: unknown) => {
        if (ctl.signal.aborted) return
        setError(e as Error)
        setLoading(false)
      })
      .finally(() => clearTimeout(timer))
    return () => {
      clearTimeout(timer)
      ctl.abort()
    }
    // `initial` is the empty value, read only when the key changes.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, tick, wait])
  return { value, error, loading, reload }
}

/** Last coding runs read per `api\norg`, so mounting another rail or switching tabs starts warm. */
const recentRuns = new Map<string, Session[]>()

/**
 * The org's conversations and coding runs, newest first, kept current by the
 * org's own session stream: a status that moves moves its dot, and one started
 * anywhere appears. A conversation's own coding runs are in its tree, not here:
 * the platform lists roots.
 */
export function useRecents(t: Target, signed: boolean): Read<Session[]> {
  const scope = signed ? `${t.api}\n${t.org ?? ''}` : null
  const warm = scope !== null ? recentRuns.get(scope) : undefined
  const threadLinks = useThreadLinks()
  const read = useRead(
    signed
      ? async () => {
          const [runs, talked] = await Promise.all([list(t, { kind: 'coding', limit: 50 }), list(t, { channel: TALK, limit: 50 })])
          const seen = new Set<string>()
          const unique = [...runs, ...talked]
            .filter((s) => !seen.has(s.id) && Boolean(seen.add(s.id)))
            .sort((a, b) => b.createdAt.localeCompare(a.createdAt))
          const links = syncThreadsFromSessions(unique)
          const next = groupSessions(unique, links).slice(0, 50)
          if (scope !== null) recentRuns.set(scope, next)
          return next
        }
      : null,
    warm ?? ([] as Session[]),
    [t.api, t.org, signed],
  )
  const [settled, setSettled] = useState(warm !== undefined)
  useEffect(() => {
    setSettled(scope !== null && recentRuns.has(scope))
  }, [scope])
  useEffect(() => {
    if (!read.loading) setSettled(true)
  }, [read.loading])
  const [rows, setRows] = useState<Session[]>(() => warm ?? read.value)
  useEffect(() => setRows(read.value), [read.value])
  const grouped = useMemo(() => groupSessions(rows, threadLinks), [rows, threadLinks])
  const last = useRef(0)
  // useRead's reload is the same function every render, so it never reopens the feed.
  const again = read.reload
  useEffect(() => {
    if (!signed) return
    // A feed of the whole org carries every agent's heartbeat, so an unseen id
    // re-reads the list at most every five seconds.
    const reload = () => {
      const now = Date.now()
      if (now - last.current < 5000) return
      last.current = now
      again()
    }
    const ctl = new AbortController()
    void watch(
      t,
      '',
      {
        session: (s) =>
          setRows((prev) => {
            const rootId = threadRoot(s.id)
            const i = prev.findIndex(
              (r) =>
                r.id === s.id ||
                r.id === rootId ||
                (s.kind === 'coding' &&
                  r.kind === 'coding' &&
                  Boolean(s.branch) &&
                  s.branch.startsWith('agent/') &&
                  r.branch === s.branch &&
                  r.repo.toLowerCase() === s.repo.toLowerCase()),
            )
            if (i === -1) {
              // A session this list has not seen: re-read rather than guess its kind.
              if (!s.kind || s.kind === 'coding') reload()
              return prev
            }
            const next = prev.slice()
            const existing = prev[i]!
            next[i] = {
              ...existing,
              status: s.status || existing.status,
              title: existing.id === s.id ? s.title || existing.title : existing.title || s.title,
              branch: s.branch || existing.branch,
              pr: s.pr || existing.pr,
              sandbox: s.sandbox || existing.sandbox,
              updatedAt: s.updatedAt || s.createdAt || existing.updatedAt,
              endedAt: s.endedAt,
            }
            return next
          }),
        open: (n) => {
          if (n > 0) reload()
        },
      },
      ctl.signal,
    ).catch(() => {
      /* a refused feed leaves the list as read */
    })
    return () => ctl.abort()
  }, [t, signed, again])
  return { ...read, value: grouped, loading: read.loading && !settled }
}

export function usePlaces(t: Target, signed: boolean): Read<Place[]> {
  return useRead(signed ? () => places(t) : null, [] as Place[], [t, signed])
}

export function useProjects(t: Target, signed: boolean): Read<Project[]> {
  return useRead(signed ? () => projects(t) : null, [] as Project[], [t, signed])
}

/** A value kept in this browser, per key; storage that throws keeps it in memory. */
export function useKept<T>(key: string, initial: T): [T, (v: T) => void] {
  const [value, setValue] = useState<T>(() => {
    try {
      const raw = window.localStorage.getItem(key)
      return raw === null ? initial : (JSON.parse(raw) as T)
    } catch {
      return initial
    }
  })
  const set = useCallback(
    (v: T) => {
      setValue(v)
      try {
        window.localStorage.setItem(key, JSON.stringify(v))
      } catch {
        /* kept for this page only */
      }
    },
    [key],
  )
  return [value, set]
}

export interface RunState {
  detail: Read<Detail | null>
  /**
   * The run's record, as last read or pushed: status, branch, pull request.
   * The coding service writes it; events are the narration, and any member of
   * the org can append one, so a link or a branch is never taken from them.
   */
  record: Session | null
  /** Every turn, recorded and live, in order. */
  events: Event[]
  /** The run's status as last reported by the record or the feed. */
  status: string
  /** Why the feed stopped, when it was refused. */
  refused: string
}

/**
 * One run: the recorded detail, then the live feed added to it. A reconnect
 * after a drop re-reads the detail — frames missed while dropped are only in
 * the record. `id` null reads nothing.
 */
export function useRun(t: Target, id: string | null): RunState {
  const detail = useRead<Detail | null>(id ? () => get(t, id, { limit: 500 }) : null, null, [t, id])
  const [live, setLive] = useState<Event[]>([])
  const [pushed, setPushed] = useState<Session | null>(null)
  const [refused, setRefused] = useState('')
  const reload = useRef(detail.reload)
  reload.current = detail.reload
  const seenId = useRef<string | null>(null)
  const highestSeq = useRef(0)
  if (seenId.current !== id) {
    seenId.current = id
    highestSeq.current = 0
  }
  if (detail.value && detail.value.id === id) {
    for (const e of detail.value.recent) {
      if (e.sessionId === id && e.seq > highestSeq.current) highestSeq.current = e.seq
    }
  }

  // The stream is followed by the run's tree: a conversation's coding run is in the
  // conversation's (talk.ts), and its frames carry that root.
  const root = (detail.value?.id === id && detail.value.root) || id
  useEffect(() => {
    setLive([])
    setPushed(null)
    setRefused('')
    if (!id || !root) return
    const ctl = new AbortController()
    void watch(
      t,
      root,
      {
        // A frame from a stream this run has already left is not this run's.
        event: (e) => {
          if (!ctl.signal.aborted && e.sessionId === id) {
            if (e.seq > highestSeq.current) highestSeq.current = e.seq
            setLive((prev) => [...prev, e])
          }
        },
        session: (s) => {
          if (!ctl.signal.aborted && s.id === id) setPushed(s)
        },
        // A reconnect may have missed frames, and so may the move to the run's
        // own tree once its read named one: either is read again.
        open: (n) => {
          if (n > 0 || root !== id) reload.current()
        },
      },
      ctl.signal,
      { after: root === id ? highestSeq.current : 0 },
    ).catch((e: unknown) => setRefused((e as Error).message))
    return () => ctl.abort()
  }, [t, id, root])

  // The read and the frames describe THIS run or nothing: a read kept from the
  // run before (it stays until the next one lands, or forever if it fails) must
  // not lend this run its pull request.
  const read = detail.value && detail.value.id === id ? detail.value : null
  const frame = pushed && pushed.id === id ? pushed : null
  const events = useMemo(() => merge(read?.recent ?? [], live), [read, live])
  // A frame newer than the read wins; the read fills what a frame left empty.
  const record = useMemo((): Session | null => {
    const base = read
    const pushed = frame
    if (!pushed) return base
    if (!base) return pushed
    return {
      ...base,
      ...pushed,
      title: pushed.title || base.title,
      branch: pushed.branch || base.branch,
      pr: pushed.pr || base.pr,
      mode: pushed.mode || base.mode,
    }
  }, [read, frame])
  return { detail, record, events, status: record?.status ?? '', refused }
}
