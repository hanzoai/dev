/**
 * Runs, as the platform records them: one session per run — and Dev's
 * conversations (talk.ts), one session for every turn.
 *
 *   GET   /v1/agent/sessions?kind=&channel=&project=&status=&limit=&after=   newest first, {sessions, next}
 *   GET   /v1/agent/sessions/{id}                            + the 50 most recent events
 *   GET   /v1/agent/sessions/{id}/events?after=&limit=       its whole log, a page at a time, {events, next}
 *   PATCH /v1/agent/sessions/{id}         {title}|{published} rename it; open its story to the public build route
 *   POST  /v1/agent/sessions/{id}/message {message}           steer a running run
 *   POST  /v1/agent/sessions/{id}/pause                       pause it; a sandbox run keeps its work on its branch
 *   POST  /v1/agent/sessions/{id}/resume                      ask a paused run to go on
 *   POST  /v1/agent/sessions/{id}/stop    {message}           end it, work kept
 *   GET  /v1/agent/sessions/stream?root={id}                 SSE: `session` and `event` frames,
 *                                                            each wrapped: {"session":{…}}, {"event":{…}}
 *
 * The stream is best-effort by the platform's own statement: a subscriber that
 * falls 256 frames behind is dropped. So `watch` reconnects, and every
 * reconnect is reported, which is the caller's cue to re-read the detail.
 */
import { phrase } from '@hanzo/i18n'

import { call, headers, query, Refusal, seg, type Target } from './call.ts'
import { read } from './sse.ts'

export type Status = 'running' | 'paused' | 'done' | 'error' | 'stopped' | string

export interface Session {
  id: string
  title: string
  status: Status
  agent: string
  actor: string
  repo: string
  project: string
  target: string
  /** The hostname of the machine it runs on, as that machine reported it, or ''. */
  host: string
  /** The folder it is working in now, as its machine reports it (a linked shell moves), or ''. */
  cwd: string
  /** What sort of run: 'coding' for a coding run. */
  kind: string
  /** The branch the run started from, or ''. */
  base: string
  /** The branch the run pushes to — the only one it may write. */
  branch: string
  /** 'sandbox', or the id of the org's machine it runs on. */
  environment: string
  /** 'build' or 'plan', as the coding service recorded it; '' for other runs. */
  mode: string
  /** The pull request the run proposed, or ''. */
  pr: string
  /** The sandbox the run leased, once it has one. It is gone when the run ends. */
  sandbox: string
  /** The org the run is in, which the public build route is addressed by. */
  org: string
  /** Whether its story is open to the public build route. Only a run that names a project can be. */
  published: boolean
  /** Where it was started: 'app' for a conversation in Dev (talk.ts), '' for a run on its own. */
  channel: string
  /** The session it belongs to — a conversation, for the coding runs it started — or ''. */
  parent: string
  /** The top of its tree: its own id for a session of its own. A stream is followed by it. */
  root: string
  events: number
  createdAt: string
  updatedAt: string
  endedAt: string
}

export interface Event {
  id: string
  sessionId: string
  seq: number
  kind: string
  actor: string
  payload: unknown
  createdAt: string
}

export interface Detail extends Session {
  recent: Event[]
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> =>
  v && typeof v === 'object' ? (v as Record<string, unknown>) : {}

export function session(raw: unknown): Session {
  const s = obj(raw)
  return {
    id: str(s.id),
    title: str(s.title),
    status: str(s.status),
    agent: str(s.agent),
    actor: str(s.actor),
    repo: str(s.repo),
    project: str(s.project),
    target: str(s.target),
    host: str(s.host),
    cwd: str(s.cwd),
    kind: str(s.kind),
    base: str(s.base),
    branch: str(s.branch),
    environment: str(s.environment),
    mode: str(s.mode),
    pr: str(s.pr),
    sandbox: str(s.sandbox),
    org: str(s.org),
    published: s.published === true,
    channel: str(s.channel),
    parent: str(s.parentSessionId),
    root: str(s.rootSessionId),
    events: num(s.events),
    createdAt: str(s.createdAt),
    updatedAt: str(s.updatedAt),
    endedAt: str(s.endedAt),
  }
}

export function event(raw: unknown): Event {
  const e = obj(raw)
  return {
    id: str(e.id),
    sessionId: str(e.sessionId),
    seq: num(e.seq),
    kind: str(e.kind),
    actor: str(e.actor),
    payload: e.payload,
    createdAt: str(e.createdAt),
  }
}

/**
 * How long runs in one mode took, in whole minutes: the median and the ninetieth
 * percentile of the finished ones. Null under three, which is too few to say.
 */
export function took(runs: Session[], mode: string): [number, number] | null {
  const spans = runs
    .filter((r) => r.mode === mode && r.status === 'done' && r.endedAt)
    .map((r) => Date.parse(r.endedAt) - Date.parse(r.createdAt))
    .filter((ms) => Number.isFinite(ms) && ms > 0)
    .sort((a, b) => a - b)
  if (spans.length < 3) return null
  const at = (q: number) => Math.max(1, Math.round(spans[Math.min(spans.length - 1, Math.floor(q * spans.length))]! / 60_000))
  return [at(0.5), at(0.9)]
}

export interface ListQuery {
  kind?: string
  /** The sessions started on one channel: 'app' for Dev's conversations. */
  channel?: string
  project?: string
  /** running, paused, done or error: the four the platform filters on. */
  status?: string
  limit?: number
  /** The `next` of the page before. */
  after?: string
}

export interface Page {
  sessions: Session[]
  /** The cursor for the page after this one, or '' on the last. */
  next: string
}

export async function page(t: Target, q: ListQuery = {}): Promise<Page> {
  const raw = obj(await call<unknown>(t, 'GET', `/v1/agent/sessions${query({ ...q })}`))
  return {
    sessions: (Array.isArray(raw.sessions) ? raw.sessions : []).map(session).filter((s) => s.id),
    next: str(raw.next),
  }
}

export async function list(t: Target, q: ListQuery = {}): Promise<Session[]> {
  return (await page(t, q)).sessions
}

export interface GetOptions {
  limit?: number
  after?: number
}

export async function get(t: Target, id: string, opts?: GetOptions): Promise<Detail> {
  const raw = obj(
    await call<unknown>(
      t,
      'GET',
      `/v1/agent/sessions/${seg(id)}${query({ limit: opts?.limit, after: opts?.after })}`,
    ),
  )
  let recent = (Array.isArray(raw.recentEvents) ? raw.recentEvents : []).map(event)
  // When `limit` is explicitly requested (e.g. Run's initial 500-event load) and a
  // long coding run still exceeds one page (`recent[0].seq > 1`), page forward from
  // seq 0 so middle history is never dropped. Without `opts`, keep the lightweight
  // 2-event head fetch so short summary callers stay single-hop.
  if (recent.length > 0 && recent[0]!.seq > 1 && opts?.after === undefined) {
    const tailFloor = recent[0]!.seq
    if (opts?.limit && opts.limit > 50) {
      try {
        const earlier: Event[] = []
        let cursor = 0
        for (let page = 0; page < 6 && cursor < tailFloor - 1; page++) {
          const chunk = obj(
            await call<unknown>(
              t,
              'GET',
              `/v1/agent/sessions/${seg(id)}/events${query({ after: cursor || undefined, limit: Math.min(1000, opts.limit) })}`,
            ),
          )
          const rows = (Array.isArray(chunk.events) ? chunk.events : []).map(event)
          earlier.push(...rows.filter((e) => e.seq > 0 && e.seq < tailFloor))
          const next = num(chunk.next)
          if (next <= cursor || next >= tailFloor - 1) break
          cursor = next
        }
        const have = new Set(recent.map((e) => e.seq))
        recent = [...earlier.filter((e) => !have.has(e.seq)), ...recent].sort((a, b) => a.seq - b.seq)
      } catch {
        /* keep the tail */
      }
    } else {
      try {
        const head = obj(await call<unknown>(t, 'GET', `/v1/agent/sessions/${seg(id)}/events?limit=2`))
        const first = (Array.isArray(head.events) ? head.events : []).map(event)
        const have = new Set(recent.map((e) => e.seq))
        recent = [...first.filter((e) => e.seq > 0 && !have.has(e.seq)), ...recent].sort((a, b) => a.seq - b.seq)
      } catch {
        /* keep the tail */
      }
    }
  }
  return {
    ...session(raw),
    recent,
  }
}

/**
 * A session's whole log, oldest first, read a page at a time — what a conversation
 * draws from its first word, where the detail carries only the latest fifty.
 */
export async function log(t: Target, id: string): Promise<Event[]> {
  const out: Event[] = []
  let after = 0
  for (;;) {
    const raw = obj(await call<unknown>(t, 'GET', `/v1/agent/sessions/${seg(id)}/events${query({ after: after || undefined, limit: 1000 })}`))
    out.push(...(Array.isArray(raw.events) ? raw.events : []).map(event))
    const next = num(raw.next)
    if (next <= after) return out
    after = next
  }
}

export async function message(t: Target, id: string, text: string): Promise<void> {
  const m = text.trim()
  if (!m) throw new Refusal(400, phrase('Say something to the run'))
  await call<unknown>(t, 'POST', `/v1/agent/sessions/${seg(id)}/message`, { message: m })
}

export async function stop(t: Target, id: string, why = 'Stopped from the builder'): Promise<void> {
  await call<unknown>(t, 'POST', `/v1/agent/sessions/${seg(id)}/stop`, { message: why })
}

/** A run, deleted for good: its record and every link it was shared by. */
export async function remove(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/agent/sessions/${seg(id)}`)
}

export async function pause(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'POST', `/v1/agent/sessions/${seg(id)}/pause`, {})
}

export async function resume(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'POST', `/v1/agent/sessions/${seg(id)}/resume`, {})
}

/** A new title, up to the platform's 512 characters. */
export async function rename(t: Target, id: string, title: string): Promise<Session> {
  const name = title.trim()
  if (!name) throw new Refusal(400, phrase('Give the run a name'))
  if (name.length > 512) throw new Refusal(400, phrase('A run’s name is at most 512 characters'))
  return session(await call<unknown>(t, 'PATCH', `/v1/agent/sessions/${seg(id)}`, { title: name }))
}

/**
 * Open the run's story to the public build route, or close it. The platform
 * refuses to open one that names no project, because that route is keyed on
 * the org and the project.
 */
export async function publish(t: Target, id: string, on: boolean): Promise<Session> {
  return session(await call<unknown>(t, 'PATCH', `/v1/agent/sessions/${seg(id)}`, { published: on }))
}

/** Where anyone can read a published run's story: the public build route, by org and project. '' without both. */
export const story = (t: Target, org: string | null, project: string): string =>
  org && project ? `${t.api}/v1/agent/builds/${seg(org)}/${seg(project)}` : ''

export interface Watch {
  session?: (s: Session) => void
  event?: (e: Event) => void
  /** Each (re)connect. The first is 0; a later one means frames may have been missed. */
  open?: (attempt: number) => void
}

export interface WatchOptions {
  /** Resume replaying missed events strictly after this sequence number. */
  after?: number
}

/**
 * Follow one run's tree until `signal` aborts. Reconnects with a capped backoff
 * and replays missed frames via `?after=<lastSeq>` + `Last-Event-ID`; a refusal
 * (401/403) ends it, because retrying a no is not recovery.
 */
export async function watch(t: Target, root: string, on: Watch, signal: AbortSignal, opts?: WatchOptions): Promise<void> {
  let opened = 0
  let fails = 0
  let lastSeq = opts?.after && Number.isFinite(opts.after) && opts.after > 0 ? Math.floor(opts.after) : 0
  while (!signal.aborted) {
    try {
      const extra: Record<string, string> = { Accept: 'text/event-stream' }
      if (lastSeq > 0) extra['Last-Event-ID'] = String(lastSeq)
      const res = await fetch(`${t.api}/v1/agent/sessions/stream${query({ root, after: lastSeq > 0 ? lastSeq : undefined })}`, {
        headers: headers(t, extra),
        cache: 'no-store',
        signal,
      })
      if (res.status === 401 || res.status === 403) throw new Refusal(res.status, phrase('This account cannot follow this run'))
      if (!res.ok || !res.body) throw new Error(`stream answered ${res.status}`)
      on.open?.(opened++)
      fails = 0
      await read(
        res.body,
        (f) => {
          let data: unknown
          try {
            data = JSON.parse(f.data)
          } catch {
            return
          }
          if (f.event === 'session') on.session?.(session(obj(data).session))
          else if (f.event === 'event') {
            const ev = event(obj(data).event)
            if (!root || !ev.sessionId || ev.sessionId === root) {
              const fid = Number(f.id)
              if (Number.isInteger(fid) && fid > lastSeq) lastSeq = fid
              if (ev.seq > lastSeq) lastSeq = ev.seq
            }
            on.event?.(ev)
          }
        },
        signal,
      )
    } catch (e) {
      if (e instanceof Refusal) throw e
      if (signal.aborted) return
      fails += 1
    }
    const wait = Math.min(15000, 1000 * 2 ** Math.min(fails, 4))
    await new Promise<void>((done) => {
      const t = setTimeout(done, wait)
      signal.addEventListener('abort', () => (clearTimeout(t), done()), { once: true })
    })
  }
}
