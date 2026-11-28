/**
 * Automations: a task that runs itself, on a schedule or when asked.
 *
 *   GET    /v1/auto/automations            {data: [automation]}
 *   POST   /v1/auto/automations            create one: name and instructions
 *   GET    /v1/auto/automations/{id}       one
 *   PATCH  /v1/auto/automations/{id}       change it, or switch it on or off
 *   DELETE /v1/auto/automations/{id}       delete it, its schedule and its runs
 *   POST   /v1/auto/automations/{id}/run   {run: {id, status}}  one run, now
 *   GET    /v1/auto/automations/{id}/runs  {data: [run]}        newest first
 *   POST   /v1/auto/automations/{id}/runs/{run}/review  {post}  post a held post, or discard it
 *   GET    /v1/auto/templates              {data: [starter]}    to start a new one from
 *   POST   /v1/auto/cron                   {schedule, says, next}  a schedule from a person's words
 *   GET    /v1/auto/jobs                   {data: [job]}        what runs without you, across products
 *   GET    /v1/auto/jobs/{id}              one job
 *   GET    /v1/auto/events                 text/event-stream: `job` and `automation`, the reader's own
 *
 * Each run is one Dev run in this organization, started as the person who last
 * saved what it does. A draft is a flow named and not yet told what to do: it
 * runs once its instructions are saved.
 */
import { phrase } from '@hanzo/i18n'
import type { Say } from '@hanzo/i18n/react'

import { english } from '../say.ts'
import { call, headers, query, reason, Refusal, seg, type Target } from './call.ts'
import { inApp, type Source } from './library.ts'
import { read as stream } from './sse.ts'

export type { Source }

export type Kind = 'manual' | 'once' | 'hourly' | 'daily' | 'weekdays' | 'weekly' | 'cron'
export type Permissions = 'ask' | 'auto'

/** When it runs, in the person's own words and zone. */
export interface Schedule {
  kind: Kind
  /** HH:MM, 24-hour, for daily, weekdays and weekly; 00:MM for hourly; YYYY-MM-DDTHH:MM for once, absent to run now. */
  at?: string
  /** mon … sun, for weekly. */
  day?: string
  /** Five fields, for cron. */
  cron?: string
  /** An IANA zone; the platform reads an absent one as UTC. */
  tz?: string
}

/** What an automation is: what the editor holds and the platform keeps. */
export interface Draft {
  name: string
  instructions: string
  /** A model id, or null for Enso, the default. */
  model: string | null
  schedule: Schedule
  permissions: Permissions
  notify: boolean
  /** The connected accounts each run's answer is posted to: x, linkedin, facebook. */
  postTo: string[]
  enabled: boolean
  /** The product that made it: chat, dev, studio or workflow. */
  source: Source
}

export interface Last {
  id: string
  status: string
  at: string
  summary: string
}

export interface Automation extends Draft {
  id: string
  project: string | null
  /** Named and not yet told what to do: it never runs until its instructions are saved. */
  draft: boolean
  /** When it runs next, RFC 3339, or null when it runs only when asked or is off. */
  next: string | null
  last: Last | null
  created: string
  updated: string
}

/** Where one post went: its link, or the platform's reason it did not go. */
export interface Posted {
  to: string
  url: string
  error: string
}

/** One run: succeeded, failed, running, queued, review (a post waiting for you), skipped or refused. */
export interface Run {
  id: string
  status: string
  at: string
  finished: string | null
  summary: string
  /** The Dev run it started, or null when it started none. */
  session: string | null
  /** The post its agent wrote, for an automation that posts. */
  draft: string
  posts: Posted[]
  /** The Library items it made. */
  outputs: string[]
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const nullable = (v: unknown): string | null => (typeof v === 'string' && v ? v : null)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
const KINDS: readonly Kind[] = ['manual', 'once', 'hourly', 'daily', 'weekdays', 'weekly', 'cron']
const SOURCES: readonly Source[] = ['chat', 'dev', 'studio', 'workflow']
const sourceOf = (v: unknown): Source => (SOURCES.includes(v as Source) ? (v as Source) : 'dev')
const strings = (v: unknown): string[] => (Array.isArray(v) ? v : []).filter((x): x is string => typeof x === 'string' && x !== '')

function schedule(raw: unknown): Schedule {
  const o = obj(raw)
  const kind = KINDS.includes(o.kind as Kind) ? (o.kind as Kind) : 'manual'
  const out: Schedule = { kind }
  for (const k of ['at', 'day', 'cron', 'tz'] as const) if (str(o[k])) out[k] = str(o[k])
  return out
}

/** One automation as the platform answers it, or null for a row with no id. */
export function read(raw: unknown): Automation | null {
  const o = obj(raw)
  const id = str(o.id)
  if (!id) return null
  const last = obj(o.last)
  return {
    id,
    name: str(o.name) || id,
    instructions: str(o.instructions),
    project: nullable(o.project),
    model: nullable(o.model),
    schedule: schedule(o.schedule),
    permissions: o.permissions === 'auto' ? 'auto' : 'ask',
    notify: o.notify === true,
    postTo: (Array.isArray(o.postTo) ? o.postTo : []).filter((x): x is string => typeof x === 'string'),
    enabled: o.enabled === true,
    source: sourceOf(o.source),
    draft: o.draft === true,
    next: nullable(o.next),
    last: str(last.id) ? { id: str(last.id), status: str(last.status), at: str(last.at), summary: str(last.summary) } : null,
    created: str(o.created),
    updated: str(o.updated),
  }
}

/** The Dev run a transcript link opens: `…/dev?run=sess_…`. */
function session(link: unknown): string | null {
  const m = str(link).match(/[?&]run=(sess_[0-9a-f]{32})\b/)
  return m ? m[1]! : null
}

const list = (raw: unknown): unknown[] => (Array.isArray(obj(raw).data) ? (obj(raw).data as unknown[]) : [])

export async function automations(t: Target): Promise<Automation[]> {
  return list(await call<unknown>(t, 'GET', '/v1/auto/automations'))
    .map(read)
    .filter((a): a is Automation => a !== null)
}

export async function automation(t: Target, id: string): Promise<Automation> {
  const a = read(await call<unknown>(t, 'GET', `/v1/auto/automations/${seg(id)}`))
  if (!a) throw new Error(phrase('The automation came back without an id'))
  return a
}

export async function create(t: Target, d: Draft): Promise<Automation> {
  const a = read(await call<unknown>(t, 'POST', '/v1/auto/automations', d))
  if (!a) throw new Error(phrase('The automation was created and came back without an id'))
  return a
}

/** What changed between two drafts, field by field: what a save sends. */
export function changes(was: Draft, now: Draft): Partial<Draft> {
  const out: Partial<Draft> = {}
  for (const k of Object.keys(now) as (keyof Draft)[]) {
    if (JSON.stringify(was[k]) !== JSON.stringify(now[k])) (out as Record<string, unknown>)[k] = now[k]
  }
  return out
}

/** Change an automation. A change to anything but `enabled` makes the caller the person it runs as. */
export async function save(t: Target, id: string, change: Partial<Draft>): Promise<Automation> {
  const a = read(await call<unknown>(t, 'PATCH', `/v1/auto/automations/${seg(id)}`, change))
  if (!a) throw new Error(phrase('The automation was saved and came back without an id'))
  return a
}

export async function remove(t: Target, id: string): Promise<void> {
  await call(t, 'DELETE', `/v1/auto/automations/${seg(id)}`)
}

/** Start one run now. While one is going, the platform records this start as skipped. */
export async function start(t: Target, id: string): Promise<{ id: string; status: string }> {
  const run = obj(obj(await call<unknown>(t, 'POST', `/v1/auto/automations/${seg(id)}/run`)).run)
  return { id: str(run.id), status: str(run.status) }
}

export async function runs(t: Target, id: string): Promise<Run[]> {
  return list(await call<unknown>(t, 'GET', `/v1/auto/automations/${seg(id)}/runs`))
    .map((raw) => {
      const o = obj(raw)
      return run(o)
    })
    .filter((r) => r.id)
}

function run(o: Record<string, unknown>): Run {
  const posts = (Array.isArray(o.posts) ? o.posts : [])
    .map(obj)
    .filter((p) => str(p.to))
    .map((p) => ({ to: str(p.to), url: /^https:\/\//.test(str(p.url)) ? str(p.url) : '', error: str(p.error) }))
  return { id: str(o.id), status: str(o.status), at: str(o.at), finished: nullable(o.finished), summary: str(o.summary), session: session(o.transcript), draft: str(o.draft), posts, outputs: strings(o.outputs) }
}

/** Post a held post to the automation's accounts, or discard it. */
export async function review(t: Target, id: string, runId: string, post: boolean): Promise<Run> {
  return run(obj(await call<unknown>(t, 'POST', `/v1/auto/automations/${seg(id)}/runs/${seg(runId)}/review`, { post })))
}

/** A weekly schedule's sentence by its day: one sentence each, so a language says the day its own way. */
const DAYS: Record<string, string> = {
  mon: phrase('Mondays at {time} · {zone}'),
  tue: phrase('Tuesdays at {time} · {zone}'),
  wed: phrase('Wednesdays at {time} · {zone}'),
  thu: phrase('Thursdays at {time} · {zone}'),
  fri: phrase('Fridays at {time} · {zone}'),
  sat: phrase('Saturdays at {time} · {zone}'),
  sun: phrase('Sundays at {time} · {zone}'),
}

/** A once schedule's local date, as a person reads it, and its time. */
function once(at: string): { date: string; time: string } | null {
  const m = at.match(/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})$/)
  if (!m) return null
  const d = new Date(Number(m[1]), Number(m[2]) - 1, Number(m[3]), Number(m[4]), Number(m[5]))
  return { date: d.toLocaleDateString(undefined, { weekday: 'short', month: 'short', day: 'numeric' }), time: `${m[4]}:${m[5]}` }
}

/** A schedule the way a person says it, in the words of the `t` handed (English without one). */
export function words(s: Schedule, t: Say = english): string {
  const zone = s.tz || 'UTC'
  switch (s.kind) {
    case 'manual':
      return t('When you run it')
    case 'once': {
      if (!s.at) return t('Once, now')
      const when = once(s.at)
      return when ? t('Once, {date} at {time} · {zone}', { ...when, zone }) : t('Once, {at} · {zone}', { at: s.at, zone })
    }
    case 'hourly':
      return t('Every hour at :{minute}', { minute: (s.at ?? '00:00').slice(3) || '00' })
    case 'daily':
      return t('Every day at {time} · {zone}', { time: s.at ?? '', zone })
    case 'weekdays':
      return t('Weekdays at {time} · {zone}', { time: s.at ?? '', zone })
    case 'weekly': {
      const day = DAYS[s.day ?? '']
      return day ? t(day, { time: s.at ?? '', zone }) : t('{day} at {time} · {zone}', { day: s.day ?? '', time: s.at ?? '', zone })
    }
    case 'cron': {
      // The two a person says most, in their words; any other expression as written.
      const minutes = (s.cron ?? '').match(/^\*\/(\d+) \* \* \* \*$/)
      if (minutes) return t('Every {n} minutes · {zone}', { n: minutes[1]!, zone })
      const hours = (s.cron ?? '').match(/^0 \*\/(\d+) \* \* \*$/)
      if (hours) return t('Every {n} hours · {zone}', { n: hours[1]!, zone })
      return t('Cron {cron} · {zone}', { cron: s.cron ?? '', zone })
    }
  }
}

/** A ready-made automation to start a new one from. Its schedule names no zone: the page fills in the person's. */
export interface Starter {
  key: string
  name: string
  description: string
  instructions: string
  schedule: Schedule
}

/** The starters, from GET /v1/auto/templates. */
export async function starters(t: Target): Promise<Starter[]> {
  return list(await call<unknown>(t, 'GET', '/v1/auto/templates'))
    .map((raw) => {
      const o = obj(raw)
      return { key: str(o.key), name: str(o.name), description: str(o.description), instructions: str(o.instructions), schedule: schedule(o.schedule) }
    })
    .filter((x) => x.key && x.name && x.instructions)
}

/** What a person's words mean as a schedule: the schedule, how the platform says it back, and its next run. */
export interface Parsed {
  schedule: Schedule
  says: string
  /** RFC 3339, or null for one that runs only when asked. */
  next: string | null
}

/** A schedule from a person's own words ("every monday at 9am"), read in `tz`. */
export async function cron(t: Target, text: string, tz: string, init?: { signal?: AbortSignal }): Promise<Parsed> {
  const o = obj(await call<unknown>(t, 'POST', '/v1/auto/cron', { text, tz }, init))
  return { schedule: schedule(o.schedule), says: str(o.says), next: nullable(o.next) }
}

/** What a job is doing, in the five words every product shares. */
export type State = 'scheduled' | 'running' | 'succeeded' | 'failed' | 'cancelled'
export const STATES: readonly State[] = ['scheduled', 'running', 'succeeded', 'failed', 'cancelled']

/** One run of anything that runs without its person watching. */
export interface Job {
  id: string
  product: Source
  kind: string
  title: string
  state: State
  /** The product's own word for it: queued, review, skipped… */
  status: string
  /** 0 to 1, or null when the product does not say. */
  progress: number | null
  automation: string | null
  /** The Dev run it started, whose transcript is its log. */
  session: string | null
  ref: string
  origin: string
  summary: string
  error: string
  /** The Library items it made. */
  outputs: string[]
  /** Cents, or null while unknown. */
  cost: number | null
  started: string
  finished: string | null
  updated: string
}

/** A state the platform did not name is read from the product's own word. */
function stateOf(state: unknown, status: string): State {
  if (STATES.includes(state as State)) return state as State
  switch (status) {
    case 'queued':
      return 'scheduled'
    case 'running':
    case 'review':
      return 'running'
    case 'succeeded':
      return 'succeeded'
    case 'failed':
    case 'refused':
      return 'failed'
    case 'skipped':
      return 'cancelled'
  }
  return 'scheduled'
}

/** One job as the platform answers it, or null for a row with no id. */
export function job(raw: unknown): Job | null {
  const o = obj(raw)
  const id = str(o.id)
  if (!id) return null
  const status = str(o.status)
  const progress = typeof o.progress === 'number' && Number.isFinite(o.progress) ? Math.min(1, Math.max(0, o.progress)) : null
  const cost = typeof o.cost === 'number' && Number.isFinite(o.cost) && o.cost >= 0 ? Math.round(o.cost) : null
  const sess = str(o.session)
  return {
    id,
    product: sourceOf(o.product),
    kind: str(o.kind),
    title: str(o.title) || id,
    state: stateOf(o.state, status),
    status,
    progress,
    automation: nullable(o.automation),
    session: /^sess_[0-9a-f]{32}$/.test(sess) ? sess : null,
    ref: str(o.ref),
    origin: inApp(o.origin),
    summary: str(o.summary),
    error: str(o.error),
    outputs: strings(o.outputs),
    cost,
    started: str(o.started),
    finished: nullable(o.finished),
    updated: str(o.updated),
  }
}

export interface JobFilter {
  product?: Source | ''
  state?: State | ''
  limit?: number
}

export async function jobs(t: Target, f: JobFilter = {}): Promise<Job[]> {
  return list(await call<unknown>(t, 'GET', `/v1/auto/jobs${query({ product: f.product, state: f.state, limit: f.limit })}`))
    .map(job)
    .filter((j): j is Job => j !== null)
}

export async function one(t: Target, id: string): Promise<Job> {
  const j = job(await call<unknown>(t, 'GET', `/v1/auto/jobs/${seg(id)}`))
  if (!j) throw new Error(phrase('The job came back without an id'))
  return j
}

/** What the events stream says: a job or an automation, as it stands now. */
export type Heard = { kind: 'job'; job: Job } | { kind: 'automation'; automation: Automation }

/** One frame of /v1/auto/events, or null for one this page does not read. */
export function heard(event: string, data: string): Heard | null {
  let raw: unknown
  try {
    raw = JSON.parse(data)
  } catch {
    return null
  }
  if (event === 'job') {
    const j = job(raw)
    return j ? { kind: 'job', job: j } : null
  }
  if (event === 'automation') {
    const a = read(raw)
    return a ? { kind: 'automation', automation: a } : null
  }
  return null
}

/**
 * Read /v1/auto/events until `signal` ends it or the stream closes, calling `on`
 * for each job and automation it names. A fetch, because EventSource cannot
 * carry the bearer.
 */
export async function events(t: Target, on: (h: Heard) => void, signal: AbortSignal, opened?: () => void): Promise<void> {
  const res = await fetch(`${t.api}/v1/auto/events`, { headers: headers(t, { Accept: 'text/event-stream' }), cache: 'no-store', signal })
  if (!res.ok) throw new Refusal(res.status, (await reason(res)) || `GET /v1/auto/events answered ${res.status}`)
  if (!res.body) return
  opened?.()
  await stream(
    res.body,
    (f) => {
      const h = heard(f.event, f.data)
      if (h) on(h)
    },
    signal,
  )
}
