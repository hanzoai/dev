/**
 * The organization's webhooks: https endpoints that each matching platform event
 * is POSTed to, signed.
 *
 *   GET    /v1/webhook                    {data: [endpoint]}, newest first, secrets withheld
 *   POST   /v1/webhook                    {url, events, description} → 201, the secret shown this once
 *   DELETE /v1/webhook/{id}               204
 *   POST   /v1/webhook/{id}/test          one signed test event, sent now → {delivered, httpStatus, durationMs, error}
 *   GET    /v1/webhook/{id}/deliveries    {data: [attempt]}, newest first
 *
 * The org is the caller's; an endpoint another org owns reads as not found.
 * Events are subject patterns (`commerce.order.>`); none means every event.
 */
import { phrase } from '@hanzo/i18n'

import { call, query, seg, type Target } from './call.ts'

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
const arr = (v: unknown): unknown[] => (Array.isArray(v) ? v : [])

export interface Endpoint {
  id: string
  url: string
  /** Subject patterns; empty is every event. */
  events: string[]
  description: string
  /** active | disabled */
  status: string
  /** The signing secret: present only in the answer to a create. */
  secret: string
  created: string
  /** Settled in the last seven days, and how many of those failed. */
  deliveries: number
  failures: number
}

export function endpoint(raw: unknown): Endpoint {
  const e = obj(raw)
  return {
    id: str(e.id),
    url: str(e.url),
    events: arr(e.events).filter((s): s is string => typeof s === 'string' && s !== ''),
    description: str(e.description),
    status: str(e.status) || 'active',
    secret: str(e.secret),
    created: str(e.created),
    deliveries: num(e.deliveries7d),
    failures: num(e.failures7d),
  }
}

export async function endpoints(t: Target): Promise<Endpoint[]> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/webhook'))
  return arr(raw.data).map(endpoint).filter((e) => e.id)
}

/** The patterns in what a person typed: comma or space separated. */
export function patterns(typed: string): string[] {
  return [...new Set(typed.split(/[\s,]+/).map((s) => s.trim()).filter(Boolean))]
}

export interface Draft {
  url: string
  events: string[]
  description: string
}

/** Why a draft cannot be sent, or ''. The platform refuses the same things. English, marked (`phrase`): a screen says it through `t`. */
export function refuse(d: Draft): string {
  let u: URL
  try {
    u = new URL(d.url.trim())
  } catch {
    return phrase('Enter the https address to deliver to')
  }
  if (u.protocol !== 'https:') return phrase('A webhook is delivered over https only')
  if (d.events.length > 64) return phrase('At most 64 event patterns')
  return ''
}

export async function add(t: Target, d: Draft): Promise<Endpoint> {
  const why = refuse(d)
  if (why) throw new Error(why)
  return endpoint(await call<unknown>(t, 'POST', '/v1/webhook', { url: d.url.trim(), events: d.events, description: d.description.trim() }))
}

export async function remove(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/webhook/${seg(id)}`)
}

export interface Trial {
  delivered: boolean
  /** 0 when the endpoint never answered. */
  status: number
  ms: number
  error: string
}

export async function test(t: Target, id: string): Promise<Trial> {
  const r = obj(await call<unknown>(t, 'POST', `/v1/webhook/${seg(id)}/test`, {}))
  return { delivered: r.delivered === true, status: num(r.httpStatus), ms: num(r.durationMs), error: str(r.error) }
}

export interface Attempt {
  subject: string
  /** ok | retrying | failed */
  status: string
  code: number
  attempt: number
  error: string
  at: string
}

export async function deliveries(t: Target, id: string, limit = 10): Promise<Attempt[]> {
  const raw = obj(await call<unknown>(t, 'GET', `/v1/webhook/${seg(id)}/deliveries${query({ limit })}`))
  return arr(raw.data)
    .map(obj)
    .map((a) => ({
      subject: str(a.subject),
      status: str(a.status),
      code: num(a.httpStatus),
      attempt: num(a.attempt),
      error: str(a.error),
      at: str(a.created),
    }))
}
