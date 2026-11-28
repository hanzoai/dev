/**
 * One request path to the platform.
 *
 * Every typed op below goes through `call`, so the bearer, the org scope, the
 * no-store cache rule and the reading of a refusal are decided once. The
 * platform answers a refusal as RFC 9457 problem details; older handlers still
 * answer `{error:{message}}` or `{msg}`, and all three read the same way here.
 *
 * A sentence this client writes is English, marked (`phrase`) for the
 * translator: a module is not a component and asks no `t`, so a screen says
 * what a module threw through its own (`told`), and a module that builds text
 * takes the screen's `t` as its last argument, English (../say.ts `english`)
 * when none is handed.
 */
import { phrase, translate } from '@hanzo/i18n'
import type { Say } from '@hanzo/i18n/react'

import { english } from '../say.ts'

/** Where the platform is and who is asking. The host supplies it. */
export interface Target {
  /** The platform origin, no trailing slash: calls are `${api}/v1/…`. */
  api: string
  /** The signed-in person's bearer, or null for nobody. */
  token: () => string | null
  /**
   * Obtain a fresh bearer when the stored token is missing/expired (`force = false`)
   * or after a 401 (`force = true`). Absent, `call` sends `token()` as-is.
   */
  refresh?: (force?: boolean) => Promise<string | null>
  /** The org every call is scoped to, sent as `X-Org-Id`. */
  org: string | null
}

/** A refusal: the status, the platform's own sentence for it, and the code it names the refusal by ('' for none). */
export class Refusal extends Error {
  constructor(
    readonly status: number,
    message: string,
    readonly code = '',
  ) {
    super(message)
    this.name = 'Refusal'
  }
}

/**
 * A sentence this client wrote with something of the moment in it (a file's
 * name, an address), thrown where no `t` is at hand: its message is the
 * English, filled, and a screen says it from its source and values.
 */
export class Sentence extends Error {
  constructor(
    readonly source: string,
    readonly values: Readonly<Record<string, string | number>> = {},
  ) {
    super(translate({}, source, values))
  }
}

/**
 * An error's own sentence, as the screen that caught it says it: one this
 * client wrote, in the reader's language; the platform's, which is no key, as given.
 */
export function told(e: unknown, t: Say = english): string {
  if (e instanceof Sentence) return t(e.source, e.values)
  return t(e instanceof Error ? e.message : String(e))
}

export type Query = Record<string, string | number | boolean | null | undefined>

/** `?a=1&b=2` from the defined, non-empty entries of `q`, or ''. */
export function query(q: Query = {}): string {
  const s = new URLSearchParams()
  for (const [k, v] of Object.entries(q)) {
    if (v === undefined || v === null || v === '') continue
    s.set(k, String(v))
  }
  const out = s.toString()
  return out ? `?${out}` : ''
}

/** Whether a JWT's `exp` claim is expired (or within 30s clock skew). Opaque tokens return false. */
export function isExpired(token: string, now = Date.now()): boolean {
  const parts = token.split('.')
  if (parts.length !== 3 || !parts[1]) return false
  try {
    const b64 = parts[1].replace(/-/g, '+').replace(/_/g, '/')
    const pad = b64.length % 4 ? '='.repeat(4 - (b64.length % 4)) : ''
    const payload = JSON.parse(atob(b64 + pad)) as { exp?: unknown }
    return typeof payload.exp === 'number' && payload.exp * 1000 <= now + 30_000
  } catch {
    return false
  }
}

/** The headers every call carries: the bearer and the org, when there are ones. */
export function headers(t: Target, extra?: HeadersInit, resolvedToken?: string | null): Headers {
  const h = new Headers(extra)
  const token = resolvedToken !== undefined ? resolvedToken : t.token()
  if (token && !h.has('Authorization')) h.set('Authorization', `Bearer ${token}`)
  if (t.org && !h.has('X-Org-Id')) h.set('X-Org-Id', t.org)
  return h
}

/** The sentence a refusal carries, whichever envelope it arrived in, and the problem's `code` when it names one. */
async function problem(res: Response): Promise<{ why: string; code: string }> {
  try {
    const body = (await res.json()) as {
      detail?: string
      title?: string
      code?: unknown
      error?: { message?: string } | string
      msg?: string
      message?: string
    }
    const err = typeof body.error === 'string' ? body.error : body.error?.message
    return { why: body.detail || err || body.msg || body.message || body.title || '', code: typeof body.code === 'string' ? body.code : '' }
  } catch {
    return { why: '', code: '' }
  }
}

/** The sentence a refusal carries, whichever envelope it arrived in. */
export async function reason(res: Response): Promise<string> {
  return (await problem(res)).why
}

/** A path segment, escaped. Ids and names from a row never compose a path raw. */
export const seg = (s: string): string => encodeURIComponent(s)

/**
 * One JSON call. Throws `Refusal` on any non-2xx, with the platform's reason, or
 * the status when it gave none. A 204 answers `undefined`.
 */
export async function call<T>(
  t: Target,
  method: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE',
  path: string,
  body?: unknown,
  init?: { signal?: AbortSignal },
): Promise<T> {
  let token = t.token()
  if (t.refresh && (!token || isExpired(token)) && !init?.signal?.aborted) {
    try {
      const fresh = await t.refresh(false)
      if (fresh) token = fresh
    } catch {
      /* keep the current token; a 401 refusal below still surfaces */
    }
  }
  const extra = body === undefined ? undefined : { 'Content-Type': 'application/json' }
  const payload = body === undefined ? undefined : JSON.stringify(body)
  const send = (tok: string | null) =>
    fetch(`${t.api}${path}`, {
      method,
      headers: headers(t, extra, tok),
      body: payload,
      // A run, a repo list and a status are facts about now. A cached refusal
      // outlives the fix.
      cache: 'no-store',
      signal: init?.signal,
    })
  let res = await send(token)
  if (res.status === 401 && t.refresh && !init?.signal?.aborted) {
    try {
      const fresh = await t.refresh(true)
      if (fresh && fresh !== token) {
        token = fresh
        res = await send(token)
      }
    } catch {
      /* fall through to the original 401 refusal */
    }
  }
  if (!res.ok) {
    const { why, code } = await problem(res)
    throw new Refusal(res.status, why || `${method} ${path.split('?')[0]} answered ${res.status}`, code)
  }
  if (res.status === 204) return undefined as T
  const text = await res.text()
  return (text ? JSON.parse(text) : undefined) as T
}

/**
 * The `{status, msg, data}` envelope IAM and the model service answer with. A
 * refusal there is `status: "error"` with its sentence in `msg`, whatever the
 * HTTP status was — often 200 — so it is read here and thrown like any other.
 */
export function unwrap(raw: unknown): unknown {
  const r = (raw && typeof raw === 'object' ? raw : {}) as { status?: unknown; msg?: unknown; data?: unknown }
  if (r.status === 'error') throw new Refusal(400, typeof r.msg === 'string' && r.msg ? r.msg : phrase('The platform refused that'))
  return r.data
}
