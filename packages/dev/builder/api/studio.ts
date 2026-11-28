/**
 * Hanzo Studio: a creation is a run of a template, its files kept in the Library.
 *
 *   GET  /v1/studio/templates           {data: [template]}  the public catalog, then the org's own
 *   POST /v1/studio/runs                {template, input, title?, parent?} → 201 run, queued
 *   GET  /v1/studio/runs/{id}           the run: status, text, outputs
 *   POST /v1/studio/runs/{id}/cancel    stop it; what it reserved is released
 *
 * A run is priced and its estimate reserved against the org's wallet before it
 * is queued (402 when the wallet cannot cover it). `parent` makes the run the
 * next version of that one. Its outputs are Library items: their `url` is
 * signed and expires, their `origin` is the app path that opens the run in
 * Studio and does not.
 */
import { phrase } from '@hanzo/i18n'

import { call, Refusal, seg, Sentence, type Target } from './call.ts'
import { item, type Item } from './library.ts'

export interface Run {
  id: string
  /** queued, running, succeeded, failed or cancelled. */
  status: string
  title: string
  /** What the run's writing step said, when its product is words. */
  text: string
  outputs: Item[]
  /** Why it failed, in the platform's words, or ''. */
  error: string
}

export interface Start {
  template: string
  input: Record<string, unknown>
  title?: string
  parent?: string
  /** A brand (design system) the run reads as brand.* — its palette, type and voice. */
  brand?: string
}

/**
 * A design system: one of Studio's brands, the one store for them. Its tokens
 * are CSS custom properties (--background, --primary, --font-sans, --radius-md,
 * …, the form Studio's renderer reads), with `--component-*` and `--voice-*` as
 * quoted lines a model follows (design.ts `tokensOf`).
 *
 *   GET  /v1/studio/brands        {data: [brand]}   the ones you may read
 *   GET  /v1/studio/brands/{id}   one
 *   POST /v1/studio/brands        {name, tokens, shared?} → brand
 */
export interface Brand {
  id: string
  name: string
  tokens: Record<string, unknown>
}

export function brandOf(raw: unknown): Brand {
  const o = obj(raw)
  return { id: str(o.id), name: str(o.name), tokens: obj(o.tokens) }
}

export async function brands(t: Target, init?: { signal?: AbortSignal }): Promise<Brand[]> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/studio/brands', undefined, init))
  return (Array.isArray(raw.data) ? raw.data : []).map(brandOf).filter((b) => b.id)
}

export async function brand(t: Target, id: string, init?: { signal?: AbortSignal }): Promise<Brand> {
  return brandOf(await call<unknown>(t, 'GET', `/v1/studio/brands/${seg(id)}`, undefined, init))
}

export async function keepBrand(t: Target, b: { name: string; tokens: Record<string, unknown> }, signal?: AbortSignal): Promise<Brand> {
  return brandOf(await call<unknown>(t, 'POST', '/v1/studio/brands', { name: b.name, tokens: b.tokens }, { signal }))
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})

export function run(raw: unknown): Run {
  const o = obj(raw)
  const e = obj(o.error)
  return {
    id: str(o.id),
    status: str(o.status),
    title: str(o.title),
    text: str(o.text),
    outputs: (Array.isArray(o.outputs) ? o.outputs : []).map(item).filter((x): x is Item => x !== null),
    error: str(e.detail) || str(e.title),
  }
}

/** A run that has ended, one way or the other. */
export const ended = (r: Run): boolean => r.status !== 'queued' && r.status !== 'running' && r.status !== ''

/** The ids of the templates this org may run. */
export async function templates(t: Target, init?: { signal?: AbortSignal }): Promise<string[]> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/studio/templates', undefined, init))
  return (Array.isArray(raw.data) ? raw.data : []).map((x) => str(obj(x).id)).filter(Boolean)
}

/** How often a run is read while it works, and the longest it is waited for. */
const EVERY = 2_000
const LONGEST = 5 * 60_000

const pause = (ms: number, signal?: AbortSignal) =>
  new Promise<void>((done, fail) => {
    const id = setTimeout(done, ms)
    signal?.addEventListener(
      'abort',
      () => {
        clearTimeout(id)
        fail(signal.reason)
      },
      { once: true },
    )
  })

/**
 * Start a run and wait for it to end. Aborted, the run is cancelled so its
 * reservation is released; a run that fails is thrown with its reason.
 */
export async function make(t: Target, s: Start, signal?: AbortSignal): Promise<Run> {
  let r = run(
    await call<unknown>(t, 'POST', '/v1/studio/runs', s, { signal }).catch((e: unknown) => {
      throw e instanceof Refusal && e.status === 402 ? new Error(phrase('Your balance does not cover it. Add credit in Settings › Billing.')) : e
    }),
  )
  if (!r.id) throw new Error(phrase('Studio started the run and named no id'))
  const id = r.id
  const stop = () => void call(t, 'POST', `/v1/studio/runs/${seg(id)}/cancel`, {}).catch(() => undefined)
  signal?.addEventListener('abort', stop, { once: true })
  try {
    const until = Date.now() + LONGEST
    while (!ended(r)) {
      if (Date.now() > until) throw new Error(phrase('Studio is still making it. It will be in your Library when it is done.'))
      await pause(EVERY, signal)
      r = run(await call<unknown>(t, 'GET', `/v1/studio/runs/${seg(id)}`, undefined, { signal }))
    }
  } finally {
    signal?.removeEventListener('abort', stop)
  }
  if (r.status !== 'succeeded') throw r.error ? new Error(r.error) : new Sentence(phrase('Studio could not make it ({status})'), { status: r.status })
  return r
}
