/**
 * A person's API keys, each its own credential: as many as they like, of either
 * type — secret (sk-, it belongs on a server) or publishable (pk-, safe in a
 * page's source).
 *
 *   GET    /v1/account/keys        {keys: [{id, name, type, prefix, key?, status, limit?, created, …}]}
 *   POST   /v1/account/keys        {name, type, limit?} → the key, its secret answered once
 *   DELETE /v1/account/keys/{id}   revoke exactly that one; it stays listed as revoked
 *
 * Creating a key never touches another. A secret key is answered once and by its
 * prefix after; a publishable key is public by construction and always comes back
 * whole. A limit only narrows what a key may reach — `model:zen5`,
 * `project:acme`, `product:train`, `read:*`.
 */
import { phrase } from '@hanzo/i18n'

import { call, Sentence, type Target } from './call.ts'

export const KINDS = ['secret', 'publishable'] as const
export type Kind = (typeof KINDS)[number]

export interface Key {
  id: string
  name: string
  type: Kind
  /** The recognizable head, never enough to use the key. */
  prefix: string
  /** The whole key: a publishable key's only. */
  key: string
  /** active, expired, revoked or disabled. */
  status: string
  /** `kind:name` entries; none reaches whatever its holder does. */
  limit: string[]
  /** When it was made, as IAM records it. */
  created: string
}

export interface Minted {
  id: string
  name: string
  type: Kind
  /** The key itself. A secret one is never readable again. */
  key: string
  limit: string[]
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const list = (v: unknown): string[] => (Array.isArray(v) ? v.filter((s): s is string => typeof s === 'string' && s !== '') : [])
const kind = (v: unknown): Kind | null => (KINDS.includes(v as Kind) ? (v as Kind) : null)

export async function keys(t: Target): Promise<Key[]> {
  const r = await call<{ keys?: unknown }>(t, 'GET', '/v1/account/keys')
  const rows = Array.isArray(r?.keys) ? r.keys : []
  const out: Key[] = []
  for (const raw of rows) {
    const o = (raw && typeof raw === 'object' ? raw : {}) as Record<string, unknown>
    const type = kind(o.type)
    if (!type || !str(o.id)) continue
    out.push({
      id: str(o.id),
      name: str(o.name),
      type,
      prefix: str(o.prefix),
      key: str(o.key),
      status: str(o.status) || 'active',
      limit: list(o.limit),
      created: str(o.created),
    })
  }
  return out
}

/** A `kind:name` limit: a lowercase kind, a colon, and a name or `*`. */
export const LIMIT = /^[a-z][a-z0-9-]*:[^\s,:]+$/

/** The limits a person typed, split on commas and spaces; throws on the first that is not one. */
export function limits(raw: string): string[] {
  const out = raw
    .split(/[\s,]+/)
    .map((s) => s.trim())
    .filter(Boolean)
  const bad = out.find((s) => !LIMIT.test(s))
  if (bad) throw new Sentence(phrase('{limit} is not a limit — write it as kind:name, like model:zen5'), { limit: bad })
  return out
}

/** Create a new key beside the ones held. The answer is the only time a secret key is shown. */
export async function mint(t: Target, type: Kind, name = '', limit: string[] = []): Promise<Minted> {
  const body: Record<string, unknown> = { type }
  if (name.trim()) body.name = name.trim()
  if (limit.length) body.limit = limit
  const r = await call<Record<string, unknown>>(t, 'POST', '/v1/account/keys', body)
  const key = str(r?.key)
  if (!key) throw new Error(phrase('The platform answered no key'))
  return { id: str(r?.id), name: str(r?.name), type: kind(r?.type) ?? type, key, limit: list(r?.limit) }
}

/** Revoke exactly one key, by id. Every other key keeps working. */
export async function revoke(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/account/keys/${encodeURIComponent(id)}`)
}
