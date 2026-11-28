/**
 * What Hanzo remembers about the signed-in person, in the org they act in.
 *
 *   GET  /v1/ai/memory/list?limit=  {status, data: [memory]}, newest first
 *   POST /v1/ai/memory/remember     {content} → {status, data: memory}
 *   POST /v1/ai/memory/delete       {id: "<owner>/<name>"}
 *
 * The platform keys every memory on the validated caller and org, never on the
 * request, so these read and change the caller's own and nobody else's. A
 * refusal arrives in the envelope, usually under a 200.
 */
import { phrase } from '@hanzo/i18n'

import { call, query, unwrap, type Target } from './call.ts'

export interface Memory {
  /** `<owner>/<name>`, what a delete names. */
  id: string
  content: string
  /** user, fact, … as the platform files it. */
  kind: string
  /** When it was last written, as the platform wrote it. */
  updated: string
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')

export function memoryOf(raw: unknown): Memory | null {
  const o = (raw && typeof raw === 'object' ? raw : {}) as Record<string, unknown>
  const owner = str(o.owner)
  const name = str(o.name)
  if (!owner || !name) return null
  return { id: `${owner}/${name}`, content: str(o.content), kind: str(o.kind), updated: str(o.updatedTime) || str(o.createdTime) }
}

export async function memories(t: Target, limit = 100): Promise<Memory[]> {
  const rows = unwrap(await call<unknown>(t, 'GET', `/v1/ai/memory/list${query({ limit })}`))
  return (Array.isArray(rows) ? rows : []).map(memoryOf).filter((m): m is Memory => m !== null)
}

export async function remember(t: Target, content: string): Promise<Memory | null> {
  const said = content.trim()
  if (!said) throw new Error(phrase('Say what Hanzo should remember'))
  return memoryOf(unwrap(await call<unknown>(t, 'POST', '/v1/ai/memory/remember', { content: said })))
}

export async function forget(t: Target, id: string): Promise<void> {
  unwrap(await call<unknown>(t, 'POST', '/v1/ai/memory/delete', { id }))
}
