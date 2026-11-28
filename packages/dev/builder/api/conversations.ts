/**
 * The person's Chat conversations, as the agent service keeps them (the list
 * @hanzo/ai `threads.list` and @hanzo/rooms read).
 *
 *   GET /v1/agents/chat/conversations → {conversations: [{id, title, updatedAt, pinned, archived}]}
 *
 * Pinned first, then most recently spoken in; archived ones are left out. Only
 * the caller's own: another member's conversations are never listed.
 */
import { call, type Target } from './call.ts'

export interface Conversation {
  id: string
  /** Taken from the opening message until renamed. */
  title: string
  /** RFC 3339: when it was last spoken in, or ''. */
  updated: string
  pinned: boolean
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})

export function conversation(raw: unknown): Conversation | null {
  const o = obj(raw)
  const id = str(o.id)
  if (!id) return null
  return { id, title: str(o.title), updated: str(o.updatedAt), pinned: o.pinned === true }
}

export async function conversations(t: Target, signal?: AbortSignal): Promise<Conversation[]> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/agents/chat/conversations', undefined, { signal }))
  return (Array.isArray(raw.conversations) ? raw.conversations : []).map(conversation).filter((c): c is Conversation => c !== null)
}
