/**
 * The fleet's native MCP servers, as the MCP server itself lists them.
 *
 *   POST /v1/mcp  {"method":"tools/list"} → {result:{tools:[…]}}
 *
 * One tool is one native server. Its operations are the names in `op`. A run
 * starts a server the first time it calls it; the server stays up afterwards.
 * `describe` is how a caller reads one operation's schema, so it is not a server.
 */
import { phrase } from '@hanzo/i18n'

import { call, Refusal, type Target } from './call.ts'

export interface Native {
  name: string
  description: string
  ops: string[]
}

const words = (v: unknown): string[] => (Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string' && x !== '') : [])

/** The servers in one tools/list answer. A missing tool array is a refusal. */
export function nativesOf(body: unknown): Native[] {
  const root = body && typeof body === 'object' ? (body as Record<string, unknown>) : {}
  const err = root.error
  if (err && typeof err === 'object') {
    const message = (err as { message?: unknown }).message
    throw new Refusal(200, typeof message === 'string' && message ? message : phrase('The MCP server refused the list'))
  }
  const result = root.result && typeof root.result === 'object' ? (root.result as Record<string, unknown>) : null
  const tools = result && Array.isArray(result.tools) ? result.tools : null
  if (!tools) throw new Refusal(502, phrase('The MCP server did not list its tools'))
  const out: Native[] = []
  for (const raw of tools) {
    if (!raw || typeof raw !== 'object') continue
    const t = raw as Record<string, unknown>
    const name = typeof t.name === 'string' ? t.name : ''
    if (!name || name === 'describe') continue
    const schema = t.inputSchema && typeof t.inputSchema === 'object' ? (t.inputSchema as Record<string, unknown>) : {}
    const props = schema.properties && typeof schema.properties === 'object' ? (schema.properties as Record<string, unknown>) : {}
    const op = props.op && typeof props.op === 'object' ? (props.op as Record<string, unknown>) : {}
    out.push({
      name,
      description: typeof t.description === 'string' ? t.description : '',
      ops: words(op.enum),
    })
  }
  return out
}

/** Every native server the fleet MCP server is serving. */
export async function natives(t: Target): Promise<Native[]> {
  return nativesOf(
    await call<unknown>(t, 'POST', '/v1/mcp', { jsonrpc: '2.0', id: 1, method: 'tools/list' }),
  )
}
