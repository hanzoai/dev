/**
 * The org's tool plane: every tool it can reach, and which of them are on.
 *
 *   GET /v1/tool?source=&activated=true   {tools: [{name, source, description, activated, dispatchable}]}
 *   GET /v1/tool/activation               {enabled: [name]}
 *   PUT /v1/tool/activation               {activate: [name], deactivate: [name]} → {enabled: [name]}
 *
 * A tool is on per org. On is what puts a skill's SKILL.md in an agent's prompt
 * and what lets an MCP server's tool be called; a tool that is off is listed and
 * refused at call time. Any member of the org may switch one — the handler asks
 * for the org and nothing more.
 */
import { call, query, type Target } from './call.ts'

export interface Tool {
  /** The flat, fleet-wide name: `skill_<name>`, `<server id>_<tool>`, `agent_<name>`… */
  name: string
  /** connector, function, zap-service, agent, skill or mcp. */
  source: string
  description: string
  activated: boolean
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
export const names = (v: unknown): string[] => (Array.isArray(v) ? v.filter((n): n is string => typeof n === 'string' && n !== '') : [])

export function tool(raw: unknown): Tool {
  const o = obj(raw)
  return { name: str(o.name), source: str(o.source), description: str(o.description), activated: o.activated === true }
}

/** The tools in one listing, whichever of the plane's two envelopes it came in. */
export function toolsOf(raw: unknown): Tool[] {
  const list = obj(raw).tools
  return (Array.isArray(list) ? list : []).map(tool).filter((x) => x.name)
}

/** Every tool the org reaches, narrowed to one source and to the ones that are on when asked. */
export async function tools(t: Target, q: { source?: string; activated?: boolean } = {}): Promise<Tool[]> {
  return toolsOf(await call<unknown>(t, 'GET', `/v1/tool${query({ source: q.source, activated: q.activated ? 'true' : undefined })}`))
}

/** The names that are on. */
export async function enabled(t: Target): Promise<string[]> {
  return names(obj(await call<unknown>(t, 'GET', '/v1/tool/activation')).enabled)
}

/** Switch names on and off; answers every name that is on afterwards. */
export async function toggle(t: Target, on: string[], off: string[] = []): Promise<string[]> {
  return names(obj(await call<unknown>(t, 'PUT', '/v1/tool/activation', { activate: on, deactivate: off })).enabled)
}
