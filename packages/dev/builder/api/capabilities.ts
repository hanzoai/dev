/**
 * What the agent may use: every tool the org can reach, and which of them are
 * switched on.
 *
 *   GET /v1/tool → {tools: [{name, source, description, dispatchable, activated}]}
 *   PUT /v1/tool/activation  {activate: [], deactivate: []} → {enabled: []}
 *
 * Activation is per org (and project): a tool that is off is listed but
 * refused when an agent calls it. At most 256 names go in one request, so a
 * larger change is sent in turns.
 */
import { phrase } from '@hanzo/i18n'

import { call, type Target } from './call.ts'

/** Where a tool comes from, in the order the platform ranks them. */
export const SOURCES = ['connector', 'function', 'zap-service', 'agent', 'skill', 'mcp'] as const
export type Source = (typeof SOURCES)[number]

export interface Tool {
  name: string
  source: Source
  description: string
  activated: boolean
}

const BATCH = 256
const str = (v: unknown): string => (typeof v === 'string' ? v : '')

export function toolOf(raw: unknown): Tool | null {
  const o = (raw && typeof raw === 'object' ? raw : {}) as Record<string, unknown>
  const name = str(o.name)
  const source = str(o.source) as Source
  if (!name || !SOURCES.includes(source)) return null
  return { name, source, description: str(o.description), activated: o.activated === true }
}

export async function tools(t: Target): Promise<Tool[]> {
  const r = await call<{ tools?: unknown }>(t, 'GET', '/v1/tool')
  return (Array.isArray(r?.tools) ? r.tools : []).map(toolOf).filter((x): x is Tool => x !== null)
}

/** Switch every tool in `names` on (or off), and answer the org's activated set. */
export async function activate(t: Target, names: string[], on: boolean): Promise<string[]> {
  if (!names.length) throw new Error(phrase('There is nothing to switch'))
  let enabled: string[] = []
  for (let i = 0; i < names.length; i += BATCH) {
    const part = names.slice(i, i + BATCH)
    const r = await call<{ enabled?: unknown }>(t, 'PUT', '/v1/tool/activation', on ? { activate: part, deactivate: [] } : { activate: [], deactivate: part })
    enabled = Array.isArray(r?.enabled) ? r.enabled.filter((n): n is string => typeof n === 'string') : []
  }
  return enabled
}
