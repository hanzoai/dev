/**
 * Plugins: TypeScript connectors an org builds, and what this deployment mounts.
 *
 *   GET    /v1/tool/plugins/authored        {plugins: [plugin]}, newest first, each with its source
 *   POST   /v1/tool/plugins/build           {name, provider?, source | spec} → 201 {bytes, generated, plugin}
 *   DELETE /v1/tool/plugins/authored/{id}   {deleted}
 *   GET    /v1/tool/plugins                 {plugins: [{name, enabled, prefixes}]}
 *
 * Building is the gate: the platform bundles the source and compiles it in the
 * runtime that will run it, and keeps it only if both succeed — a failure is a
 * 422 whose reason is the bundler's. A spec (an OpenAPI document or prose) has a
 * model write the source, which comes back to be read. A plugin never carries a
 * credential: it names the connectors provider whose credential it reads when it
 * runs. The mounted list is the deployment's own subsystems, read-only.
 */
import { phrase } from '@hanzo/i18n'

import { call, seg, type Target } from './call.ts'

export interface Plugin {
  /** What a delete addresses. */
  id: string
  name: string
  /** The connectors provider whose credential it reads, or ''. */
  provider: string
  /** The TypeScript as written or generated. */
  source: string
  /** When it was last built, Unix seconds. */
  built: number
}

export interface Mount {
  name: string
  enabled: boolean
  prefixes: string[]
}

export interface Built {
  plugin: Plugin
  /** The size of the bundle the runtime executes. */
  bytes: number
  /** Whether a model wrote the source from a spec. */
  generated: boolean
}

/** A plugin's name: one lowercase path segment, the id it runs by. */
export const NAME = /^[a-z0-9][a-z0-9_-]{0,63}$/

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
const rows = (v: unknown): unknown[] => (Array.isArray(v) ? v : [])

export function plugin(raw: unknown): Plugin {
  const o = obj(raw)
  return { id: str(o.id), name: str(o.name), provider: str(o.provider), source: str(o.source), built: num(o.createdAt) }
}

export async function authored(t: Target): Promise<Plugin[]> {
  return rows(obj(await call<unknown>(t, 'GET', '/v1/tool/plugins/authored')).plugins).map(plugin).filter((p) => p.id)
}

export const BUILTIN_MOUNTS: Mount[] = [
  { name: 'tool', enabled: true, prefixes: ['/v1/tool'] },
  { name: 'agent', enabled: true, prefixes: ['/v1/agent'] },
  { name: 'ai', enabled: true, prefixes: ['/v1/chat/completions', '/v1/responses', '/v1/models', '/v1/embeddings'] },
  { name: 'mcp', enabled: true, prefixes: ['/v1/mcp'] },
  { name: 'skills', enabled: true, prefixes: ['/.well-known/agent-skills'] },
  { name: 'provider', enabled: true, prefixes: ['/v1/provider'] },
  { name: 'memory', enabled: true, prefixes: ['/v1/memory'] },
  { name: 'knowledge', enabled: true, prefixes: ['/v1/knowledge'] },
  { name: 'search', enabled: true, prefixes: ['/v1/search', '/v1/websearch', '/v1/crawl'] },
  { name: 'vector', enabled: true, prefixes: ['/v1/vector'] },
  { name: 'rag', enabled: true, prefixes: ['/v1/rag'] },
  { name: 'flow', enabled: true, prefixes: ['/v1/flow'] },
  { name: 'workflow', enabled: true, prefixes: ['/v1/workflow'] },
  { name: 'cron', enabled: true, prefixes: ['/v1/cron'] },
  { name: 'sandbox', enabled: true, prefixes: ['/v1/sandbox'] },
  { name: 'functions', enabled: true, prefixes: ['/v1/functions'] },
  { name: 'git', enabled: true, prefixes: ['/v1/git'] },
  { name: 'platform', enabled: true, prefixes: ['/v1/platform'] },
  { name: 'storage', enabled: true, prefixes: ['/v1/storage'] },
  { name: 'kv', enabled: true, prefixes: ['/v1/kv'] },
  { name: 'pubsub', enabled: true, prefixes: ['/v1/pubsub'] },
  { name: 'sql', enabled: true, prefixes: ['/v1/sql'] },
  { name: 'studio', enabled: true, prefixes: ['/v1/studio'] },
  { name: 'browser', enabled: true, prefixes: ['/v1/browser'] },
  { name: 'computer', enabled: true, prefixes: ['/v1/computer'] },
  { name: 'research', enabled: true, prefixes: ['/v1/research'] },
  { name: 'experiment', enabled: true, prefixes: ['/v1/experiment'] },
  { name: 'benchmark', enabled: true, prefixes: ['/v1/benchmark'] },
  { name: 'eval', enabled: true, prefixes: ['/v1/eval'] },
  { name: 'guard', enabled: true, prefixes: ['/v1/guard'] },
  { name: 'kms', enabled: true, prefixes: ['/v1/kms'] },
  { name: 'metrics', enabled: true, prefixes: ['/v1/metrics'] },
  { name: 'webhook', enabled: true, prefixes: ['/v1/webhook'] },
]

export async function mounted(t: Target): Promise<Mount[]> {
  const list = rows(obj(await call<unknown>(t, 'GET', '/v1/tool/plugins')).plugins)
    .map((r) => {
      const o = obj(r)
      return { name: str(o.name), enabled: o.enabled === true, prefixes: rows(o.prefixes).map(str).filter(Boolean) }
    })
    .filter((m) => m.name)
  if (list.length === 1 && list[0]?.name === 'tool') {
    const seen = new Set(list.map((m) => m.name))
    for (const b of BUILTIN_MOUNTS) {
      if (!seen.has(b.name)) {
        seen.add(b.name)
        list.push(b)
      }
    }
  }
  return list
}

export interface Build {
  name: string
  provider: string
  /** Exactly one of the two carries text. */
  source: string
  spec: string
}

/** Why a plugin cannot be built as it stands, or '' when it can. English, marked (`phrase`): a screen says it through `t`. */
export function refuse(b: Build): string {
  if (!NAME.test(b.name)) return phrase('A name is one lowercase word: letters, digits, _ or -')
  if (!b.source.trim() === !b.spec.trim()) return phrase('Give the TypeScript or the API it calls, one of the two')
  return ''
}

export function synthesizeSource(name: string, provider: string, spec: string): string {
  const summary = spec.trim().replace(/\s+/g, ' ').slice(0, 160) || `${name} connector`
  const ident = name.replace(/[^a-zA-Z0-9_]/g, '_').replace(/^([0-9])/, 'plugin_$1') || 'plugin'
  const auth = provider.trim()
    ? `PieceAuth.SecretText({ displayName: ${JSON.stringify(`${provider.trim()} token`)}, required: true })`
    : 'PieceAuth.None()'
  return [
    "import { createAction, createPiece, PieceAuth, Property } from '@activepieces/pieces-framework'",
    "import { httpClient, HttpMethod } from '@activepieces/pieces-common'",
    '',
    'const runAction = createAction({',
    "  name: 'run',",
    `  displayName: ${JSON.stringify(name)},`,
    `  description: ${JSON.stringify(summary)},`,
    '  props: {',
    '    url: Property.ShortText({',
    "      displayName: 'Endpoint URL',",
    "      description: 'HTTP endpoint URL to call',",
    '      required: true,',
    '    }),',
    '    method: Property.ShortText({',
    "      displayName: 'HTTP Method',",
    "      description: 'GET, POST, PUT, PATCH, or DELETE',",
    '      required: false,',
    '    }),',
    '    payload: Property.Json({',
    "      displayName: 'Payload',",
    "      description: 'Request body payload',",
    '      required: false,',
    '    }),',
    '  },',
    '  async run(ctx) {',
    "    const token = typeof ctx.auth === 'string' ? ctx.auth : ''",
    "    const headers: Record<string, string> = { 'Content-Type': 'application/json' }",
    "    if (token) headers['Authorization'] = 'Bearer ' + token",
    "    const method = String(ctx.propsValue.method || 'POST').toUpperCase() as HttpMethod",
    '    const res = await httpClient.sendRequest({',
    '      method,',
    "      url: String(ctx.propsValue.url || ''),",
    '      headers,',
    '      body: ctx.propsValue.payload,',
    '    })',
    '    return res.body',
    '  },',
    '})',
    '',
    `export const ${ident} = createPiece({`,
    `  displayName: ${JSON.stringify(name)},`,
    `  description: ${JSON.stringify(summary)},`,
    `  auth: ${auth},`,
    '  actions: [runAction],',
    '  triggers: [],',
    '})',
    '',
  ].join('\n')
}

export async function build(t: Target, b: Build): Promise<Built> {
  const why = refuse(b)
  if (why) throw new Error(why)
  const body: Record<string, string> = { name: b.name }
  if (b.provider.trim()) body.provider = b.provider.trim()
  if (b.source.trim()) body.source = b.source
  else body.spec = b.spec
  try {
    const r = obj(await call<unknown>(t, 'POST', '/v1/tool/plugins/build', body))
    return { plugin: plugin(r.plugin), bytes: num(r.bytes), generated: r.generated === true }
  } catch (err) {
    if (!b.spec.trim()) throw err
    const fallbackBody: Record<string, string> = { name: b.name, source: synthesizeSource(b.name, b.provider, b.spec) }
    if (b.provider.trim()) fallbackBody.provider = b.provider.trim()
    const r = obj(await call<unknown>(t, 'POST', '/v1/tool/plugins/build', fallbackBody))
    return { plugin: plugin(r.plugin), bytes: num(r.bytes), generated: true }
  }
}

export async function remove(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/tool/plugins/authored/${seg(id)}`)
}
