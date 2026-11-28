/**
 * The contract behind Customize: exact addresses, headers and bodies for the
 * skills, connectors, plugins and agents clients and the tool plane they share,
 * and what each makes of the answer. `fetch` is replaced per test with a
 * recorder that answers what the test says the platform answers.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import * as agents from './agents.ts'
import type { Target } from './call.ts'
import * as connectors from './connectors.ts'
import * as plugins from './plugins.ts'
import * as skills from './skills.ts'
import * as tools from './tools.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'acme' }

interface Seen {
  url: string
  method: string
  headers: Headers
  body: unknown
}

/** Every call answers `body` with `status`; a string body with a non-JSON type is sent as it is. */
function answer(status: number, body: unknown, type = 'application/json') {
  const seen: Seen[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      seen.push({
        url,
        method: init.method ?? 'GET',
        headers: new Headers(init.headers),
        body: init.body ? JSON.parse(String(init.body)) : undefined,
      })
      const text = body === undefined ? null : typeof body === 'string' && type !== 'application/json' ? body : JSON.stringify(body)
      return new Response(text, { status, headers: { 'content-type': type } })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

describe('the tool plane', () => {
  it('lists one source, the ones that are on when asked', async () => {
    const seen = answer(200, { tools: [{ name: 'gh_search', source: 'mcp', description: 'Search', activated: true }, { source: 'mcp' }] })
    expect(await tools.tools(T, { source: 'mcp', activated: true })).toEqual([{ name: 'gh_search', source: 'mcp', description: 'Search', activated: true }])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool?source=mcp&activated=true')
    expect(seen[0].headers.get('x-org-id')).toBe('acme')
  })

  it('reads and switches what is on', async () => {
    let seen = answer(200, { enabled: ['skill_triage', 7] })
    expect(await tools.enabled(T)).toEqual(['skill_triage'])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/activation')
    seen = answer(200, { enabled: ['gh_search'] })
    expect(await tools.toggle(T, ['gh_search'], ['skill_triage'])).toEqual(['gh_search'])
    expect(seen[0]).toMatchObject({ method: 'PUT', url: 'https://api.hanzo.ai/v1/tool/activation', body: { activate: ['gh_search'], deactivate: ['skill_triage'] } })
  })
})

describe('skills', () => {
  it('reads the skills that are on, and the org’s own with their SKILL.md', async () => {
    let seen = answer(200, { source: 'skill', tools: [{ name: 'skill_git_repos', source: 'skill', activated: true }] })
    expect((await skills.active(T)).map((x) => x.name)).toEqual(['skill_git_repos'])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/skills?activated=true')
    seen = answer(200, {
      skills: [
        { id: 'triage', name: 'triage', description: 'How we triage', content: '# Triage', createdAt: 1700000000, admitted: true },
        { id: 'old', name: 'old', content: '# Old', admitted: false },
      ],
    })
    expect(await skills.authored(T)).toEqual([
      { id: 'triage', name: 'triage', description: 'How we triage', content: '# Triage', created: 1700000000, source: '', admitted: true },
      { id: 'old', name: 'old', description: '', content: '# Old', created: 0, source: '', admitted: false },
    ])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/skills/authored')
  })

  it('writes one by name and removes it by id', async () => {
    let seen = answer(201, { skill: { id: 'triage', name: 'triage', description: 'd', content: '# T' } })
    expect((await skills.write(T, { name: 'triage', description: 'd', content: '# T' })).id).toBe('triage')
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/tool/skills', body: { name: 'triage', description: 'd', content: '# T' } })
    seen = answer(200, { deleted: 'a b' })
    await skills.remove(T, 'a b')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/tool/skills/a%20b' })
  })

  it('refuses what the handler would', async () => {
    const seen = answer(201, {})
    await expect(skills.write(T, { name: 'Triage Notes', description: '', content: '# T' })).rejects.toThrow(/one lowercase word/)
    await expect(skills.write(T, { name: 'triage', description: '', content: '  ' })).rejects.toThrow(/SKILL\.md/)
    expect(seen).toEqual([])
    expect(skills.tool('triage')).toBe('skill_triage')
    expect(skills.nameOf('skill_triage')).toBe('triage')
    expect(skills.nameOf('gh_search')).toBe('')
  })

  it('reads the brand catalogue and a SKILL.md without a bearer', async () => {
    let seen = answer(200, {
      skills: [{ name: 'git_repos', description: 'List repositories', service: 'git', works: ['engineering'] }, { description: 'nameless' }],
      products: [{ name: 'git', path: '_git/index.json', skill_count: 4 }],
    })
    expect(await skills.brand(T)).toEqual({
      skills: [{ name: 'git_repos', description: 'List repositories', product: 'git', works: ['engineering'] }],
      products: [{ name: 'git', count: 4 }],
    })
    expect(seen[0].url).toBe('https://api.hanzo.ai/.well-known/agent-skills/index.json')
    expect(seen[0].headers.has('authorization')).toBe(false)
    seen = answer(200, '# Git repositories\n', 'text/markdown')
    expect(await skills.document(T, 'git_repos')).toBe('# Git repositories\n')
    expect(seen[0].url).toBe('https://api.hanzo.ai/.well-known/agent-skills/git_repos/SKILL.md')
    answer(404, { error: 'not found' })
    await expect(skills.document(T, 'nope')).rejects.toMatchObject({ status: 404, message: 'not found' })
  })
})

describe('connectors', () => {
  it('reads the org’s servers; a secret is only ever a flag', async () => {
    const seen = answer(200, {
      servers: [{ id: 'stripe-com', org: 'acme', name: 'Stripe', url: 'https://mcp.stripe.com', authHeader: 'Authorization', hasSecret: true, listing: 'com.stripe_mcp', source: 'catalog', createdAt: 1 }],
    })
    expect(await connectors.servers(T)).toEqual([
      { id: 'stripe-com', name: 'Stripe', url: 'https://mcp.stripe.com', header: 'Authorization', secret: true, listing: 'com.stripe_mcp', created: 1, admitted: true },
    ])
    answer(200, { servers: [{ id: 'old', name: 'Old', url: 'https://old.example/mcp', admitted: false }] })
    expect((await connectors.servers(T))[0]?.admitted).toBe(false)
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/mcp/servers')
  })

  it('adds a URL, or a listing by its id alone', async () => {
    let seen = answer(201, { id: 'docs', name: 'Docs', url: 'https://docs.example/mcp' })
    await connectors.add(T, { name: ' Docs ', url: 'https://docs.example/mcp', header: 'Authorization', secret: 'Bearer x' })
    expect(seen[0]).toMatchObject({
      method: 'POST',
      url: 'https://api.hanzo.ai/v1/tool/mcp/servers',
      body: { name: 'Docs', url: 'https://docs.example/mcp', authHeader: 'Authorization', secret: 'Bearer x' },
    })
    seen = answer(201, { id: 'stripe-com' })
    await connectors.add(T, { listing: 'com.stripe_mcp', header: 'Authorization', secret: '' })
    expect(seen[0].body).toEqual({ listing: 'com.stripe_mcp' })
    await expect(connectors.add(T, { name: 'x', url: 'ftp://x' })).rejects.toThrow(/http\(s\)/)
    await expect(connectors.add(T, { name: 'x', url: 'https://x', secret: 's' })).rejects.toThrow(/header/)
  })

  it('removes one by id', async () => {
    const seen = answer(204, undefined)
    await connectors.remove(T, 'stripe-com')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/tool/mcp/servers/stripe-com' })
  })

  it('pages the shelf and reads one listing', async () => {
    let seen = answer(200, {
      catalog: [
        { id: 'com.stripe_mcp', name: 'com.stripe/mcp', title: 'Stripe', featured: true, transports: ['streamable-http'], remotes: [{ transport: 'streamable-http', url: 'https://mcp.stripe.com' }] },
        { id: 'io.x_pkg', name: 'io.x/pkg', transports: ['stdio'], packages: [{ registry: 'npm', identifier: '@x/mcp', runtime: 'npx' }] },
      ],
      total: 1200,
      limit: 48,
      offset: 48,
    })
    const page = await connectors.shelf(T, { text: ' stripe ', limit: 48, offset: 48 })
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/catalog?q=stripe&limit=48&offset=48')
    expect(page.total).toBe(1200)
    expect(page.offset).toBe(48)
    expect(page.listings.map(connectors.ready)).toEqual([true, false])
    expect(connectors.titleOf(page.listings[1])).toBe('io.x/pkg')
    expect(page.listings[1].packages).toEqual([{ registry: 'npm', identifier: '@x/mcp', runtime: 'npx', version: '' }])
    seen = answer(200, { id: 'com.stripe_mcp', name: 'com.stripe/mcp', repo: 'https://github.com/stripe/mcp' })
    expect((await connectors.one(T, 'com.stripe_mcp')).repo).toBe('https://github.com/stripe/mcp')
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/catalog/com.stripe_mcp')
  })

  it('knows a server’s tools by their prefix', () => {
    const s = connectors.server({ id: 'gh' })
    expect(connectors.owns(s, 'gh_search')).toBe(true)
    expect(connectors.owns(s, 'ghx_search')).toBe(false)
  })
})

describe('plugins', () => {
  it('reads what the org built and what the deployment mounts', async () => {
    let seen = answer(200, { plugins: [{ id: 'p1a2b3c', name: 'acme', provider: 'acme', source: 'export const x = 1', createdAt: 9 }] })
    expect(await plugins.authored(T)).toEqual([{ id: 'p1a2b3c', name: 'acme', provider: 'acme', source: 'export const x = 1', built: 9 }])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/plugins/authored')
    seen = answer(200, { plugins: [{ name: 'tools', enabled: true, prefixes: ['/v1/tool'] }] })
    expect(await plugins.mounted(T)).toEqual([{ name: 'tools', enabled: true, prefixes: ['/v1/tool'] }])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/plugins')
    seen = answer(200, { plugins: [{ name: 'tool', enabled: true, prefixes: ['/v1/tool'] }] })
    const full = await plugins.mounted(T)
    expect(full.length).toBeGreaterThanOrEqual(plugins.BUILTIN_MOUNTS.length)
    expect(full.map((m) => m.name)).toContain('agent')
  })

  it('builds from source or from a spec, never both', async () => {
    let seen = answer(201, { bytes: 2048, generated: false, plugin: { id: 'p1', name: 'acme' } })
    const out = await plugins.build(T, { name: 'acme', provider: '', source: 'export {}', spec: '' })
    expect(out).toMatchObject({ bytes: 2048, generated: false, plugin: { id: 'p1', name: 'acme' } })
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/tool/plugins/build', body: { name: 'acme', source: 'export {}' } })
    seen = answer(201, { bytes: 10, generated: true, plugin: { id: 'p2', name: 'acme' } })
    await plugins.build(T, { name: 'acme', provider: 'acme', source: '', spec: 'POST /v1/things creates a thing' })
    expect(seen[0].body).toEqual({ name: 'acme', provider: 'acme', spec: 'POST /v1/things creates a thing' })
    expect(plugins.synthesizeSource('bestpluginever', '', 'build a bot to post 26x a week')).toContain('export const bestpluginever = createPiece(')
    await expect(plugins.build(T, { name: 'acme', provider: '', source: 'a', spec: 'b' })).rejects.toThrow(/one of the two/)
    await expect(plugins.build(T, { name: 'Acme', provider: '', source: 'a', spec: '' })).rejects.toThrow(/one lowercase word/)
  })

  it('says why a build failed in the bundler’s words', async () => {
    answer(422, { status: 422, title: 'Unprocessable Entity', detail: 'bundle: Expected ";" but found "x"', source: 'x x' }, 'application/problem+json')
    await expect(plugins.build(T, { name: 'acme', provider: '', source: 'x x', spec: '' })).rejects.toMatchObject({ status: 422, message: 'bundle: Expected ";" but found "x"' })
  })

  it('removes one by id', async () => {
    const seen = answer(200, { deleted: 'p1' })
    await plugins.remove(T, 'p1')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/tool/plugins/authored/p1' })
  })
})

describe('agents', () => {
  const row = {
    id: 'agent_1',
    name: 'helper',
    model: 'zen5.8',
    description: 'Answers',
    tools: ['skill_triage'],
    status: 'ready',
    runs: 3,
    cap_micro_usd: 10_000_000,
    max_task_micro_usd: 1_000_000,
    consumed_micro_usd: 250_000,
    period: 'month',
  }

  it('reads the org’s agents and one with its instructions', async () => {
    let seen = answer(200, { agents: [row] })
    const [a] = await agents.agents(T)
    expect(a).toMatchObject({ id: 'agent_1', name: 'helper', runs: 3, cap: 10_000_000, task: 1_000_000, spent: 250_000, period: 'month', instructions: '' })
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/agent')
    seen = answer(200, { ...row, instructions: 'be terse' })
    expect((await agents.one(T, 'helper')).instructions).toBe('be terse')
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/agent/helper')
  })

  it('creates one with its budget in micro-USD, leaving the model to the platform when unset', async () => {
    const seen = answer(201, row)
    await agents.create(T, { ...agents.EMPTY, name: 'helper', instructions: 'be terse', tools: ['*'], cap: '10', task: '0.5', period: 'week' })
    expect(seen[0]).toMatchObject({
      method: 'POST',
      url: 'https://api.hanzo.ai/v1/agent',
      body: { name: 'helper', description: '', instructions: 'be terse', tools: ['*'], cap_micro_usd: 10_000_000, max_task_micro_usd: 500_000, period: 'week' },
    })
    expect(seen[0].body).not.toHaveProperty('model')
  })

  it('refuses a draft without a budget the handler would take', async () => {
    const seen = answer(201, row)
    await expect(agents.create(T, { ...agents.EMPTY, name: 'helper' })).rejects.toThrow(/spend each month/)
    await expect(agents.create(T, { ...agents.EMPTY, name: 'helper', cap: '1', task: '2' })).rejects.toThrow(/more than the whole month/)
    await expect(agents.create(T, { ...agents.EMPTY, name: '-x', cap: '1', task: '1' })).rejects.toThrow(/name/)
    expect(seen).toEqual([])
  })

  it('patches only what changed, and nothing when nothing did', async () => {
    const was = agents.agent({ ...row, instructions: 'be terse' })
    let seen = answer(200, row)
    await agents.update(T, was, { ...agents.draft(was), instructions: 'be terse and cite', cap: '20' })
    expect(seen[0]).toMatchObject({ method: 'PATCH', url: 'https://api.hanzo.ai/v1/agent/agent_1', body: { instructions: 'be terse and cite', cap_micro_usd: 20_000_000 } })
    expect(Object.keys(seen[0].body as object).sort()).toEqual(['cap_micro_usd', 'instructions'])
    seen = answer(200, row)
    expect(await agents.update(T, was, agents.draft(was))).toBe(was)
    expect(seen).toEqual([])
  })

  it('removes one by ref', async () => {
    const seen = answer(204, undefined)
    await agents.remove(T, 'agent_1')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/agent/agent_1' })
  })
})
