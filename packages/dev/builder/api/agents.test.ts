import { afterEach, describe, expect, it, vi } from 'vitest'

import { free, handle, link, person, run, use, type Cast } from './agents.ts'

const t = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'acme' }
const EINSTEIN: Cast = { name: 'Einstein', title: 'Einstein', description: 'Legal & IP Counsel', category: 'Legal', model: 'enso', instructions: 'You are Einstein.', avatar: '/agents/einstein.webp' }

afterEach(() => vi.unstubAllGlobals())

/** The API as Use meets it: the org's agents, and every POST recorded. */
function api(have: string[]) {
  const sent: Record<string, unknown>[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init?: RequestInit) => {
      if (init?.method === 'POST') {
        const body = JSON.parse(String(init.body))
        sent.push(body)
        return new Response(JSON.stringify({ ...body, id: 'agent_1' }), { status: 201 })
      }
      return new Response(JSON.stringify({ agents: have.map((name) => ({ name })) }), { status: 200 })
    }),
  )
  return sent
}

describe('a template', () => {
  it('is made the org’s own under its name, with its face linked, its model and instructions, and a budget', async () => {
    const sent = api([])
    const a = await use(t, EINSTEIN, 'https://hanzo.ai/dev/-/customize/agents')
    expect(a.name).toBe('Einstein')
    expect(a.avatar).toBe('https://hanzo.ai/agents/einstein.webp')
    expect(sent[0]).toEqual({
      name: 'Einstein',
      description: 'Legal & IP Counsel',
      model: 'enso',
      instructions: 'You are Einstein.',
      tools: [],
      avatar: 'https://hanzo.ai/agents/einstein.webp',
      cap_micro_usd: 10_000_000,
      max_task_micro_usd: 1_000_000,
      period: 'month',
    })
  })

  it('made twice is a second agent, never the first overwritten, and the template stays as it was', async () => {
    const before = { ...EINSTEIN }
    const sent = api(['einstein', 'Einstein-2'])
    expect((await use(t, EINSTEIN, 'https://hanzo.ai/')).name).toBe('Einstein-3')
    expect(sent).toHaveLength(1)
    expect(EINSTEIN).toEqual(before)
  })

  it('reads its instructions when they are a file', async () => {
    const sent = api([])
    await use(t, { ...EINSTEIN, name: 'Spärck Jones', read: async () => '# Karen' }, 'https://hanzo.ai/')
    expect(sent[0]).toMatchObject({ name: 'Sparck-Jones', instructions: '# Karen' })
  })

  it('links a face only over https', () => {
    expect(link('/agents/dev.png', 'https://hanzo.team/x')).toBe('https://hanzo.team/agents/dev.png')
    expect(link('/agents/dev.png', 'http://localhost:5173/')).toBe('')
    expect(link(undefined, 'https://hanzo.ai/')).toBe('')
  })

  it('takes a free name in any case, up to 64', () => {
    expect(free('dev', [{ name: 'DEV' }, { name: 'dev-2' }])).toBe('dev-3')
    expect(free('x'.repeat(64), [{ name: 'x'.repeat(64) }])).toHaveLength(64)
    expect(handle('  Ada Lovelace!')).toBe('Ada-Lovelace')
  })

  it('from a person is their name, who they are and their writing', () => {
    expect(person(' Zach ', 'Builds Hanzo.', 'Ship it.', 'https://x/z.png')).toEqual({
      name: 'Zach',
      title: 'Zach',
      description: 'Builds Hanzo.',
      category: 'Person',
      model: '',
      instructions: 'You are Zach.\n\nBuilds Hanzo.\n\nWriting by Zach:\n\nShip it.',
      avatar: 'https://x/z.png',
    })
    expect(person('Ada', '', '').instructions).toBe('You are Ada.')
  })

  it('reads a run as its profile lists it', () => {
    expect(run({ id: 'run_1', status: 'ok', model: 'enso', input: 'hi', output: 'hello', durationMs: 12, createdAt: '2026-10-20T00:00:00Z' })).toEqual({ id: 'run_1', status: 'ok', model: 'enso', input: 'hi', output: 'hello', error: '', ms: 12, at: '2026-10-20T00:00:00Z' })
  })

  it('elects a preset to run autonomously 24/7, proactively on a schedule, and event-driven', async () => {
    const sent = api([])
    const a = await use(t, EINSTEIN, 'https://hanzo.ai/dev/customize', {
      executionMode: 'long-running',
      schedule: '*/15 * * * *',
      triggers: ['webhook', 'git', 'channel', 'watch'],
    })
    expect(a.executionMode).toBe('long-running')
    expect(a.schedule).toBe('*/15 * * * *')
    expect(a.triggers).toEqual(['webhook', 'git', 'channel', 'watch'])
    expect(sent[0]).toMatchObject({
      name: 'Einstein',
      executionMode: 'long-running',
      schedule: '*/15 * * * *',
      triggers: ['webhook', 'git', 'channel', 'watch'],
    })
  })
})
