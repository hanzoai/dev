/**
 * The Library's and Scheduled's wire: the addresses and bodies sent, what is
 * made of each answer, and the words and groupings the two screens draw.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import type { Target } from './call.ts'
import * as auto from './auto.ts'
import * as library from './library.ts'
import { about, dollars, lasted, merged, titled } from '../activity.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'acme' }

interface Seen {
  url: string
  method: string
  headers: Headers
  body: unknown
}

function answer(status: number, body: unknown) {
  const seen: Seen[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      seen.push({ url, method: init.method ?? 'GET', headers: new Headers(init.headers), body: init.body ? JSON.parse(String(init.body)) : undefined })
      return new Response(body === undefined ? null : JSON.stringify(body), { status, headers: { 'Content-Type': 'application/json' } })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

const ITEM = {
  id: `lib_${'a'.repeat(32)}`,
  kind: 'image',
  source: 'chat',
  name: 'poster.png',
  mime: 'image/png',
  bytes: 2048,
  width: 1024,
  height: 1024,
  seconds: 0,
  pages: 0,
  owner: 'u1',
  mine: true,
  shared: false,
  project: '',
  ref: 'thread_1',
  origin: '/chat?thread=thread_1',
  url: 'https://s3.hanzo.ai/hanzo-library/x?sig=1',
  download: 'https://s3.hanzo.ai/hanzo-library/x?sig=2',
  preview: '',
  expires: '2026-10-16T12:00:00Z',
  created: '2026-10-16T10:00:00Z',
  updated: '2026-10-16T10:00:00Z',
}

describe('library', () => {
  it('lists with every filter it was given and none it was not, and reads the next cursor', async () => {
    const seen = answer(200, { data: [ITEM, { name: 'no id' }], next: 'c2' })
    const page = await library.items(T, { kind: 'image', source: 'chat', q: ' poster ', scope: 'mine', limit: 60, cursor: 'c1' })
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/library?kind=image&source=chat&q=poster&scope=mine&limit=60&cursor=c1')
    expect(seen[0].headers.get('authorization')).toBe('Bearer tok')
    expect(seen[0].headers.get('x-org-id')).toBe('acme')
    expect(page).toEqual({ data: [ITEM], next: 'c2' })
    answer(200, { data: [] })
    const all = await library.items(T, { scope: 'all' })
    expect(all).toEqual({ data: [], next: '' })
  })

  it('reads an item defensively: unknown kinds are files, foreign origins and unsigned URLs are dropped', () => {
    const x = library.item({ ...ITEM, kind: 'hologram', source: 'mars', origin: '//evil.example/x', url: 'javascript:alert(1)', preview: 'http://plain', bytes: -4 })!
    expect(x.kind).toBe('file')
    expect(x.source).toBe('dev')
    expect(x.origin).toBe('')
    expect(x.url).toBe('')
    expect(x.preview).toBe('')
    expect(x.bytes).toBe(0)
    expect(library.item({ name: 'no id' })).toBeNull()
    expect(library.inApp('/\\evil')).toBe('')
    expect(library.inApp('/dev/sess_1')).toBe('/dev/sess_1')
  })

  it('uploads the bytes as base64 with what made them, and refuses empty and oversized files before sending', async () => {
    const seen = answer(200, ITEM)
    const bytes = new TextEncoder().encode('hello')
    const x = await library.upload(T, { name: 'note.txt', type: 'text/plain', bytes }, { source: 'chat', ref: 'thread_1', origin: '/chat?thread=thread_1', key: 'thread_1/m1/0' })
    expect(x.id).toBe(ITEM.id)
    expect(seen[0]).toMatchObject({
      method: 'POST',
      url: 'https://api.hanzo.ai/v1/library',
      body: { name: 'note.txt', mime: 'text/plain', data: 'aGVsbG8=', source: 'chat', ref: 'thread_1', origin: '/chat?thread=thread_1', key: 'thread_1/m1/0' },
    })
    await expect(library.upload(T, { name: 'empty', type: '', bytes: new Uint8Array() }, { source: 'chat' })).rejects.toThrow('empty')
    await expect(library.upload(T, { name: 'big', type: '', blob: { size: library.MAX + 1 } as Blob }, { source: 'chat' })).rejects.toThrow('larger than 2 GB')
  })

  it('renames, shares and moves by PATCH, deletes by DELETE, and escapes the id', async () => {
    const seen = answer(200, { ...ITEM, shared: true })
    expect((await library.change(T, 'lib 1', { shared: true })).shared).toBe(true)
    expect(seen[0]).toMatchObject({ method: 'PATCH', url: 'https://api.hanzo.ai/v1/library/lib%201', body: { shared: true } })
    const gone = answer(204, undefined)
    await library.remove(T, ITEM.id)
    expect(gone[0]).toMatchObject({ method: 'DELETE', url: `https://api.hanzo.ai/v1/library/${ITEM.id}` })
    answer(200, {})
    await expect(library.get(T, ITEM.id)).rejects.toThrow('without an id')
  })

  it('encodes large byte arrays without overflowing the stack', () => {
    const big = new Uint8Array(200_000).fill(65)
    expect(library.base64(big)).toBe(Buffer.from(big).toString('base64'))
  })

  it('groups items under Today, Yesterday, then the month, with the year only when it is not this one', () => {
    const now = new Date(2026, 9, 16, 15)
    const at = (y: number, m: number, d: number) => ({ ...library.item(ITEM)!, id: `lib_${y}${m}${d}`, created: new Date(y, m - 1, d, 9).toISOString() })
    const groups = library.days([at(2026, 10, 16), at(2026, 10, 16), at(2026, 10, 15), at(2026, 10, 2), at(2026, 9, 30), at(2025, 12, 1)], now)
    expect(groups.map((g) => [g.label, g.rows.length])).toEqual([
      ['Today', 2],
      ['Yesterday', 1],
      [new Date(2026, 9, 2).toLocaleDateString(undefined, { month: 'long' }), 1],
      [new Date(2026, 8, 30).toLocaleDateString(undefined, { month: 'long' }), 1],
      [new Date(2025, 11, 1).toLocaleDateString(undefined, { month: 'long', year: 'numeric' }), 1],
    ])
    expect(library.day('not a date', now)).toBe('Earlier')
  })

  it('says a size the way a person reads it', () => {
    expect(library.size(12)).toBe('12 B')
    expect(library.size(2048)).toBe('2.0 KB')
    expect(library.size(5 * 1024 * 1024)).toBe('5.0 MB')
    expect(library.size(300 * 1024 * 1024)).toBe('300 MB')
  })
})

const JOB = {
  id: 'run_1',
  product: 'chat',
  kind: 'automation',
  title: 'Monday summary',
  state: 'running',
  status: 'running',
  progress: 0.4,
  automation: `flow_${'1'.repeat(32)}`,
  session: `sess_${'b'.repeat(32)}`,
  ref: 'run_1',
  origin: '/dev/-/automations/flow_1',
  summary: '',
  error: '',
  outputs: ['lib_1'],
  cost: 12,
  started: '2026-10-16T10:00:00Z',
  finished: null,
  updated: '2026-10-16T10:00:05Z',
}

describe('scheduled', () => {
  it("reads a job, its state from the five words or from the product's own", () => {
    expect(auto.job(JOB)).toMatchObject({ id: 'run_1', product: 'chat', state: 'running', progress: 0.4, outputs: ['lib_1'], cost: 12, session: JOB.session })
    const words: [string, auto.State][] = [
      ['queued', 'scheduled'],
      ['review', 'running'],
      ['succeeded', 'succeeded'],
      ['refused', 'failed'],
      ['skipped', 'cancelled'],
      ['something new', 'scheduled'],
    ]
    for (const [status, state] of words) expect(auto.job({ id: 'j', status })?.state).toBe(state)
    const odd = auto.job({ id: 'j', product: 'pluto', progress: 7, cost: -1, session: 'nope', origin: 'https://evil.example' })!
    expect(odd).toMatchObject({ product: 'dev', progress: 1, cost: null, session: null, origin: '' })
    expect(auto.job({})).toBeNull()
  })

  it('lists jobs with its filters, reads one, and turns words into a schedule', async () => {
    const seen = answer(200, { data: [JOB] })
    expect(await auto.jobs(T, { product: 'studio', state: 'running', limit: 50 })).toHaveLength(1)
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/auto/jobs?product=studio&state=running&limit=50')
    const one = answer(200, JOB)
    expect((await auto.one(T, 'run_1')).title).toBe('Monday summary')
    expect(one[0].url).toBe('https://api.hanzo.ai/v1/auto/jobs/run_1')
    const said = answer(200, { schedule: { kind: 'cron', cron: '*/5 * * * *', tz: 'UTC' }, says: 'Every 5 minutes', next: '2026-10-16T10:05:00Z' })
    expect(await auto.cron(T, 'every 5 minutes', 'UTC')).toEqual({ schedule: { kind: 'cron', cron: '*/5 * * * *', tz: 'UTC' }, says: 'Every 5 minutes', next: '2026-10-16T10:05:00Z' })
    expect(said[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/auto/cron', body: { text: 'every 5 minutes', tz: 'UTC' } })
  })

  it('reads a stream frame as a job or an automation, and nothing else', () => {
    expect(auto.heard('job', JSON.stringify(JOB))).toMatchObject({ kind: 'job', job: { id: 'run_1' } })
    expect(auto.heard('automation', JSON.stringify({ id: 'flow_1', name: 'x', source: 'studio' }))).toMatchObject({ kind: 'automation', automation: { id: 'flow_1', source: 'studio' } })
    expect(auto.heard('job', 'not json')).toBeNull()
    expect(auto.heard('ping', '{}')).toBeNull()
  })

  it('reads the events stream with the bearer and calls back for each frame', async () => {
    const body = new ReadableStream<Uint8Array>({
      start(c) {
        c.enqueue(new TextEncoder().encode(`: hi\n\nevent: job\ndata: ${JSON.stringify(JOB)}\n\n`))
        c.close()
      },
    })
    const fetched = vi.fn(async () => new Response(body, { status: 200 }))
    vi.stubGlobal('fetch', fetched)
    const heard: auto.Heard[] = []
    await auto.events(T, (h) => heard.push(h), new AbortController().signal)
    expect(heard).toHaveLength(1)
    const init = (fetched.mock.calls[0] as unknown as [string, RequestInit])[1]
    expect(new Headers(init.headers).get('authorization')).toBe('Bearer tok')
    vi.stubGlobal(
      'fetch',
      vi.fn(async () => new Response(JSON.stringify({ detail: 'no stream here' }), { status: 404 })),
    )
    await expect(auto.events(T, () => {}, new AbortController().signal)).rejects.toThrow('no stream here')
  })

  it('says a once schedule, and carries source and outputs', () => {
    expect(auto.words({ kind: 'once' })).toBe('Once, now')
    expect(auto.words({ kind: 'cron', cron: '*/5 * * * *', tz: 'UTC' })).toBe('Every 5 minutes · UTC')
    expect(auto.words({ kind: 'cron', cron: '0 */2 * * *' })).toBe('Every 2 hours · UTC')
    expect(auto.words({ kind: 'cron', cron: '0 9 1 * *' })).toBe('Cron 0 9 1 * * · UTC')
    expect(auto.words({ kind: 'once', at: '2026-10-20T09:00', tz: 'UTC' })).toMatch(/^Once, .+ at 09:00 · UTC$/)
    expect(auto.read({ id: 'flow_1', schedule: { kind: 'once', at: '2026-10-20T09:00' } })?.schedule).toEqual({ kind: 'once', at: '2026-10-20T09:00' })
    expect(auto.read({ id: 'flow_1', source: 'workflow' })?.source).toBe('workflow')
    expect(auto.read({ id: 'flow_1', source: 'nowhere' })?.source).toBe('dev')
  })

  it("draws a run's line: from where, when, how long, its cost and what it made", () => {
    const j = auto.job(JOB)!
    const at = Date.parse('2026-10-16T10:01:30Z')
    expect(lasted(j.started, j.finished, at)).toBe('1m 30s')
    expect(lasted(j.started, '2026-10-16T12:05:00Z')).toBe('2h 5m')
    expect(lasted('', null)).toBe('')
    expect(dollars(12)).toBe('$0.12')
    expect(about(j, at)).toContain('Chat')
    expect(about(j, at)).toContain('$0.12')
    expect(about(j, at)).toContain('1 output')
  })

  it('keeps the activity newest first with each job once', () => {
    const a = auto.job({ ...JOB, id: 'a', started: '2026-10-16T09:00:00Z' })!
    const b = auto.job({ ...JOB, id: 'b', started: '2026-10-16T11:00:00Z' })!
    const again = auto.job({ ...JOB, id: 'a', started: '2026-10-16T09:00:00Z', state: 'succeeded' })!
    expect(merged(merged([a], b), again).map((j) => [j.id, j.state])).toEqual([
      ['b', 'running'],
      ['a', 'succeeded'],
    ])
  })

  it('names a task from the first line of what it does', () => {
    expect(titled('Summarize the week\nand more')).toBe('Summarize the week')
    expect(titled('x'.repeat(80))).toHaveLength(58)
  })
})
