/**
 * Dev's conversation: what one thing said sends, the session every turn goes to,
 * and how its record reads — the person's words, the answers, the coding runs a
 * turn started, why a turn failed, and what it is doing now.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import type { Target } from './call.ts'
import { log, session } from './sessions.ts'
import { body, now, say, talks } from './talk.ts'
import { cards } from './turn.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'hanzo' }
const CONV = `sess_${'c'.repeat(32)}`

afterEach(() => vi.unstubAllGlobals())

describe('saying something', () => {
  it('sends only what was said and chosen, with no empty values', () => {
    expect(body({ input: '  hello  ' })).toEqual({ input: 'hello' })
    expect(body({ input: 'fix it', session: CONV, repo: ' site ', base: '', target: '', model: 'zen6', effort: '', issue: 0 })).toEqual({
      input: 'fix it',
      session: CONV,
      repo: 'site',
      model: 'zen6',
    })
    expect(body({ input: 'plan it', mode: 'manual', effort: 'high', issue: 7, target: 'tgt_1', developerInstructions: ' # Skills ' })).toEqual({
      input: 'plan it',
      mode: 'manual',
      effort: 'high',
      issue: 7,
      target: 'tgt_1',
      developerInstructions: '# Skills',
    })
  })

  it('posts to the conversation door and answers its session, the same one every turn', async () => {
    const seen: { url: string; method: string; body: unknown }[] = []
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string, init: RequestInit = {}) => {
        seen.push({ url, method: init.method ?? 'GET', body: JSON.parse(String(init.body)) })
        return new Response(JSON.stringify({ sessionId: CONV, runId: `run_${seen.length}` }), { status: 200, headers: { 'content-type': 'application/json' } })
      }),
    )
    expect(await say(T, { input: 'hello' })).toEqual({ session: CONV, run: 'run_1' })
    expect(await say(T, { session: CONV, input: 'and then' })).toEqual({ session: CONV, run: 'run_2' })
    expect(seen.map((s) => [s.method, s.url, s.body])).toEqual([
      ['POST', 'https://api.hanzo.ai/v1/agent/turns', { input: 'hello' }],
      ['POST', 'https://api.hanzo.ai/v1/agent/turns', { input: 'and then', session: CONV }],
    ])
  })

  it('refuses to send nothing, and an answer that names no session', async () => {
    await expect(say(T, { input: '   ' })).rejects.toThrow('Say something')
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({}), { status: 200, headers: { 'content-type': 'application/json' } })))
    await expect(say(T, { input: 'hi' })).rejects.toThrow('The conversation answered with no session')
  })

  it('tells a conversation from a run in it by its parent, and reads its tree', () => {
    const conv = session({ id: CONV, channel: 'app', rootSessionId: CONV })
    // A conversation's run is born in its tree, on its channel.
    const run = session({ id: 'sess_r', kind: 'coding', channel: 'app', parentSessionId: CONV, rootSessionId: CONV })
    const alone = session({ id: 'sess_s', kind: 'coding', rootSessionId: 'sess_s' })
    expect(talks(conv)).toBe(true)
    expect(talks(run)).toBe(false)
    expect(talks(alone)).toBe(false)
    expect(run).toMatchObject({ parent: CONV, root: CONV, channel: 'app' })
  })
})

describe('a conversation’s log', () => {
  it('is read whole, a page at a time, until the platform names no page beyond', async () => {
    const asked: string[] = []
    const pages = [
      { events: [{ id: 'e1', sessionId: CONV, seq: 1, kind: 'message', payload: { role: 'user', text: 'hi' } }], next: 1 },
      { events: [{ id: 'e2', sessionId: CONV, seq: 2, kind: 'message', payload: { text: 'hello' } }] },
    ]
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string) => {
        asked.push(url)
        return new Response(JSON.stringify(pages[asked.length - 1]), { status: 200, headers: { 'content-type': 'application/json' } })
      }),
    )
    expect((await log(T, CONV)).map((e) => e.seq)).toEqual([1, 2])
    expect(asked).toEqual([
      `https://api.hanzo.ai/v1/agent/sessions/${CONV}/events?limit=1000`,
      `https://api.hanzo.ai/v1/agent/sessions/${CONV}/events?after=1&limit=1000`,
    ])
  })
})

describe('a conversation’s record', () => {
  const ev = (seq: number, kind: string, payload: unknown) => ({ seq, kind, payload })
  const record = [
    ev(1, 'message', { role: 'user', text: 'build me a page' }),
    ev(2, 'progress', { phase: 'running', activity: 'Starting a coding agent' }),
    ev(3, 'spawn', { session: 'sess_run1', paused: false }),
    ev(4, 'progress', { phase: 'running', activity: 'Install' }),
    ev(5, 'spawn', { session: 'sess_run1', paused: true }),
    ev(6, 'log', { runId: 'run_1', status: 'ok', model: 'enso', error: '' }),
    ev(7, 'message', { text: 'Built it.', tools: '- agent' }),
    ev(8, 'message', { role: 'user', text: 'now make it blue' }),
    ev(9, 'log', { runId: 'run_2', status: 'error', error: 'the model did not answer' }),
  ]

  it('reads as the conversation was had: the words, the run once, the answer, and why a turn failed', () => {
    const drawn = cards(record)
    expect(drawn.map((c) => (c.kind === 'said' ? `${c.who}:${c.text}` : c.kind === 'run' ? `run:${c.session}` : c.kind))).toEqual([
      'person:build me a page',
      'run:sess_run1',
      'agent:Built it.',
      'person:now make it blue',
      'note',
      'step',
    ])
  })

  it('says what the turn is doing now', () => {
    expect(now(record)).toBe('Install')
    expect(now([])).toBe('')
  })

  it('draws a question the run asked, and its answer once given', () => {
    const ask = [
      ev(1, 'status', { status: 'asked', question: 'qst_1', text: 'Start a coding run: build the page', options: [{ label: 'Start it' }, { label: 'Not now' }] }),
    ]
    expect(cards(ask)).toMatchObject([{ kind: 'ask', question: 'qst_1', options: ['Start it', 'Not now'], answer: '' }])
    expect(cards([...ask, ev(2, 'status', { status: 'answered', question: 'qst_1', answer: 'Start it', chosen: true })])).toMatchObject([
      { kind: 'ask', question: 'qst_1', answer: 'Start it' },
    ])
  })
})
