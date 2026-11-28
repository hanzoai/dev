/**
 * A run's controls and its transcript: the addresses and bodies for paging,
 * pausing, resuming, renaming and sharing a run; the follow-up and the approved
 * plan a finished run starts; the cards its events draw; and the markdown the
 * agent writes.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import { blocks, spans } from '../markdown.ts'
import type { Target } from './call.ts'
import { approve, followUp, headline, type Earlier } from './coding.ts'
import * as sessions from './sessions.ts'
import { answer, cards, settled, steps, type Card } from './turn.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'hanzo' }

interface Seen {
  url: string
  method: string
  body: unknown
}

function answerWith(status: number, body: unknown) {
  const seen: Seen[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      seen.push({ url, method: init.method ?? 'GET', body: init.body ? JSON.parse(String(init.body)) : undefined })
      return new Response(body === undefined ? null : JSON.stringify(body), { status, headers: { 'content-type': 'application/json' } })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

describe('sessions', () => {
  it('pages the list with its filters and hands back the cursor', async () => {
    const seen = answerWith(200, { sessions: [{ id: 'sess_1', title: 'a', published: true, org: 'hanzo' }, { title: 'no id' }], next: 'c2' })
    const p = await sessions.page(T, { kind: 'coding', status: 'paused', limit: 50, after: 'c1' })
    expect(seen[0]!.url).toBe('https://api.hanzo.ai/v1/agent/sessions?kind=coding&status=paused&limit=50&after=c1')
    expect(p.next).toBe('c2')
    expect(p.sessions.map((s) => [s.id, s.published, s.org])).toEqual([['sess_1', true, 'hanzo']])
  })

  it('reads the last page as having no cursor', async () => {
    answerWith(200, { sessions: [] })
    expect((await sessions.page(T)).next).toBe('')
  })

  it('pauses and resumes at the run’s own address', async () => {
    const seen = answerWith(200, { command: 'pause' })
    await sessions.pause(T, 'sess_1')
    await sessions.resume(T, 'sess_1')
    expect(seen.map((s) => [s.method, s.url, s.body])).toEqual([
      ['POST', 'https://api.hanzo.ai/v1/agent/sessions/sess_1/pause', {}],
      ['POST', 'https://api.hanzo.ai/v1/agent/sessions/sess_1/resume', {}],
    ])
  })

  it('renames with a trimmed title, and refuses an empty or overlong one before sending', async () => {
    const seen = answerWith(200, { id: 'sess_1', title: 'New name' })
    const s = await sessions.rename(T, 'sess_1', '  New name ')
    expect(seen[0]).toEqual({ url: 'https://api.hanzo.ai/v1/agent/sessions/sess_1', method: 'PATCH', body: { title: 'New name' } })
    expect(s.title).toBe('New name')
    await expect(sessions.rename(T, 'sess_1', '  ')).rejects.toThrow('Give the run a name')
    await expect(sessions.rename(T, 'sess_1', 'x'.repeat(513))).rejects.toThrow('at most 512')
    expect(seen).toHaveLength(1)
  })

  it('shares and withdraws a run’s story, and names its public address', async () => {
    const seen = answerWith(200, { id: 'sess_1', published: true })
    expect((await sessions.publish(T, 'sess_1', true)).published).toBe(true)
    await sessions.publish(T, 'sess_1', false)
    expect(seen.map((s) => s.body)).toEqual([{ published: true }, { published: false }])
    expect(sessions.story(T, 'hanzo', 'shop')).toBe('https://api.hanzo.ai/v1/agent/builds/hanzo/shop')
    expect(sessions.story(T, 'hanzo', '')).toBe('')
  })

  it('keeps the opening user ask when a long run’s recent tail starts past seq 1', async () => {
    const seen: string[] = []
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string) => {
        seen.push(url)
        if (url.endsWith('/events?limit=2')) {
          return new Response(
            JSON.stringify({
              events: [
                { id: 'e1', sessionId: 'sess_1', seq: 1, kind: 'message', payload: { role: 'user', text: 'check this pr -\nhttps://github.com/ClickHouse/ClickHouse/pull/115724' } },
                { id: 'e2', sessionId: 'sess_1', seq: 2, kind: 'status', payload: { status: 'started', branch: 'agent/x' } },
              ],
            }),
            { status: 200, headers: { 'content-type': 'application/json' } },
          )
        }
        return new Response(
          JSON.stringify({
            id: 'sess_1',
            title: 'ClickHouse: check this pr -',
            status: 'error',
            events: 280,
            recentEvents: [{ id: 'e280', sessionId: 'sess_1', seq: 280, kind: 'status', payload: { status: 'error', error: 'stopped' } }],
          }),
          { status: 200, headers: { 'content-type': 'application/json' } },
        )
      }),
    )
    const d = await sessions.get(T, 'sess_1')
    expect(seen).toEqual([
      'https://api.hanzo.ai/v1/agent/sessions/sess_1',
      'https://api.hanzo.ai/v1/agent/sessions/sess_1/events?limit=2',
    ])
    expect(d.recent.map((e) => e.seq)).toEqual([1, 2, 280])
    const drawn = cards(d.recent, 'build')
    expect(drawn[0]).toMatchObject({
      kind: 'said',
      who: 'person',
      text: 'check this pr -\nhttps://github.com/ClickHouse/ClickHouse/pull/115724',
    })
  })

  it('fetches limit=500 and backfills middle pages when a long coding run exceeds one page', async () => {
    const seen: string[] = []
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string) => {
        seen.push(url)
        if (url.endsWith('/sessions/sess_big?limit=500')) {
          return new Response(
            JSON.stringify({
              id: 'sess_big',
              status: 'running',
              events: 503,
              recentEvents: [
                { id: 'e4', sessionId: 'sess_big', seq: 4, kind: 'log', payload: { message: 'step 4' } },
                { id: 'e5', sessionId: 'sess_big', seq: 5, kind: 'status', payload: { status: 'done' } },
              ],
            }),
            { status: 200, headers: { 'content-type': 'application/json' } },
          )
        }
        if (url.endsWith('/sessions/sess_big/events?limit=500')) {
          return new Response(
            JSON.stringify({
              events: [
                { id: 'e1', sessionId: 'sess_big', seq: 1, kind: 'message', payload: { role: 'user', text: 'initial ask' } },
                { id: 'e2', sessionId: 'sess_big', seq: 2, kind: 'log', payload: { message: 'step 2' } },
              ],
              next: 2,
            }),
            { status: 200, headers: { 'content-type': 'application/json' } },
          )
        }
        return new Response(
          JSON.stringify({
            events: [{ id: 'e3', sessionId: 'sess_big', seq: 3, kind: 'log', payload: { message: 'step 3' } }],
            next: 0,
          }),
          { status: 200, headers: { 'content-type': 'application/json' } },
        )
      }),
    )
    const d = await sessions.get(T, 'sess_big', { limit: 500 })
    expect(seen).toEqual([
      'https://api.hanzo.ai/v1/agent/sessions/sess_big?limit=500',
      'https://api.hanzo.ai/v1/agent/sessions/sess_big/events?limit=500',
      'https://api.hanzo.ai/v1/agent/sessions/sess_big/events?after=2&limit=500',
    ])
    expect(d.recent.map((e) => e.seq)).toEqual([1, 2, 3, 4, 5])
  })
})

const EARLIER: Earlier = {
  id: 'sess_1',
  title: 'universe: Add the widget',
  repo: 'hanzoai/universe',
  base: 'main',
  environment: 'sandbox',
  project: '',
  mode: 'build',
  pushed: true,
}

describe('a follow-up', () => {
  it('resumes the session cleanly without prepending a 1-line summary stub when after is set', () => {
    const ask = followUp(EARLIER, ' now add tests ')
    expect(ask).toMatchObject({ repo: 'hanzoai/universe', after: 'sess_1', mode: 'build' })
    expect(ask.base).toBeUndefined()
    expect(ask.targetId).toBeUndefined()
    expect(ask.prompt).toBe('now add tests')
  })

  it('goes on in the sandbox the run kept, whether it pushed or not', () => {
    const ask = followUp({ ...EARLIER, pushed: false, mode: 'plan' }, 'go on')
    expect(ask).toMatchObject({ after: 'sess_1', mode: 'plan', prompt: 'go on' })
    expect(ask.base).toBeUndefined()
  })

  it('on a machine, which keeps no sandbox, starts where the run started when it pushed nothing', () => {
    const ask = followUp({ ...EARLIER, environment: 'tgt_1', pushed: false }, 'go on')
    expect(ask.after).toBeUndefined()
    expect(ask).toMatchObject({
      base: 'main',
      targetId: 'tgt_1',
      prompt: 'go on\n\nThis follows an earlier run on this codebase: “Add the widget”.',
    })
  })

  it('goes on with nothing said by continuing the resumed session cleanly', () => {
    expect(followUp(EARLIER, '').prompt).toBe('Continue where the earlier run left off.')
    expect(followUp({ ...EARLIER, environment: 'tgt_1', pushed: false }, '').prompt).toBe(
      'Add the widget\n\nContinue where the earlier run left off.',
    )
  })

  it('stays on the machine and in the project the run was on', () => {
    expect(followUp({ ...EARLIER, environment: 'tgt_1', project: 'widgets' }, 'x')).toMatchObject({ targetId: 'tgt_1', project: 'widgets' })
  })

  it('builds an approved plan in the sandbox the plan read, titled by its ask', () => {
    const ask = approve({ ...EARLIER, mode: 'plan', pushed: false }, '1. Read it\n2. Change it\n')
    expect(ask).toMatchObject({ mode: 'build', after: 'sess_1', repo: 'hanzoai/universe' })
    expect(ask.prompt).toBe('Add the widget\n\nCarry out this plan:\n\n1. Read it\n2. Change it')
  })

  it('reads the ask out of a title that leads with the codebase', () => {
    expect(headline('universe: Add the widget', 'hanzoai/universe')).toBe('Add the widget')
    expect(headline('Something else', 'hanzoai/universe')).toBe('Something else')
  })
})

const ev = (seq: number, kind: string, payload: unknown) => ({ id: `e${seq}`, sessionId: 's', seq, kind, actor: 'a', payload, createdAt: '' })

/** One line of the agent's own narration, as the harness streams it. */
const line = (type: string, item?: Record<string, unknown>) => ({ type, ...(item ? { item } : {}) })

describe('cards', () => {
  const run = [
    ev(1, 'message', { role: 'user', text: 'Add the widget' }),
    ev(2, 'status', { status: 'started', branch: 'agent/ab12' }),
    ev(3, 'tool-call', { step: 'lease', message: 'leasing a dev sandbox', status: 'running' }),
    ev(4, 'tool-call', { step: 'leased', message: 'sandbox m_1 (dev)', status: 'running' }),
    ev(5, 'tool-call', { step: 'clone', message: 'cloning the codebase', status: 'running' }),
    ev(6, 'log', { message: 'Cloning into universe…\n' }),
    ev(7, 'tool-call', { step: 'exit', message: 'exit 0' }),
    ev(8, 'event', line('thread.started')),
    ev(9, 'event', line('item.completed', { id: 'i0', type: 'reasoning', text: '**Reading** the widget' })),
    ev(10, 'event', line('item.completed', { id: 'i1', type: 'agent_message', text: 'I will look at the code first.' })),
    ev(11, 'event', line('item.started', { id: 'i2', type: 'command_execution', command: 'go test ./...', aggregated_output: '', status: 'in_progress' })),
    ev(12, 'log', { message: 'FAIL widgets\n' }),
    ev(13, 'control', { command: 'message', message: 'use table tests' }),
    ev(14, 'event', line('item.completed', { id: 'i2', type: 'command_execution', command: 'go test ./...', aggregated_output: 'FAIL widgets', exit_code: 1, status: 'failed' })),
    ev(15, 'event', line('item.completed', { id: 'i3', type: 'file_change', changes: [{ path: '/work/widget.go', kind: 'update' }], status: 'completed' })),
    ev(16, 'event', line('item.started', { id: 'i4', type: 'todo_list', items: [{ text: 'Read', completed: true }, { text: 'Test', completed: false }] })),
    ev(17, 'event', line('item.completed', { id: 'i5', type: 'mcp_tool_call', server: 'run', tool: 'ask_user', status: 'completed' })),
    ev(18, 'event', line('item.completed', { id: 'i6', type: 'web_search', query: 'go table tests' })),
    ev(19, 'event', line('item.completed', { id: 'i7', type: 'agent_message', text: 'Added **New**. See `widget.go`.' })),
    ev(20, 'event', line('turn.completed')),
    ev(21, 'tool-call', { step: 'push', message: 'pushing agent/ab12', status: 'running' }),
    ev(22, 'tool-call', { step: 'done', message: 'finished', status: 'ok' }),
    ev(23, 'status', { status: 'done', changed: true, branch: 'agent/ab12', answer: 'Added **New**. See `widget.go`.' }),
  ]
  const got = cards(run, 'build')
  const of = <K extends Card['kind']>(k: K) => got.filter((c): c is Extract<Card, { kind: K }> => c.kind === k)

  it('draws the run’s own steps and the agent’s tools, and never its log', () => {
    expect(of('step').map((s) => [s.name, s.detail, s.ran])).toEqual([
      ['Sandbox', 'leasing a dev sandbox', 'done'],
      ['Clone', 'cloning the codebase', 'done'],
      ['Called', 'run.ask_user()', 'done'],
      ['Search', 'go table tests', 'done'],
      ['Push', 'pushing agent/ab12', 'running'],
    ])
    expect(JSON.stringify(got)).not.toContain('Cloning into')
  })

  it('draws each item once, as it last stood', () => {
    expect(of('shell')).toMatchObject([{ command: 'go test ./...', output: 'FAIL widgets', ran: 'error' }])
    expect(of('edit')).toMatchObject([{ files: [{ path: 'widget.go', kind: 'update', added: null, removed: null }], ran: 'done' }])
    expect(of('think')).toMatchObject([{ text: '**Reading** the widget' }])
    expect(of('todo')).toMatchObject([{ items: [{ text: 'Read', done: true }, { text: 'Test', done: false }] }])
    expect(JSON.stringify(got)).not.toContain('/work/')
  })

  it('draws what the agent said once, the answer last, and a person’s words as theirs', () => {
    const said = of('said')
    expect(said.filter((s) => s.who === 'agent').map((s) => s.text)).toEqual(['I will look at the code first.', 'Added **New**. See `widget.go`.'])
    expect(got.findLast((c) => c.kind === 'said')).toMatchObject({ text: 'Added **New**. See `widget.go`.' })
    expect(said.filter((s) => s.who === 'person').map((s) => s.text)).toEqual(['Add the widget', 'use table tests'])
    expect(got[0]).toMatchObject({ kind: 'said', who: 'person', text: 'Add the widget' })
    expect(of('note').map((n) => n.text)).toEqual(['Started on agent/ab12', 'Pushed agent/ab12'])
  })

  it('says a failed turn in one plain line, its detail folded beneath', () => {
    const failed = cards([ev(1, 'event', { type: 'turn.failed', error: { message: 'unexpected status 503 Service Unavailable: this model is temporarily unavailable' } })])
    expect(failed.map((c) => c.kind)).toEqual(['note', 'step'])
    expect(failed[0]).toMatchObject({ text: 'The model did not answer. Try again in a moment, or pick another model.' })
    expect(failed[1]).toMatchObject({ name: 'Error', ran: 'error' })
  })

  it('draws a plan run’s answer as its plan, once', () => {
    const plan = '1. Read widget.go\n2. Add New'
    const got = cards(
      [
        ev(1, 'event', line('item.completed', { id: 'i0', type: 'agent_message', text: plan })),
        ev(2, 'status', { status: 'done', mode: 'plan', changed: false, plan, answer: plan }),
      ],
      'plan',
    )
    expect(got.map((c) => c.kind)).toEqual(['plan'])
    expect(answer([ev(2, 'status', { status: 'done', plan })])).toBe(plan)
  })

  it('never draws a plan for a run whose record is not a plan', () => {
    expect(cards([ev(1, 'status', { status: 'done', mode: 'plan', changed: false, plan: 'do it' })], 'build').map((c) => c.kind)).toEqual(['note'])
  })

  it('says what a person asked of the run', () => {
    const got = cards([ev(1, 'control', { command: 'pause' }), ev(2, 'control', { command: 'stop', message: 'Stopped from the builder' })])
    expect(got.map((c) => (c.kind === 'note' ? c.text : c.kind))).toEqual(['Pause asked', 'Stop asked'])
  })
})

describe('steps', () => {
  it('lists the run’s steps, not the ends of the commands in them, and settles each one another followed', () => {
    expect(
      steps([
        ev(1, 'tool-call', { step: 'lease', status: 'running' }),
        ev(2, 'tool-call', { step: 'clone', status: 'running' }),
        ev(3, 'tool-call', { step: 'exit', message: 'exit 0' }),
        ev(4, 'tool-call', { step: 'install', status: 'running' }),
      ]),
    ).toEqual([
      { name: 'lease', done: true },
      { name: 'clone', done: true },
      { name: 'install', done: false },
    ])
  })
})

describe('settled', () => {
  it('waits for the run to say where its work was kept', () => {
    expect(settled([ev(1, 'status', { status: 'started' })])).toEqual({ settled: false, pushed: false })
    expect(settled([ev(1, 'status', { status: 'started' }), ev(2, 'status', { status: 'stopped', changed: true, branch: 'agent/x' })])).toEqual({
      settled: true,
      pushed: true,
    })
    expect(settled([ev(1, 'status', { status: 'done', changed: false })])).toEqual({ settled: true, pushed: false })
    expect(settled([ev(1, 'status', { status: 'paused', changed: true })]).pushed).toBe(true)
  })
})

describe('prose', () => {
  it('reads the blocks an agent writes', () => {
    const md = '# Done\n\nChanged two files:\n\n- `a.ts`\n  still a\n- b.ts\n\n1. one\n2. two\n\n```go\nfunc New() {}\n```\n\n> quoted\n\n---\n\n| a | b |\n|---|---|\n| 1 | 2 |\nlast\nline'
    expect(blocks(md)).toEqual([
      { kind: 'heading', level: 1, text: 'Done' },
      { kind: 'paragraph', text: 'Changed two files:' },
      { kind: 'list', ordered: false, start: 1, items: [{ text: '`a.ts` still a', depth: 0 }, { text: 'b.ts', depth: 0 }] },
      { kind: 'list', ordered: true, start: 1, items: [{ text: 'one', depth: 0 }, { text: 'two', depth: 0 }] },
      { kind: 'code', lang: 'go', text: 'func New() {}' },
      { kind: 'quote', text: 'quoted' },
      { kind: 'rule' },
      { kind: 'table', head: ['a', 'b'], rows: [['1', '2']] },
      { kind: 'paragraph', text: 'last line' },
    ])
  })

  it('draws a link only to an http(s) address', () => {
    expect(spans('see [docs](https://docs.hanzo.ai) and [x](javascript:alert(1)) or ![img](https://t.example/p.png)')).toEqual([
      { kind: 'text', text: 'see ' },
      { kind: 'link', text: 'docs', href: 'https://docs.hanzo.ai' },
      { kind: 'text', text: ' and ' },
      { kind: 'text', text: 'x' },
      { kind: 'text', text: ' or ' },
      { kind: 'text', text: 'img' },
    ])
  })

  it('reads inline code, bold, italics and a bare address', () => {
    expect(spans('**New** in `a_b.ts`, _now_, at https://hanzo.build.')).toEqual([
      { kind: 'strong', text: 'New' },
      { kind: 'text', text: ' in ' },
      { kind: 'code', text: 'a_b.ts' },
      { kind: 'text', text: ', ' },
      { kind: 'em', text: 'now' },
      { kind: 'text', text: ', at ' },
      { kind: 'link', text: 'https://hanzo.build', href: 'https://hanzo.build' },
      { kind: 'text', text: '.' },
    ])
  })
})
