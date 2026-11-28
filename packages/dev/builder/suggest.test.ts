import { describe, expect, it } from 'vitest'

import type { Automation } from './api/auto.ts'
import { prefs } from './api/pref.ts'
import type { Session } from './api/sessions.ts'
import type { Issue } from './merge.ts'
import {
  ACTIONS,
  answer,
  DAY,
  dismissed,
  due,
  hash,
  idOf,
  inputs,
  MODEL,
  QUIET,
  REMEMBER,
  every,
  request,
  SCHEMA,
  seed,
  shown,
  fill,
  STARTERS,
  starter,
  sig,
  skip,
  stale,
  kept,
  type Input,
  type Kept,
} from './suggest.ts'

const PR = 'https://github.com/hanzo-inc/app/pull/29'
const BUG = 'https://github.com/hanzo-inc/kai/issues/12'
const FORGE = 'https://git.hanzo.ai/hanzoai/git/issues/1'

const issue = (o: Partial<Issue>): Issue => ({
  key: o.url ?? 'k',
  repo: '',
  forge: '',
  hint: '',
  number: 1,
  title: 'An issue',
  body: '',
  state: 'open',
  labels: [],
  assignees: [],
  url: '',
  sources: ['github'],
  updated: '2026-10-19T10:00:00Z',
  created: '',
  kind: 'issue',
  status: 'todo',
  priority: 'none',
  pull: false,
  board: '',
  ...o,
})

const run = (o: Partial<Session>): Session => ({
  id: 'sess_1',
  title: 'A run',
  status: 'done',
  agent: 'hanzo',
  actor: 'acme/u1',
  repo: 'acme/app',
  project: '',
  target: '',
  host: '',
  cwd: '',
  kind: 'coding',
  base: 'main',
  branch: 'agent/x',
  environment: 'sandbox',
  mode: 'build',
  pr: '',
  sandbox: '',
  org: 'acme',
  published: false,
  channel: '',
  parent: '',
  root: 'sess_1',
  events: 3,
  createdAt: '2026-10-19T09:00:00Z',
  updatedAt: '2026-10-19T09:30:00Z',
  endedAt: '',
  ...o,
})

const flow = (o: Partial<Automation>): Automation => ({
  id: 'auto_1',
  name: 'Monday brief',
  instructions: 'Brief me every Monday on adoption signals.\nKeep it short.',
  model: null,
  schedule: { kind: 'weekly', at: '09:00', day: 'mon' },
  permissions: 'ask',
  notify: true,
  postTo: [],
  enabled: true,
  source: 'chat',
  project: null,
  draft: false,
  next: null,
  last: null,
  created: '2026-10-01T00:00:00Z',
  updated: '2026-10-18T00:00:00Z',
  ...o,
})

const NONE = { conversations: [], issues: [], runs: [], automations: [], files: [], me: 'acme/u1' }

const SOME: Input[] = [
  { kind: 'pull', key: PR, title: 'Login redirects new emails to /signup', state: 'closed', text: 'hanzo-inc/app', href: PR, to: PR, updated: '2026-10-20T08:00:00Z' },
  { kind: 'issue', key: BUG, title: 'Kai P0 mapping bug', state: 'todo', text: 'hanzo-inc/kai', href: BUG, to: BUG, updated: '2026-10-19T08:00:00Z' },
  { kind: 'conversation', key: 'c1', title: 'Enso sales targets', state: '', text: '', href: '', to: '/chat/c1', updated: '2026-10-18T08:00:00Z' },
  { kind: 'file', key: 'lib_1', title: 'poster.png', state: 'image', text: 'studio', href: '', to: '-/artifacts', item: 'lib_1', updated: '2026-10-17T08:00:00Z' },
]

/** A chat completion whose first choice says `text`. */
const said = (text: string) => ({ choices: [{ index: 0, finish_reason: 'stop', message: { role: 'assistant', content: text } }] })
const rows = (...r: object[]) => said(JSON.stringify({ rows: r }))
const OPEN = { icon: 'github', title: 'Check the login fixes on #29', summary: 'PR #29 closed.', action: 'open', source: PR, prompt: '', every: '' }
const SEND = { icon: 'chat', title: 'Draft the Enso sales email', summary: '', action: 'send', source: '', prompt: 'Draft the email to the Enso leads from our sales targets chat.', every: '' }
const TASK = { icon: 'agent', title: 'Fix the Kai mapping bug', summary: 'It drops rows.', action: 'task', source: '', prompt: `Fix ${BUG}: the mapping drops rows.`, every: '' }
const PLAN = { icon: 'agent', title: 'Brief me on PRs each morning', summary: '', action: 'schedule', source: '', prompt: 'Summarize my open pull requests.', every: 'weekdays 09:00' }

describe('what the model reads', () => {
  it('is the person’s activity: their own runs, a GitHub address kept, the forge’s too, a draft automation left out', () => {
    const got = inputs({
      ...NONE,
      issues: [issue({ url: BUG, title: 'Kai P0 mapping bug', repo: 'hanzo-inc/kai', body: '\n\nThe mapping drops rows.\nMore.' }), issue({ url: FORGE, title: 'Resolver panic', state: 'closed', updated: '2026-10-10T00:00:00Z' })],
      runs: [run({ id: 'sess_mine', pr: PR }), run({ id: 'sess_theirs', actor: 'acme/u2' })],
      automations: [flow({}), flow({ id: 'auto_2', draft: true })],
      conversations: [{ id: 'c1', title: 'Enso sales targets', updated: '2026-10-20T07:00:00Z', pinned: false }],
    })
    expect(got.map((i) => [i.kind, i.key, i.state, i.href, i.to])).toEqual([
      ['conversation', 'c1', '', '', '/chat/c1'],
      ['issue', BUG, 'todo', BUG, BUG],
      ['run', 'sess_mine', 'done', PR, 'sess_mine'],
      ['automation', 'auto_1', 'on', '', '-/automations/auto_1'],
      ['issue', FORGE, 'closed', FORGE, FORGE],
    ])
    expect(got.find((i) => i.key === BUG)?.text).toBe('hanzo-inc/kai: The mapping drops rows.')
    expect(got.find((i) => i.kind === 'automation')?.text).toBe('Brief me every Monday on adoption signals.')
  })

  it('reads no run without knowing who the person is', () => {
    expect(inputs({ ...NONE, me: '', runs: [run({ actor: '' })] })).toEqual([])
  })

  it('takes each kind’s newest few', () => {
    const many = Array.from({ length: 20 }, (_, n) => ({ id: `c${n}`, title: `Talk ${n}`, updated: `2026-10-${String(n + 1).padStart(2, '0')}T00:00:00Z`, pinned: false }))
    const got = inputs({ ...NONE, conversations: many })
    expect(got).toHaveLength(8)
    expect(got[0]!.key).toBe('c19')
  })

  it('seeds from signup and Settings', () => {
    expect(seed({ callName: 'Z', work: ' Engineering ', instructions: '' }, { onboarding: { role: 'Software engineer', use: 'team', plan: 'pro' }, terms: {} })).toEqual({
      name: 'Z',
      work: 'Engineering',
      role: 'Software engineer',
      use: 'team',
    })
    expect(seed({}, null)).toEqual({})
  })
})

describe('the request', () => {
  it('asks Hanzo’s model under a strict schema, each item by its key, and nothing when there is nothing', () => {
    expect(request([], {}, [], new Date('2026-10-20T00:00:00Z'))).toBeNull()
    const r = request(SOME, { role: 'Software engineer' }, [BUG], new Date('2026-10-20T00:00:00Z'))!
    expect(r.model).toBe(MODEL)
    expect(r.response_format).toEqual({ type: 'json_schema', json_schema: { name: 'suggestions', strict: true, schema: SCHEMA } })
    const user = JSON.parse((r.messages as { content: string }[])[1]!.content)
    expect(user.today).toBe('2026-10-20')
    expect(user.person).toEqual({ role: 'Software engineer' })
    expect(user.skip).toEqual([BUG])
    expect(user.activity[0]).toEqual({ key: PR, kind: 'pull', title: 'Login redirects new emails to /signup', state: 'closed', text: 'hanzo-inc/app', updated: '2026-10-20' })
  })

  it('offers the model the four actions and nothing else', () => {
    expect(SCHEMA.properties.rows.items.properties.action.enum).toEqual([...ACTIONS])
    expect(ACTIONS).toEqual(['open', 'send', 'task', 'schedule'])
  })

  it('seeds an account with no activity from signup alone', () => {
    const r = request([], { work: 'Design' }, [], new Date())!
    expect(JSON.parse((r.messages as { content: string }[])[1]!.content).activity).toEqual([])
  })
})

describe('the answer', () => {
  it('binds an open to the item its key names: where it opens, its mark and a stable id', () => {
    expect(answer(rows(OPEN), SOME)).toEqual([{ id: hash(PR), icon: 'github', title: OPEN.title, summary: 'PR #29 closed.', act: { kind: 'open', to: PR } }])
    expect(answer(rows({ ...OPEN, icon: 'github', source: 'c1' }), SOME)[0]).toMatchObject({ icon: 'chat', act: { kind: 'open', to: '/chat/c1' } })
    expect(answer(rows({ ...OPEN, source: 'lib_1' }), SOME)[0]).toMatchObject({ icon: 'file', act: { kind: 'open', to: '-/artifacts', item: 'lib_1' } })
  })

  it('binds a send and a task to their prompt', () => {
    expect(answer(rows(SEND), SOME)[0]!.act).toEqual({ kind: 'send', prompt: SEND.prompt })
    expect(answer(rows(TASK), SOME)[0]!.act).toEqual({ kind: 'task', prompt: TASK.prompt })
  })

  it('binds a schedule to its prompt and a time it reads', () => {
    expect(answer(rows(PLAN), SOME)[0]!.act).toEqual({ kind: 'schedule', name: PLAN.title, prompt: PLAN.prompt, schedule: { kind: 'weekdays', at: '09:00' } })
  })

  it('drops a row it cannot bind: an open naming no item, an act with no prompt, a time it cannot read, an action it does not know', () => {
    expect(answer(rows({ ...OPEN, source: 'https://github.com/elsewhere/x/pull/1' }), SOME)).toEqual([])
    expect(answer(rows({ ...OPEN, source: '' }), SOME)).toEqual([])
    expect(answer(rows({ ...SEND, prompt: ' ' }), SOME)).toEqual([])
    expect(answer(rows({ ...TASK, prompt: '' }), SOME)).toEqual([])
    expect(answer(rows({ ...PLAN, every: 'every Monday morning' }), SOME)).toEqual([])
    expect(answer(rows({ ...SEND, action: 'fill' }), SOME)).toEqual([])
    expect(answer(rows({ ...SEND, title: '' }), SOME)).toEqual([])
  })

  it('draws a row that opens nothing with its mark as said, chat for an unknown one, never GitHub’s', () => {
    const got = answer(rows(TASK, { ...SEND, icon: 'robot' }, { ...SEND, title: 'Ask about the issue', icon: 'github' }), SOME)
    expect(got.map((r) => [r.icon, r.id])).toEqual([
      ['agent', idOf('', TASK.title)],
      ['chat', idOf('', SEND.title)],
      ['task', idOf('', 'Ask about the issue')],
    ])
  })

  it('is one row a thing, three at most', () => {
    expect(answer(rows(OPEN, OPEN), SOME)).toHaveLength(1)
    const four = ['a', 'b', 'c', 'd'].map((t) => ({ ...SEND, title: t }))
    expect(answer(rows(...four), SOME).map((r) => r.title)).toEqual(['a', 'b', 'c'])
  })

  it('reads JSON after a model’s preamble, and none from prose, an error or no answer', () => {
    expect(answer(said(`Here you go:\n${JSON.stringify({ rows: [SEND] })}\n`), SOME)).toHaveLength(1)
    expect(answer(said('Nothing to suggest.'), SOME)).toEqual([])
    expect(answer(said('{"rows": []}'), SOME)).toEqual([])
    expect(answer({ error: { message: 'no' } }, SOME)).toEqual([])
    expect(answer(null, SOME)).toEqual([])
  })
})

describe('every', () => {
  it('reads the four ways a schedule is said, and nothing else', () => {
    expect(every('hourly')).toEqual({ kind: 'hourly', at: '00:00' })
    expect(every('daily 07:30')).toEqual({ kind: 'daily', at: '07:30' })
    expect(every(' Weekdays 09:00 ')).toEqual({ kind: 'weekdays', at: '09:00' })
    expect(every('weekly mon 09:00')).toEqual({ kind: 'weekly', day: 'mon', at: '09:00' })
    for (const no of ['', 'daily', 'daily 9am', 'daily 24:00', 'weekly monday 09:00', 'weekly mon', 'hourly 10:00', 'cron * * * * *']) expect(every(no)).toBeNull()
  })
})

describe('the fingerprint and when rows are made again', () => {
  it('moves with a new source or a new state, not with order or time', () => {
    const a = sig(SOME, {})
    expect(sig([...SOME].reverse(), {})).toBe(a)
    expect(sig(SOME.map((i) => ({ ...i, updated: '2026-10-21T00:00:00Z' })), {})).toBe(a)
    expect(sig(SOME.map((i, n) => (n ? i : { ...i, state: 'open' })), {})).not.toBe(a)
    expect(sig(SOME.slice(1), {})).not.toBe(a)
    expect(sig(SOME, { role: 'Designer' })).not.toBe(a)
  })

  const at = Date.parse('2026-10-20T12:00:00Z')
  const was = (ago: number, s = 'x'): Kept => ({ at: new Date(at - ago).toISOString(), sig: s, rows: [], dismissed: [] })

  it('is due with none kept, a day on, or once the activity moved and the rows are not new', () => {
    expect(due(null, 'x', at)).toBe(true)
    expect(due(was(DAY), 'x', at)).toBe(true)
    expect(due(was(DAY - 1), 'x', at)).toBe(false)
    expect(due(was(QUIET - 1), 'y', at)).toBe(false)
    expect(due(was(QUIET), 'y', at)).toBe(true)
    expect(due({ ...was(0), at: 'garbage' }, 'x', at)).toBe(true)
  })

  it('reads no source while kept rows are new', () => {
    expect(stale(null, at)).toBe(true)
    expect(stale(was(QUIET - 1), at)).toBe(false)
    expect(stale(was(QUIET), at)).toBe(true)
  })
})

describe('what the account keeps', () => {
  const row: Kept['rows'][number] = { id: 'r1', icon: 'github', title: 'T', summary: 'S', act: { kind: 'open', to: PR } }
  const plan: Kept['rows'][number] = { id: 'r3', icon: 'agent', title: 'P', summary: '', act: { kind: 'schedule', name: 'P', prompt: 'Brief me.', schedule: { kind: 'weekly', day: 'mon', at: '09:00' } } }

  it('reads the suggest key, a row that does nothing dropped (one from before rows were actions, among them)', () => {
    expect(kept(undefined)).toBeUndefined()
    const before = { id: 'r0', icon: 'github', title: 'T', href: PR, summary: 'S', action: 'A', prompt: 'P' }
    expect(kept({ at: '2026-10-20T00:00:00Z', sig: 'x', rows: [row, plan, before, { ...row, act: { kind: 'send', prompt: '' } }, 'junk'], dismissed: ['r9', 3] })).toEqual({
      at: '2026-10-20T00:00:00Z',
      sig: 'x',
      rows: [row, plan],
      dismissed: ['r9'],
    })
  })

  it('rides the preference document with the builder’s other keys', () => {
    const doc = prefs({ theme: 'dark', suggest: { at: 'a', sig: 's', rows: [row], dismissed: [] }, gear: {} })
    expect(doc).toEqual({ theme: 'dark', suggest: { at: 'a', sig: 's', rows: [row], dismissed: [] } })
  })

  it('draws the rows not dismissed, and remembers a dismissal first', () => {
    const k: Kept = { at: 'a', sig: 's', rows: [row, { ...row, id: 'r2', act: { kind: 'open', to: BUG } }], dismissed: [] }
    expect(shown(k).map((r) => r.id)).toEqual(['r1', 'r2'])
    const after = dismissed(k, 'r1')
    expect(shown(after).map((r) => r.id)).toEqual(['r2'])
    expect(dismissed(after, 'r1').dismissed).toEqual(['r1'])
    expect(shown(null)).toEqual([])
  })

  it('starts with three that each do something at once', () => {
    expect(STARTERS).toHaveLength(3)
    for (const r of STARTERS) expect(['send', 'task']).toContain(r.act.kind)
    for (const r of STARTERS) expect(r.title.split(' ').length).toBeLessThanOrEqual(7)
  })

  it('asks what the page is for before Dev builds one', () => {
    const dev = STARTERS.find((r) => r.id === 'start:dev')!
    expect(dev.act.kind).toBe('task')
    const prompt = 'prompt' in dev.act ? dev.act.prompt : ''
    expect(prompt).toMatch(/^I want a one-page landing page\. Before you build anything, ask me /)
    for (const what of ['what it is for', 'who it is for', 'what it should say']) expect(prompt).toContain(what)
  })

  it('knows a starter, whose words are authored and translated, from a row of the person’s own', () => {
    expect(STARTERS.every(starter)).toBe(true)
    expect(STARTERS.map((r) => r.title)).toEqual(['Ask what Hanzo can do for you', 'Build a landing page with Dev', 'Get a cited brief on AI agents'])
    expect(starter({ ...STARTERS[0]!, id: idOf('', STARTERS[0]!.title) })).toBe(false)
  })

  it('fills to three with what Hanzo does, never past a dismissal, and none before the rows are read', () => {
    const one: Kept = { at: 'a', sig: 's', rows: [row], dismissed: [] }
    expect(fill(one).map((r) => r.id)).toEqual(['r1', STARTERS[0]!.id, STARTERS[1]!.id])
    expect(fill({ ...one, rows: [] }).map((r) => r.id)).toEqual(STARTERS.map((r) => r.id))
    expect(fill(dismissed(one, STARTERS[0]!.id)).map((r) => r.id)).toEqual(['r1', STARTERS[1]!.id, STARTERS[2]!.id])
    expect(fill(null)).toEqual([])
  })

  it('forgets the oldest dismissal past the limit', () => {
    let k: Kept = { at: 'a', sig: 's', rows: [], dismissed: [] }
    for (let n = 0; n <= REMEMBER; n++) k = dismissed(k, `r${n}`)
    expect(k.dismissed).toHaveLength(REMEMBER)
    expect(k.dismissed[0]).toBe(`r${REMEMBER}`)
    expect(k.dismissed).not.toContain('r0')
  })

  it('tells the model to skip what was dismissed: an item by its key, anything else by its words', () => {
    const k: Kept = { at: 'a', sig: 's', rows: [{ id: idOf('', 'Old idea'), icon: 'chat', title: 'Old idea', summary: '', act: { kind: 'send', prompt: 'P' } }], dismissed: [hash(BUG), idOf('', 'Old idea')] }
    expect(skip(k, SOME)).toEqual([BUG, 'Old idea'])
    expect(skip(null, SOME)).toEqual([])
  })
})
