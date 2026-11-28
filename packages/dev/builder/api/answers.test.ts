/**
 * What every client makes of the answers the platform really gives besides the
 * happy one: a row missing a field or carrying it as the wrong type, an empty
 * body, an envelope a refusal arrived in, a list with a row it cannot name.
 * `fetch` is replaced per test with a recorder that answers what the test says
 * the platform answers.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import * as agents from './agents.ts'
import * as auto from './auto.ts'
import { call, reason, Refusal, unwrap, type Target } from './call.ts'
import * as codebases from './codebases.ts'
import * as connectors from './connectors.ts'
import { consent, consentOf, setConsent } from './consent.ts'
import * as git from './git.ts'
import { natives, nativesOf } from './mcp.ts'
import { models } from './models.ts'
import { places, ready, SANDBOX } from './places.ts'
import * as platform from './platform.ts'
import * as plugins from './plugins.ts'
import * as projects from './projects.ts'
import * as skills from './skills.ts'
import { parse, read } from './sse.ts'
import * as tools from './tools.ts'
import { verdict } from './verdict.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'acme' }
const NOBODY: Target = { api: 'https://api.hanzo.ai', token: () => null, org: null }

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

/** Answers each call by its address, in the order the test lists them. */
function route(replies: Record<string, { status?: number; json?: unknown } | Error>) {
  const seen: Seen[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      seen.push({ url, method: init.method ?? 'GET', headers: new Headers(init.headers), body: init.body ? JSON.parse(String(init.body)) : undefined })
      const r = replies[url.replace(T.api, '')]
      if (r instanceof Error) throw r
      return new Response(JSON.stringify(r?.json ?? {}), { status: r?.status ?? 200, headers: { 'content-type': 'application/json' } })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

describe('call', () => {
  it.each([
    [{ message: 'upstream closed the stream' }, 'upstream closed the stream'],
    [{ type: 'about:blank', title: 'Too Many Requests', status: 429 }, 'Too Many Requests'],
    [{ error: 'org not found' }, 'org not found'],
    [{ error: { code: 'E_QUOTA' } }, 'POST /v1/agent answered 409'],
    [{}, 'POST /v1/agent answered 409'],
  ])('reads a refusal from whichever field carries its sentence', async (body, text) => {
    answer(409, body, 'application/problem+json')
    await expect(call(T, 'POST', '/v1/agent?x=1', {})).rejects.toMatchObject({ status: 409, message: text })
  })

  it('keeps the code a problem names, so a screen can tell one refusal from another', async () => {
    answer(403, { type: 'about:blank', title: 'Forbidden', status: 403, detail: 'not shared', code: 'folder_not_granted' }, 'application/problem+json')
    await expect(call(T, 'POST', '/v1/agent/coding', {})).rejects.toMatchObject({ status: 403, message: 'not shared', code: 'folder_not_granted' })
    answer(409, { title: 'Conflict' }, 'application/problem+json')
    await expect(call(T, 'POST', '/v1/agent/coding', {})).rejects.toMatchObject({ code: '' })
  })

  it('reads a refusal whose body is not JSON as saying nothing', async () => {
    expect(await reason(new Response('<html>Bad gateway</html>', { status: 502 }))).toBe('')
  })

  it('answers undefined for a 200 with an empty body, and for a 204', async () => {
    answer(200, undefined)
    expect(await call(T, 'PUT', '/v1/tool/activation', { activate: [] })).toBeUndefined()
    answer(204, undefined)
    expect(await call(T, 'DELETE', '/v1/agent/helper')).toBeUndefined()
  })

  it('unwraps the envelope: its data, its refusal, and an answer that is no envelope', () => {
    expect(unwrap({ status: 'ok', data: { a: 1 } })).toEqual({ a: 1 })
    expect(() => unwrap({ status: 'error', msg: 'user not found' })).toThrow('user not found')
    expect(() => unwrap({ status: 'error', msg: '' })).toThrow('The platform refused that')
    expect(() => unwrap({ status: 'error' })).toThrow(Refusal)
    expect(unwrap(null)).toBeUndefined()
    expect(unwrap('ok')).toBeUndefined()
  })
})

describe('sse', () => {
  it('reads a bare field name as an empty value, a value with no space after the colon, and an id', () => {
    expect(parse('event:session\ndata\ndata:{"id":"a"}\nid:42\nretry: 3000\n\n').frames).toEqual([{ event: 'session', data: '\n{"id":"a"}', id: '42' }])
  })

  it('drops a frame with no data, as the spec does', () => {
    expect(parse('event: ping\nid: 1\n\ndata: x\n\n').frames).toEqual([{ event: 'message', data: 'x', id: '' }])
  })

  it('stops reading when the caller aborts, even when the stream refuses to cancel', async () => {
    const ctl = new AbortController()
    const got: string[] = []
    const stream = new ReadableStream<Uint8Array>({
      start(c) {
        c.enqueue(new TextEncoder().encode('data: first\n\n'))
      },
      cancel() {
        throw new Error('the socket is already gone')
      },
    })
    const done = read(
      stream,
      (f) => {
        got.push(f.data)
        ctl.abort()
      },
      ctl.signal,
    )
    await expect(done).resolves.toBeUndefined()
    expect(got).toEqual(['first'])
  })
})

describe('consent', () => {
  it('reads IAM’s defaults when the envelope carries no data', async () => {
    const seen = answer(200, { status: 'ok', msg: '' })
    expect(await consent(T)).toEqual({ insights: true, training: '' })
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/iam/consent')
    expect(consentOf({ insights: 'yes', training: 'maybe' })).toEqual({ insights: true, training: '' })
    expect(consentOf({ insights: false, training: 'refused' })).toEqual({ insights: false, training: 'refused' })
  })

  it('passes on IAM’s refusal of a change, carried under a 200', async () => {
    answer(200, { status: 'error', msg: 'consent is locked for this account' })
    await expect(setConsent(T, { training: 'granted' })).rejects.toThrow('consent is locked for this account')
  })
})

describe('verdict', () => {
  it('records a cleared verdict as cleared', async () => {
    const seen = answer(200, { accepted: 1 })
    await verdict(T, 'sess_1', 'shop', null)
    expect(seen[0].body).toMatchObject({ properties: { verdict: 'cleared', project: 'shop' } })
  })

  it('says a verdict the bus answered with nothing did not land', async () => {
    answer(204, undefined)
    await expect(verdict(T, 'sess_1', 'shop', 'up')).rejects.toThrow('The verdict was not recorded')
  })
})

describe('agents', () => {
  it('reads a thin row as empty values and an unknown period as none', () => {
    expect(agents.agent({ name: 'helper', runs: '4', cap_micro_usd: Number.NaN, tools: 'skill_triage', period: 'year' })).toEqual({
      id: '',
      name: 'helper',
      model: '',
      description: '',
      instructions: '',
      tools: [],
      status: '',
      runs: 0,
      cap: 0,
      task: 0,
      spent: 0,
      period: '',
      avatar: '',
      emoji: '',
    })
    expect(agents.agent(null).name).toBe('')
  })

  it('drafts an agent with no budget as empty fields over a month', () => {
    const d = agents.draft(agents.agent({ name: 'helper', tools: ['*'] }))
    expect(d).toEqual({ name: 'helper', description: '', model: '', instructions: '', tools: ['*'], cap: '', task: '', period: 'month', avatar: '' })
  })

  it('lists an answer with no agents as none, and drops a row with no name', async () => {
    answer(200, { agents: [{ id: 'agent_9' }, null, { id: 'agent_1', name: 'helper' }] })
    expect((await agents.agents(T)).map((a) => a.name)).toEqual(['helper'])
    answer(200, {})
    expect(await agents.agents(T)).toEqual([])
  })

  it('refuses instructions past 32 KB and a run with no budget, before sending', async () => {
    const seen = answer(201, {})
    await expect(agents.create(T, { ...agents.EMPTY, name: 'helper', instructions: 'x'.repeat(agents.MAX + 1), cap: '1', task: '1' })).rejects.toThrow('Instructions are at most 32 KB')
    await expect(agents.create(T, { ...agents.EMPTY, name: 'helper', cap: '10', task: '0' })).rejects.toThrow('Set what one run may spend')
    expect(seen).toEqual([])
  })

  it('sends a chosen model, and patches every field that changed by the agent’s name when it has no id', async () => {
    let seen = answer(201, { id: 'agent_2', name: 'reviewer', model: 'zen5.8-coder' })
    await agents.create(T, { ...agents.EMPTY, name: 'reviewer', model: 'zen5.8-coder', cap: '5', task: '1' })
    expect(seen[0].body).toMatchObject({ name: 'reviewer', model: 'zen5.8-coder' })

    const was = agents.agent({ name: 'helper', model: 'zen5.8', description: 'Old', tools: ['skill_triage'], cap_micro_usd: 10_000_000, max_task_micro_usd: 1_000_000, period: 'month' })
    seen = answer(200, { name: 'helper' })
    await agents.update(T, was, { ...agents.draft(was), model: 'zen5.8-coder', description: ' New ', tools: ['*'], task: '2', period: 'week' })
    expect(seen[0]).toMatchObject({
      method: 'PATCH',
      url: 'https://api.hanzo.ai/v1/agent/helper',
      body: { model: 'zen5.8-coder', description: 'New', tools: ['*'], max_task_micro_usd: 2_000_000, period: 'week' },
    })
    expect(agents.changes(was, { ...agents.draft(was), model: '' })).toEqual({})
  })

  it('refuses an update past its budget before sending', async () => {
    const was = agents.agent({ id: 'agent_1', name: 'helper', cap_micro_usd: 1_000_000, max_task_micro_usd: 1_000_000, period: 'day' })
    const seen = answer(200, {})
    await expect(agents.update(T, was, { ...agents.draft(was), task: '3' })).rejects.toThrow('One run cannot spend more than the whole day')
    expect(seen).toEqual([])
  })
})

describe('automations', () => {
  it('drops a row with no id, names one by its id when it has no name, and reads what is missing as off, manual and asking', () => {
    expect(auto.read({ name: 'Nightly' })).toBeNull()
    expect(auto.read('flow_3')).toBeNull()
    expect(auto.read({ id: 'flow_1', schedule: { kind: 'yearly', at: 9 }, permissions: 'root', enabled: 'yes', last: { status: 'succeeded' } })).toEqual({
      id: 'flow_1',
      name: 'flow_1',
      instructions: '',
      project: null,
      model: null,
      schedule: { kind: 'manual' },
      permissions: 'ask',
      notify: false,
      postTo: [],
      enabled: false,
      source: 'dev',
      draft: false,
      next: null,
      last: null,
      created: '',
      updated: '',
    })
  })

  it('lists none from an answer that is not a page, and keeps a run with no transcript', async () => {
    answer(200, { automations: [] })
    expect(await auto.automations(T)).toEqual([])
    answer(200, { data: [{ id: 'run_1', status: 'refused', transcript: 'https://hanzo.ai/dev?run=nope' }, { status: 'failed' }] })
    expect(await auto.runs(T, 'flow_1')).toEqual([{ id: 'run_1', status: 'refused', at: '', finished: null, summary: '', session: null, draft: '', posts: [], outputs: [] }])
  })

  it('says when a write came back without an id, and escapes the id it addresses', async () => {
    const d = { name: 'n', instructions: 'i', model: null, schedule: { kind: 'manual' as const }, permissions: 'ask' as const, notify: false, postTo: [], enabled: true, source: 'dev' as const }
    answer(201, {})
    await expect(auto.create(T, d)).rejects.toThrow('came back without an id')
    const seen = answer(200, {})
    await expect(auto.save(T, 'flow 2', { enabled: false })).rejects.toThrow('came back without an id')
    expect(seen[0]).toMatchObject({ method: 'PATCH', url: 'https://api.hanzo.ai/v1/auto/automations/flow%202' })
  })
})

describe('codebases', () => {
  it('reads a thin row: the org from the request, the default branch, and branches without blanks', () => {
    expect(codebases.codebase({ name: 'cloud', branches: ['', 7, 'agent/1'] }, 'acme')).toEqual({
      org: 'acme',
      name: 'cloud',
      description: '',
      branch: 'main',
      public: false,
      clone: '',
      updated: '',
      branches: ['agent/1'],
    })
    expect(codebases.codebase({ name: 'cloud', defaultBranch: 'dev' })?.branches).toEqual(['dev'])
    expect(codebases.codebase(null)).toBeNull()
  })

  it('remembers a picked row the list had not seen, naming it from what it has', () => {
    expect(codebases.chosen({ owner: 'acme', name: 'site' })).toEqual({
      owner: 'acme',
      name: 'site',
      full_name: 'acme/site',
      private: false,
      default_branch: 'main',
      pushed_at: '',
      installation_id: 0,
      forge: true,
      clone: '',
    })
    expect(codebases.chosen({ owner: '', name: 'site', private: true, default_branch: 'dev' })).toMatchObject({ full_name: 'site', private: true, default_branch: 'dev' })
    expect(codebases.chosen({ owner: 'acme', name: 'site', full_name: 'Acme/Site' }).full_name).toBe('Acme/Site')
  })

  it('maps a codebase with no org onto the chip by its name alone', () => {
    const c = codebases.codebase({ name: 'notes', public: true, updatedAt: '2026-09-20T10:00:00Z' })!
    expect(codebases.asRepo(c)).toMatchObject({ owner: '', full_name: 'notes', private: false, default_branch: 'main', pushed_at: '2026-09-20T10:00:00Z' })
  })

  it('reads a bare list, a page, and anything else as no codebases; a request with no org names none', async () => {
    answer(200, [{ name: 'cloud' }])
    expect((await codebases.codebases(NOBODY)).map((c) => `${c.org}/${c.name}`)).toEqual(['/cloud'])
    answer(200, { data: 'cloud' })
    expect(await codebases.codebases(T)).toEqual([])
    answer(200, { data: [{ name: 'cloud', org: 'acme', description: 'The platform' }, { name: 'site', org: 'acme' }] })
    expect((await codebases.codebases(T, ' PLATFORM ')).map((c) => c.name)).toEqual(['cloud'])
  })

  it('creates by a name with .git dropped, and says when the forge did not name what it made', async () => {
    let seen = answer(201, { name: 'notes' })
    expect((await codebases.create(NOBODY, ' notes.git ')).org).toBe('')
    expect(seen[0].body).toEqual({ name: 'notes', description: '' })
    seen = answer(201, {})
    await expect(codebases.create(T, 'notes')).rejects.toThrow('The forge created a repository and did not name it')
    await expect(codebases.create(T, '.notes')).rejects.toThrow('A repository name starts with a letter or number')
  })

  it('reads one codebase by an escaped name, and nothing when the forge names none', async () => {
    const seen = answer(200, {})
    expect(await codebases.one(NOBODY, 'my notes')).toBeNull()
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/git/repos/my%20notes')
  })
})

describe('connectors', () => {
  it('drops a server row it cannot name, and reads a thin listing as empty values', async () => {
    answer(200, { servers: [null, { name: 'no id' }, { id: 'docs', name: 'Docs', createdAt: '1790000000' }] })
    expect(await connectors.servers(T)).toEqual([{ id: 'docs', name: 'Docs', url: '', header: '', secret: false, listing: '', created: 0, admitted: true }])
    expect(connectors.listing({ id: 'x', remotes: [{ transport: 'sse' }, 'https://x'], packages: [{ registry: 'npm' }], transports: 'stdio' })).toMatchObject({
      remotes: [],
      packages: [],
      transports: [],
    })
    expect(connectors.titleOf(connectors.listing({ name: 'io.x/mcp', title: 'X' }))).toBe('X')
  })

  it('refuses a server with no name or no URL, before sending', async () => {
    const seen = answer(201, {})
    await expect(connectors.add(T, { url: 'https://docs.example/mcp' })).rejects.toThrow('A connector needs a name')
    await expect(connectors.add(T, { name: 'Docs' })).rejects.toThrow('The URL is an http(s) address')
    expect(seen).toEqual([])
  })

  it('names a listing it adds when a name is given', async () => {
    const seen = answer(201, { id: 'com-stripe', name: 'Payments' })
    await connectors.add(T, { listing: 'com.stripe_mcp', name: ' Payments ' })
    expect(seen[0].body).toEqual({ listing: 'com.stripe_mcp', name: 'Payments' })
  })

  it('asks for the first page of the whole shelf with no search and no offset', async () => {
    const seen = answer(200, { catalog: 'none', total: '12' })
    expect(await connectors.shelf(T)).toEqual({ listings: [], total: 0, offset: 0 })
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/catalog')
  })
})

describe('git', () => {
  it('reads a tree answer with odd rows, and anything else as empty', async () => {
    answer(200, { entries: [null, { name: 'README.md', path: 'README.md', type: 'blob', size: '12' }, { name: 'nameless' }] })
    expect(await git.tree(T, 'cloud', 'main', 'docs')).toEqual([{ name: 'README.md', path: 'README.md', dir: false, size: 0 }])
    expect(git.entries(null)).toEqual([])
    expect(git.entries({ entries: 'README.md' })).toEqual([])
  })

  it('reads a truncated file as no text, at the path asked for', () => {
    expect(git.file({ truncated: true, content: 'x', size: 2_000_000 }, 'big.log')).toEqual({ path: 'big.log', text: '', binary: false, truncated: true, size: 2_000_000 })
    expect(git.file({ content: 42 }, 'n.txt')).toEqual({ path: 'n.txt', text: '', binary: false, truncated: false, size: 0 })
    expect(git.file(null, 'gone.txt').path).toBe('gone.txt')
  })
})

describe('mcp', () => {
  it('reads a server with no schema, no operations or no description, and skips a row it cannot name', async () => {
    const seen = answer(200, {
      jsonrpc: '2.0',
      id: 1,
      result: {
        tools: [
          null,
          { description: 'nameless' },
          { name: 7 },
          { name: 'ping' },
          { name: 'kms', description: 5, inputSchema: { type: 'object' } },
          { name: 'git', inputSchema: { properties: { path: { type: 'string' } } } },
          { name: 'code', inputSchema: { properties: { op: { enum: 'ask' } } } },
          { name: 's3', inputSchema: { properties: { op: { enum: ['get', 3, ''] } } } },
        ],
      },
    })
    expect(await natives(T)).toEqual([
      { name: 'ping', description: '', ops: [] },
      { name: 'kms', description: '', ops: [] },
      { name: 'git', description: '', ops: [] },
      { name: 'code', description: '', ops: [] },
      { name: 's3', description: '', ops: ['get'] },
    ])
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/mcp', body: { jsonrpc: '2.0', id: 1, method: 'tools/list' } })
  })

  it('refuses an error with no sentence, an empty body, and a result that is not a list', () => {
    expect(() => nativesOf({ jsonrpc: '2.0', id: 1, error: { code: -32601 } })).toThrow('The MCP server refused the list')
    expect(() => nativesOf({ jsonrpc: '2.0', id: 1, error: { code: -32601, message: '' } })).toThrow('The MCP server refused the list')
    expect(() => nativesOf(null)).toThrow('The MCP server did not list its tools')
    expect(() => nativesOf({ result: 'ok' })).toThrow('The MCP server did not list its tools')
    expect(() => nativesOf({ error: 'bad request', result: { tools: 'none' } })).toThrow('The MCP server did not list its tools')
  })
})

describe('models', () => {
  it('reads every row the gateway lists, with the fields it publishes, and nothing it cannot read', async () => {
    answer(200, {
      data: [
        null,
        { id: 42 },
        'zen5.8',
        { id: 'enso-auto', owned_by: 'hanzo', family: 'enso', class: 'ours' },
        { id: 'anthropic/claude-sonnet-4.5', owned_by: 'anthropic', class: 'premium', name: 'Claude Sonnet 4.5' },
      ],
    })
    expect(await models(T)).toEqual([
      { id: 'enso-auto', owned_by: 'hanzo', family: 'enso', class: 'ours' },
      { id: 'anthropic/claude-sonnet-4.5', owned_by: 'anthropic', class: 'premium', name: 'Claude Sonnet 4.5' },
    ])
  })

  it('answers no models when the catalog lists none it can read', async () => {
    answer(200, { data: 'zen5.8' })
    expect(await models(T)).toEqual([])
    answer(204, undefined)
    expect(await models(T)).toEqual([])
  })
})

describe('places', () => {
  it('takes a run only while the place is online', async () => {
    answer(200, { targets: [{ id: 'tgt_1', label: 'dgx', status: 'draining' }] })
    const [sandbox, dgx] = await places(T)
    expect(ready(sandbox!)).toBe(true)
    expect(sandbox).toBe(SANDBOX)
    expect(ready(dgx!)).toBe(false)
  })

  it('takes no run on a machine nothing has reached yet, however it was registered', async () => {
    answer(200, { targets: [{ id: 'tgt_1', label: 'workshop', status: 'online' }, { id: 'tgt_2', label: 'dgx', status: 'online', metricsAt: '2026-10-04T06:00:00Z' }] })
    const [, workshop, dgx] = await places(T)
    expect(workshop).toMatchObject({ status: 'not seen yet' })
    expect(ready(workshop!)).toBe(false)
    expect(ready(dgx!)).toBe(true)
  })
})

describe('platform', () => {
  it('releases a built image by its tag, and reads a declaration answered without a build', async () => {
    const seen = answer(202, { app: { name: 'site' }, declaration: { mode: 'commit', ref: 'main', live: true } })
    const out = await platform.declare(T, { repo: 'https://github.com/acme/site.git', ref: 'main', name: 'site', project: 'shop', mode: 'commit', tag: 'bld_1' })
    expect(seen[0].body).toEqual({ repo: 'https://github.com/acme/site.git', ref: 'main', name: 'site', partOf: 'shop', mode: 'commit', tag: 'bld_1' })
    expect(out).toEqual({ build: null, mode: 'commit', ref: 'main', review: '', live: true, hosts: [], notice: '' })
  })

  it('reads a repository in the org’s own workspace as its project’s site: live, with the notice and no review', async () => {
    const notice = "Building site from main in a sandbox; it goes live at the project's address when the build finishes."
    answer(202, {
      app: { name: 'site', org: 'acme', partOf: 'site', hosts: [] },
      build: { id: 'dep_1', status: 'building', repo: 'https://git.hanzo.ai/acme-ws/site.git', ref: 'main' },
      declaration: { declaration: {}, mode: 'commit', ref: 'main', created: false, changed: false, live: true },
      notice,
    })
    expect(await platform.declare(T, { repo: 'https://git.hanzo.ai/acme-ws/site.git', ref: 'main', name: 'site', project: 'site', mode: 'branch' })).toEqual({
      build: { id: 'dep_1', job: '', image: '', status: 'building' },
      mode: 'commit',
      ref: 'main',
      review: '',
      live: true,
      hosts: [],
      notice,
    })
  })

  it('reads an empty answer as a declaration of nothing, and a notice that is not a sentence as none', async () => {
    answer(202, undefined)
    expect(await platform.declare(T, { repo: 'r', ref: 'main', name: 'a', project: 'a', mode: 'branch' })).toEqual({ build: null, mode: '', ref: '', review: '', live: false, hosts: [], notice: '' })
    answer(202, { declaration: { live: 'yes' }, notice: 7 })
    expect(await platform.declare(T, { repo: 'r', ref: 'main', name: 'a', project: 'a', mode: 'branch' })).toMatchObject({ live: false, notice: '' })
  })

  it('lists builds, a thin row as empty values, and an answer without builds as none', async () => {
    const seen = answer(200, { builds: [{ id: 'bld_1', repo: 'acme/site', commit: 'abc123', status: 'succeeded', startedAt: '2026-09-20T10:00:00Z', duration: '1m2s' }, null] })
    expect(await platform.builds(T)).toEqual([
      { id: 'bld_1', repo: 'acme/site', commit: 'abc123', status: 'succeeded', startedAt: '2026-09-20T10:00:00Z', duration: '1m2s' },
      { id: '', repo: '', commit: '', status: '', startedAt: '', duration: '' },
    ])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/platform/builds')
    answer(200, { builds: 'none' })
    expect(await platform.builds(T)).toEqual([])
  })
})

describe('plugins', () => {
  it('drops rows it cannot name, and reads thin ones as empty values', async () => {
    answer(200, { plugins: [null, { name: 'acme' }, { id: 'p1', name: 'acme', createdAt: '9' }] })
    expect(await plugins.authored(T)).toEqual([{ id: 'p1', name: 'acme', provider: '', source: '', built: 0 }])
    answer(200, { plugins: [{ enabled: true }, { name: 'tools', enabled: 'yes', prefixes: '/v1/tool' }] })
    expect(await plugins.mounted(T)).toEqual([{ name: 'tools', enabled: false, prefixes: [] }])
    answer(200, {})
    expect(await plugins.mounted(T)).toEqual([])
  })

  it('reads a build answered with no plugin as an unnamed one', async () => {
    answer(201, { bytes: '2048' })
    expect(await plugins.build(T, { name: 'acme', provider: ' ', source: 'export {}', spec: '' })).toEqual({ plugin: { id: '', name: '', provider: '', source: '', built: 0 }, bytes: 0, generated: false })
  })
})

describe('projects', () => {
  it('frames http on 127.0.0.1 for a local builder, as on localhost', () => {
    expect(projects.safe('http://127.0.0.1:5173/', true)).toBe('http://127.0.0.1:5173/')
    expect(projects.safe('ftp://127.0.0.1/', true)).toBe('')
  })

  it('reads a thin project, and one whose repository is not a record', () => {
    expect(projects.project({ slug: 'shop', repo: 'https://github.com/acme/shop.git', visibility: 'team', createdAt: '1' })).toEqual({
      slug: 'shop',
      name: 'shop',
      repo: '',
      branch: '',
      status: '',
      live: '',
      visibility: '',
      forked: '',
      created: 0,
      updated: 0,
    })
    expect(projects.project(null).slug).toBe('')
    expect(projects.project({ slug: 'synapse', forkedFrom: 'synapse' }).forked).toBe('synapse')
  })

  it('lists a page of projects, and anything else as none', async () => {
    answer(200, { data: [{ slug: 'shop', updatedAt: 2 }, { slug: 'blog', updatedAt: 5 }] })
    expect((await projects.projects(T)).map((p) => p.slug)).toEqual(['blog', 'shop'])
    answer(200, { data: {} })
    expect(await projects.projects(T)).toEqual([])
  })

  it('renames and changes visibility in one call, and deletes by an escaped slug', async () => {
    let seen = answer(200, { slug: 'shop', name: 'Store', visibility: 'private' })
    expect((await projects.change(T, 'shop', { name: ' Store ', visibility: 'private' })).visibility).toBe('private')
    expect(seen[0].body).toEqual({ name: 'Store', visibility: 'private' })
    seen = answer(204, undefined)
    await projects.remove(T, 'my shop')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/project/my%20shop' })
  })

  it('reads the catalog’s thin rows, and an answer with no rows as none', async () => {
    answer(200, { data: [null, { slug: 'shop' }, { slug: 'mint', demo: 'http://mint.example' }, { slug: 'synapse', demo: 'https://synapse.hanzo.app' }] })
    // A demo the preview may not frame is no demo.
    expect(await projects.templates(T)).toEqual([
      { slug: 'shop', title: 'shop', category: '', description: '', framework: '', source: '', demo: '' },
      { slug: 'mint', title: 'mint', category: '', description: '', framework: '', source: '', demo: '' },
      { slug: 'synapse', title: 'synapse', category: '', description: '', framework: '', source: '', demo: 'https://synapse.hanzo.app/' },
    ])
    answer(200, { data: 'shop' })
    expect(await projects.templates(T)).toEqual([])
    answer(204, undefined)
    expect(await projects.templates(T)).toEqual([])
  })

  it('names no address for a clone URL with one segment', () => {
    expect(projects.address('cloud')).toBe('')
    expect(projects.ours('acme/cloud', '')).toBe(true)
  })

  it('takes a starter as a copy that answers building until it is published', async () => {
    const seen = answer(201, {
      id: 'prj_1',
      org: 'acme',
      slug: 'synapse-2',
      name: 'Synapse',
      repo: { url: 'https://git.hanzo.ai/acme-ws/synapse-2.git', branch: 'main' },
      framework: 'next',
      status: 'building',
      analytics: true,
      forkedFrom: 'synapse',
      createdAt: 5,
      updatedAt: 6,
    })
    expect(await projects.fork(T, 'synapse')).toEqual({
      slug: 'synapse-2',
      name: 'Synapse',
      repo: 'https://git.hanzo.ai/acme-ws/synapse-2.git',
      branch: 'main',
      status: 'building',
      live: '',
      visibility: '',
      forked: 'synapse',
      created: 5,
      updated: 6,
    })
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/project/fork', body: { slug: 'synapse' } })
  })

  it('reads a project’s deployments newest first, a thin one as empty values, one with no id as none', async () => {
    const seen = answer(200, [
      { id: 'dep_1', projectId: 'prj_1', version: 1, status: 'live', source: 'build', commit: 'abc123', liveUrl: 'https://shop.hanzo.app', files: 3, bytes: 900, message: '', createdAt: 10, updatedAt: 20 },
      { id: 'dep_2', projectId: 'prj_1', version: 2, status: 'error', source: 'build', files: 0, bytes: 0, message: 'coding: the build exited 1: missing script: build', createdAt: 30, updatedAt: 40 },
      { id: 'dep_3', version: '3', status: 7 },
      { version: 4, status: 'building' },
      null,
    ])
    expect(await projects.deployments(T, 'my shop')).toEqual([
      { id: 'dep_2', version: 2, status: 'error', commit: '', message: 'coding: the build exited 1: missing script: build', created: 30, updated: 40 },
      { id: 'dep_1', version: 1, status: 'live', commit: 'abc123', message: '', created: 10, updated: 20 },
      { id: 'dep_3', version: 0, status: '', commit: '', message: '', created: 0, updated: 0 },
    ])
    expect(seen[0]).toMatchObject({ method: 'GET', url: 'https://api.hanzo.ai/v1/project/my%20shop/deployments' })
    answer(200, { data: [{ id: 'dep_1' }] })
    expect(await projects.deployments(T, 'shop')).toEqual([])
    answer(404, { status: 404, detail: 'project not found' })
    await expect(projects.deployments(T, 'gone')).rejects.toMatchObject({ status: 404, message: 'project not found' })
  })
})

describe('skills', () => {
  it('reads a skill with no id by its name, and thin rows as empty values', async () => {
    expect(skills.skill({ name: 'triage', createdAt: '1790000000', admitted: 'no' })).toEqual({ id: 'triage', name: 'triage', description: '', content: '', created: 0, source: '', admitted: true })
    answer(200, { skills: [null, { id: 'x' }] })
    expect(await skills.authored(T)).toEqual([])
    expect(skills.catalogue({ skills: 'git_repos', products: [null, { name: 'git', skill_count: '2' }] })).toEqual({ skills: [], products: [{ name: 'git', count: 0 }] })
    expect(skills.catalogue(null)).toEqual({ skills: [], products: [] })
  })

  it('refuses a SKILL.md past 256 KB before sending', async () => {
    const seen = answer(201, {})
    await expect(skills.write(T, { name: 'big', description: '', content: 'x'.repeat(skills.MAX + 1) })).rejects.toThrow('A SKILL.md is at most 256 KB')
    expect(seen).toEqual([])
  })

  it('says the catalogue’s status when its refusal says nothing', async () => {
    answer(502, '<html>Bad gateway</html>', 'text/html')
    await expect(skills.brand(T)).rejects.toMatchObject({ status: 502, message: 'The catalogue answered 502' })
  })
})

describe('tools', () => {
  it('reads an answer with no tools, and names that are not strings, as none', async () => {
    answer(200, { tools: 'skill_triage' })
    expect(await tools.tools(T)).toEqual([])
    answer(200, null)
    expect(await tools.enabled(T)).toEqual([])
    expect(tools.names(['skill_triage', 3, '', null])).toEqual(['skill_triage'])
    expect(tools.names('skill_triage')).toEqual([])
    expect(tools.tool(null)).toEqual({ name: '', source: '', description: '', activated: false })
  })

  it('switches names on, turning none off, and reads every name that is on afterwards', async () => {
    const seen = answer(200, { enabled: ['skill_triage'] })
    expect(await tools.toggle(T, ['skill_triage'])).toEqual(['skill_triage'])
    expect(seen[0].body).toEqual({ activate: ['skill_triage'], deactivate: [] })
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool/activation')
    const plain = answer(200, { tools: [] })
    await tools.tools(T, { activated: false })
    expect(plain[0].url).toBe('https://api.hanzo.ai/v1/tool')
  })
})
