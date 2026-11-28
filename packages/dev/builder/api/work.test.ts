/**
 * The work as the platform answers it, read for what each row is: a forge
 * issue names its forge repository and links to its page; a GitHub issue the
 * task index mirrors names its address only in `extRef`, under the board's own
 * number; a Linear row's `repo` is a team's key and stays a hint. The project
 * sources are read one by one, so a source that fails leaves the others.
 *
 * Rows are shaped like the real ones: apps/task issueView (source.go
 * forgeIssue, indexIssue) and apps/provider github_index repoView.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import type { Target } from './call.ts'
import { board, boards, everything, sources, work } from './work.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'hanzo' }

/** The platform, answering each path from `paths`; anything else is a 404. */
function platform(paths: Record<string, { status?: number; body: unknown }>) {
  const urls: string[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string) => {
      urls.push(url)
      const at = new URL(url)
      const hit = paths[at.pathname]
      if (!hit) return new Response(JSON.stringify({ detail: 'not found' }), { status: 404 })
      return new Response(JSON.stringify(hit.body), { status: hit.status ?? 200, headers: { 'content-type': 'application/json' } })
    }),
  )
  return urls
}

afterEach(() => vi.unstubAllGlobals())

/** A forge issue, as forgeIssue renders one. */
export const FORGE = {
  id: '9001',
  identifier: 'build#3',
  projectKey: 'build',
  number: 3,
  kind: 'issue',
  source: 'git',
  repo: 'build',
  title: 'Projects lists boards',
  description: 'It should list repositories.',
  status: 'todo',
  priority: 'none',
  assignee: 'zeekay',
  labels: ['bug', 'ui'],
  createdAt: 1790000000,
  updatedAt: 1790500000,
  url: 'https://git.hanzo.ai/hanzoai/build/issues/3',
}

/** A GitHub issue the index mirrors, as indexIssue renders one: GitHub's address only in extRef. */
export const MIRRORED = {
  id: 'issue_7',
  identifier: 'GH#41',
  projectKey: 'GH',
  number: 41,
  kind: 'issue',
  source: 'git',
  repo: 'cloud',
  extRef: 'github:hanzoai/cloud#81',
  title: 'Pin the gateway image',
  description: 'Floating tags never reach a cluster.',
  status: 'done',
  priority: 'none',
  assignee: 'dave',
  labels: ['infra'],
  createdAt: 1790000000,
  updatedAt: 1790600000,
}

/** A Linear issue the index mirrors: its `repo` is the team's key, which is not a repository. */
export const LINEAR = {
  id: 'issue_8',
  identifier: 'LINEAR#5',
  projectKey: 'LINEAR',
  number: 5,
  kind: 'issue',
  source: 'git',
  repo: 'HCC',
  extRef: 'linear:HCC-12',
  title: 'Console billing page',
  status: 'in_progress',
  priority: 'none',
  labels: [],
  createdAt: 1790000000,
  updatedAt: 1790700000,
}

describe('boards', () => {
  it('reads a bare list, names a board by its key when it has no name, and drops one with no key', async () => {
    const urls = platform({ '/v1/task/projects': { body: [{ key: 'WEB' }, { key: 'HCC', id: 'prj_1', name: 'Hanzo Cloud/Console Program', description: 'Mirrored external issues.' }, { name: 'no key' }, null] } })
    expect(await boards(T)).toEqual([
      { id: 'WEB', key: 'WEB', name: 'WEB', description: '' },
      { id: 'prj_1', key: 'HCC', name: 'Hanzo Cloud/Console Program', description: 'Mirrored external issues.' },
    ])
    expect(urls).toEqual(['https://api.hanzo.ai/v1/task/projects'])
    expect(board('x')).toBeNull()
  })
})

describe('a board row', () => {
  it('from the forge names its forge repository and links to its page', () => {
    expect(work(FORGE)).toEqual({
      key: 'https://git.hanzo.ai/hanzoai/build/issues/3',
      repo: '',
      forge: 'build',
      hint: '',
      number: 3,
      title: 'Projects lists boards',
      body: 'It should list repositories.',
      state: 'open',
      labels: ['bug', 'ui'],
      assignees: ['zeekay'],
      url: 'https://git.hanzo.ai/hanzoai/build/issues/3',
      sources: ['forge'],
      updated: new Date(1790500000 * 1000).toISOString(),
      created: new Date(1790000000 * 1000).toISOString(),
      kind: 'issue',
      status: 'todo',
      priority: 'none',
      pull: false,
      board: 'build',
    })
  })

  it('mirrored from GitHub is GitHub’s issue: its address, its number and its link, never the board’s', () => {
    expect(work(MIRRORED)).toMatchObject({
      key: 'https://github.com/hanzoai/cloud/issues/81',
      repo: 'hanzoai/cloud',
      forge: '',
      hint: '',
      number: 81,
      state: 'closed',
      url: 'https://github.com/hanzoai/cloud/issues/81',
      sources: ['github'],
      board: 'GH',
    })
  })

  it('from Linear names no repository at all: its `repo` is a team’s key', () => {
    expect(work(LINEAR)).toMatchObject({ repo: '', forge: '', hint: '', sources: ['linear'], board: 'LINEAR', url: '', key: 'linear#5' })
  })

  it('reads a pull request, an agent’s row, a thin row, and drops one with nothing to show', () => {
    expect(work({ ...FORGE, kind: 'pr', url: 'https://git.hanzo.ai/hanzoai/build/pulls/4', number: 4 })).toMatchObject({ pull: true, key: 'https://git.hanzo.ai/hanzoai/build/pulls/4' })
    expect(work({ projectKey: 'AGT', number: 2, title: 'Add the widget', repo: 'hanzoai/build', extRef: 'agent:sess_1', status: 'in_progress' })).toMatchObject({ hint: 'hanzoai/build', sources: ['agent'], state: 'open' })
    expect(work({ title: 'Loose', url: 'javascript:alert(1)' })).toMatchObject({ key: '', url: '', sources: ['board'], updated: '' })
    expect(work({ number: 9 })).toBeNull()
    expect(work(null)).toBeNull()
  })
})

describe('the org’s work', () => {
  it('is every row once: the same GitHub issue mirrored onto two boards is one, both boards’ labels kept', async () => {
    const urls = platform({ '/v1/task/board': { body: [FORGE, MIRRORED, { ...MIRRORED, id: 'issue_9', projectKey: 'OPS', identifier: 'OPS#2', number: 2, labels: ['ops'], updatedAt: 1790000001 }, LINEAR] } })
    const all = await everything(T)
    expect(urls).toEqual(['https://api.hanzo.ai/v1/task/board'])
    expect(all).toHaveLength(3)
    expect(all.find((i) => i.repo === 'hanzoai/cloud')).toMatchObject({ labels: ['infra', 'ops'], board: 'GH', number: 81 })
  })
})

describe('a board read before the person has a seat', () => {
  /** The platform as a made workspace answers it: the board refuses until POST /v1/git/workspace seats the caller. */
  function unseated(refusal: string) {
    const seen: string[] = []
    let open = false
    const json = (status: number, body: unknown) => new Response(JSON.stringify(body), { status, headers: { 'content-type': 'application/json' } })
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string, init: RequestInit = {}) => {
        const path = new URL(url).pathname
        seen.push(`${init.method ?? 'GET'} ${path}`)
        if (path === '/v1/git/workspace' && init.method === 'POST') {
          open = true
          return json(200, { owner: 'acme', made: true, login: 'seat-ann' })
        }
        if (!open) return json(403, { detail: refusal })
        if (path === '/v1/task/board') return json(200, [FORGE])
        if (path === '/v1/task/projects') return json(200, [{ key: 'build', name: 'Build' }])
        return json(404, { detail: 'not found' })
      }),
    )
    return seen
  }
  const SET_UP = 'open Codebase, Projects or Issues in Hanzo Dev to set it up'

  it('opens the workspace, which seats them, and reads the board again', async () => {
    const seen = unseated(`no forge identity for this principal yet; ${SET_UP}`)
    const all = await everything({ ...T, org: 'acme-seat' })
    expect(all.map((i) => i.number)).toEqual([3])
    expect(seen).toEqual(['GET /v1/task/board', 'POST /v1/git/workspace', 'GET /v1/task/board'])
  })

  it('opens it for an org with no namespace on the forge yet, too', async () => {
    const seen = unseated(`this org has no namespace on the forge yet; ${SET_UP}`)
    expect((await boards({ ...T, org: 'acme-ns' })).map((b) => b.key)).toEqual(['build'])
    expect(seen).toEqual(['GET /v1/task/projects', 'POST /v1/git/workspace', 'GET /v1/task/projects'])
  })

  it('opens it once for the reads that were refused together', async () => {
    const seen = unseated(`no forge identity for this principal yet; ${SET_UP}`)
    const t = { ...T, org: 'acme-both' }
    const [all, kept] = await Promise.all([everything(t), boards(t)])
    expect(all).toHaveLength(1)
    expect(kept).toHaveLength(1)
    expect(seen.filter((s) => s === 'POST /v1/git/workspace')).toHaveLength(1)
  })

  it('says a refusal no seat can answer as it is: an API key holds none', async () => {
    const seen = unseated('no forge identity for this principal: a seat in a code workspace is a person’s')
    await expect(everything({ ...T, org: 'acme-key' })).rejects.toMatchObject({ status: 403, message: expect.stringContaining('a seat in a code workspace is a person') })
    expect(seen).toEqual(['GET /v1/task/board'])
  })
})

describe('what projects are made from', () => {
  const HUB = { repos: [{ owner: 'hanzoai', name: 'build', full_name: 'hanzoai/build', private: false, default_branch: 'main', pushed_at: '2026-10-01T00:00:00Z', installation_id: 7, codebase: 'hanzoai_build' }], next: 'c2', total: 140, unread: ['hanzo-labs'], connected: true }
  const HELD = { data: [{ name: 'hanzoai_build', org: 'hanzo', defaultBranch: 'main', updatedAt: '2026-10-02T00:00:00Z' }] }
  /** GET /v1/sync as apps/sync answers it: a repository link, an account link, and one the forge could not be read for. */
  const SYNC = {
    data: [
      { id: 's1', kind: 'git', scope: 'repo', source: { provider: 'github', locator: 'https://github.com/hanzoai/cloud.git' }, target: { provider: 'hanzo-git', locator: 'cloud' }, direction: 'pull', trigger: 'webhook', createdAt: '2026-09-01T00:00:00Z', updatedAt: '2026-10-06T00:00:00Z', native: { url: 'https://git.hanzo.ai/hanzoai/cloud', branch: 'main', status: 'synced' } },
      { id: 's2', kind: 'git', scope: 'account', source: { provider: 'github', locator: 'https://github.com/hanzoai' }, target: { provider: 'hanzo-git', locator: '' }, direction: 'pull', trigger: 'poll', createdAt: '2026-09-01T00:00:00Z' },
      { id: 's3', kind: 'git', scope: 'repo', source: { provider: 'github', locator: 'https://github.com/luxfi/node.git' }, target: { provider: 'hanzo-git', locator: 'node' }, direction: 'pull', trigger: 'poll', createdAt: '2026-09-01T00:00:00Z' },
    ],
  }
  /** GET /v1/project: a linked repository's project, and one built from the forge. */
  const BUILT = [
    { slug: 'hanzoai-cloud', name: 'hanzoai/cloud', repo: { url: 'https://github.com/hanzoai/cloud.git', branch: 'main' }, visibility: 'public', updatedAt: 1790000000 },
    { slug: 'shop', name: 'Shop', repo: { url: 'https://git.hanzo.ai/acme/shop.git', branch: 'main' }, visibility: 'private', updatedAt: 0 },
    { slug: 'draft', name: 'Draft' },
  ]

  it('reads the links, the forge, the projects and the accounts, and asks GitHub to search and page', async () => {
    const urls = platform({
      '/v1/provider/github/repos': { body: HUB },
      '/v1/git/repos': { body: HELD },
      '/v1/sync': { body: SYNC },
      '/v1/project': { body: BUILT },
      '/v1/provider/github/installations': { body: { installations: [{ login: 'hanzoai', type: 'Organization', grant: 'all', connected: true }] } },
    })
    const got = await sources(T, { q: ' bui ', owner: 'hanzoai' })
    expect(urls).toContain('https://api.hanzo.ai/v1/provider/github/repos?q=bui&owner=hanzoai&limit=100')
    expect(got).toMatchObject({ next: 'c2', unread: ['hanzo-labs'], owners: ['hanzoai', 'luxfi'], missing: [] })
    expect(got.hub.map((r) => r.codebase)).toEqual(['hanzoai_build'])
    expect(got.linked).toEqual([
      { id: 's1', owner: 'hanzoai', name: 'cloud', forge: 'cloud', synced: '2026-10-06T00:00:00Z', status: 'synced', branch: 'main', direction: 'pull' },
      { id: 's3', owner: 'luxfi', name: 'node', forge: 'node', synced: '', status: '', branch: '', direction: 'pull' },
    ])
    expect(got.held).toEqual([{ org: 'hanzo', name: 'hanzoai_build', description: '', branch: 'main', public: false, updated: '2026-10-02T00:00:00Z' }])
    expect(got.sites).toEqual([
      { slug: 'hanzoai-cloud', repo: 'https://github.com/hanzoai/cloud.git', branch: 'main', updated: new Date(1790000000 * 1000).toISOString() },
      { slug: 'shop', repo: 'https://git.hanzo.ai/acme/shop.git', branch: 'main', updated: '' },
    ])
  })

  it('stands on the links when GitHub answers nothing for an operator with no GitHub of their own, and names what failed', async () => {
    platform({ '/v1/sync': { body: SYNC }, '/v1/provider/github/repos': { body: { repos: [], connected: false } }, '/v1/git/repos': { status: 502, body: { detail: 'upstream' } } })
    const got = await sources(T)
    expect(got.hub).toEqual([])
    expect(got.linked.map((l) => `${l.owner}/${l.name}`)).toEqual(['hanzoai/cloud', 'luxfi/node'])
    expect(got.missing).toEqual(['the forge', 'the projects'])
  })

  it('reads GitHub alone for its next page', async () => {
    const urls = platform({ '/v1/provider/github/repos': { body: { ...HUB, next: '' } } })
    const got = await sources(T, { after: 'c2' })
    expect(urls).toEqual(['https://api.hanzo.ai/v1/provider/github/repos?limit=100&after=c2'])
    expect(got).toMatchObject({ next: '', missing: [], linked: [], held: [] })
  })
})
