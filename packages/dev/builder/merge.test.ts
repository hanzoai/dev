/**
 * What is the same project and the same issue, which project an issue is for,
 * and what comes first: one row per repository whichever source named it, one
 * row per issue however many boards carried it, the issue's own repository and
 * never its board's key, and the most recent first.
 */
import { describe, expect, it } from 'vitest'

import { addressOf, ask, assemble, attach, byActivity, canon, closes, handle, home, facets, issues, keyOf, owners, parse, pickProjects, place, type Granted, type Held, type Issue, type Linked, type Project } from './merge.ts'

const P = (o: Partial<Project> & { owner: string; name: string }): Project => ({
  key: keyOf(o.owner, o.name),
  description: '',
  private: null,
  branch: '',
  activity: '',
  issues: null,
  pulls: null,
  linked: false,
  mirrored: false,
  forge: '',
  synced: '',
  status: '',
  slug: '',
  codebase: '',
  url: '',
  ...o,
})

const I = (o: Partial<Issue> & { key: string }): Issue => ({
  repo: '',
  forge: '',
  hint: '',
  number: 0,
  title: '',
  body: '',
  state: 'open',
  labels: [],
  assignees: [],
  url: '',
  sources: ['forge'],
  updated: '',
  created: '',
  kind: 'issue',
  status: 'todo',
  priority: 'none',
  pull: false,
  board: '',
  ...o,
})

const G = (o: Partial<Granted> & { owner: string; name: string }): Granted => ({ private: false, default_branch: 'main', pushed_at: '', codebase: `${o.owner}_${o.name}`.toLowerCase(), ...o })
const H = (o: Partial<Held> & { name: string }): Held => ({ org: 'acme', description: '', branch: 'main', public: false, updated: '', ...o })
const L = (o: Partial<Linked> & { owner: string; name: string }): Linked => ({ forge: o.name, synced: '', status: 'synced', branch: 'main', ...o })

describe('assembling the projects', () => {
  it('is one row for a GitHub repository and the forge mirror GitHub names as its codebase', () => {
    const [one, ...rest] = assemble({ granted: [G({ owner: 'Hanzoai', name: 'Build', pushed_at: '2026-10-01T00:00:00Z', private: true })], held: [H({ name: 'hanzoai_build', description: 'The builder', updated: '2026-10-03T00:00:00Z' })] })
    expect(rest).toEqual([])
    expect(one).toEqual({
      key: 'hanzoai/build',
      owner: 'Hanzoai',
      name: 'Build',
      description: 'The builder',
      private: true,
      branch: 'main',
      activity: '2026-10-03T00:00:00Z',
      issues: null,
      pulls: null,
      linked: true,
      mirrored: true,
      forge: 'hanzoai_build',
      synced: '',
      status: '',
      slug: '',
      codebase: 'hanzoai_build',
      url: 'https://github.com/Hanzoai/Build',
    })
  })

  it('takes every repository the org linked, its forge copy and when it last synced, with no GitHub of its own', () => {
    const list = assemble({
      linked: [L({ owner: 'hanzoai', name: 'cloud', synced: '2026-10-06T00:00:00Z' }), L({ owner: 'hanzoai', name: 'fresh', status: 'pending' }), L({ owner: 'luxfi', name: 'node', status: 'conflict', forge: 'node' })],
      sites: [{ slug: 'hanzoai-cloud', repo: 'https://github.com/hanzoai/cloud.git', branch: 'main', updated: '2026-10-01T00:00:00Z' }, { slug: 'shop', repo: 'https://git.hanzo.ai/acme/shop.git', branch: 'main', updated: '' }, { slug: 'nothing', repo: 'not a url', branch: '', updated: '' }],
    })
    expect(list.map((p) => [p.key, p.linked, p.mirrored, p.forge, p.status, p.slug])).toEqual([
      ['hanzoai/cloud', true, true, 'cloud', 'synced', 'hanzoai-cloud'],
      ['hanzoai/fresh', true, false, '', 'pending', ''],
      ['luxfi/node', true, true, 'node', 'conflict', ''],
      ['acme/shop', false, true, 'shop', '', 'shop'],
    ])
    expect(list[0]).toMatchObject({ synced: '2026-10-06T00:00:00Z', activity: '2026-10-06T00:00:00Z', url: 'https://github.com/hanzoai/cloud' })
  })

  it('reads an https clone address as its account and repository', () => {
    expect(addressOf('https://github.com/hanzoai/cloud.git')).toEqual({ owner: 'hanzoai', name: 'cloud', github: true })
    expect(addressOf('https://api.hanzo.ai/v1/git/acme/shop.git')).toEqual({ owner: 'acme', name: 'shop', github: false })
    expect(addressOf('https://github.com/hanzoai/cloud/tree/main')).toBeNull()
    expect(addressOf('http://github.com/a/b')).toBeNull()
    expect(addressOf('https://github.com/solo')).toBeNull()
  })

  it('keeps a repository made on the forge as its own, under the org', () => {
    expect(assemble({ held: [H({ name: 'notes', public: true })] })).toEqual([P({ owner: 'acme', name: 'notes', private: false, branch: 'main', mirrored: true, forge: 'notes' })])
  })

  it('finds a forge repository by the namespace its issues link from, as the estate mirrors GitHub under the same owner', () => {
    const work = [I({ key: 'f', forge: 'cloud', url: 'https://git.hanzo.ai/hanzoai/cloud/issues/3', updated: '2026-10-05T00:00:00Z' })]
    const list = assemble({ granted: [G({ owner: 'hanzoai', name: 'cloud' })], held: [H({ org: 'hanzo', name: 'cloud' })], work })
    expect(list).toHaveLength(1)
    expect(list[0]).toMatchObject({ key: 'hanzoai/cloud', linked: true, mirrored: true, forge: 'cloud', issues: 1, pulls: 0, activity: '2026-10-05T00:00:00Z' })
  })

  it('adds a forge repository only its issues name, and counts each project’s open issues and pull requests', () => {
    const work = [
      I({ key: '1', forge: 'infra', url: 'https://git.hanzo.ai/hanzoai/infra/issues/1' }),
      I({ key: '2', forge: 'infra', url: 'https://git.hanzo.ai/hanzoai/infra/pulls/2', pull: true }),
      I({ key: '3', forge: 'infra', url: 'https://git.hanzo.ai/hanzoai/infra/issues/3', state: 'closed' }),
      I({ key: '4', repo: 'hanzoai/build', sources: ['github'] }),
      I({ key: '5', hint: 'HCC', sources: ['linear'] }),
    ]
    const list = assemble({ granted: [G({ owner: 'hanzoai', name: 'build' })], work })
    expect(list.map((p) => [p.key, p.issues, p.pulls, p.forge])).toEqual([
      ['hanzoai/build', 1, 0, ''],
      ['hanzoai/infra', 1, 1, 'infra'],
    ])
  })

  it('is one row for a forge repository under the same owner and name as a GitHub one, never one replacing the other', () => {
    const [one, ...rest] = assemble({ granted: [G({ owner: 'acme', name: 'notes', pushed_at: '2026-10-01T00:00:00Z' })], held: [H({ org: 'acme', name: 'notes', description: 'Notes' })] })
    expect(rest).toEqual([])
    expect(one).toMatchObject({ key: 'acme/notes', linked: true, mirrored: true, forge: 'notes', description: 'Notes', activity: '2026-10-01T00:00:00Z' })
  })

  it('takes a linked repository’s visibility from GitHub alone, never from its forge copy', () => {
    const copy = [H({ org: 'hanzo', name: 'cloud', public: true })]
    // Linked, and GitHub's listing did not answer for it (an operator with no GitHub of their own): unknown, not Public.
    expect(assemble({ linked: [L({ owner: 'hanzoai', name: 'cloud', forge: 'cloud' })], held: copy })[0]).toMatchObject({ key: 'hanzoai/cloud', private: null })
    // GitHub says private: private, whatever the copy is.
    expect(assemble({ linked: [L({ owner: 'hanzoai', name: 'cloud', forge: 'cloud' })], granted: [G({ owner: 'hanzoai', name: 'cloud', private: true })], held: copy })[0]).toMatchObject({ private: true })
    // Made on the forge: the forge's own.
    expect(assemble({ held: [H({ name: 'notes', public: true })] })[0]).toMatchObject({ private: false })
  })

  it('counts nothing when the work was not read, rather than zero', () => {
    expect(assemble({ granted: [G({ owner: 'a', name: 'b' })] })[0]).toMatchObject({ issues: null, pulls: null })
    expect(assemble({ granted: [G({ owner: 'a', name: 'b' })], work: [] })[0]).toMatchObject({ issues: 0, pulls: 0 })
  })
})

describe('which project an issue is for', () => {
  const LIST = [
    P({ owner: 'hanzoai', name: 'build', linked: true, codebase: 'hanzoai_build', forge: 'hanzoai_build', mirrored: true }),
    P({ owner: 'hanzoai', name: 'cloud', linked: true, codebase: 'hanzoai_cloud' }),
    P({ owner: 'luxfi', name: 'cloud', linked: true, codebase: 'luxfi_cloud' }),
    P({ owner: 'acme', name: 'notes', forge: 'notes', mirrored: true }),
  ]

  it('is its GitHub address, its forge repository, or the one project a run’s repository names', () => {
    expect(place({ repo: 'HanzoAI/Build', forge: '', hint: '' }, LIST)?.key).toBe('hanzoai/build')
    expect(place({ repo: '', forge: 'hanzoai_build', hint: '' }, LIST)?.key).toBe('hanzoai/build')
    expect(place({ repo: '', forge: 'notes', hint: '' }, LIST)?.key).toBe('acme/notes')
    expect(place({ repo: '', forge: '', hint: 'github.com/hanzoai/cloud.git' }, LIST)?.key).toBe('hanzoai/cloud')
    expect(place({ repo: '', forge: '', hint: 'build' }, LIST)?.key).toBe('hanzoai/build')
  })

  it('is none for a board’s or a Linear team’s key, a name two projects share, or a repository the org does not have', () => {
    expect(place({ repo: '', forge: '', hint: 'HCC' }, LIST)).toBeNull()
    expect(place({ repo: '', forge: '', hint: 'cloud' }, LIST)).toBeNull()
    expect(place({ repo: 'hanzoai/elsewhere', forge: '', hint: '' }, LIST)).toBeNull()
    expect(place({ repo: '', forge: '', hint: '' }, LIST)).toBeNull()
  })

  it('names each issue’s project as its repository where one answers', () => {
    const got = attach([I({ key: 'a', forge: 'notes' }), I({ key: 'b', hint: 'HCC' })], LIST)
    expect(got.map((i) => i.repo)).toEqual(['acme/notes', ''])
  })
})

describe('listing the projects', () => {
  it('sorts by recent activity, the unknown last, the address breaking ties', () => {
    const list = [P({ owner: 'a', name: 'old', activity: '2026-01-01T00:00:00Z' }), P({ owner: 'a', name: 'never' }), P({ owner: 'a', name: 'new', activity: '2026-10-01T00:00:00Z' }), P({ owner: 'a', name: 'also' })]
    expect(list.sort(byActivity).map((p) => p.name)).toEqual(['new', 'old', 'also', 'never'])
  })

  it('filters by owner, by open work and by every word of the search', () => {
    const list = [
      P({ owner: 'hanzoai', name: 'build', description: 'The builder UI', issues: 2, activity: '2026-10-02T00:00:00Z' }),
      P({ owner: 'hanzoai', name: 'docs', issues: 0, pulls: 0, activity: '2026-10-03T00:00:00Z' }),
      P({ owner: 'luxfi', name: 'node', pulls: 1, activity: '2026-10-01T00:00:00Z' }),
    ]
    expect(pickProjects(list).map((p) => p.name)).toEqual(['docs', 'build', 'node'])
    expect(pickProjects(list, { owner: 'HANZOAI' }).map((p) => p.name)).toEqual(['docs', 'build'])
    expect(pickProjects(list, { open: true }).map((p) => p.name)).toEqual(['build', 'node'])
    expect(pickProjects(list, { q: 'builder hanzo' }).map((p) => p.name)).toEqual(['build'])
    expect(pickProjects(list, { q: 'luxfi/node' }).map((p) => p.name)).toEqual(['node'])
    expect(owners(['luxfi', 'hanzoai'], ['HanzoAI', ''])).toEqual(['hanzoai', 'luxfi'])
  })
})

describe('an issue’s link', () => {
  it('is one key however it was written', () => {
    expect(canon('https://GitHub.com/HanzoAI/build/issues/12/?x=1#c')).toBe('https://github.com/hanzoai/build/issues/12')
    expect(canon('http://github.com/a/b/issues/1')).toBe('')
    expect(canon('javascript:alert(1)')).toBe('')
    expect(canon('')).toBe('')
  })

  it('names its repository and number', () => {
    expect(parse('https://github.com/hanzoai/build/issues/12')).toEqual({ repo: 'hanzoai/build', number: 12, pull: false })
    expect(parse('https://git.hanzo.ai/hanzoai/build/pulls/3')).toEqual({ repo: 'hanzoai/build', number: 3, pull: true })
    expect(parse('https://github.com/hanzoai/build')).toBeNull()
  })
})

describe('merging issues', () => {
  const KEY = 'https://github.com/hanzoai/build/issues/12'

  it('is one row for the same issue read twice, naming both sources, its labels and assignees together', () => {
    const forge = I({ key: KEY, repo: 'hanzoai/build', number: 12, title: 'Pin the image', state: 'open', labels: ['bug'], assignees: ['dave'], updated: '2026-10-01T00:00:00Z', sources: ['forge'], board: 'build' })
    const github = I({ key: KEY, repo: 'hanzoai/build', number: 12, title: 'Pin the gateway image', body: 'Floating tags never reach a cluster.', state: 'closed', labels: ['Bug', 'infra'], assignees: ['erin'], url: KEY, updated: '2026-10-04T00:00:00Z', sources: ['github'], board: 'GH' })
    expect(issues([forge], [github])).toEqual([
      {
        key: KEY,
        repo: 'hanzoai/build',
        forge: '',
        hint: '',
        number: 12,
        title: 'Pin the gateway image',
        body: 'Floating tags never reach a cluster.',
        state: 'closed',
        labels: ['Bug', 'infra'],
        assignees: ['erin', 'dave'],
        url: KEY,
        sources: ['forge', 'github'],
        updated: '2026-10-04T00:00:00Z',
        created: '',
        kind: 'issue',
        status: 'todo',
        priority: 'none',
        pull: false,
        board: 'GH',
      },
    ])
  })

  it('keeps what the older source alone says, and drops a row with no key', () => {
    const [one, ...rest] = issues([I({ key: 'a', board: 'ENG', body: 'Steps', updated: '2026-01-01T00:00:00Z' }), I({ key: '' })], [I({ key: 'a', title: 'Later', updated: '2026-02-01T00:00:00Z', sources: ['github', 'github'] })])
    expect(rest).toEqual([])
    expect(one).toMatchObject({ title: 'Later', body: 'Steps', board: 'ENG', sources: ['forge', 'github'] })
  })

  const LIST = [
    I({ key: '1', repo: 'hanzoai/build', number: 1, title: 'Crash on load', labels: ['bug'], assignees: ['dave'], updated: '2026-10-01T00:00:00Z' }),
    I({ key: '2', repo: 'hanzoai/build', number: 2, title: 'Dark mode', labels: ['feature'], state: 'closed', updated: '2026-10-03T00:00:00Z' }),
    I({ key: '3', repo: 'luxfi/node', number: 3, title: 'Faster sync', labels: ['Bug'], assignees: ['erin'], updated: '2026-10-02T00:00:00Z' }),
    I({ key: '4', title: 'No date', board: 'ENG' }),
  ]

  it('lists every label and assignee once, whatever the case', () => {
    expect(facets(LIST)).toEqual({ labels: ['bug', 'feature'], assignees: ['dave', 'erin'] })
  })
})

describe('where a run on an issue works, and what it closes', () => {
  const linked = { linked: true, forge: 'site' }
  const made = { linked: false, forge: 'notes' }
  const granted = { linked: true, forge: '' }

  it('works where the issue lives: a GitHub issue on GitHub, a forge issue on the forge', () => {
    expect(home({ sources: ['github'] }, linked)).toBe('github')
    expect(home({ sources: ['forge'] }, linked)).toBe('forge')
    expect(home({ sources: ['forge'] }, made)).toBe('forge')
    expect(home({ sources: ['linear'] }, linked)).toBe('forge')
    expect(home({ sources: ['board'] }, granted)).toBe('github')
    expect(home({ sources: ['agent'] }, { linked: false, forge: '' })).toBeNull()
  })

  it('closes the issue’s own number only where it lives; a board’s or Linear’s number closes nothing', () => {
    expect(closes({ sources: ['github'], number: 77 }, linked)).toBe(77)
    expect(closes({ sources: ['forge', 'github'], number: 77 }, granted)).toBe(77)
    expect(closes({ sources: ['forge'], number: 3 }, made)).toBe(3)
    expect(closes({ sources: ['forge'], number: 3 }, linked)).toBe(3)
    expect(closes({ sources: ['forge'], number: 3 }, granted)).toBe(0)
    expect(closes({ sources: ['linear'], number: 5 }, linked)).toBe(0)
    expect(closes({ sources: ['board'], number: 5 }, made)).toBe(0)
    expect(closes({ sources: ['github'], number: 0 }, linked)).toBe(0)
  })

  it('names an issue by its repository’s number, or by its board’s handle', () => {
    expect(handle({ sources: ['github'], number: 77, board: 'GH' })).toBe('#77')
    expect(handle({ sources: ['linear'], number: 5, board: 'LINEAR' })).toBe('LINEAR-5')
    expect(handle({ sources: ['board'], number: 0, board: 'ENG' })).toBe('')
  })
})

describe('the ask an issue hands New', () => {
  it('is its title and the number the run closes, its link and its body', () => {
    expect(ask({ title: 'Pin the gateway image', number: 12, url: 'https://github.com/a/b/issues/12', body: '  The tag floats.\n', board: 'GH', sources: ['github'] }, 12)).toBe(
      'Pin the gateway image (#12)\nhttps://github.com/a/b/issues/12\n\nThe tag floats.',
    )
  })

  it('names a number the run does not close by its board’s handle, never as #N', () => {
    expect(ask({ title: 'Console billing page', number: 5, url: '', body: '', board: 'LINEAR', sources: ['linear'] })).toBe('Console billing page (LINEAR-5)')
    expect(ask({ title: 'Tidy the footer', number: 0, url: '', body: '', board: '', sources: ['board'] })).toBe('Tidy the footer')
  })
})
