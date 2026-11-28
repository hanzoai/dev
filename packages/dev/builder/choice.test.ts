import { afterEach, describe, expect, it, vi } from 'vitest'

import type { Codebase } from './api/codebases.ts'
import { asHub, boardKey, gone, isForge, isHub, pinBoard, pinCodebase, pinHub, pinRepo, readPending, settle, writePending, type Offered } from './choice.ts'

/** Web Storage as a browser has it. */
function area(seed: Record<string, string> = {}) {
  const m = new Map(Object.entries(seed))
  return {
    getItem: (k: string) => m.get(k) ?? null,
    setItem: (k: string, v: string) => void m.set(k, v),
    removeItem: (k: string) => void m.delete(k),
    has: (k: string) => m.has(k),
  }
}

const refuse = () => {
  throw new DOMException('The operation is insecure.', 'SecurityError')
}

/** A browser with these two storage areas, which hears the events the page raises. */
function browser(local = area(), session = area()) {
  const heard: string[] = []
  vi.stubGlobal('window', { localStorage: local, sessionStorage: session, dispatchEvent: (e: Event) => heard.push(e.type) })
  return { local, session, heard }
}

/** A browser whose storage throws on every access. */
function refusing() {
  const heard: string[] = []
  const blocked = { getItem: refuse, setItem: refuse, removeItem: refuse }
  vi.stubGlobal('window', { localStorage: blocked, sessionStorage: blocked, dispatchEvent: (e: Event) => heard.push(e.type) })
  return heard
}

const WIDGET: Codebase = { org: 'acme', name: 'widget', description: '', branch: 'trunk', public: false, clone: 'https://git/acme/widget.git', updated: '', branches: [] }

afterEach(() => vi.unstubAllGlobals())

describe('pointing New at a codebase', () => {
  it('replaces the codebase, branch and draft, and keeps the mode, model and machine', () => {
    const { local } = browser(area({ 'hanzo.build.new.acme': JSON.stringify({ mode: 'plan', model: 'zen5.8', place: 'tgt_1', ask: 'old' }) }))
    pinCodebase('acme', WIDGET, 'Fix the build')
    expect(JSON.parse(local.getItem('hanzo.build.new.acme')!)).toEqual({
      mode: 'plan',
      model: 'zen5.8',
      place: 'tgt_1',
      repo: { owner: 'acme', name: 'widget', full_name: 'acme/widget', private: true, default_branch: 'trunk', pushed_at: '', installation_id: 0, forge: true, clone: 'https://git/acme/widget.git' },
      branch: 'trunk',
      ask: 'Fix the build',
      issue: 0,
    })
  })

  it('starts from nothing in a browser that kept nothing', () => {
    const { local } = browser()
    pinCodebase('acme', WIDGET)
    expect(JSON.parse(local.getItem('hanzo.build.new.acme')!)).toMatchObject({ branch: 'trunk', ask: '' })
  })

  it('starts from nothing when what is kept is not a choice', () => {
    for (const kept of ['not json', '"a string"', 'null']) {
      const { local } = browser(area({ 'hanzo.build.new.none': kept }))
      pinCodebase(null, WIDGET)
      expect(JSON.parse(local.getItem('hanzo.build.new.none')!)).toMatchObject({ branch: 'trunk', ask: '' })
    }
  })

  it('does nothing it cannot keep in a browser that refuses storage', () => {
    refusing()
    expect(() => pinCodebase('acme', WIDGET)).not.toThrow()
  })
})

describe('the board Issues shows', () => {
  it('is kept per org and announced', () => {
    const { local, heard } = browser()
    pinBoard('acme', 'web')
    expect(local.getItem(boardKey('acme'))).toBe('"web"')
    expect(boardKey(null)).toBe('hanzo.build.board.none')
    expect(heard).toEqual(['hanzo-board'])
  })

  it('is still announced when it cannot be kept', () => {
    const heard = refusing()
    pinBoard('acme', 'web')
    expect(heard).toEqual(['hanzo-board'])
  })
})

describe('pointing New at a project', () => {
  const P = { key: 'hanzoai/build', owner: 'hanzoai', name: 'build', description: '', private: false, branch: 'trunk', activity: '', issues: 0, pulls: 0, linked: true, mirrored: false, forge: '', synced: '', status: '', slug: '', codebase: 'hanzoai_build', url: '' }

  it('chooses a project on the forge by its forge name, with the issue as the draft, keeping the model', () => {
    const { local } = browser(area({ 'hanzo.build.new.acme': JSON.stringify({ model: 'zen5', mode: 'plan' }) }))
    pinRepo('acme', { ...P, mirrored: true, forge: 'hanzoai_build' }, 'Fix it (#12)', 12)
    expect(JSON.parse(local.getItem('hanzo.build.new.acme')!)).toEqual({
      model: 'zen5',
      mode: 'plan',
      repo: { owner: 'acme', name: 'hanzoai_build', full_name: 'acme/hanzoai_build', private: false, default_branch: 'trunk', pushed_at: '', installation_id: 0, forge: true, clone: '' },
      branch: 'trunk',
      ask: 'Fix it (#12)',
      issue: 12,
    })
  })

  it('chooses one linked from GitHub alone by GitHub’s address', () => {
    const { local } = browser()
    pinRepo('acme', { ...P, branch: '', private: null })
    const kept = JSON.parse(local.getItem('hanzo.build.new.acme')!)
    expect(kept.repo).toMatchObject({ full_name: 'github.com/hanzoai/build', github: true, default_branch: 'main', private: true })
    expect(isHub(kept.repo)).toBe(true)
    expect(kept.branch).toBe('main')
  })

  it('refuses one that is neither, rather than pinning a name the platform would refuse', () => {
    browser()
    expect(() => pinRepo('acme', { ...P, linked: false })).toThrow('hanzoai/build is neither on the forge nor linked from GitHub')
  })
})

describe('pointing New at a repository just brought from GitHub', () => {
  it('replaces the repository, its branch and any handed-over draft, and keeps the mode, model and machine', () => {
    const was = { repo: asHub({ owner: 'webby-ai', name: 'intel-hub', full_name: 'webby-ai/intel-hub', private: true, default_branch: 'main', pushed_at: '', installation_id: 7 }), branch: 'dev', mode: 'plan', model: 'zen5', place: 'tgt_1', ask: 'Fix the hub', issue: 4 }
    const { local } = browser(area({ 'hanzo.build.new.acme': JSON.stringify(was) }))
    pinHub('acme', { owner: 'webby-ai', name: 'book', private: false, branch: 'trunk' })
    const kept = JSON.parse(local.getItem('hanzo.build.new.acme')!)
    expect(kept).toEqual({
      mode: 'plan',
      model: 'zen5',
      place: 'tgt_1',
      repo: { owner: 'webby-ai', name: 'book', full_name: 'github.com/webby-ai/book', private: false, default_branch: 'trunk', pushed_at: '', installation_id: 0, github: true },
      branch: 'trunk',
      ask: '',
      issue: 0,
    })
    expect(isHub(kept.repo)).toBe(true)
  })

  it('does nothing it cannot keep in a browser that refuses storage', () => {
    refusing()
    expect(() => pinHub('acme', { owner: 'webby-ai', name: 'book', private: true, branch: 'main' })).not.toThrow()
  })
})

describe('a kept repository that is gone', () => {
  const HUB = asHub({ owner: 'webby-ai', name: 'intel-hub', full_name: 'webby-ai/intel-hub', private: true, default_branch: 'main', pushed_at: '', installation_id: 7 })
  const FORGE = { owner: 'acme', name: 'widget', full_name: 'acme/widget', private: true, default_branch: 'main', pushed_at: '', installation_id: 0, forge: true as const, clone: '' }
  const page = (repos: { owner: string; name: string }[], more: Partial<NonNullable<Offered['github']>> = {}): Offered['github'] => ({ repos, next: '', unread: [], ...more })
  const UNREAD: Offered = { forge: null, github: null }

  it('is a GitHub repository the whole listing no longer has: let go on GitHub, or GitHub disconnected', () => {
    expect(gone(HUB, { forge: null, github: page([{ owner: 'webby-ai', name: 'book' }]) })).toBe(true)
    // A disconnected GitHub answers an empty, whole page.
    expect(gone(HUB, { forge: null, github: page([]) })).toBe(true)
  })

  it('is not one the listing still has, whatever case it spells it in', () => {
    expect(gone(HUB, { forge: null, github: page([{ owner: 'Webby-AI', name: 'Intel-Hub' }]) })).toBe(false)
  })

  it('is never judged from a GitHub answer that is not the whole list', () => {
    expect(gone(HUB, { forge: null, github: page([], { next: 'cursor' }) })).toBe(false)
    expect(gone(HUB, { forge: null, github: page([], { unread: ['webby-ai'] }) })).toBe(false)
  })

  it('is a codebase the forge no longer holds, by its own name or a linked copy’s', () => {
    expect(gone(FORGE, { forge: ['site', 'acme_widgets'], github: null })).toBe(true)
    expect(gone(FORGE, { forge: [], github: null })).toBe(true)
    expect(gone(FORGE, { forge: ['site', 'Widget'], github: null })).toBe(false)
  })

  it('is never judged from a list that was refused, let go or is still being read', () => {
    expect(gone(HUB, UNREAD)).toBe(false)
    expect(gone(FORGE, UNREAD)).toBe(false)
    // The forge's answer says nothing about a GitHub repository, nor GitHub's about a codebase.
    expect(gone(HUB, { forge: [], github: null })).toBe(false)
    expect(gone(FORGE, { forge: null, github: page([]) })).toBe(false)
  })

  it('is never an address the person gave, nor no repository at all', () => {
    expect(gone({ ...HUB, given: true }, { forge: [], github: page([]) })).toBe(false)
    expect(gone(null, { forge: [], github: page([]) })).toBe(false)
    // Neither a forge row nor a GitHub one: New offers it as nothing already.
    expect(gone({ name: 'widget', full_name: 'acme/widget' }, { forge: [], github: page([]) })).toBe(false)
  })
})

describe('the choice New opens on', () => {
  const FIRST = { repo: null as { name: string } | null, branch: '', place: '', mode: 'build', model: 'enso-auto', effort: 'medium', ask: '' }

  it('keeps every field a codebase handed over did not name: the model, the mode and the effort', () => {
    // pinCodebase in a browser that kept nothing writes the codebase, its branch and the words alone.
    const handed = { repo: { name: 'universe' }, branch: 'main', ask: 'Fix #12' }
    expect(settle(FIRST, undefined, handed)).toEqual({ ...FIRST, repo: { name: 'universe' }, branch: 'main', ask: 'Fix #12' })
  })

  it('takes the person’s coding defaults under what was kept here', () => {
    expect(settle(FIRST, { model: 'zen5', effort: 'high', place: 'tgt_1' }, { effort: 'low' })).toMatchObject({ model: 'zen5', effort: 'low', place: 'tgt_1', mode: 'build' })
  })

  it('keeps a field kept as empty — the default branch, the sandbox — and ignores what is not a choice', () => {
    expect(settle(FIRST, { place: 'tgt_1' }, { place: '', branch: '' })).toMatchObject({ place: '', branch: '' })
    expect(settle(FIRST, null, 'nope' as never, [1] as never)).toEqual(FIRST)
  })
})

describe('a codebase on the forge', () => {
  it('is one chosen from the forge’s list, with a name', () => {
    expect(isForge({ forge: true, name: 'widget' })).toBe(true)
    expect(isForge({ forge: true, name: '' })).toBe(false)
    expect(isForge({ name: 'widget' })).toBe(false)
    expect(isForge(null)).toBe(false)
  })
})

describe('repositories on their way to the forge', () => {
  it('are kept for this tab and read back', () => {
    const { session } = browser()
    const rows = [{ fullName: 'acme/api', name: 'api' }]
    writePending('acme', rows)
    expect(readPending('acme')).toEqual(rows)
    writePending('acme', [])
    expect(session.has('hanzo.build.sync.acme')).toBe(false)
    expect(readPending('acme')).toEqual([])
  })

  it('drop what is not a repository, and read nothing from what is not a list', () => {
    browser(area(), area({ 'hanzo.build.sync.none': JSON.stringify([{ fullName: 'acme/api', name: 'api' }, null, 'x', { fullName: '', name: 'a' }, { fullName: 'a/b', name: '' }, { fullName: 'a/b' }]) }))
    expect(readPending(null)).toEqual([{ fullName: 'acme/api', name: 'api' }])
    browser(area(), area({ 'hanzo.build.sync.none': '{"fullName":"a/b"}' }))
    expect(readPending(null)).toEqual([])
    browser(area(), area({ 'hanzo.build.sync.none': 'not json' }))
    expect(readPending(null)).toEqual([])
  })

  it('are none, and forgotten quietly, in a browser that refuses storage', () => {
    refusing()
    expect(readPending('acme')).toEqual([])
    expect(() => writePending('acme', [{ fullName: 'acme/api', name: 'api' }])).not.toThrow()
  })
})

describe('a GitHub choice', () => {
  const row = { owner: 'webby-ai', name: 'intel-hub', full_name: 'webby-ai/intel-hub', private: true, default_branch: 'main', pushed_at: '', installation_id: 7 }

  it('is offered under GitHub’s address, so a forge codebase of the same name stays distinct', () => {
    const hub = asHub(row)
    expect(hub.full_name).toBe('github.com/webby-ai/intel-hub')
    expect(isHub(hub)).toBe(true)
    expect(isForge(hub)).toBe(false)
  })

  it('is never a forge row, and a forge row is never one', () => {
    expect(isHub({ forge: true, name: 'intel-hub', full_name: 'webby-ai/intel-hub' } as never)).toBe(false)
    expect(isHub({ github: true, full_name: 'webby-ai/intel-hub' })).toBe(false)
    expect(isHub(null)).toBe(false)
  })
})
