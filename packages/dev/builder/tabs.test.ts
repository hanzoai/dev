import { afterEach, describe, expect, it, vi } from 'vitest'

import { address, cleared, destination, located, moved, name, opened, PAGE, pinned, read, SEARCH, shut, stepped, went, type Tab, type Tabs } from './tabs.ts'

const SEED: Tabs = {
  tabs: [
    { id: 'artifacts', kind: 'artifacts', title: 'Artifacts' },
    { id: 'preview', kind: 'preview', title: 'Preview' },
    { id: 'terminal', kind: 'terminal', title: 'Terminal' },
  ],
  at: 'terminal',
}

afterEach(() => vi.unstubAllGlobals())

describe('the panel’s tabs', () => {
  it('chooses a kind already open rather than opening it twice', () => {
    const next = opened(SEED, { kind: 'preview', title: 'Preview' })
    expect(next.tabs).toHaveLength(3)
    expect(next.at).toBe('preview')
  })

  it('adds a kind that is not open at the end and chooses it', () => {
    const next = opened(SEED, { kind: 'files', title: 'Files' })
    expect(next.tabs.map((t) => t.id)).toEqual(['artifacts', 'preview', 'terminal', 'files'])
    expect(next.at).toBe('files')
  })

  it('opens a page once per thing: the same artifact twice is one tab', () => {
    const once = opened(SEED, { kind: PAGE, title: 'index.html', body: '<h1>hi</h1>', type: 'text/html' })
    const twice = opened(once, { kind: PAGE, title: 'index.html', body: '<h1>hi</h1>', type: 'text/html' })
    expect(twice.tabs).toHaveLength(4)
    expect(twice.at).toBe(once.at)
    const other = opened(twice, { kind: PAGE, title: 'about.html', body: '<h1>about</h1>', type: 'text/html' })
    expect(other.tabs).toHaveLength(5)
    expect(other.at).not.toBe(once.at)
  })

  it('shuts a tab and chooses the one to its right, else its left', () => {
    expect(shut(SEED, 'terminal')).toEqual({ tabs: SEED.tabs.slice(0, 2), at: 'preview' })
    expect(shut({ ...SEED, at: 'artifacts' }, 'artifacts').at).toBe('preview')
    expect(shut({ ...SEED, at: 'artifacts' }, 'preview').at).toBe('artifacts')
    expect(shut({ tabs: [SEED.tabs[0]!], at: 'artifacts' }, 'artifacts')).toEqual({ tabs: [], at: null })
  })

  it('moves a tab to a place, the others closing up', () => {
    expect(moved(SEED, 'terminal', 0).tabs.map((t) => t.id)).toEqual(['terminal', 'artifacts', 'preview'])
    expect(moved(SEED, 'artifacts', 9).tabs.map((t) => t.id)).toEqual(['preview', 'terminal', 'artifacts'])
    expect(moved(SEED, 'gone', 0)).toBe(SEED)
  })

  it('restores what was kept, dropping a page that held only an object URL', () => {
    const kept = {
      tabs: [
        { id: 'terminal', kind: 'terminal', title: 'Terminal' },
        { id: 'p1', kind: PAGE, title: 'old', url: 'blob:https://hanzo.ai/1' },
        { id: 'p2', kind: PAGE, title: 'index.html', body: '<h1>hi</h1>', type: 'text/html' },
        { id: 'p3', kind: PAGE, title: 'hanzo.ai', url: 'https://hanzo.ai' },
        { nonsense: true },
      ],
      at: 'p1',
    }
    vi.stubGlobal('window', { localStorage: { getItem: (k: string) => (k === 'hanzo.side.chat:thread:1' ? JSON.stringify(kept) : null) } })
    const back = read('chat:thread:1')!
    expect(back.tabs.map((t) => t.id)).toEqual(['terminal', 'p2', 'p3'])
    // A chosen tab that is no longer there is not a choice.
    expect(back.at).toBe('terminal')
    expect(read('chat:thread:none')).toBeNull()
  })

  it('names an address by its host and path, and opens a typed one', () => {
    expect(name('https://hanzo.ai/docs')).toBe('hanzo.ai/docs')
    expect(name('https://hanzo.ai/')).toBe('hanzo.ai')
    expect(address('hanzo.ai')).toBe('https://hanzo.ai')
    expect(address('localhost:3000')).toBe('http://localhost:3000')
    expect(address('  ')).toBe('')
  })

  it('keeps a page’s way back and forward as a browser does', () => {
    const blank: Tab = { id: 'p', kind: PAGE, title: 'New tab', url: '' }
    const a = went(blank, 'https://hanzo.ai')
    expect(a.back).toEqual([])
    const b = went(a, 'https://hanzo.ai/docs')
    expect(b).toMatchObject({ url: 'https://hanzo.ai/docs', title: 'hanzo.ai/docs', back: ['https://hanzo.ai'], ahead: [] })
    const back = stepped(b, 'back')
    expect(back).toMatchObject({ url: 'https://hanzo.ai', back: [], ahead: ['https://hanzo.ai/docs'] })
    expect(stepped(back, 'back')).toBe(back)
    const ahead = stepped(back, 'forward')
    expect(ahead).toMatchObject({ url: 'https://hanzo.ai/docs', back: ['https://hanzo.ai'], ahead: [] })
    // Going somewhere new forgets Forward.
    expect(went(back, 'https://hanzo.app')).toMatchObject({ back: ['https://hanzo.ai'], ahead: [] })
    // A page of bytes pointed at an address is that address now.
    expect(went({ id: 'q', kind: PAGE, title: 'a.html', body: '<p>', type: 'text/html' }, 'https://hanzo.ai')).toMatchObject({ body: undefined, type: undefined })
  })

  it('opens what was typed in the bar: an address as itself, words as a search', () => {
    for (const a of ['hanzo.ai', 'https://hanzo.ai/x', 'docs.hanzo.ai/dev', 'localhost:3000', 'about:blank']) expect(located(a), a).toBe(true)
    for (const w of ['hanzo', 'what is enso', 'kai 1.4', '']) expect(located(w), w).toBe(false)
    expect(destination('hanzo.ai')).toBe('https://hanzo.ai')
    expect(destination('what is enso')).toBe(`${SEARCH}what%20is%20enso`)
    expect(destination('  ')).toBe('')
  })

  it('pins a tab to the end of the pinned ones, and lets it go to lead the rest', () => {
    const one = pinned(SEED, 'terminal', true)
    expect(one.tabs.map((t) => t.id)).toEqual(['terminal', 'artifacts', 'preview'])
    const two = pinned(one, 'preview', true)
    expect(two.tabs.map((t) => [t.id, Boolean(t.pinned)])).toEqual([['terminal', true], ['preview', true], ['artifacts', false]])
    const back = pinned(two, 'terminal', false)
    expect(back.tabs.map((t) => t.id)).toEqual(['preview', 'terminal', 'artifacts'])
    expect(back.tabs.find((t) => t.id === 'terminal')).not.toHaveProperty('pinned')
    expect(pinned(back, 'preview', true)).toBe(back)
  })

  it('keeps a moved tab in its group', () => {
    const pins = pinned(pinned(SEED, 'terminal', true), 'artifacts', true)
    expect(moved(pins, 'preview', 0).tabs.map((t) => t.id), 'a plain tab cannot pass the pinned ones').toEqual(['terminal', 'artifacts', 'preview'])
    expect(moved(pins, 'terminal', 9).tabs.map((t) => t.id), 'a pinned tab cannot pass the plain ones').toEqual(['artifacts', 'terminal', 'preview'])
  })

  it('opens a new tab after the pinned ones, and a pinned one among them', () => {
    const pins = pinned(SEED, 'terminal', true)
    expect(opened(pins, { kind: 'files', title: 'Files' }).tabs.map((t) => t.id)).toEqual(['terminal', 'artifacts', 'preview', 'files'])
    expect(opened(pins, { kind: 'diff', title: 'Diff', pinned: true }).tabs.map((t) => t.id)).toEqual(['terminal', 'diff', 'artifacts', 'preview'])
  })

  it('closes a pinned tab only on purpose: its own close takes it, a clear leaves it', () => {
    const pins = pinned(SEED, 'preview', true)
    expect(shut(pins, 'preview').tabs.map((t) => t.id)).toEqual(['artifacts', 'terminal'])
    const left = cleared(pins, (t) => t.kind === 'artifacts')
    expect(left.tabs.map((t) => t.id)).toEqual(['preview', 'artifacts'])
    expect(left.at, 'the chosen tab went, so the first left is chosen').toBe('preview')
    expect(cleared({ ...pins, at: 'preview' }).tabs.map((t) => t.id)).toEqual(['preview'])
    expect(cleared(pinned(pins, 'artifacts', true), (t) => t.kind === 'terminal').tabs).toHaveLength(3)
  })

  it('reads the pinned tabs first from what was kept', () => {
    const kept: Tabs = { tabs: [{ id: 'a', kind: 'a', title: 'A' }, { id: 'b', kind: 'b', title: 'B', pinned: true }], at: 'a' }
    vi.stubGlobal('window', { localStorage: { getItem: () => JSON.stringify(kept), setItem: () => {} } })
    expect(read('pins-first')?.tabs.map((t) => t.id)).toEqual(['b', 'a'])
  })
})
