import { renderToStaticMarkup } from 'react-dom/server'
import { createElement as h } from 'react'
import { describe, expect, it, vi } from 'vitest'

const { seen, box } = vi.hoisted(() => {
  const seen: Record<string, Record<string, unknown>> = {}
  const box = (name: string) => (p: Record<string, unknown>) => {
    seen[name] = p
    return { $$typeof: Symbol.for('react.transitional.element'), type: 'div', key: null, ref: null, props: { children: p.children }, _owner: null, _store: {} }
  }
  return { seen, box }
})
vi.mock('@hanzo/gui', () => ({ SizableText: box('text'), YStack: box('ystack') }))
vi.mock('@hanzo/ui', () => ({ DropdownMenuCheckboxItem: box('item') }))

const { grant, hold, KeepAwake } = await import('./awake.tsx')

const lock = () => {
  const l = { released: 0, release: async () => void l.released++ }
  return l
}

describe('Keep computer awake', () => {
  it('is one checkbox row that reports its new state once', () => {
    const calls: boolean[] = []
    renderToStaticMarkup(h(KeepAwake, { on: false, onToggle: (on: boolean) => calls.push(on) }))
    expect(seen.item).toMatchObject({ checked: false })
    ;(seen.item!.onCheckedChange as (on: boolean) => void)(true)
    expect(calls).toEqual([true])
  })

  it('is on only once the browser grants the lock', async () => {
    const l = lock()
    expect(await hold({ wakeLock: { request: async () => l } }, null, true)).toBe(l)
    expect(await hold({ wakeLock: { request: async () => Promise.reject(new Error('denied')) } }, null, true)).toBeNull()
    expect(await hold({}, null, true)).toBeNull()
  })

  it('lets the lock go when switched off, and asks for no second one while it is held', async () => {
    const l = lock()
    expect(await hold({ wakeLock: { request: async () => lock() } }, l, true)).toBe(l)
    expect(await hold({}, l, false)).toBeNull()
    expect(l.released).toBe(1)
  })

  it('lets go of a lock that is granted after the run view has closed', async () => {
    const l = lock()
    expect(await grant({ wakeLock: { request: async () => l } }, null, true, () => true, () => {})).toBeNull()
    expect(l.released).toBe(1)
  })

  it('reports it when the browser takes the lock back', async () => {
    let fire = () => {}
    const l = { ...lock(), addEventListener: (_: 'release', f: () => void) => void (fire = f) }
    const dropped: unknown[] = []
    expect(await grant({ wakeLock: { request: async () => l } }, null, true, () => false, (x) => dropped.push(x))).toBe(l)
    expect(dropped).toEqual([])
    fire()
    expect(dropped).toEqual([l])
  })
})
