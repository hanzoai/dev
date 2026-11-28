import { afterEach, describe, expect, it, vi } from 'vitest'

import { mark, marks, RUNS_EVENT, RUNS_KEY } from './runmeta.ts'

afterEach(() => vi.unstubAllGlobals())

function device(seed: Record<string, string> = {}, refuses = false) {
  const store = new Map(Object.entries(seed))
  const target = new EventTarget()
  const said: string[] = []
  vi.stubGlobal('window', {
    localStorage: {
      getItem: (k: string) => store.get(k) ?? null,
      setItem: (k: string, v: string) => {
        if (refuses) throw new Error('full')
        store.set(k, v)
      },
    },
    addEventListener: target.addEventListener.bind(target),
    removeEventListener: target.removeEventListener.bind(target),
    dispatchEvent: (e: Event) => {
      said.push(e.type)
      return target.dispatchEvent(e)
    },
  })
  return { store, said }
}

describe('a run’s marks, one store for every list that shows it', () => {
  it('reads none where nothing was kept, and where what is kept is not a record', () => {
    device()
    expect(marks()).toEqual({})
    device({ [RUNS_KEY]: '[1,2]' })
    expect(marks()).toEqual({})
    device({ [RUNS_KEY]: 'not json' })
    expect(marks()).toEqual({})
  })

  it('marks one run without touching another, and says so on the window', () => {
    const { store, said } = device({ [RUNS_KEY]: JSON.stringify({ sess_a: { pinned: true } }) })
    mark('sess_b', { archived: true })
    mark('sess_b', { title: 'Footer' })
    expect(JSON.parse(store.get(RUNS_KEY)!)).toEqual({ sess_a: { pinned: true }, sess_b: { archived: true, title: 'Footer' } })
    expect(said).toEqual([RUNS_EVENT, RUNS_EVENT])
  })

  it('hands back the same object until something changes, as a store snapshot must', () => {
    device({ [RUNS_KEY]: JSON.stringify({ sess_a: { deleted: true } }) })
    const first = marks()
    expect(marks()).toBe(first)
    mark('sess_a', { archived: true })
    expect(marks()).not.toBe(first)
    expect(marks().sess_a).toEqual({ deleted: true, archived: true })
  })

  it('keeps a change for the page when storage refuses it', () => {
    device({}, true)
    mark('sess_a', { deleted: true })
    expect(marks().sess_a).toEqual({ deleted: true })
  })
})
