import { afterEach, describe, expect, it, vi } from 'vitest'

import { copy } from './clipboard.ts'

afterEach(() => vi.unstubAllGlobals())

describe('copying to the clipboard', () => {
  it('says it copied', async () => {
    const wrote: string[] = []
    vi.stubGlobal('navigator', { clipboard: { writeText: async (t: string) => void wrote.push(t) } })
    await expect(copy('abc')).resolves.toBe(true)
    expect(wrote).toEqual(['abc'])
  })

  it('says it did not, with nothing thrown, when the browser refuses or has no clipboard', async () => {
    vi.stubGlobal('navigator', { clipboard: { writeText: () => Promise.reject(new Error('denied')) } })
    await expect(copy('abc')).resolves.toBe(false)
    vi.stubGlobal('navigator', {})
    await expect(copy('abc')).resolves.toBe(false)
  })
})
