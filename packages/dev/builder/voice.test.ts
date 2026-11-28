import { afterEach, describe, expect, it, vi } from 'vitest'

import { spoken } from './voice.ts'

afterEach(() => vi.unstubAllGlobals())

describe('the language dictation listens in', () => {
  it('is this browser’s, when dictation offers it', () => {
    vi.stubGlobal('navigator', { language: 'fr-CA' })
    expect(spoken()).toBe('fr')
    vi.stubGlobal('navigator', { language: 'ja' })
    expect(spoken()).toBe('ja')
  })

  it('is English for a language it does not offer, and where there is no browser', () => {
    vi.stubGlobal('navigator', { language: 'pt-BR' })
    expect(spoken()).toBe('en')
    vi.stubGlobal('navigator', undefined)
    expect(spoken()).toBe('en')
  })
})
