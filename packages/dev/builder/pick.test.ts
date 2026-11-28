/**
 * The model New and a run open on: the kept pick while the catalog offers it,
 * else the catalog's own default — Enso, the router — and never nothing.
 */
import { describe, expect, it } from 'vitest'

import type { Model } from './api/models.ts'
import { standing } from './pick.ts'

const CATALOG = [
  { id: 'zen5', name: 'Zen5', family: 'zen', class: 'ours' },
  { id: 'enso-auto', name: 'Enso', family: 'enso', class: 'ours' },
  { id: 'enso-flash', name: 'Enso Flash', family: 'enso', class: 'ours' },
  { id: 'anthropic/claude-opus-5.5', name: 'Claude Opus 5.5', class: 'premium' },
] as Model[]

describe('the model a run opens on', () => {
  it('is the kept pick while the catalog offers it', () => {
    expect(standing('zen5', CATALOG)).toBe('zen5')
  })

  it('is Enso with nothing kept: the catalog’s own default for a conversation', () => {
    expect(standing('', CATALOG)).toBe('enso-auto')
  })

  it('is Enso in place of a kept premium model, which is never a default', () => {
    expect(standing('anthropic/claude-opus-5.5', CATALOG)).toBe('enso-auto')
  })

  it('is the default in place of a model the catalog no longer offers', () => {
    expect(standing('zen4', CATALOG)).toBe('enso-auto')
  })

  it('keeps the pick while the catalog has not answered, and is Enso then with none', () => {
    expect(standing('zen5', [])).toBe('zen5')
    expect(standing('', [])).toBe('enso-auto')
  })

  it('is Enso when Enso is kept, whether or not the catalog lists the router', () => {
    expect(standing('enso-auto', CATALOG)).toBe('enso-auto')
    expect(standing('enso-auto', CATALOG.filter((m) => m.id !== 'enso-auto'))).toBe('enso-auto')
  })

  it('is the first of Hanzo’s own when the catalog offers no router', () => {
    expect(standing('', CATALOG.filter((m) => m.id !== 'enso-auto'))).toBe('enso-flash')
  })
})
