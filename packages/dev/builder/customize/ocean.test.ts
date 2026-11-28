import { afterEach, describe, expect, it, vi } from 'vitest'

import { DOMAINS, FACETS, ITEMS, keep, kept, nature, score, type Form } from './ocean.ts'

const all = (form: Form, a: number) => ITEMS[form].map(() => a)
const count = (form: Form, pick: (i: (typeof ITEMS)[Form][number]) => boolean) => ITEMS[form].filter(pick).length

afterEach(() => vi.unstubAllGlobals())

describe('the published inventories', () => {
  it('the IPIP-NEO-120: 120 statements, 24 a domain, 4 a facet, 55 keyed minus, facets cycling N1 E1 O1 A1 C1 N2 …', () => {
    expect(ITEMS.long).toHaveLength(120)
    for (const d of DOMAINS) {
      expect(count('long', (i) => i.domain === d)).toBe(24)
      for (const f of FACETS(d)) expect(count('long', (i) => i.facet === f)).toBe(4)
    }
    expect(count('long', (i) => i.key === -1)).toBe(55)
    expect(ITEMS.long.slice(0, 6).map((i) => i.facet)).toEqual(['N1', 'E1', 'O1', 'A1', 'C1', 'N2'])
    expect(ITEMS.long[0]).toEqual({ text: 'Worry about things.', domain: 'N', facet: 'N1', key: 1 })
    expect(ITEMS.long[119]).toEqual({ text: 'Act without thinking.', domain: 'C', facet: 'C6', key: -1 })
    expect(new Set(ITEMS.long.map((i) => i.text)).size).toBe(120)
  })

  it('the 50 markers: 10 a factor, keyed as printed, Emotional Stability turned over into Neuroticism', () => {
    expect(ITEMS.short).toHaveLength(50)
    // Plus-keyed per factor in the published key: I 5, II 6, III 6, IV 2, V 7. IV's two are Neuroticism's minus.
    const plus = { E: 5, A: 6, C: 6, N: 8, O: 7 }
    for (const d of DOMAINS) {
      expect(count('short', (i) => i.domain === d)).toBe(10)
      expect(count('short', (i) => i.domain === d && i.key === 1)).toBe(plus[d])
    }
    expect(ITEMS.short[8]).toEqual({ text: 'Am relaxed most of the time.', domain: 'N', facet: null, key: -1 })
    expect(ITEMS.short[3]).toEqual({ text: 'Get stressed out easily.', domain: 'N', facet: null, key: 1 })
    expect(ITEMS.short[0]).toEqual({ text: 'Am the life of the party.', domain: 'E', facet: null, key: 1 })
  })
})

describe('scoring', () => {
  it('all 3s is the midpoint, 3.0, in every domain and facet', () => {
    for (const form of ['short', 'long'] as const) {
      const s = score(form, all(form, 3))
      for (const d of DOMAINS) expect(s.domains[d]).toBe(3)
      for (const m of Object.values(s.facets)) expect(m).toBe(3)
    }
    expect(Object.keys(score('long', all('long', 3)).facets)).toHaveLength(30)
    expect(score('short', all('short', 3)).facets).toEqual({})
  })

  it('a minus-keyed answer counts 6 - answer: all 5s on the 120, by hand from the published keys', () => {
    const s = score('long', all('long', 5))
    // N: 17 plus-keyed, 7 minus-keyed → (17·5 + 7·1) / 24.
    expect(s.domains.N).toBeCloseTo(92 / 24, 10)
    expect(s.facets.N1).toBe(5) // Anxiety: four plus
    expect(s.facets.N5).toBe(2) // Immoderation: one plus, three minus → (5 + 1 + 1 + 1) / 4
    expect(s.facets.A1).toBe(4) // Trust: three plus, one minus → (15 + 1) / 4
    expect(s.facets.A2).toBe(1) // Morality: four minus
    expect(s.facets.E5).toBe(5) // Excitement-seeking: four plus
  })

  it('all 5s on the 50, by hand from the published key', () => {
    const s = score('short', all('short', 5))
    expect(s.domains.E).toBe(3) // 5 plus, 5 minus
    expect(s.domains.A).toBeCloseTo(3.4, 10) // 6 plus, 4 minus
    expect(s.domains.C).toBeCloseTo(3.4, 10)
    expect(s.domains.O).toBeCloseTo(3.8, 10) // 7 plus, 3 minus
    // Emotional Stability, as published: 2 plus, 8 minus → 1.8; Neuroticism is 6 - 1.8.
    expect(s.domains.N).toBeCloseTo(4.2, 10)
  })

  it('a scale is the mean of what was answered; nothing answered is 0', () => {
    const a = all('long', 0)
    a[0] = 4 // Worry about things. (N1 +)
    const immoderate = ITEMS.long.findIndex((i) => i.text === 'Rarely overindulge.') // N5 −
    a[immoderate] = 2
    const s = score('long', a)
    expect(s.facets.N1).toBe(4)
    expect(s.facets.N5).toBe(4)
    expect(s.domains.N).toBe(4)
    expect(s.domains.O).toBe(0)
  })
})

describe('the clone’s nature', () => {
  it('in the middle of every scale it is the five means alone', () => {
    expect(nature('Dave', 'short', score('short', all('short', 3)))).toBe(
      'Big Five (IPIP 50-item Big-Five markers, means on 1 to 5): Openness 3.0, Conscientiousness 3.0, Extraversion 3.0, Agreeableness 3.0, Neuroticism 3.0.',
    )
  })

  it('reads each domain at either end, and the facets furthest out: all 5s on the 120, by hand', () => {
    // O 12 plus/12 minus → 3.0; C 11/13 → 2.8; E 18/6 → 4.0; A 7/17 → 2.2; N 17/7 → 3.8.
    expect(nature('Dave', 'long', score('long', all('long', 5)))).toBe(
      [
        'How Dave thinks and writes:',
        '- Outgoing and energetic: thinks out loud and takes the lead; writes with warmth and enthusiasm.',
        '- Direct and skeptical: questions claims and says what they think; writes bluntly, without softening.',
        '- Sensitive to pressure: alert to what could go wrong; writes with care and caveats.',
        '- Stands out: high Imagination (5.0), Self-efficacy (5.0), Excitement-seeking (5.0); low Cautiousness (1.0), Morality (1.0), Cooperation (1.0).',
        'Big Five (IPIP-NEO-120, means on 1 to 5): Openness 3.0, Conscientiousness 2.8, Extraversion 4.0, Agreeableness 2.2, Neuroticism 3.8.',
      ].join('\n'),
    )
  })
})

describe('the answers this browser keeps', () => {
  const area = (seed: Record<string, string> = {}) => {
    const m = new Map(Object.entries(seed))
    return { getItem: (k: string) => m.get(k) ?? null, setItem: (k: string, v: string) => void m.set(k, v), m }
  }

  it('keeps them per person and reads them back', () => {
    const local = area()
    vi.stubGlobal('window', { localStorage: local })
    const a = kept('dave@acme.test')
    expect(a).toEqual({ form: 'short', short: all('short', 0), long: all('long', 0) })
    a.long[0] = 4
    keep('dave@acme.test', { ...a, form: 'long' })
    expect([...local.m.keys()]).toEqual(['hanzo.build.ocean.dave@acme.test'])
    expect(kept('dave@acme.test')).toEqual({ ...a, form: 'long' })
    expect(kept('zed@acme.test').long[0]).toBe(0)
  })

  it('a sheet that is not one is blank, and storage that refuses loses nothing but the keeping', () => {
    vi.stubGlobal('window', { localStorage: area({ 'hanzo.build.ocean.x': JSON.stringify({ form: 'long', short: [9], long: all('long', 6) }) }) })
    expect(kept('x')).toEqual({ form: 'long', short: all('short', 0), long: all('long', 0) })
    const refuse = () => {
      throw new DOMException('The operation is insecure.', 'SecurityError')
    }
    vi.stubGlobal('window', { localStorage: { getItem: refuse, setItem: refuse } })
    expect(kept('x').form).toBe('short')
    expect(() => keep('x', kept('x'))).not.toThrow()
  })
})
