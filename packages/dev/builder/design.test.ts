import { describe, expect, test } from 'vitest'
import { drafted, preview, said, tokensOf, type Draft } from './design.ts'

const raw = {
  name: 'Tidepool',
  colors: { primary: '#0E7C86', secondary: '#1B3A4B', accent: '#F2A541', background: '#FFFFFF', surface: '#F4F7F8', text: '#0B1F2A', muted: '#5B7280', border: '#D5E0E4' },
  fonts: { heading: 'Fraunces', body: 'Inter', mono: 'JetBrains Mono' },
  scale: [12, 14, 16, 20, 28, 40],
  spacing: [4, 8, 12, 16, 24, 32],
  radius: { sm: 4, md: 8, lg: 16 },
  components: { button: 'Solid teal, 8px corners', card: 'White on surface, hairline border', input: 'Surface fill', nav: 'Left aligned', hero: 'Big serif headline' },
  voice: { tone: 'Calm and exact', do: ['Say what it does'], dont: ['Hype'] },
}

describe('design system', () => {
  test('a model answer becomes a draft, and a broken one says why', () => {
    expect(typeof drafted(raw)).toBe('object')
    expect(drafted({ ...raw, colors: { primary: 'teal' } })).toMatch(/primary, a background and a text/)
    expect(drafted({ ...raw, name: '' })).toMatch(/no name/)
  })
  test('tokens are the CSS custom properties Studio’s renderer takes, every key and every value', () => {
    const t = tokensOf(drafted({ ...raw, voice: { ...raw.voice, dont: ['Hype; no {braces}, "quotes" or <tags>'] } }) as Draft)
    expect(t['--primary']).toBe('#0E7C86')
    expect(t['--background']).toBe('#FFFFFF')
    expect(t['--foreground']).toBe('#0B1F2A')
    expect(t['--font-sans']).toBe('Inter')
    expect(t['--radius-md']).toBe('8px')
    expect(JSON.parse(t['--component-button']!)).toBe('Solid teal, 8px corners')
    // hanzo/render `tokens`: a key is `--name`, a value one declaration's worth.
    for (const [k, v] of Object.entries(t)) {
      expect(k).toMatch(/^--[A-Za-z0-9-]{1,64}$/)
      expect(v.length).toBeLessThanOrEqual(300)
      expect(v).not.toMatch(/[;{}<>\\]|\/\*/)
    }
  })
  test('what the model is told names every colour and the voice; the page draws them', () => {
    const b = { id: 'brd_1', name: 'Tidepool', tokens: tokensOf(drafted(raw) as Draft) }
    const told = said(b)
    expect(told).toContain('#F2A541')
    expect(told).toContain('Calm and exact')
    expect(preview(b)).toContain('--primary:#0E7C86')
    expect(preview(b)).not.toContain('<script')
  })
})
