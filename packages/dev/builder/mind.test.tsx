import { renderToString } from 'react-dom/server'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'

import type { Target } from './api/call.ts'
import { ENSO } from './api/models.ts'
import { catalog, EFFORTS, FIRST, grants, keep, kept, level, nameOf, sendable, useMind } from './mind.ts'

/** A browser's storage and events, enough for the store: what was written, and who heard. */
function browser(seed: Record<string, string> = {}) {
  const items = new Map(Object.entries(seed))
  const heard: string[] = []
  const target = new EventTarget()
  vi.stubGlobal('window', {
    localStorage: {
      getItem: (k: string) => items.get(k) ?? null,
      setItem: (k: string, v: string) => void items.set(k, v),
      removeItem: (k: string) => void items.delete(k),
    },
    addEventListener: target.addEventListener.bind(target),
    removeEventListener: target.removeEventListener.bind(target),
    dispatchEvent: (e: Event) => {
      heard.push(e.type)
      return target.dispatchEvent(e)
    },
  })
  return { items, heard }
}

const CATALOG = [
  { id: 'enso-auto', name: 'Enso', family: 'enso' as const, class: 'ours' as const, supports_reasoning: true },
  { id: 'zen6', name: 'Zen 6', family: 'zen' as const, class: 'ours' as const, supports_reasoning: true },
  { id: 'zen6-flash', name: 'Zen 6 Flash', family: 'zen' as const, class: 'ours' as const },
  { id: 'opus', name: 'Opus', class: 'premium' as const, supports_reasoning: true },
]

beforeEach(() => vi.unstubAllGlobals())
afterEach(() => vi.unstubAllGlobals())

describe('the kept choice', () => {
  it('is Enso at Auto where nothing is kept, and on a server', () => {
    expect(FIRST).toEqual({ model: ENSO, effort: 'auto' })
    expect(kept()).toEqual(FIRST)
    browser()
    expect(kept()).toEqual(FIRST)
  })

  it('is kept under one key for Chat and Dev, and the change is announced', () => {
    const { items, heard } = browser()
    keep({ model: 'zen6' })
    keep({ effort: 'max' })
    expect(JSON.parse(items.get('hanzo.mind')!)).toEqual({ model: 'zen6', effort: 'max' })
    expect(heard).toEqual(['hanzo:mind', 'hanzo:mind'])
    expect(kept()).toEqual({ model: 'zen6', effort: 'max' })
  })

  it('reads what another tab kept', () => {
    const { items } = browser()
    expect(kept()).toEqual(FIRST)
    items.set('hanzo.mind', JSON.stringify({ model: 'zen6-flash', effort: 'max' }))
    expect(kept()).toEqual({ model: 'zen6-flash', effort: 'max' })
  })

  it('reads an effort it does not know, a blank model and a broken entry as the defaults', () => {
    browser({ 'hanzo.mind': JSON.stringify({ model: '  ', effort: 'extreme' }) })
    expect(kept()).toEqual(FIRST)
    // Low is Dev's Fast effort, so a saved 'low' is kept; Medium was offered once and opens on Auto, which a run sends as medium.
    browser({ 'hanzo.mind': JSON.stringify({ model: 'zen6', effort: 'low' }) })
    expect(kept()).toEqual({ model: 'zen6', effort: 'low' })
    browser({ 'hanzo.mind': JSON.stringify({ model: 'zen6', effort: 'medium' }) })
    expect(kept()).toEqual({ model: 'zen6', effort: 'auto' })
    browser({ 'hanzo.mind': '{not json' })
    expect(kept()).toEqual(FIRST)
  })

  it('keeps only an effort it offers, whoever asks', () => {
    const { items } = browser()
    keep({ model: 'zen6', effort: 'max' })
    keep({ effort: 'medium' as never })
    expect(JSON.parse(items.get('hanzo.mind')!)).toEqual({ model: 'zen6', effort: 'auto' })
    keep({ effort: 'low' })
    expect(JSON.parse(items.get('hanzo.mind')!)).toEqual({ model: 'zen6', effort: 'low' })
    keep({ model: 'zen6-flash' })
    expect(kept()).toEqual({ model: 'zen6-flash', effort: 'low' })
  })

  it('holds a choice for the page where the browser refuses to store it', () => {
    vi.stubGlobal('window', {
      localStorage: {
        getItem: () => null,
        setItem: () => {
          throw new Error('QuotaExceededError')
        },
      },
      dispatchEvent: () => true,
    })
    keep({ model: 'zen6', effort: 'auto' })
    expect(kept()).toEqual({ model: 'zen6', effort: 'auto' })
  })

  it('offers Auto, Fast, High and Max, in that order, and nothing else', () => {
    expect(EFFORTS.map((e) => e.label)).toEqual(['Auto', 'Fast', 'High', 'Max'])
  })

  it('names to a coding run only an effort it takes: medium for Auto, low for Fast, high for the rest', () => {
    expect(EFFORTS.map((e) => level(e.id))).toEqual(['medium', 'low', 'high', 'high'])
  })
})

describe('the catalog', () => {
  it('names a model as the catalog does, and the router as Enso Auto', () => {
    expect(nameOf(ENSO, [])).toBe('Enso Auto')
    expect(nameOf('zen6', CATALOG)).toBe('Zen 6')
  })

  it('is asked for once per platform, and asked again after a refusal', async () => {
    let asked = 0
    let refuse = true
    vi.stubGlobal(
      'fetch',
      vi.fn(async () => {
        asked++
        if (refuse) return new Response(JSON.stringify({ detail: 'down' }), { status: 503 })
        return new Response(JSON.stringify({ data: CATALOG }), { status: 200 })
      }),
    )
    const t: Target = { api: 'https://api.test', token: () => 'tok', org: 'acme' }
    await expect(catalog(t)).rejects.toThrow('down')
    refuse = false
    const [a, b] = await Promise.all([catalog(t), catalog(t)])
    expect(a.map((m) => m.id)).toEqual(['enso-auto', 'zen6', 'zen6-flash', 'opus'])
    expect(b).toBe(a)
    expect(asked).toBe(2)
  })

  it('starts a surface on Enso at Auto, hiding third-party models until enabled in preferences', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({ data: CATALOG }), { status: 200 })))
    const t: Target = { api: 'https://api.first', token: () => 'tok', org: null }
    await catalog(t)
    let seen: ReturnType<typeof useMind> | null = null
    const Reads = () => {
      seen = useMind(t)
      return null
    }
    renderToString(<Reads />)
    expect(seen).toMatchObject({ model: ENSO, effort: 'auto', loading: false, error: null })
    expect(seen!.models.map((m) => m.id)).toEqual(['enso-auto', 'zen6', 'zen6-flash'])

    browser({ 'hanzo.chat.preferences': JSON.stringify({ allowThirdPartyModels: true, thirdPartyChosen: true }) })
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({ data: CATALOG }), { status: 200 })))
    renderToString(<Reads />)
    expect(seen!.models.map((m) => m.id)).toEqual(['enso-auto', 'zen6', 'zen6-flash', 'opus'])
  })
})

describe('the Free plan', () => {
  it('grants Enso Auto alone; every other plan grants every model', () => {
    expect(grants(true, ENSO)).toBe(true)
    for (const id of ['zen6', 'opus', 'enso-flash']) expect(grants(true, id)).toBe(false)
    for (const id of [ENSO, 'zen6', 'opus']) expect(grants(false, id)).toBe(true)
  })

  it('answers on Enso Auto, and a pick of another model is not kept', async () => {
    const { items } = browser({ 'hanzo.mind': JSON.stringify({ model: 'zen6', effort: 'auto' }) })
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({ data: CATALOG }), { status: 200 })))
    const t: Target = { api: 'https://api.free', token: () => 'tok', org: null }
    await catalog(t)
    let seen: ReturnType<typeof useMind> | null = null
    const Reads = () => {
      seen = useMind(t, 'free')
      return null
    }
    renderToString(<Reads />)
    expect(seen).toMatchObject({ model: ENSO, free: true })
    seen!.pick('opus')
    expect(JSON.parse(items.get('hanzo.mind')!).model).toBe('zen6')
  })
})

describe('another maker’s model on Free', () => {
  const models = [...CATALOG, { id: 'openai/gpt-6-sol', name: 'GPT-6 Sol', class: 'premium' as const }]

  it('is paid from the balance on any plan: sendable with a balance, not without', () => {
    expect(sendable(true, models, 1240, 'openai/gpt-6-sol')).toBe(true)
    expect(sendable(true, models, 0, 'openai/gpt-6-sol')).toBe(false)
    expect(sendable(true, models, null, 'openai/gpt-6-sol')).toBe(false)
  })

  it('leaves Hanzo’s own models to the plan, balance or none', () => {
    expect(sendable(true, models, 1240, 'zen6')).toBe(false)
    expect(sendable(true, models, 1240, ENSO)).toBe(true)
    expect(sendable(false, models, null, 'zen6')).toBe(true)
  })

  it('does not know a model the catalog does not list as another maker’s', () => {
    expect(sendable(true, models, 1240, 'nobody/unknown')).toBe(false)
  })
})
