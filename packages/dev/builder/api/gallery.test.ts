/**
 * The gallery's catalogue as Make something new reads it: a recorded answer of
 * gallery.hanzo.ai/templates.json (four of its rows, as served on 2026-11-08),
 * rows this Library cannot offer, a gallery that does not answer, where each
 * kind is made, and the search.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import { APP, catalogue, GALLERY, KIND, KINDS, narrow, template, UNREAD, type Template } from './gallery.ts'
import recorded from './gallery.test.json'

function answer(status: number, body: unknown) {
  const seen: { url: string; init: RequestInit }[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      seen.push({ url, init })
      return new Response(typeof body === 'string' ? body : JSON.stringify(body), { status, headers: { 'Content-Type': 'application/json' } })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

const ROW = recorded.templates[0]!
const AGENT = { slug: 'scout', kind: 'agent', displayName: 'Scout', description: 'Finds leads.', category: 'Sales', screenshotUrl: 'https://gallery.hanzo.ai/screenshots/scout.webp', instructions: 'You find leads.', model: 'zen5' }

describe('the gallery’s catalogue', () => {
  it('reads every row of the recorded answer as a site, in the gallery’s order, from the gallery itself and with no bearer', async () => {
    const seen = answer(200, recorded)
    const list = await catalogue()
    expect(seen.map((s) => s.url)).toEqual([GALLERY])
    expect(new Headers(seen[0]!.init.headers).has('Authorization')).toBe(false)
    expect(list.map((x) => [x.slug, x.name, x.kind, x.category])).toEqual([
      ['instrument', 'Instrument Lab', 'site', 'Portfolio'],
      ['warp-tunnel', 'Hyperion Tunnel', 'site', '3D & Shaders'],
      ['metrics', 'Metrics', 'site', 'Dashboard'],
      ['folio-about', 'Folio About', 'site', 'Folio Portfolio'],
    ])
    expect(list[0]!.preview).toBe('https://gallery.hanzo.ai/screenshots/instrument.webp')
    expect(list.every((x) => x.instructions === '' && x.model === '')).toBe(true)
  })

  it('drops a kind it does not know, a row with no slug or a bad one, a site with no repository and an agent with no instructions', () => {
    expect(template({ ...ROW, kind: 'hologram' })).toBeNull()
    expect(template({ ...ROW, slug: '' })).toBeNull()
    expect(template({ ...ROW, slug: '../etc' })).toBeNull()
    expect(template({ ...ROW, repo: '' })).toBeNull()
    expect(template({ ...ROW, repo: 'javascript:alert(1)' })).toBeNull()
    expect(template({ ...AGENT, instructions: '  ' })).toBeNull()
    for (const bad of [null, 'instrument', 7, []]) expect(template(bad)).toBeNull()
  })

  it('keeps an agent with what it is told and the model it thinks with, and a picture only when it is an https capture the gallery has', () => {
    expect(template(AGENT)).toMatchObject({ slug: 'scout', kind: 'agent', name: 'Scout', instructions: 'You find leads.', model: 'zen5', preview: AGENT.screenshotUrl })
    expect(template({ ...ROW, hasScreenshot: false })!.preview).toBe('')
    expect(template({ ...ROW, screenshotUrl: 'http://gallery.hanzo.ai/x.webp' })!.preview).toBe('')
    expect(template({ ...ROW, displayName: '', name: '' })!.name).toBe('instrument')
  })

  it('skips the bad rows of an answer and keeps each slug once', async () => {
    answer(200, { ...recorded, templates: [ROW, { ...ROW, displayName: 'Again' }, { ...ROW, slug: 'x', kind: 'video' }, 'junk', AGENT] })
    expect((await catalogue()).map((x) => [x.slug, x.name])).toEqual([
      ['instrument', 'Instrument Lab'],
      ['scout', 'Scout'],
    ])
  })

  it.each([
    ['answers 500', () => answer(500, { title: 'Internal Server Error' })],
    ['answers what is not JSON', () => answer(200, '<!doctype html>')],
    ['answers no list', () => answer(200, { version: 2 })],
    ['cannot be reached', () => vi.stubGlobal('fetch', vi.fn(async () => Promise.reject(new TypeError('Failed to fetch'))))],
  ])('says one plain sentence when the gallery %s', async (_, set) => {
    set()
    await expect(catalogue()).rejects.toThrow(UNREAD)
  })

  it('leaves an abort an abort', async () => {
    const stop = new AbortController()
    vi.stubGlobal('fetch', vi.fn(async () => (stop.abort(), Promise.reject(new DOMException('aborted', 'AbortError')))))
    await expect(catalogue({ signal: stop.signal })).rejects.toThrow('aborted')
  })
})

describe('where each kind is made', () => {
  it('makes a site in Dev and an agent in Customize › Agents, every kind named', () => {
    expect(APP).toEqual({ site: 'dev', agent: 'agents' })
    for (const k of KINDS) expect(KIND[k]).toBeTruthy()
  })
})

describe('the search', () => {
  const list = [...recorded.templates.map(template), template(AGENT)].filter((x): x is Template => x !== null)
  const slugs = (xs: Template[]) => xs.map((x) => x.slug)

  it('narrows to a kind, and every kind is everything', () => {
    expect(slugs(narrow(list, 'agent', ''))).toEqual(['scout'])
    expect(slugs(narrow(list, '', ''))).toEqual(['instrument', 'warp-tunnel', 'metrics', 'folio-about', 'scout'])
  })

  it('holds every word typed, in any case, against the name, the category, the framework and what it is for', () => {
    expect(slugs(narrow(list, '', 'three'))).toEqual(['warp-tunnel'])
    expect(slugs(narrow(list, '', 'PORTFOLIO'))).toEqual(['instrument', 'folio-about'])
    expect(slugs(narrow(list, '', 'portfolio html'))).toEqual(['folio-about'])
    expect(slugs(narrow(list, '', '  dashboard  '))).toEqual(['metrics'])
    expect(slugs(narrow(list, 'agent', 'portfolio'))).toEqual([])
  })
})
