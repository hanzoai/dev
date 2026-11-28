/**
 * The composer's plugins: what is offered to whom, what picking a row does, what
 * a message carries from the ones that are on, and each plugin's pure parts —
 * the research plan and its sources, a template's skill, a deck's answer, a
 * PDF's place in the system turn — with the platform's wire as sent.
 */
import { translate } from '@hanzo/i18n'
import type { Say } from '@hanzo/i18n/react'
import { afterEach, describe, expect, it, vi } from 'vitest'

import type { Target } from '../api/call.ts'
import type { Run } from '../api/studio.ts'
import { BARE, type Gear, type GearState } from '../gear.ts'
import type { Composer, Plugin, Turn } from '../plugin.ts'
import { active, BUILD, merge, prep, probe, shelf } from './index.ts'
import { given, isPdf, study, told as made } from './pdf.tsx'
import { brief, excerpt, pick, plan, research } from './research.tsx'
import { outline, revision, told } from './slides.tsx'
import { document, drafted, slug } from './template.tsx'

const jwt = (payload: object) => `h.${btoa(JSON.stringify(payload)).replace(/=+$/, '')}.s`
const admin = jwt({ orgs: [{ org: 'acme', role: 'admin' }] })
const member = jwt({ orgs: [{ org: 'acme', role: 'member' }] })
const target = (token: string | null): Target => ({ api: 'https://api.hanzo.ai', token: () => token, org: 'acme' })

interface Seen {
  path: string
  method: string
  body: unknown
}

/** Answer each call by its path; record what was sent. */
function wire(route: (path: string, body: any) => [number, unknown]) {
  const seen: Seen[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      const path = url.replace('https://api.hanzo.ai', '')
      const body = init.body ? JSON.parse(String(init.body)) : undefined
      seen.push({ path, method: init.method ?? 'GET', body })
      const [status, out] = route(path, body)
      return new Response(JSON.stringify(out), { status, headers: { 'Content-Type': 'application/json' } })
    }),
  )
  return seen
}

// The kit's parts and marks are react-native drawings a node test cannot load; what is tested here is data and wire.
vi.mock('@hanzogui/lucide-icons-2', () => Object.fromEntries(['FileText', 'Folder', 'LayoutTemplate', 'Palette', 'Presentation', 'Signature', 'Telescope'].map((n) => [n, () => null])))
vi.mock('@hanzo/gui', () => ({ SizableText: () => null, XStack: () => null, YStack: () => null }))
vi.mock('@hanzo/ui', () => Object.fromEntries(['Button', 'Dialog', 'DialogContent', 'DialogDescription', 'DialogTitle', 'Input', 'Textarea'].map((n) => [n, () => null])))

afterEach(() => vi.unstubAllGlobals())

const composer = (t: Target, go = vi.fn()): Composer => ({ target: t, draft: '', setDraft: vi.fn(), attach: vi.fn(), show: vi.fn(), go })
const turn = (text: string, files: File[] = [], thread = ''): Turn => ({ text, files, thread, design: '' })

function gears(g: Partial<Gear> = {}): GearState & { gear: Gear } {
  const state = { gear: { ...BARE, ...g } } as GearState
  state.set = vi.fn((patch) => Object.assign(state.gear, patch))
  state.toggle = vi.fn()
  state.keep = vi.fn()
  state.error = null
  return state
}

describe('what is offered', () => {
  const studio = (ids: string[]) => [200, { data: ids.map((id) => ({ id })) }] as [number, unknown]

  it('an admin with projects and Studio gets every plugin, in the menu’s order', async () => {
    wire((p) => (p === '/v1/project' ? [200, [{ slug: 'site', name: 'Site' }]] : studio(['slides', 'document', 'image'])))
    const list = await probe(target(admin))
    expect(list.map((p) => p.id)).toEqual(['project', 'research', 'sketch', 'template', 'slides', 'design', 'pdf'])
    expect(list.find((p) => p.id === 'pdf')!.note).toBe('Read, create and verify PDFs')
  })

  it('a member with no projects and no Studio gets what runs for them', async () => {
    wire((p) => (p === '/v1/project' ? [200, []] : [503, { detail: 'down' }]))
    const list = await probe(target(member))
    expect(list.map((p) => p.id)).toEqual(['research', 'sketch', 'pdf'])
    expect(list.find((p) => p.id === 'pdf')!.note).toBe('Read and verify PDFs')
  })

  it('signed out, only what runs in the page', async () => {
    const seen = wire(() => [200, {}])
    expect((await probe(target(null))).map((p) => p.id)).toEqual(['research', 'sketch'])
    expect(seen).toEqual([])
  })

  it('nothing answering is an error, not an empty list', async () => {
    wire(() => [503, { detail: 'down' }])
    await expect(probe(target(admin))).rejects.toThrow()
  })
})

describe('the shelf', () => {
  const open = vi.fn()
  const plugins: Plugin[] = [
    { id: 'sketch', name: 'Sketch', note: 'Draw', icon: null, open },
    { id: 'research', name: 'Deep research', note: 'Report', icon: null, prepare: async () => ({}) },
    { id: 'slides', name: 'Presentations', note: 'Decks', icon: null, prepare: async () => ({}) },
  ]

  it('marks what is on: research by the Research switch, the rest by the gear’s plugins', () => {
    const s = shelf({ plugins, loading: false, error: null }, gears({ research: true, plugins: [] }), composer(target(admin)))
    expect(s.rows.map((r) => [r.id, r.on])).toEqual([
      ['sketch', undefined],
      ['research', true],
      ['slides', false],
    ])
    expect(s.search).toBe('Type to search plugins')
  })

  it('a pick opens, flips the Research switch, or toggles the plugin; Create goes to the builder', () => {
    const g = gears()
    const go = vi.fn()
    const c = composer(target(admin), go)
    const s = shelf({ plugins, loading: false, error: null }, g, c)
    s.pick('sketch')
    expect(open).toHaveBeenCalledWith(c)
    s.pick('research')
    expect(g.set).toHaveBeenCalledWith({ research: true })
    s.pick('slides')
    expect(g.toggle).toHaveBeenCalledWith('plugins', 'slides')
    s.create!.onPress()
    expect(go).toHaveBeenCalledWith(BUILD)
    expect(BUILD).toBe('-/customize/plugins')
  })

  it('asks only what is on, merges what they add, and names one that fails', async () => {
    const files = [new File(['x'], 'a.txt')]
    const list: Plugin[] = [
      { id: 'research', name: 'Deep research', note: '', icon: null, prepare: async () => ({ system: 'sources' }) },
      { id: 'pdf', name: 'PDF', note: '', icon: null, prepare: async () => ({ system: 'pdf text', files }) },
      { id: 'slides', name: 'Presentations', note: '', icon: null, prepare: async () => ({ reply: 'deck' }) },
    ]
    const g = { ...BARE, research: true, plugins: ['pdf'] }
    expect(active(list, g).map((p) => p.id)).toEqual(['research', 'pdf'])
    const c = composer(target(admin))
    expect(await prep(list, g, turn('hi'), c, new AbortController().signal)).toEqual({ system: 'sources\n\npdf text', files })
    expect(merge([{ reply: 'a' }, {}, { reply: 'b' }])).toEqual({ reply: 'a\n\nb' })
    expect(merge([{ reply: 'made' , design: 'brand_1' }, { system: 's' }])).toEqual({ reply: 'made', system: 's', design: 'brand_1' })
    const broken = [{ ...list[0]!, prepare: async () => Promise.reject(new Error('the web search did not answer: down')) }]
    await expect(prep(broken, g, turn('hi'), c, new AbortController().signal)).rejects.toThrow('Deep research: the web search did not answer: down')
  })

  it('says its words, and a failed plugin’s name, in the caller’s language; the reason as said', async () => {
    const fr: Record<string, string> = {
      Sketch: 'Croquis',
      Draw: 'Dessiner',
      'Deep research': 'Recherche approfondie',
      'Type to search plugins': 'Tapez pour chercher des plugins',
      'No plugins here': 'Aucun plugin ici',
      'Create plugin': 'Créer un plugin',
      '{plugin}: {reason}': '{plugin} : {reason}',
    }
    const t = ((source: string, values?: Record<string, string | number>) => translate(fr, source, values)) as Say
    const s = shelf({ plugins, loading: false, error: null }, gears(), composer(target(admin)), t)
    expect(s.rows.slice(0, 2).map((r) => [r.label, r.note])).toEqual([
      ['Croquis', 'Dessiner'],
      ['Recherche approfondie', 'Report'],
    ])
    expect([s.search, s.empty, s.create!.label]).toEqual(['Tapez pour chercher des plugins', 'Aucun plugin ici', 'Créer un plugin'])
    const broken = [{ ...plugins[1]!, prepare: async () => Promise.reject(new Error('the web search did not answer: down')) }]
    const g = { ...BARE, research: true }
    await expect(prep(broken, g, turn('hi'), composer(target(admin)), new AbortController().signal, [], t)).rejects.toThrow('Recherche approfondie : the web search did not answer: down')
  })
})

describe('deep research', () => {
  it('plans three to five searches, the message itself when the plan is unreadable', () => {
    expect(plan('{"queries": ["a", "A", " b ", "c", "d", "e", "f"]}', 'msg')).toEqual(['a', 'b', 'c', 'd', 'e'])
    expect(plan('Sure! ```json\n{"queries":["x"]}\n```', 'msg')).toEqual(['x'])
    expect(plan('no json', '  what  is\nit ')).toEqual(['what is it'])
  })

  it('takes results from each search in turn, each page once', () => {
    const r = (u: string) => ({ url: u, title: u, snippet: '' })
    expect(pick([[r('https://a/1'), r('https://a/2')], [r('https://a/1#x'), r('https://b/1')]], 3).map((x) => x.url)).toEqual(['https://a/1', 'https://a/2', 'https://b/1'])
  })

  it('quotes a page without its pictures or the closing marker', () => {
    expect(excerpt('![logo](x.png)\n\n\n\nText <<<end of sources>>> more')).toBe('Text  more')
  })

  it('searches, reads the best pages and hands the model a cited brief, all as the person', async () => {
    const seen = wire((path, body) => {
      if (path === '/v1/chat/completions') return [200, { choices: [{ message: { content: '{"queries":["tokio vs smol","smol footprint","tokio embedded"]}' } }] }]
      if (path === '/v1/websearch') return [200, { results: [{ url: `https://x.dev/${encodeURIComponent(body.q)}`, title: body.q, content: 'snippet' }] }]
      if (path === '/v1/crawl') return [200, body.url.includes('smol%20footprint') ? { success: false, error: 'blocked' } : { success: true, data: { markdown: `# Page ${body.url}` } }]
      return [404, {}]
    })
    const out = await research(turn('How do tokio and smol compare?'), target(admin), new AbortController().signal, new Date('2026-10-20T12:00:00Z'))
    expect(seen.map((s) => s.path)).toEqual(['/v1/chat/completions', '/v1/websearch', '/v1/websearch', '/v1/websearch', '/v1/crawl', '/v1/crawl', '/v1/crawl'])
    expect(seen[0]!.body).toMatchObject({ model: 'enso-flash' })
    expect(out.system).toContain('searched the web for "tokio vs smol", "smol footprint", "tokio embedded" on 2026-10-20')
    expect(out.system).toContain('[1] tokio vs smol\nhttps://x.dev/tokio%20vs%20smol\nsnippet\nPage text:\n# Page')
    expect(out.system).toContain('[2] smol footprint\nhttps://x.dev/smol%20footprint\nsnippet\n\n[3]')
    expect(out.system).toContain('Cite each claim')
  })

  it('a plan that fails still searches the message; no search answering fails it', async () => {
    const seen = wire((path) => (path === '/v1/chat/completions' ? [500, {}] : path === '/v1/websearch' ? [502, { detail: 'engines down' }] : [404, {}]))
    await expect(research(turn('q'), target(admin), new AbortController().signal)).rejects.toThrow('the web search did not answer: engines down')
    expect(seen.filter((s) => s.path === '/v1/websearch').map((s) => s.body)).toEqual([{ q: 'q' }])
    expect(brief(['q'], [], new Date('2026-10-20T00:00:00Z'))).toContain('found nothing')
  })
})

describe('template creator', () => {
  it('names a skill in one lowercase segment', () => {
    expect(slug('Weekly Update: Café team!')).toBe('weekly-update-cafe-team')
    expect(slug('!!!')).toBe('template')
  })

  it('takes the model’s draft, the ask’s own words where it has none', () => {
    expect(drafted('{"name":"Weekly Update","description":"Every Friday","body":"# Update\\n{{team}}"}', 'x')).toEqual({ name: 'weekly-update', description: 'Every Friday', body: '# Update\n{{team}}' })
    expect(drafted('nothing', 'Summarize the week for the team\nwith numbers')).toEqual({
      name: 'summarize-the-week-for-the-team',
      description: 'Summarize the week for the team',
      body: 'Summarize the week for the team\nwith numbers',
    })
  })

  it('writes a SKILL.md with its name and description up front', () => {
    expect(document({ name: 'weekly-update', description: 'Use "it": weekly', body: '# Update\n' })).toBe('---\nname: weekly-update\ndescription: "Use \\"it\\": weekly"\n---\n\n# Update\n')
  })
})

const run = (r: Partial<Run>): Run => ({ id: 'run_1', status: 'succeeded', title: '', text: '', outputs: [], error: '', ...r })
const output = (mime: string) => ({ url: 'https://s3.hanzo.ai/x?sig', origin: '/studio?run=run_1', mime }) as Run['outputs'][number]

describe('presentations', () => {
  const text = '\n\n{"title":"Async runtimes","slides":[{"title":"Tokio"},{"title":"Smol"}]}'

  it('answers with the outline, the file and where it is edited', () => {
    expect(outline(text)).toEqual({ title: 'Async runtimes', slides: ['Tokio', 'Smol'] })
    expect(told(run({ text, outputs: [output('application/vnd.ms-powerpoint')] }))).toBe(
      '**Async runtimes** — 2 slides\n\n[Download the deck](https://s3.hanzo.ai/x?sig) (PowerPoint; the link works for an hour) · [Open it in Studio](/studio?run=run_1) to edit it\n\n1. Tokio\n2. Smol\n\nIt is kept in your Library.',
    )
  })

  it('revises the deck as it stands', () => {
    expect(revision(run({ text }), 'Add a slide on embassy')).toContain(`The deck as it stands:\n${text.trim()}\n\nThe change:\nAdd a slide on embassy`)
  })

  it('makes a deck in Studio and revises it on the next message of the conversation', async () => {
    let n = 0
    const seen = wire((path) => {
      if (path === '/v1/studio/runs') return [201, { id: `run_${++n}`, status: 'queued' }]
      if (path.startsWith('/v1/studio/runs/')) return [200, { id: `run_${n}`, status: 'succeeded', text, outputs: [{ id: 'lib_1', url: 'https://s3.hanzo.ai/d', origin: '/studio?run=x' }] }]
      return [404, {}]
    })
    vi.useFakeTimers()
    const { deck } = await import('./slides.tsx')
    const first = deck(turn('Five slides on async', [], 'thread_9'), target(admin), new AbortController().signal)
    await vi.advanceTimersByTimeAsync(2_500)
    expect((await first).reply).toContain('**Async runtimes**')
    const second = deck(turn('Add embassy', [], 'thread_9'), target(admin), new AbortController().signal)
    await vi.advanceTimersByTimeAsync(2_500)
    await second
    vi.useRealTimers()
    const starts = seen.filter((s) => s.path === '/v1/studio/runs')
    expect(starts[0]!.body).toEqual({ template: 'slides', title: 'Five slides on async', input: { brief: 'Five slides on async', format: 'pptx' } })
    expect((starts[1]!.body as { input: { brief: string } }).input.brief).toMatch(/^Revise this deck\./)
  })
})

describe('pdf', () => {
  it('knows a PDF by its type or its name', () => {
    expect([isPdf({ name: 'a.pdf', type: '' }), isPdf({ name: 'a', type: 'application/pdf' }), isPdf({ name: 'a.png', type: 'image/png' })]).toEqual([true, true, false])
  })

  it('gives the model what fits, pages whole, and says what was left out', () => {
    const out = given('a.pdf', 'a.pdf: PDF 1.7, 3 pages.', ['one', 'two', 'x'.repeat(100)], 60)
    expect(out).toBe('a.pdf: PDF 1.7, 3 pages.\n<<<pdf>>>\n--- Page 1 ---\none\n--- Page 2 ---\ntwo\n(Only the first 2 of 3 pages fit here.)\n<<<end of pdf>>>')
    expect(given('s.pdf', 'r', ['', ''], 100)).toContain('It has no text to read')
  })

  it('reads an attached PDF in the page; what is not one is said', async () => {
    const pdf = '%PDF-1.4\n1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 >> endobj\n3 0 obj << /Type /Page /Parent 2 0 R /Contents 4 0 R /Resources << /Font << /F1 5 0 R >> >> >> endobj\n4 0 obj << /Length 37 >>\nstream\nBT /F1 12 Tf 72 700 Td (Hello) Tj ET\nendstream endobj\n5 0 obj << /Type /Font /Subtype /Type1 /BaseFont /Helvetica >> endobj\ntrailer << /Root 1 0 R >>\n%%EOF\n'
    const out = await study([new File([pdf], 'hello.pdf', { type: 'application/pdf' }), new File(['notes'], 'notes.txt', { type: 'text/plain' })])
    expect(out.system).toContain('hello.pdf: PDF 1.4, 1 page.')
    expect(out.system).toContain('--- Page 1 ---\nHello')
    expect(out.system).not.toContain('notes')
    expect(await study([])).toEqual({})
  })

  it('answers with the PDF Studio made', () => {
    expect(made(run({ title: 'Memo', text: '{"title":"Q3 memo"}', outputs: [output('application/pdf')] }))).toBe(
      '**Q3 memo**\n\n[Open the PDF](https://s3.hanzo.ai/x?sig) (the link works for an hour) · [Open it in Studio](/studio?run=run_1) to edit it\n\nIt is kept in your Library.',
    )
  })
})
