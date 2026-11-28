import { createElement as h, type ReactNode } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it, vi } from 'vitest'

// Each piece draws its children; a Step draws its name and line, and what it holds only when it opens by default,
// as the real one does on its first paint.
const { box, step, none } = vi.hoisted(() => {
  const el = (type: string, props: Record<string, unknown>) => ({ $$typeof: Symbol.for('react.transitional.element'), type, key: null, ref: null, props, _owner: null, _store: {} })
  return {
    box: ({ children }: { children?: unknown }) => el('div', { children }),
    step: ({ name, detail, defaultOpen, children }: { name: string; detail?: string; defaultOpen?: boolean; children?: unknown }) =>
      el('section', { 'data-step': name, children: [detail ?? '', defaultOpen ? children : null] }),
    none: () => null,
  }
})
vi.mock('@hanzo/gui', () => ({ SizableText: box, XStack: box, YStack: box }))
vi.mock('@hanzo/ui', () => ({ Button: box }))
vi.mock('@hanzo/ui/chat', () => ({ Code: box, GAP: '$4', MEASURE: 720, Message: box, Step: step, Thread: box }))
vi.mock('@hanzogui/lucide-icons-2', () => ({ GitBranch: none }))
vi.mock('./prose.tsx', () => ({ Prose: ({ text }: { text: string }) => text as unknown as ReactNode }))
vi.mock('./reply.tsx', () => ({ artifacts: () => [], lead: () => null, Reply: ({ onRate }: { onRate?: unknown }) => (onRate ? 'rate-wired' : 'rate-missing') }))

const { Transcript } = await import('./transcript.tsx')
import type { Card } from './api/turn.ts'

const run: Card[] = [
  { kind: 'said', key: 'a', who: 'person', text: 'Make me a landing page' },
  { kind: 'note', key: 'n', text: 'Started on agent/16a6bce87b3e', step: true },
  { kind: 'step', key: 's', name: 'Sandbox', detail: 'leasing a desktop sandbox m_c06cee94', output: '', ran: 'done' },
  { kind: 'shell', key: 'c', command: 'cd ~ && npm run build', output: 'built', ran: 'done' },
  { kind: 'said', key: 'b', who: 'agent', text: 'Your page is live.' },
  { kind: 'note', key: 'end', text: 'Done — published' },
]
const draw = (devMode: boolean) => renderToStaticMarkup(h(Transcript, { cards: run, live: false, empty: null, onVerdict: async () => {}, devMode }))

describe('a run as it reads (hanzo-inc/app issues 317 and 321)', () => {
  it('rates an answer through the reply row alone, so its verdict is recorded and there is no second thumb pair', () => {
    expect(draw(false).match(/rate-wired/g)).toHaveLength(1)
    expect(draw(false)).not.toContain('rate-missing')
  })

  it('off Developer mode, shows the result and folds the process into one plain line', () => {
    const html = draw(false)
    expect(html).toContain('Make me a landing page')
    expect(html).toContain('Your page is live.')
    expect(html).toContain('Done — published')
    expect(html).toContain('data-step="Worked"')
    expect(html).toContain('Set up the workspace, ran a command')
    for (const raw of ['npm run build', 'cd ~', 'agent/16a6bce87b3e', 'm_c06cee94']) expect(html).not.toContain(raw)
  })

  it('in Developer mode, draws each step and command as it ran', () => {
    const html = draw(true)
    expect(html).not.toContain('data-step="Worked"')
    expect(html).toContain('data-step="Ran"')
    expect(html).toContain('cd ~ &amp;&amp; npm run build')
    expect(html).toContain('Started on agent/16a6bce87b3e')
  })

  it('draws an Add funds button on insufficient-balance Error steps, and omits it on other errors (#363/#364)', () => {
    const balanceCards: Card[] = [
      { kind: 'said', key: 'a', who: 'person', text: 'Make a page' },
      { kind: 'note', key: 'n', text: 'Your balance is out of credit. Add credit, then try again.' },
      { kind: 'step', key: 'e', name: 'Error', detail: '', output: 'metering: insufficient balance', ran: 'error' },
    ]
    const withFunds = renderToStaticMarkup(h(Transcript, { cards: balanceCards, live: false, empty: null, devMode: false, onAddFunds: () => {} } as never))
    expect(withFunds).toContain('Add funds')

    const vendor402Cards: Card[] = [
      { kind: 'said', key: 'a', who: 'person', text: 'Make a page' },
      { kind: 'note', key: 'n', text: 'Your balance is out of credit. Add credit, then try again.', funds: true },
    ]
    const withVendorFunds = renderToStaticMarkup(h(Transcript, { cards: vendor402Cards, live: false, empty: null, devMode: false, onAddFunds: () => {} } as never))
    expect(withVendorFunds.match(/Add funds/g)).toHaveLength(1)

    const buildErrorCards: Card[] = [
      { kind: 'said', key: 'a', who: 'person', text: 'Make a page' },
      { kind: 'step', key: 'e', name: 'Error', detail: '', output: 'TypeError: undefined is not a function', ran: 'error' },
    ]
    const withoutFunds = renderToStaticMarkup(h(Transcript, { cards: buildErrorCards, live: false, empty: null, devMode: false, onAddFunds: () => {} } as never))
    expect(withoutFunds).not.toContain('Add funds')
  })
})
