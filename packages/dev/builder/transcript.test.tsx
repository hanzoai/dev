import { createElement as h, type ReactNode } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it, vi } from 'vitest'

// Preserve semantic attributes (data-*, aria-*, role, className) so tests verify
// category icons, shimmer classes, hairline notices, timestamp dividers, and queue controls.
const { box, step, icon } = vi.hoisted(() => {
  const el = (type: string, props: Record<string, unknown>) => ({
    $$typeof: Symbol.for('react.transitional.element'),
    type,
    key: null,
    ref: null,
    props,
    _owner: null,
    _store: {},
  })
  const clean = (props: Record<string, unknown>) => {
    const out: Record<string, unknown> = {}
    for (const [k, v] of Object.entries(props)) {
      if (k === 'children' || k === 'role' || k === 'className' || k.startsWith('data-') || k.startsWith('aria-')) {
        out[k] = v
      }
    }
    return out
  }
  return {
    box: (props: Record<string, unknown>) => el(props.render === 'button' ? 'button' : 'div', clean(props)),
    step: ({ name, detail, defaultOpen, children }: { name: string; detail?: string; defaultOpen?: boolean; children?: unknown }) =>
      el('section', { 'data-step': name, children: [detail ?? '', defaultOpen ? children : null] }),
    icon: (name: string) => () => el('svg', { 'data-icon': name }),
  }
})
vi.mock('@hanzo/gui', () => ({ SizableText: box, XStack: box, YStack: box }))
vi.mock('@hanzo/ui', () => ({ Button: box }))
vi.mock('@hanzo/ui/chat', () => ({ Code: box, GAP: '$4', MEASURE: 720, Message: box, Step: step, Thread: box }))
vi.mock('@hanzogui/lucide-icons-2', () => ({
  AlertCircle: icon('AlertCircle'),
  BookOpen: icon('BookOpen'),
  ChevronDown: icon('ChevronDown'),
  ChevronRight: icon('ChevronRight'),
  CornerDownLeft: icon('CornerDownLeft'),
  FilePen: icon('FilePen'),
  GitBranch: icon('GitBranch'),
  Globe: icon('Globe'),
  Info: icon('Info'),
  Pencil: icon('Pencil'),
  Sparkles: icon('Sparkles'),
  Square: icon('Square'),
  SquareTerminal: icon('SquareTerminal'),
  Wrench: icon('Wrench'),
  X: icon('X'),
}))
vi.mock('./prose.tsx', () => ({ Prose: ({ text }: { text: string }) => text as unknown as ReactNode }))
vi.mock('./reply.tsx', () => ({ artifacts: () => [], lead: () => null, Reply: ({ onRate }: { onRate?: unknown }) => (onRate ? 'rate-wired' : 'rate-missing') }))

const { SteerStack, Transcript, TRANSCRIPT_MOTION_CSS, inferActivityCategory } = await import('./transcript.tsx')
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

  it('off Developer mode, shows the result and folds the process into one plain inline row with a category icon', () => {
    const html = draw(false)
    expect(html).toContain('Make me a landing page')
    expect(html).toContain('Your page is live.')
    expect(html).toContain('Done — published')
    expect(html).toContain('data-slot="work-row"')
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

describe('agentic transcript polish: grouped icons, shimmer, hairline notices, timestamps, and steer stack', () => {
  it('selects category-matched Lucide icons for grouped work rows (commands, reads, tools, edits, web search, thinking)', () => {
    const grouped: Card[] = [
      { kind: 'shell', key: 'c1', command: 'ls', output: '', ran: 'done', durationMs: 1200 },
      { kind: 'said', key: 'm1', who: 'agent', text: 'Checked directory.' },
      { kind: 'step', key: 't1', name: 'Loaded tools', detail: 'playwright', output: '', ran: 'done' },
      { kind: 'shell', key: 'c2', command: 'pnpm test', output: '', ran: 'done' },
      { kind: 'said', key: 'm2', who: 'agent', text: 'Ran tests.' },
      {
        kind: 'explore',
        key: 'r1',
        calls: [{ actions: [{ type: 'read', command: 'Read', name: 'app.ts', path: 'src/app.ts' }], exit: 0 }],
        ran: 'done',
      },
      { kind: 'shell', key: 'c3', command: 'git diff', output: '', ran: 'done' },
      { kind: 'said', key: 'm3', who: 'agent', text: 'Inspected diff.' },
      { kind: 'edit', key: 'e1', files: [{ path: 'src/app.ts', kind: 'update', added: 4, removed: 1 }], ran: 'done' },
      { kind: 'said', key: 'm4', who: 'agent', text: 'Updated file.' },
      { kind: 'step', key: 'w1', name: 'Search', detail: 'hanzo docs', output: '', ran: 'done' },
      { kind: 'said', key: 'm5', who: 'agent', text: 'Searched web.' },
      { kind: 'think', key: 'k1', text: 'Considering architecture' },
    ]
    const html = renderToStaticMarkup(h(Transcript, { cards: grouped, live: false, empty: null, onVerdict: async () => {}, devMode: false }))
    expect(html).toContain('data-icon="SquareTerminal"')
    expect(html).toContain('data-icon="Wrench"')
    expect(html).toContain('Loaded tools, ran a command')
    expect(html).toContain('data-icon="BookOpen"')
    expect(html).toContain('Read a file, ran a command')
    expect(html).toContain('data-icon="FilePen"')
    expect(html).toContain('data-icon="Globe"')
    expect(html).toContain('data-icon="Sparkles"')

    // Verbose view expands grouped work details with individual tool call durations.
    const expanded = renderToStaticMarkup(
      h(Transcript, { cards: grouped, live: false, empty: null, onVerdict: async () => {}, devMode: false, view: 'verbose' }),
    )
    expect(expanded).toContain('data-slot="work-details"')
    expect(expanded).toContain('1.2s')
  })

  it('renders smooth monochrome shimmer and active icon pulse for in-flight work and respects prefers-reduced-motion', () => {
    expect(TRANSCRIPT_MOTION_CSS).toContain('prefers-reduced-motion: reduce')
    expect(TRANSCRIPT_MOTION_CSS).toContain('--foreground')
    expect(TRANSCRIPT_MOTION_CSS).toContain('--muted-foreground')

    // Active running work row shimmers.
    const liveWork: Card[] = [
      { kind: 'said', key: 'a', who: 'agent', text: 'Continuing with regression tests.' },
      { kind: 'shell', key: 'c1', command: 'pnpm test', output: '', ran: 'running' },
    ]
    const activeHtml = renderToStaticMarkup(
      h(Transcript, {
        cards: liveWork,
        live: true,
        activity: 'Comparing test isolation',
        empty: null,
        onVerdict: async () => {},
        devMode: false,
      }),
    )
    expect(activeHtml).toContain('hanzo-work-shimmer')
    expect(activeHtml).toContain('hanzo-work-icon-active')
    expect(activeHtml).toContain('Comparing test isolation')
    expect(activeHtml).toContain('data-status="running"')

    // Tail live activity row renders when no work card is currently running.
    const tailOnly: Card[] = [{ kind: 'said', key: 'a', who: 'agent', text: 'Checking tests.' }]
    const tailHtml = renderToStaticMarkup(
      h(Transcript, {
        cards: tailOnly,
        live: true,
        activity: 'Comparing test isolation',
        empty: null,
        onVerdict: async () => {},
        devMode: false,
      }),
    )
    expect(tailHtml).toContain('data-slot="live-activity"')
    expect(tailHtml).toContain('Comparing test isolation')
    expect(tailHtml).toContain('hanzo-work-shimmer')
    expect(inferActivityCategory('Reading files')).toBe('read')
    expect(inferActivityCategory('Editing route.ts')).toBe('edit')
    expect(inferActivityCategory('Searching web docs')).toBe('search')
    expect(inferActivityCategory('Loading MCP tools')).toBe('tool')
    expect(inferActivityCategory('Thinking through layout')).toBe('think')
    expect(inferActivityCategory('Comparing test isolation')).toBe('command')
  })

  it('renders centered hairline status notices and centered timestamp dividers between turns', () => {
    const now = new Date('2026-11-28T11:30:00')
    const convo: Card[] = [
      { kind: 'said', key: 'u1', who: 'person', text: 'Check routing', at: '2026-11-28T10:15:00' },
      { kind: 'said', key: 'a1', who: 'agent', text: 'Checking now.', at: '2026-11-28T10:16:00' },
      {
        kind: 'note',
        key: 'lim',
        text: "You've hit your usage limit. Upgrade your plan or add credits to continue, or try again later.",
        funds: true,
        at: '2026-11-28T10:17:00',
      },
      { kind: 'said', key: 'u2', who: 'person', text: 'continue', at: '2026-11-28T11:09:00' },
    ]
    const html = renderToStaticMarkup(
      h(Transcript, { cards: convo, live: false, empty: null, onVerdict: async () => {}, devMode: false, now, onAddFunds: () => {} }),
    )
    expect(html).toContain('data-slot="status-notice"')
    expect(html).toContain('data-tone="alert"')
    expect(html).toContain('data-icon="AlertCircle"')
    expect(html).toContain('data-slot="timestamp-divider"')
    expect(html).toContain('Today 11:09')
    expect(html).toContain('continue')
  })

  it('renders stacked steering/queued messages with Steer, Interrupt, Edit, and Remove controls', () => {
    const items = [
      { id: 'q1', text: 'Also check mobile layout', mode: 'queued' as const },
      { id: 'q2', text: 'Run vitest after', mode: 'steering' as const },
    ]
    const stackHtml = renderToStaticMarkup(
      h(SteerStack, {
        items,
        onSteer: () => {},
        onInterrupt: () => {},
        onEdit: () => {},
        onRemove: () => {},
      }),
    )
    expect(stackHtml).toContain('data-slot="steer-stack"')
    expect(stackHtml).toContain('data-mode="queued"')
    expect(stackHtml).toContain('data-mode="steering"')
    expect(stackHtml).toContain('Also check mobile layout')
    expect(stackHtml).toContain('Run vitest after')
    expect(stackHtml).toContain('data-slot="steer-send"')
    expect(stackHtml).toContain('data-slot="steer-interrupt"')
    expect(stackHtml).toContain('data-slot="steer-edit"')
    expect(stackHtml).toContain('data-slot="steer-remove"')
    expect(stackHtml).toContain('Steering…')

    // Inline queued messages inside Transcript also expose full controls.
    const inlineHtml = renderToStaticMarkup(
      h(Transcript, {
        cards: [],
        live: true,
        empty: null,
        onVerdict: async () => {},
        queued: items,
        onSendQueued: () => {},
        onInterruptQueued: () => {},
        onEditQueued: () => {},
        onRemoveQueued: () => {},
      }),
    )
    expect(inlineHtml).toContain('data-slot="queued-steer"')
    expect(inlineHtml).toContain('data-slot="queued-interrupt"')
    expect(inlineHtml).toContain('data-slot="queued-edit"')
    expect(inlineHtml).toContain('data-slot="queued-remove"')
  })
})
