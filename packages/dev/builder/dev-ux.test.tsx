import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { afterEach, describe, expect, it, vi } from 'vitest'

import { href, parse, route } from './route.ts'
import { linkThread, resetThreadLinks, threadChain } from './thread.ts'

const here = (name: string) => readFileSync(resolve(import.meta.dirname, name), 'utf8')

const SESSION_A = 'sess_0123456789abcdef0123456789abcdef'
const SESSION_B = 'sess_fedcba9876543210fedcba9876543210'

// Capture Composer props and rendered DOM attributes cleanly in Node without needing jsdom.
const state = vi.hoisted(() => ({
  wide: true,
  carryText: '',
  composerProps: null as Record<string, unknown> | null,
  goCalls: [] as string[],
  runState: {
    detail: { value: null as Record<string, unknown> | null, error: null as Error | null, loading: false, reload: vi.fn() },
    record: null as Record<string, unknown> | null,
    events: [] as Array<{ id: string; sessionId: string; seq: number; kind: string; actor: string; payload: unknown; createdAt: string }>,
    status: 'running',
    refused: '',
  },
  steerCalls: [] as Array<{ session: string; text: string }>,
  stopCalls: [] as Array<{ id: string; why?: string }>,
  startCalls: [] as Array<Record<string, unknown>>,
  startError: null as Error | null,
}))

const { box, text, btn, none } = vi.hoisted(() => {
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
      if (
        k === 'children' ||
        k === 'role' ||
        k === 'id' ||
        k === 'title' ||
        k === 'style' ||
        k.startsWith('data-') ||
        k.startsWith('aria-')
      ) {
        out[k] = v
      }
    }
    if (props.minH !== undefined) out['data-min-h'] = String(props.minH)
    if (props.shrink !== undefined) out['data-shrink'] = String(props.shrink)
    if (props.overflowX !== undefined) out['data-overflow-x'] = String(props.overflowX)
    return out
  }
  return {
    box: (props: Record<string, unknown>) => el(props.render === 'button' ? 'button' : 'div', clean(props)),
    text: (props: Record<string, unknown>) => el('span', clean(props)),
    btn: (props: Record<string, unknown>) => el('button', clean(props)),
    none: () => null,
  }
})

vi.mock('@hanzo/gui', () => ({
  SizableText: text,
  XStack: box,
  YStack: box,
  useMedia: () => ({ md: state.wide, sm: state.wide }),
}))

vi.mock('@hanzo/ui', () => ({
  Button: btn,
  Dialog: ({ open, children }: { open?: boolean; children?: unknown }) => (open ? box({ children }) : null),
  DialogContent: box,
  DialogDescription: box,
  DialogTitle: box,
  DropdownMenu: ({ trigger }: { trigger?: unknown }) => box({ children: trigger }),
  DropdownMenuContent: box,
  DropdownMenuItem: box,
  DropdownMenuSeparator: none,
  DropdownMenuShortcut: text,
  DropdownMenuSub: box,
  DropdownMenuSubContent: box,
  DropdownMenuSubTrigger: box,
  DropdownMenuTrigger: ({ children }: { children?: unknown }) => box({ children }),
  ContextMenu: ({ children }: { children?: unknown }) => box({ children }),
  ContextMenuContent: none,
  ContextMenuItem: none,
  ContextMenuSeparator: none,
  ContextMenuTrigger: ({ children }: { children?: unknown }) => box({ children }),
  Input: (props: Record<string, unknown>) => box({ ...props, 'data-input': '1' }),
  Switch: none,
  Skeleton: none,
  toast: { success: vi.fn(), error: vi.fn() },
}))

vi.mock('@hanzo/ui/agents', () => ({
  Steer: ({ onCommand, withhold }: { onCommand: (c: string) => void; withhold?: readonly string[] }) =>
    box({
      'data-slot': 'steer-bar',
      'data-withhold': (withhold ?? []).join(','),
      children: btn({ 'data-slot': 'steer-stop', onClick: () => onCommand('stop'), children: 'Stop' }),
    }),
  Console: none,
  DEVICES: [],
  PreviewFrame: ({ src, title }: { src?: string | null; title?: string }) =>
    box({ 'data-slot': 'preview-frame', 'data-src': src ?? '', 'data-title': title ?? '' }),
  Views: box,
  Workspace: ({ start, middle, end, chat, children }: Record<string, unknown>) =>
    h('div', { 'data-slot': 'workspace' }, start as never, middle as never, end as never, chat as never, children as never),
  Attachments: none,
  CHAT: { id: 'chat', label: 'Chat' },
  Feedback: none,
  FileTabs: none,
  FileTree: none,
  PageSelect: none,
  ProjectChip: box,
  VIEWS: [],
  fold: () => [],
}))

vi.mock('@hanzo/ui/chat', () => ({
  Composer: (props: Record<string, unknown>) => {
    state.composerProps = props
    return box({
      'data-slot': 'composer',
      'data-busy': String(Boolean(props.busy)),
      'data-has-stop': String(typeof props.onStop === 'function'),
      'data-placeholder': String(props.placeholder ?? ''),
      'data-label': String(props.label ?? ''),
      children: props.children,
    })
  },
  Failure: box,
  Message: box,
  measure: {},
  SidebarItem: box,
  SidebarSection: ({ label, children }: { label?: string; children?: unknown }) => box({ 'data-section': label, children }),
  StatusDot: none,
}))

vi.mock('@hanzo/ui/product', () => ({ HanzoMark: none }))
vi.mock('@hanzo/ui/product/useLimits', () => ({ useLimits: () => ({ limits: null }) }))
vi.mock('@hanzo/voice', () => ({ useDictation: () => ({}), useVoice: () => ({}) }))
vi.mock('@hanzogui/lucide-icons-2', () =>
  Object.fromEntries(
    [
      'AlertCircle', 'ArrowLeft', 'ArrowRight', 'AudioLines', 'Bell', 'BookOpen', 'Bot', 'Box', 'Brain',
      'Check', 'ChevronDown', 'ChevronRight', 'Circle', 'CircleDot', 'Clapperboard', 'Clock', 'Code',
      'CodeXml', 'Columns2', 'Container', 'Copy', 'Cpu', 'CreditCard', 'Download', 'ExternalLink', 'File',
      'FileDiff', 'FileText', 'Folder', 'FolderGit2', 'Gauge', 'GitBranch', 'GitPullRequest', 'Globe',
      'Image', 'Info', 'Key', 'Laptop', 'Layers', 'LayoutTemplate', 'Library', 'Maximize2', 'Menu',
      'MessageSquare', 'Minimize2', 'Minus', 'MonitorPlay', 'MoreHorizontal', 'MousePointerClick', 'PanelLeft',
      'PanelLeftOpen', 'Paperclip', 'Pencil', 'Pin', 'Play', 'Plug', 'Plus', 'Presentation', 'Puzzle', 'RefreshCcw',
      'Rocket', 'RotateCcw', 'RotateCw', 'Search', 'Settings', 'Settings2', 'Share', 'Share2', 'Shield',
      'SlidersHorizontal', 'Sparkles', 'Square', 'SquarePen', 'SquareTerminal', 'Upload', 'User', 'Users',
      'Video', 'WandSparkles', 'X',
    ].map((n) => [n, none]),
  ),
)

vi.mock('./host.tsx', () => ({
  useHost: () => ({
    name: 'Hanzo Dev',
    api: 'https://api.hanzo.ai',
    token: () => 'tok',
    org: 'hanzo',
    person: { name: 'Dev', email: 'dev@hanzo.ai', avatar: '' },
    admin: true,
    path: SESSION_A,
    go: (p: string) => state.goCalls.push(p),
    links: { github: '', customize: '', settings: '', home: '' },
    open: vi.fn(),
  }),
  useTarget: () => ({ api: 'https://api.hanzo.ai', token: () => 'tok', org: 'hanzo' }),
  useBrowse: () => null,
  useBrowser: () => null,
}))

vi.mock('./api/coding.ts', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>
  return {
    ...actual,
    steer: vi.fn(async (_t: unknown, session: string, text: string) => {
      state.steerCalls.push({ session, text })
    }),
    start: vi.fn(async (_t: unknown, ask: Record<string, unknown>) => {
      state.startCalls.push(ask)
      if (state.startError) throw state.startError
      return { session: SESSION_B, repo: 'hanzoai/demo', branch: 'agent/abc', routed: false, target: '' }
    }),
  }
})

vi.mock('./api/sessions.ts', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>
  return {
    ...actual,
    stop: vi.fn(async (_t: unknown, id: string, why?: string) => {
      state.stopCalls.push({ id, why })
    }),
    pause: vi.fn(async () => {}),
    resume: vi.fn(async () => {}),
    list: vi.fn(async () => []),
    get: vi.fn(async (_t: unknown, id: string) => ({ id, title: 'Earlier', status: 'done', repo: 'hanzoai/demo', mode: 'build', recent: [] })),
  }
})

vi.mock('./data.ts', () => ({
  useKept: <T,>(_k: string, initial: T) => [initial, vi.fn()] as const,
  useProjects: () => ({
    value: [{ slug: 'instrument-10', name: 'Instrument', repo: 'https://git.hanzo.ai/hanzo/instrument.git', branch: 'main', status: 'live', live: 'https://instrument-10.hanzo.app', visibility: 'public', forked: '', created: 0, updated: 0 }],
    error: null,
    loading: false,
    reload: vi.fn(),
  }),
  useRead: <T,>(_fn: unknown, initial: T) => ({ value: initial, error: null, loading: false, reload: vi.fn() }),
  useRun: () => state.runState,
  useRecents: () => ({ value: [], error: null, loading: false, reload: vi.fn() }),
}))

vi.mock('./mind.ts', () => ({
  level: () => 'medium',
  pacing: () => null,
  useMind: () => ({ model: 'enso-auto', effort: 'medium' }),
}))

vi.mock('./spend.tsx', () => ({
  useSpend: () => ({ ask: vi.fn(async () => true), dialog: null }),
  useWake: () => ({ wake: vi.fn(async () => true), dialog: null }),
}))

vi.mock('./carry.tsx', () => ({
  useCarry: () => ({
    text: state.carryText,
    on: [],
    gear: { gear: { research: false, memory: false }, set: vi.fn() },
    skills: { rows: [], pick: vi.fn() },
    design: null,
    plugins: { rows: [], pick: vi.fn() },
    dialog: null,
  }),
}))

vi.mock('./foot.tsx', () => ({ DOCS: 'https://docs.hanzo.ai/docs/dev' }))
vi.mock('./scheduled.tsx', () => ({ schedule: vi.fn() }))
vi.mock('./gutter.tsx', () => ({ Gutter: none }))
vi.mock('./ask.tsx', () => ({ Confirm: none }))
vi.mock('./files.tsx', () => ({ Artifacts: none, Files: none }))
vi.mock('./git.tsx', () => ({ Git: none, Merge: none }))
vi.mock('./environment.tsx', () => ({ Environment: none }))
vi.mock('./door.tsx', () => ({ Door: none }))
vi.mock('./awake.tsx', () => ({ KeepAwake: none, useKeepAwake: () => [false, vi.fn()] as const }))
vi.mock('./dev-mode.ts', () => ({ useDevMode: () => [false, vi.fn()] as const, formatDeviceLabel: () => ({ title: 'Cloud workspace', detail: '' }) }))
vi.mock('./voice.ts', () => ({ useEar: () => vi.fn(), useSpoken: () => 'en' }))
vi.mock('./reply.tsx', () => ({ speaker: () => vi.fn() }))
vi.mock('./transcript.tsx', () => ({ Transcript: ({ cards }: { cards: unknown[] }) => box({ 'data-slot': 'transcript', 'data-cards': String(cards.length) }) }))
vi.mock('./side.tsx', () => ({
  SideProvider: ({ children }: { children?: unknown }) => box({ children }),
  useSide: () => null,
  useSplit: () => ({ side: { main: 'run', hidden: false, show: vi.fn(), full: false, fill: vi.fn() }, main: vi.fn(), aside: vi.fn(), grip: null }),
}))
vi.mock('./tabs.ts', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>
  return {
    ...actual,
    useDeck: (_key: string, seed: { tabs: Array<{ id: string; kind: string; title: string }>; at: string }) => ({
      tabs: seed.tabs,
      at: seed.at,
      open: vi.fn(),
      pick: vi.fn(),
      shut: vi.fn(),
      pin: vi.fn(),
      unpin: vi.fn(),
      move: vi.fn(),
      go: vi.fn(),
      back: vi.fn(),
      forward: vi.fn(),
      rename: vi.fn(),
    }),
    useKey: vi.fn(),
    usePanel: (_k: string, initial: boolean, wide: boolean) => [wide ? initial : false, vi.fn()] as const,
  }
})

const { Run } = await import('./run.tsx')
const { Panel } = await import('./panel.tsx')
const { Desk, SEED } = await import('./desk.tsx')
const { DevSection } = await import('./section.tsx')
const { Project } = await import('./project.tsx')

afterEach(() => {
  resetThreadLinks()
  state.wide = true
  state.carryText = ''
  state.composerProps = null
  state.goCalls = []
  state.steerCalls = []
  state.stopCalls = []
  state.startCalls = []
  state.startError = null
})

describe('session terminology and route aliases', () => {
  it('supports /dev/sessions/:id and /dev/session/:id while preserving /dev/run/:id', () => {
    expect(parse(`/run/${SESSION_A}`)).toEqual({ kind: 'run', id: SESSION_A })
    expect(parse(`/sessions/${SESSION_A}`)).toEqual({ kind: 'run', id: SESSION_A })
    expect(parse(`/session/${SESSION_A}`)).toEqual({ kind: 'run', id: SESSION_A })
    expect(href({ kind: 'run', id: SESSION_A })).toBe(`/run/${SESSION_A}`)
    expect(route(SESSION_A)).toEqual({ kind: 'run', id: SESSION_A })
  })

  it('uses New session and No sessions yet. in DevSection and sidebar components', () => {
    const html = renderToStaticMarkup(
      h(DevSection, {
        host: {
          name: 'Hanzo Dev',
          api: 'https://api.hanzo.ai',
          token: () => 'tok',
          org: 'hanzo',
          person: { name: 'Dev', email: 'dev@hanzo.ai', avatar: '' },
          admin: true,
          path: '',
          go: () => {},
          links: { github: '', customize: '', settings: '', home: '' },
          open: () => {},
        },
      }),
    )
    expect(html).toContain('New session')
    expect(html).toContain('No sessions yet.')
    expect(html).toContain('data-section="Sessions"')
    expect(html).not.toContain('New run')
    expect(html).not.toContain('No runs yet.')

    expect(here('builder.tsx')).toContain("say('New session')")
    expect(here('builder.tsx')).toContain("say('No sessions yet.')")
    expect(here('find.tsx')).toContain("say('Find a session')")
    expect(here('shelf.tsx')).toContain("say('New session')")
  })
})

describe('active session Stop button, mid-flight Steer, and single-thread follow-up', () => {
  it('stays active after a started event, shows Stop in composer, and calls POST /v1/agent/sessions/:id/stop', async () => {
    state.runState = {
      detail: { value: { id: SESSION_A, events: 2 }, error: null, loading: false, reload: vi.fn() },
      record: {
        id: SESSION_A,
        title: 'Rebrand site',
        status: 'running',
        repo: 'hanzoai/instrument',
        project: 'instrument-10',
        environment: 'sandbox',
        branch: 'agent/abc123',
        base: 'main',
        mode: 'build',
        pr: '',
        sandbox: 'sbx_1',
        channel: '',
        parent: '',
        root: SESSION_A,
      },
      // Reproduces the exact screenshot bug: status event `started` arrived while session is running.
      events: [
        { id: 'e1', sessionId: SESSION_A, seq: 1, kind: 'status', actor: 'agent', payload: { status: 'started', branch: 'agent/abc123' }, createdAt: '2026-11-27T10:00:00Z' },
      ],
      status: 'running',
      refused: '',
    }

    const html = renderToStaticMarkup(h(Run, { id: SESSION_A }))
    expect(html).toContain('data-slot="steer-bar"')
    expect(html).toContain('This session is still working.')
    expect(html).toContain('data-busy="true"')
    expect(html).toContain('data-has-stop="true"')
    expect(html).toContain('data-label="Steer this session"')

    // Clicking Stop in composer calls POST /v1/agent/sessions/:id/stop.
    const onStop = state.composerProps?.onStop as (() => void) | undefined
    expect(typeof onStop).toBe('function')
    onStop?.()
    await Promise.resolve()
    expect(state.stopCalls).toEqual([{ id: SESSION_A, why: undefined }])
  })

  it('submitting a message while running steers via /v1/agent/coding/:id/steer without calling start()', async () => {
    state.carryText = "I'm still seeing Noa Lindgren"
    state.runState = {
      detail: { value: { id: SESSION_A, events: 1 }, error: null, loading: false, reload: vi.fn() },
      record: {
        id: SESSION_A,
        title: 'Rebrand site',
        status: 'running',
        repo: 'hanzoai/instrument',
        project: 'instrument-10',
        environment: 'sandbox',
        branch: 'agent/abc123',
        base: 'main',
        mode: 'build',
        pr: '',
        sandbox: 'sbx_1',
        channel: '',
        parent: '',
        root: SESSION_A,
      },
      events: [{ id: 'e1', sessionId: SESSION_A, seq: 1, kind: 'status', actor: 'agent', payload: { status: 'started' }, createdAt: '2026-11-27T10:00:00Z' }],
      status: 'running',
      refused: '',
    }

    renderToStaticMarkup(h(Run, { id: SESSION_A }))
    const onSend = state.composerProps?.onSend as (() => void) | undefined
    expect(typeof onSend).toBe('function')
    onSend?.()
    await new Promise((r) => setTimeout(r, 0))
    expect(state.steerCalls).toEqual([{ session: SESSION_A, text: "I'm still seeing Noa Lindgren" }])
    expect(state.startCalls).toHaveLength(0)
  })

  it('continuing a finished session chains in the same thread without calling host.go(), and falls back to steer on 409 already-running race', async () => {
    state.carryText = 'Update the hero headline'
    state.runState = {
      detail: { value: { id: SESSION_A, events: 2 }, error: null, loading: false, reload: vi.fn() },
      record: {
        id: SESSION_A,
        title: 'Rebrand site',
        status: 'done',
        repo: 'hanzoai/instrument',
        project: 'instrument-10',
        environment: 'sandbox',
        branch: 'agent/abc123',
        base: 'main',
        mode: 'build',
        pr: '',
        sandbox: 'sbx_1',
        channel: '',
        parent: '',
        root: SESSION_A,
      },
      events: [{ id: 'e2', sessionId: SESSION_A, seq: 2, kind: 'status', actor: 'agent', payload: { status: 'done', changed: true }, createdAt: '2026-11-27T10:02:00Z' }],
      status: 'done',
      refused: '',
    }

    renderToStaticMarkup(h(Run, { id: SESSION_A }))
    const onSend = state.composerProps?.onSend as (() => void) | undefined
    onSend?.()
    await new Promise((r) => setTimeout(r, 0))
    expect(state.startCalls).toHaveLength(1)
    expect(state.startCalls[0]).toMatchObject({ after: SESSION_A, prompt: 'Update the hero headline' })
    // Single-thread continuity: stays on the same thread instead of navigating away to a new chat.
    expect(state.goCalls).toHaveLength(0)
    expect(threadChain(SESSION_A)).toEqual([SESSION_A, SESSION_B])

    // Race fallback: if backend still reports running on follow-up, transparently steer instead of failing.
    resetThreadLinks()
    state.startCalls = []
    state.steerCalls = []
    state.startError = new Error('coding: the run [REDACTED-TOKEN] is running; stop it or let it finish before carrying it on')
    renderToStaticMarkup(h(Run, { id: SESSION_A }))
    const onSendRace = state.composerProps?.onSend as (() => void) | undefined
    onSendRace?.()
    await new Promise((r) => setTimeout(r, 0))
    expect(state.steerCalls).toEqual([{ session: SESSION_A, text: 'Update the hero headline' }])
  })

  it('wires steer on active sandbox sessions and keeps follow-up inside the same thread without remounting', () => {
    const runSrc = here('run.tsx')
    // Active sandbox session steers via POST /v1/agent/coding/{session}/steer.
    expect(runSrc).toContain('await steer(t, activeId, words)')
    // Fallback when start() hits 409 "is running; stop it or let it finish": steers instead of throwing.
    expect(runSrc).toContain('if (!fork && isAlreadyRunningError(e) && said.trim())')
    // Follow-up links into the same thread instead of calling host.go(run.session).
    expect(runSrc).toContain('linkThread(activeId, run.session)')
    expect(runSrc).toContain('linkThread(activeId, next)')

    // Project workspace also steers sandbox runs via coding steer endpoint and links threads.
    const projSrc = here('project.tsx')
    expect(projSrc).toContain('await steerCoding(target, id, words)')
    expect(projSrc).toContain('if (current && isAlreadyRunningError(e))')
    expect(projSrc).toContain('linkThread(current, next.session)')

    // Verify thread linking keeps one chain when continuing a session.
    linkThread(SESSION_A, SESSION_B)
    expect(threadChain(SESSION_A)).toEqual([SESSION_A, SESSION_B])
  })
})

describe('narrow-viewport tab strip, shell top-right inset, and mobile responsive layout', () => {
  it('renders Panel tabs with flex-shrink: 0, scrollable tablist, and shell top-right inset', () => {
    const html = renderToStaticMarkup(
      h(Panel, {
        side: {
          tabs: SEED.tabs,
          at: 'preview',
          open: vi.fn(),
          pick: vi.fn(),
          shut: vi.fn(),
          pin: vi.fn(),
          unpin: vi.fn(),
          move: vi.fn(),
          go: vi.fn(),
          back: vi.fn(),
          forward: vi.fn(),
          rename: vi.fn(),
        } as never,
        kinds: [
          { id: 'preview', label: 'Preview', render: () => null },
          { id: 'artifacts', label: 'Artifacts', render: () => null },
          { id: 'files', label: 'Files', render: () => null },
          { id: 'diff', label: 'Changes', render: () => null },
          { id: 'terminal', label: 'Activity', render: () => null },
        ],
        onHide: vi.fn(),
      }),
    )

    // Every tab keeps flex-shrink: 0 so labels stay readable instead of squashing to P... / Ac.
    expect(html).toContain('data-slot="side-tab"')
    expect(html).toContain('data-shrink="0"')
    expect(html).toContain('flex-shrink:0')
    expect(html).toContain('data-overflow-x="auto"')
    // Panel strip reserves space for the host shell top-right buttons.
    expect(html).toContain('--hanzo-shell-top-right-inset')
    for (const label of ['Preview', 'Artifacts', 'Files', 'Changes', 'Activity']) {
      expect(html).toContain(label)
    }
  })

  it('applies --hanzo-shell-top-right-inset to Project header actions (Share / Publish)', () => {
    const html = renderToStaticMarkup(h(Project, { at: { slug: 'instrument-10' } }))
    expect(html).toContain('data-slot="project-header-actions"')
    expect(html).toContain('--hanzo-shell-top-right-inset')
    expect(html).toContain('Share')
    expect(html).toContain('Publish')
  })

  it('renders a touch-friendly Session / Workspace segmented switcher on mobile (< 768px)', () => {
    state.wide = false
    state.runState = {
      detail: { value: { id: SESSION_A, events: 1 }, error: null, loading: false, reload: vi.fn() },
      record: {
        id: SESSION_A,
        title: 'Mobile session',
        status: 'done',
        repo: 'hanzoai/instrument',
        project: 'instrument-10',
        environment: 'sandbox',
        branch: 'agent/abc123',
        base: 'main',
        mode: 'build',
        pr: '',
        sandbox: 'sbx_1',
        channel: '',
        parent: '',
        root: SESSION_A,
      },
      events: [],
      status: 'done',
      refused: '',
    }

    const html = renderToStaticMarkup(h(Run, { id: SESSION_A }))
    expect(html).toContain('data-slot="mobile-pane-switcher"')
    expect(html).toContain('role="tablist"')
    expect(html).toContain('data-min-h="40"')
    expect(html).toContain('Session')
    expect(html).toContain('Workspace')
    // Run header also respects host shell top-right inset when side panel is not beside it.
    expect(html).toContain('--hanzo-shell-top-right-inset')
  })

  it('shows Live site badge and unpublished-edits Publish / Start preview banner in Desk Preview', () => {
    const html = renderToStaticMarkup(
      h(Desk, {
        id: SESSION_A,
        repo: 'hanzoai/instrument',
        branch: 'agent/abc123',
        base: 'main',
        environment: 'sandbox',
        mode: 'build',
        pr: { href: '', label: '' },
        title: 'Rebrand site',
        project: 'instrument-10',
        sandbox: 'sbx_1',
        site: 'https://instrument-10.hanzo.app',
        published: 0,
        sessionStatus: 'done',
        events: [
          {
            id: 'e2',
            sessionId: SESSION_A,
            seq: 2,
            kind: 'event',
            actor: 'agent',
            payload: { type: 'item.completed', item: { type: 'file_change', status: 'completed' } },
            createdAt: '2026-11-27T10:01:00Z',
          },
          {
            id: 'e3',
            sessionId: SESSION_A,
            seq: 3,
            kind: 'status',
            actor: 'agent',
            payload: { status: 'done', changed: true },
            createdAt: '2026-11-27T10:02:00Z',
          },
        ],
        live: false,
        menu: [],
        refused: '',
        retry: 'Retry',
        onRetry: vi.fn(),
        onPublish: vi.fn(),
        side: {
          tabs: SEED.tabs,
          at: 'preview',
          open: vi.fn(),
          pick: vi.fn(),
          shut: vi.fn(),
          pin: vi.fn(),
          unpin: vi.fn(),
          move: vi.fn(),
          go: vi.fn(),
          back: vi.fn(),
          forward: vi.fn(),
          rename: vi.fn(),
        } as never,
        sheet: false,
        onHide: vi.fn(),
      }),
    )

    expect(html).toContain('data-slot="preview-mode"')
    expect(html).toContain('Live site')
    expect(html).toContain('data-slot="preview-stale-banner"')
    expect(html).toContain('Publish changes')
  })
})
