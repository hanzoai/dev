import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { afterEach, describe, expect, it, vi } from 'vitest'

import { Refusal } from './api/call.ts'

const state = vi.hoisted(() => ({
  openCalls: [] as string[],
  goCalls: [] as string[],
  buttonHandlers: new Map<string, () => void>(),
  readQueue: [] as Array<{ value: unknown; loading?: boolean; error?: Error | null }>,
  readIndex: 0,
  connectUrl: 'https://github.com/login/oauth/authorize?client_id=hanzo&state=xyz',
  connectCalls: [] as Array<{ back?: string }>,
  createCalls: [] as Array<Record<string, unknown>>,
  createError: null as Error | null,
  forkCalls: [] as Array<Record<string, unknown>>,
  attachCalls: [] as Array<{ name: string; url: string }>,
  projects: [
    {
      slug: 'my-landing',
      name: 'My Landing',
      repo: 'https://git.hanzo.ai/hanzo/my-landing.git',
      branch: 'main',
      status: 'live',
      live: 'https://my-landing.hanzo.app',
      visibility: 'public',
      forked: '',
      created: 0,
      updated: 0,
    },
  ] as Array<Record<string, unknown>>,
  gitTargets: [] as Array<{ id: string; repo: string; host: string; url: string; createdAt: string }>,
}))

const { box, text, btn, input, none } = vi.hoisted(() => {
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
        k === 'disabled' ||
        k === 'placeholder' ||
        k === 'value' ||
        k.startsWith('data-') ||
        k.startsWith('aria-')
      ) {
        out[k] = v
      }
    }
    const slot = typeof props['data-slot'] === 'string' ? props['data-slot'] : ''
    const aria = typeof props['aria-label'] === 'string' ? props['aria-label'] : ''
    if (typeof props.onPress === 'function') {
      if (slot) state.buttonHandlers.set(slot, props.onPress as () => void)
      if (aria) state.buttonHandlers.set(aria, props.onPress as () => void)
    }
    return out
  }
  return {
    box: (props: Record<string, unknown>) => el(props.render === 'button' ? 'button' : 'div', clean(props)),
    text: (props: Record<string, unknown>) => el('span', clean(props)),
    btn: (props: Record<string, unknown>) => el('button', clean(props)),
    input: (props: Record<string, unknown>) => el('input', { ...clean(props), readOnly: true }),
    none: () => null,
  }
})

vi.mock('@hanzo/gui', () => ({
  SizableText: text,
  XStack: box,
  YStack: box,
  useMedia: () => ({ md: true, sm: true }),
}))

vi.mock('@hanzo/ui', () => ({
  Button: btn,
  Dialog: ({ open, children }: { open?: boolean; children?: unknown }) => (open ? box({ children }) : null),
  DialogContent: box,
  DialogDescription: box,
  DialogTitle: box,
  DropdownMenu: ({ trigger }: { trigger?: unknown }) => box({ children: trigger }),
  Input: input,
  Skeleton: none,
  toast: { success: vi.fn(), error: vi.fn() },
}))

vi.mock('@hanzo/ui/agents', () => ({
  Attachments: none,
  CHAT: { id: 'chat', label: 'Chat' },
  Console: none,
  DEVICES: [],
  Feedback: none,
  FileTabs: none,
  FileTree: none,
  PageSelect: none,
  PreviewFrame: none,
  ProjectChip: box,
  VIEWS: [],
  Views: box,
  Workspace: ({ start, middle, end, chat, children }: Record<string, unknown>) =>
    h('div', { 'data-slot': 'workspace' }, start as never, middle as never, end as never, chat as never, children as never),
  fold: () => [],
}))

vi.mock('@hanzo/ui/chat', () => ({ Composer: none, Message: box }))
vi.mock('@hanzo/ui/product', () => ({ HanzoMark: none }))
vi.mock('@hanzo/ui/product/useLimits', () => ({ useLimits: () => ({ limits: null }) }))
vi.mock('@hanzo/voice', () => ({ useDictation: () => ({}), useVoice: () => ({}) }))
vi.mock('@hanzogui/lucide-icons-2', () =>
  Object.fromEntries(
    [
      'AlertCircle', 'ArrowRight', 'ArrowUpRight', 'Check', 'Clock', 'ExternalLink', 'GitBranch', 'GitFork',
      'Github', 'Globe', 'Link2', 'Lock', 'MousePointerClick', 'PanelLeft', 'Plus', 'RefreshCcw', 'RefreshCw',
      'Search', 'Share2', 'Square', 'Upload', 'X',
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
    path: '',
    go: (p: string) => state.goCalls.push(p),
    open: (url: string) => state.openCalls.push(url),
    links: { github: '', customize: '', settings: '', home: '' },
  }),
  useTarget: () => ({ api: 'https://api.hanzo.ai', token: () => 'tok', org: 'hanzo' }),
}))

vi.mock('./back.ts', () => ({
  here: () => 'https://hanzo.ai/dev/projects',
  useBack: vi.fn(),
}))

vi.mock('./api/github.ts', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>
  return {
    ...actual,
    connect: vi.fn(async (_t: unknown, back?: string) => {
      state.connectCalls.push({ back })
      return state.connectUrl
    }),
    createGitHubRepo: vi.fn(async (_t: unknown, input: Record<string, unknown>) => {
      state.createCalls.push(input)
      if (state.createError) throw state.createError
      return {
        full_name: `mcphun-spec/${input.name}`,
        html_url: `https://github.com/mcphun-spec/${input.name}`,
        clone_url: `https://github.com/mcphun-spec/${input.name}.git`,
        default_branch: 'main',
        private: Boolean(input.private),
        existing: false,
      }
    }),
    forkGitHubRepo: vi.fn(async (_t: unknown, input: Record<string, unknown>) => {
      state.forkCalls.push(input)
      return {
        full_name: `mcphun-spec/${input.name || 'forked'}`,
        html_url: `https://github.com/mcphun-spec/${input.name || 'forked'}`,
        clone_url: `https://github.com/mcphun-spec/${input.name || 'forked'}.git`,
        default_branch: 'main',
        private: false,
        existing: false,
      }
    }),
    attachGitHubMirror: vi.fn(async (_t: unknown, name: string, url: string) => {
      state.attachCalls.push({ name, url })
      return { id: 'tgt_1', repo: name, host: 'github.com', url, createdAt: '' }
    }),
    bring: vi.fn(async () => ({ queued: 1, codebases: {} })),
  }
})

vi.mock('./data.ts', () => ({
  useKept: <T,>(_k: string, initial: T) => [initial, vi.fn()] as const,
  useProjects: () => ({
    value: state.projects,
    error: null,
    loading: false,
    reload: vi.fn(),
  }),
  useRead: <T,>(_fn: unknown, initial: T) => {
    if (state.readQueue.length > 0) {
      const next = state.readQueue[state.readIndex++]
      if (next) return { value: next.value as T, error: next.error ?? null, loading: next.loading ?? false, reload: vi.fn() }
    }
    if (Array.isArray(initial) && state.gitTargets.length > 0 && typeof _fn === 'function') {
      return { value: state.gitTargets as unknown as T, error: null, loading: false, reload: vi.fn() }
    }
    return { value: initial, error: null, loading: false, reload: vi.fn() }
  },
  useRun: () => ({
    detail: { value: null, error: null, loading: false, reload: vi.fn() },
    record: null,
    events: [],
    status: 'done',
    refused: '',
  }),
}))

vi.mock('./mind.ts', () => ({
  level: () => 'medium',
  pacing: () => null,
  useMind: () => ({ model: 'enso-auto', effort: 'medium' }),
}))
vi.mock('./spend.tsx', () => ({ useSpend: () => ({ ask: vi.fn(async () => true), dialog: null }) }))
vi.mock('./gutter.tsx', () => ({ Gutter: none }))
vi.mock('./git.tsx', () => ({ Merge: none }))
vi.mock('./voice.ts', () => ({ useEar: () => vi.fn(), useSpoken: () => 'en' }))
vi.mock('./publish.tsx', () => ({ Publish: none }))

const { GitHubBadge, GitHubOAuthCallout, GitHubReposModal, SyncToGitHubDialog } = await import('./github-sync.tsx')
const { Project } = await import('./project.tsx')

afterEach(() => {
  state.openCalls = []
  state.goCalls = []
  state.buttonHandlers.clear()
  state.readQueue = []
  state.readIndex = 0
  state.connectCalls = []
  state.createCalls = []
  state.createError = null
  state.forkCalls = []
  state.attachCalls = []
  state.gitTargets = []
  state.projects = [
    {
      slug: 'my-landing',
      name: 'My Landing',
      repo: 'https://git.hanzo.ai/hanzo/my-landing.git',
      branch: 'main',
      status: 'live',
      live: 'https://my-landing.hanzo.app',
      visibility: 'public',
      forked: '',
      created: 0,
      updated: 0,
    },
  ]
})

describe('GitHubReposModal — empty connected state and populated repo list', () => {
  it('renders Create + Fork actions when GitHub is connected with 0 repositories (@mcphun-spec)', () => {
    state.readQueue = [
      { value: { configured: true, connected: true, login: 'mcphun-spec' } }, // connection
      { value: { id: 'github', name: 'GitHub', available: true, connected: true, account: 'mcphun-spec', since: '', note: '' } }, // readProvider
      { value: [] }, // installations
      { value: { repos: [], next: '', total: 0, unread: [], connected: true } }, // listGitHubRepos
      { value: [] }, // codebases
      { value: [] }, // links
    ]

    const html = renderToStaticMarkup(h(GitHubReposModal, { open: true, onOpenChange: vi.fn() }))
    expect(html).toContain('data-slot="github-empty-state"')
    expect(html).toContain('@mcphun-spec')
    expect(html).toContain('Create a new GitHub repository')
    expect(html).toContain('Fork a GitHub repository')
    expect(html).toContain('data-slot="empty-create-github-repo"')
    expect(html).toContain('data-slot="empty-fork-github-repo"')
  })

  it('renders search, 1-click Import & edit in /dev, and Create/Fork tabs when repos exist', () => {
    state.readQueue = [
      { value: { configured: true, connected: true, login: 'hanzo-dev' } },
      { value: { id: 'github', name: 'GitHub', available: true, connected: true, account: 'hanzo-dev', since: '', note: '' } },
      { value: [{ login: 'hanzoai', type: 'Organization', grant: 'all', connected: true }] },
      {
        value: {
          repos: [
            {
              owner: 'hanzoai',
              name: 'dev',
              full_name: 'hanzoai/dev',
              private: false,
              default_branch: 'main',
              pushed_at: '',
              installation_id: 1,
              codebase: 'hanzoai_dev',
            },
          ],
          next: '',
          total: 1,
          unread: [],
          connected: true,
        },
      },
      { value: [] },
      { value: [] },
    ]

    const html = renderToStaticMarkup(h(GitHubReposModal, { open: true, onOpenChange: vi.fn() }))
    expect(html).toContain('data-slot="github-repo-list"')
    expect(html).toContain('hanzoai/dev')
    expect(html).toContain('Import &amp; edit in /dev')
    expect(html).toContain('Create new repo')
    expect(html).toContain('Fork repo')
  })
})

describe('412 user-OAuth fallback CTA', () => {
  it('invokes POST /v1/provider/github/user/connect and opens GitHub authorizeUrl when clicked', async () => {
    const html = renderToStaticMarkup(
      h(GitHubOAuthCallout, {
        message: 'Personal GitHub account requires OAuth authorization.',
        label: 'Authorize GitHub account',
      }),
    )
    expect(html).toContain('data-slot="github-oauth-fallback"')
    expect(html).toContain('Authorize GitHub account')

    const onPress = state.buttonHandlers.get('github-oauth-cta')
    expect(typeof onPress).toBe('function')
    onPress?.()
    await new Promise((r) => setTimeout(r, 0))

    expect(state.connectCalls).toEqual([{ back: 'https://hanzo.ai/dev/projects' }])
    expect(state.openCalls).toEqual([state.connectUrl])
  })
})

describe('Project Sync to GitHub flow and linked GitHub badge rendering', () => {
  it('renders Sync to GitHub button in Project header when project is Hanzo Git only', () => {
    const html = renderToStaticMarkup(h(Project, { at: { slug: 'my-landing' } }))
    expect(html).toContain('data-slot="project-sync-github"')
    expect(html).toContain('Sync to GitHub')
    expect(html).not.toContain('data-slot="github-badge"')
  })

  it('renders crisp GitHub badge/link (owner/repo) when project has an outbound GitHub mirror target', () => {
    state.gitTargets = [
      {
        id: 'tgt_99',
        repo: 'my-landing',
        host: 'github.com',
        url: 'https://github.com/mcphun-spec/my-landing.git',
        createdAt: '2026-11-28T00:00:00Z',
      },
    ]

    const html = renderToStaticMarkup(h(Project, { at: { slug: 'my-landing' } }))
    expect(html).toContain('data-slot="github-badge"')
    expect(html).toContain('data-repo="mcphun-spec/my-landing"')
    expect(html).toContain('https://github.com/mcphun-spec/my-landing')
    expect(html).not.toContain('data-slot="project-sync-github"')
  })

  it('renders GitHub badge directly when project repo is already on GitHub', () => {
    state.projects = [
      {
        slug: 'hanzo-dev',
        name: 'Hanzo Dev',
        repo: 'https://github.com/hanzoai/dev.git',
        branch: 'main',
        status: 'live',
        live: 'https://dev.hanzo.app',
        visibility: 'public',
        forked: '',
        created: 0,
        updated: 0,
      },
    ]

    const html = renderToStaticMarkup(h(Project, { at: { slug: 'hanzo-dev' } }))
    expect(html).toContain('data-slot="github-badge"')
    expect(html).toContain('hanzoai/dev')
    expect(html).not.toContain('data-slot="project-sync-github"')
  })

  it('SyncToGitHubDialog pre-fills project slug and calls createGitHubRepo with linkRepo on submit', async () => {
    const onSynced = vi.fn()
    const onOpenChange = vi.fn()

    const html = renderToStaticMarkup(
      h(SyncToGitHubDialog, {
        open: true,
        onOpenChange,
        repoName: 'my-landing',
        defaultName: 'my-landing',
        defaultDescription: 'Landing page',
        onSynced,
      }),
    )

    expect(html).toContain('data-slot="sync-to-github-dialog"')
    expect(html).toContain('Create new repo')
    expect(html).toContain('Link existing repo')
    expect(html).toContain('value="my-landing"')
    expect(html).toContain('Create &amp; sync to GitHub')

    const submit = state.buttonHandlers.get('create-github-repo-submit')
    expect(typeof submit).toBe('function')
    submit?.()
    await new Promise((r) => setTimeout(r, 0))

    expect(state.createCalls).toEqual([
      {
        name: 'my-landing',
        description: 'Landing page',
        private: true,
        org: undefined,
        linkRepo: 'my-landing',
      },
    ])
    expect(onSynced).toHaveBeenCalledWith({
      owner: 'mcphun-spec',
      name: 'my-landing',
      fullName: 'mcphun-spec/my-landing',
      htmlUrl: 'https://github.com/mcphun-spec/my-landing',
      source: 'target',
    })
    expect(onOpenChange).toHaveBeenCalledWith(false)
  })

  it('SyncToGitHubDialog handles 412 refusal from createGitHubRepo without closing dialog', async () => {
    state.createError = new Refusal(412, 'personal account needs GitHub user OAuth')
    const onSynced = vi.fn()
    const onOpenChange = vi.fn()

    renderToStaticMarkup(
      h(SyncToGitHubDialog, {
        open: true,
        onOpenChange,
        repoName: 'my-landing',
        defaultName: 'my-landing',
        onSynced,
      }),
    )

    const submit = state.buttonHandlers.get('create-github-repo-submit')
    submit?.()
    await new Promise((r) => setTimeout(r, 0))

    expect(state.createCalls).toHaveLength(1)
    expect(onSynced).not.toHaveBeenCalled()
    expect(onOpenChange).not.toHaveBeenCalledWith(false)
  })

  it('renders standalone GitHubBadge with accessible link and synced label', () => {
    const html = renderToStaticMarkup(
      h(GitHubBadge, {
        fullName: 'mcphun-spec/site',
        htmlUrl: 'https://github.com/mcphun-spec/site',
        syncedLabel: 'Synced',
      }),
    )
    expect(html).toContain('href="https://github.com/mcphun-spec/site"')
    expect(html).toContain('aria-label="Open mcphun-spec/site on GitHub"')
    expect(html).toContain('mcphun-spec/site')
    expect(html).toContain('Synced')
  })
})
