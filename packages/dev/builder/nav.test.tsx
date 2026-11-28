import { describe, expect, it, vi } from 'vitest'

import type { Host } from './host.tsx'
import { library, nav, NEW, own, sections, tabs } from './nav.tsx'
import { route } from './route.ts'

// The marks are react-native-svg drawings, which a node test cannot load; what a place is and where it goes is plain data.
vi.mock('@hanzogui/lucide-icons-2', () =>
  Object.fromEntries(
    ['AudioLines', 'Bell', 'BookOpen', 'Bot', 'Box', 'Brain', 'CircleDot', 'Clapperboard', 'Clock', 'Code', 'CodeXml', 'Container', 'Cpu', 'CreditCard', 'File', 'FileText', 'FolderGit2', 'Gauge', 'Globe', 'Image', 'Key', 'LayoutTemplate', 'Library', 'MessageSquare', 'Plug', 'Plus', 'Presentation', 'Puzzle', 'Settings', 'Shield', 'SlidersHorizontal', 'Sparkles', 'SquarePen', 'SquareTerminal', 'Upload', 'User', 'Users', 'Video', 'WandSparkles'].map((n) => [n, { [n]: () => null }[n]]),
  ),
)
vi.mock('./foot.tsx', () => ({ DOCS: 'https://docs.hanzo.ai/docs/dev' }))

const host = (path = ''): Host => ({
  name: 'Hanzo Dev',
  api: 'https://api.hanzo.ai',
  token: () => 'tok',
  org: 'acme',
  person: { name: 'Dave', email: 'dave@acme.test', avatar: '' },
  admin: true,
  path,
  go: () => {},
  links: { github: '', customize: '', settings: '', home: '' },
  open: () => {},
})

describe('the builder’s places, once for every rail', () => {
  it('lists every place, grouped in the order a customer asks, and nothing behind a More', () => {
    expect(nav(host(), () => {}).map((g) => [g.label, g.links.map((l) => l.label)])).toEqual([
      ['Work', ['New session', 'Projects', 'Issues', 'Templates']],
      ['Make', ['Library', 'Scheduled tasks']],
      ['Run', ['Machines', 'Environments']],
      ['Setup', ['Customize', 'Docs']],
    ])
  })

  it('says each place in the caller’s language, and English with no caller', () => {
    const FR: Record<string, string> = { Work: 'Travail', 'New session': 'Nouvelle session', Projects: 'Projets', Docs: 'Docs', Agents: 'Agents', Skills: 'Compétences', Upload: 'Importer', 'Make something new': 'Créer quelque chose' }
    const t = ((source: string) => FR[source] ?? source) as Parameters<typeof nav>[2]
    const work = nav(host(), () => {}, t)[0]!
    expect([work.label, work.links.map((l) => l.label)]).toEqual(['Travail', ['Nouvelle session', 'Projets', 'Issues', 'Templates']])
    expect(own(host(), () => {}, t)[0]!.label).toBe('Travail')
    expect(tabs(host(), () => {}, t)[0]!.links.map((l) => l.label)).toEqual(['Agents', 'Bots', 'Compétences', 'Connectors', 'Plugins'])
    expect(library(host(), () => {}, t)[0]!.links.map((l) => l.label)).toEqual(['Créer quelque chose', 'Importer'])
  })

  it('moves to an address the grammar reads back as that place', () => {
    // Issues forgets the board it last pinned, in this browser: one with nothing kept.
    vi.stubGlobal('window', { dispatchEvent: () => true })
    const seen: string[] = []
    const all = nav(host(), (p) => seen.push(p)).flatMap((g) => g.links)
    for (const l of all.filter((l) => l.id !== 'docs')) l.onPress?.()
    expect(seen.map((p) => route(p))).toEqual([
      { kind: 'new' },
      { kind: 'screen', screen: 'projects' },
      { kind: 'screen', screen: 'issues' },
      { kind: 'screen', screen: 'templates' },
      { kind: 'screen', screen: 'artifacts' },
      { kind: 'screen', screen: 'automations' },
      { kind: 'settings', section: 'machines' },
      { kind: 'settings', section: 'environments' },
      { kind: 'customize', tab: 'agents' },
    ])
    vi.unstubAllGlobals()
  })

  it('opens the docs in a tab of their own, not in the builder', () => {
    const open = vi.fn()
    vi.stubGlobal('window', { open })
    const go = vi.fn()
    nav(host(), go)
      .flatMap((g) => g.links)
      .find((l) => l.id === 'docs')
      ?.onPress?.()
    expect(go).not.toHaveBeenCalled()
    expect(open).toHaveBeenCalledWith('https://docs.hanzo.ai/docs/dev', '_blank', 'noopener,noreferrer')
    vi.unstubAllGlobals()
  })

  it.each([
    ['', NEW],
    ['-/projects', 'projects'],
    ['-/sync', 'projects'],
    ['acme/shop', 'projects'],
    ['-/automations/new', 'automations'],
    ['-/settings/machines', 'machines'],
    ['-/settings/environments', 'environments'],
    ['-/customize/agents', 'customize'],
    ['-/customize/agents/dev', 'customize'],
    ['-/mcp', 'customize'],
  ])('marks %j as %s, and only that one', (path, id) => {
    const lit = nav(host(path), () => {})
      .flatMap((g) => g.links)
      .filter((l) => l.active)
      .map((l) => l.id)
    expect(lit).toEqual([id])
  })
})

describe('a host’s sidebar for Dev, Settings and Customize', () => {
  it('lists Dev’s own places alone where the rail holds the rest', () => {
    expect(own(host(), () => {}).flatMap((g) => g.links.map((l) => l.label))).toEqual(['New session', 'Projects', 'Issues', 'Templates'])
  })

  it('lists Settings’ sections, labelling a part only where its first row does not already say it', () => {
    const groups = sections(host('-/settings/keys'), () => {})
    expect(groups.map((g) => [g.label, g.links.map((l) => l.label)])).toEqual([
      ['', ['General', 'Account', 'Privacy', 'Billing', 'Usage', 'Capabilities', 'Memory']],
      ['', ['Code', 'Environments', 'Machines', 'API keys']],
      ['Organization', ['Members', 'Integrations', 'Notifications']],
    ])
    expect(groups.flatMap((g) => g.links).filter((l) => l.active).map((l) => l.id)).toEqual(['keys'])
  })

  it('lists Customize’s tabs Agents first, an agent’s page lighting Agents', () => {
    const groups = tabs(host('-/customize/agents/dev'), () => {})
    expect(groups[0]!.links.map((l) => [l.label, Boolean(l.active)])).toEqual([
      ['Agents', true],
      ['Bots', false],
      ['Skills', false],
      ['Connectors', false],
      ['Plugins', false],
    ])
  })
})

describe('the Library’s sidebar', () => {
  it('is Make something new and Upload, then every kind, the one in the address lit, each moving to its own address', () => {
    const seen: string[] = []
    const groups = library(host('-/artifacts/image'), (p) => seen.push(p))
    expect(groups.map((g) => g.links.map((l) => l.label))).toEqual([['Make something new', 'Upload'], ['All', 'Images', 'Videos', 'Audio', 'Documents', 'Slides', '3D', 'Code', 'Files', 'Sites']])
    expect(groups[1]!.links.filter((l) => l.active).map((l) => l.label)).toEqual(['Images'])
    for (const l of groups[1]!.links) l.onPress?.()
    expect(seen.slice(0, 3)).toEqual(['-/artifacts', '-/artifacts/image', '-/artifacts/video'])
    expect(library(host('-/artifacts'), () => {})[1]!.links.filter((l) => l.active).map((l) => l.label)).toEqual(['All'])
  })

  it('opens the template picker at its own address, lit while it is open', () => {
    const seen: string[] = []
    library(host('-/artifacts'), (p) => seen.push(p))[0]!.links[0]!.onPress?.()
    expect(seen.map(route)).toEqual([{ kind: 'screen', screen: 'artifacts', of: 'new' }])
    const open = library(host('-/artifacts/new'), () => {})
    expect(open.flatMap((g) => g.links).filter((l) => l.active).map((l) => l.id)).toEqual(['make'])
  })
})
