import { describe, expect, it, vi } from 'vitest'

import { MARKS } from './marks.tsx'
import { nav, NEW } from './nav.tsx'
import type { Host } from './host.tsx'

// Each mark is a named stand-in, so a test can say which drawing a place wears.
vi.mock('@hanzogui/lucide-icons-2', () =>
  Object.fromEntries(
    ['AudioLines', 'Bell', 'BookOpen', 'Bot', 'Box', 'Brain', 'CircleDot', 'Clapperboard', 'Clock', 'Code', 'CodeXml', 'Container', 'Cpu', 'CreditCard', 'File', 'FileText', 'FolderGit2', 'Gauge', 'Globe', 'Image', 'Key', 'LayoutTemplate', 'Library', 'MessageSquare', 'Plug', 'Plus', 'Presentation', 'Puzzle', 'Settings', 'Shield', 'SlidersHorizontal', 'Sparkles', 'SquarePen', 'SquareTerminal', 'Upload', 'User', 'Users', 'Video', 'WandSparkles'].map((n) => [n, { [n]: () => null }[n]]),
  ),
)
vi.mock('./foot.tsx', () => ({ DOCS: 'https://docs.hanzo.ai/docs/dev' }))

const host: Host = {
  name: 'Hanzo Dev',
  api: 'https://api.hanzo.ai',
  token: () => 'tok',
  org: 'acme',
  person: { name: 'Dave', email: 'dave@acme.test', avatar: '' },
  admin: true,
  path: '',
  go: () => {},
  links: { github: '', customize: '', settings: '', home: '' },
  open: () => {},
}

const type = (icon: unknown): string => (icon as { type: { name: string } }).type.name

describe('the marks, once', () => {
  it('Chat starts with the pen, Dev with `</>`, Tabs with the terminal', () => {
    expect(MARKS.chat.make.name).toBe('SquarePen')
    expect(MARKS.dev.make.name).toBe('CodeXml')
    expect(MARKS.tabs.make.name).toBe('SquareTerminal')
  })

  it('each place wears one icon: the same in the rail, the sidebars and the pages', () => {
    expect(MARKS.library.icon.name).toBe('Library')
    expect(MARKS.scheduled.icon.name).toBe('Clock')
    expect(MARKS.customize.icon.name).toBe('Puzzle')
    expect(MARKS.studio.icon.name).toBe('Clapperboard')
  })

  it('Dev’s sidebar draws its marks: New is `</>`, and Library, Scheduled tasks and Customize are the rail’s', () => {
    const links = nav(host, () => {}).flatMap((g) => g.links)
    const icon = (id: string) => type(links.find((l) => l.id === id)?.icon)
    expect(icon(NEW)).toBe(MARKS.dev.make.name)
    expect(icon('artifacts')).toBe(MARKS.library.icon.name)
    expect(icon('automations')).toBe(MARKS.scheduled.icon.name)
    expect(icon('customize')).toBe(MARKS.customize.icon.name)
  })
})
