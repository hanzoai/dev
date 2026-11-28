import { createElement, type ReactNode } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it, vi } from 'vitest'

// The drawing libraries are not under test and do not load outside a bundler:
// each draws its children, and a pressable one a button, so the markup reads.
vi.mock('@hanzo/gui', () => {
  const Draw = ({ children, role, onPress }: { children?: ReactNode; role?: string; onPress?: () => void }) =>
    createElement(onPress ? 'button' : 'div', role ? { role } : null, children)
  return { SizableText: Draw, XStack: Draw, YStack: Draw }
})
vi.mock('@hanzo/ui', () => ({ Popover: () => null, PopoverContent: () => null, PopoverTrigger: () => null }))
vi.mock('@hanzogui/lucide-icons-2', () => ({ FilePlus2: () => null, ListTodo: () => null }))

vi.mock('@hanzo/i18n/react', () => ({ useI18n: () => ({ t: (s: string) => s }) }))

const { kinds, Summary } = await import('./summary.tsx')

const SECTIONS = [
  { id: 'outputs', title: 'Outputs', lead: { title: 'Create file or site', run: () => {} }, entries: [{ id: 'o1', title: 'index.html', open: () => {} }] },
  { id: 'sources', title: 'Sources', empty: 'Nothing yet', entries: [] },
]

describe('the summary', () => {
  it('makes each section a kind of tab, in order, under its own title', () => {
    expect(kinds(SECTIONS).map((k) => [k.id, k.label])).toEqual([
      ['outputs', 'Outputs'],
      ['sources', 'Sources'],
    ])
  })

  it('draws a section’s lead, its rows, and its empty line only when it has none', () => {
    const [outputs, sources] = kinds(SECTIONS).map((k) => renderToStaticMarkup(createElement('div', null, k.render())))
    expect(outputs).toContain('Create file or site')
    expect(outputs).toContain('index.html')
    expect(outputs).not.toContain('Nothing yet')
    expect(sources).toContain('Nothing yet')
  })

  it('renders a docked inline pane with all sections when pane is true', () => {
    const html = renderToStaticMarkup(createElement(Summary, { sections: SECTIONS, pane: true }))
    expect(html).toContain('Outputs')
    expect(html).toContain('Create file or site')
    expect(html).toContain('index.html')
    expect(html).toContain('Sources')
    expect(html).toContain('Nothing yet')
  })
})
