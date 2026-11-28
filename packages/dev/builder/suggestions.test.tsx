import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it, vi } from 'vitest'

// XStack keeps the props it is drawn with, so a test can see which handlers a row's button carries.
const { seen, none, box } = vi.hoisted(() => {
  const seen: Record<string, unknown>[] = []
  const element = (type: string, props: unknown) => ({ $$typeof: Symbol.for('react.transitional.element'), type, key: null, ref: null, props, _owner: null, _store: {} })
  return {
    seen,
    none: () => null,
    box: ({ children }: { children?: unknown }) => element('div', { children }),
  }
})
vi.mock('@hanzo/gui', () => ({
  SizableText: box,
  YStack: box,
  useMedia: () => ({ sm: false }),
  XStack: (p: Record<string, unknown>) => (seen.push(p), box({ children: p.children })),
}))
vi.mock('@hanzo/ui', () => ({ Button: box, HoverCard: box, HoverCardContent: box, HoverCardTrigger: box }))
vi.mock('@hanzogui/lucide-icons-2', () => ({ Bot: none, CalendarClock: none, FileText: none, Github: none, MessageSquare: none, X: none }))

const { Suggestions } = await import('./suggestions.tsx')

describe('a suggestion row', () => {
  it('does its act from one handler, so a press cannot do it twice', () => {
    seen.length = 0
    const row = { id: 'a', icon: 'chat', title: 'Hello', summary: '', act: { kind: 'prompt', prompt: 'hi' } } as never
    renderToStaticMarkup(h(Suggestions, { rows: [row], onAct: () => {}, onDismiss: () => {}, state: 'on', onTurn: () => {} }))
    const button = seen.find((p) => p['data-act'])
    expect(button?.onPress).toBeTypeOf('function')
    expect(button).not.toHaveProperty('onClick')
  })
})
