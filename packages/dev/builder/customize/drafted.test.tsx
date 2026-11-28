import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it, vi } from 'vitest'

// Each piece keeps the props it is drawn with, so the test sees how the "Make with AI" row is cut.
const { seen, piece, none } = vi.hoisted(() => {
  const seen: Record<string, Record<string, unknown>[]> = {}
  const piece = (name: string) => (p: Record<string, unknown>) => {
    ;(seen[name] ??= []).push(p)
    return { $$typeof: Symbol.for('react.transitional.element'), type: 'div', key: null, ref: null, props: { children: p.children }, _owner: null, _store: {} }
  }
  return { seen, piece, none: () => null }
})
vi.mock('@hanzo/gui', () => ({ SizableText: piece('text'), XStack: piece('xstack'), YStack: piece('ystack') }))
vi.mock('@hanzo/ui', () => ({ Button: piece('button'), Input: piece('input'), Skeleton: piece('skeleton'), Textarea: piece('textarea') }))
vi.mock('@hanzogui/lucide-icons-2', () => ({ AlertCircle: none, Sparkles: none, X: none }))
vi.mock('../host.tsx', () => ({ useTarget: () => ({}) }))
vi.mock('./ui.tsx', () => ({ Field: piece('field'), Mark: none, Sheet: piece('sheet') }))

const { Drafted } = await import('./drafted.tsx')

describe('the Make with AI row', () => {
  it('can shrink inside a narrow panel and stacks on a phone, so it never overflows', () => {
    renderToStaticMarkup(h(Drafted, { onMade: () => {} }))
    const row = seen.xstack!.find((p) => p['$max-sm'] && (p['$max-sm'] as Record<string, unknown>).flexDirection === 'column')
    expect(row).toMatchObject({ width: '100%', minW: 0 })
    expect(seen.input![0]).toMatchObject({ minW: 0, flex: 1 })
    expect(seen.button![0]!['$max-sm']).toMatchObject({ width: '100%' })
  })
})
