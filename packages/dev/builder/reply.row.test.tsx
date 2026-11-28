import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it, vi } from 'vitest'

// Every piece draws its children, and its slot as `data-slot`, so a test can see which controls the row offers.
const { box, none } = vi.hoisted(() => ({
  box: ({ children, ...p }: { children?: unknown; 'data-slot'?: string }) => ({ $$typeof: Symbol.for('react.transitional.element'), type: 'div', key: null, ref: null, props: { 'data-slot': p['data-slot'], children }, _owner: null, _store: {} }),
  none: () => null,
}))
vi.mock('@hanzo/gui', () => ({ SizableText: box, XStack: box, YStack: box }))
vi.mock('@hanzo/ui', () => ({
  Dialog: box,
  DialogContent: box,
  DialogTitle: box,
  DropdownMenu: box,
  DropdownMenuContent: box,
  DropdownMenuItem: box,
  DropdownMenuLabel: box,
  DropdownMenuSeparator: box,
  DropdownMenuTrigger: box,
  Input: box,
}))
vi.mock('@hanzogui/lucide-icons-2', () => ({
  ArrowUp: none, Check: none, Copy: none, GitBranch: none, Globe: none, Lightbulb: none, Link2: none, MoreHorizontal: none,
  PanelRight: none, RefreshCw: none, Share: none, Square: none, ThumbsDown: none, ThumbsUp: none, Volume2: none,
}))
vi.mock('@hanzo/voice', () => ({ mouth: () => null, speech: () => null }))

const { Reply } = await import('./reply.tsx')

describe('the row under an answer', () => {
  it('offers Regenerate only where something takes it', () => {
    expect(renderToStaticMarkup(h(Reply, { text: 'hi' }))).not.toContain('reply-redo')
    expect(renderToStaticMarkup(h(Reply, { text: 'hi', onRedo: () => {} }))).toContain('reply-redo')
  })

  it('offers one rating control (a Good/Bad menu), not a second thumb pair', () => {
    const html = renderToStaticMarkup(h(Reply, { text: 'hi' }))
    expect(html.match(/data-slot="reply-rate"/g)).toHaveLength(1)
    expect(html.match(/reply-rate-up/g)).toHaveLength(1)
    expect(html.match(/reply-rate-down/g)).toHaveLength(1)
  })
})
