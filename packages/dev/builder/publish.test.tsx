import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it, vi } from 'vitest'

const { box, btn, input, none } = vi.hoisted(() => {
  const el = (type: string, props: Record<string, unknown>) => ({ $$typeof: Symbol.for('react.transitional.element'), type, key: null, ref: null, props, _owner: null, _store: {} })
  return {
    box: ({ children, role }: { children?: unknown; role?: string }) => el('div', { role, children }),
    btn: ({ children, disabled }: { children?: unknown; disabled?: boolean }) => el('button', { disabled, children }),
    input: ({ id, value, disabled, 'aria-label': label }: { id?: string; value?: string; disabled?: boolean; 'aria-label'?: string }) =>
      el('input', { id, value, disabled, readOnly: true, 'aria-label': label }),
    none: () => null,
  }
})

vi.mock('@hanzo/gui', () => ({ SizableText: box, XStack: box, YStack: box }))
vi.mock('@hanzo/ui', () => ({
  Button: btn,
  Dialog: box,
  DialogContent: box,
  DialogDescription: box,
  DialogTitle: box,
  Input: input,
}))
vi.mock('@hanzogui/lucide-icons-2', () => ({ ExternalLink: none }))
vi.mock('./host.tsx', () => ({
  useTarget: () => ({ base: 'https://api.hanzo.ai', token: () => 'tok' }),
  useHost: () => ({ admin: false, go: () => {}, person: { name: 'Test' } }),
}))

const { Publish } = await import('./publish.tsx')

describe('Publish dialog (#365)', () => {
  it('shows Preview URL separately from editable Live app name and warns when name collides with preview host', () => {
    const html = renderToStaticMarkup(
      h(Publish, {
        source: {
          repo: 'https://git.hanzo.ai/org/shop.git',
          title: 'Shop',
          ref: 'main',
          name: 'shop',
          project: 'shop',
          preview: 'https://shop.preview.hanzo.app',
        },
        onClose: () => {},
      }),
    )
    expect(html).toContain('Preview URL')
    expect(html).toContain('https://shop.preview.hanzo.app')
    expect(html).toContain('Live app name')
    expect(html).toContain('Declared as shop — change this if you want a separate name from the preview.')
    // Must not fabricate a Live URL before the platform reports one.
    expect(html).not.toContain('https://shop.hanzo.app')
    expect(html).toContain('Add to project')
  })

  it('omits collision warning when preview host uses a different slug', () => {
    const html = renderToStaticMarkup(
      h(Publish, {
        source: {
          repo: 'https://git.hanzo.ai/org/shop.git',
          title: 'Shop',
          ref: 'main',
          name: 'shop-prod',
          project: 'shop',
          preview: 'https://shop.preview.hanzo.app',
        },
        onClose: () => {},
      }),
    )
    expect(html).toContain('Declared as shop-prod')
    expect(html).not.toContain('separate name from the preview')
  })
})
