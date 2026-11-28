import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it, vi } from 'vitest'

// useRead keeps its key, so the test sees what makes the card list read again.
const { keys, box } = vi.hoisted(() => {
  const keys: unknown[][] = []
  const box = ({ children }: { children?: unknown }) => ({ $$typeof: Symbol.for('react.transitional.element'), type: 'div', key: null, ref: null, props: { children }, _owner: null, _store: {} })
  return { keys, box }
})
vi.mock('@hanzo/gui', () => ({ SizableText: box, XStack: box, YStack: box }))
vi.mock('@hanzo/ui', () => ({ Button: box, Input: box, Switch: box }))
vi.mock('../data.ts', () => ({
  useRead: (_load: unknown, initial: unknown, key: unknown[]) => {
    keys.push(key)
    return { value: initial, loading: false, error: null, reload: () => {} }
  },
}))
vi.mock('./ui.tsx', () => ({ Card: box, day: () => '', Group: box, Note: box, Row: box, Soft: box }))

const { Credits } = await import('./credits.tsx')

describe('the credits card list', () => {
  it('reads again when a card has been added in Billing', () => {
    const target = {} as never
    const cardsKey = (cardsVersion: number) => {
      keys.length = 0
      renderToStaticMarkup(h(Credits, { target, admin: true, cardsVersion }))
      return keys.find((k) => k.length === 3)
    }
    expect(cardsKey(0)).toEqual([target, undefined, 0])
    expect(cardsKey(1)).toEqual([target, undefined, 1])
  })
})
