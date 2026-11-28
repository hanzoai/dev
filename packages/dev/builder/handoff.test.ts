import { describe, expect, it } from 'vitest'

import { DEV_HANDOFF, takeHandoff } from './handoff.ts'

const store = (value?: string) => {
  const items = new Map(value === undefined ? [] : [[DEV_HANDOFF, value]])
  return { items, getItem: (k: string) => items.get(k) ?? null, removeItem: (k: string) => void items.delete(k) }
}

describe('the Chat to Dev handoff', () => {
  it('turns a prompt and code into one draft, and is read once', () => {
    const s = store(JSON.stringify({ prompt: ' Fix this ', code: 'let a = 1', language: 'ts' }))
    expect(takeHandoff(s)).toBe('Fix this\n\n```ts\nlet a = 1\n```')
    expect(s.items.has(DEV_HANDOFF)).toBe(false)
    expect(takeHandoff(s)).toBe('')
  })

  it('reads a prompt alone, and nothing from a broken or empty handoff', () => {
    expect(takeHandoff(store(JSON.stringify({ prompt: 'Hello' })))).toBe('Hello')
    expect(takeHandoff(store('{not json'))).toBe('')
    expect(takeHandoff(store())).toBe('')
  })
})
