import { describe, expect, test } from 'vitest'
import { BARE, gearOf, KEEP, shelve } from './gear.ts'

describe('gear', () => {
  test('a stored value reads as a Gear, an unknown mode as the default', () => {
    expect(gearOf({ skills: ['a', 3], mode: 'accept', research: true })).toEqual({ ...BARE, skills: ['a'], research: true })
    expect(gearOf({ mode: 'manual', allowed: ['research', 4] })).toEqual({ ...BARE, mode: 'manual', allowed: ['research'] })
  })
  test('the newest conversation leads, and the document keeps at most KEEP', () => {
    let all: Record<string, typeof BARE> = {}
    for (let i = 0; i < KEEP + 5; i++) all = shelve(all, `c${i}`, BARE)
    const keys = Object.keys(all)
    expect(keys.length).toBe(KEEP)
    expect(keys[0]).toBe(`c${KEEP + 4}`)
  })
})
