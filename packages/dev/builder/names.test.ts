import { describe, expect, it } from 'vitest'

import { formatDeviceLabel } from './dev-mode.ts'
import { displayPlace } from './api/places.ts'
import { deviceKey, friendly, names, shellKey } from './names.ts'

describe('a device named as a person says it (hanzo-inc/app issue 311)', () => {
  it('turns a macOS hostname back into the computer name it came from', () => {
    expect(friendly('Alexs-MacBook-Pro.local')).toBe("Alex's MacBook Pro")
    expect(friendly('alexs-macbook-air')).toBe("Alex's MacBook Air")
    expect(friendly('Alexs-MBP-2.local')).toBe("Alex's MacBook Pro")
    expect(friendly('Jamess-MacBook.lan')).toBe("James's MacBook")
    expect(friendly('MacBook-Pro.local')).toBe('MacBook Pro')
    expect(friendly('studio-Mac-mini')).toBe('Studio Mac mini')
    expect(friendly('Sams-iMac.local')).toBe("Sam's iMac")
  })

  it('names Windows made-up hostnames plainly and keeps a name someone chose', () => {
    expect(friendly('DESKTOP-4F7K2QX')).toBe('Windows PC')
    expect(friendly('LAPTOP-9QZ12AB')).toBe('Windows laptop')
    expect(friendly('spark-dgx.lan')).toBe('spark-dgx')
    expect(friendly('workshop')).toBe('workshop')
    expect(friendly('')).toBe('')
  })

  it('keeps the hostname, the chip and the GPU out of the title, in the detail', () => {
    expect(formatDeviceLabel('Alexs-MacBook-Pro.local · 12 vCPU / 32G / M2', { hardware: 'Apple M2 · 32 GB' })).toEqual({
      title: "Alex's MacBook Pro",
      detail: 'Apple M2 · 32 GB · Alexs-MacBook-Pro.local · 12 vCPU / 32G / M2',
    })
    expect(formatDeviceLabel('Alexs-MacBook-Pro.local', { devMode: true }).title).toBe('Alexs-MacBook-Pro.local')
    expect(formatDeviceLabel('sandbox').title).toBe('Cloud workspace')
  })

  it('keys a device the same way on every surface, and a shell under its device', () => {
    expect(deviceKey('Alexs-MacBook-Pro.local', 'whatever', 'tgt_1')).toBe('alexs-macbook-pro')
    expect(deviceKey('', 'Workshop', 'tgt_1')).toBe('workshop')
    expect(deviceKey(undefined, '', 'tgt_1')).toBe('tgt_1')
    expect(shellKey('Alexs-MacBook-Pro.local', 'Main')).toBe('alexs-macbook-pro/main')
  })

  it('reads a place by the name the person gave its machine', () => {
    const place = { id: 'tgt_1', label: 'Alexs-MacBook-Pro.local', kind: 'laptop', status: 'online', capacity: '12 vCPU / 32G', host: 'Alexs-MacBook-Pro.local' }
    expect(displayPlace(place).title).toBe("Alex's MacBook Pro")
    expect(displayPlace(place, { names: { 'alexs-macbook-pro': 'Studio Mac' } }).title).toBe('Studio Mac')
  })

  it('keeps only names from a stored value, trimmed to a line', () => {
    expect(names({ a: '  Studio   Mac ', b: '', c: 3, d: 'x'.repeat(80) })).toEqual({ a: 'Studio Mac', d: 'x'.repeat(60) })
    expect(names(null)).toEqual({})
    expect(names(['a'])).toEqual({})
  })
})
