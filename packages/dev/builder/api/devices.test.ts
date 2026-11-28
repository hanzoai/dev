import { describe, expect, it } from 'vitest'

import { displayMachine, machine } from './machines.ts'
import { displayPlace, SANDBOX } from './places.ts'

describe('device names a person reads', () => {
  it('keeps a laptop\u2019s own name in Simple mode, with its hardware as the detail', () => {
    const m = machine({
      id: 'tgt_1',
      label: 'MacBook-Pro.local',
      kind: 'laptop',
      host: 'MacBook-Pro.local',
      status: 'online',
      metricsAt: '2026-10-04T06:00:00Z',
      spec: { os: 'darwin', arch: 'arm64', cpus: 16, memory: 64 * 1024 * 1024 * 1024, gpus: [{ model: 'Apple M2' }] },
    })
    expect(displayMachine(m, { devMode: false })).toEqual({ title: 'MacBook Pro', detail: 'Apple M2 · 64 GB · MacBook-Pro.local' })
    expect(displayMachine(m, { devMode: true }).title).toBe('MacBook-Pro.local')
    // A name the person gave it wins, for a developer too, with the machine's own name beneath.
    const names = { 'macbook-pro': 'Work laptop' }
    expect(displayMachine(m, { devMode: false, names })).toEqual({ title: 'Work laptop', detail: 'Apple M2 · 64 GB · MacBook-Pro.local' })
    expect(displayMachine(m, { devMode: true, names }).title).toBe('Work laptop')
  })

  it('names the cloud workspace Auto (Cloud), or Cloud for a developer', () => {
    expect(displayPlace(SANDBOX, { devMode: false })).toEqual({
      title: 'Auto (Cloud)',
      detail: 'Runs automatically in your Hanzo cloud workspace',
    })
    expect(displayPlace(SANDBOX, { devMode: true }).title).toBe('Cloud')
  })
})
