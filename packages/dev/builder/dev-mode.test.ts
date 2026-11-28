import { afterEach, describe, expect, it, vi } from 'vitest'

import { adopt, DEV_MODE_EVENT, DEV_MODE_KEY, formatDeviceLabel, getDevMode, setDevMode } from './dev-mode.ts'

afterEach(() => vi.unstubAllGlobals())

describe('Developer mode and friendly device labels', () => {
  it('syncs hanzo.dev_mode via localStorage and hanzo:dev-mode event', () => {
    const store = new Map<string, string>()
    const events: string[] = []
    const target = new EventTarget()
    vi.stubGlobal('window', {
      localStorage: {
        getItem: (k: string) => store.get(k) ?? null,
        setItem: (k: string, v: string) => void store.set(k, v),
      },
      addEventListener: target.addEventListener.bind(target),
      removeEventListener: target.removeEventListener.bind(target),
      dispatchEvent: (e: Event) => {
        events.push(e.type)
        return target.dispatchEvent(e)
      },
    })
    expect(getDevMode()).toBe(false)
    setDevMode(true)
    expect(store.get(DEV_MODE_KEY)).toBe('true')
    expect(events).toContain(DEV_MODE_EVENT)
    expect(getDevMode()).toBe(true)
  })

  it('is off for a new person, made once from what the device shows, and kept (hanzo-inc/app issue 321)', () => {
    const device = (seed: Record<string, string>) => {
      const store = new Map(Object.entries(seed))
      vi.stubGlobal('window', {
        localStorage: { getItem: (k: string) => store.get(k) ?? null, setItem: (k: string, v: string) => void store.set(k, v) },
        addEventListener() {},
        removeEventListener() {},
        dispatchEvent: () => true,
      })
      return store
    }
    // Someone new, or who has only built with Dev, reads the result.
    let store = device({ 'hanzo.build.view.site': '"code"' })
    expect(getDevMode()).toBe(false)
    expect(store.get(DEV_MODE_KEY)).toBe('false')
    // Someone who opened a terminal or linked a machine keeps the process they had.
    store = device({ 'hanzo.tabs.fleet': JSON.stringify([{ machine: 'dgx', sandbox: false }]) })
    expect(getDevMode()).toBe(true)
    expect(store.get(DEV_MODE_KEY)).toBe('true')
    // So does someone who asked Dev for more of its transcript.
    expect(adopt({ getItem: (k) => (k === 'hanzo.build.transcript.view' ? '"verbose"' : null) })).toBe(true)
    expect(adopt({ getItem: (k) => (k === 'hanzo.build.transcript.view' ? '"normal"' : k === 'hanzo.tabs.fleet' ? '[]' : null) })).toBe(false)
    expect(adopt({ getItem: () => 'not json' })).toBe(false)
    // A choice once made is the one read, whatever the device shows later.
    store = device({ [DEV_MODE_KEY]: 'false', 'hanzo.tabs.fleet': JSON.stringify([{ machine: 'dgx' }]) })
    expect(getDevMode()).toBe(false)
  })

  it('uses the key and event the Hanzo host\u2019s own settings store uses (components/settings/kit.tsx), so one switch serves both', () => {
    expect(DEV_MODE_KEY).toBe('hanzo.dev_mode')
    expect(DEV_MODE_EVENT).toBe('hanzo:dev-mode')
  })

  it('keeps a machine\u2019s own name, as a person says it, with its hardware beneath', () => {
    expect(formatDeviceLabel('MacBook-Pro.local \u00b7 16 vCPU / 64G', { devMode: false })).toEqual({
      title: 'MacBook Pro',
      detail: 'MacBook-Pro.local \u00b7 16 vCPU / 64G',
    })
    expect(formatDeviceLabel('dgx (shell-1)', { devMode: false, hardware: '1\u00d7 GB10' })).toEqual({ title: 'dgx', detail: '1\u00d7 GB10 \u00b7 dgx (shell-1)' })
    expect(formatDeviceLabel('dave-laptop', { devMode: true, hardware: '10 vCPU' })).toEqual({ title: 'dave-laptop', detail: '10 vCPU' })
  })

  it('names the cloud workspace in plain words, through the reader\u2019s say', () => {
    expect(formatDeviceLabel('Hanzo sandbox', { devMode: false })).toEqual({ title: 'Cloud workspace', detail: 'Managed Hanzo cloud workspace' })
    expect(formatDeviceLabel('cloud-1 \u00b7 shell-1', { devMode: false })).toEqual({ title: 'Cloud workspace', detail: 'cloud-1 \u00b7 shell-1' })
    const french = ((s: string) => `fr:${s}`) as never
    expect(formatDeviceLabel('sandbox', { devMode: false, say: french }).title).toBe('fr:Cloud workspace')
  })
})
