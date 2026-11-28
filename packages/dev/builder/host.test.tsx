import { renderToString } from 'react-dom/server'
import { afterEach, describe, expect, it, vi } from 'vitest'

import { HostProvider, useHost, useTarget, type Host } from './host.tsx'
import { Look, PrefsProvider, usePrefs } from './prefs.tsx'

const HOST: Host = {
  name: 'Hanzo',
  api: 'https://api.hanzo.ai',
  token: () => 'tok',
  org: 'acme',
  person: null,
  admin: false,
  path: '',
  go: () => {},
  links: { github: '', customize: '', settings: '', home: '' },
  open: () => {},
}

afterEach(() => vi.unstubAllGlobals())

describe('what the builder takes from its host', () => {
  it('is refused, by name, to a part rendered outside the builder', () => {
    const Reads = () => <>{useHost().api}</>
    expect(() => renderToString(<Reads />)).toThrow('The builder was rendered without a host')
    const Prefs = () => <>{String(usePrefs().loading)}</>
    expect(() => renderToString(<Prefs />)).toThrow('The builder was rendered without its settings')
  })

  it('is the call target: the host’s platform, bearer and org', () => {
    let seen: ReturnType<typeof useTarget> | null = null
    const Reads = () => {
      seen = useTarget()
      return null
    }
    renderToString(
      <HostProvider host={HOST}>
        <Reads />
      </HostProvider>,
    )
    expect(seen).toEqual({ api: 'https://api.hanzo.ai', token: HOST.token, org: 'acme' })
  })
})

describe('the person’s look, before the platform answers', () => {
  /** The page drawn on a server with `kept` in this browser's storage for acme. */
  function drawn(kept: unknown): string {
    vi.stubGlobal('window', { localStorage: { getItem: (k: string) => (k === 'hanzo.build.pref.acme' ? JSON.stringify(kept) : null) } })
    return renderToString(
      <HostProvider host={HOST}>
        <PrefsProvider>
          <Look />
          body
        </PrefsProvider>
      </HostProvider>,
    )
  }

  it('draws a text size and still motion from what this browser kept', () => {
    expect(drawn({ text: 'large', motion: 'reduced' })).toContain(
      '<style data-hanzo-build="look">html:root{--type-scale:1.15!important}*,*::before,*::after{animation-duration:.01ms!important',
    )
    expect(drawn({ text: 'small' })).toContain('--type-scale:0.9!important')
  })

  it('adds nothing for the design system’s own size and motion', () => {
    const html = drawn({ text: 'medium', motion: 'system' })
    expect(html).not.toContain('<style')
    expect(html).toContain('body')
  })
})
