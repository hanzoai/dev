import { describe, expect, it } from 'vitest'

import { clean, landed, named } from './back.ts'

describe('a return from a consent page', () => {
  it('reads a GitHub grant to finish', () => {
    expect(landed('?at=-%2Fsettings%2Fintegrations&complete=github&grant=9f2c')).toEqual({ kind: 'grant', provider: 'github', grant: '9f2c' })
  })

  it('reads a connector connected, and one refused', () => {
    expect(landed('?connected=slack&account=Acme%2C+LLC')).toEqual({ kind: 'connected', provider: 'slack', account: 'Acme, LLC' })
    expect(landed('?error=github&reason=authorization+denied')).toEqual({ kind: 'error', provider: 'github', reason: 'authorization denied' })
  })

  it('takes a refusal over anything else it carries, and reads nothing from an ordinary address', () => {
    expect(landed('?complete=github&grant=g&error=github&reason=x')).toMatchObject({ kind: 'error' })
    expect(landed('?complete=github')).toBeNull()
    expect(landed('?at=-%2Fsettings%2Fintegrations')).toBeNull()
    expect(landed('')).toBeNull()
  })

  it('cleans only the answer off the address', () => {
    expect(clean('https://hanzo.ai/?at=-%2Fsettings%2Fintegrations&complete=github&grant=9f2c')).toBe('https://hanzo.ai/?at=-%2Fsettings%2Fintegrations')
    // The return the platform answers with: the page's own pair stays as written, slashes and all.
    expect(clean('https://hanzo.ai/?at=-/settings/integrations&complete=github&grant=9f2c')).toBe('https://hanzo.ai/?at=-/settings/integrations')
    expect(clean('https://hanzo.ai/?at=-/settings/integrations&error=slack&reason=token+exchange+failed')).toBe('https://hanzo.ai/?at=-/settings/integrations')
    expect(clean('https://hanzo.ai/?connected=slack&account=Acme')).toBe('https://hanzo.ai/')
    // An address with no answer is handed back exactly as written.
    expect(clean('https://hanzo.ai/?at=-/settings/integrations')).toBe('https://hanzo.ai/?at=-/settings/integrations')
  })

  it('names a provider as people write it', () => {
    expect(named('github')).toBe('GitHub')
    expect(named('slack')).toBe('Slack')
    expect(named('linear')).toBe('Linear')
  })
})
