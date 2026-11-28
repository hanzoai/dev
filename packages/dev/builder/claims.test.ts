import { describe, expect, it } from 'vitest'

import { administers, claims } from './claims.ts'

const jwt = (payload: object) => `h.${btoa(JSON.stringify(payload)).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '')}.s`

describe('claims', () => {
  it('reads a token and refuses what is not one', () => {
    expect(claims(jwt({ sub: 'u1' }))).toEqual({ sub: 'u1' })
    expect(claims(null)).toBeNull()
    expect(claims('opaque')).toBeNull()
    expect(claims('a.!!!.b')).toBeNull()
  })

  it('an admin of the org administers it, and nothing else does', () => {
    const t = jwt({ orgs: [{ org: 'acme', role: 'admin' }, { org: 'beta', role: 'member' }] })
    expect(administers(t, 'acme')).toBe(true)
    expect(administers(t, 'beta')).toBe(false)
    expect(administers(t, 'admin')).toBe(false)
    expect(administers(t, null)).toBe(false)
    expect(administers(null, 'acme')).toBe(false)
    expect(administers(jwt({ orgs: 'acme' }), 'acme')).toBe(false)
    expect(administers(jwt({ orgs: [null, { org: 'acme', role: 'admin' }] }), 'acme')).toBe(true)
  })

  it('an owner of the org administers it too', () => {
    const t = jwt({ orgs: [{ org: 'home', role: 'member' }, { org: 'acme', role: 'owner' }] })
    expect(administers(t, 'acme')).toBe(true)
    expect(administers(t, 'home')).toBe(false)
  })
})
