/**
 * The personal settings' contract: the person's own settings, profile, consent,
 * the org's tool activation, memory and project visibility — exact addresses,
 * bodies, and what each makes of the answer. `fetch` is replaced per test with
 * a recorder that answers what the test says the platform answers.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import { activate, tools } from './capabilities.ts'
import { Refusal, unwrap, type Target } from './call.ts'
import { consent, consentOf, setConsent } from './consent.ts'
import { forget, memories, remember } from './memory.ts'
import { merge, prefs, read, save } from './pref.ts'
import { photo, rename } from './profile.ts'
import { change } from './projects.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'hanzo' }

interface Seen {
  url: string
  method: string
  headers: Headers
  body: unknown
}

/** Answers every call with `bodies` in turn (the last one repeats), recording what was sent. */
function answer(status: number, ...bodies: unknown[]) {
  const seen: Seen[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      const raw = init.body
      seen.push({
        url,
        method: init.method ?? 'GET',
        headers: new Headers(init.headers),
        body: typeof raw === 'string' ? JSON.parse(raw) : raw,
      })
      const body = bodies[Math.min(seen.length - 1, bodies.length - 1)]
      return new Response(body === undefined ? null : JSON.stringify(body), { status, headers: { 'content-type': 'application/json' } })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

describe('pref', () => {
  it('reads the builder’s keys and nothing another surface saved', async () => {
    const seen = answer(200, {
      prefs: { theme: 'light', density: 'compact', text: 'huge', callName: 'Z', code: { model: 'zen5.8', effort: 'high', mode: 'plan', place: '' } },
      updatedAt: 7,
    })
    // The model and effort are the browser's one choice (mind.tsx), never read from here.
    expect(await read(T)).toEqual({ theme: 'light', callName: 'Z', code: { mode: 'plan', place: '' } })
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/pref')
    expect(seen[0].headers.get('x-org-id')).toBe('hanzo')
  })

  it('reads an empty document as no choices', async () => {
    answer(200, { prefs: {} })
    expect(await read(T)).toEqual({})
    expect(prefs(null)).toEqual({})
    expect(prefs({ code: {} })).toEqual({})
  })

  it('reads the side panel’s width as whole pixels, and a width that is not one as unset', () => {
    expect(prefs({ side: { width: 612.4 } })).toEqual({ side: { width: 612 } })
    for (const width of [0, -5, '600', Infinity, null]) expect(prefs({ side: { width } })).toEqual({})
    expect(prefs({ side: 600 })).toEqual({})
  })

  it('saves only the keys it names, with null deleting one', async () => {
    const seen = answer(200, { prefs: { theme: 'dark' }, updatedAt: 8 })
    expect(await save(T, { theme: 'dark', text: null })).toEqual({ theme: 'dark' })
    expect(seen[0]).toMatchObject({ url: 'https://api.hanzo.ai/v1/pref', method: 'PATCH', body: { theme: 'dark', text: null } })
    expect(seen[0].headers.get('content-type')).toBe('application/json')
  })

  it('merges the way the platform does: shallow, null deletes', () => {
    const base = { theme: 'dark', text: 'large', code: { mode: 'build', place: '' } } as const
    expect(merge(base, { text: null, code: { mode: 'plan' } })).toEqual({ theme: 'dark', code: { mode: 'plan' } })
  })

  it('passes a refusal on', async () => {
    answer(413, { detail: 'preferences document exceeds 16384 bytes' })
    await expect(save(T, { instructions: 'x' })).rejects.toMatchObject({ status: 413, message: 'preferences document exceeds 16384 bytes' })
  })
})

describe('the envelope', () => {
  it('answers the data, and throws the sentence of a refusal carried under a 200', () => {
    expect(unwrap({ status: 'ok', msg: '', data: [1] })).toEqual([1])
    expect(() => unwrap({ status: 'error', msg: 'Please sign in first' })).toThrow('Please sign in first')
    expect(() => unwrap({ status: 'error' })).toThrow(Refusal)
  })
})

describe('profile', () => {
  it('renames the caller, and only the caller', async () => {
    const seen = answer(200, { status: 'ok', data: { owner: 'hanzo', name: 'z', displayName: 'Zach', avatar: '' } })
    expect(await rename(T, '  Zach ')).toEqual({ displayName: 'Zach', avatar: '' })
    expect(seen[0]).toMatchObject({ url: 'https://api.hanzo.ai/v1/iam/account', method: 'PUT', body: { displayName: 'Zach' } })
  })

  it('refuses an empty name before sending, and reads IAM’s refusal', async () => {
    const seen = answer(400, { status: 'error', msg: 'please sign in first' })
    await expect(rename(T, '  ')).rejects.toThrow('A name cannot be empty')
    expect(seen).toHaveLength(0)
    await expect(rename(T, 'Z')).rejects.toMatchObject({ status: 400, message: 'please sign in first' })
  })

  it('uploads a photo as a form with the bearer, and answers its address', async () => {
    const face = 'https://api.hanzo.ai/v1/account/avatar/hanzo/z/' + 'a'.repeat(64)
    const seen = answer(200, { avatar: face })
    expect(await photo(T, new Blob([new Uint8Array([137, 80, 78, 71])], { type: 'image/png' }))).toBe(face)
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/account/avatar')
    expect(seen[0].method).toBe('POST')
    expect(seen[0].body).toBeInstanceOf(FormData)
    expect((seen[0].body as FormData).get('file')).toBeInstanceOf(Blob)
    expect(seen[0].headers.get('authorization')).toBe('Bearer tok')
    expect(seen[0].headers.has('content-type')).toBe(false)
  })

  it('refuses an empty or oversized photo, and a refusal from the platform', async () => {
    answer(415, { detail: 'a profile photo must be a PNG, JPEG, GIF or WebP image' })
    await expect(photo(T, new Blob([]))).rejects.toThrow('That file is empty')
    await expect(photo(T, new Blob([new Uint8Array(8 * 1024 * 1024 + 1)]))).rejects.toThrow('A photo is at most 8 MB')
    await expect(photo(T, new Blob(['<svg/>']))).rejects.toMatchObject({ status: 415 })
  })
})

describe('consent', () => {
  it('reads the answers, with IAM’s defaults for anything missing', async () => {
    const seen = answer(200, { status: 'ok', data: { insights: false, training: 'granted' } })
    expect(await consent(T)).toEqual({ insights: false, training: 'granted' })
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/iam/consent')
    expect(consentOf({ training: 'yes' })).toEqual({ insights: true, training: '' })
  })

  it('sends only the answer that changed', async () => {
    const seen = answer(200, { status: 'ok', data: { insights: true, training: 'refused' } })
    expect(await setConsent(T, { training: 'refused' })).toEqual({ insights: true, training: 'refused' })
    expect(seen[0]).toMatchObject({ method: 'PUT', url: 'https://api.hanzo.ai/v1/iam/consent', body: { training: 'refused' } })
  })
})

describe('capabilities', () => {
  it('lists the tools, dropping a row with no name or an unknown source', async () => {
    const seen = answer(200, {
      tools: [
        { name: 'slack_post', source: 'connector', description: 'Post', dispatchable: true, activated: true },
        { name: 'x', source: 'elsewhere' },
        { source: 'skill' },
        { name: 'review', source: 'skill' },
      ],
    })
    expect(await tools(T)).toEqual([
      { name: 'slack_post', source: 'connector', description: 'Post', activated: true },
      { name: 'review', source: 'skill', description: '', activated: false },
    ])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/tool')
  })

  it('switches a set on and off, 256 names to a request', async () => {
    const names = Array.from({ length: 300 }, (_, i) => `t${i}`)
    const seen = answer(200, { enabled: names.slice(0, 256) }, { enabled: names })
    expect(await activate(T, names, true)).toHaveLength(300)
    expect(seen.map((s) => [s.method, s.url])).toEqual([
      ['PUT', 'https://api.hanzo.ai/v1/tool/activation'],
      ['PUT', 'https://api.hanzo.ai/v1/tool/activation'],
    ])
    expect(seen[0].body).toEqual({ activate: names.slice(0, 256), deactivate: [] })
    expect(seen[1].body).toEqual({ activate: names.slice(256), deactivate: [] })
    const off = answer(200, { enabled: [] })
    expect(await activate(T, ['a'], false)).toEqual([])
    expect(off[0].body).toEqual({ activate: [], deactivate: ['a'] })
    await expect(activate(T, [], true)).rejects.toThrow('There is nothing to switch')
  })
})

describe('memory', () => {
  it('lists the caller’s memories by id', async () => {
    const seen = answer(200, {
      status: 'ok',
      msg: '',
      data: [
        { owner: 'hanzo', name: 'm1', content: 'Uses pnpm', kind: 'user', createdTime: '2026-09-01', updatedTime: '2026-09-02' },
        { owner: 'hanzo', content: 'no name' },
      ],
    })
    expect(await memories(T)).toEqual([{ id: 'hanzo/m1', content: 'Uses pnpm', kind: 'user', updated: '2026-09-02' }])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/ai/memory/list?limit=100')
  })

  it('reads a refusal in the envelope, under a 200', async () => {
    answer(200, { status: 'error', msg: 'Please sign in first', data: null })
    await expect(memories(T)).rejects.toMatchObject({ message: 'Please sign in first' })
  })

  it('remembers what was said, trimmed, and forgets by id', async () => {
    const seen = answer(200, { status: 'ok', data: { owner: 'hanzo', name: 'm2', content: 'Tabs', kind: 'user', createdTime: 'now' } })
    expect(await remember(T, ' Tabs ')).toEqual({ id: 'hanzo/m2', content: 'Tabs', kind: 'user', updated: 'now' })
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/ai/memory/remember', body: { content: 'Tabs' } })
    await expect(remember(T, '  ')).rejects.toThrow('Say what Hanzo should remember')
    const gone = answer(200, { status: 'ok', data: true })
    await forget(T, 'hanzo/m2')
    expect(gone[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/ai/memory/delete', body: { id: 'hanzo/m2' } })
  })
})

describe('project visibility', () => {
  it('changes one project, escaped, and reads what was saved', async () => {
    const seen = answer(200, { slug: 'shop', name: 'Shop', visibility: 'private' })
    expect(await change(T, 'shop', { visibility: 'private' })).toMatchObject({ slug: 'shop', visibility: 'private' })
    expect(seen[0]).toMatchObject({ method: 'PATCH', url: 'https://api.hanzo.ai/v1/project/shop', body: { visibility: 'private' } })
  })

  it('passes on the refusal an unfunded org gets for private', async () => {
    const seen = answer(402, { detail: 'payment required' })
    await expect(change(T, 'a b', { visibility: 'private' })).rejects.toMatchObject({ status: 402, message: 'payment required' })
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/project/a%20b')
  })
})
