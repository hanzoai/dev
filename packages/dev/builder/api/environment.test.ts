/**
 * A codebase's environment as the builder reads and writes it: exact addresses
 * and bodies, a secret's value only ever in a PUT body, and the names the run
 * could not export refused before anything is sent.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import type { Target } from './call.ts'
import { unhonoured } from './coding.ts'
import { environment, environments, forget, read, refuse, removeSecret, save, setSecret } from './environment.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'hanzo' }

function answer(status: number, body: unknown) {
  const seen: { url: string; method: string; body: unknown }[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      seen.push({ url, method: init.method ?? 'GET', body: init.body ? JSON.parse(String(init.body)) : undefined })
      return new Response(body === undefined ? null : JSON.stringify(body), { status, headers: { 'content-type': 'application/json' } })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

describe('environment', () => {
  it('reads a proposal beside what is saved', async () => {
    const seen = answer(200, {
      repo: 'site',
      install: 'make',
      start: '',
      secrets: ['TOKEN'],
      state: 'proposed',
      session: 'sess_1',
      proposal: { install: 'pnpm install', start: 'pnpm dev', secrets: ['DATABASE_URL'], note: 'Tests pass.' },
      updatedAt: '2026-09-26T18:00:00Z',
    })
    const e = await read(T, 'site')
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/environment/site')
    expect(e).toMatchObject({ repo: 'site', install: 'make', secrets: ['TOKEN'], state: 'proposed', session: 'sess_1' })
    expect(e.proposal).toEqual({ install: 'pnpm install', start: 'pnpm dev', secrets: ['DATABASE_URL'], note: 'Tests pass.' })
  })

  it('reads an unknown state as none, and a missing codebase from the address', () => {
    expect(environment({ state: 'haunted' }, 'site')).toMatchObject({ repo: 'site', state: 'none', proposal: null, secrets: [] })
  })

  it('lists the org', async () => {
    const seen = answer(200, { data: [{ repo: 'site', state: 'ready' }, { state: 'ready' }] })
    const list = await environments(T)
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/environment')
    expect(list.map((e) => e.repo)).toEqual(['site'])
  })

  it('saves the scripts and nothing else', async () => {
    const seen = answer(200, { repo: 'site', install: 'pnpm install', start: 'pnpm dev', state: 'ready' })
    await save(T, 'site', { install: 'pnpm install', start: 'pnpm dev' })
    expect(seen[0]).toMatchObject({ method: 'PUT', url: 'https://api.hanzo.ai/v1/environment/site', body: { install: 'pnpm install', start: 'pnpm dev' } })
  })

  it('sends a secret value in the body of its own address, never in a URL', async () => {
    const seen = answer(200, { repo: 'site', secrets: ['DATABASE_URL'], state: 'ready' })
    await setSecret(T, 'site', 'DATABASE_URL', 'postgres://u:p@db/app?x=1')
    expect(seen[0].method).toBe('PUT')
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/environment/site/secrets/DATABASE_URL')
    expect(seen[0].url).not.toContain('postgres')
    expect(seen[0].body).toEqual({ value: 'postgres://u:p@db/app?x=1' })
  })

  it('refuses a name the run could not export before sending anything', async () => {
    const seen = answer(200, {})
    for (const name of ['HANZO_API_KEY', 'path', '1X', 'A-B', '']) {
      expect(refuse(name)).not.toBe('')
      await expect(setSecret(T, 'site', name, 'v')).rejects.toThrow()
    }
    await expect(setSecret(T, 'site', 'OK', '')).rejects.toThrow('value')
    expect(seen).toHaveLength(0)
    expect(refuse('DATABASE_URL')).toBe('')
  })

  it('removes a secret and forgets an environment by address', async () => {
    const seen = answer(200, { repo: 'site', secrets: [] })
    await removeSecret(T, 'site', 'TOKEN')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/environment/site/secrets/TOKEN' })
    const gone = answer(204, undefined)
    await forget(T, 'site')
    expect(gone[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/environment/site' })
  })

  it('refuses a setup routed to a machine, as a plan is', () => {
    expect(unhonoured('setup', 'm_1')).toContain('sandbox')
    expect(unhonoured('setup', '')).toBe('')
  })
})
