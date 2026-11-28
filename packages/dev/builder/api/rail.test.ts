/**
 * The clients behind the rail's Artifacts and the Code settings: a project's
 * rename, repository, visibility and delete; the org's machines and their claim
 * keys; and a person's API keys. Exact addresses and bodies, thin rows read as
 * empty values, and what is refused before anything is sent.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import { Refusal, type Target } from './call.ts'
import { keys, limits, mint, revoke } from './keys.ts'
import { add, change as changeMachine, grant, key, machine, machines, remove as removeMachine, revoke as unshare } from './machines.ts'
import { places, SANDBOX } from './places.ts'
import { change, project, remove } from './projects.ts'

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

describe('projects', () => {
  it('reads visibility and both times, and thin rows as empty', () => {
    expect(project({ slug: 'site', visibility: 'private', createdAt: 100, updatedAt: 200 })).toMatchObject({ visibility: 'private', created: 100, updated: 200 })
    expect(project({ slug: 'site', createdAt: '100', updatedAt: Number.NaN })).toMatchObject({ visibility: '', created: 0, updated: 0 })
  })

  it('renames with only the name, trimmed', async () => {
    const seen = answer(200, { slug: 'site', name: 'Shop', visibility: 'public' })
    const p = await change(T, 'site', { name: '  Shop ' })
    expect(seen[0]).toEqual({ method: 'PATCH', url: 'https://api.hanzo.ai/v1/project/site', body: { name: 'Shop' } })
    expect(p).toMatchObject({ slug: 'site', name: 'Shop' })
  })

  it('flips visibility with only the visibility', async () => {
    const seen = answer(200, { slug: 'site', visibility: 'private' })
    expect((await change(T, 'site', { visibility: 'private' })).visibility).toBe('private')
    expect(seen[0].body).toEqual({ visibility: 'private' })
  })

  it('refuses a blank name before sending', async () => {
    const seen = answer(200, {})
    await expect(change(T, 'site', { name: '  ' })).rejects.toThrow('A project needs a name')
    expect(seen).toHaveLength(0)
  })

  it('changes the repository with only the repository: GitHub’s clone address, and the branch when one is given', async () => {
    let seen = answer(200, { slug: 'site', repo: { url: 'https://github.com/webby-ai/book.git', branch: 'trunk' } })
    const p = await change(T, 'site', { repo: { url: 'git@github.com:webby-ai/book.git', branch: ' trunk ' } })
    expect(seen[0]).toEqual({ method: 'PATCH', url: 'https://api.hanzo.ai/v1/project/site', body: { repo: { url: 'https://github.com/webby-ai/book.git', branch: 'trunk' } } })
    expect(p).toMatchObject({ repo: 'https://github.com/webby-ai/book.git', branch: 'trunk' })
    // No branch, or a blank one, is the platform's to choose.
    for (const branch of [undefined, '  ']) {
      seen = answer(200, { slug: 'site' })
      await change(T, 'site', { repo: { url: 'https://github.com/webby-ai/book', branch } })
      expect(seen[0].body).toEqual({ repo: { url: 'https://github.com/webby-ai/book.git' } })
    }
  })

  it('refuses a repository that is not on GitHub before sending, saying what it takes', async () => {
    const seen = answer(200, {})
    for (const url of ['https://gitlab.com/webby-ai/book', 'webby-ai/book', '']) {
      await expect(change(T, 'site', { repo: { url } })).rejects.toThrow('Paste a GitHub repository’s address')
    }
    expect(seen).toHaveLength(0)
  })

  it('says the platform’s reason when it will not move the site to the repository’s account', async () => {
    answer(409, { error: { code: 'conflict', message: 'site has releases, whose rows name acme/site; a site with releases does not move to webby-ai' } })
    await expect(change(T, 'site', { repo: { url: 'https://github.com/webby-ai/book' } })).rejects.toMatchObject({ status: 409, message: expect.stringContaining('does not move') })
  })

  it('says the platform’s reason when private is not paid for', async () => {
    answer(402, { error: { code: 'insufficient_funds', message: 'add funds to publish privately' } })
    await expect(change(T, 'site', { visibility: 'private' })).rejects.toMatchObject({ status: 402, message: 'add funds to publish privately' })
  })

  it('deletes by an escaped slug', async () => {
    const seen = answer(204, undefined)
    await remove(T, 'a/b')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/project/a%2Fb' })
  })
})

describe('machines', () => {
  const row = {
    id: 'tgt_1',
    label: 'workshop',
    kind: 'gpu',
    status: 'online',
    capacity: '1× GB10',
    host: 'spark',
    sessions: 4,
    running: 1,
    metricsAt: '2026-09-27T00:00:00Z',
    spec: { os: 'linux' },
    folders: ['/home/me/site', '', 7],
  }

  it('lists the org’s machines, dropping a row with no id', async () => {
    const seen = answer(200, { targets: [row, { label: 'ghost' }] })
    const list = await machines(T)
    expect(seen[0]).toMatchObject({ method: 'GET', url: 'https://api.hanzo.ai/v1/agent/targets' })
    expect(list).toEqual([
      { id: 'tgt_1', label: 'workshop', kind: 'gpu', status: 'online', capacity: '1× GB10', host: 'spark', sessions: 4, running: 1, seen: '2026-09-27T00:00:00Z', folders: ['/home/me/site'] },
    ])
  })

  it('names a machine by its host, then its id, when it has no label', () => {
    expect(machine({ id: 'tgt_2', host: 'box' }).label).toBe('box')
    expect(machine({ id: 'tgt_3' }).label).toBe('tgt_3')
  })

  it('keeps the places New offers the same rows, with the folders a machine shares', async () => {
    answer(200, { targets: [row, { ...row, id: 'tgt_2', folders: [] }] })
    expect(await places(T)).toEqual([
      SANDBOX,
      { id: 'tgt_1', label: 'workshop', kind: 'gpu', status: 'online', capacity: '1× GB10', host: 'spark', folders: ['/home/me/site'] },
      { id: 'tgt_2', label: 'workshop', kind: 'gpu', status: 'online', capacity: '1× GB10', host: 'spark' },
    ])
  })

  it('grants and revokes a folder, the folder escaped in the revoke\'s address', async () => {
    let seen = answer(201, { folder: '/home/me/my site', grantedBy: 'u', grantedAt: '2026-11-21T00:00:00Z' })
    await grant(T, 'tgt_1', '/home/me/my site')
    expect(seen[0]).toEqual({ method: 'POST', url: 'https://api.hanzo.ai/v1/agent/targets/tgt_1/folders', body: { folder: '/home/me/my site' } })
    seen = answer(200, { revoked: true, folder: '/home/me/my site' })
    await unshare(T, 'tgt_1', '/home/me/my site')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/agent/targets/tgt_1/folders?folder=%2Fhome%2Fme%2Fmy+site' })
  })

  it('registers one by hand, sending a hostname only when there is one', async () => {
    let seen = answer(201, row)
    await add(T, { label: ' workshop ', kind: 'gpu', host: ' spark ' })
    expect(seen[0]).toEqual({ method: 'POST', url: 'https://api.hanzo.ai/v1/agent/targets', body: { label: 'workshop', kind: 'gpu', host: 'spark' } })
    seen = answer(201, row)
    await add(T, { label: 'laptop', kind: 'laptop' })
    expect(seen[0].body).toEqual({ label: 'laptop', kind: 'laptop' })
    seen = answer(201, row)
    await expect(add(T, { label: ' ', kind: 'machine' })).rejects.toThrow('A machine needs a name')
    expect(seen).toHaveLength(0)
  })

  it('renames and drains with only what changed', async () => {
    const seen = answer(200, { ...row, status: 'draining' })
    expect((await changeMachine(T, 'tgt_1', { status: 'draining' })).status).toBe('draining')
    expect(seen[0]).toEqual({ method: 'PATCH', url: 'https://api.hanzo.ai/v1/agent/targets/tgt_1', body: { status: 'draining' } })
    answer(200, row)
    await changeMachine(T, 'tgt_1', { label: ' dgx ' })
    await expect(changeMachine(T, 'tgt_1', { label: '' })).rejects.toThrow('A machine needs a name')
  })

  it('says who may, when the platform hides a machine that is not yours', async () => {
    answer(404, { detail: 'target not found' })
    const err = (await removeMachine(T, 'tgt_1').catch((e: unknown) => e)) as Refusal
    expect(err).toBeInstanceOf(Refusal)
    expect(err.message).toBe('This machine is gone, or not yours to change: only the member who linked it, or an org admin, can.')
    answer(403, { detail: 'org admin required' })
    await expect(changeMachine(T, 'tgt_1', { status: 'online' })).rejects.toThrow('org admin required')
  })

  it('removes by an escaped id', async () => {
    const seen = answer(200, { deleted: true, id: 'tgt_1' })
    await removeMachine(T, 'tgt 1')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/agent/targets/tgt%201' })
  })

  it('mints a claim key once, and refuses an answer with none', async () => {
    const seen = answer(200, { targetId: 'tgt_1', claimKey: 'tk_secret' })
    expect(await key(T, 'tgt_1')).toBe('tk_secret')
    expect(seen[0]).toEqual({ method: 'POST', url: 'https://api.hanzo.ai/v1/agent/targets/tgt_1/key', body: undefined })
    answer(200, { targetId: 'tgt_1' })
    await expect(key(T, 'tgt_1')).rejects.toThrow('The platform answered no key')
  })
})

describe('keys', () => {
  it('reads every key, a secret by its prefix, and drops an unknown type', async () => {
    const seen = answer(200, {
      keys: [
        { id: 'z-secret-1', name: 'ci', type: 'secret', prefix: 'sk-ab12', status: 'active', created: '2026-09-01T00:00:00Z', limit: ['model:zen5'] },
        { id: 'z-secret-2', name: 'old', type: 'secret', prefix: 'sk-ef56', status: 'revoked' },
        { id: 'z-publishable-1', name: 'web', type: 'publishable', prefix: 'pk-cd34', key: 'pk-cd34full', status: 'active' },
        { id: 'z-master', type: 'master', prefix: 'mk-' },
      ],
    })
    expect(await keys(T)).toEqual([
      { id: 'z-secret-1', name: 'ci', type: 'secret', prefix: 'sk-ab12', key: '', status: 'active', limit: ['model:zen5'], created: '2026-09-01T00:00:00Z' },
      { id: 'z-secret-2', name: 'old', type: 'secret', prefix: 'sk-ef56', key: '', status: 'revoked', limit: [], created: '' },
      { id: 'z-publishable-1', name: 'web', type: 'publishable', prefix: 'pk-cd34', key: 'pk-cd34full', status: 'active', limit: [], created: '' },
    ])
    expect(seen[0]).toMatchObject({ method: 'GET', url: 'https://api.hanzo.ai/v1/account/keys' })
  })

  it('reads an empty answer as no keys', async () => {
    answer(200, {})
    expect(await keys(T)).toEqual([])
  })

  it('creates a new key, sending a name and a limit only when there are some', async () => {
    let seen = answer(200, { id: 'z-secret-3', name: 'key 3', key: 'sk-new', type: 'secret' })
    expect(await mint(T, 'secret')).toEqual({ id: 'z-secret-3', name: 'key 3', type: 'secret', key: 'sk-new', limit: [] })
    expect(seen[0]).toEqual({ method: 'POST', url: 'https://api.hanzo.ai/v1/account/keys', body: { type: 'secret' } })
    seen = answer(200, { id: 'z-publishable-2', name: 'site', key: 'pk-new', type: 'publishable', limit: ['project:acme'] })
    expect(await mint(T, 'publishable', 'site', ['project:acme'])).toEqual({ id: 'z-publishable-2', name: 'site', type: 'publishable', key: 'pk-new', limit: ['project:acme'] })
    expect(seen[0].body).toEqual({ type: 'publishable', name: 'site', limit: ['project:acme'] })
    answer(200, { type: 'secret' })
    await expect(mint(T, 'secret')).rejects.toThrow('The platform answered no key')
  })

  it('revokes exactly the key named, by id', async () => {
    const seen = answer(200, { id: 'z~a-secret-1', status: 'revoked' })
    await revoke(T, 'z~a-secret-1')
    expect(seen[0]).toEqual({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/account/keys/z~a-secret-1', body: undefined })
  })

  it('reads limits as kind:name, and refuses the first that is not one', () => {
    expect(limits(' model:zen5, project:acme  model:* ')).toEqual(['model:zen5', 'project:acme', 'model:*'])
    expect(limits('')).toEqual([])
    expect(() => limits('model:zen5, zen5')).toThrow('zen5 is not a limit')
    expect(() => limits('Model:zen5')).toThrow('is not a limit')
  })
})
