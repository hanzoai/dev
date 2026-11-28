/**
 * The Settings clients against answers the other contract tests leave out:
 * rows missing their fields or not rows at all, empty and 204 answers, refusals
 * that carry no sentence, and inputs refused before anything is sent. `fetch`
 * is replaced per test with a recorder that answers what the test says.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import * as billing from './billing.ts'
import type { Target } from './call.ts'
import { activate, toolOf, tools } from './capabilities.ts'
import { consentOf } from './consent.ts'
import { environment, environments } from './environment.ts'
import * as github from './github.ts'
import { keys, mint } from './keys.ts'
import { add, KINDS, machine, machines, state } from './machines.ts'
import * as members from './members.ts'
import { memories, memoryOf } from './memory.ts'
import { merge } from './pref.ts'
import { photo, rename } from './profile.ts'
import { address, project, projects, safe, templates } from './projects.ts'
import * as provider from './provider.ts'
import * as webhooks from './webhooks.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'acme' }

interface Seen {
  url: string
  method: string
  body: unknown
}

/** Answers every call with `replies` in turn (the last repeats); `undefined` is an empty body. Records what was sent. */
function answer(status: number, ...replies: unknown[]) {
  const seen: Seen[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      const body = replies[Math.min(seen.length, replies.length - 1)]
      seen.push({ url, method: init.method ?? 'GET', body: typeof init.body === 'string' ? JSON.parse(init.body) : init.body })
      return new Response(body === undefined ? null : JSON.stringify(body), { status, headers: { 'content-type': 'application/json' } })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

describe('pref', () => {
  it('leaves a key a change names as undefined as it was', () => {
    expect(merge({ theme: 'dark', text: 'large' }, { theme: undefined, text: null })).toEqual({ theme: 'dark' })
  })
})

describe('profile', () => {
  it('reads a rename IAM answered with nothing as an empty profile', async () => {
    answer(200, { status: 'ok', msg: '' })
    expect(await rename(T, 'Zach')).toEqual({ displayName: '', avatar: '' })
    answer(200, { status: 'ok', data: { displayName: 7, avatar: null } })
    expect(await rename(T, 'Zach')).toEqual({ displayName: '', avatar: '' })
  })

  it('says the status of a photo refused without a reason, and refuses an address the page cannot show', async () => {
    const png = new Blob([new Uint8Array([137, 80, 78, 71])], { type: 'image/png' })
    answer(500, undefined)
    await expect(photo(T, png)).rejects.toMatchObject({ status: 500, message: 'POST /v1/account/avatar answered 500' })
    answer(200, { avatar: 'http://api.hanzo.ai/face.png' })
    await expect(photo(T, png)).rejects.toMatchObject({ status: 502, message: 'The photo was stored without an address this page can show' })
    answer(200, [])
    await expect(photo(T, png)).rejects.toMatchObject({ status: 502 })
  })
})

describe('consent', () => {
  it('reads nothing at all as IAM’s defaults', () => {
    expect(consentOf(null)).toEqual({ insights: true, training: '' })
    expect(consentOf('granted')).toEqual({ insights: true, training: '' })
  })
})

describe('capabilities', () => {
  it('reads a row that is not one, an answer with no list, and a switch answered with no set', async () => {
    expect(toolOf(null)).toBeNull()
    answer(200, {})
    expect(await tools(T)).toEqual([])
    answer(204, undefined)
    expect(await tools(T)).toEqual([])
    answer(200, { enabled: ['a', 7, null] })
    expect(await activate(T, ['a'], true)).toEqual(['a'])
    answer(200, {})
    expect(await activate(T, ['a'], true)).toEqual([])
  })
})

describe('memory', () => {
  it('reads a row that is not one, and a list that is not a list', async () => {
    expect(memoryOf(null)).toBeNull()
    answer(200, { status: 'ok', data: { owner: 'acme', name: 'm1' } })
    expect(await memories(T)).toEqual([])
  })
})

describe('keys', () => {
  it('skips a row that is not one, and reads a key minted with no type or limit as the type asked for', async () => {
    answer(200, { keys: [null, 'sk-x', { type: 'secret', prefix: 'sk-no-id' }, { id: 'k', type: 'secret', prefix: 'sk-ab' }] })
    expect(await keys(T)).toEqual([{ id: 'k', name: '', type: 'secret', prefix: 'sk-ab', key: '', status: 'active', limit: [], created: '' }])
    answer(200, { key: 'pk-new' })
    expect(await mint(T, 'publishable')).toEqual({ id: '', name: '', type: 'publishable', key: 'pk-new', limit: [] })
    answer(204, undefined)
    await expect(mint(T, 'secret')).rejects.toThrow('The platform answered no key')
  })
})

describe('machines', () => {
  it('reads a row that is not one, and an empty answer as no machines', async () => {
    expect(machine(null)).toMatchObject({ id: '', label: '', status: '' })
    answer(204, undefined)
    expect(await machines(T)).toEqual([])
    answer(200, { targets: 'none' })
    expect(await machines(T)).toEqual([])
  })

  it('registers a named machine of every kind, with and without a hostname', async () => {
    for (const kind of KINDS) {
      const bare = answer(201, { id: 'tgt_1', label: 'workshop', kind, status: 'online' })
      expect(await add(T, { label: ' workshop ', kind, host: '  ' })).toMatchObject({ id: 'tgt_1', label: 'workshop', kind })
      expect(bare).toEqual([{ url: 'https://api.hanzo.ai/v1/agent/targets', method: 'POST', body: { label: 'workshop', kind } }])
      const hosted = answer(201, { id: 'tgt_2', label: 'workshop', kind, status: 'online', host: 'm' })
      expect(await add(T, { label: 'workshop', kind, host: ' m ' })).toMatchObject({ id: 'tgt_2', host: 'm' })
      expect(hosted.map((s) => s.body)).toEqual([{ label: 'workshop', kind, host: 'm' }])
    }
  })

  it('names a machine left unnamed after its hostname, as hanzo link does, and refuses one with neither before sending', async () => {
    const seen = answer(201, { id: 'tgt_1', label: 'gpu-01', kind: 'cloud', status: 'online', host: 'gpu-01' })
    expect(await add(T, { label: '  ', kind: 'cloud', host: 'gpu-01' })).toMatchObject({ label: 'gpu-01' })
    expect(seen.map((s) => s.body)).toEqual([{ label: 'gpu-01', kind: 'cloud', host: 'gpu-01' }])
    const none = answer(201, {})
    await expect(add(T, { label: ' ', kind: 'cloud', host: ' ' })).rejects.toThrow('A machine needs a name or a hostname')
    expect(none).toEqual([])
  })

  it('reads a machine as online only once it has beaten', () => {
    expect(state(machine({ id: 't', status: 'online', metricsAt: '2026-10-04T06:00:00Z' }))).toBe('online')
    expect(state(machine({ id: 't', status: 'online' }))).toBe('not seen yet')
    expect(state(machine({ id: 't', status: 'offline', metricsAt: '2026-10-04T06:00:00Z' }))).toBe('offline')
    expect(state(machine({ id: 't', status: 'draining' }))).toBe('draining')
  })
})

describe('billing', () => {
  it('prices in dollars when no currency is named, and in its own code when Intl knows none', () => {
    expect(billing.money(2500, '')).toBe('$25')
    expect(billing.money(2550, 'X')).toBe('25.50 X')
    expect(billing.money(2500, 'X')).toBe('25 X')
  })

  it('reads a catalog sent as {plans}, and a plan with no category as one an org can hold', async () => {
    answer(200, { plans: [{ id: 'solo', name: 'Solo', price: 900 }, { slug: 'dns', name: 'DNS', category: 'dns' }] })
    expect((await billing.plans(T)).map((p) => p.id)).toEqual(['solo'])
    answer(200, {})
    expect(await billing.plans(T)).toEqual([])
  })

  it('names a subscription by its plan id when the plan is not attached, one seat a month', async () => {
    answer(200, { subscriptions: [{ id: 'sub_1', planId: 'max', status: 'trialing', defaultPaymentMethod: 'pm_1' }, { plan: { id: 'x' } }] })
    const list = await billing.subscriptions(T)
    expect(list).toEqual([
      { id: 'sub_1', plan: 'max', name: 'max', price: 0, interval: 'month', currency: 'usd', seats: 1, status: 'trialing', ends: '', ending: false, method: 'pm_1' },
    ])
    expect(billing.subscription({ id: 's', plan: { id: 'dev', name: 'Dev' } })).toMatchObject({ plan: 'dev', name: 'Dev' })
  })

  it('reads a saved method that is not a card by its name, or else its type', async () => {
    answer(200, { data: [{ id: 'pm_1', card: { last4: '1111' } }, { id: 'pm_2', type: 'sepa_debit', name: 'SEPA account' }, { id: 'pm_3', type: 'ach' }] })
    const list = await billing.methods(T)
    expect(list.map(billing.label)).toEqual(['Card •••• 1111', 'SEPA account', 'ach'])
    expect(list[0]).toMatchObject({ type: 'card', expires: '' })
    answer(200, {})
    expect(await billing.methods(T)).toEqual([])
  })

  it('numbers an invoice by its number when it has no printed one, and names a line by its plan', async () => {
    answer(200, { invoices: [{ id: 'inv_1', number: 7, lineItems: [{ planName: 'Max', amount: 100 }, 'x'] }, { id: 'inv_2' }] })
    const [a, b] = await billing.invoices(T)
    expect(a).toMatchObject({ number: '7', currency: 'usd', lines: [{ description: 'Max', amount: 100 }, { description: '', amount: 0 }] })
    expect(b).toMatchObject({ number: '', lines: [] })
    answer(200, {})
    expect(await billing.invoices(T)).toEqual([])
  })

  it('says the status of a PDF refused without a reason', async () => {
    answer(502, undefined)
    await expect(billing.pdf(T, 'inv_1')).rejects.toMatchObject({ status: 502, message: 'The invoice answered 502' })
  })

  it('reads the balance from what is held when nothing is available, and credit in no currency as dollars', async () => {
    answer(200, { balance: 900 })
    expect(await billing.balance(T)).toBe(900)
    answer(200, { balances: [{ available: 300 }, { currency: 'USD', available: 200 }, 'x'] })
    expect(await billing.credit(T)).toBe(500)
    answer(200, {})
    expect(await billing.credit(T)).toBe(0)
  })

  it('reads a month with nothing in it, and a spend by category with no name', async () => {
    answer(200, {})
    expect(await billing.month(T)).toEqual({ plan: '', period: '', included: 0, used: 0, overage: 0, windows: [] })
    answer(200, { spend: { available: true, totalCents: 300, byCategory: [{ cents: 300 }] } })
    expect(await billing.spend(T)).toEqual({ known: true, total: 300, categories: [{ name: 'Uncategorized', cents: 300 }] })
  })

  it('reads caps answered as something other than a list as none', async () => {
    answer(200, { alerts: [] })
    expect(await billing.caps(T)).toEqual([])
    expect(billing.monthly([billing.cap({ id: 'al_1', threshold: 0 })])).toBeNull()
  })
})

describe('members', () => {
  it('names someone with no home org by their id, and reads a roster that is not one as empty', async () => {
    expect(members.member({ user: 'zed', role: 'admin' })).toEqual({ user: 'zed', name: 'zed', role: 'admin', since: '' })
    answer(200, { status: 'ok' })
    expect(await members.roster(T, 'acme')).toEqual([])
    answer(200, null)
    expect(await members.roster(T, 'acme')).toEqual([])
  })

  it('reads a refusal IAM sends under a 200, with its sentence or without one', async () => {
    answer(200, { status: 'error', msg: 'Unauthorized operation' })
    await expect(members.roster(T, 'acme')).rejects.toThrow('Unauthorized operation')
    answer(200, { status: 'error' })
    await expect(members.roster(T, 'acme')).rejects.toThrow('IAM refused the roster')
  })

  it('reads an invitation dated by createdAt, and none when the answer has no list', async () => {
    expect(members.invitation({ name: 'i', createdAt: '2026-09-01' }).created).toBe('2026-09-01')
    answer(200, {})
    expect(await members.invitations(T, 'acme')).toEqual([])
  })

  it('refuses a blank address as that, and names an invitation for an address with no letters in its name', async () => {
    const seen = answer(200, { owner: 'acme', name: 'invite-member-000000' })
    await expect(members.invite(T, 'acme', '  ')).rejects.toThrow('That is not an email address')
    await members.invite(T, 'acme', '+++@acme.test')
    expect((seen[0]!.body as { name: string }).name).toMatch(/^invite-member-[0-9a-f]{6}$/)
  })
})

describe('provider', () => {
  it('reads a connector the platform did not name by the id asked for, and an answer that is not one', async () => {
    answer(200, { available: true })
    expect(await provider.read(T, 'slack')).toMatchObject({ id: 'slack', name: 'slack', connected: false, account: '' })
    expect(provider.connector(null)).toMatchObject({ id: '', name: '' })
    answer(200, {})
    expect(await provider.channels(T)).toEqual({ channels: [], next: '' })
  })

  it('refuses to connect a provider it has no consent host for', async () => {
    answer(200, { authorizeUrl: 'https://example.com/consent' })
    await expect(provider.authorize(T, 'discord')).rejects.toThrow('The platform did not name a discord address to connect at')
  })
})

describe('webhooks', () => {
  it('reads a row that is not one, and refuses more than 64 patterns before sending', async () => {
    expect(webhooks.endpoint(null)).toMatchObject({ id: '', status: 'active', events: [] })
    const seen = answer(201, {})
    const events = Array.from({ length: 65 }, (_, i) => `e${i}.>`)
    await expect(webhooks.add(T, { url: 'https://acme.test/h', events, description: '' })).rejects.toThrow('At most 64 event patterns')
    expect(seen).toHaveLength(0)
    answer(200, {})
    expect(await webhooks.endpoints(T)).toEqual([])
    expect(await webhooks.deliveries(T, 'wh_1')).toEqual([])
  })
})

describe('environments', () => {
  it('reads a row that is not one, and an answer with no list, as nothing', async () => {
    expect(environment(null)).toMatchObject({ repo: '', state: 'none', proposal: null })
    answer(200, {})
    expect(await environments(T)).toEqual([])
  })
})

describe('projects', () => {
  it('reads a list sent bare, under data, or not at all, and a row that is not one', async () => {
    answer(200, [{ slug: 'a', updatedAt: 1 }, null])
    expect((await projects(T)).map((p) => p.slug)).toEqual(['a'])
    answer(200, { data: [{ slug: 'b' }] })
    expect((await projects(T)).map((p) => [p.slug, p.name])).toEqual([['b', 'b']])
    answer(200, { projects: [{ slug: 'c' }] })
    expect(await projects(T)).toEqual([])
    expect(project(null)).toMatchObject({ slug: '', name: '', visibility: '' })
  })

  it('frames a local address only for a builder on loopback, by either loopback name', () => {
    expect(safe('http://127.0.0.1:3000/', true)).toBe('http://127.0.0.1:3000/')
    expect(safe('http://localhost:3000/', true)).toBe('http://localhost:3000/')
    expect(safe('http://127.0.0.1:3000/', false)).toBe('')
  })

  it('titles a starter by its slug when it has no title, and skips rows that are not starters', async () => {
    answer(200, { data: [{ slug: 'mint' }, 'folio', null] })
    expect(await templates(T)).toEqual([{ slug: 'mint', title: 'mint', category: '', description: '', framework: '', source: '', demo: '' }])
    answer(200, {})
    expect(await templates(T)).toEqual([])
  })

  it('reads no owner and name from an address with one segment', () => {
    expect(address('site.git')).toBe('')
  })
})

describe('github', () => {
  it('reads answers with no lists, and asks for fifty branches when no limit is given', async () => {
    const seen = answer(200, {})
    expect(await github.repos(T)).toEqual({ repos: [], next: '', total: 0, unread: [], connected: false })
    expect(await github.branches(T, 'acme', 'site')).toEqual({ branches: [], next: '', total: 0 })
    expect(seen[1]!.url).toBe('https://api.hanzo.ai/v1/provider/github/repos/acme/site/branches?limit=50')
    expect(await github.installations(T)).toEqual([])
    expect(await github.grants(T)).toEqual({ repos: [], unread: [] })
    expect(github.repo(null)).toMatchObject({ owner: '', name: '', full_name: '' })
  })

  it('reads a grant by the shape the listing answers, and counts no account for one without an owner', () => {
    expect(github.grant({ owner: 'acme', name: 'site', full_name: 'acme/site', default_branch: 'dev', codebase: 'acme_site' })).toEqual({
      owner: 'acme',
      name: 'site',
      fullName: 'acme/site',
      private: false,
      branch: 'dev',
      codebase: 'acme_site',
      imported: false,
      status: '',
    })
    expect(github.grant({ name: 'site', full_name: 'acme/site' })).toMatchObject({ owner: 'acme', fullName: 'acme/site' })
    expect(github.grant({ name: 'site', owner: 'acme' })).toMatchObject({ fullName: 'acme/site', branch: 'main', status: '' })
    expect(github.grant({ name: 'site' })).toMatchObject({ owner: '', fullName: 'site' })
    expect(github.grant({ owner: 'acme' })).toBeNull()
    expect(github.accounts({ repos: [github.grant({ name: 'site' })!], unread: [] })).toEqual([])
  })
})
