/**
 * Money and the organization, as the builder reads and writes them: exact
 * addresses and bodies, answers read with every field allowed to be missing, and
 * the requests that must not be sent refused before they are.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import * as billing from './billing.ts'
import type { Target } from './call.ts'
import * as github from './github.ts'
import * as members from './members.ts'
import * as provider from './provider.ts'
import * as webhooks from './webhooks.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'acme' }

interface Seen {
  url: string
  method: string
  headers: Headers
  body: unknown
}

/** Answers every call with `replies` in turn (the last repeats), and records what was sent. */
function answer(status: number, ...replies: unknown[]) {
  const seen: Seen[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      const body = replies[Math.min(seen.length, replies.length - 1)]
      seen.push({ url, method: init.method ?? 'GET', headers: new Headers(init.headers), body: init.body ? JSON.parse(String(init.body)) : undefined })
      return new Response(body === undefined ? null : JSON.stringify(body), { status, headers: { 'content-type': 'application/json' } })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

describe('money', () => {
  it('drops cents on whole dollars and keeps them otherwise', () => {
    expect(billing.money(2500)).toBe('$25')
    expect(billing.money(1999)).toBe('$19.99')
    expect(billing.money(123456)).toBe('$1,234.56')
  })

  it('reads typed dollars as cents, and nothing else', () => {
    expect(billing.cents('25')).toBe(2500)
    expect(billing.cents('$1,250.5')).toBe(125050)
    expect(billing.cents('0')).toBeNull()
    expect(billing.cents('1.234')).toBeNull()
    expect(billing.cents('ten')).toBeNull()
  })
})

describe('plans', () => {
  it('reads the catalog and keeps only the plans an org can hold', async () => {
    const seen = answer(200, [
      { slug: 'free', name: 'Free', category: 'personal', price: 0 },
      { slug: 'max', name: 'Max', category: 'personal', price: 10000, priceAnnual: 8118, features: ['Enso', 3], popular: true },
      { slug: 'team', name: 'Team', category: 'team', price: 2400, perSeat: true },
      { slug: 'enterprise', name: 'Enterprise', category: 'enterprise', contactSales: true },
      { slug: 'dns-pro', name: 'DNS Pro', category: 'dns', price: 500 },
      { name: 'Nameless' },
    ])
    const list = await billing.plans(T)
    expect(seen[0]).toMatchObject({ method: 'GET', url: 'https://api.hanzo.ai/v1/billing/plans' })
    expect(list.map((p) => p.id)).toEqual(['free', 'max', 'team', 'enterprise'])
    expect(list[1]).toMatchObject({ monthly: 10000, yearly: 8118, features: ['Enso'], popular: true, currency: 'usd' })
    expect(list[2].perSeat).toBe(true)
    expect(list[3].sales).toBe(true)
  })

  it('marks the live subscription as the current plan', async () => {
    answer(200, {
      count: 2,
      subscriptions: [
        { id: 'sub_old', planId: 'dev', status: 'canceled' },
        {
          id: 'sub_1',
          planId: 'max',
          status: 'active',
          quantity: 1,
          currentPeriodEnd: '2026-10-27T00:00:00Z',
          cancelAtPeriodEnd: false,
          plan: { id: 'max', name: 'Max', price: 10000, interval: 'month', currency: 'usd' },
        },
      ],
    })
    const now = billing.current(await billing.subscriptions(T))
    expect(now).toMatchObject({ id: 'sub_1', plan: 'max', name: 'Max', price: 10000, interval: 'month', ending: false })
    expect(billing.current([])).toBeNull()
  })

  it('buys a plan with a saved card and sends no price', async () => {
    const seen = answer(201, { subscriptionId: 'sub_2', planId: 'max', amountCents: 97416, interval: 'year' })
    const r = await billing.subscribe(T, { plan: 'max', interval: 'year', method: 'pm_1' })
    expect(seen[0]).toMatchObject({
      method: 'POST',
      url: 'https://api.hanzo.ai/v1/billing/subscribe/card',
      body: { planId: 'max', paymentMethodId: 'pm_1', interval: 'year' },
    })
    expect(r).toEqual({ subscription: 'sub_2', charged: 97416 })
  })

  it('refuses to buy without a card, before sending anything', async () => {
    const seen = answer(200, {})
    await expect(billing.subscribe(T, { plan: 'max', interval: 'month', method: '' })).rejects.toThrow('Add a card')
    expect(seen).toHaveLength(0)
  })

  it('cancels at the end of the paid period, and puts it back', async () => {
    const seen = answer(200, { id: 'sub_1', status: 'active', cancelAtPeriodEnd: true })
    expect((await billing.cancel(T, 'sub/1')).ending).toBe(true)
    await billing.reactivate(T, 'sub_1')
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/billing/subscriptions/sub%2F1/cancel', body: { atPeriodEnd: true } })
    expect(seen[1]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/billing/subscriptions/sub_1/reactivate' })
  })

  it('names the platform’s refusal of a second paid plan', async () => {
    answer(409, { detail: 'already subscribed to max' })
    await expect(billing.subscribe(T, { plan: 'team', interval: 'month', method: 'pm_1' })).rejects.toMatchObject({ status: 409, message: 'already subscribed to max' })
  })
})

describe('cards', () => {
  it('lists saved cards as brand, last four and expiry, and picks the default', async () => {
    answer(200, [
      { id: 'pm_2', type: 'card', card: { brand: 'mastercard', last4: '4444', expMonth: 3, expYear: 2029 } },
      { id: 'pm_1', type: 'card', isDefault: true, card: { brand: 'visa', last4: '4242', expMonth: 12, expYear: 2027 } },
      { type: 'card' },
    ])
    const list = await billing.methods(T)
    expect(list.map((m) => m.id)).toEqual(['pm_2', 'pm_1'])
    expect(billing.label(list[1])).toBe('Visa •••• 4242')
    expect(list[1].expires).toBe('12/27')
    expect(billing.chosen(list)?.id).toBe('pm_1')
    expect(billing.chosen([])).toBeNull()
  })

  it('saves a card from the processor’s token and nothing else', async () => {
    const seen = answer(201, { id: 'pm_3', type: 'card', card: { brand: 'visa', last4: '1111' } })
    const m = await billing.save(T, 'cnon:abc')
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/billing/methods', body: { type: 'card', providerRef: 'cnon:abc' } })
    expect(m.last4).toBe('1111')
    await expect(billing.save(T, '')).rejects.toThrow('no token')
  })

  it('removes a card by its id', async () => {
    const seen = answer(200, { deleted: true, id: 'pm_1' })
    await billing.detach(T, 'pm_1')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/billing/methods/pm_1' })
  })

  it('reads the processor a card is tokenized against', async () => {
    answer(200, { provider: 'square', applicationId: 'sandbox-sq0idb', locationId: 'L1', environment: 'sandbox', live: false })
    expect(await billing.processor(T)).toEqual({ provider: 'square', application: 'sandbox-sq0idb', location: 'L1', environment: 'sandbox', live: false })
  })
})

describe('invoices', () => {
  it('reads each invoice with its lines and its total', async () => {
    const seen = answer(200, {
      count: 1,
      invoices: [
        {
          id: 'inv_1',
          numberStr: 'INV-0042',
          status: 'paid',
          createdAt: '2026-09-01T00:00:00Z',
          subtotal: 10000,
          tax: 800,
          discount: 1000,
          amountPaid: 9800,
          currency: 'usd',
          lineItems: [{ description: 'Max — September', amount: 10000 }],
        },
        { status: 'draft' },
      ],
    })
    const list = await billing.invoices(T)
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/billing/invoices')
    expect(list).toHaveLength(1)
    expect(list[0]).toMatchObject({ number: 'INV-0042', total: 9800, paid: 9800, lines: [{ description: 'Max — September', amount: 10000 }] })
  })

  it('reads the PDF with the bearer and the org', async () => {
    const f = vi.fn(async () => new Response(new Blob(['%PDF']), { status: 200, headers: { 'content-type': 'application/pdf' } }))
    vi.stubGlobal('fetch', f)
    const blob = await billing.pdf(T, 'inv_1')
    expect(blob.size).toBe(4)
    const [url, init] = f.mock.calls[0] as unknown as [string, RequestInit]
    expect(url).toBe('https://api.hanzo.ai/v1/billing/invoices/inv_1/pdf')
    expect(new Headers(init.headers).get('authorization')).toBe('Bearer tok')
    expect(new Headers(init.headers).get('x-org-id')).toBe('acme')
  })

  it('says why a PDF was refused', async () => {
    answer(404, { detail: 'invoice not found' })
    await expect(billing.pdf(T, 'inv_x')).rejects.toMatchObject({ status: 404, message: 'invoice not found' })
  })
})

describe('balance and spend', () => {
  it('reads what can still be spent, and the credit beside it', async () => {
    answer(200, { balance: 5000, holds: 0, available: 4200, account: 'acme' })
    expect(await billing.balance(T)).toBe(4200)
    answer(200, { userId: 'acme', balances: [{ currency: 'usd', available: 1500 }, { currency: 'eur', available: 900 }] })
    expect(await billing.credit(T)).toBe(1500)
  })

  it('tops up from a saved card and answers the balance after', async () => {
    const seen = answer(200, { status: 'ok', balanceCents: 7500, transactionId: 'tx_1' })
    expect(await billing.topup(T, 2500, 'pm_1')).toBe(7500)
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/billing/topup', body: { amountCents: 2500, paymentMethodId: 'pm_1' } })
    await expect(billing.topup(T, 0, 'pm_1')).rejects.toThrow('amount')
    await expect(billing.topup(T, 2500, '')).rejects.toThrow('card')
    expect(seen).toHaveLength(1)
  })

  it('reads the plan’s month: included spend and the request windows', async () => {
    const seen = answer(200, {
      plan: 'max',
      period: '2026-09',
      included: { monthlyCents: 10000, grantedCents: 500, consumedCents: 3000 },
      overageCents: 0,
      windows: [{ span: 'day', limit: 2500, used: 100, remaining: 2400, resets: '2026-09-28T00:00:00Z' }, { limit: 1 }],
    })
    const m = await billing.month(T)
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/billing/usage/rollup')
    expect(m).toMatchObject({ plan: 'max', included: 10500, used: 3000, windows: [{ span: 'day', limit: 2500, used: 100 }] })
  })

  it('reads this month’s spend by category, and says when the ledger did not answer', async () => {
    const seen = answer(200, { spend: { available: true, mtdCents: 4200, byCategory: [{ category: 'LLM', cents: 4000 }, { category: 'Storage', cents: 0 }] } })
    const s = await billing.spend(T)
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/usage/summary?range=month')
    expect(s).toEqual({ known: true, total: 4200, categories: [{ name: 'LLM', cents: 4000 }] })
    answer(200, {})
    expect(await billing.spend(T)).toEqual({ known: false, total: 0, categories: [] })
  })
})

describe('spend caps', () => {
  const ORGWIDE = { id: 'al_1', title: 'Monthly limit', threshold: 50000, enforce: true, project: '', service: '', periodSpentCents: 1200, resetsAt: '2026-10-01T00:00:00Z' }

  it('reads the org-wide monthly cap, and a spend it could not read as unknown', async () => {
    answer(200, [{ id: 'al_2', threshold: 100, project: 'p1' }, ORGWIDE, { id: 'al_3', threshold: 0, rateLimitRpm: 60 }])
    const list = await billing.caps(T)
    expect(billing.monthly(list)?.id).toBe('al_1')
    expect(list[0].spent).toBeNull()
    expect(list[1].spent).toBe(1200)
  })

  it('opens a cap that refuses spend past it when there is none', async () => {
    const seen = answer(201, ORGWIDE)
    await billing.limit(T, [], 50000)
    expect(seen[0]).toMatchObject({
      method: 'POST',
      url: 'https://api.hanzo.ai/v1/billing/alerts',
      body: { title: 'Monthly limit', threshold: 50000, currency: 'usd', project: '', service: '', enforce: true },
    })
  })

  it('changes only the ceiling of the cap there is', async () => {
    const seen = answer(200, { ...ORGWIDE, threshold: 80000 })
    await billing.limit(T, [billing.cap(ORGWIDE)], 80000)
    expect(seen[0]).toMatchObject({ method: 'PATCH', url: 'https://api.hanzo.ai/v1/billing/alerts/al_1', body: { threshold: 80000 } })
  })

  it('lifts a cap, and refuses a limit of nothing', async () => {
    const seen = answer(204, undefined)
    await billing.lift(T, 'al_1')
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/billing/alerts/al_1' })
    await expect(billing.limit(T, [], 0)).rejects.toThrow('monthly limit')
  })

  it('names the org-admin refusal', async () => {
    answer(403, { detail: 'org admin required to change spend caps' })
    await expect(billing.limit(T, [], 100)).rejects.toMatchObject({ status: 403, message: 'org admin required to change spend caps' })
  })
})

describe('members', () => {
  it('reads the org’s logo off its IAM record, enveloped or bare, and none when it has none', async () => {
    const seen = answer(200, { status: 'ok', data: { name: 'acme', logo: 'https://cdn.acme.test/mark.png' } }, { name: 'acme', logo: 'https://x.test/l.svg' }, { status: 'ok', data: { name: 'acme' } })
    expect(await members.logo(T, 'acme')).toBe('https://cdn.acme.test/mark.png')
    expect(seen[0]).toMatchObject({ url: 'https://api.hanzo.ai/v1/iam/organizations/admin/acme', method: 'GET' })
    expect(await members.logo(T, 'acme')).toBe('https://x.test/l.svg')
    expect(await members.logo(T, 'acme')).toBe('')
  })

  it('reads the org’s roster, owners first, people only', async () => {
    const seen = answer(200, {
      status: 'ok',
      data: [
        { user: 'hanzo/zed', org: 'acme', role: 'member', createdTime: '2026-09-02T00:00:00Z' },
        { user: 'acme/dave', org: 'acme', role: 'owner' },
        { team: 'eng', org: 'acme', role: 'admin' },
        { user: 'acme/erin', org: 'acme', role: 'admin', workspace: 'ws1' },
        { user: 'acme/amy', org: 'acme', role: 'boss' },
      ],
      data2: 5,
    })
    const list = await members.roster(T, 'acme')
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/iam/memberships?org=acme')
    expect(list.map((m) => [m.user, m.role])).toEqual([
      ['acme/dave', 'owner'],
      ['acme/amy', 'member'],
      ['hanzo/zed', 'member'],
    ])
    expect(list[0].name).toBe('dave')
  })

  it('reads IAM’s refusal as a refusal', async () => {
    answer(400, { status: 'error', msg: 'auth:Unauthorized operation' })
    await expect(members.roster(T, 'acme')).rejects.toMatchObject({ status: 400, message: 'auth:Unauthorized operation' })
  })

  it('lists the org’s invitations', async () => {
    const seen = answer(200, { invitations: [{ owner: 'acme', name: 'invite-amy-1a2b3c', email: 'amy@acme.test', code: 'c0de', quota: 1, usedCount: 0, state: 'Active' }], total: 1 })
    const list = await members.invitations(T, 'acme')
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/iam/invitations?owner=acme')
    expect(list[0]).toMatchObject({ email: 'amy@acme.test', code: 'c0de', seats: 1, used: 0, state: 'Active' })
  })

  it('invites one address into the org with one redeemable seat', async () => {
    const seen = answer(200, { owner: 'acme', name: 'invite-amy-lee-000000', email: 'amy.lee@acme.test', quota: 1, state: 'Active' })
    await members.invite(T, 'acme', ' Amy.Lee@Acme.test ')
    const body = seen[0].body as Record<string, unknown>
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/iam/invitations' })
    expect(body).toMatchObject({ owner: 'acme', email: 'amy.lee@acme.test', displayName: 'amy.lee@acme.test', quota: 1, usedCount: 0, state: 'Active' })
    expect(body.name).toMatch(/^invite-amy-lee-[0-9a-f]{6}$/)
    expect(body.code).toMatch(/^[0-9a-f]{16}$/)
  })

  it('refuses what is not an address, before sending anything', async () => {
    const seen = answer(200, {})
    await expect(members.invite(T, 'acme', 'amy')).rejects.toThrow('not an email address')
    expect(seen).toHaveLength(0)
  })

  it('withdraws an invitation by its owner and name', async () => {
    const seen = answer(200, { deleted: true })
    await members.revoke(T, { owner: 'acme', name: 'invite-amy-1a2b3c' })
    expect(seen[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/iam/invitations/acme/invite-amy-1a2b3c' })
  })
})

describe('connectors', () => {
  it('reads a connector and its connected account', async () => {
    const seen = answer(200, { id: 'slack', name: 'Slack', available: true, connected: true, connection: { account: 'Acme HQ', connectedAt: '2026-09-01T00:00:00Z' } })
    const c = await provider.read(T, 'slack')
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/provider/slack')
    expect(c).toMatchObject({ connected: true, account: 'Acme HQ', available: true })
  })

  it('starts the consent flow, and follows it only to the provider’s own host', async () => {
    const seen = answer(200, { authorizeUrl: 'https://slack.com/oauth/v2/authorize?client_id=1&state=s' })
    expect(await provider.authorize(T, 'slack')).toBe('https://slack.com/oauth/v2/authorize?client_id=1&state=s')
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/provider/slack/connect', body: {} })
    answer(200, { authorizeUrl: 'https://evil.example/slack' })
    await expect(provider.authorize(T, 'slack')).rejects.toThrow('did not name a slack address')
    answer(200, { authorizeUrl: 'https://github.com/apps/hanzo/installations/new?state=s' })
    expect(await provider.authorize(T, 'github')).toContain('https://github.com/apps/hanzo/installations/new')
    const back = answer(200, { authorizeUrl: 'https://slack.com/oauth/v2/authorize?client_id=1&state=s' })
    await provider.authorize(T, 'slack', 'https://hanzo.ai/?at=-/settings/integrations')
    expect(back[0]).toMatchObject({ body: { return: 'https://hanzo.ai/?at=-/settings/integrations' } })
  })

  it('disconnects, and lists the workspace’s channels', async () => {
    const seen = answer(200, { disconnected: true })
    await provider.disconnect(T, 'slack')
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/provider/slack/disconnect', body: {} })
    // One GitHub account, by its login; the org's other accounts stay.
    const one = answer(200, { disconnected: true, accounts: ['Darkhorse7stars'], uninstalled: ['Darkhorse7stars'] })
    await provider.disconnect(T, 'github', 'Darkhorse7stars')
    expect(one[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/provider/github/disconnect', body: { account: 'Darkhorse7stars' } })
    const more = answer(200, { channels: [{ id: 'C1', name: 'general', is_member: true }, { id: 'C2', name: 'ops', is_private: true }, { name: 'x' }], next_cursor: 'n2' })
    const r = await provider.channels(T, 'n1')
    expect(more[0].url).toBe('https://api.hanzo.ai/v1/provider/slack/channels?cursor=n1')
    expect(r).toEqual({
      channels: [
        { id: 'C1', name: 'general', private: false, member: true },
        { id: 'C2', name: 'ops', private: true, member: false },
      ],
      next: 'n2',
    })
  })

  it('lists the GitHub accounts this org has bound the App on', async () => {
    const seen = answer(200, {
      installUrl: 'https://github.com/apps/hanzo/installations/new',
      installations: [{ login: 'acme', type: 'Organization', grant: 'all', connected: true, htmlUrl: 'https://github.com/acme' }, { type: 'User' }],
    })
    expect(await github.installations(T)).toEqual([{ login: 'acme', type: 'Organization', grant: 'all', connected: true }])
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/provider/github/installations')
  })

  it('reads as connected through the org’s App install when the person has not connected their own', () => {
    const none = { configured: true, connected: false, login: '' }
    const install = (login: string, connected = true) => ({ login, type: 'User', grant: 'all', connected })
    expect(github.linked(none, [install('Darkhorse7stars')])).toEqual({ configured: true, connected: true, login: 'Darkhorse7stars' })
    expect(github.linked(none, [install('acme'), install('gone', false), install('zoo')])).toEqual({ configured: true, connected: true, login: 'acme, @zoo' })
    expect(github.linked(none, [install('gone', false)])).toEqual(none)
    expect(github.linked(none, [])).toEqual(none)
    // The person's own connection wins: it is who GitHub acts as.
    const mine = { configured: true, connected: true, login: 'zeekay' }
    expect(github.linked(mine, [install('acme')])).toBe(mine)
  })

  it('says a GitHub change on the window, where every surface showing GitHub hears it', () => {
    // No window (a server render): nothing to say it on.
    expect(() => github.changed()).not.toThrow()
    const page = new EventTarget()
    vi.stubGlobal('window', page)
    let reads = 0
    const stop = github.heard(() => {
      reads += 1
    })
    github.changed()
    expect(reads).toBe(1)
    // The name the Hanzo app's Quick setup (@hanzo/app Setup) says and hears.
    expect(github.CHANGED).toBe('hanzo:github')
    page.dispatchEvent(new Event('hanzo:github'))
    expect(reads).toBe(2)
    stop()
    github.changed()
    expect(reads).toBe(2)
  })
})

describe('webhooks', () => {
  it('lists the org’s endpoints with their week', async () => {
    const seen = answer(200, { data: [{ id: 'wh_1', url: 'https://acme.test/h', events: ['commerce.order.>'], status: 'active', deliveries7d: 12, failures7d: 1 }, { url: 'x' }] })
    const list = await webhooks.endpoints(T)
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/webhook')
    expect(list).toHaveLength(1)
    expect(list[0]).toMatchObject({ events: ['commerce.order.>'], deliveries: 12, failures: 1, secret: '' })
  })

  it('reads typed patterns, and refuses an address that is not https before sending', async () => {
    expect(webhooks.patterns(' commerce.order.>, event.>  event.> ')).toEqual(['commerce.order.>', 'event.>'])
    const seen = answer(201, {})
    await expect(webhooks.add(T, { url: 'http://acme.test/h', events: [], description: '' })).rejects.toThrow('https only')
    await expect(webhooks.add(T, { url: 'not a url', events: [], description: '' })).rejects.toThrow('https address')
    expect(seen).toHaveLength(0)
  })

  it('adds an endpoint and hands back its secret once', async () => {
    const seen = answer(201, { id: 'wh_2', url: 'https://acme.test/h', events: [], secret: 'whsec_1', status: 'active' })
    const e = await webhooks.add(T, { url: ' https://acme.test/h ', events: [], description: ' orders ' })
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/webhook', body: { url: 'https://acme.test/h', events: [], description: 'orders' } })
    expect(e.secret).toBe('whsec_1')
  })

  it('sends a test, reads the log, and deletes', async () => {
    const seen = answer(200, { delivered: false, httpStatus: 0, durationMs: 5000, error: 'timeout' })
    expect(await webhooks.test(T, 'wh_1')).toEqual({ delivered: false, status: 0, ms: 5000, error: 'timeout' })
    expect(seen[0]).toMatchObject({ method: 'POST', url: 'https://api.hanzo.ai/v1/webhook/wh_1/test' })
    const log = answer(200, { data: [{ subject: 'webhook.test', status: 'ok', httpStatus: 200, attempt: 1, created: '2026-09-27T00:00:00Z' }] })
    expect(await webhooks.deliveries(T, 'wh_1')).toEqual([{ subject: 'webhook.test', status: 'ok', code: 200, attempt: 1, error: '', at: '2026-09-27T00:00:00Z' }])
    expect(log[0].url).toBe('https://api.hanzo.ai/v1/webhook/wh_1/deliveries?limit=10')
    const gone = answer(204, undefined)
    await webhooks.remove(T, 'wh_1')
    expect(gone[0]).toMatchObject({ method: 'DELETE', url: 'https://api.hanzo.ai/v1/webhook/wh_1' })
  })
})

describe('what pays for another maker’s models', () => {
  it('is the prepaid figure of the balance, which never counts granted credit', async () => {
    const seen = answer(200, { available: 5000, prepaid: 1240, credit: 3760 })
    expect(await billing.prepaid(T)).toBe(1240)
    expect(seen[0]!.url).toBe('https://api.hanzo.ai/v1/billing/balance')
  })

  it('is zero when the account holds credit and nothing prepaid', async () => {
    answer(200, { available: 5000, prepaid: 0, credit: 5000 })
    expect(await billing.prepaid(T)).toBe(0)
  })

  it('is not assumed where the answer carries no prepaid figure', async () => {
    answer(200, { available: 5000 })
    expect(await billing.prepaid(T)).toBeNull()
  })
})
