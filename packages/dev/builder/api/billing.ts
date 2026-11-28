/**
 * The organization's money: its plan, the cards it pays with, its invoices, its
 * balance and credits, what it has spent this month, and the caps on that spend.
 *
 *   GET    /v1/billing/plans                     the catalog the serving brand sells (public)
 *   GET    /v1/billing/subscriptions             {count, subscriptions}
 *   POST   /v1/billing/subscribe/card            buy a plan with a saved card; one paid plan at a time (409)
 *   POST   /v1/billing/subscriptions/{id}/cancel     at the end of the paid period
 *   POST   /v1/billing/subscriptions/{id}/reactivate
 *   GET    /v1/billing/methods                   saved cards, newest first
 *   POST   /v1/billing/methods                   save a card from the processor's single-use token
 *   DELETE /v1/billing/methods/{id}
 *   GET    /v1/billing/settings                  the processor a browser tokenizes a card against
 *   GET    /v1/billing/invoices                  {count, cursor, invoices}
 *   GET    /v1/billing/invoices/{id}/pdf         the invoice as a PDF
 *   GET    /v1/billing/balance                   prepaid balance, whole cents
 *   GET    /v1/billing/credit-balance            granted credit, per currency
 *   GET    /v1/billing/credits                   {grants}: every grant, spent and lapsed included
 *   GET    /v1/billing/tier                      the rung served and its tier class
 *   GET    /v1/billing/recharge                  the auto-reload rule
 *   PUT    /v1/billing/recharge                  arm it (a card on file) or disarm it
 *   POST   /v1/billing/topup                     charge a saved card into the balance
 *   GET    /v1/billing/usage/rollup              the plan's month: included spend and request windows
 *   GET    /v1/usage/summary?range=month         this month's spend, by category
 *   GET    /v1/allowance                         the Free plan's allowance, and the pool it is served from
 *   GET    /v1/billing/alerts                    spend caps (bare array)
 *   POST   /v1/billing/alerts                    open a cap (org admin)
 *   PATCH  /v1/billing/alerts/{id}               change one (org admin)
 *   DELETE /v1/billing/alerts/{id}               lift one (org admin)
 *
 * Every figure is whole cents. The wallet, the caps and the cards are the
 * caller's own, resolved by the platform from the credential; nothing here names
 * an org or a subject. No card number passes through this file: a card is saved
 * from the token the processor's own form returns.
 */
import { phrase } from '@hanzo/i18n'

import { call, headers, reason, Refusal, seg, type Target } from './call.ts'

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
const arr = (v: unknown): unknown[] => (Array.isArray(v) ? v : [])
const words = (v: unknown): string[] => arr(v).filter((s): s is string => typeof s === 'string' && s !== '')

/** `$1,234.56` from whole cents; whole dollars drop the cents. */
export function money(cents: number, currency = 'usd'): string {
  const whole = cents % 100 === 0
  try {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: (currency || 'usd').toUpperCase(),
      minimumFractionDigits: whole ? 0 : 2,
      maximumFractionDigits: 2,
    }).format(cents / 100)
  } catch {
    return `${(cents / 100).toFixed(whole ? 0 : 2)} ${currency.toUpperCase()}`
  }
}

/** Whole cents from what a person typed as dollars — `25`, `$25.50` — or null. */
export function cents(typed: string): number | null {
  const s = typed.replace(/[$,\s]/g, '')
  if (!/^\d+(\.\d{1,2})?$/.test(s)) return null
  const c = Math.round(Number(s) * 100)
  return c > 0 ? c : null
}

// ── plans ────────────────────────────────────────────────────────────────────

export interface Plan {
  id: string
  name: string
  description: string
  /** personal, team, enterprise, … */
  category: string
  /** Per month, billed monthly. 0 is free. */
  monthly: number
  /** Per month, billed yearly; 0 when the plan has no yearly price. */
  yearly: number
  currency: string
  perSeat: boolean
  /** Priced by talking to sales; nothing to buy here. */
  sales: boolean
  popular: boolean
  features: string[]
}

export function plan(raw: unknown): Plan {
  const p = obj(raw)
  return {
    id: str(p.slug) || str(p.id),
    name: str(p.name),
    description: str(p.description),
    category: str(p.category),
    monthly: num(p.price),
    yearly: num(p.priceAnnual),
    currency: str(p.currency) || 'usd',
    perSeat: p.perSeat === true,
    sales: p.contactSales === true,
    popular: p.popular === true,
    features: words(p.features),
  }
}

/** The plans a person can hold. The catalog also carries add-ons (DNS, storage) that are not. */
const HELD = ['personal', 'team', 'enterprise']

export async function plans(t: Target): Promise<Plan[]> {
  const raw = await call<unknown>(t, 'GET', '/v1/billing/plans')
  const rows = Array.isArray(raw) ? raw : arr(obj(raw).plans)
  return rows.map(plan).filter((p) => p.id && p.name && (!p.category || HELD.includes(p.category)))
}

// ── subscriptions ────────────────────────────────────────────────────────────

export interface Subscription {
  id: string
  plan: string
  name: string
  /** Per period, per seat. */
  price: number
  interval: string
  currency: string
  seats: number
  /** trialing, active, past_due, canceled, unpaid. */
  status: string
  /** RFC 3339: paid through, and renews or ends then. */
  ends: string
  /** Ends at `ends` rather than renewing. */
  ending: boolean
  /** The saved card a renewal charges, or ''. */
  method: string
}

export function subscription(raw: unknown): Subscription {
  const s = obj(raw)
  const p = obj(s.plan)
  return {
    id: str(s.id),
    plan: str(s.planId) || str(p.id),
    name: str(p.name) || str(s.planId),
    price: num(p.price),
    interval: str(p.interval) || 'month',
    currency: str(p.currency) || 'usd',
    seats: num(s.quantity) || 1,
    status: str(s.status),
    ends: str(s.currentPeriodEnd),
    ending: s.cancelAtPeriodEnd === true,
    method: str(s.defaultPaymentMethod),
  }
}

/** Only these confer the plan. */
const LIVE = ['active', 'trialing', 'past_due']

/** The plan the org is on, or null for none (the free tier). */
export function current(list: Subscription[]): Subscription | null {
  return list.find((s) => LIVE.includes(s.status)) ?? null
}

export async function subscriptions(t: Target): Promise<Subscription[]> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/billing/subscriptions'))
  return arr(raw.subscriptions).map(subscription).filter((s) => s.id)
}

export interface Purchase {
  plan: string
  /** month | year */
  interval: 'month' | 'year'
  /** The saved card that pays. */
  method: string
}

/** Buy a plan. No amount is sent: the catalog prices it, and the platform charges that. */
export async function subscribe(t: Target, p: Purchase): Promise<{ subscription: string; charged: number }> {
  if (!p.method) throw new Error(phrase('Add a card before choosing a paid plan'))
  const raw = obj(
    await call<unknown>(t, 'POST', '/v1/billing/subscribe/card', { planId: p.plan, paymentMethodId: p.method, interval: p.interval }),
  )
  return { subscription: str(raw.subscriptionId), charged: num(raw.amountCents) }
}

/** End a plan when its paid period does, not before. */
export async function cancel(t: Target, id: string): Promise<Subscription> {
  return subscription(await call<unknown>(t, 'POST', `/v1/billing/subscriptions/${seg(id)}/cancel`, { atPeriodEnd: true }))
}

export async function reactivate(t: Target, id: string): Promise<Subscription> {
  return subscription(await call<unknown>(t, 'POST', `/v1/billing/subscriptions/${seg(id)}/reactivate`, {}))
}

// ── cards ────────────────────────────────────────────────────────────────────

export interface Address {
  name?: string
  line1?: string
  line2?: string
  city?: string
  state?: string
  postalCode?: string
  country?: string
}

export interface Method {
  id: string
  type: string
  brand: string
  last4: string
  /** `MM/YY`, or ''. */
  expires: string
  default: boolean
  /** What the platform calls it when it is not a card. */
  name: string
  billingAddress?: Address
}

export function method(raw: unknown): Method {
  const m = obj(raw)
  const c = obj(m.card)
  const a = obj(m.billingAddress)
  const month = num(c.expMonth)
  const year = num(c.expYear)
  return {
    id: str(m.id),
    type: str(m.type) || 'card',
    brand: str(c.brand),
    last4: str(c.last4),
    expires: month && year ? `${String(month).padStart(2, '0')}/${String(year % 100).padStart(2, '0')}` : '',
    default: m.isDefault === true,
    name: str(m.name),
    billingAddress:
      a && (a.line1 || a.city || a.country || a.name || a.postalCode || a.state)
        ? {
            name: str(a.name),
            line1: str(a.line1),
            line2: str(a.line2),
            city: str(a.city),
            state: str(a.state),
            postalCode: str(a.postalCode),
            country: str(a.country),
          }
        : undefined,
  }
}

/** Formats a billing address as a readable line: `Acme Inc · 123 Main St · San Francisco CA 94105 · US`. */
export function formatAddress(a?: Address): string {
  if (!a) return ''
  const parts = [
    a.name,
    [a.line1, a.line2].filter(Boolean).join(', '),
    [a.city, a.state, a.postalCode].filter(Boolean).join(' '),
    a.country,
  ].filter(Boolean)
  return parts.join(' · ')
}

/** How a saved method reads in a row: `Visa •••• 4242`. */
export function label(m: Method): string {
  if (m.last4) return `${m.brand ? m.brand.charAt(0).toUpperCase() + m.brand.slice(1) : 'Card'} •••• ${m.last4}`
  return m.name || m.type
}

export async function methods(t: Target): Promise<Method[]> {
  const raw = await call<unknown>(t, 'GET', '/v1/billing/methods')
  const rows = Array.isArray(raw) ? raw : arr(obj(raw).data)
  return rows.map(method).filter((m) => m.id)
}

/** The card a charge uses: the default, or the newest. */
export function chosen(list: Method[]): Method | null {
  return list.find((m) => m.default) ?? list[0] ?? null
}

/** Save a card from the single-use token the processor's form returned, optionally with billing info. */
export async function save(t: Target, token: string, billingAddress?: Address): Promise<Method> {
  if (!token) throw new Error(phrase('The card form returned no token'))
  const body: Record<string, unknown> = { type: 'card', providerRef: token }
  if (billingAddress && Object.values(billingAddress).some(Boolean)) {
    body.billingAddress = billingAddress
  }
  return method(await call<unknown>(t, 'POST', '/v1/billing/methods', body))
}

export async function updateMethod(t: Target, id: string, what: { billingAddress?: Address }): Promise<Method> {
  return method(await call<unknown>(t, 'PATCH', `/v1/billing/methods/${seg(id)}`, what))
}

export async function detach(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/billing/methods/${seg(id)}`)
}

export async function makeDefault(t: Target, id: string): Promise<Method> {
  return method(await call<unknown>(t, 'POST', `/v1/billing/methods/${seg(id)}/default`))
}

export interface Processor {
  /** `square`, or '' where none is configured. */
  provider: string
  application: string
  location: string
  /** `production` charges real cards; anything else is the sandbox. */
  environment: string
  live: boolean
}

export async function processor(t: Target): Promise<Processor> {
  const p = obj(await call<unknown>(t, 'GET', '/v1/billing/settings'))
  return {
    provider: str(p.provider),
    application: str(p.applicationId),
    location: str(p.locationId),
    environment: str(p.environment),
    live: p.live === true,
  }
}

// ── invoices ─────────────────────────────────────────────────────────────────

export interface Line {
  description: string
  amount: number
}

export interface Invoice {
  id: string
  /** `INV-0042`, or '' for a draft. */
  number: string
  date: string
  status: string
  currency: string
  subtotal: number
  tax: number
  discount: number
  credit: number
  paid: number
  due: number
  /** What the invoice comes to before credit is applied. */
  total: number
  start: string
  end: string
  lines: Line[]
}

export function invoice(raw: unknown): Invoice {
  const i = obj(raw)
  const subtotal = num(i.subtotal)
  const tax = num(i.tax)
  const discount = num(i.discount)
  return {
    id: str(i.id),
    number: str(i.numberStr) || (num(i.number) ? String(num(i.number)) : ''),
    date: str(i.createdAt),
    status: str(i.status),
    currency: str(i.currency) || 'usd',
    subtotal,
    tax,
    discount,
    credit: num(i.creditApplied),
    paid: num(i.amountPaid),
    due: num(i.amountDue),
    total: subtotal - discount + tax,
    start: str(i.periodStart),
    end: str(i.periodEnd),
    lines: arr(i.lineItems)
      .map(obj)
      .map((l) => ({ description: str(l.description) || str(l.planName), amount: num(l.amount) })),
  }
}

export async function invoices(t: Target): Promise<Invoice[]> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/billing/invoices'))
  return arr(raw.invoices).map(invoice).filter((i) => i.id)
}

/** The invoice as a PDF, read with the caller's bearer. */
export async function pdf(t: Target, id: string): Promise<Blob> {
  const res = await fetch(`${t.api}/v1/billing/invoices/${seg(id)}/pdf`, { headers: headers(t), cache: 'no-store' })
  if (!res.ok) throw new Refusal(res.status, (await reason(res)) || `The invoice answered ${res.status}`)
  return res.blob()
}

// ── balance, credit, spend ───────────────────────────────────────────────────

/** What the org can still spend from its prepaid balance, in cents. */
export async function balance(t: Target): Promise<number> {
  const b = obj(await call<unknown>(t, 'GET', '/v1/billing/balance'))
  return num(b.available ?? b.balance)
}

/**
 * What can pay for another maker's models, whole cents: cash and the credit line, never
 * granted credit (`GET /v1/billing/balance` `prepaid`). Null where the answer carries no
 * such figure, so a caller does not assume it.
 */
export async function prepaid(t: Target): Promise<number | null> {
  const b = obj(await call<unknown>(t, 'GET', '/v1/billing/balance'))
  return typeof b.prepaid === 'number' && Number.isFinite(b.prepaid) ? b.prepaid : null
}

/** Granted credit the org can spend, in US cents. */
export async function credit(t: Target): Promise<number> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/billing/credit-balance'))
  return arr(raw.balances)
    .map(obj)
    .filter((b) => (str(b.currency) || 'usd').toLowerCase() === 'usd')
    .reduce((sum, b) => sum + num(b.available), 0)
}

/** One grant of credit, spent, lapsed and voided included: the ledger, not the spendable figure. */
export interface Grant {
  id: string
  name: string
  amount: number
  remaining: number
  currency: string
  /** RFC 3339, or '' for one that does not lapse. */
  expires: string
  active: boolean
}

export async function grants(t: Target): Promise<Grant[]> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/billing/credits'))
  return arr(raw.grants)
    .map(obj)
    .map((g) => ({
      id: str(g.id),
      name: str(g.name),
      amount: num(g.amountCents),
      remaining: num(g.remainingCents),
      currency: str(g.currency) || 'usd',
      expires: str(g.expiresAt),
      active: g.active === true && g.voided !== true,
    }))
    .filter((g) => g.id)
}

/**
 * The rung an organization is served as (`max-20x`, '' with none), the tier
 * class it is served under (`pro`), and the display name of the plan its
 * subscription is on (`planName`, `Unlimited`; '' with none).
 */
export interface Tier {
  plan: string
  tier: string
  name: string
}

export async function tier(t: Target): Promise<Tier> {
  const r = obj(await call<unknown>(t, 'GET', '/v1/billing/tier'))
  return { plan: str(r.plan), tier: str(obj(r.tier).name), name: str(r.planName) }
}

/** Auto-reload: top the balance up by `amount` whenever it falls below `threshold`, charging the card on file. */
export interface Recharge {
  enabled: boolean
  threshold: number
  amount: number
  currency: string
  /** RFC 3339 of the last reload, or ''. */
  last: string
}

function recharged(raw: unknown): Recharge {
  const r = obj(raw)
  return {
    enabled: r.enabled === true,
    threshold: num(r.thresholdCents),
    amount: num(r.amountCents),
    currency: str(r.currency) || 'usd',
    last: str(r.lastRechargedAt),
  }
}

/** The rule as stored; one never set reads as off. */
export async function recharge(t: Target): Promise<Recharge> {
  return recharged(await call<unknown>(t, 'GET', '/v1/billing/recharge'))
}

/** Arm or disarm the rule. Turning it on needs a card on file, and the platform says so when there is none. */
export async function arm(t: Target, rule: { enabled: boolean; threshold: number; amount: number }): Promise<Recharge> {
  return recharged(
    await call<unknown>(t, 'PUT', '/v1/billing/recharge', { enabled: rule.enabled, thresholdCents: rule.threshold, amountCents: rule.amount, currency: 'usd' }),
  )
}

/** Charge a saved card and add the amount to the balance; answers the balance after. */
export async function topup(t: Target, amount: number, card: string): Promise<number> {
  if (!Number.isInteger(amount) || amount <= 0) throw new Error(phrase('Enter an amount to add'))
  if (!card) throw new Error(phrase('Add a card before buying more'))
  const r = obj(await call<unknown>(t, 'POST', '/v1/billing/topup', { amountCents: amount, paymentMethodId: card }))
  return num(r.balanceCents)
}

export interface Window {
  /** hour, day, week or month. */
  span: string
  /** Requests the plan includes in this span; 0 is no bound. */
  limit: number
  used: number
  /** RFC 3339. */
  resets: string
}

export interface Month {
  plan: string
  /** `2026-09`. */
  period: string
  /** Spend the plan includes this month, and how much of it is used. */
  included: number
  used: number
  overage: number
  windows: Window[]
}

export async function month(t: Target): Promise<Month> {
  const r = obj(await call<unknown>(t, 'GET', '/v1/billing/usage/rollup'))
  const inc = obj(r.included)
  return {
    plan: str(r.plan),
    period: str(r.period),
    included: num(inc.monthlyCents) + num(inc.grantedCents),
    used: num(inc.consumedCents),
    overage: num(r.overageCents),
    windows: arr(r.windows)
      .map(obj)
      .map((w) => ({ span: str(w.span), limit: num(w.limit), used: num(w.used), resets: str(w.resets) }))
      .filter((w) => w.span),
  }
}

export interface Spend {
  /** Whether the ledger answered; false means the zeros are not measurements. */
  known: boolean
  /** Month to date. */
  total: number
  categories: { name: string; cents: number }[]
}

export async function spend(t: Target): Promise<Spend> {
  const r = obj(await call<unknown>(t, 'GET', '/v1/usage/summary?range=month'))
  const s = obj(r.spend)
  return {
    known: s.available === true,
    total: num(s.mtdCents) || num(s.totalCents),
    categories: arr(s.byCategory)
      .map(obj)
      .map((c) => ({ name: str(c.category) || 'Uncategorized', cents: num(c.cents) }))
      .filter((c) => c.cents > 0),
  }
}

// ── the Free plan ────────────────────────────────────────────────────────────

/** The pool every free user shares, as its vendor accounts last said. */
export interface Pool {
  /** available, busy or exhausted. */
  state: string
  /** RFC 3339: when the pool refills, where the platform says; ''. */
  resets: string
}

/**
 * What the caller has left of the Free plan: limited usage, from a pool shared
 * by all free users. `pooled` is false for a paid plan, whose usage is its spend.
 */
export interface Allowance {
  plan: string
  /** Requests allowed in `window`; 0 is no bound. */
  limit: number
  used: number
  spent: boolean
  /** hour or day. */
  window: string
  /** RFC 3339. */
  resets: string
  pooled: boolean
  /** Absent when the platform could not read it. */
  pool: Pool | null
}

const stamp = (unix: unknown): string => (num(unix) > 0 ? new Date(num(unix) * 1000).toISOString() : '')

export async function allowance(t: Target): Promise<Allowance> {
  const r = obj(await call<unknown>(t, 'GET', '/v1/allowance'))
  const p = r.pool && typeof r.pool === 'object' ? obj(r.pool) : null
  return {
    plan: str(r.plan),
    limit: num(r.limit),
    used: num(r.used),
    spent: r.spent === true,
    window: str(r.window),
    resets: stamp(r.resets),
    pooled: r.pooled === true,
    pool: p && str(p.state) ? { state: str(p.state), resets: stamp(p.resets) } : null,
  }
}

// ── spend caps ───────────────────────────────────────────────────────────────

export interface Cap {
  id: string
  title: string
  /** The ceiling for the calendar month, in cents; 0 bounds no spend. */
  threshold: number
  /** Refuses spend past the ceiling, rather than only warning. */
  enforce: boolean
  project: string
  service: string
  /** Spent this month against it, or null when the platform could not read it. */
  spent: number | null
  resets: string
}

export function cap(raw: unknown): Cap {
  const a = obj(raw)
  return {
    id: str(a.id),
    title: str(a.title),
    threshold: num(a.threshold),
    enforce: a.enforce === true,
    project: str(a.project),
    service: str(a.service),
    spent: typeof a.periodSpentCents === 'number' ? a.periodSpentCents : null,
    resets: str(a.resetsAt),
  }
}

export async function caps(t: Target): Promise<Cap[]> {
  const raw = await call<unknown>(t, 'GET', '/v1/billing/alerts')
  return arr(raw).map(cap).filter((c) => c.id)
}

/** The org-wide monthly cap: no project, no service, a ceiling. */
export function monthly(list: Cap[]): Cap | null {
  return list.find((c) => !c.project && !c.service && c.threshold > 0) ?? null
}

/** Set the org's monthly cap: change the one there is, or open one that refuses spend past it. */
export async function limit(t: Target, list: Cap[], threshold: number): Promise<Cap> {
  if (!Number.isInteger(threshold) || threshold <= 0) throw new Error(phrase('Enter a monthly limit'))
  const was = monthly(list)
  if (was) return cap(await call<unknown>(t, 'PATCH', `/v1/billing/alerts/${seg(was.id)}`, { threshold }))
  return cap(
    await call<unknown>(t, 'POST', '/v1/billing/alerts', {
      title: 'Monthly limit',
      threshold,
      currency: 'usd',
      project: '',
      service: '',
      enforce: true,
      softPct: 80,
      rateLimitRpm: 0,
    }),
  )
}

export async function lift(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/billing/alerts/${seg(id)}`)
}
