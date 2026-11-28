/**
 * A plan as a person reads it, and how much of it is used.
 *
 * NAMED BY FAMILY. A slug is the catalog's rung (`max-20x`); a person reads the
 * family — Free, Go, Pro, Max, Team, Agency, Enterprise — with the rung inside
 * it as a small tag (`20x`). A slug outside the families reads as words
 * (`growth-plus` is Growth Plus); one that is an id is never printed, and reads
 * as the tier it is served under. Where billing names the plan the subscription
 * is on (`GET /v1/billing/tier` `planName`, `Unlimited`), that name is the
 * label, whatever the slug.
 *
 * MEASURED IN SHARES. `GET /v1/ai/limits` (cloud apps/ai/limits_ops.go) answers
 * every window as a percent used, its state and when it starts over — the
 * session, the UTC day, and each class's billing period — never an amount.
 *
 * THREE KINDS OF ACCOUNT, three meters: on a PLAN its windows are the meter, on
 * FREE the free allowance left today is, and with CREDITS and no plan (pay as
 * you go) the balance is. A plan's meter never shows money.
 *
 * WORDS in English: a family's name is a name, the rest is marked (`phrase`)
 * or takes the caller's @hanzo/i18n `t`, so a surface says it in the reader's
 * language.
 */
import { phrase } from '@hanzo/i18n'
import type { Say } from '@hanzo/i18n/react'

import { english } from './say.ts'

export interface Label {
  /** The family: `Max`. */
  name: string
  /** The rung inside it, drawn small beside the name: `20x`; '' for none. */
  tag: string
}

/** The families, by the slug's first word. `dev` is the slug Pro is sold under. */
const FAMILY: Record<string, string> = {
  free: phrase('Free'),
  go: 'Go',
  dev: 'Pro',
  pro: 'Pro',
  max: 'Max',
  team: 'Team',
  agency: 'Agency',
  advisory: 'Agency',
  dedicated: 'Agency',
  pod: 'Agency',
  enterprise: 'Enterprise',
}

/** The tier classes billing serves a rung under (`GET /v1/billing/tier` `tier.name`) that name a plan. */
const TIER: Record<string, string> = { pro: 'Pro', enterprise: 'Enterprise' }

/** A slug that is an id rather than a name: a UUID, a run of hex, a prefixed id, or digits. */
const ID = /^[0-9a-f]{8}-[0-9a-f]{4}-|[0-9a-f]{12,}|^[a-z]+_[0-9a-z]{6,}$|^\d+$/i

/** A word as a title reads it; a multiple (`2x`) stays as it is. */
const word = (w: string): string => (/^\d+x$/.test(w) ? w : w.charAt(0).toUpperCase() + w.slice(1))

/**
 * A plan as a person reads it: the plan's display name where billing gives one
 * (`Unlimited`), else from the slug billing serves (`max-20x`) and the tier
 * class beside it (`pro`). Null where none names a plan: no name, no slug and a
 * free or trial tier, which is no plan at all.
 */
export function label(slug?: string | null, tier?: string | null, name?: string | null): Label | null {
  const named = (name ?? '').trim()
  if (named) return { name: named, tag: '' }
  const raw = (slug ?? '').trim().toLowerCase()
  const fallback = (): Label | null => {
    const t = TIER[(tier ?? '').trim().toLowerCase()]
    return t ? { name: t, tag: '' } : null
  }
  if (!raw) return fallback()
  // A rung that is an id still IS a rung: the account is on a plan, so it reads as one.
  if (ID.test(raw)) return fallback() ?? { name: phrase('Paid plan'), tag: '' }
  const s = raw.replace(/[\s_]+/g, '-')
  const [head, ...rest] = s.split('-').filter(Boolean)
  const family = FAMILY[head]
  if (!family) return { name: s.split('-').filter(Boolean).map(word).join(' '), tag: '' }
  const tail = rest.join('-')
  const tag = /^\d+x$/.test(tail) ? tail : /^(annual|yearly|year)$/.test(tail) ? phrase('Annual') : tail ? rest.map(word).join(' ') : ''
  return { name: family, tag }
}

/** A label as a sentence says it: `Max 20x`. */
export const said = (l: Label | null): string => (l ? (l.tag ? `${l.name} ${l.tag}` : l.name) : '')

export type State = 'ok' | 'near' | 'limited'
const STATES: readonly State[] = ['ok', 'near', 'limited']

/** One window, as a share used. */
export interface Share {
  /** Used, 0 to 100. */
  percent: number
  state: State
  /** When it starts over, RFC 3339; '' for a session not running, which starts with the next request. */
  resets: string
}

const obj = (v: unknown): Record<string, unknown> | null => (v && typeof v === 'object' && !Array.isArray(v) ? (v as Record<string, unknown>) : null)

/**
 * One window off the wire, or null for a window that bounds nothing. The
 * server states a window's state only when the plan sets it a limit
 * (limits_ops.go `window`), so a window with no state is unbounded and not drawn.
 */
export function share(v: unknown): Share | null {
  const o = obj(v)
  const state = STATES.find((s) => s === o?.state)
  if (!o || !state || typeof o.percent !== 'number' || !Number.isFinite(o.percent)) return null
  return {
    percent: Math.min(100, Math.max(0, Math.round(o.percent))),
    state,
    resets: typeof o.resets_at === 'string' ? o.resets_at : '',
  }
}

/** Which meter an account reads. */
export type Kind = 'plan' | 'free' | 'credits'

/**
 * The meter for a plan slug (billing's or the limits', `free` or '' for none),
 * the prepaid balance in cents (null where it could not be read) and the tier
 * class billing serves the account under: a paid plan's windows — a rung, or a
 * paid class with no rung (a plan recorded outside checkout, a partner org) —
 * and with no plan, the balance where there is money in it, else the free
 * allowance.
 */
export function kind(plan: string | null | undefined, balance: number | null, tier?: string | null): Kind {
  const p = (plan ?? '').trim().toLowerCase()
  if ((p && p !== 'free') || TIER[(tier ?? '').trim().toLowerCase()]) return 'plan'
  return balance !== null && balance > 0 ? 'credits' : 'free'
}

/**
 * When something starts over, as a sentence says it: a time for within a day
 * (`5:00 PM`), else a date (`Nov 1`), periods ending on UTC days. '' for none.
 */
export function when(iso: string, now: number = Date.now()): string {
  const t = iso ? Date.parse(iso) : NaN
  if (Number.isNaN(t)) return ''
  const d = new Date(t)
  return t - now < 86_400_000
    ? d.toLocaleTimeString('en-US', { hour: 'numeric', minute: '2-digit' })
    : d.toLocaleDateString('en-US', { timeZone: 'UTC', month: 'short', day: 'numeric' })
}

/** A class of model as a sentence names it. */
const CLASS: Record<string, string> = { premium: 'premium', ours: 'Hanzo' }

/** What was used, by the classes that used it, as one phrase each. */
const USED: Record<string, string> = {
  '': phrase('usage'),
  premium: phrase('premium model usage'),
  Hanzo: phrase('Hanzo model usage'),
  'premium and Hanzo': phrase('premium and Hanzo model usage'),
}

/**
 * What a reader whose plan's included usage is used is told: the plan, which
 * models, and when they come back — `Your Max 20x plan’s included premium model
 * usage is used until 5:00 PM.` In the language of `t`.
 */
export function spent(l: Label | null, classes: readonly string[], resets: string, now: number = Date.now(), t: Say = english): string {
  const names = classes.map((c) => CLASS[c]).filter(Boolean).join(' and ')
  const what = USED[names] !== undefined ? t(USED[names]) : `${names} model usage`
  const at = when(resets, now)
  if (!l) return at ? t('Your plan’s included {what} is used until {time}.', { what, time: at }) : t('Your plan’s included {what} is used.', { what })
  const plan = said({ name: t(l.name), tag: l.tag ? t(l.tag) : '' })
  return at ? t('Your {plan} plan’s included {what} is used until {time}.', { plan, what, time: at }) : t('Your {plan} plan’s included {what} is used.', { plan, what })
}

/**
 * The ways past a spent allowance, in this order and two at most: keep going
 * on credits — or, with none the server would let pay, add some — then the
 * upgrade; and a switch to another model where the server offers one. Taken
 * from what the server sent, so nothing is offered that it would refuse.
 */
export function ways<A extends { kind: string; label: string }>(actions: readonly A[], t: Say = english): A[] {
  const pick = (kind: string) => actions.find((a) => a.kind === kind)
  const topup = pick('topup')
  return [pick('credits') ?? (topup ? { ...topup, label: t('Add credits') } : undefined), pick('upgrade'), pick('switch')].filter(
    (a): a is A => a !== undefined,
  )
}
