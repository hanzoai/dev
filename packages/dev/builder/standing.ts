/**
 * Where an account stands: which plan it is on, which meter it reads, and the
 * reads behind it — the tier for the plan's name, the limits for its windows,
 * the free allowance, and the prepaid balance. One read for every surface that
 * shows a plan (the account menus, Usage, Billing), so they cannot disagree.
 *
 * Each read stands alone: one that fails leaves its part empty and the rest
 * drawn. Only when none answers is the whole read an error.
 */
import { phrase } from '@hanzo/i18n'
import type { Say } from '@hanzo/i18n/react'

import { allowance, balance, tier, type Allowance } from './api/billing.ts'
import type { Target } from './api/call.ts'
import { limits, type Limits } from './api/limits.ts'
import { useRead, type Read } from './data.ts'
import { kind, label, when, type Kind, type Label, type Share, type State } from './plan.ts'
import { english } from './say.ts'

export interface Standing {
  kind: Kind
  /** What the account is on, as a title: the plan's name, Free, or Pay as you go. */
  label: Label | null
  limits: Limits | null
  /** The free allowance, where the platform answered it. */
  allowance: Allowance | null
  /** The prepaid balance in cents, or null where it could not be read. */
  balance: number | null
}

const got = <T>(r: PromiseSettledResult<T>): T | null => (r.status === 'fulfilled' ? r.value : null)

/** Every read at once. Rejects only when all of them were refused, with the first refusal. */
export async function stand(t: Target): Promise<Standing> {
  const all = await Promise.allSettled([tier(t), limits(t), allowance(t), balance(t)])
  if (all.every((r) => r.status === 'rejected')) throw (all[0] as PromiseRejectedResult).reason
  const [billed, used, free, money] = [got(all[0]), got(all[1]), got(all[2]), got(all[3])]
  // Billing names what was bought; the limits name the rung the gate counts. A
  // plan either one names is the plan.
  const slug = billed?.plan || (used && used.plan !== 'free' ? used.plan : '')
  const k = kind(slug, money, billed?.tier)
  return {
    kind: k,
    label: k === 'plan' ? label(slug, billed?.tier, billed?.name) : k === 'free' ? { name: phrase('Free'), tag: '' } : { name: phrase('Pay as you go'), tag: '' },
    limits: used,
    allowance: free,
    balance: money,
  }
}

/** The standing for `t`, read when `enabled` (a menu that is open) and again when the org changes. */
export function useStanding(t: Target, enabled = true): Read<Standing | null> {
  return useRead(enabled && t.token() ? () => stand(t) : null, null as Standing | null, [t, enabled])
}

/** One window of a plan as a menu lists it. */
export interface Row {
  name: 'Session' | 'Today' | 'Month'
  percent: number
  state: State
  /** `Resets 5:00 PM`, or '' where nothing is running yet. */
  resets: string
}

const RANK: Record<State, number> = { ok: 0, near: 1, limited: 2 }

/**
 * A plan's windows, shortest first: the session, the UTC day, and the month —
 * the billing period of whichever class of model has used the most of it,
 * because that is the one that stops a request first.
 */
export function rows(l: Limits | null, now: number = Date.now(), t: Say = english): Row[] {
  if (!l) return []
  const month = Object.values(l.classes).reduce<Share | null>(
    (a, c) => (!c ? a : !a || c.percent > a.percent || (c.percent === a.percent && RANK[c.state] > RANK[a.state]) ? c : a),
    null,
  )
  const out: Row[] = []
  const add = (name: Row['name'], w: Share | null) => {
    if (!w) return
    const at = when(w.resets, now)
    out.push({ name, percent: w.percent, state: w.state, resets: at ? t('Resets {time}', { time: at }) : '' })
  }
  add('Session', l.session)
  add('Today', l.day)
  add('Month', month)
  return out
}

/** The share of the free allowance left in its window, 0 to 100, or null where none bounds the account. */
export function left(a: Allowance | null): number | null {
  if (!a || a.limit <= 0) return null
  return Math.max(0, 100 - Math.round((Math.min(a.used, a.limit) / a.limit) * 100))
}
