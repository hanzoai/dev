/**
 * The plan's usage, as the gate counts it.
 *
 *   GET /v1/ai/limits   the plan, its request windows and each class it includes, as shares
 *   PUT /v1/ai/limits   {creditsAfterAllowance}: keep using a model on credits once the
 *                       plan's included usage of it is spent; answers the limits after it
 *
 * The shape is cloud's `aiLimits` (apps/ai/limits_ops.go): `plan` is the rung the
 * caller is served as, `free` with none; `session` and `day` bound every request
 * the plan covers; `classes.premium` (third-party frontier models) and
 * `classes.ours` (Hanzo's) are each the billing period's share used, with a
 * shorter `window` where the plan sets one. Never a dollar, a count or a cap.
 *
 * Only an org admin may change how a pooled org wallet pays (403 otherwise).
 */
import { share, type Share, type State } from '../plan.ts'
import { call, type Target } from './call.ts'

export type Class = 'premium' | 'ours'
export const CLASSES: readonly Class[] = ['premium', 'ours']

/** A class's billing period, who pays for its next request, and its short window. */
export interface Allowance extends Share {
  /** plan, prepaid, credits, or none. */
  paying: string
  window: Share | null
}

/** A way past a limit, in the order the server sends them. */
export interface Action {
  /** upgrade and topup are pages (`url`); credits is the choice above; switch names a model. */
  kind: 'upgrade' | 'topup' | 'credits' | 'switch'
  label: string
  url: string
  plan: string
  model: string
}

/** One model whose share of the plan is used for now, answered by its fallback in chat. */
export interface Paused {
  model: string
  fallback: string
  resets: string
}

export interface Limits {
  /** The rung served (`max-20x`); `free` where no plan counts. */
  plan: string
  /** The billing period, RFC 3339; '' with no plan. */
  start: string
  end: string
  /** The worst class's state. */
  state: State
  session: Share | null
  day: Share | null
  classes: Partial<Record<Class, Allowance>>
  /** Present in limited mode: why, and in which classes. */
  limited: { reason: string; classes: Class[]; message: string } | null
  actions: Action[]
  /** The plan that raises these limits, '' at the top. */
  upgrade: string
  /** Whether the payer keeps using a model on credits once the plan's usage of it is spent. */
  credits: boolean
  paused: Paused[]
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' && !Array.isArray(v) ? (v as Record<string, unknown>) : {})
const arr = (v: unknown): unknown[] => (Array.isArray(v) ? v : [])
const KINDS: readonly Action['kind'][] = ['upgrade', 'topup', 'credits', 'switch']
const STATES: readonly State[] = ['ok', 'near', 'limited']

/** A class off the wire: its period always carries a state, so one without is not a class. */
function allowance(v: unknown): Allowance | null {
  const w = share(v)
  if (!w) return null
  const o = obj(v)
  return { ...w, paying: str(o.paying) || 'none', window: share(o.window) }
}

/** The body of `GET /v1/ai/limits`, read field by field: what is not there is not drawn. */
export function parse(raw: unknown): Limits {
  const r = obj(raw)
  const classes: Partial<Record<Class, Allowance>> = {}
  for (const c of CLASSES) {
    const got = allowance(obj(r.classes)[c])
    if (got) classes[c] = got
  }
  const lim = r.limited ? obj(r.limited) : null
  return {
    plan: str(r.plan) || 'free',
    start: str(r.period_start),
    end: str(r.period_end),
    state: STATES.find((s) => s === r.state) ?? 'ok',
    session: share(r.session),
    day: share(r.day),
    classes,
    limited: lim
      ? {
          reason: str(lim.reason),
          classes: arr(lim.classes).filter((c): c is Class => CLASSES.includes(c as Class)),
          message: str(lim.message),
        }
      : null,
    actions: arr(r.actions)
      .map(obj)
      .flatMap((a): Action[] => {
        const kind = KINDS.find((k) => k === a.kind)
        if (!kind) return []
        const url = str(a.url)
        const model = str(a.model)
        // A page with no address, or a switch to no model, is not a way past anything.
        if ((kind === 'upgrade' || kind === 'topup') && !url) return []
        if (kind === 'switch' && !model) return []
        return [{ kind, label: str(a.label), url, plan: str(a.plan), model }]
      }),
    upgrade: str(r.upgrade),
    credits: r.credits_after_allowance === true,
    paused: arr(r.paused)
      .map(obj)
      .filter((p) => str(p.model))
      .map((p) => ({ model: str(p.model), fallback: str(p.fallback), resets: str(p.resets_at) })),
  }
}

export async function limits(t: Target): Promise<Limits> {
  return parse(await call<unknown>(t, 'GET', '/v1/ai/limits'))
}

/** Turn on or off paying from credits once the plan's included usage runs out; answers the limits after. */
export async function allow(t: Target, on: boolean): Promise<Limits> {
  return parse(await call<unknown>(t, 'PUT', '/v1/ai/limits', { creditsAfterAllowance: on }))
}
