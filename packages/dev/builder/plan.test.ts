import { afterEach, describe, expect, it, vi } from 'vitest'

import { parse } from './api/limits.ts'
import { kind, label, said, share, spent, ways, when } from './plan.ts'
import { left, rows, stand } from './standing.ts'

describe('label', () => {
  it('names the family and keeps the rung as a small tag', () => {
    expect(label('max-20x')).toEqual({ name: 'Max', tag: '20x' })
    expect(label('max-5x')).toEqual({ name: 'Max', tag: '5x' })
    expect(label('max')).toEqual({ name: 'Max', tag: '' })
    expect(said(label('max-20x'))).toBe('Max 20x')
  })

  it('reads every family the catalog sells', () => {
    expect(label('free')).toEqual({ name: 'Free', tag: '' })
    expect(label('go')).toEqual({ name: 'Go', tag: '' })
    expect(label('dev')).toEqual({ name: 'Pro', tag: '' })
    expect(label('pro')).toEqual({ name: 'Pro', tag: '' })
    expect(label('team')).toEqual({ name: 'Team', tag: '' })
    expect(label('team-annual')).toEqual({ name: 'Team', tag: 'Annual' })
    expect(label('advisory')).toEqual({ name: 'Agency', tag: '' })
    expect(label('dedicated')).toEqual({ name: 'Agency', tag: '' })
    expect(label('enterprise')).toEqual({ name: 'Enterprise', tag: '' })
  })

  it('reads a slug it does not know as words', () => {
    expect(label('growth-plus')).toEqual({ name: 'Growth Plus', tag: '' })
    expect(label('white-label')).toEqual({ name: 'White Label', tag: '' })
    expect(label('Max 20x')).toEqual({ name: 'Max', tag: '20x' })
  })

  it('never prints an id', () => {
    for (const id of ['3f2b8c1e-9d4a-4b7e-8c2f-1a2b3c4d5e6f', 'sub_9f8e7d6c5b4a', 'plan_abc123def', '00000000000000000000', '12345']) {
      const l = label(id)
      expect(l).toEqual({ name: 'Paid plan', tag: '' })
      expect(said(l)).not.toContain(id)
    }
    expect(label('3f2b8c1e-9d4a-4b7e-8c2f-1a2b3c4d5e6f', 'pro')).toEqual({ name: 'Pro', tag: '' })
  })

  it('names a plan by the name billing gives it, whatever the slug', () => {
    expect(label('unlimited', 'pro', 'Unlimited')).toEqual({ name: 'Unlimited', tag: '' })
    expect(label('', 'pro', 'Unlimited')).toEqual({ name: 'Unlimited', tag: '' })
    expect(label('plan_abc123def', 'pro', ' Unlimited ')).toEqual({ name: 'Unlimited', tag: '' })
    expect(label('', 'pro', '')).toEqual({ name: 'Pro', tag: '' })
    expect(label('max-20x', 'pro', null)).toEqual({ name: 'Max', tag: '20x' })
  })

  it('falls back to the tier class with no rung, and is null with no plan at all', () => {
    expect(label('', 'enterprise')).toEqual({ name: 'Enterprise', tag: '' })
    expect(label(null, 'pro')).toEqual({ name: 'Pro', tag: '' })
    expect(label('', 'free')).toBeNull()
    expect(label('', 'starter')).toBeNull()
    expect(label(undefined)).toBeNull()
    expect(said(null)).toBe('')
  })
})

describe('kind', () => {
  it('is the plan, else credits with money, else free', () => {
    expect(kind('max-20x', 4200)).toBe('plan')
    expect(kind('max-20x', 0)).toBe('plan')
    expect(kind('', 0, 'enterprise')).toBe('plan')
    expect(kind('free', 4200)).toBe('credits')
    expect(kind('', 4200)).toBe('credits')
    expect(kind('free', 0)).toBe('free')
    expect(kind('', null)).toBe('free')
  })
})

describe('share', () => {
  it('reads a bounded window and drops one the plan does not bound', () => {
    expect(share({ percent: 42.4, state: 'ok', resets_at: '2026-10-07T17:00:00Z' })).toEqual({ percent: 42, state: 'ok', resets: '2026-10-07T17:00:00Z' })
    expect(share({ percent: 0, resets_at: null })).toBeNull()
    expect(share({ percent: 140, state: 'limited', resets_at: null })).toEqual({ percent: 100, state: 'limited', resets: '' })
    expect(share(null)).toBeNull()
  })
})

const NOW = Date.parse('2026-10-07T12:00:00Z')

describe('when and spent', () => {
  it('says a time within a day and a date past it', () => {
    expect(when('2026-11-01T00:00:00Z', NOW)).toBe('Nov 1')
    expect(when('', NOW)).toBe('')
    expect(when('2026-10-07T17:00:00Z', NOW)).toMatch(/\d{1,2}:00 (AM|PM)/)
  })

  it('names the plan, the models and the reset', () => {
    expect(spent({ name: 'Max', tag: '20x' }, ['premium'], '2026-11-01T00:00:00Z', NOW)).toBe('Your Max 20x plan’s included premium model usage is used until Nov 1.')
    expect(spent({ name: 'Pro', tag: '' }, ['premium', 'ours'], '', NOW)).toBe('Your Pro plan’s included premium and Hanzo model usage is used.')
    expect(spent(null, [], '', NOW)).toBe('Your plan’s included usage is used.')
  })
})

describe('ways', () => {
  const UP = { kind: 'upgrade', label: 'Upgrade to Max 20x', url: 'https://hanzo.ai/pay/cart?plan=max-20x' }
  const CREDITS = { kind: 'credits', label: 'Continue with credits', url: '/v1/ai/limits' }
  const TOPUP = { kind: 'topup', label: 'Add prepaid credit', url: 'https://hanzo.ai/pay' }
  it('offers continuing on credits first where the org holds some, then the upgrade', () => {
    expect(ways([UP, CREDITS, TOPUP]).map((a) => a.label)).toEqual(['Continue with credits', 'Upgrade to Max 20x'])
  })
  it('offers adding credits where there are none to continue on', () => {
    expect(ways([UP, TOPUP]).map((a) => a.label)).toEqual(['Add credits', 'Upgrade to Max 20x'])
    expect(ways([TOPUP]).map((a) => a.kind)).toEqual(['topup'])
  })
  it('keeps a model switch the server offers', () => {
    expect(ways([UP, { kind: 'switch', label: 'Try Enso', model: 'enso' }, TOPUP]).map((a) => a.kind)).toEqual(['topup', 'upgrade', 'switch'])
  })
})

/** GET /v1/ai/limits for a Max 20x payer, as cloud's limits_ops.go `view` writes it. */
const MAX = {
  plan: 'max-20x',
  period_start: '2026-10-01T00:00:00Z',
  period_end: '2026-11-01T00:00:00Z',
  state: 'near',
  classes: {
    premium: { percent: 85, state: 'near', paying: 'plan', resets_at: '2026-11-01T00:00:00Z', window: { percent: 40, state: 'ok', resets_at: '2026-10-07T17:00:00Z' } },
    ours: { percent: 20, state: 'ok', paying: 'plan', resets_at: '2026-11-01T00:00:00Z' },
  },
  session: { percent: 35, state: 'ok', resets_at: '2026-10-07T15:00:00Z' },
  day: { percent: 60, state: 'ok', resets_at: '2026-10-08T00:00:00Z' },
  actions: [{ kind: 'topup', label: 'Add prepaid credit', url: 'https://hanzo.ai/pay' }],
  credits_after_allowance: false,
}

describe('limits', () => {
  it('reads the wire shape whole', () => {
    const l = parse(MAX)
    expect(l.plan).toBe('max-20x')
    expect(l.session).toEqual({ percent: 35, state: 'ok', resets: '2026-10-07T15:00:00Z' })
    expect(l.classes.premium?.window).toEqual({ percent: 40, state: 'ok', resets: '2026-10-07T17:00:00Z' })
    expect(l.classes.ours?.window).toBeNull()
    expect(l.credits).toBe(false)
    expect(l.limited).toBeNull()
    expect(l.actions).toHaveLength(1)
  })

  it('reads free as no plan, with nothing to draw', () => {
    const l = parse({ plan: 'free', state: 'ok', classes: {}, actions: [{ kind: 'upgrade', label: 'Upgrade to Pro', url: 'https://hanzo.ai/pay/cart?plan=dev', plan: 'dev' }], upgrade: 'dev', credits_after_allowance: false })
    expect(l.session).toBeNull()
    expect(l.day).toBeNull()
    expect(rows(l, NOW)).toEqual([])
  })

  it('lists a plan’s windows shortest first, the month being the class that has used the most', () => {
    expect(rows(parse(MAX), NOW).map((r) => [r.name, r.percent, r.state])).toEqual([
      ['Session', 35, 'ok'],
      ['Today', 60, 'ok'],
      ['Month', 85, 'near'],
    ])
    expect(rows(parse(MAX), NOW)[2].resets).toBe('Resets Nov 1')
  })

  it('drops an action with nowhere to go', () => {
    expect(parse({ plan: 'dev', actions: [{ kind: 'upgrade', label: 'Upgrade' }, { kind: 'switch', label: 'Try' }, { kind: 'nope' }] }).actions).toEqual([])
  })
})

describe('left', () => {
  it('is the share of the free allowance left, or null where none bounds the account', () => {
    expect(left({ plan: 'free', limit: 20, used: 5, spent: false, window: 'day', resets: '', pooled: true, pool: null })).toBe(75)
    expect(left({ plan: 'free', limit: 20, used: 25, spent: true, window: 'day', resets: '', pooled: true, pool: null })).toBe(0)
    expect(left({ plan: 'max', limit: 0, used: 0, spent: false, window: '', resets: '', pooled: false, pool: null })).toBeNull()
    expect(left(null)).toBeNull()
  })
})

describe('stand', () => {
  afterEach(() => vi.unstubAllGlobals())

  /** Answers each read by its path; an unlisted path is refused. */
  const answer = (bodies: Record<string, unknown>) =>
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string) => {
        const body = bodies[new URL(url).pathname]
        return body === undefined ? new Response(null, { status: 404 }) : new Response(JSON.stringify(body), { status: 200 })
      }),
    )
  const T = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'acme' }

  it('labels the plan by the tier answer’s planName, and by the slug without one', async () => {
    answer({ '/v1/billing/tier': { plan: 'unlimited', planName: 'Unlimited', tier: { name: 'pro' } }, '/v1/billing/balance': { available: 0 } })
    expect((await stand(T)).label).toEqual({ name: 'Unlimited', tag: '' })
    answer({ '/v1/billing/tier': { plan: 'max-20x', tier: { name: 'pro' } }, '/v1/billing/balance': { available: 0 } })
    expect((await stand(T)).label).toEqual({ name: 'Max', tag: '20x' })
  })
})
