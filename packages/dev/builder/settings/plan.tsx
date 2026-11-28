/**
 * The plan's usage, as the one view every Usage page draws: Dev's Settings,
 * the rooms' Settings and hanzo.ai's organization settings.
 *
 * On a PLAN: its name, its period, the request windows (session, day) and each
 * class of model's share of the period with its shorter window — shares only,
 * never money — and the choice of what happens when the allowance runs out:
 * `Use credits when my plan's allowance runs out` (PUT /v1/ai/limits), off
 * until the payer turns it on, and saying what it spends. Once a class is used
 * up it leads with that, its reset, and the two ways on.
 *
 * On FREE: the share of today's free allowance left and the shared pool's
 * state, and Upgrade. With CREDITS and no plan: the balance, because it is
 * what the account spends.
 *
 * It reads as `target`, so it needs no host: a host says where the plans and
 * Billing are (`onPlans`, `onBilling`).
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a plan's name, a
 * model's and the ways on the server offers are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { Button, Switch } from '@hanzo/ui'
import { useState, type ReactNode } from 'react'

import { money } from '../api/billing.ts'
import { told, type Target } from '../api/call.ts'
import { allow, CLASSES, type Action, type Class, type Limits } from '../api/limits.ts'
import { INK, Title } from '../meter.tsx'
import { spent, ways, when, type Share, type State } from '../plan.ts'
import { left, useStanding, type Standing } from '../standing.ts'
import { Card, day, Group, Note, Row, Soft } from './ui.tsx'

const NAME: Record<Class, string> = { premium: phrase('Premium models'), ours: phrase('Hanzo models') }
const FILL = { ok: '$ink', near: '$yellow10', limited: '$red10' } as const satisfies Record<State, string>

/** A thin bar of a share used. */
function Bar({ percent, state }: { percent: number; state: State }) {
  return (
    <YStack height={6} rounded={999} bg="$hover" overflow="hidden" role="progressbar" aria-valuenow={percent} aria-valuemin={0} aria-valuemax={100}>
      <YStack height={6} width={`${percent}%`} rounded={999} bg={FILL[state]} />
    </YStack>
  )
}

/** One measured row: what, how much of it is used, when it starts over, its bar, and what hangs under it. */
function Gauge({ title, w, first, says, now, children }: { title: string; w: Share; first?: boolean; says?: string; now?: number; children?: ReactNode }) {
  const { t } = useI18n()
  const at = when(w.resets, now)
  return (
    <YStack gap="$2" px="$3" py="$3" borderTopWidth={first ? 0 : 1} borderColor="$borderColor" data-slot="plan-window" data-state={w.state}>
      <XStack items="baseline" justify="space-between" flexWrap="wrap" columnGap="$3" rowGap="$1">
        <SizableText size="$2" color="$ink">
          {title}
        </SizableText>
        <SizableText size="$1" color={INK[w.state]} style={{ fontVariantNumeric: 'tabular-nums' }}>
          {[says ?? t('{n}% used', { n: w.percent }), at ? t('Resets {time}', { time: at }) : ''].filter(Boolean).join(' · ')}
        </SizableText>
      </XStack>
      <Bar percent={w.percent} state={w.state} />
      {children}
    </YStack>
  )
}

/** Opens what an action names: a page, or the host's own place for it. */
type Go = (a: Action) => void

/** The view, once read. */
export function Reading({
  s,
  target,
  onPlans,
  onBilling,
  now,
}: {
  s: Standing
  target: Target
  onPlans?: () => void
  onBilling: () => void
  now?: number
}) {
  const { t } = useI18n()
  const [after, setAfter] = useState<Limits | null>(null)
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState('')
  const l = after ?? s.limits
  const upgrade = l?.actions.find((a) => a.kind === 'upgrade')

  const choose = async (on: boolean) => {
    setBusy(true)
    setNote('')
    try {
      setAfter(await allow(target, on))
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setBusy(false)
    }
  }

  const go: Go = (a) => {
    if (a.kind === 'credits') return void choose(true)
    if (a.kind === 'upgrade' && onPlans) return onPlans()
    if (a.kind === 'topup') return onBilling()
    if (a.url) window.location.assign(a.url)
  }

  // On a plan: a class used up, or one model paused, leads with its ways on, the upgrade among them.
  const used = s.kind === 'plan' && l ? CLASSES.filter((c) => l.classes[c]?.state === 'limited') : []
  const capped = s.kind === 'plan' ? (l?.paused[0] ?? null) : null
  const resets = used.map((c) => l!.classes[c]!.resets).sort()[0] ?? ''
  const until = capped ? when(capped.resets, now) : ''
  const model = capped ? capped.model.replace(/\*$/, '') : ''
  const notice = used.length
    ? spent(s.label, used, resets, now, t)
    : capped
      ? capped.fallback
        ? until
          ? t('{model} is paused until {time}; chat continues on {fallback}.', { model, time: until, fallback: capped.fallback })
          : t('{model} is paused; chat continues on {fallback}.', { model, fallback: capped.fallback })
        : until
          ? t('{model} is paused until {time}.', { model, time: until })
          : t('{model} is paused.', { model })
      : ''
  const head = (
    <XStack items="center" gap="$3" flexWrap="wrap" data-slot="plan-head">
      <YStack flex={1} minW={200} gap="$1">
        {s.label ? <Title label={s.label} size="$5" /> : null}
        <SizableText size="$2" color="$soft">
          {s.kind === 'plan'
            ? l?.start && l.end
              ? t('Current period {start} – {end}', { start: day(l.start), end: day(l.end) })
              : t('Your plan')
            : s.kind === 'free'
              ? t('Limited usage of free models, from a pool every free account shares.')
              : t('No plan. Usage is paid from your credits.')}
        </SizableText>
      </YStack>
      {(upgrade && !notice) || (s.kind !== 'plan' && onPlans) ? (
        <Button size="sm" variant={s.kind === 'plan' ? 'outline' : 'primary'} onPress={() => (upgrade ? go(upgrade) : onPlans?.())}>
          {upgrade?.label || t('Upgrade')}
        </Button>
      ) : null}
    </XStack>
  )

  if (s.kind === 'free') {
    const share = left(s.allowance)
    const a = s.allowance
    return (
      <YStack gap="$5" data-slot="plan-usage" data-kind="free">
        {head}
        {a && share !== null ? (
          <Card>
            <Gauge
              first
              title={a.window === 'hour' ? t('This hour') : t('Today')}
              w={{ percent: 100 - share, state: share === 0 ? 'limited' : share <= 20 ? 'near' : 'ok', resets: a.resets }}
              says={t('{n}% left', { n: share })}
              now={now}
            />
            {a.pool ? (
              <Row
                title={t('Shared pool')}
                detail={
                  a.pool.state === 'exhausted'
                    ? when(a.pool.resets, now)
                      ? t('Used up until {time}', { time: when(a.pool.resets, now) })
                      : t('Used up')
                    : a.pool.state === 'busy'
                      ? t('Busy. Try again in a minute.')
                      : t('Every free request is being served.')
                }
              />
            ) : null}
          </Card>
        ) : (
          <Soft>{t('The free allowance could not be read.')}</Soft>
        )}
        <XStack items="center" gap="$3" flexWrap="wrap">
          <SizableText size="$2" color="$soft" flex={1} minW={200}>
            {t('Credits pay as you go for any model, with or without a plan.')}
          </SizableText>
          <Button size="sm" variant="outline" onPress={onBilling}>
            {t('Add credits')}
          </Button>
        </XStack>
      </YStack>
    )
  }

  if (s.kind === 'credits') {
    return (
      <YStack gap="$5" data-slot="plan-usage" data-kind="credits">
        {head}
        <Card>
          <Row
            first
            title={t('Credits')}
            detail={t('What your usage is paid from, at each model’s list price.')}
            trailing={
              <SizableText size="$2" color="$ink" data-slot="plan-balance">
                {s.balance === null ? t('Unavailable') : money(s.balance)}
              </SizableText>
            }
          />
        </Card>
        <XStack>
          <Button size="sm" variant="outline" onPress={onBilling}>
            {t('Add funds')}
          </Button>
        </XStack>
      </YStack>
    )
  }

  const on = l?.credits ?? false
  return (
    <YStack gap="$5" data-slot="plan-usage" data-kind="plan">
      {head}

      {notice ? (
        <XStack items="center" gap="$2.5" flexWrap="wrap" px="$3" py="$2.5" rounded="$3" borderWidth={1} borderColor="$borderColor" data-slot="plan-spent">
          <YStack width={8} height={8} rounded={4} bg="$red10" shrink={0} aria-hidden />
          <SizableText size="$2" color="$ink" flex={1} minW={200}>
            {notice}
          </SizableText>
          <XStack gap="$2" flexWrap="wrap">
            {ways(l?.actions ?? [], t).map((a, i) => (
              <Button key={a.kind} size="sm" variant={i === 0 ? 'primary' : 'outline'} disabled={busy} data-kind={a.kind} onPress={() => go(a)}>
                {a.label}
              </Button>
            ))}
          </XStack>
        </XStack>
      ) : null}

      {!l ? (
        <Soft>{t('The plan’s usage could not be read.')}</Soft>
      ) : !l.session && !l.day && !Object.keys(l.classes).length ? (
        <Soft>{t('This plan sets no usage limits.')}</Soft>
      ) : (
        <>
          {l.session || l.day ? (
            <Group title={t('Requests')} detail={t('Every request the plan covers counts in both.')}>
              <Card>
                {l.session ? (
                  <Gauge
                    first
                    title={t('Session')}
                    w={l.session}
                    says={l.session.resets ? undefined : [t('{n}% used', { n: l.session.percent }), t('Starts with your next request')].join(' · ')}
                    now={now}
                  />
                ) : null}
                {l.day ? <Gauge first={!l.session} title={t('Today')} w={l.day} now={now} /> : null}
              </Card>
            </Group>
          ) : null}
          {Object.keys(l.classes).length ? (
            <Group title={t('By model class')} detail={t('Each class’s share of the billing period, and of its shorter window where the plan sets one.')}>
              <Card>
                {CLASSES.filter((c) => l.classes[c]).map((c, i) => {
                  const k = l.classes[c]!
                  return (
                    <YStack key={c} data-class={c}>
                      <Gauge first={i === 0} title={t(NAME[c])} w={k} says={k.paying === 'credits' ? [t('{n}% used', { n: k.percent }), t('Paying from credits')].join(' · ') : undefined} now={now}>
                        {k.window ? (
                          <XStack gap="$2" items="center" data-slot="plan-short">
                            <SizableText size="$1" color="$soft" shrink={0}>
                              {t('Shorter window')}
                            </SizableText>
                            <YStack flex={1}>
                              <Bar percent={k.window.percent} state={k.window.state} />
                            </YStack>
                            <SizableText size="$1" color="$soft" shrink={0}>
                              {[`${k.window.percent}%`, when(k.window.resets, now) ? t('Resets {time}', { time: when(k.window.resets, now) }) : ''].filter(Boolean).join(' · ')}
                            </SizableText>
                          </XStack>
                        ) : null}
                      </Gauge>
                    </YStack>
                  )
                })}
              </Card>
            </Group>
          ) : null}
          <Group title={t('When the allowance runs out')} detail={t('Credits are separate from your plan’s usage allowance. They live in Billing.')}>
            <Card>
              <Row
                first
                title={t('Use credits when my plan’s allowance runs out')}
                detail={
                  on
                    ? t('On. A model whose included usage is used keeps working, paid from credits — prepaid first, then granted credit where the model takes it — at its list price.')
                    : t('Off. Chat continues on a free model, and other requests wait for the plan to reset.')
                }
                trailing={
                  <Switch aria-label={t('Use credits when my plan’s allowance runs out')} checked={on} disabled={busy} onCheckedChange={(next: boolean) => void choose(next)} />
                }
              />
              <Row
                title={t('Credits')}
                detail={s.balance === 0 ? t('There are no credits to spend yet.') : t('The balance, top-ups and auto-reload.')}
                trailing={
                  <Button size="sm" variant="ghost" onPress={onBilling}>
                    {s.balance === 0 ? t('Add credits') : t('Billing')}
                  </Button>
                }
              />
            </Card>
          </Group>
        </>
      )}
      <Note>{note}</Note>
    </YStack>
  )
}

/** The plan's usage, read as `target`. */
export function Plan({ target, onPlans, onBilling, now }: { target: Target; onPlans?: () => void; onBilling: () => void; now?: number }) {
  const { t } = useI18n()
  const read = useStanding(target)
  if (read.value) return <Reading s={read.value} target={target} onPlans={onPlans} onBilling={onBilling} now={now} />
  return <Soft>{read.error ? told(read.error, t) : t('Reading the plan…')}</Soft>
}
