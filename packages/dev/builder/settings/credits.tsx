/**
 * Credits, as the one view every Billing page draws: Dev's Settings, the
 * rooms' Settings and hanzo.ai's organization settings.
 *
 * CREDITS ARE NOT THE PLAN. A plan includes usage, measured in shares on Usage;
 * credits are money — the prepaid balance bought with a card and the credit
 * Hanzo granted — and they pay for usage past the plan when the payer chose to
 * continue with credits, and for everything when there is no plan. So the
 * balance, the grants, top-ups, auto-reload, this month's spend and the monthly
 * limit on it live here, and nowhere a plan is named.
 *
 * Money moves two ways, the host's choice: `add` names a page that takes it (the
 * pay site); without one a saved card is charged here (POST /v1/billing/topup)
 * and a person with no card is sent to add one (`onCard`).
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a grant's, a limit's
 * and a kind of spend's names are the ledger's, drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { Button, Input, Switch } from '@hanzo/ui'
import { useEffect, useState } from 'react'

import {
  arm,
  balance,
  caps,
  cents,
  chosen,
  credit,
  grants,
  label,
  lift,
  limit,
  methods,
  money,
  monthly,
  recharge,
  spend,
  topup,
  type Cap,
  type Grant,
  type Method,
  type Recharge,
  type Spend,
} from '../api/billing.ts'
import { told, type Target } from '../api/call.ts'
import { useRead } from '../data.ts'
import { Card, day, Group, Note, Row, Soft } from './ui.tsx'

/** What the credits section says first, on every surface: English, marked (`phrase`), drawn through `t`. */
export const SEPARATE = phrase(
  'Credits are separate from your plan’s usage allowance. They pay for usage past it when you choose to continue with credits, and for all usage when there is no plan.',
)

/** A money figure as read: the amount, or why there is none. */
function Figure({ read }: { read: { value: number; error: Error | null; loading: boolean } }) {
  const { t } = useI18n()
  return (
    <SizableText size="$2" color={read.error ? '$soft' : '$ink'} numberOfLines={1} maxW={200}>
      {read.error ? t('Unavailable') : read.loading ? '…' : money(read.value)}
    </SizableText>
  )
}

/** A thin bar of a whole, red once a limit is used up. */
function Bar({ part, whole, limit }: { part: number; whole: number; limit: boolean }) {
  const pct = Math.min(100, Math.max(0, (part / whole) * 100))
  return (
    <YStack height={6} rounded={999} bg="$hover" overflow="hidden" role="progressbar" aria-valuenow={Math.round(pct)} aria-valuemin={0} aria-valuemax={100}>
      <YStack height={6} width={`${pct}%`} rounded={999} bg={limit && pct >= 100 ? '$red10' : '$ink'} />
    </YStack>
  )
}

function Meter({ title, used, of, says, first, limit = true }: { title: string; used: number; of: number; says: string; first?: boolean; limit?: boolean }) {
  return (
    <YStack gap="$2" px="$3" py="$3" borderTopWidth={first ? 0 : 1} borderColor="$borderColor">
      <XStack items="baseline" justify="space-between" flexWrap="wrap" columnGap="$3" rowGap="$1">
        <SizableText size="$2" color="$ink">
          {title}
        </SizableText>
        <SizableText size="$1" color="$soft">
          {says}
        </SizableText>
      </XStack>
      <Bar part={used} whole={of} limit={limit} />
    </YStack>
  )
}

/** `Resets Oct 1, 2026`, or '' for none. */
const resets = (iso: string, t: Say): string => (day(iso) ? t('Resets {date}', { date: day(iso) }) : '')

export function Credits({ target, admin, add, onCard, cardsVersion = 0 }: { target: Target; admin: boolean; add?: string; onCard?: () => void; cardsVersion?: number }) {
  const { t } = useI18n()
  const left = useRead(() => balance(target), 0, [target])
  const granted = useRead(() => credit(target), 0, [target])
  const rows = useRead(() => grants(target), [] as Grant[], [target])
  const spent = useRead(() => spend(target), null as Spend | null, [target])
  const rule = useRead(() => recharge(target), null as Recharge | null, [target])
  const ceilings = useRead(() => caps(target), [] as Cap[], [target])
  const cards = useRead(add ? null : () => methods(target), [] as Method[], [target, add, cardsVersion])
  const [buying, setBuying] = useState(false)
  const [amount, setAmount] = useState('25')
  const [ceiling, setCeiling] = useState('')
  const [below, setBelow] = useState('')
  const [reload, setReload] = useState('')
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')

  // The rule's figures are what the form starts from, once read.
  useEffect(() => {
    if (!rule.value) return
    setBelow(rule.value.threshold ? String(rule.value.threshold / 100) : '')
    setReload(rule.value.amount ? String(rule.value.amount / 100) : '')
  }, [rule.value])

  const act = async (what: () => Promise<unknown>, done: string, after: () => void) => {
    setWorking(true)
    setNote('')
    try {
      await what()
      after()
      setNote(done)
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setWorking(false)
    }
  }

  const card = chosen(cards.value)
  const adding = cents(amount)
  const cap = monthly(ceilings.value)
  const others = ceilings.value.filter((c) => c !== cap)
  const s = spent.value
  const top = Math.max(0, ...(s?.categories ?? []).map((c) => c.cents))
  const live = rows.value.filter((g) => g.active && g.remaining > 0)

  const buy = () => {
    if (!adding) return setNote(t('Enter an amount in dollars'))
    void act(() => topup(target, adding, card!.id), t('{amount} is added to the balance', { amount: money(adding) }), () => {
      setBuying(false)
      left.reload()
    })
  }

  const setLimit = () => {
    const c = cents(ceiling)
    if (!c) return setNote(t('Enter a monthly limit in dollars'))
    void act(() => limit(target, ceilings.value, c), t('Spend stops at {amount} a month', { amount: money(c) }), () => {
      setCeiling('')
      ceilings.reload()
    })
  }

  const saveRule = (enabled: boolean) => {
    const threshold = below.trim() === '' || below.trim() === '0' ? 0 : cents(below)
    const each = cents(reload)
    if (enabled && (threshold === null || !each)) return setNote(t('Enter the balance to reload below and the amount to add, in dollars'))
    void act(
      () => arm(target, { enabled, threshold: threshold ?? 0, amount: each ?? rule.value?.amount ?? 0 }),
      enabled ? t('Adds {amount} whenever the balance falls below {floor}', { amount: money(each ?? 0), floor: money(threshold ?? 0) }) : t('Auto-reload is off'),
      rule.reload,
    )
  }

  return (
    <YStack gap="$6" data-slot="credits">
      <Group
        title={t('Credits')}
        detail={t(SEPARATE)}
        action={
          add ? (
            <Button size="sm" variant="outline" onPress={() => window.open(add, '_blank', 'noopener')}>
              {t('Add funds')}
            </Button>
          ) : buying || !admin ? undefined : (
            <Button size="sm" variant="outline" onPress={() => setBuying(true)}>
              {t('Buy more')}
            </Button>
          )
        }
      >
        <Card>
          <Row first title={t('Prepaid balance')} detail={t('Available for all models and usage')} trailing={<Figure read={left} />} />
          <Row title={t('Granted credit')} detail={t('Given by Hanzo; some models take only prepaid')} trailing={<Figure read={granted} />} />
          <Row
            title={t('Spent this month')}
            detail={s && !s.known ? t('The ledger did not answer, so this is not a measurement') : undefined}
            trailing={<Figure read={{ ...spent, value: s?.total ?? 0 }} />}
          />
          {buying ? (
            <YStack gap="$2" px="$3" py="$3" borderTopWidth={1} borderColor="$borderColor">
              {card ? (
                <>
                  <XStack gap="$2" items="center" flexWrap="wrap">
                    <YStack width={140}>
                      <Input value={amount} onChangeText={setAmount} aria-label={t('Amount in dollars')} placeholder="25" inputMode="decimal" />
                    </YStack>
                    <SizableText flex={1} minW={160} size="$1" color="$soft">
                      {t('Charged to {card}', { card: label(card) })}
                    </SizableText>
                  </XStack>
                  <XStack gap="$2" justify="flex-end">
                    <Button size="sm" variant="ghost" onPress={() => setBuying(false)}>
                      {t('Cancel')}
                    </Button>
                    <Button size="sm" disabled={working} onPress={buy}>
                      {adding ? t('Add {amount}', { amount: money(adding) }) : t('Add funds')}
                    </Button>
                  </XStack>
                </>
              ) : (
                <XStack gap="$2" items="center" flexWrap="wrap">
                  <SizableText flex={1} minW={160} size="$2" color="$soft">
                    {cards.loading ? t('Reading cards…') : t('Add a card to buy more.')}
                  </SizableText>
                  {onCard ? (
                    <Button size="sm" variant="outline" onPress={onCard}>
                      {t('Add card')}
                    </Button>
                  ) : null}
                </XStack>
              )}
            </YStack>
          ) : null}
        </Card>
        {live.length ? (
          <Card>
            {live.map((g, i) => (
              <Row
                key={g.id}
                first={i === 0}
                title={g.name || t('Credit grant')}
                detail={[
                  t('{left} left of {amount}', { left: money(g.remaining, g.currency), amount: money(g.amount, g.currency) }),
                  g.expires ? t('expires {date}', { date: day(g.expires) }) : '',
                ]
                  .filter(Boolean)
                  .join(' · ')}
              />
            ))}
          </Card>
        ) : null}
      </Group>

      <Group title={t('Auto-reload')} detail={t('Tops the balance up from the card on file whenever it falls below a floor.')}>
        {rule.error ? (
          <Soft>{told(rule.error, t)}</Soft>
        ) : !rule.value ? (
          <Soft>{t('Reading auto-reload…')}</Soft>
        ) : (
          <Card>
            <Row
              first
              title={rule.value.enabled ? t('Adds {amount} below {floor}', { amount: money(rule.value.amount), floor: money(rule.value.threshold) }) : t('Off')}
              detail={rule.value.last ? t('Last reloaded {date}', { date: day(rule.value.last) }) : admin ? undefined : t('An org admin sets auto-reload.')}
              trailing={
                admin ? (
                  <Switch aria-label={t('Auto-reload')} checked={rule.value.enabled} disabled={working} onCheckedChange={(on: boolean) => saveRule(on)} />
                ) : undefined
              }
            />
            {admin ? (
              <XStack gap="$2" items="center" px="$3" py="$3" borderTopWidth={1} borderColor="$borderColor" flexWrap="wrap">
                <YStack width={150}>
                  <Input value={below} onChangeText={setBelow} aria-label={t('Reload below, in dollars')} placeholder={t('Below {amount}', { amount: '$5' })} inputMode="decimal" />
                </YStack>
                <YStack width={150}>
                  <Input value={reload} onChangeText={setReload} aria-label={t('Amount to add, in dollars')} placeholder={t('Add {amount}', { amount: '$25' })} inputMode="decimal" />
                </YStack>
                <XStack flex={1} justify="flex-end">
                  <Button size="sm" variant="outline" disabled={working} onPress={() => saveRule(true)}>
                    {rule.value.enabled ? t('Save') : t('Turn on')}
                  </Button>
                </XStack>
              </XStack>
            ) : null}
          </Card>
        )}
      </Group>

      <Group title={t('Spend this month')} detail={t('What credits paid for, by kind.')}>
        {spent.error ? (
          <Soft>{told(spent.error, t)}</Soft>
        ) : !s ? (
          <Soft>{t('Reading spend…')}</Soft>
        ) : s.categories.length === 0 ? (
          <Card>
            <Soft>{t('Nothing spent this month.')}</Soft>
          </Card>
        ) : (
          <Card>
            {s.categories.map((c, i) => (
              <Meter key={c.name} first={i === 0} title={c.name} used={c.cents} of={top} says={money(c.cents)} limit={false} />
            ))}
          </Card>
        )}
      </Group>

      <Group title={t('Monthly limit')} detail={t('Spend from credits past it is refused until the month starts over.')}>
        {ceilings.error ? (
          <Soft>{told(ceilings.error, t)}</Soft>
        ) : (
          <Card>
            {cap ? (
              <Meter
                first
                title={t('{amount} a month', { amount: money(cap.threshold) })}
                used={cap.spent ?? 0}
                of={cap.threshold}
                says={[cap.spent === null ? t('Spend unknown') : t('{amount} spent', { amount: money(cap.spent) }), resets(cap.resets, t)].filter(Boolean).join(' · ')}
              />
            ) : (
              <Row first title={t('No monthly limit')} detail={t('Spend is bounded by the balance alone.')} />
            )}
            {others.map((c) => (
              <Row
                key={c.id}
                title={c.title || t('Limit')}
                detail={[c.project && t('project {name}', { name: c.project }), c.service && t('service {name}', { name: c.service }), c.enforce ? t('refuses spend past it') : t('warns only')]
                  .filter(Boolean)
                  .join(' · ')}
                trailing={
                  <SizableText size="$2" color="$ink">
                    {c.threshold ? money(c.threshold) : '—'}
                  </SizableText>
                }
              />
            ))}
            {admin ? (
              <XStack gap="$2" items="center" px="$3" py="$3" borderTopWidth={1} borderColor="$borderColor" flexWrap="wrap">
                <YStack width={160}>
                  <Input value={ceiling} onChangeText={setCeiling} aria-label={t('Monthly limit in dollars')} placeholder={cap ? String(cap.threshold / 100) : '500'} inputMode="decimal" />
                </YStack>
                <XStack flex={1} gap="$2" justify="flex-end">
                  {cap ? (
                    <Button size="sm" variant="ghost" disabled={working} onPress={() => void act(() => lift(target, cap.id), t('The monthly limit is lifted'), ceilings.reload)}>
                      {t('Remove')}
                    </Button>
                  ) : null}
                  <Button size="sm" variant="outline" disabled={working || !ceiling.trim()} onPress={setLimit}>
                    {cap ? t('Change limit') : t('Set limit')}
                  </Button>
                </XStack>
              </XStack>
            ) : null}
          </Card>
        )}
        {!admin ? (
          <SizableText size="$1" color="$soft">
            {t('An org admin sets the monthly limit.')}
          </SizableText>
        ) : null}
      </Group>

      <Note>{note}</Note>
    </YStack>
  )
}
