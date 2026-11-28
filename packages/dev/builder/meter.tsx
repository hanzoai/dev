/**
 * The plan, at the top of an account menu: what the account is on, and its
 * meter. One block for every account menu — Dev's, the app's column, Chat's —
 * so the plan reads the same wherever the menu opens.
 *
 *   Max 20x        Session 40% · Resets 5:00 PM / Today 12% / Month 62%
 *   Free           62% of today's free usage left · Upgrade
 *   Pay as you go  $42.10 in credits · Add funds
 *
 * A plan's meter is its windows as shares and never money: credits are
 * Billing's. Only an account with no plan and money in its balance sees the
 * balance, because that balance is what it spends.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'

import { money } from './api/billing.ts'
import type { Read } from './data.ts'
import { left, rows, type Row, type Standing } from './standing.ts'
import { when, type Label, type State } from './plan.ts'

/** The ink a share is said in: quiet until it is close, and the one red when it is used. */
export const INK = { ok: '$soft', near: '$yellow10', limited: '$red10' } as const satisfies Record<State, string>

/** A window's name, in English; drawn through `t`. */
const WINDOW: Record<Row['name'], string> = { Session: phrase('Session'), Today: phrase('Today'), Month: phrase('Month') }

/** A plan's name with its rung beside it, small: `Max` `20x`. A family's name is a name; Free and the like are words. */
export function Title({ label, size = '$3' }: { label: Label; size?: '$3' | '$5' | '$6' }) {
  const { t } = useI18n()
  return (
    <XStack items="baseline" gap="$1.5" minW={0} data-slot="plan-name">
      <SizableText size={size} color="$ink" style={{ fontWeight: 600 }} numberOfLines={1}>
        {t(label.name)}
      </SizableText>
      {label.tag ? (
        <SizableText size="$1" color="$soft" px="$1.5" rounded="$2" borderWidth={1} borderColor="$borderColor" data-slot="plan-tag">
          {t(label.tag)}
        </SizableText>
      ) : null}
    </XStack>
  )
}

/** A quiet text button. */
function Act({ label, onPress }: { label: string; onPress: () => void }) {
  return (
    <XStack render="button" aria-label={label} onPress={onPress} px="$2" py="$1" rounded="$2" hoverStyle={{ bg: '$hover' }} cursor="pointer" shrink={0}>
      <SizableText size="$1" color="$ink" textDecorationLine="underline">
        {label}
      </SizableText>
    </XStack>
  )
}

/** The standing, drawn: what `Meter` shows once it has read. */
export function Reading({ s, onPlans, onBilling, now }: { s: Standing; onPlans: () => void; onBilling: () => void; now?: number }) {
  const { t } = useI18n()
  const windows = s.kind === 'plan' ? rows(s.limits, now, t) : []
  const share = s.kind === 'free' ? left(s.allowance) : null
  const resets = s.allowance ? when(s.allowance.resets, now) : ''
  return (
    <YStack px="$2" py="$1.5" gap="$1" data-slot="meter" data-kind={s.kind}>
      <XStack items="center" gap="$2" minW={0}>
        <XStack flex={1} minW={0}>{s.label ? <Title label={s.label} /> : null}</XStack>
        {s.kind === 'free' ? <Act label={t('Upgrade')} onPress={onPlans} /> : s.kind === 'credits' ? <Act label={t('Add funds')} onPress={onBilling} /> : null}
      </XStack>
      {windows.map((w) => (
        <XStack key={w.name} items="baseline" gap="$2" data-slot="meter-window" data-state={w.state}>
          <SizableText size="$1" color="$soft" width={56} shrink={0}>
            {t(WINDOW[w.name])}
          </SizableText>
          <SizableText size="$1" color={INK[w.state]} style={{ fontVariantNumeric: 'tabular-nums' }} shrink={0}>
            {w.percent}%
          </SizableText>
          <SizableText size="$1" color="$soft" flex={1} minW={0} numberOfLines={1} text="right">
            {w.resets}
          </SizableText>
        </XStack>
      ))}
      {s.kind === 'free' && share !== null ? (
        <SizableText size="$1" color={share === 0 ? '$red10' : '$soft'} data-slot="meter-free">
          {resets
            ? `${t('{n}% of today’s free usage left', { n: share })} · ${t('Resets {time}', { time: resets })}`
            : t('{n}% of today’s free usage left', { n: share })}
        </SizableText>
      ) : null}
      {s.kind === 'credits' && s.balance !== null ? (
        <SizableText size="$1" color="$soft" data-slot="meter-balance">
          {t('{amount} in credits', { amount: money(s.balance) })}
        </SizableText>
      ) : null}
    </YStack>
  )
}

/**
 * The block an account menu leads with: `read` is `useStanding`, asked while
 * the menu is open. `onPlans` opens the plans and `onBilling` the page credits
 * live on; the host says where each is.
 */
export function Meter({ read, onPlans, onBilling }: { read: Read<Standing | null>; onPlans: () => void; onBilling: () => void }) {
  const { t } = useI18n()
  if (read.value) return <Reading s={read.value} onPlans={onPlans} onBilling={onBilling} />
  return (
    <SizableText size="$1" color="$soft" px="$2" py="$1.5" data-slot="meter">
      {read.error ? t('Your plan could not be read.') : t('Reading your plan…')}
    </SizableText>
  )
}
