/**
 * Usage: the plan's windows and what each class of model has used of it, as
 * shares; the free allowance on Free; the balance with no plan. What happens
 * when a plan's allowance runs out is chosen here. Credits themselves — the
 * balance, top-ups, auto-reload, spend and its limit — are Billing's.
 * Its words are the reader's language (@hanzo/i18n `t`); the org's name is its own.
 */
import { YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'

import { told } from '../api/call.ts'
import { useHost, useTarget } from '../host.tsx'
import { path } from '../route.ts'
import { useStanding } from '../standing.ts'
import { Reading } from './plan.tsx'
import { Heading, Soft } from './ui.tsx'

export function Usage() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const read = useStanding(target, signed)

  return (
    <YStack gap="$6">
      <Heading
        title={t('Usage')}
        detail={host.org ? t('What {org}’s plan includes, and how much of it is used.', { org: host.org }) : t('What this organization’s plan includes, and how much of it is used.')}
      />
      {!signed ? (
        <Soft>{t('Sign in to see this organization’s usage.')}</Soft>
      ) : read.value ? (
        <Reading
          s={read.value}
          target={target}
          onPlans={() => host.go(path({ kind: 'screen', screen: 'plans' }))}
          onBilling={() => host.go(path({ kind: 'settings', section: 'billing' }))}
        />
      ) : (
        <Soft>{read.error ? told(read.error, t) : t('Reading the plan…')}</Soft>
      )}
    </YStack>
  )
}
