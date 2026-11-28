/**
 * The rail's foot: the card that offers Hanzo in Slack, and the menu the account
 * row opens — who is signed in, the plan and its meter (`Meter`), the
 * organization they act in, Settings, Usage, Billing, the plans, help and
 * logging out.
 * Everything it opens is an address of this page, but help, which is the
 * documentation.
 */
import { useCallback, useEffect, useRef, useState } from 'react'
import { Popover, SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { CreditCard, Gauge, LifeBuoy, LogOut, Settings, Slack as Mark, Sparkles, X } from '@hanzogui/lucide-icons-2'
import { MenuLabel, MenuRow, MenuRule } from '@hanzo/ui/product'

import { useHost, useTarget } from './host.tsx'
import { Meter } from './meter.tsx'
import { useStanding } from './standing.ts'
import { path } from './route.ts'

export const DOCS = 'https://docs.hanzo.ai/docs/dev'

/** Hanzo in Slack is set up under Settings; the card goes there, and once dismissed stays gone. */
export function Slack({ onOpen, onDismiss }: { onOpen: () => void; onDismiss: () => void }) {
  const { t } = useI18n()
  return (
    // A card on the rail's card: glass, a corner a step inside the rail's.
    <XStack items="center" gap="$2.5" px="$3" py="$2.5" rounded="var(--radius-lg)" borderWidth={1} borderColor="$borderColor" bg="var(--glass)">
      <Mark size={16} />
      <YStack flex={1} minW={0} gap="$0.5">
        <SizableText size="$2" color="$ink" numberOfLines={1}>
          {t('Try Hanzo in Slack')}
        </SizableText>
        <XStack render="button" aria-label={t('Set up Hanzo in Slack')} onPress={onOpen} cursor="pointer">
          <SizableText size="$1" color="$soft" textDecorationLine="underline">
            {t('Set up')}
          </SizableText>
        </XStack>
      </YStack>
      <XStack render="button" aria-label={t('Dismiss')} onPress={onDismiss} p="$1" rounded="$2" hoverStyle={{ bg: '$hover' }}>
        <X size={14} />
      </XStack>
    </XStack>
  )
}

/**
 * The account menu's open state, for whatever row opens it.
 *
 * The row that opens the menu lies outside it, so a press on the row while the
 * menu is open reaches the popover first as a press outside, which closes it —
 * and a row that then simply opened it put it straight back, a flicker instead
 * of a close. So the open state is noted as each press BEGINS, before the popover
 * acts on it, and `toggle` (the row's press) closes a menu that was open then.
 */
export function useWho(): { open: boolean; onOpenChange: (o: boolean) => void; toggle: () => void } {
  const [open, setOpen] = useState(false)
  const shown = useRef(open)
  shown.current = open
  const began = useRef(false)
  useEffect(() => {
    const down = () => {
      began.current = shown.current
    }
    document.addEventListener('pointerdown', down, true)
    return () => document.removeEventListener('pointerdown', down, true)
  }, [])
  const toggle = useCallback(() => {
    const was = began.current || shown.current
    began.current = false
    setOpen(!was)
  }, [])
  return { open, onOpenChange: setOpen, toggle }
}

/**
 * The account menu. The rail draws the account row and says when it was pressed,
 * so the menu hangs off an anchor laid over the rail's foot: above it on the
 * open rail, beside it on the collapsed one.
 */
export function Who({ open, onOpenChange, narrow }: { open: boolean; onOpenChange: (o: boolean) => void; narrow: boolean }) {
  const { t } = useI18n()
  const host = useHost()
  const standing = useStanding(useTarget(), open)
  // The organizations this person acts in; choosing one reloads the page scoped to it.
  const orgs = host.memberships ?? (host.org ? [host.org] : [])
  const go = (p: string) => {
    onOpenChange(false)
    host.go(p)
  }
  return (
    <Popover open={open} onOpenChange={onOpenChange} placement={narrow ? 'right-end' : 'top-start'} allowFlip stayInFrame offset={6}>
      <Popover.Anchor position="absolute" l={8} b={8} width={narrow ? 48 : 256} height={36} pointerEvents="none" />
      <Popover.Content
        role="menu"
        aria-label={t('Account')}
        items="stretch"
        borderWidth={1}
        elevation="$2"
        px="$1"
        py="$1"
        width={260}
        bg="$panel"
        borderColor="$borderColor"
      >
        <YStack gap="$1">
          <SizableText size="$2" color="$soft" px="$2" py="$1.5" numberOfLines={1}>
            {host.person?.email || host.person?.name || ''}
          </SizableText>
          <MenuRule />
          <Meter
            read={standing}
            onPlans={() => go(path({ kind: 'screen', screen: 'plans' }))}
            onBilling={() => go(path({ kind: 'settings', section: 'billing' }))}
          />
          {orgs.length ? (
            <>
              <MenuRule />
              <MenuLabel>{t('Organization')}</MenuLabel>
              <YStack role="radiogroup" aria-label={t('Organizations')} gap="$1">
                {orgs.map((o) => (
                  <MenuRow
                    key={o}
                    label={o}
                    active={o === host.org}
                    onPress={() => {
                      onOpenChange(false)
                      if (o !== host.org) host.chooseOrg?.(o)
                    }}
                  />
                ))}
              </YStack>
            </>
          ) : null}
          <MenuRule />
          <MenuRow label={t('Settings')} icon={<Settings size={16} />} onPress={() => go(path({ kind: 'settings', section: 'general' }))} />
          <MenuRow label={t('Usage')} icon={<Gauge size={16} />} onPress={() => go(path({ kind: 'settings', section: 'usage' }))} />
          <MenuRow label={t('Billing')} icon={<CreditCard size={16} />} onPress={() => go(path({ kind: 'settings', section: 'billing' }))} />
          <MenuRow label={t('View all plans')} icon={<Sparkles size={16} />} onPress={() => go(path({ kind: 'screen', screen: 'plans' }))} />
          <MenuRow
            label={t('Get help')}
            icon={<LifeBuoy size={16} />}
            onPress={() => {
              onOpenChange(false)
              window.open(DOCS, '_blank', 'noopener,noreferrer')
            }}
          />
          {host.signOut ? (
            <>
              <MenuRule />
              <MenuRow
                label={t('Log out')}
                icon={<LogOut size={16} />}
                onPress={() => {
                  onOpenChange(false)
                  host.signOut?.()
                }}
              />
            </>
          ) : null}
        </YStack>
      </Popover.Content>
    </Popover>
  )
}
