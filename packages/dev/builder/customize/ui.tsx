/**
 * What every Customize tab is drawn from, so the four read as one page: a grid
 * of cards that stacks on a phone, the card itself with its one action, the
 * featured card, a dialog with a title and a scrolling body, a delete that asks
 * first, the empty state that names the next step, a failure with its retry,
 * and the quiet line for loading and signed-out.
 *
 * Their own words are the reader's language (@hanzo/i18n `t`); a title, a
 * line or a label a tab hands them comes already said, since it may be a
 * name or a description that is drawn as given.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Check, MoreHorizontal, Plus, X } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogTitle, Skeleton } from '@hanzo/ui'
import { useEffect, useId, useState, type ReactNode } from 'react'

import { RING } from '../pane.ts'
import { useHost } from '../host.tsx'
import { say } from './say.ts'

/** Which half of a tab: what there is to browse and add (first), or what the org already has. */
export type View = 'discover' | 'yours'

/** What the shell hands a tab: which half, the search, the Add dialog's state, and where to say how many are yours. */
export interface Pane {
  view: View
  q: string
  adding: boolean
  onAdding: (open: boolean) => void
  onView: (v: View) => void
  onCount: (n: number) => void
}

/** Tells the shell how many are yours, whenever that changes. */
export function useCount(n: number, onCount: (n: number) => void) {
  useEffect(() => onCount(n), [n, onCount])
}

/** Whether a row answers a search: every word of it somewhere in the row's text. */
export { matches } from '../merge.ts'

/** Cards in as many columns as fit, one column on a phone. */
export function Grid({ children, label }: { children: ReactNode; label: string }) {
  return (
    <YStack
      role="list"
      aria-label={label}
      display="grid"
      gridTemplateColumns="repeat(auto-fill, minmax(min(100%, 290px), 1fr))"
      gap="$3.5"
      width="100%"
    >
      {children}
    </YStack>
  )
}

/** A square with the thing's picture, or its initial. */
export function Mark({ name, logo, icon, size = 32 }: { name: string; logo?: string; icon?: ReactNode; size?: number }) {
  const [gone, setGone] = useState(false)
  return (
    <YStack width={size} height={size} rounded="$3" bg="$raised" items="center" justify="center" shrink={0} overflow="hidden" aria-hidden>
      {logo && !gone ? (
        <YStack
          render="img"
          {...({ src: logo, alt: '', onError: () => setGone(true) } as object)}
          width={size}
          height={size}
          $platform-web={{ objectFit: 'contain' }}
        />
      ) : (
        (icon ?? (
          <SizableText size="$3" color="$ink">
            {(name.trim().charAt(0) || '?').toUpperCase()}
          </SizableText>
        ))
      )}
    </YStack>
  )
}

/**
 * One card: what it is, a few lines about it, a quiet fact under them, and its
 * action at the top right. Pressing the card opens it; the action is its own
 * control beside that, never inside it.
 */
export function Tile({
  title,
  detail,
  meta,
  mark,
  action,
  onOpen,
}: {
  title: string
  detail?: string
  meta?: string
  mark?: ReactNode
  action?: ReactNode
  onOpen?: () => void
}) {
  const body = (
    <>
      {mark}
      <YStack flex={1} minW={0} gap="$1">
        <SizableText size="$3" color="$ink" numberOfLines={1}>
          {title}
        </SizableText>
        {detail ? (
          <SizableText size="$1" color="$soft" numberOfLines={3}>
            {detail}
          </SizableText>
        ) : null}
        {meta ? (
          <SizableText size="$1" color="$soft" numberOfLines={1} opacity={0.8}>
            {meta}
          </SizableText>
        ) : null}
      </YStack>
    </>
  )
  return (
    <XStack role="listitem" items="flex-start" gap="$2" p="$3" minW={0} rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel" hoverStyle={onOpen ? { borderColor: '$edge' } : undefined}>
      {onOpen ? (
        <XStack
          render="button"
          aria-label={title}
          onPress={onOpen}
          flex={1}
          minW={0}
          gap="$3"
          items="flex-start"
          cursor="pointer"
          // A button centres its words unless told: a card reads from the line's start.
          $platform-web={{ textAlign: 'start' }}
          focusVisibleStyle={RING}
        >
          {body}
        </XStack>
      ) : (
        <XStack flex={1} minW={0} gap="$3" items="flex-start">
          {body}
        </XStack>
      )}
      {action}
    </XStack>
  )
}

/** The one card the shelf puts first, drawn larger. */
export function Featured({ title, detail, meta, mark, action, onOpen }: { title: string; detail: string; meta: string; mark: ReactNode; action?: ReactNode; onOpen: () => void }) {
  const { t } = useI18n()
  return (
    <XStack items="flex-start" gap="$4" p="$4" rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel" flexWrap="wrap" rowGap="$3">
      <XStack render="button" aria-label={title} onPress={onOpen} flex={1} minW={220} gap="$4" items="flex-start" cursor="pointer" $platform-web={{ textAlign: 'start' }}>
        {mark}
        <YStack flex={1} minW={0} gap="$1.5">
          <SizableText size="$1" color="$soft" fontWeight="500">
            {t('Featured')}
          </SizableText>
          <SizableText size="$6" color="$ink" numberOfLines={1}>
            {title}
          </SizableText>
          <SizableText size="$2" color="$soft" numberOfLines={3}>
            {detail}
          </SizableText>
          <SizableText size="$1" color="$soft" numberOfLines={1}>
            {meta}
          </SizableText>
        </YStack>
      </XStack>
      {action}
    </XStack>
  )
}

/** A card's add control: Add (Use, for a template one is made from) while it is not yours, Added once it is. */
export function Add({ name, added, busy, onPress, use = false }: { name: string; added: boolean; busy?: boolean; onPress: () => void; use?: boolean }) {
  const { t } = useI18n()
  if (added) {
    return (
      <XStack aria-label={t('{name} is added', { name })} gap="$1" height={32} items="center" shrink={0}>
        <Check size={14} />
        <SizableText size="$1" color="$soft">
          {t('Added')}
        </SizableText>
      </XStack>
    )
  }
  return (
    <Button size="sm" variant="outline" aria-label={use ? t('Use {name}', { name }) : t('Add {name}', { name })} disabled={busy} onPress={onPress}>
      <Plus size={14} /> {use ? t('Use') : t('Add')}
    </Button>
  )
}

/**
 * A phone's whole screen, clear of the notch and the home bar: pinned to the
 * viewport, since the `<dialog>` around it is inset by its padding and the
 * page's scrollbar gutter, and out of @hanzo/ui's floating-card glass, which
 * it draws on `[data-slot="dialog-content"]`.
 */
const SCREEN = {
  'data-slot': 'dialog-screen',
  position: 'fixed',
  t: 0,
  l: 0,
  width: '100vw',
  maxW: '100vw',
  height: '100dvh',
  maxH: '100dvh',
  rounded: 0,
  borderWidth: 0,
  pt: 'max(16px, env(safe-area-inset-top))',
  pr: 'max(16px, env(safe-area-inset-right))',
  pb: 'max(16px, env(safe-area-inset-bottom))',
  pl: 'max(16px, env(safe-area-inset-left))',
} as const

/**
 * A dialog with a title, a close control and a body that scrolls inside the
 * window; `full`, it is the whole screen on a phone. Opening a `<dialog>`
 * focuses its first focusable element, so the dialog is that element
 * (tabIndex -1, no ring): the close control wears the focus ring only once the
 * keyboard moves to it.
 */
export function Sheet({ title, open, onOpenChange, width = 560, full = false, children }: { title: string; open: boolean; onOpenChange: (o: boolean) => void; width?: number; full?: boolean; children: ReactNode }) {
  const { t } = useI18n()
  const screen = full && !useMedia().sm
  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent maxW={width} showCloseButton={false} maxH="88vh" gap="$3" tabIndex={-1} focusVisibleStyle={{ outlineWidth: 0 }} {...(screen ? SCREEN : null)}>
        <XStack items="center" justify="space-between" gap="$2">
          <DialogTitle numberOfLines={1} flex={1}>
            {title}
          </DialogTitle>
          <XStack
            render="button"
            aria-label={t('Close')}
            onPress={() => onOpenChange(false)}
            width={32}
            height={32}
            mr={-6}
            rounded={999}
            items="center"
            justify="center"
            shrink={0}
            cursor="pointer"
            hoverStyle={{ bg: '$hover' }}
            focusVisibleStyle={RING}
            $max-sm={{ width: 44, height: 44, mr: -12 }}
          >
            <X size={16} />
          </XStack>
        </XStack>
        <YStack gap="$3" overflow="scroll" minH={0} shrink={1}>
          {children}
        </YStack>
      </DialogContent>
    </Dialog>
  )
}

/** The quiet line for loading, a search that matched nothing, and signed-out. */
export function Soft({ children, action }: { children: string; action?: ReactNode }) {
  return (
    <YStack py="$6" items="center" gap="$3">
      <SizableText size="$2" color="$soft" text="center">
        {children}
      </SizableText>
      {action}
    </YStack>
  )
}

/** A read that failed: what went wrong in plain words, and the one control that asks again. */
export function Failed({ error, onRetry }: { error: unknown; onRetry: () => void }) {
  const { t } = useI18n()
  return (
    <YStack py="$6" items="center" gap="$3" role="alert">
      <SizableText size="$2" color="$soft" text="center">
        {say(error, t)}
      </SizableText>
      <Button size="sm" variant="outline" onPress={onRetry}>
        {t('Try again')}
      </Button>
    </YStack>
  )
}

/** Nothing here yet: what this place holds, and the next step, as buttons. */
export function Empty({ title, detail, children }: { title: string; detail: string; children?: ReactNode }) {
  return (
    <YStack py="$8" px="$5" items="center" gap="$3" rounded="$4" borderWidth={1} borderColor="var(--border-strong)" bg="$panel">
      <YStack items="center" gap="$1.5" maxW={420}>
        <SizableText size="$4" fontWeight="600" color="$ink" text="center">
          {title}
        </SizableText>
        {detail ? (
          <SizableText size="$2" color="$soft" text="center">
            {detail}
          </SizableText>
        ) : null}
      </YStack>
      {children ? (
        <XStack gap="$2" flexWrap="wrap" justify="center">
          {children}
        </XStack>
      ) : null}
    </YStack>
  )
}

/** What a signed-out visitor is told, with the one control that signs them in. */
export function Visitor({ children }: { children: string }) {
  const { t } = useI18n()
  const host = useHost()
  return (
    <Soft
      action={
        <Button size="sm" onPress={() => host.signIn?.()}>
          {t('Sign in')}
        </Button>
      }
    >
      {children}
    </Soft>
  )
}

/** What the last action did, said once, where it was done. */
export function Line({ children }: { children: string }) {
  if (!children) return null
  return (
    <SizableText size="$1" color="$soft" role="status">
      {children}
    </SizableText>
  )
}

/** A small heading over a run of cards. */
export function Part({ title, detail, action, children }: { title: string; detail: string; action?: ReactNode; children?: ReactNode }) {
  return (
    <YStack gap="$2.5" render="section" aria-label={title}>
      <XStack items="center" gap="$3">
        <YStack flex={1} minW={0} gap="$0.5">
          <SizableText render="h2" size="$4" color="$ink">
            {title}
          </SizableText>
          {detail ? (
            <SizableText size="$2" color="$soft">
              {detail}
            </SizableText>
          ) : null}
        </YStack>
        {action}
      </XStack>
      {children}
    </YStack>
  )
}

/** A labelled control, with a line under it when there is more to say. */
export function Field({ label, hint, children }: { label: string; hint?: string; children: ReactNode }) {
  return (
    <YStack gap="$1.5">
      <SizableText size="$2" color="$ink">
        {label}
      </SizableText>
      {children}
      {hint ? (
        <SizableText size="$1" color="$soft">
          {hint}
        </SizableText>
      ) : null}
    </YStack>
  )
}

/** Two or three choices side by side; the chosen one is filled. A count, when one is given, follows its label. Labels come already said. */
export function Choice<T extends string>({
  value,
  options,
  onChange,
  label,
}: {
  value: T
  options: readonly { id: T; label: string; count?: number }[]
  onChange: (v: T) => void
  label: string
}) {
  const { t } = useI18n()
  const groupId = useId()
  return (
    <XStack role="group" aria-label={label} p={2} gap={2} rounded="$3" borderWidth={1} borderColor="var(--border-strong)" bg="$panel" self="flex-start" shrink={0}>
      {options.map((o, idx) => {
        const active = o.id === value
        return (
          <XStack
            key={o.id}
            id={`${groupId}-${idx}`}
            render="button"
            aria-pressed={active}
            aria-label={o.count ? t('{label}, {count}', { label: o.label, count: o.count }) : o.label}
            onPress={() => onChange(o.id)}
            onKeyDown={(e: { key?: string; preventDefault?: () => void }) => {
              const step = e.key === 'ArrowRight' ? 1 : e.key === 'ArrowLeft' ? -1 : 0
              if (!step) return
              e.preventDefault?.()
              const nextIdx = (idx + step + options.length) % options.length
              const next = options[nextIdx]
              if (!next) return
              onChange(next.id)
              queueMicrotask(() => document.getElementById(`${groupId}-${nextIdx}`)?.focus())
            }}
            items="center"
            gap="$1.5"
            px="$3"
            py="$1"
            minH={30}
            focusVisibleStyle={RING}
          >
            <SizableText size="$2" fontWeight={active ? '500' : '400'} color={active ? '$ink' : '$soft'}>
              {o.label}
            </SizableText>
            {o.count ? (
              <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
                {o.count}
              </SizableText>
            ) : null}
          </XStack>
        )
      })}
    </XStack>
  )
}

/**
 * Compact filter pills for long category lists (e.g., agent categories): shows one clean
 * primary row on desktop (`maxVisibleDesktop`, default 6) and `mobileLimit` (default 4)
 * on narrow screens, collapsing the rest behind a `+{n} more` / `Show less` toggle pill
 * while keeping the active category always visible in the primary row.
 */
export function FilterPills<T extends string>({
  value,
  options,
  onChange,
  label,
  mobileLimit = 4,
  maxVisibleDesktop = 6,
}: {
  value: T
  options: readonly { id: T; label: string }[]
  onChange: (v: T) => void
  label: string
  mobileLimit?: number
  maxVisibleDesktop?: number
}) {
  const { t } = useI18n()
  const wide = Boolean(useMedia().sm)
  const groupId = useId()
  const [expanded, setExpanded] = useState(false)
  const limit = Math.max(1, wide ? maxVisibleDesktop : mobileLimit)
  const hasOverflow = options.length > limit
  const compact = !expanded && hasOverflow
  const visible = compact
    ? (() => {
        const base = options.slice(0, limit)
        if (base.some((o) => o.id === value)) return base
        const chosen = options.find((o) => o.id === value)
        return chosen ? [...base.slice(0, Math.max(1, limit - 1)), chosen] : base
      })()
    : options
  const hiddenCount = options.length - visible.length

  const moveFocus = (nextIdx: number) => {
    const clamped = (nextIdx + visible.length) % visible.length
    queueMicrotask(() => document.getElementById(`${groupId}-pill-${clamped}`)?.focus())
  }

  return (
    <XStack
      role="group"
      aria-label={label}
      flexWrap="wrap"
      gap="$1.5"
      rowGap="$1.5"
      width="100%"
      onKeyDown={((e: { key?: string }) => {
        if (e.key === 'Escape' && expanded) setExpanded(false)
      }) as never}
    >
      {visible.map((o, idx) => {
        const active = o.id === value
        return (
          <XStack
            key={o.id}
            id={`${groupId}-pill-${idx}`}
            render="button"
            aria-pressed={active}
            aria-label={o.label}
            onPress={() => onChange(o.id)}
            onKeyDown={((e: { key?: string; preventDefault?: () => void }) => {
              if (e.key === 'ArrowRight') {
                e.preventDefault?.()
                moveFocus(idx + 1)
              } else if (e.key === 'ArrowLeft') {
                e.preventDefault?.()
                moveFocus(idx - 1)
              } else if (e.key === 'Home') {
                e.preventDefault?.()
                moveFocus(0)
              } else if (e.key === 'End') {
                e.preventDefault?.()
                moveFocus(visible.length - 1)
              }
            }) as never}
            items="center"
            px="$3"
            py="$1"
            minH={30}
            focusVisibleStyle={RING}
          >
            <SizableText size="$1" fontWeight={active ? '500' : '400'} color={active ? '$background' : '$soft'}>
              {o.label}
            </SizableText>
          </XStack>
        )
      })}
      {hasOverflow ? (
        <XStack
          render="button"
          aria-expanded={expanded}
          aria-label={expanded ? t('Show fewer categories') : t('Show {n} more categories', { n: hiddenCount })}
          onPress={() => setExpanded((e) => !e)}
          items="center"
          px="$3"
          py="$1"
          minH={30}
          focusVisibleStyle={RING}
        >
          <SizableText size="$1" fontWeight="500" color="$ink">
            {expanded ? t('Show less') : t('+{n} more', { n: hiddenCount })}
          </SizableText>
        </XStack>
      ) : null}
    </XStack>
  )
}

/** A date from Unix seconds, or ''. */
export const day = (s: number): string => (s ? new Date(s * 1000).toLocaleDateString(undefined, { year: 'numeric', month: 'short', day: 'numeric' }) : '')

export function TilesSkeleton({ label, count = 6, withHero = false }: { label: string; count?: number; withHero?: boolean }) {
  return (
    <YStack role="status" aria-busy={true} aria-label={label} gap="$3" width="100%">
      {withHero ? (
        <XStack items="flex-start" gap="$4" p="$4" rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel">
          <Skeleton layout="block" width={48} height={48} rounded="$3" shrink={0} />
          <YStack flex={1} minW={0} gap="$2">
            <Skeleton layout="block" width={64} height={10} rounded="$2" />
            <Skeleton layout="block" width="42%" height={18} rounded="$2" />
            <Skeleton layout="block" width="84%" height={12} rounded="$2" />
            <Skeleton layout="block" width="32%" height={10} rounded="$2" />
          </YStack>
        </XStack>
      ) : null}
      <Grid label={label}>
        {Array.from({ length: count }, (_, i) => (
          <XStack key={i} items="flex-start" gap="$3" p="$3" rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel">
            <Skeleton layout="block" width={32} height={32} rounded="$3" shrink={0} />
            <YStack flex={1} minW={0} gap="$1.5">
              <Skeleton layout="block" width={i % 2 === 0 ? '54%' : '44%'} height={14} rounded="$2" />
              <Skeleton layout="block" width="90%" height={11} rounded="$2" />
              <Skeleton layout="block" width={i % 2 === 0 ? '64%' : '72%'} height={11} rounded="$2" />
            </YStack>
          </XStack>
        ))}
      </Grid>
    </YStack>
  )
}

export function RowsSkeleton({ label, count = 4 }: { label: string; count?: number }) {
  return (
    <YStack role="status" aria-busy={true} aria-label={label} gap="$2" width="100%">
      {Array.from({ length: count }, (_, i) => (
        <XStack key={i} items="center" gap="$3" px="$3" py="$2.5" minH={48} rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$panel">
          <YStack flex={1} minW={0} gap="$1.5">
            <Skeleton layout="block" width={i % 2 === 0 ? '38%' : '48%'} height={13} rounded="$2" />
            <Skeleton layout="block" width={i % 2 === 0 ? '74%' : '62%'} height={10} rounded="$2" />
          </YStack>
          <Skeleton layout="block" width={28} height={14} rounded="$2" shrink={0} />
        </XStack>
      ))}
    </YStack>
  )
}

export function MenuTrigger({ label }: { label: string }) {
  return (
    <XStack
      render="button"
      aria-label={label}
      width={32}
      height={32}
      items="center"
      justify="center"
      shrink={0}
      focusVisibleStyle={RING}
    >
      <MoreHorizontal size={15} color="$soft" aria-hidden />
    </XStack>
  )
}
