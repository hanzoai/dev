/**
 * What every Settings section is drawn from, so they read as one page: a
 * heading with its action, bordered lists of rows, labelled fields, and the
 * quiet lines for empty, loading and refused.
 *
 * What a section hands them comes already said in the reader's language
 * (@hanzo/i18n `t`), since a row's title may be a name drawn as given; their
 * own few words they say themselves.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { CopyButton } from '@hanzo/ui/product'
import type { ReactNode } from 'react'

/** A section's title, what it is for, and its one action. */
export function Heading({ title, detail, action }: { title: string; detail?: string; action?: ReactNode }) {
  return (
    <XStack items="flex-start" gap="$3">
      <YStack flex={1} minW={0} gap="$1">
        <SizableText size="$6" color="$ink">
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
  )
}

/** A group inside a section: a small title over what it holds. */
export function Group({ title, detail, action, children }: { title: string; detail?: string; action?: ReactNode; children: ReactNode }) {
  return (
    <YStack gap="$2">
      <XStack items="center" gap="$2">
        <YStack flex={1} minW={0} gap="$0.5">
          <SizableText size="$3" color="$ink">
            {title}
          </SizableText>
          {detail ? (
            <SizableText size="$1" color="$soft">
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

/** A bordered list. Its rows draw their own dividers. */
export function Card({ children }: { children: ReactNode }) {
  return (
    <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" overflow="hidden">
      {children}
    </YStack>
  )
}

/** One row of a Card: what it is, a line about it, and what can be done to it. */
export function Row({
  title,
  detail,
  leading,
  trailing,
  first,
  mono,
}: {
  title: string
  detail?: string
  leading?: ReactNode
  trailing?: ReactNode
  first?: boolean
  mono?: boolean
}) {
  return (
    <XStack items="center" gap="$3" px="$3" py="$2.5" borderTopWidth={first ? 0 : 1} borderColor="$borderColor">
      {leading}
      <YStack flex={1} minW={0} gap="$0.5">
        <SizableText
          size="$2"
          color="$ink"
          numberOfLines={1}
          style={mono ? { fontFamily: 'var(--f-mono, ui-monospace, monospace)' } : undefined}
        >
          {title}
        </SizableText>
        {detail ? (
          <SizableText size="$1" color="$soft" numberOfLines={2}>
            {detail}
          </SizableText>
        ) : null}
      </YStack>
      {trailing}
    </XStack>
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

/** The quiet line for empty, loading, signed-out and refused. */
export function Soft({ children }: { children: string }) {
  return (
    <YStack py="$5" items="center">
      <SizableText size="$2" color="$soft" style={{ textAlign: 'center' }}>
        {children}
      </SizableText>
    </YStack>
  )
}

/** `Sep 27, 2026` from an RFC 3339 instant, or '' when there is none. UTC, the calendar the platform bills and resets on. */
export function day(iso: string): string {
  const d = iso ? new Date(iso) : null
  return d && !Number.isNaN(d.getTime()) ? d.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric', timeZone: 'UTC' }) : ''
}

/** A credential the platform answers once: the value to copy, what it is, and what its copy control says. */
export function Once({ value, label, copy, children }: { value: string; label: string; copy: string; children?: ReactNode }) {
  const { t } = useI18n()
  return (
    <YStack gap="$2" px="$3" py="$3" rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$raised">
      <XStack items="center" gap="$2">
        <SizableText
          flex={1}
          minW={0}
          size="$2"
          color="$ink"
          aria-label={label}
          style={{ fontFamily: 'var(--f-mono, ui-monospace, monospace)', wordBreak: 'break-all' }}
        >
          {value}
        </SizableText>
        <CopyButton value={value} label={copy} />
      </XStack>
      <SizableText size="$1" color="$soft">
        {t('Copy it now. It is not shown again.')}
      </SizableText>
      {children}
    </YStack>
  )
}

/** What the last action did, said once, where it was done. */
export function Note({ children }: { children: string }) {
  if (!children) return null
  return (
    <SizableText size="$1" color="$soft" role="status">
      {children}
    </SizableText>
  )
}
