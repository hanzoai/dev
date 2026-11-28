/**
 * A surface's summary: what it made and what went into it, as sections of
 * rows, each one press from a tab beside it. Chat draws its Outputs and Sources
 * here; Studio its artifacts. The rows are the caller's: this draws them and
 * knows nothing of a conversation.
 *
 * `Summary` is the popover off the header's outline mark: each section's
 * heading with what it adds at its end, its leading action (Chat's Create file
 * or site), its rows, and one quiet line when it has none. `useKinds` makes
 * each section a kind of tab for `Panel`, the same rows in the panel.
 */
import { useMemo, useState, type ReactNode } from 'react'
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { FilePlus2, ListTodo } from '@hanzogui/lucide-icons-2'
import { Popover, PopoverContent, PopoverTrigger } from '@hanzo/ui'
import { RING } from './pane.ts'
import type { Kind } from './panel.tsx'

/** One row: what it is called, its mark, and what pressing it opens. */
export interface Entry {
  id: string
  title: string
  icon?: ReactNode
  /** A quieter second line: what a lookup asked, what came back. */
  detail?: string
  open?: () => void
}

/** A section: its heading, its rows, and what it says with none. */
export interface Section {
  id: string
  title: string
  entries: readonly Entry[]
  /** The line drawn when it has no rows; none, and it draws only its heading. */
  empty?: string
  /** At the heading's end: what adds to it (Chat's `+` on Sources). */
  action?: ReactNode
  /** A row before the others that does something rather than opens something. */
  lead?: { title: string; run: () => void }
}

/** A row's pitch and inset, px: the kit menu's, so this panel reads as every other one. */
const ROW_H = 32
const ROW_PX = 8

/** A row of the summary: its mark and its name, one press from a tab. */
function Line({ icon, children, detail, onPress, quiet = false }: { icon?: ReactNode; children: ReactNode; detail?: string; onPress?: () => void; quiet?: boolean }) {
  return (
    <XStack
      render={onPress ? 'button' : undefined}
      role={onPress ? 'button' : undefined}
      onPress={onPress}
      items="center"
      justify="flex-start"
      gap={ROW_PX}
      px={ROW_PX}
      minH={ROW_H}
      width="100%"
      rounded={4}
      cursor={onPress ? 'pointer' : undefined}
      hoverStyle={onPress ? { bg: '$hover' } : undefined}
      focusVisibleStyle={RING}
    >
      {icon ? (
        <XStack width={16} justify="center" shrink={0} opacity={0.75}>
          {icon}
        </XStack>
      ) : null}
      <YStack flex={1} minW={0} items="flex-start" py={detail ? '$1' : 0}>
        <SizableText fontSize={14} color={quiet ? '$soft' : '$ink'} numberOfLines={1} style={{ textAlign: 'left' }}>
          {children}
        </SizableText>
        {detail ? (
          <SizableText fontSize={12} color="$soft" numberOfLines={2} style={{ textAlign: 'left' }}>
            {detail}
          </SizableText>
        ) : null}
      </YStack>
    </XStack>
  )
}

/** A section's heading, with what it adds at its end. */
const Head = ({ children, action }: { children: string; action?: ReactNode }) => (
  <XStack items="center" justify="space-between" px={ROW_PX} minH={ROW_H}>
    <SizableText fontSize={13} color="$soft">
      {children}
    </SizableText>
    {action}
  </XStack>
)

/** One section's rows; `done` runs after a row is pressed (the popover puts itself away). */
function Rows({ section, done, details = false }: { section: Section; done?: () => void; details?: boolean }) {
  const { entries, empty, lead } = section
  return (
    <>
      {lead ? (
        <Line
          icon={<FilePlus2 size={15} aria-hidden />}
          onPress={() => {
            done?.()
            lead.run()
          }}
        >
          {lead.title}
        </Line>
      ) : null}
      {entries.map((e) => (
        <Line
          key={e.id}
          icon={e.icon}
          detail={details ? e.detail : undefined}
          onPress={
            e.open
              ? () => {
                  done?.()
                  e.open?.()
                }
              : undefined
          }
        >
          {e.title}
        </Line>
      ))}
      {!entries.length && empty ? <Line quiet>{empty}</Line> : null}
    </>
  )
}

/**
 * The summary, from the outline mark in a header. `onOpen` runs after a row is
 * pressed, for the host to show the panel the row opened a tab in.
 *
 * When `docked` is true (wide viewports where space allows), the header mark
 * toggles `open` via `onOpenChange` without opening a floating popover, and a
 * second `<Summary sections={sections} onOpen={...} pane />` docks the summary
 * inline on the right edge of the inner pane, shifting the main content left so
 * it stays open while toggled.
 */
export function Summary({
  sections,
  onOpen,
  label: named,
  open: controlledOpen,
  onOpenChange,
  docked = false,
  pane = false,
}: {
  sections: readonly Section[]
  onOpen?: () => void
  label?: string
  open?: boolean
  onOpenChange?: (open: boolean) => void
  docked?: boolean
  pane?: boolean
}) {
  const { t } = useI18n()
  const label = named ?? t('Summary')
  const [uncontrolledOpen, setUncontrolledOpen] = useState(false)
  const open = controlledOpen ?? uncontrolledOpen
  const setOpen = onOpenChange ?? setUncontrolledOpen

  if (pane) {
    return (
      <YStack
        data-slot="summary"
        role="complementary"
        aria-label={label}
        width={280}
        shrink={0}
        minH={0}
        self="stretch"
        p={8}
        overflowY="auto"
        overflowX="hidden"
      >
        {sections.map((s, i) => (
          <YStack key={s.id} pt={i ? 8 : 0}>
            <Head action={s.action}>{s.title}</Head>
            <Rows section={s} done={onOpen} details />
          </YStack>
        ))}
      </YStack>
    )
  }

  const toggle = (
    <XStack
      render="button"
      data-slot="summary-toggle"
      aria-label={label}
      aria-haspopup={docked ? undefined : 'dialog'}
      aria-expanded={open}
      {...({ title: label } as object)}
      onPress={docked ? () => setOpen(!open) : undefined}
      width={32}
      height={32}
      rounded="$3"
      items="center"
      justify="center"
      cursor="pointer"
      bg={open ? '$hover' : 'transparent'}
      hoverStyle={{ bg: '$hover' }}
      focusVisibleStyle={RING}
    >
      <ListTodo size={16} aria-hidden />
    </XStack>
  )

  if (docked) return toggle

  const done = () => {
    setOpen(false)
    onOpen?.()
  }
  return (
    <Popover open={open} onOpenChange={setOpen} allowFlip>
      <PopoverTrigger>{toggle}</PopoverTrigger>
      {/* The kit's panel as the composer's popovers draw it, 8px clear of the mark and flush with its end. */}
      <PopoverContent solid align="end" sideOffset={8} p={4} width={288} maxW="calc(100vw - 24px)">
        <YStack data-slot="summary" role="dialog" aria-label={label} self="stretch" width="100%" maxH="min(70dvh, 520px)" overflowY="auto" overflowX="hidden">
          {sections.map((s, i) => (
            <YStack key={s.id} pt={i ? 8 : 0}>
              <Head action={s.action}>{s.title}</Head>
              <Rows section={s} done={done} />
            </YStack>
          ))}
        </YStack>
      </PopoverContent>
    </Popover>
  )
}

/** Each section as a kind of tab for `Panel`: the same rows, with their second lines. */
export function kinds(sections: readonly Section[], icons: Record<string, ReactNode> = {}): Kind[] {
  return sections.map((s) => ({
    id: s.id,
    label: s.title,
    icon: icons[s.id],
    render: () => (
      <YStack gap={1}>
        <Head action={s.action}>{s.title}</Head>
        <Rows section={s} details />
      </YStack>
    ),
  }))
}

/** `kinds`, kept while the sections are the same. */
export function useKinds(sections: readonly Section[], icons?: Record<string, ReactNode>): Kind[] {
  return useMemo(() => kinds(sections, icons), [sections, icons])
}
