/**
 * A run's transcript: prose turns, collapsible inline work rows with category
 * icons and active shimmer, centered hairline notices, timestamp dividers, and
 * stacked steering/queued follow-up controls.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import {
  AlertCircle,
  BookOpen,
  ChevronDown,
  ChevronRight,
  CornerDownLeft,
  FilePen,
  GitBranch,
  Globe,
  Info,
  Pencil,
  Sparkles,
  Square,
  SquareTerminal,
  Wrench,
  X,
} from '@hanzogui/lucide-icons-2'
import { Button } from '@hanzo/ui'
import type { Verdict } from '@hanzo/ui/agents'
import { Code, GAP, MEASURE, Message, Step, Thread } from '@hanzo/ui/chat'
import { useState, type ReactNode } from 'react'

import { counted, edited, explored } from './api/cli.ts'
import { needsFunds, type Card, type Ran } from './api/turn.ts'
import { useDevMode } from './dev-mode.ts'
import { RING } from './pane.ts'
import {
  cardCategory,
  categoryOf,
  doing,
  fold,
  formatDuration,
  formatTurnTimestamp,
  plainPath,
  ranOf,
  shouldShowTimestamp,
  worked,
  type Work,
  type WorkCategory,
} from './plain.ts'
import { Prose } from './prose.tsx'
import { artifacts, lead, Reply, type Artifact, type Listen } from './reply.tsx'

/** A card's output, its tail: what a command said last is why it stopped. */
const TAIL = 6000
const tail = (s: string): string => (s.length > TAIL ? `…${s.slice(-TAIL)}` : s)

const mono = { fontFamily: 'var(--f-mono, ui-monospace, monospace)' }

/** What a thought is about: its first line, without the markdown that bolds it. */
const firstLine = (text: string): string => text.trim().split('\n')[0]!.replace(/[*_#`]/g, '').slice(0, 120)

/** Monochrome shimmer, active icon pulse, and steer-stack enter motion using @hanzo/design CSS tokens. */
export const TRANSCRIPT_MOTION_CSS = `
@keyframes hanzo-work-shimmer {
  0% { background-position: 100% 0; }
  100% { background-position: -100% 0; }
}
@keyframes hanzo-work-icon-pulse {
  0%, 100% { opacity: 0.55; transform: scale(0.96); }
  50% { opacity: 1; transform: scale(1); }
}
@keyframes hanzo-steer-enter {
  from { opacity: 0; transform: translateY(4px); }
  to { opacity: 1; transform: translateY(0); }
}
.hanzo-work-shimmer {
  background-image: linear-gradient(
    90deg,
    var(--muted-foreground, var(--fg-muted, currentColor)) 0%,
    var(--muted-foreground, var(--fg-muted, currentColor)) 36%,
    var(--foreground, var(--fg, currentColor)) 50%,
    var(--muted-foreground, var(--fg-muted, currentColor)) 64%,
    var(--muted-foreground, var(--fg-muted, currentColor)) 100%
  );
  background-size: 240% 100%;
  -webkit-background-clip: text;
  background-clip: text;
  color: transparent !important;
  animation: hanzo-work-shimmer 2.1s linear infinite;
}
.hanzo-work-icon-active {
  animation: hanzo-work-icon-pulse 1.8s ease-in-out infinite;
}
.hanzo-steer-enter {
  animation: hanzo-steer-enter 160ms cubic-bezier(0.22, 1, 0.36, 1);
}
@media (prefers-reduced-motion: reduce), (forced-colors: active) {
  .hanzo-work-shimmer {
    animation: none !important;
    background-image: none !important;
    -webkit-background-clip: border-box !important;
    background-clip: border-box !important;
    color: var(--muted-foreground, var(--fg-muted, inherit)) !important;
  }
  .hanzo-work-icon-active,
  .hanzo-steer-enter {
    animation: none !important;
  }
}
`.trim()

/** Infer a category icon for a freeform live progress label (e.g. `Comparing test isolation`). */
export function inferActivityCategory(activity: string): WorkCategory {
  const s = activity.trim()
  if (/\b(read|inspect|explor|open|view|list)/i.test(s)) return 'read'
  if (/\b(edit|writ|chang|patch|creat|modif|sav)/i.test(s)) return 'edit'
  if (/\b(search|web|fetch|browse|query|lookup)/i.test(s)) return 'search'
  if (/\b(tool|mcp|skill|load)/i.test(s)) return 'tool'
  if (/\b(think|reason|plan|analyz)/i.test(s)) return 'think'
  return 'command'
}

/** Crisp category-matched monochrome Lucide icon for inline work rows and active steps. */
export function WorkIcon({ category, active = false, size = 14 }: { category: WorkCategory; active?: boolean; size?: number }) {
  const glyph = (() => {
    switch (category) {
      case 'command':
        return <SquareTerminal size={size} color="$soft" aria-hidden />
      case 'read':
        return <BookOpen size={size} color="$soft" aria-hidden />
      case 'edit':
        return <FilePen size={size} color="$soft" aria-hidden />
      case 'search':
        return <Globe size={size} color="$soft" aria-hidden />
      case 'think':
        return <Sparkles size={size} color="$soft" aria-hidden />
      case 'tool':
      case 'setup':
        return <Wrench size={size} color="$soft" aria-hidden />
    }
  })()
  return (
    <XStack
      data-slot="work-icon"
      data-category={category}
      data-active={active ? 'true' : undefined}
      items="center"
      justify="center"
      width={size + 2}
      height={size + 2}
      shrink={0}
      {...(active ? ({ className: 'hanzo-work-icon-active' } as object) : null)}
    >
      {glyph}
    </XStack>
  )
}

/** Centered subtle timestamp separator (`Today 11:09`) between separated turns. */
function TimestampDivider({ label }: { label: string }) {
  return (
    <XStack data-slot="timestamp-divider" role="separator" width="100%" justify="center" py="$2">
      <SizableText size="$1" color="$soft">
        {label}
      </SizableText>
    </XStack>
  )
}

/** Centered hairline status / usage-limit / error notice. */
function StatusNotice({ text, alert = false, action }: { text: string; alert?: boolean; action?: ReactNode }) {
  return (
    <YStack data-slot="status-notice" data-tone={alert ? 'alert' : 'info'} role="note" width="100%" items="center" gap="$2" py="$2">
      <XStack width="100%" items="center" gap="$3">
        <YStack flex={1} height={1} bg="$borderColor" opacity={0.6} aria-hidden />
        <XStack items="center" gap="$1.5" shrink={1} maxW="82%">
          {alert ? <AlertCircle size={13} color="$soft" shrink={0} aria-hidden /> : <Info size={13} color="$soft" shrink={0} aria-hidden />}
          <SizableText size="$1" color="$soft" style={{ textAlign: 'center' }}>
            {text}
          </SizableText>
        </XStack>
        <YStack flex={1} height={1} bg="$borderColor" opacity={0.6} aria-hidden />
      </XStack>
      {action}
    </YStack>
  )
}

/** Whether a note card represents a usage limit, balance exhaustion, or failure alert. */
const isAlertNote = (text: string, funds?: boolean): boolean =>
  Boolean(funds) || /limit|credit|balance|error|stopped|failed|did not answer|could not/i.test(text)

/** A user turn: a right-aligned compact pill bubble; Show more and queue controls sit cleanly below it. */
function UserSaid({ text, badge, after }: { text: string; badge?: string; after?: ReactNode }) {
  const { t } = useI18n()
  const [open, setOpen] = useState(false)
  const long = text.length > 180 || text.includes('\n')
  const pill = !long && text.trim().length <= 64
  return (
    <YStack width="100%" items="flex-end" gap="$1" data-slot="user-turn">
      <Message role="user" body={{ rounded: pill ? 999 : '$5', px: '$3.5', py: pill ? '$1.5' : '$2' }}>
        <YStack {...(long && !open ? { maxHeight: 72, overflow: 'hidden' } : null)}>
          <Prose text={text} />
        </YStack>
      </Message>
      {badge || long || after ? (
        <XStack items="center" justify="flex-end" gap="$2" flexWrap="wrap">
          {badge ? (
            <SizableText data-slot="user-badge" size="$1" color="$soft">
              {badge}
            </SizableText>
          ) : null}
          {long ? (
            <XStack render="button" aria-expanded={open} onPress={() => setOpen((v) => !v)} px="$2" py="$0.5" rounded="$2" hoverStyle={{ bg: '$hover' }}>
              <SizableText size="$1" color="$soft">
                {open ? t('Show less') : t('Show more')}
              </SizableText>
            </XStack>
          ) : null}
          {after}
        </XStack>
      ) : null}
    </YStack>
  )
}

export interface QueuedMessage {
  id: string
  text: string
  /** `queued` waits for the active turn; `steering` is dispatching right now. */
  mode?: 'queued' | 'steering'
  createdAt?: string
}

/**
 * Stacked follow-up / steering messages shown cleanly above the composer while
 * a run works, offering instant Steer, Interrupt, Edit, and Remove controls.
 */
export function SteerStack({
  items,
  onSteer,
  onInterrupt,
  onEdit,
  onRemove,
  busy = false,
}: {
  items: readonly QueuedMessage[]
  /** Send a stacked message into the active run via the steering endpoint. */
  onSteer?: (id: string) => void
  /** Interrupt the active run and redirect immediately with this message. */
  onInterrupt?: (id: string) => void
  /** Pull the queued message back into the composer draft for editing. */
  onEdit?: (id: string) => void
  /** Drop the queued message. */
  onRemove?: (id: string) => void
  busy?: boolean
}) {
  const { t } = useI18n()
  if (items.length === 0) return null
  return (
    <YStack data-slot="steer-stack" gap="$1.5" width="100%" aria-label={t('Queued messages')}>
      {items.map((item) => {
        const steering = item.mode === 'steering'
        return (
          <XStack
            key={item.id}
            data-slot="steer-item"
            data-mode={item.mode ?? 'queued'}
            items="center"
            gap="$2"
            px="$3"
            py="$1.5"
            rounded="$4"
            borderWidth={1}
            borderColor="$borderColor"
            bg="$panel"
            {...({ className: 'hanzo-steer-enter' } as object)}
          >
            <SizableText
              size="$1"
              color="$soft"
              shrink={0}
              {...(steering ? ({ className: 'hanzo-work-shimmer' } as object) : null)}
            >
              {steering ? t('Steering…') : t('Queued')}
            </SizableText>
            <SizableText flex={1} minW={0} size="$2" color="$ink" numberOfLines={1}>
              {item.text}
            </SizableText>
            {!steering ? (
              <XStack items="center" gap="$1" shrink={0}>
                {onSteer ? (
                  <XStack
                    render="button"
                    data-slot="steer-send"
                    aria-label={t('Steer now')}
                    disabled={busy}
                    onPress={() => onSteer(item.id)}
                    items="center"
                    gap="$1"
                    px="$2"
                    py="$0.5"
                    rounded={999}
                    borderWidth={1}
                    borderColor="$borderColor"
                    bg="$hover"
                    cursor="pointer"
                    hoverStyle={{ bg: '$raised' }}
                    focusVisibleStyle={RING}
                  >
                    <CornerDownLeft size={11} color="$ink" aria-hidden />
                    <SizableText size="$1" color="$ink" fontWeight="500">
                      {t('Steer')}
                    </SizableText>
                  </XStack>
                ) : null}
                {onInterrupt ? (
                  <XStack
                    render="button"
                    data-slot="steer-interrupt"
                    aria-label={t('Interrupt and send')}
                    disabled={busy}
                    onPress={() => onInterrupt(item.id)}
                    items="center"
                    gap="$1"
                    px="$2"
                    py="$0.5"
                    rounded={999}
                    borderWidth={1}
                    borderColor="$borderColor"
                    cursor="pointer"
                    hoverStyle={{ bg: '$hover' }}
                    focusVisibleStyle={RING}
                  >
                    <Square size={9} fill="currentColor" color="$soft" aria-hidden />
                    <SizableText size="$1" color="$soft">
                      {t('Interrupt')}
                    </SizableText>
                  </XStack>
                ) : null}
                {onEdit ? (
                  <XStack
                    render="button"
                    data-slot="steer-edit"
                    aria-label={t('Edit queued message')}
                    onPress={() => onEdit(item.id)}
                    width={24}
                    height={24}
                    items="center"
                    justify="center"
                    rounded={999}
                    cursor="pointer"
                    hoverStyle={{ bg: '$hover' }}
                    focusVisibleStyle={RING}
                  >
                    <Pencil size={12} color="$soft" aria-hidden />
                  </XStack>
                ) : null}
                {onRemove ? (
                  <XStack
                    render="button"
                    data-slot="steer-remove"
                    aria-label={t('Remove queued message')}
                    onPress={() => onRemove(item.id)}
                    width={24}
                    height={24}
                    items="center"
                    justify="center"
                    rounded={999}
                    cursor="pointer"
                    hoverStyle={{ bg: '$hover' }}
                    focusVisibleStyle={RING}
                  >
                    <X size={12} color="$soft" aria-hidden />
                  </XStack>
                ) : null}
              </XStack>
            ) : null}
          </XStack>
        )
      })}
    </YStack>
  )
}

/** Compact inline live activity row at the tail of a running turn. */
function LiveActivityRow({ text, category }: { text: string; category: WorkCategory }) {
  return (
    <XStack data-slot="live-activity" data-category={category} role="status" items="center" gap="$2" py="$1">
      <WorkIcon category={category} active size={14} />
      <SizableText size="$2" color="$soft" numberOfLines={1} {...({ className: 'hanzo-work-shimmer' } as object)}>
        {text}
      </SizableText>
    </XStack>
  )
}

/** Collapsible inline work group row matching the sleek borderless monochrome style. */
function WorkRow({
  work,
  name,
  line,
  category,
  ran,
  defaultOpen,
  children,
}: {
  work: Work
  name: string
  line: string
  category: WorkCategory
  ran: Ran
  defaultOpen: boolean
  children: ReactNode
}) {
  const [open, setOpen] = useState(defaultOpen)
  const active = ran === 'running'
  const label = line || name
  return (
    <YStack data-slot="work-row" data-step={name} data-category={category} data-status={ran} width="100%" gap="$1.5">
      <XStack
        render="button"
        aria-expanded={open}
        aria-label={`${name} ${line}`.trim()}
        onPress={() => setOpen((v) => !v)}
        items="center"
        gap="$2"
        py="$0.5"
        px="$1"
        mx="$-1"
        rounded="$2"
        self="flex-start"
        maxW="100%"
        cursor="pointer"
        hoverStyle={{ bg: '$hover' }}
        focusVisibleStyle={RING}
      >
        <WorkIcon category={category} active={active} size={14} />
        <SizableText
          size="$2"
          color="$soft"
          numberOfLines={1}
          shrink={1}
          {...(active ? ({ className: 'hanzo-work-shimmer' } as object) : null)}
        >
          {label}
        </SizableText>
        <XStack items="center" opacity={open ? 0.85 : 0.45} shrink={0} aria-hidden>
          {open ? <ChevronDown size={12} color="$soft" /> : <ChevronRight size={12} color="$soft" />}
        </XStack>
      </XStack>
      {open ? (
        <YStack
          data-slot="work-details"
          data-work-key={work.key}
          pl="$4"
          ml="$1.5"
          py="$1"
          gap="$2"
          borderLeftWidth={1}
          borderColor="$borderColor"
        >
          {children}
        </YStack>
      ) : null}
    </YStack>
  )
}

export type TranscriptView = 'normal' | 'thinking' | 'verbose'

export function Transcript({
  cards,
  live,
  empty,
  header,
  activity,
  onApprove,
  approving = false,
  onVerdict,
  onOpen,
  onDiff,
  listen,
  end,
  view = 'normal',
  queued = [],
  onSendQueued,
  onInterruptQueued,
  onEditQueued,
  onRemoveQueued,
  onRun,
  onAnswer,
  onAddFunds,
  devMode,
  now,
}: {
  cards: Card[]
  /** The run is still working: a card still running shows it; otherwise it was cut off. */
  live: boolean
  empty: ReactNode
  header?: ReactNode
  /** Live in-flight activity phrase (e.g. `Comparing test isolation`) drawn with shimmer at the tail. */
  activity?: string
  /** Build what a plan says. Absent, a plan has no button. */
  onApprove?: (plan: string) => void
  approving?: boolean
  /** Record a verdict on what the agent said; throws when it did not land. */
  onVerdict: (v: Verdict) => Promise<void>
  /** Open what an answer wrote in the side panel. */
  onOpen?: (a: Artifact) => void
  /** Open the run's diff in the side panel. */
  onDiff?: () => void
  /** Read an answer aloud. */
  listen?: Listen
  /** After the last card: how the run ended, when it ended on an error. */
  end?: ReactNode
  /** How much of the agent's internal reasoning and tool output is unfolded by default. */
  view?: TranscriptView
  /** Messages queued/stacked while the run works, rendered inline when passed here. */
  queued?: QueuedMessage[]
  /** Send/steer a queued message immediately. */
  onSendQueued?: (id: string) => void
  /** Interrupt the active run and redirect with a queued message. */
  onInterruptQueued?: (id: string) => void
  /** Pull a queued message back to the draft composer for editing. */
  onEditQueued?: (id: string) => void
  /** Remove a queued message without sending. */
  onRemoveQueued?: (id: string) => void
  /** Open a coding run a conversation started, on its own page. */
  onRun?: (session: string) => void
  /** Answer a question the run asked with one of its options. Absent, a question has no buttons. */
  onAnswer?: (question: string, label: string) => void
  /** Recovery action when a run stops because the account is out of credit. */
  onAddFunds?: () => void
  /**
   * Draw every command, read, edit and step as its own card. Off, each run of
   * them folds into one inline row with a category icon that opens onto them
   * (plain.ts), so the transcript reads as its result. Absent, the reader's
   * Developer mode decides.
   */
  devMode?: boolean
  /** Reference clock for timestamp dividers (defaults to `new Date()`). */
  now?: Date
}) {
  const { t } = useI18n()
  const [developer, setDeveloper] = useDevMode()
  const dev = devMode ?? developer
  // A path a person reads off Developer mode is said in words (`~` is "your home folder").
  const path = (p: string) => (dev ? p : plainPath(p, t))
  // A card the run never finished is cut off once the run has ended, not still going.
  const state = (ran: Ran): Ran => (ran === 'running' && !live ? 'cancelled' : ran)
  // The answer still arriving: the last thing the agent said while the run works.
  const last = live ? [...cards].reverse().find((c) => c.kind === 'said' && c.who === 'agent')?.key : undefined
  const expandThink = view === 'thinking' || view === 'verbose'
  const expandVerbose = view === 'verbose'

  const withDuration = (detail: string, durationMs?: number): string => {
    const dur = formatDuration(durationMs)
    if (!dur) return detail
    return detail ? `${detail} · ${dur}` : dur
  }

  const draw = (c: Card): ReactNode => {
    switch (c.kind) {
      case 'said': {
        if (c.who === 'person')
          return <UserSaid key={c.key} text={c.text} />
        const made = artifacts(c.text)
        const first = lead(made)
        return (
          <Message key={c.key} role="assistant" busy={c.key === last}>
            <YStack gap="$1">
              <Prose text={c.text} />
              {c.key === last ? null : (
                <Reply text={c.text} listen={listen} onRate={onVerdict} onOpen={first && onOpen ? () => onOpen(first) : undefined} />
              )}
            </YStack>
          </Message>
        )
      }
      case 'think':
        return (
          <Step key={`${c.key}:${view}`} name={t('Thought')} detail={firstLine(c.text)} status="done" defaultOpen={expandThink} aria-label={t('Thought')}>
            <Prose text={c.text} />
          </Step>
        )
      case 'todo':
        return (
          <YStack key={c.key} gap="$1" px="$3" py="$2" rounded="$3" borderWidth={1} borderColor="$borderColor" aria-label={t('Plan checklist')}>
            {c.items.map((x) => (
              <XStack key={`${x.done ? '1' : '0'}:${x.text}`} gap="$2" items="flex-start">
                <SizableText size="$2" color={x.done ? '$ink' : '$soft'}>
                  {x.done ? '✓' : '○'}
                </SizableText>
                <SizableText flex={1} size="$2" color={x.done ? '$soft' : '$ink'}>
                  {x.text}
                </SizableText>
              </XStack>
            ))}
          </YStack>
        )
      case 'shell':
        // The CLI's words: Ran (Running while it works), the command as the shell got it, its output under it.
        return (
          <Step
            key={`${c.key}:${view}`}
            name={state(c.ran) === 'running' ? t('Running') : t('Ran')}
            detail={withDuration(c.command, c.durationMs)}
            status={state(c.ran)}
            defaultOpen={expandVerbose}
            aria-label={t('Command {command}', { command: c.command })}
          >
            <Code language="shell" value={c.output || t('(no output)')}>
              {c.output ? tail(c.output.replace(/\n+$/, '')) : t('(no output)')}
            </Code>
          </Step>
        )
      case 'explore':
        // The CLI's Explored block: one line an action, consecutive reads on one line (cli.ts explored).
        return (
          <YStack key={c.key} gap="$1" aria-label={state(c.ran) === 'running' ? t('Exploring') : t('Explored')}>
            <XStack items="center" gap="$1.5">
              <WorkIcon category={cardCategory(c)} active={state(c.ran) === 'running'} size={13} />
              <SizableText size="$2" color="$ink" fontWeight="600">
                {state(c.ran) === 'running' ? t('Exploring') : t('Explored')}
              </SizableText>
              {formatDuration(c.durationMs) ? (
                <SizableText size="$1" color="$soft">
                  {formatDuration(c.durationMs)}
                </SizableText>
              ) : null}
            </XStack>
            {explored(c.calls).map((l) => (
              <XStack key={`${l.title}:${l.text}`} gap="$2" pl="$3">
                <SizableText size="$2" color="$soft" shrink={0}>
                  {t(l.title)}
                </SizableText>
                <SizableText size="$2" color="$ink" style={{ ...mono, overflowWrap: 'anywhere' }}>
                  {path(l.text)}
                </SizableText>
              </XStack>
            ))}
          </YStack>
        )
      case 'edit':
        // The CLI's patch header: Added / Deleted / Edited a file, or Edited N files, with the lines added and removed.
        return (
          <Step
            key={`${c.key}:${view}`}
            name={!c.files.length ? t('Edited files') : c.files.length > 1 ? `Edited ${c.files.length} files` : edited(c.files).split(' ')[0]!}
            detail={withDuration(
              !c.files.length ? '' : c.files.length > 1 ? edited(c.files).split(' ').slice(3).join(' ') : edited(c.files).split(' ').slice(1).join(' '),
              c.durationMs,
            )}
            status={state(c.ran)}
            defaultOpen={expandVerbose}
            aria-label={c.files.length ? edited(c.files) : t('Edited files')}
          >
            <YStack gap="$2" items="flex-start">
              <SizableText size="$1" color="$soft" style={{ whiteSpace: 'pre-wrap' }}>
                {c.files.length ? c.files.map((f) => `${path(f.path)} ${counted(f)}`.trimEnd()).join('\n') : t('The agent named no files.')}
              </SizableText>
              {onDiff ? (
                <Button size="sm" variant="outline" onPress={onDiff}>
                  {t('Open the diff')}
                </Button>
              ) : null}
            </YStack>
          </Step>
        )
      case 'step':
        return (
          // The run's own error opens by default: it is the reason, and the line above it only says what to do.
          // A step's name is ours (Error, Search) or a tool's; ours is said in the reader's language.
          <Step
            key={`${c.key}:${view}`}
            name={t(c.name)}
            detail={withDuration(c.detail, c.durationMs)}
            status={state(c.ran)}
            defaultOpen={c.name === 'Error' || expandVerbose}
            aria-label={`${t(c.name)} ${c.detail}`.trim()}
          >
            {c.name === 'Error' ? (
              // The reason is read whole, wrapped, never scrolled sideways.
              <YStack gap="$2" items="flex-start">
                <SizableText size="$2" color="$ink" style={{ ...mono, whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>
                  {c.output}
                </SizableText>
                {onAddFunds && needsFunds(c.output) ? (
                  <Button size="sm" variant="primary" onPress={onAddFunds}>
                    {t('Add funds')}
                  </Button>
                ) : null}
              </YStack>
            ) : c.output.trim() ? (
              <Code language="output" value={c.output}>
                {tail(c.output.replace(/\n+$/, ''))}
              </Code>
            ) : undefined}
          </Step>
        )
      case 'note': {
        const noteText = t(c.text)
        if (/^Pushed\s+/i.test(c.text) || /^Pushed\s+/i.test(noteText)) {
          return (
            <XStack key={c.key} role="note" self="flex-start">
              <XStack items="center" gap="$1.5" px="$2.5" py="$1" $platform-web={{ display: 'inline-flex' }}>
                <GitBranch size={12} color="$soft" shrink={0} aria-hidden />
                <SizableText size="$1" color="$soft">
                  {noteText}
                </SizableText>
              </XStack>
            </XStack>
          )
        }
        if (c.step) {
          return (
            <SizableText key={c.key} size="$1" color="$soft" role="note">
              {noteText}
            </SizableText>
          )
        }
        return (
          <StatusNotice
            key={c.key}
            text={noteText}
            alert={isAlertNote(c.text, c.funds)}
            action={
              c.funds && onAddFunds ? (
                <Button size="sm" variant="primary" onPress={onAddFunds}>
                  {t('Add funds')}
                </Button>
              ) : undefined
            }
          />
        )
      }
      case 'run':
        return (
          <XStack key={c.key} items="center" gap="$2" px="$3" py="$2" rounded="$3" borderWidth={1} borderColor="$borderColor" aria-label={t('Coding run')}>
            <SizableText flex={1} minW={0} size="$2" color="$ink" numberOfLines={1}>
              {t('Coding run')}
            </SizableText>
            {onRun ? (
              <Button size="sm" variant="outline" onPress={() => onRun(c.session)}>
                {t('Open the run')}
              </Button>
            ) : null}
          </XStack>
        )
      case 'ask':
        return (
          <YStack key={c.key} gap="$2" p="$3" rounded="$3" borderWidth={1} borderColor="$borderColor" aria-label={t('Question')}>
            <Prose text={c.text} />
            {c.answer ? (
              <SizableText size="$2" color="$soft">
                {`✓ ${c.answer}`}
              </SizableText>
            ) : (
              <XStack gap="$2" flexWrap="wrap">
                {c.options.map((label) => (
                  <Button key={label} size="sm" variant="outline" disabled={!onAnswer} onPress={() => onAnswer?.(c.question, label)}>
                    {label}
                  </Button>
                ))}
              </XStack>
            )}
          </YStack>
        )
      case 'plan':
        return (
          <YStack key={c.key} gap="$3" p="$4" rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel" aria-label={t('Plan')}>
            <SizableText size="$2" color="$soft">
              {t('Plan')}
            </SizableText>
            <Prose text={c.text} />
            <XStack items="center" gap="$2" flexWrap="wrap">
              <Reply text={c.text} listen={listen} onRate={onVerdict} />
              {onApprove ? (
                <Button size="sm" disabled={approving} onPress={() => onApprove(c.text)}>
                  {approving ? t('Starting the build…') : t('Approve and build')}
                </Button>
              ) : null}
            </XStack>
          </YStack>
        )
    }
  }

  // The work between two things a person reads, as one inline row with its category icon.
  const work = (w: Work, isTail: boolean): ReactNode => {
    const ran = ranOf(w.cards, state)
    const active = [...w.cards].reverse().find((c) => 'ran' in c && state(c.ran) === 'running')
    const name = ran === 'running' ? t('Working') : t('Worked')
    const liveOverride = ran === 'running' && isTail && activity?.trim() ? activity.trim() : ''
    const line = liveOverride || (ran === 'running' ? doing(active ?? w.cards[w.cards.length - 1]!, t) : worked(w.cards, t))
    const category = liveOverride ? inferActivityCategory(liveOverride) : categoryOf(w.cards, state)
    return (
      <WorkRow key={`${w.key}:${view}`} work={w} name={name} line={line} category={category} ran={ran} defaultOpen={expandVerbose}>
        {w.cards.map(draw)}
        <XStack render="button" self="flex-start" aria-label={t('Turn on Developer mode')} onPress={() => setDeveloper(true)} px="$2" py="$1" rounded="$2" hoverStyle={{ bg: '$hover' }}>
          <SizableText size="$1" color="$soft">
            {t('Always show each step: turn on Developer mode')}
          </SizableText>
        </XStack>
      </WorkRow>
    )
  }

  const items: (Card | Work)[] = dev ? cards : fold(cards)
  const tailItem = items[items.length - 1]
  const tailIsRunningWork = tailItem?.kind === 'work' && ranOf(tailItem.cards, state) === 'running'
  const liveLabel = live && !tailIsRunningWork ? activity?.trim() || '' : ''

  // Render items with clean centered timestamp dividers between separated turns.
  const rendered: ReactNode[] = []
  let prevAt: string | undefined
  let prevWasNotice = false
  items.forEach((item, idx) => {
    const itemAt = item.kind === 'work' ? item.cards[0]?.at : item.at
    const isTurnStart = item.kind === 'said' && item.who === 'person'
    if (isTurnStart && shouldShowTimestamp(prevAt, itemAt, prevWasNotice)) {
      const stamp = itemAt ? formatTurnTimestamp(itemAt, now) : ''
      if (stamp) rendered.push(<TimestampDivider key={`ts:${item.key}`} label={stamp} />)
    }
    rendered.push(item.kind === 'work' ? work(item, idx === items.length - 1) : draw(item))
    const endAt = item.kind === 'work' ? item.cards[item.cards.length - 1]?.at : item.at
    if (endAt) prevAt = endAt
    prevWasNotice = item.kind === 'note' && !item.step
  })

  return (
    <Thread gap={GAP} column={{ maxW: MEASURE }} aria-label={t('Transcript')}>
      <style data-hanzo-dev="transcript-motion">{TRANSCRIPT_MOTION_CSS}</style>
      {header}
      {cards.length === 0 && queued.length === 0 ? <YStack py="$2">{empty}</YStack> : null}
      {rendered}
      {liveLabel ? <LiveActivityRow text={liveLabel} category={inferActivityCategory(liveLabel)} /> : null}
      {queued.map((q) => {
        const steering = q.mode === 'steering'
        return (
          <UserSaid
            key={q.id}
            text={q.text}
            badge={steering ? t('Steering…') : t('Queued')}
            after={
              !steering && (onSendQueued || onInterruptQueued || onEditQueued || onRemoveQueued) ? (
                <XStack items="center" gap="$1.5">
                  {onSendQueued ? (
                    <XStack
                      render="button"
                      data-slot="queued-steer"
                      aria-label={t('Steer now')}
                      onPress={() => onSendQueued(q.id)}
                      px="$2"
                      py="$0.5"
                      rounded={999}
                      hoverStyle={{ bg: '$hover' }}
                    >
                      <SizableText size="$1" color="$ink" fontWeight="500">
                        {t('Steer')}
                      </SizableText>
                    </XStack>
                  ) : null}
                  {onInterruptQueued ? (
                    <XStack
                      render="button"
                      data-slot="queued-interrupt"
                      aria-label={t('Interrupt and send')}
                      onPress={() => onInterruptQueued(q.id)}
                      px="$2"
                      py="$0.5"
                      rounded={999}
                      hoverStyle={{ bg: '$hover' }}
                    >
                      <SizableText size="$1" color="$soft">
                        {t('Interrupt')}
                      </SizableText>
                    </XStack>
                  ) : null}
                  {onEditQueued ? (
                    <XStack
                      render="button"
                      data-slot="queued-edit"
                      aria-label={t('Edit queued message')}
                      onPress={() => onEditQueued(q.id)}
                      px="$1.5"
                      py="$0.5"
                      rounded={999}
                      hoverStyle={{ bg: '$hover' }}
                    >
                      <SizableText size="$1" color="$soft">
                        {t('Edit')}
                      </SizableText>
                    </XStack>
                  ) : null}
                  {onRemoveQueued ? (
                    <XStack
                      render="button"
                      data-slot="queued-remove"
                      aria-label={t('Remove queued message')}
                      onPress={() => onRemoveQueued(q.id)}
                      px="$1.5"
                      py="$0.5"
                      rounded={999}
                      hoverStyle={{ bg: '$hover' }}
                    >
                      <SizableText size="$1" color="$soft">
                        {t('Remove')}
                      </SizableText>
                    </XStack>
                  ) : null}
                </XStack>
              ) : undefined
            }
          />
        )
      })}
      {end}
    </Thread>
  )
}
