/**
 * A run's transcript, drawn by what each part is (turn.ts `cards`): what the
 * agent said as markdown, its reasoning folded, each command it ran as a card
 * that opens onto its output, the files it changed, its plan as a checklist, the
 * run's own steps, a plan run's plan with the button that builds it, and in a
 * conversation the coding runs its turns started, each opening on its own page.
 * What the agent said, and its plan, can be copied and judged, as a reply on
 * claude.ai can: a thumb pressed again takes the verdict back.
 *
 * It follows the run to the bottom as it streams and stops when the reader
 * scrolls up (Thread). Everything drawn is text.
 *
 * It reads as Chat reads: the same measure (`MEASURE`), the same turns and
 * prose sizes, the same row under an answer (`Reply`: copy, listen, and open
 * what it wrote in the side panel), the same caret while an answer streams and
 * the same `Failure` when a run stops on an error. What only a run has — its
 * commands, edits and steps — are cards of their own between the turns.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { GitBranch } from '@hanzogui/lucide-icons-2'
import { Button } from '@hanzo/ui'
import type { Verdict } from '@hanzo/ui/agents'
import { Code, GAP, MEASURE, Message, Step, Thread } from '@hanzo/ui/chat'
import { useState, type ReactNode } from 'react'

import { counted, edited, explored } from './api/cli.ts'
import { needsFunds, type Card, type Ran } from './api/turn.ts'
import { useDevMode } from './dev-mode.ts'
import { doing, fold, plainPath, ranOf, worked, type Work } from './plain.ts'
import { Prose } from './prose.tsx'
import { artifacts, lead, Reply, type Artifact, type Listen } from './reply.tsx'

/** A card's output, its tail: what a command said last is why it stopped. */
const TAIL = 6000
const tail = (s: string): string => (s.length > TAIL ? `…${s.slice(-TAIL)}` : s)

const mono = { fontFamily: 'var(--f-mono, ui-monospace, monospace)' }

/** What a thought is about: its first line, without the markdown that bolds it. */
const firstLine = (text: string): string => text.trim().split('\n')[0]!.replace(/[*_#`]/g, '').slice(0, 120)

/** A user turn: a tight bubble holding only the text; Show more and Send now sit outside, below it. */
function UserSaid({ text, after }: { text: string; after?: ReactNode }) {
  const { t } = useI18n()
  const [open, setOpen] = useState(false)
  const long = text.length > 180 || text.includes('\n')
  return (
    <YStack width="100%" items="flex-end" gap="$1">
      <Message role="user">
        <YStack {...(long && !open ? { maxHeight: 72, overflow: 'hidden' } : null)}>
          <Prose text={text} />
        </YStack>
      </Message>
      {long ? (
        <XStack render="button" aria-expanded={open} onPress={() => setOpen((v) => !v)} self="flex-start" px="$2" py="$1" rounded="$2" hoverStyle={{ bg: '$hover' }}>
          <SizableText size="$1" color="$soft">
            {open ? t('Show less') : t('Show more')}
          </SizableText>
        </XStack>
      ) : null}
      {after}
    </YStack>
  )
}

export interface QueuedMessage {
  id: string
  text: string
}

export type TranscriptView = 'normal' | 'thinking' | 'verbose'

export function Transcript({
  cards,
  live,
  empty,
  header,
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
  onRun,
  onAnswer,
  onAddFunds,
  devMode,
}: {
  cards: Card[]
  /** The run is still working: a card still running shows it; otherwise it was cut off. */
  live: boolean
  empty: ReactNode
  header?: ReactNode
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
  /** Messages queued with Ctrl+Enter while the run works, waiting to be sent. */
  queued?: QueuedMessage[]
  /** Send a queued message immediately. */
  onSendQueued?: (id: string) => void
  /** Open a coding run a conversation started, on its own page. */
  onRun?: (session: string) => void
  /** Answer a question the run asked with one of its options. Absent, a question has no buttons. */
  onAnswer?: (question: string, label: string) => void
  /** Recovery action when a run stops because the account is out of credit (#363/#364). */
  onAddFunds?: () => void
  /**
   * Draw every command, read, edit and step as its own card. Off, each run of
   * them folds into one plain line that opens onto them (plain.ts), so the
   * transcript reads as its result. Absent, the reader's Developer mode decides.
   */
  devMode?: boolean
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
            {c.items.map((x, i) => (
              <XStack key={i} gap="$2" items="flex-start">
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
            detail={c.command}
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
            <SizableText size="$2" color="$ink" fontWeight="600">
              {state(c.ran) === 'running' ? t('Exploring') : t('Explored')}
            </SizableText>
            {explored(c.calls).map((l, i) => (
              <XStack key={i} gap="$2" pl="$3">
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
            detail={!c.files.length ? '' : c.files.length > 1 ? edited(c.files).split(' ').slice(3).join(' ') : edited(c.files).split(' ').slice(1).join(' ')}
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
          <Step key={`${c.key}:${view}`} name={t(c.name)} detail={c.detail} status={state(c.ran)} defaultOpen={c.name === 'Error' || expandVerbose} aria-label={`${t(c.name)} ${c.detail}`.trim()}>
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
              <XStack
                items="center"
                gap="$1.5"
                px="$2.5"
                py="$1"
                $platform-web={{ display: 'inline-flex' }}
              >
                <GitBranch size={12} color="$soft" shrink={0} aria-hidden />
                <SizableText size="$1" color="$soft">
                  {noteText}
                </SizableText>
              </XStack>
            </XStack>
          )
        }
        if (c.funds && onAddFunds) {
          return (
            <YStack key={c.key} gap="$2" items="flex-start" role="note">
              <SizableText size="$1" color="$soft">
                {noteText}
              </SizableText>
              <Button size="sm" variant="primary" onPress={onAddFunds}>
                {t('Add funds')}
              </Button>
            </YStack>
          )
        }
        return (
          <SizableText key={c.key} size="$1" color="$soft" role="note">
            {noteText}
          </SizableText>
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
  // The work between two things a person reads, as one line that opens onto it.
  const work = (w: Work): ReactNode => {
    const ran = ranOf(w.cards, state)
    const now = [...w.cards].reverse().find((c) => 'ran' in c && state(c.ran) === 'running')
    const name = ran === 'running' ? t('Working') : t('Worked')
    const line = ran === 'running' ? doing(now ?? w.cards[w.cards.length - 1]!, t) : worked(w.cards, t)
    return (
      <Step key={`${w.key}:${view}`} name={name} detail={line} status={ran} defaultOpen={expandVerbose} aria-label={`${name} ${line}`.trim()}>
        <YStack gap="$2">
          {w.cards.map(draw)}
          <XStack render="button" self="flex-start" aria-label={t('Turn on Developer mode')} onPress={() => setDeveloper(true)} px="$2" py="$1" rounded="$2" hoverStyle={{ bg: '$hover' }}>
            <SizableText size="$1" color="$soft">
              {t('Always show each step: turn on Developer mode')}
            </SizableText>
          </XStack>
        </YStack>
      </Step>
    )
  }
  return (
    <Thread gap={GAP} column={{ maxW: MEASURE }} aria-label={t('Transcript')}>
      {header}
      {cards.length === 0 && queued.length === 0 ? <YStack py="$2">{empty}</YStack> : null}
      {dev ? cards.map(draw) : fold(cards).map((x) => (x.kind === 'work' ? work(x) : draw(x)))}
      {queued.map((q) => (
        <UserSaid
          key={q.id}
          text={q.text}
          after={
            onSendQueued ? (
              <XStack
                render="button"
                aria-label={t('Send now')}
                onPress={() => onSendQueued(q.id)}
                px="$2"
                py="$0.5"
              >
                <SizableText size="$1" color="$soft">
                  {t('Send now')}
                </SizableText>
              </XStack>
            ) : undefined
          }
        />
      ))}
      {end}
    </Thread>
  )
}
