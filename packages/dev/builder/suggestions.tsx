/**
 * The suggestions under the composer (above it on a phone): up to three rows,
 * each the source's mark and one short action, its summary on hover. Pressing
 * a row DOES it (suggest.ts `Act`): it opens what it names, sends its prompt as
 * a new chat, starts its run, or makes its scheduled task. Nothing only fills
 * the composer. Its × dismisses it, and the account remembers.
 *
 * A row's words are the person's own, made from their activity, and drawn as
 * given; a starter's are authored (suggest.ts `STARTERS`), so drawn and sent in
 * the reader's language (@hanzo/i18n `t`).
 *
 * `useSuggestions` reads the kept rows from the account (`suggest` in /v1/pref)
 * and, when they are due (suggest.ts `due`), reads the person's activity and
 * asks Hanzo's model for new ones, only for a person who turned them on.
 * Whether they are on is the account's (`suggestions` in /v1/pref, set only by
 * `setSuggestions`; unset is off), read here, so every composer that draws them
 * obeys it: off, or signed out, nothing of the person's is read but that switch
 * and nothing is sent, and the one line drawn offers to turn them on. On, the
 * rows say a model made them and offer to turn them off. One document per page, read once per person and org,
 * shared by every composer on it.
 *
 *   GET  /v1/pref                         the kept rows, and callName, work, instructions
 *   GET  /v1/agents/chat/conversations    their conversations
 *   GET  /v1/task/board                   the org's issues and pull requests
 *   GET  /v1/agent/sessions?limit=20      Dev runs; their own are read
 *   GET  /v1/auto/automations             automations
 *   GET  /v1/library?scope=mine           their files
 *   POST /v1/iam/preferences {}           what signup kept (`onboarding`)
 *   POST /v1/chat/completions             the rows, as `MODEL` writes them
 *   PATCH /v1/pref {suggest}              the rows kept, and each dismissal
 *   POST /v1/auto/automations             a `schedule` row's task
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { Bot, CalendarClock, FileText, Github, MessageSquare, X } from '@hanzogui/lucide-icons-2'
import { useI18n } from '@hanzo/i18n/react'
import { Button, HoverCard, HoverCardContent, HoverCardTrigger } from '@hanzo/ui'
import { useCallback, useEffect, useMemo, useState, useSyncExternalStore } from 'react'

import { RING } from './pane.ts'
import { automations } from './api/auto.ts'
import { call, Refusal, unwrap, type Target } from './api/call.ts'
import { conversations } from './api/conversations.ts'
import { items } from './api/library.ts'
import { read, save, type Prefs } from './api/pref.ts'
import { list } from './api/sessions.ts'
import { everything } from './api/work.ts'
import { claims } from './claims.ts'
import { answer, dismissed, due, fill, inputs, request, seed, sig, skip, stale, starter, TAKE, type Act, type Icon, type Input, type Kept, type Seed, type Suggestion } from './suggest.ts'

/** A source that refuses this person (a 4xx other than an expired sign-in or a rate) has nothing for them; any other failure leaves it unknown. */
function soft<T>(p: Promise<T>, none: T): Promise<T> {
  return p.catch((e: unknown) => {
    if (e instanceof Refusal && e.status >= 400 && e.status < 500 && e.status !== 401 && e.status !== 429) return none
    throw e
  })
}

/** The person's activity and seed, or null when a source could not be read: rows are never made from half of it. */
async function gather(t: Target, p: Prefs): Promise<{ inputs: Input[]; seed: Seed } | null> {
  const c = claims(t.token())
  const me = typeof c?.owner === 'string' && typeof c?.sub === 'string' ? `${c.owner}/${c.sub}` : ''
  try {
    const [cs, is, rs, as, fs, iam] = await Promise.all([
      soft(conversations(t), []),
      soft(everything(t), []),
      soft(list(t, { limit: 20 }), []),
      soft(automations(t), []),
      soft(
        items(t, { scope: 'mine', limit: TAKE.file }).then((page) => page.data),
        [],
      ),
      soft(
        call<unknown>(t, 'POST', '/v1/iam/preferences', {}).then(unwrap),
        {} as unknown,
      ),
    ])
    return { inputs: inputs({ conversations: cs, issues: is, runs: rs, automations: as, files: fs, me }), seed: seed(p, iam) }
  } catch {
    return null
  }
}

// One document per page: the kept rows of whoever `loaded` names.
let doc: Kept | null = null
let loaded: string | null = null
/**
 * The account's switch as last read: `none` until it is (and for a guest), `on` only when the person turned them on.
 * Unset is off.
 */
export type Switch = 'none' | 'on' | 'off'
let state: Switch = 'none'
/** The run making rows now, and the save of what it made: the switch calls off the one and waits out the other. */
let run: AbortController | null = null
let saving: Promise<unknown> | null = null
const listeners = new Set<() => void>()
const emit = () => listeners.forEach((f) => f())
const subscribe = (f: () => void) => {
  listeners.add(f)
  return () => listeners.delete(f)
}

/** Read the kept rows, and make new ones when they are due. Each await checks it still speaks for `id`. */
async function refresh(t: Target, id: string): Promise<void> {
  // One run at a time; a newer one, or the switch, calls this one off.
  run?.abort()
  const ctl = new AbortController()
  run = ctl
  // Still wanted: not called off, still this person, and still switched on. Asked after every wait.
  const live = () => !ctl.signal.aborted && loaded === id && state === 'on'
  try {
    const p = await read(t)
    if (ctl.signal.aborted || loaded !== id) return
    // Off unless the person turned them on: none of their activity is read and nothing is sent.
    state = p.suggestions === 'on' ? 'on' : 'off'
    doc = state === 'on' ? (p.suggest ?? null) : null
    emit()
    if (!live() || !stale(doc, Date.now())) return
    const got = await gather(t, p)
    if (!live() || !got) return
    const fingerprint = sig(got.inputs, got.seed)
    if (!due(doc, fingerprint, Date.now())) return
    const ask = request(got.inputs, got.seed, skip(doc, got.inputs), new Date())
    const rows = ask ? answer(await call<unknown>(t, 'POST', '/v1/chat/completions', ask, { signal: ctl.signal }), got.inputs) : []
    if (!live()) return
    // Dismissals made while the model wrote are the latest document's.
    const gone = doc?.dismissed ?? []
    doc = { at: new Date().toISOString(), sig: fingerprint, rows: rows.filter((r) => !gone.includes(r.id)), dismissed: gone }
    emit()
    if (!live()) return
    saving = save(t, { suggest: doc })
    await saving
  } catch (e) {
    // Called off is not a failure.
    if (!ctl.signal.aborted) throw e
  } finally {
    if (run === ctl) run = null
  }
}

/** Whose rows: the platform, the org and the person. Signed out is nobody, and has none. */
const whose = (t: Target): string => {
  const token = t.token()
  if (!token) return ''
  const sub = claims(token)?.sub
  return `${t.api}|${t.org ?? ''}|${typeof sub === 'string' ? sub : ''}`
}

function load(t: Target): Promise<void> {
  const id = whose(t)
  if (loaded === id) return Promise.resolve()
  loaded = id
  doc = null
  state = 'none'
  emit()
  return id ? refresh(t, id) : Promise.resolve()
}

export interface Suggested {
  /** The rows to draw: three when any are, none dismissed. */
  rows: Suggestion[]
  /** Dismiss one, remembered by the account. */
  dismiss: (id: string) => void
  /** The account's switch: `none` for a guest or until it is read; off unless the person turned them on. */
  state: Switch
  /** Turn suggestions on or off for the account (`setSuggestions`). */
  turn: (on: boolean) => void
  /** Why the rows could not be read, made or kept, in the platform's words. */
  error: string | null
}

const said = (e: unknown): string => (e instanceof Error && e.message ? e.message : 'Suggestions could not be read.')

/** The person's suggestions. Signed out, or with nothing worth saying, there are none. */
export function useSuggestions(t: Target): Suggested {
  const [error, setError] = useState<string | null>(null)
  useEffect(() => {
    load(t).then(
      () => setError(null),
      (e: unknown) => setError(said(e)),
    )
  }, [t])
  const now = useSyncExternalStore(subscribe, () => doc, () => doc)
  const known = useSyncExternalStore(subscribe, () => state, () => state)
  const shown: Switch = whose(t) ? known : 'none'
  // Filled to three with what Hanzo does, so a new account and a quiet one never see one lonely row.
  const rows = useMemo(() => (shown === 'on' ? fill(now) : []), [shown, now])
  const dismiss = useCallback(
    (id: string) => {
      if (!doc || state !== 'on') return
      doc = dismissed(doc, id)
      emit()
      if (t.token()) save(t, { suggest: doc }).then(() => setError(null), (e: unknown) => setError(said(e)))
    },
    [t],
  )
  const turn = useCallback(
    (on: boolean) => {
      setSuggestions(t, on).then(() => setError(null), (e: unknown) => setError(said(e)))
    },
    [t],
  )
  return useMemo(() => ({ rows, dismiss, state: shown, turn, error }), [rows, dismiss, shown, turn, error])
}

/**
 * THE one way to turn suggestions on or off: the account's `suggestions` preference, so the switch follows the
 * person across devices and every `useSuggestions` on the page obeys at once. Off calls off a run in flight before
 * it sends anything more, waits out a save it already sent so nothing is saved back after, and forgets the rows
 * kept from their activity. On, they are read and made.
 */
export async function setSuggestions(t: Target, on: boolean): Promise<void> {
  if (!t.token()) return
  run?.abort()
  run = null
  state = on ? 'on' : 'off'
  doc = null
  emit()
  await saving?.catch(() => undefined)
  await save(t, on ? { suggestions: 'on' } : { suggestions: 'off', suggest: null })
  if (!on) return
  loaded = null
  await load(t)
}

/** Where a row leaves its act for the product that does it. */
const HAND = 'hanzo.suggest.hand'

/**
 * Leave `prompt` for the product `kind` names, which does it on arrival, once:
 * Chat sends a `send` as a new chat, Dev starts a `task` as a run. Same-tab
 * session storage, written by the person's own press, so an address can never
 * carry one (a link is not consent to spend).
 */
export function hand(kind: 'send' | 'task', prompt: string): void {
  try {
    window.sessionStorage.setItem(HAND, JSON.stringify({ kind, prompt }))
  } catch {
    // Without storage the product opens without it.
  }
}

/** The prompt left for `kind`, taken so it is done once, or ''. */
export function taken(kind: 'send' | 'task'): string {
  try {
    const v = JSON.parse(window.sessionStorage.getItem(HAND) ?? 'null') as { kind?: unknown; prompt?: unknown } | null
    if (v?.kind !== kind || typeof v.prompt !== 'string') return ''
    window.sessionStorage.removeItem(HAND)
    return v.prompt.trim()
  } catch {
    return ''
  }
}

const MARKS: Record<Icon, typeof Github> = {
  github: Github,
  agent: Bot,
  chat: MessageSquare,
  file: FileText,
  task: CalendarClock,
}

/** The row's pitch, px: a finger's on a phone, and the same at a desk. */
const PITCH = 44

export interface SuggestionsProps {
  rows: readonly Suggestion[]
  /** A row was pressed: do what it says. */
  onAct: (act: Act, row: Suggestion) => void
  onDismiss: (id: string) => void
  /** The account's switch (`Suggested.state`). Required: off draws the one line that offers to turn them on. */
  state: Switch
  /** Turn them on or off (`Suggested.turn`). Required: rows a model wrote always say so and can be turned off. */
  onTurn: (on: boolean) => void
}

/** The rows, or nothing when there are none. */
export function Suggestions({ rows, onAct, onDismiss, state, onTurn }: SuggestionsProps) {
  const { t } = useI18n()
  // A phone has no hover: its × stays in sight.
  const touch = !useMedia().sm
  if (state === 'off') {
    return (
      <XStack data-slot="suggestions-said" items="center" gap="$2" flexWrap="wrap" paddingInlineStart="$3" minH={PITCH}>
        <SizableText size="$1" color="$soft">
          {t('Hanzo’s model can suggest what to do next from your recent activity.')}
        </SizableText>
        <Button size="xs" variant="ghost" onPress={() => onTurn(true)}>
          {t('Turn on suggestions')}
        </Button>
      </XStack>
    )
  }
  if (!rows.length) return null
  return (
    <YStack data-slot="suggestions">
      <YStack role="list" aria-label={t('Suggestions')}>
        {rows.map((r) => (
          <Row key={r.id} row={r} touch={touch} onAct={onAct} onDismiss={onDismiss} />
        ))}
      </YStack>
      {/* What makes them, said where they are, with the way to stop it. */}
      <XStack data-slot="suggestions-said" items="center" gap="$2" flexWrap="wrap" paddingInlineStart="$3" minH={PITCH}>
        <SizableText size="$1" color="$soft">
          {t('Hanzo’s model reads your recent activity to suggest these.')}
        </SizableText>
        <Button size="xs" variant="ghost" onPress={() => onTurn(false)}>
          {t('Turn off suggestions')}
        </Button>
      </XStack>
    </YStack>
  )
}

/** One row: the whole of it one button that does the act, and its × beside it. Two stops for the keyboard, neither inside the other. */
function Row({ row: kept, touch, onAct, onDismiss }: { row: Suggestion; touch: boolean; onAct: (act: Act, row: Suggestion) => void; onDismiss: (id: string) => void }) {
  const { t } = useI18n()
  const [near, setNear] = useState(false)
  // A starter in the reader's words, its prompt too: what is sent is what they pressed.
  const row = starter(kept) ? { ...kept, title: t(kept.title), act: 'prompt' in kept.act ? { ...kept.act, prompt: t(kept.act.prompt) } : kept.act } : kept
  const Mark = MARKS[row.icon]
  const handleAct = () => onAct(row.act, row)
  const button = (
    <XStack
      render="button"
      data-act={row.act.kind}
      aria-label={row.title}
      onPress={handleAct}
      flex={1}
      minW={0}
      self="stretch"
      items="center"
      gap="$3"
      paddingInlineStart="$3"
      rounded="$4"
      cursor="pointer"
      focusVisibleStyle={RING}
    >
      <Mark size={18} color="$soft" />
      <SizableText fontSize={15} color="$soft" numberOfLines={1} shrink={1} minW={0}>
        {row.title}
      </SizableText>
    </XStack>
  )
  return (
    <XStack
      role="listitem"
      data-slot="suggestion"
      items="center"
      gap="$1"
      minH={PITCH}
      paddingInlineEnd="$2"
      rounded="$4"
      hoverStyle={{ bg: '$hover' }}
      onPointerEnter={() => setNear(true)}
      onPointerLeave={() => setNear(false)}
    >
      {row.summary && !touch ? (
        <HoverCard openDelay={300} closeDelay={150}>
          <HoverCardTrigger flex={1} minW={0} self="stretch" flexDirection="row">
            {button}
          </HoverCardTrigger>
          <HoverCardContent side="top" align="start" sideOffset={8} width={470} maxW="calc(100vw - 32px)" rounded="$6" p="$4">
            <SizableText fontSize={16} lineHeight={26} color="$ink">
              {row.summary}
            </SizableText>
          </HoverCardContent>
        </HoverCard>
      ) : (
        button
      )}
      <XStack
        render="button"
        aria-label={t('Dismiss {suggestion}', { suggestion: row.title })}
        onPress={() => onDismiss(row.id)}
        p="$1.5"
        rounded="$2"
        cursor="pointer"
        opacity={touch || near ? 1 : 0}
        hoverStyle={{ bg: '$hover' }}
        focusVisibleStyle={{ opacity: 1, outlineWidth: 2, outlineStyle: 'solid', outlineColor: '$outlineColor' }}
      >
        <X size={16} color="$soft" />
      </XStack>
    </XStack>
  )
}
