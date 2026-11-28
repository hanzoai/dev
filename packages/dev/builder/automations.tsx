/**
 * One scheduled task: what it does and when, its switch, Run now and its runs.
 * The list of them all is Scheduled (scheduled.tsx).
 *
 *   -/automations/new    a new one: its name, what it does, when it runs
 *   -/automations/<id>   its editor, its switch, Run now and its runs
 *
 * Every read and write is /v1/auto/automations (api/auto.ts), the caller's
 * own: a task is its person's, in the org the builder is in. A save sends only
 * what changed; the switch sends `enabled` alone, so turning one on or off never
 * changes who it runs as.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a task's name, its
 * instructions, a run's summary and post and the platform's refusals are drawn
 * as given.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { ChevronDown, ChevronLeft, ChevronRight, ExternalLink, Play, RotateCcw, SlidersHorizontal, Trash2 } from '@hanzogui/lucide-icons-2'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { Button, Input, Skeleton, Switch, Textarea, toast } from '@hanzo/ui'
import { StatusDot, type SessionStatus } from '@hanzo/ui/chat'
import { ModelPicker } from '@hanzo/ui/models'
import { ChipSelect } from '@hanzo/ui/product'
import { useLimits } from '@hanzo/ui/product/useLimits'
import { useEffect, useMemo, useRef, useState } from 'react'

import { RING, RING_INSET } from './pane.ts'
import { automation, changes, create, remove, review, runs, save, start, starters, words, type Automation, type Draft, type Kind, type Run, type Schedule, type Starter } from './api/auto.ts'
import { current, subscriptions, type Subscription } from './api/billing.ts'
import { SOCIAL } from './api/provider.ts'
import { Out } from './out.tsx'
import { useSocial } from './settings/integrations.tsx'
import { Refusal, told } from './api/call.ts'
import { ENSO, limits as readLimits, models } from './api/models.ts'
import { Choice, Field, Sheet } from './customize/ui.tsx'
import { useRead } from './data.ts'
import { useHost, useTarget } from './host.tsx'
import { runNow, switchOn } from './question.ts'
import { useSpend } from './spend.tsx'
import { SOURCE } from './api/library.ts'
import { Outputs } from './library.tsx'
import { path } from './route.ts'
import { Confirm } from './ask.tsx'

export const zone = (): string => {
  try {
    return Intl.DateTimeFormat().resolvedOptions().timeZone || 'UTC'
  } catch {
    return 'UTC'
  }
}

/** The lists' words are English, marked (`phrase`), and drawn through `t`. */
export const KINDS: readonly { id: Kind; label: string }[] = [
  { id: 'manual', label: phrase('When I run it') },
  { id: 'once', label: phrase('Once') },
  { id: 'hourly', label: phrase('Every hour') },
  { id: 'daily', label: phrase('Every day') },
  { id: 'weekdays', label: phrase('Weekdays') },
  { id: 'weekly', label: phrase('Every week') },
  { id: 'cron', label: phrase('Cron') },
]

const DAYS = [
  { id: 'mon', label: phrase('Monday') },
  { id: 'tue', label: phrase('Tuesday') },
  { id: 'wed', label: phrase('Wednesday') },
  { id: 'thu', label: phrase('Thursday') },
  { id: 'fri', label: phrase('Friday') },
  { id: 'sat', label: phrase('Saturday') },
  { id: 'sun', label: phrase('Sunday') },
]

const PERMISSIONS = [
  { id: 'ask', label: phrase('Ask first') },
  { id: 'auto', label: phrase('Act on its own') },
] as const

/** A run's status as the rail's dot draws it, and in words (marked, drawn through `t`). */
export const DOT: Record<string, SessionStatus> = { succeeded: 'done', failed: 'error', running: 'running', queued: 'running', review: 'paused', skipped: 'stopped', refused: 'stopped' }
export const WORD: Record<string, string> = {
  succeeded: phrase('Succeeded'),
  failed: phrase('Failed'),
  running: phrase('Running'),
  queued: phrase('Queued'),
  review: phrase('Waiting for your review'),
  skipped: phrase('Skipped'),
  refused: phrase('Refused'),
}
const NAME: Record<string, string> = Object.fromEntries(SOCIAL.map((p) => [p.id, p.name]))
export const live = (s: string): boolean => s === 'running' || s === 'queued'

/** A local date and time an hour from now, as a once schedule's `at`. */
const soon = (): string => {
  const d = new Date(Date.now() + 3_600_000)
  const two = (n: number) => String(n).padStart(2, '0')
  return `${d.getFullYear()}-${two(d.getMonth() + 1)}-${two(d.getDate())}T${two(d.getHours())}:00`
}

/** The schedule a kind starts from, keeping whatever of `was` still applies. */
export function shaped(kind: Kind, was: Schedule): Schedule {
  // A schedule that runs only when asked keeps no zone worth keeping: leaving it takes the person's own.
  const tz = was.kind === 'manual' || !was.tz ? zone() : was.tz
  switch (kind) {
    case 'manual':
      return { kind }
    case 'once':
      return { kind, at: soon(), tz }
    case 'hourly':
      return { kind, at: `00:${(was.at ?? '00:00').slice(3) || '00'}`, tz }
    case 'daily':
    case 'weekdays':
      return { kind, at: was.kind === 'hourly' || !was.at ? '09:00' : was.at, tz }
    case 'weekly':
      return { kind, at: was.kind === 'hourly' || !was.at ? '09:00' : was.at, day: was.day || 'mon', tz }
    case 'cron':
      return { kind, cron: was.cron || '0 9 * * 1-5', tz }
  }
}

/** What is wrong with a schedule before it is sent, or '': English, marked (`phrase`), so a screen says it through `t`. */
export function unready(s: Schedule): string {
  if ((s.kind === 'daily' || s.kind === 'weekdays' || s.kind === 'weekly') && !/^([01]?\d|2[0-3]):[0-5]\d$/.test(s.at ?? '')) return phrase('Time of day is HH:MM on a 24-hour clock.')
  if (s.kind === 'once' && s.at && !/^\d{4}-\d{2}-\d{2}T([01]\d|2[0-3]):[0-5]\d$/.test(s.at)) return phrase('Once is a date and a time: YYYY-MM-DDTHH:MM.')
  if (s.kind === 'cron' && (s.cron ?? '').trim().split(/\s+/).length !== 5) return phrase('Cron is five fields: minute, hour, day of month, month, day of week.')
  return ''
}

/** A schedule as the platform reads it: an hourly minute in two digits. */
export function tidy(d: Draft): Draft {
  const s = d.schedule
  if (s.kind !== 'hourly') return d
  const m = Math.min(59, Number((s.at ?? '').slice(3)) || 0)
  return { ...d, schedule: { ...s, at: `00:${String(m).padStart(2, '0')}` } }
}

/** A local time and date for an RFC 3339 instant. */
export const local = (iso: string): string => {
  const d = new Date(iso)
  return Number.isNaN(d.getTime()) ? '' : d.toLocaleString(undefined, { weekday: 'short', month: 'short', day: 'numeric', hour: 'numeric', minute: '2-digit' })
}

/** The editor's copy of an automation: what a save compares against. */
const drafted = (a: Automation): Draft => ({
  name: a.name,
  instructions: a.instructions,
  model: a.model,
  schedule: a.schedule,
  permissions: a.permissions,
  notify: a.notify,
  postTo: a.postTo,
  enabled: a.draft ? true : a.enabled,
  source: a.source,
})

/** `next`, keeping each field of `cur` the person changed from `was` since. */
function rebase(cur: Draft, was: Draft, next: Draft): Draft {
  const out = { ...next }
  for (const k of Object.keys(next) as (keyof Draft)[]) {
    if (JSON.stringify(cur[k]) !== JSON.stringify(was[k])) (out as Record<string, unknown>)[k] = cur[k]
  }
  return out
}

/** A run the platform refused for want of a plan: a free org with no credit for the run fee. `PLAN` says so, marked, drawn through `t`. */
export const unplanned = (e: unknown): boolean => e instanceof Refusal && e.status === 402
export const PLAN = phrase('Automations run on a paid plan; on Free each run needs $1 of credit, and this organization has none. Upgrade to run it.')


const BLANK = (): Draft => ({ name: '', instructions: '', model: null, schedule: { kind: 'manual' }, permissions: 'ask', notify: false, postTo: [], enabled: true, source: 'dev' })

/** When a task runs, chosen: how often, the day and time, and the zone it is read in. */
export function When({ s, onChange, disabled }: { s: Schedule; onChange: (s: Schedule) => void; disabled?: boolean }) {
  const { t } = useI18n()
  const setWhen = (p: Partial<Schedule>) => onChange({ ...s, ...p })
  const kinds = useMemo(() => KINDS.map((k) => ({ ...k, label: t(k.label) })), [t])
  const days = useMemo(() => DAYS.map((x) => ({ ...x, label: t(x.label) })), [t])
  const zones = useMemo(() => {
    try {
      return ['UTC', ...Intl.supportedValuesOf('timeZone').filter((z) => z !== 'UTC')].map((z) => ({ id: z, label: z }))
    } catch {
      return [{ id: 'UTC', label: 'UTC' }]
    }
  }, [])
  const kind = kinds.find((k) => k.id === s.kind) ?? kinds[0]!
  return (
    <XStack gap="$2" flexWrap="wrap" rowGap="$2" items="center" opacity={disabled ? 0.6 : 1} pointerEvents={disabled ? 'none' : 'auto'}>
      <ChipSelect name={t('When it runs')} label={kind.label} chosen={kind} items={kinds} onChange={(k) => onChange(shaped(k.id, s))} placement="bottom-start" width={200} />
      {s.kind === 'weekly' ? <ChipSelect name={t('Day')} label={days.find((x) => x.id === s.day)?.label ?? t('Monday')} chosen={days.find((x) => x.id === s.day)} items={days} onChange={(x) => setWhen({ day: x.id })} placement="bottom-start" width={180} /> : null}
      {s.kind === 'daily' || s.kind === 'weekdays' || s.kind === 'weekly' ? (
        <YStack width={110}>
          <Input value={s.at ?? ''} onChangeText={(v: string) => setWhen({ at: v.trim() })} placeholder="09:00" aria-label={t('Time of day')} inputMode="text" />
        </YStack>
      ) : null}
      {s.kind === 'once' ? (
        <YStack width={190}>
          <Input value={s.at ?? ''} onChangeText={(v: string) => setWhen({ at: v.trim() })} placeholder="2026-10-20T09:00" aria-label={t('Date and time')} autoCapitalize="none" />
        </YStack>
      ) : null}
      {s.kind === 'hourly' ? (
        <XStack items="center" gap="$2">
          <SizableText size="$2" color="$soft">
            {t('at minute')}
          </SizableText>
          <YStack width={70}>
            <Input value={(s.at ?? '00:00').slice(3)} onChangeText={(v: string) => setWhen({ at: `00:${v.replace(/\D/g, '').slice(0, 2)}` })} placeholder="00" aria-label={t('Minute')} inputMode="numeric" />
          </YStack>
        </XStack>
      ) : null}
      {s.kind === 'cron' ? (
        <YStack width={200}>
          <Input value={s.cron ?? ''} onChangeText={(v: string) => setWhen({ cron: v })} placeholder="0 9 * * 1-5" aria-label={t('Cron')} autoCapitalize="none" />
        </YStack>
      ) : null}
      {s.kind !== 'manual' ? <ChipSelect name={t('Time zone')} label={s.tz || 'UTC'} chosen={{ id: s.tz || 'UTC', label: s.tz || 'UTC' }} items={zones} onChange={(z) => setWhen({ tz: z.id })} placeholder={t('Search zones…')} placement="bottom-start" width={260} /> : null}
    </XStack>
  )
}

/** One automation: what it does and when, its switch, Run now, and its runs. `new` writes one. */
export function Editor({ id }: { id: string }) {
  const host = useHost()
  const { t } = useI18n()
  const target = useTarget()
  const desktop = Boolean(useMedia().sm)
  const spend = useSpend()
  const signed = Boolean(host.person)
  const fresh = id === 'new'
  // `tick` reads the automation again; `beat` reads its runs again.
  const [tick, setTick] = useState(0)
  const [beat, setBeat] = useState(0)
  const saved = useRead(signed && !fresh ? () => automation(target, id) : null, null as Automation | null, [target, signed, id, tick])
  const history = useRead(signed && !fresh ? () => runs(target, id) : null, [] as Run[], [target, signed, id, beat])
  const catalog = useRead(signed ? () => models(target) : null, [], [target, signed])
  const { limits } = useLimits(signed ? (signal) => readLimits(target, signal) : null, target)
  // `base` is what the form started from; a save sends what changed since, and only that.
  const [base, setBase] = useState<Draft>(BLANK)
  const [d, setD] = useState<Draft>(BLANK)
  const [ready, setReady] = useState(fresh)
  const [busy, setBusy] = useState('')
  const [note, setNote] = useState('')
  const [noteError, setNoteError] = useState(false)
  const [asking, setAsking] = useState(false)
  // Where a move away from unsaved work was headed, while it asks first; '' for nowhere.
  const [leaving, setLeaving] = useState('')
  const [upgrade, setUpgrade] = useState(false)
  const [moreStarters, setMoreStarters] = useState(false)
  const [optionsOpen, setOptionsOpen] = useState<boolean | null>(null)
  const [allRuns, setAllRuns] = useState(false)
  const social = useSocial(signed, SOCIAL.filter((p) => p.posts).map((p) => p.id))
  const subs = useRead(signed ? () => subscriptions(target) : null, [] as Subscription[], [target, signed])
  // Free is what the platform says when it names no live subscription; a read that failed says nothing.
  const free = !subs.loading && !subs.error && !current(subs.value)
  const nameRef = useRef<HTMLInputElement>(null)
  const asksRef = useRef<HTMLTextAreaElement>(null)
  const offered = useRead(signed && fresh ? () => starters(target) : null, [] as Starter[], [target, signed, fresh])
  const here = useRef(true)
  useEffect(() => {
    here.current = true
    return () => {
      here.current = false
    }
  }, [])
  const a = saved.value

  // The switch of a saved automation is saved as it moves; of a new one or a draft, with the rest.
  const switchesAlone = Boolean(a && !a.draft)
  const change = changes(base, d)
  if (switchesAlone) delete change.enabled
  const dirty = Object.keys(change).length > 0

  // What is saved now becomes the form's start, unless the person is mid-edit: then
  // their edit stands, and what they did not touch is not sent back over it.
  const clean = useRef(true)
  clean.current = !dirty
  useEffect(() => {
    if (!a) return
    if (clean.current) {
      setBase(drafted(a))
      setD(drafted(a))
    }
    setReady(true)
  }, [a])

  // A run going is read every 4s; a scheduled automation's runs every 30s, so a tick's run appears.
  const going = history.value.some((r) => live(r.status))
  const every = going ? 4000 : a && a.enabled && a.schedule.kind !== 'manual' ? 30_000 : 0
  useEffect(() => {
    if (!every) return
    const timer = setInterval(() => setBeat((n) => n + 1), every)
    return () => clearInterval(timer)
  }, [every])

  // An edit answers what the last line asked for, so the line goes with it.
  const set = (p: Partial<Draft>) => {
    setNote('')
    setNoteError(false)
    setUpgrade(false)
    setD((x) => ({ ...x, ...p }))
  }
  const wrong = t(unready(d.schedule))

  // A draft waits on its instructions: they have the focus when it opens.
  const drafty = Boolean(a?.draft)
  useEffect(() => {
    if (ready && drafty) asksRef.current?.focus()
  }, [ready, drafty])

  const act = async (what: string, work: () => Promise<void>) => {
    setBusy(what)
    setNote('')
    setNoteError(false)
    setUpgrade(false)
    try {
      await work()
    } catch (e) {
      // Wanting a plan is said in these words only where a run was asked for.
      const run = what === 'run' && unplanned(e)
      setNote(run ? t(PLAN) : told(e, t))
      setNoteError(true)
      setUpgrade(run)
    } finally {
      setBusy('')
    }
  }

  /** What a save still needs, said, with the focus on where to give it; true when nothing. */
  const complete = (): boolean => {
    const ask = !d.name.trim() ? ([t('Name it first.'), nameRef] as const) : !d.instructions.trim() ? ([t('Add instructions first.'), asksRef] as const) : null
    if (ask) {
      setNote(ask[0])
      setNoteError(true)
      ask[1].current?.focus()
      return false
    }
    if (wrong) {
      setNote(wrong)
      setNoteError(true)
      return false
    }
    return true
  }

  const keep = async () => {
    // A save after which it runs, and spends, by itself where it did not before (a new one, a draft finished On, one
    // switched on in the form) is asked first. A draft never ran, whatever its switch said.
    // A task that runs already is switched on and off by its switch alone, which asks for itself.
    const ran = !fresh && Boolean(a) && !a!.draft && base.enabled
    const turning = tidy(d).enabled && (fresh || !switchesAlone) && !ran
    if (turning && !(await spend.ask(switchOn(d.name.trim(), t)))) return
    await act('save', async () => {
      if (fresh) {
        const made = await create(target, tidy(d))
        // Only while still here: a person who left during the create stays where they went.
        if (here.current) host.go(path({ kind: 'automation', id: made.id }), { replace: true })
        return
      }
      const snap = d
      const send = changes(base, tidy(snap))
      if (switchesAlone) delete send.enabled
      const now = Object.keys(send).length ? await save(target, id, send) : a!
      // What was typed while the save was in flight stands.
      setBase(drafted(now))
      setD((cur) => rebase(cur, snap, drafted(now)))
      setTick((n) => n + 1)
      setNoteError(false)
      setNote(t('Saved.'))
    })
  }

  const flip = async (on: boolean) => {
    if (!switchesAlone) return set({ enabled: on })
    if (on && !(await spend.ask(switchOn(a!.name, t)))) return
    void act('switch', async () => {
      await save(target, id, { enabled: on })
      setTick((n) => n + 1)
      toast.success(on ? t('Automation turned on') : t('Automation paused'))
    })
  }

  const go = async () => {
    if (!(await spend.ask(runNow(a!.name, t)))) return
    await act('run', async () => {
      const run = await start(target, id)
      setBeat((n) => n + 1)
      if (run.status === 'skipped') {
        setNote(t('Not started: the previous run is still going.'))
        setNoteError(true)
      } else {
        toast.success(t('Run started'))
      }
    })
  }

  const leave = (how?: { replace?: boolean }) => host.go(path({ kind: 'screen', screen: 'automations' }), how)
  // A held post is answered once; its buttons go when it is.
  const [answered, setAnswered] = useState(() => new Set<string>())
  const answer = (r: Run, post: boolean) =>
    act('review', async () => {
      await review(target, id, r.id, post)
      setAnswered((x) => new Set(x).add(r.id))
      setBeat((n) => n + 1)
      setTick((n) => n + 1)
    })

  // Unsaved work is not left without a word, wherever the move goes.
  const away = (to: string) => (dirty ? setLeaving(to) : host.go(to))
  const back = () => away(path({ kind: 'screen', screen: 'automations' }))
  const begin = (x: Starter) => {
    const tz = x.schedule.kind === 'manual' ? undefined : zone()
    set({ name: x.name, instructions: x.instructions, schedule: tz ? { ...x.schedule, tz } : { kind: 'manual' } })
    asksRef.current?.focus()
  }

  if (!signed) return <Page title={t('Automation')} onBack={() => leave()} says={t("Sign in to see this organization's automations.")} />
  if (!fresh && saved.error && !a) return <Page title={t('Automation')} onBack={() => leave()} says={told(saved.error, t)} error onRetry={() => setTick((n) => n + 1)} />
  if (!ready) return <Page title={t('Automation')} onBack={() => leave()} says={t('Reading the automation…')} loading />

  const s = d.schedule
  // An automation that posts speaks for the organization: only an admin changes it.
  const locked = !host.admin && Boolean(a?.postTo.length)
  // A new automation or a draft has nothing to switch on until it says what to do.
  const can = switchesAlone || Boolean(d.instructions.trim())
  const on = switchesAlone ? Boolean(a?.enabled) : can && d.enabled
  const says = switchesAlone ? (on ? t('On') : t('Off')) : !can ? t('Add instructions first') : on ? t('On when saved') : t('Off when saved')
  const state = fresh ? t('Not created yet') : dirty ? t('Unsaved changes') : ''
  const sayId = `${id}-switch`
  const starterCap = 4
  const starterOverflow = offered.value.length > starterCap
  const visibleStarters = !moreStarters && starterOverflow ? offered.value.slice(0, starterCap) : offered.value
  const showOptions = optionsOpen ?? (desktop || Boolean(d.postTo.length))
  const modelLabel = d.model ? (catalog.value.find((m) => m.id === d.model)?.name ?? d.model) : 'Enso'
  const permLabel = d.permissions === 'auto' ? t('Act on its own') : t('Ask first')
  const optionsSummary = [
    modelLabel,
    permLabel,
    d.postTo.length ? t('Posts to {n}', { n: d.postTo.length }) : '',
    d.notify ? t('Email on') : '',
  ]
    .filter(Boolean)
    .join(' · ')
  const runsCap = 8
  const runsOverflow = history.value.length > runsCap
  const visibleHistory = !allRuns && runsOverflow ? history.value.slice(0, runsCap) : history.value

  return (
    <YStack flex={1} minH={0}>
      {spend.dialog}
      <YStack shrink={0} px="$6" pt="$4" pb="$3" $max-md={{ px: '$4' }} borderBottomWidth={1} borderColor="$borderColor">
        <XStack width="100%" maxW={760} mx="auto" items="center" justify="space-between" gap="$3" flexWrap="wrap" rowGap="$2">
          <XStack items="center" gap="$2">
            <Button size="sm" variant="ghost" onPress={back} aria-label={t('All scheduled tasks')}>
              <ChevronLeft size={14} {...{ 'data-i18n-mirror': '' }} />
              {t('Scheduled tasks')}
            </Button>
            {state ? (
              <SizableText size="$1" color="$soft" role="status">
                {state}
              </SizableText>
            ) : null}
          </XStack>
          <XStack items="center" gap="$2" flexWrap="wrap">
            <XStack items="center" gap="$2">
              <SizableText id={sayId} size="$2" color="$soft">
                {says}
              </SizableText>
              <Switch
                checked={on}
                disabled={busy === 'switch' || !can}
                onCheckedChange={(next: boolean) => void flip(next)}
                aria-label={switchesAlone ? t('{name} on', { name: a!.name }) : t('On when saved')}
                aria-describedby={sayId}
              />
            </XStack>
            {a && !a.draft ? (
              <Button size="sm" variant="outline" disabled={Boolean(busy) || dirty} onPress={() => void go()}>
                <Play size={14} />
                {busy === 'run' ? t('Starting…') : t('Run now')}
              </Button>
            ) : null}
            {a && dirty ? (
              <Button
                size="sm"
                variant="ghost"
                disabled={Boolean(busy)}
                onPress={() => {
                  setBase(drafted(a))
                  setD(drafted(a))
                  setNote('')
                  setNoteError(false)
                  toast.info(t('Changes discarded'))
                }}
              >
                <RotateCcw size={14} />
                {t('Discard changes')}
              </Button>
            ) : null}
            <Button size="sm" variant="primary" disabled={Boolean(busy) || locked || !(fresh || drafty || dirty)} onPress={() => complete() && void keep()}>
              {busy === 'save' ? t('Saving…') : fresh ? t('Create') : t('Save')}
            </Button>
          </XStack>
        </XStack>
        {note ? (
          <XStack width="100%" maxW={760} mx="auto" items="center" gap="$3" pt="$2" flexWrap="wrap">
            <SizableText size="$1" color={noteError ? 'var(--state-error)' : '$soft'} role="status">
              {note}
            </SizableText>
            {upgrade ? (
              <Button size="sm" variant="outline" onPress={() => away(path({ kind: 'screen', screen: 'plans' }))}>
                {t('See plans')}
              </Button>
            ) : null}
          </XStack>
        ) : null}
      </YStack>
      <YStack flex={1} minH={0} overflow="scroll" px="$6" py="$5">
        <YStack width="100%" maxW={760} mx="auto" gap="$5">
          {locked ? (
            <SizableText size="$1" color="$soft" role="note">
              {t('This automation posts to the organization’s accounts, so only an org admin changes it.')}
            </SizableText>
          ) : null}
          {free ? (
            <XStack items="center" gap="$3" px="$3" py="$2" rounded="$3" borderWidth={1} borderColor="$borderColor" flexWrap="wrap" rowGap="$2">
              <SizableText flex={1} minW={200} size="$1" color="$soft">
                {t('Paid plans include automation runs. On the Free plan each run needs $1 of credit.')}
              </SizableText>
              <Button size="sm" variant="outline" onPress={() => away(path({ kind: 'screen', screen: 'plans' }))}>
                {t('See plans')}
              </Button>
            </XStack>
          ) : null}
          <YStack gap="$1">
            <SizableText render="h1" size="$6" color="$ink" numberOfLines={1}>
              {fresh ? t('New automation') : a?.name}
            </SizableText>
            <SizableText size="$2" color="$soft">
              {fresh
                ? t('Name it, say what it should do, and when. It runs as you, in this organization.')
                : a?.draft
                  ? t('A draft: say what it should do and save it, and it runs.')
                  : [
                      t('From {product}', { product: t(SOURCE[a!.source]) }),
                      words(a!.schedule, t),
                      a!.next && a!.enabled ? t('next {when}', { when: local(a!.next) }) : a!.schedule.kind === 'manual' ? '' : t('off'),
                    ]
                      .filter(Boolean)
                      .join(' · ')}
            </SizableText>
          </YStack>

          {fresh && offered.value.length && !d.instructions.trim() ? (
            <XStack gap="$2" flexWrap="wrap" rowGap="$2" items="center">
              <SizableText size="$2" color="$soft">
                {t('Start from')}
              </SizableText>
              {visibleStarters.map((x) => (
                <Button key={x.key} size="sm" variant="outline" onPress={() => begin(x)} aria-label={t('Start from {name}', { name: x.name })}>
                  {x.name}
                </Button>
              ))}
              {starterOverflow ? (
                <XStack
                  render="button"
                  aria-expanded={moreStarters}
                  onPress={() => setMoreStarters((v) => !v)}
                  px="$2.5"
                  py="$1"
                  minH={32}
                  items="center"
                  focusVisibleStyle={RING}
                >
                  <SizableText size="$1" fontWeight="500" color="$ink">
                    {moreStarters ? t('Show less') : t('+{n} more', { n: offered.value.length - visibleStarters.length })}
                  </SizableText>
                </XStack>
              ) : null}
            </XStack>
          ) : null}
          <Field label={t('Name')}>
            <Input
              ref={nameRef}
              value={d.name}
              onChangeText={(v: string) => set({ name: v })}
              onSubmitEditing={() => asksRef.current?.focus()}
              placeholder={t('Morning briefing')}
              aria-label={t('Automation name')}
              autoFocus={fresh}
              disabled={locked}
            />
          </Field>
          <Field label={t('Instructions (required)')} hint={t("What the agent does each run. It works in a Dev sandbox with this organization's connectors, MCP servers and skills.")}>
            <Textarea
              ref={asksRef}
              disabled={locked}
              value={d.instructions}
              onChangeText={(v: string) => set({ instructions: v })}
              placeholder={t('What should it do each run? For example: summarize what needs my attention today across calendar, email and messages.')}
              aria-label={t('Instructions')}
              rows={7}
            />
          </Field>
          <Field label={t('When it runs')} hint={s.kind === 'cron' ? t('Five fields: minute, hour, day of month, month, day of week.') : s.kind === 'once' ? t('It runs at that time and then switches itself off.') : undefined}>
            <When s={s} onChange={(next) => set({ schedule: next })} disabled={locked} />
          </Field>

          <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" overflow="hidden">
            <XStack
              render="button"
              aria-expanded={showOptions}
              onPress={() => setOptionsOpen(!showOptions)}
              items="center"
              gap="$2.5"
              px="$4"
              py="$3"
              minH={44}
              width="100%"
              $platform-web={{ textAlign: 'start' }}
              focusVisibleStyle={RING_INSET}
            >
              <SlidersHorizontal size={15} color="$soft" />
              <YStack flex={1} minW={0} gap="$0.5">
                <SizableText size="$2" fontWeight="500" color="$ink">
                  {t('Options & delivery')}
                </SizableText>
                <SizableText size="$1" color="$soft" numberOfLines={1}>
                  {optionsSummary}
                </SizableText>
              </YStack>
              {showOptions ? <ChevronDown size={15} color="$soft" /> : <ChevronRight size={15} color="$soft" />}
            </XStack>
            {showOptions ? (
              <YStack gap="$4" px="$4" pb="$4" pt="$3" borderTopWidth={1} borderColor="$borderColor">
                <XStack gap="$4" items="flex-start" $max-sm={{ flexDirection: 'column', gap: '$3' }}>
                  <YStack flex={1} minW={0} $max-sm={{ flexBasis: 'auto', grow: 0, minW: 0, width: '100%' }}>
                    <Field label={t('Model')}>
                      <XStack>
                        <ModelPicker
                          size="sm"
                          name={t('Model')}
                          models={catalog.value}
                          scope="chat"
                          limits={limits}
                          value={d.model ?? ENSO}
                          onChange={(id) => set({ model: id === ENSO ? null : id })}
                          loading={catalog.loading}
                          error={catalog.error ? told(catalog.error, t) : null}
                        />
                      </XStack>
                    </Field>
                  </YStack>
                  <YStack flex={1} minW={0} $max-sm={{ flexBasis: 'auto', grow: 0, minW: 0, width: '100%' }}>
                    <Field
                      label={t('Permissions')}
                      hint={d.permissions === 'auto' ? t('Works and uses connectors without stopping.') : t('Reads and researches, changes nothing, and ends with the actions it would take for you to approve.')}
                    >
                      <Choice label={t('Permissions')} value={d.permissions} options={PERMISSIONS.map((p) => ({ ...p, label: t(p.label) }))} onChange={(p) => set({ permissions: p })} />
                    </Field>
                  </YStack>
                </XStack>

                <Field
                  label={t('Post to')}
                  hint={
                    !host.admin
                      ? t('An org admin chooses where automations post: the accounts are the organization’s.')
                      : !d.postTo.length
                        ? t('Turn an account on and each run’s answer is a post for it.')
                        : d.permissions === 'auto'
                          ? t('Each run posts what the agent writes, as written.')
                          : t('Each run holds what the agent writes for you to post or discard.')
                  }
                >
                  <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" bg="$background" overflow="hidden">
                    {SOCIAL.filter((p) => p.posts).map((p, i) => {
                      const c = social.value[p.id]
                      const on = d.postTo.includes(p.id)
                      return (
                        <XStack key={p.id} items="center" gap="$3" px="$3" py="$2.5" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                          <YStack flex={1} minW={0} gap="$0.5">
                            <SizableText size="$2" fontWeight="500" color="$ink">
                              {p.name}
                            </SizableText>
                            <SizableText size="$1" color="$soft">
                              {social.error
                                ? told(social.error, t)
                                : !c
                                  ? t('Reading…')
                                  : c.connected
                                    ? c.account
                                      ? t('Connected as {account}', { account: c.account })
                                      : t('Connected')
                                    : `${c.available ? t('Not connected: an org admin connects it in Settings → Integrations.') : t(p.needs)} ${
                                        on || d.permissions === 'ask'
                                          ? t('Runs hold the post for you to read; posting it fails until it is connected.')
                                          : t('Acting on its own, it posts only to a connected account.')
                                      }`}
                            </SizableText>
                          </YStack>
                          <Switch
                            checked={on}
                            disabled={!host.admin || (!on && !c?.connected && d.permissions === 'auto')}
                            onCheckedChange={(v: boolean) => set({ postTo: v ? [...d.postTo, p.id].sort() : d.postTo.filter((x) => x !== p.id) })}
                            aria-label={t('Post to {name}', { name: p.name })}
                          />
                        </XStack>
                      )
                    })}
                  </YStack>
                  <XStack>
                    <Button size="sm" variant="ghost" onPress={() => away(path({ kind: 'settings', section: 'integrations' }))}>
                      {t('Connect accounts')}
                    </Button>
                  </XStack>
                </Field>

                <XStack items="center" gap="$3" pt="$2" borderTopWidth={1} borderColor="$borderColor">
                  <YStack flex={1} minW={0}>
                    <SizableText size="$2" fontWeight="500" color="$ink">
                      {t('Email me when a run ends')}
                    </SizableText>
                    <SizableText size="$1" color="$soft">
                      {t('One line on how it went, with a link to the run.')}
                    </SizableText>
                  </YStack>
                  <Switch checked={d.notify} onCheckedChange={(v: boolean) => set({ notify: v })} aria-label={t('Email me when a run ends')} />
                </XStack>
              </YStack>
            ) : null}
          </YStack>

          {dirty && wrong && note !== wrong ? (
            <SizableText size="$1" color="var(--state-error)" role="status">
              {wrong}
            </SizableText>
          ) : null}
          {a ? (
            <XStack items="center" justify="space-between" gap="$3" pt="$3" borderTopWidth={1} borderColor="$borderColor" flexWrap="wrap">
              <SizableText size="$1" color="$soft">
                {t('Stops the schedule and deletes this automation’s run history.')}
              </SizableText>
              <Button size="sm" variant="ghost" tone="danger" disabled={Boolean(busy)} onPress={() => setAsking(true)}>
                <Trash2 size={14} />
                {t('Delete')}
              </Button>
            </XStack>
          ) : null}

          {a ? (
            <YStack gap="$2.5">
              <SizableText render="h2" size="$3" fontWeight="500" color="$ink">
                {t('Runs')}
              </SizableText>
              {history.error && !history.value.length ? (
                <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$4" py="$5" gap="$2.5" items="flex-start">
                  <SizableText size="$2" color="var(--state-error)">
                    {told(history.error, t)}
                  </SizableText>
                  <Button size="sm" variant="outline" onPress={() => setBeat((n) => n + 1)}>
                    {t('Try again')}
                  </Button>
                </YStack>
              ) : history.loading && !history.value.length ? (
                <YStack gap="$2" aria-busy={true}>
                  <SizableText size="$2" color="$soft">
                    {t('Reading its runs…')}
                  </SizableText>
                  <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" bg="$panel" overflow="hidden">
                    {[0, 1, 2].map((i) => (
                      <XStack key={i} items="center" gap="$3" px="$3" py="$2.5" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                        <Skeleton layout="block" width={10} height={10} rounded={999} shrink={0} />
                        <YStack flex={1} gap="$1.5">
                          <Skeleton layout="block" width="44%" height={14} rounded="$2" />
                          <Skeleton layout="block" width="68%" height={12} rounded="$2" />
                        </YStack>
                      </XStack>
                    ))}
                  </YStack>
                </YStack>
              ) : !history.value.length ? (
                <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" bg="$panel" px="$4" py="$5">
                  <SizableText size="$2" color="$soft">
                    {a.draft ? t('It runs once its instructions are saved.') : t('No runs yet. Run now starts one.')}
                  </SizableText>
                </YStack>
              ) : (
                <YStack gap="$2">
                  <YStack role="list" borderWidth={1} borderColor="$borderColor" rounded="$3" bg="$panel" overflow="hidden" aria-label={t('Run history')}>
                    {visibleHistory.map((r, i) => (
                      <XStack role="listitem" key={r.id} items="flex-start" gap="$3" rowGap="$2" px="$3" py="$2.5" borderTopWidth={i ? 1 : 0} borderColor="$borderColor" flexWrap="wrap">
                        <YStack pt="$1">
                          <StatusDot status={DOT[r.status] ?? 'idle'} />
                        </YStack>
                        <YStack flex={1} minW={180} gap="$0.5">
                          <SizableText size="$2" fontWeight="500" color="$ink">
                            {`${WORD[r.status] ? t(WORD[r.status]) : r.status} · ${local(r.at)}`}
                          </SizableText>
                          <SizableText size="$1" color="$soft" $platform-web={{ whiteSpace: 'pre-wrap' }}>
                            {r.summary || (live(r.status) ? t('Working…') : '')}
                          </SizableText>
                          {r.draft ? (
                            <YStack mt="$1.5" px="$3" py="$2" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor" role="group" aria-label={t('The post')}>
                              <SizableText size="$2" color="$ink" $platform-web={{ whiteSpace: 'pre-wrap' }}>
                                {r.draft}
                              </SizableText>
                            </YStack>
                          ) : null}
                          {r.outputs.length ? <Outputs ids={r.outputs} /> : null}
                          {r.posts.map((x) => (
                            <XStack key={x.to} items="center" gap="$2" pt="$1">
                              <SizableText size="$1" color="$ink">
                                {NAME[x.to] ?? x.to}
                              </SizableText>
                              {x.url ? (
                                <Out href={x.url} label={t('Open the {name} post', { name: NAME[x.to] ?? x.to })}>
                                  <SizableText size="$1" color="$ink" textDecorationLine="underline">
                                    {t('Posted · open')}
                                  </SizableText>
                                  <ExternalLink size={11} color="$soft" />
                                </Out>
                              ) : (
                                <SizableText size="$1" color="var(--state-error)">
                                  {x.error}
                                </SizableText>
                              )}
                            </XStack>
                          ))}
                          {r.status === 'review' && host.admin && !answered.has(r.id) ? (
                            <XStack gap="$2" pt="$2" flexWrap="wrap">
                              <Button size="sm" variant="primary" disabled={Boolean(busy)} onPress={() => void answer(r, true)} aria-label={t('Post the post of {when}', { when: local(r.at) })}>
                                {t('Post it')}
                              </Button>
                              <Button size="sm" variant="ghost" disabled={Boolean(busy)} onPress={() => void answer(r, false)} aria-label={t('Discard the post of {when}', { when: local(r.at) })}>
                                {t('Discard')}
                              </Button>
                            </XStack>
                          ) : r.status === 'review' && !host.admin ? (
                            <SizableText size="$1" color="$soft" pt="$1">
                              {t('An org admin posts or discards it.')}
                            </SizableText>
                          ) : null}
                        </YStack>
                        {r.session ? (
                          <Button size="sm" variant="ghost" onPress={() => host.go(r.session!)} aria-label={t('Open the Dev run of {when}', { when: local(r.at) })}>
                            <ExternalLink size={13} />
                            {t('Open run')}
                          </Button>
                        ) : null}
                      </XStack>
                    ))}
                  </YStack>
                  {runsOverflow ? (
                    <XStack>
                      <XStack
                        render="button"
                        aria-expanded={allRuns}
                        onPress={() => setAllRuns((v) => !v)}
                        px="$2.5"
                        py="$1.5"
                        minH={32}
                        items="center"
                        focusVisibleStyle={RING}
                      >
                        <SizableText size="$1" fontWeight="500" color="$soft">
                          {allRuns ? t('Show fewer runs') : t('Show all {n} runs', { n: history.value.length })}
                        </SizableText>
                      </XStack>
                    </XStack>
                  ) : null}
                </YStack>
              )}
            </YStack>
          ) : null}
        </YStack>
        {a ? (
          <Confirm
            title={t('Delete {name}?', { name: a.name })}
            act={t('Delete')}
            says={t('It stops running, and its schedule and its runs go with it. A run going now is stopped.')}
            open={asking}
            onOpenChange={setAsking}
            run={async () => {
              const name = a.name
              await remove(target, a.id)
              toast.success(t('{name} deleted', { name }))
              leave({ replace: true })
            }}
          />
        ) : null}
        <Sheet title={t('Leave without saving?')} open={Boolean(leaving)} onOpenChange={(o) => !o && setLeaving('')} width={420}>
          <SizableText size="$2" color="$soft">
            {fresh ? t('This automation is not created yet.') : t('Your changes to this automation are not saved.')}
          </SizableText>
          <XStack gap="$2" justify="flex-end">
            <Button size="sm" variant="ghost" onPress={() => setLeaving('')}>
              {t('Keep editing')}
            </Button>
            <Button size="sm" variant="destructive" onPress={() => host.go(leaving)}>
              {t('Leave')}
            </Button>
          </XStack>
        </Sheet>
      </YStack>
    </YStack>
  )
}

/** A page with nothing to edit yet: its title and what it says come already said. */
function Page({ title, says, onBack, loading = false, error = false, onRetry }: { title: string; says: string; onBack: () => void; loading?: boolean; error?: boolean; onRetry?: () => void }) {
  const { t } = useI18n()
  return (
    <YStack flex={1} minH={0} overflow="scroll" px="$6" py="$6">
      <YStack width="100%" maxW={760} mx="auto" gap="$4">
        <XStack>
          <Button size="sm" variant="ghost" onPress={onBack} aria-label={t('All scheduled tasks')}>
            <ChevronLeft size={14} {...{ 'data-i18n-mirror': '' }} />
            {t('Scheduled tasks')}
          </Button>
        </XStack>
        <SizableText render="h1" size="$6" color="$ink">
          {title}
        </SizableText>
        <SizableText size="$2" color="$soft">
          {says}
        </SizableText>
        {loading ? (
          <YStack gap="$3" aria-busy={true}>
            <Skeleton layout="block" width="100%" height={40} rounded="$3" />
            <Skeleton layout="block" width="100%" height={140} rounded="$3" />
            <Skeleton layout="block" width="60%" height={36} rounded="$3" />
          </YStack>
        ) : null}
        {error && onRetry ? (
          <XStack>
            <Button size="sm" variant="outline" onPress={onRetry}>
              {t('Try again')}
            </Button>
          </XStack>
        ) : null}
      </YStack>
    </YStack>
  )
}
