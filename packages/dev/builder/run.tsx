/**
 * One run, live: its transcript as it streams, the controls that reach it —
 * steer, pause, resume, stop — the pull request once it pushes one, and the
 * follow-up that continues it once it has finished.
 *
 * The detail read is the record and the stream adds to it; the two overlap
 * whenever a turn lands between the read and the subscribe, so turns are merged
 * by sequence. A reconnect after a drop re-reads the detail — the platform drops
 * a subscriber that falls behind, and frames missed then are only in the read.
 *
 * Steering a machine's run is a queue it drains between steps: a press is
 * confirmed as RECORDED, and the status is left to the feed to correct. A
 * sandbox run is steered through `POST /v1/agent/coding/{session}/steer`, which
 * stops it with its work kept and opens a follow-up with the words.
 *
 * A sandbox run is one agent invocation that takes its task on its command line
 * (apps/coding steer.go), so what continues it is a new run: a paused run is
 * carried on, and a finished one followed up, by `POST /v1/agent/coding` from
 * the branch its work was kept on — the platform's own continue, done here —
 * with the model and effort the composer shows (mind.tsx), which Chat and New share.
 *
 * It reads as a chat: the person's ask opens it, each thing the agent said can
 * be copied and judged (a verdict is `build.verdict` on the event bus, as a
 * project's turns send it), and the title is the menu of what can be done to
 * the run. Share opens its story to the public build route.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import {
  Check,
  ChevronDown,
  ChevronRight,
  Clock,
  Copy,
  ExternalLink,
  FileDiff,
  Folder,
  GitPullRequest,
  Globe,
  Layers,
  MessageSquare,
  MoreHorizontal,
  Pencil,
  Share,
  SlidersHorizontal,
  Square,
  SquareTerminal,
} from '@hanzogui/lucide-icons-2'
import {
  Button,
  Dialog,
  DialogContent,
  DialogTitle,
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuShortcut,
  DropdownMenuSub,
  DropdownMenuSubContent,
  DropdownMenuSubTrigger,
  DropdownMenuTrigger,
  Input,
  type DropdownMenuProps,
} from '@hanzo/ui'
import { Steer, type Command } from '@hanzo/ui/agents'
import { Composer, Failure, measure } from '@hanzo/ui/chat'
import { useLimits } from '@hanzo/ui/product/useLimits'
import { useDictation, useVoice } from '@hanzo/voice'
import { useEffect, useMemo, useState } from 'react'

import { Confirm } from './ask.tsx'
import { copy as toClipboard } from './clipboard.ts'
import { approve, followUp, headline, retry, start, steer, type Ask, type Earlier } from './api/coding.ts'
import { limits as readLimits } from './api/models.ts'
import { get, list, message, pause, publish, remove, rename, resume, stop, story, took, type Event } from './api/sessions.ts'
import { answer, cards, followed, outcome, pull, settled, steps, type Card } from './api/turn.ts'
import { answer as reply, say as talk, talks } from './api/talk.ts'
import { verdict } from './api/verdict.ts'
import { useKept, useProjects, useRead, useRun, type RunState } from './data.ts'
import { mark } from './runmeta.ts'
import { Desk, SEED } from './desk.tsx'
import { useDevMode } from './dev-mode.ts'
import { KeepAwake, useKeepAwake } from './awake.tsx'
import { useHost, useTarget, type Ask as Question } from './host.tsx'
import { Publish, type Source } from './publish.tsx'
import { path } from './route.ts'
import { useSpend } from './spend.tsx'
import { level, pacing, useMind } from './mind.ts'
import { Out } from './out.tsx'
import { Gutter } from './gutter.tsx'
import { gap, pane, RING, RING_INSET } from './pane.ts'
import { useEar, useSpoken } from './voice.ts'
import { speaker, type Artifact } from './reply.tsx'
import { PanelToggle } from './panel.tsx'
import { schedule } from './scheduled.tsx'
import { SideProvider, useSplit } from './side.tsx'
import { PAGE, useDeck, useKey, usePanel } from './tabs.ts'
import {
  cleanErrorText,
  isAlreadyRunningError,
  isSessionActive,
  isSessionLive,
  linkThread,
  resolveSessionState,
  threadChain,
  useThreadLinks,
} from './thread.ts'
import { carrying, compose, pick, read as readFiles, type Attached } from './tools.tsx'
import { useCarry } from './carry.tsx'
import { Talk } from './talk.tsx'
import { Transcript, type QueuedMessage, type TranscriptView } from './transcript.tsx'

/** A session's status as the line under its title says it; one the service adds later is drawn as it comes. */
const STATUS: Record<string, string> = {
  running: phrase('running'),
  started: phrase('running'),
  starting: phrase('starting'),
  queued: phrase('queued'),
  routed: phrase('running'),
  resumed: phrase('running'),
  paused: phrase('paused'),
  done: phrase('done'),
  completed: phrase('done'),
  error: phrase('error'),
  stopped: phrase('stopped'),
}

type MenuItems = NonNullable<DropdownMenuProps['items']>

/** Snapshot of an earlier turn inside a multi-turn single-thread session. */
interface TurnSnapshot {
  events: Event[]
  title: string
  repo: string
  mode: string
}

/** One session's page: a conversation (talk.tsx) or one unified coding session thread, as its record says. */
export function Run({ id }: { id: string }) {
  const host = useHost()
  const t = useTarget()
  const links = useThreadLinks()
  const chain = useMemo(() => threadChain(id, links), [id, links])
  const activeId = chain[chain.length - 1] ?? id
  // Signed out, there is nothing to read: the gateway answers a bare session with a refusal.
  const state = useRun(t, host.person ? activeId : null)
  return talks(state.record) ? <Talk id={activeId} state={state} /> : <Job id={id} activeId={activeId} chain={chain} state={state} />
}

function Job({ id, activeId, chain, state: read }: { id: string; activeId: string; chain: string[]; state: RunState }) {
  const { t: say } = useI18n()
  const host = useHost()
  const t = useTarget()
  const spend = useSpend()
  const signed = Boolean(host.person)
  const [devMode, setDevMode] = useDevMode()
  const { detail, record, events, status, refused } = read
  const [draft, setDraft] = useState('')
  const [files, setFiles] = useState<Attached[]>([])
  const [note, setNote] = useState('')
  const [busy, setBusy] = useState(false)
  const [notify, setNotify] = useState<'device' | 'in-app' | null>(null)
  // The side panel: open or shut as Dev was left, its tabs as they were arranged.
  const wide = useMedia().md
  const [desk, setDesk] = usePanel('dev', true, wide)
  const side = useDeck('dev', SEED)
  useKey(() => setDesk(!desk))
  // The side panel's column: the person's width, the run hidden behind it, full screen (side.tsx).
  const split = useSplit(desk && wide, 'run', () => setDesk(false))
  // Stop was pressed here: the session keeps its work after it answers, and says so with its status.
  const [stopping, setStopping] = useState(false)
  // The working sandbox turn words were said to here: its follow-up continues in this thread once it starts.
  const [steered, setSteered] = useState('')
  // Earlier turns in this single-thread session, keyed by session id.
  const [historyTurns, setHistoryTurns] = useState<Record<string, TurnSnapshot>>({})
  // Optimistic user prompt shown while a steer/follow-up turn spins up.
  const [pendingPrompt, setPendingPrompt] = useState<{ text: string; afterId: string } | null>(null)
  // 1-click Publish dialog opened from Preview.
  const [publishSource, setPublishSource] = useState<Source | null>(null)
  const [publishRev, setPublishRev] = useState(0)
  const [naming, setNaming] = useState(false)
  const [deleting, setDeleting] = useState(false)
  const [name, setName] = useState('')
  const [copied, setCopied] = useState('')
  const [unfolded, setUnfolded] = useState(false)
  const [outputStyle, setOutputStyle] = useKept<string>('hanzo.build.output.style', 'default')
  const [transcriptView, setTranscriptView] = useKept<TranscriptView>('hanzo.build.transcript.view', 'normal')
  const [envColumn, setEnvColumn] = useState(false)
  const [splitPanes, setSplitPanes] = useState<string[]>([])
  const [queued, setQueued] = useState<QueuedMessage[]>([])
  const [keepAwake, setKeepAwake] = useKeepAwake()
  const [nowMs, setNowMs] = useState(() => Date.now())
  // The model and effort a session started here uses: the one choice Chat and New share.
  const { limits } = useLimits(signed ? (signal) => readLimits(t, signal) : null, t)
  const mind = useMind(t, (limits as { plan?: string } | null)?.plan, host.topUp)
  const voice = useVoice({ speech: useEar(t), language: useSpoken(), ...useDictation(draft, setDraft) })
  const tools = useCarry(t, activeId, (tab) => host.go(path({ kind: 'customize', tab })), {
    draft,
    setDraft,
    attach: async (attachments) => {
      const text = attachments.filter((file) => !file.type.startsWith('image/'))
      const result = await readFiles(files, text, say)
      setFiles(result.files)
      setNote(result.note || (text.length !== attachments.length ? say('Attach text files in Dev. Images are supported in Chat.') : ''))
    },
    go: (href) => href.startsWith('http') ? host.open(href) : host.go(href),
  })
  const plugins = {
    ...tools.plugins,
    rows: tools.plugins.rows.filter((row) => row.id !== 'sketch'),
  }
  const listen = useMemo(() => speaker(t), [t])

  // Load any earlier turns in this thread chain that are not yet cached in memory.
  const earlierIds = useMemo(() => chain.slice(0, -1), [chain])
  useEffect(() => {
    if (!signed || earlierIds.length === 0) return
    const missing = earlierIds.filter((eid) => !historyTurns[eid])
    if (missing.length === 0) return
    let cancelled = false
    void Promise.all(
      missing.map(async (eid) => {
        try {
          const d = await get(t, eid, { limit: 500 })
          return [eid, { events: d.recent, title: d.title, repo: d.repo, mode: d.mode }] as const
        } catch {
          return null
        }
      }),
    ).then((entries) => {
      if (cancelled) return
      const patch: Record<string, TurnSnapshot> = {}
      for (const entry of entries) if (entry) patch[entry[0]] = entry[1]
      if (Object.keys(patch).length) setHistoryTurns((prev) => ({ ...prev, ...patch }))
    })
    return () => {
      cancelled = true
    }
  }, [signed, t, earlierIds, historyTurns])

  // Clear the optimistic prompt once the new follow-up turn starts narrating its own cards.
  useEffect(() => {
    if (pendingPrompt && activeId !== pendingPrompt.afterId && events.length > 0) {
      setPendingPrompt(null)
    }
  }, [pendingPrompt, activeId, events.length])

  const mode = record?.mode ?? ''
  // Build unified single-thread cards chronologically across earlier turns + current active turn.
  const shown = useMemo((): Card[] => {
    const buildTurn = (turnId: string, evs: Event[], turnMode: string, turnTitle: string, turnRepo: string): Card[] => {
      const drawn = cards(evs, turnMode, say)
      const ask = headline(turnTitle, turnRepo)
      const opens = drawn[0]?.kind === 'said' && drawn[0].who === 'person'
      const withAsk = ask && drawn.length && !opens ? [{ kind: 'said' as const, key: 'ask', who: 'person' as const, text: ask }, ...drawn] : drawn
      return turnId === activeId && earlierIds.length === 0
        ? withAsk
        : withAsk.map((c) => ({ ...c, key: `${turnId}:${c.key}` }))
    }
    const merged: Card[] = []
    for (const eid of earlierIds) {
      const snap = historyTurns[eid]
      if (!snap) continue
      merged.push(...buildTurn(eid, snap.events, snap.mode, snap.title, snap.repo))
    }
    merged.push(...buildTurn(activeId, events, mode, record?.title ?? '', record?.repo ?? ''))
    if (pendingPrompt && (activeId === pendingPrompt.afterId || events.length === 0)) {
      const alreadyShown = merged.some((c) => c.kind === 'said' && c.who === 'person' && c.text.trim() === pendingPrompt.text.trim())
      if (!alreadyShown) {
        merged.push({ kind: 'said', key: `pending:${pendingPrompt.afterId}`, who: 'person', text: pendingPrompt.text })
      }
    }
    return merged
  }, [earlierIds, historyTurns, activeId, events, mode, record, say, pendingPrompt])
  const end = outcome(events)
  const plan = useMemo(() => steps(events), [events])
  const kept = useMemo(() => settled(events), [events])
  const next = useMemo(() => followed(events), [events])
  const state = resolveSessionState(status, end.status)
  // Where the session's work is published: its project's own address, read again when
  // the session says it went live, never an address an event names.
  const built = useProjects(t, signed && Boolean(record?.project))
  const rebuilt = built.reload
  useEffect(() => {
    if (end.published) rebuilt()
  }, [end.published, rebuilt])
  // What a session publishes opens in the panel: the Preview, chosen and shown.
  useEffect(() => {
    if (!end.published) return
    side.open({ kind: 'preview', title: say('Preview') })
    setDesk(true)
    // On each publish, not on each render.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [end.published])
  const proj = built.value.find((p) => p.slug === record?.project)
  const site = proj?.live ?? ''
  const pr = pull(record?.pr ?? '', record?.repo ?? '')
  const live = isSessionLive(state)
  const running = signed && live && !detail.error
  const active = running && isSessionActive(state)
  const paused = running && state === 'paused'
  const finished = signed && !live && !detail.error && Boolean(record)
  // A sandbox session is held by the coding service, which pauses it with its work
  // kept and has no resume of its own; a machine drains its commands itself.
  const held = (record?.environment || 'sandbox') === 'sandbox'
  // Words go to the session while it works, unless it is a paused sandbox session, which a follow-up turn carries on.
  const steering = running && !(paused && held)
  // A follow-up clones what the session kept, so it waits until the session has said where that is.
  const waiting = (paused && held && !kept.settled) || (finished && stopping && !kept.settled)
  const rootTitle = earlierIds.length ? historyTurns[earlierIds[0]!]?.title : ''
  const title = rootTitle || record?.title || (detail.loading ? '' : say('Untitled session'))
  // A setup session explores and installs before it answers, which takes minutes.
  const setup = mode === 'setup'
  const [seen, setSeen] = useKept('hanzo.build.setup.seen', false)
  // How long setup sessions have taken here: the ones this person can see, finished.
  const past = useRead(setup && running ? () => list(t, { kind: 'coding', status: 'done', limit: 500 }) : null, [], [t, setup, running])
  const span = took(past.value, 'setup')
  const earlier: Earlier | null = record
    ? {
        id: activeId,
        title: record.title || title,
        repo: record.repo,
        base: record.base,
        environment: record.environment,
        project: record.project,
        mode,
        pushed: kept.pushed,
      }
    : null
  const planned = mode === 'plan' && finished && (state === 'done' || state === 'completed') ? answer(events) : ''
  // A failed session is tried again with the person's words as it recorded them.
  const failed = finished && state === 'error'
  const asked = shown.find((c) => c.kind === 'said' && c.who === 'person')
  const again = asked && asked.kind === 'said' ? asked.text : ''
  // What the recorded read left out once initial pagination and live replay are merged.
  const hidden = Math.max(0, (detail.value?.events ?? 0) - events.length)
  // Only a session still working is on a step.
  const current = running ? plan.find((x) => !x.done) : undefined

  // Snapshot the active turn before appending a follow-up to the thread chain.
  const snapshotCurrent = () => {
    if (!activeId) return
    setHistoryTurns((prev) => ({
      ...prev,
      [activeId]: {
        events,
        title: record?.title ?? '',
        repo: record?.repo ?? '',
        mode,
      },
    }))
  }

  // Complete the reminder in this view, using the device only when it permits it.
  useEffect(() => {
    if (!notify || running) return
    setNotify(null)
    const title = record?.title || say('Session finished')
    const body = say('This session has finished.')
    setNote(body)
    if (notify !== 'device' || typeof Notification === 'undefined' || Notification.permission !== 'granted') return
    void (async () => {
      try {
        const worker = await navigator.serviceWorker?.getRegistration()
        if (worker) await worker.showNotification(title, { body })
        else new Notification(title, { body })
      } catch {
        // The result is already shown in the session.
      }
    })()
  }, [notify, running, record?.title, say])

  const ask = async () => {
    let allowed = false
    try {
      if (typeof Notification !== 'undefined') {
        const current = Notification.permission
        allowed = (current === 'default' ? await Notification.requestPermission() : current) === 'granted'
      }
    } catch {
      // Browser permission can be unavailable even when the API exists.
    }
    setNotify(allowed ? 'device' : 'in-app')
    setNote(allowed ? '' : say('We’ll show the result here. For a device alert, allow notifications for this site in your browser or device settings.'))
  }

  /** Something that answers a sentence for the note, or throws the reason. One at a time. */
  const act = async (work: () => Promise<string>) => {
    if (busy) return
    setBusy(true)
    setNote('')
    try {
      setNote(await work())
    } catch (e) {
      setNote(cleanErrorText((e as Error).message))
    } finally {
      setBusy(false)
    }
  }

  /**
   * A new turn's ask with the model and effort the composer shows, Enso named as
   * itself: a turn that names none takes the person's saved coding default
   * (apps/coding start.go), which is not what the composer shows.
   */
  const tuned = (a: Ask): Ask => ({ ...a, model: mind.model, effort: level(mind.effort) })

  // A setup session's work IS its environment: the panel opens on it.
  useEffect(() => {
    if (mode === 'setup') side.open({ kind: 'environment', title: say('Environment') })
    // Once the session says what it is.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [mode])

  /** An answer's artifact, open in the panel as a page rendered from its own bytes. */
  const look = (a: Artifact) => {
    side.open({ kind: PAGE, title: a.name, body: a.body, type: a.mime })
    setDesk(true)
  }

  /**
   * Words for the session while it works. A machine drains them between steps; a
   * sandbox session stops with its work kept and continues in a follow-up turn
   * inside this same thread (api/coding.ts `steer`).
   */
  const tell = async (words: string): Promise<string> => {
    if (held) {
      await steer(t, activeId, words)
      setSteered(activeId)
      setPendingPrompt({ text: words, afterId: activeId })
      return say('Redirecting session — keeping current work and continuing here')
    }
    await message(t, activeId, words)
    return say('Sent — recorded on this session')
  }

  /**
   * Continue this session in the SAME thread (or branch out when `fork` is true).
   * If the backend reports the active turn is still running, seamlessly steer it
   * instead of failing with an "already running" error.
   */
  const carry = async (said: string, how: 'follow' | 'approve' | 'retry' = 'follow', fork = false): Promise<string> => {
    if (!earlier) throw new Error(say('This session is still being read'))
    if (paused) await stop(t, activeId, 'Continued in this session').catch(() => undefined)
    // A session a conversation started is carried on by the conversation (talk.ts): the
    // words go there, in its session, and so does the reader.
    if (record?.parent && !fork) {
      const words = how === 'approve' ? `Carry out this plan:\n\n${said.trim()}` : said.trim() || 'Continue where the session left off.'
      // Approving the plan is going ahead: the conversation leaves Plan for Auto, so
      // the turn it starts builds.
      await talk(t, { session: record.parent, input: words, model: mind.model, effort: level(mind.effort), ...(how === 'approve' ? { mode: 'auto' as const } : {}) })
      setDraft('')
      setFiles([])
      host.go(record.parent)
      return ''
    }
    const ask = how === 'approve' ? approve(earlier, said) : how === 'retry' ? retry(earlier, said) : followUp(earlier, said)
    let run: Awaited<ReturnType<typeof start>>
    try {
      run = await start(t, tuned(ask))
    } catch (e) {
      // Race condition: backend says the turn is still running — steer smoothly in place instead of throwing.
      if (!fork && isAlreadyRunningError(e) && said.trim()) {
        const msg = await tell(said.trim())
        setDraft('')
        setFiles([])
        return msg
      }
      throw e
    }
    setDraft('')
    setFiles([])
    setStopping(false)
    if (fork) {
      host.go(run.session)
      return ''
    }
    snapshotCurrent()
    if (said.trim()) setPendingPrompt({ text: said.trim(), afterId: activeId })
    linkThread(activeId, run.session)
    return ''
  }

  /**
   * A press that starts a turn with no words typed (Try again, Approve and build, Resume on a sandbox session, Fork) asks
   * first, before anything says it is starting; words the person typed and sent are their own ask. A no starts
   * nothing and says nothing.
   */
  const ahead = async (said: string, how: 'approve' | 'retry' | 'resume' | 'fork') => {
    if (busy) return
    const asked: Record<typeof how, Omit<Question, 'kind' | 'uses'>> = {
      retry: { title: say('Try this session again?'), what: say('This starts a new turn with the same ask.'), yes: say('Try again') },
      approve: { title: say('Build this plan?'), what: say('This starts a build turn that carries out the plan.'), yes: say('Approve and build') },
      resume: { title: say('Resume this session?'), what: say('This continues the session from its branch.'), yes: say('Resume') },
      fork: { title: say('Fork this session?'), what: say('This starts a separate session from this one’s work, with the same ask.'), yes: say('Fork') },
    }
    if (!(await spend.ask({ kind: 'run', uses: held ? 'computer' : 'credits', ...asked[how] }))) return
    await act(() => carry(said, how === 'resume' || how === 'fork' ? 'follow' : how, how === 'fork'))
  }

  // When a steered turn hands off (`followed` status event with `next`), continue inside the same thread.
  useEffect(() => {
    if (!next || next === activeId) return
    if (steered === activeId || !chain.includes(next)) {
      snapshotCurrent()
      linkThread(activeId, next)
      if (steered === activeId) setSteered('')
    }
    // On the follow-up's arrival.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [steered, activeId, next])

  // The composer sends words, with any files fenced after them, and only while it is enabled.
  const send = () => {
    const words = compose([tools.text, draft.trim()].filter(Boolean).join('\n\n'), files)
    if (!words) return
    if (steering) {
      void act(async () => {
        const said = await tell(words)
        setDraft('')
        setFiles([])
        return said
      })
      return
    }
    void act(() => carry(words))
  }

  const command = (c: Command) => {
    switch (c) {
      case 'stop':
        void act(async () => {
          await stop(t, activeId)
          setStopping(true)
          return say('Stop requested — the session keeps its work on its branch')
        })
        return
      case 'pause':
        void act(async () => {
          await pause(t, activeId)
          return held ? say('Pause requested — the session keeps its work on its branch and waits') : say('Pause asked — the machine pauses when it reads it')
        })
        return
      case 'resume':
        if (held) void ahead('', 'resume')
        else
          void act(async () => {
            await resume(t, activeId)
            return say('Resume asked — the machine goes on when it reads it')
          })
        return
    }
  }

  const copy = async (what: string, value: string) => {
    if (await toClipboard(value)) setCopied(what)
    else setNote(say('This browser would not copy.'))
  }

  const share = (on: boolean) =>
    act(async () => {
      await publish(t, activeId, on)
      detail.reload()
      return on ? say('Shared — anyone with the link can read this session’s story') : say('No longer shared')
    })

  // What the title's menu does to the session.
  const manage: MenuItems = [
    ...(signed && record
      ? [
          {
            key: 'rename',
            label: say('Rename'),
            icon: <Pencil size={16} />,
            onSelect: () => {
              setName(title)
              setNaming(true)
            },
          },
        ]
      : []),
    { key: 'copy', label: copied === 'id' ? say('Copied') : say('Copy session id'), icon: <Copy size={16} />, description: activeId, onSelect: () => void copy('id', activeId) },
    // What this run was asked, as a task that runs again on a schedule: the ask it opened with.
    ...(signed && record
      ? [
          {
            key: 'schedule',
            label: say('Schedule this'),
            icon: <Clock size={16} />,
            description: say('Run it again on a schedule'),
            onSelect: () => {
              const first = shown.find((c) => c.kind === 'said' && c.who === 'person')
              schedule(first && 'text' in first ? first.text : headline(record.title, record.repo), host.go)
            },
          },
        ]
      : []),
  ]
  // Sharing opens the public build route, which is addressed by the org and the project a run built.
  const link = record?.published ? story(t, record.org || host.org, record.project) : ''
  const sharing: MenuItems =
    signed && record?.project
      ? [
          record.published
            ? { key: 'unshare', label: say('Stop sharing'), icon: <Globe size={16} />, onSelect: () => void share(false) }
            : { key: 'share', label: say('Share publicly'), icon: <Globe size={16} />, description: say('Anyone with the link can read it'), onSelect: () => void share(true) },
          ...(link ? [{ key: 'link', label: copied === 'link' ? say('Copied') : say('Copy public link'), icon: <Copy size={16} />, description: link, onSelect: () => void copy('link', link) }] : []),
        ]
      : []

  const sendQueued = (qid: string) => {
    const item = queued.find((q) => q.id === qid)
    if (!item) return
    setQueued((prev) => prev.filter((q) => q.id !== qid))
    if (steering) {
      void act(() => tell(item.text))
      return
    }
    void act(() => carry(item.text))
  }

  const enqueue = () => {
    const words = compose([tools.text, draft.trim()].filter(Boolean).join('\n\n'), files)
    if (!words) return
    setQueued((prev) => [...prev, { id: `q-${Date.now()}-${prev.length}`, text: words }])
    setDraft('')
    setFiles([])
  }

  useEffect(() => {
    if (!running || paused) return
    const timer = setInterval(() => setNowMs(Date.now()), 1000)
    return () => clearInterval(timer)
  }, [running, paused])

  const toggleDockPane = (kind: 'terminal' | 'preview' | 'diff' | 'files', label: string) => {
    if (!desk) setDesk(true)
    setSplitPanes((prev) => {
      if (prev.includes(kind)) {
        const next = prev.filter((k) => k !== kind)
        if (next.length === 1) {
          const single = next[0]!
          side.open({ kind: single, title: say(single.charAt(0).toUpperCase() + single.slice(1)) })
        }
        return next
      }
      const activeKind = side.tabs.find((x) => x.id === side.at)?.kind ?? ''
      const base = prev.length === 0 && activeKind && ['terminal', 'preview', 'diff', 'files'].includes(activeKind) && activeKind !== kind ? [activeKind] : prev
      const next = [...base, kind]
      if (next.length === 1) side.open({ kind, title: label })
      return next
    })
  }

  const elapsedSec = useMemo(() => {
    const startMs = record?.createdAt ? Date.parse(record.createdAt) : NaN
    if (!Number.isFinite(startMs)) return 0
    return Math.max(1, Math.floor((nowMs - startMs) / 1000))
  }, [record?.createdAt, nowMs])

  const elapsedLabel = useMemo(() => {
    const mins = Math.floor(elapsedSec / 60)
    const secs = elapsedSec % 60
    return mins > 0 ? `${mins}m ${secs}s` : `${secs}s`
  }, [elapsedSec])

  const tokenLabel = useMemo(() => {
    const chars = events.reduce((sum, e) => sum + JSON.stringify(e.payload ?? '').length, 0)
    const tok = Math.max(100, Math.round(chars / 4))
    return tok >= 1000 ? `${(tok / 1000).toFixed(1)}k tokens` : `${tok} tokens`
  }, [events])

  const bgTaskCount = useMemo(
    () => shown.filter((c) => (c.kind === 'shell' || c.kind === 'step') && c.ran === 'running' && running).length,
    [shown, running],
  )

  const save = () =>
    void act(async () => {
      await rename(t, activeId, name)
      setNaming(false)
      detail.reload()
      return say('Renamed')
    })

  const placeholder = !signed
    ? say('Sign in to follow up')
    : waiting
      ? paused
        ? say('Pausing — the session is keeping its work…')
        : say('Stopping — the session is keeping its work…')
      : paused && held
        ? say('Continue this session with a follow up')
        : running
          ? setup
            ? say('Add a follow up for the setup agent')
            : say('Steer or redirect this session…')
          : finished
            ? record?.parent
              ? say('Follow up — continues the conversation')
              : say('Follow up — continues in this session')
            : say('Reading this session…')

  const openPublish =
    proj?.repo || record?.repo
      ? () => {
          const clone = proj?.repo || (record?.repo ? `https://github.com/${record.repo}.git` : '')
          if (!clone) return
          const slug = proj?.slug || record?.project || record?.repo.split('/').pop() || 'app'
          setPublishSource({
            repo: clone,
            title: proj?.name || record?.repo || title,
            ref: record?.branch || proj?.branch || record?.base || 'main',
            name: slug,
            project: slug,
            preview: site || undefined,
          })
        }
      : undefined

  const desk_ = (sheet: boolean, inset = !sheet) => (
    <Desk
      id={activeId}
      repo={record?.repo ?? ''}
      branch={record?.branch ?? ''}
      base={record?.base ?? ''}
      environment={record?.environment ?? ''}
      mode={mode}
      pr={pr}
      title={title}
      project={record?.project ?? ''}
      sandbox={record?.sandbox ?? ''}
      site={site}
      published={end.published + publishRev}
      sessionStatus={state}
      events={events}
      live={running}
      menu={[...manage, ...sharing]}
      refused={signed ? refused || (detail.error ? say(detail.error.message) : '') : say('Sign in to follow this session.')}
      retry={signed ? say('Retry') : say('Sign in')}
      onRetry={signed ? detail.reload : () => host.signIn?.()}
      side={side}
      sheet={sheet}
      topRightInset={inset}
      onPublish={openPublish}
      onHide={() => setDesk(false)}
      envColumn={envColumn}
      splitPanes={splitPanes}
      onCloseSplitPane={(pid) => setSplitPanes((prev) => prev.filter((k) => k !== pid))}
      onExpandSplitPane={(pid) => {
        setSplitPanes([])
        side.open({ kind: pid, title: say(pid.charAt(0).toUpperCase() + pid.slice(1)) })
      }}
    />
  )

  const triggerButton = (
    <XStack
      render="button"
      aria-label={say('Manage this session')}
      items="center"
      gap="$1.5"
      maxW="100%"
      minW={0}
      px="$1.5"
      mx="$-1.5"
      py="$0.5"
      minH={32}
      $max-md={{ minH: 40 }}
      rounded="$2"
      hoverStyle={{ bg: '$hover' }}
      focusVisibleStyle={RING}
    >
      <SizableText render="h1" size="$5" color="$ink" numberOfLines={1} minW={0}>
        {title}
      </SizableText>
      <ChevronDown size={14} opacity={0.7} />
    </XStack>
  )

  const mobileWorkspaceOpen = !wide && desk

  return (
    <XStack data-slot="run-split" display="grid" gridTemplateColumns={wide && desk ? 'minmax(0, 1fr) auto' : 'minmax(0, 1fr)'} width="100%" height="100%" minH={0} minW={0} overflow="hidden">
      <YStack
        data-slot="run"
        ref={split.main}
        {...pane}
        display="grid"
        gridTemplateRows={!wide ? (mobileWorkspaceOpen ? 'auto auto minmax(0, 1fr)' : 'auto auto minmax(0, 1fr) auto') : 'auto minmax(0, 1fr) auto'}
        width="100%"
        height="100%"
        minH={0}
        minW={0}
        overflow="hidden"
      >
        <XStack
          data-slot="run-header"
          px="$4"
          $max-md={{ px: '$3' }}
          pt="$3"
          pb="$2"
          gap="$1.5"
          items="center"
          minH={44}
          minW={0}
          style={!desk || !wide ? { paddingInlineEnd: 'calc(16px + var(--hanzo-shell-top-right-inset, 0px))' } : undefined}
        >
          {spend.dialog}
          <YStack flex={1} minW={0} items="flex-start">
            <XStack items="center" gap="$1" maxW="100%" minW={0}>
              {signed && record ? (
                <DropdownMenu>
                  <DropdownMenuTrigger asChild>{triggerButton}</DropdownMenuTrigger>
                  <DropdownMenuContent align="start" minW={210}>
                    <DropdownMenuSub>
                      <DropdownMenuSubTrigger>
                        <SizableText size="$2" color="$ink" flex={1}>
                          {say('Open in')}
                        </SizableText>
                      </DropdownMenuSubTrigger>
                      <DropdownMenuSubContent minW={170}>
                        <DropdownMenuItem
                          onSelect={() => {
                            window.open(`vscode://file/home/z/work/${record.repo.split('/').pop() || ''}`, '_blank')
                          }}
                        >
                          <SizableText size="$2" color="$ink">
                            VS Code
                          </SizableText>
                        </DropdownMenuItem>
                        <DropdownMenuItem
                          onSelect={() => {
                            window.open(`cursor://file/home/z/work/${record.repo.split('/').pop() || ''}`, '_blank')
                          }}
                        >
                          <SizableText size="$2" color="$ink">
                            Cursor
                          </SizableText>
                        </DropdownMenuItem>
                        <DropdownMenuItem
                          onSelect={() => {
                            side.open({ kind: 'terminal', title: say(devMode ? 'Terminal' : 'Activity') })
                            setDesk(true)
                          }}
                        >
                          <SizableText size="$2" color="$ink">
                            {say(devMode ? 'Terminal' : 'Activity')}
                          </SizableText>
                        </DropdownMenuItem>
                        {record.repo ? (
                          <DropdownMenuItem
                            onSelect={() => {
                              window.open(pr.href || `https://github.com/${record.repo}`, '_blank', 'noopener,noreferrer')
                            }}
                          >
                            <SizableText size="$2" color="$ink">
                              GitHub
                            </SizableText>
                          </DropdownMenuItem>
                        ) : null}
                        <DropdownMenuItem onSelect={() => void copy('path', record.repo || id)}>
                          <SizableText size="$2" color="$ink">
                            {say('Copy path')}
                          </SizableText>
                        </DropdownMenuItem>
                      </DropdownMenuSubContent>
                    </DropdownMenuSub>

                    <DropdownMenuSeparator />

                    <DropdownMenuItem
                      aria-label={say('Rename')}
                      onSelect={() => {
                        setName(title)
                        setNaming(true)
                      }}
                    >
                      <SizableText size="$2" color="$ink" flex={1}>
                        {say('Rename')}
                      </SizableText>
                      <DropdownMenuShortcut>R</DropdownMenuShortcut>
                    </DropdownMenuItem>

                    <DropdownMenuItem
                      aria-label={say('Fork')}
                      onSelect={() => {
                        void ahead(again || headline(record.title, record.repo), 'fork')
                      }}
                    >
                      <SizableText size="$2" color="$ink" flex={1}>
                        {say('Fork')}
                      </SizableText>
                      <DropdownMenuShortcut>F</DropdownMenuShortcut>
                    </DropdownMenuItem>

                    <DropdownMenuSeparator />

                    <DropdownMenuSub>
                      <DropdownMenuSubTrigger>
                        <SizableText size="$2" color="$ink" flex={1}>
                          {say('Transcript view')}
                        </SizableText>
                      </DropdownMenuSubTrigger>
                      <DropdownMenuSubContent minW={160}>
                        {(
                          [
                            ['normal', say('Normal')],
                            ['thinking', say('Thinking')],
                            ['verbose', say('Verbose')],
                          ] as const
                        ).map(([val, label]) => (
                          <DropdownMenuItem key={val} onSelect={() => setTranscriptView(val)}>
                            <SizableText size="$2" color="$ink" flex={1}>
                              {label}
                            </SizableText>
                            {transcriptView === val ? <Check size={14} color="$blue10" /> : null}
                          </DropdownMenuItem>
                        ))}
                      </DropdownMenuSubContent>
                    </DropdownMenuSub>

                    <DropdownMenuSub>
                      <DropdownMenuSubTrigger>
                        <SizableText size="$2" color="$ink" flex={1}>
                          {say('Output style')}
                        </SizableText>
                      </DropdownMenuSubTrigger>
                      <DropdownMenuSubContent minW={170}>
                        {(
                          [
                            ['default', say('Default')],
                            ['proactive', say('Proactive')],
                            ['concise', say('Concise')],
                            ['explanatory', say('Explanatory')],
                            ['learning', say('Learning')],
                          ] as const
                        ).map(([val, label]) => (
                          <DropdownMenuItem key={val} onSelect={() => setOutputStyle(val)}>
                            <SizableText size="$2" color="$ink" flex={1}>
                              {label}
                            </SizableText>
                            {outputStyle === val ? <Check size={14} color="$blue10" /> : null}
                          </DropdownMenuItem>
                        ))}
                        <DropdownMenuSeparator />
                        <DropdownMenuItem onSelect={() => setOutputStyle('custom')}>
                          <SizableText size="$2" color="$ink" flex={1}>
                            {say('New style...')}
                          </SizableText>
                        </DropdownMenuItem>
                      </DropdownMenuSubContent>
                    </DropdownMenuSub>

                    {/* The run's own menu holds the switch too: off, the run reads as its result (hanzo-inc/app issue 321). */}
                    {/* Drawn as this menu's other choices are (Transcript view): the words, then a check while it is on. */}
                    <DropdownMenuItem aria-label={say('Developer mode')} aria-checked={devMode} onSelect={() => setDevMode(!devMode)}>
                      <YStack flex={1} minW={0}>
                        <SizableText size="$2" color="$ink">
                          {say('Developer mode')}
                        </SizableText>
                        <SizableText size="$1" color="$soft">
                          {say('Show the terminal, commands and each step')}
                        </SizableText>
                      </YStack>
                      {devMode ? <Check size={14} color="$blue10" /> : null}
                    </DropdownMenuItem>

                    <DropdownMenuSeparator />

                    <DropdownMenuItem onSelect={() => void copy('id', activeId)}>
                      <SizableText size="$2" color="$ink" flex={1}>
                        {copied === 'id' ? say('Copied') : say('Copy session id')}
                      </SizableText>
                    </DropdownMenuItem>

                    <DropdownMenuItem
                      onSelect={() => {
                        const first = shown.find((c) => c.kind === 'said' && c.who === 'person')
                        schedule(first && 'text' in first ? first.text : headline(record.title, record.repo), host.go)
                      }}
                    >
                      <SizableText size="$2" color="$ink" flex={1}>
                        {say('Schedule this')}
                      </SizableText>
                    </DropdownMenuItem>

                    <DropdownMenuSeparator />

                    <DropdownMenuItem
                      aria-label={say('Archive')}
                      onSelect={() => {
                        // Kept out of every list of sessions (the sidebar's too, `runmeta.ts`) and still readable under Archived.
                        for (const sid of chain) mark(sid, { archived: true })
                        if (running) void stop(t, activeId, 'Archived').catch(() => undefined)
                        host.go('')
                      }}
                    >
                      <SizableText size="$2" color="$ink" flex={1}>
                        {say('Archive')}
                      </SizableText>
                      <DropdownMenuShortcut>A</DropdownMenuShortcut>
                    </DropdownMenuItem>

                    <DropdownMenuItem
                      aria-label={say('Delete')}
                      variant="destructive"
                      onSelect={() => setDeleting(true)}
                    >
                      <SizableText size="$2" color="$red10" flex={1}>
                        {say('Delete')}
                      </SizableText>
                      <DropdownMenuShortcut>D</DropdownMenuShortcut>
                    </DropdownMenuItem>
                  </DropdownMenuContent>
                </DropdownMenu>
              ) : (
                <DropdownMenu trigger={triggerButton} items={manage} />
              )}
            </XStack>
            <SizableText size="$1" color="$soft" numberOfLines={1} maxW="100%">
              {[state ? say(STATUS[state] ?? state) : detail.loading ? say('reading') : '', record?.repo, devMode ? record?.branch : '', record?.published ? say('shared') : ''].filter(Boolean).join(' · ')}
            </SizableText>
          </YStack>
          {pr.href ? (
            // From the run's record, which the coding service writes; `pull` draws only a pull request address.
            <Out href={pr.href} label={say('Open pull request {pr}', { pr: pr.label })}>
              <XStack items="center" gap="$1.5" px="$2.5" py="$1.5" rounded="$3" borderWidth={1} borderColor="$borderColor" hoverStyle={{ bg: '$hover' }}>
                <GitPullRequest size={14} />
                <SizableText size="$2" color="$ink">
                  {pr.label}
                </SizableText>
                <ExternalLink size={12} opacity={0.6} />
              </XStack>
            </Out>
          ) : null}
          {sharing.length ? (
            <DropdownMenu
              trigger={
                <XStack render="button" aria-label={say('Share')} items="center" gap="$1.5" px="$2.5" py="$1.5" rounded="$3" borderWidth={1} borderColor="$borderColor" hoverStyle={{ bg: '$hover' }}>
                  <Share size={14} />
                  <SizableText size="$2" color="$ink">
                    {say('Share')}
                  </SizableText>
                </XStack>
              }
              items={sharing}
            />
          ) : null}
          {wide ? (
            <XStack items="center" gap="$0.5">
              <XStack
                render="button"
                aria-label={say('Toggle environment column')}
                {...({ title: say('Toggle environment column') } as object)}
                aria-pressed={envColumn}
                onPress={() => {
                  if (!desk) setDesk(true)
                  setEnvColumn((v) => !v)
                }}
                minW={32}
                minH={32}
                items="center"
                justify="center"
                rounded="$2"
                bg={envColumn ? '$hover' : 'transparent'}
                hoverStyle={{ bg: '$hover' }}
                focusVisibleStyle={RING}
              >
                <SlidersHorizontal size={15} opacity={envColumn ? 1 : 0.7} />
              </XStack>
              <XStack
                render="button"
                aria-label={say(devMode ? 'Toggle terminal pane' : 'Toggle activity pane')}
                {...({ title: say(devMode ? 'Toggle terminal pane' : 'Toggle activity pane') } as object)}
                aria-pressed={splitPanes.includes('terminal')}
                onPress={() => toggleDockPane('terminal', say(devMode ? 'Terminal' : 'Activity'))}
                minW={32}
                minH={32}
                items="center"
                justify="center"
                rounded="$2"
                bg={splitPanes.includes('terminal') ? '$hover' : 'transparent'}
                hoverStyle={{ bg: '$hover' }}
                focusVisibleStyle={RING}
              >
                <SquareTerminal size={15} opacity={splitPanes.includes('terminal') ? 1 : 0.7} />
              </XStack>
              <XStack
                render="button"
                aria-label={say('Toggle diff pane')}
                {...({ title: say('Toggle diff pane') } as object)}
                aria-pressed={splitPanes.includes('diff')}
                onClick={() => toggleDockPane('diff', say('Diff'))}
                minW={32}
                minH={32}
                items="center"
                justify="center"
                rounded="$2"
                bg={splitPanes.includes('diff') ? '$hover' : 'transparent'}
                hoverStyle={{ bg: '$hover' }}
                focusVisibleStyle={RING}
              >
                <FileDiff size={15} opacity={splitPanes.includes('diff') ? 1 : 0.7} />
              </XStack>
              <XStack
                render="button"
                aria-label={say('Toggle browser pane')}
                {...({ title: say('Toggle browser pane') } as object)}
                aria-pressed={splitPanes.includes('preview')}
                onClick={() => toggleDockPane('preview', say('Preview'))}
                minW={32}
                minH={32}
                items="center"
                justify="center"
                rounded="$2"
                bg={splitPanes.includes('preview') ? '$hover' : 'transparent'}
                hoverStyle={{ bg: '$hover' }}
                focusVisibleStyle={RING}
              >
                <Globe size={15} opacity={splitPanes.includes('preview') ? 1 : 0.7} />
              </XStack>
              <XStack
                render="button"
                aria-label={say('Toggle files pane')}
                {...({ title: say('Toggle files pane') } as object)}
                aria-pressed={splitPanes.includes('files')}
                onClick={() => toggleDockPane('files', say('Files'))}
                minW={32}
                minH={32}
                items="center"
                justify="center"
                rounded="$2"
                bg={splitPanes.includes('files') ? '$hover' : 'transparent'}
                hoverStyle={{ bg: '$hover' }}
                focusVisibleStyle={RING}
              >
                <Folder size={15} opacity={splitPanes.includes('files') ? 1 : 0.7} />
              </XStack>
              <DropdownMenu>
                <DropdownMenuTrigger asChild>
                  <XStack
                    render="button"
                    aria-label={say('More workspace options')}
                    {...({ title: say('More workspace options') } as object)}
                    p="$1.5"
                    rounded="$2"
                    hoverStyle={{ bg: '$hover' }}
                  >
                    <MoreHorizontal size={15} opacity={0.7} />
                  </XStack>
                </DropdownMenuTrigger>
                <DropdownMenuContent align="end" minW={220}>
                  <DropdownMenuItem
                    onSelect={() => {
                      if (!desk) setDesk(true)
                      setEnvColumn((v) => !v)
                    }}
                  >
                    <MessageSquare size={15} />
                    <SizableText size="$2" color="$ink" flex={1}>
                      {say('Side chat')}
                    </SizableText>
                    <DropdownMenuShortcut>Ctrl+;</DropdownMenuShortcut>
                  </DropdownMenuItem>
                  <DropdownMenuItem
                    onSelect={() => {
                      if (!desk) setDesk(true)
                      setEnvColumn(true)
                    }}
                  >
                    <Layers size={15} />
                    <SizableText size="$2" color="$ink" flex={1}>
                      {say('Background tasks')}
                    </SizableText>
                    <XStack px="$1.5" py="$0.5" rounded={99} bg="$hover">
                      <SizableText size="$1" color="$soft">
                        {String(bgTaskCount)}
                      </SizableText>
                    </XStack>
                  </DropdownMenuItem>
                  <DropdownMenuSeparator />
                  <KeepAwake on={keepAwake} onToggle={(on) => void setKeepAwake(on)} />
                </DropdownMenuContent>
              </DropdownMenu>
            </XStack>
          ) : null}
          {wide ? <PanelToggle open={desk} onToggle={() => setDesk(!desk)} /> : null}
        </XStack>

        {!wide ? (
          <XStack
            data-slot="mobile-pane-switcher"
            role="tablist"
            aria-label={say('View')}
            px="$3"
            pb="$2"
            gap="$1.5"
            items="center"
            shrink={0}
            minW={0}
          >
            <XStack flex={1} minW={0} p={3} gap={3} rounded="$3" bg="$hover">
              <XStack
                render="button"
                role="tab"
                aria-selected={!desk}
                onPress={() => setDesk(false)}
                flex={1}
                minH={40}
                items="center"
                justify="center"
                rounded="$2"
                bg={!desk ? '$raised' : 'transparent'}
                focusVisibleStyle={RING}
              >
                <SizableText size="$2" fontWeight={!desk ? '600' : '400'} color={!desk ? '$ink' : '$soft'}>
                  {say('Session')}
                </SizableText>
              </XStack>
              <XStack
                render="button"
                role="tab"
                aria-selected={desk}
                onPress={() => setDesk(true)}
                flex={1}
                minH={40}
                items="center"
                justify="center"
                rounded="$2"
                bg={desk ? '$raised' : 'transparent'}
                focusVisibleStyle={RING}
              >
                <SizableText size="$2" fontWeight={desk ? '600' : '400'} color={desk ? '$ink' : '$soft'}>
                  {say('Workspace')}
                </SizableText>
              </XStack>
            </XStack>
          </XStack>
        ) : null}

        {mobileWorkspaceOpen ? (
          <YStack data-slot="mobile-workspace" flex={1} minH={0} minW={0} overflow="hidden" borderTopWidth={1} borderColor="$borderColor">
            {desk_(false, false)}
          </YStack>
        ) : (
          <>
            <Transcript
              cards={shown}
              devMode={devMode}
              view={transcriptView}
              queued={queued}
              onSendQueued={sendQueued}
              onOpen={look}
              onDiff={() => {
                side.open({ kind: 'diff', title: say('Diff') })
                setDesk(true)
              }}
              listen={signed ? listen : undefined}
              end={
                failed ? (
                  <Failure onRetry={busy ? undefined : () => void ahead(again, 'retry')}>
                    {end.problem
                      ? say('This session stopped before it finished.')
                      : // Only a session that pushed has work on its branch (turn.ts settled): one that failed before it
                        // changed anything left no branch, and Try again starts over from where it began.
                        kept.pushed
                        ? `${say('This session stopped with an error.')} ${say('Its work so far is on its branch.')}`
                        : `${say('This session stopped with an error.')} ${say('It saved nothing to its branch, so Try again starts over.')}`}
                  </Failure>
                ) : null
              }
              // A paused session's agent was stopped where it stood; what it was running is cut off.
              live={running && !paused}
              header={
                hidden ? (
                  <SizableText size="$1" color="$soft">
                    {`${hidden === 1 ? say('{n} earlier turn is not shown.', { n: hidden }) : say('{n} earlier turns are not shown.', { n: hidden })} ${say('The session’s Git tab has everything it pushed.')}`}
                  </SizableText>
                ) : null
              }
              onApprove={planned && earlier ? (p) => void ahead(p, 'approve') : undefined}
              // What dev asks its person while it works (its ask tool), answered here.
              onAnswer={signed ? (q, label) => void act(async () => (await reply(t, activeId, q, label, true), '')) : undefined}
              approving={busy}
              onVerdict={(v) => verdict(t, activeId, record?.project ?? '', v)}
              onAddFunds={() => (host.topUp ? host.topUp() : host.go(path({ kind: 'settings', section: 'billing' })))}
              empty={
                signed ? (
                  <SizableText size="$2" color="$soft">
                    {detail.error
                      ? say(detail.error.message)
                      : detail.loading
                        ? say('Reading this session…')
                        : running
                          ? setup
                            ? say('Setting up environment')
                            : say('Starting…')
                          : say('Waiting for the session’s first step…')}
                  </SizableText>
                ) : (
                  <YStack gap="$3" items="flex-start">
                    <SizableText size="$2" color="$soft">
                      {say('Sign in to follow this session.')}
                    </SizableText>
                    <Button size="sm" onPress={() => host.signIn?.()}>
                      {say('Sign in')}
                    </Button>
                  </YStack>
                )
              }
            />

            <YStack pb="$3" pt="$2" gap="$2">
              {/* At the text's width, like the composer under it. */}
              <YStack {...measure} gap="$2">
                {end.problem ? (
                  <SizableText size="$1" color="$soft">
                    {say('The branch is pushed, and the pull request could not be opened.')}
                  </SizableText>
                ) : null}
                {note || (signed && refused) ? (
                  <SizableText size="$1" color="$soft" role="status">
                    {cleanErrorText(note || refused)}
                  </SizableText>
                ) : null}
                {plan.length && devMode ? (
                  // One line until it is opened, so the steps never crowd the transcript. Off Developer mode the
                  // transcript's own Working line says where the session is, in words, and opens onto the same steps.
                  <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" overflow="hidden">
                    <XStack
                      render="button"
                      aria-label={say('Steps · {n}', { n: plan.length })}
                      aria-expanded={unfolded}
                      onClick={() => setUnfolded(!unfolded)}
                      width="100%"
                      px="$3"
                      py="$1.5"
                      minH={36}
                      gap="$2"
                      items="center"
                      hoverStyle={{ bg: '$hover' }}
                      focusVisibleStyle={RING_INSET}
                    >
                      {unfolded ? <ChevronDown size={14} /> : <ChevronRight size={14} />}
                      <SizableText size="$2" color="$ink">
                        {say('Steps · {n}', { n: plan.length })}
                      </SizableText>
                      {current && !unfolded ? (
                        <SizableText flex={1} minW={0} size="$1" color="$soft" numberOfLines={1} text="end">
                          {current.name}
                        </SizableText>
                      ) : null}
                    </XStack>
                    {unfolded
                      ? plan.map((s) => {
                          const on = s === current
                          return (
                            <XStack key={s.name} items="center" gap="$2" px="$3" py="$1.5" borderTopWidth={1} borderColor="$borderColor">
                              <SizableText size="$2" color={s.done || on ? '$ink' : '$soft'}>
                                {s.done ? '✓' : on ? '●' : '○'}
                              </SizableText>
                              <SizableText size="$2" color={s.done ? '$soft' : '$ink'} numberOfLines={1}>
                                {s.name}
                              </SizableText>
                            </XStack>
                          )
                        })
                      : null}
                  </YStack>
                ) : null}
                {setup && running && !seen ? <Onboarding onDone={() => setSeen(true)} /> : null}
                {running && !paused ? (
                  <XStack items="center" gap="$2" px="$1">
                    <YStack width={6} height={6} rounded={99} bg="var(--state-online)" shrink={0} />
                    <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
                      {devMode ? `${elapsedLabel} · ${tokenLabel} · ${say('Thinking')}` : `${elapsedLabel} · ${say('Working…')}`}
                    </SizableText>
                  </XStack>
                ) : null}
                {running ? (
                  <XStack items="center" gap="$2" px="$3" py="$2" rounded="$10" borderWidth={1} borderColor="$borderColor" flexWrap="wrap" rowGap="$1">
                    <SizableText size="$2" color="$ink" flex={1} minW={160}>
                      {paused
                        ? kept.pushed
                          ? say('Paused — its work so far is on its branch.')
                          : say('Paused.')
                        : setup
                          ? span
                            ? say('Environment setup takes ~{minutes} minutes.', { minutes: span[0] === span[1] ? span[0] : `${span[0]}–${span[1]}` })
                            : say('Environment setup takes several minutes.')
                          : say('This session is still working.')}
                    </SizableText>
                    <Steer
                      onCommand={command}
                      withhold={[
                        ...(paused ? (['pause'] as const) : (['resume'] as const)),
                        ...(busy || waiting ? (['pause', 'resume', 'stop'] as const) : []),
                      ]}
                    />
                    <Button size="sm" disabled={Boolean(notify)} onPress={() => void ask()}>
                      {notify === 'device' ? say('You will be notified') : notify === 'in-app' ? say('Watching here') : say('Notify me')}
                    </Button>
                  </XStack>
                ) : null}
              </YStack>
              <Composer
                column
                value={draft}
                onChange={setDraft}
                onSend={send}
                onStop={active && !waiting ? () => command('stop') : undefined}
                busy={active && !waiting && !draft.trim()}
                ready={Boolean(draft.trim())}
                disabled={!signed || busy || waiting || !(running || finished)}
                placeholder={placeholder}
                label={steering ? say('Steer this session') : say('Follow up on this session')}
                field={{
                  onKeyDown: (e: { key?: string; ctrlKey?: boolean; metaKey?: boolean; preventDefault?: () => void }) => {
                    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter' && steering && draft.trim()) {
                      e.preventDefault?.()
                      enqueue()
                    }
                  },
                }}
                plus={{
                  upload: () => pick(files, setFiles, setNote, say),
                  research: { on: tools.gear.gear.research, set: (on) => tools.gear.set({ research: on }) },
                  skills: tools.skills,
                  design: tools.design,
                  plugins,
                  memory: { on: tools.gear.gear.memory, set: (on) => tools.gear.set({ memory: on }) },
                }}
                slash={tools.skills}
                at={plugins}
                carried={[...tools.on, ...carrying(files, setFiles)]}
                // What a new turn from here is made with; words that steer this session use neither.
                mind={signed && !steering ? mind : null}
                effort={signed && !steering ? pacing(mind) : null}
                limits={limits}
                voice={voice}
              >
                {active && !waiting && draft.trim() ? (
                  <XStack
                    render="button"
                    data-slot="composer-stop"
                    aria-label={say('Stop session')}
                    {...({ title: say('Stop session') } as object)}
                    onPress={() => command('stop')}
                    items="center"
                    gap="$1.5"
                    px="$2.5"
                    height={30}
                    $max-md={{ minH: 40 }}
                    rounded={999}
                    borderWidth={1}
                    borderColor="var(--border-strong)"
                    bg="$panel"
                    cursor="pointer"
                    hoverStyle={{ bg: '$hover' }}
                    focusVisibleStyle={RING}
                  >
                    <Square size={10} fill="currentColor" />
                    <SizableText size="$1" fontWeight="500" color="$ink">
                      {say('Stop')}
                    </SizableText>
                  </XStack>
                ) : null}
              </Composer>
            </YStack>
          </>
        )}
      </YStack>
      {desk && wide ? (
        <YStack
          data-slot="desk"
          ref={split.aside}
          {...pane}
          // A pane of its own beside the session's, the gutter between them (useSplit reads it as the inline-start margin).
          marginInlineStart={gap}
          position="relative"
          shrink={0}
          minH={0}
        >
          {split.grip ? (
            // In the gutter between the two panes, measured inside the pane's edge.
            <Gutter {...split.grip} label={say('Resize the side panel')} inset={pane.borderWidth} />
          ) : null}
          <SideProvider value={split.side}>{desk_(false, true)}</SideProvider>
        </YStack>
      ) : null}
      {tools.dialog}
      <Publish
        source={publishSource}
        onClose={() => setPublishSource(null)}
        onLive={() => {
          rebuilt()
          setPublishRev((n) => n + 1)
        }}
      />
      <Confirm
        open={deleting}
        onOpenChange={setDeleting}
        title={say('Delete this session?')}
        says={say('“{title}” is deleted for good, with every link it was shared by. To keep it out of the list instead, archive it.', {
          title: headline(record?.title ?? '', record?.repo ?? '') || say('this session'),
        })}
        act={say('Delete')}
        run={async () => {
          if (running) await stop(t, activeId, 'Deleted').catch(() => undefined)
          for (const sid of chain) {
            await remove(t, sid).catch(() => undefined)
            mark(sid, { deleted: true })
          }
          host.go('')
        }}
      />
      <Dialog open={naming} onOpenChange={setNaming}>
        <DialogContent maxW={480}>
          <DialogTitle>{say('Rename this session')}</DialogTitle>
          <Input
            autoFocus
            value={name}
            onChangeText={setName}
            aria-label={say('The session’s name')}
            maxLength={512}
            onKeyDown={(e: { key?: string }) => {
              if (e.key === 'Enter') save()
            }}
          />
          <XStack gap="$2" justify="flex-end">
            <Button size="sm" variant="outline" onPress={() => setNaming(false)}>
              {say('Cancel')}
            </Button>
            <Button size="sm" disabled={busy || !name.trim()} onPress={save}>
              {say('Save')}
            </Button>
          </XStack>
        </DialogContent>
      </Dialog>
    </XStack>
  )
}

/** What a setup run does, said once per browser the first time one is watched. */
function Onboarding({ onDone }: { onDone: () => void }) {
  const { t } = useI18n()
  const steps = [
    t('Explore, install and check the codebase'),
    t('Name the secrets it needs — you set their values'),
    t('Propose the environment for you to review and save'),
  ]
  return (
    <YStack gap="$3" p="$5" rounded="$4" borderWidth={1} borderColor="var(--border-strong)" bg="$panel">
      <YStack gap="$1.5">
        <SizableText size="$4" fontWeight="600" color="$ink">
          {t('Set up a cloud environment')}
        </SizableText>
        <SizableText size="$2" color="$soft">
          {[
            t('An environment lets every later run install, start, test and check its changes the way an engineer does.'),
            t('Setup is agent-led and takes several minutes.'),
            t('The agent will:'),
          ].join(' ')}
        </SizableText>
      </YStack>
      <YStack gap="$1.5">
        {steps.map((s, i) => (
          <XStack key={s} gap="$2.5">
            <SizableText size="$2" color="$soft" width={16} fontVariant={['tabular-nums']}>
              {String(i + 1)}
            </SizableText>
            <SizableText size="$2" color="$ink">
              {s}
            </SizableText>
          </XStack>
        ))}
      </YStack>
      <SizableText size="$2" color="$soft">
        {t('Interrupt anytime to steer the agent or ask it questions.')}
      </SizableText>
      <Button size="sm" onPress={onDone}>
        {t('Got it')}
      </Button>
    </YStack>
  )
}
