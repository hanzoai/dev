/**
 * The run's side panel: Dev's kinds of tab in the one panel (panel.tsx) — the
 * Preview of what the run's sandbox serves (or where it is published), its
 * Artifacts, Files, Diff and Terminal, and on asking its Desktop and its
 * codebase's Environment.
 *
 * The sandbox outlives the run: kept when the run ends, parked while nobody
 * watches, retired a day after it parks. The bar under the tabs says which and
 * suspends or resumes it; a tab that needs it running resumes it (sandbox.ts).
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a repository, branch,
 * port, address, command and the sandbox's own error are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { Check, Circle, ExternalLink, Folder, GitBranch, GitPullRequest, Globe, Info, Laptop, Maximize2, MoreHorizontal, Play, RefreshCcw, Square, SquareTerminal, Upload, X } from '@hanzogui/lucide-icons-2'
import { Button, DropdownMenu, Switch, type DropdownMenuProps } from '@hanzo/ui'
import { Console, DEVICES, PreviewFrame, Views, type FrameEvent, type Line, type PreviewHandle } from '@hanzo/ui/agents'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'

import { RING } from './pane.ts'
import { cacheBustUrl, hasUnpublishedEdits, previewRevision, resolvePreviewSource } from './preview-url.ts'
import { read as readChanges, type Changes } from './api/changes.ts'
import { read as readEnvironment } from './api/environment.ts'
import { park, ports as listening, preview, sandbox as look, serve, served, stay, wake, type Box, type Port } from './api/sandbox.ts'
import type { Event } from './api/sessions.ts'
import { cards, failure, shell, steps, type ShellLine } from './api/turn.ts'
import { useKept, useRead } from './data.ts'
import { formatDeviceLabel, useDevMode } from './dev-mode.ts'
import { Door } from './door.tsx'
import { Environment } from './environment.tsx'
import { Artifacts, Files } from './files.tsx'
import { Git } from './git.tsx'
import { useHost, useTarget } from './host.tsx'
import { useWake } from './spend.tsx'
import { away, Out } from './out.tsx'
import { Panel, type Kind } from './panel.tsx'
import type { Deck, Tabs } from './tabs.ts'

/** A run's panel before anyone has arranged it: Preview chosen first so live output opens immediately. */
export const SEED: Tabs = {
  tabs: [
    { id: 'preview', kind: 'preview', title: 'Preview' },
    { id: 'artifacts', kind: 'artifacts', title: 'Artifacts' },
    { id: 'files', kind: 'files', title: 'Files' },
    { id: 'diff', kind: 'diff', title: 'Diff' },
    { id: 'terminal', kind: 'terminal', title: 'Terminal' },
  ],
  at: 'preview',
}

/** How often the sandbox's state, and what it serves, is read while the panel is open. */
const EVERY = 10_000
const PORTS = 5_000

export function Desk({
  id,
  repo,
  branch,
  base,
  environment,
  mode,
  pr,
  title,
  project,
  sandbox,
  site,
  published,
  events,
  live,
  menu,
  refused,
  retry,
  onRetry,
  side,
  sheet,
  onHide,
  envColumn = false,
  splitPanes = [],
  onCloseSplitPane,
  onExpandSplitPane,
  sessionStatus = '',
  onPublish,
  topRightInset,
}: {
  id: string
  repo: string
  branch: string
  base: string
  environment: string
  mode: string
  pr: { href: string; label: string }
  title: string
  project: string
  /** The sandbox the run leased, or '' before it leases one and on a machine. */
  sandbox: string
  /** Where the run's work is published, or '' before it is. */
  site: string
  /** The run's latest publish, which loads the page again. */
  published: number
  events: Event[]
  live: boolean
  /** What can be done to the run — rename, share, copy its id — as its header offers it. */
  menu: NonNullable<DropdownMenuProps['items']>
  refused: string
  /** The refusal's one action: Retry, or Sign in when nobody is. */
  retry: string
  onRetry: () => void
  side: Deck
  sheet: boolean
  onHide: () => void
  /** Show the Environment inspection column beside the dock panes. */
  envColumn?: boolean
  /** Active split panes in the 2x2 multi-pane dock grid ('terminal' | 'preview' | 'diff' | 'files'). */
  splitPanes?: string[]
  onCloseSplitPane?: (paneId: string) => void
  onExpandSplitPane?: (paneId: string) => void
  /** Current session lifecycle status, used to trigger Preview refresh on completion. */
  sessionStatus?: string
  /** 1-click Publish handler surfaced from the Preview toolbar. */
  onPublish?: () => void
  /** Reserve space for the host shell's top-right corner buttons. */
  topRightInset?: boolean
}) {
  const { t: say } = useI18n()
  const [devMode] = useDevMode()
  const name = repo.split('/').filter(Boolean).pop() || repo
  const lines = shell(events)
  const failed = failure(events, say)
  const box = useBox(sandbox)
  // Nothing is said of the sandbox until it has been read.
  const held = box.value?.state ?? ''
  const environ = () => side.open({ kind: 'environment', title: say('Environment') })
  const unpublished = useMemo(() => hasUnpublishedEdits(events, published), [events, published])

  const kinds: Kind[] = [
    {
      id: 'preview',
      label: say('Preview'),
      bleed: true,
      render: () => (
        <Browser
          sandbox={sandbox}
          held={held}
          site={site}
          live={live}
          repo={repo}
          events={events}
          published={published}
          sessionStatus={sessionStatus}
          hasChanges={unpublished}
          onPublish={onPublish}
          onEnvironment={environ}
        />
      ),
    },
    {
      id: 'artifacts',
      label: say('Artifacts'),
      render: () => <Artifacts session={id} sandbox={sandbox} repo={repo} branch={branch} mode={mode} project={project} site={site} pr={pr} onEnvironment={environ} />,
    },
    { id: 'files', label: say('Files'), render: () => <Files session={id} repo={repo} branch={branch} sandbox={sandbox} live={held === 'running'} /> },
    { id: 'diff', label: devMode ? say('Diff') : say('Changes'), render: () => <Git session={id} title={title} live={live} /> },
    {
      id: 'terminal',
      label: devMode ? say('Terminal') : say('Activity'),
      bleed: true,
      render: () => (
        <Terminal
          id={id}
          sandbox={sandbox}
          held={held}
          lines={lines}
          live={live}
          failed={failed}
          refused={refused}
          retry={retry}
          onRetry={onRetry}
          devMode={devMode}
        />
      ),
    },
    { id: 'desktop', label: say('Desktop'), bleed: true, render: () => <Door which="screen" sandbox={sandbox} held={held} session={id} /> },
    {
      id: 'environment',
      label: say('Environment'),
      render: () => (
        <YStack gap="$2">
          <Environment repo={name} busy={live && mode === 'setup'} />
          <SizableText size="$2" color="$ink" pt="$4">
            {say('This session')}
          </SizableText>
          <Fact label={say('Session')} value={id} />
          <Fact label={say('Repository')} value={name || '—'} />
          <Fact label={say('Branch')} value={branch || '—'} />
          <Fact label={say('Base')} value={base || '—'} />
          <Fact label={say('Runs on')} value={environment || 'sandbox'} />
          <Fact label={say('Sandbox')} value={sandbox || '—'} />
          <Fact label={say('Mode')} value={mode || 'build'} />
        </YStack>
      ),
    },
  ]

  const panelNode =
    splitPanes.length >= 2 && !sheet ? (
      <YStack
        flex={1}
        minW={0}
        minH={0}
        display="grid"
        gridTemplateColumns={splitPanes.length > 1 ? 'repeat(2, minmax(0, 1fr))' : 'minmax(0, 1fr)'}
        $max-lg={{ gridTemplateColumns: 'minmax(0, 1fr)' }}
        gridTemplateRows={splitPanes.length > 2 ? 'repeat(2, minmax(0, 1fr))' : 'minmax(0, 1fr)'}
        bg="$background"
      >
        {splitPanes.map((pid, idx) => {
          const k = kinds.find((x) => x.id === pid)
          if (!k) return null
          const headerTitle =
            pid === 'diff'
              ? devMode
                ? `${base || 'main'} → ${branch || 'HEAD'}`
                : say('Changes')
              : pid === 'terminal'
                ? devMode
                  ? `${say('Terminal')} ${name || 'workspace'}`
                  : say('Activity')
                : pid === 'preview'
                  ? say('Preview')
                  : k.label
          return (
            <YStack
              key={pid}
              minW={0}
              minH={0}
              borderLeftWidth={idx % 2 === 1 ? 1 : 0}
              borderTopWidth={idx >= 2 ? 1 : 0}
              $max-lg={{ borderLeftWidth: 0, borderTopWidth: idx > 0 ? 1 : 0 }}
              borderColor="$borderColor"
              display="grid"
              gridTemplateRows="auto minmax(0, 1fr)"
            >
              <XStack
                px="$2.5"
                py="$1.5"
                gap="$1.5"
                items="center"
                borderBottomWidth={1}
                borderColor="$borderColor"
                minH={36}
                style={idx === 1 && (topRightInset ?? !sheet) ? { paddingInlineEnd: 'calc(10px + var(--hanzo-shell-top-right-inset, 0px))' } : undefined}
              >
                <SizableText flex={1} minW={0} size="$2" color="$ink" numberOfLines={1}>
                  {headerTitle}
                </SizableText>
                {onExpandSplitPane ? (
                  <XStack
                    render="button"
                    aria-label={say('Expand {pane}', { pane: k.label })}
                    {...({ title: say('Expand {pane}', { pane: k.label }) } as object)}
                    onClick={() => onExpandSplitPane(pid)}
                    minW={30}
                    minH={30}
                    items="center"
                    justify="center"
                    rounded="$2"
                    hoverStyle={{ bg: '$hover' }}
                    focusVisibleStyle={RING}
                  >
                    <Maximize2 size={13} opacity={0.7} />
                  </XStack>
                ) : null}
                {onCloseSplitPane ? (
                  <XStack
                    render="button"
                    aria-label={say('Close {pane}', { pane: k.label })}
                    {...({ title: say('Close {pane}', { pane: k.label }) } as object)}
                    onClick={() => onCloseSplitPane(pid)}
                    minW={30}
                    minH={30}
                    items="center"
                    justify="center"
                    rounded="$2"
                    hoverStyle={{ bg: '$hover' }}
                    focusVisibleStyle={RING}
                  >
                    <X size={14} opacity={0.7} />
                  </XStack>
                ) : null}
              </XStack>
              <YStack flex={1} minH={0} overflow={k.bleed ? 'hidden' : 'scroll'} {...(k.bleed ? null : { p: '$3' })}>
                {k.render()}
              </YStack>
            </YStack>
          )
        })}
      </YStack>
    ) : (
      <Panel
        side={side}
        kinds={kinds}
        sheet={sheet}
        topRightInset={topRightInset}
        onHide={onHide}
        label={say('Session details')}
        // An empty tab offers what this session made: its site and its pull request.
        suggested={[...(site ? [{ title: name || site, url: site }] : []), ...(pr.href ? [{ title: say('Pull request {pr}', { pr: pr.label }), url: pr.href }] : [])]}
        bar={sandbox ? <Held box={box} live={live} devMode={devMode} /> : null}
        menu={
          <DropdownMenu
            trigger={
              <XStack render="button" aria-label={say('Session actions')} p="$1.5" rounded="$2" hoverStyle={{ bg: '$hover' }}>
                <MoreHorizontal size={16} />
              </XStack>
            }
            items={[...menu, { key: 'details', label: say('Details'), icon: <Info size={16} />, onSelect: environ }]}
          />
        }
      />
    )

  if (envColumn && !sheet) {
    return (
      <XStack flex={1} minW={0} minH={0} width="100%" height="100%">
        <EnvironmentInspection
          id={id}
          repo={repo}
          name={name}
          branch={branch}
          environment={environment}
          pr={pr}
          title={title}
          events={events}
          live={live}
          devMode={devMode}
          onOpenEnvironment={environ}
          onOpenDiff={() => side.open({ kind: 'diff', title: devMode ? say('Diff') : say('Changes') })}
        />
        <YStack flex={1} minW={0} minH={0} borderLeftWidth={1} borderColor="$borderColor">
          {panelNode}
        </YStack>
      </XStack>
    )
  }

  return panelNode
}

interface Watch {
  value: Box | null
  busy: boolean
  note: string
  act: (how: 'park' | 'wake') => void
}

/**
 * The run's sandbox as it stands, and what moves it. It is read while the pane is
 * open and the page is in view, and no more once it has ended or failed.
 */
function useBox(id: string): Watch {
  const t = useTarget()
  const [value, setValue] = useState<Box | null>(null)
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState('')
  useEffect(() => {
    setValue(null)
    if (!id) return
    let gone = false
    let every: ReturnType<typeof setInterval> | undefined
    const read = () => {
      if (document.hidden) return
      look(t, id).then(
        (b) => {
          if (gone) return
          setValue(b)
          if (b.state === 'gone' || b.state === 'error') clearInterval(every)
        },
        (e: Error) => !gone && setNote(e.message),
      )
    }
    read()
    every = setInterval(read, EVERY)
    return () => {
      gone = true
      clearInterval(every)
    }
  }, [t, id])
  const act = useCallback(
    (how: 'park' | 'wake') => {
      setBusy(true)
      setNote('')
      ;(how === 'park' ? park(t, id) : wake(t, id)).then(setValue, (e: Error) => setNote(e.message)).finally(() => setBusy(false))
    },
    [t, id],
  )
  return { value, busy, note, act }
}

/** When a parked sandbox is retired, as a person reads it. */
const until = (ms: number): string => new Date(ms).toLocaleString(undefined, { weekday: 'short', hour: 'numeric', minute: '2-digit' })

/**
 * The sandbox's line under the tabs: where it is, and the one thing to do to it.
 * A sandbox a run is working in is not suspended from here.
 */
function Held({ box, live, devMode }: { box: Watch; live: boolean; devMode: boolean }) {
  const { t } = useI18n()
  const { wake: agree, dialog } = useWake()
  const b = box.value
  // Waking gives the run its cloud computer back, billed again from then: asked first (spend.tsx `useWake`).
  const wake = async () => {
    if (await agree()) box.act('wake')
  }
  const state = b?.state ?? 'pending'
  const says = devMode
    ? state === 'running'
      ? t('Sandbox running')
      : state === 'parked'
        ? b?.ends
          ? t('Sandbox suspended · its files are kept until {time}', { time: until(b.ends) })
          : t('Sandbox suspended · its files are kept')
        : state === 'pending'
          ? t('Sandbox starting…')
          : state === 'gone'
            ? t('Sandbox ended')
            : b?.error
              ? t('Sandbox unavailable · {error}', { error: b.error })
              : t('Sandbox unavailable')
    : state === 'running'
      ? t('Cloud workspace ready')
      : state === 'parked'
        ? t('Cloud workspace sleeping · files saved')
        : state === 'pending'
          ? t('Starting cloud workspace…')
          : state === 'gone'
            ? t('Cloud workspace closed')
            : b?.error
              ? t('Cloud workspace unavailable · {error}', { error: b.error })
              : t('Cloud workspace unavailable')
  return (
    <XStack px="$3" py="$1.5" gap="$2" rowGap="$1.5" items="center" flexWrap="wrap" borderBottomWidth={1} borderColor="$borderColor" aria-label={devMode ? t('Sandbox') : t('Workspace status')}>
      {dialog}
      <YStack width={7} height={7} rounded={99} bg={state === 'running' ? 'var(--state-online)' : state === 'parked' ? '$soft' : '$borderColor'} shrink={0} />
      <SizableText flex={1} minW={120} size="$1" color="$soft" numberOfLines={1}>
        {box.note || says}
      </SizableText>
      {state === 'running' && !live ? (
        <Button size="sm" variant="ghost" disabled={box.busy} onPress={() => box.act('park')}>
          {t('Suspend')}
        </Button>
      ) : state === 'parked' ? (
        <Button size="sm" variant="outline" borderColor="var(--border-strong)" disabled={box.busy} onPress={() => void wake()}>
          {box.busy ? t('Resuming…') : devMode ? t('Resume') : t('Wake')}
        </Button>
      ) : null}
    </XStack>
  )
}

/** What a preview page says of itself, when it cannot serve. */
const mono = { fontFamily: 'var(--f-mono, ui-monospace, monospace)', whiteSpace: 'pre' as const }

const PREVIEW = 'hanzo-preview'

/** How often a preview is opened again on its own before Retry is the person's to press. */
const REMINTS = 2
const REMINT = 30_000

/** What a preview page's status means to the person looking at it, when it is not reopened, in the language of `say`. */
const SAYS: Record<number, (port: number, say: Say) => string> = {
  401: (_, say) => `${say('This browser keeps the preview’s cookie out of a frame.')} ${say('Open it in a new tab.')}`,
  502: (port, say) => `${say('Nothing answered on port {port}.', { port })} ${say('Start the server, then Retry.')}`,
}

/**
 * The Browser tab: what the run's sandbox serves, one port at a time, framed at
 * its own origin; or, with nothing serving, where the run is published. Only a
 * running sandbox is shown: a suspended one is resumed from the bar, never by
 * looking. A preview that lost its sandbox (409) is opened again, at most twice
 * in a while; any other status is said, and Retry is the person's.
 */
function Browser({
  sandbox,
  held,
  site,
  live,
  repo,
  events,
  published,
  sessionStatus,
  hasChanges,
  onPublish,
  onEnvironment,
}: {
  sandbox: string
  held: Box['state'] | ''
  site: string
  live: boolean
  repo: string
  events: Event[]
  published: number
  sessionStatus: string
  hasChanges: boolean
  onPublish?: () => void
  onEnvironment: () => void
}) {
  const { t: say } = useI18n()
  const t = useTarget()
  const [open, setOpen] = useState<Port[]>([])
  const [port, setPort] = useState(0)
  const [src, setSrc] = useState('')
  const [why, setWhy] = useState('')
  const [attempt, setAttempt] = useState(0)
  const [rev, setRev] = useState(0)
  const [preferLive, setPreferLive] = useState(false)
  const [serving, setServing] = useState<Serving | null>(null)
  const reopened = useRef<number[]>([])
  const frame = useRef<HTMLIFrameElement>(null)
  // A suspended sandbox is woken only by a press (Open in a new tab, Start dev server) and only on a yes.
  const { wake, dialog } = useWake()
  const running = Boolean(sandbox) && held === 'running'
  // A server the session started ended with the session, so once it has, the person can
  // start one again here, in the sandbox kept for its previews — a suspended one
  // is resumed for it. While the session works, its server is the agent's to start.
  const startable = Boolean(sandbox) && !live && (held === 'running' || held === 'parked')

  // Track turn completions, file edits, pushes, and publishes so Preview refreshes automatically.
  const revision = useMemo(() => previewRevision(events, sessionStatus, published), [events, sessionStatus, published])
  const lastRevision = useRef(revision)
  const wasLive = useRef(live)
  useEffect(() => {
    const completed = wasLive.current && !live
    wasLive.current = live
    if (lastRevision.current !== revision || completed) {
      lastRevision.current = revision
      setRev((n) => n + 1)
      if (port) setAttempt((a) => a + 1)
    }
  }, [revision, live, port])

  // What listens, read again while the sandbox runs: a dev server appears once it listens.
  // Also re-polls immediately whenever `rev` advances (e.g. a turn finished or edits landed).
  useEffect(() => {
    if (!running) {
      setOpen([])
      setPort(0)
      return
    }
    let gone = false
    const read = () =>
      listening(t, sandbox).then(
        (ps) => {
          if (gone) return
          setOpen(ps)
          setPort((p) => (ps.some((x) => x.port === p) ? p : (ps[0]?.port ?? 0)))
        },
        () => undefined,
      )
    void read()
    const every = setInterval(read, PORTS)
    return () => {
      gone = true
      clearInterval(every)
    }
  }, [t, sandbox, running, rev])

  // One address per port and attempt: its ticket is spent when the page opens it.
  useEffect(() => {
    setSrc('')
    setWhy('')
    if (!port || !sandbox) return
    let gone = false
    // Opened by the page (and again after the sandbox stopped), so it never wakes it; only a press does.
    preview(t, sandbox, port, stay).then(
      (u) => !gone && setSrc(u),
      (e: Error) => !gone && setWhy(e.message),
    )
    return () => {
      gone = true
    }
  }, [t, sandbox, port, attempt])

  // A preview that can no longer serve says so; one whose sandbox stopped is opened again, a little.
  useEffect(() => {
    if (!src) return
    const origin = new URL(src).origin
    const hear = (e: MessageEvent) => {
      if (e.origin !== origin || e.source !== frame.current?.contentWindow) return
      const m = e.data as { source?: unknown; status?: unknown } | null
      if (m?.source !== PREVIEW || typeof m.status !== 'number') return
      const now = Date.now()
      reopened.current = reopened.current.filter((at) => now - at < REMINT)
      if (m.status === 409 && reopened.current.length < REMINTS) {
        reopened.current.push(now)
        setAttempt((a) => a + 1)
        return
      }
      const words = SAYS[m.status]
      if (words || m.status === 409) setWhy(words ? words(port, say) : `${say('The sandbox stopped serving.')} ${say('Retry to open it again.')}`)
    }
    window.addEventListener('message', hear)
    return () => window.removeEventListener('message', hear)
  }, [src, port, say])

  const tab = () => away(preview(t, sandbox, port, wake)).catch((e: Error) => setWhy(e.message))

  /**
   * Start the app's server: the codebase's start command when its environment
   * names one, else its package's dev script. The page opens once it listens,
   * because what listens is read every few seconds anyway.
   */
  const start = async () => {
    setPreferLive(false)
    setServing({ command: '', up: true, log: '', why: '' })
    try {
      const named = repo ? await readEnvironment(t, repo).then((e) => e.start, () => '') : ''
      setServing({ command: await serve(t, sandbox, wake, named), up: true, log: '', why: '' })
    } catch (e) {
      setServing({ command: '', up: false, log: '', why: (e as Error).message })
    }
  }

  // While a server started here comes up, whether it is still running and what it
  // says; one that exits says why. Read for a while, never for as long as the page is open.
  const coming = Boolean(serving?.command && serving.up && !port)
  useEffect(() => {
    if (!coming) return
    let gone = false
    const until = Date.now() + WATCH
    const check = () => {
      if (Date.now() > until) {
        clearInterval(every)
        setServing((v) => v && { ...v, why: say('It is running, and nothing listens on a port yet.') })
        return
      }
      served(t, sandbox).then(
        (s) => {
          if (gone) return
          setServing((v) => v && { ...v, up: s.up, log: s.log, why: s.up ? '' : say('The dev server stopped.') })
        },
        () => undefined,
      )
    }
    const every = setInterval(check, WATCHING)
    return () => {
      gone = true
      clearInterval(every)
    }
  }, [t, sandbox, coming, say])

  const hasSandboxPreview = running && Boolean(port)
  const resolved = resolvePreviewSource({
    sandboxUrl: hasSandboxPreview ? src || `https://sandbox.local:${port}` : '',
    liveUrl: site,
    preferLive,
  })
  const showingSandbox = hasSandboxPreview && resolved.kind === 'sandbox'

  if (!showingSandbox) {
    return (
      <>
        {dialog}
        <Site
          site={site}
          rev={rev}
          onReload={() => setRev((n) => n + 1)}
          live={live}
          held={held}
          serving={serving}
          events={events}
          hasChanges={hasChanges}
          hasSandboxPreview={hasSandboxPreview}
          onUseSandbox={hasSandboxPreview ? () => setPreferLive(false) : undefined}
          onStart={startable ? () => void start() : undefined}
          onPublish={onPublish}
          onEnvironment={repo ? onEnvironment : undefined}
        />
      </>
    )
  }
  const host = open.find((p) => p.port === port)?.host ?? ''
  const framedSrc = src ? cacheBustUrl(src, rev) : ''
  return (
    <YStack flex={1} minH={0}>
      {dialog}
      <XStack px="$2" py="$1.5" gap="$1.5" items="center" borderBottomWidth={1} borderColor="$borderColor" minW={0}>
        <XStack
          render="button"
          aria-label={say('Reload the page')}
          {...({ title: say('Reload the page') } as object)}
          onPress={() => {
            setRev((n) => n + 1)
            setAttempt((a) => a + 1)
          }}
          p="$1.5"
          rounded="$2"
          shrink={0}
          hoverStyle={{ bg: '$hover' }}
        >
          <RefreshCcw size={14} />
        </XStack>
        <XStack data-slot="preview-mode" data-kind="sandbox" items="center" gap="$1" px="$2" py="$0.5" rounded={999} bg="$edge" borderWidth={1} borderColor="$borderColor" shrink={0}>
          <YStack width={6} height={6} rounded={99} bg="var(--state-online)" shrink={0} />
          <SizableText size="$1" color="$ink" numberOfLines={1}>
            {say('Sandbox preview')}
          </SizableText>
        </XStack>
        {open.length > 1 ? (
          <XStack gap="$1" shrink={0}>
            {open.map((p) => (
              <XStack
                key={p.port}
                render="button"
                aria-label={say('Port {port}', { port: p.port })}
                {...({ title: say('Port {port}', { port: p.port }) } as object)}
                onPress={() => setPort(p.port)}
                px="$2"
                py="$1"
                rounded="$2"
                bg={p.port === port ? '$hover' : 'transparent'}
              >
                <SizableText size="$1" color={p.port === port ? '$ink' : '$soft'}>
                  {String(p.port)}
                </SizableText>
              </XStack>
            ))}
          </XStack>
        ) : null}
        <SizableText flex={1} minW={0} size="$1" color="$soft" numberOfLines={1} style={mono}>
          {host || say('port {port}', { port })}
        </SizableText>
        {site ? (
          <XStack
            render="button"
            aria-label={say('Live site')}
            onPress={() => setPreferLive(true)}
            px="$2"
            height={28}
            items="center"
            rounded="$2"
            shrink={0}
            hoverStyle={{ bg: '$hover' }}
            focusVisibleStyle={RING}
          >
            <SizableText size="$1" color="$soft" numberOfLines={1}>
              {say('Live site')}
            </SizableText>
          </XStack>
        ) : null}
        {onPublish ? (
          <Button size="sm" variant="primary" onPress={onPublish} shrink={0}>
            <Upload size={13} />
            <SizableText size="$1" color="inherit">
              {say('Publish')}
            </SizableText>
          </Button>
        ) : null}
        <OpenOut label={say('Open in browser')} onPress={() => void tab()} />
      </XStack>
      {why ? (
        <YStack flex={1} items="center" justify="center" gap="$2" p="$6">
          <SizableText size="$2" color="$soft" style={{ textAlign: 'center' }}>
            {why}
          </SizableText>
          <XStack gap="$2">
            <Button size="sm" variant="outline" onPress={() => setAttempt((a) => a + 1)}>
              {say('Retry')}
            </Button>
            <Button size="sm" variant="outline" onPress={() => void tab()}>
              {say('Open in browser')}
            </Button>
          </XStack>
        </YStack>
      ) : framedSrc ? (
        <iframe
          key={`${port}-${attempt}`}
          ref={frame}
          src={framedSrc}
          title={say('What the session serves on port {port}', { port })}
          sandbox="allow-scripts allow-same-origin allow-forms allow-popups"
          style={{ border: 0, width: '100%', height: '100%', display: 'block', background: '#fff' }}
        />
      ) : (
        <YStack flex={1} items="center" justify="center">
          <SizableText size="$2" color="$soft">
            {say('Opening port {port}…', { port })}
          </SizableText>
        </YStack>
      )}
    </YStack>
  )
}

/**
 * The Preview's way out to the person's own browser (Chrome, or whichever they use; Hanzo Desktop hands it to
 * the system's browser): the live page at its own address, beside the work. Said in words, not an icon alone
 * (hanzo-inc/app issue 312). With `onPress` it is a button; without, the face of the link around it.
 */
function OpenOut({ label, onPress }: { label: string; onPress?: () => void }) {
  return (
    <XStack
      {...(onPress ? { render: 'button' as const, 'aria-label': label, onPress } : {})}
      items="center"
      gap="$1.5"
      px="$2"
      height={28}
      rounded="$2"
      borderWidth={1}
      borderColor="var(--border-strong)"
      hoverStyle={{ bg: '$hover' }}
      focusVisibleStyle={RING}
      data-slot="open-in-browser"
    >
      <ExternalLink size={13} />
      <SizableText size="$1" color="$ink" numberOfLines={1}>
        {label}
      </SizableText>
    </XStack>
  )
}

/** A server started from the Preview: what it runs, whether it still does, its last output, and why it stopped. */
interface Serving {
  /** '' until the sandbox has said what it started. */
  command: string
  up: boolean
  log: string
  why: string
}

/** How long, and how often, a server started here is watched while it comes up. */
const WATCH = 120_000
const WATCHING = 3_000

/**
 * Where the run published its work, framed, with its address, a reload and the
 * page in a tab of its own; or what keeps its sandbox from serving, and the
 * server the person can start in it.
 */
function Site({
  site,
  rev,
  onReload,
  live,
  held,
  serving,
  events,
  hasChanges,
  hasSandboxPreview,
  onUseSandbox,
  onStart,
  onPublish,
  onEnvironment,
}: {
  site: string
  rev: number
  onReload: () => void
  live: boolean
  events: Event[]
  held: Box['state'] | ''
  serving?: Serving | null
  hasChanges: boolean
  hasSandboxPreview: boolean
  onUseSandbox?: () => void
  onStart?: () => void
  onPublish?: () => void
  onEnvironment?: () => void
}) {
  const { t } = useI18n()
  const progress = live ? steps(events) : []
  const frame = useRef<PreviewHandle | null>(null)
  const [device, setDevice] = useState<'desktop' | 'mobile'>('desktop')
  const [dock, setDock] = useKept('hanzo.build.dock', 36)
  const [lines, setLines] = useState<Line[]>([])
  const onBridge = (e: FrameEvent) => {
    if (e.type === 'preview:console') setLines((was) => [...was.slice(-400), { id: `p${was.length}-${Date.now()}`, level: e.level, text: e.text, source: 'page' }])
  }
  const framedSite = site ? cacheBustUrl(site, rev) : ''
  return (
    <YStack flex={1} minH={0}>
      <XStack px="$2" py="$1.5" gap="$1.5" items="center" borderBottomWidth={1} borderColor="$borderColor" minW={0}>
        <XStack
          render="button"
          aria-label={t('Reload the page')}
          {...({ title: t('Reload the page') } as object)}
          onPress={() => {
            onReload()
            frame.current?.reload()
          }}
          p="$1.5"
          rounded="$2"
          shrink={0}
          hoverStyle={{ bg: '$hover' }}
        >
          <RefreshCcw size={14} />
        </XStack>
        {site ? (
          <XStack data-slot="preview-mode" data-kind="live" items="center" gap="$1" px="$2" py="$0.5" rounded={999} bg="$edge" borderWidth={1} borderColor="$borderColor" shrink={0}>
            <Globe size={11} opacity={0.75} />
            <SizableText size="$1" color="$ink" numberOfLines={1}>
              {t('Live site')}
            </SizableText>
          </XStack>
        ) : null}
        <SizableText flex={1} minW={0} size="$1" color="$soft" numberOfLines={1} style={mono}>
          {site || t('Nothing is serving yet')}
        </SizableText>
        {onUseSandbox ? (
          <Button size="sm" variant="outline" onPress={onUseSandbox} shrink={0}>
            {t('Sandbox preview')}
          </Button>
        ) : onStart && site ? (
          <XStack
            render="button"
            aria-label={t('Start sandbox preview')}
            onPress={onStart}
            items="center"
            gap="$1"
            px="$2"
            height={28}
            rounded="$2"
            borderWidth={1}
            borderColor="var(--border-strong)"
            shrink={0}
            hoverStyle={{ bg: '$hover' }}
            focusVisibleStyle={RING}
          >
            <Play size={12} />
            <SizableText size="$1" color="$ink" numberOfLines={1}>
              {serving?.up ? t('Starting…') : t('Sandbox preview')}
            </SizableText>
          </XStack>
        ) : null}
        {onPublish ? (
          <Button size="sm" variant="primary" onPress={onPublish} shrink={0}>
            <Upload size={13} />
            <SizableText size="$1" color="inherit">
              {t('Publish')}
            </SizableText>
          </Button>
        ) : null}
        <Views views={DEVICES} value={device} onChange={(d) => setDevice(d as 'desktop' | 'mobile')} label={t('Device')} labels="none" />
        {site ? (
          <Out href={site} label={t('Open in browser')}>
            <OpenOut label={t('Open in browser')} />
          </Out>
        ) : null}
      </XStack>
      {site && hasChanges && !live && !hasSandboxPreview ? (
        <XStack data-slot="preview-stale-banner" px="$2.5" py="$1.5" gap="$2" items="center" bg="$sunken" borderBottomWidth={1} borderColor="$borderColor" flexWrap="wrap">
          <SizableText size="$1" color="$soft" flex={1} minW={140}>
            {t('Showing the deployed live site — edits in this session are not published yet.')}
          </SizableText>
          {onStart ? (
            <Button size="sm" variant="outline" onPress={onStart}>
              {t('Start preview')}
            </Button>
          ) : null}
          {onPublish ? (
            <Button size="sm" variant="primary" onPress={onPublish}>
              {t('Publish changes')}
            </Button>
          ) : null}
        </XStack>
      ) : null}
      <PreviewFrame
        ref={frame}
        src={framedSite}
        title={t('Where this session is published')}
        device={device}
        onBridge={onBridge}
        empty={
          <YStack items="center" gap="$2.5" p="$6">
            <Globe size={28} opacity={0.45} />
            <SizableText size="$3" color="$ink">
              {t('Nothing is serving yet')}
            </SizableText>
            <SizableText size="$2" color="$soft" style={{ textAlign: 'center', maxWidth: 280 }}>
              {held === 'parked'
                ? `${t('This session’s sandbox is suspended.')} ${t('Resume it to see what it serves.')}`
                : live
                  ? t('When the session starts a server in its sandbox, the page opens here.')
                  : `${t('Nothing listens in this session’s sandbox, and it published no site.')} ${t('A server the session started stopped when the session ended.')}`}
            </SizableText>
            {progress.length ? (
              <YStack render="ol" aria-label={t('Live session steps')} gap="$2" m={0} p={0} width="100%" maxW={340}>
                {progress.map((step) => (
                  <XStack render="li" key={step.name} borderWidth={1} borderColor="$borderColor" bg="$sunken" rounded="$3" p="$2.5" gap="$2" items="center">
                    {step.done ? <Check size={15} aria-hidden /> : <Circle size={15} aria-hidden />}
                    <SizableText size="$2" flex={1}>{step.name}</SizableText>
                    <SizableText size="$1" color="$soft">{step.done ? t('Finished') : t('Running')}</SizableText>
                  </XStack>
                ))}
              </YStack>
            ) : null}
            {serving ? (
              <YStack items="center" gap="$2" width="100%">
                <SizableText size="$2" color="$ink" role="status" style={{ textAlign: 'center', maxWidth: 320 }}>
                  {serving.why || (serving.command ? t('Starting {command}…', { command: serving.command }) : t('Starting the dev server…'))}
                </SizableText>
                {serving.log ? <Tail log={serving.log} /> : null}
                {serving.why ? (
                  <XStack gap="$2" flexWrap="wrap" justify="center">
                    {onStart ? (
                      <Button size="sm" variant="outline" onPress={onStart}>
                        {t('Try again')}
                      </Button>
                    ) : null}
                    {onEnvironment ? (
                      <Button size="sm" variant="outline" onPress={onEnvironment}>
                        {t('Set a start command')}
                      </Button>
                    ) : null}
                  </XStack>
                ) : null}
              </YStack>
            ) : onStart ? (
              <Button size="sm" onPress={onStart}>
                {t('Start dev server')}
              </Button>
            ) : null}
          </YStack>
        }
      />
      {site ? <Console lines={lines} height={dock} onHeight={setDock} onClear={() => setLines([])} label={t('Console')} /> : null}
    </YStack>
  )
}

/** The last lines a server wrote, as it wrote them. */
function Tail({ log }: { log: string }) {
  return (
    <YStack width="100%" maxW={460} maxH={200} overflow="scroll" rounded="$2" borderWidth={1} borderColor="$borderColor" p="$2">
      <SizableText size="$1" color="$soft" fontFamily="$mono" $platform-web={{ whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}>
        {log.split('\n').slice(-12).join('\n')}
      </SizableText>
    </YStack>
  )
}

function EnvironmentInspection({
  id,
  repo,
  name,
  branch,
  environment,
  pr,
  title,
  events,
  live,
  devMode,
  onOpenEnvironment,
  onOpenDiff,
}: {
  id: string
  repo: string
  name: string
  branch: string
  environment: string
  pr: { href: string; label: string }
  title: string
  events: Event[]
  live: boolean
  devMode: boolean
  onOpenEnvironment: () => void
  onOpenDiff: () => void
}) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const [remoteControl, setRemoteControl] = useKept('hanzo.build.remote.control', true)
  const changes = useRead(signed ? () => readChanges(target, id) : null, null as Changes | null, [target, id, signed])
  const allCards = cards(events, '', t)
  const shellCards = allCards.filter((c): c is Extract<typeof c, { kind: 'shell' }> => c.kind === 'shell')
  const runningTasks = shellCards.filter((c) => c.ran === 'running' && live)
  const finishedCount = shellCards.filter((c) => c.ran !== 'running' || !live).length + steps(events).filter((s) => s.done).length
  const totalChars = events.reduce((sum, e) => sum + JSON.stringify(e.payload ?? '').length, 0)
  const estTokens = Math.max(1, Math.round(totalChars / 4))
  const contextPct = Math.min(100, Math.max(1, Math.round((estTokens / 128_000) * 100)))

  const added = changes.value?.files.reduce((n, f) => n + f.additions, 0) ?? 0
  const removed = changes.value?.files.reduce((n, f) => n + f.deletions, 0) ?? 0
  const envDisplay = formatDeviceLabel(environment || 'sandbox', { devMode, say: t })

  return (
    <YStack width={264} shrink={0} minH={0} overflow="scroll" bg="$background" px="$3" py="$2.5" gap="$3">
      <SizableText size="$2" color="$ink" fontWeight="600">
        {devMode ? t('Environment') : t('Workspace')}
      </SizableText>

      <YStack gap="$2">
        <XStack items="center" gap="$2">
          <Laptop size={14} opacity={0.7} />
          <SizableText flex={1} size="$2" color="$ink" numberOfLines={1}>
            {envDisplay.title}
          </SizableText>
          <XStack
            render="button"
            onPress={onOpenEnvironment}
            px="$1.5"
            py="$0.5"
            minH={28}
            items="center"
            focusVisibleStyle={RING}
          >
            <SizableText size="$1" color="$ink" textDecorationLine="underline">
              {t('Save')}
            </SizableText>
          </XStack>
        </XStack>

        <XStack items="center" gap="$2">
          <Folder size={14} opacity={0.7} />
          <SizableText size="$2" color="$ink" numberOfLines={1}>
            {name || 'workspace'}
          </SizableText>
          <SizableText size="$1" color="$soft" numberOfLines={1}>
            · ~/work
          </SizableText>
        </XStack>

        {repo ? (
          <XStack items="center" gap="$2">
            <Folder size={14} opacity={0.7} />
            <SizableText size="$2" color="$ink" numberOfLines={1}>
              {repo}
            </SizableText>
          </XStack>
        ) : null}

        {branch ? (
          <XStack items="center" gap="$2">
            <GitBranch size={14} opacity={0.7} />
            <SizableText size="$2" color="$ink" numberOfLines={1}>
              {branch}
            </SizableText>
          </XStack>
        ) : null}

        <XStack items="center" gap="$2" pt="$1">
          <SizableText flex={1} size="$2" color="$ink">
            {t('Remote Control')}
          </SizableText>
          <Switch checked={remoteControl} onCheckedChange={setRemoteControl} size="sm" aria-label={t('Remote Control')} />
        </XStack>
      </YStack>

      <YStack borderTopWidth={1} borderColor="$borderColor" pt="$2.5" gap="$2">
        <XStack items="center" gap="$1.5">
          <SizableText size="$2" color="$ink" fontWeight="500">
            {t('Context')}
          </SizableText>
          <SizableText flex={1} size="$1" color="$soft" text="end" fontVariant={['tabular-nums']}>
            {`${contextPct}%`}
          </SizableText>
          <YStack width={28} height={6} rounded={99} bg="$borderColor" overflow="hidden">
            <YStack width={`${contextPct}%`} height="100%" bg="$blue10" />
          </YStack>
        </XStack>
      </YStack>

      <YStack borderTopWidth={1} borderColor="$borderColor" pt="$2.5" gap="$2">
        <XStack items="center" gap="$1.5">
          <SizableText size="$2" color="$ink" fontWeight="500">
            {t('Connectors')}
          </SizableText>
          <SizableText flex={1} size="$1" color="$soft" text="end">
            {t('Connected')}
          </SizableText>
        </XStack>
      </YStack>

      <YStack borderTopWidth={1} borderColor="$borderColor" pt="$2.5" gap="$1.5">
        <SizableText size="$2" color="$ink" fontWeight="500">
          {t('Changes')}
        </SizableText>
        <XStack render="button" onPress={onOpenDiff} items="flex-start" gap="$2" py="$1" rounded="$2" hoverStyle={{ bg: '$hover' }}>
          <GitPullRequest size={14} color="$green10" />
          <YStack flex={1} minW={0} gap="$0.5">
            <SizableText size="$2" color="$ink" numberOfLines={1} text="start">
              {title || branch || (devMode ? t('Working tree') : t('Latest changes'))}
            </SizableText>
            <XStack gap="$1.5" items="center">
              {pr.label ? (
                <SizableText size="$1" color="$soft">
                  {pr.label}
                </SizableText>
              ) : null}
              <SizableText size="$1" color="var(--state-success)" fontVariant={['tabular-nums']}>
                {`+${added}`}
              </SizableText>
              <SizableText size="$1" color="var(--state-error)" fontVariant={['tabular-nums']}>
                {`−${removed}`}
              </SizableText>
            </XStack>
          </YStack>
        </XStack>
      </YStack>

      <YStack borderTopWidth={1} borderColor="$borderColor" pt="$2.5" gap="$2">
        <SizableText size="$2" color="$ink" fontWeight="500">
          {t('Background tasks')}
        </SizableText>
        {runningTasks.map((task) => (
          <XStack key={task.key} items="center" gap="$2" py="$1">
            <SquareTerminal size={14} opacity={0.7} />
            <SizableText flex={1} minW={0} size="$2" color="$ink" numberOfLines={1}>
              {task.command}
            </SizableText>
            <Square size={10} fill="currentColor" />
          </XStack>
        ))}
        <XStack items="center" gap="$2" py="$0.5">
          <SizableText size="$1" color="$soft">
            {t('{n} finished', { n: finishedCount })}
          </SizableText>
        </XStack>
      </YStack>
    </YStack>
  )
}


function Fact({ label, value }: { label: string; value: string }) {
  return (
    <XStack gap="$3" items="baseline">
      <SizableText size="$1" color="$soft" width={96}>
        {label}
      </SizableText>
      <SizableText flex={1} size="$2" color="$ink" numberOfLines={2}>
        {value}
      </SizableText>
    </XStack>
  )
}

/**
 * The Terminal tab: a shell in the run's sandbox, in the same working tree as the
 * agent, while the sandbox is kept; and the agent's log, which outlives it. The
 * shell is a tmux session named for the run, so a reopened tab reattaches to it.
 */
function Terminal({
  id,
  sandbox,
  held,
  lines,
  live,
  failed,
  refused,
  retry,
  onRetry,
  devMode,
}: {
  id: string
  sandbox: string
  held: Box['state'] | ''
  lines: ShellLine[]
  live: boolean
  /** Why the run failed, as its last status says it, or ''. */
  failed: string
  refused: string
  retry: string
  onRetry: () => void
  devMode: boolean
}) {
  const { t } = useI18n()
  const reachable = Boolean(sandbox) && held === 'running'
  // The agent log is the tab it opens on; the shell is one press away for everyone whenever the sandbox is reachable.
  const [log, setLog] = useState(true)
  const showShell = reachable && !log
  return (
    <YStack flex={1} minH={0} bg="$background">
      <XStack px="$2" py="$1.5" gap="$1" rowGap="$1" flexWrap="wrap" borderBottomWidth={1} borderColor="$borderColor" items="center">
        {reachable ? (
          <XStack
            render="button"
            aria-label={t('Shell')}
            aria-pressed={showShell}
            onPress={() => setLog(false)}
            px="$2.5"
            py="$1"
            minH={30}
            items="center"
            focusVisibleStyle={RING}
          >
            <SizableText size="$1" color={showShell ? '$ink' : '$soft'}>
              {t('Shell')}
            </SizableText>
          </XStack>
        ) : null}
        <XStack
          render="button"
          aria-label={t('Agent log')}
          aria-pressed={!showShell}
          onPress={() => setLog(true)}
          px="$2.5"
          py="$1"
          minH={30}
          items="center"
          focusVisibleStyle={RING}
        >
          <SizableText size="$1" color={showShell ? '$soft' : '$ink'}>
            {t('Agent log')}
          </SizableText>
        </XStack>
      </XStack>
      {showShell ? <Door which="terminal" sandbox={sandbox} held={held} session={`run-${id.replace(/^sess_/, '').slice(0, 12)}`} /> : <Log lines={lines} live={live} failed={failed} refused={refused} retry={retry} onRetry={onRetry} />}
    </YStack>
  )
}

function Log({
  lines,
  live,
  failed,
  refused,
  retry,
  onRetry,
}: {
  lines: ShellLine[]
  live: boolean
  failed: string
  refused: string
  retry: string
  onRetry: () => void
}) {
  const { t } = useI18n()
  const prompt = 'workspace'
  const idle = lines.length === 0 && !live
  return (
    <YStack flex={1} minH={0}>
      <YStack flex={1} minH={0} overflow="scroll" px="$3" py="$3" gap="$1">
        {idle ? (
          <YStack flex={1} items="center" justify="center" gap="$3" py="$8">
            <SizableText size="$2" color="$soft" text="center">
              {refused || (failed ? t('This session stopped before its first command: {reason}', { reason: failed }) : t('This session finished without writing a command.'))}
            </SizableText>
            {refused ? (
              <Button size="sm" variant="outline" onPress={onRetry}>
                {retry}
              </Button>
            ) : null}
          </YStack>
        ) : (
          lines.map((line, i) =>
            line.role === 'cmd' ? (
              <XStack key={i} gap="$2">
                <SizableText size="$1" color="$soft" fontFamily="$mono" $platform-web={{ whiteSpace: 'pre' }}>
                  {`${prompt} $`}
                </SizableText>
                <SizableText flex={1} size="$1" color="$ink" fontFamily="$mono" $platform-web={{ whiteSpace: 'pre' }}>
                  {line.text}
                </SizableText>
              </XStack>
            ) : (
              <SizableText key={i} size="$1" color="$ink" fontFamily="$mono" $platform-web={{ whiteSpace: 'pre' }}>
                {line.text}
              </SizableText>
            ),
          )
        )}
        {live ? (
          <XStack gap="$2" items="center">
            <SizableText size="$1" color="$soft" fontFamily="$mono" $platform-web={{ whiteSpace: 'pre' }}>
              {`${prompt} $`}
            </SizableText>
            <YStack width={7} height={14} bg="$ink" />
          </XStack>
        ) : null}
      </YStack>
      <XStack px="$3" py="$2" borderTopWidth={1} borderColor="$borderColor">
        <SizableText size="$1" color="$soft">
          {t('Commands appear here as the run writes them.')}
        </SizableText>
      </XStack>
    </YStack>
  )
}
