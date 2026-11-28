/**
 * A project's workspace, in the same window: the chat on the left, what it
 * built on the right.
 *
 * A project is a repository on the forge, addressed `<org>/<repo>`; the site it
 * publishes to, when it has one, is the org's project row on that repository
 * (or of the same name). A deployed site with no repository on the forge is
 * opened by its slug (Artifacts), and works the same way.
 *
 *   bar     mark · project · history · chat toggle | Preview Files Code Layers |
 *           Share · Publish; on a phone the views (with Chat) are a row under it
 *   left    the project's runs as a conversation, suggestions, and the ask
 *   right   the deployed page, framed under its own bar (reload · page · device
 *           · open), and kept loaded while another view is open; the repository
 *           at the run's branch as a tree and as read-only code; the element a
 *           person picked
 *   dock    the run's console — its steps and its log, as they stream
 *
 * The edge between left and right is dragged, or moved with the arrow keys, to
 * size the chat, and the width is kept per browser; the right side keeps room
 * for a page whatever the chat asked for.
 *
 * A chat turn IS a coding run: `POST /v1/agent/coding` with the project, the
 * mode, and the previous run as `after`, so the next ask builds on the branch
 * the last one pushed. Files and Code read native git at that branch. The
 * preview is the project's own deployed address; a run's sandbox serves no
 * address the platform publishes, and nothing here invents one.
 *
 * A project in a workspace the platform made is published by the platform: a
 * template taken there reads `building` until its copy is `live` or failed, and
 * is read again until then; a run that publishes what it pushed says so on its
 * final status, which reads the project again and loads the preview again. Its
 * pull request is merged from beside its link. A build that failed, or that has
 * run past STALL, says so in the preview with the platform's reason and offers
 * Publish again, which builds the project's repository at its branch.
 *
 * Derived from the Hanzo App v2 editor (github.com/hanzoai/build-v2), itself
 * derived from OSW Studio and DeepSite (MIT). See NOTICE.
 *
 * Its words are the reader's language (@hanzo/i18n `t`, the target is
 * `target`); a project's and a run's names, branches, paths and the
 * platform's reasons are drawn as given.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import {
  AlertCircle,
  Check,
  Clock,
  ExternalLink,
  Globe,
  MousePointerClick,
  PanelLeft,
  RefreshCcw,
  Search,
  Share2,
  Square,
  Upload,
} from '@hanzogui/lucide-icons-2'
import {
  Attachments,
  CHAT,
  Console,
  DEVICES,
  Feedback,
  FileTabs,
  FileTree,
  PageSelect,
  PreviewFrame,
  ProjectChip,
  VIEWS,
  Views,
  Workspace,
  fold,
  type Attachment,
  type FrameEvent,
  type Line,
  type OpenFile,
  type PreviewHandle,
  type Verdict,
} from '@hanzo/ui/agents'
import { Composer, Message } from '@hanzo/ui/chat'
import { Button, Dialog, DialogContent, DialogTitle, Input, Skeleton, toast } from '@hanzo/ui'
import { HanzoMark } from '@hanzo/ui/product'
import { useLimits } from '@hanzo/ui/product/useLimits'
import { useDictation, useVoice } from '@hanzo/voice'
import { useEffect, useMemo, useRef, useState } from 'react'

import { copy as toClipboard } from './clipboard.ts'
import { RING } from './pane.ts'
import { read as pushed, type Pull } from './api/changes.ts'
import { codebases, one, type Codebase } from './api/codebases.ts'
import { start, steer as steerCoding, type Mode } from './api/coding.ts'
import { blob, tree } from './api/git.ts'
import { limits as readLimits } from './api/models.ts'
import { declare } from './api/platform.ts'
import { address, deployments, name as repoName, ours, templates, type Project as Row } from './api/projects.ts'
import { ports as listening, preview as sandboxPreview, stay } from './api/sandbox.ts'
import { list, message, stop, type Session } from './api/sessions.ts'
import { decode, followed, outcome, pull, said, scrub, who } from './api/turn.ts'
import { verdict as record } from './api/verdict.ts'
import { useKept, useProjects, useRead, useRun } from './data.ts'
import { Merge } from './git.tsx'
import { Gutter } from './gutter.tsx'
import { useHost, useTarget } from './host.tsx'
import { cacheBustUrl, previewRevision, resolvePreviewSource } from './preview-url.ts'
import { republish } from './question.ts'
import { useSpend } from './spend.tsx'
import { MODES } from './gear.ts'
import { level, pacing, useMind } from './mind.ts'
import { Out } from './out.tsx'
import { useEar, useSpoken } from './voice.ts'
import { Publish, type Source } from './publish.tsx'
import { path } from './route.ts'
import { cleanErrorText, isAlreadyRunningError, isSessionActive, isSessionLive, linkThread, resolveSessionState } from './thread.ts'
import { carrying, compose, pick, type Attached } from './tools.tsx'

type ViewId = 'preview' | 'files' | 'code' | 'layers'

/** How often a project being published is read again, ms. */
const AGAIN = 5000

/** How long a build may run before the builder offers to publish again, s. A template's build takes a few minutes. */
const STALL = 10 * 60

/** The chat's width beside the work, px: its default, and the two it is kept between. */
const CHAT_WIDTH = 360
const CHAT_FLOOR = 300
const CHAT_CEIL = 640
/** The Workspace's gutter between the chat and the work, and its margin right of the work, px ($3). */
const GUTTER = 12
/** What the work keeps beside the chat however wide the chat was asked to be, px. */
const WORK = 400 + 2 * GUTTER

/** What a workspace is opened on: a repository on the forge, or a deployed site by its slug. */
export type Where = { slug: string } | { org: string; name: string }

/** Whether the site `p` is the repository `r`'s: built from it, or named as it is. */
export function siteOf(p: Row, r: { org: string; name: string }): boolean {
  const want = `${r.org}/${r.name}`.toLowerCase()
  return (p.repo !== '' && address(p.repo).toLowerCase() === want) || p.slug === r.name.toLowerCase()
}

/** A site's slug for a repository's name: lowercase letters, digits and hyphens, at most 40. */
export function slugOf(name: string): string {
  return name
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '')
    .slice(0, 40)
    .replace(/-+$/, '')
}

/** One run in the conversation: what was asked, and what it came to. */
function Turn({ run, open, onOpen, project }: { run: Session; open: boolean; onOpen: () => void; project: string }) {
  const target = useTarget()
  const { t } = useI18n()
  const [v, setV] = useState<Verdict>(null)
  const [note, setNote] = useState('')
  return (
    <YStack gap="$2">
      <Message role="user">
        <SizableText size="$2" color="$ink">
          {run.title || t('Untitled session')}
        </SizableText>
      </Message>
      <Message
        role="assistant"
        busy={isSessionActive(run.status)}
        actions={
          <XStack items="center" gap="$2">
            <Feedback
              text={run.title}
              verdict={v}
              onVerdict={(next) => {
                setV(next)
                record(target, run.id, project, next).catch((e: unknown) => {
                  setV(null)
                  setNote((e as Error).message)
                })
              }}
            />
            {note ? (
              <SizableText size="$1" color="$soft">
                {note}
              </SizableText>
            ) : null}
            {!open ? (
              <XStack
                render="button"
                onPress={onOpen}
                px="$1.5"
                py="$1"
                minH={28}
                items="center"
                focusVisibleStyle={RING}
              >
                <SizableText size="$1" color="$soft" textDecorationLine="underline">
                  {t('Show steps')}
                </SizableText>
              </XStack>
            ) : null}
          </XStack>
        }
      >
        <SizableText size="$2" color="$ink">
          {run.status === 'done' || run.status === 'completed'
            ? t('Done.')
            : run.status === 'error'
              ? t('This session hit an error.')
              : run.status === 'stopped'
                ? t('Stopped.')
                : t('Working…')}
        </SizableText>
      </Message>
    </YStack>
  )
}

export function Project({ at }: { at: Where }) {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const spend = useSpend()
  const signed = Boolean(host.person)
  const all = useProjects(target, signed)
  // The repository the address names, and the forge's record of it.
  const named = 'slug' in at ? null : at
  const forge = useRead(named && signed ? () => one(target, named.name) : null, null as Codebase | null, [target, signed, named?.name])
  const project: Row | null = all.value.find((p) => (named ? siteOf(p, named) : 'slug' in at && p.slug === at.slug)) ?? null
  // The site this workspace publishes to: the project's, or the repository's name as one.
  const slug = project?.slug ?? ('slug' in at ? at.slug : slugOf(named?.name ?? ''))
  const title = project?.name ?? named?.name ?? slug
  // A project that has never served reads `building` while it is published, and
  // is read again until it is live or its build failed.
  const building = project?.status === 'building'
  const failed = project?.status === 'error'
  const { reload: again } = all
  // The clock a build is measured against, moved each time the project is read again.
  const [now, setNow] = useState(() => Date.now())
  useEffect(() => {
    if (!building) return
    const every = setInterval(() => {
      again()
      setNow(Date.now())
    }, AGAIN)
    return () => clearInterval(every)
  }, [building, again])
  // When the build began, and why it failed, are its latest deployment's to say.
  const failure = useRead(building || failed ? () => deployments(target, slug) : null, [], [target, slug, building, failed, project?.updated])
  const latest = failure.value[0]
  const began = latest?.status === 'building' ? latest.created : 0
  const ran = began ? Math.max(0, Math.floor(now / 1000) - began) : 0
  const stalled = building && ran > STALL
  // Publish again: the project's repository, built at its branch, the way Publish builds it.
  const [redo, setRedo] = useState<{ busy: boolean; error: string }>({ busy: false, error: '' })
  // The repository's clone address: the forge's for a repository, the site's own otherwise.
  const clone = named ? (forge.value?.clone ?? '') : (project?.repo ?? '')
  const home = named ? (forge.value?.branch ?? 'main') : project?.branch || 'main'
  const retry = async () => {
    if (!clone || redo.busy) return
    // Built again in a cloud sandbox: asked first.
    if (!(await spend.ask(republish(title, t)))) return
    setRedo({ busy: true, error: '' })
    try {
      await declare(target, { repo: clone, ref: home, name: slug, project: slug, mode: 'branch' })
      setNow(Date.now())
      again()
      setRedo({ busy: false, error: '' })
      toast.success(t('Publish queued for {name}', { name: title }))
    } catch (e) {
      setRedo({ busy: false, error: (e as Error).message })
    }
  }

  // The site's runs, or the org's when the repository has no site yet.
  const sited = !named || Boolean(project)
  const listed = useRead(
    signed ? () => list(target, sited ? { kind: 'coding', project: slug, limit: 50 } : { kind: 'coding', limit: 50 }) : null,
    [] as Session[],
    [target, signed, slug, sited],
  )
  // Only the runs on this project's own repository: a record can be moved into
  // any project, and a run's branch is what Files, Code and Publish read.
  const mine = (r: Session) => (named ? ours(r.repo, `${named.org}/${named.name}`) && Boolean(r.repo) : ours(r.repo, project?.repo ?? ''))
  const runs = { ...listed, value: listed.value.filter(mine) }
  const ordered = useMemo(() => [...runs.value].reverse(), [listed.value, project?.repo, named?.org, named?.name])
  const [chosen, setChosen] = useState<string | null>(null)
  const current = chosen ?? runs.value[0]?.id ?? null
  const run = useRun(target, current)
  const end = outcome(run.events)
  const nextSession = useMemo(() => followed(run.events), [run.events])
  const pr = pull(run.record?.pr ?? '', run.record?.repo ?? '')
  const runState = resolveSessionState(run.status, end.status)
  const running = Boolean(current) && isSessionLive(runState)
  const active = Boolean(current) && isSessionActive(runState)
  const sandboxId = run.record?.sandbox ?? ''

  // Follow a steered turn seamlessly into its continuation session.
  useEffect(() => {
    if (!current || !nextSession || nextSession === current) return
    linkThread(current, nextSession)
    setChosen(nextSession)
    runs.reload()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [current, nextSession])

  const [view, setView] = useKept<ViewId>(`hanzo.build.view.${slug}`, 'preview')
  const [device, setDevice] = useState<'desktop' | 'mobile'>('desktop')
  const [collapsed, setCollapsed] = useKept('hanzo.build.chat', false)
  const [split, setSplit] = useKept('hanzo.build.split', CHAT_WIDTH)
  // The workspace's width, which bounds the chat so the work keeps its room.
  const [room, setRoom] = useState(0)
  const most = room ? Math.max(CHAT_FLOOR, Math.min(CHAT_CEIL, room - WORK)) : CHAT_CEIL
  const across = Math.min(most, Math.max(CHAT_FLOOR, split))
  // Below md the chat and the work take turns, and Chat is one more view to switch back to.
  const [pane, setPane] = useState<'chat' | 'view'>('chat')
  const wide = useMedia().md
  // A workspace too narrow for the chat at its floor and the work at its room
  // (a tablet beside the host's column) shows the work, and the chat when asked:
  // never both cut. Asked here is for this visit; the kept choice is the wide one's.
  const cramped = wide && room > 0 && room < CHAT_FLOOR + WORK
  const [asked, setAsked] = useState(false)
  const shut = cramped ? !asked : collapsed
  const [dock, setDock] = useKept('hanzo.build.dock', 36)
  const [mode, setMode] = useKept<Mode>('hanzo.build.mode', 'build')
  const [page, setPage] = useState('/')
  const [draft, setDraft] = useState('')
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState('')
  const [picked, setPicked] = useState<Attachment[]>([])
  const [attached, setAttached] = useState<Attached[]>([])
  // The model and effort Chat and New share, and dictation into the ask.
  const { limits } = useLimits(signed ? (signal) => readLimits(target, signal) : null, target)
  const mind = useMind(target, (limits as { plan?: string } | null)?.plan, host.topUp)
  const voice = useVoice({ speech: useEar(target), language: useSpoken(), ...useDictation(draft, setDraft) })
  const [bridge, setBridge] = useState(false)
  const [editing, setEditing] = useState(false)
  const [pageLines, setPageLines] = useState<Line[]>([])
  const [history, setHistory] = useState(false)
  const [publish, setPublish] = useState<Source | null>(null)
  const [switcher, setSwitcher] = useState(false)
  const [switchQ, setSwitchQ] = useState('')
  const repos = useRead(switcher && signed ? () => codebases(target) : null, [] as Codebase[], [target, switcher, signed])
  const filteredRepos = useMemo(() => {
    const needle = switchQ.trim().toLowerCase()
    return needle ? repos.value.filter((c) => `${c.org}/${c.name}`.toLowerCase().includes(needle)) : repos.value
  }, [repos.value, switchQ])
  const frame = useRef<PreviewHandle | null>(null)
  const [previewRev, setPreviewRev] = useState(0)
  const [preferLive, setPreferLive] = useState(false)
  const [sandboxPort, setSandboxPort] = useState(0)
  const [sandboxUrl, setSandboxUrl] = useState('')

  // What was published or edited may be new files at the same address: re-read the
  // project, re-poll sandbox ports, and cache-bust reload the preview frame.
  const fresh = () => {
    again()
    setPreviewRev((n) => n + 1)
    frame.current?.reload()
  }

  // Poll the active session's sandbox for a live dev-server port so Preview prefers
  // live edits in the sandbox over the pre-edit deployed site.
  useEffect(() => {
    if (!signed || !sandboxId) {
      setSandboxPort(0)
      setSandboxUrl('')
      return
    }
    let gone = false
    const poll = () =>
      listening(target, sandboxId).then(
        (ps) => {
          if (gone) return
          setSandboxPort((p) => (ps.some((x) => x.port === p) ? p : (ps[0]?.port ?? 0)))
        },
        () => undefined,
      )
    void poll()
    const every = setInterval(poll, 5000)
    return () => {
      gone = true
      clearInterval(every)
    }
  }, [target, signed, sandboxId, previewRev])

  useEffect(() => {
    setSandboxUrl('')
    if (!signed || !sandboxId || !sandboxPort) return
    let gone = false
    sandboxPreview(target, sandboxId, sandboxPort, stay).then(
      (u) => !gone && setSandboxUrl(u),
      () => undefined,
    )
    return () => {
      gone = true
    }
  }, [target, signed, sandboxId, sandboxPort, previewRev])

  // Auto-refresh Preview whenever a turn finishes, edits/pushes land, or a publish completes.
  const revKey = useMemo(() => previewRevision(run.events, runState, end.published), [run.events, runState, end.published])
  const lastRev = useRef<{ run: string | null; key: string; wasActive: boolean }>({ run: null, key: '', wasActive: false })
  useEffect(() => {
    if (!run.record) return
    if (lastRev.current.run !== current) {
      lastRev.current = { run: current, key: revKey, wasActive: active }
      return
    }
    const finishedTurn = lastRev.current.wasActive && !active
    const advanced = lastRev.current.key !== revKey
    lastRev.current = { run: current, key: revKey, wasActive: active }
    if (advanced || finishedTurn) fresh()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [current, run.record, revKey, active])

  // Where the open run's pull request stands on the forge: Merge is offered while it is open.
  const proposal = useRead<{ run: string; pull: Pull | null } | null>(
    signed && current && pr.href ? () => pushed(target, current).then((c) => ({ run: current, pull: c.pull })) : null,
    null,
    [target, signed, current, pr.href],
  )
  const proposing = proposal.value?.run === current && proposal.value.pull?.state === 'open'
  // Files is drawn again after a merge, from what the forge holds now.
  const [round, setRound] = useState(0)

  // Files and Code: native git, at the branch the open run pushed (or the project's own).
  const repo = named ? named.name : project?.repo ? repoName(project.repo) : ''
  const ref = run.record?.branch || home
  const [files, setFiles] = useState<OpenFile[]>([])
  const [file, setFile] = useState<string | null>(null)
  useEffect(() => {
    setFiles([])
    setFile(null)
  }, [repo, ref])

  const openFile = async (path: string) => {
    setFile(path)
    setView('code')
    if (files.some((f) => f.path === path)) return
    setFiles((was) => [...was, { path, readOnly: true }])
    try {
      const b = await blob(target, repo, ref, path)
      const content = b.binary ? '' : b.truncated ? '' : b.text
      const error = b.binary ? t('A binary file — nothing to show as text.') : b.truncated ? t('Past the 1 MiB view cap — clone the repository to read it.') : undefined
      setFiles((was) => was.map((f) => (f.path === path ? { ...f, content, error } : f)))
    } catch (e) {
      setFiles((was) => was.map((f) => (f.path === path ? { ...f, error: (e as Error).message } : f)))
    }
  }

  // Pages: the HTML files at the repository's root, as the page picker's list.
  const root = useRead(repo ? () => tree(target, repo, ref, '') : null, [], [target, repo, ref])
  const pages = useMemo(
    () => [
      { id: '/', label: t('Homepage') },
      ...root.value
        .filter((e) => !e.dir && /\.html?$/i.test(e.name) && !/^index\.html?$/i.test(e.name))
        .map((e) => ({ id: `/${e.path}`, label: e.name.replace(/\.html?$/i, '') })),
    ],
    [root.value, t],
  )

  // Prefer the live sandbox preview when available; fall back to deployed live site or starter demo.
  const forked = project && !project.live && !building && !failed ? project.forked : ''
  const starters = useRead(forked ? () => templates(target) : null, [], [target.api, forked])
  const starter = forked ? starters.value.find((s) => s.slug === forked && s.demo) : undefined
  const resolvedPreview = resolvePreviewSource({
    sandboxUrl,
    liveUrl: project?.live,
    starterUrl: starter?.demo,
    preferLive,
  })
  const rawSrc = resolvedPreview.url
    ? resolvedPreview.kind === 'sandbox'
      ? resolvedPreview.url
      : new URL(page, resolvedPreview.url).toString()
    : null
  const src = rawSrc ? cacheBustUrl(rawSrc, previewRev) : null
  const why = failure.value[0]?.message || failure.error?.message || (failure.loading ? '' : t('Its build ended without saying why.'))

  const merged = () => {
    proposal.reload()
    root.reload()
    setRound((n) => n + 1)
  }

  const lines: Line[] = useMemo(
    () => [
      ...run.events
        .filter((e) => e.kind === 'log' || e.kind === 'tool-call' || e.kind === 'status')
        .map((e): Line => {
          // The console is where a run's error is read whole; the chat says it in one plain line.
          const body = decode(e.payload)
          const error = e.kind === 'status' && body && typeof body === 'object' && typeof body.error === 'string' ? scrub(body.error, t) : ''
          return {
            id: `r${e.seq || e.id}`,
            level: error || (e.kind === 'status' && /error/.test(said(e, run.record?.mode).toLowerCase())) ? 'error' : e.kind === 'tool-call' ? 'info' : 'log',
            text: error || said(e, run.record?.mode, t),
            source: e.kind === 'status' ? 'run' : who(e.actor),
          }
        })
        .filter((l) => l.text),
      ...pageLines,
    ],
    [run.events, run.record?.mode, pageLines],
  )

  const onBridge = (e: FrameEvent) => {
    if (e.type === 'preview:ready') {
      setBridge(true)
      // A new document starts closed to picking; open it again if the person had.
      if (editing) frame.current?.post({ type: 'preview:editable', active: true })
    }
    else if (e.type === 'preview:select')
      // The whole selector, as it will ride the next ask — never a shortened name
      // that hides what the page sent.
      setPicked([{ id: e.info.selector, kind: 'element', label: e.info.selector }])
    else if (e.type === 'preview:navigate') {
      // The bridge admits only a path on the framed site's own origin; kept as its path and query.
      const to = new URL(e.path, window.location.origin)
      setPage(`${to.pathname}${to.search}`)
    }
    else if (e.type === 'preview:console')
      setPageLines((was) => [...was.slice(-400), { id: `p${was.length}-${Date.now()}`, level: e.level, text: e.text, source: 'page' }])
  }

  // Steer an active session: sandbox sessions use POST /v1/agent/coding/{id}/steer, machines use /message.
  const steer = async (id: string, wordsOverride?: string) => {
    const words = (wordsOverride ?? draft).trim()
    if (!words) return
    const isSandbox = (run.record?.environment || 'sandbox') === 'sandbox'
    try {
      if (isSandbox) {
        await steerCoding(target, id, words)
        setDraft('')
        setPicked([])
        setAttached([])
        setNote(t('Redirecting session — keeping current work and continuing here'))
      } else {
        await message(target, id, words)
        setDraft('')
        setPicked([])
        setAttached([])
        setNote(t('Sent — recorded on this session'))
      }
    } catch (e) {
      setNote(cleanErrorText((e as Error).message))
    }
  }

  // A suggestion sends its own words; the composer, a draft with words in it.
  const send = async (text?: string) => {
    const ask = (text ?? draft).trim()
    if (busy || !ask) return
    setBusy(true)
    setNote('')
    try {
      // The page's own strings ride as quoted data, never as prose, and the page
      // is named only when the person chose it from the picker.
      const where = pages.find((p) => p.id === page)?.label
      const context = picked.length
        ? `\n\nThe person picked an element in the preview${where ? ` of the ${where} page` : ''}. Its CSS selector, as data: ${JSON.stringify(picked[0]!.id)}`
        : ''
      const prompt = compose(`${ask}${context}`, attached)
      // A project with no repository yet sends neither, and its first session makes one.
      let next: Awaited<ReturnType<typeof start>>
      try {
        next = await start(target, {
          prompt,
          // A repository with no site yet runs on the repository alone.
          project: project?.slug,
          repo,
          base: named ? forge.value?.branch : project?.branch,
          after: current ?? undefined,
          mode,
          // Enso named as itself: a turn that names none takes the person's saved coding default.
          model: mind.model,
          effort: level(mind.effort),
        })
      } catch (e) {
        if (current && isAlreadyRunningError(e)) {
          await steer(current, prompt)
          return
        }
        throw e
      }
      if (current) linkThread(current, next.session)
      setDraft('')
      setPicked([])
      setAttached([])
      setChosen(next.session)
      runs.reload()
    } catch (e) {
      setNote(cleanErrorText((e as Error).message))
    } finally {
      setBusy(false)
    }
  }

  const share = async () => {
    const link = project?.live || `${window.location.origin}${window.location.pathname}`
    setNote((await toClipboard(link)) ? t('Copied {link}', { link }) : link)
  }

  if (!signed) {
    return (
      <YStack flex={1} items="center" justify="center" px="$4">
        <YStack borderWidth={1} borderColor="var(--border-strong)" rounded="$4" bg="$panel" px="$6" py="$8" gap="$3" items="center" width="100%" maxW={440}>
          <SizableText size="$4" fontWeight="500" color="$ink" text="center">
            {t('Sign in to open this project.')}
          </SizableText>
          <Button size="sm" variant="primary" onPress={() => host.signIn?.()}>
            {t('Sign in')}
          </Button>
        </YStack>
      </YStack>
    )
  }

  // A repository of another organization is opened in that one.
  if (named && host.org && named.org.toLowerCase() !== host.org.toLowerCase()) {
    const member = host.memberships?.some((m) => m.toLowerCase() === named.org.toLowerCase()) && host.chooseOrg
    return (
      <YStack flex={1} items="center" justify="center" px="$4">
        <YStack borderWidth={1} borderColor="var(--border-strong)" rounded="$4" bg="$panel" px="$6" py="$8" gap="$3" items="center" width="100%" maxW={480}>
          <SizableText size="$4" fontWeight="500" color="$ink" text="center">
            {t('{repo} is in {org}, and you are working in {current}.', { repo: `${named.org}/${named.name}`, org: named.org, current: host.org })}
          </SizableText>
          {member ? (
            <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => host.chooseOrg?.(named.org)}>
              {t('Switch to {org}', { org: named.org })}
            </Button>
          ) : (
            <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => host.go(path({ kind: 'screen', screen: 'projects' }))}>
              {t('See this organization’s projects')}
            </Button>
          )}
        </YStack>
      </YStack>
    )
  }

  if (named ? !forge.loading && !forge.value : !all.loading && !project && !all.error) {
    return (
      <YStack flex={1} items="center" justify="center" px="$4">
        <YStack borderWidth={1} borderColor="var(--border-strong)" rounded="$4" bg="$panel" px="$6" py="$8" gap="$3" items="center" width="100%" maxW={480}>
          <SizableText size="$4" fontWeight="500" color="$ink" text="center">
            {named
              ? forge.error && !/not found|404/i.test(forge.error.message)
                ? forge.error.message
                : t('{org} has no repository named {name}.', { org: named.org, name: named.name })
              : host.org
                ? t('{org} has no project named {name}.', { org: host.org, name: slug })
                : t('There is no project named {name}.', { name: slug })}
          </SizableText>
          <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => host.go(path({ kind: 'screen', screen: named ? 'projects' : 'artifacts' }))}>
            {named ? t('See this organization’s projects') : t('See what this organization has built')}
          </Button>
        </YStack>
      </YStack>
    )
  }

  const chat = (
    <YStack data-slot="project-chat" flex={1} minH={0} position="relative">
      {/* Its first line sits level with the preview's toolbar beside it. */}
      <YStack flex={1} minH={0} overflow="scroll" px="$3" pt="$2" pb="$4" gap="$4" $md={{ pr: 0 }}>
        {ordered.length === 0 ? (
          runs.loading ? (
            <YStack gap="$3" aria-busy={true}>
              <SizableText size="$2" color="$soft">
                {t('Reading this project’s sessions…')}
              </SizableText>
              <Skeleton layout="block" width="84%" height={14} rounded="$2" />
              <Skeleton layout="block" width="66%" height={14} rounded="$2" />
              <Skeleton layout="block" width="92%" height={56} rounded="$3" />
            </YStack>
          ) : (
            <SizableText size="$2" color="$ink">
              {project?.live
                ? t('{name} is loaded — it is in the preview, and every run on it is under the clock above. Say what to change and it gets built.', { name: project.name })
                : stalled
                  ? t('{name} has been publishing for {minutes} minutes without finishing. Publish it again from the preview, or say what to change and it gets built.', { name: project?.name ?? '', minutes: Math.floor(ran / 60) })
                  : failed
                    ? t('{name} could not be published. Publish it again from the preview, or say what to change and it gets built.', { name: project?.name ?? '' })
                    : building
                      ? t('{name} is being published — it appears in the preview when its build finishes. Say what to change and it gets built.', { name: project?.name ?? '' })
                      : starter
                        ? t('{name} is loaded — the preview shows the {starter} starter until your copy is published. Say what to change and it gets built.', { name: project?.name ?? '', starter: starter.title })
                        : project
                          ? t('{name} is loaded and nothing is published yet. Say what to change and it gets built.', { name: project.name })
                          : named
                            ? t('{repo} is open on {branch}. Say what to change and it gets built there.', { repo: `${named.org}/${named.name}`, branch: home })
                            : t('{name} is loaded. Say what to change and it gets built.', { name: slug })}
            </SizableText>
          )
        ) : (
          ordered.map((r) => (
            <YStack key={r.id} gap="$2">
              <Turn run={r} open={r.id === current} onOpen={() => setChosen(r.id)} project={slug} />
              {r.id === current ? (
                <YStack gap="$1" paddingInlineStart="$2" borderInlineStartWidth={1} borderColor="$borderColor">
                  {fold(run.events.map((e) => ({ kind: e.kind, actor: who(e.actor), seq: e.seq, id: e.id, text: said(e, run.record?.mode, t) })).filter((b) => b.text)).map((b) => (
                    <SizableText key={b.key} size="$1" color="$soft">
                      {b.text}
                    </SizableText>
                  ))}
                  {pr.href ? (
                    <XStack items="center" gap="$2" flexWrap="wrap">
                      <Out href={pr.href} label={t('Open pull request {pr}', { pr: pr.label })}>
                        <SizableText size="$1" color="$ink" textDecorationLine="underline">
                          {t('Pull request {pr}', { pr: pr.label })}
                        </SizableText>
                        <ExternalLink size={11} />
                      </Out>
                      <Merge session={r.id} open={proposing} onMerged={merged} />
                    </XStack>
                  ) : null}
                </YStack>
              ) : null}
            </YStack>
          ))
        )}
      </YStack>
      {/* The ask: flush with the column's foot, as wide as the column less its gutter, the chips above it on the same edges. */}
      <YStack data-slot="project-ask" px="$3" pb="$3" gap="$2" minW={0} $md={{ pr: 0 }}>
        {/* A run from a workspace always runs in the sandbox, so every mode is honoured there. */}
        {note ? (
          <SizableText size="$1" color="$soft" role="status">
            {note}
          </SizableText>
        ) : null}
        {picked.length ? <Attachments items={picked} onRemove={() => setPicked([])} /> : null}
        <Composer
          column
          value={draft}
          onChange={setDraft}
          onSend={() => void (current && running ? steer(current) : send())}
          onStop={
            current && active
              ? () =>
                  void stop(target, current).then(
                    () => setNote(t('Stop requested — the session keeps its work on its branch')),
                    (e: unknown) => setNote(cleanErrorText((e as Error).message)),
                  )
              : undefined
          }
          busy={busy || (active && !draft.trim())}
          placeholder={running ? t('Steer or redirect this session…') : t('Ask Hanzo for edits')}
          label={t('Ask Hanzo for edits')}
          plus={{ upload: () => pick(attached, setAttached, setNote, t) }}
          carried={carrying(attached, setAttached)}
          mode={running ? null : { value: mode === 'plan' ? 'plan' : 'auto', set: (m) => setMode(m === 'plan' ? 'plan' : 'build'), items: MODES.filter((m) => m.id !== 'manual') }}
          mind={running ? null : mind}
          effort={running ? null : pacing(mind)}
          voice={voice}
        >
          {current && active && draft.trim() ? (
            <XStack
              render="button"
              data-slot="composer-stop"
              aria-label={t('Stop session')}
              {...({ title: t('Stop session') } as object)}
              onPress={() =>
                void stop(target, current).then(
                  () => setNote(t('Stop requested — the session keeps its work on its branch')),
                  (e: unknown) => setNote(cleanErrorText((e as Error).message)),
                )
              }
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
                {t('Stop')}
              </SizableText>
            </XStack>
          ) : null}
        </Composer>
      </YStack>
      {/* In the gutter between the chat and the work. */}
      {wide && !shut ? (
        <Gutter side="left" span={across} floor={CHAT_FLOOR} ceil={most} reset={CHAT_WIDTH} onSpan={setSplit} label={t('Resize the chat')} gap={GUTTER} />
      ) : null}
    </YStack>
  )

  // The preview stays loaded while another view is open, so coming back to it
  // is the page as it was left.
  const preview = (
    <YStack data-slot="preview" flex={1} minH={0} minW={0} gap="$2" display={view === 'preview' ? 'flex' : 'none'}>
      {/* The framed page's own controls, beside it: reload, preview source pill, the page, the width, and the page in a tab of its own. */}
      <XStack data-slot="preview-bar" items="center" gap="$1.5" minW={0} shrink={0} $max-md={{ px: '$2' }}>
        <XStack
          render="button"
          aria-label={t('Reload the preview')}
          {...({ title: t('Reload the preview') } as object)}
          onPress={fresh}
          p="$1.5"
          $max-md={{ minW: 40, minH: 40, items: 'center', justify: 'center' }}
          rounded="$3"
          shrink={0}
          hoverStyle={{ bg: '$hover' }}
        >
          <RefreshCcw size={16} />
        </XStack>
        {resolvedPreview.kind === 'sandbox' || resolvedPreview.kind === 'live' ? (
          <XStack
            render={sandboxUrl && project?.live ? 'button' : undefined}
            onPress={sandboxUrl && project?.live ? () => setPreferLive((v) => !v) : undefined}
            data-slot="preview-mode"
            data-kind={resolvedPreview.kind}
            items="center"
            gap="$1"
            px="$2"
            py="$0.5"
            rounded={999}
            bg="$edge"
            borderWidth={1}
            borderColor="$borderColor"
            shrink={0}
            hoverStyle={sandboxUrl && project?.live ? { bg: '$hover' } : undefined}
          >
            {resolvedPreview.kind === 'sandbox' ? <YStack width={6} height={6} rounded={99} bg="var(--state-online)" shrink={0} /> : <Globe size={11} opacity={0.75} />}
            <SizableText size="$1" color="$ink" numberOfLines={1}>
              {resolvedPreview.kind === 'sandbox' ? t('Sandbox preview') : t('Live site')}
            </SizableText>
          </XStack>
        ) : null}
        <PageSelect pages={pages} value={page} onChange={setPage} shrink={1} />
        <XStack flex={1} />
        <Views views={DEVICES} value={device} onChange={(d) => setDevice(d as 'desktop' | 'mobile')} label={t('Device')} labels="none" />
        {rawSrc ? (
          <Out href={rawSrc} label={t('Open in a new tab')}>
            <XStack items="center" gap="$1.5" px="$2" py="$1" rounded="$3" hoverStyle={{ bg: '$hover' }}>
              <SizableText size="$1" color="$soft" $max-md={{ display: 'none' }}>
                {t('Preview')}
              </SizableText>
              <ExternalLink size={15} />
            </XStack>
          </Out>
        ) : null}
      </XStack>
      <PreviewFrame
        ref={frame}
        src={src}
        title={t('{title} preview', { title })}
        device={device}
        onBridge={onBridge}
        empty={
          <YStack items="center" justify="center" p="$6" width="100%">
            <YStack items="center" gap="$2.5" px="$5" py="$6" rounded="$4" borderWidth={1} borderColor="var(--border-strong)" bg="$panel" width="100%" maxW={420}>
              <YStack width={36} height={36} rounded="$3" bg="$raised" items="center" justify="center" aria-hidden>
                {stalled || failed ? <AlertCircle size={18} color="var(--state-error)" /> : <Globe size={18} color="$soft" />}
              </YStack>
              <SizableText size="$3" fontWeight="500" color="$ink" text="center">
                {stalled
                  ? t('Publishing is taking too long')
                  : building
                    ? project?.forked
                      ? t('Publishing your copy…')
                      : t('Publishing…')
                    : failed
                      ? t('Publishing failed')
                      : t('Nothing deployed yet')}
              </SizableText>
              <SizableText size="$2" color="$soft" text="center">
                {stalled
                  ? t('Its build started {minutes} minutes ago and has not finished.', { minutes: Math.floor(ran / 60) })
                  : building
                    ? t('Its page appears here when the build finishes.')
                    : failed
                      ? why
                      : t('Publish this project and its page appears here.')}
              </SizableText>
              {spend.dialog}
              {(stalled || failed) && clone ? (
                <Button size="sm" variant="outline" borderColor="var(--border-strong)" disabled={redo.busy} onPress={() => void retry()}>
                  {redo.busy ? t('Publishing…') : t('Publish again')}
                </Button>
              ) : null}
              {redo.error ? (
                <SizableText size="$2" color="var(--state-error)" text="center" role="alert">
                  {redo.error}
                </SizableText>
              ) : null}
            </YStack>
          </YStack>
        }
        toolbar={
          bridge ? (
            <XStack
              render="button"
              aria-pressed={editing}
              aria-label={editing ? t('Stop picking elements') : t('Pick an element to edit')}
              {...({ title: editing ? t('Stop picking elements') : t('Pick an element to edit') } as object)}
              onPress={() => {
                const next = !editing
                setEditing(next)
                frame.current?.post({ type: 'preview:editable', active: next })
              }}
              minW={36}
              minH={36}
              items="center"
              justify="center"
              rounded="$10"
              focusVisibleStyle={RING}
            >
              <MousePointerClick size={16} />
            </XStack>
          ) : undefined
        }
      />
    </YStack>
  )

  const body =
    view === 'preview' ? null : view === 'files' ? (
      repo && root.error ? (
        <YStack p="$4">
          <SizableText size="$2" color="var(--state-error)" role="status">
            {t('Nothing to show for {repo} at {ref}: the forge answered “{reason}”.', { repo, ref, reason: root.error.message })}
          </SizableText>
        </YStack>
      ) : repo ? (
        <FileTree
          key={round}
          load={async (dir) => (await tree(target, repo, ref, dir)).map((e) => ({ path: e.path, kind: e.dir ? ('dir' as const) : ('file' as const) }))}
          value={file}
          onSelect={(p) => void openFile(p)}
          label={t('{repo} at {ref}', { repo, ref })}
        />
      ) : (
        <SizableText size="$2" color="$soft" p="$4">
          {t('This project has no repository yet. Its first run creates one.')}
        </SizableText>
      )
    ) : view === 'code' ? (
      <FileTabs
        files={files}
        value={file}
        onSelect={setFile}
        onClose={(p) => {
          setFiles((was) => was.filter((f) => f.path !== p))
          if (file === p) setFile(null)
        }}
        empty={
          <SizableText size="$2" color="$soft" p="$4">
            {t('Open a file from Files. Code here is read at {ref}; ask in the chat to change it.', { ref })}
          </SizableText>
        }
      />
    ) : (
      <YStack p="$4" gap="$2">
        {picked.length ? (
          picked.map((p) => (
            <XStack key={p.id} items="center" gap="$2.5" px="$3" py="$2.5" rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$panel">
              <MousePointerClick size={14} color="$soft" />
              <SizableText size="$2" color="$ink" fontFamily="$mono" flex={1} minW={0}>
                {p.label} — {p.id}
              </SizableText>
            </XStack>
          ))
        ) : (
          <YStack p="$5" rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel" items="center" gap="$2">
            <SizableText size="$2" color="$soft" text="center">
              {bridge
                ? t('Pick an element in the preview to see it here and attach it to your next ask.')
                : t('Layers are read through the preview bridge, and this page does not run it. Its elements cannot be listed from here.')}
            </SizableText>
          </YStack>
        )}
      </YStack>
    )

  const views = (
    <Views
      views={[CHAT, ...VIEWS]}
      value={pane === 'chat' && !wide ? 'chat' : view}
      onChange={(v) => {
        if (v === 'chat') return setPane('chat')
        setView(v as ViewId)
        setPane('view')
      }}
      label={t('View')}
    />
  )
  // On a phone the bar has no room for the views beside the project: there
  // they were a scroller of the bar's leftovers, 49px at 375 beside a project
  // named "Instrument", showing part of Chat and none of Code (hanzo-inc/app#305).
  // So below md they take a row of their own under the bar, in whichever pane
  // shows (one pane at a time there, so one row).
  const row = wide ? null : (
    <XStack data-slot="views-row" justify="center" px="$3" pb="$2" shrink={0} minW={0} overflowX="auto" $platform-web={{ scrollbarWidth: 'none' }}>
      {views}
    </XStack>
  )

  return (
    <>
      <Workspace
        start={
          // Shrinks with the bar, so the project's name truncates rather than
          // running under Publish on a phone.
          <XStack items="center" gap="$2" minW={0} shrink={1}>
            <XStack
              render="button"
              aria-label={t('All sessions')}
              {...({ title: t('All sessions') } as object)}
              onPress={() => host.go('')}
              p="$1"
              $max-md={{ minW: 40, minH: 40, items: 'center', justify: 'center' }}
              shrink={0}
            >
              <HanzoMark size={20} />
            </XStack>
            <ProjectChip name={title} onPress={() => setSwitcher(true)} />
            <XStack
              render="button"
              aria-label={t('History')}
              {...({ title: t('History') } as object)}
              onPress={() => setHistory(true)}
              p="$1.5"
              $max-md={{ minW: 40, minH: 40, items: 'center', justify: 'center' }}
              rounded="$3"
              shrink={0}
              hoverStyle={{ bg: '$hover' }}
            >
              <Clock size={16} />
            </XStack>
            {/* The chat folds away beside the work; on a phone the views switch to it instead. */}
            <XStack
              render="button"
              aria-label={shut ? t('Show the chat') : t('Hide the chat')}
              {...({ title: shut ? t('Show the chat') : t('Hide the chat') } as object)}
              aria-pressed={!shut}
              onClick={() => (cramped ? setAsked(!asked) : setCollapsed(!collapsed))}
              minW={32}
              minH={32}
              items="center"
              justify="center"
              rounded="$3"
              shrink={0}
              hoverStyle={{ bg: '$hover' }}
              focusVisibleStyle={RING}
              $max-md={{ display: 'none' }}
            >
              <PanelLeft size={16} />
            </XStack>
          </XStack>
        }
        middle={wide ? views : undefined}
        end={
          <XStack
            data-slot="project-header-actions"
            items="center"
            gap="$2"
            shrink={0}
            style={{ paddingInlineEnd: 'var(--hanzo-shell-top-right-inset, 0px)' }}
          >
            {/* Share gives way on a phone, where Publish is the one that matters. */}
            <XStack display="none" $md={{ display: 'flex' }} shrink={0}>
              <Button variant="outline" size="sm" onPress={() => void share()}>
                <Share2 size={14} /> {t('Share')}
              </Button>
            </XStack>
            <Button
              size="sm"
              variant="primary"
              aria-label={t('Publish')}
              disabled={!clone || !slug}
              onPress={() => setPublish({ repo: clone, title, ref, name: slug, project: slug, preview: project?.live || undefined })}
              shrink={0}
            >
              <Upload size={14} />
              {/* A glyph alone on a phone, where the bar's middle needs the room. */}
              <SizableText size="$2" color="inherit" $max-md={{ display: 'none' }}>
                {t('Publish')}
              </SizableText>
            </Button>
          </XStack>
        }
        chat={
          <>
            {row}
            {chat}
          </>
        }
        dock={
          <Console
            lines={lines}
            height={dock}
            onHeight={setDock}
            onClear={() => setPageLines([])}
            status={run.status || end.status || undefined}
            label={t('Console')}
          />
        }
        collapsed={shut}
        pane={pane}
        width={across}
        onLayout={(e) => setRoom(e.nativeEvent.layout.width)}
      >
        {row}
        {preview}
        {body}
      </Workspace>

      <Dialog open={history} onOpenChange={setHistory}>
        <DialogContent maxW={480}>
          <DialogTitle>{t('Sessions on {title}', { title })}</DialogTitle>
          <YStack gap="$1">
            {runs.value.length === 0 ? (
              <SizableText size="$2" color="$soft">
                {t('No sessions yet.')}
              </SizableText>
            ) : (
              runs.value.map((r) => {
                const active = r.id === current
                return (
                  <XStack
                    key={r.id}
                    render="button"
                    aria-current={active ? 'true' : undefined}
                    onPress={() => {
                      setChosen(r.id)
                      setHistory(false)
                    }}
                    justify="space-between"
                    items="center"
                    gap="$2"
                    px="$2.5"
                    py="$2"
                    minH={40}
                    $platform-web={{ textAlign: 'start' }}
                    focusVisibleStyle={RING}
                  >
                    <SizableText size="$2" color="$ink" fontWeight={active ? '500' : '400'} numberOfLines={1} flex={1}>
                      {r.title || t('Untitled session')}
                    </SizableText>
                    <SizableText size="$1" color="$soft" shrink={0}>
                      {r.status}
                    </SizableText>
                  </XStack>
                )
              })
            )}
          </YStack>
        </DialogContent>
      </Dialog>

      <Dialog
        open={switcher}
        onOpenChange={(o) => {
          setSwitcher(o)
          if (!o) setSwitchQ('')
        }}
      >
        <DialogContent maxW={420} gap="$3">
          <DialogTitle>{t('Projects')}</DialogTitle>
          {repos.value.length > 6 ? (
            <Input
              value={switchQ}
              onChangeText={setSwitchQ}
              placeholder={t('Find a project…')}
              aria-label={t('Find a project')}
              startAdornment={<Search size={14} color="$soft" />}
            />
          ) : null}
          <YStack gap="$1" maxH={420} overflow="scroll">
            {repos.error ? (
              <SizableText size="$2" color="$soft">
                {repos.error.message}
              </SizableText>
            ) : repos.loading && repos.value.length === 0 ? (
              <YStack gap="$2">
                <SizableText size="$2" color="$soft">
                  {t('Reading the forge…')}
                </SizableText>
                <Skeleton layout="block" height={36} rounded="$3" />
                <Skeleton layout="block" height={36} rounded="$3" />
                <Skeleton layout="block" height={36} rounded="$3" />
              </YStack>
            ) : filteredRepos.length === 0 ? (
              <SizableText size="$2" color="$soft" py="$2">
                {t('Nothing matches.')}
              </SizableText>
            ) : (
              filteredRepos.map((c) => {
                const here = Boolean(named) && c.name === named?.name
                return (
                  <XStack
                    key={`${c.org}/${c.name}`}
                    render="button"
                    aria-label={t('Open {repo}', { repo: `${c.org}/${c.name}` })}
                    aria-current={here ? 'page' : undefined}
                    onPress={() => {
                      setSwitcher(false)
                      setSwitchQ('')
                      host.go(path({ kind: 'repo', org: c.org || host.org || '', name: c.name }))
                    }}
                    px="$2.5"
                    py="$2"
                    minH={40}
                    items="center"
                    justify="space-between"
                    gap="$2"
                    rounded="$3"
                    hoverStyle={{ bg: '$hover' }}
                    $platform-web={{ textAlign: 'start' }}
                    focusVisibleStyle={RING}
                  >
                    <SizableText size="$2" color="$ink" numberOfLines={1}>
                      {c.name}
                    </SizableText>
                    {here ? <Check size={14} color="$ink" /> : null}
                  </XStack>
                )
              })
            )}
          </YStack>
          <Button
            variant="outline"
            size="sm"
            onPress={() => {
              setSwitcher(false)
              setSwitchQ('')
              host.go(path({ kind: 'screen', screen: 'projects' }))
            }}
          >
            {t('All projects')}
          </Button>
        </DialogContent>
      </Dialog>

      <Publish source={publish} onClose={() => setPublish(null)} onLive={fresh} />
    </>
  )
}
