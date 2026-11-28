/**
 * New: the empty state. A heading at the top of the column, and at the foot of
 * the pane the composer — where the run goes (Cloud: the sandbox with the
 * codebase's environment; or one of the org's machines, under remote control),
 * which repository and branch it starts from, and the ask itself — the one
 * composer Chat draws too (@hanzo/ui/chat): Dev hands it the three pills, the
 * chips that go to its repositories, issues and templates, and its commands.
 *
 * Sending opens a conversation (api/talk.ts) and goes to it: the agent answers,
 * and decides for itself when the ask needs a sandbox or a coding run — on the
 * repository, branch and machine chosen here, and only planning when Plan is on.
 * A repository is never a gate: a run with none works in a new folder, in the
 * sandbox or on one of the org's machines (hanzo-inc/cloud issue 779), and
 * GitHub can be connected later. On a host that has the computer (Hanzo
 * Desktop, `host.local`), New also offers This computer: the run works in a
 * folder there, Auto picking the folder used last (local.ts), and needs no
 * repository at all (hanzo-inc/app issues 309 and 310). On a machine whose owner lets the web use some of its
 * folders, the run can work in one of those instead of a repository (shared.ts): Auto is the folder the machine's
 * linked shell is open in, when it is one of them. The choices a person made here are kept in this
 * browser, per org, so the next New starts where the last one did — and a kept
 * repository New can no longer offer is let go, so the next run is not sent
 * back to it. A repository can also be given by its address (Use a git URL).
 *
 * Its words are the reader's language (@hanzo/i18n `t`, the target is
 * `target`); a codebase's, a machine's and a branch's names are drawn as given.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { CircleDot, Cloud, Code, FolderGit2, FolderOpen, GitBranch, Globe, Laptop, LayoutTemplate, Link2, Monitor, Play } from '@hanzogui/lucide-icons-2'
import { Button, Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@hanzo/ui'
import { Composer, MEASURE, type Command, type Shelf } from '@hanzo/ui/chat'
import { BranchSelect, ChipSelect, FooterLink, RepoSelect, type Repo as RowRepo } from '@hanzo/ui/product'
import { useLimits } from '@hanzo/ui/product/useLimits'
import { useDictation, useTalk, useVoice } from '@hanzo/voice'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'

import { Refusal, told } from './api/call.ts'
import { start, unhonoured, type Mode } from './api/coding.ts'
import { list as running, type Session } from './api/sessions.ts'
import { say } from './api/talk.ts'
import { asRepo, chosen, codebases, one, type ForgeRepo } from './api/codebases.ts'
import { heard, repos as githubRepos } from './api/github.ts'
import { plain, read, SETUP, unset, type Environment } from './api/environment.ts'
import { homes as readHomes, type Home, type Homes } from './api/work.ts'
import { SetupDialog } from './environment.tsx'
import { limits as readLimits } from './api/models.ts'
import { displayPlace, ready, SANDBOX, type Place } from './api/places.ts'
import { useDeviceNames } from './names.ts'
import type { At } from './address.ts'
import { Address } from './ask.tsx'
import { asHub, gone, isForge, isHub, settle, type HubRepo } from './choice.ts'
import { useCached, useKept, usePlaces, useRead } from './data.ts'
import { useDevMode } from './dev-mode.ts'
import { takeHandoff } from './handoff.ts'
import { useHost, useLocal, useSite, useTarget } from './host.tsx'
import { failed, HERE, named, recent, where, type Folder } from './local.ts'
import { aim, NOT_GRANTED, shell } from './shared.ts'
import { plainAnswer } from './plain.ts'
import { useRedesign } from './redesign.tsx'
import { urlOf } from './site.ts'
import { level, pacing, useMind } from './mind.ts'
import { usePrefs } from './prefs.tsx'
import { Publish, type Source } from './publish.tsx'
import { path } from './route.ts'
import { useCarry } from './carry.tsx'
import { DEV_MODES } from './gear.ts'
import { Panel, PanelToggle } from './panel.tsx'
import { PAGE, useDeck, useKey } from './tabs.ts'
import { taken } from './suggestions.tsx'
import { carrying, compose, pick, read as readFiles, type Attached } from './tools.tsx'
import { about, asked as critiqued, attach as asFile, critique, local as onThisComputer, named as pagesIn, read as readPage, shown as shownPage } from './page.ts'
import { useEar, useSpoken } from './voice.ts'

/** One vocabulary for the mode, on New and in a workspace: English, marked (`phrase`), drawn through `t`. */
export const MODES = [
  { id: 'build', label: phrase('Build'), hint: phrase('Edits, commits and pushes a branch') },
  { id: 'plan', label: phrase('Plan'), hint: phrase('Plans the change and writes nothing') },
] as const

/** What a person chose last time, per org. */
interface Kept {
  /** `given` marks a GitHub repository given by its address (choice.ts `HubRepo`). */
  repo: (RowRepo & { forge?: boolean; github?: boolean; clone?: string; given?: boolean }) | null
  branch: string
  place: string
  /** The folder chosen, by its path: on This computer one of its own (local.ts), on a machine one its owner granted (shared.ts); '' is Auto. */
  folder: string
  mode: Mode
  ask: string
  /** The issue the handed-over words are, by the number the run's pull request closes; 0 for none. */
  issue: number
}

/** The org's repositories before they have been read: no answer, so nothing is judged from it. */
const NOHOMES: Homes = { homes: [], whole: false }

const FIRST: Kept = { repo: null, branch: '', place: '', folder: '', mode: 'build', ask: '', issue: 0 }

/** No folders: a place that shares none. */
const NONE: readonly string[] = []

/** This computer, as a place: always there on a host that has it. */
const HERE_PLACE: Place = { id: HERE, label: 'This computer', kind: 'here', status: 'online', capacity: '' }

/** What a run on this computer is doing, or did: its folder, its ask, and once it finished, what the agent said and did. */
interface Ran {
  folder: Folder
  ask: string
  going: boolean
  ok: boolean
  /** What the agent said at the end. */
  output: string
  /** What it did on the way: shown in Developer mode only. */
  process: string
}

/**
 * What was typed here and not sent, kept in this tab.
 *
 * Signing in leaves the page for hanzo.id and comes back (app/enter.ts), and the
 * sentence is still in the composer after. A draft its person SENT while signed
 * out is sent on the return: pressing Send was the ask, and the sign-in stood
 * between it and the run. Only this page writes `sent`, so no link can start a
 * run on somebody's behalf — an address carrying words only fills the composer.
 *
 * sessionStorage, because a draft belongs to the tab it was typed in and not to
 * the next person at this browser; a browser that refuses storage keeps it for
 * the life of the page, as before.
 */
export interface Unsent {
  draft: string
  files: Attached[]
  sent?: boolean
}

const UNSENT = 'hanzo.build.unsent'

let held: Unsent = { draft: '', files: [] }

function unsent(): Unsent {
  try {
    const v = JSON.parse(window.sessionStorage.getItem(UNSENT) ?? 'null') as Unsent | null
    if (v && typeof v.draft === 'string' && Array.isArray(v.files)) return v
  } catch {
    /* a browser that refuses storage, or a value that is not ours */
  }
  return held
}

/** Hold what New has unsent, or forget it once there is nothing. */
export function hold(u: Unsent): void {
  held = u
  try {
    if (!u.draft && !u.files.length) window.sessionStorage.removeItem(UNSENT)
    else window.sessionStorage.setItem(UNSENT, JSON.stringify(u))
  } catch {
    /* kept for the life of the page */
  }
}

/**
 * A run on this computer: where it works, and once it ends, what the agent said, in words (`plainAnswer`). What it did
 * on the way (its commands and what they printed) is drawn only in Developer mode; the first run waits while the host
 * fetches the agent, and says so.
 */
function RanHere({ ran, setup, devMode, onCancel }: { ran: Ran; setup: number | null; devMode: boolean; onCancel?: () => void }) {
  const { t } = useI18n()
  const fetching = ran.going && setup !== null && setup < 100
  const head = ran.going
    ? t('Working in {name}…', { name: ran.folder.name })
    : ran.ok
      ? t('Done in {name}', { name: ran.folder.name })
      : t('Stopped in {name}', { name: ran.folder.name })
  const said = ran.output ? plainAnswer(ran.output, ran.folder, t) : ''
  return (
    <YStack data-slot="ran-here" role="status" aria-live="polite" gap="$1.5" mx="$3" px="$3" py="$2.5" rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$background" minW={0}>
      <XStack items="center" gap="$2" minW={0}>
        <FolderOpen size={13} color="$soft" />
        <SizableText size="$2" color="$ink" fontWeight="500" numberOfLines={1} flex={1} minW={0}>
          {head}
        </SizableText>
      </XStack>
      {fetching ? (
        <XStack items="center" gap="$2" minW={0}>
          <SizableText data-slot="ran-setup" size="$1" color="$soft" flex={1} minW={0}>
            {t('Setting up Hanzo Dev on this computer… {percent}%', { percent: setup })}
          </SizableText>
          {onCancel ? (
            <Button size="xs" variant="ghost" data-slot="ran-setup-cancel" onPress={onCancel}>
              {t('Cancel')}
            </Button>
          ) : null}
        </XStack>
      ) : null}
      {said ? (
        <YStack maxH={320} overflowY="auto" minW={0}>
          <SizableText data-slot="ran-answer" size="$2" color="$ink" whiteSpace="pre-wrap" wordWrap="break-word">
            {said}
          </SizableText>
        </YStack>
      ) : !ran.going && !ran.ok ? (
        <SizableText data-slot="ran-answer" size="$2" color="$ink">
          {t('Hanzo Dev stopped before it finished. Try again.')}
        </SizableText>
      ) : null}
      {devMode && ran.process ? (
        <YStack data-slot="ran-process" gap="$1" minW={0}>
          <SizableText size="$1" color="$soft" fontWeight="500">
            {t('What Hanzo ran')}
          </SizableText>
          <SizableText size="$1" color="$soft" fontFamily="$mono" whiteSpace="pre-wrap" wordWrap="break-word" maxH={240} overflowY="auto" data-i18n-ltr="">
            {ran.process}
          </SizableText>
        </YStack>
      ) : null}
    </YStack>
  )
}

export function Landing({ onStarted }: { onStarted: (session: string) => void }) {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const media = useMedia()
  const wide = Boolean(media.md)
  const signed = Boolean(host.person)
  const [devMode] = useDevMode()
  const { prefs, save: savePrefs } = usePrefs()
  const local = useLocal()
  const site = useSite()
  const [stored, keep] = useKept<Partial<Kept> | null>(`hanzo.build.new.${host.org ?? 'none'}`, null)
  // What was chosen here, over the person's coding defaults (Settings → Code), over
  // the first run's. Field by field: a codebase or an issue handed over from its own
  // screen keeps only the codebase and the words, and the rest still has a value.
  // On a host that has the computer, the first run's place is This computer.
  const kept = settle(local ? { ...FIRST, place: HERE } : FIRST, prefs.code, stored)
  const set = (patch: Partial<Kept>) => keep({ ...kept, ...patch })

  const [was] = useState(unsent)
  const [draft, setDraft] = useState(kept.ask || was.draft)
  // The issue handed over with the words, while the run is still on its repository.
  const [issue, setIssue] = useState(kept.ask ? kept.issue : 0)
  const [files, setFiles] = useState<Attached[]>(was.files)
  useEffect(() => hold({ draft, files }), [draft, files])
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState('')
  const [adding, setAdding] = useState<Source | null>(null)
  const [offering, setOffering] = useState(false)
  const [desk, setDesk] = useState(false)
  const deck = useDeck('dev-new', { tabs: [{ id: 'tab-1', kind: PAGE, title: '' }], at: 'tab-1' })
  useKey(() => setDesk((d) => !d))
  // Asking for a repository's address, for one the lists do not show.
  const [giving, setGiving] = useState(false)
  // The repository and branch lists are read again each time they open.
  const [picking, setPicking] = useState(false)
  const [branching, setBranching] = useState(false)

  // A codebase or an issue hands its words over once. Left in the kept choice,
  // every later visit to New would put them back.
  useEffect(() => {
    if (kept.ask || kept.issue) set({ ask: '', issue: 0 })
    // Chat's "Open in Dev" leaves its prompt and code here (handoff.ts).
    const handed = takeHandoff(window.sessionStorage)
    if (handed) {
      setDraft(handed)
      setNote(t('Imported from Chat — ready to run in Dev.'))
    }
    // The handover is read on this mount only.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const places = usePlaces(target, signed)
  const names = useDeviceNames(signed ? target : null)
  // The model and effort Chat shares, and dictation into the ask.
  const { limits } = useLimits(signed ? (signal) => readLimits(target, signal) : null, target)
  const mind = useMind(target, (limits as { plan?: string } | null)?.plan, host.topUp)
  const voice = useVoice({ speech: useEar(target), language: useSpoken(), ...useDictation(draft, setDraft) })
  const talk = useTalk({ baseUrl: target.api, token: () => target.token(), ...(target.org ? { org: target.org } : {}) })
  // What the run carries beside the ask: skills, memory and the mode, as Chat's turns do.
  const carry = useCarry(target, '', (tab) => host.go(path({ kind: 'customize', tab })), {
    draft,
    setDraft,
    // A run reads text: a file that is one rides the ask, a picture goes with a chat.
    attach: (list) => {
      const text = list.filter((f) => !f.type.startsWith('image/'))
      if (text.length < list.length) setNote(t('A run reads text files; send a picture in Chat.'))
      if (text.length) {
        const dt = new DataTransfer()
        for (const f of text) dt.items.add(f)
        void readFiles(files, dt.files).then(({ files: next, note: why }) => {
          setFiles(next)
          if (why) setNote(why)
        })
      }
    },
    go: (href) => (href.startsWith('http') ? host.open(href) : host.go(href)),
  }, DEV_MODES)
  const mode: Mode = carry.mode.value === 'plan' ? 'plan' : 'build'
  // The chosen codebase's environment, so New can say when it has none yet.
  const codebase = isForge(kept.repo) ? kept.repo.name : ''
  // The org's repositories on the forge: what a run and an environment can name.
  const homes = useCached<Homes>(signed ? `${host.org}:homes` : null, (signal) => readHomes(target, signal), NOHOMES)
  // A kept codebase the org's whole list does not hold — a board's key once handed over as one — is not offered as one.
  const stranger =
    codebase !== '' && homes.value.whole && !homes.value.homes.some((h) => h.name.toLowerCase() === codebase.toLowerCase())
  // Read once the list has answered (or failed), so a name that is not the org's is never asked for.
  const settled = homes.value !== NOHOMES || homes.error !== null
  const found = useRead(signed && codebase && settled && !stranger ? () => read(target, codebase) : null, null as Environment | null, [target, signed, codebase, settled, stranger])
  // A read kept from the codebase before is not this one's.
  const env = { ...found, value: found.value && found.value.repo === codebase && !stranger ? found.value : null }
  // This computer, where the host has it: the run goes to a folder here and never to the platform.
  const here = local !== null && kept.place === HERE
  const place: Place = here ? SANDBOX : (places.value.find((p) => p.id === kept.place) ?? SANDBOX)
  // The folders this computer opened, most recent first, and the one a run here goes to: the one chosen, else Auto's.
  const folders = local ? recent(local.folders()) : []
  // The folder Auto picks is the one the words typed so far are about (local.ts `route`), so the pill says where Send goes.
  const folderAt = where(folders, kept.folder, draft)
  // On a machine: the folders its owner lets the web use, the one its linked shell is open in, and the one a run goes to.
  const granted = here ? NONE : (place.folders ?? NONE)
  const shells = useRead(granted.length ? () => running(target, { status: 'running', limit: 100 }) : null, [] as Session[], [target, place.id, granted.length > 0])
  const aimed = aim(granted, kept.folder, shell(shells.value, place))
  // The machine a run was refused on for a folder it does not share: there, the folders it does are offered.
  const [offered, setOffered] = useState('')
  const [ran, setRan] = useState<Ran | null>(null)
  // How far the agent's first-run setup has got, while the host fetches it.
  const [fetched, setFetched] = useState<number | null>(null)
  // The folders found on the computer arrive after the first read.
  const [, folded] = useState(0)
  useEffect(() => local?.changed?.(() => folded((n) => n + 1)), [local])
  // An address pasted here is offered as a redesign of that site (redesign.tsx): the folder the person chose is built in, Auto makes a new one.
  const redesign = useRedesign({
    local,
    site,
    here,
    draft,
    chosen: folderAt.auto ? null : folderAt.folder,
    carried: carry.text,
    devMode,
    started: () => {
      setDraft('')
      setFiles([])
      setNote('')
    },
  })
  // Why this mode cannot run where it is sent: a plan is the sandbox's, on a machine and on this computer alike.
  const refused = here ? (mode === 'plan' ? t('A plan runs in the cloud workspace. On this computer, switch to Build.') : '') : unhonoured(mode, place.id, t)

  // The kept repository is checked against what New can offer now (choice.ts
  // `gone`): on arrival, and again each time the GitHub connection changes
  // anywhere on the page (api/github.ts `heard`). One that is gone is let go with
  // its branch and the issue it would close, so the composer holds none and Send
  // opens the picker — a repository let go on GitHub, or a GitHub disconnected,
  // is not where the next run goes.
  const address = isHub(kept.repo) ? kept.repo.full_name : ''
  const given = kept.repo?.given === true
  // The choice as it is when an answer lands, so nothing chosen while it was read is written back over.
  const latest = useRef(kept)
  latest.current = kept
  const drop = useCallback(() => {
    setIssue(0)
    keep({ ...latest.current, repo: null, branch: '' })
  }, [keep])
  const [looks, look] = useState(0)
  const { reload: readHomesAgain } = homes
  useEffect(
    () =>
      heard(() => {
        look((n) => n + 1)
        readHomesAgain()
      }),
    [readHomesAgain],
  )
  // A codebase is judged by the org's list above. That list is drawn at once from
  // what this page read before, so only its own answer on this visit judges: read
  // to the end, neither refused nor let go as slow, and from both of its sources.
  const fresh = !homes.loading && homes.error === null && homes.value !== NOHOMES && homes.value.whole
  const forgeNames = useMemo(() => (fresh ? homes.value.homes.map((h) => h.name) : null), [fresh, homes.value])
  useEffect(() => {
    if ((codebase && gone(latest.current.repo, { forge: forgeNames, github: null })) || stranger) drop()
  }, [codebase, forgeNames, stranger, drop])
  // A GitHub repository, by GitHub's listing narrowed to its own address, so one
  // page is the whole answer for it however many the connection grants. A read
  // refused is no list; one this page has moved on from is not heard.
  useEffect(() => {
    if (!signed || !address || given) return
    const ctl = new AbortController()
    void githubRepos(target, { q: address.slice('github.com/'.length), limit: 100 }, ctl.signal)
      .catch(() => null)
      .then((github) => {
        if (!ctl.signal.aborted && gone(latest.current.repo, { forge: null, github })) drop()
      })
    return () => ctl.abort()
  }, [signed, target, address, given, looks, drop])

  const known = useRef(new Map<string, ForgeRepo>())
  const hubs = useRef(new Map<string, HubRepo>())

  // The forge's codebases, then the repositories the org's GitHub installation
  // grants. A GitHub row is chosen as GitHub's address, so a run on it clones,
  // pushes and proposes on GitHub, where its people work. A GitHub read that
  // fails leaves the forge's list standing.
  const loadRepos = useCallback(
    async (q: string) => {
      const [page, hub] = await Promise.all([codebases(target, q), githubRepos(target, { q }).catch(() => null)])
      const repos = page.map(asRepo)
      for (const r of repos) known.current.set(r.name, r)
      const github = (hub?.repos ?? []).map(asHub)
      for (const r of github) hubs.current.set(r.full_name.toLowerCase(), r)
      return { repos: [...repos.map((r) => ({ ...r, source: 'forge' })), ...github.map((r) => ({ ...r, source: 'github' }))], next: null }
    },
    [target],
  )

  // Drawn only once a forge codebase is chosen; one the forge no longer lists offers its default branch.
  const home = kept.repo?.default_branch || 'main'
  const loadBranches = useCallback(
    async (q: string) => {
      const got = await one(target, codebase)
      const needle = q.trim().toLowerCase()
      const names = (got?.branches ?? [home]).filter((b) => !needle || b.toLowerCase().includes(needle))
      return { branches: names.map((name) => ({ name })), next: null }
    },
    [target, codebase, home],
  )

  const hostOf = (u: string) => {
    try {
      return new URL(u).hostname
    } catch {
      return ''
    }
  }

  /**
   * The web pages an ask names, read and ready to ride it as files (page.ts). `only` narrows which addresses are
   * read; the rest go to the run as words. Strict (a run on the platform): a page that could not be read is said
   * and the answer is null, so no run starts on a page it could not read. Not strict (a run on this computer, which
   * may be about anything): what was read rides the ask, and `missed` says what was not.
   */
  const pagesFor = async (said: string, only: (url: string) => boolean = () => true, strict = true): Promise<{ files: Attached[]; missed: string } | null> => {
    const want = pagesIn(said).filter(only)
    if (!want.length) return { files: [], missed: '' }
    setNote(t('Reading {page}…', { page: want.map(shownPage).join(', ') }))
    const read = await Promise.allSettled(want.map((u) => readPage(target, u)))
    const files = read.flatMap((r) => (r.status === 'fulfilled' ? [asFile(r.value)] : []))
    const failed = read.find((r): r is PromiseRejectedResult => r.status === 'rejected')
    const missed = failed ? told(failed.reason, t) : ''
    if (missed && strict) {
      setNote(missed)
      return null
    }
    setNote('')
    return { files, missed }
  }

  // The composer sends only a draft with words in it, and not while a send is out.
  const send = async (said: string = draft) => {
    // A site's address, alone or asked to be redesigned, starts a redesign (redesign.tsx); any other ask is a run.
    const asked = redesign.can ? urlOf(said) : null
    if (asked) return void (await redesign.start(asked))
    // This computer needs no sign-in to the platform and no repository: the agent runs in the folder, here.
    if (here && local) return void (await sendHere(said))
    if (!signed) {
      hold({ draft: said, files, sent: true })
      host.signIn?.()
      return
    }
    const repo = isForge(kept.repo) ? kept.repo : null
    const hub = isHub(kept.repo) ? kept.repo : null
    // No repository is no gate: a machine run with none works in a new folder there (cloud issue 779).
    // A plan or a setup routed to a machine is refused here, in its own words, before it leaves the page.
    const why = unhonoured(mode, place.id, t)
    if (why) {
      setNote(why)
      return
    }
    setBusy(true)
    setNote('')
    setOffered('')
    try {
      // A page the ask names is read first, and rides it: a run never starts on a page that could not be read.
      // Only an ask about its pages reads them (page.ts `about`); any other link is words for the agent.
      const pages = about(said) ? await pagesFor(said) : { files: [], missed: '' }
      if (!pages) return
      // What the plugins that are on add: words for the run, or an answer of their own.
      const prepped = carry.preparing ? await carry.prep({ text: said.trim(), files: [], thread: '' }, new AbortController().signal) : {}
      // An answer of a plugin's own (a deck) is the answer: no run is started for it.
      if (prepped.reply) {
        // Its words, without the page it drew (Chat shows that in the side panel).
        setNote(prepped.reply.split('```')[0]!.trim())
        return
      }
      // The first thing said opens a conversation (api/talk.ts) and starts nothing:
      // the agent answers, and leases a sandbox or starts a coding run only when the
      // ask needs one — on the repository, branch, machine and Plan chosen here.
      // Skills, memories and plugin system prompts travel out-of-band as
      // developerInstructions so the user prompt stays clean and cacheable.
      const developerInstructions = [carry.text, prepped.system ?? ''].filter(Boolean).join('\n\n')
      const input = compose(said.trim(), [...files, ...pages.files])
      // A folder on the machine is a coding run there, in that folder: a conversation names no folder.
      const sent = aimed.dir
        ? await start(target, {
            prompt: input,
            targetId: place.id,
            dir: aimed.dir,
            mode,
            model: mind.model,
            effort: level(mind.effort),
            ...(developerInstructions ? { developerInstructions } : {}),
          })
        : await say(target, {
            input,
            ...(developerInstructions ? { developerInstructions } : {}),
            // The name alone. The org rides the request, and a slash is not a repo name.
            // An empty base or place is not sent: the default branch, and the sandbox.
            repo: repo?.name ?? hub?.full_name,
            base: repo ? kept.branch : hub ? kept.branch || hub.default_branch : undefined,
            target: place.id,
            // The mode the composer shows: Auto starts a build, Manual asks first, Plan only plans.
            mode: carry.mode.value === 'manual' ? 'manual' : mode === 'plan' ? 'plan' : 'auto',
            model: mind.model,
            effort: level(mind.effort),
            issue,
          })
      setDraft('')
      setFiles([])
      setIssue(0)
      onStarted(sent.session)
    } catch (e) {
      // The owner stopped sharing the folder, or never did: say how to share it, and offer the ones shared now.
      if (e instanceof Refusal && e.code === NOT_GRANTED) {
        setNote(t('That folder is not shared with the web. Share it from Hanzo Desktop or `hanzo link grant {folder}`', { folder: aimed.dir }))
        setOffered(place.id)
        places.reload()
        shells.reload()
        return
      }
      setNote(plain(e, aimed.dir ? named(aimed.dir) : (repo?.name ?? hub?.full_name ?? (place.id ? activePlaceDisplay.title : t('a new project'))), 'run', t))
    } finally {
      setBusy(false)
    }
  }

  /**
   * A run on this computer: in the folder chosen, else Auto's (the one the ask is about, else the one used last), else one
   * the person picks now, once. The agent reads, edits and runs there and nowhere else; what it said is shown under the
   * composer when it ends, and what it did only in Developer mode.
   */
  const sendHere = async (said: string) => {
    if (!local || busy || redesign.busy) return
    if (refused) {
      setNote(refused)
      return
    }
    let folder = where(folders, kept.folder, said).folder
    if (!folder) {
      setNote(t('Choose a folder for this run'))
      folder = await local.pick().catch(() => null)
      if (!folder) return
      set({ place: HERE, folder: '' })
    }
    local.use(folder.path)
    const ask = said.trim()
    // A public page is read for the run while someone is signed in; an address on this computer (a localhost
    // server) is the agent's own to open, and a signed-out run reads nothing from the platform.
    setBusy(true)
    const pages = (signed && about(ask) ? await pagesFor(ask, (u) => !onThisComputer(hostOf(u)), false) : null) ?? { files: [], missed: '' }
    const prompt = [carry.text, compose(ask, [...files, ...pages.files])].filter(Boolean).join('\n\n')
    setBusy(true)
    setNote(pages.missed ? t('{why} The run goes on without that page.', { why: pages.missed }) : '')
    setRan({ folder, ask, going: true, ok: true, output: '', process: '' })
    setFetched(null)
    try {
      const done = await local.run(folder.path, prompt, setFetched)
      setRan({ folder, ask, going: false, ok: done.ok, output: done.output.trim(), process: (done.process ?? '').trim() })
      setDraft('')
      setFiles([])
    } catch (e) {
      setRan(null)
      setNote(failed(e, t))
    } finally {
      setFetched(null)
      setBusy(false)
    }
  }

  // A draft sent while signed out goes once its person is in, and only once.
  const owed = useRef(was.sent === true)
  useEffect(() => {
    if (!signed || !owed.current || !draft.trim()) return
    owed.current = false
    void send()
    // Sent once, on the sign-in that it waited for.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [signed])

  // A run a suggestion in Chat left for Dev starts on arrival, once its person is known.
  const [handed] = useState(() => ({ prompt: taken('task') }))
  useEffect(() => {
    if (!signed || !handed.prompt) return
    const p = handed.prompt
    handed.prompt = ''
    setDraft(p)
    void send(p)
    // Started once, on the sign-in that it waited for.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [signed])

  /** Start the agent that finds a codebase's environment, and open it: its run is the progress. A refusal is the dialog's to say. */
  const setup = async (repo: string) => {
    const run = await start(target, { prompt: SETUP, repo, mode: 'setup', model: mind.model, effort: level(mind.effort) })
    onStarted(run.session)
  }

  /** The codebase a dialog chose, chosen here too. */
  const choose = (h: Home) => {
    const row = chosen({ owner: h.label.split('/')[0] ?? '', name: h.name, full_name: h.label, default_branch: h.branch })
    setIssue(0)
    set({ repo: row, branch: row.default_branch })
  }

  /**
   * The repository at an address the person gave, chosen as GitHub's — as a row
   * of the GitHub list is, so the run clones, pushes and proposes there — and
   * kept as given. One the list has shown keeps the branch and privacy it said;
   * any other starts from its default branch.
   */
  const give = (at: At) => {
    const listed = hubs.current.get(`github.com/${at.owner}/${at.name}`.toLowerCase())
    const row: HubRepo = {
      ...(listed ?? asHub({ owner: at.owner, name: at.name, full_name: `${at.owner}/${at.name}`, private: false, default_branch: '', pushed_at: '', installation_id: 0 })),
      given: true,
    }
    setIssue(0)
    setNote('')
    set({ repo: row, branch: row.default_branch })
  }

  // Cloud is the managed workspace, set up by the chosen project's environment; the rest are the org's machines.
  const placeItems = useMemo(
    () => [
      // This computer first, where the host has it: a folder here is the nearest place a run can go.
      ...(local
        ? [
            {
              id: HERE,
              label: t('This computer'),
              hint: folderAt.folder ? folderAt.folder.name : t('A folder on this computer'),
              detail: undefined as string | undefined,
              icon: <Laptop size={13} color="$soft" />,
              disabled: false,
              place: HERE_PLACE,
            },
          ]
        : []),
      ...(places.value.length ? places.value : [SANDBOX]).map((p) => {
        const display = displayPlace(p, { devMode, say: t, names })
        return {
          id: p.id || 'sandbox',
          label: display.title,
          hint: p.id
            ? devMode
              ? [p.status, p.capacity].filter(Boolean).join(' · ')
              : p.status === 'online'
                ? t('Online')
                : p.status
            : devMode
              ? ''
              : t('Cloud workspace'),
          detail: display.detail,
          icon: p.id ? <Monitor size={13} color="$soft" /> : <Cloud size={13} color="$soft" />,
          disabled: !ready(p),
          place: p,
        }
      }),
    ],
    [places.value, devMode, t, names, local, folderAt.folder],
  )

  const activePlaceDisplay = useMemo(
    () => displayPlace(place, { devMode, say: t, names }),
    [place, devMode, t, names],
  )
  const placeTooltip = here
    ? t('Runs on this computer, in the folder you choose. No repository needed.')
    : aimed.dir
      ? t('Runs on this machine, in a folder its owner shares with the web. No repository needed.')
      : place.id
      ? t('Runs on this machine, in a new folder unless you choose a repository')
      : t('Runs automatically in your Hanzo cloud workspace')
  const placeHardwareHint = place.id && !devMode && !here ? activePlaceDisplay.detail : undefined
  // The place chosen, as the pill and the `+` menu mark it.
  const placeId = here ? HERE : place.id

  // The Folder pill's choices: Auto, then on This computer every folder it opened, most recent first, and on a
  // machine the folders its owner shares with the web, and no others.
  const AUTO = '__auto__'
  const folderItems = useMemo(() => {
    const item = (id: string, label: string, hint: string) => ({ id, label, hint, icon: <FolderOpen size={13} color="$soft" /> })
    return here
      ? [item(AUTO, t('Auto'), t('The folder your ask is about, else the one you used last')), ...folders.map((f) => item(f.path, f.name, f.path))]
      : [item(AUTO, t('Auto'), t('The folder your shell there is open in, when it is shared; else a new folder')), ...granted.map((f) => item(f, named(f), f))]
  }, [here, folders, granted, t])
  // The folder the run goes to, as the pill says it: Auto's or the one chosen.
  const aimedAt = here
    ? { name: folderAt.folder?.name ?? '', id: folderAt.auto ? AUTO : folderAt.folder?.path, auto: folderAt.auto }
    : { name: aimed.dir ? named(aimed.dir) : '', id: aimed.auto ? AUTO : aimed.dir, auto: aimed.auto }
  const chooseFolder = async () => {
    if (!local) return
    const f = await local.pick().catch(() => null)
    if (f) set({ place: HERE, folder: f.path })
  }

  const branch = kept.branch || kept.repo?.default_branch || 'main'
  const forge = isForge(kept.repo)
  const hasRepo = Boolean(isForge(kept.repo) || isHub(kept.repo))
  const screen = (to: 'issues' | 'templates' | 'projects' | 'sync') => () => host.go(path({ kind: 'screen', screen: to }))

  // The composer's pills: where the run goes, which repository, which branch.
  const pills = (
    <>
      <TooltipProvider delay={150}>
        <Tooltip placement="top" offset={6}>
          <TooltipTrigger asChild>
            <XStack items="center" minW={0} maxW="100%">
              <ChipSelect
                name={t('Where the run runs')}
                icon={here ? <Laptop size={13} color="$soft" /> : place.id ? <Monitor size={13} color="$soft" /> : <Cloud size={13} color="$soft" />}
                label={here ? t('This computer') : activePlaceDisplay.title}
                chosen={placeItems.find((i) => i.place.id === placeId)}
                items={placeItems}
                onChange={(i) => set({ place: i.place.id })}
                placeholder={devMode ? t('Search machines…') : t('Search devices…')}
                placement="top-start"
                loading={places.loading}
                error={places.error?.message ?? null}
                footer={<FooterLink label={t('Manage…')} onPress={() => host.go(path({ kind: 'settings', section: 'machines' }))} />}
              />
            </XStack>
          </TooltipTrigger>
          <TooltipContent
            maxW={280}
            px="$3"
            py="$2"
            z="var(--z-popover)"
          >
            <YStack gap="$0.5">
              <SizableText fontFamily="$body" size="$1" color="$ink">
                {placeTooltip}
              </SizableText>
              {placeHardwareHint ? (
                <SizableText fontFamily="$body" size="$1" color="$soft">
                  {placeHardwareHint}
                </SizableText>
              ) : null}
            </YStack>
          </TooltipContent>
        </Tooltip>
      </TooltipProvider>
      {here || granted.length ? (
        <ChipSelect
          name={t('Folder')}
          icon={<FolderOpen size={13} color="$soft" />}
          label={aimedAt.name ? (aimedAt.auto ? t('Auto · {name}', { name: aimedAt.name }) : aimedAt.name) : here ? t('Choose folder…') : t('Auto')}
          chosen={folderItems.find((i) => i.id === aimedAt.id)}
          items={folderItems}
          onChange={(i) => set({ folder: i.id === AUTO ? '' : i.id })}
          placeholder={t('Search folders…')}
          placement="top-start"
          footer={
            here ? (
              <FooterLink label={t('Choose folder…')} onPress={() => void chooseFolder()} />
            ) : (
              <FooterLink label={t('Manage shared folders…')} onPress={() => host.go(path({ kind: 'settings', section: 'machines' }))} />
            )
          }
        />
      ) : null}
      {here || aimed.dir ? null : (
      <RepoSelect
        open={picking}
        onOpenChange={setPicking}
        value={isForge(kept.repo) ? { ...kept.repo, source: 'forge' } : isHub(kept.repo) ? { ...kept.repo, source: 'github' } : null}
        onChange={(r) => {
          const hub = (r as { github?: boolean }).github ? hubs.current.get((r.full_name ?? '').toLowerCase()) : undefined
          setIssue(0)
          if (hub) {
            set({ repo: hub, branch: hub.default_branch })
            return
          }
          const row = known.current.get(r.name) ?? chosen(r)
          set({ repo: row, branch: row.default_branch })
        }}
        load={loadRepos}
        connect={
          hasRepo ? (
            <FooterLink
              label={t('Work without a repository')}
              onPress={() => {
                drop()
                setPicking(false)
              }}
            />
          ) : undefined
        }
        tabs={[
          { id: 'all', label: t('All') },
          { id: 'forge', label: t('Organization') },
          { id: 'github', label: 'GitHub' },
        ]}
        troubleshoot={{ label: t('Link GitHub project…'), onPress: screen('sync') }}
        action={{
          label: t('Add to project'),
          onPress: (r) => {
            // A GitHub row is built from GitHub's own address: cloud git's first segment is a Hanzo org, and a GitHub owner is not one.
            const github = (r as { github?: boolean }).github === true
            const row = github ? undefined : known.current.get(r.name)
            setAdding({
              repo: github ? `https://github.com/${r.owner}/${r.name}` : row?.clone || `${target.api}/v1/git/${r.owner}/${r.name}.git`,
              title: row?.full_name || `${r.owner}/${r.name}`,
              ref: r.default_branch || 'main',
              name: r.name,
            })
          },
        }}
        placeholder={t('Repository')}
        placement="top-start"
        disabled={!signed}
      />
      )}
      {forge && !here && !aimed.dir ? (
        <BranchSelect open={branching} onOpenChange={setBranching} value={branch} onChange={(b) => set({ branch: b })} load={loadBranches} placement="top-start" />
      ) : null}
    </>
  )

  const commands: Command[] = [
    { id: 'run', label: t('Run'), keys: 'Ctrl+↵', icon: <Play size={14} color="$soft" />, disabled: !draft.trim(), run: () => void send() },
    { id: 'pick-repo', label: t('Link GitHub project…'), icon: <FolderGit2 size={14} color="$soft" />, run: () => setPicking(true) },
    { id: 'issues', label: t('View issues'), icon: <CircleDot size={14} color="$soft" />, run: screen('issues') },
    { id: 'templates', label: t('Browse templates'), icon: <LayoutTemplate size={14} color="$soft" />, run: screen('templates') },
    { id: 'projects', label: t('Open projects'), icon: <Code size={14} color="$soft" />, run: screen('projects') },
    { id: 'sync', label: t('Connect GitHub…'), icon: <GitBranch size={14} color="$soft" />, run: screen('sync') },
    { id: 'address', label: t('Use a git URL'), icon: <Link2 size={14} color="$soft" />, run: () => setGiving(true) },
    ...(redesign.can ? [{ id: 'redesign', label: t('Redesign a site…'), icon: <Globe size={14} color="$soft" />, run: redesign.ask }] : []),
  ]

  // The `+` menu's Connectors shelf gives power users 1-click access to attach a GitHub project even when the composer bar is kept clean.
  const connectors: Shelf = {
    rows: homes.value.homes.slice(0, 12).map((h) => ({
      id: h.name,
      label: h.label,
      note: h.branch,
      icon: <FolderGit2 size={16} />,
      on: codebase !== '' && codebase.toLowerCase() === h.name.toLowerCase(),
    })),
    pick: (id) => {
      if (codebase && codebase.toLowerCase() === id.toLowerCase()) {
        drop()
        return
      }
      const match = homes.value.homes.find((h) => h.name === id)
      if (match) choose(match)
    },
    loading: homes.loading,
    empty: t('Start any prompt directly, or link a GitHub project'),
    create: { label: t('Link GitHub project…'), onPress: () => setPicking(true) },
    browse: { label: t('Connect GitHub…'), onPress: screen('sync') },
    manage: { label: t('Open projects'), onPress: screen('projects') },
  }

  // Dev takes text files, so Sketch's image drawings stay off its `+` and `@` shelves.
  const devPlugins: Shelf = {
    ...carry.plugins,
    rows: carry.plugins.rows.filter((r) => r.id !== 'sketch'),
    pick: (id) => {
      if (id !== 'sketch') carry.plugins.pick(id)
    },
  }

  // The machines a run can go to, as the `+` menu's Devices: the same choice as the pill's.
  const devices: Shelf = {
    rows: placeItems.map((i) => ({
      id: i.place.id || 'sandbox',
      label: i.label,
      note: i.hint,
      icon: i.place.id === HERE ? <Laptop size={16} /> : i.place.id ? <Monitor size={16} /> : <Cloud size={16} />,
      on: i.place.id === placeId,
    })),
    pick: (id: string) => set({ place: id === 'sandbox' ? '' : id }),
    loading: places.loading,
    error: places.error?.message ?? null,
    create: { label: t('Link GitHub project…'), onPress: () => setPicking(true) },
    manage: { label: t('Manage devices…'), onPress: () => host.go(path({ kind: 'settings', section: 'machines' })) },
  }

  return (
    <XStack flex={1} width="100%" height="100%" minW={0} minH={0} overflow="hidden">
      <YStack flex={1} minW={0} minH={0} height="100%" overflow="hidden">
        <XStack data-slot="conversation-head" shrink={0} items="center" justify="space-between" gap="$2" px="$3" height={36} $max-md={{ display: 'none' }}>
          <XStack items="center" gap="$2" minW={0} flex={1} />
          <XStack data-slot="conversation-controls" items="center" gap="$1" shrink={0}>
            {wide ? <PanelToggle open={desk} onToggle={() => setDesk((d) => !d)} /> : null}
          </XStack>
        </XStack>
        <YStack
          display="grid"
          // Optical center (~43% down viewport on desktop) instead of geometric 50% + top bar offset.
          gridTemplateRows={wide ? 'minmax(24px, 0.76fr) auto auto minmax(48px, 1.24fr)' : 'minmax(0, 1fr) auto'}
          items="center"
          $platform-web={{ justifyItems: 'center' }}
          width="100%"
          flex={1}
          minH={0}
          overflowX="hidden"
          data-slot="new"
        >
          {wide ? <YStack /> : null}
          <YStack
            display="grid"
            // One track, its question set at the track's foot, just above the composer: a
            // second `auto` track left the question alone in the first, at the pane's top.
            gridTemplateRows="minmax(0, 1fr)"
            items="flex-end"
            width="100%"
            maxW={MEASURE}
            height={wide ? undefined : '100%'}
            px="$4"
            pb="$5"
            overflowY="auto"
          >
            <SizableText
              render="h1"
              fontFamily="$heading"
              fontSize="$7"
              lineHeight="$7"
              fontWeight="600"
              letterSpacing={'-0.025em' as never}
              color="$ink"
              text="center"
            >
              {t('Build what’s next.')}
            </SizableText>
          </YStack>
          <YStack width="100%" maxW={MEASURE} gap="$2" pb="$3">
            <YStack gap="$2">
              {env.value && (env.value.state === 'proposed' || unset(env.value)) && !place.id && !here ? (
                <XStack
                  items="center"
                  justify="space-between"
                  gap="$3"
                  rowGap="$2"
                  mx="$3"
                  px="$3"
                  py="$2"
                  flexWrap="wrap"
                >
                  <SizableText size="$2" color="$ink" flex={1} minW={180}>
                    {env.value.state === 'proposed'
                      ? t('A proposed environment for {repo} is waiting for review.', { repo: codebase })
                      : t('{repo} has no environment yet, so every run starts it bare.', { repo: codebase })}
                  </SizableText>
                  {env.value.state === 'proposed' && env.value.session ? (
                    <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => host.go(env.value!.session)}>
                      {t('Review')}
                    </Button>
                  ) : (
                    <Button size="sm" variant="primary" onPress={() => setOffering(true)}>
                      {t('Set up environment')}
                    </Button>
                  )}
                </XStack>
              ) : null}
              {note || refused || mind.note ? (
                <SizableText size="$1" color="$soft" role="status" px="$3">
                  {note || refused || mind.note}
                </SizableText>
              ) : null}
              {offered === place.id && granted.length ? (
                <XStack px="$3" gap="$2" flexWrap="wrap" data-slot="shared-folders">
                  {granted.map((f) => (
                    <Button
                      key={f}
                      size="sm"
                      variant="outline"
                      borderColor="var(--border-strong)"
                      aria-label={t('Use {folder}', { folder: f })}
                      onPress={() => {
                        set({ folder: f })
                        setOffered('')
                        setNote('')
                      }}
                    >
                      <XStack items="center" gap="$1.5">
                        <FolderOpen size={14} />
                        <SizableText size="$2" color="$ink">
                          {named(f)}
                        </SizableText>
                      </XStack>
                    </Button>
                  ))}
                </XStack>
              ) : null}
              {redesign.view}
              {ran ? <RanHere ran={ran} setup={fetched} devMode={devMode} onCancel={local?.cancel ? () => void local.cancel?.() : undefined} /> : null}
              {/* On Hanzo Desktop's This computer the host redesigns a site itself (redesign.tsx), and its offer is the one shown. */}
              {!redesign.can && pagesIn(draft).length && !critiqued(draft) && !busy ? (
                <XStack px="$3" data-slot="web-page-offer">
                  <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => setDraft(critique(pagesIn(draft)[0]!))}>
                    <XStack items="center" gap="$1.5">
                      <Globe size={14} />
                      <SizableText size="$2" color="$ink">
                        {t('Critique and redesign {page}', { page: shownPage(pagesIn(draft)[0]!) })}
                      </SizableText>
                    </XStack>
                  </Button>
                </XStack>
              ) : null}
              <Composer
                column
                value={draft}
                onChange={setDraft}
                onSend={() => void send()}
                busy={busy || redesign.busy}
                placeholder={t('Ask, build, or run anything…')}
                label={t('Describe a task or ask a question')}
                plus={{
                  upload: () => pick(files, setFiles, setNote, t),
                  research: { on: carry.gear.gear.research, set: (on) => carry.gear.set({ research: on }) },
                  skills: carry.skills,
                  connectors,
                  design: carry.design,
                  plugins: devPlugins,
                  devices,
                  memory: { on: carry.gear.gear.memory, set: (on) => carry.gear.set({ memory: on }) },
                }}
                carried={[...carry.on, ...carrying(files, setFiles)]}
                slash={carry.skills}
                at={devPlugins}
                pills={pills}
                commands={commands}
                mode={carry.mode}
                mind={mind}
                effort={pacing(mind)}
                limits={limits}
                onUpgrade={() => host.go(path({ kind: 'screen', screen: 'plans' }))}
                voice={voice}
                language={prefs.language ?? null}
                onLanguage={(id) => void savePrefs({ language: id })}
                talk={talk}
              />
            </YStack>
          </YStack>
          {wide ? <YStack /> : null}
          <Publish source={adding} onClose={() => setAdding(null)} />
          <Address
            open={giving}
            onOpenChange={setGiving}
            title={t('Use a git URL')}
            says={t('Runs work on the GitHub repository at this address until you choose another. A private one needs GitHub connected.')}
            act={t('Use')}
            address=""
            run={async (at) => give(at)}
          />
          {carry.dialog}
          {codebase ? (
            <SetupDialog
              repo={codebase}
              homes={homes}
              open={offering}
              onOpenChange={setOffering}
              onStart={setup}
              onRepo={choose}
              onSaved={() => {
                setOffering(false)
                env.reload()
              }}
            />
          ) : null}
        </YStack>
      </YStack>
      {desk && wide ? (
        <YStack width={440} $max-lg={{ width: 340 }} maxW="50%" shrink={0} height="100%" borderLeftWidth={1} borderColor="$borderColor" bg="$background" minW={0} minH={0} overflow="hidden">
          <Panel side={deck} kinds={[]} onHide={() => setDesk(false)} label={t('Tabs')} />
        </YStack>
      ) : null}
    </XStack>
  )
}
