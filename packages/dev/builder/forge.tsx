/**
 * Projects and issues, read from the forge and opened in this window.
 *
 * A project is a repository, linked from GitHub or started from a template;
 * choosing one opens its workspace, where every run works on it. Issues are
 * the forge's and the task index's, merged (merge.ts): the same issue read
 * from two boards is one row. A repository's GitHub issues sync onto it.
 * Choosing an issue opens New on the issue's own repository — its GitHub
 * address, or the forge repository it is filed on, never a board's key — with
 * the issue as the ask; an issue no project answers to asks which one.
 *
 * The page's words are the reader's language (@hanzo/i18n `t`, the target is
 * `target`); a repository's org, name, branch and description, an issue's
 * title, labels and people, and the forge's own refusals are drawn as given.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import {
  ArrowRight,
  Check,
  ChevronDown,
  CircleCheck,
  CircleDot,
  CircleSlash,
  Copy,
  ExternalLink,
  FolderGit2,
  GitBranch,
  Github,
  GitPullRequest,
  LayoutTemplate,
  MoreHorizontal,
  RefreshCw,
  Search,
  SlidersHorizontal,
  Trash2,
  X,
} from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogTitle, DropdownMenu, Input, Skeleton, toast } from '@hanzo/ui'
import { useEffect, useMemo, useState, type ReactNode } from 'react'

import { copy as toClipboard } from './clipboard.ts'
import { RING, RING_INSET } from './pane.ts'
import { ago } from './ago.ts'
import { codebases, remove as removeRepo, type Codebase } from './api/codebases.ts'
import { unlink as removeLink } from './api/links.ts'
import { remove as removeProject } from './api/projects.ts'
import { Scheduled } from './scheduled.tsx'
import { boards, everything, sources, UNREAD, type Board, type Sources } from './api/work.ts'
import { boardKey, pinRepo, readPending, writePending } from './choice.ts'
import { useKept, useRead } from './data.ts'
import { useDevMode } from './dev-mode.ts'
import { useHost, useTarget } from './host.tsx'
import type { Screen } from './route.ts'
import { path } from './route.ts'
import { Customize } from './customize/index.tsx'
import { Sync } from './sync.tsx'
import { changed, connect, connection, disconnect as unlink, grants, heard, installations, linked, syncIssues, type Connection, type Installation } from './api/github.ts'
import { disconnect } from './api/provider.ts'
import { here, useBack } from './back.ts'
import { Out } from './out.tsx'
import { ask, assemble, attach, closes, facets, handle, home, keyOf, matches, pickProjects, place, type Issue, type Project } from './merge.ts'
import { removal, stopped, type Removal, type Step } from './removal.ts'
import { Confirm } from './ask.tsx'

function Note({ children }: { children: string }) {
  return (
    <SizableText size="$2" color="$soft">
      {children}
    </SizableText>
  )
}

const STATUS: Record<string, string> = {
  backlog: phrase('Backlog'),
  todo: phrase('To do'),
  in_progress: phrase('In progress'),
  done: phrase('Done'),
  canceled: phrase('Canceled'),
}

export function Forge({ screen }: { screen: Exclude<Screen, 'artifacts' | 'templates'> }) {
  if (screen === 'automations') return <Scheduled />
  if (screen === 'sync') return <Sync />
  // The fleet's native servers are Connectors → Discover now; the old address lands there.
  if (screen === 'mcp') return <Customize tab="connectors" view="discover" />
  if (screen === 'projects') return <Projects />
  return <Issues />
}

const PAGE = 25

/**
 * The organization's projects: its repositories on the forge, which arrive one
 * of two ways — linked from GitHub, or started from a template. A project is a
 * repository; opening one opens its workspace at `<org>/<repo>`, and every run
 * there works on that repository. There is no project without one.
 *
 * Remove takes one away (../removal.ts): Hanzo's copy of a GitHub repository
 * stops syncing and goes, with its project, and GitHub's is never touched; the
 * forge's own goes with its history. It asks once, on the row, saying which.
 */
function Projects() {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const signed = Boolean(host.person)
  const [q, setQ] = useState('')
  const [sort, setSort] = useState<'name' | 'updated'>('updated')
  const [page, setPage] = useState(0)
  const [landed, setLanded] = useState(0)
  useEffect(() => {
    setPage(0)
  }, [q, sort])
  const list = useRead(signed ? () => codebases(target) : null, [] as Codebase[], [target, signed, landed])
  const needle = q.trim().toLowerCase()
  const pending = readPending(host.org)
  const pendingKey = pending.map((p) => p.fullName).join('\n')
  const syncing = new Set(pending.map((p) => p.name))

  // A repository asked for from GitHub is listed as syncing until the forge holds it.
  useEffect(() => {
    const waiting = readPending(host.org)
    if (!signed || waiting.length === 0) return
    let stop = false
    grants(target)
      .then((g) => {
        if (stop) return
        const done = new Set(g.repos.filter((r) => r.imported).map((r) => r.fullName))
        const left = waiting.filter((p) => !done.has(p.fullName))
        if (left.length !== waiting.length) {
          writePending(host.org, left)
          setLanded((n) => n + 1)
        }
      })
      .catch(() => {})
    return () => {
      stop = true
    }
  }, [signed, target, host.org, pendingKey])

  const shown = [
    ...list.value,
    ...pending
      .filter((p) => !list.value.some((c) => c.name === p.name))
      .map(
        (p): Codebase => ({
          org: host.org ?? '',
          name: p.name,
          description: '',
          branch: 'main',
          public: false,
          clone: '',
          updated: '',
          branches: ['main'],
        }),
      ),
  ]
    .filter((c) => !needle || `${c.org}/${c.name} ${c.description}`.toLowerCase().includes(needle))
    .slice()
    .sort((a, b) => {
      if (sort === 'updated') {
        const d = (Date.parse(b.updated) || 0) - (Date.parse(a.updated) || 0)
        if (d !== 0) return d
      }
      return a.name.localeCompare(b.name)
    })
  const pages = Math.max(1, Math.ceil(shown.length / PAGE))
  const safePage = Math.min(page, pages - 1)
  const slice = shown.slice(safePage * PAGE, safePage * PAGE + PAGE)
  const from = shown.length === 0 ? 0 : safePage * PAGE + 1
  const to = safePage * PAGE + slice.length
  const none = !list.loading && !list.error && list.value.length === 0 && pending.length === 0

  const open = (c: Codebase) => host.go(path({ kind: 'repo', org: c.org || host.org || '', name: c.name }))
  const link = () => host.go(path({ kind: 'screen', screen: 'sync' }))
  const template = () => host.go(path({ kind: 'screen', screen: 'templates' }))

  // What each repository is tied to on Hanzo — the links into it, GitHub's names
  // for the copies it made, the projects built from it — read for Remove alone.
  const made = useRead(signed ? () => sources(target) : null, NONE, [target, signed, landed])
  const [confirming, setConfirming] = useState<Removal | null>(null)
  const [removing, setRemoving] = useState(false)
  const [said, setSaid] = useState('')
  const [saidError, setSaidError] = useState(false)
  // A plan stands on the links, the forge and the projects all having answered:
  // one that missed a link leaves a copy that syncs back, and one that missed a
  // project leaves its site up. GitHub's grants only say which copies GitHub made.
  const whole = !made.loading && made.value.missing.every((m) => m === UNREAD.hub)
  // Any member may: the sync app, the forge and the projects app each confine a
  // delete to the caller's own org and ask no more (cloud apps/sync delete,
  // apps/git deleteRepo, apps/projects del). A row still syncing in is not
  // offered, since its import would bring it back.
  const plan = (c: Codebase): Removal | null =>
    signed && whole && !syncing.has(c.name)
      ? removal(c.name, { granted: made.value.hub, linked: made.value.linked, held: made.value.held, sites: made.value.sites }, t)
      : null
  const drop = (s: Step): Promise<void> =>
    s.kind === 'link' ? removeLink(target, s.id) : s.kind === 'mirror' ? removeRepo(target, s.name) : removeProject(target, s.slug)
  // One step at a time, in order, stopping at the first refusal: nothing is left half-removed unsaid.
  const remove = async (r: Removal) => {
    setRemoving(true)
    setSaid('')
    setSaidError(false)
    let at = 0
    try {
      for (; at < r.steps.length; at++) await drop(r.steps[at])
      toast.success(t('{repo} is removed from Hanzo.', { repo: r.repo }))
    } catch (e) {
      setSaid(stopped(r, at, (e as Error).message, t))
      setSaidError(true)
    } finally {
      setRemoving(false)
      setConfirming(null)
      // Read again either way: what went leaves the list, and the next plan names only what is left.
      list.reload()
      made.reload()
    }
  }
  const copyRepo = (named: string) => {
    void toClipboard(named).then((ok) => (ok ? toast.success(t('Copied {repo}', { repo: named })) : toast.error(t('Could not copy repository name'))))
  }

  return (
    <YStack flex={1} minH={0} overflow="scroll" px="$6" py="$6" $max-md={{ px: '$4' }}>
      <YStack width="100%" maxW={1040} mx="auto" gap="$4">
        <XStack justify="space-between" items="flex-start" gap="$4" flexWrap="wrap">
          <YStack gap="$1" flex={1} minW={240}>
            <SizableText render="h1" size="$6" fontWeight="500" color="$ink">
              {t('Projects')}
            </SizableText>
            <SizableText size="$2" color="$soft">
              {host.org
                ? t('{org}’s projects and linked GitHub repositories. Open one to work on it.', { org: host.org })
                : t('Projects and linked GitHub repositories. Open one to work on it.')}
            </SizableText>
          </YStack>
          {signed ? (
            <XStack gap="$2" items="center" flexWrap="wrap">
              <Button size="sm" variant="outline" onPress={template}>
                <LayoutTemplate size={14} />
                {t('Create from template')}
              </Button>
              <Button size="sm" variant={none ? 'outline' : 'primary'} borderColor={none ? 'var(--border-strong)' : undefined} onPress={link}>
                <Github size={14} />
                {t('Link a GitHub repo')}
              </Button>
            </XStack>
          ) : null}
        </XStack>
        {signed ? <GitHubLink onSync={link} /> : null}
        {said ? (
          <SizableText size="$2" color={saidError ? 'var(--state-error)' : '$soft'} role="status">
            {said}
          </SizableText>
        ) : null}
        {list.error ? (
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$8" gap="$3" items="center">
            <SizableText size="$2" color="var(--state-error)" text="center">
              {list.error.message}
            </SizableText>
            <Button size="sm" variant="outline" onPress={list.reload}>
              {t('Try again')}
            </Button>
          </YStack>
        ) : !signed ? (
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$8" gap="$3" items="center">
            <SizableText size="$2" color="$soft" text="center">
              {t("Sign in to see this organization's projects.")}
            </SizableText>
            <Button size="sm" variant="primary" onPress={() => host.signIn?.()}>
              {t('Sign in')}
            </Button>
          </YStack>
        ) : list.loading && list.value.length === 0 && pending.length === 0 ? (
          <YStack gap="$3" aria-busy={true}>
            <Note>{t('Loading projects…')}</Note>
            <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" bg="$panel" overflow="hidden">
              {[0, 1, 2, 3].map((i) => (
                <XStack key={i} items="center" gap="$3" px="$3" py="$3" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                  <Skeleton layout="block" width={18} height={18} rounded="$2" shrink={0} />
                  <YStack flex={1} gap="$1.5">
                    <Skeleton layout="block" width="38%" height={14} rounded="$2" />
                    <Skeleton layout="block" width="56%" height={12} rounded="$2" />
                  </YStack>
                  <Skeleton layout="block" width={64} height={12} rounded="$2" shrink={0} />
                </XStack>
              ))}
            </YStack>
          </YStack>
        ) : none ? (
          <YStack gap="$4">
            <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$6" gap="$1.5" items="center">
              <SizableText size="$2" color="$soft" text="center" maxW={460}>
                {t('No projects yet. A project is a repository: link one from GitHub, or start one from a template.')}
              </SizableText>
            </YStack>
            <XStack gap="$3" flexWrap="wrap">
              <Choice
                label={t('Link a GitHub repo')}
                icon={<Github size={20} />}
                says={t('Mirror repositories you already have. They stay in step with GitHub, and their issues can come too.')}
                onPress={link}
              />
              <Choice
                label={t('Create from template')}
                icon={<LayoutTemplate size={20} />}
                says={t('Start from a finished product — a site, an app, a store — and make it yours.')}
                onPress={template}
              />
            </XStack>
          </YStack>
        ) : (
          <>
            <XStack gap="$2" items="center">
              <YStack flex={1} $max-md={{ display: 'none' }} />
              <YStack width={320} minW={0} shrink={1} $max-md={{ width: 'auto', flex: 1 }}>
                <Input
                  value={q}
                  onChangeText={setQ}
                  placeholder={t('Find a project…')}
                  aria-label={t('Find a project')}
                  startAdornment={<Search size={14} color="$soft" aria-hidden />}
                  endAdornment={
                    q ? (
                      <XStack
                        render="button"
                        aria-label={t('Clear search')}
                        onPress={() => setQ('')}
                        minW={24}
                        minH={24}
                        items="center"
                        justify="center"
                        focusVisibleStyle={RING}
                      >
                        <X size={13} color="$soft" />
                      </XStack>
                    ) : undefined
                  }
                  onKeyDown={(e: { key?: string }) => {
                    if (e.key === 'Escape' && q) setQ('')
                  }}
                />
              </YStack>
            </XStack>
            {shown.length === 0 ? (
              <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$7" gap="$3" items="center">
                <Note>{t('Nothing matches.')}</Note>
                <Button size="sm" variant="outline" onPress={() => setQ('')}>
                  {t('Clear search')}
                </Button>
              </YStack>
            ) : (
              <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" overflow="hidden">
                <XStack px="$3" py="$2" gap="$3" items="center" bg="$panel">
                  <XStack flex={1} minW={0} render="button" aria-label={t('Sort by name')} items="center" gap="$1" onPress={() => setSort('name')}>
                    <SizableText size="$1" color="$soft" numberOfLines={1}>
                      {t('Name')}
                    </SizableText>
                    {sort === 'name' ? <ChevronDown size={12} /> : null}
                  </XStack>
                  <SizableText size="$1" color="$soft" width={120} $max-md={{ display: 'none' }}>
                    {t('Branch')}
                  </SizableText>
                  <XStack render="button" aria-label={t('Sort by last updated')} items="center" justify="flex-end" gap="$1" width={120} shrink={0} $max-md={{ width: 84 }} onPress={() => setSort('updated')}>
                    <SizableText size="$1" color="$soft" numberOfLines={1}>
                      {t('Updated')}
                    </SizableText>
                    {sort === 'updated' ? <ChevronDown size={12} /> : null}
                  </XStack>
                  {/* Actions column, so every row's columns stand under their headers. */}
                  <YStack width={32} shrink={0} />
                </XStack>
                {slice.map((c) => {
                  const key = `${c.org}/${c.name}`
                  const named = `${c.org ? `${c.org}/` : ''}${c.name}`
                  const r = plan(c)
                  return (
                    <YStack key={key} borderTopWidth={1} borderColor="$borderColor">
                      <XStack items="center" gap="$3" paddingInlineEnd="$3" hoverStyle={{ bg: '$hover' }}>
                        <XStack
                          render="button"
                          aria-label={t('Open {repo}', { repo: named })}
                          onPress={() => open(c)}
                          $platform-web={{ textAlign: 'start' }}
                          flex={1}
                          minW={0}
                          items="center"
                          gap="$3"
                          paddingInlineStart="$3"
                          py="$2.5"
                          minH={44}
                          focusVisibleStyle={RING_INSET}
                        >
                          <XStack shrink={0}>
                            <FolderGit2 size={15} color="$soft" aria-hidden />
                          </XStack>
                          <YStack flex={1} minW={0}>
                            {/* `org/name` is an address: it keeps its own order in a right-to-left page. */}
                            <XStack minW={0} maxW="100%" overflow="hidden" items="baseline" self="flex-start" {...{ 'data-i18n-ltr': '' }}>
                              {/* The org gives way first: a long one (a wallet's is 27 characters) must not run under the Updated column. */}
                              {c.org ? (
                                <SizableText size="$2" color="$soft" numberOfLines={1} shrink={100} minW={0}>
                                  {`${c.org}/`}
                                </SizableText>
                              ) : null}
                              <SizableText size="$2" color="$ink" fontWeight="500" numberOfLines={1} shrink={1} minW={0}>
                                {c.name}
                              </SizableText>
                            </XStack>
                            {c.description ? (
                              <SizableText size="$1" color="$soft" numberOfLines={1}>
                                {c.description}
                              </SizableText>
                            ) : null}
                          </YStack>
                          <XStack width={120} items="center" gap="$1.5" minW={0} $max-md={{ display: 'none' }}>
                            <GitBranch size={12} color="$soft" />
                            <SizableText size="$1" color="$soft" numberOfLines={1}>
                              {c.branch}
                            </SizableText>
                          </XStack>
                          <SizableText size="$1" color="$soft" width={120} shrink={0} $max-md={{ width: 84 }} text="end" numberOfLines={1}>
                            {syncing.has(c.name) ? t('Syncing…') : ago(c.updated)}
                          </SizableText>
                        </XStack>
                        <XStack width={32} shrink={0} justify="center">
                          <DropdownMenu
                            trigger={
                              <XStack
                                render="button"
                                aria-label={t('Actions for {repo}', { repo: named })}
                                disabled={removing}
                                width={32}
                                height={32}
                                items="center"
                                justify="center"
                                focusVisibleStyle={RING}
                              >
                                <MoreHorizontal size={16} color="$soft" />
                              </XStack>
                            }
                            items={[
                              { key: 'open', label: t('Open workspace'), icon: <ArrowRight size={14} />, onSelect: () => open(c) },
                              { key: 'copy', label: t('Copy repository'), icon: <Copy size={14} />, onSelect: () => copyRepo(named) },
                              ...(r
                                ? [
                                    { type: 'separator' as const },
                                    { key: 'remove', label: t('Remove from Hanzo'), icon: <Trash2 size={14} />, destructive: true, onSelect: () => setConfirming(r) },
                                  ]
                                : []),
                            ]}
                          />
                        </XStack>
                      </XStack>
                    </YStack>
                  )
                })}
                <XStack px="$3" py="$2" justify="space-between" items="center" gap="$2" flexWrap="wrap" borderTopWidth={1} borderColor="$borderColor">
                  <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
                    {t('Showing {from}–{to} of {total}', { from, to, total: shown.length })}
                  </SizableText>
                  <XStack gap="$3" items="center">
                    <XStack render="button" aria-label={t('Previous page')} disabled={safePage === 0} onPress={() => setPage(safePage - 1)}>
                      <SizableText size="$1" color={safePage === 0 ? '$soft' : '$ink'}>
                        {t('Prev')}
                      </SizableText>
                    </XStack>
                    <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
                      {`${safePage + 1} / ${pages}`}
                    </SizableText>
                    <XStack render="button" aria-label={t('Next page')} disabled={safePage >= pages - 1} onPress={() => setPage(safePage + 1)}>
                      <SizableText size="$1" color={safePage >= pages - 1 ? '$soft' : '$ink'}>
                        {t('Next')}
                      </SizableText>
                    </XStack>
                  </XStack>
                </XStack>
              </YStack>
            )}
          </>
        )}
      </YStack>
      <Confirm
        open={confirming !== null}
        onOpenChange={(o) => !o && !removing && setConfirming(null)}
        title={confirming ? t('Remove {repo}?', { repo: confirming.repo }) : t('Remove project?')}
        says={confirming?.says ?? ''}
        act={t('Remove')}
        run={async () => {
          if (confirming) await remove(confirming)
        }}
      />
    </YStack>
  )
}

/** One of the two ways in, as a card you press. */
function Choice({ label, icon, says, onPress }: { label: string; icon: ReactNode; says: string; onPress: () => void }) {
  return (
    <YStack
      render="button"
      aria-label={label}
      onPress={onPress}
      flex={1}
      flexBasis={240}
      minW={0}
      gap="$2"
      p="$4"
      rounded="$4"
      borderWidth={1}
      borderColor="$borderColor"
      bg="$panel"
      items="flex-start"
      focusVisibleStyle={RING}
    >
      {icon}
      <XStack items="center" gap="$1.5">
        <SizableText size="$4" color="$ink" fontWeight="500">
          {label}
        </SizableText>
        <ArrowRight size={14} {...{ 'data-i18n-mirror': '' }} />
      </XStack>
      <SizableText size="$2" color="$soft" text="start">
        {says}
      </SizableText>
    </YStack>
  )
}

/**
 * The GitHub connection, in one line: who it is connected as and the way to
 * sync, or the one press that connects it. The person's own connection, or else
 * the accounts the org has bound the App on (`linked`), so an org that installed
 * the App in onboarding does not read as unconnected here. GitHub returns here,
 * and this page finishes the connect (../back.ts).
 *
 * Disconnect lets go of it, so another account can be connected: the person's own
 * link, or (for an org admin) the org's App accounts, which the platform also
 * uninstalls on GitHub. It asks once before doing either.
 *
 * A change made here, in Settings or in the Hanzo app's Quick setup is said on
 * the window (api/github.ts `changed`), and this line reads it again.
 */
function GitHubLink({ onSync }: { onSync: () => void }) {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const me = useRead(() => connection(target), null as Connection | null, [target])
  // An org whose installations cannot be read falls back to the person's own connection.
  const installed = useRead(() => installations(target).catch(() => []), [] as Installation[], [target])
  const [note, setNote] = useState('')
  const [busy, setBusy] = useState(false)
  const [asking, setAsking] = useState(false)
  const { reload: readMe } = me
  const { reload: readInstalled } = installed
  useEffect(
    () =>
      heard(() => {
        readMe()
        readInstalled()
      }),
    [readMe, readInstalled],
  )
  useBack(target, true, setNote)
  const c = me.value ? linked(me.value, installed.value) : null
  // A connection that cannot be read says nothing here: Sync from GitHub above still works.
  if (me.error || !c || !c.configured) return note ? <Note>{note}</Note> : null

  const link = async () => {
    setBusy(true)
    setNote('')
    try {
      host.open(await connect(target, here()))
    } catch (e) {
      setNote((e as Error).message)
      setBusy(false)
    }
  }

  // The person's own link is theirs to drop; the org's App accounts are an org admin's.
  const own = me.value?.connected === true
  const leave = c.connected && (own || host.admin)
  const drop = async () => {
    setBusy(true)
    setNote('')
    try {
      await (own ? unlink(target) : disconnect(target, 'github'))
      setAsking(false)
      setNote(t('GitHub is disconnected. Connect it again to choose another account.'))
      changed()
    } catch (e) {
      setNote((e as Error).message)
    } finally {
      setBusy(false)
    }
  }

  return (
    <XStack
      items="center"
      gap="$3"
      px="$3"
      py="$2.5"
      rounded="$3"
      borderWidth={1}
      borderColor="$borderColor"
      bg="$panel"
      flexWrap="wrap"
    >
      <Github size={16} />
      <YStack flex={1} minW={200}>
        <SizableText size="$2" color="$ink">
          {c.connected ? t('GitHub connected as {login}', { login: `@${c.login}` }) : t('Connect GitHub to bring your repositories here')}
        </SizableText>
        <SizableText size="$1" color="$soft">
          {asking
            ? own
              ? t('Disconnect your GitHub from Hanzo?')
              : t('Disconnect @{login}? Hanzo’s GitHub App is removed from it too, unless another organization uses it.', { login: c.login })
            : note || (c.connected ? t('Choose repositories to mirror; each stays in step with GitHub.') : t('One step on GitHub, then pick the repositories to sync.'))}
        </SizableText>
      </YStack>
      {c.connected && asking ? (
        <XStack gap="$2">
          <Button size="sm" variant="outline" disabled={busy} onPress={() => setAsking(false)}>
            {t('Cancel')}
          </Button>
          <Button size="sm" variant="primary" disabled={busy} onPress={() => void drop()}>
            {busy ? t('Disconnecting…') : t('Confirm disconnect')}
          </Button>
        </XStack>
      ) : c.connected ? (
        <XStack gap="$2">
          <Button size="sm" variant="outline" onPress={onSync}>
            <RefreshCw size={14} />
            {t('Sync repositories')}
          </Button>
          {leave ? (
            <Button size="sm" variant="outline" disabled={busy} onPress={() => setAsking(true)}>
              {t('Disconnect')}
            </Button>
          ) : null}
        </XStack>
      ) : (
        <Button size="sm" variant="primary" disabled={busy} onPress={() => void link()}>
          <Github size={14} />
          {busy ? t('Opening GitHub…') : t('Connect GitHub')}
        </Button>
      )}
    </XStack>
  )
}

type State = 'open' | 'closed'
type Kind = 'all' | 'issue' | 'pr'
type Origin = 'all' | 'github' | 'hanzo'
type Order = 'newest' | 'oldest' | 'updated'

/** The menus' words, marked (`phrase`) and drawn through `t`; GitHub and Hanzo are names, drawn as given. */
const KINDS: Record<Kind, string> = { all: phrase('Issues and pull requests'), issue: phrase('Issues'), pr: phrase('Pull requests') }
const ORIGINS: Record<Origin, string> = { all: phrase('Every source'), github: 'GitHub', hanzo: 'Hanzo' }
const ORDERS: Record<Order, string> = { newest: phrase('Newest'), oldest: phrase('Oldest'), updated: phrase('Recently updated') }

/** Mirrored from GitHub: the task index carried it from GitHub (`extRef` github:…). */
const fromGitHub = (i: Issue): boolean => i.sources.includes('github')

/** Done and canceled are closed; every other column is open work. */
const closed = (i: Issue): boolean => i.state === 'closed'

/** An RFC 3339 time as milliseconds, 0 when there is none. */
const ms = (iso: string): number => Date.parse(iso) || 0

/** The repository an issue is for, once the projects have named it, else the forge's name for it, else its board. */
const projectOf = (i: Issue): string => i.repo || i.forge || i.board

/**
 * Whether `i` is filed under the project `key`: its board, its forge
 * repository, or its repository by address or by name — either case. A board
 * and the repository it is named for are one project, as the forge files them.
 */
function inProject(i: Issue, key: string): boolean {
  const k = key.toLowerCase()
  return [i.board, i.forge, i.repo, i.repo.split('/')[1] ?? ''].some((v) => v !== '' && v.toLowerCase() === k)
}

/** How a person refers to an issue: `owner/name#N` on its own repository, the board's handle otherwise, '' with neither. */
function ref(i: Issue): string {
  const tag = handle(i)
  const at = i.repo || i.forge
  return tag.startsWith('#') && at ? `${at}${tag}` : tag
}

/** A dropdown that picks one of `options`, its trigger naming the choice. */
function Pick<K extends string>({ label, value, options, active = false, onPick }: { label: string; value: K; options: Record<K, string>; active?: boolean; onPick: (k: K) => void }) {
  return (
    <DropdownMenu
      trigger={
        <XStack render="button" aria-label={label} items="center" gap="$1" px="$2" py="$1" rounded="$2" hoverStyle={{ bg: '$hover' }}>
          <SizableText size="$2" color="$soft" numberOfLines={1}>
            {options[value]}
          </SizableText>
          <ChevronDown size={12} />
        </XStack>
      }
      items={(Object.keys(options) as K[]).map((k) => ({
        key: k || '*',
        label: options[k],
        icon: k === value ? <Check size={14} /> : <YStack width={14} />,
        onSelect: () => onPick(k),
      }))}
    />
  )
}

/** The words a list carries as a menu: every one of them, or `all`. */
const menu = (all: string, values: string[]): Record<string, string> => Object.fromEntries([['', all], ...values.map((v) => [v, v])])

function Mark({ i }: { i: Issue }) {
  if (i.pull) return <GitPullRequest size={16} color={closed(i) ? '$soft' : '$ink'} />
  if (i.status === 'canceled') return <CircleSlash size={16} color="$soft" />
  if (closed(i)) return <CircleCheck size={16} color="$soft" />
  return <CircleDot size={16} color="$ink" />
}

function Tag({ children }: { children: string }) {
  return (
    <XStack px="$2" py={1} rounded={999} borderWidth={1} borderColor="$borderColor" shrink={0}>
      <SizableText size="$1" color="$soft" numberOfLines={1}>
        {children}
      </SizableText>
    </XStack>
  )
}

const NONE: Sources = { hub: [], linked: [], held: [], sites: [], owners: [], next: '', unread: [], missing: [] }

/**
 * Pressing an issue: New, on the issue's own repository, holding the issue. A
 * GitHub issue runs on GitHub, where its pull request closes it; a forge issue
 * on the forge. A repository not among the projects read so far is looked up by
 * its address; an issue no project answers to asks which one it is for — a
 * board's key or a Linear team's is never taken for a repository.
 */
function useOpen(list: Project[], work: Issue[]) {
  const host = useHost()
  const t = useTarget()
  const [asking, setAsking] = useState<Issue | null>(null)
  const [finding, setFinding] = useState('')
  const go = (i: Issue, p: Project) => {
    // A project the person picked for an issue that is not its own closes nothing there.
    const own = place(i, [p]) !== null
    const n = own ? closes(i, p) : 0
    pinRepo(host.org, p, ask(i, n), n, own ? home(i, p) : undefined)
    setAsking(null)
    host.go('')
  }
  const open = async (i: Issue) => {
    const p = place(i, list)
    if (p && (p.forge || p.linked)) return go(i, p)
    if (i.repo) {
      setFinding(i.key)
      const [owner = '', name = ''] = i.repo.split('/')
      const found = await sources(t, { q: name, owner })
        .then((s) => assemble({ granted: s.hub, linked: s.linked, held: s.held, sites: s.sites, work }).find((x) => x.key === keyOf(owner, name)) ?? null)
        .catch(() => null)
      setFinding('')
      if (found && (found.forge || found.linked)) return go(i, found)
    }
    setAsking(i)
  }
  const dialog = <Which issue={asking} list={list} onPick={(p) => asking && go(asking, p)} onClose={() => setAsking(null)} />
  return { open: (i: Issue) => void open(i), finding, dialog }
}

/** Asks which project an issue is for, from the org's own list: never a free string. */
function Which({ issue, list, onPick, onClose }: { issue: Issue | null; list: Project[]; onPick: (p: Project) => void; onClose: () => void }) {
  const { t } = useI18n()
  const [q, setQ] = useState('')
  useEffect(() => {
    if (!issue) setQ('')
  }, [issue])
  const shown = pickProjects(
    list.filter((p) => p.forge || p.linked),
    { q },
  ).slice(0, 50)
  const named = issue?.repo || issue?.hint || ''
  return (
    <Dialog open={issue !== null} onOpenChange={(o) => (o ? null : onClose())}>
      <DialogContent maxW={480} gap="$3">
        <DialogTitle>{t('Which project is this for?')}</DialogTitle>
        <SizableText size="$2" color="$soft">
          {named
            ? t('“{name}” is not one of this organization’s projects. Choose the repository the run works on.', { name: named })
            : issue?.board
              ? t('This issue names no repository: it is on the {board} board. Choose the one the run works on.', { board: issue.board })
              : t('This issue names no repository. Choose the one the run works on.')}
        </SizableText>
        <Input
          value={q}
          onChangeText={setQ}
          placeholder={t('Find a project…')}
          aria-label={t('Find a project')}
          startAdornment={<Search size={14} color="$soft" />}
        />
        <YStack gap="$1" maxH={320} overflow="scroll">
          {shown.length === 0 ? (
            <Note>{list.length ? t('Nothing matches.') : t('No projects are linked yet.')}</Note>
          ) : (
            shown.map((p) => (
              <XStack key={p.key} render="button" aria-label={t('Work on {repo}', { repo: `${p.owner}/${p.name}` })} onPress={() => onPick(p)} px="$2.5" py="$2" rounded="$3" items="center" justify="flex-start" hoverStyle={{ bg: '$hover' }}>
                {/* `owner/name` is an address: it keeps its own order in a right-to-left page. */}
                <XStack minW={0} shrink={1} {...{ 'data-i18n-ltr': '' }}>
                  <SizableText size="$2" color="$soft" numberOfLines={1} shrink={0}>
                    {`${p.owner}/`}
                  </SizableText>
                  <SizableText size="$2" color="$ink" numberOfLines={1} shrink={1}>
                    {p.name}
                  </SizableText>
                </XStack>
              </XStack>
            ))
          )}
        </YStack>
        <XStack justify="flex-end">
          <Button size="sm" variant="outline" onPress={onClose}>
            {t('Cancel')}
          </Button>
        </XStack>
      </DialogContent>
    </Dialog>
  )
}

/**
 * Every project's issues, read the way GitHub reads them: open or closed, one
 * project or all of them, narrowed by words, kind, source, label and assignee,
 * and ordered. The same issue on two boards is one row. Each project syncs its
 * own issues from GitHub; choosing an issue starts the next run on the issue's
 * own repository with the issue as the ask.
 */
function Issues() {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const media = useMedia()
  const wideNav = Boolean(media.md)
  const wideFilters = Boolean(media.sm)
  const [devMode] = useDevMode()
  const signed = Boolean(host.person)
  const [board, setBoard] = useKept<string>(boardKey(host.org), '')
  const [state, setState] = useState<State>('open')
  const [kind, setKind] = useState<Kind>('all')
  const [origin, setOrigin] = useState<Origin>('all')
  const [order, setOrder] = useState<Order>('newest')
  const [label, setLabel] = useState('')
  const [assignee, setAssignee] = useState('')
  const [q, setQ] = useState('')
  const [moreBoards, setMoreBoards] = useState(false)
  const [showFilters, setShowFilters] = useState(false)
  const [pulled, setPulled] = useState(0)
  const [busy, setBusy] = useState(false)
  const [said, setSaid] = useState<{ text: string; connect?: boolean } | null>(null)
  const list = useRead(signed ? () => everything(target) : null, [] as Issue[], [target, signed, pulled])
  const known = useRead(signed ? () => boards(target) : null, [] as Board[], [target, signed])
  // What the org's projects are made from, so an issue is placed on its own repository.
  const made = useRead(signed ? () => sources(target) : null, NONE, [target, signed])
  const projects = useMemo(
    () => assemble({ granted: made.value.hub, linked: made.value.linked, held: made.value.held, sites: made.value.sites, work: list.value }),
    [made.value, list.value],
  )
  const work = useMemo(() => attach(list.value, projects), [list.value, projects])
  const { open, finding, dialog } = useOpen(projects, list.value)

  // Another screen chose the board (the rail's Issues chooses every board) while this one is open.
  useEffect(() => {
    const on = (e: Event) => setBoard((e as CustomEvent<string>).detail)
    window.addEventListener('hanzo-board', on)
    return () => window.removeEventListener('hanzo-board', on)
  }, [setBoard])
  useEffect(() => setSaid(null), [board])

  // The projects: every board the forge names, and every repository an issue is filed under that no board is named for.
  const column = new Map<string, { key: string; name: string; open: number }>()
  for (const b of known.value) column.set(b.key.toLowerCase(), { key: b.key, name: b.name, open: 0 })
  for (const i of work) {
    const key = projectOf(i)
    if (key && ![...column.values()].some((p) => inProject(i, p.key))) column.set(key.toLowerCase(), { key, name: key, open: 0 })
  }
  for (const p of column.values()) p.open = work.filter((i) => !closed(i) && inProject(i, p.key)).length
  const rows = [...column.values()].sort((a, b) => a.name.localeCompare(b.name))
  const current = board ? column.get(board.toLowerCase()) : undefined
  const title = current?.name ?? board

  // Progressive disclosure: cap visible project filters to 6 on desktop (4 on compact screens), keeping the selected project visible.
  const allBoardChips = [{ key: '', name: t('All projects'), open: work.filter((i) => !closed(i)).length }, ...rows]
  const boardCap = wideNav ? 6 : 4
  const boardOverflow = allBoardChips.length > boardCap
  const visibleBoards =
    !moreBoards && boardOverflow
      ? (() => {
          const base = allBoardChips.slice(0, boardCap)
          if (base.some((b) => b.key.toLowerCase() === board.toLowerCase())) return base
          const active = allBoardChips.find((b) => b.key.toLowerCase() === board.toLowerCase())
          return active ? [...base.slice(0, Math.max(1, boardCap - 1)), active] : base
        })()
      : allBoardChips
  const hiddenBoardCount = allBoardChips.length - visibleBoards.length

  const inBoard = work.filter((i) => !board || inProject(i, board))
  const { labels, assignees } = facets(inBoard)
  const scoped = inBoard
    .filter((i) => kind === 'all' || (kind === 'pr') === i.pull)
    .filter((i) => origin === 'all' || (origin === 'github') === fromGitHub(i))
    .filter((i) => !label || i.labels.some((l) => l.toLowerCase() === label.toLowerCase()))
    .filter((i) => !assignee || i.assignees.some((a) => a.toLowerCase() === assignee.toLowerCase()))
    .filter((i) => matches(q, i.title, ref(i), projectOf(i), ...i.assignees, ...i.labels))
  const opened = scoped.filter((i) => !closed(i))
  const shut = scoped.filter(closed)
  const shown = (state === 'open' ? opened : shut).slice().sort((a, b) => {
    if (order === 'updated') return ms(b.updated || b.created) - ms(a.updated || a.created)
    if (order === 'oldest') return ms(a.created) - ms(b.created)
    return ms(b.created) - ms(a.created)
  })
  const secondaryFilterCount = (label ? 1 : 0) + (assignee ? 1 : 0) + (origin !== 'all' ? 1 : 0)
  const narrowed = q.trim() !== '' || kind !== 'all' || secondaryFilterCount > 0
  const clearFilters = () => {
    setQ('')
    setKind('all')
    setOrigin('all')
    setLabel('')
    setAssignee('')
  }

  const pull = async () => {
    setBusy(true)
    setSaid(null)
    try {
      const s = await syncIssues(target, board)
      const where = board ? title : s.repos === 1 ? t('1 repository') : t('{n} repositories', { n: s.repos })
      const parts = [
        s.issues === 1 ? t('1 issue synced from {where}', { where }) : t('{n} issues synced from {where}', { n: s.issues, where }),
        t('{n} new', { n: s.created }),
        t('{n} updated', { n: s.updated }),
      ]
      if (s.failed) parts.push(t('{n} failed', { n: s.failed }))
      setSaid({ text: `${parts.join(' · ')}.${s.truncated ? ` ${t('More remain — sync again to continue.')}` : ''}` })
      setPulled((n) => n + 1)
    } catch (e) {
      const text = (e as Error).message
      setSaid({ text, connect: /not connected/i.test(text) })
    } finally {
      setBusy(false)
    }
  }

  const link = async () => {
    try {
      host.open(await connect(target, here()))
    } catch (e) {
      setSaid({ text: (e as Error).message })
    }
  }

  const copyRef = (named: string) => {
    void toClipboard(named).then((ok) => (ok ? toast.success(t('Copied {ref}', { ref: named })) : toast.error(t('Could not copy reference'))))
  }

  const chip = (key: string, name: string, count: number | null) => {
    const on = key.toLowerCase() === board.toLowerCase()
    return (
      <XStack
        key={key || '*'}
        render="button"
        aria-label={key ? t('Show {project}', { project: name }) : t('Show every project')}
        aria-pressed={on}
        onPress={() => setBoard(key)}
        items="center"
        gap="$2"
        px="$2.5"
        py="$1.5"
        minH={32}
        rounded="$3"
        shrink={0}
        focusVisibleStyle={RING}
      >
        <SizableText flex={1} size="$2" color={on ? '$ink' : '$soft'} numberOfLines={1} text="start">
          {name}
        </SizableText>
        {count ? (
          <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
            {count}
          </SizableText>
        ) : null}
      </XStack>
    )
  }

  return (
    <YStack flex={1} minH={0} overflow="scroll" px="$6" py="$6" $max-md={{ px: '$4' }}>
      <YStack width="100%" maxW={1120} mx="auto" gap="$4">
        <XStack justify="space-between" items="flex-start" gap="$4" flexWrap="wrap">
          <YStack gap="$1" flex={1} minW={240}>
            <SizableText render="h1" size="$6" fontWeight="500" color="$ink">
              {board ? t('Issues · {project}', { project: title }) : t('Issues')}
            </SizableText>
            <SizableText size="$2" color="$soft">
              {board
                ? t('Work on {project}. Choosing an issue starts the next run on its own repository.', { project: title })
                : t('Work across every project. Choosing an issue starts the next run on its own repository.')}
            </SizableText>
          </YStack>
          <Button size="sm" variant="outline" disabled={!signed || busy} onPress={() => void pull()}>
            <Github size={14} />
            {busy ? t('Syncing…') : board ? t('Sync {project} from GitHub', { project: title }) : t('Sync all from GitHub')}
          </Button>
        </XStack>
        {said ? (
          <XStack items="center" gap="$3" flexWrap="wrap">
            <Note>{said.text}</Note>
            {said.connect ? (
              <Button size="sm" variant="primary" onPress={() => void link()}>
                <Github size={14} />
                {t('Connect GitHub')}
              </Button>
            ) : null}
          </XStack>
        ) : null}
        <XStack gap="$5" items="flex-start" $max-md={{ flexDirection: 'column', gap: '$3' }}>
          <YStack
            render="nav"
            aria-label={t('Projects')}
            width={220}
            shrink={0}
            gap="$0.5"
          >
            <SizableText size="$1" color="$soft" px="$2.5" pb="$1" $max-md={{ display: 'none' }}>
              {t('Projects')}
            </SizableText>
            {visibleBoards.map((p) => chip(p.key, p.name, p.open))}
            {boardOverflow ? (
              <XStack
                render="button"
                aria-expanded={moreBoards}
                aria-label={moreBoards ? t('Show fewer projects') : t('Show {n} more projects', { n: hiddenBoardCount })}
                onPress={() => setMoreBoards((v) => !v)}
                items="center"
                px="$2.5"
                py="$1.5"
                minH={32}
                shrink={0}
                focusVisibleStyle={RING}
              >
                <SizableText size="$1" fontWeight="500" color="$ink">
                  {moreBoards ? t('Show less') : t('+{n} more', { n: hiddenBoardCount })}
                </SizableText>
              </XStack>
            ) : null}
          </YStack>
          <YStack flex={1} minW={0} gap="$3" width="100%" $max-md={{ flexBasis: 'auto', grow: 0, minW: 0, width: '100%' }} $max-sm={{ flexBasis: 'auto', grow: 0, minW: 0, width: '100%' }}>
            <Input
              value={q}
              onChangeText={setQ}
              placeholder={t('Search issues')}
              aria-label={t('Search issues')}
              disabled={!signed}
              startAdornment={<Search size={14} color="$soft" />}
              endAdornment={
                q ? (
                  <XStack
                    render="button"
                    aria-label={t('Clear search')}
                    onPress={() => setQ('')}
                    minW={24}
                    minH={24}
                    items="center"
                    justify="center"
                    focusVisibleStyle={RING}
                  >
                    <X size={13} color="$soft" />
                  </XStack>
                ) : undefined
              }
              onKeyDown={(e: { key?: string }) => {
                if (e.key === 'Escape' && q) setQ('')
              }}
            />
            {list.error ? (
              <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$8" gap="$3" items="center">
                <SizableText size="$2" color="var(--state-error)" text="center">
                  {list.error.message}
                </SizableText>
                <Button size="sm" variant="outline" onPress={list.reload}>
                  {t('Try again')}
                </Button>
              </YStack>
            ) : !signed ? (
              <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$8" gap="$3" items="center">
                <SizableText size="$2" color="$soft" text="center">
                  {t("Sign in to see this organization's issues.")}
                </SizableText>
                <Button size="sm" variant="primary" onPress={() => host.signIn?.()}>
                  {t('Sign in')}
                </Button>
              </YStack>
            ) : list.loading && list.value.length === 0 ? (
              <YStack gap="$3" aria-busy={true}>
                <Note>{t('Loading issues…')}</Note>
                <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" bg="$panel" overflow="hidden">
                  {[0, 1, 2, 3, 4].map((i) => (
                    <XStack key={i} items="center" gap="$3" px="$3" py="$3" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                      <Skeleton layout="block" width={16} height={16} rounded={999} shrink={0} />
                      <YStack flex={1} gap="$1.5">
                        <Skeleton layout="block" width="54%" height={14} rounded="$2" />
                        <Skeleton layout="block" width="36%" height={12} rounded="$2" />
                      </YStack>
                    </XStack>
                  ))}
                </YStack>
              </YStack>
            ) : (
              <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" overflow="hidden">
                <XStack px="$3" py="$2" gap="$3" items="center" bg="$panel" flexWrap="wrap">
                  <XStack gap="$3" items="center" flex={1} minW={0}>
                    <XStack render="button" aria-label={t('Open issues')} aria-pressed={state === 'open'} items="center" gap="$1.5" onPress={() => setState('open')}>
                      <CircleDot size={14} color={state === 'open' ? '$ink' : '$soft'} />
                      <SizableText size="$2" color={state === 'open' ? '$ink' : '$soft'} fontWeight={state === 'open' ? '500' : '400'} fontVariant={['tabular-nums']}>
                        {t('{n} open', { n: opened.length })}
                      </SizableText>
                    </XStack>
                    <XStack render="button" aria-label={t('Closed issues')} aria-pressed={state === 'closed'} items="center" gap="$1.5" onPress={() => setState('closed')}>
                      <Check size={14} color={state === 'closed' ? '$ink' : '$soft'} />
                      <SizableText size="$2" color={state === 'closed' ? '$ink' : '$soft'} fontWeight={state === 'closed' ? '500' : '400'} fontVariant={['tabular-nums']}>
                        {t('{n} closed', { n: shut.length })}
                      </SizableText>
                    </XStack>
                  </XStack>
                  <XStack flex={1} $max-sm={{ display: 'none' }} />
                  <XStack gap="$1" rowGap="$1" items="center" flexWrap="wrap" shrink={1} minW={0}>
                    {wideFilters || showFilters ? (
                      <>
                        <Pick label={t('Label')} value={label} active={Boolean(label)} options={menu(t('Any label'), labels)} onPick={setLabel} />
                        <Pick label={t('Assignee')} value={assignee} active={Boolean(assignee)} options={menu(t('Anyone'), assignees)} onPick={setAssignee} />
                      </>
                    ) : null}
                    <Pick label={t('Kind')} value={kind} active={kind !== 'all'} options={{ all: t(KINDS.all), issue: t(KINDS.issue), pr: t(KINDS.pr) }} onPick={setKind} />
                    {wideFilters || showFilters ? (
                      <Pick label={t('Source')} value={origin} active={origin !== 'all'} options={{ ...ORIGINS, all: t(ORIGINS.all) }} onPick={setOrigin} />
                    ) : null}
                    <Pick label={t('Sort')} value={order} active={order !== 'newest'} options={{ newest: t(ORDERS.newest), oldest: t(ORDERS.oldest), updated: t(ORDERS.updated) }} onPick={setOrder} />
                    {!wideFilters ? (
                      <XStack
                        render="button"
                        aria-expanded={showFilters}
                        aria-label={showFilters ? t('Hide filters') : t('More filters')}
                        onPress={() => setShowFilters((v) => !v)}
                        items="center"
                        gap="$1.5"
                        px="$2"
                        py="$1"
                        minH={44}
                        focusVisibleStyle={RING}
                      >
                        <SlidersHorizontal size={13} color={showFilters || secondaryFilterCount ? '$ink' : '$soft'} />
                        <SizableText size="$2" color={showFilters || secondaryFilterCount ? '$ink' : '$soft'}>
                          {secondaryFilterCount ? t('Filters ({n})', { n: secondaryFilterCount }) : t('Filters')}
                        </SizableText>
                      </XStack>
                    ) : null}
                  </XStack>
                </XStack>
                {shown.length === 0 ? (
                  <YStack px="$3" py="$6" gap="$3" items="center" borderTopWidth={1} borderColor="$borderColor">
                    <Note>{state === 'open' ? (narrowed ? t('No open issues match.') : t('Nothing open.')) : t('Nothing closed.')}</Note>
                    {narrowed ? (
                      <Button size="sm" variant="outline" onPress={clearFilters}>
                        {t('Clear filters')}
                      </Button>
                    ) : list.value.length === 0 ? (
                      <XStack gap="$2" flexWrap="wrap" justify="center">
                        <Button size="sm" variant="outline" disabled={busy} onPress={() => void pull()}>
                          <Github size={14} />
                          {t('Sync from GitHub')}
                        </Button>
                        <Button size="sm" onPress={() => host.go('')}>
                          {t('Start a new run')}
                        </Button>
                      </XStack>
                    ) : null}
                  </YStack>
                ) : (
                  shown.map((i) => {
                    const named = ref(i)
                    return (
                      <XStack key={i.key || `${i.board}:${i.title}`} items="center" gap="$1.5" paddingInlineEnd="$2.5" borderTopWidth={1} borderColor="$borderColor" hoverStyle={{ bg: '$hover' }}>
                        <XStack
                          render="button"
                          aria-label={t('Build {issue}', { issue: named || i.title })}
                          onPress={() => open(i)}
                          disabled={finding === i.key && i.key !== ''}
                          flex={1}
                          minW={0}
                          items="flex-start"
                          gap="$3"
                          paddingInlineStart="$3"
                          py="$2.5"
                          minH={44}
                          $platform-web={{ textAlign: 'start' }}
                          focusVisibleStyle={RING_INSET}
                        >
                          <YStack pt={2}>
                            <Mark i={i} />
                          </YStack>
                          <YStack flex={1} minW={0} gap="$1" items="flex-start">
                            <XStack gap="$2" items="center" flexWrap="wrap" maxW="100%">
                              <SizableText size="$3" color="$ink" fontWeight="500" numberOfLines={1} shrink={1}>
                                {i.title}
                              </SizableText>
                              {STATUS[i.status] && !closed(i) && i.status !== 'todo' ? <Tag>{t(STATUS[i.status])}</Tag> : null}
                              {!STATUS[i.status] ? <Tag>{i.status}</Tag> : null}
                              {i.priority !== 'none' && i.priority ? <Tag>{i.priority}</Tag> : null}
                              {i.labels.slice(0, 4).map((l) => (
                                <Tag key={l}>{l}</Tag>
                              ))}
                            </XStack>
                            <SizableText size="$1" color="$soft" numberOfLines={1}>
                              {[
                                named || (i.pull ? (devMode ? t('pr') : t('Pull request')) : i.kind),
                                !board && projectOf(i) && !named.startsWith(projectOf(i)) ? projectOf(i) : '',
                                i.created ? t('opened {when}', { when: ago(i.created) }) : '',
                                i.assignees.length ? t('assigned to {people}', { people: i.assignees.join(', ') }) : '',
                                fromGitHub(i) ? 'GitHub' : '',
                                finding === i.key && i.key !== '' ? t('opening…') : '',
                              ]
                                .filter(Boolean)
                                .join(' · ')}
                            </SizableText>
                          </YStack>
                        </XStack>
                        {i.url ? (
                          <Out href={i.url} label={fromGitHub(i) ? t('Open {issue} on GitHub', { issue: named || i.title }) : t('Open {issue} in project', { issue: named || i.title })}>
                            <XStack width={32} height={32} $touchable={{ minW: 44, minH: 44 }} $max-sm={{ minW: 44, minH: 44 }} items="center" justify="center" rounded="$2" hoverStyle={{ bg: '$raised' }}>
                              <ExternalLink size={14} color="$soft" />
                            </XStack>
                          </Out>
                        ) : null}
                        <DropdownMenu
                          trigger={
                            <XStack
                              render="button"
                              aria-label={t('Actions for {issue}', { issue: named || i.title })}
                              width={32}
                              height={32}
                              items="center"
                              justify="center"
                              focusVisibleStyle={RING}
                            >
                              <MoreHorizontal size={16} color="$soft" />
                            </XStack>
                          }
                          items={[
                            { key: 'work', label: t('Work on this issue'), icon: <ArrowRight size={14} />, onSelect: () => open(i) },
                            ...(named
                              ? [{ key: 'copy', label: t('Copy reference'), icon: <Copy size={14} />, onSelect: () => copyRef(named) }]
                              : []),
                            ...(i.url
                              ? [
                                  {
                                    key: 'external',
                                    label: fromGitHub(i) ? t('Open on GitHub') : t('Open in project'),
                                    icon: <ExternalLink size={14} />,
                                    onSelect: () => window.open(i.url, '_blank', 'noopener,noreferrer'),
                                  },
                                ]
                              : []),
                          ]}
                        />
                      </XStack>
                    )
                  })
                )}
              </YStack>
            )}
          </YStack>
        </XStack>
      </YStack>
      {dialog}
    </YStack>
  )
}
