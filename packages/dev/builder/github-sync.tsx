/**
 * GitHub repository creation, public `owner/repo` forking, outbound mirror sync,
 * and repository import modal for `/dev`.
 *
 * Follows the monochrome design system (`@hanzo/gui`, `@hanzo/ui`, `@hanzo/i18n`)
 * and works cleanly from mobile (`390px`) to desktop (`1440px`).
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import {
  ArrowRight,
  ArrowUpRight,
  GitBranch,
  GitFork,
  Github,
  Globe,
  Link2,
  Lock,
  Plus,
  Search,
  X,
} from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogDescription, DialogTitle, Input, Skeleton, toast } from '@hanzo/ui'
import { useEffect, useId, useMemo, useState } from 'react'

import { here } from './back.ts'
import { told } from './api/call.ts'
import { codebases, type Codebase } from './api/codebases.ts'
import {
  attachGitHubMirror,
  bring,
  connect,
  connection,
  createGitHubRepo,
  forkGitHubRepo,
  installations,
  isGitHubUserOAuthRequired,
  parseGitHubRepo,
  repos as listGitHubRepos,
  type Connection,
  type CreatedRepo,
  type GitHubRepoBadgeInfo,
  type Installation,
  type Repo,
  type Repos,
} from './api/github.ts'
import { links, type Link } from './api/links.ts'
import { authorize, read as readProvider, type Connector } from './api/provider.ts'
import { pinHub, readPending, writePending, type Pending } from './choice.ts'
import { useRead } from './data.ts'
import { useHost, useTarget } from './host.tsx'
import { Out } from './out.tsx'
import { RING } from './pane.ts'
import { path } from './route.ts'

const EMPTY_REPOS: Repos = { repos: [], next: '', total: 0, unread: [], connected: false }

/**
 * Crisp monochrome GitHub badge/link (`owner/repo ↗`) with optional sync status.
 */
export function GitHubBadge({
  fullName,
  htmlUrl,
  syncedLabel,
  compact = false,
}: {
  fullName: string
  htmlUrl: string
  syncedLabel?: string
  compact?: boolean
}) {
  const { t } = useI18n()
  return (
    <XStack
      data-slot="github-badge"
      data-repo={fullName}
      items="center"
      rounded={999}
      borderWidth={1}
      borderColor="$borderColor"
      bg="$panel"
      px="$2"
      py="$0.5"
      minH={26}
      minW={0}
      shrink={1}
      maxW={compact ? 180 : 240}
      $max-md={{ maxW: compact ? 132 : 180 }}
      hoverStyle={{ bg: '$hover' }}
    >
      <Out href={htmlUrl} label={t('Open {repo} on GitHub', { repo: fullName })}>
        <XStack items="center" gap="$1.5" minW={0} maxW="100%" {...{ 'data-i18n-ltr': '' }}>
          <Github size={12} color="$ink" aria-hidden />
          <SizableText size="$1" color="$ink" fontWeight="500" numberOfLines={1} minW={0} shrink={1}>
            {fullName}
          </SizableText>
          {syncedLabel && !compact ? (
            <SizableText size="$1" color="$soft" shrink={0} $max-md={{ display: 'none' }}>
              {`· ${syncedLabel}`}
            </SizableText>
          ) : null}
          <ArrowUpRight size={11} color="$soft" aria-hidden />
        </XStack>
      </Out>
    </XStack>
  )
}

/**
 * 1-click user OAuth fallback callout shown when creating/forking under a
 * personal GitHub account returns HTTP 412 or 409.
 */
export function GitHubOAuthCallout({
  message,
  label,
}: {
  message?: string
  label?: string
}) {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState('')

  const authorizeUser = async () => {
    setBusy(true)
    setErr('')
    try {
      const url = await connect(target, here())
      host.open(url)
    } catch (e) {
      setErr(told(e, t))
      setBusy(false)
    }
  }

  return (
    <YStack
      data-slot="github-oauth-fallback"
      role="alert"
      gap="$2"
      px="$3"
      py="$2.5"
      rounded="$3"
      borderWidth={1}
      borderColor="var(--border-strong)"
      bg="$panel"
    >
      <SizableText size="$2" color="$ink">
        {message || t('Authorize your personal GitHub account to create or fork repositories.')}
      </SizableText>
      {err ? (
        <SizableText size="$1" color="var(--state-error)">
          {err}
        </SizableText>
      ) : null}
      <XStack>
        <Button
          size="sm"
          variant="primary"
          data-slot="github-oauth-cta"
          aria-label={label || t('Authorize GitHub account')}
          disabled={busy}
          onPress={() => void authorizeUser()}
        >
          <Github size={14} />
          {busy ? t('Opening GitHub…') : label || t('Authorize GitHub account')}
        </Button>
      </XStack>
    </YStack>
  )
}

/**
 * Segmented toggle pill button used for tabs and visibility switches.
 */
function SegmentButton({
  active,
  label,
  icon,
  role,
  onPress,
  disabled = false,
  dataSlot,
}: {
  active: boolean
  label: string
  icon?: React.ReactNode
  role?: 'tab' | 'button'
  onPress: () => void
  disabled?: boolean
  dataSlot?: string
}) {
  return (
    <XStack
      render="button"
      data-slot={dataSlot}
      role={role}
      aria-selected={role === 'tab' ? active : undefined}
      aria-pressed={role !== 'tab' ? active : undefined}
      aria-label={label}
      disabled={disabled}
      onPress={onPress}
      items="center"
      justify="center"
      gap="$1.5"
      px="$3"
      py="$1.5"
      minH={36}
      rounded="$3"
      borderWidth={1}
      borderColor={active ? '$ink' : '$borderColor'}
      bg={active ? '$ink' : '$panel'}
      cursor={disabled ? 'default' : 'pointer'}
      opacity={disabled ? 0.6 : 1}
      focusVisibleStyle={RING}
    >
      {icon}
      <SizableText size="$2" fontWeight={active ? '500' : '400'} color={active ? '$background' : '$ink'} numberOfLines={1}>
        {label}
      </SizableText>
    </XStack>
  )
}

/**
 * Form to create a new GitHub repository (`POST /v1/provider/github/repos`).
 * Handles 412/409 personal account OAuth requirement with a 1-click CTA.
 */
export function CreateGitHubRepoForm({
  defaultName = '',
  defaultDescription = '',
  linkRepo,
  orgs = [],
  submitLabel,
  oauthLabel,
  onCreated,
  onCancel,
}: {
  defaultName?: string
  defaultDescription?: string
  /** Hanzo Git repo slug to link and push immediately after creation. */
  linkRepo?: string
  orgs?: string[]
  submitLabel?: string
  oauthLabel?: string
  onCreated: (repo: CreatedRepo) => void | Promise<void>
  onCancel?: () => void
}) {
  const target = useTarget()
  const { t } = useI18n()
  const id = useId()
  const [name, setName] = useState(defaultName)
  const [description, setDescription] = useState(defaultDescription)
  const [isPrivate, setIsPrivate] = useState(true)
  const [org, setOrg] = useState('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState('')
  const [needsOAuth, setNeedsOAuth] = useState(false)

  useEffect(() => {
    setName(defaultName)
    setDescription(defaultDescription)
    setError('')
    setNeedsOAuth(false)
  }, [defaultName, defaultDescription, linkRepo])

  const submit = async () => {
    const trimmed = name.trim()
    if (!trimmed) {
      setError(t('Enter a repository name.'))
      return
    }
    setBusy(true)
    setError('')
    setNeedsOAuth(false)
    try {
      const created = await createGitHubRepo(target, {
        name: trimmed,
        description: description.trim() || undefined,
        private: isPrivate,
        org: org || undefined,
        linkRepo,
      })
      await onCreated(created)
    } catch (e) {
      if (isGitHubUserOAuthRequired(e)) {
        setNeedsOAuth(true)
        setError(told(e, t))
      } else {
        setError(told(e, t))
      }
    } finally {
      setBusy(false)
    }
  }

  return (
    <YStack data-slot="create-github-repo-form" gap="$3">
      <YStack gap="$1.5">
        <SizableText render="label" size="$2" color="$soft" htmlFor={`${id}-name`}>
          {t('Repository name')}
        </SizableText>
        <Input
          id={`${id}-name`}
          value={name}
          onChangeText={setName}
          placeholder="my-project"
          aria-label={t('Repository name')}
          disabled={busy}
          onKeyDown={(e: { key?: string }) => {
            if (e.key === 'Enter') void submit()
          }}
        />
      </YStack>

      <YStack gap="$1.5">
        <SizableText render="label" size="$2" color="$soft" htmlFor={`${id}-desc`}>
          {t('Description')}
        </SizableText>
        <Input
          id={`${id}-desc`}
          value={description}
          onChangeText={setDescription}
          placeholder={t('Short description (optional)')}
          aria-label={t('Description')}
          disabled={busy}
          onKeyDown={(e: { key?: string }) => {
            if (e.key === 'Enter') void submit()
          }}
        />
      </YStack>

      <YStack gap="$1.5">
        <SizableText size="$2" color="$soft">
          {t('Visibility')}
        </SizableText>
        <XStack gap="$2" flexWrap="wrap" role="group" aria-label={t('Visibility')}>
          <SegmentButton
            dataSlot="visibility-private"
            active={isPrivate}
            label={t('Private')}
            icon={<Lock size={13} color={isPrivate ? '$background' : '$ink'} />}
            disabled={busy}
            onPress={() => setIsPrivate(true)}
          />
          <SegmentButton
            dataSlot="visibility-public"
            active={!isPrivate}
            label={t('Public')}
            icon={<Globe size={13} color={!isPrivate ? '$background' : '$ink'} />}
            disabled={busy}
            onPress={() => setIsPrivate(false)}
          />
        </XStack>
      </YStack>

      {orgs.length > 0 ? (
        <YStack gap="$1.5">
          <SizableText size="$2" color="$soft">
            {t('Owner')}
          </SizableText>
          <XStack gap="$1.5" flexWrap="wrap" role="group" aria-label={t('Owner')}>
            <SegmentButton active={org === ''} label={t('Personal account')} disabled={busy} onPress={() => setOrg('')} />
            {orgs.map((o) => (
              <SegmentButton key={o} active={org === o} label={o} disabled={busy} onPress={() => setOrg(o)} />
            ))}
          </XStack>
        </YStack>
      ) : null}

      {needsOAuth ? (
        <GitHubOAuthCallout
          message={error || t('Your personal GitHub account needs authorization before creating repositories.')}
          label={oauthLabel}
        />
      ) : error ? (
        <SizableText size="$2" color="var(--state-error)" role="alert">
          {error}
        </SizableText>
      ) : null}

      <XStack gap="$2" justify="flex-end" flexWrap="wrap">
        {onCancel ? (
          <Button size="sm" variant="outline" disabled={busy} onPress={onCancel}>
            {t('Cancel')}
          </Button>
        ) : null}
        <Button
          size="sm"
          variant="primary"
          data-slot="create-github-repo-submit"
          disabled={busy || !name.trim()}
          onPress={() => void submit()}
        >
          <Plus size={14} />
          {busy ? t('Creating…') : submitLabel || t('Create repository')}
        </Button>
      </XStack>
    </YStack>
  )
}

/**
 * Form to fork a GitHub repository (`POST /v1/provider/github/fork`).
 * Supports `owner/repo`, GitHub URLs, and bare repository names.
 */
export function ForkGitHubRepoForm({
  orgs = [],
  submitLabel,
  onForked,
  onCancel,
}: {
  orgs?: string[]
  submitLabel?: string
  onForked: (repo: CreatedRepo) => void | Promise<void>
  onCancel?: () => void
}) {
  const target = useTarget()
  const { t } = useI18n()
  const id = useId()
  const [repo, setRepo] = useState('')
  const [name, setName] = useState('')
  const [org, setOrg] = useState('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState('')
  const [needsOAuth, setNeedsOAuth] = useState(false)

  const submit = async () => {
    const trimmed = repo.trim()
    if (!trimmed) {
      setError(t('Enter a repository to fork, such as owner/repo.'))
      return
    }
    setBusy(true)
    setError('')
    setNeedsOAuth(false)
    try {
      const forked = await forkGitHubRepo(target, {
        repo: trimmed,
        org: org || undefined,
        name: name.trim() || undefined,
      })
      await onForked(forked)
    } catch (e) {
      if (isGitHubUserOAuthRequired(e)) {
        setNeedsOAuth(true)
        setError(told(e, t))
      } else {
        setError(told(e, t))
      }
    } finally {
      setBusy(false)
    }
  }

  return (
    <YStack data-slot="fork-github-repo-form" gap="$3">
      <YStack gap="$1.5">
        <SizableText render="label" size="$2" color="$soft" htmlFor={`${id}-repo`}>
          {t('Repository to fork')}
        </SizableText>
        <Input
          id={`${id}-repo`}
          value={repo}
          onChangeText={setRepo}
          placeholder="owner/repo"
          aria-label={t('Repository to fork')}
          disabled={busy}
          onKeyDown={(e: { key?: string }) => {
            if (e.key === 'Enter') void submit()
          }}
        />
        <SizableText size="$1" color="$soft">
          {t('Enter any public GitHub repository (e.g. hanzoai/ui) or paste its URL.')}
        </SizableText>
      </YStack>

      <YStack gap="$1.5">
        <SizableText render="label" size="$2" color="$soft" htmlFor={`${id}-rename`}>
          {t('New repository name (optional)')}
        </SizableText>
        <Input
          id={`${id}-rename`}
          value={name}
          onChangeText={setName}
          placeholder={t('Keep original name')}
          aria-label={t('New repository name (optional)')}
          disabled={busy}
          onKeyDown={(e: { key?: string }) => {
            if (e.key === 'Enter') void submit()
          }}
        />
      </YStack>

      {orgs.length > 0 ? (
        <YStack gap="$1.5">
          <SizableText size="$2" color="$soft">
            {t('Fork into')}
          </SizableText>
          <XStack gap="$1.5" flexWrap="wrap" role="group" aria-label={t('Fork into')}>
            <SegmentButton active={org === ''} label={t('Personal account')} disabled={busy} onPress={() => setOrg('')} />
            {orgs.map((o) => (
              <SegmentButton key={o} active={org === o} label={o} disabled={busy} onPress={() => setOrg(o)} />
            ))}
          </XStack>
        </YStack>
      ) : null}

      {needsOAuth ? (
        <GitHubOAuthCallout
          message={error || t('Authorize your personal GitHub account to fork repositories into it.')}
        />
      ) : error ? (
        <SizableText size="$2" color="var(--state-error)" role="alert">
          {error}
        </SizableText>
      ) : null}

      <XStack gap="$2" justify="flex-end" flexWrap="wrap">
        {onCancel ? (
          <Button size="sm" variant="outline" disabled={busy} onPress={onCancel}>
            {t('Cancel')}
          </Button>
        ) : null}
        <Button
          size="sm"
          variant="primary"
          data-slot="fork-github-repo-submit"
          disabled={busy || !repo.trim()}
          onPress={() => void submit()}
        >
          <GitFork size={14} />
          {busy ? t('Forking…') : submitLabel || t('Fork & edit in /dev')}
        </Button>
      </XStack>
    </YStack>
  )
}

/**
 * Dialog to sync a Hanzo Git project to GitHub:
 *  - Create & sync a new GitHub repository (`POST /v1/provider/github/repos` with `linkRepo`), OR
 *  - Link an existing GitHub repository URL (`POST /v1/git/repos/:name/targets`).
 */
export function SyncToGitHubDialog({
  open,
  onOpenChange,
  repoName,
  defaultName,
  defaultDescription = '',
  onSynced,
}: {
  open: boolean
  onOpenChange: (open: boolean) => void
  repoName: string
  defaultName?: string
  defaultDescription?: string
  onSynced: (info: GitHubRepoBadgeInfo) => void
}) {
  const target = useTarget()
  const { t } = useI18n()
  const id = useId()
  const [mode, setMode] = useState<'create' | 'link'>('create')
  const [existingUrl, setExistingUrl] = useState('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState('')
  const [needsOAuth, setNeedsOAuth] = useState(false)

  const installed = useRead(
    open ? () => installations(target).catch(() => [] as Installation[]) : null,
    [] as Installation[],
    [target, open],
  )
  const orgLogins = useMemo(
    () => installed.value.filter((i) => i.connected && i.type.toLowerCase() === 'organization').map((i) => i.login),
    [installed.value],
  )

  useEffect(() => {
    if (!open) return
    setMode('create')
    setExistingUrl('')
    setError('')
    setNeedsOAuth(false)
  }, [open, repoName])

  const attachExisting = async () => {
    const at = parseGitHubRepo(existingUrl)
    if (!at) {
      setError(t('Enter a GitHub repository URL or owner/repo (e.g. https://github.com/owner/repo).'))
      return
    }
    setBusy(true)
    setError('')
    setNeedsOAuth(false)
    try {
      await attachGitHubMirror(target, repoName, existingUrl)
      const fullName = `${at.owner}/${at.name}`
      const htmlUrl = `https://github.com/${fullName}`
      toast.success(t('Syncing {repo} to GitHub ({target})', { repo: repoName, target: fullName }))
      onSynced({ owner: at.owner, name: at.name, fullName, htmlUrl, source: 'target' })
      onOpenChange(false)
    } catch (e) {
      if (isGitHubUserOAuthRequired(e)) {
        setNeedsOAuth(true)
        setError(told(e, t))
      } else {
        setError(told(e, t))
      }
    } finally {
      setBusy(false)
    }
  }

  return (
    <Dialog open={open} onOpenChange={(o) => !busy && onOpenChange(o)}>
      <DialogContent
        data-slot="sync-to-github-dialog"
        width="100%"
        maxW={480}
        maxH="85vh"
        overflow="scroll"
        gap="$3"
      >
        <DialogTitle>{t('Sync to GitHub')}</DialogTitle>
        <DialogDescription>
          {t('Push {repo} to a new or existing GitHub repository and keep commits synced.', { repo: repoName })}
        </DialogDescription>

        <XStack role="tablist" aria-label={t('Sync method')} gap="$2" flexWrap="wrap">
          <SegmentButton
            role="tab"
            dataSlot="sync-tab-create"
            active={mode === 'create'}
            label={t('Create new repo')}
            icon={<Plus size={13} color={mode === 'create' ? '$background' : '$ink'} />}
            disabled={busy}
            onPress={() => {
              setMode('create')
              setError('')
              setNeedsOAuth(false)
            }}
          />
          <SegmentButton
            role="tab"
            dataSlot="sync-tab-link"
            active={mode === 'link'}
            label={t('Link existing repo')}
            icon={<Link2 size={13} color={mode === 'link' ? '$background' : '$ink'} />}
            disabled={busy}
            onPress={() => {
              setMode('link')
              setError('')
              setNeedsOAuth(false)
            }}
          />
        </XStack>

        {mode === 'create' ? (
          <CreateGitHubRepoForm
            defaultName={defaultName || repoName}
            defaultDescription={defaultDescription}
            linkRepo={repoName}
            orgs={orgLogins}
            submitLabel={t('Create & sync to GitHub')}
            oauthLabel={t('Authorize GitHub to create repos')}
            onCancel={() => onOpenChange(false)}
            onCreated={(created) => {
              const parts = created.full_name.split('/')
              const owner = parts[0] ?? ''
              const name = parts[1] ?? created.full_name
              toast.success(t('Synced to {repo} on GitHub', { repo: created.full_name }))
              onSynced({
                owner,
                name,
                fullName: created.full_name,
                htmlUrl: created.html_url || `https://github.com/${created.full_name}`,
                source: 'target',
              })
              onOpenChange(false)
            }}
          />
        ) : (
          <YStack data-slot="link-existing-github-form" gap="$3">
            <YStack gap="$1.5">
              <SizableText render="label" size="$2" color="$soft" htmlFor={`${id}-url`}>
                {t('GitHub repository')}
              </SizableText>
              <Input
                id={`${id}-url`}
                value={existingUrl}
                onChangeText={setExistingUrl}
                placeholder="https://github.com/owner/repo"
                aria-label={t('GitHub repository URL')}
                disabled={busy}
                onKeyDown={(e: { key?: string }) => {
                  if (e.key === 'Enter') void attachExisting()
                }}
              />
              <SizableText size="$1" color="$soft">
                {t('Attaches an outbound mirror target and triggers an initial sync push.')}
              </SizableText>
            </YStack>

            {needsOAuth ? (
              <GitHubOAuthCallout
                message={error || t('Authorize your GitHub account to push to this repository.')}
              />
            ) : error ? (
              <SizableText size="$2" color="var(--state-error)" role="alert">
                {error}
              </SizableText>
            ) : null}

            <XStack gap="$2" justify="flex-end" flexWrap="wrap">
              <Button size="sm" variant="outline" disabled={busy} onPress={() => onOpenChange(false)}>
                {t('Cancel')}
              </Button>
              <Button
                size="sm"
                variant="primary"
                data-slot="link-existing-github-submit"
                disabled={busy || !existingUrl.trim()}
                onPress={() => void attachExisting()}
              >
                <Link2 size={14} />
                {busy ? t('Syncing…') : t('Link & sync to GitHub')}
              </Button>
            </XStack>
          </YStack>
        )}
      </DialogContent>
    </Dialog>
  )
}

/**
 * Modal/drawer experience for "Link a GitHub repo" / "Sync repositories" on `/dev/projects`:
 * - When connected with 0 repositories, presents first-class "Create a new GitHub repository"
 *   and "Fork a GitHub repository" actions instead of a dead-end empty state.
 * - When repositories are listed, provides instant search, 1-click "Import & edit in /dev",
 *   and clean access to "Create new repo" and "Fork repo".
 * - Responsive from 390px mobile to 1440px desktop with zero overlap.
 */
export function GitHubReposModal({
  open,
  onOpenChange,
  initialTab = 'browse',
  onChanged,
}: {
  open: boolean
  onOpenChange: (open: boolean) => void
  initialTab?: 'browse' | 'create' | 'fork'
  onChanged?: () => void
}) {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const signed = Boolean(host.person)

  const [tab, setTab] = useState<'browse' | 'create' | 'fork'>(initialTab)
  const [q, setQ] = useState('')
  const [importing, setImporting] = useState('')
  const [connecting, setConnecting] = useState(false)
  const [error, setError] = useState('')
  const [needsOAuth, setNeedsOAuth] = useState(false)

  useEffect(() => {
    if (!open) return
    setTab(initialTab)
    setQ('')
    setError('')
    setNeedsOAuth(false)
    setImporting('')
  }, [open, initialTab])

  const me = useRead(open && signed ? () => connection(target).catch(() => null) : null, null as Connection | null, [target, open, signed])
  const providerConn = useRead(open && signed ? () => readProvider(target, 'github').catch(() => null) : null, null as Connector | null, [target, open, signed])
  const installed = useRead(open && signed ? () => installations(target).catch(() => [] as Installation[]) : null, [] as Installation[], [target, open, signed])
  const repoPage = useRead(open && signed ? () => listGitHubRepos(target, { limit: 100 }) : null, EMPTY_REPOS, [target, open, signed])
  const held = useRead(open && signed ? () => codebases(target).catch(() => [] as Codebase[]) : null, [] as Codebase[], [target, open, signed])
  const syncedLinks = useRead(open && signed ? () => links(target).catch(() => [] as Link[]) : null, [] as Link[], [target, open, signed])

  const isConnected = Boolean(
    me.value?.connected ||
      providerConn.value?.connected ||
      repoPage.value.connected ||
      installed.value.some((i) => i.connected),
  )
  const login =
    me.value?.login ||
    providerConn.value?.account ||
    installed.value.find((i) => i.connected)?.login ||
    ''
  const orgLogins = useMemo(
    () => installed.value.filter((i) => i.connected && i.type.toLowerCase() === 'organization').map((i) => i.login),
    [installed.value],
  )
  const loading = me.loading || repoPage.loading

  const heldNames = useMemo(() => new Set(held.value.map((c) => c.name.toLowerCase())), [held.value])
  const linkedMap = useMemo(
    () => new Map(syncedLinks.value.map((l) => [`${l.owner}/${l.name}`.toLowerCase(), l])),
    [syncedLinks.value],
  )

  const needle = q.trim().toLowerCase()
  const filteredRepos = useMemo(
    () =>
      repoPage.value.repos.filter(
        (r) => !needle || r.full_name.toLowerCase().includes(needle) || r.name.toLowerCase().includes(needle) || r.owner.toLowerCase().includes(needle),
      ),
    [repoPage.value.repos, needle],
  )

  const startConnect = async () => {
    setConnecting(true)
    setError('')
    try {
      host.open(await connect(target, here()))
    } catch (e) {
      setError(told(e, t))
      setConnecting(false)
    }
  }

  const installApp = async () => {
    setConnecting(true)
    setError('')
    try {
      host.open(await authorize(target, 'github', here()))
    } catch (e) {
      setError(told(e, t))
      setConnecting(false)
    }
  }

  const openCreatedOrForked = async (created: CreatedRepo, verb: 'created' | 'forked') => {
    const [owner = '', name = created.full_name] = created.full_name.split('/')
    try {
      const queued = await bring(target, [created.full_name])
      const codebaseName = queued.codebases[created.full_name.toLowerCase()] || name
      const nextPending: Pending[] = [
        ...readPending(host.org).filter((p) => p.fullName.toLowerCase() !== created.full_name.toLowerCase()),
        { fullName: created.full_name, name: codebaseName },
      ]
      writePending(host.org, nextPending)
    } catch {
      // Personal OAuth repos can still be edited directly via github.com/owner/repo even without an org mirror.
    }
    pinHub(host.org, {
      owner,
      name,
      private: created.private,
      branch: created.default_branch || 'main',
    })
    toast.success(
      verb === 'created'
        ? t('Created {repo} on GitHub — opening in Dev', { repo: created.full_name })
        : t('Forked {repo} to GitHub — opening in Dev', { repo: created.full_name }),
    )
    onChanged?.()
    onOpenChange(false)
    host.go('')
  }

  const importAndEdit = async (r: Repo) => {
    const existingLink = linkedMap.get(r.full_name.toLowerCase())
    const forgeName = existingLink?.forge || r.codebase || r.name
    if (heldNames.has(forgeName.toLowerCase())) {
      onOpenChange(false)
      host.go(path({ kind: 'repo', org: host.org || '', name: forgeName }))
      return
    }
    setImporting(r.full_name)
    setError('')
    setNeedsOAuth(false)
    try {
      const queued = await bring(target, [r.full_name])
      const codebaseName = queued.codebases[r.full_name.toLowerCase()] || r.codebase || r.name
      const nextPending: Pending[] = [
        ...readPending(host.org).filter((p) => p.fullName.toLowerCase() !== r.full_name.toLowerCase()),
        { fullName: r.full_name, name: codebaseName },
      ]
      writePending(host.org, nextPending)
      pinHub(host.org, {
        owner: r.owner,
        name: r.name,
        private: r.private,
        branch: r.default_branch || 'main',
      })
      toast.success(t('Importing {repo} — opening in Dev', { repo: r.full_name }))
      onChanged?.()
      onOpenChange(false)
      host.go('')
    } catch (e) {
      if (isGitHubUserOAuthRequired(e)) {
        // When user has personal GitHub connected, they can still work directly on github.com/owner/repo.
        if (me.value?.connected) {
          pinHub(host.org, {
            owner: r.owner,
            name: r.name,
            private: r.private,
            branch: r.default_branch || 'main',
          })
          toast.success(t('Opened {repo} in Dev', { repo: r.full_name }))
          onOpenChange(false)
          host.go('')
          return
        }
        setNeedsOAuth(true)
        setError(told(e, t))
      } else {
        setError(told(e, t))
      }
    } finally {
      setImporting('')
    }
  }

  return (
    <Dialog open={open} onOpenChange={(o) => !importing && onOpenChange(o)}>
      <DialogContent
        data-slot="github-repos-modal"
        width="100%"
        maxW={540}
        maxH="85vh"
        overflow="scroll"
        gap="$3"
      >
        <XStack items="center" justify="space-between" gap="$2" flexWrap="wrap">
          <YStack gap="$0.5" flex={1} minW={0}>
            <DialogTitle>{t('GitHub repositories')}</DialogTitle>
            {isConnected && login ? (
              <SizableText size="$1" color="$soft" numberOfLines={1}>
                {t('Connected as @{login}', { login })}
              </SizableText>
            ) : null}
          </YStack>
        </XStack>

        {!signed ? (
          <YStack gap="$3" items="center" py="$4">
            <SizableText size="$2" color="$soft" text="center">
              {t('Sign in to connect GitHub and work on repositories in Dev.')}
            </SizableText>
            <Button size="sm" variant="primary" onPress={() => host.signIn?.()}>
              {t('Sign in')}
            </Button>
          </YStack>
        ) : !isConnected && !loading ? (
          <YStack data-slot="github-not-connected" gap="$3" py="$2">
            <SizableText size="$2" color="$soft">
              {t('Connect your GitHub account to import, create, or fork repositories right from Dev.')}
            </SizableText>
            {error ? (
              <SizableText size="$2" color="var(--state-error)" role="alert">
                {error}
              </SizableText>
            ) : null}
            <XStack gap="$2" flexWrap="wrap">
              <Button size="sm" variant="primary" disabled={connecting} onPress={() => void startConnect()}>
                <Github size={14} />
                {connecting ? t('Opening GitHub…') : t('Connect GitHub')}
              </Button>
              {host.admin ? (
                <Button size="sm" variant="outline" disabled={connecting} onPress={() => void installApp()}>
                  {t('Install on organization')}
                </Button>
              ) : null}
            </XStack>
          </YStack>
        ) : (
          <>
            <XStack role="tablist" aria-label={t('GitHub actions')} gap="$2" flexWrap="wrap">
              <SegmentButton
                role="tab"
                dataSlot="github-tab-browse"
                active={tab === 'browse'}
                label={
                  repoPage.value.repos.length > 0
                    ? t('Repositories ({n})', { n: repoPage.value.total || repoPage.value.repos.length })
                    : t('Repositories')
                }
                icon={<Github size={13} color={tab === 'browse' ? '$background' : '$ink'} />}
                onPress={() => {
                  setTab('browse')
                  setError('')
                  setNeedsOAuth(false)
                }}
              />
              <SegmentButton
                role="tab"
                dataSlot="github-tab-create"
                active={tab === 'create'}
                label={t('Create new repo')}
                icon={<Plus size={13} color={tab === 'create' ? '$background' : '$ink'} />}
                onPress={() => {
                  setTab('create')
                  setError('')
                  setNeedsOAuth(false)
                }}
              />
              <SegmentButton
                role="tab"
                dataSlot="github-tab-fork"
                active={tab === 'fork'}
                label={t('Fork repo')}
                icon={<GitFork size={13} color={tab === 'fork' ? '$background' : '$ink'} />}
                onPress={() => {
                  setTab('fork')
                  setError('')
                  setNeedsOAuth(false)
                }}
              />
            </XStack>

            {tab === 'browse' ? (
              loading && repoPage.value.repos.length === 0 ? (
                <YStack gap="$2" aria-busy={true} py="$2">
                  <SizableText size="$2" color="$soft">
                    {t('Reading your GitHub repositories…')}
                  </SizableText>
                  <Skeleton layout="block" height={40} rounded="$3" />
                  <Skeleton layout="block" height={40} rounded="$3" />
                  <Skeleton layout="block" height={40} rounded="$3" />
                </YStack>
              ) : repoPage.error && repoPage.value.repos.length === 0 ? (
                <YStack gap="$3" py="$2">
                  <SizableText size="$2" color="var(--state-error)" role="alert">
                    {told(repoPage.error, t)}
                  </SizableText>
                  <XStack gap="$2" flexWrap="wrap">
                    <Button size="sm" variant="outline" onPress={repoPage.reload}>
                      {t('Try again')}
                    </Button>
                    <Button size="sm" variant="primary" onPress={() => setTab('create')}>
                      <Plus size={14} />
                      {t('Create a new GitHub repository')}
                    </Button>
                    <Button size="sm" variant="outline" onPress={() => setTab('fork')}>
                      <GitFork size={14} />
                      {t('Fork a GitHub repository')}
                    </Button>
                  </XStack>
                </YStack>
              ) : repoPage.value.repos.length === 0 ? (
                <YStack data-slot="github-empty-state" gap="$3" py="$2">
                  <YStack gap="$1">
                    <SizableText size="$3" fontWeight="500" color="$ink">
                      {t('No repositories on GitHub yet')}
                    </SizableText>
                    <SizableText size="$2" color="$soft">
                      {login
                        ? t('Your GitHub account (@{login}) has 0 repositories right now. Create a new repository or fork any public owner/repo to start editing in Dev.', { login })
                        : t('Your connected GitHub account has 0 repositories right now. Create a new repository or fork any public owner/repo to start editing in Dev.')}
                    </SizableText>
                  </YStack>
                  <XStack gap="$2.5" flexWrap="wrap">
                    <YStack
                      render="button"
                      data-slot="empty-create-github-repo"
                      aria-label={t('Create a new GitHub repository')}
                      onPress={() => setTab('create')}
                      flex={1}
                      flexBasis={200}
                      minW={0}
                      gap="$1.5"
                      p="$3.5"
                      rounded="$4"
                      borderWidth={1}
                      borderColor="$borderColor"
                      bg="$panel"
                      items="flex-start"
                      hoverStyle={{ bg: '$hover' }}
                      focusVisibleStyle={RING}
                      $platform-web={{ textAlign: 'start' }}
                    >
                      <Plus size={18} color="$ink" />
                      <SizableText size="$3" fontWeight="500" color="$ink">
                        {t('Create a new GitHub repository')}
                      </SizableText>
                      <SizableText size="$1" color="$soft">
                        {t('Name, description, and visibility — created on GitHub and opened in Dev.')}
                      </SizableText>
                    </YStack>
                    <YStack
                      render="button"
                      data-slot="empty-fork-github-repo"
                      aria-label={t('Fork a GitHub repository')}
                      onPress={() => setTab('fork')}
                      flex={1}
                      flexBasis={200}
                      minW={0}
                      gap="$1.5"
                      p="$3.5"
                      rounded="$4"
                      borderWidth={1}
                      borderColor="$borderColor"
                      bg="$panel"
                      items="flex-start"
                      hoverStyle={{ bg: '$hover' }}
                      focusVisibleStyle={RING}
                      $platform-web={{ textAlign: 'start' }}
                    >
                      <GitFork size={18} color="$ink" />
                      <SizableText size="$3" fontWeight="500" color="$ink">
                        {t('Fork a GitHub repository')}
                      </SizableText>
                      <SizableText size="$1" color="$soft">
                        {t('Fork any public owner/repo to your GitHub and edit it in Dev.')}
                      </SizableText>
                    </YStack>
                  </XStack>
                  {host.admin ? (
                    <XStack>
                      <Button size="sm" variant="ghost" disabled={connecting} onPress={() => void installApp()}>
                        {t('Grant access to more repositories on GitHub')}
                      </Button>
                    </XStack>
                  ) : null}
                </YStack>
              ) : (
                <YStack data-slot="github-repo-list" gap="$2.5">
                  <Input
                    value={q}
                    onChangeText={setQ}
                    placeholder={t('Search repositories…')}
                    aria-label={t('Search repositories')}
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
                  />

                  {needsOAuth ? (
                    <GitHubOAuthCallout message={error} />
                  ) : error ? (
                    <SizableText size="$2" color="var(--state-error)" role="alert">
                      {error}
                    </SizableText>
                  ) : null}

                  <YStack
                    borderWidth={1}
                    borderColor="$borderColor"
                    rounded="$3"
                    maxH={320}
                    overflow="scroll"
                  >
                    {filteredRepos.length === 0 ? (
                      <YStack p="$4" gap="$2" items="center">
                        <SizableText size="$2" color="$soft">
                          {t('Nothing matches.')}
                        </SizableText>
                        <Button size="sm" variant="outline" onPress={() => setQ('')}>
                          {t('Clear search')}
                        </Button>
                      </YStack>
                    ) : (
                      filteredRepos.map((r, i) => {
                        const existingLink = linkedMap.get(r.full_name.toLowerCase())
                        const forgeName = existingLink?.forge || r.codebase || r.name
                        const onForge = heldNames.has(forgeName.toLowerCase())
                        const isBusy = importing === r.full_name
                        return (
                          <XStack
                            key={r.full_name}
                            items="center"
                            gap="$2.5"
                            px="$3"
                            py="$2.5"
                            minH={48}
                            borderTopWidth={i ? 1 : 0}
                            borderColor="$borderColor"
                            hoverStyle={{ bg: '$hover' }}
                            flexWrap="wrap"
                          >
                            <XStack items="center" gap="$2" flex={1} minW={160}>
                              {r.private ? <Lock size={14} color="$soft" aria-hidden /> : <Globe size={14} color="$soft" aria-hidden />}
                              <YStack flex={1} minW={0}>
                                <SizableText size="$2" color="$ink" fontWeight="500" numberOfLines={1} {...{ 'data-i18n-ltr': '' }}>
                                  {r.full_name}
                                </SizableText>
                                {r.default_branch ? (
                                  <XStack items="center" gap="$1">
                                    <GitBranch size={10} color="$soft" aria-hidden />
                                    <SizableText size="$1" color="$soft" numberOfLines={1}>
                                      {r.default_branch}
                                    </SizableText>
                                  </XStack>
                                ) : null}
                              </YStack>
                            </XStack>
                            <Button
                              size="sm"
                              variant={onForge ? 'outline' : 'primary'}
                              data-slot="import-edit-repo"
                              aria-label={
                                onForge
                                  ? t('Open {repo} in /dev', { repo: r.full_name })
                                  : t('Import & edit {repo} in /dev', { repo: r.full_name })
                              }
                              disabled={Boolean(importing)}
                              onPress={() => void importAndEdit(r)}
                              shrink={0}
                            >
                              {isBusy ? t('Importing…') : onForge ? t('Open in /dev') : t('Import & edit in /dev')}
                              {!isBusy ? <ArrowRight size={13} /> : null}
                            </Button>
                          </XStack>
                        )
                      })
                    )}
                  </YStack>
                </YStack>
              )
            ) : tab === 'create' ? (
              <CreateGitHubRepoForm
                orgs={orgLogins}
                submitLabel={t('Create & edit in /dev')}
                onCreated={(r) => openCreatedOrForked(r, 'created')}
                onCancel={() => setTab('browse')}
              />
            ) : (
              <ForkGitHubRepoForm
                orgs={orgLogins}
                submitLabel={t('Fork & edit in /dev')}
                onForked={(r) => openCreatedOrForked(r, 'forked')}
                onCancel={() => setTab('browse')}
              />
            )}
          </>
        )}
      </DialogContent>
    </Dialog>
  )
}
