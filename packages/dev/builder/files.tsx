/**
 * A run's Files tab: its working tree, live from the sandbox while the run holds
 * one — every edit the agent has made, committed or not — and its branch on the
 * forge otherwise. And its Artifacts tab: what the run left — every file it added
 * or changed and the whole change as a patch, kept after its sandbox is gone; its
 * previews, pull request and deployments — then its published site, its branch,
 * its environment proposal and its project.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); names, paths and a
 * file's contents are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { Download, ExternalLink, File, FileDiff, Folder, Globe, GitBranch, GitPullRequest, Layers, MonitorPlay, Rocket, Search, Settings2 } from '@hanzogui/lucide-icons-2'
import { Input } from '@hanzo/ui'
import { useState, type ReactNode } from 'react'

import { blob, tree } from './api/changes.ts'
import { artifact, artifacts, type Artifact } from './api/coding.ts'
import type { Blob, Entry } from './api/git.ts'
import { preview, read as look, type Node } from './api/sandbox.ts'
import { useRead } from './data.ts'
import { useHost, useTarget } from './host.tsx'
import { useWake } from './spend.tsx'
import { away, Out } from './out.tsx'

type Source = 'live' | 'branch'

const monoWrap = { whiteSpace: 'pre-wrap' as const, fontFamily: 'var(--f-mono, ui-monospace, monospace)' }

export function Files({
  session,
  repo,
  branch,
  sandbox,
  live,
}: {
  session: string
  repo: string
  branch: string
  sandbox: string
  live: boolean
}) {
  const { t } = useI18n()
  const reachable = live && Boolean(sandbox)
  // Live once the sandbox is reachable, unless the branch was picked.
  const [source, setSource] = useState<Source | null>(null)
  const from: Source = reachable ? (source ?? 'live') : 'branch'

  return (
    <YStack gap="$3">
      {reachable ? (
        <XStack gap="$1" items="center" flexWrap="wrap">
          <Pick on={from === 'live'} onPress={() => setSource('live')}>
            {t('Live')}
          </Pick>
          <Pick on={from === 'branch'} onPress={() => setSource('branch')}>
            {t('Branch')}
          </Pick>
        </XStack>
      ) : null}
      {from === 'live' ? <Live sandbox={sandbox} /> : <Branch session={session} label={`${repo}${branch ? `@${branch}` : ''}`} />}
    </YStack>
  )
}

/** The sandbox's working tree, as it is right now. */
function Live({ sandbox }: { sandbox: string }) {
  const target = useTarget()
  const { t } = useI18n()
  const [dir, setDir] = useState('')
  const [file, setFile] = useState('')
  const list = useRead(() => look(target, sandbox, dir), null as Node | null, [target, sandbox, dir])
  const body = useRead(file ? () => look(target, sandbox, file) : null, null as Node | null, [target, sandbox, file])
  const at = (name: string) => (dir ? `${dir}/${name}` : name)

  // A name is only a name until it is read: the answer says whether it was a directory.
  const open = async (name: string) => {
    const path = at(name)
    try {
      const n = await look(target, sandbox, path)
      if (n.dir) {
        setDir(path)
        setFile('')
      } else {
        setFile(path)
      }
    } catch {
      setFile(path)
    }
  }

  return (
    <Listing
      where={`/work${dir ? `/${dir}` : ''}`}
      up={dir ? () => (setDir(dir.includes('/') ? dir.slice(0, dir.lastIndexOf('/')) : ''), setFile('')) : undefined}
      error={list.error?.message ?? ''}
      loading={list.loading && !list.value}
      rows={(list.value?.names ?? []).map((name) => ({ key: name, name, dir: null, onPress: () => void open(name) }))}
      file={file}
      body={
        body.error
          ? body.error.message
          : body.value
            ? body.value.binary
              ? t('Binary file')
              : body.value.truncated
                ? t('Too large to show')
                : body.value.text || t('Empty file')
            : ''
      }
    />
  )
}

/** The run's branch on the forge; its base until it has pushed. */
function Branch({ session, label }: { session: string; label: string }) {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const signed = Boolean(host.person)
  const [dir, setDir] = useState('')
  const [file, setFile] = useState('')
  const list = useRead(signed ? () => tree(target, session, dir) : null, [] as Entry[], [target, session, dir, signed])
  const body = useRead(file ? () => blob(target, session, file) : null, null as Blob | null, [target, session, file])

  if (!signed) return <Soft>{t('Sign in to read this run’s files.')}</Soft>
  return (
    <Listing
      where={`${label}${dir ? `/${dir}` : ''}`}
      up={dir ? () => (setDir(dir.includes('/') ? dir.slice(0, dir.lastIndexOf('/')) : ''), setFile('')) : undefined}
      error={list.error?.message ?? ''}
      loading={list.loading && list.value.length === 0}
      rows={list.value.map((e) => ({
        key: e.path,
        name: e.name,
        dir: e.dir,
        onPress: () => (e.dir ? (setDir(e.path), setFile('')) : setFile(e.path)),
      }))}
      file={file}
      body={
        body.error
          ? body.error.message
          : body.value
            ? body.value.binary
              ? t('Binary file')
              : body.value.truncated
                ? t('Too large to show')
                : body.value.text || t('Empty file')
            : ''
      }
    />
  )
}

function Listing({
  where,
  up,
  error,
  loading,
  rows,
  file,
  body,
}: {
  where: string
  up?: () => void
  error: string
  loading: boolean
  rows: { key: string; name: string; dir: boolean | null; onPress: () => void }[]
  file: string
  body: string
}) {
  const { t } = useI18n()
  const [filter, setFilter] = useState('')
  const needle = filter.replace(/^\?/, '').trim().toLowerCase()
  const shown = needle ? rows.filter((r) => r.name.toLowerCase().includes(needle)) : rows
  return (
    <YStack gap="$2">
      <XStack items="center" gap="$1.5" px="$2.5" py="$1" rounded="$3" borderWidth={1} borderColor="$borderColor">
        <Search size={13} opacity={0.6} />
        <Input
          unstyled
          flex={1}
          minW={0}
          value={filter}
          onChangeText={setFilter}
          placeholder={t('Filter, or ? to search in files')}
          aria-label={t('Filter, or ? to search in files')}
          style={{ fontSize: 12, outlineStyle: 'none', background: 'transparent' }}
        />
      </XStack>
      <XStack gap="$2" items="center">
        {up ? (
          <XStack render="button" aria-label={t('Up one directory')} onPress={up} px="$1" py="$1">
            <SizableText size="$1" color="$soft">
              {t('Up')}
            </SizableText>
          </XStack>
        ) : null}
        <SizableText size="$1" color="$soft" numberOfLines={1}>
          {where}
        </SizableText>
      </XStack>
      {error ? (
        <SizableText size="$2" color="$soft">
          {error}
        </SizableText>
      ) : loading ? (
        <SizableText size="$2" color="$soft">
          {t('Reading…')}
        </SizableText>
      ) : shown.length === 0 ? (
        <SizableText size="$2" color="$soft">
          {needle ? t('No matching files.') : t('This directory is empty.')}
        </SizableText>
      ) : (
        shown.map((r) => (
          <XStack
            key={r.key}
            render="button"
            aria-label={r.dir === false ? t('Read {name}', { name: r.name }) : t('Open {name}', { name: r.name })}
            onPress={r.onPress}
            items="center"
            gap="$2"
            py="$1"
            hoverStyle={{ bg: '$hover' }}
          >
            {r.dir === false ? <File size={14} /> : r.dir ? <Folder size={14} /> : <File size={14} opacity={0.6} />}
            <SizableText size="$2" color="$ink" numberOfLines={1} text="start">
              {r.name}
            </SizableText>
          </XStack>
        ))
      )}
      {file && body ? (
        <YStack gap="$1" pt="$2" borderTopWidth={1} borderColor="$borderColor">
          <SizableText size="$1" color="$soft">
            {file}
          </SizableText>
          <SizableText size="$1" color="$ink" style={monoWrap}>
            {body}
          </SizableText>
        </YStack>
      ) : null}
    </YStack>
  )
}

/** A stored artifact's size, as a person reads it. */
const size = (n: number): string => (n < 1024 ? `${n} B` : n < 1 << 20 ? `${(n / 1024).toFixed(1)} KB` : `${(n / (1 << 20)).toFixed(1)} MB`)

/** What each kind of artifact is drawn with, and says it is, in the reader's language. */
const KIND: Record<Artifact['kind'], { icon: ReactNode; note: (a: Artifact, t: Say) => string }> = {
  file: { icon: <File size={16} />, note: (a, t) => t('Changed file · {size}', { size: size(a.size) }) },
  patch: { icon: <FileDiff size={16} />, note: (a, t) => t('The whole change as a patch · {size}', { size: size(a.size) }) },
  preview: { icon: <MonitorPlay size={16} />, note: (a, t) => t('What the run served on port {port}', { port: a.port }) },
  pull: { icon: <GitPullRequest size={16} />, note: (_, t) => t('Pull request') },
  deploy: { icon: <Rocket size={16} />, note: (_, t) => t('Deployment') },
}

export function Artifacts({
  session,
  sandbox,
  repo,
  branch,
  mode,
  project,
  site,
  pr,
  onEnvironment,
}: {
  session: string
  /** The run's sandbox, which serves its previews while it is kept. */
  sandbox: string
  repo: string
  branch: string
  mode: string
  project: string
  /** Where the run's work is published, or ''. */
  site: string
  pr: { href: string; label: string }
  onEnvironment: () => void
}) {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const { wake, dialog } = useWake()
  const left = useRead(host.person ? () => artifacts(target, session) : null, { saved: '', artifacts: [] }, [target, session, host.person])
  const [note, setNote] = useState('')

  // Bytes come with the person's bearer, so a stored artifact is fetched, then saved.
  const save = async (a: Artifact) => {
    try {
      const url = URL.createObjectURL(await artifact(target, session, a.name))
      const link = document.createElement('a')
      link.href = url
      link.download = a.name.split('/').pop() || a.name
      link.click()
      setTimeout(() => URL.revokeObjectURL(url), 10_000)
    } catch (e) {
      setNote((e as Error).message)
    }
  }
  // A preview is opened with a fresh ticket, in a tab of its own.
  const open = (a: Artifact) => away(preview(target, sandbox, a.port, wake)).catch((e: Error) => setNote(e.message))

  const items: { key: string; icon: ReactNode; title: string; note: string; href?: string; onPress?: () => void; action?: ReactNode }[] = []
  for (const a of left.value.artifacts) {
    const stored = a.kind === 'file' || a.kind === 'patch'
    const link = a.kind === 'pull' || a.kind === 'deploy'
    if ((a.kind === 'preview' && !sandbox) || (link && !/^https:\/\//.test(a.url))) continue
    items.push({
      key: `${a.kind}:${a.name}`,
      icon: KIND[a.kind].icon,
      title: link ? a.url.replace(/^https:\/\//, '') : a.name,
      note: KIND[a.kind].note(a, t),
      href: link ? a.url : undefined,
      onPress: stored ? () => void save(a) : a.kind === 'preview' ? () => void open(a) : undefined,
      action: stored ? <Download size={14} /> : a.kind === 'preview' ? <ExternalLink size={14} /> : undefined,
    })
  }
  if (site) items.push({ key: 'site', icon: <Globe size={16} />, title: site.replace(/^https:\/\//, ''), note: t('Where this session is published'), href: site })
  if (pr.href) items.push({ key: 'pr', icon: <GitPullRequest size={16} />, title: t('Pull request {pr}', { pr: pr.label }), note: t('Opened on the forge'), href: pr.href })
  if (branch) items.push({ key: 'branch', icon: <GitBranch size={16} />, title: branch, note: repo ? t('The branch this session pushes to in {repo}', { repo }) : t('The branch this session pushes to') })
  if (mode === 'setup') items.push({ key: 'environment', icon: <Settings2 size={16} />, title: t('Environment proposal'), note: t('The install and start scripts this session found'), onPress: onEnvironment })
  if (project) items.push({ key: 'project', icon: <Layers size={16} />, title: project, note: t('The project this session builds'), onPress: () => host.go(project) })

  if (!items.length) return <Soft>{left.error?.message || (left.loading ? t('Reading what this session left…') : t('This session has produced no artifacts yet.'))}</Soft>
  return (
    <YStack gap="$2">
      {dialog}
      {note ? (
        <SizableText size="$1" color="$soft" role="status">
          {note}
        </SizableText>
      ) : null}
      <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" overflow="hidden">
        {items.map((a, i) => {
          const row = (
            <XStack items="center" gap="$3" px="$3" py="$2.5" borderTopWidth={i ? 1 : 0} borderColor="$borderColor" hoverStyle={a.href || a.onPress ? { bg: '$hover' } : undefined}>
              {a.icon}
              <YStack flex={1} minW={0} $platform-web={{ textAlign: 'start' }}>
                <SizableText size="$2" color="$ink" numberOfLines={1}>
                  {a.title}
                </SizableText>
                <SizableText size="$1" color="$soft" numberOfLines={1}>
                  {a.note}
                </SizableText>
              </YStack>
              {a.action ?? (a.href ? <ExternalLink size={14} /> : null)}
            </XStack>
          )
          return a.href ? (
            <Out key={a.key} href={a.href} label={a.title}>
              <YStack width="100%">{row}</YStack>
            </Out>
          ) : a.onPress ? (
            <YStack key={a.key} render="button" aria-label={a.title} onPress={a.onPress}>
              {row}
            </YStack>
          ) : (
            <YStack key={a.key}>{row}</YStack>
          )
        })}
      </YStack>
    </YStack>
  )
}

function Pick({ on, onPress, children }: { on: boolean; onPress: () => void; children: string }) {
  return (
    <XStack render="button" aria-label={children} onPress={onPress} px="$2.5" py="$1" rounded="$2" bg={on ? '$hover' : 'transparent'}>
      <SizableText size="$2" color={on ? '$ink' : '$soft'}>
        {children}
      </SizableText>
    </XStack>
  )
}

function Soft({ children }: { children: string }) {
  return (
    <YStack py="$6" items="center">
      <SizableText size="$2" color="$soft" style={{ textAlign: 'center' }}>
        {children}
      </SizableText>
    </YStack>
  )
}
