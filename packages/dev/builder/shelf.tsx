/**
 * Two shelves: the sites this org has deployed (Sites, drawn at the foot of the
 * Library — library.tsx), and the public starters (Templates).
 *
 * A project is a real row — `/v1/project`, the same one a deployed site is
 * served from — so opening one opens its workspace at its own address. A
 * starter is taken as a COPY of its own repository (`POST /v1/project/fork`),
 * so what opens is that template, not a model's imitation of it.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { Boxes, ExternalLink, GitBranch, Globe, Lock, MoreHorizontal, Pencil, Search, Trash2, X } from '@hanzogui/lucide-icons-2'
import { Button, DropdownMenu, Input, Skeleton } from '@hanzo/ui'
import { useEffect, useMemo, useState } from 'react'

import { RING, RING_INSET } from './pane.ts'
import { clone } from './address.ts'
import { address, change, fork, remove, templates, type Project, type Template } from './api/projects.ts'
import { Address, Confirm, Rename } from './ask.tsx'
import { useProjects, useRead } from './data.ts'
import { useHost, useTarget } from './host.tsx'
import { copying } from './question.ts'
import { useSpend } from './spend.tsx'
import { Out } from './out.tsx'

const SHOTS = ['/templates', 'https://gallery.hanzo.ai/screenshots', 'https://hanzo.ai/templates'] as const

function Head({ title, says }: { title: string; says: string }) {
  return (
    <YStack gap="$1" pb="$4">
      <SizableText render="h1" size="$6" color="$ink">
        {title}
      </SizableText>
      <SizableText size="$2" color="$soft">
        {says}
      </SizableText>
    </YStack>
  )
}

function Card({ onPress, label, disabled = false, children }: { onPress: () => void; label: string; disabled?: boolean; children: React.ReactNode }) {
  return (
    <YStack
      render="button"
      onPress={onPress}
      disabled={disabled}
      aria-label={label}
      width="100%"
      minW={0}
      rounded="$4"
      borderWidth={1}
      borderColor="$borderColor"
      bg="$panel"
      overflow="hidden"
      opacity={disabled ? 0.72 : 1}
      focusVisibleStyle={RING}
      $platform-web={{ textAlign: 'start' }}
    >
      {children}
    </YStack>
  )
}

/** Copying a starter into a project of this org, then opening it: one copy at a time. */
function useFork() {
  const t = useTarget()
  const host = useHost()
  const { t: say } = useI18n()
  const spend = useSpend()
  const [busy, setBusy] = useState('')
  const [failed, setFailed] = useState('')
  const take = async (slug: string, name: string) => {
    if (busy) return
    if (!host.person) return host.signIn?.()
    // The copy is built and published at once: asked first.
    if (!(await spend.ask(copying(name, say)))) return
    setBusy(slug)
    setFailed('')
    try {
      const made = await fork(t, slug)
      host.go(made.slug || slug)
    } catch (e) {
      setFailed((e as Error).message)
    } finally {
      setBusy('')
    }
  }
  return { busy, failed, take, dialog: spend.dialog }
}

/** When a project was last edited, or made if it never was; Unix seconds. */
const when = (p: Project): number => p.updated || p.created

/** The org's projects under the month each was last edited, newest first. */
function months(list: Project[], say: Say): { label: string; rows: Project[] }[] {
  const out = new Map<string, Project[]>()
  for (const p of [...list].sort((a, b) => when(b) - when(a))) {
    const s = when(p)
    const label = s ? new Date(s * 1000).toLocaleDateString(undefined, { month: 'long', year: 'numeric' }) : say('Earlier')
    out.set(label, [...(out.get(label) ?? []), p])
  }
  return [...out].map(([label, rows]) => ({ label, rows }))
}

function edited(s: number, say: Say): string {
  if (!s) return ''
  const d = new Date(s * 1000)
  const year = d.getFullYear() === new Date().getFullYear() ? undefined : 'numeric'
  return say('Edited {date}', { date: d.toLocaleDateString(undefined, { month: 'short', day: 'numeric', year }) })
}

/**
 * What this org has deployed: a few starters to make something new from, then
 * every site under the month it was last edited, whether it is public or
 * private, and its rename, repository, visibility and delete. A site belongs to
 * the org and names no author, so the list does not split yours from a
 * colleague's — which is why it is a section of its own under the Library's
 * items, which are yours.
 *
 * Its repository is the one its builds are cut from, by a GitHub address in any
 * form GitHub hands out (address.ts), with the branch whose pushes rebuild it.
 */
export function Sites({ q = '', onMatch }: { q?: string; onMatch?: (n: number) => void } = {}) {
  const { t: say } = useI18n()
  const t = useTarget()
  const host = useHost()
  const list = useProjects(t, Boolean(host.person))
  const starters = useRead(() => templates(t), [] as Template[], [t.api])
  const { busy, failed, take, dialog } = useFork()
  const [renaming, setRenaming] = useState<Project | null>(null)
  const [relinking, setRelinking] = useState<Project | null>(null)
  const [deleting, setDeleting] = useState<Project | null>(null)
  const [note, setNote] = useState('')

  const needle = q.trim().toLowerCase()
  const filtered = useMemo(
    () => (needle ? list.value.filter((p) => `${p.name} ${p.slug} ${p.live ?? ''} ${p.repo ?? ''}`.toLowerCase().includes(needle)) : list.value),
    [list.value, needle],
  )
  useEffect(() => {
    onMatch?.(filtered.length)
  }, [filtered.length, onMatch])

  const flip = async (p: Project) => {
    const visibility = p.visibility === 'private' ? 'public' : 'private'
    setNote('')
    try {
      await change(t, p.slug, { visibility })
      setNote(visibility === 'public' ? say('{name} is public now', { name: p.name }) : say('{name} is private now', { name: p.name }))
      list.reload()
    } catch (e) {
      setNote((e as Error).message)
    }
  }

  if (needle && filtered.length === 0) return null

  const said = failed || note
  return (
    <YStack width="100%" role="region" aria-label={say('Sites')}>
      {dialog}
      <YStack width="100%">
        <YStack gap="$1" pb="$4">
          <SizableText render="h2" size="$5" color="$ink">
            {say('Sites')}
          </SizableText>
          <SizableText size="$2" color="$soft">
            {`${say('What this organization has built.')} ${say('Open one to keep building it.')}`}
          </SizableText>
        </YStack>
        {said ? (
          <SizableText size="$2" color="$soft" role="status" pb="$3">
            {said}
          </SizableText>
        ) : null}
        {!needle && starters.value.length ? (
          <YStack gap="$3" pb="$6" width="100%">
            <SizableText size="$3" fontWeight="500" color="$ink">
              {say('Make something new')}
            </SizableText>
            <YStack
              display="grid"
              gridTemplateColumns="repeat(4, minmax(0, 1fr))"
              gap="$3"
              width="100%"
            >
              {starters.value.slice(0, 4).map((x) => (
                <Card key={x.slug} label={say('Start from {template}', { template: x.title })} disabled={Boolean(busy) && busy !== x.slug} onPress={() => void take(x.slug, x.title)}>
                  <Shot slug={x.slug} />
                  <YStack px="$3" py="$2.5">
                    <SizableText size="$2" color="$ink" numberOfLines={1}>
                      {busy === x.slug ? say('Copying…') : x.title}
                    </SizableText>
                  </YStack>
                </Card>
              ))}
            </YStack>
          </YStack>
        ) : null}
        {list.error ? (
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$7" gap="$3" items="center">
            <SizableText size="$2" color="var(--state-error)" text="center">
              {say(list.error.message)}
            </SizableText>
            <Button size="sm" variant="outline" onPress={list.reload}>
              {say('Try again')}
            </Button>
          </YStack>
        ) : !host.person ? (
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$7" gap="$3" items="center">
            <SizableText size="$2" color="$soft" text="center">
              {say('Sign in to see what your organization has built.')}
            </SizableText>
            <Button size="sm" variant="primary" onPress={() => host.signIn?.()}>
              {say('Sign in')}
            </Button>
          </YStack>
        ) : list.loading && list.value.length === 0 ? (
          <YStack gap="$3" aria-busy={true}>
            <SizableText size="$2" color="$soft">
              {say('Reading your projects…')}
            </SizableText>
            <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" bg="$panel" overflow="hidden">
              {[0, 1, 2].map((i) => (
                <XStack key={i} items="center" gap="$3" px="$3" py="$3" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                  <Skeleton layout="block" width={16} height={16} rounded={999} shrink={0} />
                  <YStack flex={1} gap="$1.5">
                    <Skeleton layout="block" width="36%" height={14} rounded="$2" />
                    <Skeleton layout="block" width="52%" height={12} rounded="$2" />
                  </YStack>
                </XStack>
              ))}
            </YStack>
          </YStack>
        ) : filtered.length === 0 ? (
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$7" gap="$3" items="center">
            <SizableText size="$2" color="$soft" text="center">
              {needle ? say('Nothing matches.') : `${say('Nothing built yet.')} ${say('Describe something on the New page and it lands here.')}`}
            </SizableText>
            {needle ? null : (
              <Button size="sm" variant="primary" onPress={() => host.go('')}>
                {say('New session')}
              </Button>
            )}
          </YStack>
        ) : (
          <YStack gap="$5">
            {months(filtered, say).map((g) => (
              <YStack key={g.label} gap="$2">
                <SizableText size="$2" color="$soft">
                  {g.label}
                </SizableText>
                <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" overflow="hidden">
                  {g.rows.map((p, i) => (
                    <XStack key={p.slug} items="center" pr="$2" borderTopWidth={i ? 1 : 0} borderColor="$borderColor" hoverStyle={{ bg: '$hover' }}>
                      <XStack
                        flex={1}
                        minW={0}
                        render="button"
                        aria-label={say('Open {name}', { name: p.name })}
                        onPress={() => host.go(p.slug)}
                        items="center"
                        gap="$3"
                        px="$3"
                        py="$2.5"
                        minH={44}
                        cursor="pointer"
                        $platform-web={{ textAlign: 'start' }}
                        focusVisibleStyle={RING_INSET}
                      >
                        {p.visibility === 'private' ? <Lock size={16} opacity={0.7} /> : <Globe size={16} opacity={0.7} />}
                        <YStack flex={1} minW={0} gap="$0.5">
                          <SizableText size="$3" color="$ink" numberOfLines={1}>
                            {p.name}
                          </SizableText>
                          <SizableText size="$1" color="$soft" numberOfLines={1}>
                            {[p.visibility === 'private' ? say('Private') : say('Public'), edited(when(p), say), p.live ? new URL(p.live).host : say('Not deployed yet')]
                              .filter(Boolean)
                              .join(' · ')}
                          </SizableText>
                        </YStack>
                      </XStack>
                      <DropdownMenu
                        trigger={
                          <XStack render="button" aria-label={say('Actions for {name}', { name: p.name })} px="$1.5" py="$1" rounded="$2" hoverStyle={{ bg: '$raised' }}>
                            <MoreHorizontal size={16} />
                          </XStack>
                        }
                        items={[
                          ...(p.live
                            ? [{ key: 'live', label: say('Open live site'), icon: <ExternalLink size={14} />, onSelect: () => window.open(p.live, '_blank', 'noopener,noreferrer') }]
                            : []),
                          { key: 'rename', label: say('Rename'), icon: <Pencil size={14} />, onSelect: () => setRenaming(p) },
                          { key: 'repo', label: say('Change repository'), icon: <GitBranch size={14} />, onSelect: () => setRelinking(p) },
                          {
                            key: 'visibility',
                            label: p.visibility === 'private' ? say('Make public') : say('Make private'),
                            icon: p.visibility === 'private' ? <Globe size={14} /> : <Lock size={14} />,
                            onSelect: () => void flip(p),
                          },
                          { type: 'separator' },
                          { key: 'delete', label: say('Delete'), icon: <Trash2 size={14} />, destructive: true, onSelect: () => setDeleting(p) },
                        ]}
                      />
                    </XStack>
                  ))}
                </YStack>
              </YStack>
            ))}
          </YStack>
        )}
      </YStack>
      {renaming ? (
        <Rename
          open
          onOpenChange={(o) => !o && setRenaming(null)}
          title={say('Rename')}
          name={renaming.name}
          run={async (name) => {
            await change(t, renaming.slug, { name })
            setNote(say('Renamed to {name}', { name }))
            list.reload()
          }}
        />
      ) : null}
      {relinking ? (
        <Address
          open
          onOpenChange={(o) => !o && setRelinking(null)}
          title={say('Change repository')}
          says={say('The GitHub repository {name} is built from. A push to its branch builds it again.', { name: relinking.name })}
          act={say('Save')}
          address={relinking.repo}
          branch={relinking.branch || 'main'}
          run={async (at, branch) => {
            const next = await change(t, relinking.slug, { repo: { url: clone(at), branch } })
            setNote(say('{name} is built from {repo} now', { name: relinking.name, repo: address(next.repo) || `${at.owner}/${at.name}` }))
            list.reload()
          }}
        />
      ) : null}
      {deleting ? (
        <Confirm
          open
          onOpenChange={(o) => !o && setDeleting(null)}
          title={say('Delete {name}?', { name: deleting.name })}
          says={`${say('Its site stops answering, its releases are dropped, and its address is released.')} ${say('This cannot be undone.')}`}
          act={say('Delete')}
          run={async () => {
            await remove(t, deleting.slug)
            setNote(say('{name} is deleted', { name: deleting.name }))
            list.reload()
          }}
        />
      ) : null}
    </YStack>
  )
}

/**
 * A starter's picture: a capture of its page from the top, 16:10. The frame has
 * the capture's own shape at every width the grid gives a card, and the image is
 * anchored at its top edge, so nothing of the page's header is cropped away.
 */
function Shot({ slug }: { slug: string }) {
  const [step, setStep] = useState(0)
  const base = SHOTS[step]
  return (
    <YStack width="100%" aspectRatio={16 / 10} bg="$hover" items="center" justify="center" position="relative" overflow="hidden">
      <YStack items="center" gap="$2" opacity={0.7}>
        <Boxes size={24} color="$soft" />
        <SizableText size="$1" color="$soft" fontFamily="$mono">
          {slug}
        </SizableText>
      </YStack>
      {base ? (
        <YStack
          render={<img src={`${base}/${encodeURIComponent(slug)}.webp`} alt="" loading="lazy" onError={() => setStep((s) => s + 1)} />}
          position="absolute"
          t={0}
          r={0}
          b={0}
          l={0}
          width="100%"
          height="100%"
          $platform-web={{ objectFit: 'cover', objectPosition: 'top' }}
        />
      ) : null}
    </YStack>
  )
}

export function Templates() {
  const { t: say } = useI18n()
  const host = useHost()
  const t = useTarget()
  const desktop = Boolean(useMedia().sm)
  const list = useRead(() => templates(t), [] as Template[], [t.api])
  const { busy, failed, take, dialog } = useFork()
  const [q, setQ] = useState('')
  const [cat, setCat] = useState('')
  const [moreCats, setMoreCats] = useState(false)

  const categories = useMemo(() => {
    const seen = new Set<string>()
    const out: string[] = []
    for (const x of list.value) {
      if (x.category && !seen.has(x.category)) {
        seen.add(x.category)
        out.push(x.category)
      }
    }
    return out
  }, [list.value])

  const allCategoryPills = useMemo(
    () => [{ id: '', label: say('All') }, ...categories.map((c) => ({ id: c, label: c }))],
    [categories, say],
  )
  const catCap = desktop ? 6 : 4
  const catOverflow = allCategoryPills.length > catCap
  const visibleCategoryPills =
    !moreCats && catOverflow
      ? (() => {
          const base = allCategoryPills.slice(0, catCap)
          if (base.some((p) => p.id === cat)) return base
          const active = allCategoryPills.find((p) => p.id === cat)
          return active ? [...base.slice(0, Math.max(1, catCap - 1)), active] : base
        })()
      : allCategoryPills
  const hiddenCatCount = allCategoryPills.length - visibleCategoryPills.length

  const filtered = useMemo(() => {
    const needle = q.trim().toLowerCase()
    return list.value.filter((x) => {
      if (cat && x.category !== cat) return false
      if (!needle) return true
      return `${x.title} ${x.slug} ${x.framework} ${x.category} ${x.description}`.toLowerCase().includes(needle)
    })
  }, [list.value, cat, q])
  const narrowed = q.trim() !== '' || cat !== ''

  return (
    <YStack flex={1} minH={0} overflow="scroll" px="$6" py="$6" items="center">
      {dialog}
      <YStack width="100%" maxW={1080}>
        <Head title={say('Templates')} says={`${say('Start from a working app.')} ${say('Picking one copies its repository into a project of yours.')}`} />
        {failed ? (
          <SizableText size="$2" color="$soft" role="status" pb="$3">
            {failed}
          </SizableText>
        ) : null}
        {list.value.length > 0 ? (
          <YStack gap="$2.5">
            <YStack maxW={420} width="100%">
              <Input
                value={q}
                onChangeText={setQ}
                placeholder={say('Search templates…')}
                aria-label={say('Search templates')}
                startAdornment={<Search size={14} color="$soft" />}
                endAdornment={
                  q ? (
                    <XStack
                      render="button"
                      aria-label={say('Clear search')}
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
            {categories.length > 1 ? (
              <XStack role="group" aria-label={say('Category')} gap="$1.5" rowGap="$1.5" flexWrap="wrap" width="100%">
                {visibleCategoryPills.map((c) => {
                  const on = cat === c.id
                  return (
                    <XStack
                      key={c.id || 'all'}
                      render="button"
                      aria-pressed={on}
                      aria-label={c.label}
                      onPress={() => setCat(c.id)}
                      items="center"
                      px="$3"
                      py="$1"
                      minH={32}
                      focusVisibleStyle={RING}
                    >
                      <SizableText size="$1" fontWeight={on ? '500' : '400'} color={on ? '$background' : '$soft'}>
                        {c.label}
                      </SizableText>
                    </XStack>
                  )
                })}
                {catOverflow ? (
                  <XStack
                    render="button"
                    aria-expanded={moreCats}
                    aria-label={moreCats ? say('Show fewer categories') : say('Show {n} more categories', { n: hiddenCatCount })}
                    onPress={() => setMoreCats((v) => !v)}
                    items="center"
                    px="$3"
                    py="$1"
                    minH={32}
                    focusVisibleStyle={RING}
                  >
                    <SizableText size="$1" fontWeight="500" color="$ink">
                      {moreCats ? say('Show less') : say('+{n} more', { n: hiddenCatCount })}
                    </SizableText>
                  </XStack>
                ) : null}
              </XStack>
            ) : null}
          </YStack>
        ) : null}
        {list.error ? (
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$8" gap="$3" items="center">
            <SizableText size="$2" color="var(--state-error)" text="center">
              {say(list.error.message)}
            </SizableText>
            <Button size="sm" variant="outline" onPress={list.reload}>
              {say('Try again')}
            </Button>
          </YStack>
        ) : list.loading && !list.value.length ? (
          <YStack gap="$3" aria-busy={true}>
            <SizableText size="$2" color="$soft">
              {say('Reading templates…')}
            </SizableText>
            <YStack display="grid" gridTemplateColumns="repeat(auto-fill, minmax(min(100%, 260px), 1fr))" gap="$3" width="100%">
              {[0, 1, 2, 3, 4, 5].map((i) => (
                <YStack key={i} rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel" overflow="hidden">
                  <Skeleton layout="block" width="100%" aspectRatio={16 / 10} rounded={0} />
                  <YStack p="$3" gap="$1.5">
                    <Skeleton layout="block" width="55%" height={15} rounded="$2" />
                    <Skeleton layout="block" width="85%" height={12} rounded="$2" />
                  </YStack>
                </YStack>
              ))}
            </YStack>
          </YStack>
        ) : !list.value.length ? (
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$8" gap="$3" items="center">
            <YStack items="center" gap="$1" maxW={420}>
              <SizableText size="$4" fontWeight="500" color="$ink" text="center">
                {say('No templates right now')}
              </SizableText>
              <SizableText size="$2" color="$soft" text="center">
                {`${say('No templates here right now.')} ${say('The whole catalog has more.')}`}
              </SizableText>
            </YStack>
            <XStack gap="$2" flexWrap="wrap" justify="center">
              <Button size="sm" variant="outline" onPress={() => host.open('https://gallery.hanzo.ai')}>
                <ExternalLink size={14} />
                {say('The whole catalog')}
              </Button>
              <Button size="sm" variant="primary" onPress={() => host.go('')}>
                {say('Start a new run')}
              </Button>
            </XStack>
          </YStack>
        ) : !filtered.length ? (
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$4" bg="$panel" px="$5" py="$8" gap="$3" items="center">
            <SizableText size="$2" color="$soft" text="center">
              {say('Nothing matches.')}
            </SizableText>
            {narrowed ? (
              <Button
                size="sm"
                variant="outline"
                onPress={() => {
                  setQ('')
                  setCat('')
                }}
              >
                {say('Clear filters')}
              </Button>
            ) : null}
          </YStack>
        ) : (
          <YStack display="grid" gridTemplateColumns="repeat(auto-fill, minmax(min(100%, 260px), 1fr))" gap="$3" width="100%">
            {filtered.map((x) => (
              <Card key={x.slug} label={say('Start from {template}', { template: x.title })} disabled={Boolean(busy) && busy !== x.slug} onPress={() => void take(x.slug, x.title)}>
                <Shot slug={x.slug} />
                <YStack p="$3" gap="$1">
                  <SizableText size="$3" color="$ink" numberOfLines={1}>
                    {busy === x.slug ? say('Copying…') : x.title}
                  </SizableText>
                  {x.framework ? (
                    <SizableText size="$1" color="$soft" numberOfLines={1}>
                      {x.framework}
                    </SizableText>
                  ) : null}
                  <SizableText size="$1" color="$soft" numberOfLines={2}>
                    {x.description}
                  </SizableText>
                </YStack>
              </Card>
            ))}
          </YStack>
        )}
        <YStack pt="$5">
          <Out href="https://gallery.hanzo.ai">
            <SizableText size="$2" color="$soft">
              {say('The whole catalog')}
            </SizableText>
            <ExternalLink size={12} opacity={0.6} />
          </Out>
        </YStack>
      </YStack>
    </YStack>
  )
}
