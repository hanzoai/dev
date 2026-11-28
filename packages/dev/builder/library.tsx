/**
 * The Library: everything you made — in Chat, in Dev, in Studio and in
 * workflows — in one place, newest first (api/library.ts).
 *
 *   -/artifacts           the Library; `?item=<id>` opens one item's preview
 *   -/artifacts/new       the Library under Make something new (make.tsx)
 *
 * A tile opens its item in the side panel (panel.tsx), one tab per item, so a
 * few can be read side by side; on a phone the panel is a sheet. Each item
 * carries what can be done with it: open where it was made, download, share
 * with the org, copy its link, rename, move to a project, use in Studio or in
 * Chat, and delete. What a colleague shared is drawn with their name and offers
 * only what a reader may do with it.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); an item's name, a
 * colleague's, a project's and what an item holds are drawn as given.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import {
  AudioLines,
  Box,
  Check,
  Code2,
  Copy,
  Download,
  ExternalLink,
  FileText,
  FolderInput,
  Image as Picture,
  LayoutGrid,
  List,
  MessageSquare,
  MoreHorizontal,
  Pencil,
  Plus,
  Presentation,
  Search,
  Share2,
  Sparkles,
  Trash2,
  Type,
  Video,
  X,
} from '@hanzogui/lucide-icons-2'
import { phrase } from '@hanzo/i18n'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { Button, Dialog, DialogContent, DialogTitle, DropdownMenu, Input, Skeleton, toast, type DropdownMenuProps } from '@hanzo/ui'
import { ChipSelect } from '@hanzo/ui/product'
import { useCallback, useEffect, useMemo, useRef, useState, type ReactNode } from 'react'

import { copy as toClipboard } from './clipboard.ts'
import { ago } from './ago.ts'
import { told } from './api/call.ts'
import { change, days, get, items as list, KINDS, remove, size, SOURCE, type Item, type Kind as ItemKind, type Scope, type Source } from './api/library.ts'
import { Confirm, Rename } from './ask.tsx'
import { useKept, useProjects, useRead } from './data.ts'
import { useHost, useTarget } from './host.tsx'
import { Make } from './make.tsx'
import { KEPT } from './nav.tsx'
import { path, route } from './route.ts'
import { Panel, type Kind } from './panel.tsx'
import { gap, pane, RING, RING_INSET } from './pane.ts'
import { Sites } from './shelf.tsx'
import { useDeck } from './tabs.ts'

type MenuItems = NonNullable<DropdownMenuProps['items']>

/** Where an item was made: Chat, Dev and Studio by their names, the rest in words, marked (`phrase`) and drawn through `t`. */
const FROM: readonly { id: Source | ''; label: string }[] = [
  { id: '', label: phrase('Anywhere') },
  { id: 'chat', label: 'Chat' },
  { id: 'dev', label: 'Dev' },
  { id: 'studio', label: 'Studio' },
  { id: 'workflow', label: phrase('Workflows') },
]

const WHOSE: readonly { id: Scope; label: string }[] = [
  { id: 'all', label: phrase('Mine and shared') },
  { id: 'mine', label: phrase('Mine') },
  { id: 'shared', label: phrase('Shared with me') },
]

/** A day's list, named: the days the Library says in words each a sentence, a month as given. */
const MADE: Record<string, string> = { Today: phrase('Made today'), Yesterday: phrase('Made yesterday'), Earlier: phrase('Made earlier') }
const made = (label: string, t: Say): string => (MADE[label] ? t(MADE[label]) : t('Made {month}', { month: label.toLowerCase() }))

/** How many items a page reads. */
const PAGE = 60

/** The side panel's scope: the items a reader opened, kept as they left them. */
const DECK = 'library'
const TAB = 'item:'

/** A kind's mark. */
function glyph(kind: ItemKind, n = 18): ReactNode {
  switch (kind) {
    case 'image':
      return <Picture size={n} />
    case 'video':
      return <Video size={n} />
    case 'audio':
      return <AudioLines size={n} />
    case 'document':
      return <FileText size={n} />
    case 'slides':
      return <Presentation size={n} />
    case 'type':
      return <Type size={n} />
    case 'model':
      return <Box size={n} />
    case 'code':
      return <Code2 size={n} />
    default:
      return <FileText size={n} />
  }
}

/** Known same-origin agent stills shelved by Dev, used if the signed store URL is unreachable. */
const AGENT_STILLS = new Set([
  'antje.jpg', 'art.png', 'biz.png', 'creative.png', 'cyber.png', 'data.png', 'des.png', 'dev.png',
  'doc.png', 'ed.png', 'edu.png', 'fin.png', 'gov.png', 'huma.png', 'lang.png', 'law.png', 'mana.png',
  'mark.png', 'mo.png', 'mu.png', 'opera.png', 'rev.png', 'sal.png', 'sci.png', 'su.png', 'vi.png',
  'vid.png', 'zach.jpg',
])

function localStill(x: Item): string {
  const name = x.name.trim().toLowerCase()
  if (AGENT_STILLS.has(name)) return `/agents/${name}`
  return ''
}

/** The still a tile shows: the item's preview, or an image's own bytes. */
const still = (x: Item): string => x.preview || (x.kind === 'image' && x.mime !== 'image/svg+xml' ? x.url : '') || localStill(x)

/** One square: the still, or the kind's mark on a quiet ground. */
export function Thumb({ x, size: n }: { x: Item; size?: number }) {
  const [fallback, setFallback] = useState(false)
  const [broken, setBroken] = useState(false)
  const primary = still(x)
  const backup = localStill(x)
  const src = fallback && backup ? backup : primary
  return (
    <YStack width={n ?? '100%'} height={n} aspectRatio={n ? undefined : 1} rounded="$3" overflow="hidden" bg="$raised" items="center" justify="center" shrink={0} aria-hidden>
      {src && !broken ? (
        <YStack
          render={
            <img
              src={src}
              alt=""
              loading="lazy"
              onError={() => {
                if (!fallback && backup && backup !== primary) setFallback(true)
                else setBroken(true)
              }}
            />
          }
          width="100%"
          height="100%"
          $platform-web={{ objectFit: 'cover', display: 'block' }}
        />
      ) : (
        <YStack opacity={0.7}>{glyph(x.kind, n ? Math.max(14, Math.round(n / 2.6)) : 28)}</YStack>
      )}
    </YStack>
  )
}

/** What a tile says under its name: where it came from, whose, and when. */
function meta(x: Item, t: Say): string {
  return [t(SOURCE[x.source]), x.mine ? '' : t('shared by {owner}', { owner: x.owner }), ago(x.created)].filter(Boolean).join(' · ')
}

/** An item's link, to paste: this page with the item named. */
function linkTo(x: Item): string {
  const u = new URL(window.location.href)
  u.search = ''
  u.hash = ''
  u.searchParams.set('item', x.id)
  return u.toString()
}

const ID = /^lib_[0-9a-f]{32}$/
/** Where a move into the Library leaves the item it should open. */
const WANT = 'hanzo.build.library.item'

/** Open the Library at one item: the move leaves its id for the screen to take. */
export function want(id: string): void {
  try {
    sessionStorage.setItem(WANT, id)
  } catch {
    // Without storage the Library opens without it.
  }
}

/** The item to open on arrival: one a move left, or the one `?item=` names. */
function named(): string {
  if (typeof window === 'undefined') return ''
  let left = ''
  try {
    left = sessionStorage.getItem(WANT) ?? ''
    sessionStorage.removeItem(WANT)
  } catch {
    left = ''
  }
  const id = left || new URLSearchParams(window.location.search).get('item') || ''
  return ID.test(id) ? id : ''
}

/** What can be done with one item, as menu rows. */
function useActs(onChanged: (x: Item) => void, onGone: (id: string) => void) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const [renaming, setRenaming] = useState<Item | null>(null)
  const [deleting, setDeleting] = useState<Item | null>(null)
  const [moving, setMoving] = useState<Item | null>(null)
  const [note, setNote] = useState('')

  const act = async (work: () => Promise<void>) => {
    setNote('')
    try {
      await work()
    } catch (e) {
      setNote(told(e, t))
    }
  }
  const copy = (text: string, said: string) =>
    act(async () => {
      if (!(await toClipboard(text))) throw new Error(t('This browser would not copy.'))
      toast.success(said)
    })

  const rows = (x: Item): MenuItems => [
    ...(x.origin ? [{ key: 'open', label: t('Open where it was made'), icon: <ExternalLink size={16} />, onSelect: () => host.open(x.origin) }] : []),
    ...(x.download ? [{ key: 'download', label: t('Download'), icon: <Download size={16} />, onSelect: () => window.open(x.download, '_blank', 'noopener,noreferrer') }] : []),
    ...(x.mine
      ? [
          {
            key: 'share',
            label: x.shared ? t('Stop sharing') : t('Share with organization'),
            icon: <Share2 size={16} />,
            description: x.shared ? t('Only you see it') : t('Everyone in it can open it'),
            onSelect: () =>
              void act(async () => {
                const next = await change(target, x.id, { shared: !x.shared })
                onChanged(next)
                setNote(next.shared ? t('{name} is shared with the organization', { name: next.name }) : t('{name} is yours alone', { name: next.name }))
              }),
          },
        ]
      : []),
    { key: 'link', label: t('Copy link'), icon: <Copy size={16} />, onSelect: () => void copy(linkTo(x), t('Link copied')) },
    ...(x.mine
      ? [
          { key: 'rename', label: t('Rename'), icon: <Pencil size={16} />, onSelect: () => setRenaming(x) },
          { key: 'move', label: t('Move to project'), icon: <FolderInput size={16} />, onSelect: () => setMoving(x) },
        ]
      : []),
    { key: 'studio', label: t('Use in Studio'), icon: <Sparkles size={14} />, onSelect: () => host.open(`/design?asset=${encodeURIComponent(x.id)}`) },
    { key: 'chat', label: t('Use in Chat'), icon: <MessageSquare size={14} />, onSelect: () => host.open(`/chat?attach=${encodeURIComponent(x.id)}`) },
    ...(x.mine
      ? [
          { type: 'separator' as const },
          { key: 'delete', label: t('Delete'), icon: <Trash2 size={14} />, destructive: true, onSelect: () => setDeleting(x) },
        ]
      : []),
  ]

  const dialogs = (
    <>
      <Rename
        open={Boolean(renaming)}
        onOpenChange={(o) => !o && setRenaming(null)}
        title={t('Rename')}
        name={renaming?.name ?? ''}
        run={async (name) => {
          const next = await change(target, renaming!.id, { name })
          onChanged(next)
          toast.success(t('Renamed to {name}', { name: next.name }))
        }}
      />
      <Confirm
        open={Boolean(deleting)}
        onOpenChange={(o) => !o && setDeleting(null)}
        title={t('Delete {name}?', { name: deleting?.name ?? '' })}
        says={t('It leaves the Library and its file is deleted. Anyone it was shared with loses it too.')}
        act={t('Delete')}
        run={async () => {
          const dead = deleting!
          await remove(target, dead.id)
          onGone(dead.id)
          toast.success(t('{name} deleted', { name: dead.name }))
        }}
      />
      <Move
        item={moving}
        onClose={() => setMoving(null)}
        onMoved={(next) => {
          onChanged(next)
          setNote(next.project ? t('{name} is in {project}', { name: next.name, project: next.project }) : t('{name} is in no project', { name: next.name }))
        }}
      />
    </>
  )
  return { rows, dialogs, note }
}

/** Choosing the project an item belongs to, or none. */
function Move({ item: x, onClose, onMoved }: { item: Item | null; onClose: () => void; onMoved: (x: Item) => void }) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const projects = useProjects(target, Boolean(host.person) && Boolean(x))
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState('')
  const [q, setQ] = useState('')
  useEffect(() => {
    if (!x) {
      setQ('')
      setNote('')
    }
  }, [x])
  const needle = q.trim().toLowerCase()
  const choices = useMemo(
    () =>
      [{ slug: '', name: t('No project') }, ...projects.value].filter(
        (p) => !needle || `${p.name} ${p.slug}`.toLowerCase().includes(needle),
      ),
    [projects.value, needle, t],
  )
  const pick = async (project: string) => {
    if (!x) return
    setBusy(true)
    setNote('')
    try {
      onMoved(await change(target, x.id, { project }))
      onClose()
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setBusy(false)
    }
  }
  return (
    <Dialog open={Boolean(x)} onOpenChange={(o) => !o && !busy && onClose()}>
      <DialogContent maxW={420} showCloseButton={false} gap="$3">
        <DialogTitle>{t('Move to project')}</DialogTitle>
        {projects.value.length > 6 ? (
          <Input
            value={q}
            onChangeText={setQ}
            placeholder={t('Find a project…')}
            aria-label={t('Find a project')}
            startAdornment={<Search size={14} color="$soft" />}
          />
        ) : null}
        <YStack gap="$1" maxH={320} overflow="scroll">
          {projects.loading && !projects.value.length ? (
            <YStack gap="$2">
              <SizableText size="$2" color="$soft">
                {t('Reading projects…')}
              </SizableText>
              <Skeleton layout="block" height={36} rounded="$3" />
              <Skeleton layout="block" height={36} rounded="$3" />
              <Skeleton layout="block" height={36} rounded="$3" />
            </YStack>
          ) : projects.error ? (
            <SizableText size="$2" color="$soft">
              {told(projects.error, t)}
            </SizableText>
          ) : choices.length === 0 ? (
            <SizableText size="$2" color="$soft" py="$2">
              {t('Nothing matches.')}
            </SizableText>
          ) : (
            choices.map((p) => {
              const active = x?.project === p.slug
              return (
                <XStack
                  key={p.slug || 'none'}
                  render="button"
                  aria-label={p.slug ? t('Move to {project}', { project: p.name || p.slug }) : t('Move to no project')}
                  aria-current={active ? 'true' : undefined}
                  disabled={busy}
                  onPress={() => void pick(p.slug)}
                  px="$3"
                  py="$2"
                  minH={38}
                  items="center"
                  justify="space-between"
                  gap="$2"
                  $platform-web={{ textAlign: 'start' }}
                  focusVisibleStyle={RING}
                >
                  <SizableText size="$2" color="$ink" numberOfLines={1} flex={1}>
                    {p.name || p.slug}
                  </SizableText>
                  {active ? <Check size={14} color="$ink" /> : null}
                </XStack>
              )
            })
          )}
        </YStack>
        {note ? (
          <SizableText size="$1" color="$soft" role="status">
            {note}
          </SizableText>
        ) : null}
        <XStack justify="flex-end">
          <Button size="sm" variant="ghost" disabled={busy} onPress={onClose}>
            {t('Cancel')}
          </Button>
        </XStack>
      </DialogContent>
    </Dialog>
  )
}

/** One item's menu, as a quiet control. */
function Acts({ x, rows }: { x: Item; rows: MenuItems }) {
  const { t } = useI18n()
  return (
    <DropdownMenu
      trigger={
        <XStack render="button" aria-label={t('Actions for {name}', { name: x.name })} width={30} height={30} items="center" justify="center" rounded="$2" hoverStyle={{ bg: '$hover' }} cursor="pointer" shrink={0}>
          <MoreHorizontal size={16} />
        </XStack>
      }
      items={rows}
    />
  )
}

/** One tile of the grid: the still, then its name and where it came from; its menu beside the name. */
function Tile({ x, onOpen, rows }: { x: Item; onOpen: () => void; rows: MenuItems }) {
  const { t } = useI18n()
  return (
    <YStack role="listitem" gap="$2" minW={0}>
      <YStack
        render="button"
        aria-label={t('Open {name}', { name: x.name })}
        onClick={onOpen}
        width="100%"
        rounded="$4"
        overflow="hidden"
        cursor="pointer"
        focusVisibleStyle={RING}
      >
        <Thumb x={x} />
      </YStack>
      <XStack items="flex-start" gap="$1" minW={0}>
        <YStack flex={1} minW={0} gap="$0.5">
          <SizableText size="$2" color="$ink" numberOfLines={1}>
            {x.name}
          </SizableText>
          <SizableText size="$1" color="$soft" numberOfLines={1}>
            {meta(x, t)}
          </SizableText>
        </YStack>
        <Acts x={x} rows={rows} />
      </XStack>
    </YStack>
  )
}

/** One row of the list: a small still, its name, where it came from, how large, and its menu. */
function Row({ x, first, onOpen, rows }: { x: Item; first?: boolean; onOpen: () => void; rows: MenuItems }) {
  const { t } = useI18n()
  return (
    <XStack role="listitem" items="center" gap="$2" pr="$2" borderTopWidth={first ? 0 : 1} borderColor="$borderColor" hoverStyle={{ bg: '$hover' }} minW={0}>
      <XStack
        render="button"
        aria-label={t('Open {name}', { name: x.name })}
        onPress={onOpen}
        flex={1}
        minW={0}
        minH={44}
        px="$3"
        py="$2"
        items="center"
        gap="$3"
        $platform-web={{ textAlign: 'start' }}
        focusVisibleStyle={RING_INSET}
      >
        <Thumb x={x} size={40} />
        <YStack flex={1} minW={0} gap="$0.5">
          <SizableText size="$2" color="$ink" numberOfLines={1}>
            {x.name}
          </SizableText>
          <SizableText size="$1" color="$soft" numberOfLines={1}>
            {meta(x, t)}
          </SizableText>
        </YStack>
        <SizableText size="$1" color="$soft" width={72} text="end" fontVariant={['tabular-nums']} $max-md={{ display: 'none' }}>
          {size(x.bytes)}
        </SizableText>
      </XStack>
      <Acts x={x} rows={rows} />
    </XStack>
  )
}

/** One item, read fresh so its signed URLs are live: the thing itself, then what it is. */
export function Preview({ id, rows }: { id: string; rows?: (x: Item) => MenuItems }) {
  const { t } = useI18n()
  const target = useTarget()
  const host = useHost()
  const one = useRead(host.person ? () => get(target, id) : null, null as Item | null, [target, id, host.person])
  const x = one.value
  if (!x) {
    if (!host.person) {
      return (
        <SizableText size="$2" color="$soft">
          {t('Sign in to open it.')}
        </SizableText>
      )
    }
    if (one.error) {
      return (
        <YStack gap="$2.5" items="flex-start">
          <SizableText size="$2" color="var(--state-error)">
            {told(one.error, t)}
          </SizableText>
          <Button size="sm" variant="outline" onPress={one.reload}>
            {t('Try again')}
          </Button>
        </YStack>
      )
    }
    return (
      <YStack gap="$3">
        <Skeleton layout="block" width="100%" height={240} rounded="$4" />
        <YStack gap="$1.5">
          <Skeleton layout="block" width="65%" height={18} rounded="$2" />
          <Skeleton layout="block" width="45%" height={14} rounded="$2" />
        </YStack>
        <SizableText size="$1" color="$soft">
          {t('Reading…')}
        </SizableText>
      </YStack>
    )
  }
  return (
    <YStack gap="$3" minH={0}>
      <Shown x={x} />
      <XStack items="flex-start" gap="$2">
        <YStack flex={1} minW={0} gap="$1">
          <SizableText size="$3" color="$ink">
            {x.name}
          </SizableText>
          <SizableText size="$1" color="$soft">
            {[meta(x, t), size(x.bytes), x.width && x.height ? `${x.width}×${x.height}` : '', x.project ? t('in {project}', { project: x.project }) : '', x.shared ? t('shared with the organization') : '']
              .filter(Boolean)
              .join(' · ')}
          </SizableText>
        </YStack>
        {rows ? <Acts x={x} rows={rows(x)} /> : null}
      </XStack>
      <XStack gap="$2" flexWrap="wrap" rowGap="$2">
        {x.origin ? (
          <Button size="sm" variant="outline" onPress={() => host.open(x.origin)}>
            <ExternalLink size={14} />
            {t('Open where it was made')}
          </Button>
        ) : null}
        {x.download ? (
          <Button size="sm" variant="outline" onPress={() => window.open(x.download, '_blank', 'noopener,noreferrer')}>
            <Download size={14} />
            {t('Download')}
          </Button>
        ) : null}
      </XStack>
    </YStack>
  )
}

/** The thing itself, drawn the way its kind is read. */
function Shown({ x }: { x: Item }) {
  const [fallback, setFallback] = useState(false)
  const backup = localStill(x)
  const imgSrc = fallback && backup ? backup : x.url || backup
  if (!x.url && !backup) return <Thumb x={x} />
  if (x.kind === 'image' && x.mime !== 'image/svg+xml')
    return (
      <YStack
        render={
          <img
            src={imgSrc}
            alt={x.name}
            onError={() => {
              if (!fallback && backup && backup !== x.url) setFallback(true)
            }}
          />
        }
        width="100%"
        maxH="60vh"
        $platform-web={{ objectFit: 'contain', display: 'block' }}
      />
    )
  if (!x.url) return <Thumb x={x} />
  if (x.kind === 'video')
    return (
      <YStack
        render={<video src={x.url} poster={x.preview || undefined} controls preload="metadata" aria-label={x.name} />}
        width="100%"
        maxH="60vh"
        $platform-web={{ display: 'block' }}
      />
    )
  if (x.kind === 'audio') return <YStack render={<audio src={x.url} controls preload="metadata" aria-label={x.name} />} width="100%" />
  if (x.mime === 'application/pdf')
    return (
      <YStack
        render={<iframe src={x.url} title={x.name} />}
        width="100%"
        height="60vh"
        maxH="60vh"
        $platform-web={{ display: 'block' }}
      />
    )
  if (x.mime.startsWith('text/') || x.kind === 'code' || x.mime === 'application/json') return <Text x={x} />
  return <Thumb x={x} />
}

/** A text item's first 200 KB, as written; in a frame where the store does not answer this page's read. */
function Text({ x }: { x: Item }) {
  const { t } = useI18n()
  const read = useRead(
    async () => {
      const res = await fetch(x.url, { cache: 'no-store' })
      if (!res.ok) throw new Error(`The file answered ${res.status}`)
      return (await res.text()).slice(0, 200_000)
    },
    '',
    [x.url],
  )
  if (read.error)
    return (
      <YStack
        render={<iframe src={x.url} title={x.name} />}
        width="100%"
        height="60vh"
        $platform-web={{ display: 'block' }}
      />
    )
  return (
    <YStack rounded="$3" bg="$raised" borderWidth={1} borderColor="$borderColor" p="$3" maxH="60vh" overflow="scroll">
      {read.loading && !read.value ? (
        <YStack gap="$2">
          <Skeleton layout="block" width="90%" height={13} rounded="$2" />
          <Skeleton layout="block" width="76%" height={13} rounded="$2" />
          <Skeleton layout="block" width="82%" height={13} rounded="$2" />
          <SizableText size="$1" color="$soft">
            {t('Reading…')}
          </SizableText>
        </YStack>
      ) : (
        <SizableText size="$1" color="$ink" fontFamily="$mono" $platform-web={{ whiteSpace: 'pre-wrap' }}>
          {read.value}
        </SizableText>
      )}
    </YStack>
  )
}

/** A run's outputs as small tiles: each opens in the Library. */
export function Outputs({ ids }: { ids: string[] }) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const key = ids.join(',')
  const got = useRead(host.person && ids.length ? () => Promise.all(ids.slice(0, 12).map((id) => get(target, id).catch(() => null))) : null, [] as (Item | null)[], [target, key, host.person])
  const shown = got.value.filter((x): x is Item => x !== null)
  if (!ids.length) return null
  return (
    <XStack gap="$2" flexWrap="wrap" rowGap="$2" pt="$1.5" role="list" aria-label={t('What it made')}>
      {shown.map((x) => (
        <XStack key={x.id} role="listitem">
          <XStack
            render="button"
            aria-label={t('Open {name} in the Library', { name: x.name })}
            onPress={() => {
              want(x.id)
              host.go('-/artifacts')
            }}
            items="center"
            gap="$2"
            minH={38}
            paddingInlineEnd="$2.5"
            rounded="$3"
            borderWidth={1}
            borderColor="$borderColor"
            overflow="hidden"
            cursor="pointer"
            maxW={240}
            focusVisibleStyle={RING}
          >
            <Thumb x={x} size={36} />
            <SizableText size="$1" color="$ink" numberOfLines={1}>
              {x.name}
            </SizableText>
          </XStack>
        </XStack>
      ))}
      {got.loading && !shown.length ? (
        <SizableText size="$1" color="$soft">
          {ids.length === 1 ? t('Reading what it made…') : t('Reading the {n} things it made…', { n: ids.length })}
        </SizableText>
      ) : null}
    </XStack>
  )
}

/** A list read page by page: the first on every change of filter, the next on reaching its end. */
function usePages(load: ((cursor: string, signal: AbortSignal) => Promise<{ data: Item[]; next: string }>) | null, key: string) {
  const [rows, setRows] = useState<Item[]>([])
  const [next, setNext] = useState('')
  const [error, setError] = useState<Error | null>(null)
  const [loading, setLoading] = useState(Boolean(load))
  const busy = useRef<AbortController | null>(null)
  const [tick, setTick] = useState(0)

  const read = useCallback(
    (cursor: string) => {
      if (!load) return
      busy.current?.abort()
      const ctl = new AbortController()
      busy.current = ctl
      setLoading(true)
      load(cursor, ctl.signal)
        .then((page) => {
          if (ctl.signal.aborted) return
          setRows((was) => (cursor ? [...was, ...page.data.filter((x) => !was.some((w) => w.id === x.id))] : page.data))
          setNext(page.next)
          setError(null)
        })
        .catch((e: unknown) => {
          if (!ctl.signal.aborted) setError(e as Error)
        })
        .finally(() => {
          if (!ctl.signal.aborted) setLoading(false)
        })
    },
    [load],
  )

  useEffect(() => {
    setRows([])
    setNext('')
    read('')
    return () => busy.current?.abort()
    // `key` names the filter; `read` follows it.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, tick, Boolean(load)])

  const more = useCallback(() => {
    if (next && !loading) read(next)
  }, [next, loading, read])
  const patch = (x: Item) => setRows((was) => was.map((w) => (w.id === x.id ? x : w)))
  const drop = (id: string) => setRows((was) => was.filter((w) => w.id !== id))
  return { rows, next, error, loading, more, patch, drop, reload: () => setTick((n) => n + 1) }
}

/** Calls `on` when the element it is set on comes into view. */
function useEnd(on: () => void) {
  const seen = useRef(on)
  seen.current = on
  const [el, setEl] = useState<Element | null>(null)
  useEffect(() => {
    if (!el || typeof IntersectionObserver === 'undefined') return
    const io = new IntersectionObserver((entries) => entries.some((e) => e.isIntersecting) && seen.current(), { rootMargin: '400px' })
    io.observe(el)
    return () => io.disconnect()
  }, [el])
  return (node: unknown) => setEl(node instanceof Element ? node : null)
}


export function Library() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const media = useMedia()
  const wide = media.md
  const desktop = Boolean(media.sm)
  // The kind is the address's (-/artifacts/<kind>), so the sidebar, a link and Back all name it.
  const at = route(host.path)
  const sitesOnly = at.kind === 'screen' && at.of === 'sites'
  const making = at.kind === 'screen' && at.of === 'new'
  const kind: ItemKind | '' = at.kind === 'screen' && at.of && at.of !== 'sites' && at.of !== 'new' ? (at.of as ItemKind) : ''
  const setKind = (k: ItemKind | 'sites' | 'new' | '') => host.go(path({ kind: 'screen', screen: 'artifacts', ...(k ? { of: k } : {}) }))
  const [source, setSource] = useState<Source | ''>('')
  const [scope, setScope] = useState<Scope>('all')
  const [typed, setTyped] = useState('')
  const [q, setQ] = useState('')
  const [siteMatches, setSiteMatches] = useState(0)
  const [moreKinds, setMoreKinds] = useState(false)
  const [view, setView] = useKept<'grid' | 'list'>('hanzo.build.library.view', 'grid')
  // One org's items are not another's: each org's open tabs are kept apart.
  const side = useDeck(`${DECK}.${host.org ?? ''}`, { tabs: [], at: null })
  const [shown, setShown] = useState(false)

  // A search is sent once the typing settles.
  useEffect(() => {
    const id = setTimeout(() => setQ(typed.trim()), 250)
    return () => clearTimeout(id)
  }, [typed])

  const load = useMemo(() => (signed && !sitesOnly ? (cursor: string, signal: AbortSignal) => list(target, { kind, source, scope, q, limit: PAGE, cursor }, { signal }) : null), [signed, sitesOnly, target, kind, source, scope, q])
  const pages = usePages(load, `${kind}|${source}|${scope}|${q}|${target.org}`)
  const end = useEnd(pages.more)
  const reload = pages.reload
  useEffect(() => {
    window.addEventListener(KEPT, reload)
    return () => window.removeEventListener(KEPT, reload)
  }, [reload])

  const look = useCallback(
    (x: { id: string; name: string }) => {
      side.open({ id: `${TAB}${x.id}`, kind: `${TAB}${x.id}`, title: x.name })
      setShown(true)
    },
    [side],
  )
  // An item a link named opens once, on arrival.
  const arrived = useRef(false)
  useEffect(() => {
    if (arrived.current || !signed) return
    arrived.current = true
    const id = named()
    if (id) get(target, id).then(look, () => look({ id, name: t('Item') }))
  }, [signed, target, look, t])

  const acts = useActs(
    (x) => {
      pages.patch(x)
      if (side.tabs.some((tab) => tab.kind === `${TAB}${x.id}`)) side.rename(`${TAB}${x.id}`, x.name)
    },
    (id) => {
      pages.drop(id)
      side.shut(`${TAB}${id}`)
    },
  )

  const kinds: Kind[] = side.tabs
    .filter((tab) => tab.kind.startsWith(TAB))
    .map((tab) => ({ id: tab.kind, label: tab.title, render: () => <Preview key={tab.kind} id={tab.kind.slice(TAB.length)} rows={acts.rows} /> }))
  const panel = (sheet: boolean) => <Panel side={side} kinds={kinds} sheet={sheet} onHide={() => setShown(false)} label={t('Item preview')} />
  const open = shown && side.tabs.length > 0

  const groups = days(pages.rows)
  const narrowed = Boolean(kind || source || q || scope !== 'all')
  const showSites = !sitesOnly && signed && !kind && (source === '' || source === 'dev') && scope !== 'shared'
  useEffect(() => {
    if (!showSites) setSiteMatches(0)
  }, [showSites])
  const emptyUnfiltered = signed && !sitesOnly && !pages.loading && !pages.error && !pages.rows.length && !narrowed
  const clearFilters = () => {
    setTyped('')
    setQ('')
    setSource('')
    setScope('all')
    if (kind || sitesOnly) setKind('')
  }
  const make = (primary: boolean) => (
    <Button
      size="sm"
      variant={primary ? 'primary' : 'outline'}
      shrink={0}
      onPress={() => setKind('new')}
    >
      <Plus size={14} />
      {t('Make something new')}
    </Button>
  )
  // The choices in the reader's words; Chat, Dev and Studio stay their names.
  const from = FROM.map((f) => ({ ...f, label: t(f.label) }))
  const whose = WHOSE.map((w) => ({ ...w, label: t(w.label) }))

  // Progressive disclosure: cap visible kind pills to 6 on desktop (4 on mobile), keeping the active kind visible.
  const allKindPills = useMemo(
    () => [
      ...KINDS.map((k) => ({ id: k.id as ItemKind | '' | 'sites', label: t(k.label) })),
      { id: 'sites' as const, label: t('Sites') },
    ],
    [t],
  )
  const activeKindId: ItemKind | '' | 'sites' = sitesOnly ? 'sites' : kind
  const kindCap = desktop ? 6 : 4
  const kindOverflow = allKindPills.length > kindCap
  const visibleKindPills =
    !moreKinds && kindOverflow
      ? (() => {
          const base = allKindPills.slice(0, kindCap)
          if (base.some((p) => p.id === activeKindId)) return base
          const chosen = allKindPills.find((p) => p.id === activeKindId)
          return chosen ? [...base.slice(0, Math.max(1, kindCap - 1)), chosen] : base
        })()
      : allKindPills
  const hiddenKindCount = allKindPills.length - visibleKindPills.length

  return (
    <XStack flex={1} minH={0} minW={0} width="100%">
      <YStack flex={1} minH={0} minW={0} overflow="scroll" px="$6" py="$6" $max-md={{ px: '$4', py: '$4' }}>
        <YStack width="100%" maxW={1120} mx="auto" gap="$4">
          <XStack items="center" gap="$3">
            <SizableText render="h1" size="$7" fontWeight="600" color="$ink" flex={1}>
              {sitesOnly ? t('Sites') : KINDS.find((k) => k.id === kind)?.id ? t(KINDS.find((k) => k.id === kind)!.label) : t('Library')}
            </SizableText>
            <XStack items="center" gap="$2" shrink={0}>
              {/* A host whose sidebar lists the kinds has Make something new there. */}
              {host.sections ? null : make(!emptyUnfiltered && signed)}
              {sitesOnly ? null : (
                <XStack role="radiogroup" aria-label={t('View')} p={2} gap={2} rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$panel">
                  <ViewMark label={t('Grid')} on={view === 'grid'} onPress={() => setView('grid')}>
                    <LayoutGrid size={15} color={view === 'grid' ? '$ink' : '$soft'} />
                  </ViewMark>
                  <ViewMark label={t('List')} on={view === 'list'} onPress={() => setView('list')}>
                    <List size={15} color={view === 'list' ? '$ink' : '$soft'} />
                  </ViewMark>
                </XStack>
              )}
            </XStack>
          </XStack>

          <YStack gap="$2.5">
            {/* A host whose sidebar lists the kinds draws them there. */}
            {host.sections ? null : (
              <XStack role="radiogroup" aria-label={t('Kind')} gap="$1.5" rowGap="$1.5" flexWrap="wrap" width="100%">
                {visibleKindPills.map((k) => (
                  <KindChip key={k.id || 'all'} label={k.label} on={activeKindId === k.id} onClick={() => setKind(k.id)} />
                ))}
                {kindOverflow ? (
                  <XStack
                    render="button"
                    aria-expanded={moreKinds}
                    aria-label={moreKinds ? t('Show fewer kinds') : t('Show {n} more kinds', { n: hiddenKindCount })}
                    onPress={() => setMoreKinds((v) => !v)}
                    items="center"
                    px="$3"
                    py="$1"
                    minH={32}
                    focusVisibleStyle={RING}
                  >
                    <SizableText size="$1" fontWeight="500" color="$ink">
                      {moreKinds ? t('Show less') : t('+{n} more', { n: hiddenKindCount })}
                    </SizableText>
                  </XStack>
                ) : null}
              </XStack>
            )}
            {sitesOnly ? null : (
              <XStack gap="$2" rowGap="$2" items="center" flexWrap="wrap">
                <YStack flex={1} minW={220} maxW={420} $max-sm={{ minW: '100%', maxW: '100%' }}>
                  <Input
                    value={typed}
                    onChangeText={setTyped}
                    placeholder={t('Search your library')}
                    aria-label={t('Search your library')}
                    disabled={!signed}
                    startAdornment={<Search size={14} color="$soft" />}
                    endAdornment={
                      typed ? (
                        <XStack
                          render="button"
                          aria-label={t('Clear search')}
                          onPress={() => setTyped('')}
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
                      if (e.key === 'Escape' && typed) setTyped('')
                    }}
                  />
                </YStack>
                <XStack gap="$2" flexWrap="wrap" rowGap="$2" items="center">
                  <ChipSelect
                    name={t('From')}
                    label={from.find((f) => f.id === source)!.label}
                    chosen={from.find((f) => f.id === source)}
                    items={from}
                    onChange={(f) => setSource(f.id)}
                    placement="bottom-start"
                    width={200}
                  />
                  <ChipSelect
                    name={t('Whose')}
                    label={whose.find((w) => w.id === scope)!.label}
                    chosen={whose.find((w) => w.id === scope)}
                    items={whose}
                    onChange={(w) => setScope(w.id)}
                    placement="bottom-start"
                    width={220}
                  />
                  {narrowed ? (
                    <Button size="sm" variant="ghost" onPress={clearFilters}>
                      {t('Clear filters')}
                    </Button>
                  ) : null}
                </XStack>
              </XStack>
            )}
          </YStack>

          {acts.note ? (
            <SizableText size="$2" color="$soft" role="status">
              {acts.note}
            </SizableText>
          ) : null}

          {sitesOnly ? (
            <Sites />
          ) : !signed ? (
            <Said says={t('Sign in to see your library.')}>
              <Button size="sm" onPress={() => host.signIn?.()}>
                {t('Sign in')}
              </Button>
            </Said>
          ) : pages.error && !pages.rows.length ? (
            <Said says={told(pages.error, t)} error>
              <Button size="sm" variant="outline" onPress={pages.reload}>
                {t('Try again')}
              </Button>
            </Said>
          ) : pages.loading && !pages.rows.length ? (
            <YStack gap="$3" aria-busy={true}>
              <SizableText size="$2" color="$soft">
                {t('Reading…')}
              </SizableText>
              {view === 'grid' ? (
                <YStack gap={16} $platform-web={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(min(46%, 168px), 1fr))' }}>
                  {[0, 1, 2, 3, 4, 5, 6, 7].map((i) => (
                    <YStack key={i} gap="$2">
                      <Skeleton layout="block" width="100%" aspectRatio={1} rounded="$4" />
                      <Skeleton layout="block" width="72%" height={14} rounded="$2" />
                      <Skeleton layout="block" width="48%" height={12} rounded="$2" />
                    </YStack>
                  ))}
                </YStack>
              ) : (
                <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" bg="$panel" overflow="hidden">
                  {[0, 1, 2, 3, 4, 5].map((i) => (
                    <XStack key={i} items="center" gap="$3" px="$3" py="$2.5" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                      <Skeleton layout="block" width={40} height={40} rounded="$3" shrink={0} />
                      <YStack flex={1} gap="$1.5">
                        <Skeleton layout="block" width="42%" height={14} rounded="$2" />
                        <Skeleton layout="block" width="28%" height={12} rounded="$2" />
                      </YStack>
                    </XStack>
                  ))}
                </YStack>
              )}
            </YStack>
          ) : !pages.rows.length ? (
            narrowed ? (
              <YStack gap="$5">
                {showSites ? <Sites q={q} onMatch={setSiteMatches} /> : null}
                {!showSites || siteMatches === 0 ? (
                  <Said says={t('Nothing matches.')}>
                    <Button size="sm" variant="outline" onPress={clearFilters}>
                      {t('Clear filters')}
                    </Button>
                  </Said>
                ) : null}
              </YStack>
            ) : (
              <YStack gap="$5">
                <Said says={t('Nothing here yet. What you make in Chat, Dev and Studio lands here.')}>{make(true)}</Said>
                {showSites ? (
                  <YStack pt="$6" borderTopWidth={1} borderColor="$borderColor">
                    <Sites onMatch={setSiteMatches} />
                  </YStack>
                ) : null}
              </YStack>
            )
          ) : (
            <YStack gap="$5">
              {/* The org's deployed sites are Dev's builds: shown on All/Dev and filtered alongside library items. */}
              {showSites ? (
                <YStack pb={!q || siteMatches > 0 ? '$4' : 0} borderBottomWidth={!q || siteMatches > 0 ? 1 : 0} borderColor="$borderColor">
                  <Sites q={q} onMatch={setSiteMatches} />
                </YStack>
              ) : null}
              {groups.map((g) => (
                <YStack key={g.label} gap="$3" role="group" aria-label={t(g.label)}>
                  <SizableText size="$2" color="$soft" fontWeight="500">
                    {t(g.label)}
                  </SizableText>
                  {view === 'grid' ? (
                    <YStack role="list" aria-label={made(g.label, t)} gap={16} $platform-web={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(min(46%, 168px), 1fr))' }}>
                      {g.rows.map((x) => (
                        <Tile key={x.id} x={x} onOpen={() => look(x)} rows={acts.rows(x)} />
                      ))}
                    </YStack>
                  ) : (
                    <YStack role="list" aria-label={made(g.label, t)} borderWidth={1} borderColor="$borderColor" rounded="$3" bg="$panel" overflow="hidden">
                      {g.rows.map((x, i) => (
                        <Row key={x.id} x={x} first={i === 0} onOpen={() => look(x)} rows={acts.rows(x)} />
                      ))}
                    </YStack>
                  )}
                </YStack>
              ))}
              <YStack ref={end} height={1} />
              {pages.loading ? (
                <SizableText size="$1" color="$soft">
                  {t('Reading more…')}
                </SizableText>
              ) : pages.error ? (
                <SizableText size="$1" color="$soft" role="status">
                  {told(pages.error, t)}
                </SizableText>
              ) : null}
            </YStack>
          )}
        </YStack>
      </YStack>
      {open && !wide ? panel(true) : null}
      {open && wide ? (
        <YStack data-slot="library-side" {...pane} ml={gap} shrink={0} minH={0} width={440} maxW="45%">
          {panel(false)}
        </YStack>
      ) : null}
      {acts.dialogs}
      <Make open={making} onClose={() => host.go(path({ kind: 'screen', screen: 'artifacts' }), { replace: true })} />
    </XStack>
  )
}

function KindChip({ label, on, onClick }: { label: string; on: boolean; onClick: () => void }) {
  return (
    <XStack
      render="button"
      role="radio"
      aria-checked={on}
      aria-label={label}
      onPress={onClick}
      items="center"
      px="$3"
      py="$1"
      minH={32}
      shrink={0}
      focusVisibleStyle={RING}
    >
      <SizableText size="$1" fontWeight={on ? '500' : '400'} color={on ? '$background' : '$soft'}>
        {label}
      </SizableText>
    </XStack>
  )
}

function ViewMark({ label, on, onPress, children }: { label: string; on: boolean; onPress: () => void; children: ReactNode }) {
  return (
    <XStack
      render="button"
      role="radio"
      aria-checked={on}
      aria-label={label}
      onClick={onPress}
      minW={30}
      minH={28}
      items="center"
      justify="center"
      cursor="pointer"
      focusVisibleStyle={RING}
    >
      {children}
    </XStack>
  )
}

function Said({ says, error = false, children }: { says: string; error?: boolean; children?: ReactNode }) {
  return (
    <YStack rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel" px="$5" py="$8" gap="$3" items="center">
      <SizableText size="$2" color={error ? 'var(--state-error)' : '$soft'} text="center">
        {says}
      </SizableText>
      {children}
    </YStack>
  )
}
