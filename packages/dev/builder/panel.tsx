/**
 * The side panel, once, for Chat and Dev: tabs over what a conversation or a
 * run made, read and served (tabs.ts) — a browser's tabs beside the
 * conversation. The header shows them (`PanelToggle`, while they are hidden),
 * the strip's own toggle hides them, and Ctrl+Shift+B (⌘⇧B) or ⌘. / Ctrl+.
 * does either from anywhere but a field (`useKey`).
 *
 * The strip: the tabs, `+`, and at its end the surface's menu, pop-out and the
 * hide toggle. A page tab has a browser's bar — Back and Forward through the
 * addresses it went through, reload, the address, and its menu — and an empty
 * one offers the surface's suggestions.
 *
 * WHAT A PAGE CAN SHOW IS SAID, NOT FAKED. A host that draws pages itself
 * (`Host.browse`: Hanzo Desktop's native webviews) shows any address. A browser
 * page frames only what may be framed — this page's bytes and our own hosts
 * (`frames`) — and offers any other address in a new window instead of a frame
 * the site would refuse.
 *
 * The column the panel stands in is the host's (side.tsx `useSplit`): its
 * width and edge. The strip reads it for full screen (Escape returns) and, while
 * the panel has the middle column's room, the way back to it. On a phone the
 * panel is the whole window instead (`sheet`).
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { ArrowLeft, ArrowRight, ChevronRight, Columns2, ExternalLink, Globe, Maximize2, Minimize2, Minus, MoreHorizontal, PanelLeftOpen, Pin, Plus, RotateCcw, RotateCw, X } from '@hanzogui/lucide-icons-2'
import {
  ContextMenu,
  ContextMenuContent,
  ContextMenuItem,
  ContextMenuSeparator,
  ContextMenuTrigger,
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuSub,
  DropdownMenuSubContent,
  DropdownMenuSubTrigger,
  DropdownMenuTrigger,
} from '@hanzo/ui'
import { Fragment, useEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import { createPortal } from 'react-dom'

import { RING, RING_INSET } from './pane.ts'
import { copy } from './clipboard.ts'
import { useBrowse, useBrowser, type Browser, type Shown } from './host.tsx'
import { useSide } from './side.tsx'
import { destination, PAGE, type Deck, type Tab } from './tabs.ts'

/** A kind of tab the surface offers. */
export interface Kind {
  id: string
  label: string
  render: () => ReactNode
  /** Drawn edge to edge, scrolling itself: a terminal, a frame. */
  bleed?: boolean
  /** Its mark in the strip. */
  icon?: ReactNode
}

/** A place an empty tab offers. */
export interface Suggestion {
  title: string
  url: string
  /** An image of the place's own mark; its initial without one. */
  icon?: string
}

const MAC = typeof navigator !== 'undefined' && /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent)

/** The tabs' shortcut, as this platform writes it. */
const KEYS = MAC ? '⌘⇧B' : 'Ctrl+Shift+B'

/** Every key that shows and hides the tabs (tabs.ts `useKey`). */
const SHORTCUTS = 'Control+Shift+B Meta+Shift+B Control+Period Meta+Period'

/** A tab's title where it has none of its own. */
const BLANK = phrase('New tab')

/** On an icon that points along the line (Back, Forward): it turns in a right-to-left page. */
const MIRROR = { 'data-i18n-mirror': '' } as object

/** The row heights, px: the strip (the conversation header's row), and a page's bar. */
const STRIP = 56
const BAR = 48
/** A round control in a page's bar, px. */
const PILL = 34

/** A press on an icon control of the panel's: quiet until pointed at, lit while its thing is open. */
const quiet = (on = false, disabled = false) =>
  ({
    items: 'center',
    justify: 'center',
    shrink: 0,
    rounded: 8,
    borderWidth: 0,
    cursor: disabled ? 'default' : 'pointer',
    opacity: disabled ? 0.35 : 1,
    bg: on ? '$hover' : 'transparent',
    hoverStyle: disabled ? undefined : ({ bg: '$hover' } as const),
    focusVisibleStyle: RING,
  }) as const

/**
 * The header's control for the tabs, while they are hidden: an icon among the
 * header's icons. Open, the strip's own toggle stands at the panel's end and
 * this one is not drawn, so the page carries one toggle at a time. Its words
 * are the reader's language; the key chord is an argument, as this platform writes it.
 */
export function PanelToggle({ open, onToggle }: { open: boolean; onToggle: () => void }) {
  const { t } = useI18n()
  if (open) return null
  return (
    <XStack
      render="button"
      data-slot="side-toggle"
      aria-label={t('Show tabs')}
      aria-expanded={false}
      aria-keyshortcuts={SHORTCUTS}
      {...({ title: t('Show tabs ({keys})', { keys: KEYS }) } as object)}
      onPress={onToggle}
      width={32}
      height={32}
      {...quiet()}
    >
      <Plus size={16} />
    </XStack>
  )
}

/** Our own hosts: a page of theirs answers a frame on any of our pages. */
const OURS = ['hanzo.ai', 'hanzo.app', 'hanzo.bot']

/** Whether a browser page may frame an address: this page's bytes, this origin, and our own hosts. */
export function frames(url: string): boolean {
  let u: URL
  try {
    u = new URL(url, typeof window === 'undefined' ? 'https://hanzo.ai' : window.location.href)
  } catch {
    return false
  }
  if (u.protocol === 'blob:' || u.protocol === 'data:') return true
  if (typeof window !== 'undefined' && u.origin === window.location.origin) return true
  if (u.protocol !== 'https:') return false
  return OURS.some((h) => u.hostname === h || u.hostname.endsWith(`.${h}`))
}

/** A site or a preview we host: a page under one of our hosts, not this page's own origin or our apexes. */
export function hosted(url: string): boolean {
  let u: URL
  try {
    u = new URL(url)
  } catch {
    return false
  }
  if (u.protocol !== 'https:' || (typeof window !== 'undefined' && u.origin === window.location.origin)) return false
  return OURS.some((h) => u.hostname.endsWith(`.${h}`) && u.hostname !== `www.${h}`)
}

const suffix = (s: string): string => {
  const dot = s.lastIndexOf('.')
  return dot > 0 ? s.slice(dot + 1).toLowerCase() : ''
}

const PICTURES = ['png', 'jpg', 'jpeg', 'gif', 'webp', 'avif', 'svg']
const FRAMED = ['html', 'htm', 'txt', 'md', 'json', 'csv', 'pdf', 'css', 'js', 'ts']

/** The address a tab can be opened at in a window of its own: its own, or its bytes. Null for a kind or a blank page. */
function outward(tab: Tab | null): string | null {
  if (!tab || tab.kind !== PAGE) return null
  if (tab.body !== undefined) return URL.createObjectURL(new Blob([tab.body], { type: tab.type || 'text/plain' }))
  return tab.url || null
}

/** Opens a tab's address or bytes in a new window. */
function popOut(tab: Tab | null): void {
  const href = outward(tab)
  if (href) window.open(href, '_blank', 'noopener,noreferrer')
}

/** A place's mark: its image, else its initial on a quiet tile. */
function Mark({ icon, title }: { icon?: string; title: string }) {
  const [broken, setBroken] = useState(false)
  return (
    <XStack width={40} height={40} rounded={10} items="center" justify="center" bg="$edge" overflow="hidden" shrink={0}>
      {icon && !broken ? (
        <YStack
          render={<img src={icon} alt="" width={28} height={28} onError={() => setBroken(true)} />}
          $platform-web={{ objectFit: 'contain' }}
        />
      ) : (
        <SizableText size="$4" fontWeight="600" color="$ink" aria-hidden>
          {(title.trim()[0] ?? '·').toUpperCase()}
        </SizableText>
      )}
    </XStack>
  )
}

/** An empty page: the places the surface suggests, as cards in a row that scrolls sideways. */
function Suggested({ suggested, onGo }: { suggested: readonly Suggestion[]; onGo: (url: string) => void }) {
  const { t } = useI18n()
  return (
    <YStack data-slot="side-suggested" px="$4" pt="$4" gap="$3">
      <SizableText fontSize={13} color="$soft">
        {t('Suggested')}
      </SizableText>
      {suggested.length ? (
        <XStack role="list" aria-label={t('Suggested')} gap="$2" overflowX="auto" overflowY="hidden" pb="$2">
          {suggested.map((s) => (
            <YStack key={s.url} role="listitem" shrink={0}>
              <YStack
                render="button"
                aria-label={s.title}
                onPress={() => onGo(s.url)}
                width={120}
                items="center"
                gap="$2"
                py="$2.5"
                px="$1.5"
                focusVisibleStyle={RING}
              >
                <Mark icon={s.icon} title={s.title} />
                <SizableText size="$2" color="$ink" numberOfLines={1} maxW="100%">
                  {s.title}
                </SizableText>
              </YStack>
            </YStack>
          ))}
        </XStack>
      ) : (
        <SizableText fontSize={13} color="$soft">
          {t('No suggestions.')}
        </SizableText>
      )}
    </YStack>
  )
}

/** A remote address a browser page may not frame: what it is, and the one way to open it. */
function Elsewhere({ tab }: { tab: Tab }) {
  const { t } = useI18n()
  return (
    <YStack flex={1} items="center" justify="center" gap="$3" p="$5" data-slot="side-elsewhere">
      <Globe size={20} opacity={0.6} />
      <SizableText fontSize={13} color="$ink" numberOfLines={1} maxW="100%">
        {tab.title || tab.url}
      </SizableText>
      <XStack
        render="button"
        onPress={() => popOut(tab)}
        items="center"
        gap="$1.5"
        px="$3"
        minH={34}
        rounded={999}
        borderWidth={1}
        borderColor="$borderColor"
        cursor="pointer"
        hoverStyle={{ bg: '$hover' }}
        focusVisibleStyle={RING}
      >
        <ExternalLink size={13} />
        <SizableText fontSize={13} color="$ink">
          {t('Open in a new window')}
        </SizableText>
      </XStack>
    </YStack>
  )
}

/** A page the menu acts on. */
export interface Here {
  url: string
  title: string
}

/** A page asked about, with what the reader had selected in it where a page can read that. */
export interface Asked extends Here {
  selection?: string
}

/** Something the surface can do with a page: a row of the menu's Skills. */
export interface Skill {
  id: string
  name: string
  run: (page: Here) => void
}

/** What the surface does with a page from its menu: each one draws its row. */
export interface Acts {
  /** Ask about this page: the conversation takes it, with what was selected. */
  onAsk?: (page: Asked) => void
  /** Open in Dev: a preview or a site we host (`hosted`), opened where it is built. */
  onDev?: (page: Here) => void
  /** Take a screenshot: the picture, handed to the surface (Chat attaches it). */
  onCapture?: (file: File, page: Here) => void
  skills?: readonly Skill[]
}

export interface PageProps extends Acts {
  tab: Tab
  /** Point the page at an address. */
  onGo: (url: string) => void
  onBack?: () => void
  onForward?: () => void
  /** The title the page gave itself. */
  onTitle?: (title: string) => void
  /** Pin the tab, or let it go. */
  onPin?: (on: boolean) => void
  suggested?: readonly Suggestion[]
}

/** A row of the page's menu, px: a browser's menu is denser than a form's. */
const ROW = 29

/** A browser's zoom steps, percent. */
const ZOOMS = [25, 33, 50, 67, 75, 80, 90, 100, 110, 125, 150, 175, 200, 250, 300]

/** The next zoom step past `now`, one way. */
const zoomed = (now: number, way: 1 | -1): number => {
  const at = ZOOMS.findIndex((z) => z >= now)
  const i = at < 0 ? ZOOMS.length - 1 : ZOOMS[at] === now ? at + way : way > 0 ? at : at - 1
  return ZOOMS[Math.max(0, Math.min(ZOOMS.length - 1, i))]!
}

/** A row of the menu: its words, and what stands at its end. */
/** Hand the page's picture to `onCapture` as a file; a capture the host browser cancels or fails is dropped, with nothing thrown. */
export async function screenshot(browser: Browser | null, shown: Shown | null, onCapture: Acts['onCapture']): Promise<void> {
  if (!shown || !browser?.capture || !onCapture) return
  try {
    const blob = await browser.capture(shown)
    const name = `Screenshot ${new Date().toISOString().replace(/[:T]/g, '-').slice(0, 19)}.png`
    onCapture(new File([blob], name, { type: blob.type || 'image/png' }), { url: shown.url, title: shown.title })
  } catch {
    // Cancelled or failed in the host browser.
  }
}

function Row({ label, onSelect, disabled, end }: { label: string; onSelect: () => void; disabled?: boolean; end?: ReactNode }) {
  return (
    <DropdownMenuItem disabled={disabled} onSelect={onSelect} minH={ROW} py={0}>
      <SizableText fontSize={13} color="$ink" flex={1} numberOfLines={1}>
        {label}
      </SizableText>
      {end}
    </DropdownMenuItem>
  )
}

/** The zoom row: the level between − and +, and the way back to 100. Plain buttons, so a press keeps the menu open. */
function Zoom({ level, onLevel, disabled }: { level: number; onLevel: (percent: number) => void; disabled: boolean }) {
  const { t } = useI18n()
  const step = (way: 1 | -1) => () => onLevel(zoomed(level, way))
  const box = (label: string, onSelect: () => void, child: ReactNode) => (
    <XStack
      render="button"
      aria-label={label}
      aria-disabled={disabled || undefined}
      onPress={disabled ? undefined : onSelect}
      width={24}
      height={24}
      {...quiet(false, disabled)}
    >
      {child}
    </XStack>
  )
  return (
    <XStack data-slot="page-zoom" role="group" aria-label={t('Zoom')} items="center" gap="$2" px={8} height={38} opacity={disabled ? 0.5 : 1}>
      <SizableText fontSize={13} color="$ink" flex={1}>
        {t('Zoom')}
      </SizableText>
      <XStack items="center" gap={2} borderWidth={1} borderColor="$borderColor" rounded={8} px={2} height={28}>
        {box(t('Zoom out'), step(-1), <Minus size={12} />)}
        <SizableText size="$1" color="$ink" width={40} text="center" aria-live="polite" fontVariant={['tabular-nums']}>
          {`${level}%`}
        </SizableText>
        {box(t('Zoom in'), step(1), <Plus size={12} />)}
      </XStack>
      {box(t('Reset zoom'), () => onLevel(100), <RotateCcw size={13} />)}
    </XStack>
  )
}

/**
 * Skills, a submenu. Its panel is mounted only while open: a closed one stays
 * laid out over the menu and takes its presses.
 */
function Skills({ skills, here }: { skills: readonly Skill[]; here: Here | null }) {
  const { t } = useI18n()
  const [open, setOpen] = useState(false)
  return (
    <DropdownMenuSub open={open} onOpenChange={setOpen}>
      <DropdownMenuSubTrigger disabled={!here} minH={ROW} py={0}>
        <SizableText fontSize={13} color="$ink" flex={1}>
          {t('Skills')}
        </SizableText>
      </DropdownMenuSubTrigger>
      {open ? (
        <DropdownMenuSubContent minW={200}>
          {skills.map((k) => (
            <Row key={k.id} label={k.name} onSelect={() => here && k.run(here)} />
          ))}
        </DropdownMenuSubContent>
      ) : null}
    </DropdownMenuSub>
  )
}

/**
 * The page's menu, in a browser's order: what we add first (ask about it, open
 * it in Dev, a skill, a new window, its link), then what is done to the page
 * (find, print, zoom, device toolbar, a screenshot), then the browser's own
 * (passwords, downloads, history, clearing) and its settings. A row is drawn
 * only where it can be done — by the host's browser (`Host.browser`), or by a
 * page itself for a frame it may reach — and one that needs a page is dim on
 * an empty tab.
 */
function Actions({
  tab,
  here,
  frame,
  framed,
  same,
  level,
  onLevel,
  acts,
  browser,
  onPin,
}: {
  tab: Tab
  /** The page shown, or null on an empty tab. */
  here: Here | null
  frame: React.RefObject<HTMLIFrameElement | null>
  /** The page is drawn in a frame or a picture this page holds. */
  framed: boolean
  /** That frame is this origin's, so this page can reach into it. */
  same: boolean
  level: number
  onLevel: (percent: number) => void
  acts: Acts
  browser: Browser | null
  onPin?: (on: boolean) => void
}) {
  const { t } = useI18n()
  const shown = here ? { id: tab.id, ...here } : null
  const none = !shown
  const pick = () => {
    if (!same) return undefined
    try {
      return frame.current?.contentWindow?.getSelection()?.toString() || undefined
    } catch {
      return undefined
    }
  }
  const ask = async () => {
    if (!shown || !acts.onAsk) return
    const selection = browser?.selection ? await browser.selection(shown).catch(() => '') : pick()
    acts.onAsk({ url: shown.url, title: shown.title, ...(selection ? { selection } : {}) })
  }
  const print = browser?.print ?? (same ? () => frame.current?.contentWindow?.print() : undefined)
  const zoom = Boolean(browser?.zoom) || framed
  const ours = [
    acts.onAsk ? <Row key="ask" label={t('Ask about this page')} disabled={none} onSelect={() => void ask()} /> : null,
    acts.onDev && shown && hosted(shown.url) ? <Row key="dev" label={t('Open in Dev')} onSelect={() => acts.onDev?.({ url: shown.url, title: shown.title })} /> : null,
    acts.skills?.length ? <Skills key="skills" skills={acts.skills} here={here} /> : null,
    <Row key="out" label={t('Open in a new window')} disabled={none} onSelect={() => popOut(tab)} />,
    <Row key="copy" label={t('Copy link')} disabled={none || !tab.url} onSelect={() => tab.url && void copy(tab.url)} />,
    onPin ? <Row key="pin" label={tab.pinned ? t('Unpin tab') : t('Pin tab')} onSelect={() => onPin(!tab.pinned)} /> : null,
  ].filter(Boolean)
  const page = [
    browser?.find ? <Row key="find" label={t('Find in page')} disabled={none} onSelect={() => shown && browser.find?.(shown)} /> : null,
    print ? <Row key="print" label={t('Print')} disabled={none} onSelect={() => shown && print(shown)} /> : null,
    zoom ? <Zoom key="zoom" level={level} onLevel={onLevel} disabled={none} /> : null,
    browser?.devices ? <Row key="devices" label={t('Show device toolbar')} disabled={none} onSelect={() => shown && browser.devices?.(shown)} /> : null,
    browser?.capture && acts.onCapture ? <Row key="capture" label={t('Take a screenshot')} disabled={none} onSelect={() => void screenshot(browser, shown, acts.onCapture)} /> : null,
  ].filter(Boolean)
  const own = [
    browser?.passwords ? <Row key="passwords" label={t('Passwords and autofill')} onSelect={browser.passwords} end={<ChevronRight size={14} opacity={0.6} />} /> : null,
    browser?.downloads ? <Row key="downloads" label={t('Downloads')} onSelect={browser.downloads} /> : null,
    browser?.history ? <Row key="history" label={t('History')} onSelect={browser.history} /> : null,
    browser?.clear ? <Row key="clear" label={t('Clear browsing data')} onSelect={browser.clear} /> : null,
  ].filter(Boolean)
  const settings = browser?.settings ? [<Row key="settings" label={t('Browser settings')} onSelect={browser.settings} />] : []
  const groups = [ours, page, own, settings].filter((g) => g.length)
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <XStack
          render="button"
          aria-label={t('Page actions')}
          {...({ title: t('Page actions') } as object)}
          width={PILL}
          height={PILL}
          {...quiet()}
        >
          <MoreHorizontal size={16} />
        </XStack>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="end" minW={240} data-slot="page-menu">
        {groups.flatMap((g, i) => (i ? [<DropdownMenuSeparator key={`line-${i}`} />, ...g] : g))}
      </DropdownMenuContent>
    </DropdownMenu>
  )
}

/**
 * A page tab: its bar, then an address, bytes rendered as their type, or the
 * suggestions of an empty tab.
 *
 * Two sandboxes. Bytes and an object URL take THIS page's origin, so a script
 * an answer wrote runs without `allow-same-origin`, never with our storage. A
 * remote page takes its own origin, and withholding same-origin breaks it.
 */
export function Page({ tab, onGo, onBack, onForward, onTitle, onPin, suggested = [], ...acts }: PageProps) {
  const { t } = useI18n()
  const Browse = useBrowse()
  const browser = useBrowser()
  const frame = useRef<HTMLIFrameElement | null>(null)
  // The page's zoom: the host's browser zooms its own; a frame here is scaled.
  const [level, setLevel] = useState(100)
  const own = tab.body !== undefined
  const src = useMemo(() => (own ? URL.createObjectURL(new Blob([tab.body!], { type: tab.type || 'text/plain' })) : (tab.url ?? '')), [own, tab.body, tab.type, tab.url])
  useEffect(() => () => (own ? URL.revokeObjectURL(src) : undefined), [own, src])
  const [round, again] = useState(0)
  const [typed, setTyped] = useState(tab.url ?? '')
  const [typing, setTyping] = useState(false)
  // A move the page made, Back or a suggestion is the bar's new address.
  useEffect(() => setTyped(tab.url ?? ''), [tab.url])
  const local = own || src.startsWith('blob:')
  const type = tab.type ?? ''
  const ext = suffix(tab.title)
  const picture = type.startsWith('image/') || PICTURES.includes(ext)
  const shows = !local || own || !type || type.startsWith('text/') || type === 'application/pdf' || type === 'application/json' || FRAMED.includes(ext)
  const back = Boolean(tab.back?.length && onBack)
  const ahead = Boolean(tab.ahead?.length && onForward)
  const go = () => {
    const to = destination(typed)
    if (to) onGo(to)
  }
  const native = !local && Boolean(Browse)
  const elsewhere = !local && !native && Boolean(src) && !frames(src)
  const framed = Boolean(src) && !native && !elsewhere
  const same = framed && !local && (() => {
    try {
      return new URL(src, window.location.href).origin === window.location.origin
    } catch {
      return false
    }
  })()
  const here = src ? { url: tab.url || src, title: tab.title } : null
  const zoomTo = (percent: number) => {
    setLevel(percent)
    if (browser?.zoom && native && here) browser.zoom({ id: tab.id, ...here }, percent)
  }
  // A frame scaled from its corner, its box grown to fill the panel at the scale.
  const scale = native || level === 100 ? null : ({ width: `${10000 / level}%`, height: `${10000 / level}%`, transform: `scale(${level / 100})`, transformOrigin: '0 0' } as const)

  return (
    <YStack flex={1} minH={0} data-slot="side-page">
      <XStack data-slot="side-bar" minH={BAR} px="$2" py="$1" gap="$2" items="center" borderBottomWidth={1} borderColor="$borderColor" shrink={0}>
        <XStack items="center" minH={PILL} px={4} gap={2} rounded={999} bg="$edge" shrink={0}>
          <XStack
            render="button"
            aria-label={t('Back')}
            {...({ title: t('Back') } as object)}
            aria-disabled={!back || undefined}
            onPress={back ? onBack : undefined}
            width={28}
            height={28}
            {...quiet(false, !back)}
          >
            <ArrowLeft size={15} {...MIRROR} />
          </XStack>
          <XStack
            render="button"
            aria-label={t('Forward')}
            {...({ title: t('Forward') } as object)}
            aria-disabled={!ahead || undefined}
            onPress={ahead ? onForward : undefined}
            width={28}
            height={28}
            {...quiet(false, !ahead)}
          >
            <ArrowRight size={15} {...MIRROR} />
          </XStack>
          <XStack
            render="button"
            aria-label={t('Reload this tab')}
            {...({ title: t('Reload this tab') } as object)}
            onPress={() => again((n) => n + 1)}
            width={28}
            height={28}
            {...quiet()}
          >
            <RotateCw size={14} />
          </XStack>
        </XStack>
        <XStack
          flex={1}
          minW={0}
          items="center"
          minH={PILL}
          px="$3.5"
          rounded={999}
          bg="$edge"
          borderWidth={1}
          // Focused, the address is the bar's one ringed thing, as a browser's is.
          data-slot="side-address"
        >
          <SizableText
            key={tab.id}
            render={
              <input
                value={own ? tab.title : typed}
                readOnly={own}
                placeholder={t('Search or enter a URL')}
                aria-label={t('Address')}
                spellCheck={false}
                autoFocus={!own && !tab.url}
                onFocus={(e) => {
                  setTyping(true)
                  e.currentTarget.select()
                }}
                onBlur={() => setTyping(false)}
                onChange={(e) => setTyped(e.target.value)}
                onKeyDown={(e) => {
                  if (e.key === 'Enter') go()
                  if (e.key === 'Escape') setTyped(tab.url ?? '')
                }}
              />
            }
            width="100%"
            color="$ink"
            fontFamily="$body"
            size="$2"
            // The resting bar reads centred, as a browser's new tab does; typed in, it reads from the line's start.
            text={typing || typed ? 'start' : 'center'}
            $platform-web={{ outline: 'none' }}
          />
        </XStack>
        <Actions
          tab={tab}
          here={here}
          frame={frame}
          framed={framed}
          same={same}
          level={level}
          onLevel={zoomTo}
          acts={acts}
          browser={browser}
          onPin={onPin}
        />
      </XStack>
      {!src ? (
        <Suggested suggested={suggested} onGo={onGo} />
      ) : native && Browse ? (
        <YStack key={round} flex={1} minH={0}>
          <Browse url={src} title={tab.title} onGo={onGo} onTitle={onTitle} />
        </YStack>
      ) : elsewhere ? (
        <Elsewhere tab={tab} />
      ) : picture && !own ? (
        <YStack flex={1} minH={0} items="center" justify="center" p="$3" overflowY="auto" overflowX="hidden">
          <YStack
            key={round}
            render={<img src={src} alt={tab.title} />}
            maxW="100%"
            maxH="100%"
            $platform-web={{ objectFit: 'contain', ...(scale ? { transform: scale.transform } : null) }}
          />
        </YStack>
      ) : shows ? (
        <YStack flex={1} minH={0} overflow="hidden" position="relative">
          <YStack
            key={round}
            render="iframe"
            ref={frame as never}
            {...({
              src,
              title: tab.title,
              sandbox: local ? 'allow-scripts allow-popups allow-forms' : 'allow-same-origin allow-scripts allow-popups allow-forms allow-modals',
            } as object)}
            position="absolute"
            t={0}
            l={0}
            width={scale ? (scale.width as never) : '100%'}
            height={scale ? (scale.height as never) : '100%'}
            display="block"
            $platform-web={scale ? { transform: scale.transform, transformOrigin: scale.transformOrigin } : undefined}
          />
        </YStack>
      ) : (
        <YStack flex={1} items="center" justify="center" p="$5" gap="$2">
          <SizableText size="$2" color="$soft">{t('{type} previews aren’t supported yet.', { type: (ext || type || 'This').toUpperCase() })}</SizableText>
          <SizableText size="$2" color="$soft">
            {t('Open it in a new window to view it.')}
          </SizableText>
        </YStack>
      )}
    </YStack>
  )
}

const DRAG = 'text/x-hanzo-tab'

/**
 * One tab in the strip: its mark, its title, and its cross, or its pin when it
 * is pinned. Its own menu (a right press, or a long press on touch) pins it or
 * lets it go, and closes it, whatever kind it is.
 */
function Leaf({ tab, on, side, index, label, icon }: { tab: Tab; on: boolean; side: Deck; index: number; label: string; icon: ReactNode }) {
  const { t } = useI18n()
  return (
    <ContextMenu>
      <ContextMenuTrigger asChild>
        <Strip tab={tab} on={on} side={side} index={index} label={label} icon={icon} />
      </ContextMenuTrigger>
      <ContextMenuContent minW={180} aria-label={t('{tab} tab', { tab: label })}>
        <ContextMenuItem onSelect={() => (tab.pinned ? side.unpin(tab.id) : side.pin(tab.id))}>{tab.pinned ? t('Unpin tab') : t('Pin tab')}</ContextMenuItem>
        <ContextMenuSeparator />
        <ContextMenuItem onSelect={() => side.shut(tab.id)}>{t('Close tab')}</ContextMenuItem>
      </ContextMenuContent>
    </ContextMenu>
  )
}

/** The tab itself, as the strip draws it. */
function Strip({ tab, on, side, index, label, icon, ...rest }: { tab: Tab; on: boolean; side: Deck; index: number; label: string; icon: ReactNode }) {
  const { t } = useI18n()
  return (
    <XStack
      {...rest}
      data-slot="side-tab"
      data-tab={tab.id}
      items="center"
      gap="$1"
      pl="$2.5"
      pr={on || tab.pinned ? '$1' : '$2.5'}
      minH={32}
      $max-md={{ minH: 40 }}
      // Keep every tab label legible at narrow viewports; the tablist scrolls horizontally instead of crushing tabs.
      shrink={0}
      maxW={220}
      rounded={8}
      borderWidth={1}
      borderColor={on ? '$borderColor' : 'transparent'}
      bg={on ? '$edge' : 'transparent'}
      hoverStyle={{ bg: on ? '$edge' : '$hover' }}
      style={{ flexShrink: 0 }}
      {...({
        // A middle press shuts any tab, as a browser's does; the chosen one also has its cross.
        onAuxClick: (e: React.MouseEvent) => {
          if (e.button !== 1 || tab.pinned) return
          e.preventDefault()
          side.shut(tab.id)
        },
        title: label,
        draggable: true,
        onDragStart: (e: React.DragEvent) => {
          e.dataTransfer.setData(DRAG, tab.id)
          e.dataTransfer.effectAllowed = 'move'
        },
        onDragOver: (e: React.DragEvent) => {
          if (Array.from(e.dataTransfer.types).includes(DRAG)) e.preventDefault()
        },
        onDrop: (e: React.DragEvent) => {
          const id = e.dataTransfer.getData(DRAG)
          if (!id || id === tab.id) return
          e.preventDefault()
          side.move(id, index)
        },
      } as object)}
    >
      <XStack
        render="button"
        role="tab"
        id={`side-tab-${tab.id}`}
        aria-selected={on}
        aria-controls="side-body"
        tabIndex={on ? 0 : -1}
        onPress={() => side.pick(tab.id)}
        onKeyDown={(e: { key?: string; metaKey?: boolean; ctrlKey?: boolean; shiftKey?: boolean; preventDefault?: () => void }) => {
          const step = e.key === 'ArrowRight' ? 1 : e.key === 'ArrowLeft' ? -1 : 0
          if (step && e.shiftKey && (e.metaKey || e.ctrlKey)) {
            e.preventDefault?.()
            side.move(tab.id, index + step)
            return
          }
          if (step) {
            e.preventDefault?.()
            const next = side.tabs[(index + step + side.tabs.length) % side.tabs.length]
            if (!next) return
            side.pick(next.id)
            queueMicrotask(() => document.getElementById(`side-tab-${next.id}`)?.focus())
            return
          }
          if ((e.key === 'Delete' || e.key === 'Backspace') && !tab.pinned) {
            e.preventDefault?.()
            side.shut(tab.id)
          }
        }}
        shrink={0}
        minW={0}
        minH={28}
        $max-md={{ minH: 38 }}
        items="center"
        gap="$1.5"
        cursor="pointer"
        focusVisibleStyle={RING_INSET}
      >
        {icon ? (
          <XStack aria-hidden shrink={0} opacity={on ? 1 : 0.7}>
            {icon}
          </XStack>
        ) : null}
        <SizableText fontSize={13} color={on ? '$ink' : '$soft'} numberOfLines={1} $platform-web={{ whiteSpace: 'nowrap' }}>
          {label}
        </SizableText>
      </XStack>
      {tab.pinned ? (
        // A pinned tab's end is its pin, always shown: pressing it lets the tab go.
        <XStack
          render="button"
          aria-label={t('Unpin {tab}', { tab: label })}
          tabIndex={-1}
          onPress={() => side.unpin(tab.id)}
          minW={22}
          minH={22}
          items="center"
          justify="center"
          rounded={6}
          cursor="pointer"
          opacity={on ? 0.8 : 0.55}
          hoverStyle={{ bg: '$hover', opacity: 1 }}
          focusVisibleStyle={RING}
        >
          <Pin size={12} />
        </XStack>
      ) : (
        <XStack
          render="button"
          aria-label={t('Close {tab}', { tab: label })}
          tabIndex={-1}
          onPress={() => side.shut(tab.id)}
          minW={22}
          minH={22}
          items="center"
          justify="center"
          rounded={6}
          cursor="pointer"
          opacity={on ? 0.8 : 0.45}
          hoverStyle={{ bg: '$hover', opacity: 1 }}
          focusVisibleStyle={RING}
          // The chosen tab's cross: the others keep their room for their titles when the strip is shared.
          display={on ? 'flex' : 'none'}
        >
          <X size={12} />
        </XStack>
      )}
    </XStack>
  )
}

export interface PanelProps extends Acts {
  side: Deck
  kinds: readonly Kind[]
  onHide: () => void
  /** A sheet over the page (a phone) rather than the host's column. */
  sheet?: boolean
  /** Under the strip, over every tab: a line about what they all read (Dev's sandbox). */
  bar?: ReactNode
  /** At the strip's end, before pop-out and hide: the surface's own menu. */
  menu?: ReactNode
  label?: string
  /** What an empty tab offers. */
  suggested?: readonly Suggestion[]
  /** Reserve trailing space for the host shell's top-right corner buttons (`--hanzo-shell-top-right-inset`). */
  topRightInset?: boolean
}

/** The panel: the strip, and the chosen tab. */
export function Panel({ side, kinds, onHide, sheet = false, bar, menu, label, suggested, topRightInset = !sheet, ...acts }: PanelProps) {
  const { t: say } = useI18n()
  const tab = side.tabs.find((t) => t.id === side.at) ?? null
  // The column it stands in; a phone's sheet is the whole window already.
  const stands = useSide()
  const column = sheet ? null : stands
  // The chosen tab is in view in a strip that scrolls: a tab just opened stands at its end.
  useEffect(() => {
    if (side.at) document.getElementById(`side-tab-${side.at}`)?.scrollIntoView({ block: 'nearest', inline: 'nearest' })
  }, [side.at, side.tabs.length])
  const kind = tab && tab.kind !== PAGE ? kinds.find((k) => k.id === tab.kind) : undefined
  const bleed = !tab || tab.kind === PAGE || Boolean(kind?.bleed)
  const closed = kinds.filter((k) => !side.tabs.some((t) => t.kind === k.id))
  // A page's title is the page's own; an empty one is a new tab, in the reader's language.
  const title = (t: Tab) => (t.kind === PAGE ? (t.title && t.title !== BLANK ? t.title : say(BLANK)) : (kinds.find((k) => k.id === t.kind)?.label ?? t.title))
  const mark = (t: Tab) => (t.kind === PAGE ? <Globe size={14} /> : (kinds.find((k) => k.id === t.kind)?.icon ?? null))
  const blank = () => side.open({ kind: PAGE, title: BLANK, url: '' })
  const poppable = tab?.kind === PAGE && (tab.body !== undefined || Boolean(tab.url))
  const plus = (
    <XStack
      render="button"
      aria-label={say('Open a tab')}
      {...({ title: say('Open a tab') } as object)}
      width={28}
      height={28}
      {...quiet()}
      onPress={closed.length ? undefined : blank}
    >
      <Plus size={16} />
    </XStack>
  )

  const body = (
    <YStack role="complementary" aria-label={label ?? say('Side panel')} data-slot="side" flex={1} minW={0} minH={0} overflow="hidden" rounded="inherit">
      <XStack
        data-slot="side-strip"
        height={STRIP}
        px="$2"
        gap="$1.5"
        items="center"
        borderBottomWidth={1}
        borderColor="$borderColor"
        shrink={0}
        minW={0}
        style={topRightInset ? { paddingInlineEnd: 'calc(8px + var(--hanzo-shell-top-right-inset, 0px))' } : undefined}
      >
        {column?.hidden && !column.full ? (
          <XStack
            render="button"
            data-slot="side-show"
            onPress={column.show}
            minH={32}
            px="$2"
            gap="$1.5"
            {...quiet()}
          >
            <PanelLeftOpen size={16} />
            <SizableText fontSize={13} color="$ink">
              {column.main === 'run' ? say('Show session') : column.main === 'chat' ? say('Show chat') : say('Show {main}', { main: column.main })}
            </SizableText>
          </XStack>
        ) : null}
        {/* The strip scrolls sideways in a narrow panel; `+`, the menu, pop-out and hide stay put. */}
        <XStack
          role="tablist"
          aria-label={say('Tabs')}
          flex={1}
          minW={0}
          gap="$1"
          items="center"
          overflowX="auto"
          overflowY="hidden"
          $platform-web={{ scrollbarWidth: 'none', msOverflowStyle: 'none', WebkitOverflowScrolling: 'touch' }}
        >
          {side.tabs.map((t, i) => (
            <Leaf key={t.id} tab={t} on={t.id === side.at} side={side} index={i} label={title(t)} icon={mark(t)} />
          ))}
        </XStack>
        {closed.length ? (
          <DropdownMenu
            trigger={plus}
            items={[
              ...closed.map((k) => ({ key: k.id, label: k.label, onSelect: () => side.open({ kind: k.id, title: k.label }) })),
              { key: PAGE, label: say('Page'), description: say('An address, framed here'), onSelect: blank },
            ]}
          />
        ) : (
          plus
        )}
        {menu}
        <XStack
          render="button"
          aria-label={say('Open this tab in a new window')}
          {...({ title: say('Open this tab in a new window') } as object)}
          aria-disabled={!poppable || undefined}
          onPress={poppable ? () => popOut(tab) : undefined}
          width={28}
          height={28}
          {...quiet(false, !poppable)}
        >
          <ExternalLink size={14} />
        </XStack>
        {column ? (
          <XStack
            render="button"
            data-slot="side-full"
            aria-label={column.full ? say('Exit full screen') : say('Full screen')}
            aria-pressed={column.full}
            aria-keyshortcuts={column.full ? 'Escape' : undefined}
            {...({ title: column.full ? say('Exit full screen ({key})', { key: 'Esc' }) : say('Full screen') } as object)}
            onPress={() => column.fill(!column.full)}
            width={28}
            height={28}
            {...quiet(column.full)}
          >
            {column.full ? <Minimize2 size={14} /> : <Maximize2 size={14} />}
          </XStack>
        ) : null}
        <XStack
          render="button"
          data-slot="side-toggle"
          aria-label={say('Hide tabs')}
          aria-expanded
          aria-keyshortcuts={SHORTCUTS}
          {...({ title: say('Hide tabs ({keys})', { keys: KEYS }) } as object)}
          onPress={onHide}
          width={32}
          height={32}
          {...quiet(true)}
        >
          <Columns2 size={16} />
        </XStack>
      </XStack>
      {bar}
      <YStack
        id="side-body"
        role="tabpanel"
        aria-labelledby={tab ? `side-tab-${tab.id}` : undefined}
        // What a tab lays over itself (a door's "Connecting…") stays inside it, under the strip.
        position="relative"
        flex={1}
        minH={0}
        overflowY={bleed ? 'hidden' : 'auto'}
        overflowX="hidden"
        px={bleed ? 0 : '$3'}
        py={bleed ? 0 : '$3'}
      >
        {!tab ? (
          <YStack flex={1} items="center" justify="center" p="$5">
            <SizableText fontSize={13} color="$soft">
              {say('No tabs open.')}
            </SizableText>
          </YStack>
        ) : tab.kind === PAGE ? (
          <Page
            key={tab.id}
            tab={tab}
            onGo={(url) => side.go(tab.id, url)}
            onBack={() => side.back(tab.id)}
            onForward={() => side.forward(tab.id)}
            onTitle={(t) => side.rename(tab.id, t)}
            onPin={(on) => (on ? side.pin(tab.id) : side.unpin(tab.id))}
            suggested={suggested}
            {...acts}
          />
        ) : kind ? (
          <Fragment key={tab.id}>{kind.render()}</Fragment>
        ) : (
          <SizableText size="$2" color="$soft">
            {say('This tab is not offered here.')}
          </SizableText>
        )}
      </YStack>
    </YStack>
  )

  if (!sheet || typeof document === 'undefined') return body
  // A phone: the panel over the whole window, clear of the notch and the home
  // indicator. The strip's toggle puts it away.
  return createPortal(
    <YStack
      data-slot="side-sheet"
      position="fixed"
      t={0}
      r={0}
      b={0}
      l={0}
      z={60}
      bg="$background"
      pt={'env(safe-area-inset-top)' as never}
      pb={'env(safe-area-inset-bottom)' as never}
    >
      {body}
    </YStack>,
    document.body,
  )
}
