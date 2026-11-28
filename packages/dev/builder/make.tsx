/**
 * Make something new: the Library's template picker (-/artifacts/new), fed live
 * by gallery.hanzo.ai (api/gallery.ts). Each template shows its capture, its
 * name and its kind; search and the kinds narrow them.
 *
 * Picking one makes it where its kind is made: a site is copied into a project
 * of this org and opens in Dev (`fork`), an agent becomes the org's own and
 * opens in Customize › Agents (`use`).
 *
 * The keyboard: Down (or Enter) from the search goes into the grid, the arrows
 * move across and down it, Up from its first row goes back, Enter picks, and
 * Escape closes. On a phone the picker is the whole screen.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Bot, Globe, Search, X } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogTitle, Input } from '@hanzo/ui'
import { useEffect, useMemo, useRef, useState } from 'react'

import { RING } from './pane.ts'
import { use } from './api/agents.ts'
import { told } from './api/call.ts'
import { APP, catalogue, KIND, KINDS, narrow, type Kind, type Template } from './api/gallery.ts'
import { fork } from './api/projects.ts'
import { useRead } from './data.ts'
import { useHost, useTarget } from './host.tsx'
import { copying } from './question.ts'
import { useSpend } from './spend.tsx'
import { path } from './route.ts'

/** A chip: one choice among a few, lit when chosen. */
export function Chip({ label, on, onPress }: { label: string; on: boolean; onPress: () => void }) {
  return (
    <XStack
      render="button"
      role="radio"
      aria-checked={on}
      aria-label={label}
      onPress={onPress}
      height={32}
      px="$3"
      items="center"
      rounded="$10"
      shrink={0}
      borderWidth={1}
      borderColor={on ? '$ink' : '$borderColor'}
      bg={on ? '$ink' : 'transparent'}
      hoverStyle={{ bg: on ? '$ink' : '$hover' }}
      cursor="pointer"
      focusVisibleStyle={RING}
    >
      <SizableText size="$2" color={on ? '$background' : '$ink'}>
        {label}
      </SizableText>
    </XStack>
  )
}

/** A template's capture, 16:10 from the top of its page, or its kind's mark. */
function Still({ x }: { x: Template }) {
  const [broken, setBroken] = useState(false)
  return (
    <YStack width="100%" aspectRatio={16 / 10} bg="$raised" items="center" justify="center" overflow="hidden" aria-hidden>
      {x.preview && !broken ? (
        <img src={x.preview} alt="" loading="lazy" onError={() => setBroken(true)} style={{ width: '100%', height: '100%', objectFit: 'cover', objectPosition: 'top', display: 'block' }} />
      ) : (
        <YStack opacity={0.6}>{x.kind === 'agent' ? <Bot size={24} /> : <Globe size={24} />}</YStack>
      )}
    </YStack>
  )
}

/** Where a key moves from cell `i` of `n` laid out `cols` wide: a cell, -1 for Up out of the first row, or null for a key the grid does not take. */
function step(key: string, i: number, n: number, cols: number): number | null {
  const to: Record<string, number> = { ArrowRight: i + 1, ArrowLeft: i - 1, ArrowDown: i + cols, ArrowUp: i - cols, Home: 0, End: n - 1 }
  if (!(key in to)) return null
  const v = to[key]!
  return v < 0 ? (key === 'ArrowUp' ? -1 : 0) : Math.min(v, n - 1)
}

export function Make({ open, onClose }: { open: boolean; onClose: () => void }) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const wide = useMedia().md
  const read = useRead(open ? () => catalogue() : null, [] as Template[], [open])
  const [q, setQ] = useState('')
  const [kind, setKind] = useState<Kind | ''>('')
  const [at, setAt] = useState(0)
  const [busy, setBusy] = useState('')
  const [note, setNote] = useState('')
  const spend = useSpend()
  const search = useRef<HTMLInputElement | null>(null)
  const cells = useRef<(HTMLElement | null)[]>([])

  useEffect(() => {
    if (open) setNote('')
  }, [open])

  const kinds = useMemo(() => KINDS.filter((k) => read.value.some((x) => x.kind === k)), [read.value])
  const hits = useMemo(() => narrow(read.value, kind, q), [read.value, kind, q])
  useEffect(() => setAt(0), [kind, q])

  const pick = async (x: Template) => {
    if (busy) return
    if (!host.person) return host.signIn?.()
    // A site template's copy is built and published at once: asked first. An agent template only makes the agent.
    if (APP[x.kind] === 'dev' && !(await spend.ask(copying(x.name, t)))) return
    setBusy(x.slug)
    setNote('')
    try {
      if (APP[x.kind] === 'dev') {
        const made = await fork(target, x.slug)
        host.go(made.slug || x.slug)
      } else {
        const made = await use(target, { name: x.slug, title: x.name, description: x.description, category: x.category, model: x.model, instructions: x.instructions, avatar: x.preview }, window.location.href)
        host.go(path({ kind: 'agent', name: made.name }))
      }
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setBusy('')
    }
  }

  type Key = { key?: string; preventDefault?: () => void }
  const into = (e: Key) => {
    if ((e.key !== 'ArrowDown' && e.key !== 'Enter') || !hits.length) return
    e.preventDefault?.()
    cells.current[0]?.focus()
  }
  const across = (e: Key) => {
    const els = cells.current.slice(0, hits.length)
    const i = els.findIndex((el) => el === document.activeElement)
    if (i < 0) return
    const top = els[0]?.getBoundingClientRect().top
    const cols = Math.max(1, els.filter((el) => el?.getBoundingClientRect().top === top).length)
    const to = step(e.key ?? '', i, hits.length, cols)
    if (to === null) return
    e.preventDefault?.()
    if (to < 0) search.current?.focus()
    else els[to]?.focus()
  }

  return (
    <Dialog open={open} onOpenChange={(o) => !o && onClose()}>
      <DialogContent
        data-slot="make"
        showCloseButton={false}
        p="$4"
        gap="$3"
        {...(wide ? { width: '100%', maxW: 960, maxH: '88vh' } : ({ position: 'fixed', t: 0, r: 0, b: 0, l: 0, maxW: '100%', maxH: '100%', rounded: 0, borderWidth: 0 } as const))}
      >
        {spend.dialog}
        <DialogTitle numberOfLines={1} paddingInlineEnd={40}>
          {t('Make something new')}
        </DialogTitle>
        <XStack items="center" gap="$2">
          <Search size={16} opacity={0.6} />
          <YStack flex={1} minW={0}>
            <Input ref={search} value={q} onChangeText={setQ} placeholder={t('Search templates')} aria-label={t('Search templates')} onKeyDown={into} />
          </YStack>
        </XStack>
        {kinds.length > 1 ? (
          <XStack role="radiogroup" aria-label={t('Kind')} gap="$2" overflowX="auto" overflowY="hidden" pb="$1" $platform-web={{ scrollbarWidth: 'none' }}>
            <Chip label={t('All')} on={kind === ''} onPress={() => setKind('')} />
            {kinds.map((k) => (
              <Chip key={k} label={t(KIND[k])} on={kind === k} onPress={() => setKind(k)} />
            ))}
          </XStack>
        ) : null}
        {note ? (
          <SizableText size="$2" color="$soft" role="status">
            {note}
          </SizableText>
        ) : null}
        <YStack shrink={1} minH={0} overflowY="auto" overflowX="hidden">
          {read.error ? (
            <YStack gap="$3" items="flex-start" py="$4">
              <SizableText size="$2" color="$soft">
                {told(read.error, t)}
              </SizableText>
              <Button size="sm" variant="outline" onPress={read.reload}>
                {t('Try again')}
              </Button>
            </YStack>
          ) : read.loading && !read.value.length ? (
            <SizableText size="$2" color="$soft" py="$4">
              {t('Reading templates…')}
            </SizableText>
          ) : !hits.length ? (
            <SizableText size="$2" color="$soft" py="$4">
              {t('Nothing matches.')}
            </SizableText>
          ) : (
            <YStack role="list" aria-label={t('Templates')} onKeyDown={across} gap={16} $platform-web={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(min(46%, 200px), 1fr))' }}>
              {hits.map((x, i) => (
                <YStack key={x.slug} role="listitem" minW={0}>
                  <YStack
                    ref={(el: unknown) => {
                      cells.current[i] = el instanceof HTMLElement ? el : null
                    }}
                    render="button"
                    tabIndex={i === at ? 0 : -1}
                    onFocus={() => setAt(i)}
                    aria-label={t('Start from {template}', { template: x.name })}
                    disabled={Boolean(busy)}
                    onPress={() => void pick(x)}
                    rounded="$4"
                    borderWidth={1}
                    borderColor="$borderColor"
                    overflow="hidden"
                    cursor="pointer"
                    hoverStyle={{ bg: '$hover' }}
                    focusVisibleStyle={RING}
                    $platform-web={{ textAlign: 'start' }}
                  >
                    <Still x={x} />
                    <YStack px="$2.5" py="$2" gap="$0.5" minW={0}>
                      <SizableText size="$2" color="$ink" numberOfLines={1}>
                        {x.name}
                      </SizableText>
                      <SizableText size="$1" color="$soft" numberOfLines={1}>
                        {busy === x.slug ? t('Starting…') : [t(KIND[x.kind]), x.category].filter(Boolean).join(' · ')}
                      </SizableText>
                    </YStack>
                  </YStack>
                </YStack>
              ))}
            </YStack>
          )}
        </YStack>
        {/* Last in the order, so the dialog's first focus is the search. */}
        <XStack
          render="button"
          aria-label={t('Close')}
          position="absolute"
          t={14}
          insetInlineEnd={14}
          width={32}
          height={32}
          items="center"
          justify="center"
          rounded="$2"
          hoverStyle={{ bg: '$hover' }}
          cursor="pointer"
          onPress={onClose}
        >
          <X size={16} />
        </XStack>
      </DialogContent>
    </Dialog>
  )
}
