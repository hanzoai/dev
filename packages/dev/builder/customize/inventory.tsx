/**
 * The Big Five questionnaire Clone yourself asks, and what it found.
 *
 * On a phone one statement fills the screen, with a bar, Back, and the keys 1
 * to 5; a wider window shows a focused block of 5 statements at a time (with an
 * option to view all), and the keys answer the first one left. Every answer goes
 * to `onChange` as it is given, which keeps it in this browser, so a closed
 * dialog opens where it stopped. The statements are the IPIP's (ocean.ts), drawn
 * through `t`; figures are drawn as given.
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Check, ChevronDown, ChevronLeft, ChevronRight, ChevronUp } from '@hanzogui/lucide-icons-2'
import { Button } from '@hanzo/ui'
import { memo, useCallback, useEffect, useRef, useState } from 'react'

import { RING, RING_INSET } from '../pane.ts'

import { ANCHORS, DOMAIN, DOMAINS, FACET, FACETS, ITEMS, type Answers, type Domain, type Form, type Score } from './ocean.ts'

const LEVELS = [1, 2, 3, 4, 5] as const
const PAGE_SIZE = 5

export function Inventory({ answers, onChange, onDone }: { answers: Answers; onChange: (a: Answers) => void; onDone: () => void }) {
  const { t } = useI18n()
  const phone = !useMedia().sm
  const items = ITEMS[answers.form]
  const given = answers[answers.form]
  const n = given.filter(Boolean).length
  const complete = n === items.length
  const firstOpen = given.indexOf(0)
  const [at, setAt] = useState(() => Math.max(0, firstOpen))
  const [page, setPage] = useState(() => Math.floor(Math.max(0, firstOpen) / PAGE_SIZE))
  const [showAll, setShowAll] = useState(false)
  const list = useRef<HTMLDivElement | null>(null)

  const totalPages = Math.max(1, Math.ceil(items.length / PAGE_SIZE))
  const safeAt = Math.min(at, Math.max(0, items.length - 1))
  const safePage = Math.min(page, totalPages - 1)
  const pageStart = showAll ? 0 : safePage * PAGE_SIZE
  const pageEnd = showAll ? items.length : Math.min(items.length, pageStart + PAGE_SIZE)

  // The keys answer the statement on screen, or the first unanswered one on the active page (or anywhere in the list).
  const pageOpen = given.findIndex((x, idx) => idx >= pageStart && idx < pageEnd && !x)
  const current = phone ? safeAt : showAll ? firstOpen : pageOpen >= 0 ? pageOpen : firstOpen

  const answer = (i: number, v: number) => {
    const next = given.map((x, j) => (j === i ? v : x))
    onChange({ ...answers, [answers.form]: next })
    const after = next.findIndex((x, j) => j > i && !x)
    const left = after >= 0 ? after : next.indexOf(0)
    if (phone) {
      if (left < 0) onDone()
      else setAt(left)
    } else if (showAll) {
      if (left >= 0) list.current?.querySelector(`[data-item="${left}"]`)?.scrollIntoView({ block: 'nearest' })
    } else if (left >= 0) {
      // Advance smoothly once every statement on the visible page has an answer.
      const stillOnPage = next.findIndex((x, j) => j >= pageStart && j < pageEnd && !x)
      if (stillOnPage < 0) setPage(Math.floor(left / PAGE_SIZE))
    }
  }

  const press = useRef(answer)
  press.current = answer
  // One handler for every row, so a row redraws only when its own answer or highlight changes.
  const pick = useCallback((i: number, v: number) => press.current(i, v), [])
  useEffect(() => {
    const key = (e: KeyboardEvent) => {
      const target = e.target
      if (target instanceof HTMLElement && (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA' || target.isContentEditable)) return
      const v = Number(e.key)
      if (e.metaKey || e.ctrlKey || e.altKey || !(v >= 1 && v <= 5) || current < 0) return
      e.preventDefault()
      press.current(current, v)
    }
    window.addEventListener('keydown', key)
    return () => window.removeEventListener('keydown', key)
  }, [current])

  const progressPct = items.length ? Math.round((n / items.length) * 100) : 0

  // Pinned to the top of the dialog's scroll, so the count and Done stay in reach down the list.
  const head = (
    <YStack gap="$2" pb="$2.5" borderBottomWidth={1} borderColor="$borderColor" bg="$background" z={1} position="sticky" t={0}>
      <XStack items="center" gap="$3">
        <YStack
          flex={1}
          height={6}
          overflow="hidden"
          role="progressbar"
          aria-label={t('Answered')}
          aria-valuenow={n}
          aria-valuemin={0}
          aria-valuemax={items.length}
        >
          <YStack height={6} width={`${progressPct}%`} rounded={999} bg="$ink" />
        </YStack>
        <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']} shrink={0}>
          {t('{n} of {total}', { n, total: items.length })}
        </SizableText>
        <Button size="sm" variant={complete ? 'primary' : 'outline'} borderColor={complete ? undefined : 'var(--border-strong)'} onPress={onDone}>
          {t('Done')}
        </Button>
      </XStack>
      <SizableText size="$1" color="$soft">
        {t('Describe yourself as you generally are now, not as you wish to be in the future. From 1, very inaccurate, to 5, very accurate.')}
      </SizableText>
    </YStack>
  )

  if (phone) {
    const item = items[safeAt]!
    return (
      <YStack gap="$4">
        {head}
        <YStack gap="$1.5">
          <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
            {t('Statement {i} of {total}', { i: safeAt + 1, total: items.length })}
          </SizableText>
          <SizableText size="$5" fontWeight="500" color="$ink" minH={56}>
            {t(item.text)}
          </SizableText>
        </YStack>
        <YStack role="radiogroup" aria-label={t(item.text)} gap="$2">
          {LEVELS.map((v) => {
            const selected = given[safeAt] === v
            return (
              <XStack
                key={v}
                render="button"
                role="radio"
                aria-checked={selected}
                aria-label={t(ANCHORS[v - 1])}
                onPress={() => answer(safeAt, v)}
                width="100%"
                minH={48}
                px="$3.5"
                py="$2"
                gap="$3"
                items="center"
                focusVisibleStyle={RING}
                $platform-web={{ textAlign: 'start' }}
              >
                <SizableText size="$2" fontWeight="500" color={selected ? '$ink' : '$soft'} width={16} fontVariant={['tabular-nums']} shrink={0}>
                  {v}
                </SizableText>
                <SizableText size="$3" color="$ink" flex={1} minW={0}>
                  {t(ANCHORS[v - 1])}
                </SizableText>
                {selected ? <Check size={15} color="$ink" aria-hidden /> : null}
              </XStack>
            )
          })}
        </YStack>
        <XStack items="center" justify="space-between" gap="$2">
          <Button size="sm" variant="outline" borderColor="var(--border-strong)" disabled={safeAt === 0} onPress={() => setAt(safeAt - 1)}>
            <ChevronLeft size={14} /> {t('Back')}
          </Button>
          <Button size="sm" variant="ghost" disabled={safeAt >= items.length - 1} onPress={() => setAt(safeAt + 1)}>
            {t('Next')} <ChevronRight size={14} />
          </Button>
        </XStack>
      </YStack>
    )
  }

  return (
    <YStack gap="$3">
      {head}
      <XStack items="center" justify="space-between" gap="$2" px="$1">
        <SizableText size="$1" color="$soft">
          {t('1 Very inaccurate · 5 Very accurate')}
        </SizableText>
        <SizableText size="$1" color="$soft">
          {t('Keys 1–5 answer the highlighted row')}
        </SizableText>
      </XStack>
      <YStack ref={list as never} role="list" aria-label={t('Statements')} rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel" overflow="hidden">
        {items.slice(pageStart, pageEnd).map((item, offset) => {
          const idx = pageStart + offset
          return <Statement key={idx} i={idx} first={offset === 0} text={t(item.text)} value={given[idx] ?? 0} current={idx === current} onAnswer={pick} />
        })}
      </YStack>
      <XStack items="center" justify="space-between" gap="$2" flexWrap="wrap">
        {!showAll ? (
          <XStack items="center" gap="$2">
            <Button size="sm" variant="outline" borderColor="var(--border-strong)" disabled={safePage === 0} onPress={() => setPage(safePage - 1)}>
              <ChevronLeft size={14} /> {t('Previous')}
            </Button>
            <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
              {t('Page {page} of {total}', { page: safePage + 1, total: totalPages })}
            </SizableText>
            <Button size="sm" variant="outline" borderColor="var(--border-strong)" disabled={safePage >= totalPages - 1} onPress={() => setPage(safePage + 1)}>
              {t('Next')} <ChevronRight size={14} />
            </Button>
          </XStack>
        ) : (
          <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
            {t('Showing all {total} statements', { total: items.length })}
          </SizableText>
        )}
        <Button size="sm" variant="ghost" onPress={() => setShowAll((v) => !v)}>
          {showAll ? t('Show 5 at a time') : t('Show all {total}', { total: items.length })}
        </Button>
      </XStack>
    </YStack>
  )
}

/** One statement of the list, with its five answers. */
const Statement = memo(function Statement({
  i,
  first,
  text,
  value,
  current,
  onAnswer,
}: {
  i: number
  first: boolean
  text: string
  value: number
  current: boolean
  onAnswer: (i: number, v: number) => void
}) {
  const { t } = useI18n()
  return (
    <XStack
      role="listitem"
      data-item={i}
      items="center"
      gap="$3"
      px="$3"
      py="$2.5"
      minH={48}
      borderTopWidth={first ? 0 : 1}
    >
      <SizableText size="$1" color="$soft" width={24} fontVariant={['tabular-nums']} shrink={0}>
        {i + 1}
      </SizableText>
      <SizableText flex={1} minW={0} size="$2" fontWeight={current ? '500' : '400'} color={value && !current ? '$soft' : '$ink'}>
        {text}
      </SizableText>
      <XStack role="radiogroup" aria-label={text} gap="$1.5" shrink={0}>
        {LEVELS.map((v) => {
          const selected = value === v
          return (
            <XStack
              key={v}
              render="button"
              role="radio"
              aria-checked={selected}
              aria-label={t(ANCHORS[v - 1])}
              onPress={() => onAnswer(i, v)}
              width={32}
              height={32}
              items="center"
              justify="center"
              focusVisibleStyle={RING}
            >
              <SizableText size="$1" fontWeight={selected ? '600' : '400'} color={selected ? '$background' : '$soft'} fontVariant={['tabular-nums']}>
                {v}
              </SizableText>
            </XStack>
          )
        })}
      </XStack>
    </XStack>
  )
})

/**
 * The five domains as bars of their raw means, 1 to 5. On the long form, each
 * domain's six facets stay folded by default so only 5 primary bars appear at rest.
 */
export function Traits({ form, score }: { form: Form; score: Score }) {
  const { t } = useI18n()
  const [expanded, setExpanded] = useState<Partial<Record<Domain, boolean>>>({})
  const hasFacets = form === 'long'
  const allOpen = hasFacets && DOMAINS.every((d) => expanded[d])

  const toggleDomain = (d: Domain) => setExpanded((prev) => ({ ...prev, [d]: !prev[d] }))
  const toggleAll = () => {
    const next = !allOpen
    setExpanded(Object.fromEntries(DOMAINS.map((d) => [d, next])) as Record<Domain, boolean>)
  }

  return (
    <YStack gap="$2.5">
      <YStack rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel" overflow="hidden">
        {DOMAINS.map((d, idx) => {
          const open = hasFacets && Boolean(expanded[d])
          return (
            <YStack key={d} borderTopWidth={idx ? 1 : 0} borderColor="$borderColor">
              {hasFacets ? (
                <XStack
                  render="button"
                  aria-expanded={open}
                  aria-label={t('{domain}: {score}', { domain: t(DOMAIN[d]), score: score.domains[d].toFixed(1) })}
                  onPress={() => toggleDomain(d)}
                  width="100%"
                  px="$3"
                  py="$2.5"
                  minH={44}
                  items="center"
                  gap="$2"
                  focusVisibleStyle={RING_INSET}
                  $platform-web={{ textAlign: 'start' }}
                >
                  <YStack flex={1} minW={0}>
                    <Bar label={t(DOMAIN[d])} value={score.domains[d]} strong />
                  </YStack>
                  {open ? <ChevronUp size={14} color="$soft" aria-hidden /> : <ChevronDown size={14} color="$soft" aria-hidden />}
                </XStack>
              ) : (
                <YStack px="$3" py="$2.5">
                  <Bar label={t(DOMAIN[d])} value={score.domains[d]} strong />
                </YStack>
              )}
              {open ? (
                <YStack px="$3" py="$2.5" gap="$2" borderTopWidth={1} borderColor="$borderColor" bg="$background">
                  {FACETS(d).map((f) => (
                    <Bar key={f} label={t(FACET[f])} value={score.facets[f] ?? 0} />
                  ))}
                </YStack>
              ) : null}
            </YStack>
          )
        })}
      </YStack>
      {hasFacets ? (
        <XStack justify="flex-end">
          <Button size="sm" variant="ghost" onPress={toggleAll}>
            {allOpen ? t('Hide facets') : t('Show all facets')}
          </Button>
        </XStack>
      ) : null}
    </YStack>
  )
}

function Bar({ label, value, strong = false }: { label: string; value: number; strong?: boolean }) {
  const h = strong ? 6 : 4
  const pct = Math.max(0, Math.min(100, ((value - 1) / 4) * 100))
  return (
    <XStack
      items="center"
      gap="$3"
      role="meter"
      aria-label={label}
      aria-valuenow={value}
      aria-valuemin={1}
      aria-valuemax={5}
      aria-valuetext={value.toFixed(1)}
      pl={strong ? 0 : '$2'}
    >
      <XStack items="center" justify="space-between" gap="$2" width={strong ? 150 : 142} shrink={0} $max-sm={{ width: '100%' }}>
        <SizableText size={strong ? '$2' : '$1'} fontWeight={strong ? '500' : '400'} color={strong ? '$ink' : '$soft'} numberOfLines={1} flex={1} minW={0}>
          {label}
        </SizableText>
        <XStack display="none" shrink={0} $max-sm={{ display: 'flex' }}>
          <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
            {value.toFixed(1)}
          </SizableText>
        </XStack>
      </XStack>
      <YStack flex={1} minW={0} height={h} rounded={999} bg="$hover" overflow="hidden" $max-sm={{ flexBasis: 'auto', grow: 0, minW: 0, width: '100%' }}>
        <YStack height={h} width={`${pct}%`} rounded={999} bg={strong ? '$ink' : '$soft'} />
      </YStack>
      <XStack width={28} justify="flex-end" shrink={0} $max-sm={{ display: 'none' }}>
        <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
          {value.toFixed(1)}
        </SizableText>
      </XStack>
    </XStack>
  )
}
