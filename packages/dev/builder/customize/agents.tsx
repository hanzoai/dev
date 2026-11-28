/**
 * Agents: the org's own — a model, instructions, the tools it may call, and
 * what it may spend.
 *
 * Browse is every agent there is to start from: the host's default agents
 * (`cast`) and personas (`personas`), by name and category. Use makes the org's own
 * agent from one and opens it; nothing is added any other way, so an org starts
 * with none. An agent is also made from scratch, from the person signed in
 * (Clone yourself) or from anyone named. Yours lists the org's agents. Opening one is its profile page
 * (`-/customize/agents/<name>`): its face, part, model, instructions, tools,
 * budget and runs, edited in place, and only what changed is sent. An
 * agent calls only the tools it names, and only those that are on for the org,
 * so the choices are the tools that are on; "every tool" is whatever the
 * fleet's MCP server serves when it runs. A budget is required: what it may
 * spend each period, and in one run.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); an agent's name,
 * description, instructions, model and runs, and the categories the host and
 * the platform give their templates, are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { ArrowLeft, Upload, X } from '@hanzogui/lucide-icons-2'
import { Button, Input, Switch, Textarea } from '@hanzo/ui'
import { ModelPicker } from '@hanzo/ui/models'
import { useEffect, useMemo, useRef, useState, type ReactNode } from 'react'

import { RING } from '../pane.ts'
import { agents, ALL, CAST_CAP, CAST_TASK, create, draft, EMPTY, micros, one, PERIODS, person, profile, remove, SCHEDULES, TRIGGERS, update, use, type Agent, type Cast, type Draft, type Period, type Run, type Trigger } from '../api/agents.ts'
import { models, type Model } from '../api/models.ts'
import { tools, type Tool } from '../api/tools.ts'
import { useHost, useTarget, type Ask } from '../host.tsx'
import { useSpend } from '../spend.tsx'
import { useFunds } from '../mind.ts'
import { path } from '../route.ts'
import { Drafted } from './drafted.tsx'
import { Inventory, Traits } from './inventory.tsx'
import { useLoad } from './load.ts'
import { keep, kept, nature, score, type Answers, type Form } from './ocean.ts'
import { say } from './say.ts'
import { Add, Choice, Empty, Failed, Field, FilterPills, Grid, Line, Mark, matches, Part, Sheet, Soft, useCount, Visitor, type Pane } from './ui.tsx'
import { Confirm } from '../ask.tsx'

/** How many cards Browse draws at a time. */
const PAGE = 24
const ANY = phrase('All')

type AgentDomain = 'all' | 'team' | 'engineering' | 'business' | 'creative' | 'thinking'
type AgentSort = 'recommended' | 'az'

const DOMAIN_DEFS: ReadonlyArray<{ id: AgentDomain; label: string }> = [
  { id: 'all', label: phrase('All') },
  { id: 'team', label: phrase('Core team') },
  { id: 'engineering', label: phrase('Engineering & science') },
  { id: 'business', label: phrase('Business & strategy') },
  { id: 'creative', label: phrase('Design & creative') },
  { id: 'thinking', label: phrase('Writing & thinking') },
]

const ENGINEERING_CATS = new Set([
  'engineer', 'programmer', 'tech leader', 'cryptographer', 'scientist',
  'mathematician', 'physicist', 'innovator', 'logician', 'architect',
])
const BUSINESS_CATS = new Set([
  'executive', 'leader', 'investor', 'economist', 'management',
  'accountant', 'merchant', 'advertiser', 'jurist', 'diplomat', 'statesman',
])
const CREATIVE_CATS = new Set([
  'designer', 'artist', 'filmmaker', 'musician', 'composer',
  'photographer', 'sculptor', 'choreographer', 'singer', 'comedian',
])

function domainOf(c: Cast, isTeam: boolean): Exclude<AgentDomain, 'all'> {
  if (isTeam || c.category.toLowerCase() === 'team') return 'team'
  const cat = c.category.toLowerCase()
  if (ENGINEERING_CATS.has(cat)) return 'engineering'
  if (BUSINESS_CATS.has(cat)) return 'business'
  if (CREATIVE_CATS.has(cat)) return 'creative'
  return 'thinking'
}

const TRIGGER_LABELS: Record<Trigger, { title: string; detail: string }> = {
  webhook: { title: phrase('Webhook & API events'), detail: phrase('Wake immediately when an inbound webhook or API event fires.') },
  git: { title: phrase('Git, PR & CI events'), detail: phrase('React proactively to pushes, pull requests, and build failures.') },
  channel: { title: phrase('Channel & message events'), detail: phrase('Listen and respond to Slack, Discord, Telegram, and team channels.') },
  watch: { title: phrase('Proactive anomaly watch'), detail: phrase('Continuously monitor metrics, queues, and alerts 24/7.') },
}

const FAST_BOTS = new Set(['dev', 'cyber', 'sal', 'mark', 'rev', 'biz', 'su', 'data'])
const GIT_BOTS = new Set(['dev', 'cyber', 'sci', 'data', 'doc', 'lang'])

/** Any real agent preset configured to operate as a 24/7 autonomous, proactive, and event-driven bot. */
function asBot(c: Cast): Cast {
  const key = c.name.toLowerCase()
  const schedule = c.schedule || (FAST_BOTS.has(key) ? '*/15 * * * *' : '0 9 * * 1-5')
  const triggers: Trigger[] = c.triggers?.length
    ? c.triggers
    : GIT_BOTS.has(key)
      ? ['git', 'webhook', 'watch']
      : ['channel', 'webhook', 'watch']
  return {
    ...c,
    category: 'Bot',
    executionMode: 'long-running',
    schedule,
    triggers,
  }
}

/** Categories as Browse offers them: the default agents' first, as listed, then the rest by how many each holds. */
export function kinds(first: readonly Cast[], rest: readonly Cast[]): string[] {
  const lead = [...new Set(first.map((c) => c.category).filter((cat) => cat !== 'Bot'))]
  const count = new Map<string, number>()
  for (const c of rest) if (c.category !== 'Bot' && !lead.includes(c.category)) count.set(c.category, (count.get(c.category) ?? 0) + 1)
  return [ANY, ...lead, ...[...count.keys()].sort((a, b) => count.get(b)! - count.get(a)! || a.localeCompare(b))]
}

const money = (m: number): string => `$${(m / 1_000_000).toFixed(m % 10_000 ? 4 : 2).replace(/\.?0+$/, '')}`

/** A category as Browse names it: All and people, in the reader's words; the rest are the host's, as given. */
const category = (c: string, t: Say): string => (c === ANY || c === 'Person' ? t(c) : c)

/**
 * A budget's sentences by its period ('' where the agent names none): whole
 * sentences, so a language says the period its own way.
 */
const SPENT: Record<Period | '', string> = {
  day: phrase('{spent} of {cap} this day'),
  week: phrase('{spent} of {cap} this week'),
  month: phrase('{spent} of {cap} this month'),
  '': phrase('{spent} of {cap} this period'),
}
const BUDGET: Record<Period | '', string> = {
  day: phrase('{spent} of {cap} spent this day; at most {task} a run.'),
  week: phrase('{spent} of {cap} spent this week; at most {task} a run.'),
  month: phrase('{spent} of {cap} spent this month; at most {task} a run.'),
  '': phrase('{spent} of {cap} spent this period; at most {task} a run.'),
}
/** What a 24/7 bot may spend, said before it is made: whole sentences by its period. */
const AROUND: Record<Period, string> = {
  day: phrase('{name} runs by itself around the clock, on its schedule and when its triggers fire, and may spend up to {cap} a day until you pause or delete it.'),
  week: phrase('{name} runs by itself around the clock, on its schedule and when its triggers fire, and may spend up to {cap} a week until you pause or delete it.'),
  month: phrase('{name} runs by itself around the clock, on its schedule and when its triggers fire, and may spend up to {cap} a month until you pause or delete it.'),
}

/** The question before a 24/7 bot is made, whichever way it is made: Add, Elect or the editor. */
const bot = (name: string, cap: string, period: Period, t: Say): Ask => ({
  kind: 'bot',
  title: t('Start {name} running 24/7?', { name }),
  what: t(AROUND[period], { name, cap: money(micros(cap)) }),
  yes: t('Start bot'),
  uses: 'credits',
})

const SO_FAR: Record<Period | '', string> = {
  day: phrase('Spent {spent} this day.'),
  week: phrase('Spent {spent} this week.'),
  month: phrase('Spent {spent} this month.'),
  '': phrase('Spent {spent} this period.'),
}
const EACH: Record<Period, string> = { day: phrase('Each day, USD'), week: phrase('Each week, USD'), month: phrase('Each month, USD') }
const PERIOD: Record<Period, string> = { day: phrase('Day'), week: phrase('Week'), month: phrase('Month') }

const CARD = { rounded: '$4', borderWidth: 1, borderColor: '$borderColor', bg: '$panel' } as const
const CARD_HOVER = { borderColor: 'var(--border-strong)', bg: '$hover' } as const

/**
 * Agent card: top row is [Avatar] [Name + short Role] [flex={1}] [+ Use], and
 * the 2-line description spans full card width underneath so copy never crushes.
 */
function AgentCard({
  title,
  role,
  detail,
  meta,
  mark,
  action,
  onOpen,
}: {
  title: string
  role?: string
  detail?: string
  meta?: string
  mark?: ReactNode
  action?: ReactNode
  onOpen?: () => void
}) {
  const identity = (
    <>
      {mark}
      <YStack flex={1} minW={0} gap="$0.5">
        <SizableText size="$3" color="$ink" numberOfLines={1}>
          {title}
        </SizableText>
        {role ? (
          <SizableText size="$1" color="$soft" numberOfLines={1}>
            {role}
          </SizableText>
        ) : null}
      </YStack>
    </>
  )
  const footer =
    detail || meta ? (
      <YStack width="100%" minW={0} gap="$1.5">
        {detail ? (
          <SizableText size="$2" color="$soft" numberOfLines={2}>
            {detail}
          </SizableText>
        ) : null}
        {meta ? (
          <SizableText size="$1" color="$hint" numberOfLines={1}>
            {meta}
          </SizableText>
        ) : null}
      </YStack>
    ) : null

  if (onOpen && !action) {
    return (
      <YStack role="listitem" minW={0} {...CARD} hoverStyle={CARD_HOVER}>
        <YStack
          render="button"
          aria-label={title}
          onPress={onOpen}
          width="100%"
          minW={0}
          p="$4"
          gap="$3"
          $platform-web={{ textAlign: 'start' }}
          focusVisibleStyle={RING}
        >
          <XStack items="center" gap="$3" width="100%" minW={0}>
            {identity}
          </XStack>
          {footer}
        </YStack>
      </YStack>
    )
  }

  return (
    <YStack
      role="listitem"
      gap="$3"
      p="$4"
      minW={0}
      {...CARD}
    >
      <XStack items="center" gap="$3" width="100%" minW={0}>
        {onOpen ? (
          <XStack
            render="button"
            aria-label={title}
            onPress={onOpen}
            flex={1}
            minW={0}
            gap="$3"
            items="center"
            $platform-web={{ textAlign: 'start' }}
            focusVisibleStyle={RING}
          >
            {identity}
          </XStack>
        ) : (
          <XStack flex={1} minW={0} gap="$3" items="center">
            {identity}
          </XStack>
        )}
        {action}
      </XStack>
      {footer}
    </YStack>
  )
}

export function Agents({ view, q, adding, onAdding, onView, onCount, mode = 'agents' }: Pane & { mode?: 'agents' | 'bots' }) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const browsing = view === 'discover'
  const botsMode = mode === 'bots'
  const spend = useSpend()
  const mine = useLoad(signed ? () => agents(target) : null, [] as Agent[], [target, signed])
  const figures = useLoad(browsing && !botsMode && host.personas ? host.personas : null, [] as readonly Cast[], [view, botsMode])
  const yoursList = useMemo(
    () => (botsMode ? mine.value.filter((a) => a.executionMode === 'long-running') : mine.value.filter((a) => a.executionMode !== 'long-running')),
    [mine.value, botsMode],
  )
  useCount(yoursList.length, onCount)
  const [note, setNote] = useState('')
  const [domain, setDomain] = useState<AgentDomain>('all')
  const [sortBy, setSortBy] = useState<AgentSort>('recommended')
  const [showSpecialists, setShowSpecialists] = useState(false)
  const [more, setMore] = useState(PAGE)
  const [using, setUsing] = useState<Cast | null>(null)
  const [scratch, setScratch] = useState(false)
  const [making, setMaking] = useState<'self' | 'person' | null>(null)
  const [electing, setElecting] = useState<Cast | null>(null)
  useEffect(() => setMore(PAGE), [q, domain, sortBy, botsMode])

  const allCast = host.cast ?? []
  const isBotCast = (c: Cast) => c.category === 'Bot' || c.executionMode === 'long-running'
  const first: readonly Cast[] = botsMode
    ? allCast.map(asBot)
    : allCast.map((c): Cast => (isBotCast(c) ? { ...c, category: 'Team', executionMode: undefined, schedule: undefined, triggers: undefined } : c))

  const opened = (a: Agent) => host.go(path({ kind: 'agent', name: a.name }))
  const take = async (c: Cast) => {
    // Add in Bots makes one that runs around the clock on the default budget: asked first.
    if (botsMode && !(await spend.ask(bot(c.title, CAST_CAP, 'month', t)))) return
    setUsing(c)
    setNote('')
    try {
      opened(
        await use(
          target,
          c,
          window.location.href,
          botsMode
            ? {
                executionMode: 'long-running',
                schedule: c.schedule || '*/15 * * * *',
                triggers: c.triggers?.length ? c.triggers : ['webhook', 'git', 'channel', 'watch'],
              }
            : undefined,
        ),
      )
    } catch (e) {
      setNote(say(e, t))
    } finally {
      setUsing(null)
    }
  }

  // Toggleable creation panel opened by `+ New agent` / `+ New bot` — closed by default when browsing so the grid starts immediately.
  const creator =
    adding && signed ? (
      <YStack gap="$3" p="$4" rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$panel">
        <XStack items="center" justify="space-between" gap="$2">
          <SizableText size="$3" fontWeight="500" color="$ink">
            {botsMode ? t('Create a new bot') : t('Create a new agent')}
          </SizableText>
          <XStack
            render="button"
            aria-label={t('Close')}
            onPress={() => onAdding(false)}
            width={32}
            height={32}
            items="center"
            justify="center"
            shrink={0}
            focusVisibleStyle={RING}
          >
            <X size={15} color="$soft" />
          </XStack>
        </XStack>
        {botsMode ? (
          <>
            <SizableText size="$2" color="$soft">
              {t('Build a persistent 24/7 bot from scratch, or elect a preset to run on a schedule and event triggers.')}
            </SizableText>
            <XStack gap="$2" flexWrap="wrap">
              <Button
                size="sm"
                variant="primary"
                onPress={() => {
                  onAdding(false)
                  setScratch(true)
                }}
              >
                {t('Start from scratch')}
              </Button>
              {allCast.length ? (
                <Button
                  size="sm"
                  variant="outline"
                  onPress={() => {
                    onAdding(false)
                    setElecting((first[0] ?? allCast[0])!)
                  }}
                >
                  {t('Elect 24/7 preset')}
                </Button>
              ) : null}
            </XStack>
          </>
        ) : (
          <>
            <Drafted onMade={opened} />
            <XStack gap="$2" flexWrap="wrap" items="center" pt="$1" borderTopWidth={1} borderColor="$borderColor">
              <SizableText size="$1" color="$soft" paddingInlineEnd="$1">
                {t('Or start another way')}
              </SizableText>
              <Button
                size="sm"
                variant="outline"
                onPress={() => {
                  onAdding(false)
                  setScratch(true)
                }}
              >
                {t('Start from scratch')}
              </Button>
              <Button
                size="sm"
                variant="outline"
                onPress={() => {
                  onAdding(false)
                  setMaking('self')
                }}
              >
                {t('Clone yourself')}
              </Button>
              <Button
                size="sm"
                variant="outline"
                onPress={() => {
                  onAdding(false)
                  setMaking('person')
                }}
              >
                {t('Create from a person')}
              </Button>
            </XStack>
          </>
        )}
      </YStack>
    ) : null

  const modals = (
    <>
      {spend.dialog}
      {scratch ? (
        <Editor
          defaultBot={botsMode}
          onClose={() => setScratch(false)}
          onSaved={(a) => {
            setScratch(false)
            onAdding(false)
            onView('yours')
            mine.reload()
            setNote(t('{name} is saved', { name: a.name }))
          }}
        />
      ) : null}
      {making ? <Person self={making === 'self'} onClose={() => setMaking(null)} onMade={opened} /> : null}
      {electing ? <Elect preset={electing} all={allCast} onClose={() => setElecting(null)} onElected={opened} /> : null}
    </>
  )

  if (browsing) {
    const rest = botsMode ? ([] as readonly Cast[]) : figures.value.filter((c) => !isBotCast(c))
    const teamSet = new Set(first.map((c) => `${c.name}/${c.title}`))
    const sortList = (list: readonly Cast[]) =>
      sortBy === 'az' ? [...list].sort((a, b) => a.title.localeCompare(b.title)) : [...list]

    const queryText = q.trim()
    const matchCast = (c: Cast) =>
      matches(q, c.title, c.name, c.description, c.category, ...(c.after ?? []))

    const teamFiltered = sortList(first.filter((c) => (botsMode || domain === 'all' || domain === 'team') && matchCast(c)))
    const specialistsFiltered = sortList(
      rest.filter(
        (c) =>
          !botsMode &&
          (domain === 'all' || domainOf(c, teamSet.has(`${c.name}/${c.title}`)) === domain) &&
          matchCast(c),
      ),
    )

    const card = (c: Cast, inCoreTeam = false) => {
      const cat = botsMode ? t('Bot') : category(c.category, t)
      const hideTeamLabel = inCoreTeam && !botsMode && c.category.toLowerCase() === 'team'
      const role = hideTeamLabel
        ? c.after?.length
          ? t('after {names}', { names: c.after.join(', ') })
          : undefined
        : cat || undefined
      const detail =
        [
          c.description === c.title || c.description === c.category ? '' : c.description,
          !hideTeamLabel && c.after?.length ? t('after {names}', { names: c.after.join(', ') }) : '',
        ]
          .filter(Boolean)
          .join(' · ') || undefined
      const meta = botsMode
        ? [c.executionMode === 'long-running' ? t('24/7 · proactive · event-driven') : '', c.schedule].filter(Boolean).join(' · ') || undefined
        : undefined
      return (
        <AgentCard
          key={`${c.category}/${c.name}/${c.title}`}
          title={c.title}
          role={role}
          detail={detail}
          meta={meta}
          mark={<Mark name={c.title} size={40} logo={c.avatar} />}
          onOpen={signed && botsMode ? () => setElecting(c) : undefined}
          action={
            signed ? (
              <XStack items="center" gap="$1.5" shrink={0}>
                {botsMode ? (
                  <XStack $max-sm={{ display: 'none' }}>
                    <Button
                      size="sm"
                      variant="ghost"
                      disabled={using !== null}
                      aria-label={t('Elect {name} to run 24/7', { name: c.title })}
                      onPress={() => setElecting(c)}
                    >
                      {t('Configure 24/7')}
                    </Button>
                  </XStack>
                ) : null}
                <Add name={c.title} use added={false} busy={using !== null} onPress={() => void take(c)} />
              </XStack>
            ) : undefined
          }
        />
      )
    }

    const domainCounts: Record<AgentDomain, number> = {
      all: first.length + rest.length,
      team: first.length,
      engineering: rest.filter((c) => domainOf(c, false) === 'engineering').length,
      business: rest.filter((c) => domainOf(c, false) === 'business').length,
      creative: rest.filter((c) => domainOf(c, false) === 'creative').length,
      thinking: rest.filter((c) => domainOf(c, false) === 'thinking').length,
    }

    const domainOptions = DOMAIN_DEFS.filter((d) => d.id === 'all' || d.id === 'team' || domainCounts[d.id] > 0).map((d) => ({
      id: d.id,
      label: d.id === 'all' ? t(d.label) : `${t(d.label)}`,
    }))

    const hasAnyMatch = teamFiltered.length > 0 || specialistsFiltered.length > 0
    const showGroupedAll = !botsMode && domain === 'all' && !queryText
    const specialistsVisible = showGroupedAll ? showSpecialists : true

    return (
      <YStack gap="$4">
        {creator}
        {!botsMode && domainOptions.length > 1 ? (
          <XStack items="center" justify="space-between" gap="$3" flexWrap="wrap">
            <XStack grow={1} shrink={1} minW={0}>
              <FilterPills
                label={t('Category')}
                value={domain}
                options={domainOptions}
                onChange={(next) => setDomain(next as AgentDomain)}
                maxVisibleDesktop={6}
                mobileLimit={4}
              />
            </XStack>
            <XStack
              role="radiogroup"
              aria-label={t('Sort agents')}
              items="center"
              gap={2}
              p={2}
              shrink={0}
            >
              {(
                [
                  { id: 'recommended', label: t('Recommended') },
                  { id: 'az', label: t('A–Z') },
                ] as const
              ).map((opt) => {
                const active = sortBy === opt.id
                return (
                  <XStack
                    key={opt.id}
                    render="button"
                    role="radio"
                    aria-checked={active}
                    onPress={() => setSortBy(opt.id)}
                    px="$2.5"
                    py="$1"
                    minH={28}
                    focusVisibleStyle={RING}
                    items="center"
                    justify="center"
                  >
                    <SizableText size="$1" fontWeight={active ? '600' : '400'} color={active ? '$ink' : '$soft'}>
                      {opt.label}
                    </SizableText>
                  </XStack>
                )
              })}
            </XStack>
          </XStack>
        ) : null}
        <Line>{note}</Line>

        {!hasAnyMatch ? (
          figures.loading ? null : queryText ? (
            <Soft>{t(botsMode ? 'No bot matches “{q}”.' : 'No agent matches “{q}”.', { q: queryText })}</Soft>
          ) : botsMode ? (
            <Soft>{t('Create a persistent 24/7 bot from scratch or elect any preset.')}</Soft>
          ) : (
            <Soft>{t('No agent matches “{q}”.', { q: queryText })}</Soft>
          )
        ) : (
          <YStack gap="$5">
            {teamFiltered.length > 0 ? (
              <YStack gap="$3">
                {!botsMode ? (
                  <XStack items="baseline" justify="space-between" gap="$2">
                    <SizableText render="h2" size="$4" color="$ink">
                      {t('Core team')}
                    </SizableText>
                    <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
                      {t('{n} ready-to-use specialists', { n: teamFiltered.length })}
                    </SizableText>
                  </XStack>
                ) : null}
                <Grid label={botsMode ? t('Bots to start from') : t('Core team agents')}>
                  {teamFiltered.map((c) => card(c, !botsMode))}
                </Grid>
              </YStack>
            ) : null}

            {specialistsFiltered.length > 0 ? (
              <YStack
                gap="$3"
                pt={teamFiltered.length > 0 ? '$5' : 0}
                borderTopWidth={teamFiltered.length > 0 ? 1 : 0}
              >
                <XStack items="center" justify="space-between" gap="$3" flexWrap="wrap">
                  <YStack gap="$1.5" flex={1} minW={200}>
                    <SizableText render="h2" size="$4" color="$ink">
                      {domain === 'all' ? t('Domain thinkers & personas') : t(DOMAIN_DEFS.find((d) => d.id === domain)?.label ?? 'Personas')}
                    </SizableText>
                    <SizableText size="$2" color="$soft">
                      {t('{n} curated personas across science, strategy, design, and writing', { n: specialistsFiltered.length })}
                    </SizableText>
                  </YStack>
                  {showGroupedAll ? (
                    <Button
                      size="sm"
                      variant="outline"
                      onPress={() => setShowSpecialists((v) => !v)}
                    >
                      {showSpecialists ? t('Hide personas') : t('Browse {n} personas', { n: specialistsFiltered.length })}
                    </Button>
                  ) : null}
                </XStack>
                {specialistsVisible ? (
                  <>
                    <Grid label={t('Domain thinkers & personas')}>
                      {specialistsFiltered.slice(0, more).map((c) => card(c, false))}
                    </Grid>
                    {specialistsFiltered.length > more ? (
                      <XStack>
                        <Button size="sm" variant="outline" onPress={() => setMore(more + PAGE)}>
                          {t('Show more ({remaining} left)', { remaining: specialistsFiltered.length - more })}
                        </Button>
                      </XStack>
                    ) : null}
                  </>
                ) : null}
              </YStack>
            ) : null}
          </YStack>
        )}

        {figures.error ? <Failed error={figures.error} onRetry={figures.reload} /> : figures.loading ? <Soft>{t('Loading…')}</Soft> : null}
        {modals}
      </YStack>
    )
  }

  if (!signed) {
    return <Visitor>{t(botsMode ? 'Sign in to see your bots.' : 'Sign in to see your agents.')}</Visitor>
  }
  const shown = yoursList.filter((a) => matches(q, a.name, a.description, a.model))
  return (
    <YStack gap="$3">
      {creator}
      <Line>{note}</Line>
      {mine.error && !yoursList.length ? (
        <Failed error={mine.error} onRetry={mine.reload} />
      ) : mine.loading && !yoursList.length ? (
        <Soft>{t(botsMode ? 'Loading your bots…' : 'Loading your agents…')}</Soft>
      ) : !yoursList.length ? (
        <Empty
          title={t(botsMode ? 'No bots yet' : 'No agents yet')}
          detail={t(botsMode ? 'Create a 24/7 bot from scratch or browse presets to start from.' : 'Create one with AI, from scratch, or browse agents to start from.')}
        >
          <Button size="sm" variant="outline" onPress={() => onView('discover')}>
            {t(botsMode ? 'Browse bots' : 'Browse agents')}
          </Button>
          <Button size="sm" onPress={() => onAdding(true)}>
            {t(botsMode ? 'New bot' : 'New agent')}
          </Button>
        </Empty>
      ) : !shown.length ? (
        <Soft>{t(botsMode ? 'No bot of yours matches “{q}”.' : 'No agent of yours matches “{q}”.', { q: q.trim() })}</Soft>
      ) : (
        <Grid label={t(botsMode ? 'Your bots' : 'Your agents')}>
          {shown.map((a) => (
            <AgentCard
              key={a.id || a.name}
              title={a.name}
              role={a.model || undefined}
              detail={a.description || t('No description.')}
              meta={[
                a.executionMode === 'long-running' ? t('24/7') : '',
                a.schedule || '',
                a.runs === 1 ? t('1 run') : t('{n} runs', { n: a.runs }),
                reach(a, t),
                a.cap ? t(SPENT[a.period], { spent: money(a.spent), cap: money(a.cap) }) : '',
              ]
                .filter(Boolean)
                .join(' · ')}
              mark={<Face agent={a} cast={host.cast ?? []} />}
              onOpen={() => opened(a)}
            />
          ))}
        </Grid>
      )}
      {modals}
    </YStack>
  )
}

/** Elect an agent or bot preset to run autonomously 24/7, proactively on a schedule, and event-driven. */
function Elect({
  preset,
  all,
  onClose,
  onElected,
}: {
  preset: Cast
  all: readonly Cast[]
  onClose: () => void
  onElected: (a: Agent) => void
}) {
  const { t } = useI18n()
  const target = useTarget()
  const [picked, setPicked] = useState<Cast>(preset)
  const [autonomous, setAutonomous] = useState(true)
  const [schedule, setSchedule] = useState(preset.schedule || '*/15 * * * *')
  const [triggers, setTriggers] = useState<Trigger[]>(preset.triggers?.length ? preset.triggers : ['webhook', 'git', 'channel', 'watch'])
  const [cap, setCap] = useState(CAST_CAP)
  const [task, setTask] = useState(CAST_TASK)
  const [period, setPeriod] = useState<Period>('month')
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')
  const spend = useSpend()

  const selectPreset = (c: Cast) => {
    setPicked(c)
    setSchedule(c.schedule || '*/15 * * * *')
    setTriggers(c.triggers?.length ? c.triggers : ['webhook', 'git', 'channel', 'watch'])
  }

  const toggleTrigger = (tr: Trigger, yes: boolean) => {
    setTriggers((prev) => (yes ? [...new Set([...prev, tr])] : prev.filter((x) => x !== tr)))
  }

  const save = async () => {
    if (autonomous && !(await spend.ask(bot(picked.title, cap, period, t)))) return
    setWorking(true)
    setNote('')
    try {
      const created = await use(target, picked, window.location.href, {
        executionMode: autonomous ? 'long-running' : 'one-shot',
        schedule: autonomous ? schedule.trim() || '*/15 * * * *' : undefined,
        triggers: autonomous ? triggers : undefined,
        cap,
        task,
        period,
      })
      onElected(created)
    } catch (e) {
      setNote(say(e, t))
      setWorking(false)
    }
  }

  return (
    <Sheet title={t('Elect 24/7 autonomous preset')} open onOpenChange={(o) => !o && onClose()} width={680}>
      {spend.dialog}
      <XStack items="center" gap="$3" p="$3" rounded="$3" borderWidth={1} borderColor="$borderColor">
        <Mark name={picked.title} size={44} logo={picked.avatar} />
        <YStack flex={1} minW={0}>
          <SizableText size="$3" color="$ink">
            {picked.title}
          </SizableText>
          <SizableText size="$1" color="$soft" numberOfLines={2}>
            {picked.description || picked.category}
          </SizableText>
        </YStack>
      </XStack>
      {all.length > 1 ? (
        <Field label={t('Preset')} hint={t('Select any agent or bot preset to elect as an autonomous 24/7 worker.')}>
          <XStack gap="$1.5" flexWrap="wrap" maxH={136} overflow="scroll" p="$1" borderWidth={1} borderColor="$borderColor" bg="$panel" rounded="$3">
            {all.map((c) => {
              const active = c.name === picked.name && c.category === picked.category
              return (
                <Button
                  key={`${c.category}/${c.name}`}
                  size="sm"
                  variant={active ? 'outline' : 'ghost'}
                  aria-pressed={active}
                  onPress={() => selectPreset(c)}
                >
                  {c.title}
                </Button>
              )
            })}
          </XStack>
        </Field>
      ) : null}
      <XStack items="center" gap="$3" p="$3" rounded="$3" borderWidth={1} borderColor="$borderColor">
        <YStack flex={1} minW={0}>
          <SizableText size="$2" color="$ink">
            {t('Run autonomously 24/7')}
          </SizableText>
          <SizableText size="$1" color="$soft">
            {t('Stays active continuously (long-running), waking proactively on its schedule and when events fire.')}
          </SizableText>
        </YStack>
        <Switch checked={autonomous} onCheckedChange={setAutonomous} aria-label={t('Run autonomously 24/7')} />
      </XStack>
      {autonomous ? (
        <>
          <Field label={t('Proactive schedule')} hint={t('Choose how often this agent wakes proactively, or enter any 5-field cron expression.')}>
            <YStack gap="$2">
              <XStack gap="$1.5" flexWrap="wrap">
                {SCHEDULES.map((s) => {
                  const active = schedule.trim() === s.cron
                  return (
                    <Button
                      key={s.cron}
                      size="sm"
                      variant="outline"
                      aria-pressed={active}
                      onPress={() => setSchedule(s.cron)}
                    >
                      {t(s.label)}
                    </Button>
                  )
                })}
              </XStack>
              <Input value={schedule} onChangeText={setSchedule} placeholder="*/15 * * * *" aria-label={t('Proactive schedule')} />
            </YStack>
          </Field>
          <YStack gap="$2">
            <SizableText size="$2" color="$ink">
              {t('Event-driven triggers')}
            </SizableText>
            <YStack borderWidth={1} borderColor="$borderColor" rounded="$3">
              {TRIGGERS.map((tr, i) => (
                <XStack key={tr} items="center" gap="$3" px="$3" py="$2" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                  <YStack flex={1} minW={0}>
                    <SizableText size="$2" color="$ink">
                      {t(TRIGGER_LABELS[tr].title)}
                    </SizableText>
                    <SizableText size="$1" color="$soft">
                      {t(TRIGGER_LABELS[tr].detail)}
                    </SizableText>
                  </YStack>
                  <Switch checked={triggers.includes(tr)} onCheckedChange={(v: boolean) => toggleTrigger(tr, v)} aria-label={t(TRIGGER_LABELS[tr].title)} />
                </XStack>
              ))}
            </YStack>
          </YStack>
        </>
      ) : null}
      <YStack gap="$2">
        <SizableText size="$2" color="$ink">
          {t('Budget')}
        </SizableText>
        <XStack gap="$3" flexWrap="wrap" rowGap="$2" items="flex-end">
          <YStack gap="$1" width={140}>
            <SizableText size="$1" color="$soft">
              {t(EACH[period])}
            </SizableText>
            <Input value={cap} onChangeText={setCap} placeholder="10" aria-label={t('Budget each period')} inputMode="decimal" />
          </YStack>
          <YStack gap="$1" width={140}>
            <SizableText size="$1" color="$soft">
              {t('One run, USD')}
            </SizableText>
            <Input value={task} onChangeText={setTask} placeholder="1" aria-label={t('Budget for one run')} inputMode="decimal" />
          </YStack>
          <Choice label={t('Period')} value={period} options={PERIODS.map((p) => ({ id: p, label: t(PERIOD[p]) }))} onChange={(p: Period) => setPeriod(p)} />
        </XStack>
      </YStack>
      <Line>{note}</Line>
      <XStack gap="$2" items="center">
        <XStack flex={1} />
        <Button size="sm" variant="ghost" onPress={onClose}>
          {t('Cancel')}
        </Button>
        <Button size="sm" variant="primary" disabled={working || (autonomous && !schedule.trim())} onPress={() => void save()}>
          {autonomous ? t('Elect and run 24/7') : t('Create')}
        </Button>
      </XStack>
    </Sheet>
  )
}

/**
 * Clone yourself, or anyone named: a name, who they are, and writing of theirs,
 * pasted or uploaded. Yourself also takes the Big Five questionnaire, kept in
 * this browser as it is answered; its result seeds the instructions, which are
 * shown to edit before the agent is made.
 */
function Person({ self, onClose, onMade }: { self: boolean; onClose: () => void; onMade: (a: Agent) => void }) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const me = self ? host.person : null
  const who = me?.email || me?.name || ''
  const [name, setName] = useState(me?.name ?? '')
  const [about, setAbout] = useState('')
  const [writing, setWriting] = useState('')
  const [answers, setAnswers] = useState(() => kept(who))
  const [taking, setTaking] = useState(false)
  const [edited, setEdited] = useState<string | null>(null)
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')
  const file = useRef<HTMLInputElement | null>(null)
  const sheet = answers[answers.form]
  const answered = sheet.filter(Boolean).length
  const result = self && answered === sheet.length ? score(answers.form, sheet) : null
  const cast = person(name, about, writing, me?.avatar, result ? nature(name.trim(), answers.form, result) : '')
  const instructions = edited ?? cast.instructions
  const answer = (a: Answers) => {
    setAnswers(a)
    keep(who, a)
  }
  const read = async (files: FileList | null) => {
    const texts = await Promise.all([...(files ?? [])].map((f) => f.text()))
    setWriting((w) => [w.trim(), ...texts.map((x) => x.trim())].filter(Boolean).join('\n\n'))
  }
  const save = async () => {
    setWorking(true)
    setNote('')
    try {
      onMade(await use(target, { ...cast, instructions }, window.location.href))
    } catch (e) {
      setNote(say(e, t))
      setWorking(false)
    }
  }
  return (
    <Sheet title={self ? t('Clone yourself') : t('Create from a person')} open onOpenChange={(o) => !o && onClose()} full>
      {taking ? (
        <Inventory answers={answers} onChange={answer} onDone={() => setTaking(false)} />
      ) : (
        <>
          <Field label={t('Name')}>
            <Input value={name} onChangeText={setName} aria-label={t('Name')} />
          </Field>
          <Field label={t('Description')}>
            <Textarea value={about} onChangeText={setAbout} aria-label={t('Description')} rows={3} />
          </Field>
          {self ? (
            <Field label={t('Personality')}>
              <YStack gap="$3">
                <Choice
                  label={t('Questionnaire')}
                  value={answers.form}
                  options={[
                    { id: 'short', label: t('50 statements') },
                    { id: 'long', label: t('120 statements') },
                  ]}
                  onChange={(form: Form) => answer({ ...answers, form })}
                />
                {result ? <Traits form={answers.form} score={result} /> : null}
                <XStack>
                  <Button size="sm" variant={result ? 'ghost' : 'outline'} onPress={() => setTaking(true)}>
                    {result ? t('Change answers') : answered ? t('Resume, {n} of {total}', { n: answered, total: sheet.length }) : t('Start')}
                  </Button>
                </XStack>
              </YStack>
            </Field>
          ) : null}
          <Field label={t('Writing')}>
            <Textarea value={writing} onChangeText={setWriting} aria-label={t('Writing')} rows={6} />
          </Field>
          <XStack>
            <Button size="sm" variant="ghost" onPress={() => file.current?.click()}>
              <Upload size={14} /> {t('Upload')}
            </Button>
            <input
              ref={file}
              type="file"
              multiple
              accept=".txt,.md,.markdown,.csv,.json,.html,text/*"
              hidden
              aria-label={t('Upload writing')}
              onChange={(e) => {
                void read(e.currentTarget.files)
                e.currentTarget.value = ''
              }}
            />
          </XStack>
          <Field label={t('Instructions')}>
            <Textarea value={instructions} onChangeText={setEdited} aria-label={t('Instructions')} rows={8} />
          </Field>
          <Line>{note}</Line>
          <XStack gap="$2" items="center">
            <XStack flex={1} />
            <Button size="sm" variant="ghost" onPress={onClose}>
              {t('Cancel')}
            </Button>
            <Button size="sm" disabled={working || !name.trim()} onPress={() => void save()}>
              {t('Create')}
            </Button>
          </XStack>
        </>
      )}
    </Sheet>
  )
}

function reach(a: Agent, t: Say): string {
  if (a.tools.includes(ALL)) return t('every tool')
  return a.tools.length === 1 ? t('1 tool') : t('{n} tools', { n: a.tools.length })
}

/** An agent's face: its picture, the default agent's of its name, its emoji, or its initial. */
function Face({ agent: a, cast, size = 32 }: { agent: Agent; cast: readonly Cast[]; size?: number }) {
  const avatar = a.avatar || cast.find((c) => c.name.toLowerCase() === a.name.toLowerCase())?.avatar
  return <Mark name={a.name} size={size} logo={avatar} icon={a.emoji ? <SizableText size={size > 40 ? '$8' : '$4'}>{a.emoji}</SizableText> : undefined} />
}

/** One agent's page: who it is, what it runs on and reads, what it may call and spend, and what it has done. */
export function Profile({ name }: { name: string }) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const got = useLoad(host.person ? () => profile(target, name) : null, null as { agent: Agent; runs: Run[] } | null, [target, name])
  const [editing, setEditing] = useState(false)
  const a = got.value?.agent
  const isBot = a?.executionMode === 'long-running'
  const back = () => host.go(path({ kind: 'customize', tab: isBot ? 'bots' : 'agents' }))
  return (
    <YStack flex={1} minH={0} overflow="scroll" px="$6" py="$6" $max-md={{ px: '$4', py: '$4' }}>
      <YStack width="100%" maxW={880} mx="auto" gap="$5">
        <XStack render="button" onPress={back} items="center" gap="$1.5" self="flex-start" cursor="pointer" aria-label={t(isBot ? 'Back to bots' : 'Back to agents')}>
          <ArrowLeft size={14} color="$soft" {...{ 'data-i18n-mirror': '' }} />
          <SizableText size="$2" color="$soft">
            {t(isBot ? 'Bots' : 'Agents')}
          </SizableText>
        </XStack>
        {!host.person ? (
          <Visitor>{t('Sign in to see this agent.')}</Visitor>
        ) : got.error && !a ? (
          <Failed error={got.error} onRetry={got.reload} />
        ) : !a ? (
          <Soft>{t('Loading the agent…')}</Soft>
        ) : (
          <>
            <XStack items="center" gap="$4">
              <Face agent={a} cast={host.cast ?? []} size={64} />
              <YStack flex={1} minW={0} gap="$1">
                <SizableText render="h1" size="$7" color="$ink" numberOfLines={1}>
                  {a.name}
                </SizableText>
                <SizableText size="$3" color="$soft">
                  {a.description || t('No role written yet.')}
                </SizableText>
              </YStack>
              <Button size="sm" variant="outline" onPress={() => setEditing(true)}>
                {t('Edit')}
              </Button>
            </XStack>
            <Part title={t('Model')} detail={a.model || t('The deployment’s default')} />
            <Part
              title={t('Autonomous and event-driven')}
              detail={
                a.executionMode === 'long-running'
                  ? [t('Runs autonomously 24/7'), a.schedule ? `${t('Schedule')}: ${a.schedule}` : '', a.triggers?.length ? `${t('Triggers')}: ${a.triggers.join(', ')}` : '']
                      .filter(Boolean)
                      .join(' · ')
                  : t('One-shot (on-demand). Edit to enable 24/7 proactive and event-driven execution.')
              }
            />
            <Part title={t('Instructions')} detail={a.instructions ? '' : t('None written: it runs on its model alone.')}>
              {a.instructions ? (
                <YStack p="$3" rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$panel" maxH={320} overflow="scroll">
                  <SizableText size="$2" color="$ink" $platform-web={{ whiteSpace: 'pre-wrap' }}>
                    {a.instructions}
                  </SizableText>
                </YStack>
              ) : null}
            </Part>
            <Part title={t('Tools')} detail={a.tools.includes(ALL) ? t('Every tool the fleet’s MCP server serves.') : a.tools.length ? a.tools.join(', ') : t('None: it calls no tool.')} />
            <Part title={t('Budget')} detail={a.cap ? t(BUDGET[a.period], { spent: money(a.spent), cap: money(a.cap), task: money(a.task) }) : t('No budget set.')} />
            <Part
              title={t('Runs')}
              detail={a.runs ? t('{runs} in all; the latest {n} below.', { runs: a.runs, n: Math.min(a.runs, got.value!.runs.length) }) : t('It has not run yet.')}
            >
              {got.value!.runs.map((r) => (
                <XStack key={r.id} gap="$3" py="$2" borderTopWidth={1} borderColor="$borderColor" items="flex-start">
                  <SizableText size="$1" color={r.status === 'ok' ? '$soft' : '$bad'} width={44} shrink={0}>
                    {r.status === 'ok' ? t('ok') : t('failed')}
                  </SizableText>
                  <YStack flex={1} minW={0}>
                    <SizableText size="$2" color="$ink" numberOfLines={1}>
                      {r.input || t('(no input)')}
                    </SizableText>
                    <SizableText size="$1" color="$soft" numberOfLines={2}>
                      {r.status === 'ok' ? r.output : r.error}
                    </SizableText>
                  </YStack>
                  <SizableText size="$1" color="$soft" shrink={0}>
                    {[r.model, r.at ? new Date(r.at).toLocaleString() : ''].filter(Boolean).join(' · ')}
                  </SizableText>
                </XStack>
              ))}
            </Part>
          </>
        )}
      </YStack>
      {editing && a ? (
        <Editor
          agent={a}
          onClose={() => setEditing(false)}
          onSaved={() => {
            setEditing(false)
            got.reload()
          }}
          onDeleted={back}
        />
      ) : null}
    </YStack>
  )
}

/** Write a new agent, or change one; an existing agent is read whole first, for its instructions. */
function Editor({
  agent,
  defaultBot = false,
  onClose,
  onSaved,
  onDeleted,
}: {
  agent?: Agent
  defaultBot?: boolean
  onClose: () => void
  onSaved: (a: Agent) => void
  onDeleted?: () => void
}) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const full = useLoad(agent ? () => one(target, agent.id || agent.name) : null, null as Agent | null, [target, agent?.id])
  const catalog = useLoad(() => models(target), [] as Model[], [target])
  const funds = useFunds(target, host.topUp)
  const on = useLoad(() => tools(target, { activated: true }), [] as Tool[], [target])
  const [d, setD] = useState<Draft>(() =>
    defaultBot
      ? { ...EMPTY, executionMode: 'long-running', schedule: '*/15 * * * *', triggers: ['webhook', 'git', 'channel', 'watch'] }
      : EMPTY,
  )
  const [ready, setReady] = useState(!agent)
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')
  const [asking, setAsking] = useState(false)
  const [find, setFind] = useState('')
  const spend = useSpend()

  useEffect(() => {
    if (full.value) {
      setD(draft(full.value))
      setReady(true)
    }
  }, [full.value])

  const set = (p: Partial<Draft>) => setD((x) => ({ ...x, ...p }))
  const every = d.tools.includes(ALL)
  // The tools that are on, and any the agent already names that are not.
  const choices = useMemo(() => {
    const listed = on.value.map((x) => ({ name: x.name, source: x.source, description: x.description }))
    const seen = new Set(listed.map((x) => x.name))
    const kept = d.tools.filter((n) => n !== ALL && !seen.has(n)).map((name) => ({ name, source: t('not on'), description: '' }))
    return [...kept, ...listed]
  }, [on.value, d.tools, t])
  const shownTools = choices.filter((x) => matches(find, x.name, x.source, x.description))
  const save = async () => {
    const was = full.value
    // Made, or turned, 24/7: it starts spending by itself, so it is asked first.
    const around = d.executionMode === 'long-running' && was?.executionMode !== 'long-running'
    if (around && !(await spend.ask(bot(d.name.trim() || t('This bot'), d.cap, d.period, t)))) return
    setWorking(true)
    setNote('')
    try {
      const saved = agent && was ? await update(target, was, d) : await create(target, d)
      onSaved(saved)
    } catch (e) {
      setNote(say(e, t))
    } finally {
      setWorking(false)
    }
  }

  const pick = (name: string, yes: boolean) => set({ tools: yes ? [...d.tools, name] : d.tools.filter((n) => n !== name) })

  return (
    <Sheet title={agent ? agent.name : t(defaultBot ? 'New bot' : 'New agent')} open onOpenChange={(o) => !o && onClose()} width={680}>
      {spend.dialog}
      {agent && full.error && !full.value ? (
        <Failed error={full.error} onRetry={full.reload} />
      ) : !ready ? (
        <Soft>{t('Loading the agent…')}</Soft>
      ) : (
        <>
          <Field label={t('Name')} hint={agent ? t('An agent keeps its name.') : t('Letters, digits, . _ or -. Other agents call it as agent_<name>.')}>
            <Input value={d.name} onChangeText={(v: string) => set({ name: v })} aria-label={t('Name')} disabled={Boolean(agent)} autoCapitalize="none" />
          </Field>
          <Field label={t('Description')} hint={t('The line another agent reads to decide whether to call this one.')}>
            <Input value={d.description} onChangeText={(v: string) => set({ description: v })} placeholder={t('Answers questions about our codebase')} aria-label={t('Description')} />
          </Field>
          <Field label={t('Model')}>
            <XStack items="center" gap="$2" flexWrap="wrap">
              <ModelPicker
                size="sm"
                name={t('Model')}
                placeholder={t('The deployment’s default')}
                models={catalog.value}
                scope="agent"
                funded={funds.funded}
                balance={funds.balance}
                onTopUp={funds.topUp}
                value={d.model || undefined}
                onChange={(id) => set({ model: id })}
                loading={catalog.loading}
                error={catalog.error ? say(catalog.error, t) : null}
              />
              {d.model && !agent ? (
                <Button size="sm" variant="ghost" onPress={() => set({ model: '' })}>
                  {t('Use the default')}
                </Button>
              ) : null}
            </XStack>
          </Field>
          <Field label={t('Instructions')} hint={t('The system prompt: what the model reads before every run.')}>
            <Textarea value={d.instructions} onChangeText={(v: string) => set({ instructions: v })} placeholder={t('Be terse, and cite the file you read.')} aria-label={t('Instructions')} rows={8} />
          </Field>
          <YStack gap="$2">
            <XStack items="center" gap="$3">
              <YStack flex={1} minW={0}>
                <SizableText size="$2" color="$ink">
                  {t('Tools')}
                </SizableText>
                <SizableText size="$1" color="$soft">
                  {every
                    ? t('Every tool the fleet’s MCP server serves when it runs.')
                    : d.tools.length
                      ? t('{n} chosen. It can call only these.', { n: d.tools.length })
                      : t('None chosen: it can call no tool.')}
                </SizableText>
              </YStack>
              <SizableText size="$1" color="$soft">
                {t('Every tool')}
              </SizableText>
              <Switch checked={every} onCheckedChange={(v: boolean) => set({ tools: v ? [ALL] : [] })} aria-label={t('Every tool')} />
            </XStack>
            {every ? null : (
              <>
                <Input value={find} onChangeText={setFind} placeholder={t('Find a tool…')} aria-label={t('Find a tool')} />
                <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" maxH={240} overflow="scroll">
                  {on.loading && !on.value.length ? (
                    <Soft>{t('Loading the tools that are on…')}</Soft>
                  ) : on.error && !on.value.length ? (
                    <Failed error={on.error} onRetry={on.reload} />
                  ) : !shownTools.length ? (
                    <Soft>{choices.length ? t('No tool matches.') : t('No tool is on for your organization yet. Add skills or connectors first.')}</Soft>
                  ) : (
                    shownTools.map((x, i) => (
                      <XStack key={x.name} items="center" gap="$3" px="$3" py="$2" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                        <YStack flex={1} minW={0}>
                          <SizableText size="$2" color="$ink" fontFamily="$mono" numberOfLines={1}>
                            {x.name}
                          </SizableText>
                          <SizableText size="$1" color="$soft" numberOfLines={1}>
                            {[x.source, x.description].filter(Boolean).join(' · ')}
                          </SizableText>
                        </YStack>
                        <Switch checked={d.tools.includes(x.name)} onCheckedChange={(v: boolean) => pick(x.name, v)} aria-label={t('Use {name}', { name: x.name })} />
                      </XStack>
                    ))
                  )}
                </YStack>
              </>
            )}
          </YStack>
          <YStack gap="$2">
            <XStack items="center" gap="$3">
              <YStack flex={1} minW={0}>
                <SizableText size="$2" color="$ink">
                  {t('Autonomous and event-driven (24/7)')}
                </SizableText>
                <SizableText size="$1" color="$soft">
                  {d.executionMode === 'long-running'
                    ? t('Runs autonomously 24/7 on its proactive schedule and event triggers.')
                    : t('One-shot (on-demand). Turn on to run autonomously 24/7.')}
                </SizableText>
              </YStack>
              <Switch
                checked={d.executionMode === 'long-running'}
                onCheckedChange={(v: boolean) =>
                  set(
                    v
                      ? { executionMode: 'long-running', schedule: d.schedule || '*/15 * * * *', triggers: d.triggers?.length ? d.triggers : ['webhook', 'git', 'channel', 'watch'] }
                      : { executionMode: undefined, schedule: undefined, triggers: undefined },
                  )
                }
                aria-label={t('Run autonomously 24/7')}
              />
            </XStack>
            {d.executionMode === 'long-running' ? (
              <YStack gap="$2" p="$3" borderWidth={1} borderColor="$borderColor" rounded="$3">
                <SizableText size="$1" color="$soft">
                  {t('Proactive schedule (5-field cron)')}
                </SizableText>
                <XStack gap="$1.5" flexWrap="wrap">
                  {SCHEDULES.map((s) => {
                    const active = (d.schedule ?? '').trim() === s.cron
                    return (
                      <Button
                        key={s.cron}
                        size="sm"
                        variant="outline"
                        aria-pressed={active}
                        onPress={() => set({ schedule: s.cron })}
                      >
                        {t(s.label)}
                      </Button>
                    )
                  })}
                </XStack>
                <Input value={d.schedule ?? ''} onChangeText={(v: string) => set({ schedule: v })} placeholder="*/15 * * * *" aria-label={t('Proactive schedule')} />
                <SizableText size="$1" color="$soft">
                  {t('Event-driven triggers')}
                </SizableText>
                <XStack gap="$1.5" flexWrap="wrap">
                  {TRIGGERS.map((tr) => {
                    const active = (d.triggers ?? []).includes(tr)
                    return (
                      <Button
                        key={tr}
                        size="sm"
                        variant="outline"
                        aria-pressed={active}
                        onPress={() => set({ triggers: active ? (d.triggers ?? []).filter((x) => x !== tr) : [...(d.triggers ?? []), tr] })}
                      >
                        {t(TRIGGER_LABELS[tr].title)}
                      </Button>
                    )
                  })}
                </XStack>
              </YStack>
            ) : null}
          </YStack>
          <YStack gap="$2">
            <SizableText size="$2" color="$ink">
              {t('Budget')}
            </SizableText>
            <XStack gap="$3" flexWrap="wrap" rowGap="$2" items="flex-end">
              <YStack gap="$1" width={140}>
                <SizableText size="$1" color="$soft">
                  {t(EACH[d.period])}
                </SizableText>
                <Input value={d.cap} onChangeText={(v: string) => set({ cap: v })} placeholder="10" aria-label={t('Budget each period')} inputMode="decimal" />
              </YStack>
              <YStack gap="$1" width={140}>
                <SizableText size="$1" color="$soft">
                  {t('One run, USD')}
                </SizableText>
                <Input value={d.task} onChangeText={(v: string) => set({ task: v })} placeholder="1" aria-label={t('Budget for one run')} inputMode="decimal" />
              </YStack>
              <Choice label={t('Period')} value={d.period} options={PERIODS.map((p) => ({ id: p, label: t(PERIOD[p]) }))} onChange={(p: Period) => set({ period: p })} />
            </XStack>
            <SizableText size="$1" color="$soft">
              {[full.value && full.value.cap ? t(SO_FAR[full.value.period], { spent: money(full.value.spent) }) : '', t('A run stops at either limit.')].filter(Boolean).join(' ')}
            </SizableText>
          </YStack>
          <Line>{note}</Line>
          <XStack gap="$2" items="center">
            {agent && onDeleted ? (
              <Button size="sm" variant="ghost" onPress={() => setAsking(true)}>
                {t('Delete')}
              </Button>
            ) : null}
            <XStack flex={1} />
            <Button size="sm" variant="ghost" onPress={onClose}>
              {t('Cancel')}
            </Button>
            <Button size="sm" disabled={working} onPress={() => void save()}>
              {agent ? t('Save') : t('Create')}
            </Button>
          </XStack>
          {agent && onDeleted ? (
            <Confirm
              title={t('Delete {name}?', { name: agent.name })}
              act={t('Delete')}
              says={t('The agent and every run recorded against it are removed.')}
              open={asking}
              onOpenChange={setAsking}
              run={async () => {
                await remove(target, agent.id || agent.name)
                onDeleted()
              }}
            />
          ) : null}
        </>
      )}
    </Sheet>
  )
}
