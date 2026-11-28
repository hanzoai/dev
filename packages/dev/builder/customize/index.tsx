/**
 * Customize: what the agent brings to a run — the skills it knows, the
 * connectors it calls, the plugins it runs, and agents of the org's own. Each
 * tab is its own address. In each, Browse comes first and is where the page
 * opens: what there is to add, then what Hanzo builds in. Yours is what the org
 * already has, counted on its button. One search and one Add serve both.
 *
 * Skills and connectors ride into every run in the org, so adding, switching
 * and removing them is an org admin's and a member reads them; plugins and
 * agents are any member's.
 *
 * Its words are the reader's language (@hanzo/i18n `t`).
 */
import { SizableText, useMedia, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { BookOpen, Bot, Plug, Plus, Puzzle, Search, Users, X } from '@hanzogui/lucide-icons-2'
import { Button, Input } from '@hanzo/ui'
import { useCallback, useId, useState, type ReactNode } from 'react'

import { RING, RING_INSET } from '../pane.ts'
import { useHost } from '../host.tsx'
import { path, TABS, type Tab } from '../route.ts'
import { Agents } from './agents.tsx'
import { Connectors } from './connectors.tsx'
import { Plugins } from './plugins.tsx'
import { Skills } from './skills.tsx'
import { Choice, type Pane, type View } from './ui.tsx'

/** What every run in the org loads: an org admin's to add to, a member's to read. */
export const ORG_KIT: Tab[] = ['skills', 'connectors']

/**
 * Each tab's words, English and marked (`phrase`), drawn through `t`: its name,
 * its line, its add control, its search and the search's hint, and, for a tab
 * an org admin keeps (ORG_KIT), what a member is told.
 */
const LABEL: Record<Tab, { title: string; says: string; add: string; find: string; typing: string; admin?: string; icon: ReactNode }> = {
  skills: {
    title: phrase('Skills'),
    says: phrase('Know-how an agent reads before it works.'),
    add: phrase('New skill'),
    find: phrase('Search skills'),
    typing: phrase('Search skills…'),
    admin: phrase('Only an org admin can add or change skills. You can see what is on.'),
    icon: <BookOpen size={15} />,
  },
  connectors: {
    title: phrase('Connectors'),
    says: phrase('Apps and tools an agent can call.'),
    add: phrase('Add by URL'),
    find: phrase('Search connectors'),
    typing: phrase('Search connectors…'),
    admin: phrase('Only an org admin can add or change connectors. You can see what is on.'),
    icon: <Plug size={15} />,
  },
  plugins: { title: phrase('Plugins'), says: phrase('Connectors you build in TypeScript.'), add: phrase('Build plugin'), find: phrase('Search plugins'), typing: phrase('Search plugins…'), icon: <Puzzle size={15} /> },
  agents: {
    title: phrase('Agents'),
    says: phrase('Agents with their own model, instructions and budget.'),
    add: phrase('New agent'),
    find: phrase('Search agents'),
    typing: phrase('Search agents…'),
    icon: <Users size={15} />,
  },
  bots: {
    title: phrase('Bots'),
    says: phrase('Persistent 24/7 agents that run continuously on a schedule or when events fire.'),
    add: phrase('New bot'),
    find: phrase('Search bots'),
    typing: phrase('Search bots…'),
    icon: <Bot size={15} />,
  },
}

export function Customize({ tab, view: first }: { tab: Tab; view?: View }) {
  const { t } = useI18n()
  const host = useHost()
  const wide = Boolean(useMedia().sm)
  const tabId = useId()
  const [view, setView] = useState<View>(first ?? 'discover')
  const [q, setQ] = useState('')
  const [adding, setAdding] = useState(false)
  const [count, setCount] = useState(0)
  const onCount = useCallback((n: number) => setCount(n), [])
  const pick = (next: Tab) => {
    setQ('')
    setAdding(false)
    setView('discover')
    setCount(0)
    host.go(path({ kind: 'customize', tab: next }))
  }
  const pane: Pane = { view, q, adding, onAdding: setAdding, onView: setView, onCount }
  const kit = ORG_KIT.includes(tab)
  const may = Boolean(host.person) && (host.admin || !kit)
  const inlineCreator = tab === 'agents' || tab === 'bots'
  // Keep one primary white action per view: soften the toolbar button when an empty state or open creator panel owns the primary CTA.
  const quietToolbarAdd = (view === 'yours' && count === 0) || (inlineCreator && adding)

  const choice = (
    <Choice
      label={t('Show')}
      value={view}
      options={[
        { id: 'discover', label: t('Browse') },
        { id: 'yours', label: t('Yours'), count },
      ]}
      onChange={setView}
    />
  )

  const search = (
    <XStack
      items="center"
      gap="$2"
      px="$3"
      height={36}
      focusWithinStyle={{ borderColor: '$outlineColor' }}
    >
      <Search size={14} color="$soft" shrink={0} aria-hidden />
      <Input
        unstyled
        flex={1}
        minW={0}
        height="100%"
        p={0}
        fontSize="$2"
        color="$ink"
        value={q}
        onChangeText={setQ}
        placeholder={t(LABEL[tab].typing)}
        aria-label={t(LABEL[tab].find)}
        onKeyDown={(e: { key?: string }) => {
          if (e.key === 'Escape' && q) setQ('')
        }}
      />
      {q ? (
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
      ) : null}
    </XStack>
  )

  const addButton = may ? (
    <Button
      size="sm"
      variant={quietToolbarAdd ? 'outline' : 'primary'}
      shrink={0}
      aria-expanded={inlineCreator ? adding : undefined}
      onPress={() => setAdding(inlineCreator ? !adding : true)}
    >
      <Plus size={14} /> {t(LABEL[tab].add)}
    </Button>
  ) : null

  return (
    <YStack flex={1} minH={0} overflow="scroll" px="$6" py="$6" $max-md={{ px: '$4', py: '$4' }}>
      <YStack width="100%" maxW={1080} mx="auto" gap="$4">
        {/* A host whose sidebar lists the tabs heads the page with the tab itself. */}
        <YStack gap="$1">
          <SizableText render="h1" size="$6" color="$ink">
            {host.sections ? t(LABEL[tab].title) : t('Customize')}
          </SizableText>
          <SizableText size="$2" color="$soft">
            {host.sections ? t(LABEL[tab].says) : t('What your agent can use in every run.')}
          </SizableText>
        </YStack>

        <YStack gap="$3">
          {host.sections ? null : (
            <XStack
              role="tablist"
              aria-label={t('Customize')}
              gap="$4"
              borderBottomWidth={1}
              overflowX="auto"
              overflowY="hidden"
              $platform-web={{ scrollbarWidth: 'none' }}
            >
              {TABS.map((id, idx) => {
                const on = id === tab
                return (
                  <XStack
                    key={id}
                    id={`${tabId}-${id}`}
                    role="tab"
                    render="button"
                    aria-selected={on}
                    aria-controls={`${tabId}-panel`}
                    tabIndex={on ? 0 : -1}
                    onPress={() => pick(id)}
                    onKeyDown={(e: { key?: string; preventDefault?: () => void }) => {
                      let nextIdx: number | null = null
                      if (e.key === 'ArrowRight') nextIdx = (idx + 1) % TABS.length
                      else if (e.key === 'ArrowLeft') nextIdx = (idx - 1 + TABS.length) % TABS.length
                      else if (e.key === 'Home') nextIdx = 0
                      else if (e.key === 'End') nextIdx = TABS.length - 1
                      if (nextIdx === null) return
                      e.preventDefault?.()
                      const nextTab = TABS[nextIdx]
                      if (!nextTab) return
                      pick(nextTab)
                      queueMicrotask(() => document.getElementById(`${tabId}-${nextTab}`)?.focus())
                    }}
                    items="center"
                    gap="$1.5"
                    pb="$2"
                    minH={36}
                    mb={-1}
                    shrink={0}
                    borderTopWidth={0}
                    borderLeftWidth={0}
                    borderRightWidth={0}
                    borderBottomWidth={2}
                    focusVisibleStyle={RING_INSET}
                  >
                    <XStack opacity={on ? 1 : 0.7} $max-md={{ display: 'none' }}>
                      {LABEL[id].icon}
                    </XStack>
                    <SizableText size="$3" fontWeight={on ? '500' : '400'} color={on ? '$ink' : '$soft'}>
                      {t(LABEL[id].title)}
                    </SizableText>
                  </XStack>
                )
              })}
            </XStack>
          )}

          {host.sections ? null : (
            <SizableText size="$2" color="$soft">
              {t(LABEL[tab].says)}
            </SizableText>
          )}

          {wide ? (
            <XStack items="center" gap="$2.5">
              {choice}
              <YStack flex={1} minW={0}>
                {search}
              </YStack>
              {addButton}
            </XStack>
          ) : (
            <YStack gap="$2.5">
              <XStack items="center" justify="space-between" gap="$2" flexWrap="wrap">
                {choice}
                {addButton}
              </XStack>
              {search}
            </YStack>
          )}
          {host.person && kit && !host.admin && LABEL[tab].admin ? (
            <SizableText size="$1" color="$soft">
              {t(LABEL[tab].admin)}
            </SizableText>
          ) : null}

          <YStack role="tabpanel" id={`${tabId}-panel`} aria-labelledby={host.sections ? undefined : `${tabId}-${tab}`}>
            {tab === 'skills' ? (
              <Skills {...pane} />
            ) : tab === 'connectors' ? (
              <Connectors {...pane} />
            ) : tab === 'plugins' ? (
              <Plugins {...pane} />
            ) : tab === 'bots' ? (
              <Agents {...pane} mode="bots" />
            ) : (
              <Agents {...pane} mode="agents" />
            )}
          </YStack>
        </YStack>
      </YStack>
    </YStack>
  )
}
