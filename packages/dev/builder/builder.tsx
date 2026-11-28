/**
 * The builder: the rail, and whichever pane the address names.
 *
 *   /            New — the empty state, a composer over the codebase it works on
 *   /sess_…      one run, live
 *   /-/automations scheduled tasks and what ran; /-/automations/<id> one, to edit and run
 *   /-/projects  the org's repositories on the forge: a project is a repository
 *   /-/sync      link repositories from GitHub onto the forge
 *   /-/issues    the forge's issues
 *   /-/artifacts the Library: everything you made, in Chat, Dev and Studio
 *   /-/templates the public starters
 *   /-/customize skills, connectors, plugins and agents
 *   /<org>/<repo> a repository's workspace, in this same window
 *   /<slug>      a deployed site's workspace
 *
 * The rail is the sessions rail every Hanzo surface shares: New, every one of
 * the builder's places (nav.tsx), the org's coding runs newest first with a
 * live status dot, and at the foot the offer of Hanzo in Slack and the account,
 * whose menu opens Settings, Usage, the plans, help and logging out (foot.tsx).
 * Its collapse is an explicit toggle kept per browser.
 *
 * A workspace takes the whole window — it has its own chat column, bar and
 * panes — and its mark leads back here.
 *
 * A host with a rail of its own mounts `<Builder rail={false}>` and draws the
 * builder's places (`nav`, nav.tsx) and runs in that rail (`useSessions`, or
 * `DevSection`, section.tsx): one left column, never two.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Menu, Search } from '@hanzogui/lucide-icons-2'
import { Button } from '@hanzo/ui'
import { SessionRail, type RailSession } from '@hanzo/ui/chat'
import { HanzoMark } from '@hanzo/ui/product'
import { useEffect, useMemo, useState } from 'react'

import { safe } from './api/projects.ts'
import { Profile } from './customize/agents.tsx'
import { Customize } from './customize/index.tsx'
import { Editor } from './automations.tsx'
import { useKept, useRecents } from './data.ts'
import { Forge } from './forge.tsx'
import { HostProvider, useHost, useTarget, type Host } from './host.tsx'
import { Landing } from './landing.tsx'
import { nav, NEW } from './nav.tsx'
import { Look, PrefsProvider } from './prefs.tsx'
import { Project } from './project.tsx'
import { Plans } from './plans.tsx'
import { path, route } from './route.ts'
import { Run } from './run.tsx'
import { dot } from './section.tsx'
import { Library } from './library.tsx'
import { Templates } from './shelf.tsx'
import { Find } from './find.tsx'
import { Slack, useWho, Who } from './foot.tsx'
import { Settings } from './settings/index.tsx'

type Order = 'newest' | 'running'

function Shell() {
  const { t: say } = useI18n()
  const host = useHost()
  const t = useTarget()
  const r = route(host.path)
  const recents = useRecents(t, Boolean(host.person))
  const [collapsed, setCollapsed] = useKept('hanzo.build.rail', false)
  const [drawer, setDrawer] = useState(false)
  const [order, setOrder] = useState<Order>('newest')
  const [finding, setFinding] = useState(false)
  const who = useWho()
  const [slack, setSlack] = useKept('hanzo.build.slack', false)

  const go = (p: string) => {
    setDrawer(false)
    host.go(p)
  }

  const rows: RailSession[] = useMemo(() => {
    const list = recents.value.map((s) => ({ id: s.id, title: s.title || say('Untitled session'), status: dot(s) }))
    if (order === 'running') list.sort((a, b) => Number(b.status === 'running') - Number(a.status === 'running'))
    return list
  }, [recents.value, order, say])

  // The top left, as on claude.ai: the builder's name, which leads to New, and under
  // it the search that finds a session. The organization is the account's, so it is
  // switched from the account menu at the foot. Beside the rail from md up, beside
  // the drawer's button below it — one place at a time.
  const name = <Name onPress={() => go('')} />
  // Every place in view, none behind a More; the rail's own New starts a session.
  const links = nav(host, go, say)
    .flatMap((g) => g.links)
    .filter((l) => l.id !== NEW)

  return (
    <XStack flex={1} minH={0} minW={0} bg="$background">
      <YStack position="relative" shrink={0}>
      <SessionRail
        onNew={() => go('')}
        newLabel={say('New session')}
        searchLabel={say('Search sessions')}
        fresh={r.kind === 'new'}
        links={links}
        recents={rows}
        active={r.kind === 'run' ? r.id : null}
        onOpen={(id) => go(id)}
        onSort={() => setOrder(order === 'newest' ? 'running' : 'newest')}
        sortLabel={order === 'newest' ? say('Show running first') : say('Show newest first')}
        empty={
          <SizableText size="$1" color="$soft" px="$2">
            {!host.person ? say('Sign in to see your sessions.') : recents.error ? say(recents.error.message) : recents.loading ? say('Reading…') : say('No sessions yet.')}
          </SizableText>
        }
        notice={
          host.person && !slack ? (
            <Slack onOpen={() => go(path({ kind: 'settings', section: 'integrations' }))} onDismiss={() => setSlack(true)} />
          ) : undefined
        }
        account={
          host.person
            ? {
                // Who, and in which organization, as claude.ai says who and on which plan.
                name: [host.person.name || host.person.email, host.org].filter(Boolean).join(' · '),
                onPress: () => {
                  setDrawer(false)
                  who.toggle()
                },
              }
            : { name: say('Sign in'), onPress: () => host.signIn?.() }
        }
        collapsed={collapsed}
        onCollapse={setCollapsed}
        mark={<Brand org={host.org} logo={host.logo} />}
        pt={collapsed ? undefined : 92}
        open={drawer}
        onOpenChange={setDrawer}
        label={say('Sessions')}
      />
      {collapsed ? null : (
        <YStack position="absolute" t={10} l={8} r={8} z={2} gap="$2.5" $max-md={{ display: 'none' }}>
          <XStack px="$2.5">{name}</XStack>
          <XStack
            render="button"
            aria-label={say('Search sessions')}
            onPress={() => setFinding(true)}
            items="center"
            gap="$2"
            px="$2.5"
            height={34}
            rounded="$3"
            borderWidth={1}
            borderColor="$borderColor"
            bg="$panel"
            hoverStyle={{ bg: '$hover' }}
          >
            <Search size={15} color="$soft" />
            <SizableText size="$2" color="$soft">
              {say('Search')}
            </SizableText>
          </XStack>
        </YStack>
      )}
      {host.person ? <Who open={who.open} onOpenChange={who.onOpenChange} narrow={collapsed} /> : null}
      </YStack>
      <YStack flex={1} minW={0} minH={0} position="relative">
        {/* Below md the rail is a drawer; this is the one control that opens it. */}
        {/* Its own row, in flow: laid over the pane it covered the heading's mark. */}
        <XStack height={48} px="$2" gap="$2" items="center" shrink={0} $md={{ display: 'none' }}>
          <Button variant="ghost" size="icon-sm" minW={40} minH={40} onPress={() => setDrawer(true)} aria-label={say('Open sessions')}>
            <Menu size={18} />
          </Button>
          {name}
          <XStack flex={1} />
          <Button variant="ghost" size="icon-sm" minW={40} minH={40} onPress={() => setFinding(true)} aria-label={say('Search sessions')}>
            <Search size={18} />
          </Button>
        </XStack>
        <Pane onStarted={() => recents.reload()} />
      </YStack>
      <Find open={finding} onOpenChange={setFinding} recents={rows} onOpen={(id) => { setFinding(false); go(id) }} />
    </XStack>
  )
}

/** The wordmark: the product's name, as its host calls it, which leads to New. */
function Name({ onPress }: { onPress: () => void }) {
  const { name } = useHost()
  return (
    <XStack render="button" aria-label={name} items="center" onPress={onPress}>
      <SizableText size="$5" fontWeight="600" color="$ink" numberOfLines={1}>
        {name}
      </SizableText>
    </XStack>
  )
}

/** Whichever pane the address names, beside a rail — the builder's or the host's. */
function Pane({ onStarted }: { onStarted?: (id: string) => void }) {
  const host = useHost()
  const r = route(host.path)
  if (r.kind === 'run') return <Run key={r.id} id={r.id} />
  if (r.kind === 'settings') return <Settings section={r.section} />
  if (r.kind === 'customize') return <Customize key={r.tab} tab={r.tab} />
  if (r.kind === 'agent') return <Profile key={r.name} name={r.name} />
  if (r.kind === 'automation') return <Editor key={r.id} id={r.id} />
  if (r.kind === 'screen') {
    if (r.screen === 'artifacts') return <Library />
    if (r.screen === 'templates') return <Templates />
    if (r.screen === 'plans') return <Plans />
    return <Forge key={r.screen} screen={r.screen} />
  }
  return (
    <Landing
      onStarted={(id) => {
        onStarted?.(id)
        host.go(id)
      }}
    />
  )
}

/** The org's mark: the logo it chose; else the Hanzo mark for Hanzo or a personal account, and an organization's initial. */
function Brand({ org, logo }: { org: string | null; logo?: string }) {
  const chosen = logo ? safe(logo) : ''
  if (chosen) return <img src={chosen} alt="" width={18} height={18} style={{ borderRadius: 4, objectFit: 'cover' }} />
  if (!org || org === 'hanzo') return <HanzoMark size={18} />
  const letter = org.charAt(0).toUpperCase()
  return (
    <XStack width={18} height={18} rounded="$1" items="center" justify="center" bg="$raised" aria-hidden>
      <SizableText size="$1" fontWeight="600" color="$ink">
        {letter}
      </SizableText>
    </XStack>
  )
}

function Screens({ rail }: { rail: boolean }) {
  const host = useHost()
  const r = route(host.path)
  if (r.kind === 'project') return <Project key={r.slug} at={{ slug: r.slug }} />
  if (r.kind === 'repo') return <Project key={`${r.org}/${r.name}`} at={{ org: r.org, name: r.name }} />
  if (!rail) return <Pane />
  return <Shell />
}

/**
 * The builder. Mount it under a gui root (`<Hanzo>`), in a box with a height.
 *
 * `rail={false}` leaves the left column to the host, which lists the builder's
 * places and runs in its own rail with `DevSection`, and the ground to it too:
 * the builder is then a pane of the host's, and paints no background of its own.
 */
export function Builder({ host, rail = true }: { host: Host; rail?: boolean }) {
  return (
    <HostProvider host={host}>
      <PrefsProvider>
        <Look />
        <YStack flex={1} minH={0} minW={0} height="100%" bg={rail ? '$background' : undefined}>
          <Screens rail={rail} />
        </YStack>
      </PrefsProvider>
    </HostProvider>
  )
}
