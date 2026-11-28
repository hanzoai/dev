/**
 * What the builder needs from the page that mounts it.
 *
 * The builder is ONE component with several hosts: its own page (hanzo.build,
 * which signs in through Hanzo IAM and routes with react-router) and the Hanzo
 * app's Dev section (hanzo.ai/dev and hanzo.app/dev, which already have a
 * session, a router and a rail). Everything that differs between them is here,
 * as values, so no host forks the builder and the builder imports no host.
 */
import { createContext, useContext, useMemo, type ComponentType, type ReactNode } from 'react'

import type { Cast } from './api/agents.ts'
import type { Target } from './api/call.ts'
import type { Theme } from './api/pref.ts'
import type { Local } from './local.ts'
import type { Site } from './site.ts'

export interface Person {
  name: string
  email: string
  /** An https image, or '' for the initial. */
  avatar: string
}

/**
 * A page tab's address, drawn by the host rather than in a frame: what the
 * window can show of any site, where a browser page can frame only the sites
 * that allow it. `onGo` is a move the page made itself (a link followed), and
 * `onTitle` the title it gave itself.
 */
export type Browse = ComponentType<{ url: string; title: string; onGo: (url: string) => void; onTitle?: (title: string) => void }>

/** A page as the host's browser is asked about it: the tab, and what it shows. */
export interface Shown {
  id: string
  url: string
  title: string
}

/**
 * What the host's browser can do to the page a tab shows, each the row of the
 * same name in the tab's menu. A host draws only the rows it supplies, so the
 * menu offers nothing the window cannot do; a browser page supplies none and
 * draws what a page itself can (panel.tsx).
 */
export interface Browser {
  find?: (page: Shown) => void
  print?: (page: Shown) => void
  /** Zoom the page to `percent` (100 is its own size). */
  zoom?: (page: Shown, percent: number) => void
  devices?: (page: Shown) => void
  /** The page's pixels, as an image. */
  capture?: (page: Shown) => Promise<Blob>
  /** What the reader has selected in the page. */
  selection?: (page: Shown) => Promise<string>
  passwords?: () => void
  downloads?: () => void
  history?: () => void
  clear?: () => void
  settings?: () => void
}

/**
 * What kind of spend a press is: `task-run` one run of a scheduled task now,
 * `task` a task switched on (or made on) to run by itself, `bot` a 24/7 bot
 * made, `run` a coding run started, `machine` a suspended cloud computer woken,
 * `publish` a site built and published (a template copied, a project published again).
 */
export type SpendKind = 'run' | 'task-run' | 'task' | 'bot' | 'machine' | 'publish'

/** What a press must say before it spends, already in the person's language. */
export interface Ask {
  kind: SpendKind
  /** The question: "Run Monday summary now?" */
  title: string
  /** What will start, one sentence. */
  what: string
  /** The yes button's words: the act itself ("Run now"). */
  yes: string
  /** `computer`: it starts a cloud computer. `credits`: it counts against the plan or credits and starts no computer. */
  uses: 'computer' | 'credits'
}

/**
 * A question as the host is asked it. `again` is the key a host may remember a "Don't ask again" under, or null to
 * ask every time. It is `<kind>.<uses>` (`task-run.credits`, `run.computer`), so one yes covers only the same kind of
 * spend on the same kind of thing. A host keeps nothing it is not given a key for. A 24/7 bot's is null by its TYPE:
 * each has its own budget and period, so no yes is ever remembered for one.
 */
export type Spend = (Ask & { kind: Exclude<SpendKind, 'bot'>; again: string | null }) | (Ask & { kind: 'bot'; again: null })

export interface Host {
  /** The product's name, drawn at the top left: `Hanzo Build` on hanzo.build, `Hanzo` on hanzo.ai. */
  name: string
  /** The platform origin, no trailing slash. */
  api: string
  /** The signed-in person's bearer, or null. Read at call time, so a refresh is picked up. */
  token: () => string | null
  /** The org every call is scoped to. */
  org: string | null
  /** The org's own logo, an https address, when it has chosen one; its mark is drawn from it. */
  logo?: string
  /** The organizations this person belongs to. The switcher lists these and no others. */
  memberships?: readonly string[]
  /** Stay on this page and scope every read to `org`. */
  chooseOrg?: (org: string) => void
  /** The page's theme as last chosen, for a host that can change it. */
  theme?: Theme
  /** Paint the page in `theme`. A host without it keeps its own, and Settings offers no choice. */
  chooseTheme?: (theme: Theme) => void
  /** Who is signed in, or null. */
  person: Person | null
  /** Whether the person administers `org` (its owner or an admin): they publish to main and change its money; anyone else opens a review and reads. */
  admin: boolean
  /** The address under the mount, in route.ts's grammar: '' | a session id | `<org>/<repo>` | a slug | '-/projects' | '-/issues' | … */
  path: string
  /** Move under the mount. */
  go: (path: string, how?: { replace?: boolean }) => void
  /** Where the builder links out. On hanzo.build these are addresses of this page. */
  links: {
    /** GitHub for this page: bringing repositories onto the forge. */
    github: string
    /** Customize, an address of this host. */
    customize: string
    /** Account settings, an address of this host. */
    settings: string
    /** This host's home. On hanzo.build it is the page itself. */
    home: string
  }
  /** Open a host address in this window. hanzo.build keeps the window on its own origin. */
  open: (href: string) => void
  /**
   * Take the person to top the prepaid balance up, returning to this page with the
   * model they chose (`?model=`) once paid. Another maker's models are paid from the
   * balance on any plan, so a host that offers a top-up has them selectable on Free.
   * Absent, they follow the plan as before.
   */
  topUp?: (model?: string) => void
  signIn?: () => void
  signOut?: () => void
  /** A page tab's remote address, shown by the host (Hanzo Desktop: a native webview). Absent, a browser frames what it may. */
  browse?: Browse
  /** What the host's browser does to a page, row by row in the tab's menu. */
  browser?: Browser
  /** The host's sidebar lists Settings' sections and Customize's tabs, so the pages draw neither. */
  sections?: boolean
  /** The default agents Customize › Agents offers to start from. Absent, none. */
  cast?: readonly Cast[]
  /** Every persona Customize › Agents offers to start from, read when Browse opens. Absent, none. */
  personas?: () => Promise<readonly Cast[]>
  /**
   * The folders of the computer the host runs on, and a run in one of them (local.ts): Hanzo Desktop has them, so New
   * offers This computer with Auto picking the folder used last. A browser page has no folder of its own: absent.
   */
  local?: Local
  /**
   * Opening a site, saving a copy of it for the agent, and serving its redesign live in the person's browser (site.ts):
   * Hanzo Desktop can, so New on This computer offers to redesign a site from its address. A browser page cannot: absent.
   */
  site?: Site
  /**
   * Asks the person before a press spends (spend.tsx), and answers whether they said yes. A host that gives it is the
   * only asker: the builder draws no question of its own and spends only on `true`. Absent (this package's own page),
   * the builder asks in its own dialog. A no, a throw, an answer after the asking screen has gone, or one a newer
   * question on that screen superseded spends nothing; one left unanswered blocks nothing.
   */
  confirm?: (spend: Spend) => Promise<boolean>
}

const Context = createContext<Host | null>(null)

export function HostProvider({ host, children }: { host: Host; children: ReactNode }) {
  return <Context.Provider value={host}>{children}</Context.Provider>
}

export function useHost(): Host {
  const h = useContext(Context)
  if (!h) throw new Error('The builder was rendered without a host')
  return h
}

/** The host, or null where none is mounted: a surface that may be drawn without one (the first run's card in Chat). */
export function useHosted(): Host | null {
  return useContext(Context)
}

/** The host's way of showing a page, where one is mounted and has one. The side panel is drawn without a host too (hanzo.team's rooms). */
export function useBrowse(): Browse | null {
  return useContext(Context)?.browse ?? null
}

/** The host's computer and its folders, where the host runs on one (Hanzo Desktop). */
export function useLocal(): Local | null {
  return useContext(Context)?.local ?? null
}

/** The host's way of redesigning a site, where the host runs on a computer that can (Hanzo Desktop). */
export function useSite(): Site | null {
  return useContext(Context)?.site ?? null
}

/** The host's browser, where one is mounted and has one. */
export function useBrowser(): Browser | null {
  return useContext(Context)?.browser ?? null
}

/** The call target for this host — stable while the host's api, token reader and org are. */
export function useTarget(): Target {
  const { api, token, org } = useHost()
  return useMemo(() => ({ api, token, org }), [api, token, org])
}
