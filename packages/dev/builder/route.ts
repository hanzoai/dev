/**
 * The builder's addresses, under whatever path the host mounts it at.
 *
 *   ''              the empty state: a new run
 *   sess_<32 hex>   one run
 *   -/<screen>      a builder screen
 *   -/artifacts/<kind>      the Library narrowed to one kind (image, video, document, …)
 *   -/artifacts/new         the Library, making something new from a gallery template
 *   -/automations/<id>      one automation's editor and runs; `new` writes one
 *   -/settings[/<section>]  a section of Settings
 *   -/customize[/<tab>]     agents, skills, connectors and plugins: what the agent brings to a run
 *   -/customize/agents/<name>  one agent's profile
 *   -/plans                 the plans an organization can be on
 *   <org>/<repo>    a repository's workspace: a project is a repository on the forge
 *   <slug>          a deployed site's workspace (Artifacts)
 *
 * Unambiguous by construction: a project slug is `^[a-z0-9]([a-z0-9-]{0,38}[a-z0-9])?$`,
 * so it can hold neither the `_` a session id carries nor start with `-`, and a
 * repository is the one address with two segments, each starting with a letter
 * or a digit. Anything else is not an address here and reads as the empty state.
 *
 * A host whose address bar shows real paths writes a route with `href` and
 * reads one back with `parse` (hanzo.ai/dev/<href>):
 *
 *   ''                            New
 *   /run/<id>                     one run
 *   /projects/<org>/<repo>        a repository's workspace
 *   /projects/<slug>              a deployed site's workspace
 *   /machines · /environments     the rail's own words for -/settings/machines and -/settings/environments
 *   /<rest>                       every other address, its `-/` dropped: /projects, /customize/connectors, /settings/usage
 */
export type Screen = 'artifacts' | 'templates' | 'projects' | 'issues' | 'automations' | 'sync' | 'mcp' | 'plans'

/** Settings, one section each. Every setting the builder has lives on this page, none elsewhere. */
export const SECTIONS = [
  'general',
  'account',
  'privacy',
  'billing',
  'usage',
  'capabilities',
  'memory',
  'code',
  'environments',
  'machines',
  'keys',
  'members',
  'integrations',
  'notifications',
] as const
export type Section = (typeof SECTIONS)[number]

/** Customize, one tab each: what the agent brings to a run, yours and to discover. Agents first, then Bots. */
export const TABS = ['agents', 'bots', 'skills', 'connectors', 'plugins'] as const
export type Tab = (typeof TABS)[number]

export type Route =
  | { kind: 'new' }
  | { kind: 'run'; id: string }
  | { kind: 'screen'; screen: Screen; of?: Of }
  | { kind: 'settings'; section: Section }
  | { kind: 'customize'; tab: Tab }
  | { kind: 'agent'; name: string }
  | { kind: 'project'; slug: string }
  | { kind: 'repo'; org: string; name: string }
  | { kind: 'automation'; id: string }

/** What the Library's address names: a kind it narrows to (api/library.ts KIND_IDS, plus `sites`), or `new`, its template picker. */
export const OF = ['image', 'video', 'audio', 'document', 'slides', 'type', 'model', 'code', 'file', 'sites', 'new'] as const
export type Of = (typeof OF)[number]

export const SESSION = /^sess_[0-9a-f]{32}$/
export const SLUG = /^[a-z0-9]([a-z0-9-]{0,38}[a-z0-9])?$/
/** An organization or a repository name on the forge (api/codebases.ts `NAME`). */
export const NAME = /^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$/
/** An automation's id, or `new` for one not yet written. */
export const AUTOMATION = /^(flow_[0-9a-f]{32}|new)$/
const SCREENS: readonly Screen[] = ['artifacts', 'templates', 'projects', 'issues', 'automations', 'sync', 'mcp', 'plans']

export function route(path: string): Route {
  const p = path.replace(/^\/+|\/+$/g, '')
  if (SESSION.test(p)) return { kind: 'run', id: p }
  if (p === '-/settings' || p.startsWith('-/settings/')) {
    const section = (p.slice('-/settings/'.length) || 'general') as Section
    return SECTIONS.includes(section) ? { kind: 'settings', section } : { kind: 'new' }
  }
  if (p.startsWith('-/customize/agents/')) {
    const name = p.slice('-/customize/agents/'.length)
    return NAME.test(name) ? { kind: 'agent', name } : { kind: 'new' }
  }
  if (p === '-/customize' || p.startsWith('-/customize/')) {
    const tab = (p.slice('-/customize/'.length) || 'agents') as Tab
    return TABS.includes(tab) ? { kind: 'customize', tab } : { kind: 'new' }
  }
  if (p.startsWith('-/automations/')) {
    const id = p.slice('-/automations/'.length)
    return AUTOMATION.test(id) ? { kind: 'automation', id } : { kind: 'new' }
  }
  if (p.startsWith('-/artifacts/')) {
    const of = p.slice('-/artifacts/'.length) as Of
    return OF.includes(of) ? { kind: 'screen', screen: 'artifacts', of } : { kind: 'new' }
  }
  if (p.startsWith('-/')) {
    const s = p.slice(2) as Screen
    return SCREENS.includes(s) ? { kind: 'screen', screen: s } : { kind: 'new' }
  }
  if (SLUG.test(p)) return { kind: 'project', slug: p }
  const [org, name, ...rest] = p.split('/')
  if (!rest.length && org && name && NAME.test(org) && NAME.test(name)) return { kind: 'repo', org, name }
  return { kind: 'new' }
}

/** Every route that names no record, for a host that writes a page per address ahead of time. */
export const PLACES: readonly Route[] = [
  { kind: 'new' },
  { kind: 'automation', id: 'new' },
  ...SCREENS.map((screen): Route => ({ kind: 'screen', screen })),
  ...SECTIONS.map((section): Route => ({ kind: 'settings', section })),
  ...TABS.map((tab): Route => ({ kind: 'customize', tab })),
]

/** The places a real path names in the rail's words rather than the builder's: each rail row is a path of its own. */
const WORDS = new Map([
  ['-/settings/machines', 'machines'],
  ['-/settings/environments', 'environments'],
])
const NAMES = new Map([...WORDS].map(([p, word]) => [word, p]))

/** A route as a real path under the host's mount: '' for New, `/run/<id>`, `/projects/<org>/<repo>`, `/projects/<slug>`, else its path without `-/`. */
export function href(r: Route): string {
  if (r.kind === 'run') return `/run/${r.id}`
  if (r.kind === 'repo') return `/projects/${r.org}/${r.name}`
  if (r.kind === 'project') return `/projects/${r.slug}`
  const p = path(r)
  return p && `/${WORDS.get(p) ?? p.slice(2)}`
}

/** The route a real path under the mount names — the inverse of `href`. A query or fragment is not part of it. */
export function parse(address: string): Route {
  const p = address.split(/[?#]/)[0]!.replace(/^\/+|\/+$/g, '')
  const [head, one, two, ...rest] = p.split('/')
  if (head === 'run' || head === 'session' || head === 'sessions') return one && !two && SESSION.test(one) ? { kind: 'run', id: one } : { kind: 'new' }
  if (head === 'projects' && one) {
    if (rest.length) return { kind: 'new' }
    if (two) return NAME.test(one) && NAME.test(two) ? { kind: 'repo', org: one, name: two } : { kind: 'new' }
    return SLUG.test(one) ? { kind: 'project', slug: one } : { kind: 'new' }
  }
  return p ? route(NAMES.get(p) ?? `-/${p}`) : { kind: 'new' }
}

/** The path for a route — the inverse of `route`. */
export function path(r: Route): string {
  switch (r.kind) {
    case 'new':
      return ''
    case 'run':
      return r.id
    case 'screen':
      return r.of ? `-/${r.screen}/${r.of}` : `-/${r.screen}`
    case 'settings':
      return r.section === 'general' ? '-/settings' : `-/settings/${r.section}`
    case 'customize':
      return r.tab === 'agents' ? '-/customize' : `-/customize/${r.tab}`
    case 'agent':
      return `-/customize/agents/${r.name}`
    case 'project':
      return r.slug
    case 'repo':
      return `${r.org}/${r.name}`
    case 'automation':
      return `-/automations/${r.id}`
  }
}
