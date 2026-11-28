/**
 * The projects an organization works on and their issues, merged.
 *
 * A project is a repository: granted by the org's GitHub connection, mirrored
 * onto the forge, or made there. The same repository arrives from several
 * sources and is one row. An issue is read from the forge and from the task
 * service's mirror of GitHub; the same issue (the same link) is one row with
 * every source that carried it named.
 *
 * Which project an issue is for is the issue's own repository — the GitHub
 * address its mirror records, or the forge repository it is filed on — and
 * never the board it sits on: a board's key is not a repository.
 *
 * Pure: every function here takes rows and answers rows, so the screens and the
 * tests share one reading of what is the same, what belongs where, and what
 * comes first.
 */

export type Source = 'github' | 'forge' | 'linear' | 'agent' | 'board'

export interface Project {
  /** `owner/name`, lower case: the one address every source is matched on. */
  key: string
  owner: string
  name: string
  description: string
  /** Null when no source said. */
  private: boolean | null
  /** The default branch, or '' when no source said. */
  branch: string
  /** RFC 3339: the latest push, update or issue any source reports, or ''. */
  activity: string
  /** Open issues, or null when the work could not be read. */
  issues: number | null
  /** Open pull requests on the forge, or null when the work could not be read. */
  pulls: number | null
  /** Granted by the org's GitHub connection. */
  linked: boolean
  /** On the forge: mirrored from GitHub, or made there. */
  mirrored: boolean
  /** The forge's own name for it, which is what a run and an environment name; '' when it is not known to be there. */
  forge: string
  /** RFC 3339: when its link last synced, or ''. */
  synced: string
  /** Its link's state: `synced`, `conflict`, `pending`, `paused`, `linked` when its forge copy could not be read; '' with no link. */
  status: string
  /** The org's project (GET /v1/project) that builds from it, by slug, or ''. */
  slug: string
  /** The name GitHub's listing says it takes on the forge once brought there, or ''. */
  codebase: string
  /** Its page on GitHub, or ''. */
  url: string
}

export interface Issue {
  /** What makes two rows the same issue: its link, else its handle on its board, else its id. */
  key: string
  /** `owner/name` of the repository it is filed on, once known; '' until then. */
  repo: string
  /** The forge repository a forge row is filed on, by name; ''. */
  forge: string
  /** What an index row says its repository is, unchecked: a run's repo, or a Linear team's key. Resolved, never used raw. */
  hint: string
  number: number
  title: string
  body: string
  state: 'open' | 'closed'
  labels: string[]
  assignees: string[]
  /** The issue's own page, https only, or ''. */
  url: string
  sources: Source[]
  /** RFC 3339, or ''. */
  updated: string
  /** RFC 3339: when it was opened, or ''. */
  created: string
  /** What the task service files it as: `issue`, `pr`, `task`, `bug`, … */
  kind: string
  /** The board's column for it as the task service names it: `backlog`, `todo`, `in_progress`, `done`, `canceled`, or its own word. */
  status: string
  /** `none` unless the board says otherwise. */
  priority: string
  /** A pull request, not an issue. */
  pull: boolean
  /** The board it was read from. Shown, filtered on, never resolved as a repository. */
  board: string
}

/** `owner/name` as one key, whatever case either source used. */
export const keyOf = (owner: string, name: string): string => (owner && name ? `${owner}/${name}`.toLowerCase() : '')

const time = (iso: string): number => {
  const t = Date.parse(iso)
  return Number.isFinite(t) ? t : 0
}

const later = (a: string, b: string): string => (time(b) > time(a) ? b : a)

/**
 * An issue's link as one key: https only, no query or fragment, no trailing
 * slash, lower case. The same issue read twice has the same key.
 */
export function canon(url: string): string {
  try {
    const u = new URL(url)
    if (u.protocol !== 'https:') return ''
    return `https://${u.host}${u.pathname.replace(/\/+$/, '')}`.toLowerCase()
  } catch {
    return ''
  }
}

/** The repository and number a GitHub or forge issue link names, or null. */
export function parse(url: string): { repo: string; number: number; pull: boolean } | null {
  const m = canon(url).match(/^https:\/\/[^/]+\/([^/]+)\/([^/]+)\/(issues|pull|pulls)\/(\d+)$/)
  return m ? { repo: `${m[1]}/${m[2]}`, number: Number(m[4]), pull: m[3] !== 'issues' } : null
}

/** Whether every word of `q` is somewhere in `text`. */
export function matches(q: string, ...text: string[]): boolean {
  const words = q.trim().toLowerCase().split(/\s+/).filter(Boolean)
  if (!words.length) return true
  const hay = text.join(' ').toLowerCase()
  return words.every((w) => hay.includes(w))
}

/** A repository GitHub's listing grants, in the fields a project takes from it. */
export interface Granted {
  owner: string
  name: string
  private: boolean
  default_branch: string
  pushed_at: string
  codebase: string
}

/** A repository the forge lists, in the fields a project takes from it. */
export interface Held {
  org: string
  name: string
  description: string
  branch: string
  public: boolean
  updated: string
}

/** A repository linked from GitHub (GET /v1/sync), in the fields a project takes from it. */
export interface Linked {
  owner: string
  name: string
  forge: string
  synced: string
  status: string
  branch: string
}

/** One of the org's projects (GET /v1/project) that builds from a repository. */
export interface Site {
  slug: string
  /** Its clone address. */
  repo: string
  branch: string
  /** RFC 3339, or ''. */
  updated: string
}

/** The account and repository an https clone address names, and whether the host is GitHub. */
export function addressOf(url: string): { owner: string; name: string; github: boolean } | null {
  try {
    const u = new URL(url.trim())
    if (u.protocol !== 'https:') return null
    const parts = u.pathname.replace(/\.git$/, '').split('/').filter(Boolean)
    if (parts.length < 2) return null
    const github = u.hostname.toLowerCase() === 'github.com'
    if (github && parts.length !== 2) return null
    return { owner: parts[parts.length - 2], name: parts[parts.length - 1], github }
  } catch {
    return null
  }
}

const blank = (owner: string, name: string): Project => ({
  key: keyOf(owner, name),
  owner,
  name,
  description: '',
  private: null,
  branch: '',
  activity: '',
  issues: null,
  pulls: null,
  linked: false,
  mirrored: false,
  forge: '',
  synced: '',
  status: '',
  slug: '',
  codebase: '',
  url: '',
})

/**
 * The project an issue is for, or null. Its GitHub address first, then the
 * forge repository it is filed on, then what an index row says — matched
 * against the org's own projects, an `owner/name` exactly, a bare name only when
 * one project answers to it. A board's key is never consulted.
 */
export function place(i: Pick<Issue, 'repo' | 'forge' | 'hint'>, list: Project[]): Project | null {
  if (i.repo) return list.find((p) => p.key === i.repo.toLowerCase()) ?? null
  if (i.forge) {
    const f = i.forge.toLowerCase()
    return list.find((p) => p.forge.toLowerCase() === f) ?? list.find((p) => p.codebase === f) ?? null
  }
  const hint = i.hint
    .trim()
    .toLowerCase()
    .replace(/^(https?:\/\/)?github\.com\//, '')
    .replace(/\.git$/, '')
  if (!hint) return null
  if (hint.includes('/')) return list.find((p) => p.key === hint) ?? null
  const named = list.filter((p) => p.forge.toLowerCase() === hint || p.codebase === hint || p.name.toLowerCase() === hint)
  return named.length === 1 ? named[0] : null
}

/** Whether a clone address is the forge's own. */
export function forgeHost(url: string): boolean {
  try {
    const u = new URL(url)
    return u.protocol === 'https:' && (u.hostname.toLowerCase() === 'git.hanzo.ai' || u.pathname.startsWith('/v1/git/'))
  } catch {
    return false
  }
}

/** What the org's projects are assembled from. */
export interface Parts {
  /** GitHub's grants to the org's connection: empty for anyone it does not answer. */
  granted?: Granted[]
  /** The repositories linked from GitHub, each with its forge copy. */
  linked?: Linked[]
  /** The forge's repositories. */
  held?: Held[]
  /** The org's projects that build from a repository. */
  sites?: Site[]
  /** Every issue and pull request, or null when they were not read. */
  work?: Issue[] | null
}

/**
 * Every project the org works on, one row per repository: the repositories it
 * linked from GitHub, GitHub's grants, the forge's repositories and the ones its
 * issues are filed on, and the org's projects that build from one — each with
 * its open work counted from `work` (null counts when the work was not read).
 *
 * A forge repository is the GitHub one it copies when a link targets it, when
 * GitHub names it as that repository's `codebase`, or when it sits in a forge
 * namespace named like the GitHub owner (an issue's link says which namespace).
 */
export function assemble({ granted = [], linked = [], held = [], sites = [], work = null }: Parts): Project[] {
  const out = new Map<string, Project>()
  // Each project by its forge name and by its codebase name, kept as it is put, so a forge row finds its own in one look.
  const byForge = new Map<string, string>()
  const byCodebase = new Map<string, string>()
  const put = (p: Project) => {
    if (!p.key) return
    out.set(p.key, p)
    if (p.forge) byForge.set(p.forge.toLowerCase(), p.key)
    if (p.codebase) byCodebase.set(p.codebase, p.key)
  }
  const at = (owner: string, name: string) => out.get(keyOf(owner, name)) ?? blank(owner, name)
  for (const l of linked) {
    const p = at(l.owner, l.name)
    put({
      ...p,
      branch: p.branch || l.branch,
      activity: later(p.activity, l.synced),
      linked: true,
      mirrored: p.mirrored || (l.forge !== '' && l.status !== 'pending'),
      forge: p.forge || (l.status === 'pending' ? '' : l.forge),
      synced: later(p.synced, l.synced),
      // A link whose forge copy could not be read says only that it is linked.
      status: l.status || 'linked',
      url: `https://github.com/${l.owner}/${l.name}`,
    })
  }
  for (const g of granted) {
    const p = at(g.owner, g.name)
    put({
      ...p,
      private: g.private,
      branch: p.branch || g.default_branch,
      activity: later(p.activity, g.pushed_at),
      linked: true,
      codebase: g.codebase.toLowerCase(),
      url: `https://github.com/${g.owner}/${g.name}`,
    })
  }
  const all = () => [...out.values()]
  // Forge namespaces, as the forge's own issue links spell them.
  const spaces = new Set<string>()
  for (const i of work ?? []) {
    const where = i.forge ? parse(i.url) : null
    if (where) spaces.add(where.repo.split('/')[0].toLowerCase())
  }
  const named = (key: string | undefined) => (key ? out.get(key) : undefined)
  const onForge = (name: string, owner: string): Project => {
    const n = name.toLowerCase()
    return (
      named(byForge.get(n)) ??
      named(byCodebase.get(n)) ??
      [...spaces].map((space) => out.get(keyOf(space, name))).find((p) => p !== undefined) ??
      at(owner, name)
    )
  }
  for (const h of held) {
    const p = onForge(h.name, h.org)
    put({
      ...p,
      description: p.description || h.description,
      // A linked repository's visibility is GitHub's own, never its forge copy's: unknown until GitHub says.
      private: p.linked ? p.private : (p.private ?? !h.public),
      branch: p.branch || h.branch,
      activity: later(p.activity, h.updated),
      mirrored: true,
      // A link's copy keeps its own name: a forge repository of the same address is not it.
      forge: p.forge || h.name,
    })
  }
  for (const i of work ?? []) {
    if (!i.forge) continue
    const where = parse(i.url)
    const p = onForge(i.forge, where ? where.repo.split('/')[0] : '')
    put({ ...p, mirrored: true, forge: p.forge || i.forge })
  }
  for (const site of sites) {
    const where = addressOf(site.repo)
    // GitHub's, or the forge's own (git.hanzo.ai, or the platform's /v1/git); a site built from anywhere else is not a project here.
    if (!where || (!where.github && !forgeHost(site.repo))) continue
    const p = where.github ? at(where.owner, where.name) : onForge(where.name, where.owner)
    put({
      ...p,
      branch: p.branch || site.branch,
      activity: later(p.activity, site.updated),
      linked: p.linked || where.github,
      mirrored: p.mirrored || !where.github,
      forge: p.forge || (where.github ? '' : where.name),
      slug: p.slug || site.slug,
      url: p.url || (where.github ? `https://github.com/${where.owner}/${where.name}` : ''),
    })
  }
  if (work) {
    const counted = new Map<string, { issues: number; pulls: number; last: string }>()
    const list = all()
    for (const i of work) {
      const p = place(i, list)
      if (!p) continue
      const c = counted.get(p.key) ?? { issues: 0, pulls: 0, last: '' }
      if (i.state === 'open' && i.pull) c.pulls++
      else if (i.state === 'open') c.issues++
      c.last = later(c.last, i.updated)
      counted.set(p.key, c)
    }
    for (const p of list) {
      const c = counted.get(p.key)
      put({ ...p, issues: c?.issues ?? 0, pulls: c?.pulls ?? 0, activity: later(p.activity, c?.last ?? '') })
    }
  }
  return all()
}

/** Each issue with the project it is for named as its `repo`, where one answers. */
export function attach(list: Issue[], projects: Project[]): Issue[] {
  return list.map((i) => {
    const p = place(i, projects)
    return p ? { ...i, repo: `${p.owner}/${p.name}` } : i
  })
}

/** Most recent activity first; the unknown last; the address breaks a tie, so the order never shuffles. */
export function byActivity(a: Project, b: Project): number {
  return time(b.activity) - time(a.activity) || a.key.localeCompare(b.key)
}

export interface ProjectFilter {
  q?: string
  /** One owner, or '' for every one. */
  owner?: string
  /** Only those with an open issue or pull request. */
  open?: boolean
}

export function pickProjects(list: Project[], f: ProjectFilter = {}): Project[] {
  return list
    .filter((p) => !f.owner || p.owner.toLowerCase() === f.owner.toLowerCase())
    .filter((p) => !f.open || (p.issues ?? 0) + (p.pulls ?? 0) > 0)
    .filter((p) => matches(f.q ?? '', `${p.owner}/${p.name}`, p.description))
    .sort(byActivity)
}

/** The owners named, once each whatever the case, in name order. */
export function owners(...names: string[][]): string[] {
  const seen = new Map<string, string>()
  for (const n of names.flat()) if (n && !seen.has(n.toLowerCase())) seen.set(n.toLowerCase(), n)
  return [...seen.values()].sort((a, b) => a.localeCompare(b))
}

const union = (a: string[], b: string[]): string[] => {
  const out = [...a]
  const have = new Set(a.map((x) => x.toLowerCase()))
  for (const x of b) {
    if (have.has(x.toLowerCase())) continue
    have.add(x.toLowerCase())
    out.push(x)
  }
  return out
}

/**
 * One row per issue. The same issue read from several sources is the row the
 * most recently updated one describes, with every source named and every label
 * and assignee any of them carries.
 */
export function issues(...lists: Issue[][]): Issue[] {
  const out = new Map<string, Issue>()
  for (const i of lists.flat()) {
    if (!i.key) continue
    const had = out.get(i.key)
    if (!had) {
      out.set(i.key, { ...i, sources: [...new Set(i.sources)] })
      continue
    }
    const [lead, rest] = time(i.updated) > time(had.updated) ? [i, had] : [had, i]
    out.set(i.key, {
      ...lead,
      repo: lead.repo || rest.repo,
      forge: lead.forge || rest.forge,
      hint: lead.hint || rest.hint,
      number: lead.number || rest.number,
      title: lead.title || rest.title,
      body: lead.body || rest.body,
      url: lead.url || rest.url,
      board: lead.board || rest.board,
      labels: union(lead.labels, rest.labels),
      assignees: union(lead.assignees, rest.assignees),
      sources: [...new Set([...had.sources, ...i.sources])],
    })
  }
  return [...out.values()]
}

/** Every label and assignee the list carries, for its filters, in name order. */
export function facets(list: Issue[]): { labels: string[]; assignees: string[] } {
  const order = (a: string, b: string) => a.localeCompare(b)
  return {
    labels: list.reduce<string[]>((acc, i) => union(acc, i.labels), []).sort(order),
    assignees: list.reduce<string[]>((acc, i) => union(acc, i.assignees), []).sort(order),
  }
}

/**
 * Where a run on issue `i` works, on project `p`: where the issue lives, so the
 * run's pull request is opened beside it. A GitHub issue's run works on GitHub
 * (a linked project's forge copy is the forge's own repository, and a pull
 * request there would not reach GitHub's issue); a forge issue's on the forge.
 * Any other issue (a Linear or a board's row) works wherever the project can be
 * worked on: its forge copy, else GitHub. Null when the project is neither.
 */
export function home(i: Pick<Issue, 'sources'>, p: Pick<Project, 'linked' | 'forge'>): 'github' | 'forge' | null {
  if (i.sources.includes('github') && p.linked) return 'github'
  if (i.sources.includes('forge') && p.forge) return 'forge'
  return p.forge ? 'forge' : p.linked ? 'github' : null
}

/**
 * The number a run on `p` closes for issue `i`, or 0: the issue's own number,
 * when the run works where the issue lives. A board's or Linear's number closes
 * nothing, and neither does a GitHub issue run on the forge, or the reverse.
 */
export function closes(i: Pick<Issue, 'sources' | 'number'>, p: Pick<Project, 'linked' | 'forge'>): number {
  if (i.number <= 0) return 0
  const at = home(i, p)
  if (at === 'github' && i.sources.includes('github')) return i.number
  if (at === 'forge' && i.sources.includes('forge')) return i.number
  return 0
}

/** How a person refers to an issue: `#N` where N is the repository's own number, else the board's handle. */
export function handle(i: Pick<Issue, 'sources' | 'number' | 'board'>): string {
  if (!i.number) return ''
  if (i.sources.includes('github') || i.sources.includes('forge')) return `#${i.number}`
  return i.board ? `${i.board}-${i.number}` : ''
}

/**
 * What New's composer holds for an issue: its title and handle, its link, and
 * its body — the ask a person would write. `#N` only when the run closes N; any
 * other number is the board's (`LINEAR-5`), so the agent cannot mistake it for
 * an issue on the repository it works on.
 */
export function ask(i: Pick<Issue, 'title' | 'number' | 'url' | 'body' | 'board' | 'sources'>, closing = 0): string {
  const tag = closing ? `#${closing}` : i.board && i.number ? `${i.board}-${i.number}` : ''
  const head = tag ? `${i.title} (${tag})` : i.title
  const body = i.body.trim()
  return [head, i.url, body ? `\n${body}` : ''].filter(Boolean).join('\n')
}
