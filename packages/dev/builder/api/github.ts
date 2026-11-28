/**
 * GitHub, as the platform's GitHub App sees it for the signed-in person.
 *
 *   GET  /v1/provider/github/repos?q=&owner=&limit=&after=           most recently pushed first
 *   GET  /v1/provider/github/repos/{owner}/{repo}/branches?q=&limit=&after=   default first
 *   GET  /v1/provider/github/user              the person's own connection
 *   POST /v1/provider/github/user/connect      {return} → {authorizeUrl}
 *   POST /v1/provider/github/user/complete     {grant} → the connection
 *   POST /v1/provider/github/user/disconnect
 *   GET  /v1/provider/github/installations     the accounts this org has bound the App on (`added` narrows to them)
 *   POST /v1/provider/github/issues/backfill   {state, repo?} → mirror GitHub issues onto the board
 *
 * Paged by an opaque `after` cursor; `next` is the cursor for the page after
 * this one, or empty on the last. Rows are read defensively: a field the
 * platform left out is an empty value here, never `undefined` in a label.
 *
 * `connected` false with repositories is an org admin seeing the org's
 * installations; false with none is the cue to connect. GitHub returns a
 * connecting person to the page the connect named as its return (a Hanzo app
 * page; the console's /connectors otherwise) with `?complete=github&grant=<id>`,
 * and that page completes it (`complete`, and `landed` in ../back.ts).
 */
import { phrase } from '@hanzo/i18n'

import { call, query, seg, type Target } from './call.ts'
import { codebases } from './codebases.ts'
import { links, type Link } from './links.ts'

export interface Repo {
  owner: string
  name: string
  /** `owner/name`. */
  full_name: string
  private: boolean
  default_branch: string
  /** RFC 3339, or '' when GitHub did not say. */
  pushed_at: string
  /** The App installation that grants access, or 0 for the person's own grant. */
  installation_id: number
  /** The name it takes on the forge once brought there: `owner_name`, folded. */
  codebase: string
}

export interface Repos {
  repos: Repo[]
  next: string
  /** How many repositories match, across every page. */
  total: number
  /** The installations that could not be read: a short list is then not the whole one. */
  unread: string[]
  /** Whether this answer is the person's OWN GitHub. */
  connected: boolean
}

export interface Branch {
  name: string
  /** The sha it points at, or ''. */
  commit: string
  default: boolean
}

export interface Branches {
  branches: Branch[]
  next: string
  total: number
}

export interface Connection {
  /** Whether this deployment can connect anyone at all. */
  configured: boolean
  connected: boolean
  /** The GitHub login, when connected. */
  login: string
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> =>
  v && typeof v === 'object' ? (v as Record<string, unknown>) : {}

export function repo(raw: unknown): Repo {
  const r = obj(raw)
  const owner = str(r.owner)
  const name = str(r.name)
  return {
    owner,
    name,
    full_name: str(r.full_name) || (owner && name ? `${owner}/${name}` : name),
    private: r.private === true,
    default_branch: str(r.default_branch),
    pushed_at: str(r.pushed_at),
    installation_id: num(r.installation_id),
    codebase: str(r.codebase),
  }
}

export function branch(raw: unknown): Branch {
  const b = obj(raw)
  return { name: str(b.name), commit: str(b.commit), default: b.default === true }
}

export interface RepoQuery {
  q?: string
  owner?: string
  limit?: number
  after?: string
}

export async function repos(t: Target, p: RepoQuery = {}, signal?: AbortSignal): Promise<Repos> {
  const raw = obj(
    await call<unknown>(
      t,
      'GET',
      `/v1/provider/github/repos${query({ q: p.q?.trim(), owner: p.owner, limit: p.limit ?? 50, after: p.after })}`,
      undefined,
      { signal },
    ),
  )
  return {
    repos: (Array.isArray(raw.repos) ? raw.repos : []).map(repo).filter((r) => r.name),
    next: str(raw.next),
    total: num(raw.total),
    unread: (Array.isArray(raw.unread) ? raw.unread : []).filter((x): x is string => typeof x === 'string'),
    connected: raw.connected === true,
  }
}

export interface BranchQuery {
  q?: string
  limit?: number
  after?: string
}

export async function branches(
  t: Target,
  owner: string,
  name: string,
  p: BranchQuery = {},
  signal?: AbortSignal,
): Promise<Branches> {
  const raw = obj(
    await call<unknown>(
      t,
      'GET',
      `/v1/provider/github/repos/${seg(owner)}/${seg(name)}/branches${query({ q: p.q?.trim(), limit: p.limit ?? 50, after: p.after })}`,
      undefined,
      { signal },
    ),
  )
  return {
    branches: (Array.isArray(raw.branches) ? raw.branches : []).map(branch).filter((b) => b.name),
    next: str(raw.next),
    total: num(raw.total),
  }
}

function own(raw: unknown): Connection {
  const c = obj(raw)
  return { configured: c.configured === true, connected: c.connected === true, login: str(c.login) }
}

export async function connection(t: Target): Promise<Connection> {
  return own(await call<unknown>(t, 'GET', '/v1/provider/github/user'))
}

/**
 * Where to send the person to authorize the platform's GitHub App — github.com
 * only. `back` is the page GitHub's return lands on; the platform keeps it only
 * when it is a Hanzo app page.
 */
export async function connect(t: Target, back?: string): Promise<string> {
  const raw = obj(await call<unknown>(t, 'POST', '/v1/provider/github/user/connect', back ? { return: back } : undefined))
  const url = str(raw.authorizeUrl)
  if (!/^https:\/\/github\.com\//.test(url)) throw new Error(phrase('The platform did not name a GitHub address to connect at'))
  return url
}

/** Finish the connect GitHub's return handed this page as `grant`. */
export async function complete(t: Target, grant: string): Promise<Connection> {
  return own(await call<unknown>(t, 'POST', '/v1/provider/github/user/complete', { grant }))
}

export async function disconnect(t: Target): Promise<void> {
  await call<unknown>(t, 'POST', '/v1/provider/github/user/disconnect')
}

/**
 * Said on the window when this page changes the GitHub connection (a connect
 * finished, a link or an App account let go), so every surface showing it
 * reads it again: Projects' line, Settings' Integrations, and the Hanzo app's
 * Quick setup (@hanzo/app Setup), which says it too. A name the two packages
 * agree on, so neither imports the other.
 */
export const CHANGED = 'hanzo:github'

/** Say the GitHub connection changed. */
export function changed(): void {
  if (typeof window !== 'undefined') window.dispatchEvent(new Event(CHANGED))
}

/** Run `again` each time the GitHub connection changed; the answer stops it. */
export function heard(again: () => void): () => void {
  window.addEventListener(CHANGED, again)
  return () => window.removeEventListener(CHANGED, again)
}

/** A GitHub account the platform's App is installed on, as this org sees it. */
export interface Installation {
  login: string
  /** `Organization` or `User`. */
  type: string
  /** `all` or `selected` repositories. */
  grant: string
  /** This org has bound it, and GitHub still has the App installed there. */
  connected: boolean
}

/** The accounts this org has bound the App on. An org admin adds one through `/v1/provider/github/connect`. */
export async function installations(t: Target): Promise<Installation[]> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/provider/github/installations'))
  return (Array.isArray(raw.installations) ? raw.installations : [])
    .map(obj)
    .map((i) => ({ login: str(i.login), type: str(i.type), grant: str(i.grant), connected: i.connected === true }))
    .filter((i) => i.login)
}

/**
 * Whether this page reaches GitHub at all, and as whom: the person's own
 * connection first, else the accounts the org has bound the App on. An org that
 * installed the App (onboarding's Connect GitHub) has its repositories to sync
 * without the person connecting their own, so it reads as connected too.
 */
export function linked(me: Connection, installed: Installation[]): Connection {
  if (me.connected) return me
  const bound = installed.filter((i) => i.connected).map((i) => i.login)
  return bound.length ? { configured: me.configured, connected: true, login: bound.join(', @') } : me
}

/** One repository the GitHub connection grants this organization. */
export interface Grant {
  owner: string
  name: string
  /** `owner/name`, the selector the importer accepts. */
  fullName: string
  private: boolean
  branch: string
  /** The name it takes on the forge once brought there. */
  codebase: string
  /** On the forge: linked and landed, or held there under its codebase name. */
  imported: boolean
  /** `synced`, `conflict`, `pending`, `paused`, or '' while it is not linked. */
  status: string
}

export interface Grants {
  repos: Grant[]
  /** Accounts the connection could not read. */
  unread: string[]
  /** The accounts the organization added (`added`), each listed even before any of its repositories is. */
  added?: string[]
}

export interface Account {
  name: string
  count: number
  /** Named as unread and carrying no repositories. */
  blocked: boolean
}

/**
 * One row of GET /v1/provider/github/repos (apps/provider github_index.go
 * repoView): `owner`, `name`, `full_name`, `private`, `default_branch`,
 * `pushed_at`, `installation_id`, `codebase`. Whether it is on the forge is not
 * in the row — `codebase` is the name it would take, not a sign it has — so
 * `imported` is decided by `grants` from the links and the forge.
 */
export function grant(raw: unknown): Grant | null {
  const r = repo(raw)
  if (!r.name) return null
  const full = r.full_name
  const owner = r.owner || (full.includes('/') ? full.slice(0, full.indexOf('/')) : '')
  return {
    owner,
    name: r.name,
    fullName: full || (owner ? `${owner}/${r.name}` : r.name),
    private: r.private,
    branch: r.default_branch || 'main',
    codebase: r.codebase,
    imported: false,
    status: '',
  }
}

/** Accounts the grant names, plus any account that could not be read or that the organization added. */
export function accounts(g: Grants): Account[] {
  const counts = new Map<string, number>()
  for (const r of g.repos) {
    if (!r.owner) continue
    counts.set(r.owner, (counts.get(r.owner) ?? 0) + 1)
  }
  const unread = new Set(g.unread.filter(Boolean))
  const names = new Set<string>([...counts.keys(), ...unread])
  // GitHub's logins are case-blind: an added account is named once, as its repositories spell it.
  const seen = new Set([...names].map((n) => n.toLowerCase()))
  for (const n of g.added ?? []) {
    if (!n || seen.has(n.toLowerCase())) continue
    names.add(n)
    seen.add(n.toLowerCase())
  }
  return [...names]
    .sort((a, b) => a.localeCompare(b))
    .map((name) => {
      const count = counts.get(name) ?? 0
      return { name, count, blocked: unread.has(name) && count === 0 }
    })
}

/**
 * How many of the platform's pages a full read takes at most: a hundred rows
 * each, so the twenty thousand one view holds (apps/provider github_index.go
 * maxViewRepos). The listing is most-recently-pushed first, so a smaller bound
 * drops whole accounts whose newest push is older than the rest: twenty pages
 * left out a person's own account behind their organizations' 2,000 newer
 * repositories, whatever they granted it on GitHub.
 */
const PAGES = 200

/**
 * Every repository the GitHub connection grants, every page, with whether the
 * forge already has it: a link that has landed (GET /v1/sync, its `native`
 * state), or a forge repository under its codebase name (GET /v1/git/repos).
 */
export async function grants(t: Target, signal?: AbortSignal): Promise<Grants> {
  const rows: Repo[] = []
  let unread: string[] = []
  let after = ''
  for (let n = 0; n < PAGES; n++) {
    const page = await repos(t, { limit: 100, after }, signal)
    rows.push(...page.repos)
    unread = page.unread
    if (!page.next) break
    after = page.next
  }
  const [linked, held] = await Promise.all([links(t, signal).catch(() => [] as Link[]), codebases(t).catch(() => [])])
  const state = new Map(linked.map((l) => [`${l.owner}/${l.name}`.toLowerCase(), l.status]))
  const names = new Set(held.map((c) => c.name.toLowerCase()))
  return {
    repos: rows
      .map(grant)
      .filter((r): r is Grant => r !== null)
      .map((r) => {
        // A link's state, '' when the forge could not be read for it; absent with no link.
        const said = state.get(r.fullName.toLowerCase())
        const there = names.has(r.codebase.toLowerCase())
        return { ...r, imported: there || said === 'synced' || said === 'conflict' || said === 'paused', status: said || (there ? 'synced' : '') }
      }),
    unread: unread.filter(Boolean),
  }
}

/**
 * The repositories this organization can bring in: `grants`, narrowed to the
 * accounts the organization has added (`installations`, `connected`). The list
 * is the person's own GitHub, which reaches accounts another Hanzo workspace
 * added; the import goes through this organization's own, so it refuses every
 * repository of those. With the organization's accounts unreadable, `grants` as
 * it is: the import still says which it refuses.
 */
export async function added(t: Target, signal?: AbortSignal): Promise<Grants> {
  const [g, mine] = await Promise.all([grants(t, signal), installations(t).catch(() => null)])
  if (!mine) return g
  const logins = mine.filter((i) => i.connected).map((i) => i.login)
  const held = new Set(logins.map((l) => l.toLowerCase()))
  const ours = (name: string) => held.has(name.toLowerCase())
  return { repos: g.repos.filter((r) => ours(r.owner)), unread: g.unread.filter(ours), added: logins }
}

/** What an import queued: how many, and the name each takes on the forge, by `owner/name`. */
export interface Queued {
  queued: number
  codebases: Record<string, string>
}

/** Queue a mirror of the named repositories onto the forge. The names are `owner/name`. */
export async function bring(t: Target, repos: string[]): Promise<Queued> {
  const names = repos.map((n) => n.trim()).filter(Boolean)
  if (names.length === 0) throw new Error(phrase('Choose a repository first'))
  const raw = obj(await call<unknown>(t, 'POST', '/v1/provider/github/repos/import', { repos: names }))
  const said = Array.isArray(raw.repos) ? raw.repos : []
  const made = Array.isArray(raw.codebases) ? raw.codebases : []
  const codebases: Record<string, string> = {}
  said.forEach((r, i) => {
    if (typeof r === 'string' && typeof made[i] === 'string' && made[i]) codebases[r.toLowerCase()] = made[i] as string
  })
  return { queued: num(raw.queued), codebases }
}

/** What one issue sync did. */
export interface Synced {
  repos: number
  issues: number
  created: number
  updated: number
  failed: number
  /** The pass stopped at its budget; syncing again continues where it stopped. */
  truncated: boolean
}

/**
 * Mirror GitHub issues onto the board — open and closed, so a closed one reads as
 * closed here too. `repo` (`name` or `owner/name`) syncs one project; empty syncs
 * every repository the connection grants. Idempotent: a second sync updates and
 * never duplicates, and the App's webhook keeps them live afterwards.
 */
export async function syncIssues(t: Target, repo = ''): Promise<Synced> {
  const name = repo.trim()
  const raw = obj(
    await call<unknown>(t, 'POST', '/v1/provider/github/issues/backfill', name ? { state: 'all', repo: name } : { state: 'all' }),
  )
  return {
    repos: num(raw.repos),
    issues: num(raw.issues),
    created: num(raw.created),
    updated: num(raw.updated),
    failed: num(raw.failed),
    truncated: raw.truncated === true,
  }
}
