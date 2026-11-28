/**
 * The work an organization does: its projects, their issues, and its boards.
 *
 *   GET /v1/provider/github/repos?q=&owner=&limit=&after=   what the GitHub connection grants, most recently pushed first
 *   GET /v1/provider/github/installations                    the GitHub accounts it grants them from
 *   GET /v1/git/repos                                        the forge's repositories
 *   GET /v1/task/board                                       every issue and pull request: the forge's, and the task index's
 *   GET /v1/task/projects                                    the boards
 *   GET /v1/task/projects/{key}/issues                       one board's
 *   POST /v1/git/workspace                                   set the org's code workspace up for the caller
 *
 * In a workspace the platform made for the org, the board is read as the
 * person's own forge account, seated there by POST /v1/git/workspace (cloud
 * apps/git workspace.go). Nothing else seats them, so a board refused for want of
 * a seat — or of the org's namespace — opens the workspace and is read again
 * (`seated`).
 *
 * A project is a repository, so the list is those sources merged (merge.ts) and
 * not a second copy of them. An issue row from the board is read for what it
 * is: a forge issue names its forge repository and links to its page; a GitHub
 * issue the index mirrors names its address only in `extRef`
 * (`github:<owner>/<name>#<n>`), and its `number` there is the board's, not
 * GitHub's; any other index row's `repo` is what its writer said — a run's
 * repository, or a Linear team's key — and is resolved, never used raw.
 *
 * Creating, renaming and deleting a board are forge operations. This surface
 * reads the work and opens a run against it.
 */
import { call, Refusal, seg, type Target } from './call.ts'
import { codebases } from './codebases.ts'
import { installations, repos as granted, type Repo } from './github.ts'
import { assemble, canon, issues as merged, owners, type Held, type Issue, type Site, type Source } from '../merge.ts'
import { links, type Link } from './links.ts'
import { projects as built } from './projects.ts'

export interface Board {
  id: string
  key: string
  name: string
  description: string
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})

function rows(raw: unknown): unknown[] {
  if (Array.isArray(raw)) return raw
  const o = obj(raw)
  if (Array.isArray(o.data)) return o.data
  if (Array.isArray(o.issues)) return o.issues
  return []
}

export function board(raw: unknown): Board | null {
  const o = obj(raw)
  const key = str(o.key)
  if (!key) return null
  return { id: str(o.id) || key, key, name: str(o.name) || key, description: str(o.description) }
}

/** Unix seconds as RFC 3339, or ''. */
const instant = (s: number): string => (s > 0 ? new Date(s * 1000).toISOString() : '')

const FINISHED = new Set(['done', 'canceled'])
const GITHUB = /^github:([A-Za-z0-9][A-Za-z0-9-]*)\/([A-Za-z0-9._-]+)#(\d+)$/

/** Where an index row came from, by the reference its writer keyed it with. */
function origin(ext: string): Source {
  if (ext.startsWith('github:')) return 'github'
  if (ext.startsWith('linear:')) return 'linear'
  if (ext.startsWith('agent:')) return 'agent'
  return 'board'
}

/** One board row as an issue: what it is, where it lives, and which repository it says it is for. */
export function work(raw: unknown): Issue | null {
  const o = obj(raw)
  const title = str(o.title)
  const key = str(o.projectKey)
  const n = num(o.number)
  if (!title && !key) return null
  const ext = str(o.extRef)
  const gh = ext.match(GITHUB)
  const page = canon(str(o.url))
  // A forge row links to its own page and carries no reference of another host's.
  const forge = !ext && page ? str(o.repo) : ''
  const url = gh ? `https://github.com/${gh[1]}/${gh[2]}/issues/${gh[3]}` : page
  const assignee = str(o.assignee)
  return {
    key: canon(url) || (key && n ? `${key}#${n}`.toLowerCase() : '') || str(o.id),
    repo: gh ? `${gh[1]}/${gh[2]}` : '',
    forge,
    // A run's repository, or what a writer of the index named; a Linear row's `repo` is its team's key and never one.
    hint: gh || forge || origin(ext) === 'linear' ? '' : str(o.repo),
    number: gh ? Number(gh[3]) : n,
    title: title || 'Untitled',
    body: str(o.description),
    state: FINISHED.has(str(o.status)) ? 'closed' : 'open',
    labels: (Array.isArray(o.labels) ? o.labels : []).filter((l): l is string => typeof l === 'string' && l.trim() !== ''),
    assignees: assignee ? [assignee] : [],
    url,
    sources: [forge ? 'forge' : origin(ext)],
    updated: instant(num(o.updatedAt)) || instant(num(o.createdAt)),
    created: instant(num(o.createdAt)),
    kind: str(o.kind) || 'issue',
    status: str(o.status) || 'backlog',
    priority: str(o.priority) || 'none',
    pull: str(o.kind) === 'pr',
    board: key,
  }
}

/**
 * What the board answers a caller with no seat in the org's made workspace yet,
 * or an org with no namespace on the forge yet (cloud apps/task source.go): both
 * are what opening the workspace makes. A seat refused for being no person's — an
 * API key — is not one of them, and is said as it is.
 */
const UNSEATED = /no forge identity for this principal yet|no namespace on the forge yet/i

/** The workspace opened for a target, once while the page lives: concurrent reads share one open. */
const opened = new Map<string, Promise<unknown>>()

/** Set the org's code workspace up for the caller: its namespace, and their forge account and seat. Idempotent. */
export function open(t: Target): Promise<unknown> {
  const key = `${t.api}\n${t.org ?? ''}`
  let p = opened.get(key)
  if (!p) {
    p = call<unknown>(t, 'POST', '/v1/git/workspace', {}).catch((e: unknown) => {
      opened.delete(key)
      throw e
    })
    opened.set(key, p)
  }
  return p
}

/** A board read that, refused for want of a seat or a namespace, opens the workspace and reads once more. */
async function seated<T>(t: Target, read: () => Promise<T>): Promise<T> {
  try {
    return await read()
  } catch (e) {
    if (!(e instanceof Refusal) || e.status !== 403 || !UNSEATED.test(e.message)) throw e
    await open(t)
    return read()
  }
}

export async function boards(t: Target, signal?: AbortSignal): Promise<Board[]> {
  const raw = await seated(t, () => call<unknown>(t, 'GET', '/v1/task/projects', undefined, { signal }))
  return rows(raw)
    .map(board)
    .filter((b): b is Board => b !== null)
}

const issuesOf = (raw: unknown): Issue[] =>
  merged(
    rows(raw)
      .map(work)
      .filter((w): w is Issue => w !== null),
  )

/** Every issue and pull request the org has, from the forge and the task index, each once. */
export async function everything(t: Target, signal?: AbortSignal): Promise<Issue[]> {
  return issuesOf(await seated(t, () => call<unknown>(t, 'GET', '/v1/task/board', undefined, { signal })))
}

/** The rows a list of projects is assembled from (merge.ts `assemble`), and which sources did not answer. */
export interface Sources {
  /** GitHub's grants to the org's connection, every page read so far: empty for anyone it does not answer. */
  hub: Repo[]
  /** The repositories linked from GitHub, each with the id dropping its link takes. */
  linked: Link[]
  /** The forge's repositories. */
  held: Held[]
  /** The org's projects that build from a repository. */
  sites: Site[]
  /** The GitHub accounts the org's connection grants from, for the owner filter. */
  owners: string[]
  /** GitHub's cursor for its next page, or ''. */
  next: string
  /** GitHub accounts the connection could not read. */
  unread: string[]
  /** Sources that did not answer, as a reader names them. */
  missing: string[]
}

/**
 * What `missing` calls each source. A reader that cannot stand without one — a
 * removal that missed a link would leave a copy that comes back — asks by these.
 */
export const UNREAD = { hub: 'GitHub', linked: 'the linked repositories', forge: 'the forge', sites: 'the projects' } as const

export interface SourceQuery {
  /** Searched by GitHub; the rest are searched where they are drawn. */
  q?: string
  /** One GitHub account. */
  owner?: string
  /** GitHub's cursor: read only GitHub's page after it. */
  after?: string
}

/**
 * What the org's projects are made from, each read on its own: the links (GET
 * /v1/sync), the forge's repositories, the org's projects, and GitHub's grants
 * — searched and paged by GitHub, and empty for an operator with no GitHub of
 * their own, which is why it is never the only source. One that fails leaves the
 * others standing and is named in `missing`. A next page reads GitHub alone.
 */
export async function sources(t: Target, p: SourceQuery = {}, signal?: AbortSignal): Promise<Sources> {
  const missing: string[] = []
  const soft = <T>(what: string, read: Promise<T>): Promise<T | null> =>
    read.catch((e: unknown) => {
      if (signal?.aborted) throw e
      missing.push(what)
      return null
    })
  const first = !p.after
  const [hub, linked, forge, sites, accounts] = await Promise.all([
    soft(UNREAD.hub, granted(t, { q: p.q, owner: p.owner, limit: 100, after: p.after }, signal)),
    first ? soft(UNREAD.linked, links(t, signal)) : null,
    first ? soft(UNREAD.forge, codebases(t)) : null,
    first ? soft(UNREAD.sites, built(t)) : null,
    first ? installations(t).catch(() => []) : [],
  ])
  return {
    hub: hub?.repos ?? [],
    linked: linked ?? [],
    held: (forge ?? []).map((c) => ({ org: c.org, name: c.name, description: c.description, branch: c.branch, public: c.public, updated: c.updated })),
    sites: (sites ?? []).filter((x) => x.repo).map((x) => ({ slug: x.slug, repo: x.repo, branch: x.branch, updated: x.updated ? new Date(x.updated * 1000).toISOString() : '' })),
    owners: owners(
      accounts.map((a) => a.login),
      (linked ?? []).map((l) => l.owner),
    ),
    next: hub?.next ?? '',
    unread: hub?.unread ?? [],
    missing,
  }
}

/** A repository a run and an environment can address on the forge, by the name its copy has there. */
export interface Home {
  /** The forge name: what a run's `repo` and `/v1/environment/{repo}` take. */
  name: string
  /** `owner/name` as people know it: GitHub's address for a linked one, the org's for one made on the forge. */
  label: string
  branch: string
}

export interface Homes {
  homes: Home[]
  /** Both sources answered, so a name missing from `homes` is not the org's. */
  whole: boolean
}

/**
 * Every repository on the forge a run can work on: the forge's own and every
 * linked repository's copy (GET /v1/git/repos does not list an estate's copies;
 * GET /v1/sync does). Throws only when neither answered.
 */
export async function homes(t: Target, signal?: AbortSignal): Promise<Homes> {
  const failed: unknown[] = []
  const soft = <T>(read: Promise<T>): Promise<T | null> =>
    read.catch((e: unknown) => {
      failed.push(e)
      return null
    })
  const [linked, forge] = await Promise.all([soft(links(t, signal)), soft(codebases(t))])
  if (!linked && !forge) throw failed[0]
  const list = assemble({
    linked: linked ?? [],
    held: (forge ?? []).map((c) => ({ org: c.org, name: c.name, description: c.description, branch: c.branch, public: c.public, updated: c.updated })),
  })
  return {
    homes: list
      .filter((p) => p.forge)
      .map((p) => ({ name: p.forge, label: `${p.owner}/${p.name}`, branch: p.branch || 'main' }))
      .sort((a, b) => a.label.localeCompare(b.label)),
    whole: failed.length === 0,
  }
}
