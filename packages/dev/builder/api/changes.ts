/**
 * What a run changed, read from the forge it pushed to, as the person asking,
 * and its pull request merged there.
 *
 *   GET  /v1/agent/coding/{session}/changes     commits base..head, each file's patch, the pull request
 *   GET  /v1/agent/coding/{session}/tree?path=  one directory at the run's branch (its base until it pushes)
 *   GET  /v1/agent/coding/{session}/blob?path=  one file there, in /v1/git's blob shape
 *   POST /v1/agent/coding/{session}/merge       → {number, url, state, base}; 409 with the forge's reason
 *
 * Before the run pushes, changes answers empty, not 404.
 */
import { call, query, seg, type Target } from './call.ts'
import { entries, file, type Blob, type Entry } from './git.ts'

export interface Commit {
  sha: string
  /** The first line. */
  message: string
  author: string
  date: string
}

export interface Change {
  path: string
  /** The old path of a rename, or ''. */
  from: string
  status: 'added' | 'modified' | 'deleted' | 'renamed'
  additions: number
  deletions: number
  /** The file's unified diff hunks, or '' for a binary file. */
  patch: string
  truncated: boolean
}

export interface Review {
  author: string
  state: string
  body: string
  at: string
}

export interface Pull {
  number: number
  url: string
  title: string
  state: 'open' | 'closed' | 'merged'
  /** Null when the forge has not worked it out. */
  mergeable: boolean | null
  reviews: Review[]
}

export interface Changes {
  repo: string
  base: string
  head: string
  commits: Commit[]
  files: Change[]
  pull: Pull | null
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
const arr = (v: unknown): unknown[] => (Array.isArray(v) ? v : [])

const STATUS = ['added', 'modified', 'deleted', 'renamed'] as const
const PULL = ['open', 'closed', 'merged'] as const

export function changes(raw: unknown): Changes {
  const o = obj(raw)
  const p = o.pull ? obj(o.pull) : null
  return {
    repo: str(o.repo),
    base: str(o.base),
    head: str(o.head),
    commits: arr(o.commits)
      .map(obj)
      .map((c) => ({ sha: str(c.sha), message: str(c.message), author: str(c.author), date: str(c.date) }))
      .filter((c) => c.sha),
    files: arr(o.files)
      .map(obj)
      .map((f) => ({
        path: str(f.path),
        from: str(f.from),
        status: STATUS.find((s) => s === f.status) ?? 'modified',
        additions: num(f.additions),
        deletions: num(f.deletions),
        patch: str(f.patch),
        truncated: f.truncated === true,
      }))
      .filter((f) => f.path),
    pull:
      p && num(p.number)
        ? {
            number: num(p.number),
            url: str(p.url),
            title: str(p.title),
            state: PULL.find((s) => s === p.state) ?? 'open',
            mergeable: typeof p.mergeable === 'boolean' ? p.mergeable : null,
            reviews: arr(p.reviews)
              .map(obj)
              .map((r) => ({ author: str(r.author), state: str(r.state), body: str(r.body), at: str(r.at) })),
          }
        : null,
  }
}

export async function read(t: Target, session: string): Promise<Changes> {
  return changes(await call<unknown>(t, 'GET', `/v1/agent/coding/${seg(session)}/changes`))
}

export async function tree(t: Target, session: string, path = ''): Promise<Entry[]> {
  return entries(await call<unknown>(t, 'GET', `/v1/agent/coding/${seg(session)}/tree${query({ path })}`))
}

export async function blob(t: Target, session: string, path: string): Promise<Blob> {
  return file(await call<unknown>(t, 'GET', `/v1/agent/coding/${seg(session)}/blob${query({ path })}`), path)
}

/** A pull request after its merge. */
export interface Merged {
  number: number
  url: string
  /** `merged`. */
  state: string
  /** The branch it was merged into. */
  base: string
}

/**
 * Merge the run's pull request into its base, at the commit the forge holds
 * now: a push after that is refused, not merged unseen. One already merged
 * answers as it is; a conflict, a closed pull request or a rule on the base is
 * a 409 carrying the forge's reason. Nothing is forced.
 */
export async function merge(t: Target, session: string): Promise<Merged> {
  const o = obj(await call<unknown>(t, 'POST', `/v1/agent/coding/${seg(session)}/merge`, {}))
  return { number: num(o.number), url: str(o.url), state: str(o.state), base: str(o.base) }
}
