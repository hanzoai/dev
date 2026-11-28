/**
 * Codebases on the forge, the org's own git.
 *
 *   GET    /v1/git/repos          {data: [repo]}   most recently updated first
 *   GET    /v1/git/repos/{name}   one repo, with its branch names
 *   DELETE /v1/git/repos/{name}   the repo and its history, gone
 *
 * A coding run addresses a repository by its name. The org is the tenant on
 * the request, so the name is the whole address — `owner/name` is a path the
 * engine refuses.
 */
import { phrase } from '@hanzo/i18n'

import { call, seg, type Target } from './call.ts'

export interface Codebase {
  org: string
  name: string
  description: string
  /** HEAD's branch, or 'main' when the forge did not say. */
  branch: string
  public: boolean
  /** The https remote `git clone` takes, or ''. */
  clone: string
  updated: string
  branches: string[]
}

/** What the composer chip stores. `forge` marks a choice made from this list. */
export interface ForgeRepo {
  owner: string
  name: string
  full_name: string
  private: boolean
  default_branch: string
  pushed_at: string
  installation_id: number
  forge: true
  clone: string
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const obj = (v: unknown): Record<string, unknown> =>
  v && typeof v === 'object' ? (v as Record<string, unknown>) : {}

export function codebase(raw: unknown, org = ''): Codebase | null {
  const o = obj(raw)
  const name = str(o.name)
  if (!name) return null
  const branches = (Array.isArray(o.branches) ? o.branches : []).filter((b): b is string => typeof b === 'string' && b !== '')
  const branch = str(o.defaultBranch) || 'main'
  return {
    org: str(o.org) || org,
    name,
    description: str(o.description),
    branch,
    public: o.public === true,
    clone: str(o.cloneUrl),
    updated: str(o.updatedAt),
    branches: branches.length ? branches : [branch],
  }
}

/** A chip row remembered as a forge codebase, when the list had not seen it yet. */
export function chosen(r: { owner: string; name: string; full_name?: string; private?: boolean; default_branch?: string }): ForgeRepo {
  return {
    owner: r.owner,
    name: r.name,
    full_name: r.full_name || (r.owner ? `${r.owner}/${r.name}` : r.name),
    private: r.private === true,
    default_branch: r.default_branch || 'main',
    pushed_at: '',
    installation_id: 0,
    forge: true,
    clone: '',
  }
}

/** The chip's row for a codebase the forge listed. */
export function asRepo(c: Codebase): ForgeRepo {
  const owner = c.org
  return {
    owner,
    name: c.name,
    full_name: owner ? `${owner}/${c.name}` : c.name,
    private: !c.public,
    default_branch: c.branch,
    pushed_at: c.updated,
    installation_id: 0,
    forge: true,
    clone: c.clone,
  }
}

function rows(raw: unknown): unknown[] {
  if (Array.isArray(raw)) return raw
  const o = obj(raw)
  return Array.isArray(o.data) ? o.data : []
}

const NAME = /^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$/

/** Provision an empty repository. The name is the handle and the last path segment. */
export async function create(t: Target, name: string, description = ''): Promise<Codebase> {
  const handle = name.trim().replace(/\.git$/, '')
  if (!NAME.test(handle)) throw new Error(phrase('A repository name starts with a letter or number, and may contain letters, numbers, dots, underscores and hyphens.'))
  const raw = await call<unknown>(t, 'POST', '/v1/git/repos', { name: handle, description: description.trim() })
  const row = codebase(raw, t.org ?? '')
  if (!row) throw new Error(phrase('The forge created a repository and did not name it'))
  return row
}

export async function codebases(t: Target, q = ''): Promise<Codebase[]> {
  const raw = await call<unknown>(t, 'GET', '/v1/git/repos')
  const needle = q.trim().toLowerCase()
  return rows(raw)
    .map((r) => codebase(r, t.org ?? ''))
    .filter((c): c is Codebase => c !== null)
    .filter((c) => !needle || `${c.org}/${c.name} ${c.description}`.toLowerCase().includes(needle))
}

/** One codebase, so the branch chip can list what HEAD can point at. */
export async function one(t: Target, name: string): Promise<Codebase | null> {
  const raw = await call<unknown>(t, 'GET', `/v1/git/repos/${seg(name)}`)
  return codebase(raw, t.org ?? '')
}

/**
 * Delete a codebase from the forge, and its history with it. Nothing else goes:
 * a link that syncs into it, or a project built from it, is dropped on its own.
 */
export async function remove(t: Target, name: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/git/repos/${seg(name)}`)
}
