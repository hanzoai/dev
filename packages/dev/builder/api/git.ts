/**
 * A repository's files at one revision, and its outbound mirror targets,
 * from the platform's native git.
 *
 *   GET  /v1/git/repos/{name}/tree?ref=&path=   one directory, dirs first
 *   GET  /v1/git/repos/{name}/blob?ref=&path=   one file; binary is base64,
 *                                               past 1 MiB it is truncated with no content
 *   GET  /v1/git/repos/{name}/targets           outbound mirror targets
 *   POST /v1/git/repos/{name}/targets           attach outbound GitHub mirror + initial sync push
 *   POST /v1/git/repos/{name}/link              link an existing GitHub repo
 *
 * A run pushes its branch to the forge, not here, so a run's files are read
 * through the run (changes.ts); these read a codebase as the platform holds it.
 */
import { ADDRESS, clone, onGitHub, type At } from '../address.ts'
import { call, query, seg, type Target } from './call.ts'

export interface Entry {
  name: string
  path: string
  dir: boolean
  size: number
}

export interface Blob {
  path: string
  /** Text, or '' when the file is binary or past the view cap. */
  text: string
  binary: boolean
  truncated: boolean
  size: number
}

/** An outbound mirror target attached to a Hanzo Git repository. */
export interface GitTarget {
  id: string
  repo: string
  host: string
  url: string
  createdAt: string
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})

/** GitHub owner/name shorthand segment. */
const GH_SEG = /^[A-Za-z0-9][A-Za-z0-9._-]{0,99}$/

/**
 * Parse a GitHub repository reference in any form GitHub hands out (`onGitHub`),
 * or in `owner/repo` shorthand (e.g. `hanzoai/dev`).
 */
export function parseGitHubRepo(input: string): At | null {
  const s = input.trim()
  if (!s) return null
  const full = onGitHub(s)
  if (full) return full
  const cleaned = s.replace(/\/+$/, '').replace(/\.git$/i, '')
  const parts = cleaned.split('/')
  if (parts.length !== 2) return null
  const [owner, name] = parts
  return owner && name && GH_SEG.test(owner) && GH_SEG.test(name) ? { owner, name } : null
}

export function gitTarget(raw: unknown): GitTarget | null {
  const o = obj(raw)
  const url = str(o.url)
  if (!url) return null
  return {
    id: str(o.id),
    repo: str(o.repo),
    host: str(o.host),
    url,
    createdAt: str(o.createdAt),
  }
}

/** List outbound mirror targets for a Hanzo Git repository. */
export async function listGitTargets(t: Target, name: string, signal?: AbortSignal): Promise<GitTarget[]> {
  const raw = await call<unknown>(t, 'GET', `/v1/git/repos/${seg(name)}/targets`, undefined, { signal })
  const rows = Array.isArray(raw) ? raw : Array.isArray(obj(raw).data) ? (obj(raw).data as unknown[]) : []
  return rows.map(gitTarget).filter((x): x is GitTarget => x !== null)
}

/**
 * Attach an outbound GitHub mirror target to a Hanzo Git repository,
 * which also triggers an initial sync push.
 */
export async function attachGitHubMirror(t: Target, name: string, url: string): Promise<GitTarget> {
  const at = parseGitHubRepo(url)
  if (!at) throw new Error(ADDRESS)
  const cloneUrl = clone(at)
  const raw = await call<unknown>(t, 'POST', `/v1/git/repos/${seg(name)}/targets`, { url: cloneUrl })
  return (
    gitTarget(raw) ?? {
      id: '',
      repo: name,
      host: 'github.com',
      url: cloneUrl,
      createdAt: '',
    }
  )
}

/** Link an existing GitHub repository to a Hanzo Git repository. */
export async function linkGitHubRepo(t: Target, name: string, source: string): Promise<void> {
  const at = parseGitHubRepo(source)
  if (!at) throw new Error(ADDRESS)
  await call<unknown>(t, 'POST', `/v1/git/repos/${seg(name)}/link`, { source: clone(at) })
}

export async function tree(t: Target, repo: string, ref: string, path = ''): Promise<Entry[]> {
  return entries(await call<unknown>(t, 'GET', `/v1/git/repos/${seg(repo)}/tree${query({ ref, path })}`))
}

/** A tree answer's entries. A run's branch on the forge answers in the same shape. */
export function entries(raw: unknown): Entry[] {
  const o = (raw && typeof raw === 'object' ? raw : {}) as { entries?: unknown }
  const rows = Array.isArray(o.entries) ? o.entries : []
  return rows
    .map((r) => {
      const o = (r && typeof r === 'object' ? r : {}) as Record<string, unknown>
      return {
        name: str(o.name),
        path: str(o.path),
        dir: o.type === 'tree',
        size: typeof o.size === 'number' ? o.size : 0,
      }
    })
    .filter((e) => e.path)
}

export async function blob(t: Target, repo: string, ref: string, path: string): Promise<Blob> {
  return file(await call<unknown>(t, 'GET', `/v1/git/repos/${seg(repo)}/blob${query({ ref, path })}`), path)
}

/** A blob answer. A run's branch on the forge answers in the same shape. */
export function file(raw: unknown, path: string): Blob {
  const o = (raw && typeof raw === 'object' ? raw : {}) as Record<string, unknown>
  const binary = o.binary === true
  const truncated = o.truncated === true
  return {
    path: str(o.path) || path,
    text: binary || truncated ? '' : str(o.content),
    binary,
    truncated,
    size: typeof o.size === 'number' ? o.size : 0,
  }
}
