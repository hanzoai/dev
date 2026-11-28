/**
 * A repository's files at one revision, from the platform's native git.
 *
 *   GET /v1/git/repos/{name}/tree?ref=&path=   one directory, dirs first
 *   GET /v1/git/repos/{name}/blob?ref=&path=   one file; binary is base64,
 *                                              past 1 MiB it is truncated with no content
 *
 * A run pushes its branch to the forge, not here, so a run's files are read
 * through the run (changes.ts); these read a codebase as the platform holds it.
 */
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

const str = (v: unknown): string => (typeof v === 'string' ? v : '')

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
