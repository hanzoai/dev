/**
 * The org's projects: what it has built, and where each one is served.
 *
 *   GET    /v1/project                     → Project[]
 *   PATCH  /v1/project/{slug}              rename it, make it public or private, or change its repository
 *   DELETE /v1/project/{slug}              delete it and take its site down
 *   GET    /v1/project/{slug}/deployments  → Deployment[], newest first
 *
 * `slug` is the one key every surface shares — the address under /dev, the
 * site's name, the `project` a run is tagged with. A project belongs to the org,
 * not to a person: the record names no author, so any member lists, changes and
 * deletes it. Private is paid, and an unfunded org is refused rather than left
 * public without being told.
 */
import { phrase } from '@hanzo/i18n'

import { ADDRESS, clone, onGitHub } from '../address.ts'
import { call, seg, type Target } from './call.ts'

export interface Project {
  slug: string
  name: string
  /** The clone URL, or '' for a project with no repository. */
  repo: string
  /** The branch pushes to which rebuild it, or ''. */
  branch: string
  /**
   * Where it stands: `building` while a project that has never served is being
   * published, `live` once it serves, `error` when its build failed. A live
   * project stays `live` while it is built again.
   */
  status: string
  /** The deployed address, or '' when nothing has shipped. */
  live: string
  /** `public` or `private`, or '' when the platform did not say. */
  visibility: '' | Visibility
  /** The catalog template's slug, or `<org>/<slug>` of the published project, this was taken from; '' for neither. */
  forked: string
  /** Unix seconds; 0 when unknown. A deploy does not move `updated`. */
  created: number
  updated: number
}

export type Visibility = 'public' | 'private'

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)

const loopback = (host: string): boolean => host === 'localhost' || host === '127.0.0.1'

/** Whether this page itself is served from loopback — local work. */
const here = (): boolean => typeof window !== 'undefined' && loopback(window.location.hostname)

/** An address the preview may frame: https, or http on loopback when the builder is local too. */
export function safe(url: string, local = here()): string {
  try {
    const u = new URL(url)
    if (u.protocol === 'https:') return u.toString()
    // Loopback only for a builder that is itself on loopback: a published row
    // must not point colleagues' browsers at their own local services.
    if (local && u.protocol === 'http:' && loopback(u.hostname)) return u.toString()
  } catch {
    /* not an address */
  }
  return ''
}

export function project(raw: unknown): Project {
  const p = (raw && typeof raw === 'object' ? raw : {}) as Record<string, unknown>
  const repo = (p.repo && typeof p.repo === 'object' ? p.repo : {}) as Record<string, unknown>
  return {
    slug: str(p.slug),
    name: str(p.name) || str(p.slug),
    repo: str(repo.url),
    branch: str(repo.branch),
    status: str(p.status),
    live: safe(str(p.liveUrl)),
    visibility: p.visibility === 'public' || p.visibility === 'private' ? p.visibility : '',
    forked: str(p.forkedFrom),
    created: num(p.createdAt),
    updated: num(p.updatedAt),
  }
}

export async function projects(t: Target): Promise<Project[]> {
  const raw = await call<unknown>(t, 'GET', '/v1/project')
  const rows = Array.isArray(raw) ? raw : Array.isArray((raw as { data?: unknown })?.data) ? (raw as { data: unknown[] }).data : []
  return rows
    .map(project)
    .filter((p) => p.slug)
    .sort((a, b) => b.updated - a.updated)
}

/**
 * Change a project's name, who can see it, or the repository it builds from;
 * what is not sent is left as it is.
 *
 * `repo` is the repository's address as a person gives it (../address.ts) and
 * the branch whose pushes rebuild it — left out, the platform takes `main`. It
 * is a GitHub repository, by any address GitHub hands out, and goes as its https
 * clone address; any other is refused before anything is sent. The platform may
 * still refuse to move a site that a repository in another account would serve
 * from elsewhere (409), and says why.
 */
export async function change(
  t: Target,
  slug: string,
  what: { name?: string; visibility?: Visibility; repo?: { url: string; branch?: string } },
): Promise<Project> {
  const body: { name?: string; visibility?: Visibility; repo?: { url: string; branch?: string } } = {}
  if (what.name !== undefined) {
    const n = what.name.trim()
    if (!n) throw new Error(phrase('A project needs a name'))
    body.name = n
  }
  if (what.visibility !== undefined) body.visibility = what.visibility
  if (what.repo !== undefined) {
    const at = onGitHub(what.repo.url)
    if (!at) throw new Error(ADDRESS)
    const branch = what.repo.branch?.trim()
    body.repo = branch ? { url: clone(at), branch } : { url: clone(at) }
  }
  return project(await call<unknown>(t, 'PATCH', `/v1/project/${seg(slug)}`, body))
}

/** Delete a project. Its site stops answering and its slug is free again. */
export async function remove(t: Target, slug: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/project/${seg(slug)}`)
}

/** One attempt to put a project on its address, as the platform recorded it. */
export interface Deployment {
  id: string
  /** Counts the project's deployments from 1. */
  version: number
  /** `building`, `live` or `error`, as the platform said. */
  status: string
  /** The revision that was built, or ''. */
  commit: string
  /** What happened, in words: the build's own note, or on a failure why it failed. */
  message: string
  /** Unix seconds; 0 when unknown. */
  created: number
  updated: number
}

export function deployment(raw: unknown): Deployment {
  const d = (raw && typeof raw === 'object' ? raw : {}) as Record<string, unknown>
  return {
    id: str(d.id),
    version: num(d.version),
    status: str(d.status),
    commit: str(d.commit),
    message: str(d.message),
    created: num(d.createdAt),
    updated: num(d.updatedAt),
  }
}

/** A project's deployments, newest first: the first says how its latest build went. */
export async function deployments(t: Target, slug: string): Promise<Deployment[]> {
  const raw = await call<unknown>(t, 'GET', `/v1/project/${seg(slug)}/deployments`)
  return (Array.isArray(raw) ? raw : [])
    .map(deployment)
    .filter((d) => d.id)
    .sort((a, b) => b.version - a.version)
}

/** The repository name a project's clone URL names — the last path segment, without `.git`. */
export function name(clone: string): string {
  const cut = clone.replace(/\.git$/, '').replace(/\/+$/, '')
  return cut.slice(cut.lastIndexOf('/') + 1)
}

/** A starter from the public catalog. */
export interface Template {
  slug: string
  title: string
  category: string
  description: string
  framework: string
  /** The repository the starter is cut from. */
  source: string
  /** The starter's own live page, or '' when it has none the preview may frame. */
  demo: string
}

/** The public catalog. It is reference content: no bearer, no org. */
export async function templates(t: Target): Promise<Template[]> {
  const raw = await call<{ data?: unknown }>({ api: t.api, token: () => null, org: null }, 'GET', '/v1/templates')
  const rows = Array.isArray(raw?.data) ? raw.data : []
  return rows
    .map((r) => {
      const o = (r && typeof r === 'object' ? r : {}) as Record<string, unknown>
      return {
        slug: str(o.slug),
        title: str(o.title) || str(o.slug),
        category: str(o.category),
        description: str(o.description),
        framework: str(o.framework),
        source: str(o.source),
        demo: safe(str(o.demo)),
      }
    })
    .filter((x) => x.slug)
}

/**
 * Take a copy of a starter: a project whose repository is the starter's own,
 * so what opens is that template rather than a model's imitation of it. In an
 * org whose code is in a workspace the platform made, the copy is published at
 * once, and answers `building` until it is `live` or its build failed.
 */
export async function fork(t: Target, slug: string): Promise<Project> {
  return project(await call<unknown>(t, 'POST', '/v1/project/fork', { slug }))
}

/** `owner/name` from a clone URL — its last two path segments, without `.git`. */
export function address(clone: string): string {
  const parts = clone.replace(/\.git$/, '').replace(/\/+$/, '').split('/')
  return parts.length >= 2 ? `${parts[parts.length - 2]}/${parts[parts.length - 1]}` : ''
}

/**
 * Whether a run worked on this project's repository. A run's record can be
 * moved into any project of the org, so a project shows only the runs on its
 * own code — their branch is what Files, Code and Publish read.
 */
export function ours(runRepo: string, projectClone: string): boolean {
  if (!projectClone) return true
  const want = address(projectClone).toLowerCase()
  const have = runRepo.toLowerCase()
  return have === want || (!have.includes('/') && have === want.split('/')[1])
}
