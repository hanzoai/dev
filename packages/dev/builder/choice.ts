/**
 * The choice New starts from, kept in this browser so a codebase or an issue
 * opened from its own screen is what the composer is holding when it mounts.
 *
 * The key is the one `useKept` in New writes. A merge keeps the mode, the
 * model and the machine; it only replaces the fields the caller names.
 */
import { asRepo, type Codebase, type ForgeRepo } from './api/codebases.ts'
import type { Project } from './merge.ts'

export interface Choice {
  repo: ForgeRepo | null
  branch: string
  place: string
  mode: string
  model: string
  effort: string
  /** A draft handed over by another screen. New copies it once and clears it. */
  ask: string
  /** The issue that draft is, by the number the run's pull request closes; 0 for none. Handed over with it. */
  issue: number
}

const KEY = (org: string | null) => `hanzo.build.new.${org ?? 'none'}`

export const boardKey = (org: string | null) => `hanzo.build.board.${org ?? 'none'}`

function read(org: string | null): Record<string, unknown> {
  try {
    const raw = window.localStorage.getItem(KEY(org))
    const v = raw ? JSON.parse(raw) : null
    return v && typeof v === 'object' ? (v as Record<string, unknown>) : {}
  } catch {
    return {}
  }
}

function write(org: string | null, value: Record<string, unknown>) {
  try {
    window.localStorage.setItem(KEY(org), JSON.stringify(value))
  } catch {
    /* the next New starts empty */
  }
}

/**
 * The choice New opens on, field by field: the last layer that says something
 * wins. What was kept here, over the person's coding defaults, over the first
 * run's. A field kept as '' stays '' (the default branch, the sandbox): only a
 * field that is absent falls through, so a codebase or an issue handed over from
 * its own screen — which keeps the codebase and the words alone — still opens on
 * a model, a mode and an effort.
 */
export function settle<T extends object>(first: T, ...layers: (Partial<T> | null | undefined)[]): T {
  let out = { ...first }
  for (const layer of layers) {
    if (!layer || typeof layer !== 'object' || Array.isArray(layer)) continue
    out = { ...out, ...Object.fromEntries(Object.entries(layer).filter(([, v]) => v !== undefined && v !== null)) }
  }
  return out
}

/** Point New at a forge codebase, and optionally a first draft. */
export function pinCodebase(org: string | null, c: Codebase, ask = '') {
  const repo = asRepo(c)
  write(org, { ...read(org), repo, branch: repo.default_branch, ask, issue: 0 })
}

/** Which board Issues is showing. '' is every board. */
export function pinBoard(org: string | null, key: string) {
  try {
    window.localStorage.setItem(boardKey(org), JSON.stringify(key))
  } catch {
    /* Issues then shows every board */
  }
  window.dispatchEvent(new CustomEvent('hanzo-board', { detail: key }))
}

/**
 * Point New at a project, and optionally a first draft (an issue's ask) and the
 * issue it closes. `where` is where the run works (merge.ts `home`): on the forge
 * it is chosen by its forge name, which is what a run and its environment
 * address; on GitHub by GitHub's address, so the run clones and proposes there.
 * Left out, the forge when the project is there. A project that is neither
 * cannot be worked on, and is refused here rather than handed to the platform.
 */
export function pinRepo(org: string | null, p: Project, ask = '', issue = 0, where: 'github' | 'forge' | null = p.forge ? 'forge' : p.linked ? 'github' : null): void {
  const branch = p.branch || 'main'
  let repo: ForgeRepo | HubRepo
  if (where === 'forge' && p.forge) {
    repo = { owner: org ?? '', name: p.forge, full_name: org ? `${org}/${p.forge}` : p.forge, private: p.private ?? true, default_branch: branch, pushed_at: '', installation_id: 0, forge: true, clone: '' }
  } else if (where === 'github' && p.linked) {
    repo = asHub({ owner: p.owner, name: p.name, full_name: `${p.owner}/${p.name}`, private: p.private ?? true, default_branch: branch, pushed_at: '', installation_id: 0 })
  } else {
    throw new Error(`${p.owner}/${p.name} is neither on the forge nor linked from GitHub`)
  }
  write(org, { ...read(org), repo, branch, ask, issue })
}

/**
 * Point New at a repository on GitHub, by GitHub's address: the one Sync just
 * brought in (sync.tsx), so the next run works on it and not on whatever was
 * chosen before. It is GitHub's address and not its forge copy's, because the
 * copy is still on its way there. A draft or an issue handed over with the
 * last choice was that repository's, and goes with it.
 */
export function pinHub(org: string | null, r: { owner: string; name: string; private: boolean; branch: string }): void {
  const repo = asHub({ owner: r.owner, name: r.name, full_name: `${r.owner}/${r.name}`, private: r.private, default_branch: r.branch, pushed_at: '', installation_id: 0 })
  write(org, { ...read(org), repo, branch: repo.default_branch, ask: '', issue: 0 })
}

/**
 * A repository on GitHub, chosen from the org's installation: the run works there.
 * Its `full_name` is GitHub's address, `github.com/owner/name` — what the list
 * shows, so a forge codebase of the same name is never mistaken for it, and what
 * a coding run takes to clone, push and propose on GitHub.
 */
export interface HubRepo {
  owner: string
  name: string
  full_name: string
  private: boolean
  default_branch: string
  pushed_at: string
  installation_id: number
  github: true
  /**
   * Given by its address (address.ts), not chosen from a list. It is the
   * person's word for where the run works, so no list is asked whether it is
   * still there: a public repository is on no list, and GitHub admits a run on
   * it all the same.
   */
  given?: boolean
}

export const isHub = (repo: { github?: boolean; full_name?: string } | null): repo is HubRepo =>
  Boolean(repo && repo.github === true && repo.full_name?.startsWith('github.com/'))

/** A GitHub listing row as the composer offers it. */
export const asHub = (r: Omit<HubRepo, 'github'>): HubRepo => ({ ...r, full_name: `github.com/${r.owner}/${r.name}`, github: true })

export const isForge = (repo: { forge?: boolean; name?: string } | null): repo is ForgeRepo =>
  Boolean(repo && repo.forge === true && repo.name)

/**
 * What New can offer now, read to judge the repository it kept. Each is null
 * when it was not read — refused, let go, or still being read — and null
 * judges nothing.
 */
export interface Offered {
  /** Every name a run can address on the forge: the forge's own repositories (GET /v1/git/repos) and every linked copy (GET /v1/sync). */
  forge: readonly string[] | null
  /** A page of GitHub's listing (GET /v1/provider/github/repos), with what it says stands behind it. */
  github: { repos: readonly { owner: string; name: string }[]; next: string; unread: readonly string[] } | null
}

/**
 * Whether the repository New kept is gone from what it can offer: a GitHub
 * repository GitHub no longer lists — let go there, or GitHub disconnected,
 * which lists nothing — or a codebase the forge no longer holds. Left kept, it
 * is where every next run would go, whatever was linked or synced since.
 *
 * Only a WHOLE answer says a repository is missing. A GitHub page with another
 * after it, or with an installation it could not read, is not the whole list,
 * and a list that was not read is no list, so a refused or slow read never
 * loses a good choice. An address the person gave is theirs, and no list is
 * asked about it.
 */
export function gone(repo: { forge?: boolean; github?: boolean; name?: string; full_name?: string; given?: boolean } | null, now: Offered): boolean {
  if (!repo || repo.given) return false
  if (isHub(repo)) {
    const listed = now.github
    if (!listed || listed.next || listed.unread.length) return false
    // Each row by the address `asHub` gives it, which is the one New keeps.
    const want = repo.full_name.toLowerCase()
    return !listed.repos.some((r) => `github.com/${r.owner}/${r.name}`.toLowerCase() === want)
  }
  if (isForge(repo)) {
    if (!now.forge) return false
    const want = repo.name.toLowerCase()
    return !now.forge.some((n) => n.toLowerCase() === want)
  }
  return false
}

/** A repository queued onto the forge and not reported landed yet. */
export interface Pending {
  fullName: string
  name: string
}

const syncingKey = (org: string | null) => `hanzo.build.sync.${org ?? 'none'}`

export function readPending(org: string | null): Pending[] {
  try {
    const raw = window.sessionStorage.getItem(syncingKey(org))
    const v = raw ? JSON.parse(raw) : []
    if (!Array.isArray(v)) return []
    return v.filter(
      (p): p is Pending =>
        Boolean(p) && typeof p === 'object' && typeof p.fullName === 'string' && p.fullName !== '' && typeof p.name === 'string' && p.name !== '',
    )
  } catch {
    return []
  }
}

export function writePending(org: string | null, rows: Pending[]) {
  try {
    if (rows.length === 0) window.sessionStorage.removeItem(syncingKey(org))
    else window.sessionStorage.setItem(syncingKey(org), JSON.stringify(rows))
  } catch {
    /* the table then omits the in-flight rows */
  }
}
