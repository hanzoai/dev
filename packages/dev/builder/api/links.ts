/**
 * The repositories an organization has linked from GitHub, as the sync app
 * holds them: one link per repository (an account link declares one for every
 * repository its account holds), each with its copy on the forge.
 *
 *   GET    /v1/sync        {data: [syncView]}   apps/sync sync_api.go
 *   DELETE /v1/sync/{id}   drop one link
 *
 * A repository link's source is GitHub's https clone address; its target is the
 * forge, by the bare name the copy has there. `updatedAt` is bumped by every
 * reconcile, so it reads as the last time the link synced. `native` is the
 * forge copy: its page, its default branch and how it stands — `synced`,
 * `conflict`, `pending` (not on the forge yet) or `paused` — and is absent when
 * the forge could not be read, which is not reported as healthy.
 *
 * Every link the sync app holds is also a row of GET /v1/project (apps/sync
 * project.go), so this list is the org's linked projects and not a copy of them.
 *
 * Dropping a link stops the sync both ways: the sync app withdraws the push
 * back to GitHub as it deletes the link. The copy on the forge and the project
 * are left as they are, so a removal that wants them gone asks for each.
 */
import { call, seg, type Target } from './call.ts'

export interface Link {
  /** The link's handle, which dropping it takes. */
  id: string
  /** The GitHub account. */
  owner: string
  /** The repository; '' for an account link. */
  name: string
  /** The forge copy's name, which a run and an environment address; ''. */
  forge: string
  /** RFC 3339: when it last synced, or ''. */
  synced: string
  /** `synced`, `conflict`, `pending`, `paused`, or '' when the forge could not be read. */
  status: string
  /** The forge copy's default branch, or ''. */
  branch: string
  /** `both`, `pull`, `push` or `off`. */
  direction: string
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})

/** `owner` and `name` of a GitHub https address, `name` '' for an account; null for any other host. */
export function github(locator: string): { owner: string; name: string } | null {
  try {
    const u = new URL(locator.trim())
    if (u.protocol !== 'https:' || u.hostname.toLowerCase() !== 'github.com') return null
    const [owner = '', name = '', ...rest] = u.pathname
      .replace(/\.git$/, '')
      .split('/')
      .filter(Boolean)
    if (!owner || rest.length) return null
    return { owner, name }
  } catch {
    return null
  }
}

export function link(raw: unknown): Link | null {
  const o = obj(raw)
  if (str(o.kind) && str(o.kind) !== 'git') return null
  const source = obj(o.source)
  const target = obj(o.target)
  const at = str(source.provider) === 'github' ? github(str(source.locator)) : null
  if (!at) return null
  const native = o.native ? obj(o.native) : null
  return {
    id: str(o.id),
    owner: at.owner,
    name: str(o.scope) === 'account' ? '' : at.name,
    forge: str(target.provider) === 'hanzo-git' ? str(target.locator) : '',
    synced: str(o.updatedAt),
    status: native ? str(native.status) : '',
    branch: native ? str(native.branch) : '',
    direction: str(o.direction),
  }
}

/** Every repository link the org has; account links are left out, as each declares its repositories' own. */
export async function links(t: Target, signal?: AbortSignal): Promise<Link[]> {
  const raw = obj(await call<unknown>(t, 'GET', '/v1/sync', undefined, { signal }))
  return (Array.isArray(raw.data) ? raw.data : [])
    .map(link)
    .filter((l): l is Link => l !== null && l.name !== '')
}

/** Drop one link: GitHub stops syncing into its copy, and its copy stops pushing back. */
export async function unlink(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/sync/${seg(id)}`)
}
