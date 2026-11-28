/**
 * Connectors: the MCP servers an org adds, and the shelf it picks them from.
 *
 *   GET    /v1/tool/mcp/servers        {servers: [server]}
 *   POST   /v1/tool/mcp/servers        {name, url} or {listing, name?}, with {authHeader, secret} → 201 server
 *   DELETE /v1/tool/mcp/servers/{id}   204
 *   GET    /v1/tool/catalog?q=&limit=&offset=   {catalog: [listing], total, limit, offset}
 *   GET    /v1/tool/catalog/{id}       one listing in full
 *
 * A server's secret is sealed in KMS by the platform and never answered again;
 * a record says only whether it has one. Its tools are GET /v1/tool?source=mcp
 * (tools.ts), named `<server id>_<tool>`, and each is called only once it is on.
 * A listing can be added here only when it serves streamable HTTP; one that
 * ships only a package needs somewhere to run first. The fleet's own servers,
 * on for every run, are mcp.ts.
 */
import { phrase } from '@hanzo/i18n'

import { call, query, seg, type Target } from './call.ts'

export interface Server {
  /** The server's id in the org, and the prefix of every tool it brings. */
  id: string
  name: string
  url: string
  /** The header the sealed secret is sent in, or ''. */
  header: string
  /** Whether a secret is sealed for it. */
  secret: boolean
  /** The catalog listing it was added from, or '' for a URL typed in. */
  listing: string
  created: number
  /** False when no admin put it in place, so no run carries it; true, or unsaid by an older platform. */
  admitted: boolean
}

export interface Remote {
  transport: string
  url: string
}

export interface Package {
  registry: string
  identifier: string
  runtime: string
  version: string
}

export interface Listing {
  id: string
  /** The publisher's reverse-DNS name, `com.stripe/mcp`. */
  name: string
  title: string
  description: string
  vendor: string
  version: string
  logo: string
  featured: boolean
  official: boolean
  transports: string[]
  remotes: Remote[]
  packages: Package[]
  repo: string
  site: string
}

export interface Page {
  listings: Listing[]
  total: number
  offset: number
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
const rows = (v: unknown): unknown[] => (Array.isArray(v) ? v : [])

export function server(raw: unknown): Server {
  const o = obj(raw)
  return {
    id: str(o.id),
    name: str(o.name),
    url: str(o.url),
    header: str(o.authHeader),
    secret: o.hasSecret === true,
    listing: str(o.listing),
    created: num(o.createdAt),
    admitted: o.admitted !== false,
  }
}

export function listing(raw: unknown): Listing {
  const o = obj(raw)
  return {
    id: str(o.id),
    name: str(o.name),
    title: str(o.title),
    description: str(o.description),
    vendor: str(o.vendor),
    version: str(o.version),
    logo: str(o.logo),
    featured: o.featured === true,
    official: o.official === true,
    transports: rows(o.transports).map(str).filter(Boolean),
    remotes: rows(o.remotes)
      .map((r) => ({ transport: str(obj(r).transport), url: str(obj(r).url) }))
      .filter((r) => r.url),
    packages: rows(o.packages)
      .map((p) => {
        const x = obj(p)
        return { registry: str(x.registry), identifier: str(x.identifier), runtime: str(x.runtime), version: str(x.version) }
      })
      .filter((p) => p.identifier),
    repo: str(o.repo),
    site: str(o.site),
  }
}

/** What a listing is called on screen. */
export const titleOf = (l: Listing): string => l.title || l.name

/** Whether a listing can be added here and now: the platform dials its streamable-HTTP remote. */
export const ready = (l: Listing): boolean => l.remotes.some((r) => r.transport === 'streamable-http')

/** A server's tools among the plane's, by the prefix its id gives them. */
export const owns = (s: Server, toolName: string): boolean => toolName.startsWith(`${s.id}_`)

export async function servers(t: Target): Promise<Server[]> {
  return rows(obj(await call<unknown>(t, 'GET', '/v1/tool/mcp/servers')).servers).map(server).filter((s) => s.id)
}

export interface Adding {
  /** Required with a URL; a listing takes its own title when this is empty. */
  name?: string
  url?: string
  listing?: string
  header?: string
  secret?: string
}

/** Why a server cannot be added as it stands, or '' when it can. English, marked (`phrase`): a screen says it through `t`. */
export function refuse(a: Adding): string {
  if (!a.listing) {
    if (!a.name?.trim()) return phrase('A connector needs a name')
    if (!/^https?:\/\/[^\s/]+/.test(a.url?.trim() ?? '')) return phrase('The URL is an http(s) address')
  }
  if (a.secret && !a.header?.trim()) return phrase('Name the header the secret is sent in')
  return ''
}

export async function add(t: Target, a: Adding): Promise<Server> {
  const why = refuse(a)
  if (why) throw new Error(why)
  const body: Record<string, string> = {}
  if (a.listing) body.listing = a.listing
  else body.url = a.url!.trim()
  if (a.name?.trim()) body.name = a.name.trim()
  // The header names where the secret goes, so it rides with one or not at all.
  if (a.secret) {
    body.authHeader = a.header!.trim()
    body.secret = a.secret
  }
  return server(await call<unknown>(t, 'POST', '/v1/tool/mcp/servers', body))
}

export async function remove(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/tool/mcp/servers/${seg(id)}`)
}

/** A page of the shelf, featured first, then by name. */
export async function shelf(t: Target, q: { text?: string; limit?: number; offset?: number } = {}): Promise<Page> {
  const r = obj(await call<unknown>(t, 'GET', `/v1/tool/catalog${query({ q: q.text?.trim(), limit: q.limit, offset: q.offset || undefined })}`))
  return { listings: rows(r.catalog).map(listing).filter((l) => l.id), total: num(r.total), offset: num(r.offset) }
}

export async function one(t: Target, id: string): Promise<Listing> {
  return listing(await call<unknown>(t, 'GET', `/v1/tool/catalog/${seg(id)}`))
}
