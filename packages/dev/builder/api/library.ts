/**
 * The Library: everything a person made, in Chat, Dev, Studio and workflows, in
 * one index.
 *
 *   GET    /v1/library            {data: [item], next}   newest first, filtered
 *   GET    /v1/library/{id}       one item, with fresh signed URLs
 *   POST   /v1/library            keep one: its bytes as base64, at most 10 MiB
 *   PATCH  /v1/library/{id}       rename, share with the org, move to a project
 *   DELETE /v1/library/{id}       its row and its bytes
 *
 * An item is its owner's; another member of the org reads it only once its owner
 * shares it, and changes it never. Its `url`, `download` and `preview` are
 * signed and stop working at `expires`: read the item again for fresh ones.
 */
import { phrase } from '@hanzo/i18n'

import { call, query, seg, Sentence, type Target } from './call.ts'

export const KIND_IDS = ['image', 'video', 'audio', 'document', 'slides', 'type', 'model', 'code', 'file'] as const
export type Kind = (typeof KIND_IDS)[number]

/** The kinds a reader narrows to, in the order the Library lists them, '' being all; each label English and marked (`phrase`), drawn through `t`. */
export const KINDS: readonly { id: Kind | ''; label: string }[] = [
  { id: '', label: phrase('All') },
  { id: 'image', label: phrase('Images') },
  { id: 'video', label: phrase('Videos') },
  { id: 'audio', label: phrase('Audio') },
  { id: 'document', label: phrase('Documents') },
  { id: 'slides', label: phrase('Slides') },
  { id: 'model', label: phrase('3D') },
  { id: 'code', label: phrase('Code') },
  { id: 'file', label: phrase('Files') },
]

export const SOURCES = ['chat', 'dev', 'studio', 'workflow'] as const
export type Source = (typeof SOURCES)[number]

export type Scope = 'all' | 'mine' | 'shared'

/** A product, as a person names it: Chat, Dev and Studio are names, and Workflow a word, marked (`phrase`). */
export const SOURCE: Record<Source, string> = { chat: 'Chat', dev: 'Dev', studio: 'Studio', workflow: phrase('Workflow') }

export interface Item {
  id: string
  kind: Kind
  source: Source
  name: string
  mime: string
  bytes: number
  width: number
  height: number
  seconds: number
  pages: number
  /** The subject that made it. */
  owner: string
  /** The reader made it. */
  mine: boolean
  /** Its owner shared it with the org. */
  shared: boolean
  project: string
  /** What made it: a conversation, a Dev run, a Studio run, an automation's run. */
  ref: string
  /** The app path back to what made it, or '' for none. */
  origin: string
  /** The original, to open in place. */
  url: string
  /** The original, as a download. */
  download: string
  /** A still that shows it, or ''. An image is its own. */
  preview: string
  expires: string
  created: string
  updated: string
}

/** What a list is narrowed to. */
export interface Filter {
  kind?: Kind | ''
  source?: Source | ''
  ref?: string
  project?: string
  q?: string
  /** RFC 3339 bounds on when an item was made. */
  since?: string
  until?: string
  scope?: Scope
  limit?: number
  cursor?: string
}

export interface Page {
  data: Item[]
  /** The cursor of the next page, or '' at the end. */
  next: string
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) && v >= 0 ? v : 0)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})

/** A signed address: https, and nothing a link could be made to run. */
const signed = (v: unknown): string => (/^https:\/\//.test(str(v)) ? str(v) : '')

/** An app path back to what made an item: under this origin, never another's. */
export const inApp = (v: unknown): string => {
  const s = str(v)
  return s.startsWith('/') && !s.startsWith('//') && !s.startsWith('/\\') ? s : ''
}

/** One item as the platform answers it, or null for a row with no id. */
export function item(raw: unknown): Item | null {
  const o = obj(raw)
  const id = str(o.id)
  if (!id) return null
  const kind = (KIND_IDS as readonly string[]).includes(str(o.kind)) ? (o.kind as Kind) : 'file'
  const source = (SOURCES as readonly string[]).includes(str(o.source)) ? (o.source as Source) : 'dev'
  return {
    id,
    kind,
    source,
    name: str(o.name) || id,
    mime: str(o.mime),
    bytes: num(o.bytes),
    width: num(o.width),
    height: num(o.height),
    seconds: num(o.seconds),
    pages: num(o.pages),
    owner: str(o.owner),
    mine: o.mine === true,
    shared: o.shared === true,
    project: str(o.project),
    ref: str(o.ref),
    origin: inApp(o.origin),
    url: signed(o.url),
    download: signed(o.download),
    preview: signed(o.preview),
    expires: str(o.expires),
    created: str(o.created),
    updated: str(o.updated),
  }
}

export async function items(t: Target, f: Filter = {}, init?: { signal?: AbortSignal }): Promise<Page> {
  const raw = obj(
    await call<unknown>(
      t,
      'GET',
      `/v1/library${query({
        kind: f.kind,
        source: f.source,
        ref: f.ref,
        project: f.project,
        q: f.q?.trim(),
        since: f.since,
        until: f.until,
        scope: f.scope === 'all' ? undefined : f.scope,
        limit: f.limit,
        cursor: f.cursor,
      })}`,
      undefined,
      init,
    ),
  )
  const data = (Array.isArray(raw.data) ? raw.data : []).map(item).filter((x): x is Item => x !== null)
  return { data, next: str(raw.next) }
}

export async function get(t: Target, id: string): Promise<Item> {
  const x = item(await call<unknown>(t, 'GET', `/v1/library/${seg(id)}`))
  if (!x) throw new Error(phrase('The item came back without an id'))
  return x
}

/** What an upload says about where a thing came from. */
export interface Made {
  source: Source
  kind?: Kind
  ref?: string
  origin?: string
  project?: string
  /** The same owner, source and key answer the item already made. */
  key?: string
  width?: number
  height?: number
  seconds?: number
  pages?: number
}

/** The largest thing an inline upload carries; larger files up to `MAX` reserve a signed PUT. */
export const INLINE = 10 << 20

/** The largest thing one upload carries (2 GiB). */
export const MAX = 2 * 1024 * 1024 * 1024

/** Bytes as base64, in chunks a call stack holds. */
export function base64(bytes: Uint8Array): string {
  let s = ''
  for (let i = 0; i < bytes.length; i += 0x8000) s += String.fromCharCode(...bytes.subarray(i, i + 0x8000))
  return btoa(s)
}

/** Keep a file in the Library. */
export async function upload(t: Target, file: { name: string; type: string; bytes?: Uint8Array; blob?: Blob }, made: Made): Promise<Item> {
  const len = file.blob ? file.blob.size : (file.bytes?.length ?? 0)
  if (!len) throw new Sentence(phrase('{name} is empty'), { name: file.name })
  if (len > MAX) throw new Sentence(phrase('{name} is larger than 2 GB'), { name: file.name })
  const mime = file.type || 'application/octet-stream'
  if (len > INLINE) {
    const reserved = obj(
      await call<unknown>(t, 'POST', '/v1/library', {
        name: file.name,
        mime,
        bytes: len,
        ...made,
      }),
    )
    const id = str(reserved.id)
    const up = obj(reserved.upload)
    const putUrl = signed(up.url)
    if (!id) throw new Error(phrase('The item was kept and came back without an id'))
    if (!putUrl) {
      const x = item(reserved)
      if (!x) throw new Error(phrase('The item was kept and came back without an id'))
      return x
    }
    const headers: Record<string, string> = {}
    if (Array.isArray(up.headers)) {
      for (const h of up.headers) {
        const ho = obj(h)
        const hn = str(ho.name)
        if (hn) headers[hn] = str(ho.value)
      }
    }
    const res = await fetch(putUrl, {
      method: 'PUT',
      headers,
      body: (file.blob ?? file.bytes) as BodyInit,
    })
    if (!res.ok) throw new Sentence(phrase('{name} was not kept'), { name: file.name })
    const done = item(await call<unknown>(t, 'PATCH', `/v1/library/${seg(id)}`, { commit: true }))
    if (!done) throw new Error(phrase('The item was kept and came back without an id'))
    return done
  }
  const bytes = file.bytes ?? new Uint8Array(await file.blob!.arrayBuffer())
  const x = item(
    await call<unknown>(t, 'POST', '/v1/library', {
      name: file.name,
      mime,
      data: base64(bytes),
      ...made,
    }),
  )
  if (!x) throw new Error(phrase('The item was kept and came back without an id'))
  return x
}

export interface Change {
  name?: string
  shared?: boolean
  project?: string
  commit?: boolean
}

export async function change(t: Target, id: string, c: Change): Promise<Item> {
  const x = item(await call<unknown>(t, 'PATCH', `/v1/library/${seg(id)}`, c))
  if (!x) throw new Error(phrase('The item was changed and came back without an id'))
  return x
}

export async function remove(t: Target, id: string): Promise<void> {
  await call(t, 'DELETE', `/v1/library/${seg(id)}`)
}

/** A size the way a person reads it. */
export function size(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`
  const units = ['KB', 'MB', 'GB']
  let n = bytes / 1024
  let u = 0
  while (n >= 1024 && u < units.length - 1) {
    n /= 1024
    u++
  }
  return `${n < 10 ? n.toFixed(1) : Math.round(n)} ${units[u]}`
}

/** The day an instant falls on, as a section of the Library: Today, Yesterday (English, marked), then the month. */
export function day(iso: string, now = new Date()): string {
  const d = new Date(iso)
  if (Number.isNaN(d.getTime())) return phrase('Earlier')
  const start = (x: Date) => new Date(x.getFullYear(), x.getMonth(), x.getDate()).getTime()
  const days = Math.round((start(now) - start(d)) / 86_400_000)
  if (days <= 0) return phrase('Today')
  if (days === 1) return phrase('Yesterday')
  return d.toLocaleDateString(undefined, d.getFullYear() === now.getFullYear() ? { month: 'long' } : { month: 'long', year: 'numeric' })
}

/** Items under the day each was made, in the order given (newest first). */
export function days(list: Item[], now = new Date()): { label: string; rows: Item[] }[] {
  const out: { label: string; rows: Item[] }[] = []
  for (const x of list) {
    const label = day(x.created, now)
    const last = out[out.length - 1]
    if (last && last.label === label) last.rows.push(x)
    else out.push({ label, rows: [x] })
  }
  return out
}
