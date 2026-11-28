/**
 * Fallback preview resolver when no live sandbox preview / published site is serving:
 * session-edited file -> project README.md -> first readable workspace file -> null.
 */
import type { Target } from './api/call.ts'
import { blob as sessionBlob, read as readChanges, tree as sessionTree, type Change } from './api/changes.ts'
import { artifact, artifacts } from './api/coding.ts'
import { blob as gitBlob, parseGitHubRepo } from './api/git.ts'
import { read as sandboxRead } from './api/sandbox.ts'
import type { Event } from './api/sessions.ts'
import { decode } from './api/turn.ts'

export type FallbackPreviewFormat = 'markdown' | 'html' | 'svg' | 'image' | 'code'
export type FallbackPreviewSource = 'session' | 'readme' | 'repo'

export interface FallbackPreview {
  path: string
  format: FallbackPreviewFormat
  source: FallbackPreviewSource
  /** UTF-8 file text for markdown, html, svg, and code previews. */
  text: string
  /** Data URL or direct URL for image previews. */
  imageUrl?: string
}

const IMAGE_MIMES: Readonly<Record<string, string>> = {
  png: 'image/png',
  jpg: 'image/jpeg',
  jpeg: 'image/jpeg',
  gif: 'image/gif',
  webp: 'image/webp',
  avif: 'image/avif',
  ico: 'image/x-icon',
  svg: 'image/svg+xml',
}

const MARKDOWN_EXTS = new Set(['md', 'mdx', 'markdown'])
const HTML_EXTS = new Set(['html', 'htm'])
const README_NAMES = ['README.md', 'readme.md', 'Readme.md', 'README.MD', 'README.mdx', 'README.txt', 'README']
const PREFERRED_FALLBACK_FILES = ['index.html', 'package.json', 'Cargo.toml', 'pyproject.toml', 'go.mod']

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const rel = (path: string): string => path.trim().replace(/^\/work\//, '').replace(/^\.\//, '')

function extOf(path: string): string {
  const base = path.split('/').pop() ?? path
  const dot = base.lastIndexOf('.')
  return dot > 0 ? base.slice(dot + 1).toLowerCase() : ''
}

export function previewFormatOf(path: string): FallbackPreviewFormat {
  const ext = extOf(path)
  if (MARKDOWN_EXTS.has(ext)) return 'markdown'
  if (HTML_EXTS.has(ext)) return 'html'
  if (ext === 'svg') return 'svg'
  if (ext in IMAGE_MIMES) return 'image'
  return 'code'
}

export function imageMimeOf(path: string): string {
  return IMAGE_MIMES[extOf(path)] ?? 'application/octet-stream'
}

/** Minimal sandboxed HTML wrapper for inline `.svg` previews. */
export function svgPreviewDocument(svg: string): string {
  return `<!doctype html><html><head><meta charset="utf-8"><style>html,body{margin:0;padding:16px;height:100%;box-sizing:border-box;display:flex;align-items:center;justify-content:center;background:transparent;}svg{max-width:100%;max-height:100%;height:auto;}</style></head><body>${svg}</body></html>`
}

/**
 * Extract the paths created or modified during this session in most-recent-first order.
 * Deleted files are removed from the result unless recreated later.
 */
export function sessionEditedPaths(events: readonly Pick<Event, 'kind' | 'payload' | 'seq'>[]): string[] {
  const ordered: string[] = []
  const touch = (rawPath: string) => {
    const p = rel(rawPath)
    if (!p) return
    const idx = ordered.indexOf(p)
    if (idx >= 0) ordered.splice(idx, 1)
    ordered.push(p)
  }
  const remove = (rawPath: string) => {
    const p = rel(rawPath)
    const idx = ordered.indexOf(p)
    if (idx >= 0) ordered.splice(idx, 1)
  }

  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    const body = decode(e.payload)
    if (!body || typeof body === 'string') continue
    if (e.kind === 'event') {
      const item = body.item && typeof body.item === 'object' ? (body.item as Record<string, unknown>) : null
      if (item && str(item.type) === 'file_change' && Array.isArray(item.changes)) {
        for (const c of item.changes) {
          if (!c || typeof c !== 'object') continue
          const change = c as Record<string, unknown>
          const p = str(change.path)
          if (!p) continue
          if (change.kind === 'delete') remove(p)
          else touch(p)
        }
      }
    } else if (e.kind === 'status' && Array.isArray(body.changed)) {
      for (const entry of body.changed) {
        if (typeof entry === 'string') touch(entry)
      }
    }
  }

  return ordered.reverse()
}

/** Candidate raw GitHub URLs for `path` across the session's ref and default branches. */
export function githubRawUrls(repo: string, refs: readonly string[], path: string): string[] {
  const parsed = parseGitHubRepo(repo)
  if (!parsed) return []
  const encodedPath = path
    .split('/')
    .filter(Boolean)
    .map((segment) => encodeURIComponent(segment))
    .join('/')
  if (!encodedPath) return []
  const uniqueRefs: string[] = []
  for (const ref of [...refs, 'main', 'master']) {
    const trimmed = ref.trim()
    if (trimmed && !uniqueRefs.includes(trimmed)) uniqueRefs.push(trimmed)
  }
  return uniqueRefs.map(
    (ref) => `https://raw.githubusercontent.com/${encodeURIComponent(parsed.owner)}/${encodeURIComponent(parsed.name)}/${encodeURIComponent(ref)}/${encodedPath}`,
  )
}

function isReadmeName(name: string): boolean {
  const lower = name.toLowerCase()
  return lower === 'readme.md' || lower === 'readme.mdx' || lower === 'readme.txt' || lower === 'readme'
}

function pickRootFile(names: readonly string[]): { name: string; isReadme: boolean } | null {
  const readme = names.find((n) => isReadmeName(n))
  if (readme) return { name: readme, isReadme: true }
  for (const preferred of PREFERRED_FALLBACK_FILES) {
    const match = names.find((n) => n.toLowerCase() === preferred.toLowerCase())
    if (match) return { name: match, isReadme: false }
  }
  const firstVisible = names.find((n) => n && !n.startsWith('.') && !n.endsWith('.lock') && n !== 'node_modules')
  return firstVisible ? { name: firstVisible, isReadme: false } : null
}

async function blobToDataUrl(blob: globalThis.Blob, mime: string): Promise<string> {
  const bytes = new Uint8Array(await blob.arrayBuffer())
  let binary = ''
  for (let i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]!)
  return `data:${mime};base64,${btoa(binary)}`
}

async function fetchRawText(urls: readonly string[], signal?: AbortSignal): Promise<string | null> {
  if (typeof fetch !== 'function') return null
  for (const url of urls) {
    if (signal?.aborted) return null
    try {
      const res = await fetch(url, { signal })
      if (res.ok) {
        const text = await res.text()
        if (text.trim()) return text
      }
    } catch {
      // Try next branch/source.
    }
  }
  return null
}

export interface ResolveFallbackInput {
  target: Target
  session: string
  sandbox: string
  running: boolean
  signed: boolean
  repo: string
  branch: string
  base: string
  events: readonly Pick<Event, 'kind' | 'payload' | 'seq'>[]
  signal?: AbortSignal
}

async function loadPath(
  path: string,
  source: FallbackPreviewSource,
  input: ResolveFallbackInput,
  artifactNames: ReadonlySet<string>,
  changeByPath: ReadonlyMap<string, Change>,
): Promise<FallbackPreview | null> {
  if (input.signal?.aborted) return null
  const format = previewFormatOf(path)
  const ref = input.branch || input.base || 'main'

  if (format === 'image') {
    const mime = imageMimeOf(path)
    if (input.running && input.sandbox) {
      const node = await sandboxRead(input.target, input.sandbox, path).catch(() => null)
      if (node && !node.dir && !node.truncated && node.base64) {
        return { path, format, source, text: '', imageUrl: `data:${mime};base64,${node.base64}` }
      }
    }
    if (input.signed && input.session && artifactNames.has(path)) {
      const data = await artifact(input.target, input.session, path).catch(() => null)
      if (data) {
        const imageUrl = await blobToDataUrl(data, mime).catch(() => '')
        if (imageUrl) return { path, format, source, text: '', imageUrl }
      }
    }
    const rawUrls = githubRawUrls(input.repo, [input.branch, input.base], path)
    if (rawUrls.length > 0 && typeof fetch === 'function') {
      for (const url of rawUrls) {
        if (input.signal?.aborted) return null
        try {
          const res = await fetch(url, { method: 'HEAD', signal: input.signal })
          if (res.ok) return { path, format, source, text: '', imageUrl: url }
        } catch {
          // Try next ref.
        }
      }
    }
    return null
  }

  // 1. Live sandbox file (uncommitted edits included; never wakes a parked sandbox).
  if (input.running && input.sandbox) {
    const node = await sandboxRead(input.target, input.sandbox, path).catch(() => null)
    if (node && !node.dir && !node.binary && !node.truncated && node.text !== '') {
      return { path, format, source, text: node.text }
    }
  }
  // 2. Session branch blob via coding API.
  if (input.signed && input.session) {
    const b = await sessionBlob(input.target, input.session, path).catch(() => null)
    if (b && !b.binary && !b.truncated && b.text !== '') {
      return { path, format, source, text: b.text }
    }
  }
  // 3. Stored session artifact bytes.
  if (input.signed && input.session && artifactNames.has(path)) {
    const data = await artifact(input.target, input.session, path).catch(() => null)
    if (data) {
      const text = await data.text().catch(() => '')
      if (text !== '') return { path, format, source, text }
    }
  }
  // 4. Platform git repo blob.
  if (input.signed && input.repo) {
    const b = await gitBlob(input.target, input.repo, ref, path).catch(() => null)
    if (b && !b.binary && !b.truncated && b.text !== '') {
      return { path, format, source, text: b.text }
    }
  }
  // 5. GitHub raw fallback when repo is known.
  if (input.repo) {
    const text = await fetchRawText(githubRawUrls(input.repo, [input.branch, input.base], path), input.signal)
    if (text !== null) return { path, format, source, text }
  }
  // 6. Diff patch fallback when a session modified the file but full bytes are unavailable.
  const change = changeByPath.get(path)
  if (change?.patch) {
    return { path, format: 'code', source, text: change.patch }
  }
  return null
}

/**
 * Resolve the best fallback preview when no live dev server or deployed site is serving:
 *   1. Most recently created/modified file in the session (events / changes / artifacts)
 *   2. Project README.md (or first readable root file when no README exists)
 *   3. `null` only when there are genuinely zero files
 */
export async function resolveFallbackPreview(input: ResolveFallbackInput): Promise<FallbackPreview | null> {
  if (input.signal?.aborted) return null

  const [changesResult, artifactsResult] =
    input.signed && input.session
      ? await Promise.all([
          readChanges(input.target, input.session).catch(() => null),
          artifacts(input.target, input.session).catch(() => null),
        ])
      : [null, null]

  if (input.signal?.aborted) return null

  const changeByPath = new Map<string, Change>()
  const changePaths: string[] = []
  for (const f of changesResult?.files ?? []) {
    if (f.status === 'deleted') continue
    const p = rel(f.path)
    if (!p) continue
    changeByPath.set(p, f)
    changePaths.push(p)
  }

  const artifactNames = new Set<string>()
  const artifactPaths: string[] = []
  for (const a of artifactsResult?.artifacts ?? []) {
    if (a.kind !== 'file') continue
    const p = rel(a.name)
    if (!p) continue
    artifactNames.add(p)
    artifactPaths.push(p)
  }

  const editedCandidates: string[] = []
  const addCandidate = (p: string) => {
    if (p && !editedCandidates.includes(p)) editedCandidates.push(p)
  }
  for (const p of sessionEditedPaths(input.events)) addCandidate(p)
  for (const p of changePaths) addCandidate(p)
  for (const p of artifactPaths) addCandidate(p)

  // Step 2: Most recently created/modified file in this session.
  for (const path of editedCandidates.slice(0, 5)) {
    const found = await loadPath(path, 'session', input, artifactNames, changeByPath)
    if (found) return found
  }

  if (input.signal?.aborted) return null

  // Step 3: Project README.md (or first readable root file if repo has files without a README).
  if (input.running && input.sandbox) {
    const root = await sandboxRead(input.target, input.sandbox, '').catch(() => null)
    if (root?.dir && root.names.length > 0) {
      const picked = pickRootFile(root.names)
      if (picked) {
        const found = await loadPath(picked.name, picked.isReadme ? 'readme' : 'repo', input, artifactNames, changeByPath)
        if (found) return found
      }
    }
  }

  if (input.signed && input.session) {
    const entries = await sessionTree(input.target, input.session, '').catch(() => [])
    const fileNames = entries.filter((e) => !e.dir).map((e) => e.path)
    if (fileNames.length > 0) {
      const picked = pickRootFile(fileNames)
      if (picked) {
        const found = await loadPath(picked.name, picked.isReadme ? 'readme' : 'repo', input, artifactNames, changeByPath)
        if (found) return found
      }
    }
  }

  if (input.repo) {
    const ref = input.branch || input.base || 'main'
    if (input.signed) {
      for (const readme of README_NAMES.slice(0, 2)) {
        const b = await gitBlob(input.target, input.repo, ref, readme).catch(() => null)
        if (b && !b.binary && !b.truncated && b.text !== '') {
          return { path: readme, format: previewFormatOf(readme), source: 'readme', text: b.text }
        }
      }
    }
    for (const readme of README_NAMES.slice(0, 2)) {
      const text = await fetchRawText(githubRawUrls(input.repo, [input.branch, input.base], readme), input.signal)
      if (text !== null) {
        return { path: readme, format: previewFormatOf(readme), source: 'readme', text }
      }
    }
  }

  return null
}
