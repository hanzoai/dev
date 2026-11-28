/**
 * A run's sandbox. It outlives the run: kept after the run ends, parked (its pod
 * stopped, its disk and working tree kept) while nobody watches, and retired a
 * day after it parks; a free plan's sandbox is not parked, and ends within the
 * hour. Resume gives a parked one a pod again under the same id and tree.
 *
 *   GET  /v1/sandbox/{id}                              its status and when its lease ends
 *   POST /v1/sandbox/{id}/pause                        park it: stop the pod, keep the disk
 *   POST /v1/sandbox/{id}/resume                       a pod again, same id and /work
 *   POST /v1/sandbox/{id}/{screen|terminal}/ticket     one ticket, spent when the page's socket opens (30 s)
 *   GET  /v1/sandbox/{id}/ports                        what listens in it, each with its preview host
 *   POST /v1/sandbox/{id}/preview {port}               an address that opens that port in a browser
 *   POST /v1/sandbox/{id}/exec {argv, timeoutSec}      one command in /work: {exitCode, stdout, stderr}
 *   POST /v1/sandbox/read {id, path}                   a directory's names, or a file's bytes
 *
 * A door or a preview asked of a sandbox that stopped as it was asked is 409: it
 * is resumed, and asked again once.
 */
import { phrase } from '@hanzo/i18n'

import { call, Refusal, seg, type Target } from './call.ts'

export type Door = 'screen' | 'terminal'

/** Where a sandbox is in its life. */
export type State = 'pending' | 'running' | 'parked' | 'error' | 'gone'

export interface Box {
  id: string
  state: State
  /** When its lease ends, or a parked sandbox is retired; 0 when unknown. Unix ms. */
  ends: number
  /** Why it could not come up, with state `error`. */
  error: string
}

const STATES: State[] = ['pending', 'running', 'parked', 'error']

function box(raw: unknown, id: string): Box {
  const o = raw && typeof raw === 'object' ? (raw as Record<string, unknown>) : {}
  // A parked sandbox is also said to be paused; a status this does not know is not one to open.
  const state = STATES.find((s) => s === (o.status === 'paused' ? 'parked' : o.status)) ?? 'error'
  const ends = typeof o.expiresAt === 'number' && o.expiresAt > 0 ? o.expiresAt * 1000 : 0
  return { id, state, ends, error: typeof o.error === 'string' ? o.error : '' }
}

/** The sandbox as it stands. One the caller no longer holds is gone. */
export async function sandbox(t: Target, id: string): Promise<Box> {
  try {
    return box(await call(t, 'GET', `/v1/sandbox/${seg(id)}`), id)
  } catch (e) {
    if (e instanceof Refusal && e.status === 404) return { id, state: 'gone', ends: 0, error: '' }
    throw e
  }
}

/** Park it: its pod stops, its disk and working tree stay. */
export async function park(t: Target, id: string): Promise<Box> {
  return box(await call(t, 'POST', `/v1/sandbox/${seg(id)}/pause`), id)
}

/** Give it a pod again, under the same id and tree. */
export async function wake(t: Target, id: string): Promise<Box> {
  return box(await call(t, 'POST', `/v1/sandbox/${seg(id)}/resume`), id)
}

/** How long a resumed sandbox is waited on to run. */
const UP = 90_000

/** Resume it and wait until it runs. */
async function up(t: Target, id: string): Promise<void> {
  let b = await wake(t, id)
  const until = Date.now() + UP
  while (b.state === 'pending' && Date.now() < until) {
    await new Promise((r) => setTimeout(r, 1500))
    b = await sandbox(t, id)
  }
  if (b.state !== 'running') throw new Refusal(503, b.error || phrase('The sandbox did not start again. Try again.'))
}

/**
 * Whether the person agrees to wake a suspended sandbox, which bills its cloud
 * computer again: the UI's spend question (spend.tsx `useWake`). Nothing here
 * wakes one without it.
 */
export type Wake = () => Promise<boolean>

/**
 * What the page passes where nobody pressed anything (a door or a preview opening as the page loads, or reopening
 * after the sandbox stopped): it never wakes one, so a remembered yes cannot wake a workspace by a page being open.
 * The suspended state says so, beside its Wake.
 */
export const stay: Wake = async () => false

/** Asks what needs a running sandbox; a parked one is resumed first, only on the person's yes. */
async function running<T>(t: Target, id: string, ask: () => Promise<T>, agree: Wake): Promise<T> {
  try {
    return await ask()
  } catch (e) {
    if (!(e instanceof Refusal) || e.status !== 409) throw e
    if (!(await agree())) throw new Refusal(409, phrase('The cloud workspace is suspended. Wake it to go on.'))
    await up(t, id)
    return ask()
  }
}

/** A shell's tmux session name: the sandbox reattaches to it when the door opens again. */
export const NAME = /^[A-Za-z0-9_][A-Za-z0-9_-]{0,63}$/

/** Where to frame one door, with a fresh ticket in its address. */
export async function door(t: Target, id: string, which: Door, agree: Wake, session = ''): Promise<string> {
  const r = await running(t, id, () => call<{ url?: unknown }>(t, 'POST', `/v1/sandbox/${seg(id)}/${which}/ticket`), agree)
  const path = typeof r?.url === 'string' ? r.url : ''
  if (!path.startsWith('/v1/')) throw new Refusal(502, phrase('The sandbox answered with no address to open'))
  const arg = which === 'terminal' && NAME.test(session) ? `&arg=${session}` : ''
  return `${t.api}${path}${arg}`
}

export interface Port {
  port: number
  host: string
}

/** What listens in the sandbox, ascending. */
export async function ports(t: Target, id: string): Promise<Port[]> {
  const r = await call<{ ports?: unknown }>(t, 'GET', `/v1/sandbox/${seg(id)}/ports`)
  const list = Array.isArray(r?.ports) ? r.ports : []
  return list
    .map((p) => (p && typeof p === 'object' ? (p as Record<string, unknown>) : {}))
    .filter((p) => typeof p.port === 'number' && p.port > 0 && p.port < 65536)
    .map((p) => ({
      port: p.port as number,
      host: typeof p.host === 'string' ? p.host : '',
    }))
}

/**
 * An address that opens one port in a browser: its own origin, with a single-use
 * ticket that sets the preview's cookie and lands on its root.
 */
export async function preview(t: Target, id: string, port: number, agree: Wake): Promise<string> {
  const r = await running(
    t,
    id,
    () =>
      call<{ url?: unknown }>(t, 'POST', `/v1/sandbox/${seg(id)}/preview`, {
        port,
      }),
    agree,
  )
  let url: URL
  try {
    url = new URL(typeof r?.url === 'string' ? r.url : '')
  } catch {
    throw new Refusal(502, phrase('The sandbox answered with no address to open'))
  }
  if (url.protocol !== 'https:') throw new Refusal(502, phrase('The sandbox answered with no address to open'))
  return url.href
}

/**
 * Where a server started from the Preview keeps its process group and its output:
 * under /tmp, as a run keeps its own files out of the checkout it commits.
 */
export const SERVE_DIR = '/tmp/hanzo-preview'

/**
 * The shell that starts the app's server for the Preview, detached in a session of
 * its own so it outlives the one command that started it. A server a run starts
 * ends with the run (the agent's harness takes it down), so the Preview starts one
 * again. It runs the codebase's start command when its environment names one ($1),
 * else the package's `dev` script, or `start`, by the package manager its lockfile
 * names, with the environment's secrets sourced as a run sources them (cloud
 * apps/coding envPath). A server this started before is stopped first, its whole
 * group. It prints what it started, or exits 3 saying why nothing could be.
 */
export const SERVE = [
  `d=${SERVE_DIR}`,
  'mkdir -p "$d"',
  'if [ -f /tmp/hanzo-run/env ]; then . /tmp/hanzo-run/env; fi',
  'cmd=$1',
  'if [ -z "$cmd" ]; then',
  '  [ -f package.json ] || { echo no-package; exit 3; }',
  '  if [ -f pnpm-lock.yaml ]; then pm=pnpm; elif [ -f yarn.lock ]; then pm=yarn; elif [ -f bun.lock ] || [ -f bun.lockb ]; then pm=bun; else pm=npm; fi',
  `  run=$(node -e 'const s=require("./package.json").scripts||{};process.stdout.write(s.dev?"dev":s.start?"start":"")' 2>/dev/null)`,
  '  [ -n "$run" ] || { echo no-script; exit 3; }',
  '  cmd="$pm run $run"',
  'fi',
  'old=$(cat "$d/pid" 2>/dev/null)',
  'if [ -n "$old" ] && kill -0 "$old" 2>/dev/null; then kill -TERM "-$old" 2>/dev/null || kill -TERM "$old" 2>/dev/null; sleep 1; fi',
  'if command -v setsid >/dev/null 2>&1; then setsid nohup sh -c "$cmd" </dev/null >"$d/log" 2>&1 & else nohup sh -c "$cmd" </dev/null >"$d/log" 2>&1 & fi',
  'echo $! >"$d/pid"',
  'printf %s "$cmd"',
].join('\n')

/** Whether the server the Preview started still runs (`up` or `down`), then its output's last lines. */
export const SERVED = [
  `d=${SERVE_DIR}`,
  'p=$(cat "$d/pid" 2>/dev/null)',
  'if [ -n "$p" ] && kill -0 "$p" 2>/dev/null; then echo up; else echo down; fi',
  'tail -n 40 "$d/log" 2>/dev/null',
].join('\n')

/** Why there was nothing to start, as SERVE says it. */
const UNSTARTED: Record<string, string> = {
  'no-package': phrase('This checkout has no package.json to start. Set a start command in its Environment.'),
  'no-script': phrase('Its package.json has no dev or start script. Set a start command in its Environment.'),
}

interface Ran {
  code: number
  out: string
  err: string
}

/** One command in the sandbox, as an argument vector: its exit code and output. A failed command is an answer, not a refusal. */
async function exec(t: Target, id: string, argv: string[], timeoutSec: number): Promise<Ran> {
  const r = (await call<Record<string, unknown>>(t, 'POST', `/v1/sandbox/${seg(id)}/exec`, { argv, timeoutSec })) ?? {}
  return {
    code: typeof r.exitCode === 'number' ? r.exitCode : -1,
    out: typeof r.stdout === 'string' ? r.stdout : '',
    err: typeof r.stderr === 'string' ? r.stderr : '',
  }
}

/**
 * Start the app's server in the sandbox, detached, and answer the command it runs:
 * `start` when the codebase's environment names one, else its package's dev script.
 * A parked sandbox is resumed first, on the person's yes.
 */
export async function serve(t: Target, id: string, agree: Wake, start = ''): Promise<string> {
  const r = await running(t, id, () => exec(t, id, ['sh', '-c', SERVE, 'sh', start.trim()], 30), agree)
  const said = r.out.trim()
  if (r.code === 0 && said) return said
  const why = UNSTARTED[said]
  if (r.code === 3 && why) throw new Refusal(422, why)
  throw new Refusal(502, r.err.trim().split('\n').pop() || phrase('The dev server could not be started.'))
}

export interface Served {
  /** Whether the server the Preview started still runs. */
  up: boolean
  /** The last lines it wrote. */
  log: string
}

/** Whether the server the Preview started still runs, and what it last wrote. */
export async function served(t: Target, id: string): Promise<Served> {
  const r = await exec(t, id, ['sh', '-c', SERVED], 10)
  const [first = '', ...rest] = r.out.split('\n')
  return { up: first.trim() === 'up', log: rest.join('\n').trimEnd() }
}

export interface Node {
  /** The resolved path inside the sandbox. */
  path: string
  dir: boolean
  /** A directory's entries, bare names, sorted. */
  names: string[]
  /** A file's text, or '' when it is binary or past the view cap. */
  text: string
  binary: boolean
  truncated: boolean
}

/** What a file shows at most. */
const CAP = 1 << 20

/** One path in the sandbox, relative to its working directory. */
export async function read(t: Target, sandbox: string, path: string): Promise<Node> {
  const o = (await call<Record<string, unknown>>(t, 'POST', '/v1/sandbox/read', { id: sandbox, path })) ?? {}
  const dir = o.dir === true
  const names = Array.isArray(o.entries) ? o.entries.filter((n): n is string => typeof n === 'string').sort() : []
  const bytes = !dir && typeof o.data === 'string' ? decode(o.data) : new Uint8Array()
  const binary = bytes.includes(0)
  const truncated = bytes.length > CAP
  return {
    path: typeof o.path === 'string' ? o.path : path,
    dir,
    names,
    text: binary || truncated ? '' : new TextDecoder().decode(bytes),
    binary,
    truncated,
  }
}

function decode(b64: string): Uint8Array {
  const bin = atob(b64)
  const out = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i)
  return out
}
