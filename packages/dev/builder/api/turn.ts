/**
 * What a run's events SAY, as text a person reads.
 *
 * A curated projection, never a dump: an event carries the machinery a run
 * happened on — hosts, paths, whole file bodies — and printing it would put
 * someone's filesystem in front of anyone who can read the run. Each kind gets a
 * sentence built from the few fields that describe the outcome; the rest is
 * dropped by omission, because a projection cannot leak a field it never reads.
 *
 * Every string here is rendered as TEXT by the transcript. Nothing is HTML.
 *
 * The coding plane's vocabulary (apps/coding): `status` for a lifecycle move
 * (started, routed, done, error, stopped, paused), `tool-call` for a step of the
 * run itself, `event` for one line of the agent's own structured narration (the
 * harness's JSON stream: its messages, reasoning, commands, edits, tool calls and
 * plan), and `log` for raw output, which only the Terminal's log shows. A
 * person's steering is a `control` {command, message}. A run on a project with a
 * site ends `done` with `live` when it published what it pushed, or `unpublished`
 * saying why it did not.
 *
 * `cards` is the transcript as it is drawn: the run's steps and the agent's
 * items, never its log.
 *
 * The sentences are this client's, said in the language of the `t` a caller
 * hands (@hanzo/i18n; English without one); what the run and the agent said
 * is drawn as given. A step's name is a name the transcript keys on (`Error`
 * opens itself), so it stays English, marked (`phrase`), and the transcript
 * draws it through `t`.
 */
import { phrase } from '@hanzo/i18n'
import type { Say } from '@hanzo/i18n/react'

import { english } from '../say.ts'
import { actions, called, counted, edited, explored, explores, shown, type Changed, type Explore } from './cli.ts'
import type { Event } from './sessions.ts'

const str = (v: unknown): string => (typeof v === 'string' ? v : '')

type Said = Extract<Card, { kind: 'said' }>

const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})

/** The payload as an object. A live frame may carry it as a JSON string. */
export function decode(payload: unknown): Record<string, unknown> | string | null {
  if (payload && typeof payload === 'object') return payload as Record<string, unknown>
  if (typeof payload !== 'string') return null
  const text = payload.trim()
  if (!text.startsWith('{')) return payload
  // JSON that opens with a brace is an object; anything else is prose that opens with one.
  try {
    return JSON.parse(text) as Record<string, unknown>
  } catch {
    return payload
  }
}

/** A file's name, never the host path it sat at. */
const base = (path: string): string => {
  const cut = path.replace(/\/+$/, '')
  return cut.slice(cut.lastIndexOf('/') + 1)
}

/**
 * A finished run's line, by whether it pushed a branch (Done when it did not),
 * its pull request (none, one that could not be opened, its address) and its
 * site (none, not published and why, published): each a whole line, so a
 * language puts the parts in its own order.
 */
const FINISHED: Record<'pushed' | 'done', Record<'none' | 'broke' | 'pr', Record<'none' | 'out' | 'live', string>>> = {
  pushed: {
    none: { none: phrase('Pushed {branch}'), out: phrase('Pushed {branch} — not published: {why}'), live: phrase('Pushed {branch} — published') },
    broke: {
      none: phrase('Pushed {branch} — the pull request could not be opened'),
      out: phrase('Pushed {branch} — the pull request could not be opened — not published: {why}'),
      live: phrase('Pushed {branch} — the pull request could not be opened — published'),
    },
    pr: { none: phrase('Pushed {branch} — {pr}'), out: phrase('Pushed {branch} — {pr} — not published: {why}'), live: phrase('Pushed {branch} — {pr} — published') },
  },
  done: {
    none: { none: phrase('Done'), out: phrase('Done — not published: {why}'), live: phrase('Done — published') },
    broke: {
      none: phrase('Done — the pull request could not be opened'),
      out: phrase('Done — the pull request could not be opened — not published: {why}'),
      live: phrase('Done — the pull request could not be opened — published'),
    },
    pr: { none: phrase('Done — {pr}'), out: phrase('Done — {pr} — not published: {why}'), live: phrase('Done — {pr} — published') },
  },
}

function status(b: Record<string, unknown>, mode: string, t: Say): string {
  const branch = str(b.branch)
  switch (str(b.status)) {
    case 'started':
      return branch ? t('Started on {branch}', { branch }) : t('Started')
    case 'routed':
      return t('Sent to a machine')
    case 'done': {
      // A plan's answer IS its final status: the run read and wrote nothing.
      // Whether the run planned is the RECORD's word — any member of the org
      // can append an event saying `mode: plan`, and its text is not an answer.
      // A setup is a plan put to one question, and answers the same way.
      if (mode === 'plan') return str(b.plan) || t('Planned — the run answered with no plan')
      if (mode === 'setup') return str(b.plan) || t('Set up — the run answered with no environment')
      if (b.changed === false) return t('Done — nothing to change')
      const pr = str(b.pr)
      // A run on a project with a site publishes what it pushed, or says why not.
      const why = str(b.unpublished)
      const line = FINISHED[branch ? 'pushed' : 'done'][str(b.prError) ? 'broke' : pr ? 'pr' : 'none'][why ? 'out' : str(b.live) ? 'live' : 'none']
      return t(line, { branch, pr, why })
    }
    case 'error':
      return plain(str(b.error), t)
    case 'stopped':
      return branch && b.changed ? t('Stopped — work kept on {branch}', { branch }) : t('Stopped')
    case 'paused':
      return branch && b.changed ? t('Paused — work kept on {branch}', { branch }) : t('Paused')
    case 'followed':
      // A run redirected from a chat thread is carried on by a follow-up (apps/coding steer.go).
      return str(b.next) ? t('Continued in a follow-up run') : t('The follow-up run could not start')
  }
  return str(b.status)
}

/**
 * Whether an error is a model vendor's own words: a provider's name, its rate
 * headers, a key, or the JSON body it answers with. Those never reach the run
 * page — they say whose model it was and nothing a person can act on — so the
 * run says its plain line alone.
 */
export function vendor(error: string): boolean {
  return /open\s?router|anthropic|openai|azure|bedrock|vertex|gemini|mistral|cohere|deepinfra|fireworks|together\.ai|groq|x-ratelimit|upstream|\bprovider\b|\bsk-[A-Za-z0-9_-]{6,}|"(error|code|message)"\s*:/i.test(error)
}

/** An error as the run page may show it: its own words, or the plain line when they are a vendor's. */
export const scrub = (error: string, t: Say = english): string => (vendor(error) ? plain(error, t) : error)

const OURS_RE = /providers_exhausted|every provider refused|temporarily unavailable/i
const FREE_SANDBOXES_RE = /free sandboxes .*are used/i
const FUNDS_RE = /\b402 Payment Required|insufficient (credit|balance)|top up/i

/** Whether an error message indicates insufficient balance / credit / free-sandbox exhaustion (#363/#364). */
export function needsFunds(error: string): boolean {
  if (!error) return false
  if (FREE_SANDBOXES_RE.test(error)) return true
  return !OURS_RE.test(error) && FUNDS_RE.test(error)
}

/** Extract an error reason from a `log` event payload, or null for ordinary/raw log lines (#363/#364). */
function logError(b: Record<string, unknown> | null): string | null {
  if (!b) return null
  const err = str(b.error).trim()
  if (str(b.runId)) {
    return str(b.status) === 'error' || err ? err : null
  }
  // Without runId, exclude raw command/app log shapes so stdout/stderr JSON lines cannot fake a run failure.
  if ('stdout' in b || 'stderr' in b || 'text' in b || 'command' in b || 'level' in b || 'msg' in b) return null
  if (str(b.status) === 'error' || err) return err
  return null
}

/**
 * A run's error as one plain line with what to do, never the log it came from:
 * that goes in the Error step beside it (cards), unless it is a vendor's.
 */
export function plain(error: string, t: Say = english): string {
  if (!error) return t('The run hit an error')
  const ours = OURS_RE.test(error)
  if (FREE_SANDBOXES_RE.test(error)) return t('This plan’s free sandboxes are used for now. Add credit to keep building, or try again later.')
  if (needsFunds(error)) return t('Your balance is out of credit. Add credit, then try again.')
  if (ours || /\b(429 Too Many Requests|503 Service Unavailable)|stream disconnected|retries exhausted|rate limit/i.test(error))
    return t('The model did not answer. Try again in a moment, or pick another model.')
  return t('The run stopped with an error. Try again, or follow up with what to change.')
}

/**
 * One event's sentence, or '' when it says nothing a person needs. `mode` is
 * the run's, from its record: a plan run's final status says the plan.
 */
export function said(e: Pick<Event, 'kind' | 'payload'>, mode = '', t: Say = english): string {
  const body = decode(e.payload)
  if (typeof body === 'string') return body
  if (!body) return ''
  if (e.kind === 'status') return status(body, mode, t)
  for (const key of ['message', 'text', 'content', 'result', 'command']) {
    const v = body[key]
    if (typeof v === 'string' && v) return v
  }
  if (str(body.type) === 'done' && Array.isArray(body.changed)) {
    const names = body.changed.map((f) => base(str(f))).filter(Boolean)
    return names.length === 0
      ? t('Done — no files changed')
      : names.length <= 3
        ? t('Done — changed {files}', { files: names.join(', ') })
        : t('Done — changed {n} files', { n: names.length })
  }
  if (str(body.type) === 'error') return scrub(str(body.error), t) || t('The run hit an error')
  if (e.kind === 'log') {
    const err = logError(body)
    if (err !== null) return scrub(err, t) || t('The run hit an error')
  }
  return str(body.step) || str(body.name)
}

/** One line of the shell: a command the run started, or output that followed it. */
export interface ShellLine {
  role: 'cmd' | 'out'
  text: string
}

/**
 * The shell beside the transcript. A tool call is the command; a log line, and
 * any stdout or stderr the payload carried, is the output. Status stays in the
 * transcript. Each line is capped so a payload that carried a file body cannot
 * fill the pane.
 */
export function shell(events: Pick<Event, 'kind' | 'payload' | 'seq'>[]): ShellLine[] {
  const cap = 4000
  const clip = (s: string) => (s.length > cap ? `${s.slice(0, cap)}…` : s)
  const out: ShellLine[] = []
  const push = (role: ShellLine['role'], text: string) => {
    const line = clip(text)
    if (line) out.push({ role, text: line })
  }
  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    if (e.kind !== 'log' && e.kind !== 'tool-call') continue
    const body = decode(e.payload)
    if (typeof body === 'string') {
      push('out', body)
      continue
    }
    if (!body) continue
    const step = str(body.step)
    const message = str(body.message) || str(body.text) || str(body.command)
    const stdout = str(body.stdout)
    const stderr = str(body.stderr)
    if (e.kind === 'tool-call' && step) push('cmd', step)
    if (message && message !== step) push('out', message)
    if (stdout) push('out', stdout)
    if (stderr) push('out', stderr)
  }
  return out.slice(-400)
}

/** A step the run named. Done is a step whose status has settled. */
export interface StepLine {
  name: string
  done: boolean
}

/**
 * Tool calls, in the order they were first named. A later status for the same
 * step wins, and a run's steps follow one another, so a step another has
 * followed is over. `exit` is not a step: it is how the sandbox says one command
 * in it ended (apps/sandbox work.go).
 */
export function steps(events: Pick<Event, 'kind' | 'payload' | 'seq'>[]): StepLine[] {
  const order: string[] = []
  const done = new Map<string, boolean>()
  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    if (e.kind !== 'tool-call') continue
    const body = decode(e.payload)
    if (!body || typeof body === 'string') continue
    const name = str(body.step)
    if (!name || name === 'exit') continue
    if (!done.has(name)) order.push(name)
    const status = str(body.status)
    done.set(name, status === 'ok' || status === 'done' || status === 'error')
  }
  return order.map((name, i) => ({ name, done: i < order.length - 1 || done.get(name) === true }))
}

export interface Outcome {
  /** The last lifecycle status the run narrated, or ''. */
  status: string
  /**
   * That the pull request could not be opened, as narrated, or ''. Shown as a
   * fact only — its text comes from an event any member can write, so it is
   * never put on screen as the reason.
   */
  problem: string
  /**
   * The place in the run (its `seq`) of the latest status that said its work
   * went live on its project's site, or 0. It is the cue to read the project
   * again; the address shown is the project's own, never one an event names.
   */
  published: number
}

/**
 * What the run narrated about how it ended, latest winning. Status text only:
 * the branch and the pull request are read from the run's record (`pull`).
 */
/** Whether an event starts or progresses a turn after an earlier turn's error (#363/#364). */
function resumesTurn(kind: string, b: Record<string, unknown> | null): boolean {
  if (kind === 'progress' || kind === 'spawn') return true
  if (kind === 'message') return true
  if (kind === 'tool-call') return true
  if (kind === 'event' && b) {
    const t = str(b.type)
    return t !== 'error' && t !== 'turn.failed'
  }
  return false
}

export function outcome(events: Pick<Event, 'kind' | 'payload' | 'seq'>[]): Outcome {
  const out: Outcome = { status: '', problem: '', published: 0 }
  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    const decoded = decode(e.payload)
    const b = decoded && typeof decoded === 'object' ? decoded : null
    if (e.kind === 'log') {
      if (logError(b) !== null) out.status = 'error'
      continue
    }
    if (e.kind === 'status') {
      if (!b) continue
      out.status = str(b.status) || out.status
      out.problem = str(b.prError) || out.problem
      if (str(b.live)) out.published = e.seq
      continue
    }
    if (out.status === 'error' && resumesTurn(e.kind, b)) out.status = ''
  }
  return out
}

/** Why the run failed, as its last status or error log says it, or '' when that status is not a failure. */
export function failure(events: Pick<Event, 'kind' | 'payload' | 'seq'>[], t: Say = english): string {
  let out = ''
  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    const decoded = decode(e.payload)
    const b = decoded && typeof decoded === 'object' ? decoded : null
    if (e.kind === 'log') {
      const err = logError(b)
      if (err !== null) out = scrub(err, t) || t('The run hit an error')
      continue
    }
    if (e.kind === 'status') {
      if (!b || !str(b.status)) continue
      out = str(b.status) === 'error' ? scrub(str(b.error), t) || t('The run hit an error') : ''
      continue
    }
    if (out && resumesTurn(e.kind, b)) out = ''
  }
  return out
}

/** The turns, ordered and deduped: a recorded read and the live feed overlap. */
export function merge<T extends Pick<Event, 'id' | 'seq'>>(...lists: T[][]): T[] {
  const seen = new Map<string, T>()
  for (const list of lists) for (const e of list) seen.set(e.seq ? `s${e.seq}` : `i${e.id}`, e)
  return [...seen.values()].sort((a, b) => a.seq - b.seq)
}

/** The actor, short enough to label a block: `hanzo/2d4d67ab`, not the whole subject. */
export function who(actor: string): string {
  const cut = actor.indexOf('/')
  if (cut === -1) return actor.split('-')[0] || actor
  const head = actor.slice(cut + 1).split('-')[0]!
  return head ? `${actor.slice(0, cut)}/${head}` : actor
}

/**
 * A pull request address the builder will draw as a link: https, on GitHub or
 * the platform's own git, shaped like a pull request, and IN THE RUN'S OWN
 * REPOSITORY (`repo`, `owner/name`). Anything else is ''. Read from the run's
 * record, which the coding service writes — never from an event, which any
 * member of the org can append — and bound to the repository, because a record
 * can be moved into a project it did not work on.
 */
export function pull(url: string, repo: string): { href: string; label: string } {
  const m = /^https:\/\/(?:github\.com|git\.hanzo\.ai)\/([\w.-]+)\/([\w.-]+)\/pulls?\/(\d+)\/?$/.exec(url)
  if (!m || !repo || `${m[1]}/${m[2]}`.toLowerCase() !== repo.toLowerCase()) return { href: '', label: '' }
  return { href: url, label: `#${m[3]}` }
}

/** How a card's work went. One still running once the run has ended was cut off. */
export type Ran = 'running' | 'done' | 'error' | 'cancelled'

/**
 * One piece of the transcript, drawn by what it is.
 *
 *   said   prose: the agent's messages (markdown), or a person's steering words
 *   think  the agent's reasoning, folded until opened
 *   shell  a command the agent ran, and what it printed: the CLI's "Ran"
 *   explore  reads, listings and searches, consecutive ones together: the CLI's "Explored"
 *   edit   files the agent changed, with the lines each adds and removes; the diff is the run's Git tab
 *   step   a step of the run itself (lease, clone, install, push), or a tool the agent called
 *   todo   the agent's plan, as a checklist it ticks
 *   note   a lifecycle line: started, pushed, stopped
 *   plan   a plan run's answer, which can be approved into a build
 *   run    a coding run a conversation's turn started (talk.ts), opened on its own page
 *   ask    a question the run asked its person, with its options, and the answer once given
 */
export type Card =
  | { kind: 'said'; key: string; who: 'person' | 'agent'; text: string }
  | { kind: 'think'; key: string; text: string }
  | { kind: 'shell'; key: string; command: string; output: string; ran: Ran }
  | { kind: 'explore'; key: string; calls: Explore[]; ran: Ran }
  | { kind: 'edit'; key: string; files: Changed[]; ran: Ran }
  | { kind: 'step'; key: string; name: string; detail: string; output: string; ran: Ran }
  | { kind: 'todo'; key: string; items: { text: string; done: boolean }[] }
  /** `step`: a line about the run getting going (started, sent to a machine), the process rather than its end. */
  | { kind: 'note'; key: string; text: string; step?: true; funds?: true }
  | { kind: 'plan'; key: string; text: string }
  | { kind: 'run'; key: string; session: string }
  | { kind: 'ask'; key: string; question: string; text: string; options: string[]; answer: string }

/** What the run's own steps are called on a card: English, marked (`phrase`); the transcript draws them through `t`. */
const NAMES: Record<string, string> = {
  lease: phrase('Sandbox'),
  workspace: phrase('Workspace'),
  clone: phrase('Clone'),
  branch: phrase('Branch'),
  secrets: phrase('Secrets'),
  install: phrase('Install'),
  start: phrase('Start'),
  skills: phrase('Skills'),
  artifacts: phrase('Save artifacts'),
  push: phrase('Push'),
  kept: phrase('Keep the sandbox'),
}

/** Steps another card already says: `leased` finishes the lease, `done` is the final status, `ended` and `exit` are housekeeping. */
const QUIET = new Set(['leased', 'done', 'ended', 'exit'])

const ranOf = (status: string): Ran => (status === 'ok' || status === 'done' ? 'done' : status === 'error' ? 'error' : 'running')

/** How an agent's item went: in progress, finished, or failed or declined. */
function itemRan(item: Record<string, unknown>): Ran {
  switch (str(item.status)) {
    case 'in_progress':
      return 'running'
    case 'failed':
      return 'error'
    case 'declined':
      return 'cancelled'
  }
  return typeof item.exit_code === 'number' && item.exit_code !== 0 ? 'error' : 'done'
}

/** A path in the workspace, without the sandbox's working directory above it. */
const rel = (path: string): string => path.replace(/^\/work\//, '')

/**
 * One item of the agent's narration as a card, or null when it says nothing a
 * person reads. `key` is the card's, kept as the item is updated.
 */
function item(it: Record<string, unknown>, key: string): Card | null {
  switch (str(it.type)) {
    case 'agent_message':
      return str(it.text) ? { kind: 'said', key, who: 'agent', text: str(it.text) } : null
    case 'reasoning':
      return str(it.text) ? { kind: 'think', key, text: str(it.text) } : null
    case 'command_execution': {
      // What the command did is the agent's own parse of it (cli.ts): exploration, or a command it ran.
      const did = actions(it.command_actions)
      if (explores(did)) return { kind: 'explore', key, calls: [{ actions: did, exit: typeof it.exit_code === 'number' ? it.exit_code : null }], ran: itemRan(it) }
      return { kind: 'shell', key, command: shown(str(it.command)), output: str(it.aggregated_output), ran: itemRan(it) }
    }
    case 'file_change': {
      const changes = Array.isArray(it.changes) ? it.changes : []
      const files = changes
        .map((c): Changed => {
          const o = obj(c)
          const kind = o.kind === 'add' || o.kind === 'delete' ? o.kind : 'update'
          const n = (v: unknown) => (typeof v === 'number' ? v : null)
          return { path: rel(str(o.path)), kind, added: n(o.added), removed: n(o.removed) }
        })
        .filter((f) => f.path)
      return { kind: 'edit', key, files, ran: itemRan(it) }
    }
    case 'mcp_tool_call': {
      const ran = itemRan(it)
      const said = Array.isArray(obj(it.result).content) ? (obj(it.result).content as unknown[]).map((c) => str(obj(c).text)).filter(Boolean).join('\n') : ''
      return {
        kind: 'step',
        key,
        name: ran === 'running' ? phrase('Calling') : phrase('Called'),
        detail: called(str(it.server), str(it.tool), it.arguments),
        output: str(obj(it.error).message) || said,
        ran,
      }
    }
    case 'web_search':
      return { kind: 'step', key, name: phrase('Search'), detail: str(it.query), output: '', ran: 'done' }
    case 'todo_list': {
      const items = Array.isArray(it.items) ? it.items : []
      return { kind: 'todo', key, items: items.map((x) => ({ text: str(obj(x).text), done: obj(x).completed === true })).filter((x) => x.text) }
    }
  }
  return null
}

/**
 * A run's events, as the cards its transcript draws. `mode` is the run's, from
 * its record: a plan's final status is its plan, and nothing else draws a plan.
 *
 * The run's own steps arrive as `tool-call` {step, message, status}. The agent
 * narrates itself as `event` lines: an item is started, updated and completed
 * under one id, and its card is drawn once and kept current. Ids count from zero
 * in each agent process, so an id is the agent's within the thread it started. A
 * turn that fails is one plain line with the detail folded beneath it, drawn once
 * however many times it is said; an `error` on its own is the agent retrying and
 * is drawn only if the turn then fails. The final status carries the agent's
 * answer, drawn once. `log` is never drawn here.
 */
export function cards(events: Pick<Event, 'kind' | 'payload' | 'seq'>[], mode = '', t: Say = english): Card[] {
  const out: Card[] = []
  let seq = 0
  let n = 0
  const mint = () => `${seq}.${n++}`
  let phase: Extract<Card, { kind: 'step' }> | null = null
  const items = new Map<string, number>()
  let thread = 0
  let said = ''
  const answers = new Set<string>()
  const steered = new Set<Said>()
  const person = (text: string) => {
    const c: Said = { kind: 'said', key: mint(), who: 'person', text }
    out.push(c)
    return c
  }
  const failures = new Set<string>()
  const spawned = new Set<string>()
  const asked = new Map<string, number>()
  const failed = (message: string) => {
    if (failures.has(message)) return
    failures.add(message)
    const rawStep = Boolean(message && !vendor(message))
    const noteFunds = !rawStep && needsFunds(message)
    out.push({ kind: 'note', key: mint(), text: plain(message, t), ...(noteFunds ? { funds: true as const } : {}) })
    // The reason, read whole, when it is the run's own; a vendor's words stay off the page.
    if (rawStep) out.push({ kind: 'step', key: mint(), name: phrase('Error'), detail: '', output: message, ran: 'error' })
  }
  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    seq = e.seq
    n = 0
    const body = decode(e.payload)
    const b = body && typeof body === 'object' ? body : null
    switch (e.kind) {
      case 'event': {
        if (!b) break
        const type = str(b.type)
        if (type === 'thread.started') thread++
        if (type === 'error') said = str(b.message)
        if (type === 'turn.failed') failed(str(obj(b.error).message) || said)
        if (type !== 'item.started' && type !== 'item.updated' && type !== 'item.completed') break
        const it = obj(b.item)
        const id = str(it.id) && `${thread}:${str(it.id)}`
        const at = id ? items.get(id) : undefined
        const card = item(it, at === undefined ? mint() : out[at]!.key)
        if (!card) break
        if (at === undefined) {
          if (id) items.set(id, out.length)
          out.push(card)
        } else out[at] = card
        break
      }
      case 'tool-call': {
        if (!b) break
        const step = str(b.step)
        const message = str(b.message)
        if (step === 'exit') {
          // One command in the sandbox ended: `exit 0`, `exit 1`, or `ended: …`.
          if (phase && phase.ran === 'running') phase.ran = message === 'exit 0' ? 'done' : 'error'
          break
        }
        if (step === 'leased') {
          const lease = out.find((c): c is Extract<Card, { kind: 'step' }> => c.kind === 'step' && c.name === NAMES.lease)
          if (lease) lease.ran = 'done'
        }
        if (!step || QUIET.has(step)) break
        const name = NAMES[step] ?? step.charAt(0).toUpperCase() + step.slice(1)
        if (phase && phase.name === name) {
          phase.ran = ranOf(str(b.status))
          if (message) phase.detail = message
          break
        }
        phase = { kind: 'step', key: mint(), name, detail: message, output: '', ran: ranOf(str(b.status)) }
        out.push(phase)
        break
      }
      case 'status': {
        phase = null
        if (!b) break
        // A question the run asked its person (apps/agents ask.go), and its answer, drawn on the question.
        if (str(b.status) === 'asked') {
          const options = (Array.isArray(b.options) ? b.options : []).map((o) => str(obj(o).label)).filter(Boolean)
          asked.set(str(b.question), out.length)
          out.push({ kind: 'ask', key: mint(), question: str(b.question), text: str(b.text), options, answer: '' })
          break
        }
        if (str(b.status) === 'answered') {
          const at = asked.get(str(b.question))
          const card = at === undefined ? undefined : out[at]
          if (card?.kind === 'ask') out[at!] = { ...card, answer: str(b.answer) }
          break
        }
        const done = str(b.status) === 'done'
        const text = str(b.plan) || str(b.answer)
        if (done && (mode === 'plan' || mode === 'setup') && text) {
          answers.add(text.trim())
          out.push(mode === 'plan' ? { kind: 'plan', key: mint(), text } : { kind: 'said', key: mint(), who: 'agent', text })
          break
        }
        const answer = str(b.answer).trim()
        if (answer) {
          answers.add(answer)
          out.push({ kind: 'said', key: mint(), who: 'agent', text: answer })
        }
        if (str(b.status) === 'error') {
          failed(str(b.error))
          break
        }
        const line = status(b, mode, t)
        const going = str(b.status) === 'started' || str(b.status) === 'routed'
        if (line) out.push(going ? { kind: 'note', key: mint(), text: line, step: true } : { kind: 'note', key: mint(), text: line })
        break
      }
      case 'control': {
        if (!b) break
        const command = str(b.command)
        const said = str(b.message)
        if (command === 'message' && said) steered.add(person(said))
        else if (command === 'pause') out.push({ kind: 'note', key: mint(), text: t('Pause asked') })
        else if (command === 'resume') out.push({ kind: 'note', key: mint(), text: t('Resume asked') })
        else if (command === 'stop') out.push({ kind: 'note', key: mint(), text: t('Stop asked') })
        break
      }
      case 'message': {
        const text = typeof body === 'string' ? body : str(b?.text) || str(b?.content) || str(b?.message)
        if (!text) break
        if (str(b?.role) === 'user') person(text)
        else out.push({ kind: 'said', key: mint(), who: 'agent', text })
        break
      }
      case 'spawn': {
        // A conversation's turn started a coding run, or follows the one that carried it on.
        const session = str(b?.session)
        if (!session || spawned.has(session)) break
        spawned.add(session)
        out.push({ kind: 'run', key: mint(), session })
        break
      }
      case 'log': {
        // A conversation's turn or coding run that failed says why on its record of the run.
        // A turn the platform refuses before it starts (no balance) carries the reason and no status.
        const err = logError(b)
        if (err !== null) failed(err)
        break
      }
    }
  }
  // The answer a status carries is also the agent's last message: each is drawn once, as the answer.
  const kept = new Set<string>()
  for (let i = out.length - 1; i >= 0; i--) {
    const c = out[i]!
    if (!((c.kind === 'said' && c.who === 'agent') || c.kind === 'plan') || !answers.has(c.text.trim())) continue
    if (kept.has(c.text.trim())) out.splice(i, 1)
    kept.add(c.text.trim())
  }
  // The ask opens the conversation, ahead of the steps that set the run up.
  const ask = out.findIndex((c) => c.kind === 'said' && c.who === 'person' && !steered.has(c))
  if (ask > 0) out.unshift(...out.splice(ask, 1))
  return gather(out)
}

/**
 * Consecutive exploration as the CLI keeps it: one Explored block, open across the
 * reads, listings and searches that follow each other — the agent's reasoning
 * between them included, which stays where it was said, after the block.
 */
function gather(cards: Card[]): Card[] {
  const out: Card[] = []
  let open: Extract<Card, { kind: 'explore' }> | null = null
  for (const c of cards) {
    if (c.kind === 'explore') {
      if (open) {
        open.calls = [...open.calls, ...c.calls]
        if (c.ran === 'running') open.ran = 'running'
        continue
      }
      open = { ...c, calls: [...c.calls] }
      out.push(open)
      continue
    }
    if (c.kind !== 'think') open = null
    out.push(c)
  }
  return out
}

/**
 * Cards as the CLI prints them, line for line: what a run's transcript reads as in
 * `dev`'s terminal. The transcript draws the same words (transcript.tsx); this is
 * them as text, which the parity tests hold to the CLI's own.
 */
export function printed(cards: Card[], live = false): string {
  const lines: string[] = []
  const under = (first: string[], rest = '    ') => first.map((l, i) => `${i === 0 ? '  └ ' : rest}${l}`)
  for (const c of cards) {
    switch (c.kind) {
      case 'explore':
        lines.push(`• ${c.ran === 'running' && live ? 'Exploring' : 'Explored'}`, ...under(explored(c.calls).map((l) => `${l.title} ${l.text}`)))
        break
      case 'shell': {
        const output = c.output.replace(/\n+$/, '')
        lines.push(`• ${c.ran === 'running' && live ? 'Running' : 'Ran'} ${c.command}`, ...under(output ? output.split('\n') : ['(no output)']))
        break
      }
      case 'edit':
        lines.push(`• ${edited(c.files)}`)
        if (c.files.length > 1) lines.push(...c.files.map((f) => `  └ ${f.path} ${counted(f)}`.trimEnd()))
        break
      case 'step':
        if (c.name === 'Called' || c.name === 'Calling') lines.push(`• ${c.name} ${c.detail}`)
        break
      case 'said':
        if (c.who === 'agent') lines.push(`• ${c.text}`)
        break
    }
  }
  return lines.join('\n')
}

/** The plan a plan run answered with, from its final status, or ''. */
export function answer(events: Pick<Event, 'kind' | 'payload' | 'seq'>[]): string {
  let out = ''
  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    if (e.kind !== 'status') continue
    const b = decode(e.payload)
    if (b && typeof b === 'object' && str(b.status) === 'done') out = str(b.plan)
  }
  return out
}

export interface Settled {
  /** The run has said how it ended, or that it paused: its work is where it will stay. */
  settled: boolean
  /** That work is on the run's own branch, which a follow-up can start from. */
  pushed: boolean
}

/**
 * Whether the run has kept its work yet, and whether that work is on its
 * branch. A stop or a pause commits and pushes AFTER it is asked, and says so
 * in its status (apps/coding coding.go interrupted): until then a follow-up
 * would clone a branch the push has not reached.
 */
export function settled(events: Pick<Event, 'kind' | 'payload' | 'seq'>[]): Settled {
  const out: Settled = { settled: false, pushed: false }
  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    const decoded = decode(e.payload)
    const b = decoded && typeof decoded === 'object' ? decoded : null
    if (e.kind === 'log') {
      if (logError(b) !== null) {
        out.settled = true
        out.pushed = false
      }
      continue
    }
    if (e.kind !== 'status') {
      if (out.settled && resumesTurn(e.kind, b)) out.settled = false
      continue
    }
    if (!b) continue
    const s = str(b.status)
    if (s === 'done' || s === 'error' || s === 'stopped' || s === 'paused') {
      out.settled = true
      out.pushed = b.changed === true
    } else if (s) {
      out.settled = false
    }
  }
  return out
}

/** The session of the run that carried this one on (its `followed` status), or ''. */
export function followed(events: Pick<Event, 'kind' | 'payload' | 'seq'>[]): string {
  let next = ''
  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    if (e.kind !== 'status') continue
    const b = decode(e.payload)
    if (b && typeof b !== 'string' && str(b.status) === 'followed') next = str(b.next)
  }
  return next
}
