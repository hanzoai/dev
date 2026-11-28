/**
 * THE RESULT FIRST (hanzo-inc/app issues 317 and 321).
 *
 * With Developer mode off, a run reads as what was asked, what the agent said
 * and how it ended. Everything it did on the way there — the commands it ran,
 * the files it read and changed, the sandbox it leased, its reasoning — folds
 * into one line between the turns, saying in plain words what the work was
 * ("Set up the workspace, read 4 files, changed index.html"). That line opens
 * onto the very cards Developer mode draws, so the process stays one press away
 * and nobody has to read a shell line to know what happened.
 *
 * A path a person might still read there is said in words: `~` is "your home
 * folder", `/work` is "your project".
 */
import type { Say } from '@hanzo/i18n/react'

import type { Card, Ran } from './api/turn.ts'
import { english } from './say.ts'

/** A run of process cards between two things a person reads. */
export interface Work {
  kind: 'work'
  /** The first card's key: a run of work keeps its key, and its open state, as it grows. */
  key: string
  cards: Card[]
}

/** The run's own steps that set it up, by the names turn.ts gives them. */
const SETUP = new Set(['Sandbox', 'Workspace', 'Clone', 'Branch', 'Secrets', 'Install', 'Start', 'Skills'])

/** The sandbox kept for its screen and previews once the run is done: housekeeping, said nowhere. */
const KEPT = 'Keep the sandbox'

/**
 * Whether a card is the process — how the agent got there — rather than what a
 * person reads: what was asked, what the agent said, its plan, its question, a
 * coding run it started, how the run ended, and the reason it failed.
 */
export function isProcess(c: Card): boolean {
  switch (c.kind) {
    case 'think':
    case 'shell':
    case 'explore':
    case 'edit':
      return true
    case 'step':
      return c.name !== 'Error'
    case 'note':
      return c.step === true
  }
  return false
}

/** The transcript with each run of process cards gathered into one `Work`. */
export function fold(cards: readonly Card[]): (Card | Work)[] {
  const out: (Card | Work)[] = []
  for (const c of cards) {
    if (!isProcess(c)) {
      out.push(c)
      continue
    }
    const last = out[out.length - 1]
    if (last?.kind === 'work') last.cards.push(c)
    else out.push({ kind: 'work', key: `work:${c.key}`, cards: [c] })
  }
  return out
}

/**
 * How a run of work went: still going while any of it is, cut off when a step
 * was, otherwise done. The kept sandbox is left out: it stays "running" for as
 * long as the sandbox is kept, which is not the work still going.
 */
export function ranOf(cards: readonly Card[], state: (r: Ran) => Ran = (r) => r): Ran {
  const ran = cards.filter((c) => !(c.kind === 'step' && c.name === KEPT)).map((c) => ('ran' in c ? state(c.ran) : 'done'))
  if (ran.includes('running')) return 'running'
  if (ran.includes('cancelled')) return 'cancelled'
  return 'done'
}

// Paths as a shell writes them, at a word's start: the home folder (`~`, `$HOME`,
// `/home/<user>`, `/Users/<user>`, `/root`) and the sandbox's working directory (`/work`).
const HOME = /(^|[\s'"`(=:,])(?:~|\$HOME|\/home\/[\w.-]+|\/Users\/[\w.-]+|\/root)(\/[^\s'"`),;]*)?(?=$|[\s'"`),;:.])/g
const WORK = /(^|[\s'"`(=:,])\/work(\/[^\s'"`),;]*)?(?=$|[\s'"`),;:.])/g

/**
 * A line with the paths in it said in words: `~` is "your home folder",
 * `~/notes/todo.md` is "notes/todo.md in your home folder", `/work` is "your
 * project" and `/work/src/app.ts` is "src/app.ts". Anything else is left as written.
 */
export function plainPath(text: string, say: Say = english): string {
  // The path inside a tail, and the sentence's own punctuation after it, which stays where it was written.
  const split = (tail: string | undefined): [string, string] => {
    const written = (tail ?? '').replace(/^\/+/, '')
    const stop = /[.!?:]*$/.exec(written)?.[0] ?? ''
    return [written.slice(0, written.length - stop.length).replace(/\/+$/, ''), stop]
  }
  return text
    .replace(HOME, (_, lead: string, tail?: string) => {
      const [inside, stop] = split(tail)
      return lead + (inside ? say('{path} in your home folder', { path: inside }) : say('your home folder')) + stop
    })
    .replace(WORK, (_, lead: string, tail?: string) => {
      const [inside, stop] = split(tail)
      return lead + (inside || say('your project')) + stop
    })
}

/** A text as a pattern that matches it literally. */
const literal = (text: string): string => text.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')

/**
 * What an agent said after working in a folder, said in words. Paths inside the folder are named by it
 * ("checkout.html in shop"), the folder's own path by its name, and the home folder and the sandbox's `/work`
 * as `plainPath` says them. Anything else is left as it was written.
 */
export function plainAnswer(text: string, folder: { path: string; name: string }, say: Say = english): string {
  const root = folder.path.replace(/[/\\]+$/, '')
  if (!root) return plainPath(text, say)
  const own = new RegExp(`(^|[\\s'"\`(=:,])${root.split(/[/\\]+/).map(literal).join('[/\\\\]+')}([/\\\\][^\\s'"\`),;]*)?(?=$|[\\s'"\`),;:.!?])`, 'g')
  const inside = text.replace(own, (_, lead: string, tail?: string) => {
    const written = (tail ?? '').replace(/^[/\\]+/, '')
    const stop = /[.!?:]*$/.exec(written)?.[0] ?? ''
    const rest = written.slice(0, written.length - stop.length).replace(/[/\\]+$/, '')
    return lead + (rest ? say('{path} in {folder}', { path: rest, folder: folder.name }) : folder.name) + stop
  })
  return plainPath(inside, say)
}

/** A file's name as a person reads it: the last part of its path, said in words. */
const fileName = (path: string, say: Say): string => {
  const plain = plainPath(path, say)
  return plain === path ? path.split('/').filter(Boolean).pop() || path : plain
}

/** The high-level activity category of a work card or group, used for crisp category icons. */
export type WorkCategory = 'command' | 'read' | 'edit' | 'tool' | 'search' | 'think' | 'setup'

/** Whether a step card represents loading/discovering tools (e.g. ToolSearch / MCP discovery). */
export const isToolLoad = (c: Extract<Card, { kind: 'step' }>): boolean =>
  c.name === 'Loaded tools' || c.name === 'Loading tools' || /^tool[_-]?search\b|^load[_-]?tool\b|^list[_-]?tools\b/i.test(c.detail.trim())

/** Category of one card for icon selection. */
export function cardCategory(c: Card): WorkCategory {
  switch (c.kind) {
    case 'shell':
      return 'command'
    case 'explore': {
      const actions = c.calls.flatMap((call) => call.actions)
      if (actions.length > 0 && actions.every((a) => a.type === 'search')) return 'search'
      return 'read'
    }
    case 'edit':
      return 'edit'
    case 'think':
      return 'think'
    case 'step':
      if (c.name === 'Search') return 'search'
      if (c.name === 'Called' || c.name === 'Calling' || isToolLoad(c)) return 'tool'
      if (SETUP.has(c.name) || c.name === KEPT) return 'setup'
      return 'tool'
    case 'note':
      return 'setup'
    default:
      return 'command'
  }
}

/**
 * Primary category for a grouped work row so the leading icon always agrees with
 * the active step (while running) or the leading phrase of the summary (when done).
 */
export function categoryOf(cards: readonly Card[], state: (r: Ran) => Ran = (r) => r): WorkCategory {
  const active = [...cards].reverse().find((c) => 'ran' in c && state(c.ran) === 'running' && !(c.kind === 'step' && c.name === KEPT))
  if (active) return cardCategory(active)
  let hasSetup = false
  let hasToolLoad = false
  let hasRead = false
  let hasCommand = false
  let hasSearch = false
  let hasTool = false
  let hasEdit = false
  let hasThink = false
  for (const c of cards) {
    switch (c.kind) {
      case 'think':
        hasThink = true
        break
      case 'shell':
        hasCommand = true
        break
      case 'explore':
        hasRead = true
        break
      case 'edit':
        hasEdit = true
        break
      case 'note':
        hasSetup = true
        break
      case 'step':
        if (SETUP.has(c.name)) hasSetup = true
        else if (isToolLoad(c)) hasToolLoad = true
        else if (c.name === 'Search') hasSearch = true
        else if (c.name === 'Called' || c.name === 'Calling') hasTool = true
        break
    }
  }
  if (hasToolLoad) return 'tool'
  if (hasRead) return 'read'
  if (hasEdit && !hasCommand && !hasSetup) return 'edit'
  if (hasCommand) return 'command'
  if (hasSearch) return 'search'
  if (hasTool) return 'tool'
  if (hasEdit) return 'edit'
  if (hasSetup) return 'setup'
  if (hasThink) return 'think'
  return 'command'
}

/** Compact human duration for a tool call (`340ms`, `1.4s`, `2m 10s`), or `''` when absent. */
export function formatDuration(ms?: number): string {
  if (typeof ms !== 'number' || !Number.isFinite(ms) || ms < 0) return ''
  if (ms < 1000) return `${Math.round(ms)}ms`
  if (ms < 60_000) {
    const tenths = Math.round(ms / 100)
    return tenths < 100 ? `${(tenths / 10).toFixed(1)}s` : `${Math.round(ms / 1000)}s`
  }
  const totalSec = Math.round(ms / 1000)
  const mins = Math.floor(totalSec / 60)
  const secs = totalSec % 60
  return secs > 0 ? `${mins}m ${secs}s` : `${mins}m`
}

const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'] as const
const pad2 = (n: number): string => (n < 10 ? `0${n}` : `${n}`)
const dayKey = (d: Date): string => `${d.getFullYear()}-${d.getMonth()}-${d.getDate()}`

/** Clean centered turn timestamp (`Today 11:09`, `Yesterday 14:22`, `Nov 27 11:09`). */
export function formatTurnTimestamp(iso: string, now: Date = new Date()): string {
  const ms = Date.parse(iso)
  if (!Number.isFinite(ms)) return ''
  const d = new Date(ms)
  const hhmm = `${pad2(d.getHours())}:${pad2(d.getMinutes())}`
  if (dayKey(d) === dayKey(now)) return `Today ${hhmm}`
  const yesterday = new Date(now.getFullYear(), now.getMonth(), now.getDate() - 1)
  if (dayKey(d) === dayKey(yesterday)) return `Yesterday ${hhmm}`
  const md = `${MONTHS[d.getMonth()]!} ${d.getDate()}`
  return d.getFullYear() === now.getFullYear() ? `${md} ${hhmm}` : `${md}, ${d.getFullYear()} ${hhmm}`
}

/**
 * Whether a centered timestamp divider should appear before a turn: when turns
 * cross a calendar day, have a gap >= `gapMs` (default 15m), or when a new user
 * turn resumes after a status/limit/paused notice.
 */
export function shouldShowTimestamp(prevAt?: string, nextAt?: string, afterNotice = false, gapMs = 15 * 60_000): boolean {
  if (!nextAt) return false
  const nextMs = Date.parse(nextAt)
  if (!Number.isFinite(nextMs)) return false
  if (afterNotice) return true
  if (!prevAt) return false
  const prevMs = Date.parse(prevAt)
  if (!Number.isFinite(prevMs)) return false
  if (dayKey(new Date(prevMs)) !== dayKey(new Date(nextMs))) return true
  return nextMs - prevMs >= gapMs
}

/** What one card was doing, in plain words, for the line while the work goes on. */
export function doing(c: Card, say: Say = english): string {
  switch (c.kind) {
    case 'think':
      return say('Thinking it through')
    case 'shell':
      return say('Running a command')
    case 'explore':
      return say('Looking through the files')
    case 'edit':
      return c.files.length === 1 ? say('Changing {file}', { file: fileName(c.files[0]!.path, say) }) : say('Changing files')
    case 'note':
      return say('Starting')
    case 'step':
      if (SETUP.has(c.name)) return say('Setting up the workspace')
      if (isToolLoad(c)) return say('Loading tools')
      if (c.name === 'Search') return say('Searching the web')
      if (c.name === 'Called' || c.name === 'Calling') return say('Using a tool')
      if (c.name === 'Push' || c.name === 'Save artifacts') return say('Saving the work')
      if (c.name === KEPT) return say('Keeping the workspace')
      return say('Checking the work')
  }
  return say('Working')
}

/**
 * What a run of work did, said in one line: "Set up the workspace, read 4
 * files, changed index.html". Counts, never commands or paths.
 */
export function worked(cards: readonly Card[], say: Say = english): string {
  let setup = false
  let thought = false
  let commands = 0
  const read = new Set<string>()
  let looked = false
  const changed = new Set<string>()
  let searched = false
  let loadedTools = 0
  let tools = 0
  let saved = false
  let checked = false
  for (const c of cards) {
    switch (c.kind) {
      case 'think':
        thought = true
        break
      case 'shell':
        commands++
        break
      case 'explore':
        for (const call of c.calls)
          for (const a of call.actions) {
            if (a.type === 'read') read.add(a.name)
            else looked = true
          }
        break
      case 'edit':
        for (const f of c.files) changed.add(f.path)
        break
      case 'note':
        setup = true
        break
      case 'step':
        if (SETUP.has(c.name)) setup = true
        else if (isToolLoad(c)) loadedTools++
        else if (c.name === 'Search') searched = true
        else if (c.name === 'Called' || c.name === 'Calling') tools++
        else if (c.name === 'Push' || c.name === 'Save artifacts') saved = true
        else if (c.name !== KEPT) checked = true
        break
    }
  }
  const parts: string[] = []
  if (setup) parts.push(say('Set up the workspace'))
  if (loadedTools) parts.push(say('loaded tools'))
  if (thought && !commands && !read.size && !changed.size && !loadedTools && !tools && !searched) parts.push(say('thought it through'))
  if (read.size) parts.push(read.size === 1 ? say('read a file') : say('read {n} files', { n: read.size }))
  else if (looked) parts.push(say('looked through the files'))
  if (commands) parts.push(commands === 1 ? say('ran a command') : say('ran {n} commands', { n: commands }))
  if (searched) parts.push(say('searched the web'))
  if (tools) parts.push(tools === 1 ? say('used a tool') : say('used {n} tools', { n: tools }))
  if (changed.size) parts.push(changed.size === 1 ? say('changed {file}', { file: fileName([...changed][0]!, say) }) : say('changed {n} files', { n: changed.size }))
  if (saved) parts.push(say('saved the work'))
  if (checked && !parts.length) parts.push(say('checked the work'))
  const line = parts.join(', ')
  return line ? line.charAt(0).toUpperCase() + line.slice(1) : ''
}
