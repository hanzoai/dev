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
        else if (c.name === 'Search') searched = true
        else if (c.name === 'Called' || c.name === 'Calling') tools++
        else if (c.name === 'Push' || c.name === 'Save artifacts') saved = true
        else if (c.name !== KEPT) checked = true
        break
    }
  }
  const parts: string[] = []
  if (setup) parts.push(say('Set up the workspace'))
  if (thought && !commands && !read.size && !changed.size) parts.push(say('thought it through'))
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
