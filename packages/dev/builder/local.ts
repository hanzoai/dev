/**
 * Working in a folder on this computer, with no repository (hanzo-inc/app issues 309 and 310).
 *
 * A HOST THAT HAS THE COMPUTER says so. Hanzo Desktop can open a folder dialog and run the coding agent in a folder
 * (`dev exec -C <folder>`), so it hands the builder a `Local` (host.tsx); a browser page has no folder of its own and
 * hands none, and New then offers the cloud workspace and the org's machines as before. Nothing here sniffs a user
 * agent: the host says what it has.
 *
 * AUTO IS THE FOLDER THE ASK IS ABOUT, ELSE THE ONE USED LAST (`route`). The folders are the ones Hanzo opened and
 * the project folders the host found on the computer; a person who chose no folder works in the one their words
 * name ("fix the footer in shop" goes to `shop`), and in the one they worked in most recently when their words
 * name none. One who has no folder at all is asked for one, once, on their first send, and that folder is then
 * the one Auto picks. Choosing a folder in the pill keeps it for the next New; choosing Auto again goes back to
 * the rule. The pill says where the run will go as the words are typed, so a person can always say before they send.
 *
 * A run on this computer reads, edits and runs in its folder and nowhere else (the agent's workspace-write sandbox),
 * and it needs no repository: a folder that is a git checkout keeps its history, and one that is not stays one.
 */
import { phrase } from '@hanzo/i18n'

import type { Say } from '@hanzo/i18n/react'

import { english } from './say.ts'

/** A folder on this computer, as the host keeps it. */
export interface Folder {
  /** The absolute path the agent runs in. */
  path: string
  /** Its last segment, or the name the person gave it. */
  name: string
  /** When a run last used it (ms since the epoch); 0 when never. */
  used?: number
}

/** What a run on this computer answers with once it has finished. */
export interface Done {
  ok: boolean
  /** What the agent said at the end: its answer, and what it changed. What a person reads. */
  output: string
  /** What it did on the way: its commands and what they printed. Drawn only in Developer mode. */
  process?: string
}

/** How far the coding agent's setup has got, 0 to 100, while the host fetches it for the first run. */
export type Setup = (percent: number) => void

/** What a host with the computer can do with its folders. */
export interface Local {
  /** The folders this computer has: the ones opened in Hanzo and the project folders found on it (`recent` sorts them). */
  folders: () => readonly Folder[]
  /** Hears when `folders` has changed, as the ones found arrive after the first read; answers how to stop hearing. */
  changed?: (listener: () => void) => () => void
  /** The computer's own folder dialog; null when the person closes it. The folder chosen is kept. */
  pick: () => Promise<Folder | null>
  /** Mark a folder as the one used now, so Auto picks it next. */
  use: (path: string) => void
  /** Run the coding agent in `folder` with `prompt`, and answer once it has finished. `setup` hears the agent being fetched. */
  run: (folder: string, prompt: string, setup?: Setup) => Promise<Done>
  /** Stop the agent being fetched for the first run, when a person no longer wants it; the run then ends as cancelled (`failed`). */
  cancel?: () => Promise<void> | void
  /** This computer's hostname, the name `hanzo link` registers it under: how its own machine is found among the org's (settings/shared.tsx). */
  host?: () => Promise<string>
}

/** The place id New keeps for this computer. A machine's is `tgt_…` and the cloud workspace's is ''. */
export const HERE = 'here'

/** The folders, most recently used first; a folder never used goes last, in the order the host gave. */
export function recent(folders: readonly Folder[]): Folder[] {
  return folders
    .map((f, i) => ({ f, i }))
    .sort((a, b) => (b.f.used ?? 0) - (a.f.used ?? 0) || a.i - b.i)
    .map(({ f }) => f)
}

/** The folder Auto picks: the one used most recently, or null when this computer has opened none. */
export function auto(folders: readonly Folder[]): Folder | null {
  return recent(folders)[0] ?? null
}

/** Names a path says nowhere in particular: where people keep their projects, not which one. */
const COMMON = new Set([
  'users', 'user', 'home', 'documents', 'desktop', 'downloads', 'projects', 'project', 'code', 'src', 'source', 'sources',
  'work', 'dev', 'developer', 'repos', 'repo', 'repositories', 'github', 'git', 'workspace', 'workspaces', 'sites', 'mnt',
  'volumes', 'data', 'the', 'and', 'for', 'new',
])

/** The words of a text: letters and digits, lower case, accents folded; a camelCase name is two words. */
function wordsOf(text: string): string[] {
  return text
    .replace(/([\p{Ll}\p{N}])(\p{Lu})/gu, '$1 $2')
    .normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .split(/[^\p{L}\p{N}]+/u)
    .filter(Boolean)
}

/** The words that tell one folder from another: not a place projects live in, and not a letter or two. */
const telling = (text: string): string[] => wordsOf(text).filter((w) => w.length >= 3 && !COMMON.has(w))

/** Whether the ask says `word`, in the singular or the plural. */
const says = (ask: ReadonlySet<string>, word: string): boolean => ask.has(word) || ask.has(`${word}s`) || (word.endsWith('s') && ask.has(word.slice(0, -1)))

/**
 * The folder an ask is about, else the one used last.
 *
 * A folder is named by its own name first ("shop", "my-shop"; each word of it the ask says counts, and the
 * folder it is all of beats one it is a little of), and then by the folders it sits in (`~/work/acme/web` is
 * about "acme"). The ask is read as words: nothing is guessed from what it means, so the same words always go to
 * the same folder, and a tie goes to the folder used more recently. An ask that names none goes where Auto
 * went before: the folder used last.
 */
export function route(folders: readonly Folder[], ask: string): Folder | null {
  const said = new Set(wordsOf(ask))
  const phrase = ` ${[...said].join(' ')} `
  let best: { folder: Folder; score: Score } | null = null
  for (const folder of recent(folders)) {
    const score = scored(folder, said, phrase)
    if (score[0] > 0 && (!best || beats(score, best.score))) best = { folder, score }
  }
  return best?.folder ?? auto(folders)
}

/** How well an ask names a folder: how it does so (its own name 2, a folder above it 1, not at all 0), how much of the name it says, and how many words. */
type Score = readonly [how: number, share: number, words: number]

const beats = (a: Score, b: Score): boolean => (a[0] !== b[0] ? a[0] > b[0] : a[1] !== b[1] ? a[1] > b[1] : a[2] > b[2])

function scored(folder: Folder, said: ReadonlySet<string>, phrase: string): Score {
  const own = telling(folder.name)
  const hits = own.filter((w) => says(said, w)).length
  const whole = wordsOf(folder.name).join(' ')
  const distinct = wordsOf(folder.name).some((w) => !COMMON.has(w))
  if (hits > 0 || (distinct && phrase.includes(` ${whole} `))) return [2, own.length ? hits / own.length : 1, hits]
  const above = telling(folder.path.replace(/[/\\]+$/, '').split(/[/\\]/).slice(0, -1).join('/')).filter((w) => says(said, w)).length
  return above > 0 ? [1, 0, above] : [0, 0, 0]
}

/**
 * The folder a run goes to: the one chosen (`kept`, a path), while this computer still lists it, else the one the
 * ask is about, else the one used last (`route`). A chosen folder the host no longer lists was removed, and is
 * not where the next run goes.
 */
export function where(folders: readonly Folder[], kept: string, ask = ''): { folder: Folder | null; auto: boolean } {
  const chosen = kept ? folders.find((f) => f.path === kept) : undefined
  if (chosen) return { folder: chosen, auto: false }
  return { folder: route(folders, ask), auto: true }
}

/** A folder's last segment, for a path the host gave no name. */
export function named(path: string): string {
  return path.replace(/[/\\]+$/, '').split(/[/\\]/).pop() || path
}

/** What the host answers once the first-run setup a person cancelled has stopped. */
export const CANCELLED = /^Setup was cancelled\.?$/i

/**
 * Why a run on this computer stopped before it began, in the reader's words. The host fetches the agent itself
 * and words what went wrong in plain sentences (no connection, a download that did not check out, a folder that
 * is gone), so those are said as they came; a host that could not find the agent at all is told to update.
 * What the agent itself printed is its answer, and is shown as it is.
 */
export function failed(e: unknown, say: Say = english): string {
  const why = (e instanceof Error ? e.message : typeof e === 'string' ? e : '').trim()
  // A person's own choice, not a fault: the host says exactly this once the setup they cancelled has stopped.
  if (CANCELLED.test(why)) return say('Setup was cancelled. Send again whenever you like.')
  if (/failed to invoke dev|no such file|os error 2|cannot find/i.test(why)) {
    return say('Hanzo Dev is not set up on this computer yet. Update Hanzo Desktop, then send again.')
  }
  return why || say('The run on this computer could not start. Try again.')
}

/** Words New marks for the reader's language (`phrase`), drawn through `t`. */
export const WORDS = {
  here: phrase('This computer'),
  auto: phrase('Auto'),
  choose: phrase('Choose folder…'),
  pick: phrase('Choose a folder for this run'),
} as const
