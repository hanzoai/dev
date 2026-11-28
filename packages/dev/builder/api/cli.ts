/**
 * A run's tool calls as `dev` prints them in its terminal (hanzoai/dev, the TUI's
 * exec, patch and MCP cells): the run IS `dev exec`, so its transcript reads in
 * the CLI's own words and groups —
 *
 *   • Explored                      reads, listings and searches, consecutive ones in one block
 *     └ Search widget in src        one line an action
 *       Read lib.rs, main.rs        consecutive reads, one line, each name once
 *   • Ran cargo test                any other command, the shell's own wrapper taken off
 *     └ ok
 *   • Edited lib.rs (+2 -1)         a patch: Added / Deleted / Edited, or "Edited N files (+a -r)"
 *   • Called hanzo.search({"q":1})  an MCP tool, server.tool(arguments)
 *
 * What a command does is the agent's own parse of it (`command_actions` in exec's
 * stream), and the lines a patch adds and removes are its own count (`added`,
 * `removed`): nothing here parses a command or a diff. Running, a block says
 * Exploring, Running or Calling, as the CLI does.
 */
import { phrase } from '@hanzo/i18n'

/** One action a command performs, as the agent parses it (exec's `command_actions`). */
export type Action =
  | { type: 'read'; command: string; name: string; path: string }
  | { type: 'listFiles'; command: string; path: string | null }
  | { type: 'search'; command: string; query: string | null; path: string | null }
  | { type: 'unknown'; command: string }

/** One call in an Explored block: what it did, and how it exited (null while it runs). */
export interface Explore {
  actions: Action[]
  exit: number | null
}

/** One line of an Explored block: the CLI's verb, and what it names. */
export interface Line {
  title: string
  text: string
}

/** One file of a patch, with the lines it adds and removes — null from a `dev` too old to count them. */
export interface Changed {
  path: string
  kind: 'add' | 'delete' | 'update'
  added: number | null
  removed: number | null
}

/** A file's line counts as the CLI writes them, "(+a -r)", or nothing when they were not counted. */
export const counted = (f: Pick<Changed, 'added' | 'removed'>): string => (f.added === null || f.removed === null ? '' : `(+${f.added} -${f.removed})`)

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const nullable = (v: unknown): string | null => (typeof v === 'string' ? v : null)

/** A command's actions as exec's stream states them; anything it does not name is not one. */
export function actions(raw: unknown): Action[] {
  if (!Array.isArray(raw)) return []
  return raw.flatMap((a): Action[] => {
    const o = a && typeof a === 'object' ? (a as Record<string, unknown>) : {}
    switch (o.type) {
      case 'read':
        return [{ type: 'read', command: str(o.command), name: str(o.name), path: str(o.path) }]
      case 'listFiles':
        return [{ type: 'listFiles', command: str(o.command), path: nullable(o.path) }]
      case 'search':
        return [{ type: 'search', command: str(o.command), query: nullable(o.query), path: nullable(o.path) }]
      case 'unknown':
        return [{ type: 'unknown', command: str(o.command) }]
    }
    return []
  })
}

/** Whether a call is exploration: it read, listed or searched, and did nothing else. */
export const explores = (a: Action[]): boolean => a.length > 0 && a.every((x) => x.type !== 'unknown')

/**
 * An Explored block's lines, as the CLI writes them: a call that only read, and the
 * reads that follow it, are one "Read" line naming each file once; any other call
 * says each action on a line of its own. A call that exited non-zero says so on its
 * last line — "(command exit N)" for a compound command.
 */
export function explored(calls: Explore[]): Line[] {
  const out: Line[] = []
  const failed = (c: Explore) => c.exit !== null && c.exit !== 0
  const reads = (c: Explore) => c.actions.every((a) => a.type === 'read')
  for (let i = 0; i < calls.length; ) {
    const call = calls[i]!
    let n = 1
    if (reads(call) && !failed(call)) while (i + n < calls.length && !failed(calls[i + n]!) && reads(calls[i + n]!)) n++
    const group = calls.slice(i, i + n)
    i += n
    const lines: Line[] = reads(call)
      ? [{ title: phrase('Read'), text: [...new Set(group.flatMap((c) => c.actions.map((a) => (a.type === 'read' ? a.name : ''))))].join(', ') }]
      : call.actions.map((a): Line => {
          switch (a.type) {
            case 'read':
              return { title: phrase('Read'), text: a.name }
            case 'listFiles':
              return { title: phrase('List'), text: a.path ?? a.command }
            case 'search':
              return { title: phrase('Search'), text: a.query !== null && a.path !== null ? `${a.query} in ${a.path}` : (a.query ?? a.command) }
            default:
              return { title: phrase('Run'), text: a.command }
          }
        })
    if (failed(call)) {
      const last = lines[lines.length - 1]!
      last.text += call.actions.length > 1 ? ` (command exit ${call.exit})` : ` (exit ${call.exit})`
    }
    out.push(...lines)
  }
  return out
}

/**
 * A command as the CLI shows it: a shell's own wrapper (`bash -lc '…'`) taken off,
 * the script it ran left. exec's stream joins the argv as a shell would (shlex), so
 * the script is the wrapper's last word, unquoted.
 */
export function shown(command: string): string {
  const m = /^(?:\S*\/)?(?:bash|zsh|sh)\s+-l?c\s+(.+)$/s.exec(command.trim())
  return m ? unquote(m[1]!) : command
}

/** One shell word, its quoting taken off: single quotes, double quotes and backslashes. */
function unquote(word: string): string {
  let out = ''
  for (let i = 0; i < word.length; i++) {
    const c = word[i]!
    if (c === "'") {
      const end = word.indexOf("'", i + 1)
      if (end < 0) return word
      out += word.slice(i + 1, end)
      i = end
    } else if (c === '"') {
      let j = i + 1
      for (; j < word.length && word[j] !== '"'; j++) out += word[j] === '\\' && j + 1 < word.length ? word[++j] : word[j]
      i = j
    } else if (c === '\\' && i + 1 < word.length) out += word[++i]
    else if (/\s/.test(c)) return word // more than one word: not one script
    else out += c
  }
  return out
}

/** A patch's header, as the CLI writes it: one file named with its verb, or the count of files. */
export function edited(files: Changed[]): string {
  if (files.length === 1) {
    const [f] = files as [Changed]
    const verb = f.kind === 'add' ? 'Added' : f.kind === 'delete' ? 'Deleted' : 'Edited'
    return `${verb} ${f.path} ${counted(f)}`.trim()
  }
  const known = files.every((f) => f.added !== null && f.removed !== null)
  const total = known ? { added: files.reduce((n, f) => n + f.added!, 0), removed: files.reduce((n, f) => n + f.removed!, 0) } : { added: null, removed: null }
  return `Edited ${files.length} ${files.length === 1 ? 'file' : 'files'} ${counted(total)}`.trim()
}

/** An MCP call as the CLI names it: server.tool(arguments), the arguments as compact JSON. */
export const called = (server: string, tool: string, args: unknown): string =>
  `${server}.${tool}(${args === undefined || args === null ? '' : JSON.stringify(args)})`
