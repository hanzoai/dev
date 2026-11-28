/**
 * A folder on a linked machine, for a run started from the web (hanzo-inc/app issue 310).
 *
 * THE MACHINE'S OWNER SAYS WHICH FOLDERS. They grant one from Hanzo Desktop or with `hanzo link grant <folder>`, and
 * the platform admits a run that names a folder only inside one (403 `folder_not_granted` otherwise; the machine checks
 * the real path again). So the Folder pill on a machine lists the granted folders and nothing else.
 *
 * AUTO IS THE FOLDER THE LINKED SHELL IS OPEN IN, when it is granted: the shell reports where it is as it moves (the
 * session's `cwd`), and that is where the person is working. A shell open anywhere else, or none, leaves Auto with no
 * folder, and the run works in a new folder on the machine, as a run that names none always has.
 */
import type { Session } from './api/sessions.ts'

/** The code the platform refuses a run under when its folder is not inside a grant. */
export const NOT_GRANTED = 'folder_not_granted'

/** Whether `dir` is `grant` or lies under it: `/a/b` is under `/a`, `/ab` is not (apps/agents folders.go `within`). */
export const inside = (dir: string, grant: string): boolean => dir === grant || dir.startsWith(`${grant.replace(/\/+$/, '')}/`)

/** The folder the machine's linked shell is open in: the most recently active running shell there (a session no run tagged) that says, or ''. */
export function shell(sessions: readonly Session[], machine: { id: string; host?: string }): string {
  const on = sessions.filter(
    (s) => s.status === 'running' && !s.kind && s.cwd && (s.target === machine.id || (Boolean(machine.host) && s.host === machine.host)),
  )
  return [...on].sort((a, b) => b.updatedAt.localeCompare(a.updatedAt))[0]?.cwd ?? ''
}

/** The folder a run on the machine works in: the one chosen while it is still granted, else Auto's (the shell's, when granted), else '' (a new folder). */
export function aim(granted: readonly string[], kept: string, cwd: string): { dir: string; auto: boolean } {
  if (kept && granted.includes(kept)) return { dir: kept, auto: false }
  return { dir: cwd && granted.some((g) => inside(cwd, g)) ? cwd : '', auto: true }
}
