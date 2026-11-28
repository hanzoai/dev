/**
 * Dev's conversation, as the platform keeps it (apps/agents talk.go): one
 * session, every turn.
 *
 *   POST /v1/agent/turns {session?, input, model?, effort?, repo?, base?, target?, mode?, issue?}
 *        → {sessionId, runId}
 *   POST /v1/agent/sessions/{id}/answer {question, text, chosen}   answer what a turn asked
 *
 * The first thing said opens the conversation; everything said after continues
 * the SAME session under a new run, once its last turn has answered — what is said
 * while a turn still works is that turn's next input (sessions.ts `message`). The
 * agent answers, and decides for itself when a turn needs a sandbox (leased on its
 * first use, kept for the conversation) or a coding run (which carries on the
 * conversation's last, in its sandbox and on its branch). A turn that only talks
 * uses neither, and a turn ends when its answer does: a build it started is
 * followed after, and its outcome is said into the conversation when it ends.
 *
 * The repository, branch, machine and mode are kept on the conversation from the
 * first message: every coding run its turns start works on them, auto starts it,
 * manual asks first (a question answered here), plan only reads and plans. The
 * model and effort ride each turn. Between turns the session rests `paused`.
 */
import { phrase } from '@hanzo/i18n'

import { call, Refusal, seg, type Target } from './call.ts'
import type { Event, Session } from './sessions.ts'
import { decode } from './turn.ts'

/** The channel a conversation's session is on. */
export const TALK = 'app'

/**
 * Whether a session is a conversation, rather than one run. A conversation's runs
 * are on its channel too — born in its tree, its person's alone — and are told
 * from it by having a parent: the conversation is the root.
 */
export const talks = (s: Pick<Session, 'channel' | 'parent'> | null | undefined): boolean => s?.channel === TALK && !s.parent

export interface Say {
  /** The conversation to continue. Absent starts one. */
  session?: string
  /** What the person said. */
  input: string
  /** The model, by the id GET /v1/models lists. */
  model?: string
  /** The repository: its name in the org, or owner/name on GitHub. */
  repo?: string
  /** The branch a coding run starts from. */
  base?: string
  /** One of the org's machines. Absent is the sandbox. */
  target?: string
  /** How the conversation's coding runs go: on their own, asked for first, or plans only. */
  mode?: 'auto' | 'manual' | 'plan'
  /** How hard a coding run thinks: low, medium or high. */
  effort?: string
  /** The issue on `repo` a coding run resolves. */
  issue?: number
  /** Out-of-band instructions (skills, memories) kept separate from `input`. */
  instructions?: string
  /** Explicit alias for `instructions` forwarded to turns and coding runs. */
  developerInstructions?: string
}

export interface Said {
  /** The conversation: the same id on every turn. */
  session: string
  /** This turn's run within it. */
  run: string
}

/** The request body: only what was said, with no empty values sent. */
export function body(s: Say): Record<string, string | number | boolean> {
  const out: Record<string, string | number | boolean> = { input: s.input.trim() }
  for (const k of [
    'session',
    'model',
    'effort',
    'repo',
    'base',
    'target',
    'mode',
    'instructions',
    'developerInstructions',
  ] as const) {
    const v = s[k]?.trim()
    if (v) out[k] = v
  }
  if (s.issue && Number.isInteger(s.issue) && s.issue > 0) out.issue = s.issue
  return out
}

/** Say one thing in a conversation: a new one without `session`, that one with it. */
export async function say(t: Target, s: Say): Promise<Said> {
  if (!s.input.trim()) throw new Refusal(400, phrase('Say something'))
  const r = await call<Partial<Record<string, unknown>>>(t, 'POST', '/v1/agent/turns', body(s))
  const session = typeof r?.sessionId === 'string' ? r.sessionId : ''
  if (!session) throw new Refusal(502, phrase('The conversation answered with no session'))
  return { session, run: typeof r?.runId === 'string' ? r.runId : '' }
}

/** What a conversation's turn is doing now: its latest progress line, or ''. */
export function now(events: Pick<Event, 'kind' | 'payload' | 'seq'>[]): string {
  let doing = ''
  for (const e of [...events].sort((a, b) => a.seq - b.seq)) {
    const b = decode(e.payload)
    if (e.kind === 'progress' && b && typeof b !== 'string' && typeof b.activity === 'string') doing = b.activity
  }
  return doing
}

/** Answer a question a session's run asked: an option pressed, or the person's own words. */
export async function answer(t: Target, session: string, question: string, text: string, chosen: boolean): Promise<void> {
  if (!text.trim()) throw new Refusal(400, phrase('Say an answer'))
  await call(t, 'POST', `/v1/agent/sessions/${seg(session)}/answer`, { question, text: text.trim(), chosen })
}
