/**
 * What a run's events say: every lifecycle sentence, the shell and the steps
 * beside the transcript, how a run ended and whether it kept its work, and the
 * cards each kind of event draws.
 */
import { describe, expect, it } from 'vitest'

import { answer, cards, decode, failure, followed, merge, outcome, said, settled, shell, steps, vendor, who, type Card } from './turn.ts'

const ev = (seq: number, kind: string, payload: unknown) => ({ id: `e${seq}`, seq, kind, payload })
const status = (payload: Record<string, unknown>, mode = '') => said({ kind: 'status', payload }, mode)

/** A card without its key, still told apart by its kind. */
type Bare = { [K in Card['kind']]: Omit<Extract<Card, { kind: K }>, 'key'> }[Card['kind']]
const bare = (cs: Card[]): Bare[] => cs.map(({ key: _, ...c }) => c as Bare)

describe('decode', () => {
  it('takes an object as it is, and a JSON object out of a string', () => {
    expect(decode({ a: 1 })).toEqual({ a: 1 })
    expect(decode('  {"a":1} ')).toEqual({ a: 1 })
  })

  it('keeps prose, even prose that opens with a brace, and reads anything else as nothing', () => {
    expect(decode('hello')).toBe('hello')
    expect(decode('{not json')).toBe('{not json')
    expect(decode(7)).toBeNull()
    expect(decode(null)).toBeNull()
  })
})

describe('said', () => {
  it('says how a run started and where it went', () => {
    expect(status({ status: 'started' })).toBe('Started')
    expect(status({ status: 'routed' })).toBe('Sent to a machine')
  })

  it('says how a run finished, by its mode', () => {
    expect(status({ status: 'done', plan: 'Use pnpm' }, 'setup')).toBe('Use pnpm')
    expect(status({ status: 'done' }, 'setup')).toBe('Set up — the run answered with no environment')
    expect(status({ status: 'done', branch: 'agent/x', prError: 'no token', pr: 'https://x' })).toBe('Pushed agent/x — the pull request could not be opened')
    expect(status({ status: 'done', pr: 'https://x' })).toBe('Done — https://x')
    expect(status({ status: 'done' })).toBe('Done')
  })

  it('says a run on a project with a site published what it pushed, or why it did not', () => {
    expect(status({ status: 'done', changed: true, branch: 'agent/x', pr: '#4', live: 'https://shop.hanzo.app' })).toBe('Pushed agent/x — #4 — published')
    expect(status({ status: 'done', changed: true, branch: 'agent/x', pr: '#4', unpublished: 'coding: the build exited 1: missing script: build' })).toBe(
      'Pushed agent/x — #4 — not published: coding: the build exited 1: missing script: build',
    )
    // Why it did not is said even beside an address.
    expect(status({ status: 'done', live: 'https://shop.hanzo.app', unpublished: 'the store refused index.html' })).toBe('Done — not published: the store refused index.html')
    expect(status({ status: 'done', live: 7, unpublished: false })).toBe('Done')
  })

  it('says an error, a stop, a pause and a follow-up, with the work kept only when it was', () => {
    expect(status({ status: 'error' })).toBe('The run hit an error')
    expect(status({ status: 'error', error: 'the dev harness exited 1: ERROR stream disconnected - retries exhausted' })).toBe(
      'The model did not answer. Try again in a moment, or pick another model.',
    )
    expect(status({ status: 'error', error: 'unexpected status 402 Payment Required: insufficient balance' })).toBe('Your balance is out of credit. Add credit, then try again.')
    expect(status({ status: 'error', error: 'model "enso-flash": every provider refused — tried enso (402)' })).toBe(
      'The model did not answer. Try again in a moment, or pick another model.',
    )
    expect(status({ status: 'error', error: 'panic at line 503 of main.go' })).toBe('The run stopped with an error. Try again, or follow up with what to change.')
    expect(
      status({
        status: 'error',
        error: 'coding: lease sandbox: a desktop lease costs 9 cents for its first hour and this wallet holds 0, and its 20 free sandboxes this day are used; top up to lease more',
      }),
    ).toBe('This plan’s free sandboxes are used for now. Add credit to keep building, or try again later.')
    expect(status({ status: 'error', error: 'coding: a sandbox needs at least 15 minutes of funded compute (0.04); top up to start a run' })).toBe(
      'Your balance is out of credit. Add credit, then try again.',
    )
    expect(status({ status: 'stopped', branch: 'agent/x', changed: true })).toBe('Stopped — work kept on agent/x')
    expect(status({ status: 'stopped', branch: 'agent/x', changed: false })).toBe('Stopped')
    expect(status({ status: 'stopped', changed: true })).toBe('Stopped')
    expect(status({ status: 'paused', branch: 'agent/x', changed: true })).toBe('Paused — work kept on agent/x')
    expect(status({ status: 'paused' })).toBe('Paused')
    expect(status({ status: 'followed', next: 'sess_2' })).toBe('Continued in a follow-up run')
    expect(status({ status: 'followed' })).toBe('The follow-up run could not start')
  })

  it('says a status it does not know by its name, and nothing for none', () => {
    expect(status({ status: 'queued' })).toBe('queued')
    expect(status({})).toBe('')
  })

  it('reads the first thing an event says, in order', () => {
    expect(said({ kind: 'log', payload: 'plain words' })).toBe('plain words')
    expect(said({ kind: 'log', payload: 3 })).toBe('')
    expect(said({ kind: 'log', payload: { message: '', text: 'the text' } })).toBe('the text')
    expect(said({ kind: 'log', payload: { content: 'the content' } })).toBe('the content')
    expect(said({ kind: 'log', payload: { result: 'the result' } })).toBe('the result')
    expect(said({ kind: 'tool-call', payload: { command: 'ls' } })).toBe('ls')
    expect(said({ kind: 'tool-call', payload: { step: 'lease' } })).toBe('lease')
    expect(said({ kind: 'tool-call', payload: { name: 'fs/read' } })).toBe('fs/read')
  })

  it('names the files a finished event changed, by name, and counts many', () => {
    expect(said({ kind: 'event', payload: { type: 'done', changed: [] } })).toBe('Done — no files changed')
    expect(said({ kind: 'event', payload: { type: 'done', changed: ['/w/a/', 7] } })).toBe('Done — changed a')
    expect(said({ kind: 'event', payload: { type: 'done', changed: ['a', 'b', 'c', 'd'] } })).toBe('Done — changed 4 files')
  })

  it('says an error event’s reason, or that there was one', () => {
    expect(said({ kind: 'event', payload: { type: 'error', error: 'clone failed' } })).toBe('clone failed')
    expect(said({ kind: 'event', payload: { type: 'error' } })).toBe('The run hit an error')
  })
})

describe('shell', () => {
  it('prints a tool call’s command once, then what it said, printed and complained', () => {
    const lines = shell([
      ev(1, 'tool-call', { step: 'test', message: 'test', stdout: 'ok', stderr: 'warn' }),
      ev(2, 'tool-call', { message: 'no step' }),
      ev(3, 'log', { text: 'from text' }),
      ev(4, 'log', { command: 'from command' }),
      ev(5, 'log', ''),
      ev(6, 'log', 9),
      ev(7, 'log', 'bare'),
    ])
    expect(lines).toEqual([
      { role: 'cmd', text: 'test' },
      { role: 'out', text: 'ok' },
      { role: 'out', text: 'warn' },
      { role: 'out', text: 'no step' },
      { role: 'out', text: 'from text' },
      { role: 'out', text: 'from command' },
      { role: 'out', text: 'bare' },
    ])
  })

  it('keeps the last four hundred lines', () => {
    const lines = shell(Array.from({ length: 450 }, (_, i) => ev(i + 1, 'log', `line ${i + 1}`)))
    expect(lines).toHaveLength(400)
    expect(lines[0]!.text).toBe('line 51')
  })
})

describe('steps', () => {
  it('skips what is not a named step, and settles one that ended in error', () => {
    expect(
      steps([
        ev(1, 'tool-call', 'prose'),
        ev(2, 'tool-call', 5),
        ev(3, 'tool-call', { message: 'unnamed' }),
        ev(4, 'tool-call', { step: 'build', status: 'error' }),
      ]),
    ).toEqual([{ name: 'build', done: true }])
    expect(steps([ev(1, 'tool-call', { step: 'push', status: 'done' })])).toEqual([{ name: 'push', done: true }])
  })
})

describe('outcome', () => {
  it('keeps the latest status and problem, and skips what says neither', () => {
    expect(
      outcome([
        ev(1, 'status', { status: 'started', prError: 'no token' }),
        ev(2, 'status', 'prose'),
        ev(3, 'status', 4),
        ev(4, 'status', {}),
        ev(5, 'log', { status: 'done' }),
      ]),
    ).toEqual({ status: 'started', problem: 'no token', published: 0 })
  })

  it('keeps where the latest status said the run’s work went live, and never the address', () => {
    expect(
      outcome([
        ev(9, 'status', { status: 'done', changed: true, live: 'https://shop.hanzo.app' }),
        ev(2, 'status', { status: 'done', changed: true, live: 'https://evil.example' }),
        ev(12, 'status', { status: 'done', changed: true, unpublished: 'the build exited 1' }),
        ev(14, 'log', { live: 'https://shop.hanzo.app' }),
      ]),
    ).toEqual({ status: 'done', problem: '', published: 9 })
    expect(outcome([ev(3, 'status', { status: 'done', live: '' })]).published).toBe(0)
  })
})

describe('failure', () => {
  it('says why the run failed while its last status is a failure, and nothing once it is not', () => {
    expect(failure([ev(1, 'status', { status: 'started' }), ev(2, 'status', { status: 'error', error: 'clone failed' })])).toBe('clone failed')
    expect(failure([ev(2, 'status', { status: 'error' })])).toBe('The run hit an error')
    expect(failure([ev(1, 'status', { status: 'error', error: 'x' }), ev(2, 'status', { status: 'done' })])).toBe('')
    expect(failure([ev(1, 'status', { status: 'error', error: 'x' }), ev(2, 'status', {}), ev(3, 'log', { status: 'done' })])).toBe('x')
    expect(failure([])).toBe('')
  })
})

describe('merge', () => {
  it('tells apart turns with no sequence by their id', () => {
    expect(merge([{ id: 'a', seq: 0 }], [{ id: 'b', seq: 0 }, { id: 'a', seq: 0 }]).map((e) => e.id)).toEqual(['a', 'b'])
  })
})

describe('who', () => {
  it('keeps a subject it cannot shorten', () => {
    expect(who('hanzo/')).toBe('hanzo/')
    expect(who('-x')).toBe('-x')
  })
})

describe('answer and settled', () => {
  it('reads the plan off the last finished status only', () => {
    expect(answer([ev(1, 'log', { status: 'done', plan: 'no' }), ev(2, 'status', 'prose'), ev(3, 'status', { status: 'error', plan: 'no' })])).toBe('')
  })

  it('settles on an error, a stop or a pause, and keeps the work only when it changed', () => {
    expect(settled([ev(1, 'log', { status: 'done' }), ev(2, 'status', 'prose')])).toEqual({ settled: false, pushed: false })
    expect(settled([ev(1, 'status', { status: 'error' })])).toEqual({ settled: true, pushed: false })
    expect(settled([ev(1, 'status', { status: 'stopped', changed: true })])).toEqual({ settled: true, pushed: true })
    expect(settled([ev(1, 'status', { status: 'running' })])).toEqual({ settled: false, pushed: false })
  })

  it('names the follow-up that carried the run on, and nothing before it starts', () => {
    expect(followed([ev(1, 'status', { status: 'stopped', changed: true })])).toBe('')
    expect(followed([ev(2, 'status', { status: 'followed', next: 'sess_2' }), ev(1, 'status', { status: 'stopped' })])).toBe('sess_2')
    expect(followed([ev(1, 'status', { status: 'followed', error: 'did not start' })])).toBe('')
    expect(followed([ev(1, 'log', { status: 'followed', next: 'sess_2' })])).toBe('')
  })
})

describe('cards', () => {
  it('says a run’s error in one line, with what it said folded into an Error step', () => {
    const raw = 'the dev harness exited 1: 2026-09-29T16:08:00Z ERROR code_core::codex::streaming: stream disconnected - retries exhausted'
    const got = cards([ev(1, 'status', { status: 'error', error: raw })]).map(({ key: _, ...c }) => c)
    expect(got).toEqual([
      { kind: 'note', text: 'The model did not answer. Try again in a moment, or pick another model.' },
      { kind: 'step', name: 'Error', detail: '', output: raw, ran: 'error' },
    ])
  })

  it('says a model vendor’s error in the plain line alone: its words, its name and its key never reach the page', () => {
    const raw = 'OpenRouter: 429 Too Many Requests {"error":{"message":"Rate limit exceeded: free-models-per-day","code":429,"metadata":{"provider_name":"Anthropic"}}} sk-or-v1-abcdef123456'
    const got = cards([ev(1, 'status', { status: 'error', error: raw })]).map(({ key: _, ...c }) => c)
    expect(got).toEqual([{ kind: 'note', text: 'The model did not answer. Try again in a moment, or pick another model.' }])
    expect(JSON.stringify(got)).not.toMatch(/openrouter|anthropic|sk-or/i)
    expect(failure([ev(1, 'status', { status: 'error', error: raw })])).toBe('The model did not answer. Try again in a moment, or pick another model.')
    expect(said({ kind: 'event', payload: { type: 'error', error: 'upstream provider openai returned 500' } })).toBe('The run stopped with an error. Try again, or follow up with what to change.')
    // A run's own failure keeps its words: they say what to fix.
    expect(vendor('git clone failed: repository not found')).toBe(false)
    expect(vendor('the dev harness exited 1: stream disconnected - retries exhausted')).toBe(false)
  })

  it('draws the run’s own steps as they move, naming a step it does not know', () => {
    const got = cards([
      ev(1, 'tool-call', 'prose'),
      ev(2, 'tool-call', { step: 'leased', message: 'no lease to finish' }),
      ev(3, 'tool-call', { message: 'unnamed' }),
      ev(4, 'tool-call', { step: 'ended' }),
      ev(5, 'tool-call', { step: 'install', message: 'pnpm install', status: 'running' }),
      ev(6, 'tool-call', { step: 'install', status: 'ok' }),
      ev(7, 'tool-call', { step: 'install', message: 'installed', status: 'running' }),
      ev(8, 'tool-call', { step: 'exit', message: 'exit 1' }),
      ev(9, 'tool-call', { step: 'exit', message: 'exit 0' }),
      ev(10, 'tool-call', { step: 'migrate', message: 'migrating', status: 'error' }),
    ])
    expect(bare(got)).toEqual([
      { kind: 'step', name: 'Install', detail: 'installed', output: '', ran: 'error' },
      { kind: 'step', name: 'Migrate', detail: 'migrating', output: '', ran: 'error' },
    ])
  })

  it('ends a command with nothing open as nothing', () => {
    expect(cards([ev(1, 'tool-call', { step: 'exit', message: 'exit 0' })])).toEqual([])
  })

  it('says a turn the platform refused: its record carries the reason and no status', () => {
    const refused = ev(2, 'log', { runId: 'run_1', error: 'metering: insufficient balance', model: '', status: '', durationMs: 0 })
    expect(bare(cards([ev(1, 'message', { role: 'user', text: 'Make a page' }), refused]))).toEqual([
      { kind: 'said', who: 'person', text: 'Make a page' },
      { kind: 'note', text: 'Your balance is out of credit. Add credit, then try again.' },
      { kind: 'step', name: 'Error', detail: '', output: 'metering: insufficient balance', ran: 'error' },
    ])
  })

  it('surfaces log errors in failure(), outcome(), settled(), and cards() even without runId (#363/#364)', async () => {
    const mod = (await import('./turn.ts')) as typeof import('./turn.ts') & { needsFunds?: (e: string) => boolean }
    const refused = ev(2, 'log', { error: 'metering: insufficient balance' })
    const seq = [ev(1, 'message', { role: 'user', text: 'Make a page' }), refused]
    expect(bare(cards(seq))).toEqual([
      { kind: 'said', who: 'person', text: 'Make a page' },
      { kind: 'note', text: 'Your balance is out of credit. Add credit, then try again.' },
      { kind: 'step', name: 'Error', detail: '', output: 'metering: insufficient balance', ran: 'error' },
    ])
    expect(mod.failure(seq)).toBe('metering: insufficient balance')
    expect(mod.outcome(seq).status).toBe('error')
    expect(mod.settled(seq).settled).toBe(true)
    expect(mod.needsFunds?.('metering: insufficient balance')).toBe(true)
    expect(mod.needsFunds?.('402 Payment Required')).toBe(true)
    expect(mod.needsFunds?.('This plan free sandboxes are used')).toBe(true)
    expect(mod.needsFunds?.('SyntaxError: unexpected token')).toBe(false)

    // Multi-turn Talk: a new turn after Turn 1's log error clears stale error outcome/failure/settled.
    const turn2 = [...seq, ev(3, 'message', { role: 'user', text: 'Try again now' }), ev(4, 'progress', { text: 'Thinking' })]
    expect(mod.outcome(turn2).status).toBe('')
    expect(mod.failure(turn2)).toBe('')
    expect(mod.settled(turn2).settled).toBe(false)

    // Raw app/command JSON log lines must not trigger false run errors.
    const rawLogs = [
      ev(1, 'log', { level: 'error', error: 'fixture connection refused' }),
      ev(2, 'log', { message: 'retrying', status: '' }),
      ev(3, 'log', { text: 'stdout', error: 'warn' }),
      ev(4, 'log', { error: '   ' }),
    ]
    expect(cards(rawLogs)).toEqual([])
    expect(mod.outcome(rawLogs).status).toBe('')
    expect(mod.failure(rawLogs)).toBe('')

    // Vendor JSON 402 error hides raw Error step, so the note card carries `funds: true`.
    const vendor402 = [ev(1, 'log', { error: '402 Payment Required: {"error":"insufficient balance"}' })]
    expect(bare(cards(vendor402))).toEqual([
      { kind: 'note', text: 'Your balance is out of credit. Add credit, then try again.', funds: true },
    ])
  })

  it('never draws a log line: raw output is the Terminal’s', () => {
    expect(cards([ev(1, 'log', 'codex\nhi\n'), ev(2, 'log', { text: 'there\n' }), ev(3, 'log', {})])).toEqual([])
  })

  it('keeps each agent process’s items apart, though their ids start again', () => {
    const msg = (seq: number, text: string) => ev(seq, 'event', { type: 'item.completed', item: { id: 'item_0', type: 'agent_message', text } })
    const got = cards([ev(1, 'event', { type: 'thread.started' }), msg(2, 'first'), ev(3, 'control', { command: 'message', message: 'go on' }), ev(4, 'event', { type: 'thread.started' }), msg(5, 'second')])
    expect(bare(got).map((c) => (c.kind === 'said' ? c.text : c.kind))).toEqual(['first', 'go on', 'second'])
  })

  it('draws an error the agent retried past as nothing, and a failed turn once', () => {
    const retried = cards([ev(1, 'event', { type: 'error', message: 'Reconnecting… 1/5' }), ev(2, 'event', { type: 'item.completed', item: { id: 'a', type: 'agent_message', text: 'done' } })])
    expect(bare(retried).map((c) => c.kind)).toEqual(['said'])
    const failed = cards([
      ev(1, 'event', { type: 'error', message: 'stream disconnected - retries exhausted' }),
      ev(2, 'event', { type: 'turn.failed', error: {} }),
      ev(3, 'status', { status: 'error', error: 'stream disconnected - retries exhausted' }),
    ])
    expect(bare(failed).map((c) => c.kind)).toEqual(['note', 'step'])
  })

  it('draws each answer once, whichever status carried it', () => {
    const got = cards([ev(1, 'status', { status: 'paused', answer: 'A1' }), ev(2, 'status', { status: 'done', answer: 'A2' })])
    expect(bare(got).filter((c) => c.kind === 'said').map((c) => (c.kind === 'said' ? c.text : ''))).toEqual(['A1', 'A2'])
  })

  it('draws nothing for a line of narration it cannot read', () => {
    expect(cards([ev(1, 'event', 'prose'), ev(2, 'event', { type: 'item.completed', item: { type: 'unknown' } }), ev(3, 'event', { type: 'turn.completed' })])).toEqual([])
  })

  it('draws a setup run’s answer as what the agent said, once', () => {
    const got = cards(
      [
        ev(1, 'tool-call', { message: 'running the task' }),
        ev(2, 'log', { message: 'codex\nUse pnpm\n' }),
        ev(3, 'status', { status: 'done', plan: 'Use pnpm' }),
      ],
      'setup',
    )
    expect(bare(got)).toEqual([{ kind: 'said', who: 'agent', text: 'Use pnpm' }])
  })

  it('keeps a person’s words that match the answer, and a status that says nothing draws nothing', () => {
    const got = cards([ev(1, 'message', { role: 'user', text: 'Use pnpm' }), ev(2, 'status', 'prose'), ev(3, 'status', {}), ev(4, 'status', { status: 'done', plan: 'Use pnpm' })], 'plan')
    expect(bare(got)).toEqual([
      { kind: 'said', who: 'person', text: 'Use pnpm' },
      { kind: 'plan', text: 'Use pnpm' },
    ])
  })

  it('draws the person’s ask once, and each steer where it was said', () => {
    const on = (seq: number) => ev(seq, 'event', { type: 'item.completed', item: { id: `i${seq}`, type: 'agent_message', text: 'On it.' } })
    const got = cards([ev(1, 'message', { role: 'user', text: 'Add the widget' }), on(2), ev(3, 'control', { command: 'message', message: 'and tests' }), on(4)])
    expect(bare(got).map((c) => (c.kind === 'said' ? `${c.who}: ${c.text}` : c.kind))).toEqual(['person: Add the widget', 'agent: On it.', 'person: and tests', 'agent: On it.'])
  })

  it('opens with the ask, ahead of the steps that set the run up, and leaves a steer where it was said', () => {
    const asked = cards([
      ev(1, 'status', { status: 'started' }),
      ev(2, 'tool-call', { step: 'clone', status: 'ok' }),
      ev(3, 'message', { role: 'user', text: 'Add the widget' }),
    ])
    expect(bare(asked).map((c) => c.kind + (c.kind === 'said' ? `:${c.who}` : ''))).toEqual(['said:person', 'note', 'step'])
    const steered = cards([ev(1, 'status', { status: 'started' }), ev(2, 'control', { command: 'message', message: 'faster' })])
    expect(bare(steered).map((c) => c.kind)).toEqual(['note', 'said'])
  })

  it('says what a person asked, and nothing for a control it cannot read', () => {
    const got = cards([
      ev(1, 'control', 'prose'),
      ev(2, 'control', { command: 'message', message: 'faster' }),
      ev(3, 'control', { command: 'message' }),
      ev(4, 'control', { command: 'resume' }),
      ev(5, 'control', { command: 'reboot' }),
    ])
    expect(bare(got)).toEqual([
      { kind: 'said', who: 'person', text: 'faster' },
      { kind: 'note', text: 'Resume asked' },
    ])
  })

  it('draws a message by who sent it, whichever field carries its words', () => {
    const got = cards([
      ev(1, 'message', 'bare words'),
      ev(2, 'message', { role: 'assistant', content: 'from content' }),
      ev(3, 'message', { message: 'from message' }),
      ev(4, 'message', {}),
      ev(5, 'heartbeat', { at: 1 }),
    ])
    expect(bare(got)).toEqual([
      { kind: 'said', who: 'agent', text: 'bare words' },
      { kind: 'said', who: 'agent', text: 'from content' },
      { kind: 'said', who: 'agent', text: 'from message' },
    ])
  })
})
