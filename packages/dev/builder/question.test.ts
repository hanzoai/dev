/**
 * The rule for a spend's yes (question.ts), apart from any dialog: what a host
 * confirm's answer counts as, and the key a host may remember a yes under.
 */
import { describe, expect, it, vi } from 'vitest'

import type { Ask, Spend } from './host.tsx'
import { again, asker } from './question.ts'

const RUN: Ask = { kind: 'task-run', title: 'Run it now?', what: 'This starts one run.', yes: 'Run now', uses: 'credits' }
const BOT: Ask = { kind: 'bot', title: 'Start it 24/7?', what: 'It runs around the clock.', yes: 'Start bot', uses: 'credits' }

/** A host confirm whose answers the test gives, one per question, in order. */
function held() {
  const asked: Spend[] = []
  const answers: ((v: unknown) => void)[] = []
  const fails: ((e: unknown) => void)[] = []
  const confirm = (s: Spend) =>
    new Promise<boolean>((resolve, reject) => {
      asked.push(s)
      answers.push(resolve as (v: unknown) => void)
      fails.push(reject)
    })
  return { asked, answers, fails, confirm }
}

describe('a spend question', () => {
  it('is a yes only when the host answers exactly true', async () => {
    expect(await asker(async () => true).ask(RUN)).toBe(true)
    expect(await asker(async () => false).ask(RUN)).toBe(false)
    // Anything truthy that is not `true` is not a yes.
    expect(await asker((async () => 'yes') as unknown as () => Promise<boolean>).ask(RUN)).toBe(false)
  })

  it('is a no when the host confirm throws or rejects', async () => {
    const throws = vi.fn(async () => {
      throw new Error('no dialog')
    })
    expect(await asker(throws).ask(RUN)).toBe(false)
    const h = held()
    const a = asker(h.confirm)
    const answer = a.ask(RUN)
    h.fails[0]!(new Error('gone'))
    expect(await answer).toBe(false)
  })

  it('is a no when the yes comes after the screen that asked has gone', async () => {
    const h = held()
    const a = asker(h.confirm)
    const answer = a.ask(RUN)
    a.leave()
    h.answers[0]!(true)
    expect(await answer).toBe(false)
    // Back again, it asks afresh and a yes counts.
    a.enter()
    const next = a.ask(RUN)
    h.answers[1]!(true)
    expect(await next).toBe(true)
  })

  it('two quick presses spend once: the newer question supersedes the waiting one', async () => {
    const h = held()
    const a = asker(h.confirm)
    const first = a.ask(RUN)
    const second = a.ask(RUN)
    h.answers[0]!(true)
    h.answers[1]!(true)
    expect([await first, await second]).toEqual([false, true])
  })

  it('a host that never answers leaves nothing stuck: the next press asks again', async () => {
    const h = held()
    const a = asker(h.confirm)
    const lost = a.ask(RUN)
    const next = a.ask(RUN)
    expect(h.asked).toHaveLength(2)
    h.answers[1]!(true)
    expect(await next).toBe(true)
    h.answers[0]!(true)
    expect(await lost).toBe(false)
  })

  it('hands the host the key a yes may be remembered under: kind and what it uses, never a bot', async () => {
    const h = held()
    const a = asker(h.confirm)
    void a.ask(RUN)
    void a.ask({ ...RUN, kind: 'run', uses: 'computer' })
    void a.ask(BOT)
    expect(h.asked.map((s) => s.again)).toEqual(['task-run.credits', 'run.computer', null])
    expect(again({ ...RUN, kind: 'run', uses: 'credits' })).toBe('run.credits')
  })
})

describe('a 24/7 bot', () => {
  it('can never carry a key to remember a yes under: the type refuses one', () => {
    // @ts-expect-error a bot's `again` is null by its type (host.tsx `Spend`)
    const bad: Spend = { ...BOT, again: 'bot.credits' }
    const good: Spend = { ...BOT, kind: 'bot', again: null }
    expect([bad.again, good.again]).toEqual(['bot.credits', null])
  })
})
