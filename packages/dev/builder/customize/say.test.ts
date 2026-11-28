import { afterEach, describe, expect, it, vi } from 'vitest'

import { Refusal } from '../api/call.ts'
import { within } from './load.ts'
import { Slow } from '../data.ts'
import { say } from './say.ts'

afterEach(() => vi.useRealTimers())

describe('what a failure tells a person', () => {
  it.each([
    [new Refusal(401, 'token expired: exp 1790000000'), 'Your sign-in has expired. Sign in again.'],
    [new Refusal(403, 'a validated principal is required'), 'Your account can’t do this here. Ask an org admin.'],
    [new Refusal(404, 'mount /v1/tool: no instance running'), 'Not found. It may have been removed.'],
    [new Refusal(429, 'rate limited: bucket org:acme'), 'Too many requests. Wait a moment and try again.'],
    [new Refusal(500, 'panic: runtime error: index out of range'), 'Hanzo could not answer just now. Try again.'],
    [new Refusal(503, 'no instance running'), 'Hanzo could not answer just now. Try again.'],
    [new Refusal(418, 'teapot'), 'Hanzo could not do that. Try again.'],
  ])('says %s in plain words, never the platform’s', (e, words) => {
    expect(say(e)).toBe(words)
  })

  it('keeps the platform’s own reason where it names what was wrong with what was sent', () => {
    expect(say(new Refusal(422, 'bundle: Expected ";" but found "oops"'))).toBe('bundle: Expected ";" but found "oops"')
    expect(say(new Refusal(409, 'A skill named triage exists'))).toBe('A skill named triage exists')
    expect(say(new Refusal(400, 'The URL is not public'))).toBe('The URL is not public')
  })

  it('does not pass on the bare line the client writes when the platform gave no reason', () => {
    expect(say(new Refusal(400, 'POST /v1/tool/skills answered 400'))).toBe('Hanzo could not do that. Try again.')
  })

  it('says a dropped connection, an unreadable answer and a slow one as what they are', () => {
    expect(say(new TypeError('Failed to fetch'))).toBe('Could not reach Hanzo. Check your connection and try again.')
    expect(say(new SyntaxError('Unexpected token < in JSON'))).toBe('Hanzo answered with something unreadable. Try again.')
    expect(say(new Slow())).toBe('Hanzo is taking too long to answer. Try again.')
  })

  it('says a sentence this client wrote as written, and something for anything else', () => {
    expect(say(new Error('A name is one lowercase word: letters, digits, _ or -'))).toBe('A name is one lowercase word: letters, digits, _ or -')
    expect(say('nope')).toBe('Something went wrong. Try again.')
    expect(say(new Error(''))).toBe('Something went wrong. Try again.')
  })
})

describe('a read with a deadline', () => {
  it('answers what the read answers when it answers in time', async () => {
    await expect(within(Promise.resolve(7), 1000)).resolves.toBe(7)
    await expect(within(Promise.reject(new Refusal(404, 'x')), 1000)).rejects.toBeInstanceOf(Refusal)
  })

  it('gives up as Slow when the read hangs past the deadline', async () => {
    vi.useFakeTimers()
    const hung = within(new Promise<number>(() => {}), 15_000)
    vi.advanceTimersByTime(15_000)
    await expect(hung).rejects.toBeInstanceOf(Slow)
  })
})
