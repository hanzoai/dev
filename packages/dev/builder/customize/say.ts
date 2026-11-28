/**
 * What a read or a write that failed tells a person: plain words, never the
 * platform's internals. A refusal is read by its status; only a 400, 409 or
 * 422 carries the platform's own sentence, because those name what was wrong
 * with what was sent (a name taken, a build that failed and why). A sentence
 * this client wrote itself (a name that does not fit) is said as written.
 *
 * Said in the reader's language: the screen hands its `t` (English without
 * one). A sentence a module wrote is English and marked, so it goes through
 * `t`; the platform's own words are no key and come back from it as given.
 */
import type { Say } from '@hanzo/i18n/react'

import { Refusal, told } from '../api/call.ts'
import { Slow } from '../data.ts'
import { english } from '../say.ts'

/** The line call.ts writes when the platform gave no reason: a method, a path and a status. */
const BARE = /^(GET|POST|PUT|PATCH|DELETE) \S* answered \d+$/

export function say(e: unknown, t: Say = english): string {
  if (e instanceof Slow) return t('Hanzo is taking too long to answer. Try again.')
  if (e instanceof Refusal) {
    if (e.status === 401) return t('Your sign-in has expired. Sign in again.')
    if (e.status === 403) return t('Your account can’t do this here. Ask an org admin.')
    if (e.status === 404) return t('Not found. It may have been removed.')
    if (e.status === 429) return t('Too many requests. Wait a moment and try again.')
    if (e.status >= 500) return t('Hanzo could not answer just now. Try again.')
    if ((e.status === 400 || e.status === 409 || e.status === 422) && e.message && !BARE.test(e.message)) return t(e.message)
    return t('Hanzo could not do that. Try again.')
  }
  // fetch rejects with a TypeError when the network does; a body that is not JSON is a SyntaxError.
  if (e instanceof TypeError) return t('Could not reach Hanzo. Check your connection and try again.')
  if (e instanceof SyntaxError) return t('Hanzo answered with something unreadable. Try again.')
  if (e instanceof Error && e.message) return told(e, t)
  return t('Something went wrong. Try again.')
}
