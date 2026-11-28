/**
 * Coming back from a provider's consent page.
 *
 * A connect names the page it starts on as its return (`here`). The platform
 * keeps that address when it is a Hanzo app page, and its callback lands the
 * person back on it with the answer in the query:
 *
 *   ?complete=github&grant=<id>         a GitHub connect to finish (POST /v1/provider/github/user/complete)
 *   ?connected=<provider>&account=<n>   an organization's connector, connected
 *   ?error=<provider>&reason=<why>      either, refused
 *
 * `useBack` reads that answer once, takes it off the address so a reload does
 * not replay it, finishes a GitHub grant, and says what happened. GitHub
 * connected either way is said on the window too (`changed`), so every surface
 * showing GitHub reads it again, the Hanzo app's Quick setup among them.
 */
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { useEffect } from 'react'

import type { Target } from './api/call.ts'
import { changed, complete } from './api/github.ts'

/** The query keys a return carries its answer in. */
export const ANSWER = ['complete', 'grant', 'connected', 'account', 'error', 'reason'] as const

export type Landing =
  | { kind: 'grant'; provider: string; grant: string }
  | { kind: 'connected'; provider: string; account: string }
  | { kind: 'error'; provider: string; reason: string }

/** The answer a return carried in `search`, or null when it carried none. */
export function landed(search: string): Landing | null {
  const q = new URLSearchParams(search)
  const error = q.get('error')
  if (error) return { kind: 'error', provider: error, reason: q.get('reason') ?? '' }
  const grant = q.get('grant')
  const done = q.get('complete')
  if (done && grant) return { kind: 'grant', provider: done, grant }
  const connected = q.get('connected')
  if (connected) return { kind: 'connected', provider: connected, account: q.get('account') ?? '' }
  return null
}

/** The decoded key of one `k=v` pair, or '' when it does not decode. */
function key(pair: string): string {
  try {
    return decodeURIComponent(pair.split('=')[0].replace(/\+/g, ' '))
  } catch {
    return ''
  }
}

/** `href` without the answer keys, every other pair exactly as written; `href` itself when it carries none. */
export function clean(href: string): string {
  const url = new URL(href)
  const pairs = url.search.slice(1).split('&').filter(Boolean)
  const kept = pairs.filter((p) => !(ANSWER as readonly string[]).includes(key(p)))
  if (kept.length === pairs.length) return href
  url.search = kept.length ? `?${kept.join('&')}` : ''
  return url.href
}

/** This page's address, as a connect's return. */
export function here(): string {
  return clean(window.location.href)
}

const NAMES: Record<string, string> = { github: 'GitHub', slack: 'Slack' }

/** A provider id as people write it. */
export const named = (id: string): string => NAMES[id] ?? (id ? id[0].toUpperCase() + id.slice(1) : phrase('The connection'))

/**
 * Finish what a return carried, once a person is signed in: `say` gets the
 * sentence, and `after` runs once something was connected. GitHub's own
 * readers hear `changed` instead (api/github.ts `heard`).
 */
export function useBack(t: Target, signed: boolean, say: (note: string) => void, after?: () => void): void {
  const { t: words } = useI18n()
  useEffect(() => {
    if (!signed) return
    const l = landed(window.location.search)
    if (!l) return
    // Off the address first: a grant completes once, and a reload must not try again.
    const next = clean(window.location.href)
    window.history.replaceState(window.history.state, '', next)
    if (l.kind === 'error') {
      // The provider's own reason is said as it came; with none, ours.
      say(l.reason ? `${words(named(l.provider))}: ${l.reason}` : words('{provider}: not connected', { provider: words(named(l.provider)) }))
      return
    }
    if (l.kind === 'connected') {
      say(
        l.account
          ? words('{provider} is connected as {account}', { provider: words(named(l.provider)), account: l.account })
          : words('{provider} is connected', { provider: words(named(l.provider)) }),
      )
      // The org's App, installed on a GitHub account.
      if (l.provider === 'github') changed()
      after?.()
      return
    }
    if (l.provider !== 'github') return
    complete(t, l.grant).then(
      (c) => {
        say(c.connected ? words('Connected as @{login}', { login: c.login }) : words('GitHub did not connect; connect again'))
        changed()
        after?.()
      },
      (e: unknown) => say((e as Error).message),
    )
    // Once per landing: the address is clean after the first run.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [signed])
}
