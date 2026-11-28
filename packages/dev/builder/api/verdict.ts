/**
 * A person's verdict on what a run said, recorded on the platform's own event
 * bus in the org it was said in.
 *
 *   POST /v1/event {type: 'track', event: 'build.verdict', sessionId, properties}
 *
 * The receipt says whether it landed; a verdict that did not is reported, not
 * shown as recorded.
 */
import { phrase } from '@hanzo/i18n'

import { call, type Target } from './call.ts'

export type Verdict = 'up' | 'down' | null

export async function verdict(t: Target, session: string, project: string, v: Verdict): Promise<void> {
  const out = await call<{ accepted?: number }>(t, 'POST', '/v1/event', {
    type: 'track',
    event: 'build.verdict',
    sessionId: session,
    properties: { verdict: v ?? 'cleared', project },
  })
  if (!out || !out.accepted) throw new Error(phrase('The verdict was not recorded'))
}
