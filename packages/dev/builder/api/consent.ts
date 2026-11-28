/**
 * The signed-in person's answers to Hanzo's data-sharing questions, as IAM
 * records them for every Hanzo surface.
 *
 *   GET /v1/iam/consent → {status, data: {insights, training}}
 *   PUT /v1/iam/consent   {insights?, training?}   only the answers sent change
 *
 * `training` has three states: unanswered (''), granted, refused. Only granted
 * lets Hanzo train on this person's data; unanswered is read as no. `insights`
 * is anonymous product usage, with no prompt or answer text, and is on until
 * turned off. Every change is audited by IAM with the answer before and after.
 */
import { call, unwrap, type Target } from './call.ts'

export type Answer = '' | 'granted' | 'refused'

export interface Consent {
  insights: boolean
  training: Answer
}

const ANSWERS: readonly Answer[] = ['', 'granted', 'refused']

/** IAM's defaults for anything it did not say: insights on, training unanswered. */
export function consentOf(raw: unknown): Consent {
  const o = (raw && typeof raw === 'object' ? raw : {}) as Record<string, unknown>
  return {
    insights: typeof o.insights === 'boolean' ? o.insights : true,
    training: ANSWERS.includes(o.training as Answer) ? (o.training as Answer) : '',
  }
}

export async function consent(t: Target): Promise<Consent> {
  return consentOf(unwrap(await call<unknown>(t, 'GET', '/v1/iam/consent')))
}

export async function setConsent(t: Target, change: Partial<Consent>): Promise<Consent> {
  return consentOf(unwrap(await call<unknown>(t, 'PUT', '/v1/iam/consent', change)))
}
