/**
 * One completion from Hanzo's own models, asked with the person's own bearer so
 * it is attributed to them and metered as theirs.
 *
 *   POST /v1/chat/completions   {model, messages, max_tokens} → {choices: [{message: {content}}]}
 */
import { call, type Target } from './call.ts'

/** The quick model a plugin thinks with: planning a search, drafting a template. */
export const QUICK = 'enso-flash'

export async function ask(t: Target, system: string, user: string, signal?: AbortSignal, tokens = 800, model = QUICK): Promise<string> {
  const raw = await call<{ choices?: { message?: { content?: unknown } }[] }>(
    t,
    'POST',
    '/v1/chat/completions',
    { model, max_tokens: tokens, messages: [{ role: 'system', content: system }, { role: 'user', content: user }] },
    { signal },
  )
  const said = raw?.choices?.[0]?.message?.content
  return typeof said === 'string' ? said : ''
}

/** The first JSON object in a model's answer, or null: a model may talk around it. */
export function json(said: string): Record<string, unknown> | null {
  const from = said.indexOf('{')
  const to = said.lastIndexOf('}')
  if (from < 0 || to <= from) return null
  try {
    const v: unknown = JSON.parse(said.slice(from, to + 1))
    return v && typeof v === 'object' && !Array.isArray(v) ? (v as Record<string, unknown>) : null
  } catch {
    return null
  }
}
