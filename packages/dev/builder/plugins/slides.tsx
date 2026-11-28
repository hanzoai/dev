/**
 * Presentations: while it is on, a message makes a deck in Hanzo Studio (the
 * `slides` template, a PowerPoint file) and the answer is the deck: its outline,
 * the file, and the run in Studio, where it is edited. A later message in the
 * same conversation revises the last deck rather than starting over.
 */
import { phrase } from '@hanzo/i18n'
import { Presentation } from '@hanzogui/lucide-icons-2'

import { json } from '../api/chat.ts'
import type { Target } from '../api/call.ts'
import { make, type Run } from '../api/studio.ts'
import type { Plugin, Prep, Turn } from '../plugin.ts'

/** The last deck each conversation made: what a later message revises. */
const decks = new Map<string, Run>()

/** What a deck's writing step said: its title and its slides' titles. */
export function outline(text: string): { title: string; slides: string[] } {
  const o = json(text) ?? {}
  const slides = (Array.isArray(o.slides) ? o.slides : [])
    .map((s) => (s && typeof s === 'object' ? (s as Record<string, unknown>).title : ''))
    .map((t) => (typeof t === 'string' ? t.trim() : ''))
    .filter(Boolean)
  return { title: typeof o.title === 'string' ? o.title.trim() : '', slides }
}

/** The brief a revision is made from: the deck as it stands, then the change asked for. */
export function revision(last: Run, ask: string): string {
  return `Revise this deck. Keep what the change does not touch.\n\nThe deck as it stands:\n${last.text.trim()}\n\nThe change:\n${ask}`
}

/** The answer a made deck is: its outline, the file, and where it is edited. */
export function told(r: Run): string {
  const { title, slides } = outline(r.text)
  const file = r.outputs[0]
  const head = `**${title || r.title || 'Your deck'}**${slides.length ? ` — ${slides.length} slides` : ''}`
  const links = [
    file?.url ? `[Download the deck](${file.url}) (PowerPoint; the link works for an hour)` : '',
    file?.origin ? `[Open it in Studio](${file.origin}) to edit it` : '',
  ].filter(Boolean)
  const list = slides.map((s, i) => `${i + 1}. ${s}`).join('\n')
  return [head, links.join(' · '), list, 'It is kept in your Library.'].filter(Boolean).join('\n\n')
}

export async function deck(turn: Turn, t: Target, signal: AbortSignal): Promise<Prep> {
  const ask = turn.text.trim()
  if (!ask) return {}
  const last = turn.thread ? decks.get(turn.thread) : undefined
  const r = await make(t, { template: 'slides', title: ask.slice(0, 80), input: { brief: last ? revision(last, ask) : ask, format: 'pptx' }, ...(turn.design ? { brand: turn.design } : {}) }, signal)
  if (turn.thread) decks.set(turn.thread, r)
  return { reply: told(r) }
}

export const slides: Plugin = {
  id: 'slides',
  name: phrase('Presentations'),
  note: phrase('Create and edit decks'),
  icon: <Presentation size={16} />,
  prepare: (turn, c, signal) => deck(turn, c.target, signal),
}
