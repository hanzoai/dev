/**
 * Design system: while it is on, a message is a brief for one — "make me a
 * design system for X", or a site to take it from — and the answer is the
 * system: drafted by a Hanzo model under a strict schema (palette, type,
 * spacing, radii, components, voice), kept as a Studio brand (the one store for
 * them), drawn as a page in the side panel, and attached to the conversation, so
 * everything made after it follows it. From `+` › Design system it attaches to
 * any later conversation, in Chat or in Dev.
 */
import { phrase } from '@hanzo/i18n'
import { Palette } from '@hanzogui/lucide-icons-2'

import { call, type Target } from '../api/call.ts'
import { keepBrand } from '../api/studio.ts'
import { drafted, preview, SCHEMA, tokensOf } from '../design.ts'
import type { Plugin, Prep, Turn } from '../plugin.ts'

/** Hanzo's model that decodes under a strict schema. */
export const DRAFTER = 'zen5-flash'

/** The site a brief points at, if it names one. */
export const siteOf = (text: string): string => text.match(/https?:\/\/[^\s)]+/)?.[0] ?? ''

const BRIEF =
  'You design design systems. From the brief (and the site text, when given), answer one complete, coherent system: ' +
  'a name; eight hex colours with enough contrast for text on background and on primary; heading, body and mono fonts (Google Fonts names where you can); ' +
  'a type scale of 6-8 px sizes; a spacing scale of 6-10 px steps; sm, md and lg radii; one line each for button, card, input, nav and hero; ' +
  'and a voice: its tone in a sentence, 3-5 do and 3-5 dont lines. Take colours, fonts and tone from the site when one is given.'

async function site(t: Target, url: string, signal: AbortSignal): Promise<string> {
  if (!url) return ''
  const d = await call<{ success?: boolean; data?: { markdown?: string } }>(t, 'POST', '/v1/crawl', { url }, { signal }).catch(() => null)
  return d?.success ? (d.data?.markdown ?? '').slice(0, 6000) : ''
}

export async function make(turn: Turn, t: Target, signal: AbortSignal): Promise<Prep> {
  const brief = turn.text.trim()
  if (!brief) return {}
  const page = await site(t, siteOf(brief), signal)
  const raw = await call<{ choices?: { message?: { content?: unknown } }[] }>(
    t,
    'POST',
    '/v1/chat/completions',
    {
      model: DRAFTER,
      max_tokens: 2000,
      messages: [
        { role: 'system', content: BRIEF },
        { role: 'user', content: page ? `${brief}\n\nThe site, as text:\n${page}` : brief },
      ],
      response_format: { type: 'json_schema', json_schema: { name: 'design_system', strict: true, schema: SCHEMA } },
    },
    { signal },
  )
  const text = raw?.choices?.[0]?.message?.content
  let parsed: unknown = null
  try {
    parsed = JSON.parse(typeof text === 'string' ? text : '')
  } catch {
    throw new Error('The model did not answer a design system. Try the brief again.')
  }
  const d = drafted(parsed)
  if (typeof d === 'string') throw new Error(d)
  const b = await keepBrand(t, { name: d.name, tokens: tokensOf(d) }, signal)
  const file = `${d.name.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'design-system'}.html`
  const reply = [
    `**${d.name}** is saved and attached to this conversation: what you build here follows it, and \`+\` › Design system attaches it anywhere else.`,
    `Colours ${Object.values(d.colors).join(' ')} · ${d.fonts.heading} / ${d.fonts.body} · radius ${Object.values(d.radius).join('/')}px`,
    d.voice.tone ? `Voice: ${d.voice.tone}` : '',
    '```html ' + file + '\n' + preview(b) + '\n```',
  ]
  return { reply: reply.filter(Boolean).join('\n\n'), design: b.id }
}

export const designer: Plugin = {
  id: 'design',
  name: phrase('Design system'),
  note: phrase('Make one from a brief or a site'),
  icon: <Palette size={16} />,
  prepare: (turn, c, signal) => make(turn, c.target, signal),
}
