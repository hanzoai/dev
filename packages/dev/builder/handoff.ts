/**
 * What Chat's "Open in Dev" leaves in this tab's session storage for the New
 * page to pick up: a prompt, and optionally some code to run it on. One key, one
 * reader, so Chat and Dev cannot drift apart.
 */
export const DEV_HANDOFF = 'hanzo.handoff.dev'

/** The draft the handoff makes, or '' for none; the handoff is consumed either way. */
export function takeHandoff(store: Pick<Storage, 'getItem' | 'removeItem'>): string {
  try {
    const raw = store.getItem(DEV_HANDOFF)
    if (!raw) return ''
    store.removeItem(DEV_HANDOFF)
    const h = JSON.parse(raw) as { prompt?: unknown; code?: unknown; language?: unknown }
    const text = (v: unknown) => (typeof v === 'string' ? v.trim() : '')
    const code = text(h.code)
    return [text(h.prompt), code && `\`\`\`${text(h.language)}\n${code}\n\`\`\``].filter(Boolean).join('\n\n')
  } catch {
    return ''
  }
}
