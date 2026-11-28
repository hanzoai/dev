/**
 * A link out of the builder: a new tab, no handle back to this page, no
 * referrer. Only ever handed an https address the caller has already checked.
 * `away` is the same for an address that has to be minted first.
 */
import type { ReactNode } from 'react'

export function Out({ href, label, children }: { href: string; label?: string; children: ReactNode }) {
  return (
    <a
      href={href}
      target="_blank"
      rel="noopener noreferrer"
      aria-label={label}
      title={label}
      style={{ color: 'inherit', textDecoration: 'none', display: 'inline-flex', alignItems: 'center', gap: 6, minWidth: 0, maxWidth: '100%' }}
    >
      {children}
    </a>
  )
}

/**
 * Opens a tab for an address that takes a moment to mint: the tab opens now, while
 * the press still counts, and goes to the address once it comes. A refusal closes it.
 */
export async function away(url: Promise<string>): Promise<void> {
  const w = window.open('about:blank', '_blank')
  if (w) w.opener = null
  try {
    const to = await url
    if (w) w.location.replace(to)
    else window.open(to, '_blank', 'noopener')
  } catch (e) {
    w?.close()
    throw e
  }
}
