/**
 * The person's own settings, read once for the whole builder and shared by
 * everything that follows them: Settings writes them, New starts from the
 * coding defaults and says the name, dictation listens in the chosen language,
 * and the page takes the theme, the text size and the motion.
 *
 * The last document read is kept in this browser, per org, so the page is
 * drawn the person's way before the platform answers. A save shows at once and
 * is taken back, key by key, if the platform refuses it.
 *
 * A host mounts it too, above the builder (the Hanzo app, for Chat's side
 * panel), and the builder's own then reads through that one: one document, one
 * read. The look (`Look`) is the builder's alone: a host has its own.
 */
import { phrase } from '@hanzo/i18n'
import { createContext, useCallback, useContext, useEffect, useMemo, useRef, type ReactNode } from 'react'

import { merge, read, save, type Patch, type Prefs } from './api/pref.ts'
import { useKept, useRead } from './data.ts'
import { useHost, useTarget } from './host.tsx'

export interface PrefState {
  prefs: Prefs
  /** The first read is still in flight. */
  loading: boolean
  error: Error | null
  /** Merge a change in and keep it. Throws the platform's refusal. */
  save: (patch: Patch) => Promise<void>
}

const Context = createContext<PrefState | null>(null)

export function PrefsProvider({ children }: { children: ReactNode }) {
  return useContext(Context) ? children : <Keep>{children}</Keep>
}

function Keep({ children }: { children: ReactNode }) {
  const host = useHost()
  const t = useTarget()
  const signed = Boolean(host.person)
  const [kept, keep] = useKept<Prefs>(`hanzo.build.pref.${host.org ?? 'none'}`, {})
  const now = useRef(kept)
  const doc = useRead(signed ? () => read(t) : null, null as Prefs | null, [t, signed])

  useEffect(() => {
    if (!doc.value) return
    now.current = doc.value
    keep(doc.value)
  }, [doc.value, keep])

  const write = useCallback(
    async (patch: Patch) => {
      if (!signed) throw new Error(phrase('Sign in to save your settings.'))
      const before = now.current
      now.current = merge(before, patch)
      keep(now.current)
      try {
        now.current = await save(t, patch)
        keep(now.current)
      } catch (e) {
        // Only the keys this change named go back; another change since stands.
        const undo: Patch = {}
        for (const k of Object.keys(patch) as (keyof Prefs)[]) (undo as Record<string, unknown>)[k] = before[k] ?? null
        now.current = merge(now.current, undo)
        keep(now.current)
        throw e
      }
    },
    [signed, t, keep],
  )

  const value = useMemo(
    () => ({ prefs: kept, loading: doc.loading, error: doc.error, save: write }),
    [kept, doc.loading, doc.error, write],
  )
  return <Context.Provider value={value}>{children}</Context.Provider>
}

/** The settings; with `false`, null where nothing holds them (the side panel drawn without a host: hanzo.team's rooms). */
export function usePrefs(): PrefState
export function usePrefs(need: false): PrefState | null
export function usePrefs(need = true): PrefState | null {
  const p = useContext(Context)
  if (!p && need) throw new Error('The builder was rendered without its settings')
  return p
}

/** The type scale each text size is drawn at. Medium is the design system's own. */
export const SCALE = { small: 0.9, medium: 1, large: 1.15 } as const

/** Every animation and transition finished at once, the way the system setting does it. */
const STILL =
  '*,*::before,*::after{animation-duration:.01ms!important;animation-iteration-count:1!important;transition-duration:.01ms!important;scroll-behavior:auto!important}'

/**
 * The saved theme handed to the host, and the text size and motion put on the
 * page. A stylesheet rather than a property on <html>: the design system's own
 * root re-applies its appearance on mount, and a rule marked important outlasts
 * that, while the builder is on the page and no longer.
 */
export function Look() {
  const host = useHost()
  const { theme, text, motion } = usePrefs().prefs
  useEffect(() => {
    if (theme && theme !== host.theme) host.chooseTheme?.(theme)
    // The host's theme is followed, not watched: a choice made elsewhere stands until this one changes.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [theme])
  const rules = [
    text && text !== 'medium' ? `html:root{--type-scale:${SCALE[text]}!important}` : '',
    motion === 'reduced' ? STILL : '',
  ].join('')
  return rules ? <style data-hanzo-build="look">{rules}</style> : null
}
