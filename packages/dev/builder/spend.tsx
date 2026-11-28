/**
 * Asking before a press spends: a run that starts now, a task that runs itself,
 * a bot that runs around the clock, a cloud computer woken. ONE way to ask: the
 * host's `confirm` (host.tsx) when it has one, so a host with its own spend
 * question (the Hanzo app's `useSpend`) is the only asker and the person is
 * never asked twice; this package's one confirm (ask.tsx) only where the host
 * gives none (its standalone page). What counts as a yes is question.ts's rule.
 */
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { useCallback, useEffect, useMemo, useRef, useState, type ReactNode } from 'react'

import { Confirm } from './ask.tsx'
import { useHosted, type Ask, type Spend } from './host.tsx'
import { asker } from './question.ts'

/** What the spend costs, said after `what`. No price: the package knows none it could stand behind. */
export const USES: Record<Spend['uses'], string> = {
  computer: phrase('It starts a cloud computer, which counts against your plan or your credits.'),
  credits: phrase('It counts against your plan or your credits.'),
}

/**
 * `ask(question)` answers true only on a yes; spend only then. Draw `dialog`
 * anywhere in the caller's tree (it is null under a host confirm).
 */
export function useSpend(): { ask: (a: Ask) => Promise<boolean>; dialog: ReactNode } {
  const { t } = useI18n()
  const confirm = useHosted()?.confirm
  const [shown, setShown] = useState<Spend | null>(null)
  const answer = useRef<((yes: boolean) => void) | null>(null)
  const settle = (yes: boolean) => {
    const resolve = answer.current
    answer.current = null
    setShown(null)
    resolve?.(yes)
  }
  // The host's confirm as it is now, so a host that hands a new function each render still gets one stable asker.
  const theirs = useRef(confirm)
  theirs.current = confirm
  const asked = useMemo(
    () =>
      asker((spend) => {
        const host = theirs.current
        if (host) return host(spend)
        return new Promise<boolean>((resolve) => {
          // A newer question takes the dialog; the one it replaces is a no.
          answer.current?.(false)
          answer.current = resolve
          setShown(spend)
        })
      }),
    [],
  )
  useEffect(() => {
    asked.enter()
    return () => {
      asked.leave()
      answer.current?.(false)
      answer.current = null
    }
  }, [asked])

  const dialog = confirm ? null : (
    <Confirm
      open={shown !== null}
      onOpenChange={(open) => !open && settle(false)}
      title={shown?.title ?? ''}
      says={shown ? `${shown.what} ${t(USES[shown.uses])}` : ''}
      act={shown?.yes ?? ''}
      danger={false}
      run={async () => settle(true)}
    />
  )
  return { ask: asked.ask, dialog }
}

/**
 * The one question before a suspended cloud workspace is woken, from its Wake
 * button or from anything that needs it running (a door, a preview, its dev
 * server): `wake()` answers whether the person agreed (api/sandbox.ts `Wake`).
 */
export function useWake(): { wake: () => Promise<boolean>; dialog: ReactNode } {
  const { t } = useI18n()
  const { ask, dialog } = useSpend()
  // One function for the screen's life: effects that open a door or a preview hold it, and must not run again for it.
  const words = useRef(t)
  words.current = t
  const wake = useCallback(
    () =>
      ask({
        kind: 'machine',
        title: words.current('Wake the cloud workspace?'),
        what: words.current('This starts its cloud computer again, with its files as they were, until it is suspended or the run ends.'),
        yes: words.current('Wake'),
        uses: 'computer',
      }),
    [ask],
  )
  return { wake, dialog }
}
