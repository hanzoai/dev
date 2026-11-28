/**
 * The run menu's "Keep computer awake": a checkbox row (so a screen reader hears
 * on or off) over the browser's screen wake lock. It reads "on" only once the
 * lock is granted, and the lock is let go when the run view closes.
 */
import { SizableText, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { DropdownMenuCheckboxItem } from '@hanzo/ui'
import { useEffect, useRef, useState } from 'react'

export interface Lock {
  release: () => Promise<void>
  /** The browser fires `release` when it takes the lock back (a hidden tab, low power). */
  addEventListener?: (type: 'release', listener: () => void) => void
}
type Wake = { wakeLock?: { request: (type: 'screen') => Promise<Lock> } }

/** The lock held after asking for `on`: a new one when it is granted, none when it is refused, unsupported or let go. */
export async function hold(nav: Wake, held: Lock | null, on: boolean): Promise<Lock | null> {
  if (!on) {
    await held?.release().catch(() => undefined)
    return null
  }
  if (held) return held
  try {
    return (await nav.wakeLock?.request('screen')) ?? null
  } catch {
    return null
  }
}

/**
 * Ask for the lock and see it through: a lock granted after the view has closed
 * is let go at once, and one the browser later takes back is reported to `dropped`.
 */
export async function grant(nav: Wake, held: Lock | null, on: boolean, closed: () => boolean, dropped: (lock: Lock) => void): Promise<Lock | null> {
  const next = await hold(nav, held, on)
  if (!next || next === held) return next
  if (closed()) {
    await next.release().catch(() => undefined)
    return null
  }
  next.addEventListener?.('release', () => dropped(next))
  return next
}

/** [on, set]: `on` follows the lock itself, and the lock goes when the view does. */
export function useKeepAwake(): [boolean, (on: boolean) => Promise<void>] {
  const [on, setOn] = useState(false)
  const lock = useRef<Lock | null>(null)
  const closed = useRef(false)
  useEffect(() => {
    closed.current = false
    return () => {
      closed.current = true
      void lock.current?.release().catch(() => undefined)
    }
  }, [])
  const set = async (want: boolean) => {
    lock.current = await grant(
      navigator as unknown as Wake,
      lock.current,
      want,
      () => closed.current,
      (l) => {
        if (lock.current === l) {
          lock.current = null
          setOn(false)
        }
      },
    )
    if (!closed.current) setOn(lock.current !== null)
  }
  return [on, set]
}

export function KeepAwake({ on, onToggle }: { on: boolean; onToggle: (on: boolean) => void }) {
  const { t } = useI18n()
  return (
    <DropdownMenuCheckboxItem checked={on} onCheckedChange={onToggle} preventCloseOnSelect>
      <YStack flex={1} minW={0}>
        <SizableText size="$2" color="$ink">
          {t('Keep computer awake')}
        </SizableText>
        <SizableText size="$1" color="$soft">
          {t('Only for this session')}
        </SizableText>
      </YStack>
    </DropdownMenuCheckboxItem>
  )
}
