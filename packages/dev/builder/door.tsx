/**
 * One door into a run's sandbox — its screen or its shell — framed from cloud's
 * own page with a fresh ticket in its address, while the sandbox runs. A
 * suspended sandbox is resumed from the pane's bar, never by looking at it.
 *
 * The page says when its socket is up. A page that says it failed, or silence
 * past the deadline, is a failed connection, and Retry mints a new ticket: a
 * ticket is spent the moment its socket opens, so reloading the old address can
 * only fail again.
 *
 * What a door says is the reader's language (@hanzo/i18n `t`), one sentence
 * per door; the page's own reason for a failed connection is drawn as given.
 */
import { SizableText, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { Button } from '@hanzo/ui'
import { useEffect, useRef, useState } from 'react'

import { door, stay, type Door as Which, type State as Held } from './api/sandbox.ts'
import { useTarget } from './host.tsx'

/** What each page signs its messages to the window that frames it with. */
const SOURCE = { screen: 'hanzo-screen', terminal: 'hanzo-term' } as const
const DEADLINE = 20_000

/** What each door says of itself, one sentence per door, marked (`phrase`) and drawn through `t`. */
const SAYS = {
  screen: {
    starting: phrase('The desktop opens once the run’s sandbox is up.'),
    parked: phrase('This run’s sandbox is suspended with its files kept. Resume it from the bar above to open its desktop.'),
    error: phrase('This run’s sandbox could not start, so it has no desktop.'),
    unreachable: phrase('Unable to reach the desktop. The run may have stopped.'),
    title: phrase('The run’s desktop'),
    connecting: phrase('Connecting to the desktop…'),
  },
  terminal: {
    starting: phrase('The terminal opens once the run’s sandbox is up.'),
    parked: phrase('This run’s sandbox is suspended with its files kept. Resume it from the bar above to open its terminal.'),
    error: phrase('This run’s sandbox could not start, so it has no terminal.'),
    unreachable: phrase('Unable to reach the terminal. The run may have stopped.'),
    title: phrase('The run’s terminal'),
    connecting: phrase('Connecting to the terminal…'),
  },
} as const

type State = 'opening' | 'open' | 'failed'

/**
 * `held` is where the run's sandbox is in its life, '' until it has been read. Only
 * a running sandbox has an open door.
 */
export function Door({ which, sandbox, held, session }: { which: Which; sandbox: string; held: Held | ''; session: string }) {
  const target = useTarget()
  const { t } = useI18n()
  const says = SAYS[which]
  const frame = useRef<HTMLIFrameElement>(null)
  const [src, setSrc] = useState('')
  const [state, setState] = useState<State>('opening')
  const [why, setWhy] = useState('')
  const [attempt, setAttempt] = useState(0)
  const up = Boolean(sandbox) && held === 'running'

  useEffect(() => {
    if (!up) return
    let gone = false
    setSrc('')
    setState('opening')
    setWhy('')
    // Opened by the page, so it never wakes the sandbox: the bar's Wake does, on a press.
    door(target, sandbox, which, stay, session).then(
      (u) => {
        if (!gone) setSrc(u)
      },
      (e: unknown) => {
        if (gone) return
        setState('failed')
        setWhy((e as Error).message)
      },
    )
    return () => {
      gone = true
    }
  }, [target, sandbox, which, session, up, attempt])

  useEffect(() => {
    if (!src) return
    const origin = new URL(src, window.location.href).origin
    const hear = (e: MessageEvent) => {
      if (e.origin !== origin || e.source !== frame.current?.contentWindow) return
      const m = e.data as { source?: unknown; ready?: unknown; why?: unknown; retry?: unknown } | null
      if (!m || m.source !== SOURCE[which]) return
      if (m.retry === true) {
        setAttempt((a) => a + 1)
      } else if (m.ready === true) {
        setState('open')
      } else if (m.ready === false) {
        setState('failed')
        setWhy(typeof m.why === 'string' ? m.why : '')
      }
    }
    window.addEventListener('message', hear)
    const late = setTimeout(() => setState((s) => (s === 'opening' ? 'failed' : s)), DEADLINE)
    return () => {
      window.removeEventListener('message', hear)
      clearTimeout(late)
    }
  }, [src, which])

  if (!sandbox || held === '' || held === 'pending') {
    return <Shut title={t('Starting')} body={t(says.starting)} />
  }
  if (held === 'parked') {
    return <Shut title={t('Suspended')} body={t(says.parked)} />
  }
  if (held === 'gone') {
    return <Shut title={t('Ended')} body={t('This run’s sandbox has ended. Its work is on its branch and in its artifacts.')} />
  }
  if (held === 'error') {
    return <Shut title={t('Unavailable')} body={t(says.error)} />
  }
  if (state === 'failed') {
    return (
      <Shut title={t('Failed to connect')} body={why || t(says.unreachable)}>
        <Button size="sm" variant="outline" onPress={() => setAttempt((a) => a + 1)}>
          {t('Retry')}
        </Button>
      </Shut>
    )
  }
  return (
    <YStack flex={1} minH={0} position="relative" bg="#000">
      {src ? (
        <iframe
          ref={frame}
          src={src}
          title={t(says.title)}
          allow="clipboard-read; clipboard-write"
          style={{ border: 0, width: '100%', height: '100%', display: 'block', background: '#000' }}
        />
      ) : null}
      {state === 'opening' ? (
        <YStack position="absolute" t={0} l={0} r={0} b={0} items="center" justify="center" pointerEvents="none">
          <SizableText size="$2" color="$soft">
            {t(says.connecting)}
          </SizableText>
        </YStack>
      ) : null}
    </YStack>
  )
}

function Shut({ title, body, children }: { title: string; body: string; children?: React.ReactNode }) {
  return (
    <YStack flex={1} items="center" justify="center" gap="$2" px="$6" py="$8">
      <SizableText size="$3" color="$ink">
        {title}
      </SizableText>
      <SizableText size="$2" color="$soft" style={{ textAlign: 'center', maxWidth: 360 }}>
        {body}
      </SizableText>
      {children}
    </YStack>
  )
}
