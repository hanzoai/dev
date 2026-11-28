/**
 * The builder's places and runs, for a host that draws its own rail.
 *
 * A host that mounts `<Builder rail={false}>` keeps ONE left column — its own —
 * and lists the builder in it with these. `useSessions` is the same read and
 * the same org-wide feed the built-in rail uses, so a run started in the pane
 * appears here with its live status. `DevSection` draws the builder's places
 * and runs as rows of the host's sidebar (the `@hanzo/ui/chat` Sidebar the
 * built-in rail is made of), and every row moves the builder through the
 * host's own `go`.
 */
import { SizableText } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { SidebarItem, SidebarSection, StatusDot, type SessionStatus } from '@hanzo/ui/chat'
import { useMemo, type ReactNode } from 'react'

import type { Session } from './api/sessions.ts'
import { talks } from './api/talk.ts'
import { useRecents, type Read } from './data.ts'
import type { Host } from './host.tsx'
import { nav } from './nav.tsx'
import { route } from './route.ts'

/** A run's status, as the rail's dot draws it. Anything else is idle. */
export const DOTS: Record<string, SessionStatus> = {
  running: 'running',
  paused: 'paused',
  done: 'done',
  stopped: 'stopped',
  error: 'error',
}

/**
 * The dot a row draws. A conversation rests paused between turns (api/talk.ts):
 * it has answered and waits for the person, which reads as done, not as a run held.
 */
export const dot = (s: Pick<Session, 'status' | 'channel' | 'parent'>): SessionStatus =>
  talks(s) && s.status === 'paused' ? 'done' : (DOTS[s.status] ?? 'idle')

/** The org's coding runs, newest first, kept live by the org's feed. */
export function useSessions(host: Host): Read<Session[]> {
  const t = useMemo(() => ({ api: host.api, token: host.token, org: host.org }), [host.api, host.token, host.org])
  return useRecents(t, Boolean(host.person))
}

const Note = ({ children }: { children: string }) => (
  <SizableText size="$1" color="$soft" px="$2" py="$1">
    {children}
  </SizableText>
)

/**
 * The builder's places, grouped as every rail groups them (nav.tsx), then the
 * runs. `children` sit between the places and the runs — where a host lists its
 * own. `onPick` fires after a move, so a host that draws its rail as a drawer
 * on a phone can close it.
 */
export function DevSection({
  host,
  onPick,
  label,
  children,
}: {
  host: Host
  onPick?: () => void
  /** The runs' heading; absent, "Runs" in the reader's language. */
  label?: string
  children?: ReactNode
}) {
  const { t } = useI18n()
  const sessions = useSessions(host)
  const r = route(host.path)
  // Every row, a place or a run, closes a drawer behind it.
  const then = (press?: () => void) => () => {
    press?.()
    onPick?.()
  }
  return (
    <>
      {nav(host, host.go, t).map((g) => (
        <SidebarSection key={g.id} label={g.label}>
          {g.links.map((l) => (
            <SidebarItem key={l.id} icon={l.icon} active={l.active} onPress={then(l.onPress)}>
              {l.label}
            </SidebarItem>
          ))}
        </SidebarSection>
      ))}
      {children}
      <SidebarSection label={label ?? t('Sessions')}>
        {!host.person ? (
          <Note>{t('Sign in to see your sessions.')}</Note>
        ) : sessions.value.length ? (
          sessions.value.map((s) => (
            <SidebarItem
              key={s.id}
              icon={<StatusDot status={dot(s)} />}
              active={r.kind === 'run' && r.id === s.id}
              onPress={then(() => host.go(s.id))}
            >
              {s.title || t('Untitled session')}
            </SidebarItem>
          ))
        ) : (
          <Note>
            {sessions.error
              ? t(sessions.error.message)
              : sessions.loading
                ? t('Reading…')
                : `${t('No sessions yet.')} ${t('Say what to build and it lands here.')}`}
          </Note>
        )}
      </SidebarSection>
    </>
  )
}
