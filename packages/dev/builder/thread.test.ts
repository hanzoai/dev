import { afterEach, describe, expect, it } from 'vitest'

import type { Session } from './api/sessions.ts'
import {
  cleanErrorText,
  deriveThreadLinks,
  groupSessions,
  isAlreadyRunningError,
  isSessionActive,
  isSessionLive,
  linkThread,
  resetThreadLinks,
  resolveSessionState,
  threadChain,
  threadRoot,
} from './thread.ts'

const makeSession = (overrides: Partial<Session> & { id: string }): Session => ({
  id: overrides.id,
  title: overrides.title ?? 'Session title',
  status: overrides.status ?? 'done',
  agent: 'hanzo',
  actor: 'hanzo/u1',
  repo: overrides.repo ?? 'hanzoai/demo',
  project: overrides.project ?? 'demo',
  target: '',
  host: '',
  cwd: '',
  kind: overrides.kind ?? 'coding',
  base: overrides.base ?? 'main',
  branch: overrides.branch ?? '',
  environment: overrides.environment ?? 'sandbox',
  mode: overrides.mode ?? 'build',
  pr: overrides.pr ?? '',
  sandbox: overrides.sandbox ?? '',
  org: 'hanzo',
  published: false,
  channel: overrides.channel ?? '',
  parent: overrides.parent ?? '',
  root: overrides.root ?? '',
  events: overrides.events ?? 5,
  createdAt: overrides.createdAt ?? '2026-11-27T10:00:00Z',
  updatedAt: overrides.updatedAt ?? overrides.createdAt ?? '2026-11-27T10:00:00Z',
  endedAt: overrides.endedAt ?? '',
})

afterEach(() => {
  resetThreadLinks()
})

describe('session active/live status resolution', () => {
  it('keeps started, starting, queued, routed, and resumed sessions live and active', () => {
    for (const st of ['running', 'started', 'starting', 'queued', 'routed', 'resumed']) {
      expect(isSessionActive(st)).toBe(true)
      expect(isSessionLive(st)).toBe(true)
      // A `started` status event while the record says `running` MUST stay `running` (never flip to finished).
      const resolved = resolveSessionState('running', st)
      expect(isSessionLive(resolved)).toBe(true)
      expect(isSessionActive(resolved)).toBe(true)
    }
  })

  it('transitions to terminal or paused when narrated outcome settles', () => {
    expect(resolveSessionState('running', 'done')).toBe('done')
    expect(resolveSessionState('running', 'completed')).toBe('completed')
    expect(resolveSessionState('running', 'error')).toBe('error')
    expect(resolveSessionState('running', 'stopped')).toBe('stopped')
    expect(resolveSessionState('running', 'paused')).toBe('paused')
    expect(isSessionLive(resolveSessionState('running', 'done'))).toBe(false)
    expect(isSessionActive(resolveSessionState('running', 'paused'))).toBe(false)
    expect(isSessionLive(resolveSessionState('running', 'paused'))).toBe(true)
  })
})

describe('error normalization & running detection', () => {
  it('detects server already-running refusals so callers can steer instead of throwing', () => {
    expect(isAlreadyRunningError(new Error('coding: the run [REDACTED-TOKEN] is running; stop it or let it finish before carrying it on'))).toBe(true)
    expect(isAlreadyRunningError(new Error('coding: that run is already being followed up'))).toBe(true)
    expect(isAlreadyRunningError(new Error('network timeout'))).toBe(false)
  })

  it('rewrites server-redacted [REDACTED-TOKEN] cleanly without mangling real session IDs', () => {
    expect(cleanErrorText('coding: the run [REDACTED-TOKEN] is running; stop it or let it finish')).toBe(
      'coding: this session is running; stop it or let it finish',
    )
    expect(cleanErrorText('Session sess_0123456789abcdef0123456789abcdef failed')).toBe(
      'Session sess_0123456789abcdef0123456789abcdef failed',
    )
  })
})

describe('single-thread chaining and Recents deduplication', () => {
  it('chains multi-turn follow-ups under a single root and ignores cycles', () => {
    expect(linkThread('sess_1', 'sess_2')).toBe('sess_1')
    expect(linkThread('sess_2', 'sess_3')).toBe('sess_1')
    // Cycle attempt is ignored safely.
    expect(linkThread('sess_3', 'sess_1')).toBe('sess_1')

    expect(threadRoot('sess_3')).toBe('sess_1')
    expect(threadChain('sess_1')).toEqual(['sess_1', 'sess_2', 'sess_3'])
    expect(threadChain('sess_2')).toEqual(['sess_1', 'sess_2', 'sess_3'])
    expect(threadChain('sess_3')).toEqual(['sess_1', 'sess_2', 'sess_3'])
  })

  it('groups chained turns sharing an agent/ branch into one Recents row with root title and latest status', () => {
    const turn1 = makeSession({
      id: 'sess_1',
      title: 'Rebrand site to Hanzo Studio',
      status: 'done',
      branch: 'agent/abcdef123456',
      createdAt: '2026-11-27T10:00:00Z',
      updatedAt: '2026-11-27T10:02:00Z',
      events: 10,
    })
    const turn2 = makeSession({
      id: 'sess_2',
      title: 'Follow up: update footer copy',
      status: 'running',
      branch: 'agent/abcdef123456',
      createdAt: '2026-11-27T10:05:00Z',
      updatedAt: '2026-11-27T10:06:00Z',
      events: 4,
    })
    const unrelated = makeSession({
      id: 'sess_other',
      title: 'Fix auth middleware',
      status: 'done',
      branch: 'agent/999999999999',
      createdAt: '2026-11-27T10:03:00Z',
      updatedAt: '2026-11-27T10:04:00Z',
    })

    const grouped = groupSessions([turn2, unrelated, turn1], {})
    expect(grouped).toHaveLength(2)
    // Continued thread bubbles to top because turn2's updatedAt is newest.
    expect(grouped[0]).toMatchObject({
      id: 'sess_1',
      title: 'Rebrand site to Hanzo Studio',
      status: 'running',
      updatedAt: '2026-11-27T10:06:00Z',
      events: 14,
    })
    expect(grouped[1]!.id).toBe('sess_other')

    const links = deriveThreadLinks([turn2, unrelated, turn1], {})
    expect(threadChain('sess_1', links)).toEqual(['sess_1', 'sess_2'])
  })

  it('groups repo-less sandbox follow-ups and explicit parent/root trees', () => {
    const s1 = makeSession({ id: 'sess_a', title: 'Landing page', repo: '', branch: '', sandbox: 'sbx_42', createdAt: '2026-11-27T09:00:00Z' })
    const s2 = makeSession({ id: 'sess_b', title: 'Change hero color', repo: '', branch: '', sandbox: 'sbx_42', status: 'running', createdAt: '2026-11-27T09:10:00Z' })
    const grouped = groupSessions([s2, s1], {})
    expect(grouped).toHaveLength(1)
    expect(grouped[0]).toMatchObject({ id: 'sess_a', title: 'Landing page', status: 'running' })
  })
})
