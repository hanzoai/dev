/**
 * A suggestion's scheduled task is made only on the person's yes, asked through
 * the host's confirm, and that question may never be remembered past its kind.
 */
import { afterEach, describe, expect, it, vi } from 'vitest'

import type { Target } from './api/call.ts'
import type { Spend } from './host.tsx'
import { book } from './suggest.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'acme' }
const ACT = { kind: 'schedule' as const, name: 'Monday brief', prompt: 'Brief me.', schedule: { kind: 'weekly' as const, day: 'mon', at: '09:00' } }

function platform() {
  const sent: { method: string; url: string }[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      sent.push({ method: init.method ?? 'GET', url })
      return new Response(JSON.stringify({ id: 'flow_1', name: 'Monday brief', enabled: true }), { status: 201, headers: { 'content-type': 'application/json' } })
    }),
  )
  return sent
}

afterEach(() => vi.unstubAllGlobals())

describe('book', () => {
  it('asks first, and makes nothing on a no or a throw', async () => {
    const sent = platform()
    const asked: Spend[] = []
    expect(await book(T, ACT, 'chat', async (s) => (asked.push(s), false))).toBeNull()
    expect(
      await book(T, ACT, 'chat', async () => {
        throw new Error('no dialog')
      }),
    ).toBeNull()
    expect(sent).toEqual([])
    expect(asked[0]).toMatchObject({ kind: 'task', uses: 'credits', again: 'task.credits', title: 'Turn Monday brief on?' })
  })

  it('makes the task, switched on, on a yes', async () => {
    const sent = platform()
    expect(await book(T, ACT, 'chat', async () => true)).toMatchObject({ id: 'flow_1' })
    expect(sent).toEqual([{ method: 'POST', url: 'https://api.hanzo.ai/v1/auto/automations' }])
  })
})
