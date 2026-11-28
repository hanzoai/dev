import { describe, expect, it } from 'vitest'

import { session } from './api/sessions.ts'
import { aim, inside, shell } from './shared.ts'

const box = { id: 'tgt_1', host: 'box' }
const live = (o: Record<string, unknown>) => session({ id: 'sess', status: 'running', host: 'box', updatedAt: '2026-11-21T10:00:00Z', ...o })

describe('a folder on a linked machine (hanzo-inc/app issue 310)', () => {
  it('is inside a grant when it is the grant or under it, never beside it', () => {
    expect(inside('/home/me/site', '/home/me/site')).toBe(true)
    expect(inside('/home/me/site/web', '/home/me/site')).toBe(true)
    expect(inside('/home/me/site/web', '/home/me/site/')).toBe(true)
    expect(inside('/home/me/sites', '/home/me/site')).toBe(false)
    expect(inside('/home/me', '/home/me/site')).toBe(false)
  })

  it('reads where the machine\'s linked shell is open: the newest running shell there, by host or by id', () => {
    const sessions = [
      live({ id: 'a', cwd: '/home/me/old', updatedAt: '2026-11-21T09:00:00Z' }),
      live({ id: 'b', cwd: '/home/me/site', updatedAt: '2026-11-21T11:00:00Z' }),
      // A coding run is not a shell, a finished shell is not open, and another machine's shell is not this one's.
      live({ id: 'c', cwd: '/home/me/run', kind: 'coding', updatedAt: '2026-11-21T12:00:00Z' }),
      live({ id: 'd', cwd: '/home/me/done', status: 'done', updatedAt: '2026-11-21T12:00:00Z' }),
      live({ id: 'e', cwd: '/home/me/else', host: 'other', updatedAt: '2026-11-21T12:00:00Z' }),
    ]
    expect(shell(sessions, box)).toBe('/home/me/site')
    expect(shell([live({ host: '', target: 'tgt_1', cwd: '/srv/app' })], box)).toBe('/srv/app')
    expect(shell([live({ cwd: '' })], box)).toBe('')
    expect(shell([live({ host: '', cwd: '/x' })], { id: 'tgt_2', host: '' })).toBe('')
  })

  it('goes to the folder chosen while it is still granted', () => {
    expect(aim(['/home/me/site', '/home/me/api'], '/home/me/api', '/home/me/site')).toEqual({ dir: '/home/me/api', auto: false })
  })

  it('Auto is the shell\'s folder when it is granted, and no folder when it is not', () => {
    expect(aim(['/home/me/site'], '', '/home/me/site/web')).toEqual({ dir: '/home/me/site/web', auto: true })
    expect(aim(['/home/me/site'], '', '/home/me/secret')).toEqual({ dir: '', auto: true })
    expect(aim(['/home/me/site'], '', '')).toEqual({ dir: '', auto: true })
  })

  it('a chosen folder its owner stopped sharing falls back to Auto', () => {
    expect(aim(['/home/me/site'], '/home/me/api', '/home/me/site')).toEqual({ dir: '/home/me/site', auto: true })
    expect(aim([], '/home/me/api', '/home/me/api')).toEqual({ dir: '', auto: true })
  })
})
