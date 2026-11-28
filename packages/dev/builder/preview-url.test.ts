import { describe, expect, it } from 'vitest'

import { cacheBustUrl, hasUnpublishedEdits, previewRevision, resolvePreviewSource } from './preview-url.ts'

describe('resolvePreviewSource', () => {
  it('prefers the live sandbox preview URL over a stale deployed liveUrl', () => {
    expect(
      resolvePreviewSource({
        sandboxUrl: 'https://3000-sbx-1.preview.hanzo.app',
        liveUrl: 'https://instrument-10.hanzo.app',
      }),
    ).toEqual({ url: 'https://3000-sbx-1.preview.hanzo.app', kind: 'sandbox' })
  })

  it('switches to live site when explicitly requested and falls back cleanly otherwise', () => {
    expect(
      resolvePreviewSource({
        sandboxUrl: 'https://3000-sbx-1.preview.hanzo.app',
        liveUrl: 'https://instrument-10.hanzo.app',
        preferLive: true,
      }),
    ).toEqual({ url: 'https://instrument-10.hanzo.app', kind: 'live' })

    expect(resolvePreviewSource({ liveUrl: 'https://instrument-10.hanzo.app' })).toEqual({
      url: 'https://instrument-10.hanzo.app',
      kind: 'live',
    })
    expect(resolvePreviewSource({ starterUrl: 'https://starter.hanzo.app' })).toEqual({
      url: 'https://starter.hanzo.app',
      kind: 'starter',
    })
    expect(resolvePreviewSource({})).toEqual({ url: '', kind: 'none' })
  })
})

describe('cacheBustUrl', () => {
  it('appends and updates _hzr revision param on http(s) URLs and leaves non-http URLs untouched', () => {
    expect(cacheBustUrl('https://instrument-10.hanzo.app/', 0)).toBe('https://instrument-10.hanzo.app/')
    expect(cacheBustUrl('https://instrument-10.hanzo.app/', 3)).toBe('https://instrument-10.hanzo.app/?_hzr=3')
    expect(cacheBustUrl('https://instrument-10.hanzo.app/?tab=home&_hzr=2', 4)).toBe('https://instrument-10.hanzo.app/?tab=home&_hzr=4')
    expect(cacheBustUrl('blob:https://hanzo.ai/123', 5)).toBe('blob:https://hanzo.ai/123')
  })
})

describe('previewRevision and hasUnpublishedEdits', () => {
  it('advances revision on file edits, pushes, turn completion, and publish', () => {
    const start = previewRevision([{ kind: 'status', seq: 1, payload: { status: 'started' } }], 'running', 0)
    const edited = previewRevision(
      [
        { kind: 'status', seq: 1, payload: { status: 'started' } },
        { kind: 'event', seq: 2, payload: { type: 'item.completed', item: { type: 'file_change', status: 'completed' } } },
      ],
      'running',
      0,
    )
    const finished = previewRevision(
      [
        { kind: 'status', seq: 1, payload: { status: 'started' } },
        { kind: 'event', seq: 2, payload: { type: 'item.completed', item: { type: 'file_change', status: 'completed' } } },
        { kind: 'status', seq: 3, payload: { status: 'done', changed: true } },
      ],
      'done',
      0,
    )
    const published = previewRevision(
      [
        { kind: 'status', seq: 1, payload: { status: 'started' } },
        { kind: 'event', seq: 2, payload: { type: 'item.completed', item: { type: 'file_change', status: 'completed' } } },
        { kind: 'status', seq: 3, payload: { status: 'done', changed: true, live: 'https://instrument-10.hanzo.app' } },
      ],
      'done',
      3,
    )
    expect(edited).not.toBe(start)
    expect(finished).not.toBe(edited)
    expect(published).not.toBe(finished)

    expect(
      hasUnpublishedEdits(
        [
          { kind: 'event', seq: 2, payload: { type: 'item.completed', item: { type: 'file_change', status: 'completed' } } },
          { kind: 'status', seq: 3, payload: { status: 'done', changed: true } },
        ],
        0,
      ),
    ).toBe(true)
    expect(
      hasUnpublishedEdits(
        [
          { kind: 'event', seq: 2, payload: { type: 'item.completed', item: { type: 'file_change', status: 'completed' } } },
          { kind: 'status', seq: 3, payload: { status: 'done', changed: true, live: 'https://instrument-10.hanzo.app' } },
        ],
        3,
      ),
    ).toBe(false)
  })
})
