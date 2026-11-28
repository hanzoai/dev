import { afterEach, describe, expect, it, vi } from 'vitest'

import type { Target } from './api/call.ts'
import { githubRawUrls, previewFormatOf, resolveFallbackPreview, sessionEditedPaths, svgPreviewDocument } from './preview-fallback.ts'

const TARGET: Target = { api: 'https://api.test', token: () => 'tok', org: 'hanzo' }

afterEach(() => {
  vi.unstubAllGlobals()
})

describe('previewFormatOf & sessionEditedPaths', () => {
  it('classifies markdown, html, svg, images, and source code cleanly', () => {
    expect(previewFormatOf('README.md')).toBe('markdown')
    expect(previewFormatOf('docs/guide.mdx')).toBe('markdown')
    expect(previewFormatOf('public/index.html')).toBe('html')
    expect(previewFormatOf('assets/logo.svg')).toBe('svg')
    expect(previewFormatOf('hero.png')).toBe('image')
    expect(previewFormatOf('photo.webp')).toBe('image')
    expect(previewFormatOf('src/app.tsx')).toBe('code')
    expect(svgPreviewDocument('<svg/>')).toContain('<svg/>')
  })

  it('orders session-edited paths most-recent-first and omits deleted files', () => {
    const paths = sessionEditedPaths([
      {
        seq: 1,
        kind: 'event',
        payload: {
          type: 'item.completed',
          item: {
            type: 'file_change',
            changes: [
              { path: '/work/src/a.ts', kind: 'add' },
              { path: '/work/src/temp.ts', kind: 'add' },
            ],
          },
        },
      },
      {
        seq: 2,
        kind: 'event',
        payload: {
          type: 'item.completed',
          item: {
            type: 'file_change',
            changes: [
              { path: '/work/src/temp.ts', kind: 'delete' },
              { path: '/work/docs/notes.md', kind: 'update' },
            ],
          },
        },
      },
    ])
    expect(paths).toEqual(['docs/notes.md', 'src/a.ts'])
  })

  it('builds deduplicated GitHub raw fallback URLs', () => {
    expect(githubRawUrls('hanzoai/dev', ['feat/ux', 'main'], 'README.md')).toEqual([
      'https://raw.githubusercontent.com/hanzoai/dev/feat%2Fux/README.md',
      'https://raw.githubusercontent.com/hanzoai/dev/main/README.md',
      'https://raw.githubusercontent.com/hanzoai/dev/master/README.md',
    ])
  })
})

describe('resolveFallbackPreview', () => {
  it('prefers the most recently created/modified session file over README.md', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string, init?: RequestInit) => {
        const u = String(url)
        if (u.endsWith('/v1/sandbox/read')) {
          const body = JSON.parse(String(init?.body)) as { path: string }
          if (body.path === 'preview.html') {
            return Response.json({ path: '/work/preview.html', dir: false, data: btoa('<h1>Hello</h1>') })
          }
          if (body.path === '') {
            return Response.json({ path: '/work', dir: true, entries: ['README.md', 'preview.html'] })
          }
        }
        return new Response('{}', { status: 404 })
      }),
    )

    const result = await resolveFallbackPreview({
      target: TARGET,
      session: 'sess_1',
      sandbox: 'sbx_1',
      running: true,
      signed: true,
      repo: 'hanzoai/demo',
      branch: 'agent/1',
      base: 'main',
      events: [
        {
          seq: 1,
          kind: 'event',
          payload: {
            type: 'item.completed',
            item: { type: 'file_change', changes: [{ path: '/work/preview.html', kind: 'add' }] },
          },
        },
      ],
    })

    expect(result).toEqual({
      path: 'preview.html',
      format: 'html',
      source: 'session',
      text: '<h1>Hello</h1>',
    })
  })

  it('renders binary image files created in the sandbox as data URLs', async () => {
    // Bytes containing NUL so sandbox.read detects binary=true and preserves base64.
    const pngHeader = btoa('\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR')
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string, init?: RequestInit) => {
        if (String(url).endsWith('/v1/sandbox/read')) {
          const body = JSON.parse(String(init?.body)) as { path: string }
          if (body.path === 'out.png') {
            return Response.json({ path: '/work/out.png', dir: false, data: pngHeader })
          }
        }
        return new Response('{}', { status: 404 })
      }),
    )

    const result = await resolveFallbackPreview({
      target: TARGET,
      session: 'sess_1',
      sandbox: 'sbx_1',
      running: true,
      signed: false,
      repo: '',
      branch: '',
      base: '',
      events: [
        {
          seq: 1,
          kind: 'event',
          payload: {
            type: 'item.completed',
            item: { type: 'file_change', changes: [{ path: 'out.png', kind: 'add' }] },
          },
        },
      ],
    })

    expect(result).toEqual({
      path: 'out.png',
      format: 'image',
      source: 'session',
      text: '',
      imageUrl: `data:image/png;base64,${pngHeader}`,
    })
  })

  it('falls back to project README.md via GitHub raw when sandbox is parked and no edits exist', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string) => {
        const u = String(url)
        if (u === 'https://raw.githubusercontent.com/hanzoai/demo/main/README.md') {
          return new Response('# Demo Project\n\nWelcome!', { status: 200 })
        }
        return new Response('{}', { status: 404 })
      }),
    )

    const result = await resolveFallbackPreview({
      target: TARGET,
      session: 'sess_1',
      sandbox: 'sbx_1',
      running: false,
      signed: true,
      repo: 'hanzoai/demo',
      branch: '',
      base: 'main',
      events: [],
    })

    expect(result).toEqual({
      path: 'README.md',
      format: 'markdown',
      source: 'readme',
      text: '# Demo Project\n\nWelcome!',
    })
  })

  it('returns null only when there are genuinely zero files', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string) => {
        if (String(url).endsWith('/v1/sandbox/read')) {
          return Response.json({ path: '/work', dir: true, entries: [] })
        }
        return new Response('{}', { status: 404 })
      }),
    )

    const result = await resolveFallbackPreview({
      target: TARGET,
      session: 'sess_empty',
      sandbox: 'sbx_empty',
      running: true,
      signed: false,
      repo: '',
      branch: '',
      base: '',
      events: [],
    })

    expect(result).toBeNull()
  })
})
