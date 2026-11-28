import { describe, expect, it, vi } from 'vitest'

vi.mock('@hanzo/gui', () => ({ SizableText: () => null, XStack: () => null, YStack: () => null }))
vi.mock('@hanzo/ui', () => ({
  Dialog: () => null,
  DialogContent: () => null,
  DialogTitle: () => null,
  DropdownMenu: () => null,
  DropdownMenuContent: () => null,
  DropdownMenuItem: () => null,
  DropdownMenuLabel: () => null,
  DropdownMenuSeparator: () => null,
  DropdownMenuTrigger: () => null,
  Input: () => null,
}))
vi.mock('@hanzogui/lucide-icons-2', () => ({
  ArrowUp: () => null,
  Check: () => null,
  Copy: () => null,
  GitBranch: () => null,
  Globe: () => null,
  Lightbulb: () => null,
  Link2: () => null,
  MoreHorizontal: () => null,
  PanelRight: () => null,
  RefreshCw: () => null,
  Share: () => null,
  Square: () => null,
  ThumbsDown: () => null,
  ThumbsUp: () => null,
  Volume2: () => null,
}))
vi.mock('@hanzo/voice', () => ({ mouth: () => null, speech: () => null }))

const { artifacts, rated, renders } = await import('./reply.tsx')

describe('an answer’s fenced blocks', () => {
  it('are named by a file name wherever the info string puts it, else by the language', () => {
    const text = '```html otter.html\n<h1>Otter</h1>\n```\n```index.html\n<p>a</p>\n```\n```ts\nconst n = 1\n```\n```\nplain\n```'
    const out = artifacts(text)
    expect(out.map((a) => a.name)).toEqual(['otter.html', 'index.html', 'ts block', 'Block 4'])
    expect(out[0]).toMatchObject({ mime: 'text/html', body: '<h1>Otter</h1>\n' })
    expect(out.filter(renders).map((a) => a.name)).toEqual(['otter.html', 'index.html'])
  })
})

describe('a verdict', () => {
  const held = () => {
    let now: 'up' | 'down' | null = null
    return { get: () => now, set: (to: 'up' | 'down' | null) => void (now = to), last: { n: 0 } }
  }
  const later = () => new Promise((r) => setTimeout(r))

  it('shows at once, and stays once it is sent', async () => {
    const v = held()
    rated(null, 'up', async () => {}, v.set, v.last)
    expect(v.get()).toBe('up')
    await later()
    expect(v.get()).toBe('up')
  })

  it('goes back to the earlier verdict when sending it fails', async () => {
    const v = held()
    rated(null, 'down', async () => Promise.reject(new Error('offline')), v.set, v.last)
    expect(v.get()).toBe('down')
    await later()
    expect(v.get()).toBeNull()
  })

  it('leaves a newer verdict alone when an older one fails, even the same one', async () => {
    const v = held()
    rated(null, 'down', async () => Promise.reject(new Error('offline')), v.set, v.last)
    rated('down', 'down', async () => {}, v.set, v.last)
    await later()
    expect(v.get()).toBe('down')
  })
})
