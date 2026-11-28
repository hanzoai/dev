import { describe, expect, it, vi } from 'vitest'

vi.mock('@hanzo/gui', () => ({}))
vi.mock('@hanzo/ui', () => ({}))
vi.mock('@hanzogui/lucide-icons-2', () => ({}))
vi.mock('./host.tsx', () => ({}))

const { screenshot } = await import('./panel.tsx')

const page = { id: 't1', url: 'https://hanzo.ai', title: 'Hanzo' }

describe('the page menu', () => {
  it('hands a capture to its host as a PNG file', async () => {
    const files: File[] = []
    await screenshot({ capture: async () => new Blob(['x'], { type: 'image/png' }) } as never, page, (f) => void files.push(f))
    expect(files).toHaveLength(1)
    expect(files[0]!.type).toBe('image/png')
  })

  it('drops a capture the host browser cancels, with nothing thrown', async () => {
    const files: File[] = []
    await expect(screenshot({ capture: async () => Promise.reject(new Error('cancelled')) } as never, page, (f) => void files.push(f))).resolves.toBeUndefined()
    expect(files).toEqual([])
  })
})
