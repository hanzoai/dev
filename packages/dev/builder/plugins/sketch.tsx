/**
 * Sketch: a pad to draw on, attached to the message as a PNG the way the
 * paperclip attaches a file. Nothing leaves the page until the message goes.
 */
import { XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { Signature } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogDescription, DialogTitle } from '@hanzo/ui'
import { useEffect, useRef, useState, type PointerEvent } from 'react'

import type { Composer, Plugin } from '../plugin.ts'

/** The picture's own size: what the PNG is, whatever size the pad is drawn at. */
const W = 1200
const H = 750
const INK = '#111111'
const PAPER = '#ffffff'
const LINE = 5

type Stroke = { x: number; y: number }[]

/** Draw every stroke on white. */
export function paint(g: CanvasRenderingContext2D, strokes: readonly Stroke[]): void {
  g.fillStyle = PAPER
  g.fillRect(0, 0, W, H)
  g.strokeStyle = INK
  g.lineWidth = LINE
  g.lineCap = 'round'
  g.lineJoin = 'round'
  for (const s of strokes) {
    if (!s.length) continue
    g.beginPath()
    g.moveTo(s[0]!.x, s[0]!.y)
    // A tap is a dot: a line to where it began.
    for (const p of s.length > 1 ? s.slice(1) : s) g.lineTo(p.x, p.y)
    g.stroke()
  }
}

function Pad({ c }: { c: Composer }) {
  const { t } = useI18n()
  const canvas = useRef<HTMLCanvasElement>(null)
  const strokes = useRef<Stroke[]>([])
  const drawing = useRef(false)
  const [count, setCount] = useState(0)
  const [busy, setBusy] = useState(false)

  const redraw = () => {
    const g = canvas.current?.getContext('2d')
    if (g) paint(g, strokes.current)
  }
  useEffect(redraw, [])

  const at = (e: PointerEvent<HTMLCanvasElement>) => {
    const r = e.currentTarget.getBoundingClientRect()
    return { x: ((e.clientX - r.left) * W) / r.width, y: ((e.clientY - r.top) * H) / r.height }
  }
  const down = (e: PointerEvent<HTMLCanvasElement>) => {
    e.currentTarget.setPointerCapture(e.pointerId)
    drawing.current = true
    strokes.current.push([at(e)])
    setCount(strokes.current.length)
    redraw()
  }
  const move = (e: PointerEvent<HTMLCanvasElement>) => {
    if (!drawing.current) return
    strokes.current[strokes.current.length - 1]!.push(at(e))
    redraw()
  }
  const up = () => {
    drawing.current = false
  }
  const undo = () => {
    strokes.current.pop()
    setCount(strokes.current.length)
    redraw()
  }
  const clear = () => {
    strokes.current = []
    setCount(0)
    redraw()
  }
  const attach = () => {
    const el = canvas.current
    if (!el) return
    setBusy(true)
    el.toBlob((blob) => {
      setBusy(false)
      if (!blob) return
      c.attach([new File([blob], 'sketch.png', { type: 'image/png' })])
      c.show(null)
    }, 'image/png')
  }

  return (
    <Dialog open onOpenChange={(o) => !o && c.show(null)}>
      <DialogContent maxW={720} width="100%">
        <DialogTitle>{t('Sketch')}</DialogTitle>
        <DialogDescription>{t('Draw, then attach it to your message.')}</DialogDescription>
        <YStack borderWidth={1} borderColor="$borderColor" rounded={8} overflow="hidden">
          <canvas
            ref={canvas}
            width={W}
            height={H}
            aria-label={t('Sketch pad')}
            data-slot="sketch-pad"
            onPointerDown={down}
            onPointerMove={move}
            onPointerUp={up}
            onPointerCancel={up}
            style={{ display: 'block', width: '100%', aspectRatio: `${W} / ${H}`, touchAction: 'none', cursor: 'crosshair', background: PAPER }}
          />
        </YStack>
        <XStack gap="$2" items="center">
          <Button size="sm" variant="ghost" disabled={!count} onPress={undo}>
            {t('Undo')}
          </Button>
          <Button size="sm" variant="ghost" disabled={!count} onPress={clear}>
            {t('Clear')}
          </Button>
          <XStack flex={1} />
          <Button size="sm" variant="ghost" onPress={() => c.show(null)}>
            {t('Cancel')}
          </Button>
          <Button size="sm" disabled={!count || busy} onPress={attach}>
            {t('Attach')}
          </Button>
        </XStack>
      </DialogContent>
    </Dialog>
  )
}

export const sketch: Plugin = {
  id: 'sketch',
  name: phrase('Sketch'),
  note: phrase('Draw and attach an image'),
  icon: <Signature size={16} />,
  open: (c) => c.show(<Pad c={c} />),
}
