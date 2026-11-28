/**
 * The grip between two panes, one for every edge: the sidebar's, the side
 * panel's, a run's chat against its work. The kit's Grip with its `inline` bar
 * (drawn only while it is reached for), its band centred across the gutter and
 * the bar halfway down it, so each edge looks and moves the same.
 *
 *   side    the edge the box it sits in is pinned to (Grip's): `left` for a
 *           sidebar, whose free edge is its right, `right` for an aside
 *   gap     the gutter's width; the frame's `--pane-gap` by default
 *   inset   the box's own border, px: an absolute place is measured inside it
 */
import { Grip, type GripProps } from '@hanzo/ui'

import { gap as GAP } from './pane.ts'

export type GutterProps = Omit<GripProps, 'bar'> & { gap?: string | number; inset?: number }

export function Gutter({ side, gap = GAP, inset = 0, ...rest }: GutterProps) {
  const g = typeof gap === 'number' ? `${gap}px` : gap
  // A CSS length gui takes as written; its types name only tokens and numbers.
  const band = `max(8px, ${g})` as `var(--${string})`
  // The band's start, so its middle is the gutter's: half the gutter and half the band out from the edge.
  const at = (w: string) => `calc(-1 * (${g} + ${w}) / 2 - ${inset}px)`
  const edge = side === 'left' ? 'r' : 'l'
  return (
    <Grip
      side={side}
      bar="inline"
      justify="center"
      width={band}
      {...{ [edge]: at(band) }}
      // A finger's band, centred on the same line.
      $touchable={{ width: 44, [edge]: at('44px') }}
      {...rest}
    />
  )
}
