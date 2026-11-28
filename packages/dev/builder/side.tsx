/**
 * The side panel's column beside a conversation or a run, once for Chat and
 * Dev: how wide it is, the middle column it can take the room from, and full
 * screen. The host draws the two columns and spreads what `useSplit` hands it
 * on them and on the kit's `Grip`; the `Panel` inside reads `useSide` for its
 * full-screen button and its way back to the middle column.
 *
 *   width   the person's, kept in their preferences (`side.width`, /v1/pref),
 *           so it follows them to every device and to both modes. Where no
 *           PrefsProvider holds them (hanzo.team's rooms) it lasts the page.
 *           Saved once a drag lets go, after a burst of keys has stopped.
 *   main    the middle column keeps MAIN px. Pulled under half of that it is
 *           hidden, never unmounted, so the transcript and the composer keep
 *           their state, and the panel takes the room. Pulled back out, or the
 *           strip's Show button, brings it back at the width kept.
 *   full    the panel over the whole window. Escape or the button returns.
 *
 * A DRAG NEVER DRAWS REACT. Each frame writes the panel's width and the middle
 * column's display on the two elements (`paint`), and the settled state is
 * written by the same hand once React commits it (`look`, after layout, before
 * paint), so the two never disagree and the host spreads no style of its own;
 * React learns the width once, when the drag lets go. The room the two share is
 * measured by a ResizeObserver, after layout, so a frame never reads layout
 * it has just dirtied. A window narrowed under the panel is painted the same
 * way: the panel gives way, and the person's width comes back with the room.
 *
 * The panel stands at the line's end and the gutter is its inline-start margin,
 * so a right-to-left page puts it on the left and its edge on its right
 * (@hanzo/i18n `dir`), and the grip grows it rightwards.
 */
import { useI18n } from '@hanzo/i18n/react'
import { createContext, useCallback, useContext, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react'

import { usePrefs } from './prefs.tsx'

/** The panel's least width, px, and the width it opens at before the person has chosen one. */
const FLOOR = 240
const WIDTH = 480
/** The middle column's least width, px: under half of it, it is hidden and the panel takes the room. */
const MAIN = 360
/** How long a burst of keys is waited out before its width is saved, ms. */
const QUIET = 400

/** What the panel's strip reads of the column it stands in. */
export interface Side {
  /** What the middle column is called, in the button that brings it back: "chat", "run". */
  main: string
  /** The middle column is hidden and the panel has the room. */
  hidden: boolean
  show: () => void
  /** The panel is over the whole window. */
  full: boolean
  fill: (on: boolean) => void
}

const Context = createContext<Side | null>(null)

/** Around the middle column's room and the panel: the panel's strip reads it. */
export const SideProvider = Context.Provider

/** The column the panel stands in, or null where it stands in none (a phone's sheet, a host's own column). */
export const useSide = (): Side | null => useContext(Context)

/** The panel's own properties, at a width or with the room. Every key every time, so React and a drag write one set. */
function look(width: number, hidden: boolean): Record<string, string> {
  return hidden
    ? { width: 'auto', flexGrow: '1', flexShrink: '1', minWidth: '0px', maxWidth: 'none', marginInlineStart: '0px', gridColumnStart: 'pane' }
    : { width: `${width}px`, flexGrow: '', flexShrink: '0', minWidth: `${FLOOR}px`, maxWidth: `calc(100vw - ${MAIN}px)`, marginInlineStart: '', gridColumnStart: '' }
}

/** Over the whole window, opaque, above the frame and under the kit's menus. */
const FULL: Record<string, string> = {
  position: 'fixed',
  inset: '0px',
  zIndex: '60',
  width: 'auto',
  maxWidth: 'none',
  minWidth: '0px',
  marginInlineStart: '0px',
  borderRadius: '0px',
  backgroundColor: 'var(--background)',
  backdropFilter: 'none',
}

const write = (el: HTMLElement | null, style: Record<string, string>) => {
  for (const [k, v] of Object.entries(style)) el?.style.setProperty(k.replace(/[A-Z]/g, (c) => `-${c.toLowerCase()}`), v)
}

export interface Split {
  /** For `SideProvider`, around the middle column and the panel. */
  side: Side
  /** The middle column's element: its display is written on it. */
  main: (el: unknown) => void
  /** The panel's column's element: its width, and full screen, are written on it. */
  aside: (el: unknown) => void
  /** The kit `Grip`'s props, but its label and its place; none while the panel is full screen. */
  grip: { side: 'left' | 'right'; span: number; floor: number; ceil: number; reset: number; onSpan: (n: number) => void; onKeep: (n: number) => void; onShut: () => void } | null
}

/**
 * The column, for a host that draws it: `open` is whether the panel is drawn
 * beside the middle column, `main` what that column is called, `onShut` puts
 * the panel away (its edge pulled past its floor).
 */
export function useSplit(open: boolean, main: string, onShut: () => void): Split {
  const { dir } = useI18n()
  const prefs = usePrefs(false)
  const kept = prefs?.prefs.side?.width
  const [width, setWidth] = useState(() => Math.max(FLOOR, kept ?? WIDTH))
  const [hidden, setHidden] = useState(false)
  const [full, setFull] = useState(false)
  const [room, setRoom] = useState(0)
  const [els, setEls] = useState<{ main: HTMLElement | null; aside: HTMLElement | null }>({ main: null, aside: null })
  // What a frame paints from, current without a render: the person's width,
  // the room the two columns share, and the gutter between them.
  const now = useRef({ width, room: 0, gap: 0, hidden })
  now.current.width = width
  now.current.hidden = hidden
  const saving = useRef<{ timer: ReturnType<typeof setTimeout> | undefined; width: number }>({ timer: undefined, width: 0 })

  /**
   * Draws the panel at `n`, or, for a person's pull (`pulled`), with the room;
   * says which, and the width drawn. A window that narrows only squeezes it.
   */
  const paint = useCallback(
    (n: number, pulled = true) => {
      const { room, gap } = now.current
      if (!room) return { hide: false, drawn: n }
      const hide = pulled && room - gap - n < MAIN / 2
      const drawn = hide ? room : Math.max(FLOOR, Math.min(n, room - gap - MAIN))
      write(els.aside, look(drawn, hide))
      if (els.main) els.main.style.display = hide ? 'none' : ''
      return { hide, drawn }
    },
    [els],
  )

  // The room, measured after layout: both columns' widths and the gutter, which
  // a hidden middle column gives to the panel too. A change of window paints
  // the person's width into it again.
  useEffect(() => {
    const { main, aside } = els
    if (!main || !aside || typeof ResizeObserver === 'undefined') return
    const measure = () => {
      // Full screen is the window's, not the room's.
      if (aside.style.position === 'fixed') return
      const shown = main.style.display !== 'none'
      if (shown) now.current.gap = parseFloat(getComputedStyle(aside).marginInlineStart) || 0
      const total = Math.round((shown ? main.getBoundingClientRect().width + now.current.gap : 0) + aside.getBoundingClientRect().width)
      // A drag moves width between the two and leaves their sum; only a new window is news.
      if (Math.abs(total - now.current.room) < 2) return
      now.current.room = total
      setRoom(total)
      if (shown) paint(now.current.width, false)
    }
    const watch = new ResizeObserver(measure)
    watch.observe(main)
    watch.observe(aside)
    return () => watch.disconnect()
  }, [els, paint])

  // The person's width, once their preferences answer with one.
  useEffect(() => {
    if (kept && kept !== now.current.width) setWidth(Math.max(FLOOR, kept))
  }, [kept])

  // The preferences change with every save they answer; the save in waiting reads them current.
  const held = useRef(prefs)
  held.current = prefs
  const flush = useCallback(() => {
    const s = saving.current
    if (s.timer === undefined) return
    clearTimeout(s.timer)
    s.timer = undefined
    // A refusal takes the width back (PrefsProvider), which is the answer.
    held.current?.save({ side: { width: s.width } }).catch(() => {})
  }, [])
  const save = useCallback(
    (n: number) => {
      clearTimeout(saving.current.timer)
      saving.current = { width: n, timer: held.current ? setTimeout(flush, QUIET) : undefined }
    },
    [flush],
  )
  // A save still waiting goes now when the column goes.
  useEffect(() => flush, [flush])

  const keep = useCallback(
    (n: number) => {
      const { hide, drawn } = paint(n)
      setHidden(hide)
      if (hide) return
      setWidth(drawn)
      save(drawn)
    },
    [paint, save],
  )

  // Shut, the panel takes nothing with it: the middle column is back and the window is the frame's.
  useEffect(() => {
    if (open) return
    setHidden(false)
    setFull(false)
  }, [open])

  // Escape leaves full screen, unless something open in it (a menu) took the key first.
  useEffect(() => {
    if (!full) return
    const leave = (e: KeyboardEvent) => {
      if (e.key !== 'Escape' || e.defaultPrevented) return
      setFull(false)
    }
    window.addEventListener('keydown', leave)
    return () => window.removeEventListener('keydown', leave)
  }, [full])

  const show = useCallback(() => {
    setHidden(false)
    const room = now.current.room
    if (room) paint(Math.min(now.current.width, room - now.current.gap - MAIN))
  }, [paint])
  const side = useMemo<Side>(() => ({ main, hidden: open && hidden, show, full: open && full, fill: setFull }), [main, open, hidden, full, show])
  const mainRef = useCallback((el: unknown) => setEls((was) => (was.main === el ? was : { ...was, main: el instanceof HTMLElement ? el : null })), [])
  const asideRef = useCallback((el: unknown) => setEls((was) => (was.aside === el ? was : { ...was, aside: el instanceof HTMLElement ? el : null })), [])

  const across = room ? Math.max(FLOOR, Math.min(width, room - now.current.gap - MAIN)) : width
  // The settled state, on the elements a drag paints. Leaving full screen
  // clears what full screen set, so every key is written every time.
  useLayoutEffect(() => {
    const full = Object.fromEntries(Object.entries(FULL).map(([k, v]) => [k, side.full ? v : '']))
    write(els.aside, { ...full, ...look(across, side.hidden), ...(side.full ? FULL : null) })
    if (els.main) els.main.style.display = side.hidden ? 'none' : ''
  }, [els, across, side.hidden, side.full])
  return {
    side,
    main: mainRef,
    aside: asideRef,
    grip: side.full
      ? null
      : { side: dir === 'rtl' ? 'left' : 'right', span: side.hidden ? room : across, floor: FLOOR, ceil: Math.max(FLOOR, room), reset: WIDTH, onSpan: (n) => void paint(n), onKeep: keep, onShut },
  }
}
