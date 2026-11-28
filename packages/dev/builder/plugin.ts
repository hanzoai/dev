/**
 * A plugin: one thing the composer can do beyond words, picked from `+` ›
 * Plugins or by typing `@`. Chat and Dev offer the same list (`usePlugins`,
 * plugins/), each one running on Hanzo's own services.
 *
 * A plugin acts in one of two ways, or both:
 *
 *   open      picked, it shows something of its own (a sketch pad, a picker)
 *             and hands its result back to the composer: a file, the draft.
 *   prepare   while it is on in the conversation (Gear.plugins), it is asked
 *             just before each message goes, and what it answers rides that
 *             message: words for the leading system turn, files, or an answer
 *             of its own that replaces the model's (`reply`).
 *
 * A plugin whose service is not there for this person is not listed: `usePlugins`
 * answers only the ones that run.
 */
import type { ReactNode } from 'react'

import type { Target } from './api/call.ts'

/** What a plugin may do to the composer it was picked from. */
export interface Composer {
  target: Target
  draft: string
  setDraft: (text: string) => void
  /** Hold files with the next message, as the paperclip does. */
  attach: (files: File[]) => void
  /** Draw a dialog over the page; null takes it down. */
  show: (node: ReactNode | null) => void
  /** Move the app (a path of the host's, or an address on another origin). */
  go: (href: string) => void
}

/** The message about to go. */
export interface Turn {
  text: string
  files: File[]
  /** The conversation's id, or '' for one not begun. */
  thread: string
  /** The design system attached to the conversation (a Studio brand id), or ''. */
  design: string
}

/** What a plugin adds to the message. */
export interface Prep {
  /** Appended to the leading system turn. */
  system?: string
  /** Files to send beside the person's. */
  files?: File[]
  /** An answer of the plugin's own, in markdown; the model is not asked. */
  reply?: string
  /** A design system the plugin made: attached to the conversation from then on. */
  design?: string
}

export interface Plugin {
  id: string
  name: string
  /** One line: what it does. */
  note: string
  icon: ReactNode
  open?: (c: Composer) => void
  prepare?: (turn: Turn, c: Composer, signal: AbortSignal) => Promise<Prep>
}
