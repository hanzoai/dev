/**
 * Activity's words: what a run's state is called and drawn as, how long it
 * took, what it cost, and the one line under its title. Pure, so the screen and
 * its tests read the same sentences.
 */
import { phrase } from '@hanzo/i18n'
import type { Say } from '@hanzo/i18n/react'
import type { SessionStatus } from '@hanzo/ui/chat'

import { ago } from './ago.ts'
import type { Job, State } from './api/auto.ts'
import { SOURCE } from './api/library.ts'
import { english } from './say.ts'

/** A job's state as the rail's dot draws it, and in words. */
export const DOTS: Record<State, SessionStatus> = { scheduled: 'idle', running: 'running', succeeded: 'done', failed: 'error', cancelled: 'stopped' }
export const SAID: Record<State, string> = {
  scheduled: phrase('Scheduled'),
  running: phrase('Running'),
  succeeded: phrase('Succeeded'),
  failed: phrase('Failed'),
  cancelled: phrase('Cancelled'),
}

/** How long a run took, or has taken so far. */
export function lasted(started: string, finished: string | null, now = Date.now()): string {
  const a = Date.parse(started)
  const b = finished ? Date.parse(finished) : now
  if (Number.isNaN(a) || Number.isNaN(b) || b < a) return ''
  const s = Math.round((b - a) / 1000)
  if (s < 60) return `${s}s`
  const m = Math.floor(s / 60)
  if (m < 60) return `${m}m ${s % 60}s`
  const h = Math.floor(m / 60)
  return `${h}h ${m % 60}m`
}

/** Cents as dollars. */
export const dollars = (cents: number): string => `$${(cents / 100).toFixed(2)}`

/** One line about a job: when it started, how long, what it cost, what it made. Its words in the language of `t`. */
export function about(j: Job, now = Date.now(), t: Say = english): string {
  const made = j.outputs.length ? (j.outputs.length === 1 ? t('{n} output', { n: 1 }) : t('{n} outputs', { n: j.outputs.length })) : ''
  return [SOURCE[j.product] ? t(SOURCE[j.product]) : '', j.started ? ago(j.started) : '', lasted(j.started, j.finished, now), j.cost !== null ? dollars(j.cost) : '', made].filter(Boolean).join(' · ')
}

/** The jobs, newest first, with `j` in its place. */
export function merged(list: Job[], j: Job): Job[] {
  const out = list.filter((x) => x.id !== j.id)
  out.push(j)
  return out.sort((a, b) => (b.started || b.updated).localeCompare(a.started || a.updated))
}

/** A task's name from what it does: its first line, short. */
export function titled(prompt: string): string {
  const first = prompt.trim().split('\n')[0]!.trim()
  return first.length > 60 ? `${first.slice(0, 57).trimEnd()}…` : first
}
