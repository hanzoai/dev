/**
 * The plugins the composer offers, from `+` › Plugins and by typing `@`: only
 * those that run for this person, each on Hanzo's own services.
 *
 *   Work in a project   the org has a project (GET /v1/project)
 *   Deep research       signed in: web search, page reading and a Hanzo model, as the person
 *   Sketch              always: it draws in the page
 *   Template Creator    an org admin: only an admin writes the org's skills
 *   Presentations       Studio runs its `slides` template (GET /v1/studio/templates)
 *   PDF                 signed in: read and verify in the page; create where Studio runs `document`
 *
 * What is there is read once per platform, org and sign-in, and kept for the page.
 *
 * Deep research IS the Research switch: its row turns `Gear.research` on and
 * off, and `prep` asks it when that is on. Every other plugin that prepares is on
 * while its id is in `Gear.plugins`; one that opens something runs when picked.
 */
import type { Say } from '@hanzo/i18n/react'
import { useEffect, useState } from 'react'

import type { Target } from '../api/call.ts'
import { templates } from '../api/studio.ts'
import { projects } from '../api/projects.ts'
import { administers } from '../claims.ts'
import { say } from '../customize/say.ts'
import type { Gear, GearState } from '../gear.ts'
import type { Composer, Plugin, Prep, Turn } from '../plugin.ts'
import type { Shelf } from '@hanzo/ui/chat'
import { path } from '../route.ts'
import { english } from '../say.ts'
import { pdf } from './pdf.tsx'
import { project } from './project.tsx'
import { designer } from './design.tsx'
import { deep } from './research.tsx'
import { sketch } from './sketch.tsx'
import { slides } from './slides.tsx'
import { template } from './template.tsx'

export { research } from './research.tsx'

export interface Plugins {
  plugins: Plugin[]
  loading: boolean
  error: string | null
}

/** Where a plugin of the org's own is built: Customize › Plugins. */
export const BUILD = path({ kind: 'customize', tab: 'plugins' })

const key = (t: Target): string => `${t.api}|${t.org ?? ''}|${t.token() ? 1 : 0}`
const seen = new Map<string, Promise<Plugin[]>>()

/** What runs for this person, in the menu's order. */
export async function probe(t: Target): Promise<Plugin[]> {
  if (!t.token()) return [deep, sketch]
  const [built, studio] = await Promise.allSettled([projects(t), templates(t)])
  if (built.status === 'rejected' && studio.status === 'rejected') throw built.reason
  const runs = studio.status === 'fulfilled' ? studio.value : []
  return [
    built.status === 'fulfilled' && built.value.length ? project : null,
    deep,
    sketch,
    administers(t.token(), t.org) ? template : null,
    runs.includes('slides') ? slides : null,
    studio.status === 'fulfilled' ? designer : null,
    pdf(runs.includes('document')),
  ].filter((p): p is Plugin => p !== null)
}

export function usePlugins(t: Target): Plugins {
  const k = key(t)
  const [state, setState] = useState<Plugins>({ plugins: [], loading: true, error: null })
  useEffect(() => {
    let live = true
    let p = seen.get(k)
    if (!p) {
      p = probe(t)
      seen.set(k, p)
      p.catch(() => seen.delete(k))
    }
    p.then(
      (plugins) => live && setState({ plugins, loading: false, error: null }),
      (e: unknown) => live && setState({ plugins: [], loading: false, error: say(e) }),
    )
    return () => {
      live = false
    }
    // `k` is the target, as far as what runs is concerned.
  }, [k])
  return state
}

/** Whether a plugin that prepares is on in this conversation. */
const on = (p: Plugin, g: Gear): boolean => (p.id === deep.id ? g.research : g.plugins.includes(p.id))

/** The `+` menu's Plugins shelf, in the words of the caller's `t` (a plugin's name and note are marked where it is written). */
export function shelf(p: Plugins, g: GearState, c: Composer, t: Say = english): Shelf {
  return {
    rows: p.plugins.map((x) => ({ id: x.id, label: t(x.name), note: t(x.note), icon: x.icon, on: x.prepare ? on(x, g.gear) : undefined })),
    pick: (id) => {
      const x = p.plugins.find((y) => y.id === id)
      if (!x) return
      if (x.open) x.open(c)
      else if (x.id === deep.id) g.set({ research: !g.gear.research })
      else g.toggle('plugins', x.id)
    },
    loading: p.loading,
    error: p.error,
    search: t('Type to search plugins'),
    empty: t('No plugins here'),
    create: { label: t('Create plugin'), onPress: () => c.go(BUILD) },
  }
}

/** The plugins this message asks, in the menu's order. */
export const active = (plugins: readonly Plugin[], g: Gear): Plugin[] => plugins.filter((p) => p.prepare && on(p, g))

/** Several plugins' preparations as one: system parts and replies in order, files together, the design system one made. */
export function merge(preps: readonly Prep[]): Prep {
  const join = (k: 'system' | 'reply') => preps.map((p) => p[k]?.trim()).filter(Boolean).join('\n\n') || undefined
  const files = preps.flatMap((p) => p.files ?? [])
  const out: Prep = {}
  const system = join('system')
  const reply = join('reply')
  // A design system a plugin made is attached by the host (carry.tsx); dropped here, it never was.
  const design = preps.map((p) => p.design).filter(Boolean).at(-1)
  if (system) out.system = system
  if (reply) out.reply = reply
  if (files.length) out.files = files
  if (design) out.design = design
  return out
}

/**
 * Ask every plugin that is on what the message carries. A plugin that fails
 * fails the message, with its name and its reason: a report asked for and not
 * researched must not arrive looking researched. Its name is said in the
 * caller's `t`; the reason is `say`'s.
 */
export async function prep(plugins: readonly Plugin[], g: Gear, turn: Turn, c: Composer, signal: AbortSignal, denied: readonly string[] = [], t: Say = english): Promise<Prep> {
  const preps = await Promise.all(
    active(plugins, g)
      .filter((p) => !denied.includes(p.id))
      .map((p) =>
        p.prepare!(turn, c, signal).catch((e: unknown) => {
          if (signal.aborted) throw e
          throw new Error(t('{plugin}: {reason}', { plugin: t(p.name), reason: say(e) }))
        }),
      ),
  )
  return merge(preps)
}
