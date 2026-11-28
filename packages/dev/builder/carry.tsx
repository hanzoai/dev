/**
 * What a message carries beside its words, assembled once for Chat and Dev:
 * the conversation's gear (gear.ts) drawn as the `+` menu's shelves and the
 * chips after `+`, and `text` — the words the gear adds to every message, read
 * again whenever the gear changes.
 *
 *   skills    each picked skill's SKILL.md (the org's own, or the brand's
 *             catalogue document): the platform's completion takes no skills
 *             field, so the text rides the message
 *   memory    what Hanzo remembers about the person (GET /v1/ai/memory/list):
 *             the platform injects none itself
 *   mode      Plan: lay the plan out and wait; Auto says nothing; Manual
 *             asks the person before each plugin runs for a message (`Allow`)
 *
 * A shelf is offered only where its service answers; a surface adds its own
 * (Chat its agents, Dev its devices) beside these.
 */
import { SizableText, XStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { BookOpen, ListChecks, Palette, ScrollText } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogDescription, DialogTitle } from '@hanzo/ui'
import { useEffect, useMemo, useRef, useState, type ReactNode } from 'react'

import { call, query, unwrap, type Target } from './api/call.ts'
import { active, authored, brand as brandSkills, document, nameOf as skillOf } from './api/skills.ts'
import { brand, brands } from './api/studio.ts'
import { useRead } from './data.ts'
import { said as designSaid } from './design.ts'
import { DEV_MODES, MODES, useGear, type GearState, type Mode } from './gear.ts'
import type { Composer, Prep, Turn } from './plugin.ts'
import { active as lighted, prep, shelf as pluginShelf, usePlugins } from './plugins/index.ts'
import type { Carried, Modes, Shelf } from '@hanzo/ui/chat'

/** The most of each skill a message carries. */
const SKILL_MAX = 16_000

/** The memories every message carries at most, newest first. */
const RECALL = 20

/** What the surface lends the plugins: its draft, its files, its way about. */
export type Lent = Omit<Composer, 'target' | 'show'>

export interface Carry {
  gear: GearState
  skills: Shelf
  /** The design systems (Studio brands): attach one, or make one by chat. */
  design: Shelf
  /** The plugins that run for this person (`@` lists the same). */
  plugins: Shelf
  /** A dialog a plugin opened, for the surface to draw; null when none is. */
  dialog: ReactNode
  /** Whether a plugin that prepares a message is on. */
  preparing: boolean
  /** What the plugins that are on add to the message about to go. */
  prep: (turn: Omit<Turn, 'design'>, signal: AbortSignal) => Promise<Prep>
  /** The chips after `+`: the skills on, Research, and Plan. */
  on: Carried[]
  /** The mode pill: this surface's modes, the conversation's choice among them. */
  mode: Modes<Mode>
  /** Whether `what` may run: in Manual the person is asked (once, always in this conversation, or not); else yes. */
  allow: (what: Asked) => Promise<boolean>
  /** The words the gear adds to every message, '' when none. */
  text: string
}

/** What Hanzo remembers about the person, newest first, as lines. */
async function remembered(t: Target): Promise<string[]> {
  const rows = unwrap(await call<unknown>(t, 'GET', `/v1/ai/memory/list${query({ limit: RECALL })}`))
  return (Array.isArray(rows) ? rows : [])
    .map((r) => (r && typeof r === 'object' ? String((r as { content?: unknown }).content ?? '') : ''))
    .filter(Boolean)
}

/**
 * The words skills add to a message: each one's SKILL.md (the org's own before
 * the brand's), at most `max` of each, or its name when no document answers.
 * Chat carries the ones Hanzo chose for a single message the same way
 * (hanzo-inc/app issue 314).
 */
export async function skillsSaid(t: Target, skills: readonly string[], max = SKILL_MAX): Promise<string> {
  if (!skills.length) return ''
  const own = t.token() ? await authored(t).catch(() => []) : []
  const parts: string[] = []
  for (const n of skills) {
    const body = own.find((s) => s.name === n)?.content || (await document(t, n).catch(() => ''))
    parts.push(body ? `Use the skill "${n}" where it applies:\n\n${body.slice(0, max)}` : `Use the "${n}" skill where it applies.`)
  }
  return parts.join('\n\n')
}

/** The words `skills`, `memory` and `mode` add to a message. */
async function said(t: Target, skills: readonly string[], memory: boolean, mode: string, design: string): Promise<string> {
  const parts: string[] = []
  if (design) {
    const b = await brand(t, design).catch(() => null)
    if (b?.name) parts.push(designSaid(b))
  }
  const taught = await skillsSaid(t, skills)
  if (taught) parts.push(taught)
  if (memory) {
    const lines = await remembered(t).catch(() => [])
    if (lines.length) parts.push(`What you remember about the person:\n${lines.map((l) => `- ${l}`).join('\n')}`)
  }
  if (mode === 'plan') parts.push('Plan mode: lay out the plan — the steps, what each changes and why — and stop. Change nothing and produce no final work until the person says go.')
  return parts.join('\n\n')
}

/** What the person answers a plugin about to run in Manual. */
type Leave = 'once' | 'always' | 'deny'

/** What Manual asks about: a plugin, or a Dev run. `id` is what "always" remembers. */
export interface Asked {
  id: string
  name: string
  note: string
}

/** Manual's pause: what is about to run, what it does, and the three answers. Closing it is a no. */
function Allow({ what, onAnswer }: { what: Asked; onAnswer: (v: Leave) => void }) {
  const { t } = useI18n()
  return (
    <Dialog open onOpenChange={(o) => !o && onAnswer('deny')}>
      <DialogContent maxW={440} width="100%" data-slot="allow">
        <DialogTitle>{t('Allow {name}?', { name: t(what.name) })}</DialogTitle>
        <DialogDescription>{what.note ? `${t(what.note)}. ${t('Manual asks before each one runs.')}` : t('Manual asks before each one runs.')}</DialogDescription>
        <XStack justify="flex-end" gap="$2" pt="$3" flexWrap="wrap">
          <Button size="sm" variant="ghost" onPress={() => onAnswer('deny')}>
            {t('Deny')}
          </Button>
          <Button size="sm" variant="outline" onPress={() => onAnswer('always')}>
            <SizableText fontSize={14}>{t('Always in this chat')}</SizableText>
          </Button>
          <Button size="sm" onPress={() => onAnswer('once')}>
            {t('Allow once')}
          </Button>
        </XStack>
      </DialogContent>
    </Dialog>
  )
}

/**
 * The conversation `key` names (`''` for one not begun), for target `t`.
 * `customize(tab)` opens Dev's Customize on that tab; `modes` are the ones this
 * surface offers (a choice it does not offer reads as Auto).
 */
export function useCarry(t: Target, key: string, customize: (tab: 'skills') => void, lent: Lent, modes: readonly Mode[] = DEV_MODES): Carry {
  const { t: say } = useI18n()
  const gear = useGear(t, key)
  const [dialog, show] = useState<ReactNode>(null)
  // The composer a plugin acts on, read when it acts: the draft moves under it.
  const lentNow = useRef(lent)
  lentNow.current = lent
  const composer = useMemo<Composer>(
    () => ({
      target: t,
      get draft() {
        return lentNow.current.draft
      },
      setDraft: (x) => lentNow.current.setDraft(x),
      attach: (f) => lentNow.current.attach(f),
      go: (h) => lentNow.current.go(h),
      show,
    }),
    [t],
  )
  const found = usePlugins(t)
  const signed = Boolean(t.token())
  const list = useRead(
    async () => {
      const [act, own, cat] = await Promise.all([
        signed ? active(t).catch(() => []) : Promise.resolve([]),
        signed ? authored(t).catch(() => []) : Promise.resolve([]),
        brandSkills(t).catch(() => ({ skills: [], products: [] })),
      ])
      const localSkills: string[] = []
      try {
        const raw = typeof localStorage !== 'undefined' ? localStorage.getItem('hanzo_enterprise_settings_v1:plugins') : null
        if (raw) {
          const parsed = JSON.parse(raw) as { skillsAndPlugins?: { status?: string; name?: string }[] }
          if (Array.isArray(parsed?.skillsAndPlugins)) {
            for (const item of parsed.skillsAndPlugins) {
              if (item?.status === 'enabled' && typeof item?.name === 'string') {
                localSkills.push(item.name)
              }
            }
          }
        }
      } catch {
        // ignore
      }
      const standard = [
        'code-review',
        'simplify',
        'security-review',
        'bug-finder',
        'test-writer',
        'refactor',
        'frontend-design',
        'api-design',
        'sql-optimizer',
        'ui-port',
        'app-port',
        'hanzo-cloud',
        'hanzo-base',
        'hanzo-code',
        'hanzo-dev',
        'hanzo-cli',
        'hanzo-commerce',
        'hanzo-bot',
        'hanzo-agent',
        'hanzo-security-review',
        'pdf-analysis',
        'xlsx-modeling',
        'pptx-deck-builder',
        'docx-redline',
        'scientific-research',
        'mcp-builder',
        'webapp-testing',
      ]
      const seen = new Set<string>()
      const out: string[] = []
      for (const n of [
        ...act.map((s) => skillOf(s.name)),
        ...own.map((s) => s.name),
        ...localSkills,
        ...standard,
        ...(Array.isArray(cat?.skills) ? cat.skills.map((s) => s.name) : []),
      ]) {
        const clean = typeof n === 'string' ? n.trim() : ''
        if (clean && !seen.has(clean)) {
          seen.add(clean)
          out.push(clean)
        }
      }
      return out
    },
    [],
    [t, signed],
  )
  const names = list.value
  const { skills, research, memory, design } = gear.gear
  const mode: Mode = modes.includes(gear.gear.mode) ? gear.gear.mode : 'auto'

  const shelf: Shelf = {
    rows: names.map((n) => ({ id: n, label: n, icon: <ScrollText size={16} />, on: skills.includes(n) })),
    pick: (id) => gear.toggle('skills', id),
    loading: list.loading,
    error: list.error ? say('The skills could not be read.') : null,
    search: names.length > 8 ? say('Search skills') : undefined,
    empty: say('No skills are on for this organization'),
    manage: { label: say('Manage skills'), onPress: () => customize('skills') },
    browse: { label: say('Browse skills'), onPress: () => customize('skills') },
  }

  const plugins = pluginShelf(found, gear, composer, say)

  // The design systems: Studio's brands, the one store for them.
  const kits = useRead(signed ? () => brands(t) : null, [], [t, signed, design])
  const named = kits.value.find((b) => b.id === design)
  const designs: Shelf = {
    rows: kits.value.map((b) => {
      return { id: b.id, label: b.name, note: [b.tokens['--primary'], b.tokens['--accent']].filter((x) => typeof x === 'string').join(' '), icon: <Palette size={16} />, on: b.id === design }
    }),
    pick: (id) => gear.set({ design: design === id ? '' : id }),
    loading: kits.loading,
    error: kits.error ? say('The design systems could not be read.') : null,
    empty: say('No design systems yet'),
    create: {
      label: say('Create design system'),
      onPress: () => {
        if (!gear.gear.plugins.includes('design')) gear.toggle('plugins', 'design')
        // The person's own words to finish, so in their language.
        lentNow.current.setDraft(say('Make me a design system for '))
      },
    },
  }
  const lit = lighted(found.plugins, gear.gear)

  const on: Carried[] = [
    ...skills.map((n) => ({ id: `skill:${n}`, label: n, icon: <ScrollText size={14} />, onRemove: () => gear.toggle('skills', n) })),
    ...(research ? [{ id: 'research', label: say('Research'), icon: <BookOpen size={14} />, onRemove: () => gear.set({ research: false }) }] : []),
    ...(mode === 'plan' ? [{ id: 'plan', label: say('Plan'), icon: <ListChecks size={14} />, onRemove: () => gear.set({ mode: 'auto' }) }] : []),
    ...(design ? [{ id: 'design', label: named?.name ?? say('Design system'), icon: <Palette size={14} />, onRemove: () => gear.set({ design: '' }) }] : []),
    ...lit
      .filter((p) => p.id !== 'research')
      .map((p) => ({ id: `plugin:${p.id}`, label: say(p.name), icon: p.icon, onRemove: () => gear.toggle('plugins', p.id) })),
  ]

  const signedIn = Boolean(t.token())
  const [text, setText] = useState('')
  const asked = skills.join('\n')
  useEffect(() => {
    if (!signedIn && !skills.length) {
      setText(mode === 'plan' ? 'Plan mode: lay out the plan and stop until the person says go.' : '')
      return
    }
    let live = true
    void said(t, skills, signedIn ? memory : false, mode, signedIn ? design : '').then((x) => live && setText(x))
    return () => {
      live = false
    }
    // `skills` is read through `asked`, its contents.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [t, signedIn, asked, memory, mode, design])

  // Manual: what is about to run is asked of the person, unless allowed for the rest of the conversation.
  const kept = useRef<string[]>([])
  kept.current = gear.gear.allowed
  const allow = async (what: Asked): Promise<boolean> => {
    if (mode !== 'manual' || kept.current.includes(what.id)) return true
    const v = await new Promise<Leave>((done) =>
      show(
        <Allow
          what={what}
          onAnswer={(x) => {
            show(null)
            done(x)
          }}
        />,
      ),
    )
    if (v === 'always') {
      kept.current = [...kept.current, what.id]
      gear.set({ allowed: kept.current })
    }
    return v !== 'deny'
  }

  return {
    gear,
    mode: { value: mode, set: (m) => gear.set({ mode: m }), items: MODES.filter((x) => modes.includes(x.id)) },
    allow,
    skills: shelf,
    plugins,
    dialog,
    on,
    text,
    preparing: lit.length > 0,
    design: designs,
    // A design system a plugin made is attached at once, and the maker turns off: it made one.
    prep: async (turn, signal) => {
      // One plugin at a time, so each pause is its own question.
      const denied: string[] = []
      for (const x of lit) if (!signal.aborted && !(await allow({ id: x.id, name: x.name, note: x.note }))) denied.push(x.id)
      const p = await prep(found.plugins, gear.gear, { ...turn, design }, composer, signal, denied, say)
      if (p.design) gear.set({ design: p.design, plugins: gear.gear.plugins.filter((x) => x !== 'design') })
      return p
    },
  }
}
