/**
 * What a screen does with the spend questions it asks, apart from any dialog:
 * the one rule for a yes. A yes counts only if it is strictly `true`, arrives
 * while the screen that asked is still here, and answers the latest question it
 * asked: a newer press supersedes one still waiting, so a host that never
 * answers leaves nothing stuck, and two quick presses spend once. A throw is a no.
 */
import type { Say } from '@hanzo/i18n/react'

import type { Ask, Spend } from './host.tsx'

/**
 * Where a host may keep "Don't ask again" for a question: its kind and what it
 * uses, so one yes covers only the same kind of spend on the same kind of thing.
 * A 24/7 bot never: each has its own budget and period, so every one is asked.
 */
export const spend = (a: Ask): Spend => (a.kind === 'bot' ? { ...a, kind: 'bot', again: null } : { ...a, kind: a.kind, again: `${a.kind}.${a.uses}` })

/** A question's key alone. */
export const again = (a: Ask): string | null => spend(a).again

export interface Asker {
  ask: (a: Ask) => Promise<boolean>
  /** The screen is back (mounted). */
  enter: () => void
  /** The screen has gone: every answer still on its way counts as a no. */
  leave: () => void
}

export function asker(confirm: (spend: Spend) => Promise<boolean>): Asker {
  let turn = 0
  let here = true
  return {
    ask: async (a) => {
      const mine = ++turn
      let yes: unknown
      try {
        yes = await confirm(spend(a))
      } catch {
        return false
      }
      return yes === true && here && mine === turn
    },
    enter: () => {
      here = true
    },
    leave: () => {
      here = false
      turn++
    },
  }
}

/** The question before Run now, in the list and in the editor alike: one run, billed, now. */
export const runNow = (name: string, t: Say): Ask => ({
  kind: 'task-run',
  title: t('Run {name} now?', { name }),
  what: t('This starts one run of {name} right away, apart from its schedule.', { name }),
  yes: t('Run now'),
  uses: 'credits',
})

/** The question before a paused task is switched on: from then it runs, and is billed, by itself. */
export const switchOn = (name: string, t: Say): Ask => ({
  kind: 'task',
  title: t('Turn {name} on?', { name }),
  what: t('It runs by itself on its schedule until you pause or delete it.'),
  yes: t('Turn on'),
  uses: 'credits',
})

/** The question before a template is copied: in a code workspace the platform made, the copy is built and published at once. */
export const copying = (name: string, t: Say): Ask => ({
  kind: 'publish',
  title: t('Start from {template}?', { template: name }),
  what: t('This copies {template} into a new project, then builds and publishes it.', { template: name }),
  yes: t('Start'),
  uses: 'computer',
})

/** The question before a project is published again: its repository is built again, in a cloud sandbox. */
export const republish = (name: string, t: Say): Ask => ({
  kind: 'publish',
  title: t('Publish {name} again?', { name }),
  what: t('This builds {name} again from its repository and publishes it.', { name }),
  yes: t('Publish again'),
  uses: 'computer',
})
