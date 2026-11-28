/**
 * What a person's work recommends: the skills the catalogue marks (`Entry.works`) for it, so a
 * new person starts with the ones that matter to them switched on rather than a list of hundreds.
 * The work is the account's own (Settings > Account, `Prefs.work`), one list for both.
 */
import { phrase } from '@hanzo/i18n'

import type { Catalogue, Entry } from './api/skills.ts'

/** What a person may say they work on, each label English and marked (`phrase`), drawn through `t`. */
export const WORK = [
  { id: 'director', label: phrase('Director or executive') },
  { id: 'engineering', label: phrase('Software engineering') },
  { id: 'data', label: phrase('Data science') },
  { id: 'design', label: phrase('Design') },
  { id: 'product', label: phrase('Product management') },
  { id: 'research', label: phrase('Research') },
  { id: 'writing', label: phrase('Writing') },
  { id: 'education', label: phrase('Education') },
  { id: 'operations', label: phrase('Operations') },
  { id: 'marketing', label: phrase('Marketing and sales') },
  { id: 'other', label: phrase('Something else') },
]

/** The most skills one work recommends: a set small enough to read before it is added. */
export const MOST = 8

/** The catalogue's skills that recommend themselves for `work`, in catalogue order, at most `MOST`. */
export function recommend(work: string | undefined, catalogue: Catalogue | null): Entry[] {
  if (!work || !catalogue) return []
  return catalogue.skills.filter((e) => e.works.includes(work)).slice(0, MOST)
}
