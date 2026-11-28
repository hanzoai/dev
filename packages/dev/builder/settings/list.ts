/**
 * The sections of Settings, in the order the page lists them, by name and part
 * alone: what a sidebar lists without loading the sections themselves
 * (sections.tsx pairs each with its body).
 *
 * A part's id is its name, and every name is English, marked (`phrase`): a
 * list draws each through `t`.
 */
import { phrase } from '@hanzo/i18n'

import type { Section } from '../route.ts'

export type Group = 'Settings' | 'Code' | 'Organization'
export const GROUPS = [phrase('Settings'), phrase('Code'), phrase('Organization')] as readonly Group[]

export const LIST: readonly { id: Section; label: string; group: Group }[] = [
  { id: 'general', label: phrase('General'), group: 'Settings' },
  { id: 'account', label: phrase('Account'), group: 'Settings' },
  { id: 'privacy', label: phrase('Privacy'), group: 'Settings' },
  { id: 'billing', label: phrase('Billing'), group: 'Settings' },
  { id: 'usage', label: phrase('Usage'), group: 'Settings' },
  { id: 'capabilities', label: phrase('Capabilities'), group: 'Settings' },
  { id: 'memory', label: phrase('Memory'), group: 'Settings' },
  { id: 'code', label: phrase('Code'), group: 'Code' },
  { id: 'environments', label: phrase('Environments'), group: 'Code' },
  { id: 'machines', label: phrase('Machines'), group: 'Code' },
  { id: 'keys', label: phrase('API keys'), group: 'Code' },
  { id: 'members', label: phrase('Members'), group: 'Organization' },
  { id: 'integrations', label: phrase('Integrations'), group: 'Organization' },
  { id: 'notifications', label: phrase('Notifications'), group: 'Organization' },
]

/** A part's label as a list draws it: none for the first part, or where the part's first row already says it (Code). */
export const heading = (g: Group): string => (g === 'Settings' || LIST.find((e) => e.group === g)?.label === g ? '' : g)
