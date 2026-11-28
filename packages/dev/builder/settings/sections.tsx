/**
 * The sections of Settings, in the order the page lists them. A section is one
 * file under settings/ and one entry here; the route grammar names every
 * section there can be (route.ts SECTIONS), and this lists the ones built.
 */
import type { ReactNode } from 'react'

import type { Section } from '../route.ts'
import { LIST, type Group } from './list.ts'
import { Account } from './account.tsx'
import { Billing } from './billing.tsx'
import { Capabilities } from './capabilities.tsx'
import { Code } from './code.tsx'
import { Environments } from './environments.tsx'
import { General } from './general.tsx'
import { Integrations } from './integrations.tsx'
import { Keys } from './keys.tsx'
import { Machines } from './machines.tsx'
import { Members } from './members.tsx'
import { Memory } from './memory.tsx'
import { Notifications } from './notifications.tsx'
import { Privacy } from './privacy.tsx'
import { Usage } from './usage.tsx'

export { GROUPS, type Group } from './list.ts'

export interface Entry {
  id: Section
  label: string
  group: Group
  body: () => ReactNode
}

const BODY: Record<Section, () => ReactNode> = {
  general: () => <General />,
  account: () => <Account />,
  privacy: () => <Privacy />,
  billing: () => <Billing />,
  usage: () => <Usage />,
  capabilities: () => <Capabilities />,
  memory: () => <Memory />,
  code: () => <Code />,
  environments: () => <Environments />,
  machines: () => <Machines />,
  keys: () => <Keys />,
  members: () => <Members />,
  integrations: () => <Integrations />,
  notifications: () => <Notifications />,
}

export const ENTRIES: Entry[] = LIST.map((e) => ({ ...e, body: BODY[e.id] }))
