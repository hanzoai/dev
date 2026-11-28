/**
 * Which model answers, and how hard it thinks: one store for Chat and Dev, and
 * the catalog both offer. The composer (@hanzo/ui/chat) changes it.
 *
 *   GET /v1/models   the catalog, read once per platform and shared
 *   hanzo.mind       the person's choice, in this browser: {model, effort}
 *
 * The effort is always under the composer; the model chip only where the person
 * turned it on (Settings → Appearance › Model in composer). Enso
 * routes every ask — a message in Chat, a step in Dev — so naming a model is an
 * override, and the panel says so before it offers one. A premium model is
 * picked for the screen it was picked on (`usePick`) and never kept, so the next
 * conversation or run opens on what was kept, Enso until something else is.
 *
 * The efforts are Auto, High and Max, Chat's words (Hanzo's
 * ThinkingEffort), Auto until another is chosen. A coding run takes low, medium
 * or high (apps/coding `efforts`), so `level` is what a run names: medium for
 * Auto, what a run took before anyone chose, and high for the rest, the most it
 * takes. Auto names one rather than none, because a run that names none takes
 * an effort the person saved to their account once (apps/coding start.go) and
 * no screen shows any more.
 */
import { phrase } from '@hanzo/i18n'
import type { Modes } from '@hanzo/ui/chat'
import { EFFORTS as EVERY } from '@hanzo/ui/chat/pure'
import { parseModels, thirdParty } from '@hanzo/ui/models/catalog'
import { useEffect, useState, useSyncExternalStore } from 'react'

import { prepaid as readPrepaid } from './api/billing.ts'
import { Refusal, type Target } from './api/call.ts'
import { ENSO, models as read, type Model } from './api/models.ts'
import { usePick } from './pick.ts'

export type Effort = 'auto' | 'low' | 'high' | 'max'

const EFFORT_LABELS: Partial<Record<Effort, string>> = {
  low: phrase('Fast'),
}

const EFFORT_NOTES: Record<Effort, string> = {
  auto: phrase('Matches thinking depth to the complexity of each prompt.'),
  low: phrase('Fastest edits and replies for straightforward coding tasks.'),
  high: phrase('Spends extra time reasoning through multi-step tasks and code.'),
  max: phrase('Deepest reasoning and verification for complex architecture.'),
}

/** The efforts a Dev run offers, in order: Auto (medium), Fast (low), High, and Max. Marked English, drawn through `t`. */
export const EFFORTS = (['auto', 'low', 'high', 'max'] as const).map((id) => {
  const base = EVERY.find((e) => e.id === id)
  return {
    id,
    label: EFFORT_LABELS[id] ?? base?.label ?? id,
    note: EFFORT_NOTES[id] ?? base?.note ?? '',
  }
})

/** The effort a coding run names (apps/coding takes low, medium or high): low for Fast, medium for Auto, high for High/Max. */
export const level = (effort: Effort): 'low' | 'medium' | 'high' =>
  effort === 'low' ? 'low' : effort === 'auto' ? 'medium' : 'high'

/** What a person chose: the model (Enso routes) and the effort. */
export interface Choice {
  model: string
  effort: Effort
}

export const FIRST: Choice = { model: ENSO, effort: 'auto' }

/** Whether a plan grants only Enso Auto. An unknown plan is not taken for Free. */
export const isFree = (plan: string | null | undefined): boolean => plan === 'free'

const KEY = 'hanzo.mind'
const CHANGE = 'hanzo:mind'
const PREFS_KEY = 'hanzo.chat.preferences'
const PREFS_EVENT = 'hanzo:chat:preferences'

/** Whether third-party cloud models are enabled in Settings → Preferences (off by default). */
export function allowThirdPartyModels(): boolean {
  try {
    const v = JSON.parse(store()?.getItem(PREFS_KEY) ?? 'null') as {
      allowThirdPartyModels?: unknown
      thirdPartyChosen?: unknown
    } | null
    return v?.thirdPartyChosen ? Boolean(v.allowThirdPartyModels) : v?.allowThirdPartyModels === true
  } catch {
    return false
  }
}

function useAllowThirdPartyModels(): boolean {
  const [allowed, setAllowed] = useState(allowThirdPartyModels)
  useEffect(() => {
    const sync = () => setAllowed(allowThirdPartyModels())
    window.addEventListener(PREFS_EVENT, sync)
    window.addEventListener('storage', sync)
    return () => {
      window.removeEventListener(PREFS_EVENT, sync)
      window.removeEventListener('storage', sync)
    }
  }, [])
  return allowed
}

/** An effort kept, read back: one the ladder still offers, XHigh (which Max replaced) as Max, else Auto. */
const effortOf = (v: unknown): Effort => (v === 'xhigh' ? 'max' : EFFORTS.some((e) => e.id === v) ? (v as Effort) : FIRST.effort)

/** The store, or null where the browser gives none (a server render, blocked site data). */
function store(): Storage | null {
  try {
    return typeof window === 'undefined' ? null : window.localStorage
  } catch {
    return null
  }
}

let raw: string | null | undefined
let held: Choice = FIRST
/** A choice the browser would not store, held for the page. */
let memo: Choice | null = null

/** The kept choice, Enso and Auto where nothing is kept. Stable while the stored text is. Low and Medium, offered once, read as Auto. */
export function kept(): Choice {
  if (memo) return memo
  let now: string | null = null
  try {
    now = store()?.getItem(KEY) ?? null
  } catch {
    now = null
  }
  if (now === raw) return held
  raw = now
  try {
    const v = JSON.parse(now ?? 'null') as Partial<Choice> | null
    held = { model: typeof v?.model === 'string' && v.model.trim() ? v.model : FIRST.model, effort: effortOf(v?.effort) }
  } catch {
    held = FIRST
  }
  return held
}

/** Keep a change, in this browser, for every surface that reads it. */
export function keep(patch: Partial<Choice>): void {
  const was = kept()
  const next = { ...was, ...patch, effort: patch.effort === undefined ? was.effort : effortOf(patch.effort) }
  try {
    store()?.setItem(KEY, JSON.stringify(next))
    memo = null
  } catch {
    // A browser that refuses storage keeps the choice for this page.
    memo = next
  }
  if (typeof window !== 'undefined') window.dispatchEvent(new Event(CHANGE))
}

function subscribe(change: () => void): () => void {
  window.addEventListener(CHANGE, change)
  window.addEventListener('storage', change)
  return () => {
    window.removeEventListener(CHANGE, change)
    window.removeEventListener('storage', change)
  }
}

/** The kept choice and the way to change it. The server and the first render read the default. */
export function useChoice(): readonly [Choice, (patch: Partial<Choice>) => void] {
  return [useSyncExternalStore(subscribe, kept, () => FIRST), keep] as const
}

/** A model's name as the catalog spells it; Enso Auto for the router. */
export function nameOf(model: string, catalog: readonly Model[]): string {
  if (model === ENSO) return 'Enso Auto'
  const row = catalog.find((m) => m.id === model)
  return row?.name ?? spelled(model)
}

/** An id said as a name: its last path part, each word capitalised (`enso-flash` → Enso Flash). */
export function spelled(id: string): string {
  const tail = id.split('/').pop() ?? id
  return tail
    .split(/[-_]/)
    .filter(Boolean)
    .map((w) => w[0]!.toUpperCase() + w.slice(1))
    .join(' ')
}

/** The ids that are the router under another name: the picker's Enso row stands for all of them. */
export const ROUTER_IDS: ReadonlySet<string> = new Set([ENSO, 'enso', 'hanzo/enso'])

// The catalog, once per platform for the page: Chat and Dev read the same list.
const catalogs = new Map<string, Promise<Model[]>>()
const settled = new Map<string, Model[]>()

/** Where this browser keeps the last list it read. */
const LAST = 'hanzo.build.models'

/** The router alone: what a chooser offers before any list was ever read, so it reads "Enso Auto" and not an id. */
const ROUTER: Model[] = parseModels([{ id: ENSO, name: 'Enso Auto', family: 'enso', class: 'ours' }])

/** The last list this browser read, or none. */
function last(): Model[] {
  try {
    return parseModels(JSON.parse(store()?.getItem(LAST) ?? 'null'))
  } catch {
    return []
  }
}

/** Which catalog a target reads: the platform, and whether anyone is signed in. */
const shelf = (t: Target): string => `${t.api}|${t.token() ? 'in' : 'out'}`

/** `GET /v1/models` for a target, asked once and shared; a refusal is not kept, so the next ask goes again. */
export function catalog(t: Target): Promise<Model[]> {
  const at = shelf(t)
  let ask = catalogs.get(at)
  if (!ask) {
    ask = read(t).then((models) => {
      settled.set(at, models)
      try {
        if (models.length) store()?.setItem(LAST, JSON.stringify(models))
      } catch {
        // The next page reads the list again.
      }
      return models
    })
    catalogs.set(at, ask)
    ask.catch(() => catalogs.delete(at))
  }
  return ask
}

/** The catalog as a chooser draws it, and the list a kept pick is judged against. */
export interface Catalog {
  /** What the chooser offers: the list read, else the last one this browser kept, else Enso alone. */
  models: Model[]
  /** What was read, for a pick to stand on: empty when nothing was, so a kept pick stands. */
  list: Model[]
  loading: boolean
  /** Why there is no list to offer, in a reader's words; null when there is one. */
  error: string | null
  /** A list shown from before, because this read failed. */
  note: string
}

/**
 * The platform's catalog, as a hook: what was read already is there on the
 * first render. The gateway answers 429 once the org's quota is spent, and a
 * list that vanishes then leaves nothing to choose from, so a failed read shows
 * the last list this browser kept, with a note; with none kept, Enso alone, and
 * the reason in the chooser — never the platform's own words.
 */
export function useCatalog(t: Target): Catalog {
  const at = shelf(t)
  const [state, setState] = useState<{ at: string; models: Model[]; error: Error | null } | null>(() =>
    settled.has(at) ? { at, models: settled.get(at)!, error: null } : null,
  )
  useEffect(() => {
    let live = true
    catalog(t).then(
      (models) => live && setState({ at, models, error: null }),
      (e: Error) => live && setState({ at, models: [], error: e }),
    )
    return () => {
      live = false
    }
    // The target is read through `at`: the origin and whether anyone is signed in.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [at])
  const now = state?.at === at ? state : settled.has(at) ? { at, models: settled.get(at)!, error: null } : null
  if (!now?.error) return { models: now?.models ?? [], list: now?.models ?? [], loading: !now, error: null, note: '' }
  const kept = last()
  if (kept.length) return { models: kept, list: kept, loading: false, error: null, note: 'The model list could not be refreshed; these are the models as last read.' }
  const spent = now.error instanceof Refusal && now.error.status === 429
  return {
    models: ROUTER,
    list: [],
    loading: false,
    error: spent ? 'The model list is resting: this organization’s quota is spent.' : 'The model list could not be read right now.',
    note: '',
  }
}

/** Whether `model` is one this plan may send: on Free, Enso Auto alone (commerce's `allowedModels`). */
export function grants(free: boolean, model: string): boolean {
  return !free || model === ENSO
}

/** Whole cents as dollars. */
const dollars = (cents: number): string => `$${(cents / 100).toFixed(2)}`

/**
 * Whether the plan and the prepaid balance (cents, null where unknown) let `id` be
 * sent: Hanzo's own models follow the plan; another maker's are paid from the balance
 * on any plan, Free included.
 */
export function sendable(free: boolean, models: readonly Model[], cents: number | null, id: string): boolean {
  const row = models.find((m) => m.id === id)
  return grants(free, id) || Boolean(row && thirdParty(row) && cents !== null && cents > 0)
}

/** The effort as the composer reads it: the one in force, its list, and the way to change it. */
export const pacing = (m: Pick<Mind, 'effort' | 'pace'>): Modes<Effort> => ({ value: m.effort, set: m.pace, items: EFFORTS })

/** The chip's whole state: the catalog, what answers next, and how to change either. */
export interface Mind {
  model: string
  effort: Effort
  models: Model[]
  loading: boolean
  error: string | null
  /** A list shown from before, because this read failed. */
  note: string
  /** The plan grants only Enso Auto: anything else is an upgrade, never a pick. */
  free: boolean
  /** A premium pick holds for this screen; any other is kept. */
  pick: (id: string) => void
  pace: (effort: Effort) => void
  /**
   * Another maker's models are paid from the prepaid balance on any plan, so on Free
   * they are not an upgrade. Present where the host can send a person to the top-up
   * and the balance was read: whether the balance can pay for a model.
   */
  funded?: (id: string) => boolean
  /** The balance, as the billing API states it, whole cents written as dollars. */
  balance?: string
  /** The way to the top-up, given the model that was chosen if one was. */
  topUp?: (id?: string) => void
}

/** The prepaid balance in cents that can pay for another maker's models, for a signed-in target, read once; null until it is, and where the API does not say. */
function useBalance(t: Target, on: boolean): number | null {
  const [cents, set] = useState<number | null>(null)
  useEffect(() => {
    if (!on || !t.token()) return
    let live = true
    readPrepaid(t).then(
      (n) => live && set(n),
      () => live && set(null),
    )
    return () => {
      live = false
    }
    // The target is rebuilt each render; what it names is the key.
  }, [on, t.api, t.org]) // eslint-disable-line react-hooks/exhaustive-deps
  return cents
}

/** What a picker needs to sell another maker's models from the balance: whether it can pay, what it holds, and the way to top it up. */
export interface Funds {
  /** The balance in cents; null until read, and where it could not be. */
  cents: number | null
  funded?: (id: string) => boolean
  balance?: string
  topUp?: (id?: string) => void
}

/** The prepaid balance for a picker, where the host offers a top-up (`Host.topUp`); with none, no funds are claimed. */
export function useFunds(t: Target, topUp?: (id?: string) => void): Funds {
  const cents = useBalance(t, topUp !== undefined)
  if (topUp === undefined || cents === null) return { cents }
  return { cents, funded: (_id: string): boolean => cents > 0, balance: dollars(cents), topUp }
}

/** The model a top-up returned to, named in the address (`?model=`); read once, and left out of the address. */
function returned(): string {
  if (typeof window === 'undefined' || !window.location) return ''
  const url = new URL(window.location.href)
  return url.searchParams.get('model') ?? ''
}

function settleReturn(): void {
  if (typeof window === 'undefined' || !window.location) return
  const url = new URL(window.location.href)
  if (!url.searchParams.has('model')) return
  url.searchParams.delete('model')
  window.history.replaceState(window.history.state, '', `${url.pathname}${url.search}${url.hash}`)
}

/**
 * The model and effort for the plan `plan` names (`GET /v1/ai/limits` `plan`).
 * On Free the model is Enso Auto, and a kept pick of any other is set aside
 * rather than sent — it comes back once the plan grants it.
 */
export function useMind(t: Target, plan?: string | null, topUp?: (id?: string) => void): Mind {
  const [choice, change] = useChoice()
  const { models, list, loading, error, note } = useCatalog(t)
  const allowThirdParty = useAllowThirdPartyModels()
  const free = isFree(plan)
  const [picked, pick] = usePick(choice.model, (id) => change({ model: id }), list)
  // Another maker's model is paid from the balance on any plan; Hanzo's own follow the plan.
  const funds = useFunds(t, topUp)
  const cents = funds.cents
  const may = (id: string): boolean => sendable(free, models, cents, id)
  const model = may(picked) ? picked : ENSO
  const listed = allowThirdParty ? models : models.filter((m) => !thirdParty(m) || m.id === model)
  // A top-up returns to this address with the model it was for: pick it once the balance can pay.
  const [back] = useState(returned)
  useEffect(() => {
    if (!back || !models.length || cents === null) return
    if (may(back)) pick(back)
    settleReturn()
    // Once, when the catalog and the balance are both here.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [models.length, cents])
  return {
    model,
    effort: choice.effort,
    models: listed,
    loading,
    error,
    note,
    free,
    pick: (id) => {
      if (may(id)) pick(id)
    },
    pace: (effort) => change({ effort }),
    ...(funds.funded ? { funded: funds.funded, balance: funds.balance!, topUp: funds.topUp! } : {}),
  }
}
