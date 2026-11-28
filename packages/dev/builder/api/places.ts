/**
 * Where a run can run: the platform's cloud workspace, or one of the org's machines.
 *
 *   GET /v1/agent/targets → {targets: [...]}, newest first (machines.ts)
 *
 * The cloud workspace is not a row the platform lists — it is what a run gets when it
 * names no target — so it is the first place here and carries no id. A machine
 * can take a run only while it is online.
 */
import { phrase } from '@hanzo/i18n'

import type { Say } from '@hanzo/i18n/react'

import { formatDeviceLabel } from '../dev-mode.ts'
import { deviceKey, type Names } from '../names.ts'
import { english } from '../say.ts'
import type { Target } from './call.ts'
import { machines, state } from './machines.ts'

export interface Place {
  /** `tgt_…`, or '' for the cloud workspace. */
  id: string
  label: string
  /** laptop | gpu | cloud | … as the platform names it; 'sandbox' for the cloud workspace. */
  kind: string
  /** online | offline | draining | not seen yet, as machines.ts `state` reads it; the cloud workspace is always online. */
  status: string
  /** "10 vCPU / 122G / 1× GB10", when the machine reported one. */
  capacity: string
  /** The hostname its runs report, or '' (the cloud workspace, or a machine addressed by id only). */
  host?: string
  /** A machine's folders its owner lets a run from the web work in (machines.ts); none elsewhere. */
  folders?: string[]
}

/** The cloud workspace, named in English and marked (`phrase`): a screen draws its label through `t`. */
export const SANDBOX: Place = { id: '', label: phrase('Hanzo sandbox'), kind: 'sandbox', status: 'online', capacity: '' }

export async function places(t: Target): Promise<Place[]> {
  const list = await machines(t)
  return [SANDBOX, ...list.map((m) => ({ id: m.id, label: m.label, kind: m.kind, status: state(m), capacity: m.capacity, ...(m.host ? { host: m.host } : {}), ...(m.folders.length ? { folders: m.folders } : {}) }))]
}

/** A place as a person reads it: the name they gave the machine (`names`, from `useDeviceNames`), else its own as a person says it. */
export function displayPlace(p: Place, opts?: { devMode?: boolean; say?: Say; names?: Names }): { title: string; detail?: string } {
  const say = opts?.say ?? english
  if (!p.id) return opts?.devMode ? { title: 'Cloud' } : { title: say('Auto (Cloud)'), detail: say('Runs automatically in your Hanzo cloud workspace') }
  return formatDeviceLabel(p.label, { hardware: p.capacity, host: p.host, name: opts?.names?.[deviceKey(p.host, p.label, p.id)], devMode: opts?.devMode, say })
}

/** Whether a run sent here now would be taken. */
export const ready = (p: Place): boolean => p.status === 'online'
