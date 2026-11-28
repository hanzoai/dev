/**
 * The model a surface starts its next run on, and how a pick changes it.
 *
 * A premium pick holds HERE, for the runs started from this screen, and is
 * never kept as the default — the next New opens on what was kept, Enso unless
 * the person kept another. Any other pick is kept. A kept premium model (from
 * before this rule) is not a default either, so Enso stands in for it.
 */
import { useState } from 'react'

import { defaultModel } from '@hanzo/ui/models/catalog'

import { ENSO, type Model } from './api/models.ts'

/**
 * What a run starts on with nothing picked on this screen: Enso when Enso is
 * kept — the router serves every ask whether or not a catalog lists it — else
 * the kept model while the catalog still offers it (or has not answered yet),
 * else the catalog's own default for a conversation, else Enso. Never '' — an
 * empty chooser reads "Choose a model" and the run would name nothing on purpose.
 */
export function standing(kept: string, catalog: readonly Model[]): string {
  if (kept === ENSO) return ENSO
  const premium = catalog.find((m) => m.id === kept)?.class === 'premium'
  const offered = catalog.length === 0 || catalog.some((m) => m.id === kept)
  if (kept && !premium && offered) return kept
  return defaultModel(catalog, 'chat') || ENSO
}

export function usePick(kept: string, keep: (id: string) => void, catalog: readonly Model[]) {
  const [once, setOnce] = useState<string | null>(null)
  const premium = (id: string) => catalog.find((m) => m.id === id)?.class === 'premium'
  const pick = (id: string) => {
    if (premium(id)) return setOnce(id)
    setOnce(null)
    keep(id)
  }
  return [once ?? standing(kept, catalog), pick] as const
}
