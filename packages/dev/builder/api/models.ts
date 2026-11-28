/**
 * The models a run can be routed through, from the platform's own catalog.
 *
 *   GET /v1/models → {data: [{id, class, family, …}]}
 *
 * Read by `@hanzo/ui/models` `parseModels`, so a model's class and family are
 * the gateway's fields, never guessed from its id. `ENSO` is the router: it
 * picks a model per step, which is why it is the default, and a run that names
 * it names no model at all.
 */
import { parseModels, type ModelCatalogEntry } from '@hanzo/ui/models/catalog'

import { call, type Target } from './call.ts'

export { ENSO } from '@hanzo/ui/models/catalog'
export type Model = ModelCatalogEntry

export async function models(t: Target): Promise<Model[]> {
  return parseModels(await call<unknown>(t, 'GET', '/v1/models'))
}

/** The payer's plan usage, as `GET /v1/ai/limits` answers it: what a picker marks paused. */
export function limits(t: Target, signal?: AbortSignal): Promise<unknown> {
  return call<unknown>(t, 'GET', '/v1/ai/limits', undefined, { signal })
}

