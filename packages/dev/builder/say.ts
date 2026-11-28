/**
 * `t` where no component is asking: the English, its `{name}` arguments filled.
 * A helper that builds words outside a render (a rail's rows, a formatter)
 * takes the caller's @hanzo/i18n `t` and falls back to this.
 */
import { translate } from '@hanzo/i18n'
import type { Say } from '@hanzo/i18n/react'

export const english = ((source: string, values?: Readonly<Record<string, string | number>>) => translate({}, source, values)) as Say
