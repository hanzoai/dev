import { I18nProvider } from '@hanzo/i18n/react'
import type { ReactNode } from 'react'
import { renderToString } from 'react-dom/server'
import { describe, expect, it } from 'vitest'

import { useSplit, type Split } from './side.tsx'

/** The grip `useSplit` hands its host, drawn under `wrap`. */
function grip(wrap: (node: ReactNode) => ReactNode): Split['grip'] {
  let got: Split['grip'] = null
  function Probe() {
    got = useSplit(true, 'chat', () => {}).grip
    return null
  }
  renderToString(wrap(<Probe />))
  return got
}

describe('the side column', () => {
  it('stands at the line’s end: its grip on its left edge, and on its right edge right to left', () => {
    expect(grip((n) => n)?.side).toBe('right')
    expect(grip((n) => <I18nProvider initialLocale="fr-FR">{n}</I18nProvider>)?.side).toBe('right')
    expect(grip((n) => <I18nProvider initialLocale="ar">{n}</I18nProvider>)?.side).toBe('left')
  })
})
