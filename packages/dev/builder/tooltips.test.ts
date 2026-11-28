import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { describe, expect, it } from 'vitest'

const here = (name: string) => readFileSync(resolve(import.meta.dirname, name), 'utf8')

describe('workspace and preview toolbar hover tooltips (cli#52)', () => {
  it('run.tsx workspace header toggle icons expose hover titles', () => {
    const src = here('run.tsx')
    for (const label of [
      "title: say('Toggle environment column')",
      "title: say(devMode ? 'Toggle terminal pane' : 'Toggle activity pane')",
      "title: say('Toggle diff pane')",
      "title: say('Toggle browser pane')",
      "title: say('Toggle files pane')",
      "title: say('More workspace options')",
    ]) {
      expect(src).toContain(label)
    }
  })

  it('desk.tsx preview and split-pane toolbar icons expose hover titles', () => {
    const src = here('desk.tsx')
    for (const label of [
      "title: say('Expand {pane}', { pane: k.label })",
      "title: say('Close {pane}', { pane: k.label })",
      "title: say('Reload the page')",
      "title: say('Port {port}', { port: p.port })",
      "title: t('Reload the page')",
    ]) {
      expect(src).toContain(label)
    }
  })

  it('panel.tsx navigation and tab-strip icon buttons expose hover titles', () => {
    const src = here('panel.tsx')
    for (const label of [
      "title: t('Page actions')",
      "title: t('Back')",
      "title: t('Forward')",
      "title: t('Reload this tab')",
      "title: say('Open a tab')",
      "title: say('Open this tab in a new window')",
    ]) {
      expect(src).toContain(label)
    }
  })

  it('project.tsx preview bar and workspace header icon buttons expose hover titles', () => {
    const src = here('project.tsx')
    for (const label of [
      "title: t('Reload the preview')",
      "title: editing ? t('Stop picking elements') : t('Pick an element to edit')",
      "title: t('All sessions')",
      "title: t('History')",
      "title: shut ? t('Show the chat') : t('Hide the chat')",
    ]) {
      expect(src).toContain(label)
    }
  })

  it('out.tsx external links expose hover titles matching aria-label', () => {
    const src = here('out.tsx')
    expect(src).toContain('aria-label={label}')
    expect(src).toContain('title={label}')
  })
})
