import { describe, expect, it } from 'vitest'

import { blocks, spans } from './markdown.ts'

describe('the blocks of what an agent writes', () => {
  it('reads Windows line endings and every heading level', () => {
    expect(blocks('### Plan ###\r\nstep one\r\n###### small')).toEqual([
      { kind: 'heading', level: 3, text: 'Plan' },
      { kind: 'paragraph', text: 'step one' },
      { kind: 'heading', level: 6, text: 'small' },
    ])
  })

  it('keeps a fence’s body verbatim, a fence with no language, and one never closed', () => {
    expect(blocks('say\n~~~\n# not a heading\n\n- not a list\n~~~\n```ts\nconst a = 1')).toEqual([
      { kind: 'paragraph', text: 'say' },
      { kind: 'code', lang: '', text: '# not a heading\n\n- not a list' },
      { kind: 'code', lang: 'ts', text: 'const a = 1' },
    ])
  })

  it('reads the rules each marker makes', () => {
    expect(blocks('***\n_ _ _\n- - -')).toEqual([{ kind: 'rule' }, { kind: 'rule' }, { kind: 'rule' }])
  })

  it('reads a quote over several lines as one', () => {
    expect(blocks('> one\n>two\n> **three**\nafter')).toEqual([
      { kind: 'quote', text: 'one two **three**' },
      { kind: 'paragraph', text: 'after' },
    ])
  })

  it('reads a list’s start, nesting by spaces or tabs, and the line that ends it', () => {
    expect(blocks('3) three\n4) four\n  1. nested\n\t\t\t\t- deep\n        deeper still\nnot an item')).toEqual([
      {
        kind: 'list',
        ordered: true,
        start: 3,
        items: [
          { text: 'three', depth: 0 },
          { text: 'four', depth: 0 },
          { text: 'nested', depth: 1 },
          { text: 'deep deeper still', depth: 3 },
        ],
      },
      { kind: 'paragraph', text: 'not an item' },
    ])
    expect(blocks('* a\n+ b')).toEqual([{ kind: 'list', ordered: false, start: 1, items: [{ text: 'a', depth: 0 }, { text: 'b', depth: 0 }] }])
  })

  it('reads a table to its last row, and a row with no divider under it as words', () => {
    expect(blocks('| name | state |\n| :--- | ---: |\n| api | up |\n| web |')).toEqual([
      { kind: 'table', head: ['name', 'state'], rows: [['api', 'up'], ['web']] },
    ])
    expect(blocks('| a | b |')).toEqual([{ kind: 'paragraph', text: '| a | b |' }])
    expect(blocks('| a | b |\nplain')).toEqual([{ kind: 'paragraph', text: '| a | b | plain' }])
  })

  it('reads nothing from nothing', () => {
    expect(blocks('')).toEqual([])
    expect(blocks('\n  \n')).toEqual([])
  })
})

describe('the inline pieces of a line', () => {
  it('reads bold and italics in either marker', () => {
    expect(spans('__bold__ and *it* and **also**')).toEqual([
      { kind: 'strong', text: 'bold' },
      { kind: 'text', text: ' and ' },
      { kind: 'em', text: 'it' },
      { kind: 'text', text: ' and ' },
      { kind: 'strong', text: 'also' },
    ])
  })

  it('reads code fenced by several backticks, and a link with a title and parentheses', () => {
    expect(spans('``pnpm i`` then [the spec](https://ex.test/a_(b) "Spec")')).toEqual([
      { kind: 'code', text: 'pnpm i' },
      { kind: 'text', text: ' then ' },
      { kind: 'link', text: 'the spec', href: 'https://ex.test/a_(b)' },
    ])
  })

  it('leaves a word joined by underscores as it is', () => {
    expect(spans('snake_case_name')).toEqual([{ kind: 'text', text: 'snake_case_name' }])
  })

  it('reads nothing from nothing', () => {
    expect(spans('')).toEqual([])
  })
})
