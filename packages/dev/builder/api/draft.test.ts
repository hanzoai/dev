import { describe, expect, it } from 'vitest'

import { free, parse, portraitOf, saved } from './draft.ts'

describe('a drafted agent', () => {
  it('reads the fields the model wrote, around whatever else it said', () => {
    const d = parse('Here you go:\n{"name":"Marie Curie","role":"Physicist and chemist","instructions":"You are Marie Curie.","opening":"What shall we measure?","emoji":"","figure":"Marie Curie"} Hope that helps.')
    expect(d).toEqual({ name: 'MarieCurie', role: 'Physicist and chemist', instructions: 'You are Marie Curie.', opening: 'What shall we measure?', emoji: '', figure: 'Marie Curie' })
  })

  it('is nothing where the model named no one or wrote no instructions', () => {
    expect(parse('I cannot do that.')).toBeNull()
    expect(parse('{"name":"Cfo","instructions":""}')).toBeNull()
    expect(parse('{"name":"!!!","instructions":"You are."}')).toBeNull()
  })

  it('keeps the first line it says in the instructions it is saved with', () => {
    expect(saved({ instructions: 'You are Cfo. ', opening: 'Show me the numbers.' })).toBe('You are Cfo.\n\nBegin a new conversation with: Show me the numbers.')
    expect(saved({ instructions: 'You are Cfo.', opening: '' })).toBe('You are Cfo.')
  })
})

describe('a portrait', () => {
  const page = (index: number, licence: string, url = 'https://upload.wikimedia.org/x.jpg') => ({
    index,
    imageinfo: [{ thumburl: url, descriptionurl: `https://commons.wikimedia.org/wiki/File:${index}.jpg`, extmetadata: { LicenseShortName: { value: licence } } }],
  })

  it('is free to use only in the public domain or under CC0', () => {
    for (const l of ['Public domain', 'PD-old-100', 'CC0', 'cc0 1.0']) expect(free(l), l).toBe(true)
    for (const l of ['CC BY-SA 4.0', 'CC BY 2.0', 'Fair use', '']) expect(free(l), l).toBe(false)
  })

  it('is the best-ranked file that is free, with where it came from and the licence', () => {
    const body = { query: { pages: { 1: page(2, 'CC BY-SA 4.0'), 2: page(3, 'Public domain'), 3: page(1, 'CC BY 2.0') } } }
    expect(portraitOf(body)).toEqual({ url: 'https://upload.wikimedia.org/x.jpg', page: 'https://commons.wikimedia.org/wiki/File:3.jpg', licence: 'Public domain' })
  })

  it('is none where nothing is free, or a file is not served over https', () => {
    expect(portraitOf({ query: { pages: { 1: page(1, 'CC BY 2.0') } } })).toBeNull()
    expect(portraitOf({ query: { pages: { 1: page(1, 'Public domain', 'http://x/y.jpg') } } })).toBeNull()
    expect(portraitOf({})).toBeNull()
    expect(portraitOf(null)).toBeNull()
  })
})
