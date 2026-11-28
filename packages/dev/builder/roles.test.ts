import { describe, expect, it } from 'vitest'

import { catalogue, type Catalogue } from './api/skills.ts'
import { MOST, recommend, WORK } from './roles.ts'

const shelf = (rows: [name: string, works: string[]][]): Catalogue => ({
  skills: rows.map(([name, works]) => ({ name, description: `What ${name} does`, product: name.split('_')[0]!, works })),
  products: [],
})

describe('the skills a work recommends', () => {
  it('reads the works a catalogue entry names, and none when it names none', () => {
    const read = catalogue({ skills: [{ name: 'a_b', service: 'a', works: ['data', 'research'] }, { name: 'c_d', service: 'c' }] })
    expect(read.skills.map((e) => e.works)).toEqual([['data', 'research'], []])
  })

  it('offers the entries that name the work, in catalogue order', () => {
    const all = shelf([['a_x', ['data']], ['b_y', ['design']], ['c_z', ['data', 'design']]])
    expect(recommend('data', all).map((e) => e.name)).toEqual(['a_x', 'c_z'])
    expect(recommend('design', all)[0]).toMatchObject({ name: 'b_y', description: 'What b_y does', product: 'b' })
  })

  it('keeps one click within what can sensibly be added', () => {
    const many = shelf(Array.from({ length: MOST + 5 }, (_, i): [string, string[]] => [`s_${i}`, ['data']]))
    expect(recommend('data', many)).toHaveLength(MOST)
  })

  it('offers nothing for no work, one no entry names, "something else", or before the catalogue is read', () => {
    const all = shelf([['a_x', ['data']]])
    expect(WORK.some((w) => w.id === 'other')).toBe(true)
    for (const work of [undefined, '', 'astronaut', 'other']) expect(recommend(work, all)).toEqual([])
    expect(recommend('data', null)).toEqual([])
  })
})
