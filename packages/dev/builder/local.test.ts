import { describe, expect, it } from 'vitest'

import { auto, failed, HERE, named, recent, route, where, type Folder } from './local.ts'

const a: Folder = { path: '/Users/me/site', name: 'site', used: 100 }
const b: Folder = { path: '/Users/me/api', name: 'api', used: 300 }
const c: Folder = { path: '/Users/me/notes', name: 'notes' }

describe('a folder on this computer (hanzo-inc/app issues 309 and 310)', () => {
  it('lists the folder used most recently first, and one never used last', () => {
    expect(recent([a, c, b]).map((f) => f.name)).toEqual(['api', 'site', 'notes'])
  })

  it('Auto picks the folder used last', () => {
    expect(auto([a, b, c])?.path).toBe('/Users/me/api')
  })

  it('Auto has nothing to pick on a computer that opened no folder', () => {
    expect(auto([])).toBeNull()
    expect(where([], '')).toEqual({ folder: null, auto: true })
  })

  it('a folder the person chose wins over Auto while the computer still lists it', () => {
    expect(where([a, b], '/Users/me/site')).toEqual({ folder: a, auto: false })
  })

  it('a chosen folder the computer no longer lists falls back to Auto', () => {
    expect(where([a, b], '/Users/me/gone')).toEqual({ folder: b, auto: true })
  })

  it('names a folder by its last segment', () => {
    expect(named('/Users/me/site/')).toBe('site')
    expect(named('C:\\work\\api')).toBe('api')
  })

  it('says what stopped a run in the host\'s own plain words, and never sends anyone to a terminal', () => {
    const offline = failed(new Error('Hanzo Dev could not be downloaded. Check your internet connection and try again.'))
    expect(offline).toBe('Hanzo Dev could not be downloaded. Check your internet connection and try again.')
    expect(failed('The folder /Users/me/gone is not on this computer any more. Choose another folder.')).toContain('not on this computer any more')
    // A host that never found the agent (an older Desktop) is asked to update, not to install anything.
    const missing = failed(new Error('Failed to invoke dev at "dev": No such file or directory (os error 2)'))
    expect(missing).toBe('Hanzo Dev is not set up on this computer yet. Update Hanzo Desktop, then send again.')
    for (const said of [offline, missing, failed(new Error(''))]) expect(said).not.toMatch(/npm|install -g|terminal|repositor|github/i)
    expect(failed(new Error(''))).toBe('The run on this computer could not start. Try again.')
    // A setup the person cancelled is their own choice, said as one: never a fault, never the host's raw words.
    for (const e of [new Error('Setup was cancelled.'), 'Setup was cancelled', new Error(' Setup was cancelled. ')]) {
      expect(failed(e)).toBe('Setup was cancelled. Send again whenever you like.')
    }
    expect(failed(new Error('Setup was cancelled by the firewall, retry'))).toContain('firewall')
  })

  it("keeps This computer's place id apart from a machine's and the sandbox's", () => {
    expect(HERE).not.toBe('')
    expect(HERE.startsWith('tgt_')).toBe(false)
  })
})

describe('Auto is the folder the ask is about, else the one used last', () => {
  const shop: Folder = { path: '/Users/me/work/shop', name: 'shop', used: 100 }
  const blog: Folder = { path: '/Users/me/work/my-blog', name: 'my-blog', used: 300 }
  const docs: Folder = { path: '/Users/me/work/acme/docs', name: 'docs', used: 200 }
  const web: Folder = { path: '/Users/me/work/acme/web', name: 'web', used: 50 }
  const all = [shop, blog, docs, web]

  it('goes to the folder the words name, over the one used last', () => {
    expect(route(all, 'Fix the checkout in shop')).toBe(shop)
    expect(route(all, 'add a post to my blog')).toBe(blog)
    expect(route(all, 'Update the DOCS for the new api')).toBe(docs)
  })

  it('reads a name in the singular or the plural, accents folded, camelCase as two words', () => {
    expect(route(all, 'list the shops we have')).toBe(shop)
    expect(route([{ path: '/x/Café', name: 'Café' }, blog], 'open the cafe menu')?.name).toBe('Café')
    expect(route([{ path: '/x/myShop', name: 'myShop', used: 1 }, blog], 'tidy up my shop page')?.name).toBe('myShop')
  })

  it('falls back to the folder used last when the words name none', () => {
    expect(route(all, 'make it faster')).toBe(blog)
    expect(route(all, '')).toBe(blog)
    expect(route([], 'anything')).toBeNull()
  })

  it('goes to a folder by the folder it sits in when its own name is not said', () => {
    expect(route([blog, web], 'redo the acme homepage')).toBe(web)
    // A name said beats a place said.
    expect(route([docs, web], 'acme web')).toBe(web)
  })

  it('does not take where projects live for which project', () => {
    expect(route(all, 'work on my project in the home folder')).toBe(blog)
    expect(route([{ path: '/Users/me/code', name: 'code', used: 1 }, shop], 'write some code')).toBe(shop)
  })

  it('prefers the folder it names whole, then the more recent one', () => {
    const wide: Folder = { path: '/Users/me/work/shop-site-redesign', name: 'shop-site-redesign', used: 900 }
    expect(route([wide, shop], 'fix the shop')).toBe(shop)
    const twin: Folder = { path: '/Users/me/old/shop', name: 'shop', used: 10 }
    expect(route([twin, shop], 'fix the shop')).toBe(shop)
    expect(route([wide, shop], 'redo the shop site redesign')).toBe(wide)
  })

  it('reads a short name only when the whole of it is said', () => {
    const ui: Folder = { path: '/Users/me/ui', name: 'ui', used: 1 }
    expect(route([ui, blog], 'fix the ui')).toBe(ui)
    expect(route([ui, blog], 'build a quick demo')).toBe(blog)
  })

  it('is the same answer every time for the same words', () => {
    const once = route(all, 'something about docs and shop')
    for (let i = 0; i < 5; i++) expect(route(all, 'something about docs and shop')).toBe(once)
    expect(once).toBe(docs)
  })

  it('where() follows the ask only for Auto: a folder chosen stays chosen', () => {
    expect(where(all, '', 'fix the shop')).toEqual({ folder: shop, auto: true })
    expect(where(all, '/Users/me/work/shop', 'write a post on my blog')).toEqual({ folder: shop, auto: false })
    expect(where(all, '/Users/me/gone', 'fix the shop')).toEqual({ folder: shop, auto: true })
    expect(where(all, '')).toEqual({ folder: blog, auto: true })
  })
})
