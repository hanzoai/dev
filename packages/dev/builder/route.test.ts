import { describe, expect, it } from 'vitest'

import { href, parse, path, PLACES, route, type Route } from './route.ts'

const ID = 'sess_0992f90537264a6b154ebff799f38e1c'
const FLOW = 'flow_c2b111c12ea23f05f55b3d973dbeb267'

describe('route', () => {
  it.each([
    ['', { kind: 'new' }],
    ['/', { kind: 'new' }],
    [ID, { kind: 'run', id: ID }],
    [`/${ID}/`, { kind: 'run', id: ID }],
    ['-/artifacts', { kind: 'screen', screen: 'artifacts' }],
    ['-/templates', { kind: 'screen', screen: 'templates' }],
    ['-/projects', { kind: 'screen', screen: 'projects' }],
    ['-/issues', { kind: 'screen', screen: 'issues' }],
    ['-/automations', { kind: 'screen', screen: 'automations' }],
    [`-/automations/${FLOW}`, { kind: 'automation', id: FLOW }],
    ['-/automations/new', { kind: 'automation', id: 'new' }],
    ['-/sync', { kind: 'screen', screen: 'sync' }],
    ['-/mcp', { kind: 'screen', screen: 'mcp' }],
    ['-/settings', { kind: 'settings', section: 'general' }],
    ['-/settings/account', { kind: 'settings', section: 'account' }],
    ['-/artifacts/image', { kind: 'screen', screen: 'artifacts', of: 'image' }],
    ['-/artifacts/new', { kind: 'screen', screen: 'artifacts', of: 'new' }],
    ['-/customize', { kind: 'customize', tab: 'agents' }],
    ['-/customize/skills', { kind: 'customize', tab: 'skills' }],
    ['-/customize/agents/dev', { kind: 'agent', name: 'dev' }],
    ['-/customize/connectors', { kind: 'customize', tab: 'connectors' }],
    ['-/plans', { kind: 'screen', screen: 'plans' }],
    ['-/settings/environments/', { kind: 'settings', section: 'environments' }],
    ['mega-shop', { kind: 'project', slug: 'mega-shop' }],
    ['artifacts', { kind: 'project', slug: 'artifacts' }],
    ['hanzo/circle', { kind: 'repo', org: 'hanzo', name: 'circle' }],
    ['/acme/Shop.web_2/', { kind: 'repo', org: 'acme', name: 'Shop.web_2' }],
  ])('%s', (p, r) => expect(route(p)).toEqual(r))

  it.each(['sess_nothex', 'Mega-Shop', '-/elsewhere', '-/settings/elsewhere', '-/settings/skills', '-/customize/elsewhere', '-/artifacts/elsewhere', '-/automations/flow_nothex', '-/automations/../x', '-leading', '-/codebases', 'a/b/c', 'a/-b', '.a/b', 'a//b', 'x'.repeat(41), `a/${'x'.repeat(65)}`, '../etc', 'javascript:alert(1)', 'https://evil.example'])(
    'reads %s as the empty state',
    (p) => expect(route(p)).toEqual({ kind: 'new' }),
  )

  it('inverts', () => {
    for (const p of ['', ID, '-/artifacts', '-/projects', '-/issues', '-/automations', `-/automations/${FLOW}`, '-/automations/new', '-/sync', '-/mcp', '-/settings', '-/settings/machines', '-/customize', '-/customize/plugins', '-/plans', 'mega-shop', 'hanzo/circle']) expect(path(route(p))).toBe(p)
  })
})

describe('href', () => {
  it.each([
    [{ kind: 'new' }, ''],
    [{ kind: 'run', id: ID }, `/run/${ID}`],
    [{ kind: 'project', slug: 'mega-shop' }, '/projects/mega-shop'],
    [{ kind: 'repo', org: 'hanzo', name: 'circle' }, '/projects/hanzo/circle'],
    [{ kind: 'screen', screen: 'sync' }, '/sync'],
    [{ kind: 'screen', screen: 'projects' }, '/projects'],
    [{ kind: 'screen', screen: 'issues' }, '/issues'],
    [{ kind: 'screen', screen: 'artifacts' }, '/artifacts'],
    [{ kind: 'screen', screen: 'artifacts', of: 'new' }, '/artifacts/new'],
    [{ kind: 'screen', screen: 'templates' }, '/templates'],
    [{ kind: 'screen', screen: 'automations' }, '/automations'],
    [{ kind: 'settings', section: 'machines' }, '/machines'],
    [{ kind: 'settings', section: 'environments' }, '/environments'],
    [{ kind: 'settings', section: 'general' }, '/settings'],
    [{ kind: 'settings', section: 'usage' }, '/settings/usage'],
    [{ kind: 'customize', tab: 'agents' }, '/customize'],
    [{ kind: 'customize', tab: 'skills' }, '/customize/skills'],
    [{ kind: 'agent', name: 'dev' }, '/customize/agents/dev'],
    [{ kind: 'customize', tab: 'connectors' }, '/customize/connectors'],
    [{ kind: 'automation', id: FLOW }, `/automations/${FLOW}`],
    [{ kind: 'screen', screen: 'plans' }, '/plans'],
  ] as [Route, string][])('%j is %s', (r, h) => expect(href(r)).toBe(h))
})

describe('parse', () => {
  it('inverts href over every place and every kind of record', () => {
    const records: Route[] = [
      { kind: 'run', id: ID },
      { kind: 'project', slug: 'mega-shop' },
      { kind: 'project', slug: 'artifacts' },
      { kind: 'repo', org: 'hanzo', name: 'circle' },
      { kind: 'repo', org: 'acme', name: 'Shop.web_2' },
      { kind: 'automation', id: FLOW },
    ]
    for (const r of [...PLACES, ...records]) expect(parse(href(r))).toEqual(r)
  })

  it('names every place once', () => {
    const paths = PLACES.map(href)
    expect(new Set(paths).size).toBe(paths.length)
    expect(paths).toContain('')
    expect(paths).toContain('/machines')
    expect(paths).toContain('/environments')
  })

  it.each([
    ['/projects/', { kind: 'screen', screen: 'projects' }],
    ['projects?x=1#y', { kind: 'screen', screen: 'projects' }],
    ['/settings/machines', { kind: 'settings', section: 'machines' }],
    ['/settings/environments', { kind: 'settings', section: 'environments' }],
    ['/projects/hanzo/circle/', { kind: 'repo', org: 'hanzo', name: 'circle' }],
  ] as [string, Route][])('reads %s, which is not how href writes it, as the same place', (p, r) => expect(parse(p)).toEqual(r))

  it.each([
    '/run',
    '/run/mega-shop',
    `/run/${ID}/x`,
    `/${ID}`,
    '/mega-shop',
    '/projects/Mega-Shop',
    `/projects/${ID}`,
    '/projects/a/b/c/d',
    '/projects/a/b/c',
    '/projects/-a/b',
    '/projects/a/..',
    '/codebase',
    '/codebases',
    '/settings/elsewhere',
    '/elsewhere',
    '/../etc',
    '//evil.example',
    '/javascript:alert(1)',
  ])('reads %s as the empty state', (p) => expect(parse(p)).toEqual({ kind: 'new' }))
})
