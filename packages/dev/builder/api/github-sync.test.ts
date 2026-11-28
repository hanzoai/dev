import { afterEach, describe, expect, it, vi } from 'vitest'

import { Refusal, type Target } from './call.ts'
import { attachGitHubMirror, linkGitHubRepo, listGitTargets, parseGitHubRepo } from './git.ts'
import {
  createGitHubRepo,
  forkGitHubRepo,
  githubRepoFrom,
  isGitHubUserOAuthRequired,
} from './github.ts'

const T: Target = { api: 'https://api.hanzo.ai', token: () => 'tok', org: 'hanzo' }

interface Seen {
  url: string
  method: string
  body: unknown
}

function answer(status: number, body: unknown) {
  const seen: Seen[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init: RequestInit = {}) => {
      seen.push({
        url,
        method: init.method ?? 'GET',
        body: init.body ? JSON.parse(String(init.body)) : undefined,
      })
      return new Response(body === undefined ? null : JSON.stringify(body), {
        status,
        headers: { 'content-type': 'application/json' },
      })
    }),
  )
  return seen
}

afterEach(() => vi.unstubAllGlobals())

describe('createGitHubRepo', () => {
  it('POSTs /v1/provider/github/repos with name, description, private, org, and linkRepo', async () => {
    const seen = answer(201, {
      full_name: 'mcphun-spec/my-app',
      html_url: 'https://github.com/mcphun-spec/my-app',
      clone_url: 'https://github.com/mcphun-spec/my-app.git',
      default_branch: 'main',
      private: true,
      existing: false,
    })

    const res = await createGitHubRepo(T, {
      name: '  my-app ',
      description: 'My Hanzo app',
      private: true,
      org: 'hanzoai',
      linkRepo: 'my-app',
    })

    expect(seen[0]).toEqual({
      method: 'POST',
      url: 'https://api.hanzo.ai/v1/provider/github/repos',
      body: {
        name: 'my-app',
        description: 'My Hanzo app',
        private: true,
        org: 'hanzoai',
        linkRepo: 'my-app',
      },
    })
    expect(res).toEqual({
      full_name: 'mcphun-spec/my-app',
      html_url: 'https://github.com/mcphun-spec/my-app',
      clone_url: 'https://github.com/mcphun-spec/my-app.git',
      default_branch: 'main',
      private: true,
      existing: false,
    })
  })

  it('rejects empty name before calling fetch', async () => {
    const spy = vi.fn()
    vi.stubGlobal('fetch', spy)
    await expect(createGitHubRepo(T, { name: '   ' })).rejects.toThrow('Enter a repository name')
    expect(spy).not.toHaveBeenCalled()
  })

  it('throws Refusal with status 412 detected by isGitHubUserOAuthRequired', async () => {
    answer(412, { detail: 'personal GitHub account requires user OAuth authorization' })
    const err = await createGitHubRepo(T, { name: 'demo' }).catch((e: unknown) => e)
    expect(err).toBeInstanceOf(Refusal)
    expect((err as Refusal).status).toBe(412)
    expect(isGitHubUserOAuthRequired(err)).toBe(true)
    expect(isGitHubUserOAuthRequired(new Refusal(409, 'oauth required'))).toBe(true)
    expect(isGitHubUserOAuthRequired(new Refusal(500, 'server error'))).toBe(false)
  })
})

describe('forkGitHubRepo', () => {
  it('POSTs /v1/provider/github/fork for owner/repo, full GitHub URLs, and bare repo names', async () => {
    const seen = answer(200, {
      full_name: 'mcphun-spec/ui',
      html_url: 'https://github.com/mcphun-spec/ui',
      clone_url: 'https://github.com/mcphun-spec/ui.git',
      default_branch: 'main',
      private: false,
      existing: false,
    })

    const res = await forkGitHubRepo(T, { repo: 'hanzoai/ui', name: 'my-ui', org: 'hanzo-labs' })
    expect(seen[0]).toEqual({
      method: 'POST',
      url: 'https://api.hanzo.ai/v1/provider/github/fork',
      body: { repo: 'hanzoai/ui', org: 'hanzo-labs', name: 'my-ui' },
    })
    expect(res.full_name).toBe('mcphun-spec/ui')

    // Normalizes full GitHub URL to owner/repo
    const seenUrl = answer(200, { full_name: 'mcphun-spec/dev' })
    const urlRes = await forkGitHubRepo(T, { repo: 'https://github.com/hanzoai/dev.git' })
    expect(seenUrl[0].body).toEqual({ repo: 'hanzoai/dev' })
    expect(urlRes.html_url).toBe('https://github.com/mcphun-spec/dev')
    expect(urlRes.clone_url).toBe('https://github.com/mcphun-spec/dev.git')
    expect(urlRes.default_branch).toBe('main')

    // Bare repo name passes through cleanly
    const seenBare = answer(200, { full_name: 'mcphun-spec/starter', existing: true })
    const bareRes = await forkGitHubRepo(T, { repo: 'starter' })
    expect(seenBare[0].body).toEqual({ repo: 'starter' })
    expect(bareRes.existing).toBe(true)
  })

  it('rejects empty repo before calling fetch', async () => {
    const spy = vi.fn()
    vi.stubGlobal('fetch', spy)
    await expect(forkGitHubRepo(T, { repo: '  ' })).rejects.toThrow('Enter a GitHub repository to fork')
    expect(spy).not.toHaveBeenCalled()
  })
})

describe('attachGitHubMirror, listGitTargets, linkGitHubRepo, and githubRepoFrom', () => {
  it('POSTs /v1/git/repos/:name/targets with normalized https clone URL', async () => {
    const seen = answer(201, {
      id: 'tgt_1',
      repo: 'my-project',
      host: 'github.com',
      url: 'https://github.com/mcphun-spec/my-project.git',
      createdAt: '2026-11-28T00:00:00Z',
    })

    const target = await attachGitHubMirror(T, 'my-project', 'mcphun-spec/my-project')
    expect(seen[0]).toEqual({
      method: 'POST',
      url: 'https://api.hanzo.ai/v1/git/repos/my-project/targets',
      body: { url: 'https://github.com/mcphun-spec/my-project.git' },
    })
    expect(target).toEqual({
      id: 'tgt_1',
      repo: 'my-project',
      host: 'github.com',
      url: 'https://github.com/mcphun-spec/my-project.git',
      createdAt: '2026-11-28T00:00:00Z',
    })
  })

  it('accepts full GitHub URLs and rejects non-GitHub URLs in attachGitHubMirror', async () => {
    const seen = answer(200, {})
    const res = await attachGitHubMirror(T, 'app 1', 'https://github.com/hanzoai/app-1')
    expect(seen[0].url).toBe('https://api.hanzo.ai/v1/git/repos/app%201/targets')
    expect(seen[0].body).toEqual({ url: 'https://github.com/hanzoai/app-1.git' })
    expect(res.url).toBe('https://github.com/hanzoai/app-1.git')

    await expect(attachGitHubMirror(T, 'app', 'https://gitlab.com/a/b')).rejects.toThrow('github.com')
    expect(parseGitHubRepo('owner/repo')).toEqual({ owner: 'owner', name: 'repo' })
    expect(parseGitHubRepo('https://github.com/owner/repo.git')).toEqual({ owner: 'owner', name: 'repo' })
    expect(parseGitHubRepo('https://gitlab.com/owner/repo')).toBeNull()
  })

  it('GETs /v1/git/repos/:name/targets and parses outbound mirror targets', async () => {
    const seen = answer(200, {
      data: [
        {
          id: 'tgt_1',
          repo: 'demo',
          host: 'github.com',
          url: 'https://github.com/mcphun-spec/demo.git',
          createdAt: '2026-11-28T01:00:00Z',
        },
      ],
    })
    const list = await listGitTargets(T, 'demo')
    expect(seen[0]).toEqual({ method: 'GET', url: 'https://api.hanzo.ai/v1/git/repos/demo/targets', body: undefined })
    expect(list).toHaveLength(1)
    expect(list[0].url).toBe('https://github.com/mcphun-spec/demo.git')
  })

  it('POSTs /v1/git/repos/:name/link with normalized GitHub source URL', async () => {
    const seen = answer(200, { ok: true })
    await linkGitHubRepo(T, 'demo', 'hanzoai/demo')
    expect(seen[0]).toEqual({
      method: 'POST',
      url: 'https://api.hanzo.ai/v1/git/repos/demo/link',
      body: { source: 'https://github.com/hanzoai/demo.git' },
    })
  })

  it('resolves GitHub badge info from direct GitHub repo URL or outbound mirror targets', () => {
    expect(githubRepoFrom('https://github.com/hanzoai/dev.git')).toEqual({
      owner: 'hanzoai',
      name: 'dev',
      fullName: 'hanzoai/dev',
      htmlUrl: 'https://github.com/hanzoai/dev',
      source: 'repo',
    })

    expect(
      githubRepoFrom('https://git.hanzo.ai/hanzo/demo.git', [
        { id: 't1', repo: 'demo', host: 'github.com', url: 'https://github.com/mcphun-spec/demo.git', createdAt: '' },
      ]),
    ).toEqual({
      owner: 'mcphun-spec',
      name: 'demo',
      fullName: 'mcphun-spec/demo',
      htmlUrl: 'https://github.com/mcphun-spec/demo',
      source: 'target',
    })

    expect(githubRepoFrom('https://git.hanzo.ai/hanzo/local-only.git', [])).toBeNull()
  })
})
