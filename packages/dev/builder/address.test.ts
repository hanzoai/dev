import { describe, expect, it } from 'vitest'

import { ADDRESS, clone, onGitHub } from './address.ts'

const BOOK = { owner: 'webby-ai', name: 'book' }

describe('a pasted repository address', () => {
  it('names a repository on GitHub in every form GitHub hands out', () => {
    for (const text of [
      'https://github.com/webby-ai/book',
      'https://github.com/webby-ai/book.git',
      'https://github.com/webby-ai/book/',
      'http://github.com/webby-ai/book',
      'https://www.github.com/webby-ai/book',
      'git@github.com:webby-ai/book.git',
      'git@github.com:webby-ai/book',
      'ssh://git@github.com/webby-ai/book.git',
      'git://github.com/webby-ai/book.git',
      'github.com/webby-ai/book',
      'github.com/webby-ai/book.git',
    ]) {
      expect(onGitHub(text), text).toEqual(BOOK)
    }
  })

  it('reads past the space around it, the host’s case, a page’s query or fragment, and a user in front of the host', () => {
    expect(onGitHub('  https://GitHub.com/webby-ai/book.GIT \n')).toEqual(BOOK)
    expect(onGitHub('https://github.com/webby-ai/book?tab=readme-ov-file')).toEqual(BOOK)
    expect(onGitHub('https://github.com/webby-ai/book#readme')).toEqual(BOOK)
    // A clone address copied with who it signs in as: the repository is read, and the rest goes no further.
    expect(onGitHub('https://someone@github.com/webby-ai/book.git')).toEqual(BOOK)
  })

  it('keeps the names as GitHub spells them', () => {
    expect(onGitHub('https://github.com/Webby-AI/Intel.Hub_2')).toEqual({ owner: 'Webby-AI', name: 'Intel.Hub_2' })
  })

  it('names nothing on another host, or a host that only looks like GitHub', () => {
    for (const text of [
      'https://gitlab.com/webby-ai/book',
      'https://git.hanzo.ai/webby-ai/book.git',
      'https://github.com.example.com/webby-ai/book',
      'https://example.com/github.com/webby-ai/book',
      'https://github.com@example.com/webby-ai/book',
      'git@gitlab.com:webby-ai/book.git',
    ]) {
      expect(onGitHub(text), text).toBeNull()
    }
  })

  it('names nothing that is not one repository: an account, a page inside one, a bare name or nothing at all', () => {
    for (const text of [
      '',
      '   ',
      'book',
      'webby-ai/book',
      'https://github.com',
      'https://github.com/webby-ai',
      'https://github.com/webby-ai/book/tree/main',
      'https://github.com//webby-ai/book',
      'github.com/webby-ai/book extra',
    ]) {
      expect(onGitHub(text), text).toBeNull()
    }
  })

  it('names nothing GitHub would not take as a name, so no path or flag reaches the platform', () => {
    for (const text of ['https://github.com/-rf/book', 'https://github.com/webby-ai/..', 'https://github.com/webby-ai/.git', 'github.com/webby ai/book', `github.com/webby-ai/${'a'.repeat(101)}`]) {
      expect(onGitHub(text), text).toBeNull()
    }
  })

  it('is refused in a sentence that says what is taken', () => {
    expect(ADDRESS).toContain('https://github.com/owner/name')
    expect(ADDRESS).toContain('git@github.com:owner/name.git')
  })

  it('is handed to a project as its https clone address', () => {
    expect(clone(BOOK)).toBe('https://github.com/webby-ai/book.git')
    expect(onGitHub(clone(BOOK))).toEqual(BOOK)
  })
})
