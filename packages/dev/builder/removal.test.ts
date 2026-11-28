/**
 * What removing a project does, read off what the org's projects are made from:
 * Hanzo's copy of a GitHub repository goes with its link and its project, and
 * GitHub's is never touched; a repository only the forge holds goes with its
 * history; a repository GitHub grants that Hanzo never copied has nothing here
 * to remove. A refusal stops the rest and says what is left.
 *
 * Rows are shaped as api/work.ts `sources` answers them: a copy is listed on the
 * forge as `<account>_<name>` (cloud apps/sync importer.go `flat`), and a link
 * names the bare name it was declared with.
 */
import type { Say } from '@hanzo/i18n/react'
import { describe, expect, it } from 'vitest'

import { removal, stopped, type Made } from './removal.ts'

/** An org with a linked repository, an import, a template and a grant it never brought in. */
const MADE: Made = {
  granted: [
    { owner: 'hanzoai', name: 'build', codebase: 'hanzoai_build' },
    { owner: 'hanzoai', name: 'docs', codebase: 'hanzoai_docs' },
  ],
  linked: [
    { id: 'sync_1', owner: 'hanzoai', name: 'cloud', forge: 'cloud' },
    { id: 'sync_2', owner: 'luxfi', name: 'node', forge: 'node' },
  ],
  held: [
    { org: 'acme', name: 'hanzoai_cloud' },
    { org: 'acme', name: 'hanzoai_build' },
    { org: 'acme', name: 'intel-hub' },
    { org: 'acme', name: 'notes' },
  ],
  sites: [
    { slug: 'hanzoai-cloud', repo: 'https://github.com/hanzoai/cloud.git' },
    { slug: 'intel-hub', repo: 'https://git.hanzo.ai/acme/intel-hub.git' },
  ],
}

describe('removing a project', () => {
  it('drops a GitHub copy’s link, then the copy, then its project, and says GitHub’s repository is not touched', () => {
    expect(removal('hanzoai_cloud', MADE)).toEqual({
      repo: 'hanzoai/cloud',
      from: 'github',
      steps: [
        { kind: 'link', id: 'sync_1' },
        { kind: 'mirror', name: 'hanzoai_cloud' },
        { kind: 'project', slug: 'hanzoai-cloud' },
      ],
      says: 'Hanzo stops syncing hanzoai/cloud and deletes its copy here, with its project and any site published from it. Your repository on GitHub is not touched.',
    })
  })

  it('knows a copy by GitHub naming it as the repository’s forge name, with no link of its own', () => {
    expect(removal('HanzoAI_Build', MADE)).toEqual({
      repo: 'hanzoai/build',
      from: 'github',
      steps: [{ kind: 'mirror', name: 'hanzoai_build' }],
      says: 'Hanzo stops syncing hanzoai/build and deletes its copy here. Your repository on GitHub is not touched.',
    })
  })

  it('knows an import by the project it made from GitHub’s address, when GitHub’s listing did not answer', () => {
    const made: Made = { granted: [], linked: [], held: [{ org: 'acme', name: 'acme_intel-hub' }], sites: [{ slug: 'intel-hub-2', repo: 'https://github.com/Acme/Intel-Hub.git' }] }
    expect(removal('acme_intel-hub', made)).toMatchObject({
      repo: 'Acme/Intel-Hub',
      from: 'github',
      steps: [
        { kind: 'mirror', name: 'acme_intel-hub' },
        { kind: 'project', slug: 'intel-hub-2' },
      ],
    })
  })

  it('deletes a repository only the forge holds, history and all, with the project built from it', () => {
    expect(removal('intel-hub', MADE)).toEqual({
      repo: 'acme/intel-hub',
      from: 'hanzo',
      steps: [
        { kind: 'mirror', name: 'intel-hub' },
        { kind: 'project', slug: 'intel-hub' },
      ],
      says: 'This deletes acme/intel-hub and all of its history from Hanzo, with its project and any site published from it. This cannot be undone.',
    })
    expect(removal('notes', MADE)?.says).toBe('This deletes acme/notes and all of its history from Hanzo. This cannot be undone.')
  })

  it('never takes the forge’s own repository for a copy because a GitHub one has its name', () => {
    // acme/notes on GitHub is linked and built from; its copy is acme_notes, so neither is this repository's.
    const made: Made = {
      granted: [{ owner: 'acme', name: 'notes', codebase: 'acme_notes' }],
      linked: [{ id: 'sync_9', owner: 'acme', name: 'notes', forge: 'notes' }],
      held: [{ org: 'acme', name: 'notes' }],
      sites: [{ slug: 'acme-notes', repo: 'https://github.com/acme/notes.git' }],
    }
    expect(removal('notes', made)).toEqual({
      repo: 'acme/notes',
      from: 'hanzo',
      steps: [{ kind: 'mirror', name: 'notes' }],
      says: 'This deletes acme/notes and all of its history from Hanzo. This cannot be undone.',
    })
  })

  it('offers nothing for a repository GitHub grants that Hanzo never copied, or a link whose copy has not landed', () => {
    expect(removal('hanzoai_docs', MADE)).toBeNull()
    expect(removal('luxfi_node', MADE)).toBeNull()
    expect(removal('nowhere', MADE)).toBeNull()
  })

  it('says its sentences in the reader’s language', () => {
    const t = ((source: string, values?: Record<string, string | number>) => `«${source}» ${values?.repo ?? ''}`) as Say
    expect(removal('notes', MADE, t)?.says).toBe('«This deletes {repo} and all of its history from Hanzo. This cannot be undone.» acme/notes')
  })
})

describe('a removal that was refused', () => {
  const copy = removal('hanzoai_cloud', MADE)!
  const own = removal('intel-hub', MADE)!

  it('removed nothing when its first step was refused', () => {
    expect(stopped(copy, 0, 'sync not found')).toBe('hanzoai/cloud was not removed: sync not found')
    expect(stopped(own, 0, 'repo not found')).toBe('acme/intel-hub was not removed: repo not found')
  })

  it('says what went and what is still here when a later one was', () => {
    expect(stopped(copy, 1, 'repo not found')).toBe('hanzoai/cloud no longer syncs from GitHub, but its copy is still on Hanzo: repo not found')
    expect(stopped(copy, 2, 'project not found')).toBe('Hanzo’s copy of hanzoai/cloud is deleted, but its project is still on Hanzo: project not found')
    expect(stopped(own, 1, 'project not found')).toBe('acme/intel-hub is deleted, but its project is still on Hanzo: project not found')
  })
})
