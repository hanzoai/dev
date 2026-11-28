/**
 * What removing one of the org's projects does, decided before anything is.
 *
 * A project on Projects is a repository the forge holds, and it is one of two
 * things. Hanzo's COPY of a repository on GitHub: removing it drops the link
 * that syncs into it, then the copy, then the project built from it — and the
 * repository on GitHub is never touched. Or the forge's OWN repository, made
 * from a template or created there: removing it deletes the repository and all
 * of its history, which Hanzo knows of nowhere else. A repository GitHub grants
 * that Hanzo never copied has nothing here to remove.
 *
 * Which one it is, is read off what the org's projects are made from (api/work.ts
 * `sources`), and only off what says so of THIS copy: a link that writes into
 * it, GitHub's listing naming it as a repository's forge name, or a project
 * built from that repository on GitHub. A forge repository merely named like a
 * GitHub one is the forge's own and is said to be, since deleting it may be
 * deleting the only history there is.
 *
 * The calls go in that order — link, copy, project — and stop at the first
 * refusal, so what a removal leaves is always the tail of its steps and is said.
 *
 * Pure: the screen asks and the tests ask the same question. Its words take the
 * screen's @hanzo/i18n `t`, English when none is handed.
 */
import type { Say } from '@hanzo/i18n/react'

import type { Link } from './api/links.ts'
import { addressOf, forgeHost, type Granted, type Held, type Site } from './merge.ts'
import { english } from './say.ts'

/** One call a removal makes. */
export type Step =
  /** DELETE /v1/sync/{id}: GitHub stops syncing into the copy, and the copy stops pushing back. */
  | { kind: 'link'; id: string }
  /** DELETE /v1/git/repos/{name}: the repository goes from the forge, history and all. */
  | { kind: 'mirror'; name: string }
  /** DELETE /v1/project/{slug}: the project built from it goes, and its site stops answering. */
  | { kind: 'project'; slug: string }

export interface Removal {
  /** `owner/name` as people know it: GitHub's address for a copy, the org's for the forge's own. */
  repo: string
  /** `github` for Hanzo's copy of a repository on GitHub; `hanzo` for one whose only copy Hanzo knows of is its own. */
  from: 'github' | 'hanzo'
  /** The calls, in the order they are made. */
  steps: Step[]
  /** What removing it does, in full, for the one question asked before it is done. */
  says: string
}

/** What the org's projects are made from (api/work.ts `sources`), as far as removing one reads it. */
export interface Made {
  granted: Pick<Granted, 'owner' | 'name' | 'codebase'>[]
  linked: Pick<Link, 'id' | 'owner' | 'name' | 'forge'>[]
  held: Pick<Held, 'org' | 'name'>[]
  sites: Pick<Site, 'slug' | 'repo'>[]
}

/**
 * The name a GitHub repository's copy has on the forge: its account and its name,
 * folded (`hanzoai_cloud`; cloud forge.Imported). GET /v1/git/repos lists the copy
 * by it and GitHub's listing gives it as `codebase`. A link is declared with the
 * bare name (`cloud`), which a forge repository of the org's own can also have,
 * so the bare name is never taken for the copy.
 */
const imported = (owner: string, name: string): string => (owner && name ? `${owner}_${name}`.toLowerCase() : '')

/**
 * What removing the forge repository `name` does, or null when the forge does
 * not hold it: there is nothing of its own on Hanzo to remove.
 */
export function removal(name: string, made: Made, t: Say = english): Removal | null {
  const n = name.toLowerCase()
  const held = made.held.find((h) => h.name.toLowerCase() === n)
  if (!held) return null
  const links = made.linked.filter((l) => imported(l.owner, l.forge) === n)
  // A project is built from the copy itself, or from the repository on GitHub it is the copy of.
  const sites = made.sites.filter((s) => {
    const at = addressOf(s.repo)
    if (!at) return false
    return at.github ? imported(at.owner, at.name) === n : forgeHost(s.repo) && at.name.toLowerCase() === n
  })
  // The repository on GitHub this is Hanzo's copy of, where anything says so of this copy.
  const hub =
    links.at(0) ??
    made.granted.find((g) => g.codebase.toLowerCase() === n) ??
    sites.map((s) => addressOf(s.repo)).find((at) => at?.github)
  const repo = hub ? `${hub.owner}/${hub.name}` : held.org ? `${held.org}/${held.name}` : held.name
  const steps: Step[] = [
    ...links.map((l): Step => ({ kind: 'link', id: l.id })),
    { kind: 'mirror', name: held.name },
    ...sites.map((s): Step => ({ kind: 'project', slug: s.slug })),
  ]
  const built = sites.length > 0
  if (hub) {
    return {
      repo,
      from: 'github',
      steps,
      says: built
        ? t('Hanzo stops syncing {repo} and deletes its copy here, with its project and any site published from it. Your repository on GitHub is not touched.', { repo })
        : t('Hanzo stops syncing {repo} and deletes its copy here. Your repository on GitHub is not touched.', { repo }),
    }
  }
  return {
    repo,
    from: 'hanzo',
    steps,
    says: built
      ? t('This deletes {repo} and all of its history from Hanzo, with its project and any site published from it. This cannot be undone.', { repo })
      : t('This deletes {repo} and all of its history from Hanzo. This cannot be undone.', { repo }),
  }
}

/**
 * What is left when step `at` of `r` was refused, said with the refusal `why`:
 * nothing gone when it was the first step, else what went and what is still here.
 */
export function stopped(r: Removal, at: number, why: string, t: Say = english): string {
  const values = { repo: r.repo, why }
  const step = r.steps[at]
  // The links go first: one refused leaves the copy and its project as they were.
  if (at === 0 || !step || step.kind === 'link') return t('{repo} was not removed: {why}', values)
  if (step.kind === 'mirror') return t('{repo} no longer syncs from GitHub, but its copy is still on Hanzo: {why}', values)
  return r.from === 'github'
    ? t('Hanzo’s copy of {repo} is deleted, but its project is still on Hanzo: {why}', values)
    : t('{repo} is deleted, but its project is still on Hanzo: {why}', values)
}
