/**
 * A repository's address, as a person pastes it: what GitHub's page, its Code
 * button or a terminal hands them.
 *
 *   https://github.com/owner/name        the page; with .git, the clone address
 *   git@github.com:owner/name.git        the SSH clone address
 *   ssh://git@github.com/owner/name.git  the same, as a URL
 *   github.com/owner/name                the address without its scheme
 *
 * It is read to the repository it names, and the platform is handed that in
 * the form each place takes: a coding run `github.com/owner/name` (choice.ts
 * `asHub`; apps/coding start.go reads it with forge.GitHub), a project its https
 * clone address (`clone`; PATCH /v1/project/{slug} takes `repo.url` as one).
 *
 * GitHub only. A run on a repository on GitHub alone is admitted by GitHub
 * itself, while another host's `owner/name` is read by the platform as the org's
 * own namespace on the forge and refused — so taking it here would only move the
 * refusal to after the ask was typed. Anything else is refused here, in a
 * sentence that says what is taken.
 */
import { phrase } from '@hanzo/i18n'

/** A repository on GitHub, by its account and its name. */
export interface At {
  owner: string
  name: string
}

/** What a pasted address may be, said when it is something else. */
export const ADDRESS = phrase('Paste a GitHub repository’s address, like https://github.com/owner/name, github.com/owner/name or git@github.com:owner/name.git.')

/** GitHub's shape for an account or a repository name, as the platform checks it (forge/github.go `ghName`). */
const NAME = /^[A-Za-z0-9][A-Za-z0-9._-]{0,99}$/

/**
 * The address as a URL, or without its scheme: any scheme git speaks, a user in
 * front of the host (`git@`, or a token a clone address was copied with, which
 * goes no further than this), and a page's query or fragment (`?tab=readme`)
 * after the path. The host is github.com itself, so `github.com.example` or a
 * `github.com@` user in front of another host is no address of GitHub's.
 */
const URL_FORM = /^(?:(?:https?|ssh|git|git\+ssh):\/\/)?(?:[^\s/@]+@)?(?:www\.)?github\.com\/([^\s?#]*)(?:[?#]\S*)?$/i

/** The SSH clone address, as GitHub's Code button gives it: `git@github.com:owner/name.git`. */
const SCP_FORM = /^(?:[^\s/@:]+@)?github\.com:([^\s?#]*)$/i

/** The GitHub repository a pasted address names, or null when it names none. */
export function onGitHub(text: string): At | null {
  const s = text.trim()
  const m = URL_FORM.exec(s) ?? SCP_FORM.exec(s)
  if (!m) return null
  // A trailing slash, then `.git`, are the clone address's and not the name's.
  const parts = m[1].replace(/\/+$/, '').replace(/\.git$/i, '').split('/')
  if (parts.length !== 2) return null
  const [owner, name] = parts
  return NAME.test(owner) && NAME.test(name) ? { owner, name } : null
}

/** The https address a project's repository is cloned from: what the sync app writes for a GitHub repository too. */
export const clone = (at: At): string => `https://github.com/${at.owner}/${at.name}.git`
