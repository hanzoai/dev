/**
 * Redesigning a site from its address (hanzo-inc/app issue 312, hanzoai/desktop issue 15).
 *
 * A person pastes a site's address into New on This computer. Hanzo opens the site, reads it, critiques it, and
 * builds a redesign in a folder here while a live preview of it runs in the person's browser.
 *
 * THE HOST DOES WHAT THE AGENT CANNOT. The agent that rebuilds the site works in its folder with no network, so the
 * host (Hanzo Desktop) opens the site from the computer's own connection and saves a copy into the folder
 * (`capture`: the page, its words, its stylesheets, and a picture when the person's browser can take one), serves the
 * folder's `redesign/` live (`preview`), and shows it in Chrome (`show`). The agent reads the copy, writes its
 * critique as its answer, and writes the new site as static files into `redesign/`; the preview follows each file it
 * saves. A host that cannot do this (a browser page) supplies no `Site`, and New offers nothing of it.
 *
 * Nothing here chooses where a request may go: the host decides what it will open. What is here is the contract, and
 * the pure parts of it: which words are a site's address, what the folder is called, and what the agent is told.
 */
import type { Say } from '@hanzo/i18n/react'

import type { Folder } from './local.ts'
import { english } from './say.ts'

/** What a host saved of a site, as it says it. */
export interface Saved {
  /** What the site calls itself, or its address. */
  title: string
  /** Where the site ended, after any redirects: the address the copy is of. */
  url: string
  /** The files saved under `.hanzo/reference/` in the folder, by name. */
  files: readonly string[]
  /** Whether one of them is a picture of the page. */
  screenshot: boolean
}

/** A live preview of a folder's `redesign/`. */
export interface Live {
  /** The address that shows it, on this computer. */
  url: string
}

/** What a host that can redesign a site can do. Every answer is plain words when it refuses: they are said as they come. */
export interface Site {
  /** The folder a redesign of `name` goes in: a new one, named for it. It is made when the site is saved. */
  folder: (name: string) => Promise<Folder>
  /** Open the site and save a copy of it into `folder`, for the agent to read. */
  capture: (url: string, folder: string) => Promise<Saved>
  /** Serve `folder`'s `redesign/` live; the same address for as long as it runs, reloading as its files change. */
  preview: (folder: string) => Promise<Live>
  /** Stop serving it. */
  stop: (folder: string) => Promise<void>
  /** Show a preview in the person's browser: Chrome where it is installed. */
  show: (url: string) => Promise<void>
}

/** The top-level names a bare address may end in: the ones people paste. A file name (`index.html`, `README.md`, `main.rs`) is none of them. */
const TLDS = new Set(
  'com org net io ai app dev co me xyz site shop store online tech design studio agency news blog edu gov info biz us uk ca de fr jp au nz in eu nl se no es it ch be pl br mx cn'.split(' '),
)

/** The verbs that ask for this with an address in the same sentence. */
const REDESIGN = /\b(re-?design|re-?build|re-?vamp|re-?style|re-?work|modernis[ez]e|refresh|critique|improve|review)\b/i

/** A scheme-less address that names this computer: it carries a port, so the word `localhost` alone is not one. */
const LOCAL = /^(?:localhost|127(?:\.\d{1,3}){3}|\[::1\]):\d{1,5}(?:[/?#]\S*)?$/i

/** A bare name: labels, a top-level name people paste, an optional port and path. */
const BARE = /^(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+([a-z]{2,24})(?::\d{1,5})?(?:[/?#]\S*)?$/i

/** What a sentence hangs off the front of an address, and off its end, that is not part of it. */
const FRONT = /^[(<["'“‘]+/

/** An address as written in a sentence: without the brackets, quotes and punctuation round it (a `)` that closes a `(` inside it stays). */
function bare(word: string): string {
  let text = word.trim().replace(FRONT, '')
  for (;;) {
    const last = text[text.length - 1]
    const open = last === ')' && (text.match(/\(/g)?.length ?? 0) >= (text.match(/\)/g)?.length ?? 0)
    if (!last || open || !'.,;:!?\'"”’>])'.includes(last)) return text
    text = text.slice(0, -1)
  }
}

/** One word as an address, or null. A scheme is taken as written; a bare name must end in a name people paste. */
export function addressOf(word: string): string | null {
  const text = bare(word)
  if (!text || text.length > 2048) return null
  let full: string
  if (/^https?:\/\//i.test(text)) full = text
  else if (LOCAL.test(text)) full = `http://${text}`
  else {
    const m = BARE.exec(text)
    if (!m || !TLDS.has(m[1]!.toLowerCase())) return null
    full = `https://${text}`
  }
  try {
    const url = new URL(full)
    if (!url.hostname || url.username || url.password) return null
    if (!url.hostname.includes('.') && !url.hostname.includes(':') && url.hostname !== 'localhost') return null
    return url.href
  } catch {
    return null
  }
}

/** A site named in the ask, and what the person said besides. */
export interface Asked {
  /** The address, whole. */
  url: string
  /** The rest of what they said, which goes to the agent as their direction; '' for an address alone. */
  note: string
}

/**
 * The site an ask is about, or null when it is about none. An ask is a site's when it is an address alone, or has one
 * address and asks to redesign, rebuild, restyle or critique: anything else is a message that happens to hold a link,
 * and is sent as it is.
 */
export function urlOf(ask: string): Asked | null {
  const words = ask.trim().split(/\s+/).filter(Boolean)
  if (!words.length) return null
  const found = words.map((w) => ({ w, url: addressOf(w) })).filter((x) => x.url !== null)
  if (words.length === 1) return found[0] ? { url: found[0].url!, note: '' } : null
  if (found.length !== 1) return null
  const note = words.filter((w) => w !== found[0]!.w).join(' ')
  return REDESIGN.test(note) ? { url: found[0]!.url!, note } : null
}

/** The site's own name: its host without `www.` and its top-level name (`blog.example.com` is `blog-example`). */
export function siteName(url: string): string {
  let host = ''
  try {
    host = new URL(url).hostname.toLowerCase()
  } catch {
    return 'site'
  }
  host = host.replace(/^www\./, '').replace(/^\[|\]$/g, '')
  if (host === 'localhost' || host.endsWith('.localhost') || /^[\d.]+$/.test(host) || host.includes(':')) return 'localhost'
  const labels = host.split('.')
  if (labels.length > 1) labels.pop()
  if (labels.length > 1 && ['co', 'com', 'org', 'net', 'ac', 'gov', 'edu'].includes(labels[labels.length - 1]!)) labels.pop()
  return labels.join('-').replace(/[^a-z0-9-]+/g, '-').replace(/-+/g, '-').replace(/^-|-$/g, '') || 'site'
}

/** What the redesign's folder is called: `example-redesign`. */
export function folderName(url: string): string {
  return `${siteName(url)}-redesign`
}

/** What a person reads for a site: its host, without `www.`. */
export function hostOf(url: string): string {
  try {
    return new URL(url).host.replace(/^www\./, '')
  } catch {
    return url
  }
}

/** What each saved file is, in the words the agent is told. */
function describe(file: string): string {
  if (file === 'text.md') return 'the site’s address, its words in reading order, and the pictures it shows'
  if (file === 'page.html') return 'the page as the site sent it'
  if (/^style-\d+\.css$/.test(file)) return 'one of its stylesheets'
  if (/^screenshot\./.test(file)) return 'how the page looks'
  return 'saved from the site'
}

/**
 * What the agent is told. It has no network and no browser: it reads the copy the host saved, answers with a plain
 * critique, and writes the new site as static files into `redesign/`, which is already being shown.
 */
export function brief(saved: Saved, note = ''): string {
  const what = saved.title && saved.title !== saved.url ? `${saved.url} (${saved.title})` : saved.url
  const lines = [
    `Redesign the website ${what}.`,
    '',
    'The site is saved in this folder for you to read, so you need no network:',
    ...saved.files.map((f) => `- .hanzo/reference/${f}: ${describe(f)}`),
    '',
    'Do two things.',
    '',
    '1. Critique it. Begin your answer with a short critique in plain language: what works, what does not, and what you will change, across design, content and usability. Ten lines at most, no jargon.',
    '2. Rebuild it. Build the redesigned site as plain static files in the folder `redesign/` here: `index.html`, with its CSS, any scripts and any images beside it, linked by relative paths. Keep the site’s own words and purpose. Improve the layout, type, colour, spacing and clarity, and make it work well on a phone. Pictures may be linked from their own addresses, as text.md lists them. Do not start a server and do not use the network: a live preview of `redesign/` is already open for the person and reloads by itself each time you save a file.',
  ]
  if (note.trim()) lines.push('', `The person added: “${note.trim()}”`)
  lines.push('', 'End by saying, in a sentence or two, what you changed and what to look at first.')
  return lines.join('\n')
}

/** Why a step stopped, in the reader's words: the host's own plain refusals as they come, else this. */
export function stopped(e: unknown, say: Say = english): string {
  const why = typeof e === 'string' ? e : e instanceof Error ? e.message : ''
  return why || say('The redesign could not start. Try again.')
}
