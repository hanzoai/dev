/**
 * Skills: what an agent knows how to do, as a SKILL.md it reads.
 *
 *   GET    /v1/tool/skills?activated=true   {tools: [Tool]}  the org's skills that are on, brand and own
 *   GET    /v1/tool/skills/authored         {skills: [skill]} the org's own, with their SKILL.md
 *   POST   /v1/tool/skills                  {name, description, content} → 201 {skill}; the same name revises it
 *   DELETE /v1/tool/skills/{id}             {deleted}
 *   GET    /.well-known/agent-skills/index.json         the brand's catalogue, public
 *   GET    /.well-known/agent-skills/{name}/SKILL.md     one of its documents, public
 *   POST   /v1/tool/detect                  {prompt, maxSkills, maxTools} → {skills, tools}  Kai's pick for one message
 *
 * A skill is on when its tool name, `skill_<name>`, is activated (tools.ts):
 * that is how a catalogue skill is added to an org and how an org's own skill is
 * switched off. The brand's skill wins a name an org's own skill also takes.
 */
import { phrase } from '@hanzo/i18n'

import { call, reason, Refusal, seg, type Target } from './call.ts'
import { toolsOf, type Tool } from './tools.ts'

/** An org's own skill. */
export interface Skill {
  /** Derived from the name, and what a delete addresses. */
  id: string
  name: string
  description: string
  /** The SKILL.md body. */
  content: string
  /** When it was last written, Unix seconds. */
  created: number
  /** The repository it was read from, or '' for one written here. */
  source: string
  /** False when no admin wrote it, so no run carries it; true, or unsaid by an older platform. */
  admitted: boolean
}

/** One skill of the brand's catalogue. */
export interface Entry {
  name: string
  description: string
  /** The product it belongs to. */
  product: string
  /** The kinds of work (`roles.ts` WORK) the catalogue recommends it for. */
  works: string[]
}

export interface Catalogue {
  skills: Entry[]
  products: { name: string; count: number }[]
}

/** A skill's name: one lowercase path segment, as the handler takes it. */
export const NAME = /^[a-z0-9][a-z0-9_-]{0,63}$/
/** The most SKILL.md the handler keeps. */
export const MAX = 256 << 10

/** The tool name a skill is switched on by. */
export const tool = (name: string): string => `skill_${name}`
/** The skill a tool name switches, or '' when it is not a skill's. */
export const nameOf = (tool: string): string => (tool.startsWith('skill_') ? tool.slice('skill_'.length) : '')

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
const rows = (v: unknown): unknown[] => (Array.isArray(v) ? v : [])

export function skill(raw: unknown): Skill {
  const o = obj(raw)
  const name = str(o.name)
  return {
    id: str(o.id) || name,
    name,
    description: str(o.description),
    content: str(o.content),
    created: num(o.createdAt),
    source: str(o.source),
    admitted: o.admitted !== false,
  }
}

export function catalogue(raw: unknown): Catalogue {
  const o = obj(raw)
  return {
    skills: rows(o.skills)
      .map((r) => {
        const e = obj(r)
        return { name: str(e.name), description: str(e.description), product: str(e.service), works: rows(e.works).map(str).filter(Boolean) }
      })
      .filter((e) => e.name),
    products: rows(o.products)
      .map((r) => {
        const p = obj(r)
        return { name: str(p.name), count: num(p.skill_count) }
      })
      .filter((p) => p.name),
  }
}

/** The org's skills that are on, brand and own. */
export async function active(t: Target): Promise<Tool[]> {
  return toolsOf(await call<unknown>(t, 'GET', '/v1/tool/skills?activated=true'))
}

export async function authored(t: Target): Promise<Skill[]> {
  return rows(obj(await call<unknown>(t, 'GET', '/v1/tool/skills/authored')).skills).map(skill).filter((s) => s.name)
}

/** Why a skill cannot be written as it stands, or '' when it can. English, marked (`phrase`): a screen says it through `t`. */
export function refuse(s: { name: string; content: string }): string {
  if (!NAME.test(s.name)) return phrase('A name is one lowercase word: letters, digits, _ or -')
  if (!s.content.trim()) return phrase('A skill needs its SKILL.md')
  if (new TextEncoder().encode(s.content).length > MAX) return phrase('A SKILL.md is at most 256 KB')
  return ''
}

export async function write(t: Target, s: { name: string; description: string; content: string }): Promise<Skill> {
  const why = refuse(s)
  if (why) throw new Error(why)
  return skill(obj(await call<unknown>(t, 'POST', '/v1/tool/skills', { name: s.name, description: s.description, content: s.content })).skill)
}

export async function remove(t: Target, id: string): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/tool/skills/${seg(id)}`)
}

/** A public document of the brand's catalogue. It needs no bearer, so it sends none. */
async function open(t: Target, path: string): Promise<Response> {
  const res = await fetch(`${t.api}/.well-known/agent-skills/${path}`)
  if (!res.ok) throw new Refusal(res.status, (await reason(res)) || `The catalogue answered ${res.status}`)
  return res
}

export async function brand(t: Target): Promise<Catalogue> {
  return catalogue(await (await open(t, 'index.json')).json())
}

/** One catalogue skill's SKILL.md. */
export async function document(t: Target, name: string): Promise<string> {
  return (await open(t, `${seg(name)}/SKILL.md`)).text()
}

/** One thing Kai picked for a message: a skill (`skill_<name>`) or a tool, how likely it applies, and what it does. */
export interface Detected {
  /** The skill's own name for a skill (`code-review`), else the tool's. */
  name: string
  skill: boolean
  /** Where it comes from: skill, mcp, connector, function, agent. */
  source: string
  description: string
  /** 0 to 1. */
  probability: number
}

/**
 * Which skills and tools apply to `prompt`, as Kai decides it (cloud apps/tools
 * detect.go; a lexical fallback answers when Kai does not). Only asks: nothing
 * is switched on (`autoActivate` is left out), so a message carries what was
 * picked for it and nothing more.
 */
export async function detect(t: Target, prompt: string, opts: { maxSkills?: number; maxTools?: number; signal?: AbortSignal } = {}): Promise<Detected[]> {
  const raw = await call<unknown>(t, 'POST', '/v1/tool/detect', { prompt, maxSkills: opts.maxSkills ?? 3, maxTools: opts.maxTools ?? 3 }, { signal: opts.signal })
  const r = (raw && typeof raw === 'object' ? raw : {}) as { skills?: unknown; tools?: unknown }
  const read = (list: unknown, skill: boolean): Detected[] =>
    (Array.isArray(list) ? list : []).flatMap((x) => {
      const o = (x && typeof x === 'object' ? x : {}) as Record<string, unknown>
      const tool = str(o.name)
      const name = skill ? nameOf(tool) || tool : tool
      return name ? [{ name, skill, source: str(o.source), description: str(o.description), probability: num(o.probability) }] : []
    })
  return [...read(r.skills, true), ...read(r.tools, false)]
}
