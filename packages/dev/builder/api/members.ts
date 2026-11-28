/**
 * Who is in the organization, and who has been asked to join it, as Hanzo IAM
 * holds them.
 *
 *   GET    /v1/iam/memberships?org=              the roster: {status, data: [membership], data2: count}
 *   GET    /v1/iam/invitations?owner=            {invitations, total} (org admin)
 *   POST   /v1/iam/invitations                   issue one (org admin)
 *   DELETE /v1/iam/invitations/{owner}/{name}    withdraw one (org admin)
 *   GET    /v1/iam/organizations/admin/{org}     the org's record, whose `logo` is its mark (member)
 *
 * IAM decides the scope from the credential: a member reads the roster of an org
 * they belong to, and only an admin of the org reads or writes its invitations.
 * An invitation is a code IAM redeems when someone signs up with it; IAM sends
 * no email of its own.
 */
import { phrase } from '@hanzo/i18n'

import { call, query, seg, Sentence, type Target } from './call.ts'

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const num = (v: unknown): number => (typeof v === 'number' && Number.isFinite(v) ? v : 0)
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})
const arr = (v: unknown): unknown[] => (Array.isArray(v) ? v : [])

export type Role = 'owner' | 'admin' | 'member'
const ROLES: readonly Role[] = ['owner', 'admin', 'member']

export interface Member {
  /** `<homeOrg>/<username>`, IAM's id for the person. */
  user: string
  /** The username alone. */
  name: string
  role: Role
  since: string
}

export function member(raw: unknown): Member {
  const m = obj(raw)
  const user = str(m.user)
  const role = str(m.role) as Role
  return {
    user,
    name: user.includes('/') ? user.slice(user.indexOf('/') + 1) : user,
    role: ROLES.includes(role) ? role : 'member',
    since: str(m.createdTime),
  }
}

/** The org's people, owners first. Grants to a team or narrowed to one project are not the org's roster. */
export async function roster(t: Target, org: string): Promise<Member[]> {
  const raw = obj(await call<unknown>(t, 'GET', `/v1/iam/memberships${query({ org })}`))
  if (raw.status === 'error') throw new Error(str(raw.msg) || phrase('IAM refused the roster'))
  const rank = (r: Role) => ROLES.indexOf(r)
  return arr(raw.data)
    .map(obj)
    .filter((m) => str(m.user) && !str(m.workspace) && !str(m.project))
    .map(member)
    .sort((a, b) => rank(a.role) - rank(b.role) || a.name.localeCompare(b.name))
}

export interface Invitation {
  owner: string
  name: string
  email: string
  /** What the invitee enters when they sign up. */
  code: string
  /** How many may join with it, and how many have. */
  seats: number
  used: number
  /** `Active` is redeemable; anything else is not. */
  state: string
  created: string
}

export function invitation(raw: unknown): Invitation {
  const i = obj(raw)
  return {
    owner: str(i.owner),
    name: str(i.name),
    email: str(i.email),
    code: str(i.code),
    seats: num(i.quota),
    used: num(i.usedCount),
    state: str(i.state),
    created: str(i.createdTime) || str(i.createdAt),
  }
}

export async function invitations(t: Target, org: string): Promise<Invitation[]> {
  const raw = obj(await call<unknown>(t, 'GET', `/v1/iam/invitations${query({ owner: org })}`))
  return arr(raw.invitations).map(invitation).filter((i) => i.name)
}

/** An address that can receive an invitation. */
export const EMAIL = /^[^\s@]+@[^\s@]+\.[^\s@]+$/

/** Random hex, from the browser's own generator. */
function hex(bytes: number): string {
  const b = new Uint8Array(bytes)
  crypto.getRandomValues(b)
  return Array.from(b, (x) => x.toString(16).padStart(2, '0')).join('')
}

/** Invite one address into the org: one seat, redeemable now, pinned to that address. */
export async function invite(t: Target, org: string, email: string): Promise<Invitation> {
  const address = email.trim().toLowerCase()
  if (!EMAIL.test(address)) throw email.trim() ? new Sentence(phrase('{email} is not an email address'), { email: email.trim() }) : new Error(phrase('That is not an email address'))
  const local = address.slice(0, address.indexOf('@')).replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '').slice(0, 24)
  return invitation(
    await call<unknown>(t, 'POST', '/v1/iam/invitations', {
      owner: org,
      name: `invite-${local || 'member'}-${hex(3)}`,
      displayName: address,
      email: address,
      code: hex(8),
      quota: 1,
      usedCount: 0,
      state: 'Active',
    }),
  )
}

export async function revoke(t: Target, i: Pick<Invitation, 'owner' | 'name'>): Promise<void> {
  await call<unknown>(t, 'DELETE', `/v1/iam/invitations/${seg(i.owner)}/${seg(i.name)}`)
}

/** The org's own logo address as IAM holds it, or '' when it has none. */
export async function logo(t: Target, org: string): Promise<string> {
  const raw = obj(await call<unknown>(t, 'GET', `/v1/iam/organizations/admin/${encodeURIComponent(org)}`))
  return str(obj(raw.data ?? raw).logo)
}
