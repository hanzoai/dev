/**
 * The signed-in person's own profile on Hanzo IAM: the name they are shown by
 * and their photo.
 *
 *   PUT  /v1/iam/account  {displayName} → {status, data: {owner, name, displayName, avatar, …}}
 *   POST /v1/account/avatar  multipart `file` → {avatar}   the photo, also written to IAM
 *
 * Both act on the caller alone: neither request names a person. The email is a
 * sign-in identifier and changes only through IAM's verification, never here.
 */
import { phrase } from '@hanzo/i18n'

import { call, headers, reason, Refusal, unwrap, type Target } from './call.ts'

export interface Profile {
  displayName: string
  avatar: string
}

const str = (v: unknown): string => (typeof v === 'string' ? v : '')
const obj = (v: unknown): Record<string, unknown> => (v && typeof v === 'object' ? (v as Record<string, unknown>) : {})

/** Save the name the person is shown by. An empty name is refused here. */
export async function rename(t: Target, name: string): Promise<Profile> {
  const displayName = name.trim()
  if (!displayName) throw new Error(phrase('A name cannot be empty'))
  const p = obj(unwrap(await call<unknown>(t, 'PUT', '/v1/iam/account', { displayName })))
  return { displayName: str(p.displayName), avatar: str(p.avatar) }
}

/** The formats the platform keeps a photo in; it decides by the bytes, this only filters the picker. */
export const PHOTO = 'image/png,image/jpeg,image/gif,image/webp'
const LIMIT = 8 << 20

/** Store a new photo and answer its address. */
export async function photo(t: Target, file: Blob): Promise<string> {
  if (!file.size) throw new Error(phrase('That file is empty'))
  if (file.size > LIMIT) throw new Error(phrase('A photo is at most 8 MB'))
  const form = new FormData()
  form.append('file', file)
  const res = await fetch(`${t.api}/v1/account/avatar`, { method: 'POST', headers: headers(t), body: form, cache: 'no-store' })
  if (!res.ok) throw new Refusal(res.status, (await reason(res)) || `POST /v1/account/avatar answered ${res.status}`)
  const url = str(obj(await res.json()).avatar)
  if (!url.startsWith('https://')) throw new Refusal(502, phrase('The photo was stored without an address this page can show'))
  return url
}
