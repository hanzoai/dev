/**
 * What a hanzo.id access token says, read without trusting it for anything the
 * platform does not check again.
 */

/** The token's claims, or null when it is not a JWT. */
export function claims(token: string | null): Record<string, unknown> | null {
  if (!token) return null
  try {
    const part = token.split('.')[1]
    if (!part) return null
    return JSON.parse(atob(part.replace(/-/g, '+').replace(/_/g, '/'))) as Record<string, unknown>
  } catch {
    return null
  }
}

/** The coarse IAM roles that administer an org: its owner and its admins. */
const ADMIN = ['owner', 'admin']

/**
 * Whether the token says its person administers `org`: owner or admin, read off
 * the token's own `orgs` claim — never the reserved `admin` org, which is
 * platform authority and not what a builder decides anything by. The platform
 * enforces it again; this only decides which controls are offered.
 */
export function administers(token: string | null, org: string | null): boolean {
  if (!org) return false
  const set = claims(token)?.orgs
  return (Array.isArray(set) ? set : []).some((r) => {
    const m = r as { org?: unknown; role?: unknown } | null
    return m?.org === org && ADMIN.includes(m.role as string)
  })
}
