import type { WebSection } from "./workspace-section.logic";

/** hanzo.app is served at hanzo.ai: the app is a path on the apex (@hanzo/app lib/host). */
export const HANZO_APP_ORIGIN = "https://hanzo.ai";

/** The routes of hanzo.ai's app/library, app/design and plugins pages. */
export const WEB_SECTION_PATHS = {
  library: "/library",
  design: "/design",
  plugins: "/dev/-/customize/plugins",
} as const satisfies Readonly<Record<WebSection, string>>;

export function webSectionUrl(section: WebSection, origin: string = HANZO_APP_ORIGIN): string {
  return `${origin}${WEB_SECTION_PATHS[section]}`;
}

/** Whether the web view may load `url`: the app's own origin, over https. Anything else opens outside. */
export function isAppUrl(url: string, origin: string = HANZO_APP_ORIGIN): boolean {
  try {
    const parsed = new URL(url);
    return parsed.protocol === "https:" && parsed.origin === origin;
  } catch {
    return false;
  }
}

export interface WebSession {
  readonly accessToken: string;
  readonly refreshToken: string | null;
  /** Epoch milliseconds, or null for a token that does not expire. */
  readonly expiresAt: number | null;
}

/**
 * The @hanzo/iam SDK's storage keys, which hanzo.app reads from localStorage (@hanzo/app
 * lib/session: `bearer()`, `live()`, and the BOOT script that marks a signed-in page).
 * Same names, same units: expires_at is epoch milliseconds.
 */
export const SESSION_KEYS = {
  accessToken: "hanzo_iam_access_token",
  refreshToken: "hanzo_iam_refresh_token",
  expiresAt: "hanzo_iam_expires_at",
} as const;

/**
 * The script run before the page's own, on the app's origin only: writes the signed-in account's
 * tokens where hanzo.app looks for a session, so the section opens signed in with no second
 * sign-in. Values go through JSON.stringify, so a token can never end the string it sits in.
 */
export function sessionScript(session: WebSession): string {
  const entries: [string, string][] = [[SESSION_KEYS.accessToken, session.accessToken]];
  if (session.refreshToken) entries.push([SESSION_KEYS.refreshToken, session.refreshToken]);
  if (session.expiresAt !== null) entries.push([SESSION_KEYS.expiresAt, String(session.expiresAt)]);
  const writes = entries
    .map(([key, value]) => `localStorage.setItem(${JSON.stringify(key)},${JSON.stringify(value)});`)
    .join("");
  return `try{${writes}}catch(_){}true;`;
}
