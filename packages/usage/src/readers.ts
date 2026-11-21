import { antigravity, claude, codex, cursor, hanzo, muse, zai } from "./accounts.ts";
import { SINGLE_FETCHERS, type Fetcher } from "./fetchers.ts";

/** Every provider with a live reader, by catalog id. */
export const FETCHERS: Readonly<Record<string, Fetcher>> = {
  ...SINGLE_FETCHERS,
  claude,
  codex,
  cursor,
  hanzo,
  antigravity,
  muse,
  zai,
};
