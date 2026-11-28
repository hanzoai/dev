import {
  antigravity,
  claude,
  codex,
  connectMuse,
  cursor,
  gemini,
  grok,
  groq,
  hanzo,
  mistral,
  muse,
  ollama,
  opencode,
  qwencloud,
  windsurf,
  zai,
} from "./accounts.ts";
import { SINGLE_FETCHERS, type Fetcher } from "./fetchers.ts";

/** Every provider with a live or local reader, by catalog id. */
export const FETCHERS: Readonly<Record<string, Fetcher>> = {
  ...SINGLE_FETCHERS,
  claude,
  codex,
  cursor,
  hanzo,
  antigravity,
  muse,
  zai,
  grok,
  opencode,
  qwencloud,
  gemini,
  mistral,
  windsurf,
  groq,
  ollama,
};

/** Providers whose login lives in another app's keychain: read once, when the user connects, never on a timer. */
export const CONNECTORS: Readonly<Record<string, Fetcher>> = { muse: connectMuse };
