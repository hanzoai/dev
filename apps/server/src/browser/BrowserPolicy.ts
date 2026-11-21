/**
 * What an agent may do through the user's own browser (HIP-1350 Browser sources).
 *
 * Consumer AI products bar automated access in their terms, so no tool here reaches a page on
 * one of them, however the agent got there. The check runs on the tab's real URL, in the tool
 * layer, before any call leaves for the extension.
 */

interface ConsumerAiOrigin {
  readonly product: string;
  readonly host: string;
  /** Limits the match to one path prefix on a shared host. */
  readonly path?: string;
}

const CONSUMER_AI_ORIGINS: ReadonlyArray<ConsumerAiOrigin> = [
  { product: "ChatGPT", host: "chatgpt.com" },
  { product: "ChatGPT", host: "chat.openai.com" },
  { product: "Claude.ai", host: "claude.ai" },
  { product: "Gemini", host: "gemini.google.com" },
  { product: "Gemini", host: "bard.google.com" },
  { product: "Grok", host: "grok.com" },
  { product: "Grok", host: "x.com", path: "/i/grok" },
  { product: "Grok", host: "twitter.com", path: "/i/grok" },
  { product: "Perplexity", host: "perplexity.ai" },
  { product: "Copilot", host: "copilot.microsoft.com" },
  { product: "Cursor", host: "cursor.com" },
];

const onHost = (hostname: string, host: string) =>
  hostname === host || hostname.endsWith(`.${host}`);

const UNKNOWN = Symbol("unknown page");

const productOf = (url: string | undefined): string | typeof UNKNOWN | undefined => {
  if (url === undefined || url === "") return UNKNOWN;
  let parsed: URL;
  try {
    parsed = new URL(url);
  } catch {
    return UNKNOWN;
  }
  if (parsed.protocol === "about:" && parsed.href === "about:blank") return undefined;
  if (parsed.protocol !== "https:" && parsed.protocol !== "http:") return UNKNOWN;
  const hostname = parsed.hostname.toLowerCase().replace(/\.$/u, "");
  const path = parsed.pathname.toLowerCase();
  return CONSUMER_AI_ORIGINS.find(
    (origin) =>
      onHost(hostname, origin.host) &&
      (origin.path === undefined || path === origin.path || path.startsWith(`${origin.path}/`)),
  )?.product;
};

/**
 * Why the page at `url` is off limits, or undefined when it is not. A URL that does not parse,
 * or is not http(s) or about:blank, cannot be shown to be safe, so it is refused too.
 */
export const refusal = (url: string | undefined): string | undefined => {
  const product = productOf(url);
  if (product === undefined) return undefined;
  if (product === UNKNOWN) {
    return "Hanzo Dev cannot tell what page this tab shows, so it does not drive it. Open an http(s) page in the tab.";
  }
  return (
    `Hanzo Dev does not drive ${product} through the browser: its terms bar automated access. ` +
    "Use its API key, or the provider's own signed-in client, to run work there."
  );
};

/** The extension methods an agent's browser tools reach. Nothing else is callable. */
export const BROWSER_METHODS = {
  tabs: "hanzo.listTabs",
  snapshot: "hanzo.snapshot",
  read: "hanzo.read",
  act: "hanzo.act",
} as const;

/** `provider.accounts` and `usage.report` are read only when the extension advertises them. */
export const ACCOUNTS_CAPABILITY = "provider.accounts";
export const USAGE_CAPABILITY = "usage.report";
