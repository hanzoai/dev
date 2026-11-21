import type { ProviderSnapshot } from "./schema.ts";

type Who = Pick<ProviderSnapshot, "label" | "account">;

/** "Work (z@example.com)", or whichever half the account has. Empty when it has neither. */
export const accountSuffix = ({ label, account }: Who): string =>
  [label, account ? (label ? `(${account})` : account) : undefined].filter(Boolean).join(" ");

/** "Claude · Work (z@example.com)". */
export const accountName = (s: Who, providerName: string): string => {
  const who = accountSuffix(s);
  if (!who) return providerName;
  return s.label ? `${providerName} · ${who}` : `${providerName} (${who})`;
};

/** One key per account of a provider. */
export const snapshotKey = (s: Pick<ProviderSnapshot, "provider" | "instance">): string =>
  `${s.provider}|${s.instance ?? ""}`;
