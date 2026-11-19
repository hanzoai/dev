import type { ServerProvider, ServerProviderModel } from "@hanzodev/contracts";
import { createModelCapabilities } from "@hanzodev/shared/model";
import {
  buildHanzoIamAccountFromTokens,
  type HanzoIamAccount,
} from "@hanzodev/shared/hanzoIamAuth";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Path from "effect/Path";

const DEFAULT_REASONING_DESCRIPTOR = {
  id: "reasoningEffort",
  label: "Reasoning",
  type: "select" as const,
  role: "reasoning" as const,
  currentValue: "medium",
  options: [
    { id: "low", label: "Low" },
    { id: "medium", label: "Medium", isDefault: true },
    { id: "high", label: "High" },
  ],
};

const HANZO_REASONING_CAPABILITIES = createModelCapabilities({
  optionDescriptors: [DEFAULT_REASONING_DESCRIPTOR],
});

export const CURATED_HANZO_MODELS: ReadonlyArray<ServerProviderModel> = [
  {
    slug: "enso-auto",
    name: "Enso (Auto Router)",
    shortName: "Enso",
    isCustom: false,
    isDefault: true,
    aliases: ["enso"],
    capabilities: HANZO_REASONING_CAPABILITIES,
  },
  {
    slug: "enso-ultra",
    name: "Enso Ultra",
    shortName: "Ultra",
    isCustom: false,
    capabilities: HANZO_REASONING_CAPABILITIES,
  },
  {
    slug: "enso-max",
    name: "Enso Max",
    shortName: "Max",
    isCustom: false,
    capabilities: HANZO_REASONING_CAPABILITIES,
  },
  {
    slug: "zen6-coder",
    name: "Zen 6 Coder",
    shortName: "Zen 6 Coder",
    isCustom: false,
    aliases: ["zen", "zen-coder"],
    capabilities: HANZO_REASONING_CAPABILITIES,
  },
  {
    slug: "zen6-max",
    name: "Zen 6 Max",
    shortName: "Zen 6 Max",
    isCustom: false,
    capabilities: HANZO_REASONING_CAPABILITIES,
  },
  {
    slug: "zen6",
    name: "Zen 6",
    shortName: "Zen 6",
    isCustom: false,
    capabilities: HANZO_REASONING_CAPABILITIES,
  },
  {
    slug: "zen6-flash",
    name: "Zen 6 Flash",
    shortName: "Zen 6 Flash",
    isCustom: false,
    capabilities: HANZO_REASONING_CAPABILITIES,
  },
  {
    slug: "hanzo/kai",
    name: "Kai 1.7",
    shortName: "Kai",
    isCustom: false,
    aliases: ["kai"],
    capabilities: HANZO_REASONING_CAPABILITIES,
  },
  {
    slug: "enso-flash",
    name: "Enso Flash",
    shortName: "Flash",
    isCustom: false,
    capabilities: HANZO_REASONING_CAPABILITIES,
  },
];

export interface LocalHanzoIdentitySnapshot {
  readonly activeAccountId: string | null;
  readonly accounts: ReadonlyArray<HanzoIamAccount>;
}

export const readLocalHanzoIdentities = Effect.fn("hanzoLocalIdentity.readLocalHanzoIdentities")(
  function* () {
    const fs = yield* FileSystem.FileSystem;
    const path = yield* Path.Path;
    const home = yield* HostProcess.HomeDirectory;
    const identitiesPath = path.join(home, ".hanzo", "identities.json");
    const exists = yield* fs.exists(identitiesPath).pipe(Effect.orElseSucceed(() => false));
    if (!exists) {
      return { activeAccountId: null, accounts: [] } satisfies LocalHanzoIdentitySnapshot;
    }
    const raw = yield* fs.readFileString(identitiesPath).pipe(Effect.orElseSucceed(() => ""));
    if (!raw.trim()) {
      return { activeAccountId: null, accounts: [] } satisfies LocalHanzoIdentitySnapshot;
    }
    try {
      const parsed = JSON.parse(raw) as {
        readonly active?: unknown;
        readonly identities?: Record<string, Record<string, unknown>>;
      };
      const accounts: Array<HanzoIamAccount> = [];
      for (const [id, entry] of Object.entries(parsed.identities ?? {})) {
        if (!entry || typeof entry !== "object") continue;
        const accessToken =
          typeof entry.access_token === "string" ? entry.access_token.trim() : "";
        if (!accessToken) continue;
        const refreshToken =
          typeof entry.refresh_token === "string" && entry.refresh_token.trim()
            ? entry.refresh_token.trim()
            : null;
        const owner =
          typeof entry.owner === "string" && entry.owner.trim()
            ? entry.owner.trim()
            : (id.split("/")[0] ?? "hanzo");
        const subject =
          typeof entry.subject === "string" && entry.subject.trim()
            ? entry.subject.trim()
            : (id.split("/")[1] ?? "user");
        const issuer =
          typeof entry.issuer === "string" && entry.issuer.trim()
            ? entry.issuer.trim()
            : "https://hanzo.id";
        const email =
          typeof entry.email === "string" && entry.email.trim() ? entry.email.trim() : null;
        const account = buildHanzoIamAccountFromTokens({
          issuer,
          clientId: "hanzo-app",
          accessToken,
          refreshToken,
          userinfo: {
            sub: subject,
            owner,
            name: subject,
            displayName: `${owner}/${subject}`,
            email,
          },
        });
        accounts.push({ ...account, id });
      }
      const activeRaw = typeof parsed.active === "string" && parsed.active.trim() ? parsed.active.trim() : null;
      const activeAccountId =
        accounts.find((entry) => entry.id === activeRaw)?.id ?? accounts[0]?.id ?? null;
      return { activeAccountId, accounts } satisfies LocalHanzoIdentitySnapshot;
    } catch {
      return { activeAccountId: null, accounts: [] } satisfies LocalHanzoIdentitySnapshot;
    }
  },
);

export const resolveHanzoProviderAuth = Effect.fn("hanzoLocalIdentity.resolveHanzoProviderAuth")(
  function* (): Effect.fn.Return<
    ServerProvider["auth"],
    never,
    FileSystem.FileSystem | Path.Path | HostProcess.HomeDirectory
  > {
    const snapshot = yield* readLocalHanzoIdentities();
    const active =
      snapshot.accounts.find((entry) => entry.id === snapshot.activeAccountId) ??
      snapshot.accounts[0];
    if (!active) {
      return { status: "unknown", type: "hanzo", label: "Hanzo Cloud" };
    }
    const extraCount = Math.max(0, snapshot.accounts.length - 1);
    const label =
      extraCount > 0
        ? `Hanzo IAM (${active.id} +${extraCount})`
        : `Hanzo IAM (${active.id})`;
    return {
      status: "authenticated",
      type: "hanzo",
      label,
      ...(active.email ? { email: active.email } : {}),
      workspaceId: active.owner,
    };
  },
);

export const mergeCuratedHanzoModels = (
  discovered: ReadonlyArray<ServerProviderModel> | undefined,
): ReadonlyArray<ServerProviderModel> => {
  const bySlug = new Map<string, ServerProviderModel>();
  for (const curated of CURATED_HANZO_MODELS) {
    bySlug.set(curated.slug, curated);
  }
  for (const model of discovered ?? []) {
    if (!bySlug.has(model.slug)) {
      bySlug.set(model.slug, { ...model, isDefault: false });
    }
  }
  return Array.from(bySlug.values());
};

export const resolveDefaultHanzoDevBinary = Effect.fn(
  "hanzoLocalIdentity.resolveDefaultHanzoDevBinary",
)(function* () {
  const fs = yield* FileSystem.FileSystem;
  const path = yield* Path.Path;
  const home = yield* HostProcess.HomeDirectory;
  const candidates = [
    path.join(home, ".local", "bin", "dev"),
    path.join(home, ".hanzo", "bin", "dev"),
    path.join(home, ".local", "bin", "hanzo-dev"),
    "/opt/homebrew/bin/dev",
    "/opt/homebrew/bin/hanzo-dev",
    "/usr/local/bin/dev",
    "/usr/local/bin/hanzo-dev",
  ];
  for (const candidate of candidates) {
    if (yield* fs.exists(candidate).pipe(Effect.orElseSucceed(() => false))) {
      const resolved = yield* fs.realPath(candidate).pipe(Effect.orElseSucceed(() => candidate));
      if (/\.(?:m|c)?js$/iu.test(resolved)) {
        continue;
      }
      return candidate;
    }
  }
  return "dev";
});
