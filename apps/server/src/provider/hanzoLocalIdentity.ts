import type { ServerProvider, ServerProviderModel } from "@hanzodev/contracts";
import { createModelCapabilities } from "@hanzodev/shared/model";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Path from "effect/Path";

import { readSharedIdentity, type SharedIdentityHost } from "./hanzoSharedIdentityNode.ts";

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

export type LocalHanzoIdentitySnapshot = SharedIdentitySnapshot;
export const sharedIdentityHost = Effect.fn("hanzoLocalIdentity.sharedIdentityHost")(function* () {
  return {
    platform: yield* HostProcess.Platform,
    home: yield* HostProcess.HomeDirectory,
    env: yield* HostProcess.Environment,
  } satisfies SharedIdentityHost;
});

export const readLocalHanzoIdentities = Effect.fn("hanzoLocalIdentity.readLocalHanzoIdentities")(
  function* () {
    const host = yield* sharedIdentityHost();
    return yield* Effect.promise(() => readSharedIdentity(host));
  },
);

export const resolveHanzoProviderAuth = Effect.fn("hanzoLocalIdentity.resolveHanzoProviderAuth")(
  function* () {
    const snapshot = yield* readLocalHanzoIdentities();
    const active =
      snapshot.accounts.find((entry) => entry.id === snapshot.activeAccountId) ??
      snapshot.accounts[0];
    if (!active) {
      return {
        status: "unknown",
        type: "hanzo",
        label: "Hanzo Cloud",
      } satisfies ServerProvider["auth"];
    }
    const extraCount = Math.max(0, snapshot.accounts.length - 1);
    const label =
      extraCount > 0 ? `Hanzo IAM (${active.id} +${extraCount})` : `Hanzo IAM (${active.id})`;
    return {
      status: "authenticated",
      type: "hanzo",
      label,
      ...(active.email ? { email: active.email } : {}),
      workspaceId: active.owner,
    } satisfies ServerProvider["auth"];
  },
);

export const mergeCuratedHanzoModels = (
  discovered: ReadonlyArray<ServerProviderModel> | undefined,
): ReadonlyArray<ServerProviderModel> => {
  const bySlug = new Map<string, ServerProviderModel>();
  const usedNames = new Set<string>();
  for (const curated of CURATED_HANZO_MODELS) {
    bySlug.set(curated.slug, curated);
    usedNames.add(curated.name.trim().toLowerCase());
  }
  for (const model of discovered ?? []) {
    if (!bySlug.has(model.slug)) {
      const normalizedName = model.name.trim().toLowerCase();
      const disambiguatedName = usedNames.has(normalizedName)
        ? `${model.name} (${model.slug})`
        : model.name;
      usedNames.add(disambiguatedName.trim().toLowerCase());
      bySlug.set(model.slug, { ...model, name: disambiguatedName, isDefault: false });
    }
  }
  return Array.from(bySlug.values());
};

export const resolveDefaultHanzoGenBinary = Effect.fn(
  "hanzoLocalIdentity.resolveDefaultHanzoGenBinary",
)(function* () {
  const fs = yield* FileSystem.FileSystem;
  const path = yield* Path.Path;
  const home = yield* HostProcess.HomeDirectory;
  const candidates = [
    path.join(home, ".local", "bin", "gen"),
    path.join(home, ".local", "bin", "hanzo-gen"),
    path.join(home, ".hanzo", "bin", "gen"),
    "/opt/homebrew/bin/gen",
    "/opt/homebrew/bin/hanzo-gen",
    "/usr/local/bin/gen",
    "/usr/local/bin/hanzo-gen",
  ];
  for (const candidate of candidates) {
    if (yield* fs.exists(candidate).pipe(Effect.orElseSucceed(() => false))) {
      return candidate;
    }
  }
  return null;
});

export const resolveDefaultHanzoBinary = Effect.fn("hanzoLocalIdentity.resolveDefaultHanzoBinary")(
  function* () {
    const fs = yield* FileSystem.FileSystem;
    const path = yield* Path.Path;
    const home = yield* HostProcess.HomeDirectory;
    const candidates = [
      path.join(home, ".local", "bin", "hanzo"),
      path.join(home, ".hanzo", "bin", "hanzo"),
      "/opt/homebrew/bin/hanzo",
      "/usr/local/bin/hanzo",
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
    return "hanzo";
  },
);

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
