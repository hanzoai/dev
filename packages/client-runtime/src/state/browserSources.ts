import { WS_METHODS } from "@hanzodev/contracts";
import type { Atom } from "effect/reactivity";

import type { EnvironmentRegistry } from "../connection/registry.ts";
import { createEnvironmentRpcQueryAtomFamily } from "./runtime.ts";

/** HIP-1350 Browser sources: the Hanzo extensions joined to this computer's ZAP router. */
export function createBrowserSourceAtoms<R, E>(
  runtime: Atom.AtomRuntime<EnvironmentRegistry | R, E>,
) {
  return {
    list: createEnvironmentRpcQueryAtomFamily(runtime, {
      label: "environment-data:browser-sources:list",
      tag: WS_METHODS.browserSourcesList,
      staleTimeMs: 5_000,
    }),
  };
}
