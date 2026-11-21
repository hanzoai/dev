import { ProviderDriverKind, ProviderInstanceId } from "@hanzodev/contracts";
import type { ProviderSnapshot } from "@hanzodev/usage/schema";
import { useGT } from "gt-react";
import { useState } from "react";

import {
  usePersistEnvironmentProviderInstanceMutation,
  usePrimarySettings,
} from "../../hooks/useSettings";
import { usePrimaryEnvironmentId } from "../../state/environments";
import { Button } from "../ui/button";

const DRIVER_OF: Readonly<Record<string, string>> = { claude: "claudeAgent", codex: "codex" };

/** Offers a login found on disk as a provider instance, so it is never silently merged into another account. */
export function AddAccountButton({ snapshot }: { readonly snapshot: ProviderSnapshot }) {
  const gt = useGT();
  const environmentId = usePrimaryEnvironmentId();
  const instances = usePrimarySettings((settings) => settings.providerInstances);
  const persist = usePersistEnvironmentProviderInstanceMutation(environmentId!);
  const [state, setState] = useState<"idle" | "adding" | "failed">("idle");
  const driver = DRIVER_OF[snapshot.provider];
  const path = snapshot.unconfigured?.path;
  if (!driver || !path || !environmentId) return null;
  const name = snapshot.label ?? path;
  const base = `${driver}_${name.replace(/^\.+/, "").replace(/[^a-zA-Z0-9]+/g, "_")}`;
  let id = base;
  for (let n = 2; Object.hasOwn(instances ?? {}, id); n++) id = `${base}_${n}`;
  return (
    <Button
      size="xs"
      variant="outline"
      disabled={state === "adding"}
      onClick={() => {
        setState("adding");
        void persist({
          operation: "create",
          instanceId: ProviderInstanceId.make(id),
          instance: {
            driver: ProviderDriverKind.make(driver),
            displayName: name,
            enabled: true,
            config: { homePath: path },
          },
        }).then((result) => setState(result._tag === "Failure" ? "failed" : "idle"));
      }}
    >
      {state === "failed" ? gt("Could not add; try again") : gt("Add as account")}
    </Button>
  );
}
