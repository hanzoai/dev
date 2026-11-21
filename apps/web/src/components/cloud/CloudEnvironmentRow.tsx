import { T, useGT } from "gt-react";
import { CloudIcon } from "lucide-react";
import { useState } from "react";

import { useEnvironmentDiscovery } from "~/cloud/environmentDiscovery";
import { useCloudChat } from "~/cloud/cloudChat";
import { ITEM_ROW_CLASSNAME, ITEM_ROW_INNER_CLASSNAME } from "../settings/itemRows";
import { Button } from "../ui/button";

/**
 * The Cloud environment of the signed-in account. Always listed: it runs when the cloud can
 * start a session and says "Not available yet" when it cannot.
 */
export function CloudEnvironmentRow() {
  const gt = useGT();
  const discovery = useEnvironmentDiscovery();
  const chat = useCloudChat();
  const [outcome, setOutcome] = useState<"started" | "unavailable" | "failed" | null>(null);
  if (discovery === null || !discovery.environments.some(({ type }) => type === "cloud")) {
    return null;
  }
  const available = discovery.cloud.status === "ok" && outcome !== "unavailable";
  return (
    <div className={ITEM_ROW_CLASSNAME} data-testid="cloud-environment">
      <div className={ITEM_ROW_INNER_CLASSNAME}>
        <div className="min-w-0">
          <div className="flex items-center gap-2">
            <CloudIcon aria-hidden className="size-4 shrink-0 text-muted-foreground" />
            <span className="truncate text-sm font-medium">{gt("Cloud")}</span>
          </div>
          <p className="mt-1 text-xs text-muted-foreground">
            {!available ? (
              <T>Not available yet</T>
            ) : outcome === "started" ? (
              <T>Cloud machine started</T>
            ) : outcome === "failed" ? (
              <T>Could not start a cloud machine. Try again.</T>
            ) : (
              <T>Runs in the Hanzo cloud</T>
            )}
          </p>
        </div>
        <Button
          disabled={!available}
          onClick={() => void chat.runInCloud().then((result) => setOutcome(result.kind))}
          size="sm"
          variant="outline"
        >
          <T>Start</T>
        </Button>
      </div>
    </div>
  );
}
