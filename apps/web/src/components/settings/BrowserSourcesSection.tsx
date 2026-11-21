import type { BrowserSource, BrowserSourceBrowser } from "@hanzodev/contracts";
import { useGT, useMessages } from "gt-react";

import { useLiveRefresh } from "~/hooks/useLiveRefresh";
import { browserSourceEnvironment } from "~/state/browserSources";
import type { usePrimaryEnvironmentId } from "~/state/environments";
import { useEnvironmentQuery } from "~/state/query";
import { Badge } from "../ui/badge";
import { SettingsRow, SettingsSection } from "./settingsLayout";

const REFRESH_INTERVAL_MS = 15_000;

const BROWSER_LABELS: Readonly<Record<BrowserSourceBrowser, string>> = {
  firefox: "Firefox",
  chrome: "Chrome",
  safari: "Safari",
  other: "Browser",
};

/** HIP-1350 Browser sources: the Hanzo extension, on this computer, seen and never routed to. */
export function BrowserSourcesSection({
  environmentId,
}: {
  readonly environmentId: NonNullable<ReturnType<typeof usePrimaryEnvironmentId>>;
}) {
  const gt = useGT();
  const list = useEnvironmentQuery(
    browserSourceEnvironment.list({ environmentId, input: {} }),
    useMessages(),
  );
  useLiveRefresh(() => list.refresh(), {
    key: "linked-desktop:browser-sources",
    intervalMs: REFRESH_INTERVAL_MS,
    refreshOnShow: true,
  });
  const sources = list.data?.sources ?? null;
  return (
    <SettingsSection title={gt("Browsers on this computer")}>
      {list.error ? (
        <SettingsRow title={gt("Could not load your browsers")} description={list.error} />
      ) : sources === null ? (
        <SettingsRow title={gt("Loading…")} role="status" />
      ) : sources.length === 0 ? (
        <SettingsRow
          title={gt("No browser extension connected")}
          description={gt(
            "Install the Hanzo extension in a browser and sign in. It joins this computer by itself.",
          )}
        />
      ) : (
        sources.map((source) => <BrowserSourceRow key={source.id} source={source} />)
      )}
    </SettingsSection>
  );
}

function BrowserSourceRow({ source }: { readonly source: BrowserSource }) {
  const gt = useGT();
  const browser = BROWSER_LABELS[source.browser];
  return (
    <SettingsRow
      title={source.name}
      description={
        <>
          <span className="block">{gt("on {browser}", { browser })}</span>
          <span className="block">
            {source.providers.length === 0
              ? gt("No signed-in AI products reported.")
              : source.providers
                  .map(
                    (provider) =>
                      `${provider.provider}${provider.plan === undefined ? "" : ` · ${provider.plan}`} · ${
                        provider.state === "ready" ? gt("Ready") : gt("Needs login")
                      }`,
                  )
                  .join(", ")}
          </span>
          <span className="block">
            {gt("View only. Logins stay in the browser and are never routed to.")}
          </span>
        </>
      }
      control={
        <Badge variant="outline" size="sm">
          {gt("View only")}
        </Badge>
      }
    />
  );
}
