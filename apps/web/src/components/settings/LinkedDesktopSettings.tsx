import { useAtomValue } from "@effect/atom-react";
import { thrownErrorMessage } from "@hanzodev/client-runtime/errors";
import {
  type AtomCommandResult,
  isAtomCommandInterrupted,
  squashAtomCommandFailure,
} from "@hanzodev/client-runtime/state/runtime";
import type { Desktop, DesktopLinkStatus, DesktopProviderState } from "@hanzodev/contracts";
import { useGT, useMessages } from "gt-react";
import { useState } from "react";

import { useAuth, useHanzoSession, useHanzoWebAccounts } from "~/cloud/hanzoWebAuth";
import { requestConfirmDialog } from "~/confirmDialog";
import { useLiveRefresh } from "~/hooks/useLiveRefresh";
import { useNowMinute } from "~/hooks/useNowMinute";
import { desktopEnvironment } from "~/state/desktops";
import { usePrimaryEnvironmentId } from "~/state/environments";
import { useEnvironmentQuery } from "~/state/query";
import { useAtomCommand } from "~/state/use-atom-command";
import { formatRelativeTimeLabel } from "~/timestampFormat";
import { ConnectionStatusDot } from "../ConnectionStatusDot";
import { Badge } from "../ui/badge";
import { Button } from "../ui/button";
import { Input } from "../ui/input";
import { stackedThreadToast, toastManager } from "../ui/toast";
import { ITEM_ROW_CLASSNAME, ITEM_ROW_INNER_CLASSNAME } from "./itemRows";
import { SettingsPageContainer, SettingsRow, SettingsSection } from "./settingsLayout";

const REFRESH_INTERVAL_MS = 15_000;

function OnlineDot({ online, label }: { readonly online: boolean; readonly label: string }) {
  return (
    <ConnectionStatusDot
      tooltipText={label}
      dotClassName={online ? "bg-success" : "bg-muted-foreground/30"}
      pingClassName={null}
    />
  );
}

export function LinkedDesktopSettings() {
  const gt = useGT();
  const environmentId = usePrimaryEnvironmentId();
  const status = useEnvironmentQuery(
    environmentId === null ? null : desktopEnvironment.status({ environmentId, input: {} }),
    useMessages(),
  );
  useLiveRefresh(() => status.refresh(), {
    key: "linked-desktop:status",
    intervalMs: REFRESH_INTERVAL_MS,
    refreshOnShow: true,
  });
  const data = status.data;
  return (
    <SettingsPageContainer>
      <SettingsSection title={gt("Linked to Hanzo")}>
        {environmentId === null ? (
          <SettingsRow title={gt("No environment is connected.")} role="status" />
        ) : data === null ? (
          status.error ? (
            <SettingsRow title={gt("Could not load the link")} description={status.error} />
          ) : (
            <SettingsRow title={gt("Loading…")} role="status" />
          )
        ) : (
          <StatusRow environmentId={environmentId} status={data} />
        )}
      </SettingsSection>
      {environmentId !== null && data !== null && data.desktop !== null ? (
        <>
          <DesktopsSection environmentId={environmentId} thisDesktopId={data.desktop.id} />
          <ProvidersSection providers={data.providers} />
        </>
      ) : null}
      {data !== null ? <ReportedSection status={data} /> : null}
    </SettingsPageContainer>
  );
}

type EnvironmentIdProp = NonNullable<ReturnType<typeof usePrimaryEnvironmentId>>;

function useFailureToast() {
  const gt = useGT();
  const messages = useMessages();
  return (title: string, cause: unknown) =>
    toastManager.add(
      stackedThreadToast({
        type: "error",
        title,
        description: thrownErrorMessage(cause, messages) ?? gt("Something went wrong."),
      }),
    );
}

function StatusRow({
  environmentId,
  status,
}: {
  readonly environmentId: EnvironmentIdProp;
  readonly status: DesktopLinkStatus;
}) {
  const gt = useGT();
  const reportFailure = useFailureToast();
  const nowMinute = useNowMinute();
  const link = useAtomCommand(desktopEnvironment.link, { reportFailure: false });
  const unlink = useAtomCommand(desktopEnvironment.unlink, { reportFailure: false });
  const rename = useAtomCommand(desktopEnvironment.rename, { reportFailure: false });
  const canLink = useAtomValue(desktopEnvironment.link.permissionAtom(environmentId));
  const canUnlink = useAtomValue(desktopEnvironment.unlink.permissionAtom(environmentId));
  const canRename = useAtomValue(desktopEnvironment.rename.permissionAtom(environmentId));
  const { activeAccount } = useHanzoWebAccounts();
  const { getToken } = useAuth();
  const { openSignIn } = useHanzoSession();
  const [busy, setBusy] = useState(false);
  const [draftName, setDraftName] = useState<string | null>(null);
  const desktop = status.desktop;

  const settle = async (
    run: () => Promise<AtomCommandResult<unknown, unknown>>,
    failureTitle: string,
  ): Promise<boolean> => {
    setBusy(true);
    const result = await run();
    setBusy(false);
    if (result._tag === "Failure" && !isAtomCommandInterrupted(result)) {
      reportFailure(failureTitle, squashAtomCommandFailure(result));
      return false;
    }
    return result._tag === "Success";
  };

  const startLink = async () => {
    if (status.signedIn) {
      await settle(() => link({ environmentId, input: {} }), gt("Could not link this desktop"));
      return;
    }
    const accessToken = activeAccount === null ? null : await getToken();
    if (activeAccount === null || accessToken === null) {
      openSignIn();
      return;
    }
    await settle(
      () =>
        link({
          environmentId,
          input: {
            token: {
              accessToken,
              refreshToken: activeAccount.refreshToken ?? "",
              expiresAtEpochMs: activeAccount.expiresAt ?? Number.MAX_SAFE_INTEGER,
            },
          },
        }),
      gt("Could not link this desktop"),
    );
  };

  const startUnlink = async () => {
    const confirmed = await (requestConfirmDialog(
      gt(
        "Unlink this desktop from Hanzo? It stops reporting and disappears from your linked desktops.",
      ),
      { variant: "destructive" },
    ) ?? Promise.resolve(false));
    if (!confirmed) return;
    await settle(() => unlink({ environmentId, input: {} }), gt("Could not unlink this desktop"));
  };

  const saveName = async () => {
    if (draftName === null) return;
    const saved = await settle(
      () => rename({ environmentId, input: { name: draftName } }),
      gt("Could not rename this desktop"),
    );
    if (saved) setDraftName(null);
  };

  if (desktop === null) {
    return (
      <SettingsRow
        title={gt("Not linked")}
        description={gt(
          "Link this desktop to your Hanzo account to see it, and the providers it can reach, on hanzo.app.",
        )}
        control={
          <Button size="xs" disabled={busy || !canLink} onClick={() => void startLink()}>
            {busy ? gt("Linking…") : gt("Link to Hanzo")}
          </Button>
        }
      />
    );
  }

  const online = desktop.state === "online";
  const name = draftName ?? desktop.name;
  return (
    <>
      <SettingsRow
        title={
          status.identity === null
            ? gt("Linked")
            : status.organization === null
              ? gt("Linked as {identity}", { identity: status.identity })
              : gt("Linked as {identity} · org {organization}", {
                  identity: status.identity,
                  organization: status.organization,
                })
        }
        description={
          status.lastHeartbeatAt === null
            ? gt("No heartbeat yet.")
            : gt("Last heartbeat {when}", {
                when: formatRelativeTimeLabel(status.lastHeartbeatAt, Date.parse(`${nowMinute}Z`)),
              })
        }
        status={
          <span className="inline-flex items-center gap-1.5">
            <OnlineDot online={online} label={online ? gt("Online") : gt("Offline")} />
            {online ? gt("Online") : gt("Offline")}
          </span>
        }
        control={
          <Button
            size="xs"
            variant="destructive-outline"
            disabled={busy || !canUnlink}
            onClick={() => void startUnlink()}
          >
            {gt("Unlink this desktop")}
          </Button>
        }
      />
      <SettingsRow
        title={gt("This desktop's name")}
        control={
          <form
            className="flex items-center gap-2"
            onSubmit={(event) => {
              event.preventDefault();
              void saveName();
            }}
          >
            <Input
              aria-label={gt("This desktop's name")}
              value={name}
              disabled={busy || !canRename}
              onChange={(event) => setDraftName(event.target.value)}
            />
            {draftName !== null ? (
              <>
                <Button
                  size="xs"
                  type="submit"
                  disabled={busy || draftName.trim().length === 0 || draftName === desktop.name}
                >
                  {gt("Save")}
                </Button>
                <Button
                  size="xs"
                  variant="outline"
                  type="button"
                  onClick={() => setDraftName(null)}
                >
                  {gt("Cancel")}
                </Button>
              </>
            ) : null}
          </form>
        }
      />
    </>
  );
}

function DesktopsSection({
  environmentId,
  thisDesktopId,
}: {
  readonly environmentId: EnvironmentIdProp;
  readonly thisDesktopId: string;
}) {
  const gt = useGT();
  const list = useEnvironmentQuery(
    desktopEnvironment.list({ environmentId, input: {} }),
    useMessages(),
  );
  return (
    <SettingsSection title={gt("Your linked sources")}>
      {list.error ? (
        <SettingsRow title={gt("Could not load your linked sources")} description={list.error} />
      ) : list.data === null ? (
        <SettingsRow title={gt("Loading…")} role="status" />
      ) : list.data.desktops.length === 0 ? (
        <SettingsRow title={gt("No linked sources")} />
      ) : (
        list.data.desktops.map((desktop) => (
          <DesktopRow
            key={desktop.id}
            environmentId={environmentId}
            desktop={desktop}
            isThisDesktop={desktop.id === thisDesktopId}
          />
        ))
      )}
    </SettingsSection>
  );
}

function DesktopRow({
  environmentId,
  desktop,
  isThisDesktop,
}: {
  readonly environmentId: EnvironmentIdProp;
  readonly desktop: Desktop;
  readonly isThisDesktop: boolean;
}) {
  const gt = useGT();
  const reportFailure = useFailureToast();
  const nowMinute = useNowMinute();
  const revoke = useAtomCommand(desktopEnvironment.revoke, { reportFailure: false });
  const canRevoke = useAtomValue(desktopEnvironment.revoke.permissionAtom(environmentId));
  const [busy, setBusy] = useState(false);
  const online = desktop.state === "online";
  const startRevoke = async () => {
    const confirmed = await (requestConfirmDialog(
      gt("Revoke {name}? It stops being linked and must be linked again to come back.", {
        name: desktop.name,
      }),
      { variant: "destructive" },
    ) ?? Promise.resolve(false));
    if (!confirmed) return;
    setBusy(true);
    const result = await revoke({ environmentId, input: { id: desktop.id } });
    setBusy(false);
    if (result._tag === "Failure" && !isAtomCommandInterrupted(result)) {
      reportFailure(gt("Could not revoke the desktop"), squashAtomCommandFailure(result));
    }
  };
  return (
    <div className={ITEM_ROW_CLASSNAME}>
      <div className={ITEM_ROW_INNER_CLASSNAME}>
        <div className="min-w-0 flex-1 space-y-1">
          <div className="flex min-h-5 items-center gap-1.5">
            <OnlineDot online={online} label={online ? gt("Online") : gt("Offline")} />
            <h3 className="text-sm font-medium text-foreground">{desktop.name}</h3>
            <Badge variant="outline" size="sm">
              {desktop.kind === "browser" ? gt("Browser") : gt("Desktop")}
            </Badge>
            {isThisDesktop ? (
              <Badge variant="outline" size="sm">
                {gt("This desktop")}
              </Badge>
            ) : null}
          </div>
          <p className="text-xs text-muted-foreground">
            {online ? gt("Online") : gt("Offline")}
            {desktop.lastSeen === null
              ? ""
              : ` · ${gt("Last seen {when}", {
                  when: formatRelativeTimeLabel(desktop.lastSeen, Date.parse(`${nowMinute}Z`)),
                })}`}
          </p>
        </div>
        {canRevoke ? (
          <Button
            size="xs"
            variant="destructive-outline"
            disabled={busy}
            onClick={() => void startRevoke()}
          >
            {gt("Revoke")}
          </Button>
        ) : null}
      </div>
    </div>
  );
}

function ProvidersSection({
  providers,
}: {
  readonly providers: ReadonlyArray<DesktopProviderState>;
}) {
  const gt = useGT();
  const stateLabel = (state: DesktopProviderState["state"]) =>
    state === "ready"
      ? gt("Ready")
      : state === "needs_login"
        ? gt("Needs login")
        : gt("Limit reached");
  return (
    <SettingsSection title={gt("Providers on this desktop")}>
      {providers.length === 0 ? (
        <SettingsRow title={gt("No providers to report")} />
      ) : (
        providers.map((provider) => (
          <SettingsRow
            key={provider.instance}
            title={provider.instance}
            description={
              provider.plan === undefined
                ? stateLabel(provider.state)
                : `${provider.plan} · ${stateLabel(provider.state)}`
            }
            control={
              <Badge variant="outline" size="sm">
                {provider.kind === "key" ? gt("API key") : gt("This desktop only")}
              </Badge>
            }
          />
        ))
      )}
    </SettingsSection>
  );
}

function ReportedSection({ status }: { readonly status: DesktopLinkStatus }) {
  const gt = useGT();
  return (
    <SettingsSection title={gt("What is reported")}>
      <SettingsRow
        title={gt("Level")}
        description={gt(
          "Heartbeats carry provider state only: no prompts, files, tokens or emails.",
        )}
        control={
          <Badge variant="outline" size="sm">
            {status.level === "metadata" ? gt("Metadata") : gt("Metadata and content")}
          </Badge>
        }
      />
      <SettingsRow
        title={status.report.sent ? gt("Last heartbeat sent") : gt("Next heartbeat")}
        description={
          status.report.sent
            ? gt("The exact bytes sent to Hanzo.")
            : gt("Sent once this desktop is linked.")
        }
      >
        <pre
          aria-label={gt("Heartbeat body")}
          className="mt-2 max-h-72 overflow-auto rounded-md bg-muted/40 p-3 font-mono text-xs"
        >
          {status.report.body}
        </pre>
      </SettingsRow>
    </SettingsSection>
  );
}
