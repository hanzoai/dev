import { useAtomValue } from "@effect/atom-react";
import { thrownErrorMessage } from "@hanzodev/client-runtime/errors";
import {
  type AtomCommandResult,
  isAtomCommandInterrupted,
  squashAtomCommandFailure,
} from "@hanzodev/client-runtime/state/runtime";
import {
  API_KEY_PROVIDERS,
  type Desktop,
  type DesktopLinkStatus,
  type DesktopOutbox,
  type DesktopProviderState,
  type ProviderAccount,
  type ProviderRouteClass,
} from "@hanzodev/contracts";
import { useGT, useMessages } from "gt-react";
import { useState } from "react";

import { useAuth, useHanzoWebAccounts } from "~/cloud/hanzoWebAuth";
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
import { Radio, RadioGroup } from "../ui/radio-group";
import { Select, SelectItem, SelectPopup, SelectTrigger, SelectValue } from "../ui/select";
import { Switch } from "../ui/switch";
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
          <StatusRow environmentId={environmentId} status={data} onRefresh={status.refresh} />
        )}
      </SettingsSection>
      {environmentId !== null && data !== null && data.desktop !== null ? (
        <>
          <DesktopsSection environmentId={environmentId} thisDesktopId={data.desktop.id} />
          <ProvidersSection providers={data.providers} />
        </>
      ) : null}
      {environmentId !== null && data !== null && data.signedIn ? (
        <ApiKeysSection environmentId={environmentId} />
      ) : null}
      {environmentId !== null && data !== null ? (
        <ReportedSection environmentId={environmentId} status={data} />
      ) : null}
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
  onRefresh,
}: {
  readonly environmentId: EnvironmentIdProp;
  readonly status: DesktopLinkStatus;
  readonly onRefresh: () => void;
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
      // No sign-in anywhere: this server opens the browser at Hanzo's authorization endpoint.
      await settle(
        () => link({ environmentId, input: { signIn: "browser" } }),
        gt("Could not link this desktop"),
      );
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

  const startDeviceLink = async () => {
    // The code appears in the status once the server has asked Hanzo for it.
    setTimeout(onRefresh, 2_000);
    await settle(
      () => link({ environmentId, input: { signIn: "device" } }),
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
          <>
            {status.deviceCode === null ? null : (
              <span className="text-sm">
                {gt("Enter {code} at {uri}", {
                  code: status.deviceCode.userCode,
                  uri: status.deviceCode.verificationUri,
                })}
              </span>
            )}
            {status.signedIn ? null : (
              <Button
                size="xs"
                variant="outline"
                disabled={busy || !canLink}
                onClick={() => void startDeviceLink()}
              >
                {gt("Use a code instead")}
              </Button>
            )}
            <Button size="xs" disabled={busy || !canLink} onClick={() => void startLink()}>
              {busy ? gt("Linking…") : gt("Link to Hanzo")}
            </Button>
          </>
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
                {routeLabel(gt, providerRouteClass(provider))}
              </Badge>
            }
          />
        ))
      )}
    </SettingsSection>
  );
}

// HIP-1350 Provider policy: a subscription login is `local`, a key is `key`. The `desktop`
// class stays empty until a provider's terms allow it.
const providerRouteClass = (provider: DesktopProviderState): ProviderRouteClass =>
  provider.kind === "key" ? "key" : "local";

const routeLabel = (gt: ReturnType<typeof useGT>, route: ProviderRouteClass) =>
  route === "local"
    ? gt("This desktop only")
    : route === "desktop"
      ? gt("Available on hanzo.app")
      : gt("API key");

const PROVIDER_LABELS: Readonly<Record<(typeof API_KEY_PROVIDERS)[number], string>> = {
  anthropic: "Anthropic",
  openai: "OpenAI",
  gemini: "Gemini",
  xai: "xAI",
  openrouter: "OpenRouter",
  opencode: "OpenCode",
  kiro: "Kiro",
};

function ApiKeysSection({ environmentId }: { readonly environmentId: EnvironmentIdProp }) {
  const gt = useGT();
  const reportFailure = useFailureToast();
  const accounts = useEnvironmentQuery(
    desktopEnvironment.accounts({ environmentId, input: {} }),
    useMessages(),
  );
  const add = useAtomCommand(desktopEnvironment.addAccount, { reportFailure: false });
  const remove = useAtomCommand(desktopEnvironment.removeAccount, { reportFailure: false });
  const canAdd = useAtomValue(desktopEnvironment.addAccount.permissionAtom(environmentId));
  const [provider, setProvider] = useState<(typeof API_KEY_PROVIDERS)[number]>("anthropic");
  const [label, setLabel] = useState("");
  const [value, setValue] = useState("");
  const [busy, setBusy] = useState(false);

  const submit = async () => {
    const entered = { provider, label: label.trim(), value: value.trim() };
    // The key leaves this field the moment it is sent, and is kept nowhere on this device.
    setValue("");
    setBusy(true);
    const result = await add({ environmentId, input: entered });
    setBusy(false);
    if (result._tag === "Failure" && !isAtomCommandInterrupted(result)) {
      reportFailure(gt("Could not add the key"), squashAtomCommandFailure(result));
      return;
    }
    setLabel("");
  };

  const startRemove = async (account: ProviderAccount) => {
    const confirmed = await (requestConfirmDialog(
      gt("Delete the key {label}? It is removed from Hanzo KMS.", { label: account.label }),
      { variant: "destructive" },
    ) ?? Promise.resolve(false));
    if (!confirmed) return;
    const result = await remove({ environmentId, input: { id: account.id } });
    if (result._tag === "Failure" && !isAtomCommandInterrupted(result)) {
      reportFailure(gt("Could not delete the key"), squashAtomCommandFailure(result));
    }
  };

  return (
    <SettingsSection title={gt("API keys")}>
      <SettingsRow
        title={gt("Add a key")}
        description={gt("The key goes to Hanzo KMS once. This desktop does not keep it.")}
        control={
          <form
            className="flex flex-wrap items-center justify-end gap-2"
            onSubmit={(event) => {
              event.preventDefault();
              void submit();
            }}
          >
            <Select
              value={provider}
              onValueChange={(next) => {
                if (API_KEY_PROVIDERS.includes(next as (typeof API_KEY_PROVIDERS)[number])) {
                  setProvider(next as (typeof API_KEY_PROVIDERS)[number]);
                }
              }}
            >
              <SelectTrigger size="sm" aria-label={gt("Provider")}>
                <SelectValue>
                  {(selected: (typeof API_KEY_PROVIDERS)[number] | null) =>
                    selected === null ? "" : PROVIDER_LABELS[selected]
                  }
                </SelectValue>
              </SelectTrigger>
              <SelectPopup align="end" alignItemWithTrigger={false}>
                {API_KEY_PROVIDERS.map((id) => (
                  <SelectItem key={id} value={id}>
                    {PROVIDER_LABELS[id]}
                  </SelectItem>
                ))}
              </SelectPopup>
            </Select>
            <Input
              aria-label={gt("Label")}
              placeholder={gt("Label")}
              value={label}
              disabled={busy || !canAdd}
              onChange={(event) => setLabel(event.target.value)}
            />
            <Input
              aria-label={gt("API key")}
              placeholder={gt("API key")}
              type="password"
              autoComplete="off"
              value={value}
              disabled={busy || !canAdd}
              onChange={(event) => setValue(event.target.value)}
            />
            <Button
              size="xs"
              type="submit"
              disabled={busy || !canAdd || label.trim().length === 0 || value.trim().length === 0}
            >
              {busy ? gt("Adding…") : gt("Add key")}
            </Button>
          </form>
        }
      />
      {accounts.error ? (
        <SettingsRow title={gt("Could not load your keys")} description={accounts.error} />
      ) : accounts.data === null ? (
        <SettingsRow title={gt("Loading…")} role="status" />
      ) : accounts.data.accounts.length === 0 ? (
        <SettingsRow title={gt("No keys yet")} />
      ) : (
        accounts.data.accounts.map((account) => (
          <SettingsRow
            key={account.id}
            title={account.label}
            description={`${PROVIDER_LABELS[account.provider as keyof typeof PROVIDER_LABELS] ?? account.provider}${account.fingerprint === "" ? "" : ` · …${account.fingerprint.slice(-4)}`}`}
            control={
              <>
                <Badge variant="outline" size="sm">
                  {routeLabel(gt, account.route)}
                </Badge>
                <Button
                  size="xs"
                  variant="destructive-outline"
                  disabled={!canAdd}
                  onClick={() => void startRemove(account)}
                >
                  {gt("Delete")}
                </Button>
              </>
            }
          />
        ))
      )}
    </SettingsSection>
  );
}

function ReportedSection({
  environmentId,
  status,
}: {
  readonly environmentId: EnvironmentIdProp;
  readonly status: DesktopLinkStatus;
}) {
  const gt = useGT();
  const reportFailure = useFailureToast();
  const outbox = useEnvironmentQuery(
    desktopEnvironment.outbox({ environmentId, input: {} }),
    useMessages(),
  );
  const setSendNothing = useAtomCommand(desktopEnvironment.setSendNothing, {
    reportFailure: false,
  });
  const canChange = useAtomValue(desktopEnvironment.setSendNothing.permissionAtom(environmentId));
  const sendNothing = outbox.data?.sendNothing ?? false;
  const toggle = async (next: boolean) => {
    const result = await setSendNothing({ environmentId, input: { sendNothing: next } });
    if (result._tag === "Failure" && !isAtomCommandInterrupted(result)) {
      reportFailure(gt("Could not change what is sent"), squashAtomCommandFailure(result));
    }
  };
  return (
    <SettingsSection title={gt("What is reported")}>
      <SettingsRow
        title={gt("Level")}
        description={gt(
          "Heartbeats and usage reports carry provider state and token counts only: no prompts, files, tokens or emails.",
        )}
        control={
          <RadioGroup
            aria-label={gt("Level")}
            value={status.level}
            className="flex-row items-center"
          >
            <label className="mr-4 flex items-center gap-2 text-sm">
              <Radio value="metadata" aria-label={gt("Metadata")} />
              {gt("Metadata")}
            </label>
            <label className="flex items-center gap-2 text-sm text-muted-foreground">
              <Radio value="metadata+content" disabled aria-label={gt("Metadata and content")} />
              {gt("Metadata and content")} · {gt("not available yet")}
            </label>
          </RadioGroup>
        }
      />
      <SettingsRow
        title={gt("Send nothing")}
        description={gt(
          "Stop sending usage reports. Heartbeats still tell Hanzo this desktop is online.",
        )}
        control={
          <Switch
            aria-label={gt("Send nothing")}
            checked={sendNothing}
            disabled={!canChange}
            onCheckedChange={(next) => void toggle(next)}
          />
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
      <OutboxRows outbox={outbox.data} />
    </SettingsSection>
  );
}

function OutboxRows({ outbox }: { readonly outbox: DesktopOutbox | null }) {
  const gt = useGT();
  if (outbox === null) return <SettingsRow title={gt("Loading…")} role="status" />;
  if (outbox.batches.length === 0) {
    return (
      <SettingsRow
        title={gt("Usage reports")}
        description={gt("No usage report has been written yet.")}
      />
    );
  }
  const stateLabel = (state: DesktopOutbox["batches"][number]["state"]) =>
    state === "sent"
      ? gt("Sent")
      : state === "pending"
        ? gt("Pending")
        : state === "rejected"
          ? gt("Refused")
          : gt("Replaced by a newer report");
  return (
    <SettingsRow
      title={gt("Usage reports")}
      description={gt(
        "The last 100 reports, as written before sending. These are the bytes that left.",
      )}
    >
      <div className="mt-2 space-y-2">
        {outbox.batches.map((batch) => (
          <details key={batch.seq} className="rounded-md bg-muted/40 p-2 text-xs">
            <summary className="cursor-pointer">
              {batch.at} · {stateLabel(batch.state)}
            </summary>
            <pre className="mt-2 max-h-72 overflow-auto font-mono">{batch.body}</pre>
          </details>
        ))}
      </div>
    </SettingsRow>
  );
}
