import { pluginAction, pluginSections } from "@hanzodev/client-runtime/composer-commands";
import {
  CloudChatError,
  listConnectors,
  listDesignSystems,
  listMemories,
  shelfDraftText,
  type CloudShelfRow,
} from "@hanzodev/client-runtime/cloud-chat";
import type { EnvironmentId, Plugin } from "@hanzodev/contracts";
import { useGT, useMessages } from "gt-react";
import { useEffect, useRef, useState } from "react";

import { getHanzoWebAuthController } from "../../cloud/hanzoWebAuth";
import { serverEnvironment } from "../../state/server";
import { useAtomCommand } from "../../state/use-atom-command";
import { Button } from "../ui/button";
import { Dialog, DialogHeader, DialogPanel, DialogPopup, DialogTitle } from "../ui/dialog";
import { Input } from "../ui/input";
import { Switch } from "../ui/switch";
import { composerFloatingLayerProps } from "./composerEventScope";

export type ComposerShelf = "plugins" | "connectors" | "design" | "memory";

const failureText = (cause: unknown, fallback: string): string =>
  cause instanceof CloudChatError && cause.kind === "signed-out"
    ? ""
    : cause instanceof Error
      ? cause.message
      : fallback;

function Row({
  title,
  note,
  children,
}: {
  title: string;
  note?: string | undefined;
  children?: React.ReactNode;
}) {
  return (
    <div className="flex items-center gap-3 rounded-lg px-2 py-2">
      <div className="min-w-0 flex-1">
        <div className="truncate text-sm font-medium text-foreground">{title}</div>
        {note ? <div className="truncate text-xs text-muted-foreground">{note}</div> : null}
      </div>
      {children}
    </div>
  );
}

/** `/plugins` and the `+` menu's Plugins: what is installed, what marketplaces offer, install and on/off. */
function PluginsShelf({ environmentId }: { readonly environmentId: EnvironmentId }) {
  const gt = useGT();
  const list = useAtomCommand(serverEnvironment.listPlugins, { reportFailure: false });
  const setEnabled = useAtomCommand(serverEnvironment.setPluginEnabled, { reportFailure: false });
  const install = useAtomCommand(serverEnvironment.installPlugin, { reportFailure: false });
  const [query, setQuery] = useState("");
  const [plugins, setPlugins] = useState<ReadonlyArray<Plugin>>([]);
  const [more, setMore] = useState(0);
  const [reading, setReading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState<string | null>(null);
  const generation = useRef(0);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- reads the plugin list from the machine as the query settles
  useEffect(() => {
    const mine = ++generation.current;
    // oxlint-disable-next-line react/set-state-in-effect -- the list is read from the machine, so the effect marks it reading first
    setReading(true);
    const timer = setTimeout(
      () => {
        void list({ environmentId, input: query.trim() ? { query: query.trim() } : {} }).then(
          (result) => {
            if (mine !== generation.current) return;
            setReading(false);
            if (result._tag === "Success") {
              setPlugins(result.value.plugins);
              setMore(result.value.more);
              setError(null);
            } else {
              setError(gt("Plugins could not be read. Is Claude Code installed on this machine?"));
            }
          },
        );
      },
      query ? 200 : 0,
    );
    return () => clearTimeout(timer);
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- `list` is a fresh function each render
  }, [environmentId, query]);

  const act = async (plugin: Plugin) => {
    const action = pluginAction(plugin);
    setBusy(plugin.id);
    const result =
      action === "install"
        ? await install({ environmentId, input: { id: plugin.id } })
        : await setEnabled({
            environmentId,
            input: { id: plugin.id, enabled: action === "enable" },
          });
    setBusy(null);
    if (result._tag === "Success") {
      setError(null);
      setPlugins((current) =>
        current.map((entry) => (entry.id === plugin.id ? result.value.plugin : entry)),
      );
    } else {
      setError(gt("{name} could not be changed.", { name: plugin.name }));
    }
  };

  const { installed, offered } = pluginSections(plugins);
  return (
    <>
      <div className="px-6 pb-3">
        <Input
          type="search"
          value={query}
          placeholder={gt("Search plugins")}
          aria-label={gt("Search plugins")}
          onChange={(event) => setQuery(event.currentTarget.value)}
        />
      </div>
      <DialogPanel>
        <div data-testid="plugins-list">
          {error ? (
            <p role="alert" className="px-2 pb-2 text-sm text-destructive-foreground">
              {error}
            </p>
          ) : null}
          {reading && plugins.length === 0 ? (
            <p className="px-2 text-sm text-muted-foreground">{gt("Reading plugins")}</p>
          ) : null}
          {!reading && !error && plugins.length === 0 ? (
            <p className="px-2 text-sm text-muted-foreground">{gt("No plugins match")}</p>
          ) : null}
          {installed.length > 0 ? (
            <h3 className="px-2 pb-1 text-xs font-medium text-muted-foreground">
              {gt("Installed")}
            </h3>
          ) : null}
          {installed.map((plugin) => (
            <Row
              key={plugin.id}
              title={plugin.name}
              note={[plugin.marketplace, plugin.version].filter(Boolean).join(" · ")}
            >
              <Switch
                checked={plugin.enabled}
                disabled={busy === plugin.id}
                aria-label={gt("Turn {name} on or off", { name: plugin.name })}
                onCheckedChange={() => void act(plugin)}
              />
            </Row>
          ))}
          {offered.length > 0 ? (
            <h3 className="px-2 pb-1 pt-3 text-xs font-medium text-muted-foreground">
              {gt("Available")}
            </h3>
          ) : null}
          {offered.map((plugin) => (
            <Row
              key={plugin.id}
              title={plugin.name}
              note={plugin.description ?? plugin.marketplace ?? undefined}
            >
              <Button
                size="compact"
                variant="outline"
                disabled={busy === plugin.id}
                onClick={() => void act(plugin)}
              >
                {busy === plugin.id ? gt("Installing") : gt("Install")}
              </Button>
            </Row>
          ))}
          {more > 0 ? (
            <p className="px-2 pt-2 text-xs text-muted-foreground">
              {gt("{count} more. Search to narrow.", { count: more })}
            </p>
          ) : null}
        </div>
      </DialogPanel>
    </>
  );
}

/** Connectors, design systems and memories: the cloud's lists, as hanzo.app reads them. */
function CloudShelf({
  shelf,
  onChoose,
}: {
  readonly shelf: Exclude<ComposerShelf, "plugins">;
  readonly onChoose: (text: string) => void;
}) {
  const gt = useGT();
  const m = useMessages();
  const [rows, setRows] = useState<ReadonlyArray<CloudShelfRow> | null>(null);
  const [error, setError] = useState<string | null>(null);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- reads the cloud list when the shelf opens
  useEffect(() => {
    let live = true;
    const auth = getHanzoWebAuthController();
    void (async () => {
      try {
        const credentials = { accessToken: await auth.getToken(), orgId: auth.activeOrgId };
        const read =
          shelf === "connectors"
            ? listConnectors
            : shelf === "design"
              ? listDesignSystems
              : listMemories;
        const next = await read(credentials);
        if (live) setRows(next);
      } catch (cause) {
        if (!live) return;
        setRows([]);
        setError(
          failureText(cause, gt("The list could not be read.")) ||
            gt("Sign in to Hanzo to use this."),
        );
      }
    })();
    return () => {
      live = false;
    };
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- `gt` and `m` only word the result
  }, [shelf]);

  const empty =
    shelf === "connectors"
      ? gt("No connectors yet")
      : shelf === "design"
        ? gt("No design systems yet")
        : gt("Nothing remembered yet");
  return (
    <DialogPanel>
      <div data-testid={`${shelf}-list`}>
        {error ? (
          <p role="alert" className="px-2 text-sm text-muted-foreground">
            {error}
          </p>
        ) : null}
        {rows === null ? (
          <p className="px-2 text-sm text-muted-foreground">{gt("Reading")}</p>
        ) : null}
        {rows !== null && rows.length === 0 && !error ? (
          <p className="px-2 text-sm text-muted-foreground">{empty}</p>
        ) : null}
        {(rows ?? []).map((row) => (
          <button
            key={row.id}
            type="button"
            className="block w-full rounded-lg text-start hover:bg-accent focus-visible:bg-accent"
            onClick={() => onChoose(shelfDraftText(shelf, row, m))}
          >
            <Row title={row.name} {...(row.note ? { note: row.note } : {})} />
          </button>
        ))}
      </div>
    </DialogPanel>
  );
}

export function ComposerShelfDialog({
  shelf,
  environmentId,
  onClose,
  onInsertText,
}: {
  readonly shelf: ComposerShelf | null;
  readonly environmentId: EnvironmentId;
  readonly onClose: () => void;
  readonly onInsertText: (text: string) => void;
}) {
  const gt = useGT();
  const title =
    shelf === "plugins"
      ? gt("Plugins")
      : shelf === "connectors"
        ? gt("Connectors")
        : shelf === "design"
          ? gt("Design system")
          : gt("Memory");
  return (
    <Dialog open={shelf !== null} onOpenChange={(open) => !open && onClose()}>
      <DialogPopup
        {...composerFloatingLayerProps}
        className="max-w-lg"
        data-testid="composer-shelf"
      >
        <DialogHeader>
          <DialogTitle>{title}</DialogTitle>
        </DialogHeader>
        {shelf === "plugins" ? (
          <PluginsShelf environmentId={environmentId} />
        ) : shelf !== null ? (
          <CloudShelf
            key={shelf}
            shelf={shelf}
            onChoose={(text) => {
              onInsertText(text);
              onClose();
            }}
          />
        ) : null}
      </DialogPopup>
    </Dialog>
  );
}
