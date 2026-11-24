import {
  listConnectors,
  listDesignSystems,
  shelfDraftText,
  type CloudShelfRow,
} from "@hanzodev/client-runtime/cloud-chat";
import { pluginAction, pluginSections } from "@hanzodev/client-runtime/composer-commands";
import {
  plusEntries,
  type PlusEntry,
  type PlusEntryId,
} from "@hanzodev/client-runtime/composer-plus";
import type { EnvironmentId, Plugin } from "@hanzodev/contracts";
import { msg, useGT, useMessages } from "gt-react-native";
import { useEffect, useMemo, useState } from "react";
import { Modal, Pressable, ScrollView, Switch, TextInput, View } from "react-native";

import { AppText as Text } from "../../components/ui/AppText";
import { GlassSurface } from "../../components/ui/GlassSurface";
import { SymbolView } from "../../components/ui/AppSymbol";
import { serverEnvironment } from "../../state/server";
import { toggleThreadMemoryEnabled, useThreadMemoryEnabled } from "../../state/memory-switch";
import { useAtomCommand } from "../../state/use-atom-command";
import { hanzoMobileAuthStore } from "../cloud/hanzoMobileAuth";

export type PlusPage = "main" | "connectors" | "design" | "plugins";

const SYMBOLS: Record<PlusEntryId, string> = {
  upload: "paperclip",
  library: "folder",
  screenshot: "camera",
  createImage: "photo.artframe",
  browserTabs: "macwindow",
  webSearch: "globe",
  research: "binoculars",
  sketch: "pencil.tip",
  thinking: "brain",
  agents: "person.2",
  skills: "scroll",
  connectors: "puzzlepiece.extension",
  design: "paintpalette",
  plugins: "powerplug",
  devices: "desktopcomputer",
  memory: "brain.head.profile",
};

const CLOUD = {
  connectors: listConnectors,
  design: listDesignSystems,
} as const;
const BACK = msg("Back");
const PHOTO = msg("Take a photo");
const PHOTO_NOTE = msg("Use the camera");

function Row(props: {
  readonly symbol: string;
  readonly title: string;
  readonly note?: string;
  readonly onPress: () => void;
  readonly disabled?: boolean;
}) {
  return (
    <Pressable
      accessibilityRole="button"
      accessibilityLabel={props.title}
      disabled={props.disabled}
      onPress={props.onPress}
      className="min-h-[48px] flex-row items-center gap-3 rounded-2xl px-3 py-2 active:opacity-70"
    >
      <SymbolView
        name={props.symbol as never}
        size={20}
        weight="regular"
        tintColorClassName="accent-icon"
        type="monochrome"
      />
      <View className="min-w-0 flex-1">
        <Text textRole="body" numberOfLines={1}>
          {props.title}
        </Text>
        {props.note ? (
          <Text textRole="caption" className="text-foreground-muted" numberOfLines={1}>
            {props.note}
          </Text>
        ) : null}
      </View>
    </Pressable>
  );
}

function CloudPage(props: {
  readonly shelf: "connectors" | "design";
  readonly onChoose: (text: string) => void;
}) {
  const gt = useGT();
  const m = useMessages();
  const [rows, setRows] = useState<ReadonlyArray<CloudShelfRow> | null>(null);
  const [error, setError] = useState<string | null>(null);
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- reads the cloud list when the page opens
  useEffect(() => {
    let live = true;
    void (async () => {
      try {
        const next = await CLOUD[props.shelf]({
          accessToken: await hanzoMobileAuthStore.getToken(),
          orgId: hanzoMobileAuthStore.getSnapshot().activeOrgId,
        });
        if (live) setRows(next);
      } catch {
        if (!live) return;
        setRows([]);
        setError(gt("Sign in to Hanzo to use this."));
      }
    })();
    return () => {
      live = false;
    };
  }, [props.shelf, gt]);
  return (
    <>
      {rows === null ? (
        <Text textRole="footnote" className="px-3">
          {gt("Reading")}
        </Text>
      ) : null}
      {error ? (
        <Text textRole="footnote" className="px-3">
          {error}
        </Text>
      ) : null}
      {rows?.length === 0 && !error ? (
        <Text textRole="footnote" className="px-3">
          {gt("Nothing here yet")}
        </Text>
      ) : null}
      {(rows ?? []).map((row) => (
        <Row
          key={row.id}
          symbol={SYMBOLS[props.shelf]}
          title={row.name}
          {...(row.note ? { note: row.note } : {})}
          onPress={() => props.onChoose(shelfDraftText(props.shelf, row, m))}
        />
      ))}
    </>
  );
}

/** `/plugins` and the `+` menu's Plugins: what is installed, what marketplaces offer, install and on/off. */
function PluginsPage(props: { readonly environmentId: EnvironmentId }) {
  const gt = useGT();
  const list = useAtomCommand(serverEnvironment.listPlugins, { reportFailure: false });
  const setEnabled = useAtomCommand(serverEnvironment.setPluginEnabled, { reportFailure: false });
  const install = useAtomCommand(serverEnvironment.installPlugin, { reportFailure: false });
  const [query, setQuery] = useState("");
  const [plugins, setPlugins] = useState<ReadonlyArray<Plugin>>([]);
  const [reading, setReading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState<string | null>(null);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- reads the plugin list from the machine as the query settles
  useEffect(() => {
    let live = true;
    // oxlint-disable-next-line react/set-state-in-effect -- the list is read from the machine, so the effect marks it reading first
    setReading(true);
    const timer = setTimeout(
      () => {
        void list({
          environmentId: props.environmentId,
          input: query.trim() ? { query: query.trim() } : {},
        }).then((result) => {
          if (!live) return;
          setReading(false);
          if (result._tag === "Success") {
            setPlugins(result.value.plugins);
            setError(null);
          } else
            setError(gt("Plugins could not be read. Is Claude Code installed on this machine?"));
        });
      },
      query ? 200 : 0,
    );
    return () => {
      live = false;
      clearTimeout(timer);
    };
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- `list` is a fresh function each render
  }, [props.environmentId, query]);

  const act = async (plugin: Plugin) => {
    const action = pluginAction(plugin);
    setBusy(plugin.id);
    const result =
      action === "install"
        ? await install({ environmentId: props.environmentId, input: { id: plugin.id } })
        : await setEnabled({
            environmentId: props.environmentId,
            input: { id: plugin.id, enabled: action === "enable" },
          });
    setBusy(null);
    if (result._tag === "Success") {
      setError(null);
      setPlugins((current) =>
        current.map((entry) => (entry.id === plugin.id ? result.value.plugin : entry)),
      );
    } else setError(gt("{name} could not be changed.", { name: plugin.name }));
  };

  const { installed, offered } = pluginSections(plugins);
  return (
    <>
      <TextInput
        value={query}
        onChangeText={setQuery}
        placeholder={gt("Search plugins")}
        accessibilityLabel={gt("Search plugins")}
        autoCapitalize="none"
        autoCorrect={false}
        className="mx-3 h-10 rounded-xl bg-subtle px-3 text-foreground"
      />
      {error ? (
        <Text textRole="footnote" className="px-3">
          {error}
        </Text>
      ) : null}
      {reading && plugins.length === 0 ? (
        <Text textRole="footnote" className="px-3">
          {gt("Reading plugins")}
        </Text>
      ) : null}
      {installed.map((plugin) => (
        <View key={plugin.id} className="min-h-[48px] flex-row items-center gap-3 px-3">
          <View className="min-w-0 flex-1">
            <Text textRole="body" numberOfLines={1}>
              {plugin.name}
            </Text>
            <Text textRole="caption" className="text-foreground-muted" numberOfLines={1}>
              {[plugin.marketplace, plugin.version].filter(Boolean).join(" · ")}
            </Text>
          </View>
          <Switch
            value={plugin.enabled}
            disabled={busy === plugin.id}
            accessibilityLabel={gt("Turn {name} on or off", { name: plugin.name })}
            onValueChange={() => void act(plugin)}
          />
        </View>
      ))}
      {offered.map((plugin) => (
        <View key={plugin.id} className="min-h-[48px] flex-row items-center gap-3 px-3">
          <View className="min-w-0 flex-1">
            <Text textRole="body" numberOfLines={1}>
              {plugin.name}
            </Text>
            <Text textRole="caption" className="text-foreground-muted" numberOfLines={1}>
              {plugin.description ?? plugin.marketplace ?? ""}
            </Text>
          </View>
          <Pressable
            accessibilityRole="button"
            disabled={busy === plugin.id}
            onPress={() => void act(plugin)}
            className="rounded-full bg-subtle px-3 py-1.5 active:opacity-70"
          >
            <Text textRole="label">{busy === plugin.id ? gt("Installing") : gt("Install")}</Text>
          </Pressable>
        </View>
      ))}
    </>
  );
}

/** The `+` menu on a phone: every entry of the shared list, in a Liquid Glass sheet. */
export function ComposerPlusSheet(props: {
  readonly visible: boolean;
  readonly page: PlusPage;
  readonly onPageChange: (page: PlusPage) => void;
  readonly onClose: () => void;
  readonly environmentId: EnvironmentId | null;
  readonly memoryKey: string;
  readonly supportsFiles: boolean;
  /** False where there is no thread to show a browser beside (a draft). */
  readonly offersBrowser: boolean;
  readonly onPickMedia: () => void;
  readonly onPickFiles: () => void;
  readonly onCapture: () => void;
  readonly onSketch: () => void;
  readonly onBrowser: () => void;
  readonly onInsertText: (text: string) => void;
}) {
  const gt = useGT();
  const m = useMessages();
  const memoryEnabled = useThreadMemoryEnabled(props.memoryKey);
  const entries = useMemo(
    () =>
      plusEntries(
        m,
        new Set<PlusEntryId>([
          ...(props.offersBrowser ? [] : (["browserTabs"] as const)),
          ...(props.environmentId === null ? (["plugins"] as const) : []),
        ]),
      ),
    [m, props.offersBrowser, props.environmentId],
  );

  const done = (action: () => void) => () => {
    props.onClose();
    action();
  };
  const run = (entry: PlusEntry) => {
    const { effect } = entry;
    switch (effect.kind) {
      case "attach":
        return;
      case "capture":
        return done(props.onCapture)();
      case "sketch":
        return done(props.onSketch)();
      case "insert":
        return done(() => props.onInsertText(effect.text))();
      case "mention":
        return done(() => props.onInsertText("@"))();
      case "skills":
        return done(() => props.onInsertText("$"))();
      case "browser":
        return done(props.onBrowser)();
      case "shelf":
        return props.onPageChange(effect.shelf);
      case "toggle":
        toggleThreadMemoryEnabled(props.memoryKey);
        return;
    }
  };

  const title =
    props.page === "main"
      ? null
      : (entries.find((entry) => entry.id === props.page)?.label ?? null);
  return (
    <Modal
      visible={props.visible}
      animationType="slide"
      presentationStyle="pageSheet"
      onRequestClose={props.onClose}
    >
      <GlassSurface
        className="flex-1"
        chrome="none"
        accessibilityLabel={gt("Add files, agents, skills and more")}
      >
        <View className="flex-row items-center gap-2 px-4 pb-2 pt-4">
          {props.page !== "main" ? (
            <Pressable
              accessibilityRole="button"
              accessibilityLabel={m(BACK)}
              onPress={() => props.onPageChange("main")}
            >
              <SymbolView
                name="chevron.left"
                size={18}
                weight="semibold"
                tintColorClassName="accent-icon"
                type="monochrome"
              />
            </Pressable>
          ) : null}
          <Text textRole="headline" className="flex-1">
            {title ?? gt("Add")}
          </Text>
          <Pressable
            accessibilityRole="button"
            accessibilityLabel={gt("Close")}
            onPress={props.onClose}
          >
            <SymbolView
              name="xmark.circle.fill"
              size={22}
              weight="regular"
              tintColorClassName="accent-icon"
              type="monochrome"
            />
          </Pressable>
        </View>
        <ScrollView contentContainerClassName="gap-1 px-2 pb-8">
          {props.page === "main" ? (
            entries.flatMap((entry) =>
              entry.id === "upload"
                ? [
                    <Row
                      key="photos"
                      symbol="photo"
                      title={gt("Photo Library")}
                      note={entry.note}
                      onPress={done(props.onPickMedia)}
                    />,
                    ...(props.supportsFiles
                      ? [
                          <Row
                            key="files"
                            symbol="folder"
                            title={gt("Choose Files")}
                            onPress={done(props.onPickFiles)}
                          />,
                        ]
                      : []),
                  ]
                : entry.effect.kind === "toggle"
                  ? [
                      <View
                        key={entry.id}
                        className="min-h-[48px] flex-row items-center gap-3 rounded-2xl px-3 py-2"
                      >
                        <SymbolView
                          name={SYMBOLS[entry.id] as never}
                          size={20}
                          weight="regular"
                          tintColorClassName="accent-icon"
                          type="monochrome"
                        />
                        <View className="min-w-0 flex-1">
                          <Text textRole="body" numberOfLines={1}>
                            {entry.label}
                          </Text>
                          {entry.note ? (
                            <Text
                              textRole="caption"
                              className="text-foreground-muted"
                              numberOfLines={1}
                            >
                              {entry.note}
                            </Text>
                          ) : null}
                        </View>
                        <Switch
                          value={memoryEnabled}
                          accessibilityLabel={entry.label}
                          onValueChange={() => {
                            toggleThreadMemoryEnabled(props.memoryKey);
                          }}
                        />
                      </View>,
                    ]
                  : [
                      <Row
                        key={entry.id}
                        symbol={SYMBOLS[entry.id]}
                        title={entry.id === "screenshot" ? m(PHOTO) : entry.label}
                        note={entry.id === "screenshot" ? m(PHOTO_NOTE) : entry.note}
                        onPress={() => run(entry)}
                      />,
                    ],
            )
          ) : props.page === "plugins" ? (
            props.environmentId ? (
              <PluginsPage environmentId={props.environmentId} />
            ) : null
          ) : (
            <CloudPage
              shelf={props.page}
              onChoose={(text) => done(() => props.onInsertText(text))()}
            />
          )}
        </ScrollView>
      </GlassSurface>
    </Modal>
  );
}
