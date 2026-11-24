import {
  plusEntries,
  type PlusEntry,
  type PlusEntryId,
} from "@hanzodev/client-runtime/composer-plus";
import type { EnvironmentId, ScopedThreadRef } from "@hanzodev/contracts";
import { useGT, useMessages } from "gt-react";
import {
  AppWindowIcon,
  BlocksIcon,
  BotIcon,
  BrainIcon,
  CameraIcon,
  FolderIcon,
  GlobeIcon,
  ImageIcon,
  MonitorIcon,
  PaletteIcon,
  PenToolIcon,
  PlugIcon,
  PlusIcon,
  ScrollTextIcon,
  TelescopeIcon,
  PaperclipIcon,
} from "lucide-react";
import { type ComponentType, Fragment, useRef, useState } from "react";

import { canCaptureScreen, captureScreenshot } from "../../lib/screenCapture";
import { type MemorySwitchTarget, useMemorySwitchStore } from "../../memorySwitchStore";
import { useRightPanelStore } from "../../rightPanelStore";
import { toastManager } from "../ui/toast";
import { Button } from "../ui/button";
import { Menu, MenuItem, MenuPopup, MenuSeparator, MenuShortcut, MenuTrigger } from "../ui/menu";
import { Switch } from "../ui/switch";
import { ComposerShelfDialog, type ComposerShelf } from "./ComposerShelfDialog";
import { ComposerSketchDialog } from "./ComposerSketchDialog";

const ICONS: Record<PlusEntryId, ComponentType<{ className?: string }>> = {
  upload: PaperclipIcon,
  library: FolderIcon,
  screenshot: CameraIcon,
  createImage: ImageIcon,
  browserTabs: AppWindowIcon,
  webSearch: GlobeIcon,
  research: TelescopeIcon,
  sketch: PenToolIcon,
  thinking: BrainIcon,
  agents: BotIcon,
  skills: ScrollTextIcon,
  connectors: BlocksIcon,
  design: PaletteIcon,
  plugins: PlugIcon,
  devices: MonitorIcon,
  memory: BrainIcon,
};

/**
 * The composer's `+`: every entry hanzo.app's composer has (`plusEntries`), each doing the Hanzo
 * Dev thing: files, the screen, a sketch, the browser panel, the `@` and `$` lists, plugins here
 * and connectors, design systems and memories from the cloud.
 */
export function ComposerPlusMenu({
  environmentId,
  threadRef,
  memoryTarget,
  addComposerAttachments,
  focusComposer,
  onInsertText,
  shelf,
  onShelfChange,
}: {
  readonly environmentId: EnvironmentId;
  readonly threadRef: ScopedThreadRef;
  readonly memoryTarget: MemorySwitchTarget;
  readonly addComposerAttachments: (files: File[]) => Promise<boolean>;
  readonly focusComposer: () => void;
  readonly onInsertText: (text: string) => void;
  /** The open list, held by the composer so `/plugins` opens it too. */
  readonly shelf: ComposerShelf | null;
  readonly onShelfChange: (shelf: ComposerShelf | null) => void;
}) {
  const gt = useGT();
  const m = useMessages();
  const inputRef = useRef<HTMLInputElement>(null);
  const [sketching, setSketching] = useState(false);
  const memoryEnabled = useMemorySwitchStore((store) => store.isMemoryEnabled(memoryTarget));
  const toggleMemoryEnabled = useMemorySwitchStore((store) => store.toggleMemoryEnabled);

  const entries = plusEntries(m, new Set<PlusEntryId>(canCaptureScreen() ? [] : ["screenshot"]));

  const insert = (text: string) => {
    onInsertText(text);
    focusComposer();
  };

  const run = (entry: PlusEntry) => {
    const { effect } = entry;
    switch (effect.kind) {
      case "attach":
        inputRef.current?.click();
        return;
      case "capture":
        void captureScreenshot().then(
          (file) => {
            if (file) void addComposerAttachments([file]);
          },
          (cause: unknown) =>
            toastManager.add({
              type: "error",
              title: gt("Couldn't take a screenshot"),
              description: cause instanceof Error ? cause.message : gt("Try again."),
            }),
        );
        return;
      case "sketch":
        setSketching(true);
        return;
      case "insert":
        insert(effect.text);
        return;
      case "mention":
        insert("@");
        return;
      case "skills":
        insert("$");
        return;
      case "browser":
        useRightPanelStore.getState().openBrowser(threadRef, null);
        return;
      case "shelf":
        onShelfChange(effect.shelf);
        return;
      case "toggle":
        toggleMemoryEnabled(memoryTarget);
        return;
    }
  };

  return (
    <>
      <input
        ref={inputRef}
        type="file"
        multiple
        className="hidden"
        onChange={(event) => {
          const files = Array.from(event.currentTarget.files ?? []);
          event.currentTarget.value = "";
          // Inserting a chip refocuses the editor after the draft renders;
          // focusing synchronously here would report the editor's stale text
          // over the prompt that was just written.
          void addComposerAttachments(files).then((inserted) => {
            if (!inserted) focusComposer();
          });
        }}
      />
      <Menu>
        <MenuTrigger
          render={
            <Button
              type="button"
              variant="ghost"
              size="icon-sm"
              data-slot="composer-plus-trigger"
              onPointerDown={(event) => event.preventDefault()}
              aria-label={gt("Add files, agents, skills and more")}
            />
          }
        >
          <PlusIcon className="size-4" />
        </MenuTrigger>
        <MenuPopup
          align="start"
          side="top"
          sideOffset={8}
          className="w-80 max-h-[min(32rem,70vh)] overflow-y-auto"
        >
          {entries.map((entry, index) => {
            const Icon = ICONS[entry.id];
            const startsGroup = index > 0 && entries[index - 1]?.group !== entry.group;
            const isMemoryToggle = entry.effect.kind === "toggle";
            return (
              <Fragment key={entry.id}>
                {startsGroup ? <MenuSeparator /> : null}
                <MenuItem
                  data-plus-entry={entry.id}
                  {...(isMemoryToggle
                    ? {
                        closeOnClick: false,
                        role: "menuitemcheckbox",
                        "aria-checked": memoryEnabled,
                      }
                    : {})}
                  onClick={() => run(entry)}
                >
                  <Icon className="size-4 shrink-0 text-muted-foreground" />
                  <span className="flex min-w-0 flex-1 items-baseline gap-2 truncate">
                    <span className="font-medium text-foreground">{entry.label}</span>
                    <span className="truncate text-xs text-muted-foreground">{entry.note}</span>
                  </span>
                  {isMemoryToggle ? (
                    <Switch
                      checked={memoryEnabled}
                      tabIndex={-1}
                      aria-hidden="true"
                      className="pointer-events-none shrink-0"
                    />
                  ) : entry.shortcut ? (
                    <MenuShortcut>{entry.shortcut}</MenuShortcut>
                  ) : null}
                </MenuItem>
              </Fragment>
            );
          })}
        </MenuPopup>
      </Menu>
      <ComposerShelfDialog
        shelf={shelf}
        environmentId={environmentId}
        onClose={() => {
          onShelfChange(null);
          focusComposer();
        }}
        onInsertText={onInsertText}
      />
      <ComposerSketchDialog
        open={sketching}
        onClose={() => setSketching(false)}
        onAttach={(file) => void addComposerAttachments([file])}
      />
    </>
  );
}
