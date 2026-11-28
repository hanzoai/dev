import {
  EnvironmentId,
  ProviderDriverKind,
  ProviderInstanceId,
  type ServerProviderModel,
} from "@hanzodev/contracts";
import { createModelCapabilities } from "@hanzodev/shared/model";
import { GTProvider, initializeGT } from "gt-react";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vite-plus/test";

import { getProviderModelCapabilities } from "../../providerModels";

initializeGT({ defaultLocale: "en", locales: ["en"] });
import { describeComposerEffort } from "./ComposerModelEffort";
import { EnsoRing } from "./EnsoRing";
import { TimelineRowActivityCtx, TimelineRowCtx, type TimelineRow } from "./timelineRowContext";
import { LiveWorkEntryTimelineRow, ThinkingTimelineRow } from "./TimelineWorkGroupRows";
import { WorkLogRow } from "./WorkLog";

const HANZO = ProviderDriverKind.make("hanzo");
const HANZO_INSTANCE = ProviderInstanceId.make("hanzo");

const narrowHanzoModels: ReadonlyArray<ServerProviderModel> = [
  {
    slug: "enso-auto",
    name: "Enso Auto",
    isCustom: false,
    capabilities: createModelCapabilities({
      optionDescriptors: [
        {
          id: "reasoningEffort",
          label: "Reasoning",
          type: "select",
          options: [
            { id: "low", label: "Low" },
            { id: "medium", label: "Medium", isDefault: true },
            { id: "high", label: "High" },
          ],
          currentValue: "medium",
        },
      ],
    }),
  },
  { slug: "enso-ultra", name: "Enso Ultra", isCustom: false, capabilities: {} },
  { slug: "enso-max", name: "Enso Max", isCustom: false, capabilities: {} },
  { slug: "enso-pro", name: "Enso Pro", isCustom: false, capabilities: {} },
  { slug: "enso", name: "Enso", isCustom: false, capabilities: {} },
  { slug: "enso-flash", name: "Enso Flash", isCustom: false, capabilities: {} },
];

describe("Hanzo reasoning capabilities & progressive Enso composer glyphs", () => {
  it("upgrades narrow or empty Hanzo capabilities to all 7 reasoning levels", () => {
    const caps = getProviderModelCapabilities(narrowHanzoModels, "enso-auto", HANZO);
    const reasoning = caps.optionDescriptors?.[0];
    expect(reasoning?.type).toBe("select");
    if (reasoning?.type === "select") {
      expect(reasoning.options.map((option) => option.id)).toEqual([
        "off",
        "minimal",
        "low",
        "medium",
        "high",
        "xhigh",
        "max",
      ]);
      expect(reasoning.options.find((option) => option.isDefault)?.id).toBe("medium");
    }
  });

  it("maps every Enso model to its progressive arc (only Enso and Enso Auto closed) and shows dot on max", () => {
    const cases = [
      ["enso-auto", 0],
      ["hanzo/enso-auto", 0],
      ["enso-ultra", 0.08],
      ["enso-max", 0.16],
      ["enso-pro", 0.24],
      ["enso", 0],
      ["hanzo/enso", 0],
      ["enso-hybrid", 0.32],
      ["enso-flash", 0.42],
    ] as const;

    for (const [slug, expectedGap] of cases) {
      const normal = describeComposerEffort(
        {
          provider: HANZO,
          instanceId: HANZO_INSTANCE,
          models: narrowHanzoModels,
          model: slug,
          modelOptions: [{ id: "reasoningEffort", value: "high" }],
          onModelOptionsChange: () => {},
        },
        "bars",
        "sm",
      );
      expect(normal.ring, slug).toEqual({ gap: expectedGap, dot: false });

      const maxed = describeComposerEffort(
        {
          provider: HANZO,
          instanceId: HANZO_INSTANCE,
          models: narrowHanzoModels,
          model: slug,
          modelOptions: [{ id: "reasoningEffort", value: "max" }],
          onModelOptionsChange: () => {},
        },
        "bars",
        "sm",
      );
      expect(maxed.ring, `${slug} max`).toEqual({ gap: expectedGap, dot: true });
    }
  });

  it("renders EnsoRing with distinct progressive gaps and optional max center dot", () => {
    const autoHtml = renderToStaticMarkup(<EnsoRing gap={0} />);
    const ultraHtml = renderToStaticMarkup(<EnsoRing gap={0.08} />);
    const flashHtml = renderToStaticMarkup(<EnsoRing gap={0.42} />);
    const maxHtml = renderToStaticMarkup(<EnsoRing gap={0.16} dot />);

    expect(autoHtml).toContain('data-enso-ring="0"');
    expect(ultraHtml).toContain('data-enso-ring="0.08"');
    expect(flashHtml).toContain('data-enso-ring="0.42"');
    expect(maxHtml).toContain('data-enso-ring="max"');
    expect(maxHtml).toContain("<circle");
    expect(autoHtml).not.toContain("<circle");
  });
});

function withTimelineContext(element: React.ReactNode) {
  const ctxValue = {
    routeThreadKey: "local:t-1",
    threadRef: null,
    activeThreadEnvironmentId: EnvironmentId.make("local"),
    workspaceRoot: "/repo",
    markdownCwd: "/repo",
    timestampFormat: "12-hour" as const,
    pendingApprovals: new Map(),
    workGroupViewState: {
      expandedGroupIds: new Set<string>(),
      expandedEntryIds: new Set<string>(),
      scrollPositions: new Map<string, number>(),
    },
    onToggleWorkGroup: () => {},
    onToggleWorkEntry: () => {},
    onWorkEntryContentReady: () => {},
  } as unknown as React.ComponentProps<typeof TimelineRowCtx.Provider>["value"];

  const activityValue = {
    isCompacting: false,
    isPreparingWorktree: false,
    backgroundWorktreeSetup: null,
    liveAnswerPreview: null,
    localModel: false,
    providerStartingLabel: null,
  } as React.ComponentProps<typeof TimelineRowActivityCtx.Provider>["value"];

  return (
    <GTProvider locale="en" translations={{}}>
      <TimelineRowCtx.Provider value={ctxValue}>
        <TimelineRowActivityCtx.Provider value={activityValue}>
          {element}
        </TimelineRowActivityCtx.Provider>
      </TimelineRowCtx.Provider>
    </GTProvider>
  );
}

describe("Thinking & reasoning timeline rows hide brain icon and align with Working row", () => {
  it("uses px-1 alignment when WorkLogRow has no icon and px-0.5 when an icon is present", () => {
    const noIconHtml = renderToStaticMarkup(<WorkLogRow icon={null} label="Thinking" />);
    const withIconHtml = renderToStaticMarkup(
      <WorkLogRow icon={<span data-icon="tool" />} label="Running command" />,
    );
    expect(noIconHtml).toContain("px-1");
    expect(noIconHtml).not.toContain("px-0.5");
    expect(noIconHtml).not.toContain("<svg");
    expect(withIconHtml).toContain("px-0.5");
    expect(withIconHtml).toContain('data-icon="tool"');
  });

  it("renders ThinkingTimelineRow text-only with no brain SVG and px-1 alignment", () => {
    const row: Extract<TimelineRow, { kind: "thinking" }> = {
      kind: "thinking",
      id: "thinking-1",
      createdAt: null,
      label: "Starting",
      groupId: "g-1",
      expanded: false,
    };
    const html = renderToStaticMarkup(withTimelineContext(<ThinkingTimelineRow row={row} />));
    expect(html).toContain("Starting");
    expect(html).toContain("px-1");
    expect(html).not.toContain("<svg");
  });

  it("renders LiveWorkEntryTimelineRow reasoning entries without any brain SVG", () => {
    const reasoningEntry = {
      id: "entry-reasoning",
      createdAt: "2026-11-28T00:00:00.000Z",
      label: "Thinking",
      detail: "Inspecting workspace files",
      tone: "thinking" as const,
      itemType: "reasoning" as const,
    };
    const row: Extract<TimelineRow, { kind: "work-live" }> = {
      kind: "work-live",
      id: "live-1",
      createdAt: reasoningEntry.createdAt,
      groupId: "g-1",
      entry: reasoningEntry,
      groupedEntries: [reasoningEntry],
      thought: reasoningEntry,
      active: true,
      expanded: false,
    };
    const html = renderToStaticMarkup(withTimelineContext(<LiveWorkEntryTimelineRow row={row} />));
    expect(html).toContain("Inspecting workspace files");
    expect(html).toContain("px-1");
    expect(html).not.toContain("<svg");
  });

  it("keeps the tool icon for non-reasoning tool entries in LiveWorkEntryTimelineRow", () => {
    const commandEntry = {
      id: "entry-cmd",
      createdAt: "2026-11-28T00:00:00.000Z",
      label: "Running tests",
      tone: "tool" as const,
      itemType: "command_execution" as const,
    };
    const row: Extract<TimelineRow, { kind: "work-live" }> = {
      kind: "work-live",
      id: "live-2",
      createdAt: commandEntry.createdAt,
      groupId: "g-1",
      entry: commandEntry,
      groupedEntries: [commandEntry],
      active: true,
      expanded: false,
    };
    const html = renderToStaticMarkup(withTimelineContext(<LiveWorkEntryTimelineRow row={row} />));
    expect(html).toContain("Running tests");
    expect(html).toContain("<svg");
  });
});
