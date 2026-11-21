import { BrowserSource, BrowserSourceError, OrchestratorMcpFailure } from "@hanzodev/contracts";
import * as Schema from "effect/Schema";
import { McpSchema, Tool, Toolkit } from "effect/ai";

import * as McpInvocationContext from "../../McpInvocationContext.ts";
import * as ThreadManagementService from "../../../orchestration-v2/ThreadManagementService.ts";
import * as BrowserSources from "../../../browser/BrowserSources.ts";

const dependencies = [
  McpInvocationContext.McpInvocationContext,
  ThreadManagementService.ThreadManagementService,
  BrowserSources.BrowserSources,
];

const BrowserToolFailure = Schema.Union([BrowserSourceError, OrchestratorMcpFailure]);

const sourceId = Schema.optional(
  Schema.String.annotate({
    description: "The browser's id from `browser_tabs`. Needed only when several are connected.",
  }),
);
const tabId = Schema.Number.annotate({
  description: "A tab id from `browser_tabs`. Every call names its tab.",
});

const TabsInput = Schema.Struct({ sourceId });

const TabsResult = Schema.Struct({
  source: BrowserSource,
  tabs: Schema.Array(
    Schema.Struct({
      id: Schema.Number,
      active: Schema.Boolean,
      title: Schema.optional(Schema.String),
      url: Schema.optional(Schema.String),
      blocked: Schema.optional(Schema.String),
    }),
  ),
});

const PageResult = Schema.Struct({ page: Schema.Unknown });

const BrowserTabsTool = Tool.make("browser_tabs", {
  description:
    "List the tabs open in the user's own browser, through their Hanzo extension. Call first: every other browser tool takes a tab id from here. Pages of consumer AI products are listed as blocked and cannot be used.",
  parameters: TabsInput,
  success: TabsResult,
  failure: BrowserToolFailure,
  dependencies,
})
  .annotate(Tool.Title, "List browser tabs")
  .annotate(Tool.Readonly, false)
  .annotate(Tool.Destructive, false)
  .annotate(Tool.Idempotent, true)
  .annotate(Tool.OpenWorld, true)
  .annotate(McpSchema.EnabledWhen, McpInvocationContext.hiddenFromOutsideClients);

const BrowserSnapshotTool = Tool.make("browser_snapshot", {
  description:
    "The accessibility tree of a tab in the user's own signed-in browser, with a [ref=eN] on each actionable node. Use a ref with `browser_click`. Snapshot again after the page changes.",
  parameters: Schema.Struct({
    sourceId,
    tabId,
    interactive: Schema.optional(
      Schema.Boolean.annotate({ description: "Only interactive elements, flat." }),
    ),
    selector: Schema.optional(Schema.String.annotate({ description: "Limit to this subtree." })),
  }),
  success: PageResult,
  failure: BrowserToolFailure,
  dependencies,
})
  .annotate(Tool.Title, "Snapshot browser tab")
  .annotate(Tool.Readonly, false)
  .annotate(Tool.Destructive, false)
  .annotate(Tool.Idempotent, true)
  .annotate(Tool.OpenWorld, true)
  .annotate(McpSchema.EnabledWhen, McpInvocationContext.hiddenFromOutsideClients);

const BrowserReadTool = Tool.make("browser_read", {
  description:
    "A tab in the user's own signed-in browser as markdown. `outline` returns headings only; `filter` keeps sections that mention a word.",
  parameters: Schema.Struct({
    sourceId,
    tabId,
    outline: Schema.optional(Schema.Boolean),
    filter: Schema.optional(Schema.String),
  }),
  success: PageResult,
  failure: BrowserToolFailure,
  dependencies,
})
  .annotate(Tool.Title, "Read browser tab")
  .annotate(Tool.Readonly, false)
  .annotate(Tool.Destructive, false)
  .annotate(Tool.Idempotent, true)
  .annotate(Tool.OpenWorld, true)
  .annotate(McpSchema.EnabledWhen, McpInvocationContext.hiddenFromOutsideClients);

const BrowserClickTool = Tool.make("browser_click", {
  description:
    "Click an element of a tab in the user's own signed-in browser. `selector` is a snapshot ref (@e2) or a CSS selector. A stale ref, or an element under a banner, is refused with what to do next.",
  parameters: Schema.Struct({
    sourceId,
    tabId,
    selector: Schema.String.annotate({ description: "A snapshot ref (@e2) or a CSS selector." }),
  }),
  success: PageResult,
  failure: BrowserToolFailure,
  dependencies,
})
  .annotate(Tool.Title, "Click in browser tab")
  .annotate(Tool.Readonly, false)
  .annotate(Tool.Destructive, true)
  .annotate(Tool.Idempotent, false)
  .annotate(Tool.OpenWorld, true)
  .annotate(McpSchema.EnabledWhen, McpInvocationContext.hiddenFromOutsideClients);

export const BrowserToolkit = Toolkit.make(
  BrowserTabsTool,
  BrowserSnapshotTool,
  BrowserReadTool,
  BrowserClickTool,
);
