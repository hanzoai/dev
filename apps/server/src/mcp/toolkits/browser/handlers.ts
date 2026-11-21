import * as Effect from "effect/Effect";

import * as BrowserSources from "../../../browser/BrowserSources.ts";
import * as McpToolAccess from "../../McpToolAccess.ts";
import { BrowserToolkit } from "./tools.ts";

const handlers = {
  browser_tabs: McpToolAccess.readsAsCaller((input) =>
    Effect.gen(function* () {
      const browsers = yield* BrowserSources.BrowserSources;
      return yield* browsers.tabs(input);
    }),
  ),
  browser_snapshot: McpToolAccess.readsAsCaller((input) =>
    Effect.gen(function* () {
      const browsers = yield* BrowserSources.BrowserSources;
      return { page: yield* browsers.snapshot(input) };
    }),
  ),
  browser_read: McpToolAccess.readsAsCaller((input) =>
    Effect.gen(function* () {
      const browsers = yield* BrowserSources.BrowserSources;
      return { page: yield* browsers.read(input) };
    }),
  ),
  browser_click: McpToolAccess.actsAsCaller((input) =>
    Effect.gen(function* () {
      const browsers = yield* BrowserSources.BrowserSources;
      return { page: yield* browsers.click(input) };
    }),
  ),
} satisfies McpToolAccess.Handlers<typeof BrowserToolkit.tools>;

export const layer = McpToolAccess.toLayer(BrowserToolkit, handlers);
