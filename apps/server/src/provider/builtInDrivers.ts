import {
  AcpRegistryDriver,
  type AcpRegistryDriverEnv,
} from "@hanzodev/provider-acp-registry/server";
import { AntigravityDriver, type AntigravityDriverEnv } from "./Drivers/AntigravityDriver.ts";
import { ClaudeDriver, type ClaudeDriverEnv } from "./Drivers/ClaudeDriver.ts";
import { CodexDriver, type CodexDriverEnv } from "./Drivers/CodexDriver.ts";
import { HanzoDriver, type HanzoDriverEnv } from "./Drivers/HanzoDriver.ts";
import { HanzoGenDriver, type HanzoGenDriverEnv } from "./Drivers/HanzoGenDriver.ts";
import { CursorDriver, type CursorDriverEnv } from "@hanzodev/provider-cursor/server";
import { FxDriver, type FxDriverEnv } from "@hanzodev/provider-fx/server";
import { GrokDriver, type GrokDriverEnv } from "@hanzodev/provider-grok/server";
import { OpenCodeDriver, type OpenCodeDriverEnv } from "@hanzodev/provider-opencode/server";
import { MuseDriver, type MuseDriverEnv } from "@hanzodev/provider-muse/server";
import { PiDriver, type PiDriverEnv } from "@hanzodev/provider-pi/server";
import type {
  AnyProviderDriver,
  ProviderUsageReaderEnv,
} from "@hanzodev/provider-core/server/driver";
import { KiroDriver, type KiroDriverEnv } from "@hanzodev/provider-kiro/server";

export type BuiltInDriversEnv =
  | AcpRegistryDriverEnv
  | AntigravityDriverEnv
  | ClaudeDriverEnv
  | CodexDriverEnv
  | HanzoDriverEnv
  | HanzoGenDriverEnv
  | CursorDriverEnv
  | FxDriverEnv
  | GrokDriverEnv
  | KiroDriverEnv
  | OpenCodeDriverEnv
  | PiDriverEnv
  | MuseDriverEnv;

export const BUILT_IN_DRIVERS: ReadonlyArray<AnyProviderDriver<BuiltInDriversEnv>> = [
  HanzoDriver,
  HanzoGenDriver,
  CodexDriver,
  ClaudeDriver,
  CursorDriver,
  GrokDriver,
  FxDriver,
  OpenCodeDriver,
  AntigravityDriver,
  KiroDriver,
  PiDriver,
  MuseDriver,
  AcpRegistryDriver,
];

export type BuiltInUsageReadersEnv =
  | ProviderUsageReaderEnv<typeof HanzoDriver>
  | ProviderUsageReaderEnv<typeof ClaudeDriver>
  | ProviderUsageReaderEnv<typeof CodexDriver>
  | ProviderUsageReaderEnv<typeof GrokDriver>
  | ProviderUsageReaderEnv<typeof OpenCodeDriver>
  | ProviderUsageReaderEnv<typeof AntigravityDriver>
  | ProviderUsageReaderEnv<typeof CursorDriver>
  | ProviderUsageReaderEnv<typeof PiDriver>
  | ProviderUsageReaderEnv<typeof MuseDriver>;

/** Order matters: aggregation keeps the first duplicate record. */
export const BUILT_IN_USAGE_DRIVERS: ReadonlyArray<
  AnyProviderDriver<BuiltInDriversEnv, BuiltInUsageReadersEnv>
> = [
  HanzoDriver,
  ClaudeDriver,
  CodexDriver,
  GrokDriver,
  PiDriver,
  OpenCodeDriver,
  MuseDriver,
  AntigravityDriver,
  CursorDriver,
];
