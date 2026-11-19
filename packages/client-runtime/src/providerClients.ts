import {
  AntigravitySettings,
  ClaudeSettings,
  CodexSettings,
  HanzoGenSettings,
  HanzoSettings,
  isProviderDriverKind,
  ProviderDriverKind,
} from "@hanzodev/contracts";
import { acpRegistryClient } from "@hanzodev/provider-acp-registry/client";
import { makeProviderClientRegistry, type ProviderIcon } from "@hanzodev/provider-core/client";
import { modelShortName } from "@hanzodev/provider-core/modelShortName";
import { cursorClient } from "@hanzodev/provider-cursor/client";
import { fxClient } from "@hanzodev/provider-fx/client";
import { grokClient } from "@hanzodev/provider-grok/client";
import { kiroClient } from "@hanzodev/provider-kiro/client";
import { museClient } from "@hanzodev/provider-muse/client";
import { openCodeClient } from "@hanzodev/provider-opencode/client";
import { piClient } from "@hanzodev/provider-pi/client";

export const hanzoIcon: ProviderIcon = {
  viewBox: "0 0 67 67",
  fill: { light: "#09090B", dark: "#FAFAFA" },
  paths: [
    { d: "M22.21 0H0V22.3184H22.21V0Z" },
    { d: "M66.7198 0H44.5098V22.3184H66.7198V0Z" },
    { d: "M66.7038 22.3184H22.2534L0.0878906 44.6367H44.4634L66.7038 22.3184Z" },
    { d: "M22.21 67V44.6369H0V67H22.21Z" },
    { d: "M66.7198 67V44.6369H44.5098V67H66.7198Z" },
  ],
};

export const hanzoGenIcon: ProviderIcon = {
  viewBox: "0 0 67 67",
  fill: { light: "#09090B", dark: "#FAFAFA" },
  paths: [
    { d: "M22.21 0H0V22.3184H22.21V0Z" },
    { d: "M66.7198 0H44.5098V22.3184H66.7198V0Z" },
    {
      d: "M66.7038 22.3184H22.2534L0.0878906 44.6367H44.4634L66.7038 22.3184Z",
      fill: { light: "#A1A1AA", dark: "#71717A" },
    },
    { d: "M22.21 67V44.6369H0V67H22.21Z" },
    {
      d: "M66.7198 67V44.6369H44.5098V67H66.7198ZM61.16 61.44V50.2H50.07V61.44H61.16Z",
      fillRule: "evenodd",
    },
  ],
};

// OpenRouter's Ink and Cloud glyphs from https://openrouter.ai/brand.
export const openRouterIcon: ProviderIcon = {
  viewBox: "0 0 1024 730",
  fill: { light: "#03080A", dark: "#FCFCFE" },
  paths: [
    {
      d: "M795.893 0C915.776 0 1012.95 97.9963 1012.95 218.88C1012.95 339.764 915.776 437.76 795.893 437.76L1011.2 654.869C1038.55 682.447 1019.18 729.6 980.504 729.6H361.77C161.97 729.6 0 566.273 0 364.8C0 163.327 161.97 0 361.77 0L795.893 0ZM361.77 145.92C241.89 145.92 144.708 243.916 144.708 364.8C144.708 485.684 241.89 583.68 361.77 583.68C481.649 583.68 578.831 485.684 578.831 364.8C578.831 243.916 481.649 145.92 361.77 145.92Z",
    },
  ],
};

export const providerClients = makeProviderClientRegistry([
  {
    driverKind: ProviderDriverKind.make("hanzo"),
    label: "Hanzo",
    icon: hanzoIcon,
    modelFamilies: ["Enso", "Zen ", "Kai "],
    acpRegistryAgentIds: ["hanzo-dev", "hanzo-acp"],
    settingsSchema: HanzoSettings,
  },
  {
    driverKind: ProviderDriverKind.make("hanzoGen"),
    label: "Hanzo Gen",
    icon: hanzoGenIcon,
    modelFamilies: ["Enso", "Zen ", "Kai "],
    acpRegistryAgentIds: ["hanzo-gen"],
    settingsSchema: HanzoGenSettings,
  },
  {
    driverKind: ProviderDriverKind.make("codex"),
    label: "Codex",
    modelFamilies: ["GPT-", "GPT "],
    acpRegistryAgentIds: ["codex-acp"],
    settingsSchema: CodexSettings,
  },
  {
    driverKind: ProviderDriverKind.make("claudeAgent"),
    label: "Claude",
    modelFamilies: ["Claude "],
    acpRegistryAgentIds: ["claude-acp"],
    settingsSchema: ClaudeSettings,
  },
  cursorClient,
  grokClient,
  fxClient,
  openCodeClient,
  {
    driverKind: ProviderDriverKind.make("antigravity"),
    label: "Antigravity",
    modelFamilies: ["Gemini "],
    acpRegistryAgentIds: ["antigravity-acp"],
    settingsSchema: AntigravitySettings,
  },
  kiroClient,
  museClient,
  piClient,
  acpRegistryClient,
]);

export function getProviderClient(driver: string | null | undefined) {
  return isProviderDriverKind(driver) ? providerClients.get(driver) : undefined;
}

export function resolveModelShortName(
  model: { readonly name: string; readonly shortName?: string | undefined },
  driverKind: ProviderDriverKind | null | undefined,
): string | undefined {
  if (model.shortName) return model.shortName;
  const families = driverKind ? providerClients.get(driverKind)?.modelFamilies : undefined;
  return modelShortName(model.name, families ?? []);
}
