import { EnvironmentId, type BrowserSource, type DesktopLinkStatus } from "@hanzodev/contracts";
import { AsyncResult, Atom } from "effect/reactivity";
import { describe, expect, it, vi } from "vite-plus/test";

import "~/index.css";
import { render } from "../../../test/render";
import { LinkedDesktopSettings } from "./LinkedDesktopSettings";

const environmentId = EnvironmentId.make("mac");

const mocks = vi.hoisted(() => ({ statusAtom: null as unknown, browsersAtom: null as unknown }));

vi.mock(import("~/state/environments"), async (original) => ({
  ...(await original()),
  usePrimaryEnvironmentId: () => environmentId,
}));

vi.mock(import("~/state/desktops"), async () => {
  const idle = Atom.make(AsyncResult.success({ desktops: [], accounts: [] }));
  const allowed = Atom.make(true);
  const command = { label: "command", permissionAtom: () => allowed };
  return {
    desktopEnvironment: {
      status: () => mocks.statusAtom,
      list: () => idle,
      outbox: () => Atom.make(AsyncResult.success({ sendNothing: false, batches: [] })),
      accounts: () => idle,
      link: command,
      unlink: command,
      rename: command,
      revoke: command,
      setShared: command,
      setSendNothing: command,
      addAccount: command,
      removeAccount: command,
    } as never,
  };
});

vi.mock(import("~/state/browserSources"), () => ({
  browserSourceEnvironment: { list: () => mocks.browsersAtom } as never,
}));

vi.mock(import("~/state/use-atom-command"), () => ({
  useAtomCommand: (() => () => Promise.resolve({ _tag: "Success" })) as never,
}));

vi.mock(import("~/cloud/hanzoWebAuth"), () => ({
  useAuth: (() => ({ getToken: () => Promise.resolve(null) })) as never,
  useHanzoWebAccounts: (() => ({ activeAccount: null })) as never,
}));

const status: DesktopLinkStatus = {
  signedIn: true,
  deviceCode: null,
  identity: "z@hanzo.ai",
  organization: "hanzo",
  desktop: null,
  lastHeartbeatAt: null,
  level: "metadata",
  providers: [],
  report: { body: "{}", sent: false },
};

const firefox: BrowserSource = {
  id: "browser/mac/firefox-4dec",
  browser: "firefox",
  name: "Firefox · Hanzo extension",
  capabilities: ["browser.tabs"],
  providers: [
    { provider: "chatgpt", plan: "plus", state: "ready" },
    { provider: "claude", state: "needs_login" },
  ],
};

const setup = (sources: ReadonlyArray<BrowserSource>) => {
  mocks.statusAtom = Atom.make(AsyncResult.success(status));
  mocks.browsersAtom = Atom.make(AsyncResult.success({ sources }));
};

describe("Browsers on this computer", () => {
  it("shows the extension as a view-only linked source on its browser", async () => {
    setup([firefox]);
    await using screen = await render(<LinkedDesktopSettings />);
    await expect.element(screen.getByText("Browsers on this computer")).toBeVisible();
    await expect.element(screen.getByText("Firefox · Hanzo extension")).toBeVisible();
    await expect.element(screen.getByText("on Firefox")).toBeVisible();
    await expect
      .element(screen.getByText(/chatgpt · plus · Ready, claude · Needs login/))
      .toBeVisible();
    await expect.element(screen.getByText("View only", { exact: true })).toBeVisible();
  });

  it("says so when no extension is connected", async () => {
    setup([]);
    await using screen = await render(<LinkedDesktopSettings />);
    await expect.element(screen.getByText("No browser extension connected")).toBeVisible();
  });
});
