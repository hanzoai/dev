import { EnvironmentId, type Desktop, type DesktopLinkStatus } from "@hanzodev/contracts";
import { AsyncResult, Atom } from "effect/reactivity";
import { describe, expect, it, vi } from "vite-plus/test";
import { userEvent } from "vite-plus/test/browser";

import "~/index.css";
import { render } from "../../../test/render";
import { LinkedDesktopSettings } from "./LinkedDesktopSettings";

const environmentId = EnvironmentId.make("mac");

const mocks = vi.hoisted(() => ({
  statusAtom: null as unknown,
  listAtom: null as unknown,
  calls: [] as Array<{ readonly label: string; readonly value: unknown }>,
  account: null as unknown,
}));

vi.mock(import("~/state/environments"), async (original) => ({
  ...(await original()),
  usePrimaryEnvironmentId: () => environmentId,
}));

vi.mock(import("~/state/desktops"), async () => {
  const allowed = Atom.make(true);
  const command = (label: string) => ({ label, permissionAtom: () => allowed });
  return {
    desktopEnvironment: {
      status: () => mocks.statusAtom,
      list: () => mocks.listAtom,
      link: command("link"),
      unlink: command("unlink"),
      rename: command("rename"),
      revoke: command("revoke"),
    } as never,
  };
});

vi.mock(import("~/state/use-atom-command"), () => ({
  useAtomCommand: ((command: { readonly label: string }) => (value: unknown) => {
    mocks.calls.push({ label: command.label, value });
    return Promise.resolve({ _tag: "Success" });
  }) as never,
}));

vi.mock(import("~/cloud/hanzoWebAuth"), () => ({
  useAuth: (() => ({ getToken: () => Promise.resolve("web-token") })) as never,
  useHanzoSession: (() => ({ openSignIn: () => {} })) as never,
  useHanzoWebAccounts: (() => ({ activeAccount: mocks.account })) as never,
}));

vi.mock(import("~/confirmDialog"), () => ({
  requestConfirmDialog: () => Promise.resolve(true),
}));

const thisDesktop: Desktop = {
  id: "src_1",
  kind: "desktop",
  name: "ra",
  os: "darwin",
  arch: "arm64",
  version: "1.8.4",
  state: "online",
  lastSeen: "2026-11-20T10:00:00.000Z",
  linkedAt: "2026-11-20T09:00:00.000Z",
  providers: [],
};
const body = '{\n  "at": "2026-11-20T10:00:00.000Z",\n  "version": "1.8.4",\n  "providers": []\n}';
const base: DesktopLinkStatus = {
  signedIn: true,
  identity: "z@hanzo.ai",
  organization: "hanzo",
  desktop: null,
  lastHeartbeatAt: null,
  level: "metadata",
  providers: [],
  report: { body, sent: false },
};

function setup(status: DesktopLinkStatus, desktops: ReadonlyArray<Desktop> = []) {
  mocks.statusAtom = Atom.make(AsyncResult.success(status));
  mocks.listAtom = Atom.make(AsyncResult.success({ desktops }));
  mocks.calls = [];
  mocks.account = null;
}

describe("LinkedDesktopSettings", () => {
  it("links this desktop when the server already holds a sign-in", async () => {
    setup(base);
    await using screen = await render(<LinkedDesktopSettings />);
    await userEvent.click(screen.getByRole("button", { name: "Link to Hanzo" }));
    expect(mocks.calls).toEqual([{ label: "link", value: { environmentId, input: {} } }]);
  });

  it("shows the link, each source, read-only provider chips and the exact heartbeat", async () => {
    setup(
      {
        ...base,
        desktop: thisDesktop,
        lastHeartbeatAt: thisDesktop.lastSeen,
        providers: [
          { provider: "claude", instance: "claude", kind: "login", state: "ready", plan: "max" },
        ],
        report: { body, sent: true },
      },
      [
        thisDesktop,
        { ...thisDesktop, id: "src_2", kind: "browser", name: "Chrome", state: "offline" },
      ],
    );
    await using screen = await render(<LinkedDesktopSettings />);
    await expect.element(screen.getByText("Linked as z@hanzo.ai · org hanzo")).toBeVisible();
    await expect.element(screen.getByText("This desktop only")).toBeVisible();
    await expect.element(screen.getByText(/"version": "1.8.4"/)).toBeVisible();
    await expect.element(screen.getByText("Chrome")).toBeVisible();

    await userEvent.click(screen.getByRole("button", { name: "Revoke" }).nth(1));
    expect(mocks.calls).toEqual([
      { label: "revoke", value: { environmentId, input: { id: "src_2" } } },
    ]);
    await userEvent.click(screen.getByRole("button", { name: "Unlink this desktop" }));
    expect(mocks.calls.at(-1)).toEqual({ label: "unlink", value: { environmentId, input: {} } });
  });
});
