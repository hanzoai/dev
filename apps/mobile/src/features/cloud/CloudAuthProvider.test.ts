import { managedRelaySessionAtom } from "@hanzodev/client-runtime/relay";
import { afterEach, describe, expect, it, vi } from "vite-plus/test";

import { appAtomRegistry } from "../../state/atom-registry";
import {
  activateCloudRelayAccount,
  cloudAccountService,
  deactivateCloudRelayAccount,
} from "./cloudAccountService";
import { setAgentAwarenessRelayTokenProvider } from "../agent-awareness/remoteRegistration";
import { requestConnectOnboarding } from "./connectOnboarding";
import { restoreCloudComposerDrafts } from "../../state/use-composer-drafts";

vi.mock("../../lib/runtime", () => ({
  runtime: {
    runPromiseExit: vi.fn(),
  },
}));

vi.mock("../../connection/catalog", () => ({
  environmentCatalog: {
    removeRelayEnvironments: {},
  },
}));

vi.mock("./cloud-drafts", () => ({ removeCloudEnvironments: {} }));
vi.mock("./connectOnboarding", () => ({
  clearConnectOnboardingRequest: vi.fn(),
  requestConnectOnboarding: vi.fn(),
}));
vi.mock("../../state/use-composer-drafts", () => ({
  getComposerCloudAccountId: vi.fn(async () => null),
  restoreCloudComposerDrafts: vi.fn(async () => undefined),
}));

vi.mock("../agent-awareness/remoteRegistration", () => ({
  releaseAgentAwarenessRelayTokenProvider: vi.fn(),
  setAgentAwarenessRelayTokenProvider: vi.fn(),
  unregisterAgentAwarenessDeviceForCurrentUser: vi.fn(),
}));

type SourceListener = (resources: { readonly user: { readonly id: string } | null }) => void;

function fakeHanzoSource() {
  const listeners = new Set<SourceListener>();
  const source = {
    getToken: vi.fn(async (id?: string) => (id ? `${id}-token` : "active-token")),
    addListener: (listener: SourceListener) => {
      listeners.add(listener);
      return () => listeners.delete(listener);
    },
  } as unknown as Parameters<typeof cloudAccountService.attach>[0];
  const emit = (userId: string | null) => {
    for (const listener of listeners) listener({ user: userId === null ? null : { id: userId } });
  };
  return { source, emit };
}

afterEach(() => {
  deactivateCloudRelayAccount();
  vi.clearAllMocks();
});

describe("CloudAuthProvider relay account isolation", () => {
  it("clears relay and agent-awareness credentials before cleanup can fail", async () => {
    const tokenProvider = async () => "account-1-token";
    activateCloudRelayAccount("hanzo/z", tokenProvider);
    expect(appAtomRegistry.get(managedRelaySessionAtom)?.accountId).toBe("hanzo/z");

    deactivateCloudRelayAccount();
    const cleanup = Promise.reject(new Error("Persistence removal failed.")).catch(() => undefined);

    expect(appAtomRegistry.get(managedRelaySessionAtom)).toBeNull();
    expect(vi.mocked(setAgentAwarenessRelayTokenProvider)).toHaveBeenLastCalledWith(null);
    await cleanup;
  });
});

describe("cloudAccountService", () => {
  it("activates the signed-in Hanzo IAM account once its drafts are restored", async () => {
    const { source, emit } = fakeHanzoSource();
    using _detach = { [Symbol.dispose]: cloudAccountService.attach(source) };

    emit("hanzo/z");
    expect(appAtomRegistry.get(managedRelaySessionAtom)).toBeNull();

    await vi.waitFor(() =>
      expect(appAtomRegistry.get(managedRelaySessionAtom)?.accountId).toBe("hanzo/z"),
    );
    expect(vi.mocked(restoreCloudComposerDrafts)).toHaveBeenCalledWith("hanzo/z");
    expect(vi.mocked(requestConnectOnboarding)).not.toHaveBeenCalled();
  });

  it("retries an activation whose draft restore failed on the next emission", async () => {
    const { source, emit } = fakeHanzoSource();
    using _detach = { [Symbol.dispose]: cloudAccountService.attach(source) };
    vi.mocked(restoreCloudComposerDrafts).mockRejectedValueOnce(new Error("Storage busy."));

    emit("hanzo/z");
    await vi.waitFor(() => expect(cloudAccountService.getState()._tag).toBe("Failed"));
    expect(appAtomRegistry.get(managedRelaySessionAtom)).toBeNull();

    emit("hanzo/z");
    await vi.waitFor(() =>
      expect(appAtomRegistry.get(managedRelaySessionAtom)?.accountId).toBe("hanzo/z"),
    );
  });
});
