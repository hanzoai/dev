import { afterEach, describe, expect, it, vi } from "vite-plus/test";
import {
  applyDockIconMode,
  DEFAULT_DOCK_ICON_MODE,
  normalizeDockIconMode,
  resetDockIconStoreForTests,
  resolveDockIconBridge,
} from "./DockIconSettingsRow";

afterEach(() => {
  resetDockIconStoreForTests();
  vi.unstubAllGlobals();
});

describe("DockIconSettingsRow helpers", () => {
  it("normalizes dock icon modes and defaults unknown values to branded", () => {
    expect(normalizeDockIconMode("branded")).toBe("branded");
    expect(normalizeDockIconMode("minimal")).toBe("minimal");
    expect(normalizeDockIconMode(" MINIMAL ")).toBe("minimal");
    expect(normalizeDockIconMode("other")).toBe(DEFAULT_DOCK_ICON_MODE);
    expect(normalizeDockIconMode(null)).toBe(DEFAULT_DOCK_ICON_MODE);
    expect(normalizeDockIconMode(undefined)).toBe(DEFAULT_DOCK_ICON_MODE);
  });

  it("returns null outside Tauri or on non-macOS platforms", () => {
    vi.stubGlobal("window", {});
    vi.stubGlobal("navigator", { platform: "MacIntel" });
    expect(resolveDockIconBridge()).toBeNull();

    vi.stubGlobal("window", {
      desktopBridge: {
        getClientPlatform: () => "linux",
        getDockIconMode: async () => "branded",
        setDockIconMode: async (mode: string) => mode,
      },
    });
    expect(resolveDockIconBridge()).toBeNull();
  });

  it("uses desktopBridge dock icon methods on macOS", async () => {
    const getDockIconMode = vi.fn(async () => "minimal");
    const setDockIconMode = vi.fn(async (mode: string) => mode);
    const onDockIconModeChange = vi.fn(() => () => {});

    vi.stubGlobal("window", {
      desktopBridge: {
        getClientPlatform: () => "darwin",
        getDockIconMode,
        setDockIconMode,
        onDockIconModeChange,
      },
    });

    const bridge = resolveDockIconBridge();
    expect(bridge).not.toBeNull();
    expect(await bridge?.getMode()).toBe("minimal");
    expect(await applyDockIconMode("minimal")).toBe("minimal");
    expect(setDockIconMode).toHaveBeenCalledWith("minimal");
  });

  it("falls back to Tauri invoke on macOS when desktopBridge methods are absent", async () => {
    const invoke = vi.fn(async (cmd: string, args?: Record<string, unknown>) => {
      if (cmd === "get_dock_icon_mode") return "branded";
      if (cmd === "set_dock_icon_mode") return args?.mode ?? "branded";
      return null;
    });

    vi.stubGlobal("navigator", { platform: "MacIntel" });
    vi.stubGlobal("window", {
      __TAURI_INTERNALS__: { invoke },
    });

    const bridge = resolveDockIconBridge();
    expect(bridge).not.toBeNull();
    expect(await bridge?.getMode()).toBe("branded");
    expect(await bridge?.setMode("minimal")).toBe("minimal");
    expect(invoke).toHaveBeenCalledWith("get_dock_icon_mode");
    expect(invoke).toHaveBeenCalledWith("set_dock_icon_mode", { mode: "minimal" });
  });

  it("reverts mode if setDockIconMode rejects", async () => {
    vi.stubGlobal("window", {
      desktopBridge: {
        getClientPlatform: () => "darwin",
        getDockIconMode: async () => "branded",
        setDockIconMode: async () => {
          throw new Error("IPC failed");
        },
      },
    });

    const result = await applyDockIconMode("minimal");
    expect(result).toBe("branded");
  });
});
