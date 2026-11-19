import { describe, expect, it, vi } from "vite-plus/test";
import {
  createFnVoiceController,
  createMicLeaseArbitrator,
  DEFAULT_MIC_LEASE_CHANNEL,
} from "./index.ts";

describe("createFnVoiceController", () => {
  it("starts push-to-talk when holding Fn and stops on Fn release", () => {
    vi.useFakeTimers();
    let clock = 1_000;
    let listening = false;
    const start = vi.fn(() => {
      listening = true;
    });
    const stop = vi.fn(() => {
      listening = false;
    });
    const toggle = vi.fn();

    const controller = createFnVoiceController({
      isListening: () => listening,
      start,
      stop,
      toggle,
      holdMs: 250,
      doubleTapMs: 320,
      now: () => clock,
    });

    controller.onKeyDown({ key: "Fn", code: "Fn" });
    expect(start).not.toHaveBeenCalled();

    clock += 260;
    vi.advanceTimersByTime(260);
    expect(start).toHaveBeenCalledTimes(1);
    expect(listening).toBe(true);

    controller.onKeyUp({ key: "Fn", code: "Fn" });
    expect(stop).toHaveBeenCalledTimes(1);
    expect(toggle).not.toHaveBeenCalled();
    controller.dispose();
    vi.useRealTimers();
  });

  it("toggles continuous hands-free voice on double-tap Fn and on ⇧⌘M / Ctrl+Shift+M", () => {
    vi.useFakeTimers();
    let clock = 2_000;
    let listening = false;
    const toggle = vi.fn(() => {
      listening = !listening;
    });
    const controller = createFnVoiceController({
      isListening: () => listening,
      start: vi.fn(),
      stop: vi.fn(),
      toggle,
      holdMs: 250,
      doubleTapMs: 320,
      now: () => clock,
    });

    controller.onKeyDown({ key: "Fn", code: "Fn" });
    clock += 80;
    controller.onKeyUp({ key: "Fn", code: "Fn" });
    expect(toggle).not.toHaveBeenCalled();

    clock += 120;
    controller.onKeyDown({ key: "Fn", code: "Fn" });
    clock += 70;
    controller.onKeyUp({ key: "Fn", code: "Fn" });
    expect(toggle).toHaveBeenCalledTimes(1);
    expect(listening).toBe(true);

    const preventDefault = vi.fn();
    controller.onKeyDown({
      key: "M",
      code: "KeyM",
      metaKey: true,
      shiftKey: true,
      preventDefault,
    });
    expect(preventDefault).toHaveBeenCalledTimes(1);
    expect(toggle).toHaveBeenCalledTimes(2);
    expect(listening).toBe(false);

    controller.dispose();
    vi.useRealTimers();
  });
});

describe("createMicLeaseArbitrator", () => {
  it("preempts the previous window over BroadcastChannel('hanzo-mic-lease') and ignores self-claims", () => {
    const preemptedA = vi.fn();
    const preemptedB = vi.fn();
    const windowA = createMicLeaseArbitrator({
      instanceId: "win-a",
      onPreempted: preemptedA,
    });
    const windowB = createMicLeaseArbitrator({
      instanceId: "win-b",
      onPreempted: preemptedB,
      channelName: DEFAULT_MIC_LEASE_CHANNEL,
    });

    windowA.claim();
    expect(windowA.isHolding()).toBe(true);

    windowA.handleIncoming({ type: "claim", owner: "win-a", at: 1 });
    expect(preemptedA).not.toHaveBeenCalled();
    expect(windowA.isHolding()).toBe(true);

    windowB.handleIncoming({ type: "claim", owner: "win-a", at: 1 });
    expect(preemptedB).not.toHaveBeenCalled();

    // Simulate BroadcastChannel / storage delivery from window B to window A
    windowB.claim();
    windowA.handleIncoming({ type: "claim", owner: "win-b", at: 2 });
    expect(preemptedA).toHaveBeenCalledTimes(1);
    expect(windowA.isHolding()).toBe(false);
    expect(windowB.isHolding()).toBe(true);

    windowB.release();
    expect(windowB.isHolding()).toBe(false);

    windowA.dispose();
    windowB.dispose();
  });
});
