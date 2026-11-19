// @effect-diagnostics globalDate:off globalTimers:off -- Keyboard gesture thresholds run synchronously in DOM key listeners outside an Effect runtime.

export interface FnVoiceKeyEventLike {
  readonly key: string;
  readonly code?: string | undefined;
  readonly repeat?: boolean | undefined;
  readonly metaKey?: boolean | undefined;
  readonly ctrlKey?: boolean | undefined;
  readonly shiftKey?: boolean | undefined;
  readonly altKey?: boolean | undefined;
  readonly preventDefault?: (() => void) | undefined;
}

export interface FnVoiceHooks {
  readonly isListening: () => boolean;
  readonly start: () => void;
  readonly stop: () => void;
  readonly toggle: () => void;
  readonly holdMs?: number | undefined;
  readonly doubleTapMs?: number | undefined;
  readonly enableToggleChord?: boolean | undefined;
  readonly now?: (() => number) | undefined;
}

export interface FnVoiceController {
  readonly onKeyDown: (event: FnVoiceKeyEventLike) => boolean;
  readonly onKeyUp: (event: FnVoiceKeyEventLike) => boolean;
  readonly cancelHold: () => void;
  readonly dispose: () => void;
}

function isFnKeyEvent(event: Pick<FnVoiceKeyEventLike, "key" | "code">): boolean {
  return event.key === "Fn" || event.code === "Fn";
}

function isVoiceToggleChord(event: FnVoiceKeyEventLike): boolean {
  const hasPrimaryModifier = Boolean(event.metaKey || event.ctrlKey);
  if (!hasPrimaryModifier || !event.shiftKey || event.altKey) return false;
  return event.key.toLowerCase() === "m" || event.code === "KeyM";
}

/**
 * Arbitrates `Fn` key gestures and `⇧⌘M` / `Ctrl+Shift+M` toggle for voice input:
 * - Hold `Fn` (>= holdMs, default 250ms): starts push-to-talk on hold, stops on keyup.
 * - Double-tap `Fn` (two taps within doubleTapMs, default 320ms): toggles hands-free listening.
 * - `⇧⌘M` / `Ctrl+Shift+M` (when `enableToggleChord` is true, the default): toggles voice input.
 */
export function createFnVoiceController(hooks: FnVoiceHooks): FnVoiceController {
  const holdMs = hooks.holdMs ?? 250;
  const doubleTapMs = hooks.doubleTapMs ?? 320;
  const enableToggleChord = hooks.enableToggleChord ?? true;
  const readNow = hooks.now ?? (() => Date.now());

  let downAt: number | null = null;
  let holdTimer: ReturnType<typeof setTimeout> | null = null;
  let heldActive = false;
  let lastTapAt: number | null = null;

  const clearHoldTimer = () => {
    if (holdTimer !== null) {
      clearTimeout(holdTimer);
      holdTimer = null;
    }
  };

  const onKeyDown = (event: FnVoiceKeyEventLike): boolean => {
    if (enableToggleChord && isVoiceToggleChord(event)) {
      event.preventDefault?.();
      if (!event.repeat) {
        clearHoldTimer();
        downAt = null;
        heldActive = false;
        hooks.toggle();
      }
      return true;
    }

    if (!isFnKeyEvent(event)) return false;
    if (event.repeat || downAt !== null) return true;

    downAt = readNow();
    heldActive = false;
    clearHoldTimer();
    holdTimer = setTimeout(() => {
      holdTimer = null;
      if (downAt !== null && !hooks.isListening()) {
        heldActive = true;
        hooks.start();
      }
    }, holdMs);
    return true;
  };

  const onKeyUp = (event: FnVoiceKeyEventLike): boolean => {
    if (!isFnKeyEvent(event) || downAt === null) return false;
    const now = readNow();
    const elapsed = Math.max(0, now - downAt);
    downAt = null;
    clearHoldTimer();

    if (heldActive) {
      heldActive = false;
      if (hooks.isListening()) {
        hooks.stop();
      }
      return true;
    }

    if (elapsed < holdMs) {
      if (lastTapAt !== null && now - lastTapAt <= doubleTapMs) {
        lastTapAt = null;
        hooks.toggle();
      } else {
        lastTapAt = now;
      }
    }
    return true;
  };

  const cancelHold = () => {
    clearHoldTimer();
    downAt = null;
    heldActive = false;
  };

  const dispose = () => {
    cancelHold();
    lastTapAt = null;
  };

  return { onKeyDown, onKeyUp, cancelHold, dispose };
}
