import type { SystemSettingsPane, VoiceShortcutMode } from "@hanzodev/contracts";
import { T, useGT } from "gt-react";
import {
  CheckCircle2Icon,
  ChevronLeftIcon,
  ChevronRightIcon,
  ChevronsUpDownIcon,
  HardDriveIcon,
  MailIcon,
  MessageCircleIcon,
  MicIcon,
  MonitorIcon,
  ScanIcon,
  StickyNoteIcon,
} from "lucide-react";
import { useState } from "react";

import { updateClientSettings, useClientSettings } from "../../hooks/useSettings";
import { cn } from "../../lib/utils";
import { usePermissionStatus } from "../permissions/usePermissionStatus";

export type OnboardingStepId = "computer-use" | "files-and-apps" | "dictation" | "agents";

export const DESKTOP_ONBOARDING_STEPS: readonly OnboardingStepId[] = [
  "computer-use",
  "files-and-apps",
  "dictation",
  "agents",
];

type LocalAppAccessLevel = "read-and-interact" | "read-only" | "off";

type LocalAppId = "mail" | "messages" | "notes" | "whatsapp";

interface LocalAppEntry {
  readonly id: LocalAppId;
  readonly name: string;
  readonly badgeClass: string;
  readonly defaultLevel: LocalAppAccessLevel;
}

const LOCAL_APPS: readonly LocalAppEntry[] = [
  {
    id: "mail",
    name: "Mail.app",
    badgeClass: "bg-muted text-foreground border border-border/70",
    defaultLevel: "read-and-interact",
  },
  {
    id: "messages",
    name: "Messages.app",
    badgeClass: "bg-muted text-foreground border border-border/70",
    defaultLevel: "read-and-interact",
  },
  {
    id: "notes",
    name: "Notes.app",
    badgeClass: "bg-muted text-foreground border border-border/70",
    defaultLevel: "read-and-interact",
  },
  {
    id: "whatsapp",
    name: "WhatsApp.app",
    badgeClass: "bg-muted text-foreground border border-border/70",
    defaultLevel: "read-only",
  },
];

const LOCAL_APP_PERMISSIONS_STORAGE_KEY = "hanzo:onboarding:local-app-permissions:v1";
const DICTATION_SHORTCUTS_STORAGE_KEY = "hanzo:onboarding:dictation-shortcuts:v1";

type PushToTalkChoice = "fn" | "option-space" | "none";
type HandsFreeChoice = "none" | "double-fn" | "option-space";

interface DictationShortcutPrefs {
  readonly pushToTalk: PushToTalkChoice;
  readonly handsFree: HandsFreeChoice;
}

const WAVEFORM_BARS: ReadonlyArray<readonly [number, number, number]> = [
  [75, 47, 6],
  [80, 44, 12],
  [85, 46, 8],
  [90, 42, 16],
  [95, 45, 10],
  [100, 43, 14],
  [105, 41, 18],
  [110, 45, 10],
  [115, 43, 14],
  [120, 46, 8],
  [125, 48, 4],
];

function readLocalAppPermissions(): Record<LocalAppId, LocalAppAccessLevel> {
  const defaults: Record<LocalAppId, LocalAppAccessLevel> = {
    mail: "read-and-interact",
    messages: "read-and-interact",
    notes: "read-and-interact",
    whatsapp: "read-only",
  };
  try {
    const raw = window.localStorage.getItem(LOCAL_APP_PERMISSIONS_STORAGE_KEY);
    if (!raw) return defaults;
    const parsed = JSON.parse(raw) as Record<string, unknown>;
    const valid = (v: unknown): v is LocalAppAccessLevel =>
      v === "read-and-interact" || v === "read-only" || v === "off";
    return {
      mail: valid(parsed.mail) ? parsed.mail : defaults.mail,
      messages: valid(parsed.messages) ? parsed.messages : defaults.messages,
      notes: valid(parsed.notes) ? parsed.notes : defaults.notes,
      whatsapp: valid(parsed.whatsapp) ? parsed.whatsapp : defaults.whatsapp,
    };
  } catch {
    return defaults;
  }
}

function readDictationShortcuts(mode: VoiceShortcutMode): DictationShortcutPrefs {
  const fallback: DictationShortcutPrefs =
    mode === "toggle"
      ? { pushToTalk: "none", handsFree: "double-fn" }
      : { pushToTalk: "fn", handsFree: "none" };
  try {
    const raw = window.localStorage.getItem(DICTATION_SHORTCUTS_STORAGE_KEY);
    if (!raw) return fallback;
    const parsed = JSON.parse(raw) as Record<string, unknown>;
    const pushToTalk: PushToTalkChoice =
      parsed.pushToTalk === "fn" ||
      parsed.pushToTalk === "option-space" ||
      parsed.pushToTalk === "none"
        ? parsed.pushToTalk
        : fallback.pushToTalk;
    const handsFree: HandsFreeChoice =
      parsed.handsFree === "none" ||
      parsed.handsFree === "double-fn" ||
      parsed.handsFree === "option-space"
        ? parsed.handsFree
        : fallback.handsFree;
    return { pushToTalk, handsFree };
  } catch {
    return fallback;
  }
}

function resolveVoiceShortcutMode(prefs: DictationShortcutPrefs): VoiceShortcutMode {
  const hasHold = prefs.pushToTalk !== "none";
  const hasToggle = prefs.handsFree !== "none";
  if (hasHold && hasToggle) return "auto";
  if (hasToggle) return "toggle";
  return "hold";
}

async function checkDesktopPermission(pane: SystemSettingsPane): Promise<boolean> {
  const bridge = window.desktopBridge;
  if (!bridge?.checkSystemPermission) return false;
  try {
    return await bridge.checkSystemPermission(pane);
  } catch {
    return false;
  }
}

async function openDesktopPermissionSettings(pane: SystemSettingsPane): Promise<void> {
  const bridge = window.desktopBridge;
  if (!bridge?.openSystemSettings) return;
  await bridge.openSystemSettings(pane).catch(() => undefined);
}

export function OnboardingStepNav({
  stepIndex,
  stepCount,
  onPrev,
  onNext,
}: {
  readonly stepIndex: number;
  readonly stepCount: number;
  readonly onPrev: () => void;
  readonly onNext: () => void;
}) {
  const gt = useGT();
  const canPrev = stepIndex > 0;
  const canNext = stepIndex < stepCount - 1;
  return (
    <div className="inline-flex items-center overflow-hidden rounded-full border border-border/70 bg-muted/40">
      <button
        type="button"
        aria-label={gt("Previous step")}
        disabled={!canPrev}
        onClick={onPrev}
        className={cn(
          "flex h-7 w-8 items-center justify-center transition-colors",
          canPrev
            ? "text-foreground/85 hover:bg-muted/80"
            : "cursor-default text-muted-foreground/35",
        )}
      >
        <ChevronLeftIcon className="size-3.5" />
      </button>
      <span className="h-3.5 w-px bg-border/70" aria-hidden="true" />
      <button
        type="button"
        aria-label={gt("Next step")}
        disabled={!canNext}
        onClick={onNext}
        className={cn(
          "flex h-7 w-8 items-center justify-center transition-colors",
          canNext
            ? "text-foreground/85 hover:bg-muted/80"
            : "cursor-default text-muted-foreground/35",
        )}
      >
        <ChevronRightIcon className="size-3.5" />
      </button>
    </div>
  );
}

export function OnboardingStepDots({
  stepIndex,
  stepCount,
}: {
  readonly stepIndex: number;
  readonly stepCount: number;
  readonly onSelect?: (index: number) => void;
}) {
  const gt = useGT();
  const segments = 3;
  const done = stepIndex + 0.5;
  const pct = Math.max(0, Math.min(1, stepCount > 0 ? done / stepCount : 0));
  const reached = pct * segments;
  return (
    <div
      role="progressbar"
      aria-label={gt("Setup progress")}
      aria-valuemin={0}
      aria-valuemax={100}
      aria-valuenow={Math.round(pct * 100)}
      className="mx-auto mt-6 flex w-full max-w-md items-center gap-1.5"
    >
      {([0, 1, 2] as const).map((segment) => (
        <div
          key={segment}
          aria-hidden="true"
          className="h-1 flex-1 overflow-hidden rounded-full bg-border"
        >
          <div
            className="h-full rounded-full bg-foreground transition-[width] duration-200 ease-out"
            style={{ width: `${Math.max(0, Math.min(1, reached - segment)) * 100}%` }}
          />
        </div>
      ))}
    </div>
  );
}

function ComputerUseIllustration() {
  return (
    <svg
      viewBox="0 0 220 96"
      fill="none"
      aria-hidden="true"
      className="mx-auto mb-6 h-24 w-55 select-none"
    >
      {/* Left tilted window */}
      <g transform="translate(22 14) rotate(-6)">
        <rect
          width="74"
          height="58"
          rx="6"
          className="fill-zinc-25/90 stroke-border"
          strokeWidth="1"
        />
        <rect x="6" y="10" width="16" height="42" rx="2" className="fill-muted" />
        <circle cx="14" cy="18" r="2.5" className="fill-muted-foreground/50" />
        <circle cx="14" cy="26" r="2.5" className="fill-muted-foreground/50" />
        <circle cx="14" cy="34" r="2.5" className="fill-muted-foreground/50" />
        <rect x="28" y="14" width="36" height="4" rx="2" className="fill-muted-foreground/30" />
        <rect x="28" y="22" width="28" height="4" rx="2" className="fill-muted-foreground/20" />
        <rect x="28" y="30" width="32" height="4" rx="2" className="fill-muted-foreground/20" />
      </g>
      {/* Right tilted window */}
      <g transform="translate(132 10) rotate(5)">
        <rect
          width="68"
          height="56"
          rx="6"
          className="fill-zinc-25/90 stroke-border"
          strokeWidth="1"
        />
        <rect x="8" y="10" width="20" height="3.5" rx="1.5" className="fill-muted-foreground/30" />
        <rect x="8" y="18" width="46" height="4" rx="2" className="fill-muted-foreground/20" />
        <rect x="8" y="26" width="38" height="4" rx="2" className="fill-muted-foreground/20" />
        <rect x="8" y="34" width="42" height="4" rx="2" className="fill-muted-foreground/20" />
      </g>
      {/* Foreground active window */}
      <g transform="translate(58 20)">
        <rect
          width="104"
          height="68"
          rx="7"
          className="fill-zinc-25 stroke-foreground"
          strokeWidth="1.5"
        />
        <circle cx="9" cy="9" r="2" className="fill-muted-foreground/50" />
        <circle cx="16" cy="9" r="2" className="fill-muted-foreground/35" />
        <circle cx="23" cy="9" r="2" className="fill-muted-foreground/25" />
        <line x1="0" y1="16" x2="104" y2="16" className="stroke-border" strokeWidth="1" />
        <rect x="8" y="23" width="22" height="3.5" rx="1.5" className="fill-muted-foreground/35" />
        <rect x="8" y="31" width="18" height="3.5" rx="1.5" className="fill-muted-foreground/20" />
        <rect x="8" y="39" width="20" height="3.5" rx="1.5" className="fill-muted-foreground/20" />
        <rect x="38" y="23" width="54" height="3.5" rx="1.5" className="fill-muted-foreground/35" />
        <rect x="38" y="31" width="44" height="3.5" rx="1.5" className="fill-muted-foreground/20" />
        <rect x="38" y="39" width="48" height="3.5" rx="1.5" className="fill-muted-foreground/20" />
        <rect x="38" y="47" width="32" height="3.5" rx="1.5" className="fill-muted-foreground/20" />
        {/* Click burst + pointer */}
        <path
          d="M52 29V24M45 33L41 30M59 33L63 30M45 41L41 44"
          className="stroke-background"
          strokeWidth="1.4"
          strokeLinecap="round"
        />
        <path
          d="M52 35L62 43.5L56.8 44.3L54.2 49L52 35Z"
          className="fill-background stroke-zinc-25"
          strokeWidth="1"
          strokeLinejoin="round"
        />
      </g>
    </svg>
  );
}

function FilesAndAppsIllustration() {
  return (
    <div
      aria-hidden="true"
      className="mx-auto mb-6 flex h-16 items-center justify-center -space-x-2 select-none"
    >
      <div className="flex size-11 -rotate-6 items-center justify-center rounded-xl border border-border/70 bg-card text-foreground shadow-md">
        <MonitorIcon className="size-5" />
      </div>
      <div className="flex size-11 -rotate-2 items-center justify-center rounded-xl border border-border/70 bg-muted text-foreground shadow-md">
        <HardDriveIcon className="size-5" />
      </div>
      <div className="z-10 flex size-12 items-center justify-center rounded-xl bg-primary text-primary-foreground shadow-lg">
        <MessageCircleIcon className="size-6" />
      </div>
      <div className="flex size-11 rotate-2 items-center justify-center rounded-xl border border-border/70 bg-muted text-foreground shadow-md">
        <StickyNoteIcon className="size-5" />
      </div>
      <div className="flex size-11 rotate-6 items-center justify-center rounded-xl border border-border/70 bg-card text-foreground shadow-md">
        <MailIcon className="size-5" />
      </div>
    </div>
  );
}

function DictationIllustration() {
  return (
    <svg
      viewBox="0 0 180 92"
      fill="none"
      aria-hidden="true"
      className="mx-auto mb-6 h-24 w-45 select-none"
    >
      <rect x="46" y="10" width="76" height="72" rx="6" className="fill-zinc-25 stroke-border" />
      <circle cx="54" cy="18" r="1.5" className="fill-muted-foreground/40" />
      <circle cx="60" cy="18" r="1.5" className="fill-muted-foreground/40" />
      <circle cx="66" cy="18" r="1.5" className="fill-muted-foreground/40" />
      <rect x="55" y="27" width="42" height="3.5" rx="1.5" className="fill-muted-foreground/25" />
      <rect x="55" y="35" width="54" height="3.5" rx="1.5" className="fill-muted-foreground/25" />
      <rect x="55" y="43" width="38" height="3.5" rx="1.5" className="fill-muted-foreground/25" />
      <rect x="55" y="59" width="48" height="3.5" rx="1.5" className="fill-muted-foreground/25" />
      <rect x="55" y="67" width="30" height="3.5" rx="1.5" className="fill-muted-foreground/25" />
      {/* Floating waveform pill */}
      <g>
        <rect
          x="64"
          y="38"
          width="74"
          height="24"
          rx="12"
          className="fill-zinc-25 stroke-border"
          strokeWidth="1.2"
        />
        {WAVEFORM_BARS.map(([x, y, h]) => (
          <rect key={x} x={x} y={y} width="2.5" height={h} rx="1.25" className="fill-background" />
        ))}
      </g>
      {/* Pointer */}
      <path
        d="M57 52L66 59L61 60L59 64.5L57 52Z"
        className="fill-background stroke-zinc-25"
        strokeWidth="1"
        strokeLinejoin="round"
      />
    </svg>
  );
}

function LocalAppIcon({ id }: { readonly id: LocalAppId }) {
  switch (id) {
    case "mail":
      return <MailIcon className="size-3.5" />;
    case "messages":
      return <MessageCircleIcon className="size-3.5" />;
    case "notes":
      return <StickyNoteIcon className="size-3.5" />;
    case "whatsapp":
      return <MessageCircleIcon className="size-3.5" />;
  }
}

function PillAllowButton({
  onClick,
  busy = false,
}: {
  readonly onClick: () => void;
  readonly busy?: boolean;
}) {
  return (
    <button
      type="button"
      disabled={busy}
      onClick={onClick}
      className="inline-flex h-7 shrink-0 items-center justify-center rounded-full bg-primary px-3.5 text-xs font-medium text-primary-foreground transition-opacity hover:opacity-90 disabled:opacity-50"
    >
      <T context="verb, grant a permission">Allow</T>
    </button>
  );
}

function GrantedBadge() {
  const gt = useGT();
  return (
    <span
      role="status"
      aria-label={gt("Allowed", { $context: "permission status" })}
      className="flex size-6 shrink-0 items-center justify-center text-foreground"
    >
      <CheckCircle2Icon className="size-5 fill-foreground text-background" aria-hidden="true" />
    </span>
  );
}

function PrimaryContinueButton({ onClick }: { readonly onClick: () => void }) {
  return (
    <button
      type="button"
      onClick={onClick}
      className="inline-flex h-10 min-w-44 items-center justify-center rounded-full bg-primary px-7 text-sm font-medium text-primary-foreground shadow-sm transition-opacity hover:opacity-90"
    >
      <T>Continue</T>
    </button>
  );
}

export function OnboardingPermissionsStepView({
  step,
  stepIndex,
  stepCount,
  onSelectStep,
  onNext,
}: {
  readonly step: Exclude<OnboardingStepId, "agents">;
  readonly stepIndex: number;
  readonly stepCount: number;
  readonly onSelectStep: (index: number) => void;
  readonly onNext: () => void;
}) {
  const gt = useGT();
  const voiceShortcutMode = useClientSettings((settings) => settings.voiceShortcutMode);
  const [localAppPermissions, setLocalAppPermissions] = useState<
    Record<LocalAppId, LocalAppAccessLevel>
  >(readLocalAppPermissions);
  const [dictationShortcuts, setDictationShortcuts] = useState<DictationShortcutPrefs>(() =>
    readDictationShortcuts(voiceShortcutMode),
  );
  const [micRequesting, setMicRequesting] = useState(false);

  const permissions = usePermissionStatus(
    async () => {
      const [accessibility, screenRecording, fullDiskAccess, microphone] = await Promise.all([
        checkDesktopPermission("accessibility"),
        checkDesktopPermission("screen-recording"),
        checkDesktopPermission("full-disk-access"),
        checkDesktopPermission("microphone"),
      ]);
      return {
        accessibility,
        screenRecording,
        fullDiskAccess,
        microphone,
      };
    },
    {
      accessibility: false,
      screenRecording: false,
      fullDiskAccess: false,
      microphone: false,
    },
    true,
  );

  const updateLocalAppLevel = (id: LocalAppId, level: LocalAppAccessLevel) => {
    setLocalAppPermissions((prev) => {
      const next = { ...prev, [id]: level };
      try {
        window.localStorage.setItem(LOCAL_APP_PERMISSIONS_STORAGE_KEY, JSON.stringify(next));
      } catch {
        // Ignore quota errors on private/read-only storage.
      }
      return next;
    });
  };

  const updateDictationPrefs = (patch: Partial<DictationShortcutPrefs>) => {
    setDictationShortcuts((prev) => {
      const next: DictationShortcutPrefs = { ...prev, ...patch };
      try {
        window.localStorage.setItem(DICTATION_SHORTCUTS_STORAGE_KEY, JSON.stringify(next));
      } catch {
        // Ignore quota errors.
      }
      void updateClientSettings({ voiceShortcutMode: resolveVoiceShortcutMode(next) }).catch(
        () => undefined,
      );
      return next;
    });
  };

  const requestMicrophone = async () => {
    setMicRequesting(true);
    try {
      if (navigator.mediaDevices?.getUserMedia) {
        const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
        for (const track of stream.getTracks()) {
          track.stop();
        }
        return;
      }
    } catch {
      // Fall through to System Settings if denied or unavailable in WebView.
    } finally {
      setMicRequesting(false);
    }
    await openDesktopPermissionSettings("microphone");
  };

  const computerUseReady = permissions.status.accessibility && permissions.status.screenRecording;
  const fullDiskGranted = permissions.status.fullDiskAccess;

  return (
    <div className="mx-auto flex min-h-full w-full max-w-lg flex-col items-center justify-center px-6 py-12">
      {step === "computer-use" ? (
        <div className="w-full text-center">
          <ComputerUseIllustration />
          <h1 className="text-2xl font-semibold tracking-tight text-foreground">
            <T>Allow Hanzo Dev to use your computer?</T>
          </h1>
          <p className="mx-auto mt-2 max-w-md text-sm text-muted-foreground">
            <T>
              Ask Hanzo Dev to complete a task, then step away. Hanzo Dev has it ready when
              you&apos;re back.
            </T>
          </p>

          <div className="mt-7 overflow-hidden rounded-2xl border border-border/60 bg-card/70 text-left">
            <div className="flex items-center gap-3.5 px-4 py-3.5">
              <span className="flex size-8 shrink-0 items-center justify-center rounded-lg bg-muted/60 text-muted-foreground">
                <MonitorIcon className="size-4" />
              </span>
              <div className="min-w-0 flex-1">
                <div className="text-sm font-medium text-foreground">
                  <T>Accessibility</T>
                </div>
              </div>
              {permissions.status.accessibility ? (
                <GrantedBadge />
              ) : (
                <PillAllowButton
                  onClick={() => void openDesktopPermissionSettings("accessibility")}
                />
              )}
            </div>
            <div className="mx-4 h-px bg-border/60" />
            <div className="flex items-center gap-3.5 px-4 py-3.5">
              <span className="flex size-8 shrink-0 items-center justify-center rounded-lg bg-muted/60 text-muted-foreground">
                <ScanIcon className="size-4" />
              </span>
              <div className="min-w-0 flex-1">
                <div className="text-sm font-medium text-foreground">
                  <T>Screen Recording</T>
                </div>
                <div className="text-xs text-muted-foreground">
                  <T>Allow Hanzo Dev to take screenshots</T>
                </div>
              </div>
              {permissions.status.screenRecording ? (
                <GrantedBadge />
              ) : (
                <PillAllowButton
                  onClick={() => void openDesktopPermissionSettings("screen-recording")}
                />
              )}
            </div>
          </div>

          <p className="mt-3.5 px-1 text-left text-xs leading-relaxed text-muted-foreground">
            <T>You can manage computer use permissions and blocked apps any time in Settings.</T>
          </p>

          <div className="mt-8 flex flex-col items-center gap-2">
            {computerUseReady ? (
              <PrimaryContinueButton onClick={onNext} />
            ) : (
              <button
                type="button"
                onClick={onNext}
                className="px-4 py-1.5 text-sm font-medium text-foreground/90 transition-colors hover:text-foreground"
              >
                <T>Skip</T>
              </button>
            )}
          </div>
        </div>
      ) : null}

      {step === "files-and-apps" ? (
        <div className="w-full text-center">
          <FilesAndAppsIllustration />
          <h1 className="text-2xl font-semibold tracking-tight text-foreground">
            <T>Allow Hanzo Dev to access your files and local apps?</T>
          </h1>
          <p className="mx-auto mt-2 max-w-md text-sm text-muted-foreground">
            <T>
              You don&apos;t need to hunt through folders, notes and messages anymore. With your
              approval, Hanzo Dev can find, read and edit your files and local apps.
            </T>
          </p>

          {fullDiskGranted ? (
            <>
              <div className="mt-5 inline-flex items-center gap-2 rounded-full border border-border/60 bg-card/70 px-3.5 py-1.5 text-xs font-medium text-foreground">
                <HardDriveIcon className="size-3.5 text-muted-foreground" />
                <span>
                  <T>Full Disk Access</T>
                </span>
                <CheckCircle2Icon
                  className="size-3.5 fill-foreground text-background"
                  aria-hidden="true"
                />
              </div>

              <div className="mt-6 text-left">
                <div className="mb-2 px-1 text-xs font-medium text-muted-foreground">
                  <T>Select permissions for these apps</T>
                </div>
                <div className="overflow-hidden rounded-2xl border border-border/60 bg-card/70">
                  {LOCAL_APPS.map((app, index) => (
                    <div key={app.id}>
                      {index > 0 ? <div className="mx-4 h-px bg-border/60" /> : null}
                      <div className="flex items-center gap-3 px-4 py-3">
                        <span
                          className={cn(
                            "flex size-6 shrink-0 items-center justify-center rounded-md",
                            app.badgeClass,
                          )}
                        >
                          <LocalAppIcon id={app.id} />
                        </span>
                        <span className="min-w-0 flex-1 text-sm font-medium text-foreground">
                          {app.name}
                        </span>
                        <div className="relative inline-flex items-center">
                          <select
                            aria-label={app.name}
                            value={localAppPermissions[app.id]}
                            onChange={(event) =>
                              updateLocalAppLevel(
                                app.id,
                                event.target.value as LocalAppAccessLevel,
                              )
                            }
                            className="appearance-none bg-transparent pr-6 text-right text-xs font-medium text-foreground/90 outline-none"
                          >
                            <option value="read-and-interact" className="bg-popover text-foreground">
                              {gt("Read and interact")}
                            </option>
                            <option value="read-only" className="bg-popover text-foreground">
                              {gt("Read only")}
                            </option>
                            <option value="off" className="bg-popover text-foreground">
                              {gt("Off")}
                            </option>
                          </select>
                          <ChevronsUpDownIcon
                            className="pointer-events-none absolute right-0.5 size-3.5 text-muted-foreground"
                            aria-hidden="true"
                          />
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
                <p className="mt-3 px-1 text-xs leading-relaxed text-muted-foreground">
                  <T>
                    When you turn off an app here, Hanzo Dev will be blocked from accessing it.
                    Hanzo Dev can still access apps outside of this list which also store
                    information on your disk when requested.
                  </T>
                </p>
              </div>

              <div className="mt-7 flex justify-center">
                <PrimaryContinueButton onClick={onNext} />
              </div>
            </>
          ) : (
            <>
              <div className="mt-7 overflow-hidden rounded-2xl border border-border/60 bg-card/70 text-left">
                <div className="flex items-center gap-3.5 px-4 py-3.5">
                  <span className="flex size-8 shrink-0 items-center justify-center rounded-lg bg-muted/60 text-muted-foreground">
                    <HardDriveIcon className="size-4" />
                  </span>
                  <div className="min-w-0 flex-1">
                    <div className="text-sm font-medium text-foreground">
                      <T>Full Disk Access</T>
                    </div>
                    <div className="text-xs text-muted-foreground">
                      <T>Allow Hanzo Dev to access your files and local apps</T>
                    </div>
                  </div>
                  <PillAllowButton
                    onClick={() => void openDesktopPermissionSettings("full-disk-access")}
                  />
                </div>
              </div>

              <p className="mt-3.5 px-1 text-left text-xs leading-relaxed text-muted-foreground">
                <T>
                  You can turn on or off Full Disk Access and add blocked folders any time in
                  Settings.
                </T>
              </p>

              <div className="mt-8 flex justify-center">
                <button
                  type="button"
                  onClick={onNext}
                  className="px-4 py-1.5 text-sm font-medium text-foreground/90 transition-colors hover:text-foreground"
                >
                  <T>Skip</T>
                </button>
              </div>
            </>
          )}
        </div>
      ) : null}

      {step === "dictation" ? (
        <div className="w-full text-center">
          <DictationIllustration />
          <h1 className="text-2xl font-semibold tracking-tight text-foreground">
            <T>Enable dictation</T>
          </h1>
          <p className="mx-auto mt-2 max-w-md text-sm text-muted-foreground">
            <T>Turn speech to text using a seamless voice-first workflow.</T>
          </p>

          {permissions.status.microphone ? (
            <div className="mt-4 inline-flex items-center gap-2 rounded-full border border-border/60 bg-card/70 px-3.5 py-1 text-xs font-medium text-foreground">
              <MicIcon className="size-3.5 text-muted-foreground" />
              <span>
                <T>Microphone</T>
              </span>
              <CheckCircle2Icon
                className="size-3.5 fill-foreground text-background"
                aria-hidden="true"
              />
            </div>
          ) : (
            <div className="mt-6 overflow-hidden rounded-2xl border border-border/60 bg-card/70 text-left">
              <div className="flex items-center gap-3.5 px-4 py-3">
                <span className="flex size-8 shrink-0 items-center justify-center rounded-lg bg-muted/60 text-muted-foreground">
                  <MicIcon className="size-4" />
                </span>
                <div className="min-w-0 flex-1">
                  <div className="text-sm font-medium text-foreground">
                    <T>Microphone</T>
                  </div>
                  <div className="text-xs text-muted-foreground">
                    <T>Allow Hanzo Dev to use your microphone for dictation</T>
                  </div>
                </div>
                <PillAllowButton busy={micRequesting} onClick={() => void requestMicrophone()} />
              </div>
            </div>
          )}

          <div className="mt-6 text-left">
            <div className="mb-2 px-1 text-xs font-medium text-muted-foreground">
              <T>Shortcuts</T>
            </div>
            <div className="overflow-hidden rounded-2xl border border-border/60 bg-card/70">
              <div className="flex items-center gap-3 px-4 py-3.5">
                <div className="min-w-0 flex-1">
                  <div className="text-sm font-medium text-foreground">
                    <T>Push to talk</T>
                  </div>
                  <div className="text-xs text-muted-foreground">
                    <T>Hold the key to dictate</T>
                  </div>
                </div>
                <div className="relative inline-flex items-center">
                  <select
                    aria-label={gt("Push to talk")}
                    value={dictationShortcuts.pushToTalk}
                    onChange={(event) =>
                      updateDictationPrefs({
                        pushToTalk: event.target.value as PushToTalkChoice,
                      })
                    }
                    className="appearance-none bg-transparent pr-6 text-right text-xs font-medium text-foreground/90 outline-none"
                  >
                    <option value="fn" className="bg-popover text-foreground">
                      {gt("Fn", { $context: "keyboard key name" })}
                    </option>
                    <option value="option-space" className="bg-popover text-foreground">
                      {gt("Option + Space", { $context: "keyboard shortcut" })}
                    </option>
                    <option value="none" className="bg-popover text-foreground">
                      {gt("No shortcut")}
                    </option>
                  </select>
                  <ChevronsUpDownIcon
                    className="pointer-events-none absolute right-0.5 size-3.5 text-muted-foreground"
                    aria-hidden="true"
                  />
                </div>
              </div>
              <div className="mx-4 h-px bg-border/60" />
              <div className="flex items-center gap-3 px-4 py-3.5">
                <div className="min-w-0 flex-1">
                  <div className="text-sm font-medium text-foreground">
                    <T>Hands-free mode</T>
                  </div>
                  <div className="text-xs text-muted-foreground">
                    <T>Press the key to start and stop dictation without holding</T>
                  </div>
                </div>
                <div className="relative inline-flex items-center">
                  <select
                    aria-label={gt("Hands-free mode")}
                    value={dictationShortcuts.handsFree}
                    onChange={(event) =>
                      updateDictationPrefs({
                        handsFree: event.target.value as HandsFreeChoice,
                      })
                    }
                    className="appearance-none bg-transparent pr-6 text-right text-xs font-medium text-foreground/90 outline-none"
                  >
                    <option value="none" className="bg-popover text-foreground">
                      {gt("No shortcut")}
                    </option>
                    <option value="double-fn" className="bg-popover text-foreground">
                      {gt("Double-tap Fn")}
                    </option>
                    <option value="option-space" className="bg-popover text-foreground">
                      {gt("Option + Space", { $context: "keyboard shortcut" })}
                    </option>
                  </select>
                  <ChevronsUpDownIcon
                    className="pointer-events-none absolute right-0.5 size-3.5 text-muted-foreground"
                    aria-hidden="true"
                  />
                </div>
              </div>
            </div>
          </div>

          <div className="mt-8 flex justify-center">
            <PrimaryContinueButton onClick={onNext} />
          </div>
        </div>
      ) : null}

      <OnboardingStepDots
        stepIndex={stepIndex}
        stepCount={stepCount}
        onSelect={onSelectStep}
      />
    </div>
  );
}
