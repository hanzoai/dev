import { T, useGT } from "gt-react";
import { useEffect, useState, type CSSProperties } from "react";

import { HanzoIcon } from "../components/Icons";
import {
  dragDesktopWindow,
  toggleDesktopMaximize,
} from "../components/desktop/DesktopWindowControls";
import { isElectron, isTauri } from "../env";
import { ensureHanzoThemeChromeStylesheet } from "../lib/windowControlsOverlay";
import type { HanzoWebAuthController } from "./hanzoWebAuth";

export const HANZO_DEV_APP_ICON_DATA_URL =
  "data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20width%3D%221024%22%20height%3D%221024%22%20viewBox%3D%220%200%201024%201024%22%3E%3Cdefs%3E%3ClinearGradient%20id%3D%22bg%22%20x1%3D%220%22%20y1%3D%220%22%20x2%3D%220%22%20y2%3D%221%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%2319191b%22%2F%3E%3Cstop%20offset%3D%2228%25%22%20stop-color%3D%22%2309090a%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23000000%22%2F%3E%3C%2FlinearGradient%3E%3CradialGradient%20id%3D%22topGlow%22%20cx%3D%2250%25%22%20cy%3D%226%25%22%20r%3D%2265%25%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.14%22%2F%3E%3Cstop%20offset%3D%2245%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.03%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220%22%2F%3E%3C%2FradialGradient%3E%3ClinearGradient%20id%3D%22rimGrad%22%20x1%3D%220%22%20y1%3D%220%22%20x2%3D%220%22%20y2%3D%221%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.34%22%2F%3E%3Cstop%20offset%3D%2222%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.12%22%2F%3E%3Cstop%20offset%3D%2275%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.05%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.10%22%2F%3E%3C%2FlinearGradient%3E%3ClinearGradient%20id%3D%22glyphWhite%22%20gradientUnits%3D%22userSpaceOnUse%22%20x1%3D%220%22%20y1%3D%22220%22%20x2%3D%220%22%20y2%3D%22800%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%23ffffff%22%2F%3E%3Cstop%20offset%3D%2275%25%22%20stop-color%3D%22%23f4f4f7%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23d8d8e0%22%2F%3E%3C%2FlinearGradient%3E%3ClinearGradient%20id%3D%22glyphShade%22%20x1%3D%220%22%20y1%3D%220%22%20x2%3D%221%22%20y2%3D%221%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%23ffffff%22%2F%3E%3Cstop%20offset%3D%2265%25%22%20stop-color%3D%22%23ececf0%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23d2d2da%22%2F%3E%3C%2FlinearGradient%3E%3CclipPath%20id%3D%22shellClip%22%3E%3Cpath%20d%3D%22M%20248.00%2C24.00%20L%20776.00%2C24.00%20C%20905.92%2C24.00%201000.00%2C118.08%201000.00%2C248.00%20L%201000.00%2C776.00%20C%201000.00%2C905.92%20905.92%2C1000.00%20776.00%2C1000.00%20L%20248.00%2C1000.00%20C%20118.08%2C1000.00%2024.00%2C905.92%2024.00%2C776.00%20L%2024.00%2C248.00%20C%2024.00%2C118.08%20118.08%2C24.00%20248.00%2C24.00%20Z%22%2F%3E%3C%2FclipPath%3E%3C%2Fdefs%3E%3Cpath%20d%3D%22M%20248.00%2C24.00%20L%20776.00%2C24.00%20C%20905.92%2C24.00%201000.00%2C118.08%201000.00%2C248.00%20L%201000.00%2C776.00%20C%201000.00%2C905.92%20905.92%2C1000.00%20776.00%2C1000.00%20L%20248.00%2C1000.00%20C%20118.08%2C1000.00%2024.00%2C905.92%2024.00%2C776.00%20L%2024.00%2C248.00%20C%2024.00%2C118.08%20118.08%2C24.00%20248.00%2C24.00%20Z%22%20fill%3D%22url(%23bg)%22%2F%3E%3Cpath%20d%3D%22M%20248.00%2C24.00%20L%20776.00%2C24.00%20C%20905.92%2C24.00%201000.00%2C118.08%201000.00%2C248.00%20L%201000.00%2C776.00%20C%201000.00%2C905.92%20905.92%2C1000.00%20776.00%2C1000.00%20L%20248.00%2C1000.00%20C%20118.08%2C1000.00%2024.00%2C905.92%2024.00%2C776.00%20L%2024.00%2C248.00%20C%2024.00%2C118.08%20118.08%2C24.00%20248.00%2C24.00%20Z%22%20fill%3D%22url(%23topGlow)%22%2F%3E%3Cpath%20d%3D%22M%20249.00%2C31.00%20L%20775.00%2C31.00%20C%20901.44%2C31.00%20993.00%2C122.56%20993.00%2C249.00%20L%20993.00%2C775.00%20C%20993.00%2C901.44%20901.44%2C993.00%20775.00%2C993.00%20L%20249.00%2C993.00%20C%20122.56%2C993.00%2031.00%2C901.44%2031.00%2C775.00%20L%2031.00%2C249.00%20C%2031.00%2C122.56%20122.56%2C31.00%20249.00%2C31.00%20Z%22%20fill%3D%22none%22%20stroke%3D%22url(%23rimGrad)%22%20stroke-width%3D%2210%22%2F%3E%3Cg%20clip-path%3D%22url(%23shellClip)%22%3E%3Cpath%20d%3D%22M%20224%20344%20L%20448%20512%20L%20224%20680%22%20fill%3D%22none%22%20stroke%3D%22url(%23glyphWhite)%22%20stroke-width%3D%2292%22%20stroke-linecap%3D%22round%22%20stroke-linejoin%3D%22round%22%2F%3E%3Crect%20x%3D%22504%22%20y%3D%22628%22%20width%3D%22296%22%20height%3D%2292%22%20rx%3D%2246%22%20fill%3D%22url(%23glyphWhite)%22%2F%3E%3C%2Fg%3E%3C%2Fsvg%3E";

const HANZO_APP_ALT = "Hanzo Dev";
const SKIP_SIGNIN_STORAGE_KEY = "hanzo.dev.skip_signin";

function isLocalOrDesktopHost(): boolean {
  if (isElectron || isTauri) return true;
  if (typeof window === "undefined") return false;
  const host = window.location.hostname;
  return host === "localhost" || host === "127.0.0.1";
}

export function readSkippedSignIn(): boolean {
  if (typeof window === "undefined") return false;
  try {
    return window.localStorage.getItem(SKIP_SIGNIN_STORAGE_KEY) === "1";
  } catch {
    return false;
  }
}

export function writeSkippedSignIn(skipped: boolean): void {
  if (typeof window === "undefined") return;
  try {
    if (skipped) window.localStorage.setItem(SKIP_SIGNIN_STORAGE_KEY, "1");
    else window.localStorage.removeItem(SKIP_SIGNIN_STORAGE_KEY);
  } catch {
    // ignore storage failures
  }
}

export function HanzoDevAppIcon({ size = 64 }: { readonly size?: number }) {
  return (
    <img
      src={HANZO_DEV_APP_ICON_DATA_URL}
      width={size}
      height={size}
      alt={HANZO_APP_ALT}
      data-hanzo-app-icon="hanzo-dev"
      style={{ display: "block", width: size, height: size, flexShrink: 0 }}
    />
  );
}

export function HanzoLoginMasthead({
  desktop = false,
}: {
  readonly desktop?: boolean;
  readonly macDesktop?: boolean;
}) {
  return (
    <header
      data-slot="masthead"
      data-tauri-drag-region={desktop ? "" : undefined}
      className={desktop ? "drag-region" : undefined}
      onMouseDown={desktop ? dragDesktopWindow : undefined}
      onDoubleClick={desktop ? toggleDesktopMaximize : undefined}
      style={{
        width: "100%",
        height: 44,
        paddingInline: 14,
        boxSizing: "border-box",
        display: "flex",
        alignItems: "center",
        justifyContent: "flex-end",
        flexShrink: 0,
      }}
    />
  );
}

export function HanzoLoginFooter() {
  return (
    <footer data-slot="footer" className="flex shrink-0 items-center justify-center px-6 pt-2">
      <HanzoIcon
        width={20}
        height={20}
        style={{ color: "var(--hz-login-fg, #fafafa)", opacity: 0.9 }}
        aria-hidden="true"
      />
    </footer>
  );
}

const GOOGLE_ICON_DATA_URL =
  "data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20width%3D%2224%22%20height%3D%2224%22%20viewBox%3D%220%200%2024%2024%22%3E%3Cpath%20fill%3D%22%234285F4%22%20d%3D%22M23.49%2012.27c0-.79-.07-1.54-.19-2.27H12v4.51h6.47c-.29%201.48-1.14%202.73-2.4%203.58v3h3.86c2.26-2.09%203.56-5.17%203.56-8.82Z%22%2F%3E%3Cpath%20fill%3D%22%2334A853%22%20d%3D%22M12%2024c3.24%200%205.95-1.08%207.93-2.91l-3.86-3c-1.08.72-2.45%201.16-4.07%201.16-3.13%200-5.78-2.11-6.73-4.96H1.29v3.09C3.26%2021.3%207.31%2024%2012%2024Z%22%2F%3E%3Cpath%20fill%3D%22%23FBBC05%22%20d%3D%22M5.27%2014.29A7.12%207.12%200%200%201%204.9%2012c0-.8.14-1.57.37-2.29V6.62H1.29A11.99%2011.99%200%200%200%200%2012c0%201.92.46%203.74%201.29%205.38l3.98-3.09Z%22%2F%3E%3Cpath%20fill%3D%22%23EA4335%22%20d%3D%22M12%204.75c1.77%200%203.35.61%204.6%201.8l3.42-3.42C17.95%201.19%2015.24%200%2012%200%207.31%200%203.26%202.7%201.29%206.62l3.98%203.09c.95-2.85%203.6-4.96%206.73-4.96Z%22%2F%3E%3C%2Fsvg%3E";

function GoogleGlyph() {
  return <img src={GOOGLE_ICON_DATA_URL} width={16} height={16} alt="" aria-hidden="true" />;
}

function GitHubGlyph() {
  return (
    <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">
      <path d="M12 .5C5.65.5.5 5.66.5 12.02c0 5.09 3.29 9.41 7.86 10.94.58.1.79-.25.79-.56v-2.02c-3.2.7-3.87-1.39-3.87-1.39-.52-1.33-1.28-1.69-1.28-1.69-1.05-.72.08-.71.08-.71 1.16.08 1.77 1.2 1.77 1.2 1.03 1.77 2.7 1.26 3.36.96.1-.75.4-1.26.73-1.55-2.55-.29-5.24-1.28-5.24-5.7 0-1.26.45-2.29 1.19-3.1-.12-.29-.52-1.47.11-3.06 0 0 .97-.31 3.18 1.18a11.05 11.05 0 0 1 5.8 0c2.2-1.49 3.17-1.18 3.17-1.18.63 1.59.23 2.77.12 3.06.74.81 1.18 1.84 1.18 3.1 0 4.43-2.69 5.4-5.25 5.69.41.36.78 1.06.78 2.14v3.17c0 .31.21.67.8.56A11.53 11.53 0 0 0 23.5 12.02C23.5 5.66 18.35.5 12 .5Z" />
    </svg>
  );
}

const SOCIAL_BTN_STYLE: CSSProperties = {
  height: 40,
  width: "100%",
  borderRadius: 10,
  border: "1px solid var(--hz-login-secondary-border, rgba(255, 255, 255, 0.12))",
  backgroundColor: "var(--hz-login-secondary-bg, #141417)",
  color: "var(--hz-login-secondary-fg, #fafafa)",
  fontSize: 13.5,
  fontWeight: 500,
  display: "inline-flex",
  alignItems: "center",
  justifyContent: "center",
  gap: 8,
  cursor: "pointer",
  transition: "opacity 0.14s ease",
};

export function HanzoSignInPanel({
  controller,
  isBusy,
  pendingAuthorizeUrl,
  errorMessage,
  bare = false,
  onSignedIn,
  onContinueWithoutLogin,
}: {
  readonly controller: HanzoWebAuthController;
  readonly isBusy: boolean;
  readonly pendingAuthorizeUrl: string | null;
  readonly errorMessage: string | null;
  readonly bare?: boolean;
  readonly onSignedIn?: () => void;
  readonly onContinueWithoutLogin?: () => void;
}) {
  const gt = useGT();
  const [showTokenEntry, setShowTokenEntry] = useState(false);
  const [tokenInput, setTokenInput] = useState("");
  const [callbackUrlInput, setCallbackUrlInput] = useState("");
  const [localError, setLocalError] = useState<string | null>(null);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- ensures light/dark login CSS variables are mounted
  useEffect(() => {
    ensureHanzoThemeChromeStylesheet();
  }, []);

  const handleTokenSubmit = async () => {
    setLocalError(null);
    try {
      await controller.signInWithAccessToken({ accessToken: tokenInput });
      setTokenInput("");
      setShowTokenEntry(false);
      onSignedIn?.();
    } catch (error) {
      setLocalError(error instanceof Error ? error.message : String(error));
    }
  };

  const canSkip = onContinueWithoutLogin !== undefined || isLocalOrDesktopHost();
  const error = localError ?? errorMessage;

  return (
    <div
      data-testid="hanzo-sign-in"
      className="flex w-full flex-col items-center gap-4 text-center"
      style={{
        maxWidth: 360,
        padding: bare ? "8px 16px" : "20px 24px",
        borderRadius: bare ? 0 : 16,
        border: bare ? "none" : "1px solid var(--hz-login-card-border, rgba(255, 255, 255, 0.10))",
        backgroundColor: bare ? "transparent" : "var(--hz-login-bg, #000000)",
        color: "var(--hz-login-fg, #fafafa)",
      }}
    >
      {bare ? null : <div className="hidden" data-slot="masthead" />}

      <HanzoDevAppIcon size={64} />

      <div className="space-y-1.5">
        <h2
          className="text-xl font-semibold tracking-tight"
          style={{ color: "var(--hz-login-fg, #fafafa)" }}
        >
          <T>Sign in to Hanzo Dev</T>
        </h2>
        <p className="text-xs" style={{ color: "var(--hz-login-muted, #a1a1aa)" }}>
          <T>Cloud coding agents, local workspaces, and connected computers.</T>
        </p>
      </div>

      {error ? (
        <div className="w-full rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-left text-xs text-destructive-foreground">
          {error}
        </div>
      ) : null}

      <div className="flex w-full flex-col gap-2.5">
        <button
          type="button"
          disabled={isBusy}
          onClick={() => void controller.startOAuthSignIn()}
          style={SOCIAL_BTN_STYLE}
          className="disabled:opacity-50"
        >
          <GoogleGlyph />
          <T>Continue with Google</T>
        </button>

        <button
          type="button"
          disabled={isBusy}
          onClick={() => void controller.startOAuthSignIn()}
          style={SOCIAL_BTN_STYLE}
          className="disabled:opacity-50"
        >
          <GitHubGlyph />
          <T>Continue with GitHub</T>
        </button>

        <div className="flex w-full items-center gap-3 py-0.5">
          <span
            style={{
              flex: 1,
              height: 1,
              backgroundColor: "var(--hz-login-card-border, rgba(255, 255, 255, 0.10))",
            }}
          />
          <span className="text-xs" style={{ color: "var(--hz-login-muted, #a1a1aa)" }}>
            <T>or</T>
          </span>
          <span
            style={{
              flex: 1,
              height: 1,
              backgroundColor: "var(--hz-login-card-border, rgba(255, 255, 255, 0.10))",
            }}
          />
        </div>

        <button
          type="button"
          disabled={isBusy}
          onClick={() => void controller.startOAuthSignIn()}
          style={{
            height: 40,
            width: "100%",
            borderRadius: 10,
            border: "none",
            backgroundColor: "var(--hz-login-primary-bg, #ffffff)",
            color: "var(--hz-login-primary-fg, #09090b)",
            fontSize: 13.5,
            fontWeight: 600,
            cursor: "pointer",
          }}
          className="disabled:opacity-50"
        >
          <T>Continue with email</T>
        </button>

        {canSkip ? (
          <button
            id="continue-without-login"
            type="button"
            onClick={() => {
              writeSkippedSignIn(true);
              onContinueWithoutLogin?.();
              controller.closeModal();
            }}
            style={{
              height: 40,
              width: "100%",
              borderRadius: 10,
              border: "1px solid var(--hz-login-guest-border, rgba(255, 255, 255, 0.14))",
              backgroundColor: "var(--hz-login-guest-bg, #121214)",
              color: "var(--hz-login-guest-fg, #e4e4e7)",
              fontSize: 13.5,
              fontWeight: 500,
              cursor: "pointer",
            }}
          >
            <T>Continue without signing in</T>
          </button>
        ) : null}
      </div>

      <button
        type="button"
        onClick={() => setShowTokenEntry((prev) => !prev)}
        className="text-xs underline underline-offset-2"
        style={{ color: "var(--hz-login-muted, #a1a1aa)" }}
      >
        {showTokenEntry ? <T>Hide API key</T> : <T>Use an API key</T>}
      </button>

      {pendingAuthorizeUrl ? (
        <div
          data-testid="pending-oauth-recovery"
          className="w-full space-y-2 rounded-lg p-3 text-left text-xs"
          style={{ border: "1px solid var(--hz-login-card-border, rgba(255, 255, 255, 0.14))" }}
        >
          <div className="flex flex-wrap items-center justify-between gap-2">
            <span className="font-medium">
              <T>Waiting for browser sign-in to return via hanzodev://...</T>
            </span>
            <div className="flex items-center gap-1.5">
              <button
                type="button"
                onClick={controller.reopenPendingSignIn}
                className="rounded-md px-2.5 py-1 text-xs font-medium"
                style={{
                  border: "1px solid var(--hz-login-card-border, rgba(255, 255, 255, 0.18))",
                }}
              >
                <T>Reopen browser</T>
              </button>
              <button
                type="button"
                onClick={controller.cancelPendingSignIn}
                className="rounded-md px-2.5 py-1 text-xs"
                style={{
                  border: "1px solid var(--hz-login-card-border, rgba(255, 255, 255, 0.18))",
                }}
              >
                <T>Cancel</T>
              </button>
            </div>
          </div>
          <div className="flex gap-2">
            <input
              type="text"
              value={callbackUrlInput}
              onChange={(event) => setCallbackUrlInput(event.currentTarget.value)}
              placeholder="hanzodev://oauth/callback?code=...&state=..."
              aria-label={gt("Paste OAuth callback URL")}
              className="min-w-0 flex-1 rounded-md bg-transparent px-2.5 py-1 font-mono text-xs focus:outline-hidden"
              style={{ border: "1px solid var(--hz-login-card-border, rgba(255, 255, 255, 0.18))" }}
            />
            <button
              type="button"
              disabled={!callbackUrlInput.trim() || isBusy}
              onClick={() => {
                const raw = callbackUrlInput.trim();
                setCallbackUrlInput("");
                void controller.handleDeepLinkCallback(raw);
              }}
              className="rounded-md px-2.5 py-1 text-xs font-semibold disabled:opacity-50"
              style={{
                backgroundColor: "var(--hz-login-primary-bg, #ffffff)",
                color: "var(--hz-login-primary-fg, #09090b)",
              }}
            >
              <T>Complete</T>
            </button>
          </div>
        </div>
      ) : null}

      {showTokenEntry ? (
        <div className="w-full space-y-2">
          <input
            type="password"
            value={tokenInput}
            onChange={(event) => setTokenInput(event.target.value)}
            placeholder={gt("Paste your API key")}
            aria-label={gt("Hanzo API key")}
            className="w-full rounded-lg bg-transparent px-3 py-2 font-mono text-xs focus:outline-hidden"
            style={{ border: "1px solid var(--hz-login-card-border, rgba(255, 255, 255, 0.18))" }}
          />
          <button
            type="button"
            disabled={isBusy || !tokenInput.trim()}
            onClick={() => void handleTokenSubmit()}
            className="w-full rounded-lg px-3 py-1.5 text-xs font-semibold disabled:opacity-50"
            style={{ border: "1px solid var(--hz-login-card-border, rgba(255, 255, 255, 0.18))" }}
          >
            <T>Save API key</T>
          </button>
        </div>
      ) : null}

      <HanzoLoginFooter />
    </div>
  );
}
