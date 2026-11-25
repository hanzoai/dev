import { T, useGT } from "gt-react";
import { useState, type CSSProperties } from "react";

import { HanzoIcon } from "../components/Icons";
import {
  dragDesktopWindow,
  toggleDesktopMaximize,
} from "../components/desktop/DesktopWindowControls";
import type { HanzoWebAuthController } from "./hanzoWebAuth";

export const HANZO_DEV_APP_ICON_DATA_URL =
  "data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20width%3D%221024%22%20height%3D%221024%22%20viewBox%3D%220%200%201024%201024%22%3E%3Cdefs%3E%3ClinearGradient%20id%3D%22bg%22%20x1%3D%220%22%20y1%3D%220%22%20x2%3D%220%22%20y2%3D%221%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%2319191b%22%2F%3E%3Cstop%20offset%3D%2228%25%22%20stop-color%3D%22%2309090a%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23000000%22%2F%3E%3C%2FlinearGradient%3E%3CradialGradient%20id%3D%22topGlow%22%20cx%3D%2250%25%22%20cy%3D%226%25%22%20r%3D%2265%25%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.14%22%2F%3E%3Cstop%20offset%3D%2245%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.03%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220%22%2F%3E%3C%2FradialGradient%3E%3ClinearGradient%20id%3D%22rimGrad%22%20x1%3D%220%22%20y1%3D%220%22%20x2%3D%220%22%20y2%3D%221%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.34%22%2F%3E%3Cstop%20offset%3D%2222%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.12%22%2F%3E%3Cstop%20offset%3D%2275%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.05%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23ffffff%22%20stop-opacity%3D%220.10%22%2F%3E%3C%2FlinearGradient%3E%3ClinearGradient%20id%3D%22glyphWhite%22%20gradientUnits%3D%22userSpaceOnUse%22%20x1%3D%220%22%20y1%3D%22220%22%20x2%3D%220%22%20y2%3D%22800%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%23ffffff%22%2F%3E%3Cstop%20offset%3D%2275%25%22%20stop-color%3D%22%23f4f4f7%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23d8d8e0%22%2F%3E%3C%2FlinearGradient%3E%3ClinearGradient%20id%3D%22glyphShade%22%20x1%3D%220%22%20y1%3D%220%22%20x2%3D%221%22%20y2%3D%221%22%3E%3Cstop%20offset%3D%220%25%22%20stop-color%3D%22%23ffffff%22%2F%3E%3Cstop%20offset%3D%2265%25%22%20stop-color%3D%22%23ececf0%22%2F%3E%3Cstop%20offset%3D%22100%25%22%20stop-color%3D%22%23d2d2da%22%2F%3E%3C%2FlinearGradient%3E%3CclipPath%20id%3D%22shellClip%22%3E%3Cpath%20d%3D%22M%20248.00%2C24.00%20L%20776.00%2C24.00%20C%20905.92%2C24.00%201000.00%2C118.08%201000.00%2C248.00%20L%201000.00%2C776.00%20C%201000.00%2C905.92%20905.92%2C1000.00%20776.00%2C1000.00%20L%20248.00%2C1000.00%20C%20118.08%2C1000.00%2024.00%2C905.92%2024.00%2C776.00%20L%2024.00%2C248.00%20C%2024.00%2C118.08%20118.08%2C24.00%20248.00%2C24.00%20Z%22%2F%3E%3C%2FclipPath%3E%3C%2Fdefs%3E%3Cpath%20d%3D%22M%20248.00%2C24.00%20L%20776.00%2C24.00%20C%20905.92%2C24.00%201000.00%2C118.08%201000.00%2C248.00%20L%201000.00%2C776.00%20C%201000.00%2C905.92%20905.92%2C1000.00%20776.00%2C1000.00%20L%20248.00%2C1000.00%20C%20118.08%2C1000.00%2024.00%2C905.92%2024.00%2C776.00%20L%2024.00%2C248.00%20C%2024.00%2C118.08%20118.08%2C24.00%20248.00%2C24.00%20Z%22%20fill%3D%22url(%23bg)%22%2F%3E%3Cpath%20d%3D%22M%20248.00%2C24.00%20L%20776.00%2C24.00%20C%20905.92%2C24.00%201000.00%2C118.08%201000.00%2C248.00%20L%201000.00%2C776.00%20C%201000.00%2C905.92%20905.92%2C1000.00%20776.00%2C1000.00%20L%20248.00%2C1000.00%20C%20118.08%2C1000.00%2024.00%2C905.92%2024.00%2C776.00%20L%2024.00%2C248.00%20C%2024.00%2C118.08%20118.08%2C24.00%20248.00%2C24.00%20Z%22%20fill%3D%22url(%23topGlow)%22%2F%3E%3Cpath%20d%3D%22M%20249.00%2C31.00%20L%20775.00%2C31.00%20C%20901.44%2C31.00%20993.00%2C122.56%20993.00%2C249.00%20L%20993.00%2C775.00%20C%20993.00%2C901.44%20901.44%2C993.00%20775.00%2C993.00%20L%20249.00%2C993.00%20C%20122.56%2C993.00%2031.00%2C901.44%2031.00%2C775.00%20L%2031.00%2C249.00%20C%2031.00%2C122.56%20122.56%2C31.00%20249.00%2C31.00%20Z%22%20fill%3D%22none%22%20stroke%3D%22url(%23rimGrad)%22%20stroke-width%3D%2210%22%2F%3E%3Cg%20clip-path%3D%22url(%23shellClip)%22%3E%3Cpath%20d%3D%22M%20224%20344%20L%20448%20512%20L%20224%20680%22%20fill%3D%22none%22%20stroke%3D%22url(%23glyphWhite)%22%20stroke-width%3D%2292%22%20stroke-linecap%3D%22round%22%20stroke-linejoin%3D%22round%22%2F%3E%3Crect%20x%3D%22504%22%20y%3D%22628%22%20width%3D%22296%22%20height%3D%2292%22%20rx%3D%2246%22%20fill%3D%22url(%23glyphWhite)%22%2F%3E%3C%2Fg%3E%3C%2Fsvg%3E";

const HANZO_WORDMARK_TEXT = "hanzo";
const HANZO_SITE_URL = "https://hanzo.ai";
const HANZO_APP_ALT = "Hanzo Dev";
const HANZO_COPYRIGHT_TEXT = "© 2026 Hanzo AI";

const WORDMARK_STYLE: CSSProperties = {
  display: "inline-block",
  fontSize: 28,
  fontWeight: 700,
  letterSpacing: "-0.04em",
  lineHeight: 1,
  color: "#fafafa",
  transform: "translateY(-0.08em)",
};

const WORDMARK_LINK_STYLE: CSSProperties = {
  display: "inline-flex",
  alignItems: "center",
  height: 34,
  color: "#fafafa",
  textDecoration: "none",
};

function HanzoWordmark() {
  return (
    <span data-size="28" data-slot="wordmark" style={WORDMARK_STYLE}>
      {HANZO_WORDMARK_TEXT}
    </span>
  );
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
  macDesktop = false,
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
        height: desktop ? 44 : 60,
        paddingLeft: macDesktop ? 84 : 24,
        paddingRight: desktop && !macDesktop ? 112 : 24,
        boxSizing: "border-box",
        display: "flex",
        alignItems: "center",
        justifyContent: "space-between",
        flexShrink: 0,
      }}
    >
      {desktop ? (
        <div data-tauri-drag-region="" style={{ ...WORDMARK_LINK_STYLE, userSelect: "none" }}>
          <HanzoWordmark />
        </div>
      ) : (
        <a href={HANZO_SITE_URL} style={WORDMARK_LINK_STYLE}>
          <HanzoWordmark />
        </a>
      )}
    </header>
  );
}

export function HanzoLoginFooter() {
  return (
    <footer
      data-slot="footer"
      className="flex shrink-0 flex-col items-center justify-center gap-2 px-6 pt-3"
    >
      <HanzoIcon width={22} height={22} style={{ color: "#fafafa" }} aria-hidden="true" />
      <span className="text-xs leading-4" style={{ color: "#a1a1aa" }}>
        {HANZO_COPYRIGHT_TEXT}
      </span>
    </footer>
  );
}

/**
 * Sign in to Hanzo Dev: canonical lowercase `hanzo` wordmark, 64px Obsidian Glass
 * Squircle app icon, one primary OAuth action, and a compact API-key link.
 */
export function HanzoSignInPanel({
  controller,
  isBusy,
  pendingAuthorizeUrl,
  errorMessage,
  onSignedIn,
}: {
  readonly controller: HanzoWebAuthController;
  readonly isBusy: boolean;
  readonly pendingAuthorizeUrl: string | null;
  readonly errorMessage: string | null;
  readonly onSignedIn?: () => void;
}) {
  const gt = useGT();
  const [showTokenEntry, setShowTokenEntry] = useState(false);
  const [tokenInput, setTokenInput] = useState("");
  const [callbackUrlInput, setCallbackUrlInput] = useState("");
  const [localError, setLocalError] = useState<string | null>(null);

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

  const error = localError ?? errorMessage;
  return (
    <div
      data-testid="hanzo-sign-in"
      className="flex flex-col items-center gap-5 rounded-2xl border border-white/10 bg-black px-6 pt-5 pb-6 text-center text-white"
      style={{ color: "#fafafa" }}
    >
      <div className="flex w-full items-center justify-between pr-8" data-slot="masthead">
        <a href={HANZO_SITE_URL} style={WORDMARK_LINK_STYLE}>
          <HanzoWordmark />
        </a>
      </div>

      <HanzoDevAppIcon size={64} />

      <div className="space-y-2">
        <h2 className="text-xl font-semibold" style={{ color: "#fafafa" }}>
          <T>Sign in to Hanzo Dev</T>
        </h2>
        <p className="text-sm" style={{ color: "#a1a1aa" }}>
          <T>Cloud coding agents, local workspaces, and connected computers.</T>
        </p>
      </div>

      {error ? (
        <div className="w-full rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-left text-xs text-destructive-foreground">
          {error}
        </div>
      ) : null}

      <button
        type="button"
        disabled={isBusy}
        onClick={() => void controller.startOAuthSignIn()}
        className="inline-flex h-11 w-full items-center justify-center rounded-full bg-white px-4 text-sm font-semibold text-black hover:bg-white/90 disabled:opacity-50"
      >
        <T>Continue with Hanzo</T>
      </button>
      <button
        type="button"
        onClick={() => setShowTokenEntry((prev) => !prev)}
        className="text-xs text-white/65 underline underline-offset-2 hover:text-white"
      >
        {showTokenEntry ? <T>Hide API key</T> : <T>Use an API key</T>}
      </button>

      {pendingAuthorizeUrl ? (
        <div
          data-testid="pending-oauth-recovery"
          className="w-full space-y-2.5 rounded-lg border border-white/15 p-3 text-left text-xs"
        >
          <div className="flex flex-wrap items-center justify-between gap-2">
            <span className="font-medium">
              <T>Waiting for browser sign-in to return via hanzodev://...</T>
            </span>
            <div className="flex items-center gap-1.5">
              <button
                type="button"
                onClick={controller.reopenPendingSignIn}
                className="rounded-md border border-white/20 px-2.5 py-1 text-xs font-medium hover:bg-white/10"
              >
                <T>Reopen browser</T>
              </button>
              <button
                type="button"
                onClick={controller.cancelPendingSignIn}
                className="rounded-md border border-white/20 px-2.5 py-1 text-xs text-white/65 hover:bg-white/10 hover:text-white"
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
              className="min-w-0 flex-1 rounded-md border border-white/20 bg-transparent px-2.5 py-1 font-mono text-xs focus:outline-hidden"
            />
            <button
              type="button"
              disabled={!callbackUrlInput.trim() || isBusy}
              onClick={() => {
                const raw = callbackUrlInput.trim();
                setCallbackUrlInput("");
                void controller.handleDeepLinkCallback(raw);
              }}
              className="rounded-md bg-white px-2.5 py-1 text-xs font-semibold text-black disabled:opacity-50"
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
            className="w-full rounded-lg border border-white/20 bg-transparent px-3 py-2 font-mono text-xs focus:outline-hidden"
          />
          <button
            type="button"
            disabled={isBusy || !tokenInput.trim()}
            onClick={() => void handleTokenSubmit()}
            className="w-full rounded-lg border border-white/20 px-3 py-1.5 text-xs font-semibold hover:bg-white/10 disabled:opacity-50"
          >
            <T>Save API key</T>
          </button>
        </div>
      ) : null}

      <HanzoLoginFooter />
    </div>
  );
}
