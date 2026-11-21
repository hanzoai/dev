import { T, useGT } from "gt-react";
import { useState } from "react";

import { HanzoIcon } from "../components/Icons";
import type { HanzoWebAuthController } from "./hanzoWebAuth";

/**
 * Sign in to Hanzo: one primary action and a small API-key link. The brand is the host's
 * (the build's IAM issuer), never a choice here. Light text on the dark card through
 * currentColor, so the mark can never vanish against it.
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
      className="flex flex-col items-center gap-5 rounded-2xl bg-black px-6 py-8 text-center text-white"
    >
      <HanzoIcon className="size-10" />
      <div className="space-y-2">
        <h2 className="text-xl font-semibold">
          <T>Sign in to Hanzo</T>
        </h2>
        <p className="text-sm text-white/65">
          <T>Sign in to chat, and to control Hanzo Dev on your Mac from your phone.</T>
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
        className="text-xs text-white/55 underline underline-offset-2 hover:text-white"
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
    </div>
  );
}
