import { useEffect, useState, useSyncExternalStore, type ReactNode } from "react";

import { setDesktopWindowMode } from "../components/desktop/DesktopWindowControls";
import { isTauri } from "../env";
import {
  HanzoLoginMasthead,
  HanzoSignInPanel,
  readSkippedSignIn,
  writeSkippedSignIn,
} from "./HanzoSignInPanel";
import { getHanzoWebAuthController } from "./hanzoWebAuth";

export function DesktopSignInGate({ children }: { readonly children: ReactNode }) {
  const controller = getHanzoWebAuthController();
  const isLoaded = useSyncExternalStore(
    controller.subscribe,
    () => controller.isLoaded,
    () => false,
  );
  const accountCount = useSyncExternalStore(
    controller.subscribe,
    () => controller.accounts.length,
    () => 0,
  );
  const isBusy = useSyncExternalStore(
    controller.subscribe,
    () => controller.isBusy,
    () => false,
  );
  const pendingAuthorizeUrl = useSyncExternalStore(
    controller.subscribe,
    () => controller.pendingOAuthAuthorizeUrl,
    () => null,
  );
  const errorMessage = useSyncExternalStore(
    controller.subscribe,
    () => controller.errorMessage,
    () => null,
  );
  const [skipped, setSkipped] = useState(readSkippedSignIn);
  const [prevAccountCount, setPrevAccountCount] = useState(accountCount);

  if (prevAccountCount !== accountCount) {
    setPrevAccountCount(accountCount);
    if (prevAccountCount > 0 && accountCount === 0 && skipped) {
      writeSkippedSignIn(false);
      setSkipped(false);
    }
  }

  const showLoginGate = isTauri && isLoaded && accountCount === 0 && !skipped;

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- resizes Tauri window between compact 460x640 login and 1360x860 workspace
  useEffect(() => {
    if (!isTauri || !isLoaded) return;
    setDesktopWindowMode(showLoginGate ? "login" : "workspace");
  }, [isLoaded, showLoginGate]);

  if (!isTauri) return children;
  if (!isLoaded) {
    return (
      <div
        style={{
          minHeight: "100dvh",
          width: "100%",
          backgroundColor: "var(--hz-login-bg, #000000)",
        }}
      />
    );
  }
  if (!showLoginGate) return children;

  return (
    <div
      data-testid="hanzo-dev-signin-landing"
      style={{
        minHeight: "100dvh",
        width: "100%",
        backgroundColor: "var(--hz-login-bg, #000000)",
        color: "var(--hz-login-fg, #fafafa)",
        padding: "0 0 16px",
        boxSizing: "border-box",
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        justifyContent: "space-between",
      }}
    >
      <HanzoLoginMasthead desktop />
      <main className="my-auto flex w-full items-center justify-center px-4">
        <HanzoSignInPanel
          bare
          controller={controller}
          isBusy={isBusy}
          pendingAuthorizeUrl={pendingAuthorizeUrl}
          errorMessage={errorMessage}
          onContinueWithoutLogin={() => {
            writeSkippedSignIn(true);
            setSkipped(true);
          }}
        />
      </main>
      <span />
    </div>
  );
}
