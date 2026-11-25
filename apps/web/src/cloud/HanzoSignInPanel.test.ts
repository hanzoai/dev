import { createElement } from "react";
import { renderToStaticMarkup } from "react-dom/server";
import { beforeAll, describe, expect, it } from "vite-plus/test";

import { DesktopWindowControls } from "../components/desktop/DesktopWindowControls";
import { startI18n } from "../i18n/runtime";
import {
  HANZO_DEV_APP_ICON_DATA_URL,
  HanzoDevAppIcon,
  HanzoLoginFooter,
  HanzoLoginMasthead,
  HanzoSignInPanel,
} from "./HanzoSignInPanel";
import type { HanzoWebAuthController } from "./hanzoWebAuth";

const stubController = {
  startOAuthSignIn: async () => undefined,
  signInWithAccessToken: async () => undefined,
  reopenPendingSignIn: () => undefined,
  cancelPendingSignIn: () => undefined,
  handleDeepLinkCallback: async () => false,
  closeModal: () => undefined,
} as unknown as HanzoWebAuthController;

describe("Hanzo Dev compact login & frameless window chrome", () => {
  beforeAll(() => {
    startI18n();
  });

  it("renders a clean 44px wordmark-free drag masthead", () => {
    const desktopHtml = renderToStaticMarkup(createElement(HanzoLoginMasthead, { desktop: true }));
    expect(desktopHtml).toContain('data-slot="masthead"');
    expect(desktopHtml).toContain('data-tauri-drag-region=""');
    expect(desktopHtml).toContain("height:44px");
    expect(desktopHtml).toContain("justify-content:flex-end");
    expect(desktopHtml).not.toContain('data-slot="wordmark"');
    expect(desktopHtml).not.toContain(">hanzo<");
  });

  it("renders crisp 28x28 monochrome Minimize, Maximize, and Close window controls", () => {
    const controlsHtml = renderToStaticMarkup(
      createElement(DesktopWindowControls, { forceRender: true }),
    );
    expect(controlsHtml).toContain('data-slot="window-controls"');
    expect(controlsHtml).toContain('data-tauri-drag-region="false"');
    expect(controlsHtml).toContain("width:28px");
    expect(controlsHtml).toContain("height:28px");
    expect(controlsHtml).toContain('aria-label="Minimize"');
    expect(controlsHtml).toContain('aria-label="Maximize"');
    expect(controlsHtml).toContain('aria-label="Close"');
  });

  it("renders 64px hanzo-dev squircle, Google/GitHub/Email buttons, Continue without signing in, and 20px Hanzo mark", () => {
    const panelHtml = renderToStaticMarkup(
      createElement(HanzoSignInPanel, {
        bare: true,
        controller: stubController,
        isBusy: false,
        pendingAuthorizeUrl: null,
        errorMessage: null,
        onContinueWithoutLogin: () => undefined,
      }),
    );
    expect(panelHtml).toContain('data-hanzo-app-icon="hanzo-dev"');
    expect(panelHtml).toContain('width="64"');
    expect(panelHtml).toContain("Sign in to Hanzo Dev");
    expect(panelHtml).toContain("Continue with Google");
    expect(panelHtml).toContain("Continue with GitHub");
    expect(panelHtml).toContain("Continue with email");
    expect(panelHtml).toContain('id="continue-without-login"');
    expect(panelHtml).toContain("Continue without signing in");

    const iconHtml = renderToStaticMarkup(createElement(HanzoDevAppIcon, { size: 64 }));
    expect(HANZO_DEV_APP_ICON_DATA_URL.startsWith("data:image/svg+xml,")).toBe(true);
    expect(decodeURIComponent(HANZO_DEV_APP_ICON_DATA_URL)).toContain(
      "M 224 344 L 448 512 L 224 680",
    );
    expect(iconHtml).toContain('alt="Hanzo Dev"');

    const footerHtml = renderToStaticMarkup(createElement(HanzoLoginFooter));
    expect(footerHtml).toContain('width="20"');
    expect(footerHtml).toContain('height="20"');
  });
});
