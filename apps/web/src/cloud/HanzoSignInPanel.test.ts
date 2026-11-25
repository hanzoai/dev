import { createElement } from "react";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vite-plus/test";

import { DesktopWindowControls } from "../components/desktop/DesktopWindowControls";
import {
  HANZO_DEV_APP_ICON_DATA_URL,
  HanzoDevAppIcon,
  HanzoLoginFooter,
  HanzoLoginMasthead,
} from "./HanzoSignInPanel";

describe("Hanzo Dev canonical login & frameless window chrome", () => {
  it("renders the 28px corner hanzo wordmark masthead with no maxWidth constraint", () => {
    const webHtml = renderToStaticMarkup(
      createElement(HanzoLoginMasthead, { desktop: false, macDesktop: false }),
    );
    expect(webHtml).toContain('data-slot="masthead"');
    expect(webHtml).toContain('data-slot="wordmark"');
    expect(webHtml).toContain('data-size="28"');
    expect(webHtml).toContain("font-size:28px");
    expect(webHtml).toContain("padding-left:24px");
    expect(webHtml).not.toContain("max-width");
    expect(webHtml).toContain(">hanzo<");

    const macHtml = renderToStaticMarkup(
      createElement(HanzoLoginMasthead, { desktop: true, macDesktop: true }),
    );
    expect(macHtml).toContain('data-tauri-drag-region=""');
    expect(macHtml).toContain("padding-left:84px");
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

  it("embeds the 64px canonical Obsidian Glass Squircle hanzo-dev icon and Hanzo AI footer", () => {
    const iconHtml = renderToStaticMarkup(createElement(HanzoDevAppIcon, { size: 64 }));
    expect(iconHtml).toContain('width="64"');
    expect(iconHtml).toContain('height="64"');
    expect(iconHtml).toContain('data-hanzo-app-icon="hanzo-dev"');
    expect(HANZO_DEV_APP_ICON_DATA_URL.startsWith("data:image/svg+xml,")).toBe(true);
    expect(decodeURIComponent(HANZO_DEV_APP_ICON_DATA_URL)).toContain(
      "M 224 344 L 448 512 L 224 680",
    );

    const footerHtml = renderToStaticMarkup(createElement(HanzoLoginFooter));
    expect(footerHtml).toContain("© 2026 Hanzo AI");
  });
});
