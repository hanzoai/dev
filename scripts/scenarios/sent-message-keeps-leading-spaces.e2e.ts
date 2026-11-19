/// <reference lib="dom" />
// The DOM lib types the function this scenario runs in the page.
import { expect } from "e2e";

import { test, WEB } from "./hanzo.ts";

const LINES = ["Plan", "   Step one", "   Step two", "        Detail a", "        Detail b"];

test(
  "A sent message shows the spaces typed at the start of its lines",
  WEB,
  async ({ hanzo, app, browser, screen }) => {
    const project = await hanzo.project("demo");
    const thread = await hanzo.thread({ project, title: "Outline", messages: ["Start"] });
    await hanzo.open(thread);

    const composer = screen.getByRole("textbox");
    for (const [index, line] of LINES.entries()) {
      if (index > 0) await composer.press("Shift+Enter");
      await composer.pressSequentially(line);
    }
    await composer.press("Enter");
    await expect(screen.getByText("second fixture turn complete", { exact: false })).toBeVisible();
    await app.screenshot("sent");

    // The visible text, which keeps whitespace only where the page shows it.
    const shown = await browser.evaluate(
      () =>
        [...document.querySelectorAll<HTMLElement>("[data-user-message-body]")]
          .map((body) => body.innerText)
          .find((text) => text.includes("Detail b")) ?? null,
    );
    expect(shown).toBe(LINES.join("\n"));
  },
);
