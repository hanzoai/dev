import { expect } from "e2e";

import { test, WEB } from "./hanzo.ts";

test(
  "Thread search finds an accented message by a short unaccented word",
  WEB,
  async ({ hanzo, app, screen }) => {
    const project = await hanzo.project("demo");
    const thread = await hanzo.thread({ project, title: "Weekly notes", messages: ["Start"] });
    await hanzo.open(thread);

    // Search reads what the user wrote; messages the CLI sends count as an agent's.
    const composer = screen.getByRole("textbox");
    await composer.pressSequentially("École fermée à Lyon");
    await composer.press("Enter");
    await expect(screen.getByText("second fixture turn complete", { exact: false })).toBeVisible();

    await screen.getByRole("combobox", "Search threads").fill("fermée ec");
    await expect(screen.getByRole("listbox", "Thread search results")).toContainText(
      "École fermée",
    );
    await app.screenshot("search");
  },
);
