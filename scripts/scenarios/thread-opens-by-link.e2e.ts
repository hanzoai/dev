import { expect } from "e2e";

import { test, WEB } from "./hanzo.ts";

test(
  "A seeded thread opens by its link and shows its history",
  WEB,
  async ({ hanzo, app, screen }) => {
    const project = await hanzo.project("demo");
    const thread = await hanzo.thread({
      project,
      title: "Earlier work",
      messages: ["first", "second"],
    });
    await hanzo.open(thread);

    await expect(screen.getByText("second fixture turn complete", { exact: false })).toBeVisible();
    await app.screenshot("thread");
  },
);
