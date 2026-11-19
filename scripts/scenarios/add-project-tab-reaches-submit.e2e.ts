import { expect } from "e2e";

import { test, WEB } from "./hanzo.ts";

test(
  "Tab moves from the add-project Git URL field to its submit button",
  WEB,
  async ({ hanzo, app, screen, browser }) => {
    await hanzo.project("demo");
    await hanzo.open();

    await screen.getByRole("button", "Add project").tap();
    await screen.getByText("Git URL").tap();
    await screen.getByRole("combobox").fill("https://github.com/hanzoai/dev.git");
    const submit = screen.getByRole("button", "Continue (Enter)");
    await expect(submit).toBeEnabled();

    await browser.keyboard.press("Tab");
    await app.screenshot("after-tab");
    await expect(submit).toBeFocused();
  },
);
