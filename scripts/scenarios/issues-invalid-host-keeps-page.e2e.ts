import { expect } from "e2e";

import { test, WEB } from "./hanzo.ts";

test("An invalid host filter keeps the Issues page open", WEB, async ({ hanzo, app, browser }) => {
  await hanzo.project("demo");
  await hanzo.open("/issues?host=true");

  await app.screenshot("issues-invalid-host");
  await expect(browser.locator("h1")).toHaveText("Issues");
});
