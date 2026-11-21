// ui.mjs drives the real Hanzo Dev web client, through bridge.mjs, in chat mode:
// send a message, read the reply, send another, rename the thread, settle it.
//
//   node ui.mjs <dev checkout> <bridge url> <storage state with a Dev session>
//
// The reply is the framed command's stand-in model, which repeats what it was
// told, so the test reads it back.
import { createRequire } from "node:module";
import assert from "node:assert/strict";

const [checkout, base, state] = process.argv.slice(2);
const require = createRequire(checkout + "/node_modules/.pnpm/playwright@1.64.1/node_modules/playwright/");
const { chromium } = require("playwright");
const browser = await chromium.launch();
const page = await (await browser.newContext({ viewport: { width: 1400, height: 900 }, storageState: state })).newPage();
const ask = async (text) => {
  await page.getByTestId("composer-editor").click();
  await page.keyboard.type(text);
  await page.waitForTimeout(2000);
  await page.keyboard.press("Enter");
};

await page.goto(base);
await page.waitForTimeout(6000);
await ask("Hello from the wasm loop");
await page.getByText('You said: "Hello from the wasm loop"').waitFor({ timeout: 20000 });
await page.waitForURL(/\/[0-9a-f-]{36}\/[0-9a-f-]{36}$/);
await ask("And again");
await page.getByText('You said: "And again"').waitFor({ timeout: 20000 });

await page.getByText("Chat", { exact: true }).last().click({ button: "right" });
await page.getByRole("menuitem", { name: "Rename thread" }).click();
await page.keyboard.press("ControlOrMeta+a");
await page.keyboard.type("Wasm Notes");
await page.keyboard.press("Enter");
await page.getByText("Wasm Notes").first().waitFor({ timeout: 10000 });

const before = await page.getByText("Wasm Notes").count();
await page.getByText("Wasm Notes").first().click({ button: "right" });
await page.getByRole("menuitem", { name: "Settle thread" }).click();
await page.waitForTimeout(2000);
// A settled thread leaves the list of active ones.
assert.ok((await page.getByText("Wasm Notes").count()) < before);
console.log("ok: connect, send, reply, follow-up, rename, settle");
await browser.close();
