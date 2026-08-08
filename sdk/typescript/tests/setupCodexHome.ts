import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";

import { afterEach, beforeEach } from "@jest/globals";

const originalCodexHome = process.env.DEV_HOME;
let currentCodexHome: string | undefined;

beforeEach(async () => {
  currentCodexHome = await fs.mkdtemp(path.join(os.tmpdir(), "codex-sdk-test-"));
  process.env.DEV_HOME = currentCodexHome;
});

afterEach(async () => {
  const codexHomeToDelete = currentCodexHome;
  currentCodexHome = undefined;

  if (originalCodexHome === undefined) {
    delete process.env.DEV_HOME;
  } else {
    process.env.DEV_HOME = originalCodexHome;
  }

  if (codexHomeToDelete) {
    await fs.rm(codexHomeToDelete, { recursive: true, force: true });
  }
});
