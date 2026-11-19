import { assert, it } from "@effect/vitest";

import {
  repositoryPathFromRemoteUrl,
  transportSafeSourceControlErrorValue,
} from "./SourceControlProvider.ts";

it("removes URL credentials, query parameters, and fragments from error transport values", () => {
  assert.strictEqual(
    transportSafeSourceControlErrorValue(
      "https://user:secret@example.test/org/repo/pull/42?token=secret#discussion",
    ),
    "https://example.test/org/repo/pull/42",
  );
});

it.each([
  ["git@github.com:HanzoDev/hanzodev.git", "HanzoDev/hanzodev"],
  ["github.com:HanzoDev/hanzodev.git", "HanzoDev/hanzodev"],
  ["gitlab.com:HanzoDev/platform/hanzodev", "HanzoDev/platform/hanzodev"],
  ["https://github.com/HanzoDev/hanzodev.git", "HanzoDev/hanzodev"],
  ["https://github.com/HanzoDev/hanzodev.git?view=compact", "HanzoDev/hanzodev"],
  ["https://github.com/HanzoDev/hanzodev.git#section", "HanzoDev/hanzodev"],
  ["C:/repos/hanzodev", null],
])("reads the repository path of %s", (remoteUrl, expected) => {
  assert.strictEqual(repositoryPathFromRemoteUrl(remoteUrl), expected);
});
