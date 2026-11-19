import { assert, describe, it } from "@effect/vitest";

import { mergeLegacyLocalStorage } from "./legacyLocalStorageMerge.ts";

const memoryStorage = (initial: Record<string, string> = {}) => {
  const items = new Map(Object.entries(initial));
  return {
    items,
    getItem: (key: string) => items.get(key) ?? null,
    setItem: (key: string, value: string) => void items.set(key, value),
  };
};

const stash = (...ids: string[]) =>
  JSON.stringify({
    version: 2,
    state: { entries: ids.map((id) => ({ id, prompt: `prompt ${id}` })) },
  });

const drafts = (version: number, threadKey: string, prompt: string) =>
  JSON.stringify({
    version,
    state: {
      draftsByThreadKey: { [threadKey]: { prompt } },
      draftThreadsByThreadKey: {},
      logicalProjectDraftThreadKeyByLogicalProjectKey: {},
    },
  });

describe("mergeLegacyLocalStorage", () => {
  it("copies V1 keys into an empty profile, except the per-install client id", () => {
    const storage = memoryStorage();
    mergeLegacyLocalStorage(storage, {
      "hanzodev:theme": "dark",
      "hanzodev:prompt-stash:v2": stash("a"),
      "hanzo.backgroundActivity.clientId": "v1-client",
    });
    assert.equal(storage.getItem("hanzodev:theme"), "dark");
    assert.equal(storage.getItem("hanzodev:prompt-stash:v2"), stash("a"));
    assert.equal(storage.getItem("hanzo.backgroundActivity.clientId"), null);
  });

  it("keeps V2 values for plain keys", () => {
    const storage = memoryStorage({ "hanzodev:theme": "light" });
    mergeLegacyLocalStorage(storage, { "hanzodev:theme": "dark" });
    assert.equal(storage.getItem("hanzodev:theme"), "light");
  });

  it("appends V1 stash entries after V2's without duplicating ids", () => {
    const storage = memoryStorage({ "hanzodev:prompt-stash:v2": stash("new", "shared") });
    mergeLegacyLocalStorage(storage, { "hanzodev:prompt-stash:v2": stash("shared", "old") });
    const ids = JSON.parse(storage.getItem("hanzodev:prompt-stash:v2")!).state.entries.map(
      (entry: { id: string }) => entry.id,
    );
    assert.deepStrictEqual(ids, ["new", "shared", "old"]);
  });

  it("adds V1 drafts for other threads and keeps V2's draft for the same thread", () => {
    const storage = memoryStorage({
      "hanzodev:composer-drafts:v1": drafts(9, "env:shared", "v2 text"),
    });
    const legacy = JSON.parse(drafts(9, "env:shared", "v1 text"));
    legacy.state.draftsByThreadKey["env:old"] = { prompt: "v1 only" };
    mergeLegacyLocalStorage(storage, { "hanzodev:composer-drafts:v1": JSON.stringify(legacy) });
    const merged = JSON.parse(storage.getItem("hanzodev:composer-drafts:v1")!);
    assert.equal(merged.state.draftsByThreadKey["env:shared"].prompt, "v2 text");
    assert.equal(merged.state.draftsByThreadKey["env:old"].prompt, "v1 only");
  });

  it("leaves V2 drafts alone when the stored shapes differ in version", () => {
    const current = drafts(10, "env:a", "v2");
    const storage = memoryStorage({ "hanzodev:composer-drafts:v1": current });
    mergeLegacyLocalStorage(storage, { "hanzodev:composer-drafts:v1": drafts(9, "env:b", "v1") });
    assert.equal(storage.getItem("hanzodev:composer-drafts:v1"), current);
  });

  it("caps the merged stash at the store's entry limit, dropping the oldest V1 entries", () => {
    const v2Ids = Array.from({ length: 15 }, (_, index) => `v2-${index}`);
    const v1Ids = Array.from({ length: 10 }, (_, index) => `v1-${index}`);
    const storage = memoryStorage({ "hanzodev:prompt-stash:v2": stash(...v2Ids) });
    mergeLegacyLocalStorage(storage, { "hanzodev:prompt-stash:v2": stash(...v1Ids) });
    const ids = JSON.parse(storage.getItem("hanzodev:prompt-stash:v2")!).state.entries.map(
      (entry: { id: string }) => entry.id,
    );
    assert.deepStrictEqual(ids, [...v2Ids, ...v1Ids.slice(0, 5)]);
  });

  it("keeps importing other keys after one write fails, and reports the failure", () => {
    const storage = memoryStorage();
    const setItem = storage.setItem;
    storage.setItem = (key, value) => {
      if (key === "hanzodev:theme") throw new DOMException("full", "QuotaExceededError");
      setItem(key, value);
    };
    const complete = mergeLegacyLocalStorage(storage, {
      "hanzodev:theme": "dark",
      "hanzodev:last-editor": "zed",
    });
    assert.equal(storage.getItem("hanzodev:last-editor"), "zed");
    assert.isFalse(complete);
  });
});
