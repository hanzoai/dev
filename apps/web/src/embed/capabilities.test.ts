import { describe, expect, it } from "vite-plus/test";

import { showsCodeTools } from "./capabilities";

describe("showsCodeTools", () => {
  const base = { embedded: true, hostWantsTools: true, openThread: null, hasWorkspace: false };
  it("shows them in standalone Dev", () => {
    expect(showsCodeTools({ ...base, embedded: false, hostWantsTools: false })).toBe(true);
  });
  it("hides them where the host says it is a conversation (/chat)", () => {
    expect(showsCodeTools({ ...base, hostWantsTools: false })).toBe(false);
  });
  it("hides them for a conversation thread, shows them for one with a workspace", () => {
    expect(showsCodeTools({ ...base, openThread: "env/t1", hasWorkspace: false })).toBe(false);
    expect(showsCodeTools({ ...base, openThread: "env/t1", hasWorkspace: true })).toBe(true);
  });
  it("shows them on /dev with no thread open", () => {
    expect(showsCodeTools(base)).toBe(true);
  });
});
