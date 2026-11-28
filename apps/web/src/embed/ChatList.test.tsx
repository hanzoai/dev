import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it, vi } from "vite-plus/test";

vi.mock("./homeChat", () => ({
  useCloudThreads: () => ({
    controller: { renameThread: vi.fn() },
    open: null,
    threads: [
      {
        key: "t-today",
        remoteId: "r-1",
        title: "Fix preview pane",
        updatedAt: Date.now() - 5 * 60_000,
        pinned: true,
      },
      {
        key: "t-yesterday",
        remoteId: "r-2",
        title: "Add Enso rings",
        updatedAt: Date.now() - 26 * 3_600_000,
        pinned: false,
      },
      {
        key: "t-older",
        remoteId: null,
        title: "Old local note",
        updatedAt: Date.now() - 45 * 86_400_000,
        pinned: false,
      },
    ],
  }),
}));

import { DevChatList } from "./ChatList";

const CLOUD = { api: "https://api.test", token: async () => "tok" };

describe("DevChatList density & Priority / Day-by-Day view", () => {
  it("renders view toggle and 1/2/3-line density rows cleanly", () => {
    const oneLine = renderToStaticMarkup(
      <DevChatList cloud={CLOUD} density={1} viewMode="priority" />,
    );
    expect(oneLine).toContain('data-testid="chat-list-view-toggle"');
    expect(oneLine).toContain('data-density="1"');
    expect(oneLine).toContain('data-view-mode="priority"');
    expect(oneLine).not.toContain("Pinned conversation");
    expect(oneLine).not.toContain("data-chat-day-group");

    const twoLines = renderToStaticMarkup(
      <DevChatList cloud={CLOUD} density={2} viewMode="priority" />,
    );
    expect(twoLines).toContain('data-density="2"');
    expect(twoLines).toContain("Cloud");
    expect(twoLines).toContain("Local");
    expect(twoLines).not.toContain("Pinned conversation");

    const threeLines = renderToStaticMarkup(
      <DevChatList cloud={CLOUD} density={3} viewMode="priority" />,
    );
    expect(threeLines).toContain('data-density="3"');
    expect(threeLines).toContain("Pinned conversation");
  });

  it("groups conversations by day bucket when viewMode is chronological", () => {
    const html = renderToStaticMarkup(
      <DevChatList cloud={CLOUD} density={2} viewMode="chronological" />,
    );
    expect(html).toContain('data-view-mode="chronological"');
    expect(html).toContain('data-chat-day-group="Today"');
    expect(html).toContain('data-chat-day-group="Yesterday"');
    expect(html).toContain('data-chat-day-group="Older"');
  });
});
