import { describe, expect, it, vi } from "vite-plus/test";
import { page, userEvent } from "vite-plus/test/browser";

const toggle = vi.hoisted(() => vi.fn());
const state = vi.hoisted(() => ({ open: false, refusal: null as unknown }));
vi.mock("@hanzo/voice", () => ({
  useTalk: () => ({
    open: state.open,
    state: "listening",
    reply: "",
    level: 0,
    blocked: null,
    reason: null,
    refusal: state.refusal,
    toggle,
  }),
}));

import { render } from "../../test/render";
import { TalkMode } from "./TalkMode";

const cloud = { api: "https://api.test", token: async () => "t" };

describe("TalkMode", () => {
  it("opens talk mode from the waveform button and says it is listening", async () => {
    state.open = false;
    await using screen = await render(<TalkMode cloud={cloud} />, { router: false });
    await userEvent.click(page.getByTestId("talk-mode"));
    expect(toggle).toHaveBeenCalledOnce();
    screen.rerender(<TalkMode cloud={cloud} />);
    state.open = true;
    screen.rerender(<TalkMode cloud={{ ...cloud }} />);
    await expect.element(page.getByText("Listening. Speak, and Hanzo answers aloud.")).toBeVisible();
    await expect.element(page.getByTestId("talk-mode")).toHaveAttribute("aria-pressed", "true");
  });
});
