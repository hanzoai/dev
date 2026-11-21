import { describe, expect, it, vi } from "vite-plus/test";
import { page } from "vite-plus/test/browser";

import { render } from "../../test/render";
import { DevHome } from "./DevHome";
import type { DevEnvironment } from "./runtime";

const environments: DevEnvironment[] = [
  { id: "cloud", kind: "cloud", label: "Cloud", available: false, note: "Not available yet" },
];

const models = {
  data: [
    { id: "enso-flash", name: "Enso Flash", outputs: ["text"], endpoint: "/v1/chat/completions" },
    { id: "kai-1", name: "Kai 1", outputs: ["decision"], endpoint: "/v1/decisions" },
  ],
};

describe("DevHome", () => {
  it("opens on the composer with no environment, with Dev's own composer", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string) =>
        String(url).includes("/v1/models") ? Response.json(models) : new Response("{}", { status: 404 }),
      ),
    );
    await using screen = await render(
      <DevHome
        cloud={{ api: "https://api.test", token: async () => "t" }}
        mode="dev"
        environments={environments}
        environment="cloud"
        onEnvironment={() => {}}
      />,
      { router: false },
    );
    await expect.element(page.getByText("Build what's next.")).toBeVisible();
    await expect.element(page.getByTestId("composer-editor")).toBeVisible();
    await expect.element(page.getByTestId("environment-chip")).toBeVisible();
  });

  it("offers the org's personas and the sparkle, and the sparkle rewrites the draft with Undo", async () => {
    const completions: unknown[] = [];
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string, init?: RequestInit) => {
        const u = String(url);
        if (u.includes("/v1/models")) return Response.json(models);
        if (u.endsWith("/v1/agents")) return Response.json({ agents: [{ id: "a1", name: "dev", model: "enso", description: "Builds" }] });
        if (u.endsWith("/v1/chat/completions")) {
          completions.push(JSON.parse(String(init?.body)));
          return Response.json({ choices: [{ message: { content: "A clearer prompt." } }] });
        }
        return new Response("{}", { status: 404 });
      }),
    );
    await using screen = await render(
      <DevHome
        cloud={{ api: "https://api.test", token: async () => "t" }}
        mode="chat"
        environments={environments}
        environment="cloud"
        onEnvironment={() => {}}
      />,
      { router: false },
    );
    await expect.element(page.getByTestId("persona-chip")).toBeVisible();
    await expect.element(page.getByTestId("enhance")).toBeVisible();
    await page.getByTestId("composer-editor").click();
    await page.getByTestId("composer-editor").fill("fix teh bug");
    await page.getByTestId("enhance").click();
    await vi.waitFor(() => expect(completions).toHaveLength(1));
    await expect.element(page.getByTestId("enhance-undo")).toBeVisible();
  });
});
