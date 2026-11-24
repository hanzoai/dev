import { describe, expect, it, vi } from "vite-plus/test";
import { page, userEvent } from "vite-plus/test/browser";

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
        String(url).includes("/v1/models")
          ? Response.json(models)
          : new Response("{}", { status: 404 }),
      ),
    );
    await using _screen = await render(
      <DevHome
        cloud={{ api: "https://api.test", token: async () => "t" }}
        mode="dev"
        environments={[{ id: "cloud", kind: "cloud", label: "Cloud", available: true }]}
        environment="cloud"
        onEnvironment={() => {}}
      />,
      { router: false },
    );
    await expect.element(page.getByText("Build what's next.")).toBeVisible();
    await expect.element(page.getByTestId("composer-editor")).toBeVisible();
    await expect.element(page.getByTestId("environment-chip")).toBeVisible();
  });

  it("opens the persona picker via /persona and activates the selected org persona", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string) => {
        const u = String(url);
        if (u.includes("/v1/models")) return Response.json(models);
        if (u.endsWith("/v1/agents"))
          return Response.json({
            agents: [{ id: "a1", name: "dev", model: "enso", description: "Builds" }],
          });
        return new Response("{}", { status: 404 });
      }),
    );
    await using _screen = await render(
      <DevHome
        cloud={{ api: "https://api.test", token: async () => "t" }}
        mode="chat"
        environments={environments}
        environment="cloud"
        onEnvironment={() => {}}
      />,
      { router: false },
    );
    await page.getByTestId("composer-editor").click();
    await page.getByTestId("composer-editor").fill("/persona dev");
    await page.getByRole("button", { name: /submit message/i }).click();
    await expect.element(page.getByTestId("active-persona-banner")).toBeVisible();
  });

  it("offers a web-search toggle in chat mode, off until pressed", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string) =>
        String(url).includes("/v1/models")
          ? Response.json(models)
          : new Response("{}", { status: 404 }),
      ),
    );
    await using _screen = await render(
      <DevHome
        cloud={{ api: "https://api.test", token: async () => "t" }}
        mode="chat"
        environments={environments}
        environment={null}
        onEnvironment={() => {}}
      />,
      { router: false },
    );
    await page.getByTestId("composer-editor").click();
    const webSearch = page.getByTestId("web-search-toggle");
    await expect.element(webSearch).toBeVisible();
    await expect.element(webSearch).toHaveAttribute("aria-pressed", "false");
    await webSearch.click();
    await expect.element(webSearch).toHaveAttribute("aria-pressed", "true");
    await webSearch.click();
    await expect.element(webSearch).toHaveAttribute("aria-pressed", "false");
  });

  it("sends a turn in Cloud with no environment: Enter and the Send button POST a chat completion with the bearer, and no environment is discovered", async () => {
    const posts: Array<{ url: string; auth: string | null; body: unknown }> = [];
    const wellKnown: string[] = [];
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string, init?: RequestInit) => {
        const u = String(url);
        if (u.includes(".well-known/hanzo/environment")) wellKnown.push(u);
        if (u.includes("/v1/models")) return Response.json(models);
        if (u.endsWith("/v1/chat/completions")) {
          const headers = new Headers(init?.headers as HeadersInit);
          posts.push({
            url: u,
            auth: headers.get("authorization"),
            body: JSON.parse(String(init?.body)),
          });
          return new Response(
            'data: {"choices":[{"delta":{"content":"ok"}}]}\n\ndata: [DONE]\n\n',
            {
              headers: { "content-type": "text/event-stream" },
            },
          );
        }
        return new Response("{}", { status: 404 });
      }),
    );
    await using _screen = await render(
      <DevHome
        cloud={{ api: "https://api.test", token: async () => "iam-token" }}
        mode="chat"
        environments={environments}
        environment="cloud"
        onEnvironment={() => {}}
      />,
      { router: false },
    );
    await page.getByTestId("composer-editor").click();
    await page.getByTestId("composer-editor").fill("Reply with ok");
    await page.getByRole("button", { name: /submit message/i }).click();
    await vi.waitFor(() => expect(posts).toHaveLength(1));
    expect(posts[0]?.auth).toBe("Bearer iam-token");
    expect(wellKnown).toEqual([]);
  });

  it("sends a turn in Cloud with no environment: Enter POSTs a chat completion with the bearer, and no environment is discovered", async () => {
    const posts: Array<{ url: string; auth: string | null; body: unknown }> = [];
    const wellKnown: string[] = [];
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string, init?: RequestInit) => {
        const u = String(url);
        if (u.includes(".well-known/hanzo/environment")) wellKnown.push(u);
        if (u.includes("/v1/models")) return Response.json(models);
        if (u.endsWith("/v1/chat/completions")) {
          const headers = new Headers(init?.headers as HeadersInit);
          posts.push({
            url: u,
            auth: headers.get("authorization"),
            body: JSON.parse(String(init?.body)),
          });
          return new Response(
            'data: {"choices":[{"delta":{"content":"ok"}}]}\n\ndata: [DONE]\n\n',
            {
              headers: { "content-type": "text/event-stream" },
            },
          );
        }
        return new Response("{}", { status: 404 });
      }),
    );
    await using _screen = await render(
      <DevHome
        cloud={{ api: "https://api.test", token: async () => "iam-token" }}
        mode="chat"
        environments={environments}
        environment="cloud"
        onEnvironment={() => {}}
      />,
      { router: false },
    );
    await page.getByTestId("composer-editor").click();
    await page.getByTestId("composer-editor").fill("Reply with ok");
    await userEvent.keyboard("{Enter}");
    await vi.waitFor(() => expect(posts).toHaveLength(1));
    expect(posts[0]?.auth).toBe("Bearer iam-token");
    expect(wellKnown).toEqual([]);
  });
});
