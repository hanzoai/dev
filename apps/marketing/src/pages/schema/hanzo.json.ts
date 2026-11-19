import type { APIRoute } from "astro";

import { buildHanzoProjectFileJsonSchema } from "@hanzodev/shared/hanzoProjectFile";

export const GET: APIRoute = () =>
  new Response(`${JSON.stringify(buildHanzoProjectFileJsonSchema(), null, 2)}\n`, {
    headers: { "Content-Type": "application/json" },
  });
