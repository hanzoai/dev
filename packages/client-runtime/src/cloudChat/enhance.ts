// @effect-diagnostics globalFetch:off -- Cloud chat is a plain promise client shared with React Native and the browser.
/**
 * The sparkle: rewrite a draft prompt through the gateway, as a chat completion on the person's
 * account, and answer the rewrite. The draft is never changed here; the caller shows both and
 * keeps the original for Undo.
 */
import { buildHanzoApiHeaders } from "@hanzodev/shared/hanzoIamAuth";

import {
  CloudChatError,
  DEFAULT_MODEL_ID,
  httpFailure,
  resolveCloudChatOptions,
  type CloudChatAuth,
  type CloudChatOptions,
} from "./api.ts";

export const ENHANCE_INSTRUCTION =
  "Rewrite the person's draft prompt so it is clearer, more specific and better structured for an AI assistant. " +
  "Keep their intent, their language and every fact, name and constraint in it; add nothing they did not mean. " +
  "Answer with the rewritten prompt only: no preface, no quotes, no explanation.";

export async function enhancePrompt(input: {
  readonly auth: CloudChatAuth;
  readonly prompt: string;
  readonly model?: string | undefined;
  readonly signal?: AbortSignal | undefined;
  readonly options?: CloudChatOptions | undefined;
}): Promise<string> {
  const draft = input.prompt.trim();
  if (draft === "") throw new CloudChatError("failed", "Nothing to enhance");
  const token = input.auth.accessToken?.trim();
  if (!token) throw new CloudChatError("signed-out", "Not signed in");
  const { fetch, baseUrl } = resolveCloudChatOptions(input.options ?? {});
  let response: Response;
  try {
    response = await fetch(`${baseUrl}/v1/chat/completions`, {
      method: "POST",
      headers: buildHanzoApiHeaders({
        accessToken: token,
        orgId: input.auth.orgId,
        extra: { "Content-Type": "application/json" },
      }),
      body: JSON.stringify({
        model: input.model ?? DEFAULT_MODEL_ID,
        stream: false,
        messages: [
          { role: "system", content: ENHANCE_INSTRUCTION },
          { role: "user", content: draft },
        ],
      }),
      ...(input.signal ? { signal: input.signal } : {}),
    });
  } catch {
    throw new CloudChatError("unavailable", "Network unavailable");
  }
  if (!response.ok) throw httpFailure(response.status);
  const body = (await response.json()) as { choices?: Array<{ message?: { content?: unknown } }> };
  const out = body.choices?.[0]?.message?.content;
  if (typeof out !== "string" || out.trim() === "") throw new CloudChatError("failed", "Empty rewrite");
  return out.trim();
}
