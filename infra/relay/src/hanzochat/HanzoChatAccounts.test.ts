import { describe, expect, it } from "@effect/vitest";
import { vi } from "vite-plus/test";
import * as Effect from "effect/Effect";
import * as Fiber from "effect/Fiber";
import * as TestClock from "effect/testing/TestClock";
import * as Layer from "effect/Layer";
import * as Redacted from "effect/Redacted";
import * as HttpClient from "effect/http/HttpClient";
import * as HttpClientResponse from "effect/http/HttpClientResponse";

import * as RelayConfiguration from "../Config.ts";
import * as HanzoChatAccounts from "./HanzoChatAccounts.ts";

const SECRET = "shared-link-secret";

const relaySettings = (
  hanzoChat: RelayConfiguration.HanzoChatSettings | null,
): RelayConfiguration.RelayConfiguration["Service"] => ({
  relayIssuer: "https://relay.example.test",
  apns: null,
  iamOrigin: "https://hanzo.id",
  iamAudience: "hanzo-dev-relay",
  apnsDeliveryJobSigningSecret: Redacted.make("apns-delivery-secret"),
  cloudMintPrivateKey: Redacted.make("cloud-mint-private-key"),
  cloudMintPublicKey: "cloud-mint-public-key",
  managedEndpointBaseDomain: undefined,
  managedEndpointNamespace: undefined,
  ...(hanzoChat ? { hanzoChat } : {}),
});

const fakeHanzoChat = (body: unknown, status = 200) => {
  const requests: Array<{ url: string; authorization: string | undefined; body: unknown }> = [];
  const client = HttpClient.make((request) =>
    Effect.gen(function* () {
      requests.push({
        url: request.url,
        authorization: request.headers["authorization"],
        body:
          request.body._tag === "Uint8Array"
            ? JSON.parse(new TextDecoder().decode(request.body.body))
            : null,
      });
      return HttpClientResponse.fromWeb(
        request,
        new Response(JSON.stringify(body), {
          status,
          headers: { "content-type": "application/json" },
        }),
      );
    }),
  );
  return { client, requests };
};

const provide = (
  client: HttpClient.HttpClient,
  hanzoChat: RelayConfiguration.HanzoChatSettings | null = {
    baseUrl: "https://chat.example.test",
    linkSecret: Redacted.make(SECRET),
  },
) =>
  Effect.provide(
    HanzoChatAccounts.layer.pipe(
      Layer.provide(RelayConfiguration.layer(relaySettings(hanzoChat))),
      Layer.provide(Layer.succeed(HttpClient.HttpClient, client)),
    ),
  );

describe("HanzoChatAccounts", () => {
  it.effect("signs link tokens in the format Hanzo Chat verifies", () =>
    Effect.gen(function* () {
      const accounts = yield* HanzoChatAccounts.HanzoChatAccounts;
      const url = new URL(yield* accounts.createLinkUrl({ userId: "user_1", email: "a@b.co" }));
      expect(`${url.origin}${url.pathname}`).toBe("https://chat.example.test/link/hanzodev");

      const [payload = "", signature = ""] = (url.searchParams.get("token") ?? "").split(".");
      // Verify with Web Crypto, as Hanzo Chat does, rather than the signing library.
      const verified = yield* Effect.promise(async () => {
        const encoder = new TextEncoder();
        const key = await crypto.subtle.importKey(
          "raw",
          encoder.encode(SECRET),
          { name: "HMAC", hash: "SHA-256" },
          false,
          ["verify"],
        );
        return crypto.subtle.verify(
          "HMAC",
          key,
          Buffer.from(signature, "base64url"),
          encoder.encode(payload),
        );
      });
      expect(verified).toBe(true);
      expect(JSON.parse(Buffer.from(payload, "base64url").toString("utf8"))).toEqual({
        v: 1,
        sub: "user_1",
        email: "a@b.co",
        exp: 600,
      });
    }).pipe(provide(fakeHanzoChat({}).client)),
  );

  it.effect("asks Hanzo Chat for the link with the shared secret", () => {
    const hanzoChat = fakeHanzoChat({
      link: { email: "theo@hanzo.chat", plan: "premier", linkedAt: 1 },
    });
    return Effect.gen(function* () {
      const accounts = yield* HanzoChatAccounts.HanzoChatAccounts;
      const link = yield* accounts.getLink({ userId: "user_1" });
      expect(link).toEqual({ email: "theo@hanzo.chat", plan: "premier" });
      expect(hanzoChat.requests).toEqual([
        {
          url: "https://chat.example.test/api/hanzo/link",
          authorization: `Bearer ${SECRET}`,
          body: { userId: "user_1" },
        },
      ]);
    }).pipe(provide(hanzoChat.client));
  });

  it.effect("fails instead of guessing when Hanzo Chat errors", () =>
    Effect.gen(function* () {
      const accounts = yield* HanzoChatAccounts.HanzoChatAccounts;
      const error = yield* Effect.flip(accounts.getLink({ userId: "user_1" }));
      expect(error._tag).toBe("HanzoChatRequestFailed");
    }).pipe(provide(fakeHanzoChat({ error: "nope" }, 401).client)),
  );

  it.effect("times out when Hanzo Chat stalls while sending the body", () => {
    const client = HttpClient.make((request) =>
      Effect.succeed(
        HttpClientResponse.fromWeb(
          request,
          new Response(new ReadableStream({ start: () => {} }), { status: 200 }),
        ),
      ),
    );
    return Effect.gen(function* () {
      const accounts = yield* HanzoChatAccounts.HanzoChatAccounts;
      const fiber = yield* Effect.forkChild(Effect.flip(accounts.getLink({ userId: "user_1" })));
      yield* TestClock.adjust("5 seconds");
      const error = yield* Fiber.join(fiber);
      expect(error._tag).toBe("HanzoChatRequestFailed");
    }).pipe(provide(client));
  });

  it.effect("refuses to link an account whose token carries no email", () =>
    Effect.gen(function* () {
      const accounts = yield* HanzoChatAccounts.HanzoChatAccounts;
      const error = yield* Effect.flip(accounts.createLinkUrl({ userId: "u", email: undefined }));
      expect(error._tag).toBe("AccountEmailMissing");
    }).pipe(provide(fakeHanzoChat({}).client)),
  );

  it.effect("reports itself unavailable without a shared secret", () =>
    Effect.gen(function* () {
      const accounts = yield* HanzoChatAccounts.HanzoChatAccounts;
      expect(accounts.available).toBe(false);
      const error = yield* Effect.flip(accounts.getLink({ userId: "user_1" }));
      expect(error._tag).toBe("HanzoChatNotConfigured");
    }).pipe(provide(fakeHanzoChat({}).client, null)),
  );
});
