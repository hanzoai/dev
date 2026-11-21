import { DesktopLinkError } from "@hanzodev/contracts";
import * as Duration from "effect/Duration";
import * as Effect from "effect/Effect";
import * as Option from "effect/Option";
import * as HttpClient from "effect/http/HttpClient";
import * as HttpClientRequest from "effect/http/HttpClientRequest";

import * as CliTokenManager from "./CliTokenManager.ts";
import { apiBaseUrlConfig } from "./publicConfig.ts";

const REQUEST_TIMEOUT = Duration.seconds(10);

export const fail = (reason: DesktopLinkError["reason"], message: string, cause?: unknown) =>
  new DesktopLinkError({ reason, message, ...(cause === undefined ? {} : { cause }) });

export const storageFailure = (cause: unknown) =>
  fail("storage", "Could not read or write this desktop's link on disk.", cause);

const unreachable = (cause: unknown) =>
  fail("unreachable", "Could not reach Hanzo. Check the connection and try again.", cause);

export const reject = (status: number) =>
  status === 401
    ? fail("sign_in_required", "Hanzo did not accept this sign-in. Sign in again.")
    : status === 409
      ? fail("limit", "Hanzo refused this: a limit is reached or the item already exists.")
      : fail("rejected", `Hanzo refused the request (${status}).`);

/** The signed-in user's calls to api.hanzo.ai: the IAM bearer, a timeout, typed errors. */
export const makeSourceApi = Effect.gen(function* () {
  const tokens = yield* CliTokenManager.CloudCliTokenManager;
  const httpClient = yield* HttpClient.HttpClient;
  const apiBaseUrl = yield* apiBaseUrlConfig.pipe(Effect.orDie);

  /** One call to the registry. The status code is the caller's to read, the body is JSON. */
  const call = Effect.fn("SourceApi.call")(function* (input: {
    readonly method: "GET" | "POST" | "DELETE";
    readonly path: string;
    readonly accessToken: string;
    readonly body?: string;
    readonly headers?: Readonly<Record<string, string>>;
  }) {
    const url = `${apiBaseUrl}${input.path}`;
    const base =
      input.method === "GET"
        ? HttpClientRequest.get(url)
        : input.method === "POST"
          ? HttpClientRequest.post(url)
          : HttpClientRequest.delete(url);
    const request = base.pipe(
      HttpClientRequest.bearerToken(input.accessToken),
      HttpClientRequest.setHeaders(input.headers ?? {}),
      input.body === undefined
        ? (self) => self
        : HttpClientRequest.bodyText(input.body, "application/json"),
    );
    return yield* httpClient.execute(request).pipe(
      Effect.flatMap((response) =>
        response.text.pipe(
          Effect.map((text) => ({
            status: response.status,
            json: ((): unknown => {
              try {
                return text.length === 0 ? null : JSON.parse(text);
              } catch {
                return null;
              }
            })(),
          })),
        ),
      ),
      Effect.timeout(REQUEST_TIMEOUT),
      Effect.mapError(unreachable),
    );
  });

  const requireToken = tokens.getExisting.pipe(
    Effect.mapError(() => fail("sign_in_required", "Sign in to Hanzo first.")),
    Effect.flatMap(
      Option.match({
        onNone: () => Effect.fail(fail("sign_in_required", "Sign in to Hanzo first.")),
        onSome: Effect.succeed,
      }),
    ),
  );

  return { call, requireToken };
});
