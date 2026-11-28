import {
  type DesktopLinkError,
  type ProviderAccount,
  type ProviderAccountAddInput,
} from "@hanzodev/contracts";
import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Schema from "effect/Schema";

import { fail, makeSourceApi, reject } from "./SourceApi.ts";

const WireAccount = Schema.Struct({
  id: Schema.String,
  provider: Schema.String,
  kind: Schema.Literal("key"),
  label: Schema.String,
  fingerprint: Schema.optional(Schema.String),
  created: Schema.String,
  route: Schema.optional(Schema.Literals(["local", "desktop", "key"])),
});
const WireAccountList = Schema.Struct({ accounts: Schema.Array(WireAccount) });
const decodeAccount = Schema.decodeUnknownEffect(WireAccount);
const decodeAccountList = Schema.decodeUnknownEffect(WireAccountList);
const encodeAddBody = Schema.encodeSync(
  Schema.fromJsonString(
    Schema.Struct({
      provider: Schema.String,
      kind: Schema.Literal("key"),
      label: Schema.String,
      value: Schema.String,
    }),
  ),
);

// An API key is class `key` unless Hanzo says otherwise.
const toAccount = (wire: typeof WireAccount.Type): ProviderAccount => ({
  id: wire.id,
  provider: wire.provider,
  kind: "key",
  label: wire.label,
  fingerprint: wire.fingerprint ?? "",
  created: wire.created,
  route: wire.route ?? "key",
});

const unreadable = (cause: unknown) => fail("rejected", "Hanzo sent an unreadable reply.", cause);

/** HIP-1350 provider accounts. A key goes to Hanzo KMS once; nothing here stores its value. */
export class ProviderAccounts extends Context.Service<
  ProviderAccounts,
  {
    readonly list: Effect.Effect<ReadonlyArray<ProviderAccount>, DesktopLinkError>;
    readonly add: (
      input: typeof ProviderAccountAddInput.Type,
    ) => Effect.Effect<ProviderAccount, DesktopLinkError>;
    readonly remove: (id: string) => Effect.Effect<void, DesktopLinkError>;
  }
>()("hanzo/cloud/ProviderAccounts") {}

export const layer = Layer.effect(
  ProviderAccounts,
  Effect.gen(function* () {
    const { call, requireToken } = yield* makeSourceApi;

    const list = Effect.gen(function* () {
      const token = yield* requireToken;
      const response = yield* call({
        method: "GET",
        path: "/v1/machines/accounts",
        accessToken: token.accessToken,
      });
      if (response.status !== 200) return yield* reject(response.status);
      const wire = yield* decodeAccountList(response.json).pipe(Effect.mapError(unreadable));
      return wire.accounts.map(toAccount);
    }).pipe(Effect.withSpan("ProviderAccounts.list"));

    const add = Effect.fn("ProviderAccounts.add")(function* (
      input: typeof ProviderAccountAddInput.Type,
    ) {
      const token = yield* requireToken;
      const response = yield* call({
        method: "POST",
        path: "/v1/machines/accounts",
        accessToken: token.accessToken,
        body: encodeAddBody({
          provider: input.provider,
          kind: "key",
          label: input.label,
          value: input.value,
        }),
      });
      if (response.status !== 200 && response.status !== 201) {
        return yield* reject(response.status);
      }
      const wire = yield* decodeAccount(response.json).pipe(Effect.mapError(unreadable));
      return toAccount(wire);
    });

    const remove = Effect.fn("ProviderAccounts.remove")(function* (id: string) {
      const token = yield* requireToken;
      const response = yield* call({
        method: "DELETE",
        path: `/v1/machines/accounts/${encodeURIComponent(id)}`,
        accessToken: token.accessToken,
      });
      if (response.status !== 404 && response.status >= 300) {
        return yield* reject(response.status);
      }
    });

    return ProviderAccounts.of({ list, add, remove });
  }),
);
