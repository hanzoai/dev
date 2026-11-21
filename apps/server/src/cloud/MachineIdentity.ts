// @effect-diagnostics globalDate:off globalDateInEffect:off
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Option from "effect/Option";

import { sharedIdentityHost } from "../provider/hanzoLocalIdentity.ts";
import { hanzoAccessToken, readSharedIdentity } from "../provider/hanzoSharedIdentityNode.ts";
import { MachineIdentityToken } from "./CliTokenManager.ts";

/** The active machine identity as the token the cloud clients send; refresh stays with the CLI. */
const machineIdentityToken = Effect.gen(function* () {
  const host = yield* sharedIdentityHost();
  return yield* Effect.promise(async () => {
    const accessToken = await hanzoAccessToken(host);
    if (!accessToken) return Option.none();
    const snapshot = await readSharedIdentity(host);
    const active = snapshot.accounts.find((account) => account.id === snapshot.activeAccountId);
    return Option.some({
      accessToken,
      refreshToken: "",
      expiresAtEpochMs: active?.expiresAt ?? Date.now() + 60 * 60_000,
      ...(active ? { identity: active.email ?? active.id } : {}),
    });
  });
});

export const layer = Layer.succeed(MachineIdentityToken, machineIdentityToken);
