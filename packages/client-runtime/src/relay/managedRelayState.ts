import { msg } from "gt-i18n";
import { MarkedText, messageFromError, translatableMessage } from "../errors/translatable.ts";
import type { EnvironmentId } from "@hanzodev/contracts";
import { decodeRelayJwt } from "@hanzodev/shared/relayJwt";
import * as Cause from "effect/Cause";
import * as Clock from "effect/Clock";
import * as Effect from "effect/Effect";
import * as Option from "effect/Option";
import * as Schema from "effect/Schema";
import * as Stream from "effect/Stream";
import { AsyncResult, Atom, AtomRegistry } from "effect/reactivity";

import { findErrorTraceId } from "../errors/errorTrace.ts";
import * as ManagedRelay from "./managedRelay.ts";
import { relayProtectedErrorText } from "./errorPresentation.ts";

const DEFAULT_STALE_TIME_MS = 15_000;
const DEFAULT_IDLE_TTL_MS = 5 * 60_000;
const IAM_TOKEN_EXPIRY_SKEW_MS = 5_000;
// The IAM client retries a failed token request itself, but waits 3 s before the first
// retry and longer before each next one. A fresh request usually answers at
// once, so a slow read is replaced instead of waited out.
const IAM_TOKEN_READ_TIMEOUT_MS = 4_000;
const IAM_TOKEN_READ_ATTEMPTS = 3;
const isManagedRelayRequestFailedError = Schema.is(ManagedRelay.ManagedRelayRequestFailedError);

export interface ManagedRelaySession {
  readonly accountId: string;
  readonly readIamToken: () => Effect.Effect<string | null, ManagedRelaySessionError>;
}

export interface ManagedRelaySessionInput {
  readonly accountId: string;
  readonly readIamToken: () => Promise<string | null>;
}

interface ManagedRelaySessionControl {
  readonly updateReadIamToken: (
    readIamToken: ManagedRelaySessionInput["readIamToken"],
  ) => void;
}

export interface ManagedRelaySnapshotState<A> {
  readonly data: A | null;
  readonly error: string | null;
  /** `error` as marked text, when the failure carries it. */
  readonly i18nError?: MarkedText;
  readonly errorTraceId: string | null;
  readonly isPending: boolean;
}

export interface ManagedRelayQueryEvent {
  readonly operation: "environments" | "devices" | "hanzochat-link";
  readonly stage: "iam-token" | "relay-request";
  readonly phase: "start" | "success" | "failure";
  readonly accountId: string;
  readonly environmentId?: string;
  readonly message?: string;
  readonly traceId?: string | null;
}

export class ManagedRelaySessionError extends Schema.TaggedError<ManagedRelaySessionError>()(
  "ManagedRelaySessionError",
  {
    message: Schema.String,
    i18nMessage: Schema.optionalKey(MarkedText),
    cause: Schema.optional(Schema.Defect()),
  },
) {}

export const managedRelaySessionAtom = Atom.make<ManagedRelaySession | null>(null).pipe(
  Atom.keepAlive,
  Atom.withLabel("managed-relay:session"),
);

const managedRelaySessionControls = new WeakMap<ManagedRelaySession, ManagedRelaySessionControl>();

export function createManagedRelaySession(input: ManagedRelaySessionInput): ManagedRelaySession {
  let cachedToken: { readonly token: string; readonly expiresAtMillis: number } | null = null;
  let pendingToken: {
    readonly operation: Promise<string | null>;
    readonly startedAtMillis: number;
  } | null = null;
  let readIamToken = input.readIamToken;
  let tokenProviderGeneration = 0;

  const readCachedIamToken = async (nowMillis: number): Promise<string | null> => {
    if (cachedToken && cachedToken.expiresAtMillis > nowMillis + IAM_TOKEN_EXPIRY_SKEW_MS) {
      return cachedToken.token;
    }
    // A read that outlived its timeout is left running, since it still fills
    // the cache, but later readers start a fresh one instead of joining it.
    if (pendingToken && nowMillis - pendingToken.startedAtMillis < IAM_TOKEN_READ_TIMEOUT_MS) {
      return await pendingToken.operation;
    }

    const operationGeneration = tokenProviderGeneration;
    const operation = readIamToken().then((token) => {
      if (operationGeneration !== tokenProviderGeneration) {
        return token;
      }
      if (!token) {
        cachedToken = null;
        return null;
      }
      const expiresAtSeconds = Effect.runSync(
        decodeRelayJwt(token).pipe(
          Effect.map((claims) => claims.exp),
          Effect.orElseSucceed(() => undefined),
        ),
      );
      cachedToken =
        typeof expiresAtSeconds === "number"
          ? { token, expiresAtMillis: expiresAtSeconds * 1_000 }
          : null;
      return token;
    });
    const pending = { operation, startedAtMillis: nowMillis };
    pendingToken = pending;
    try {
      return await operation;
    } finally {
      if (pendingToken === pending) {
        pendingToken = null;
      }
    }
  };

  const session: ManagedRelaySession = {
    accountId: input.accountId,
    readIamToken: Effect.fn("clientRuntime.managedRelaySession.readIamToken")(function* () {
      for (let attempt = 0; attempt < IAM_TOKEN_READ_ATTEMPTS; attempt += 1) {
        const nowMillis = yield* Clock.currentTimeMillis;
        const token = yield* Effect.tryPromise({
          try: () => readCachedIamToken(nowMillis),
          catch: (cause) =>
            new ManagedRelaySessionError({
              ...translatableMessage(msg("Could not obtain the Hanzo Link session token.")),
              cause,
            }),
        }).pipe(Effect.timeoutOption(IAM_TOKEN_READ_TIMEOUT_MS));
        if (Option.isSome(token)) return token.value;
      }
      return yield* new ManagedRelaySessionError(
        translatableMessage(msg("Timed out obtaining the Hanzo Link session token.")),
      );
    }),
  };
  managedRelaySessionControls.set(session, {
    updateReadIamToken: (nextReadIamToken) => {
      readIamToken = nextReadIamToken;
      tokenProviderGeneration += 1;
      pendingToken = null;
    },
  });
  return session;
}

export function setManagedRelaySession(
  registry: AtomRegistry.AtomRegistry,
  input: ManagedRelaySessionInput | null,
): void {
  const current = registry.get(managedRelaySessionAtom);
  if (input === null) {
    if (current !== null) {
      registry.set(managedRelaySessionAtom, null);
    }
    return;
  }
  if (current?.accountId === input.accountId) {
    const control = managedRelaySessionControls.get(current);
    if (control) {
      // The session can replace its token reader during routine same-account refreshes.
      // Keep the session stable so those refreshes do not invalidate queries or reconnect leases.
      control.updateReadIamToken(input.readIamToken);
      return;
    }
  }
  registry.set(managedRelaySessionAtom, createManagedRelaySession(input));
}

export function managedRelayAccountChanges(
  registry: AtomRegistry.AtomRegistry,
): Stream.Stream<string | null> {
  return AtomRegistry.toStream(registry, managedRelaySessionAtom).pipe(
    Stream.map((session) => session?.accountId ?? null),
    Stream.changes,
    Stream.drop(1),
  );
}

function readSessionIamToken(
  session: ManagedRelaySession,
): Effect.Effect<string, ManagedRelaySessionError> {
  return session.readIamToken().pipe(
    Effect.filterOrFail(
      (token): token is string => Boolean(token),
      () =>
        new ManagedRelaySessionError(
          translatableMessage(msg("The Hanzo Link session token is unavailable.")),
        ),
    ),
  );
}

const SIGN_IN_BEFORE = {
  deregister: msg("Sign in to Hanzo Link before deregistering an environment."),
  "link-hanzochat": msg("Sign in to Hanzo Link before linking Hanzo Chat."),
  "unlink-hanzochat": msg("Sign in to Hanzo Link before unlinking Hanzo Chat."),
};

function requireSessionIamToken(
  registry: AtomRegistry.AtomRegistry,
  accountId: string,
  action: "deregister" | "link-hanzochat" | "unlink-hanzochat",
): Effect.Effect<string, ManagedRelaySessionError> {
  const session = registry.get(managedRelaySessionAtom);
  if (!session || session.accountId !== accountId) {
    return Effect.fail(new ManagedRelaySessionError(translatableMessage(SIGN_IN_BEFORE[action])));
  }
  return readSessionIamToken(session);
}

export const deregisterManagedRelayEnvironment = Effect.fn(
  "clientRuntime.managedRelaySession.deregisterEnvironment",
)(function* (
  registry: AtomRegistry.AtomRegistry,
  input: { readonly accountId: string; readonly environmentId: EnvironmentId },
) {
  const iamToken = yield* requireSessionIamToken(registry, input.accountId, "deregister");
  const relay = yield* ManagedRelay.ManagedRelayClient;
  yield* relay.unlinkEnvironment({ iamToken, environmentId: input.environmentId });
});

/** Returns the Hanzo Chat page URL where the signed-in user confirms the link. */
export const createManagedRelayHanzoChatLinkRequest = Effect.fn(
  "clientRuntime.managedRelaySession.createHanzoChatLinkRequest",
)(function* (registry: AtomRegistry.AtomRegistry, input: { readonly accountId: string }) {
  const iamToken = yield* requireSessionIamToken(registry, input.accountId, "link-hanzochat");
  const relay = yield* ManagedRelay.ManagedRelayClient;
  return (yield* relay.createHanzoChatLinkRequest({ iamToken })).url;
});

export const unlinkManagedRelayHanzoChat = Effect.fn(
  "clientRuntime.managedRelaySession.unlinkHanzoChat",
)(function* (registry: AtomRegistry.AtomRegistry, input: { readonly accountId: string }) {
  const iamToken = yield* requireSessionIamToken(registry, input.accountId, "unlink-hanzochat");
  const relay = yield* ManagedRelay.ManagedRelayClient;
  yield* relay.unlinkHanzoChat({ iamToken });
});

function requireIamToken(
  get: Atom.AtomContext,
  accountId: string,
): Effect.Effect<string, ManagedRelaySessionError> {
  const session = get(managedRelaySessionAtom);
  if (!session || session.accountId !== accountId) {
    return Effect.fail(
      new ManagedRelaySessionError(
        translatableMessage(msg("Sign in to Hanzo Link before loading relay data.")),
      ),
    );
  }
  return readSessionIamToken(session);
}

function describeManagedRelayFailure(cause: Cause.Cause<unknown>): {
  readonly message: string;
  readonly i18nMessage?: MarkedText;
  readonly traceId: string | null;
} {
  const error = Cause.squash(cause);
  const traceId = findErrorTraceId(error);
  if (isManagedRelayRequestFailedError(error) && error.relayError) {
    return { ...translatableMessage(...relayProtectedErrorText(error.relayError)), traceId };
  }
  return error instanceof Error
    ? { ...messageFromError(error), traceId }
    : { ...translatableMessage(msg("Could not load Hanzo Link data.")), traceId };
}

export function readManagedRelaySnapshotState<A>(
  result: AsyncResult.AsyncResult<A, unknown>,
): ManagedRelaySnapshotState<A> {
  const failure = result._tag === "Failure" ? describeManagedRelayFailure(result.cause) : null;
  return {
    data: AsyncResult.getOrElse(result, () => null),
    error: failure?.message ?? null,
    ...(failure?.i18nMessage !== undefined ? { i18nError: failure.i18nMessage } : {}),
    errorTraceId: failure?.traceId ?? null,
    isPending: result.waiting,
  };
}

// Logged where the listing runs so each failed fetch reports once, however many
// components read the atom.
const logListingFailure =
  (label: string) =>
  <A, E, R>(self: Effect.Effect<A, E, R>): Effect.Effect<A, E, R> =>
    self.pipe(
      Effect.tapCause((cause) => {
        if (Cause.hasInterruptsOnly(cause)) return Effect.void;
        const { message, traceId } = describeManagedRelayFailure(cause);
        return Effect.logError(`[hanzo-cloud] Relay ${label} listing failed`).pipe(
          Effect.annotateLogs({ message, traceId }),
        );
      }),
    );

export function createManagedRelayQueryManager(
  runtime: Atom.AtomRuntime<ManagedRelay.ManagedRelayClient>,
  options?: {
    readonly staleTimeMs?: number;
    readonly idleTtlMs?: number;
    readonly onQueryEvent?: (event: ManagedRelayQueryEvent) => void;
  },
) {
  const staleTime = options?.staleTimeMs ?? DEFAULT_STALE_TIME_MS;
  const idleTtl = options?.idleTtlMs ?? DEFAULT_IDLE_TTL_MS;
  const observe = Effect.fn("managedRelayState.observe")(function* <A, E, R>(
    input: Omit<ManagedRelayQueryEvent, "phase" | "message" | "traceId">,
    effect: Effect.Effect<A, E, R>,
  ): Effect.fn.Return<A, E, R> {
    options?.onQueryEvent?.({ ...input, phase: "start" });
    return yield* effect.pipe(
      Effect.onExit((exit) =>
        Effect.sync(() => {
          if (exit._tag === "Success") {
            options?.onQueryEvent?.({ ...input, phase: "success" });
            return;
          }
          const error = Cause.squash(exit.cause);
          options?.onQueryEvent?.({
            ...input,
            phase: "failure",
            message: error instanceof Error ? error.message : String(error),
            traceId: findErrorTraceId(error),
          });
        }),
      ),
    );
  });

  const environmentsAtom = Atom.family((accountId: string) =>
    runtime
      .atom((get) =>
        Effect.gen(function* () {
          const base = { operation: "environments" as const, accountId };
          const iamToken = yield* observe(
            { ...base, stage: "iam-token" },
            requireIamToken(get, accountId),
          );
          const relay = yield* ManagedRelay.ManagedRelayClient;
          return yield* observe(
            { ...base, stage: "relay-request" },
            relay.listEnvironments({ iamToken }),
          );
        }).pipe(logListingFailure("environment")),
      )
      .pipe(
        Atom.swr({ staleTime, revalidateOnMount: true }),
        Atom.setIdleTTL(idleTtl),
        Atom.withLabel(`managed-relay:environments:${accountId}`),
      ),
  );

  const devicesAtom = Atom.family((accountId: string) =>
    runtime
      .atom((get) =>
        Effect.gen(function* () {
          const base = { operation: "devices" as const, accountId };
          const iamToken = yield* observe(
            { ...base, stage: "iam-token" },
            requireIamToken(get, accountId),
          );
          const relay = yield* ManagedRelay.ManagedRelayClient;
          return yield* observe(
            { ...base, stage: "relay-request" },
            relay.listDevices({ iamToken }),
          );
        }).pipe(logListingFailure("device")),
      )
      .pipe(
        Atom.swr({ staleTime, revalidateOnMount: true }),
        Atom.setIdleTTL(idleTtl),
        Atom.withLabel(`managed-relay:devices:${accountId}`),
      ),
  );

  const hanzoChatLinkAtom = Atom.family((accountId: string) =>
    runtime
      .atom((get) =>
        Effect.gen(function* () {
          const base = { operation: "hanzochat-link" as const, accountId };
          const iamToken = yield* observe(
            { ...base, stage: "iam-token" },
            requireIamToken(get, accountId),
          );
          const relay = yield* ManagedRelay.ManagedRelayClient;
          return yield* observe(
            { ...base, stage: "relay-request" },
            relay.getHanzoChatLink({ iamToken }),
          );
        }),
      )
      .pipe(
        Atom.swr({ staleTime, revalidateOnMount: true }),
        Atom.setIdleTTL(idleTtl),
        Atom.withLabel(`managed-relay:hanzochat-link:${accountId}`),
      ),
  );

  return {
    environmentsAtom,
    devicesAtom,
    hanzoChatLinkAtom,
    refreshEnvironments(registry: AtomRegistry.AtomRegistry, accountId: string): void {
      registry.refresh(environmentsAtom(accountId));
    },
    refreshDevices(registry: AtomRegistry.AtomRegistry, accountId: string): void {
      registry.refresh(devicesAtom(accountId));
    },
    refreshHanzoChatLink(registry: AtomRegistry.AtomRegistry, accountId: string): void {
      registry.refresh(hanzoChatLinkAtom(accountId));
    },
  };
}
