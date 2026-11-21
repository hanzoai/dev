import * as ExpoCrypto from "expo-crypto";

// Hermes has no `globalThis.crypto`; PKCE, OAuth state and device ids need getRandomValues.
if (typeof globalThis.crypto?.getRandomValues !== "function") {
  Object.defineProperty(globalThis, "crypto", {
    configurable: true,
    writable: true,
    value: {
      ...(globalThis.crypto as object | undefined),
      getRandomValues: <T extends ArrayBufferView | null>(array: T): T =>
        ExpoCrypto.getRandomValues(array as never) as T,
    },
  });
}
