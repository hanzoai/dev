import { requireOptionalNativeModule } from "expo";

/** A Zen model this phone can run, as the native module lists it. */
export interface OnDeviceModel {
  readonly id: string;
  readonly name: string;
  readonly bytes: number;
  readonly memoryGb: number;
  readonly installed: boolean;
  /** The phone has the memory for it. */
  readonly fits: boolean;
}

export interface OnDeviceUsage {
  readonly prompt_tokens?: number;
  readonly completion_tokens?: number;
  readonly prompt_tok_per_sec?: number;
  readonly tok_per_sec?: number;
  readonly seconds?: number;
}

interface Subscription {
  remove(): void;
}

interface HanzoEngineNative {
  available(): boolean;
  memory(): { total: number; footprint: number };
  models(): OnDeviceModel[];
  download(id: string): Promise<void>;
  remove(id: string): Promise<void>;
  chat(requestId: string, id: string, messages: string, maxTokens: number): Promise<string>;
  cancel(requestId: string): void;
  addListener(
    event: "onToken",
    listener: (e: { requestId: string; text: string }) => void,
  ): Subscription;
  addListener(
    event: "onProgress",
    listener: (e: { id: string; done: number; total: number }) => void,
  ): Subscription;
}

const native = requireOptionalNativeModule<HanzoEngineNative>("HanzoEngine");

/** Hanzo Engine is linked into this build, so a Zen model can run on the phone. */
export const onDeviceEngineAvailable: boolean = native?.available() === true;

export const onDeviceMemory = (): { total: number; footprint: number } | null =>
  native?.memory() ?? null;

export const onDeviceModels = (): ReadonlyArray<OnDeviceModel> => native?.models() ?? [];

export function downloadOnDeviceModel(
  id: string,
  onProgress?: (done: number, total: number) => void,
): Promise<void> {
  if (native === null) return Promise.reject(new Error("Hanzo Engine is not in this build"));
  const sub = onProgress
    ? native.addListener("onProgress", (e) => e.id === id && onProgress(e.done, e.total))
    : null;
  return native.download(id).finally(() => sub?.remove());
}

let counter = 0;

/** Streams a reply from the on-device engine. Resolves with the usage the engine reported. */
export async function chatOnDevice(input: {
  readonly model: string;
  readonly messages: ReadonlyArray<{ readonly role: string; readonly content: string }>;
  readonly maxTokens?: number;
  readonly onDelta: (text: string) => void;
  readonly signal?: AbortSignal | undefined;
}): Promise<OnDeviceUsage> {
  if (native === null) throw new Error("Hanzo Engine is not in this build");
  const requestId = `chat-${Date.now()}-${++counter}`;
  const sub = native.addListener("onToken", (e) => {
    if (e.requestId === requestId) input.onDelta(e.text);
  });
  const cancel = () => native.cancel(requestId);
  input.signal?.addEventListener("abort", cancel, { once: true });
  try {
    const report = await native.chat(
      requestId,
      input.model,
      JSON.stringify(input.messages),
      input.maxTokens ?? 1024,
    );
    return JSON.parse(report) as OnDeviceUsage;
  } finally {
    input.signal?.removeEventListener("abort", cancel);
    sub.remove();
  }
}
