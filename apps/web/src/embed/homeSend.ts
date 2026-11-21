import {
  attachFile,
  carriedIn,
  compose,
  readableFiles,
  retrieve,
  type CloudChatAttachment,
  type CloudChatAuth,
  type CloudChatMessage,
  type CloudChatOptions,
  type CloudChatTurn,
  type FileRef,
  type PersonaRecord,
  type WorkFile,
} from "@hanzodev/client-runtime/cloud-chat";

/** How long a send waits for a freshly attached file to have passages before it says so. */
export const READ_WAIT_MS = 30_000;

export interface PreparedTurn {
  readonly prompt: string;
  readonly attachments: ReadonlyArray<CloudChatAttachment>;
  readonly turn: CloudChatTurn;
}

const refOf = (file: WorkFile): FileRef => ({ id: file.id, name: file.name, type: file.type, size: file.size });

const unread = (files: ReadonlyArray<WorkFile>): string =>
  files.some((file) => file.status === "failed")
    ? "a file could not be read"
    : "still indexing; try again in a moment";

/**
 * One turn as sent. Documents go by reference: uploaded to the workspace, indexed, and the passages
 * that answer the question ride the turn with their citations (files earlier turns carried are read
 * again). Images and text files stay inline. A persona's instructions ride as the system message
 * and its model answers.
 */
export async function prepareTurn(input: {
  readonly auth: CloudChatAuth;
  readonly options?: CloudChatOptions | undefined;
  readonly prompt: string;
  readonly attachments: ReadonlyArray<CloudChatAttachment>;
  readonly history: ReadonlyArray<CloudChatMessage>;
  readonly persona: PersonaRecord | null;
  readonly say: (message: string | null) => void;
}): Promise<PreparedTurn> {
  const turn: CloudChatTurn = input.persona
    ? { system: input.persona.instructions ?? undefined, model: input.persona.model ?? undefined }
    : {};
  const documents = input.attachments.filter((a): a is Extract<CloudChatAttachment, { kind: "file" }> => a.kind === "file");
  const inline = input.attachments.filter((a) => a.kind !== "file");
  const reused = carriedIn(input.history);
  if (documents.length === 0 && reused.length === 0)
    return { prompt: input.prompt, attachments: input.attachments, turn };
  let why: string | undefined;
  let landed: WorkFile[] = [];
  try {
    input.say(`Uploading ${documents.map((d) => d.name).join(", ")}`);
    landed = await Promise.all(
      documents.map(async (d) => {
        const bytes = await (await fetch(d.dataUrl)).blob();
        return attachFile(input.auth, { name: d.name, type: bytes.type, bytes }, input.options);
      }),
    );
  } catch (cause) {
    why = cause instanceof Error ? cause.message : "a file could not be added";
  }
  try {
    const attached = landed.map(refOf);
    const earlier = reused.filter((file) => !attached.some((a) => a.id === file.id));
    const all = [...attached, ...earlier];
    if (all.length === 0) return { prompt: compose(input.prompt, { attached: [], reused: [] }, null), attachments: inline, turn };
    input.say(`Reading ${all.map((f) => f.name).join(", ")}`);
    const now = landed.length > 0 ? await readableFiles(input.auth, landed, READ_WAIT_MS, input.options) : [];
    let grounds = null;
    try {
      grounds = await retrieve(
        input.auth,
        input.prompt || `What is in ${all.map((f) => f.name).join(", ")}?`,
        all.map((f) => f.id),
        8,
        input.options,
      );
    } catch {
      why = why ?? "the workspace could not be read just now";
    }
    if (!grounds?.passages.length) why = why ?? unread(now);
    return { prompt: compose(input.prompt, { attached, reused: earlier }, grounds, why), attachments: inline, turn };
  } finally {
    input.say(null);
  }
}
