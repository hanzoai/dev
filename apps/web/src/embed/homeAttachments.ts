import type { CloudChatAttachment } from "@hanzodev/client-runtime/cloud-chat";

import type { ComposerFileAttachment, ComposerImageAttachment } from "../composerDraftStore";

const dataUrl = (file: Blob): Promise<string> =>
  new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => resolve(String(reader.result));
    reader.onerror = () => reject(reader.error);
    reader.readAsDataURL(file);
  });

const isText = (file: File): boolean =>
  file.type.startsWith("text/") ||
  /\.(md|txt|json|ya?ml|toml|csv|tsx?|jsx?|py|go|rs|html?|css|sh)$/i.test(file.name);

/**
 * What the composer holds, read into what a cloud turn carries: images as data URLs, text files
 * inline, and any other file (a PDF) as a data URL the model reads itself. A file with no bytes
 * (one that needs reattaching) is left out.
 */
export async function readAttachments(
  images: ReadonlyArray<ComposerImageAttachment>,
  files: ReadonlyArray<ComposerFileAttachment>,
): Promise<ReadonlyArray<CloudChatAttachment>> {
  const out: CloudChatAttachment[] = [];
  for (const image of images) out.push({ kind: "image", dataUrl: await dataUrl(image.file) });
  for (const entry of files) {
    const file = entry.file;
    if (file === null) continue;
    out.push(
      isText(file)
        ? { kind: "text", name: file.name, text: await file.text() }
        : { kind: "file", name: file.name, dataUrl: await dataUrl(file) },
    );
  }
  return out;
}
