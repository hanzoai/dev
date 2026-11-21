import type { CloudChatAttachment } from "@hanzodev/client-runtime/cloud-chat";

import { imageMimeType } from "@hanzodev/shared/image";

import type { DraftComposerAttachment } from "../../lib/composerImages";

/** Largest file the landing reads into a message; a data URL is a third larger on the wire. */
export const CLOUD_CHAT_FILE_BYTES = 8 * 1024 * 1024;

export type CloudAttachmentKind = "image" | "text" | "pdf" | "other";

const TEXT_NAME =
  /\.(txt|md|markdown|json|ya?ml|toml|csv|tsv|xml|html?|css|log|[cm]?[jt]sx?|py|rb|go|rs|java|kt|swift|c|h|cc|cpp|hpp|sh|sql)$/i;

/** How a draft attachment travels in a chat message. */
export function cloudAttachmentKind(attachment: DraftComposerAttachment): CloudAttachmentKind {
  if (attachment.type === "image" || imageMimeType(attachment) !== null) return "image";
  const mime = attachment.mimeType.toLowerCase();
  if (mime === "application/pdf" || attachment.name.toLowerCase().endsWith(".pdf")) return "pdf";
  if (
    mime.startsWith("text/") ||
    mime === "application/json" ||
    mime === "application/xml" ||
    TEXT_NAME.test(attachment.name)
  )
    return "text";
  return "other";
}

/** Reads draft attachments into what a chat request carries. */
export async function readCloudAttachments(
  attachments: ReadonlyArray<DraftComposerAttachment>,
): Promise<ReadonlyArray<CloudChatAttachment>> {
  const { File } = await import("expo-file-system");
  const out: CloudChatAttachment[] = [];
  for (const attachment of attachments) {
    const kind = cloudAttachmentKind(attachment);
    if (kind === "image" && attachment.type === "image" && attachment.dataUrl !== undefined) {
      out.push({ kind: "image", dataUrl: attachment.dataUrl });
    } else if (attachment.fileUri === undefined) {
      continue;
    } else if (kind === "image") {
      const base64 = await new File(attachment.fileUri).base64();
      out.push({ kind: "image", dataUrl: `data:${attachment.mimeType};base64,${base64}` });
    } else if (kind === "text") {
      out.push({
        kind: "text",
        name: attachment.name,
        text: await new File(attachment.fileUri).text(),
      });
    } else if (kind === "pdf") {
      const base64 = await new File(attachment.fileUri).base64();
      out.push({
        kind: "file",
        name: attachment.name,
        dataUrl: `data:application/pdf;base64,${base64}`,
      });
    }
  }
  return out;
}
