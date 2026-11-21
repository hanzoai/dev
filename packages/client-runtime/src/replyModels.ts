/**
 * Which catalog models may write a conversation's replies: a chat model lists `text` in its
 * `/v1/models` outputs; a decision model (Kai) lists `decision`, runs inside the product (gate
 * scoring, routing) and is never picked by a person. One predicate for /chat, /dev and desktop.
 */
export interface CatalogModelKind {
  readonly endpoint?: string | undefined;
  readonly outputs?: ReadonlyArray<string> | undefined;
}

export function writesReplies(model: CatalogModelKind): boolean {
  if (model.outputs !== undefined && model.outputs.length > 0) {
    return model.outputs.includes("text");
  }
  return model.endpoint === undefined || model.endpoint === "/v1/chat/completions";
}
