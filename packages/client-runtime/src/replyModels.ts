/**
 * Which catalog models may write a conversation's replies. `/v1/models` says what each one
 * returns: a chat model lists `text` in `outputs` and answers at the chat-completions endpoint;
 * a decision model (Kai) lists `decision` and answers at `/v1/decisions`. Decision models run
 * inside the product (gate scoring, routing) and are never the model a person picks.
 * One predicate for /chat, /dev and the desktop app.
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
