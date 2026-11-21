/**
 * Incremental Server-Sent Events decoder for OpenAI-compatible chat streams. Network chunks split
 * anywhere, including mid-line and mid-UTF-16 pair boundaries of the transport; `push` buffers the
 * tail until its line ends. `[DONE]` ends the stream.
 */
export type SseEvent = { readonly kind: "delta"; readonly text: string } | { readonly kind: "done" };

export interface SseParser {
  push(chunk: string): ReadonlyArray<SseEvent>;
  flush(): ReadonlyArray<SseEvent>;
}

function parseLine(line: string): SseEvent | null {
  if (!line.startsWith("data:")) return null;
  const payload = line.slice(5).trim();
  if (payload === "") return null;
  if (payload === "[DONE]") return { kind: "done" };
  try {
    const body = JSON.parse(payload) as {
      choices?: ReadonlyArray<{ delta?: { content?: unknown } }>;
    };
    const text = body.choices?.[0]?.delta?.content;
    return typeof text === "string" && text !== "" ? { kind: "delta", text } : null;
  } catch {
    return null;
  }
}

export function createSseParser(): SseParser {
  let buffer = "";
  let finished = false;
  const drain = (lines: ReadonlyArray<string>): ReadonlyArray<SseEvent> => {
    const events: SseEvent[] = [];
    for (const raw of lines) {
      if (finished) break;
      const event = parseLine(raw.replace(/\r$/, ""));
      if (event === null) continue;
      events.push(event);
      if (event.kind === "done") finished = true;
    }
    return events;
  };
  return {
    push(chunk) {
      buffer += chunk;
      const lines = buffer.split("\n");
      buffer = lines.pop() ?? "";
      return drain(lines);
    },
    flush() {
      const rest = buffer;
      buffer = "";
      return drain([rest]);
    },
  };
}
