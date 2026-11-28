/**
 * Server-Sent Events, read off a fetch body.
 *
 * `EventSource` cannot send an Authorization header, and the session stream
 * refuses a request without a principal, so the stream is read as a plain
 * response body and parsed here. The parser is pure: bytes in, frames and the
 * unfinished tail out, so it is tested without a network.
 *
 * Per the spec a frame ends at a blank line; `event:` names it (default
 * `message`), `data:` lines join with a newline, and a line starting `:` is a
 * comment — the platform's 25-second `: ping` keep-alive.
 */

export interface Frame {
  event: string
  data: string
  id: string
}

/** Split `buffer` into complete frames and the tail that has not ended yet. */
export function parse(buffer: string): { frames: Frame[]; rest: string } {
  const text = buffer.replace(/\r\n?/g, '\n')
  const frames: Frame[] = []
  let start = 0
  for (;;) {
    const end = text.indexOf('\n\n', start)
    if (end === -1) break
    const block = text.slice(start, end)
    start = end + 2
    let event = 'message'
    let id = ''
    const data: string[] = []
    for (const line of block.split('\n')) {
      if (!line || line.startsWith(':')) continue
      const colon = line.indexOf(':')
      const field = colon === -1 ? line : line.slice(0, colon)
      let value = colon === -1 ? '' : line.slice(colon + 1)
      if (value.startsWith(' ')) value = value.slice(1)
      if (field === 'event') event = value
      else if (field === 'data') data.push(value)
      else if (field === 'id') id = value
    }
    if (data.length) frames.push({ event, data: data.join('\n'), id })
  }
  return { frames, rest: text.slice(start) }
}

/** Read `body` to its end, calling `on` for each frame. */
export async function read(
  body: ReadableStream<Uint8Array>,
  on: (f: Frame) => void,
  signal?: AbortSignal,
): Promise<void> {
  const reader = body.getReader()
  const decoder = new TextDecoder()
  let rest = ''
  const stop = () => void reader.cancel().catch(() => {})
  signal?.addEventListener('abort', stop, { once: true })
  try {
    for (;;) {
      const { value, done } = await reader.read()
      if (done) break
      const out = parse(rest + decoder.decode(value, { stream: true }))
      rest = out.rest
      for (const f of out.frames) on(f)
    }
  } finally {
    signal?.removeEventListener('abort', stop)
  }
}
