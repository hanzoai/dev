/**
 * The markdown an agent writes, read into blocks and inline pieces: paragraphs,
 * headings, lists, quotes, rules, tables and fenced code; inline code, bold,
 * italics and links. What prose.tsx draws, as plain values.
 *
 * A link keeps an address only when it is http(s); anything else, and every
 * image, is its words.
 */
export type Block =
  | { kind: 'paragraph'; text: string }
  | { kind: 'heading'; level: number; text: string }
  | { kind: 'code'; lang: string; text: string }
  | { kind: 'list'; ordered: boolean; start: number; items: { text: string; depth: number }[] }
  | { kind: 'quote'; text: string }
  | { kind: 'rule' }
  | { kind: 'table'; head: string[]; rows: string[][] }

const FENCE = /^\s{0,3}(```+|~~~+)\s*([\w+#.-]*)\s*$/
const HEADING = /^\s{0,3}(#{1,6})\s+(.*?)\s*#*\s*$/
const ITEM = /^(\s*)([-*+]|\d{1,9}[.)])\s+(.*)$/
const RULE = /^\s{0,3}([-*_])(\s*\1){2,}\s*$/
const QUOTE = /^\s{0,3}>\s?(.*)$/
const ROW = /^\s*\|(.*)\|\s*$/
const DIVIDER = /^\s*\|?\s*:?-{3,}:?\s*(\|\s*:?-{3,}:?\s*)*\|?\s*$/

/** A table row's cells. Only a line ROW matched is one. */
const cells = (line: string): string[] => ROW.exec(line)![1]!.split('|').map((c) => c.trim())

/** The blocks of a markdown text, in order. */
export function blocks(md: string): Block[] {
  const lines = md.replace(/\r\n?/g, '\n').split('\n')
  const out: Block[] = []
  let para: string[] = []
  const flush = () => {
    if (para.length) out.push({ kind: 'paragraph', text: para.join(' ').trim() })
    para = []
  }
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!
    const fence = FENCE.exec(line)
    if (fence) {
      flush()
      const body: string[] = []
      i++
      while (i < lines.length && !lines[i]!.trim().startsWith(fence[1]!)) body.push(lines[i++]!)
      out.push({ kind: 'code', lang: fence[2]!, text: body.join('\n') })
      continue
    }
    if (!line.trim()) {
      flush()
      continue
    }
    const heading = HEADING.exec(line)
    if (heading) {
      flush()
      out.push({ kind: 'heading', level: heading[1]!.length, text: heading[2]! })
      continue
    }
    if (RULE.test(line)) {
      flush()
      out.push({ kind: 'rule' })
      continue
    }
    if (ROW.test(line) && DIVIDER.test(lines[i + 1] ?? '')) {
      flush()
      const head = cells(line)
      const rows: string[][] = []
      i += 2
      while (i < lines.length && ROW.test(lines[i]!)) rows.push(cells(lines[i++]!))
      i--
      out.push({ kind: 'table', head, rows })
      continue
    }
    const quote = QUOTE.exec(line)
    if (quote) {
      flush()
      const body = [quote[1]!]
      while (i + 1 < lines.length && QUOTE.test(lines[i + 1]!)) body.push(QUOTE.exec(lines[++i]!)![1]!)
      out.push({ kind: 'quote', text: body.join(' ').trim() })
      continue
    }
    const item = ITEM.exec(line)
    if (item) {
      flush()
      const ordered = /\d/.test(item[2]!)
      const items: { text: string; depth: number }[] = []
      const start = ordered ? parseInt(item[2]!, 10) : 1
      let j = i
      while (j < lines.length) {
        const m = ITEM.exec(lines[j]!)
        if (m) {
          items.push({ text: m[3]!, depth: Math.min(3, Math.floor(m[1]!.replace(/\t/g, '  ').length / 2)) })
          j++
          continue
        }
        // A line indented under an item continues it.
        if (lines[j]!.trim() && /^\s{2,}/.test(lines[j]!)) {
          items[items.length - 1]!.text += ` ${lines[j]!.trim()}`
          j++
          continue
        }
        break
      }
      out.push({ kind: 'list', ordered, start, items })
      i = j - 1
      continue
    }
    para.push(line.trim())
  }
  flush()
  return out
}

export type Span =
  | { kind: 'text' | 'code' | 'strong' | 'em'; text: string }
  | { kind: 'link'; text: string; href: string }

const INLINE =
  /(`+)([^`]+?)\1|!?\[([^\]]+)\]\(((?:[^()\s]|\([^()\s]*\))+)(?:\s+"[^"]*")?\)|\*\*(.+?)\*\*|__(.+?)__|\*([^*\s](?:[^*]*[^*\s])?)\*|(?<![\w])_([^_\s](?:[^_]*[^_\s])?)_(?![\w])|(https?:\/\/[^\s<>()]+[^\s<>().,;:!?'"])/g

const safe = (href: string): string => (/^https?:\/\/[^\s]+$/i.test(href) ? href : '')

/** A line's inline pieces. A link to anything but http(s) is its words. */
export function spans(text: string): Span[] {
  const out: Span[] = []
  let at = 0
  for (const m of text.matchAll(INLINE)) {
    if (m.index! > at) out.push({ kind: 'text', text: text.slice(at, m.index) })
    if (m[2] !== undefined) out.push({ kind: 'code', text: m[2] })
    else if (m[3] !== undefined) {
      const href = safe(m[4]!)
      out.push(href && !m[0].startsWith('!') ? { kind: 'link', text: m[3], href } : { kind: 'text', text: m[3] })
    } else if (m[5] !== undefined || m[6] !== undefined) out.push({ kind: 'strong', text: (m[5] ?? m[6])! })
    else if (m[7] !== undefined || m[8] !== undefined) out.push({ kind: 'em', text: (m[7] ?? m[8])! })
    // What is left is the bare address, the only other thing INLINE matches.
    else out.push({ kind: 'link', text: m[9]!, href: m[9]! })
    at = m.index! + m[0].length
  }
  if (at < text.length) out.push({ kind: 'text', text: text.slice(at) })
  return out
}
