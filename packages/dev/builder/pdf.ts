/**
 * A PDF, read where it is: its text page by page, and what its structure says
 * about it — version, pages, metadata, encryption, how often it was revised
 * after it was written, and whether each signature still covers the bytes it
 * signed. Nothing leaves the browser.
 *
 * Text comes out of each page's content streams (and the forms they draw),
 * decoded with each font's ToUnicode map: Flate is undone by the platform's own
 * `DecompressionStream`, objects packed into object streams are unpacked, and a
 * font without a map is read as single-byte Latin. What it cannot read it says
 * it cannot: an encrypted file, a page that is only pictures (a scan needs OCR),
 * a stream in a filter other than Flate.
 *
 * A signature is checked for integrity, not trust: that its byte range starts at
 * the first byte, frames exactly its own signature, and reaches the end of the
 * file (or how many bytes were added after it), and that the digest the
 * signature carries (CMS `messageDigest`) is the digest of those bytes. Whether
 * the signer's certificate is trusted is not decided here.
 */

/** A name (`/Type`), a string's bytes, an indirect reference, an operator in a content stream. */
export class Name {
  constructor(readonly n: string) {}
}
export class Str {
  constructor(readonly s: string) {}
}
export class Ref {
  constructor(readonly n: number) {}
}
export class Op {
  constructor(readonly o: string) {}
}
export type Dict = Map<string, Value>
export type Value = number | boolean | null | Name | Str | Ref | Op | Value[] | Dict

/** The largest file read. */
export const LARGEST = 50 << 20

/** Bytes as a string of the same length, one char per byte. */
export function latin(b: Uint8Array): string {
  let s = ''
  for (let i = 0; i < b.length; i += 0x8000) s += String.fromCharCode(...b.subarray(i, i + 0x8000))
  return s
}

const bytesOf = (s: string): Uint8Array => Uint8Array.from(s, (c) => c.charCodeAt(0))

/** Hex digits as bytes, a lone last digit read as if a 0 followed it. */
function unhex(h: string): string {
  const d = h.replace(/[^0-9a-fA-F]/g, '')
  let out = ''
  for (let i = 0; i < d.length; i += 2) out += String.fromCharCode(parseInt(d.slice(i, i + 2).padEnd(2, '0'), 16))
  return out
}

const SPACE = /[\0\t\n\f\r ]/
const DELIM = /[()<>[\]{}/%]/

/** A cursor over PDF syntax: the objects of a file, or the operators of a content stream. */
export class Lexer {
  constructor(
    readonly src: string,
    public at = 0,
  ) {}

  skip(): void {
    const s = this.src
    while (this.at < s.length) {
      const c = s[this.at]!
      if (SPACE.test(c)) this.at++
      else if (c === '%') while (this.at < s.length && s[this.at] !== '\n' && s[this.at] !== '\r') this.at++
      else break
    }
  }

  /** The next value, or undefined at the end. */
  next(): Value | undefined {
    this.skip()
    const s = this.src
    if (this.at >= s.length) return undefined
    const c = s[this.at]!
    if (c === '<' && s[this.at + 1] === '<') {
      this.at += 2
      const d: Dict = new Map()
      for (;;) {
        this.skip()
        if (this.at >= s.length) return d
        if (s.startsWith('>>', this.at)) {
          this.at += 2
          return d
        }
        const k = this.next()
        if (!(k instanceof Name)) {
          if (k === undefined) return d
          continue
        }
        const v = this.next()
        if (v !== undefined) d.set(k.n, v)
      }
    }
    if (c === '<') {
      const end = s.indexOf('>', this.at)
      const out = unhex(s.slice(this.at + 1, end < 0 ? s.length : end))
      this.at = end < 0 ? s.length : end + 1
      return new Str(out)
    }
    if (c === '[') {
      this.at++
      const a: Value[] = []
      for (;;) {
        this.skip()
        if (this.at >= s.length) return a
        if (s[this.at] === ']') {
          this.at++
          return a
        }
        const v = this.next()
        if (v === undefined) return a
        a.push(v)
      }
    }
    if (c === '(') return this.literal()
    if (c === '/') {
      let e = this.at + 1
      while (e < s.length && !SPACE.test(s[e]!) && !DELIM.test(s[e]!)) e++
      const n = s.slice(this.at + 1, e).replace(/#([0-9a-fA-F]{2})/g, (_, h: string) => String.fromCharCode(parseInt(h, 16)))
      this.at = e
      return new Name(n)
    }
    if (c === ']' || c === '>' || c === ')' || c === '{' || c === '}') {
      this.at++
      return this.next()
    }
    let e = this.at
    while (e < s.length && !SPACE.test(s[e]!) && !DELIM.test(s[e]!)) e++
    const word = s.slice(this.at, e)
    this.at = e
    if (/^[+-]?(\d+\.?\d*|\.\d+)$/.test(word)) {
      // `12 0 R` is a reference; anything else is the number it reads as.
      const m = /^\s+(\d+)\s+R(?=[\s/<>[\]()%]|$)/.exec(s.slice(e, e + 32))
      if (m && /^\d+$/.test(word)) {
        this.at = e + m[0].length
        return new Ref(Number(word))
      }
      return Number(word)
    }
    if (word === 'true') return true
    if (word === 'false') return false
    if (word === 'null') return null
    return new Op(word)
  }

  private literal(): Str {
    const s = this.src
    let depth = 0
    let out = ''
    let i = this.at
    for (; i < s.length; i++) {
      const c = s[i]!
      if (c === '(') {
        if (depth++ === 0) continue
      } else if (c === ')') {
        if (--depth === 0) break
      } else if (c === '\\') {
        const d = s[++i] ?? ''
        const esc: Record<string, string> = { n: '\n', r: '\r', t: '\t', b: '\b', f: '\f' }
        if (d in esc) out += esc[d]
        else if (/[0-7]/.test(d)) {
          let o = d
          while (o.length < 3 && /[0-7]/.test(s[i + 1] ?? '')) o += s[++i]
          out += String.fromCharCode(parseInt(o, 8) & 0xff)
        } else if (d === '\r') {
          if (s[i + 1] === '\n') i++
        } else if (d !== '\n') out += d
        continue
      }
      out += c
    }
    this.at = i + 1
    return new Str(out)
  }
}

interface Obj {
  value: Value
  /** A stream's bytes as stored, still encoded. */
  data?: Uint8Array
}

export interface Doc {
  bytes: Uint8Array
  src: string
  objects: Map<number, Obj>
}

const dict = (v: Value | undefined): Dict => (v instanceof Map ? v : new Map())
const name = (v: Value | undefined): string => (v instanceof Name ? v.n : '')
const num = (v: Value | undefined): number => (typeof v === 'number' ? v : NaN)

export function resolve(doc: Doc, v: Value | undefined, depth = 0): Value | undefined {
  return v instanceof Ref && depth < 8 ? resolve(doc, doc.objects.get(v.n)?.value, depth + 1) : v
}

/** Undo a stream's filters: Flate, ASCII85 and hex, in the order named. Null for one it cannot undo. */
export async function decode(doc: Doc, o: Obj): Promise<Uint8Array | null> {
  if (!o.data) return null
  const f = resolve(doc, dict(o.value).get('Filter'))
  const filters = (Array.isArray(f) ? f : f === undefined ? [] : [f]).map((x) => name(resolve(doc, x)))
  let out: Uint8Array | null = o.data
  for (const x of filters) {
    if (!out) return null
    if (x === 'FlateDecode' || x === 'Fl') out = await inflate(out)
    else if (x === 'ASCII85Decode' || x === 'A85') out = a85(latin(out))
    else if (x === 'ASCIIHexDecode' || x === 'AHx') out = bytesOf(unhex(latin(out).split('>')[0]!))
    else return null
  }
  return out
}

/** ASCII85, as PDF writes it: `z` for four zeros, ending at `~>`. */
export function a85(s: string): Uint8Array {
  const body = s.replace(/^\s*<~/, '').split('~>')[0]!.replace(/\s+/g, '')
  const out: number[] = []
  let group: number[] = []
  const flush = (n: number) => {
    while (group.length < 5) group.push(84)
    let v = 0
    for (const d of group) v = v * 85 + d
    for (let k = 3; k >= 4 - n; k--) out.push((v >>> (k * 8)) & 0xff)
    group = []
  }
  for (const c of body) {
    if (c === 'z' && !group.length) {
      out.push(0, 0, 0, 0)
      continue
    }
    const d = c.charCodeAt(0) - 33
    if (d < 0 || d > 84) continue
    group.push(d)
    if (group.length === 5) flush(4)
  }
  if (group.length > 1) flush(group.length - 1)
  return Uint8Array.from(out)
}

export async function inflate(data: Uint8Array): Promise<Uint8Array | null> {
  const ds = new DecompressionStream('deflate')
  const w = ds.writable.getWriter()
  w.write(data as Uint8Array<ArrayBuffer>).catch(() => undefined)
  w.close().catch(() => undefined)
  const r = ds.readable.getReader()
  const parts: Uint8Array[] = []
  let n = 0
  try {
    for (;;) {
      const { done, value } = await r.read()
      if (done) break
      parts.push(value)
      n += value.length
    }
  } catch {
    // Junk after the end of the data, or data cut short: what came out stands.
  }
  if (!n) return null
  const out = new Uint8Array(n)
  let at = 0
  for (const p of parts) {
    out.set(p, at)
    at += p.length
  }
  return out
}

/** Every object in the file, a later definition of a number winning, and those packed in object streams. */
export async function open(bytes: Uint8Array): Promise<Doc> {
  const src = latin(bytes)
  const doc: Doc = { bytes, src, objects: new Map() }
  const head = /(\d+)\s+(\d+)\s+obj\b/g
  for (let m = head.exec(src); m; m = head.exec(src)) {
    const lx = new Lexer(src, m.index + m[0].length)
    const value = lx.next() ?? null
    lx.skip()
    const o: Obj = { value }
    if (src.startsWith('stream', lx.at)) {
      let from = lx.at + 6
      if (src[from] === '\r') from++
      if (src[from] === '\n') from++
      // A direct /Length that lands on endstream is trusted; otherwise endstream is found.
      const len = num(dict(value).get('Length'))
      let to = from + len
      if (!(Number.isInteger(len) && len >= 0 && /^\s*endstream/.test(src.slice(to, to + 32)))) {
        to = src.indexOf('endstream', from)
        if (to < 0) to = src.length
        while (to > from && (src[to - 1] === '\n' || src[to - 1] === '\r')) to--
      }
      o.data = bytes.subarray(from, to)
      head.lastIndex = to
    }
    doc.objects.set(Number(m[1]), o)
  }
  for (const o of [...doc.objects.values()]) {
    const d = dict(o.value)
    if (name(d.get('Type')) !== 'ObjStm') continue
    const data = await decode(doc, o)
    if (!data) continue
    const s = latin(data)
    const first = num(resolve(doc, d.get('First')))
    const count = num(resolve(doc, d.get('N')))
    if (!Number.isInteger(first) || !Number.isInteger(count)) continue
    const lx = new Lexer(s)
    const pairs: number[] = []
    for (let i = 0; i < count * 2; i++) pairs.push(num(lx.next()))
    for (let i = 0; i < count; i++) {
      const id = pairs[i * 2]!
      if (!Number.isInteger(id) || doc.objects.has(id)) continue
      doc.objects.set(id, { value: new Lexer(s, first + pairs[i * 2 + 1]!).next() ?? null })
    }
  }
  return doc
}

/** The last trailer's word for `key`: a classic trailer or a cross-reference stream's, both plain text. */
function trailer(doc: Doc, key: string): Ref | null {
  const all = [...doc.src.matchAll(new RegExp(`/${key}\\s+(\\d+)\\s+\\d+\\s+R`, 'g'))]
  const m = all[all.length - 1]
  return m ? new Ref(Number(m[1])) : null
}

/** The pages in reading order, each with the resources it inherits. */
export function pages(doc: Doc): { page: Dict; resources: Dict }[] {
  const out: { page: Dict; resources: Dict }[] = []
  const seen = new Set<Dict>()
  const walk = (node: Dict, res: Value | undefined) => {
    if (seen.has(node)) return
    seen.add(node)
    const here = node.get('Resources') ?? res
    const kids = resolve(doc, node.get('Kids'))
    if (Array.isArray(kids)) for (const k of kids) walk(dict(resolve(doc, k)), here)
    else if (name(node.get('Type')) === 'Page') out.push({ page: node, resources: dict(resolve(doc, here)) })
  }
  const root = trailer(doc, 'Root')
  if (root) walk(dict(resolve(doc, dict(resolve(doc, root)).get('Pages'))), undefined)
  if (!out.length) {
    for (const [, o] of [...doc.objects].sort((a, b) => a[0] - b[0])) {
      const d = dict(o.value)
      if (name(d.get('Type')) === 'Page') out.push({ page: d, resources: dict(resolve(doc, d.get('Resources'))) })
    }
  }
  return out
}

const be16 = (s: string): string => {
  let out = ''
  for (let i = 0; i + 1 < s.length; i += 2) out += String.fromCharCode((s.charCodeAt(i) << 8) | s.charCodeAt(i + 1))
  return out
}

/** A CMap's hex destination as text: UTF-16BE, or one byte as written by a producer that ignored that. */
function hexText(h: string): string {
  const s = unhex(h)
  return s.length === 1 ? s : be16(s)
}

/** A font as text reads it: how many bytes a code takes, what each code says, and how far it moves the pen. */
export interface Font {
  width: number
  map: Map<number, string> | null
  /** Each code's advance, in thousandths of an em; a code not listed advances `dw`. */
  advance: Map<number, number>
  dw: number
}

/** A ToUnicode CMap: its code width and its map. */
export function cmap(s: string, width: number): Pick<Font, 'width' | 'map'> {
  const map = new Map<number, string>()
  const space = /begincodespacerange([\s\S]*?)endcodespacerange/.exec(s)
  const lo = space ? /<([0-9a-fA-F]+)>/.exec(space[1]!) : null
  if (lo) width = Math.max(1, Math.ceil(lo[1]!.length / 2))
  for (const block of s.matchAll(/beginbfchar([\s\S]*?)endbfchar/g)) {
    for (const m of block[1]!.matchAll(/<([0-9a-fA-F]+)>\s*<([0-9a-fA-F]*)>/g)) map.set(parseInt(m[1]!, 16), hexText(m[2]!))
  }
  for (const block of s.matchAll(/beginbfrange([\s\S]*?)endbfrange/g)) {
    for (const m of block[1]!.matchAll(/<([0-9a-fA-F]+)>\s*<([0-9a-fA-F]+)>\s*(<[0-9a-fA-F]*>|\[[^\]]*\])/g)) {
      const a = parseInt(m[1]!, 16)
      const b = Math.min(parseInt(m[2]!, 16), a + 0xffff)
      if (m[3]!.startsWith('[')) {
        const each = [...m[3]!.matchAll(/<([0-9a-fA-F]*)>/g)]
        for (let c = a; c <= b && c - a < each.length; c++) map.set(c, hexText(each[c - a]![1]!))
      } else {
        const base = hexText(m[3]!.slice(1, -1))
        const last = base.charCodeAt(base.length - 1)
        for (let c = a; c <= b; c++) map.set(c, base.slice(0, -1) + String.fromCharCode(last + (c - a)))
      }
    }
  }
  return { width, map: map.size ? map : null }
}

/** Glyph names a simple font's /Differences may give a code, as the text they stand for. */
const GLYPHS: Record<string, string> = {
  fi: 'fi', fl: 'fl', ff: 'ff', ffi: 'ffi', ffl: 'ffl', space: ' ', hyphen: '-', minus: '−', period: '.', comma: ',', colon: ':',
  semicolon: ';', exclam: '!', question: '?', quoteright: '’', quoteleft: '‘', quotedblleft: '“', quotedblright: '”',
  quotesingle: "'", quotedbl: '"', endash: '–', emdash: '—', bullet: '•', ellipsis: '…', parenleft: '(', parenright: ')',
  bracketleft: '[', bracketright: ']', braceleft: '{', braceright: '}', slash: '/', backslash: '\\', bar: '|', at: '@',
  numbersign: '#', dollar: '$', percent: '%', ampersand: '&', asterisk: '*', plus: '+', equal: '=', less: '<', greater: '>',
  underscore: '_', asciitilde: '~', asciicircum: '^', grave: '`', zero: '0', one: '1', two: '2', three: '3', four: '4',
  five: '5', six: '6', seven: '7', eight: '8', nine: '9', section: '§', dagger: '†', degree: '°', copyright: '©',
  registered: '®', trademark: '™',
}

/** A simple font's /Differences as text, where each glyph name says what it is. */
export function differences(v: Value | undefined): Map<number, string> | null {
  if (!Array.isArray(v)) return null
  const map = new Map<number, string>()
  let code = 0
  for (const x of v) {
    if (typeof x === 'number') code = x
    else if (x instanceof Name) {
      const n = x.n
      const said = /^[A-Za-z]$/.test(n) ? n : /^uni[0-9A-Fa-f]{4}$/.test(n) ? String.fromCharCode(parseInt(n.slice(3), 16)) : GLYPHS[n]
      if (said !== undefined) map.set(code, said)
      code++
    }
  }
  return map.size ? map : null
}

/** The advances a font lists: a simple font's /Widths from /FirstChar, a composite font's /W. */
function advances(doc: Doc, f: Dict): Pick<Font, 'advance' | 'dw'> {
  const advance = new Map<number, number>()
  const kid = dict(resolve(doc, (resolve(doc, f.get('DescendantFonts')) as Value[] | undefined)?.[0]))
  if (kid.size) {
    const w = resolve(doc, kid.get('W'))
    const list = Array.isArray(w) ? w.map((x) => resolve(doc, x)) : []
    for (let i = 0; i < list.length; ) {
      const c = num(list[i])
      const next = list[i + 1]
      if (Array.isArray(next)) {
        next.forEach((x, k) => advance.set(c + k, num(resolve(doc, x))))
        i += 2
      } else {
        const to = num(next)
        const v = num(list[i + 2])
        for (let k = c; k <= to && k - c < 0xffff; k++) advance.set(k, v)
        i += 3
      }
    }
    const dw = num(resolve(doc, kid.get('DW')))
    return { advance, dw: Number.isFinite(dw) ? dw : 1000 }
  }
  const first = num(resolve(doc, f.get('FirstChar')))
  const w = resolve(doc, f.get('Widths'))
  if (Array.isArray(w) && Number.isInteger(first)) w.forEach((x, k) => advance.set(first + k, num(resolve(doc, x))))
  const missing = num(resolve(doc, dict(resolve(doc, f.get('FontDescriptor'))).get('MissingWidth')))
  return { advance, dw: missing > 0 ? missing : 500 }
}

async function font(doc: Doc, v: Value | undefined): Promise<Font> {
  const f = dict(resolve(doc, v))
  const width = name(f.get('Subtype')) === 'Type0' ? 2 : 1
  const to = f.get('ToUnicode')
  const o = to instanceof Ref ? doc.objects.get(to.n) : undefined
  const data = o ? await decode(doc, o) : null
  const read = data ? cmap(latin(data), width) : { width, map: width === 1 ? differences(resolve(doc, dict(resolve(doc, f.get('Encoding'))).get('Differences'))) : null }
  return { ...read, ...advances(doc, f) }
}

/** WinAnsi's 0x80–0x9F, where it parts from Latin-1: how a simple font with no map is read. */
const WIN = '€\u0081‚ƒ„…†‡ˆ‰Š‹Œ\u008dŽ\u008f\u0090‘’“”•–—˜™š›œ\u009džŸ'

/** The codes a string holds, `width` bytes each. */
export function codes(s: string, width: number): number[] {
  const out: number[] = []
  for (let i = 0; i + width <= s.length; i += width) {
    let code = 0
    for (let j = 0; j < width; j++) code = (code << 8) | s.charCodeAt(i + j)
    out.push(code)
  }
  return out
}

/** What one code says in a font. A two-byte font with no map says nothing: its codes are glyph numbers. */
export function say(code: number, f: Pick<Font, 'width' | 'map'> | undefined): string {
  const said = f?.map?.get(code)
  if (said !== undefined) return said
  if ((f?.width ?? 1) > 1) return ''
  return code >= 0x80 && code < 0xa0 ? WIN[code - 0x80]! : String.fromCharCode(code)
}

/**
 * The text a content stream draws, in the fonts its resources name, and in the
 * forms it draws. The pen is followed through the text operators with each
 * font's advances, so a move down a line breaks the line and a move right past
 * where the last word ended is a space, whichever way the producer placed it.
 */
async function draw(doc: Doc, content: string, resources: Dict, depth = 0): Promise<string> {
  const fonts = dict(resolve(doc, resources.get('Font')))
  const forms = dict(resolve(doc, resources.get('XObject')))
  const known = new Map<string, Font>()
  let f: Font | undefined
  let size = 0
  let m = [1, 0, 0, 1]
  let lx = 0
  let ly = 0
  let x = 0
  let y = 0
  let leading = 0
  let out = ''
  const across = () => Math.abs(size * Math.hypot(m[0]!, m[1]!)) || 10
  const up = () => Math.abs(size * Math.hypot(m[2]!, m[3]!)) || 10
  const line = () => {
    if (out && !out.endsWith('\n')) out += '\n'
  }
  const gap = () => {
    if (out && !/\s$/.test(out)) out += ' '
  }
  const move = (nx: number, ny: number) => {
    if (out && !out.endsWith('\n')) {
      if (Math.abs(ny - y) > 0.5 * up()) line()
      else if (nx > x + 0.15 * across()) gap()
    }
    x = nx
    y = ny
  }
  const td = (tx: number, ty: number) => {
    lx += tx * m[0]! + ty * m[2]!
    ly += tx * m[1]! + ty * m[3]!
    move(lx, ly)
  }
  const put = (s: string) => {
    for (const c of codes(s, f?.width ?? 1)) {
      out += say(c, f)
      x += (((f?.advance.get(c) ?? f?.dw ?? 500) / 1000) * across())
    }
  }
  const lex = new Lexer(content)
  const args: Value[] = []
  for (let v = lex.next(); v !== undefined; v = lex.next()) {
    if (!(v instanceof Op)) {
      args.push(v)
      continue
    }
    const op = v.o
    const n = (i: number) => (typeof args[i] === 'number' ? (args[i] as number) : 0)
    if (op === 'BT') {
      m = [1, 0, 0, 1]
      lx = ly = 0
    } else if (op === 'Tf' && args[0] instanceof Name) {
      const id = args[0].n
      if (!known.has(id)) known.set(id, await font(doc, fonts.get(id)))
      f = known.get(id)
      size = n(1)
    } else if (op === 'Tm') {
      m = [n(0), n(1), n(2), n(3)]
      lx = n(4)
      ly = n(5)
      move(lx, ly)
    } else if (op === 'Td') td(n(0), n(1))
    else if (op === 'TD') {
      leading = -n(1)
      td(n(0), n(1))
    } else if (op === 'TL') leading = n(0)
    else if (op === 'T*') td(0, -leading)
    else if (op === 'Tj' && args[0] instanceof Str) put(args[0].s)
    else if ((op === "'" || op === '"') && args[args.length - 1] instanceof Str) {
      td(0, -leading)
      line()
      put((args[args.length - 1] as Str).s)
    } else if (op === 'TJ' && Array.isArray(args[0])) {
      for (const t of args[0]) {
        if (t instanceof Str) put(t.s)
        else if (typeof t === 'number') {
          const shift = (-t / 1000) * across()
          x += shift
          if (shift > 0.15 * across()) gap()
        }
      }
    } else if (op === 'BI') {
      // An inline picture: its bytes run from ID to EI and are not syntax.
      const id = content.indexOf('ID', lex.at)
      const ei = id < 0 ? -1 : content.slice(id).search(/\sEI(?=\s|$)/)
      lex.at = ei < 0 ? content.length : id + ei + 3
    } else if (op === 'Do' && args[0] instanceof Name && depth < 3) {
      const ref = forms.get(args[0].n)
      const o = ref instanceof Ref ? doc.objects.get(ref.n) : undefined
      const d = dict(o?.value)
      if (o && name(d.get('Subtype')) === 'Form') {
        const data = await decode(doc, o)
        if (data) {
          const inner = await draw(doc, latin(data), d.has('Resources') ? dict(resolve(doc, d.get('Resources'))) : resources, depth + 1)
          if (inner) {
            line()
            out += inner
            line()
          }
        }
      }
    }
    args.length = 0
  }
  return out
}

/** Each page's text, in reading order. */
export async function text(doc: Doc): Promise<string[]> {
  const out: string[] = []
  for (const { page, resources } of pages(doc)) {
    // Contents is one stream or an array of them, either one possibly behind a reference.
    const c = page.get('Contents')
    const all = resolve(doc, c)
    const parts: string[] = []
    for (const r of Array.isArray(all) ? all : [c]) {
      const o = r instanceof Ref ? doc.objects.get(r.n) : undefined
      const data = o ? await decode(doc, o) : null
      if (data) parts.push(latin(data))
    }
    const said = await draw(doc, parts.join('\n'), resources)
    out.push(
      said
        .split('\n')
        .map((l) => l.replace(/[\x00-\x08\x0b-\x1f]/g, '').replace(/\s+$/, ''))
        .join('\n')
        .replace(/\n{3,}/g, '\n\n')
        .trim(),
    )
  }
  return out
}

/** A PDF text string: UTF-16 or UTF-8 with its mark, else one byte a character. */
export function words(s: Str | Value | undefined): string {
  if (!(s instanceof Str)) return ''
  const t = s.s
  if (t.startsWith('\xfe\xff')) return be16(t.slice(2))
  if (t.startsWith('\xef\xbb\xbf')) return new TextDecoder().decode(bytesOf(t.slice(3)))
  return t
}

/** A PDF date (`D:20261020113000-07'00'`) as a person reads it, or the string as it was. */
export function date(s: string): string {
  const m = /^D?:?(\d{4})(\d{2})?(\d{2})?(\d{2})?(\d{2})?(\d{2})?([Z+-])?(\d{2})?'?(\d{2})?/.exec(s)
  if (!m) return s
  const [, y, mo = '01', d = '01', h = '00', mi = '00', se = '00', z, zh, zm = '00'] = m
  const zone = !z ? '' : z === 'Z' ? ' UTC' : ` ${z}${zh ?? '00'}:${zm}`
  return `${y}-${mo}-${d} ${h}:${mi}:${se}${zone}`
}

export interface Signature {
  /** Who the signature dictionary names, or ''. */
  name: string
  reason: string
  location: string
  time: string
  /** Its SubFilter: adbe.pkcs7.detached, ETSI.CAdES.detached, … */
  filter: string
  /** The byte range starts at the first byte and frames exactly the signature's own bytes. */
  framed: boolean
  /** Bytes in the file after the end of what was signed; 0 when the signature covers the whole file. */
  after: number
  /** The digest the signature carries matches the signed bytes; null when it was not checked. */
  intact: boolean | null
  /** The digest that was checked, `SHA-256`, or why none was. */
  digest: string
}

export interface Check {
  version: string
  pages: number
  encrypted: boolean
  /** How many times it was saved: 1 is as first written, each incremental update adds one. */
  saves: number
  info: Record<string, string>
  /** What is wrong with its structure, in words; empty when nothing is. */
  problems: string[]
  signatures: Signature[]
}

const INFO = ['Title', 'Author', 'Subject', 'Keywords', 'Creator', 'Producer', 'CreationDate', 'ModDate'] as const

/** The OID of the CMS messageDigest attribute, 1.2.840.113549.1.9.4, as DER. */
const MESSAGE_DIGEST = '\x06\x09\x2a\x86\x48\x86\xf7\x0d\x01\x09\x04'
const DIGESTS: Record<number, string> = { 20: 'SHA-1', 32: 'SHA-256', 48: 'SHA-384', 64: 'SHA-512' }

/** A DER length at `i`: its value and where the content starts. */
function length(s: string, i: number): [number, number] {
  const b = s.charCodeAt(i)
  if (b < 0x80) return [b, i + 1]
  let n = 0
  for (let k = 1; k <= (b & 0x7f); k++) n = n * 256 + s.charCodeAt(i + k)
  return [n, i + 1 + (b & 0x7f)]
}

/** The digest a CMS signature says it signed: the value of its messageDigest attribute, or ''. */
export function claimed(der: string): string {
  const at = der.indexOf(MESSAGE_DIGEST)
  if (at < 0) return ''
  let i = at + MESSAGE_DIGEST.length
  if (der.charCodeAt(i) !== 0x31) return ''
  ;[, i] = length(der, i + 1)
  if (der.charCodeAt(i) !== 0x04) return ''
  const [n, from] = length(der, i + 1)
  return der.slice(from, from + n)
}

async function signature(doc: Doc, d: Dict): Promise<Signature> {
  const range = resolve(doc, d.get('ByteRange'))
  const [a, b, c, e] = (Array.isArray(range) ? range : []).map((x) => num(resolve(doc, x)))
  const filter = name(resolve(doc, d.get('SubFilter')))
  const n = doc.bytes.length
  const ok = [a, b, c, e].every((x) => Number.isInteger(x) && x! >= 0) && a! + b! <= c! && c! + e! <= n
  const framed = ok && a === 0 && doc.src[b!] === '<' && doc.src[c! - 1] === '>'
  const out: Signature = {
    name: words(resolve(doc, d.get('Name'))),
    reason: words(resolve(doc, d.get('Reason'))),
    location: words(resolve(doc, d.get('Location'))),
    time: date(words(resolve(doc, d.get('M')))),
    filter,
    framed,
    after: ok ? n - (c! + e!) : n,
    intact: null,
    digest: '',
  }
  if (!ok) {
    out.digest = 'its byte range does not fit the file'
    return out
  }
  if (filter && !/detached$/i.test(filter)) {
    out.digest = `${filter} is not checked here`
    return out
  }
  const der = resolve(doc, d.get('Contents'))
  const want = claimed(der instanceof Str ? der.s : '')
  const algo = DIGESTS[want.length]
  if (!algo) {
    out.digest = 'the signature names no digest that can be checked here'
    return out
  }
  const signed = new Uint8Array(b! + e!)
  signed.set(doc.bytes.subarray(a!, a! + b!), 0)
  signed.set(doc.bytes.subarray(c!, c! + e!), b!)
  const got = latin(new Uint8Array(await crypto.subtle.digest(algo, signed)))
  out.intact = got === want
  out.digest = algo
  return out
}

/** What the file's structure says about it. */
export async function check(doc: Doc): Promise<Check> {
  const { src } = doc
  const problems: string[] = []
  const version = /%PDF-(\d\.\d)/.exec(src.slice(0, 1024))?.[1] ?? ''
  if (!version) problems.push('It does not start with a PDF header.')
  if (!src.slice(-1024).includes('%%EOF')) problems.push('It does not end with %%EOF, so it may be cut short.')
  const starts = [...src.matchAll(/startxref\s+(\d+)/g)]
  const last = starts[starts.length - 1]
  if (!last) problems.push('It has no startxref, so a reader cannot find its objects without rebuilding the index.')
  else {
    const at = Number(last[1])
    if (!src.startsWith('xref', at) && !/^\d+\s+\d+\s+obj/.test(src.slice(at, at + 32)))
      problems.push(`Its index is not where the file says (byte ${at}): it was edited by something that did not rewrite it, or it is damaged.`)
  }
  const list = pages(doc)
  const root = trailer(doc, 'Root')
  const count = root ? num(resolve(doc, dict(resolve(doc, dict(resolve(doc, root)).get('Pages'))).get('Count'))) : NaN
  if (!root) problems.push('It names no document catalog (/Root).')
  else if (Number.isInteger(count) && count !== list.length) problems.push(`Its page tree says ${count} pages and holds ${list.length}.`)
  const info: Record<string, string> = {}
  const where = trailer(doc, 'Info')
  const raw = where ? dict(resolve(doc, where)) : new Map<string, Value>()
  for (const k of INFO) {
    const v = words(resolve(doc, raw.get(k)))
    if (v) info[k] = k.endsWith('Date') ? date(v) : v
  }
  const signatures: Signature[] = []
  for (const o of doc.objects.values()) {
    const d = dict(o.value)
    if (d.has('ByteRange') && d.has('Contents')) signatures.push(await signature(doc, d))
  }
  return {
    version,
    pages: list.length,
    encrypted: /\/Encrypt(?![A-Za-z])/.test(src),
    saves: Math.max(1, (src.match(/%%EOF/g) ?? []).length),
    info,
    problems,
    signatures,
  }
}

export interface Read {
  check: Check
  /** Each page's text; empty when the file is encrypted. */
  text: string[]
}

/** Read a PDF: its structure, and its text when it is not encrypted. */
export async function read(bytes: Uint8Array): Promise<Read> {
  if (bytes.length > LARGEST) throw new Error(`It is larger than ${LARGEST >> 20} MB`)
  const doc = await open(bytes)
  const c = await check(doc)
  return { check: c, text: c.encrypted ? [] : await text(doc) }
}

/** What a read found, in words a model is given. */
export function report(file: string, r: Read): string {
  const c = r.check
  const lines = [`${file}: PDF ${c.version || '(no version)'}, ${c.pages} page${c.pages === 1 ? '' : 's'}.`]
  const meta = Object.entries(c.info).map(([k, v]) => `${k}: ${v}`)
  if (meta.length) lines.push(`Metadata — ${meta.join('; ')}.`)
  if (c.encrypted) lines.push('It is encrypted, so its text could not be read here.')
  lines.push(c.saves > 1 ? `It was changed after it was first written: saved ${c.saves} times.` : 'It is as first written: saved once.')
  lines.push(c.problems.length ? `Structure problems: ${c.problems.join(' ')}` : 'Its structure checks out: header, index and page tree agree.')
  if (!c.signatures.length) lines.push('It carries no digital signature.')
  c.signatures.forEach((s, i) => {
    const who = [s.name && `by ${s.name}`, s.time && `at ${s.time}`, s.reason && `for "${s.reason}"`].filter(Boolean).join(' ')
    const covers = !s.framed ? 'its byte range does not frame its own signature' : s.after ? `${s.after} bytes were added after it was signed` : 'it covers the whole file'
    const digest = s.intact === null ? `the digest was not checked (${s.digest})` : s.intact ? `the signed bytes are intact (${s.digest} matches)` : `the signed bytes were changed (${s.digest} does not match)`
    lines.push(`Signature ${i + 1}${who ? ` ${who}` : ''}: ${covers}; ${digest}. Whether the signer's certificate is trusted was not checked.`)
  })
  return lines.join('\n')
}
