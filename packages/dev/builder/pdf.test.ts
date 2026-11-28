import { describe, expect, it } from 'vitest'

import { a85, claimed, cmap, date, differences, latin, Name, read, report, Str, words } from './pdf.ts'

const bytes = (s: string): Uint8Array => Uint8Array.from(s, (c) => c.charCodeAt(0))

async function deflate(s: string): Promise<string> {
  const out = await new Response(new Blob([bytes(s) as Uint8Array<ArrayBuffer>]).stream().pipeThrough(new CompressionStream('deflate'))).arrayBuffer()
  return latin(new Uint8Array(out))
}

/** A whole file from its numbered objects (1-based), with a true index, trailer and startxref. */
function file(objects: string[], trailer: string, head = '%PDF-1.7\n%\xe2\xe3\xcf\xd3\n'): string {
  let out = head
  const at: number[] = []
  objects.forEach((o, i) => {
    at.push(out.length)
    out += `${i + 1} 0 obj\n${o}\nendobj\n`
  })
  const xref = out.length
  out += `xref\n0 ${objects.length + 1}\n0000000000 65535 f \n${at.map((n) => `${String(n).padStart(10, '0')} 00000 n \n`).join('')}`
  return `${out}trailer\n<< /Size ${objects.length + 1} ${trailer} >>\nstartxref\n${xref}\n%%EOF\n`
}

const stream = (dict: string, data: string) => `<< ${dict} /Length ${data.length} >>\nstream\n${data}\nendstream`

/** A two-page memo in Helvetica, uncompressed, with a UTF-16 title. */
function memo(): string {
  const p1 = 'BT /F1 12 Tf 72 720 Td (Quarterly memo) Tj 0 -14 Td [(Revenue)-300(grew)-300(14%)] TJ ET'
  const p2 = 'BT /F1 12 Tf 72 720 Td (Second page) Tj T* (\\(kept\\) 30 days) Tj ET'
  return file(
    [
      '<< /Type /Catalog /Pages 2 0 R >>',
      '<< /Type /Pages /Kids [3 0 R 4 0 R] /Count 2 /Resources << /Font << /F1 5 0 R >> >> >>',
      '<< /Type /Page /Parent 2 0 R /Contents 6 0 R >>',
      '<< /Type /Page /Parent 2 0 R /Contents 7 0 R >>',
      '<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>',
      stream('', p1),
      stream('', p2.replace('T*', '0 -14 TD')),
      '<< /Title <FEFF004D0065006D006F> /Author (Z) /CreationDate (D:20261020113000-07\'00\') >>',
    ],
    '/Root 1 0 R /Info 8 0 R',
  )
}

describe('pdf text', () => {
  it('reads a simple font page by page, a line per move down, a space per gap', async () => {
    const r = await read(bytes(memo()))
    expect(r.text).toEqual(['Quarterly memo\nRevenue grew 14%', 'Second page\n(kept) 30 days'])
    expect(r.check).toMatchObject({ version: '1.7', pages: 2, encrypted: false, saves: 1, problems: [], signatures: [] })
    expect(r.check.info).toEqual({ Title: 'Memo', Author: 'Z', CreationDate: '2026-10-20 11:30:00 -07:00' })
  })

  it('reads a compressed two-byte font through its ToUnicode map, kept in an object stream', async () => {
    // Codes 0001–0003 say H, i, ! by a range and a char; the font's dict lives packed in object 9.
    const map = '/CIDInit /ProcSet findresource begin 1 begincodespacerange <0000> <FFFF> endcodespacerange 1 beginbfrange <0001> <0002> [<0048> <0069>] endbfrange 1 beginbfchar <0003> <0021> endbfchar end'
    const content = 'BT /F1 10 Tf 1 0 0 1 72 700 Tm <000100020003> Tj ET'
    const packed = '5 0 << /Type /Font /Subtype /Type0 /BaseFont /X /Encoding /Identity-H /ToUnicode 6 0 R /DescendantFonts [<< /W [1 [600 300 300]] >>] >>'
    const pdf = file(
      [
        '<< /Type /Catalog /Pages 2 0 R >>',
        '<< /Type /Pages /Kids [3 0 R] /Count 1 >>',
        '<< /Type /Page /Parent 2 0 R /Contents 4 0 R /Resources << /Font << /F1 5 0 R >> >> >>',
        stream('/Filter /FlateDecode', await deflate(content)),
        'null',
        stream('/Filter [/FlateDecode]', await deflate(map)),
        'null',
        'null',
        stream(`/Type /ObjStm /N 1 /First ${'5 0 '.length} /Filter /FlateDecode`, await deflate(packed)),
      ],
      '/Root 1 0 R',
    )
    // Object 5 was written as `null` directly; the packed one only fills a number nothing else defines.
    const r = await read(bytes(pdf.replace('5 0 obj\nnull', '10 0 obj\nnull')))
    expect(r.text).toEqual(['Hi!'])
  })

  it('reads ASCII85, and names from a simple font’s /Differences', async () => {
    expect(latin(a85('<~87cURD_*#4DfTZ)+T~>'))).toBe('Hello, World!')
    expect(latin(a85('z~>'))).toBe('\0\0\0\0')
    const d = differences([2, new Name('fi'), new Name('uni00E9'), 65, new Name('A'), new Name('nosuch'), new Name('B')])
    expect([...(d ?? [])]).toEqual([
      [2, 'fi'],
      [3, 'é'],
      [65, 'A'],
      [67, 'B'],
    ])
    const m = cmap('1 begincodespacerange <00> <FF> endcodespacerange 1 beginbfrange <41> <43> <0061> endbfrange', 2)
    expect(m.width).toBe(1)
    expect([...(m.map ?? [])]).toEqual([
      [0x41, 'a'],
      [0x42, 'b'],
      [0x43, 'c'],
    ])
  })
})

describe('pdf check', () => {
  it('says what is wrong with a damaged file', async () => {
    const good = memo()
    const cut = good.slice(0, good.lastIndexOf('%%EOF'))
    expect((await read(bytes(cut))).check.problems).toEqual(['It does not end with %%EOF, so it may be cut short.'])
    const moved = good.replace(/startxref\n\d+/, 'startxref\n12')
    expect((await read(bytes(moved))).check.problems[0]).toMatch(/^Its index is not where the file says \(byte 12\)/)
    const counted = good.replace('/Count 2', '/Count 3')
    expect((await read(bytes(counted))).check.problems).toEqual(['Its page tree says 3 pages and holds 2.'])
    expect((await read(bytes(good.replace('%PDF-1.7', '%XYZ')))).check.problems).toContain('It does not start with a PDF header.')
  })

  it('checks that a signature frames its bytes and that they are the ones it signed', async () => {
    // A CMS fragment carrying messageDigest, the way a detached signature does: OID, SET, OCTET STRING.
    const der = (digest: string) => `\x30\x80\x06\x09\x2a\x86\x48\x86\xf7\x0d\x01\x09\x04\x31${String.fromCharCode(digest.length + 2)}\x04${String.fromCharCode(digest.length)}${digest}`
    const slot = 2 * 80
    const base = memo().replace('%%EOF\n', '')
    const sig = `9 0 obj\n<< /Type /Sig /Filter /Adobe.PPKLite /SubFilter /adbe.pkcs7.detached /Name (Ada) /M (D:20261020) /ByteRange [0 AAAAAAAAAA BBBBBBBBBB CCCCCCCCCC] /Contents <${'0'.repeat(slot)}> >>\nendobj\n%%EOF\n`
    let pdf = base + sig
    const open = pdf.indexOf('/Contents <') + '/Contents '.length
    const close = open + slot + 2
    const range = (n: number) => String(n).padStart(10, '0')
    pdf = pdf.replace('AAAAAAAAAA', range(open)).replace('BBBBBBBBBB', range(close)).replace('CCCCCCCCCC', range(pdf.length - close))
    const signed = bytes(pdf.slice(0, open) + pdf.slice(close))
    const digest = latin(new Uint8Array(await crypto.subtle.digest('SHA-256', signed as Uint8Array<ArrayBuffer>)))
    const hex = [...der(digest)].map((c) => c.charCodeAt(0).toString(16).padStart(2, '0')).join('').padEnd(slot, '0')
    pdf = pdf.slice(0, open + 1) + hex + pdf.slice(open + 1 + slot)

    const s = (await read(bytes(pdf))).check.signatures
    expect(s).toEqual([{ name: 'Ada', reason: '', location: '', time: '2026-10-20 00:00:00', filter: 'adbe.pkcs7.detached', framed: true, after: 0, intact: true, digest: 'SHA-256' }])

    const added = (await read(bytes(`${pdf}% more\n`))).check.signatures[0]!
    expect([added.after, added.intact]).toEqual([7, true])

    const changed = (await read(bytes(pdf.replace('Quarterly', 'Quarterlx')))).check.signatures[0]!
    expect(changed.intact).toBe(false)
    expect(report('memo.pdf', await read(bytes(pdf.replace('Quarterly', 'Quarterlx'))))).toContain('the signed bytes were changed (SHA-256 does not match)')
  })

  it('reads a PDF string and date as a person would', () => {
    expect(words(new Str('\xfe\xff\x00H\x00i'))).toBe('Hi')
    expect(words(new Str('\xef\xbb\xbfcaf\xc3\xa9'))).toBe('café')
    expect(words(new Str('plain'))).toBe('plain')
    expect(date("D:20261020113000Z")).toBe('2026-10-20 11:30:00 UTC')
    expect(date('not a date')).toBe('not a date')
    expect(claimed('no digest here')).toBe('')
  })
})
