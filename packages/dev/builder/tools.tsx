/**
 * Files with an ask, for every composer the builder draws: they ride the
 * prompt as text — nothing is uploaded anywhere else. The paperclip is the
 * composer's (@hanzo/ui/chat); this reads what it picks and says what is held.
 */
import type { Say } from '@hanzo/i18n/react'
import { Paperclip } from '@hanzogui/lucide-icons-2'
import type { Carried } from '@hanzo/ui/chat'

import { english } from './say.ts'

export interface Attached {
  name: string
  text: string
}

const EACH = 100_000
const ALL = 200_000

/** The ask with its attachments appended as fenced text. */
export function compose(prompt: string, files: Attached[]): string {
  if (!files.length) return prompt
  return prompt + files.map((f) => `\n\n\`${f.name}\`:\n\`\`\`\n${f.text}\n\`\`\``).join('')
}

/** Read `list` into `have`, within the caps; answers the new list and what was refused, in the language of `t`. */
export async function read(have: Attached[], list: ArrayLike<File>, t: Say = english): Promise<{ files: Attached[]; note: string }> {
  const files = [...have]
  let total = files.reduce((n, f) => n + f.text.length, 0)
  let note = ''
  for (const file of Array.from(list)) {
    if (file.size > EACH) {
      note = t('{file} is over 100 KB; attach a smaller file or point the run at the repository.', { file: file.name })
      continue
    }
    const text = await file.text()
    if (total + text.length > ALL) {
      note = t('Attachments are capped at 200 KB in all.')
      break
    }
    total += text.length
    files.push({ name: file.name, text })
  }
  return { files, note }
}

/** Ask the browser for files and read them in, within the caps; the picker is minted per press. */
export function pick(have: Attached[], onFiles: (f: Attached[]) => void, onNote: (n: string) => void, t: Say = english): void {
  if (typeof document === 'undefined') return
  const ask = document.createElement('input')
  ask.type = 'file'
  ask.multiple = true
  ask.onchange = () => {
    if (!ask.files?.length) return
    void read(have, ask.files, t).then(({ files, note }) => {
      onFiles(files)
      onNote(note)
    })
  }
  ask.click()
}

/** The attached files as what the composer carries: a removable chip each. */
export function carrying(files: Attached[], onFiles: (f: Attached[]) => void): Carried[] {
  return files.map((f) => ({ id: `file:${f.name}`, label: f.name, icon: <Paperclip size={14} />, onRemove: () => onFiles(files.filter((x) => x !== f)) }))
}
