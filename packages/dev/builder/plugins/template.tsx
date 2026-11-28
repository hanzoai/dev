/**
 * Template Creator: the ask in the composer, saved as a reusable template — one
 * of the org's own skills (POST /v1/tool/skills), switched on as it is saved so
 * it is under `+` › Skills from then on. A Hanzo model drafts the template from
 * the ask (its inputs as {{placeholders}}); the person reads and edits it before
 * it is saved. Writing a skill takes an org admin, so only an admin is offered it.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { LayoutTemplate } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogDescription, DialogTitle, Input, Textarea } from '@hanzo/ui'
import { useEffect, useState } from 'react'

import { ask, json } from '../api/chat.ts'
import { NAME, refuse, tool, write } from '../api/skills.ts'
import { toggle } from '../api/tools.ts'
import { say } from '../customize/say.ts'
import type { Composer, Plugin } from '../plugin.ts'

export interface Draft {
  name: string
  description: string
  body: string
}

const DRAFT = `Turn the person's request into a reusable template, saved as a skill an agent follows. Answer only JSON: {"name": "a short lowercase-hyphenated name", "description": "one line: when to use it", "body": "markdown, under 250 words: what it is for, the inputs it needs written as {{placeholders}}, then the steps"}.`

/** A skill's name from any words: one lowercase segment of letters, digits and hyphens. */
export function slug(text: string): string {
  const s = text
    .toLowerCase()
    .normalize('NFKD')
    .replace(/[̀-ͯ]/g, '')
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '')
    .slice(0, 48)
    .replace(/-+$/, '')
  return NAME.test(s) ? s : 'template'
}

/** The first line of a text, cut to a line's length. */
const first = (text: string): string => (text.trim().split('\n')[0] ?? '').slice(0, 160).trim()

/** What a model's draft says, each part the ask's own where the draft has none. */
export function drafted(said: string, draft: string): Draft {
  const o = json(said) ?? {}
  const str = (v: unknown) => (typeof v === 'string' ? v.trim() : '')
  return {
    name: slug(str(o.name) || draft.split(/\s+/).slice(0, 6).join(' ')),
    description: first(str(o.description) || draft),
    body: str(o.body) || draft.trim(),
  }
}

/** The SKILL.md a template is saved as: its name and description up front, then the template. */
export function document(d: Draft): string {
  return `---\nname: ${d.name}\ndescription: ${JSON.stringify(d.description.replace(/\s+/g, ' ').trim())}\n---\n\n${d.body.trim()}\n`
}

function Creator({ c }: { c: Composer }) {
  const { t } = useI18n()
  const from = c.draft.trim()
  const [d, setD] = useState<Draft>({ name: from ? slug(from.split(/\s+/).slice(0, 6).join(' ')) : '', description: first(from), body: from })
  const [drafting, setDrafting] = useState(Boolean(from))
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState('')
  const [saved, setSaved] = useState('')

  useEffect(() => {
    if (!from) return
    const stop = new AbortController()
    ask(c.target, DRAFT, from, stop.signal, 2000)
      .then((said) => setD(drafted(said, from)))
      .catch(() => undefined)
      .finally(() => setDrafting(false))
    return () => stop.abort()
  }, [c.target, from])

  const save = async () => {
    const content = document(d)
    const why = refuse({ name: d.name, content })
    if (why) {
      setNote(why)
      return
    }
    setBusy(true)
    setNote('')
    try {
      const s = await write(c.target, { name: d.name, description: d.description, content })
      await toggle(c.target, [tool(s.name)])
      setSaved(s.name)
    } catch (e) {
      setNote(say(e))
    } finally {
      setBusy(false)
    }
  }

  return (
    <Dialog open onOpenChange={(o) => !o && c.show(null)}>
      <DialogContent maxW={640} width="100%">
        <DialogTitle>{t('Template Creator')}</DialogTitle>
        <DialogDescription>
          {saved ? t('Saved as {name}. It is under + › Skills.', { name: saved }) : drafting ? t('Hanzo is drafting the template from your message…') : t('A reusable template, saved as one of your skills.')}
        </DialogDescription>
        {saved ? null : (
          <YStack gap="$3">
            <Input value={d.name} onChangeText={(name: string) => setD({ ...d, name })} placeholder="weekly-update" aria-label={t('Name')} autoCapitalize="none" />
            <Input value={d.description} onChangeText={(description: string) => setD({ ...d, description })} placeholder={t('When to use it')} aria-label={t('Description')} />
            <Textarea value={d.body} onChangeText={(body: string) => setD({ ...d, body })} placeholder={'# Weekly update\n\nInputs: {{team}}, {{week}}\n\n1. …'} aria-label={t('Template')} rows={12} maxH="40vh" overflowY="auto" />
          </YStack>
        )}
        {note ? (
          <SizableText size="$2" color="$soft" role="status">
            {note}
          </SizableText>
        ) : null}
        <XStack gap="$2" justify="flex-end">
          {saved ? (
            <Button size="sm" onPress={() => c.show(null)}>
              {t('Done')}
            </Button>
          ) : (
            <>
              <Button size="sm" variant="ghost" onPress={() => c.show(null)}>
                {t('Cancel')}
              </Button>
              <Button size="sm" disabled={busy || drafting || !d.name || !d.body.trim()} onPress={() => void save()}>
                {t('Save')}
              </Button>
            </>
          )}
        </XStack>
      </DialogContent>
    </Dialog>
  )
}

export const template: Plugin = {
  id: 'template',
  name: phrase('Template Creator'),
  note: phrase('Templates saved as skills'),
  icon: <LayoutTemplate size={16} />,
  open: (c) => c.show(<Creator c={c} />),
}
