/**
 * Work in a project: pick one of the org's projects (GET /v1/project). A
 * conversation does not belong to a project — work in one is a Dev run tagged
 * with it — so a message in the composer starts a Dev run on the picked project
 * (POST /v1/agent/coding, the person's saved model) and opens it; with nothing
 * written, the project's workspace opens to start one there.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { Folder } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogDescription, DialogTitle, Input } from '@hanzo/ui'
import { useEffect, useState } from 'react'

import { start } from '../api/coding.ts'
import { address, name, projects, type Project } from '../api/projects.ts'
import { say } from '../customize/say.ts'
import type { Composer, Plugin } from '../plugin.ts'

/** The projects a search keeps: those whose name, address or repository holds every word. */
export function find(list: readonly Project[], q: string): Project[] {
  const words = q.toLowerCase().split(/\s+/).filter(Boolean)
  return list.filter((p) => {
    const hay = `${p.name} ${p.slug} ${address(p.repo)}`.toLowerCase()
    return words.every((w) => hay.includes(w))
  })
}

function Picker({ c }: { c: Composer }) {
  const { t } = useI18n()
  const [list, setList] = useState<Project[] | null>(null)
  const [q, setQ] = useState('')
  const [note, setNote] = useState('')
  const [busy, setBusy] = useState('')
  const ask = c.draft.trim()

  useEffect(() => {
    let live = true
    projects(c.target).then(
      (v) => live && setList(v),
      (e: unknown) => live && setNote(say(e)),
    )
    return () => {
      live = false
    }
  }, [c.target])

  const pick = async (p: Project) => {
    if (!ask) {
      c.show(null)
      c.go(p.slug)
      return
    }
    setBusy(p.slug)
    setNote('')
    try {
      const run = await start(c.target, { prompt: ask, project: p.slug, repo: p.repo ? name(p.repo) : undefined })
      c.setDraft('')
      c.show(null)
      c.go(run.session)
    } catch (e) {
      setNote(say(e))
      setBusy('')
    }
  }

  const shown = list ? find(list, q) : []
  return (
    <Dialog open onOpenChange={(o) => !o && c.show(null)}>
      <DialogContent maxW={520} width="100%">
        <DialogTitle>{t('Work in a project')}</DialogTitle>
        <DialogDescription>{ask ? t('Your message starts a Dev run on the project you pick.') : t('Open a project to start working in it.')}</DialogDescription>
        <Input value={q} onChangeText={setQ} placeholder={t('Search projects')} aria-label={t('Search projects')} autoFocus />
        <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" overflow="hidden" maxH={360} style={{ overflowY: 'auto' }}>
          {!list && !note ? (
            <SizableText size="$2" color="$soft" p="$3">
              {t('Reading your projects…')}
            </SizableText>
          ) : null}
          {list && !shown.length ? (
            <SizableText size="$2" color="$soft" p="$3">
              {q ? t('Nothing matches') : t('No projects yet')}
            </SizableText>
          ) : null}
          {shown.map((p, i) => (
            <XStack
              key={p.slug}
              render="button"
              aria-label={ask ? t('Start a run on {name}', { name: p.name }) : t('Open {name}', { name: p.name })}
              disabled={Boolean(busy)}
              onPress={() => void pick(p)}
              items="center"
              gap="$3"
              px="$3"
              py="$2.5"
              borderTopWidth={i ? 1 : 0}
              borderColor="$borderColor"
              cursor={busy ? 'default' : 'pointer'}
              hoverStyle={{ bg: '$hover' }}
              $platform-web={{ textAlign: 'start' }}
            >
              <Folder size={16} opacity={0.7} />
              <YStack flex={1} minW={0}>
                <SizableText size="$2" color="$ink" numberOfLines={1}>
                  {busy === p.slug ? t('Starting…') : p.name}
                </SizableText>
                <SizableText size="$1" color="$soft" numberOfLines={1}>
                  {p.repo ? address(p.repo) : t('No repository yet')}
                </SizableText>
              </YStack>
            </XStack>
          ))}
        </YStack>
        {note ? (
          <SizableText size="$2" color="$soft" role="status">
            {note}
          </SizableText>
        ) : null}
        <XStack justify="flex-end">
          <Button size="sm" variant="ghost" onPress={() => c.show(null)}>
            {t('Cancel')}
          </Button>
        </XStack>
      </DialogContent>
    </Dialog>
  )
}

export const project: Plugin = {
  id: 'project',
  name: phrase('Work in a project'),
  note: phrase('Start a Dev run in a project'),
  icon: <Folder size={16} />,
  open: (c) => c.show(<Picker c={c} />),
}
