/**
 * The folders a machine shares with the web (hanzo-inc/app issue 310), under its row in Settings → Machines.
 *
 * A run started from the web works in a folder of the machine only inside one its owner granted. Each is listed here
 * with Stop sharing. On Hanzo Desktop the machine that is this computer (`here`, found by its hostname) also has Let the
 * web use a folder…, which opens the computer's own folder dialog and grants the folder chosen. Granting and revoking
 * are the machine's owner's alone: the platform refuses anyone else, an org admin included, and its sentence is shown
 * as said.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a folder's path is drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { FolderOpen, FolderPlus } from '@hanzogui/lucide-icons-2'
import { Button } from '@hanzo/ui'
import { useState } from 'react'

import { told } from '../api/call.ts'
import { grant, revoke, type Machine } from '../api/machines.ts'
import { useLocal, useTarget } from '../host.tsx'
import { Note } from './ui.tsx'

export function Shared({ m, here, onChanged }: { m: Machine; here: boolean; onChanged: () => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const local = useLocal()
  // The folder being granted or revoked ('+' while the dialog is open), so one change is out at a time.
  const [busy, setBusy] = useState('')
  const [note, setNote] = useState('')
  const sharing = here && local !== null
  if (!sharing && m.folders.length === 0) return null

  const act = async (what: string, run: () => Promise<boolean>) => {
    setBusy(what)
    setNote('')
    try {
      if (await run()) onChanged()
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setBusy('')
    }
  }
  const share = () =>
    act('+', async () => {
      const f = await local?.pick()
      if (!f) return false
      await grant(target, m.id, f.path)
      return true
    })
  const stop = (folder: string) =>
    act(folder, async () => {
      await revoke(target, m.id, folder)
      return true
    })

  return (
    <YStack px="$4" pb="$3" gap="$2" data-slot="shared" aria-label={t('Folders the web may use')}>
      <SizableText size="$1" color="$soft" fontWeight="500">
        {t('Folders the web may use')}
      </SizableText>
      {m.folders.length === 0 ? (
        <SizableText size="$1" color="$soft">
          {t('None yet. A run sent here from the web works in a new folder.')}
        </SizableText>
      ) : (
        m.folders.map((f) => (
          <XStack key={f} items="center" justify="space-between" gap="$2" minW={0}>
            <XStack items="center" gap="$1.5" minW={0} flex={1}>
              <FolderOpen size={13} color="$soft" />
              <SizableText size="$2" color="$ink" fontFamily="$mono" numberOfLines={1} {...{ 'data-i18n-ltr': '' }}>
                {f}
              </SizableText>
            </XStack>
            <Button size="sm" variant="ghost" disabled={busy !== ''} aria-label={t('Stop sharing {folder}', { folder: f })} onPress={() => void stop(f)}>
              {t('Stop sharing')}
            </Button>
          </XStack>
        ))
      )}
      {sharing ? (
        <XStack>
          <Button size="sm" variant="outline" disabled={busy !== ''} onPress={() => void share()}>
            <FolderPlus size={14} /> {t('Let the web use a folder…')}
          </Button>
        </XStack>
      ) : null}
      <Note>{note}</Note>
    </YStack>
  )
}
