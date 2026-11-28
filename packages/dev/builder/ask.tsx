/**
 * Asking one thing before an act: to confirm one that cannot be undone, the
 * new name for something, or the address of the repository it works on. A
 * refusal is said in the dialog, which stays open, so the reason is read where
 * the act was asked for.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import {
  AlertDialog,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogTitle,
  Button,
  Dialog,
  DialogContent,
  DialogTitle,
  Input,
} from '@hanzo/ui'
import { useEffect, useId, useRef, useState } from 'react'

import { ADDRESS, onGitHub, type At } from './address.ts'

/** One act in flight, and what refused it. */
function useAct(onDone: () => void) {
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')
  const go = async (run: () => Promise<void>) => {
    setWorking(true)
    setNote('')
    try {
      await run()
      setWorking(false)
      onDone()
    } catch (e) {
      setNote((e as Error).message)
      setWorking(false)
    }
  }
  return { working, note, setNote, go }
}

function Said({ note }: { note: string }) {
  if (!note) return null
  return (
    <SizableText size="$1" color="$soft" role="status">
      {note}
    </SizableText>
  )
}

/**
 * THE confirm of this package: the one question asked before an act that cannot
 * be taken back (a delete) or that spends (spend.tsx draws it where the host
 * asks no question of its own). An AlertDialog: Cancel has the focus when it
 * opens, so Enter never says yes, a click outside is refused, and Escape shuts
 * the question alone, never a sheet or dialog it was asked from. Focus goes
 * back to what had it, the control that asked.
 */
export function Confirm({
  open,
  onOpenChange,
  title,
  says,
  act,
  no,
  danger = true,
  run,
}: {
  open: boolean
  onOpenChange: (o: boolean) => void
  title: string
  /** The whole consequence, stated plainly. */
  says: string
  /** The button's word: Delete, Remove, Forget, Run now. */
  act: string
  /** The safe answer's words, where Cancel would read as the act ("Keep plan" beside "Cancel plan"). */
  no?: string
  /** Red, for an act that destroys something; a spend is not one. */
  danger?: boolean
  run: () => Promise<void>
}) {
  const { t } = useI18n()
  const { working, note, setNote, go } = useAct(() => onOpenChange(false))
  // What had the focus when the question opened, read as it opens: before the dialog moves the focus to Cancel.
  const opener = useRef<HTMLElement | null>(null)
  const was = useRef(false)
  if (open && !was.current && typeof document !== 'undefined') opener.current = document.activeElement as HTMLElement | null
  was.current = open
  useEffect(() => {
    if (open) return
    const back = opener.current
    opener.current = null
    if (!back) return
    // After the dialog's own focus handling on close, back to the control that asked, if it is still on the page.
    const id = setTimeout(() => {
      if (back.isConnected) back.focus()
    }, 0)
    return () => clearTimeout(id)
  }, [open])
  const close = (o: boolean) => {
    if (working) return
    setNote('')
    onOpenChange(o)
  }
  return (
    <AlertDialog open={open} onOpenChange={close}>
      <AlertDialogContent
        data-slot="confirm"
        onKeyDown={(e: { key?: string; stopPropagation?: () => void }) => {
          // The question's own Escape: a sheet it was asked from never hears it.
          if (e.key === 'Escape') e.stopPropagation?.()
        }}
      >
        <AlertDialogTitle>{title}</AlertDialogTitle>
        <AlertDialogDescription>{says}</AlertDialogDescription>
        <Said note={note} />
        <AlertDialogFooter>
          <AlertDialogCancel variant="outline" data-slot="confirm-cancel" disabled={working} onPress={() => close(false)}>
            {no ?? t('Cancel')}
          </AlertDialogCancel>
          {/* A plain button, not the dialog's close part: a refusal is said here, with the question still open. */}
          <Button size="sm" variant={danger ? 'destructive' : 'primary'} data-slot="confirm-yes" disabled={working} onPress={() => void go(run)}>
            {act}
          </Button>
        </AlertDialogFooter>
      </AlertDialogContent>
    </AlertDialog>
  )
}

export function Rename({
  open,
  onOpenChange,
  title,
  name,
  run,
}: {
  open: boolean
  onOpenChange: (o: boolean) => void
  title: string
  /** The name it has now. */
  name: string
  run: (next: string) => Promise<void>
}) {
  const { t } = useI18n()
  const [value, setValue] = useState(name)
  const { working, note, setNote, go } = useAct(() => onOpenChange(false))
  useEffect(() => {
    if (open) setValue(name)
  }, [open, name])
  const close = (o: boolean) => {
    if (working) return
    setNote('')
    onOpenChange(o)
  }
  const save = () => {
    const next = value.trim()
    if (!next) return setNote(t('It needs a name'))
    if (next === name) return close(false)
    void go(() => run(next))
  }
  return (
    <Dialog open={open} onOpenChange={close}>
      <DialogContent maxW={420} showCloseButton={false}>
        <DialogTitle>{title}</DialogTitle>
        <Input
          autoFocus
          value={value}
          onChangeText={setValue}
          aria-label={t('Name')}
          onKeyDown={(e: { key?: string }) => {
            if (e.key === 'Enter') save()
          }}
        />
        <Said note={note} />
        <XStack gap="$2" justify="flex-end">
          <Button size="sm" variant="ghost" disabled={working} onPress={() => close(false)}>
            {t('Cancel')}
          </Button>
          <Button size="sm" disabled={working} onPress={save}>
            {t('Save')}
          </Button>
        </XStack>
      </DialogContent>
    </Dialog>
  )
}

/**
 * The repository something works on, by the address a person has for it:
 * GitHub's page, its clone address, or either without the scheme (address.ts).
 * Anything else is refused here, in the dialog, with the forms it takes, before
 * anything is sent. With a `branch`, the branch is asked beside it.
 */
export function Address({
  open,
  onOpenChange,
  title,
  says,
  act,
  address,
  branch,
  run,
}: {
  open: boolean
  onOpenChange: (o: boolean) => void
  title: string
  /** What the address decides, stated plainly. */
  says: string
  /** The button's word: Use, Save. */
  act: string
  /** The address it has now, or ''. */
  address: string
  /** The branch it has now. Left out, no branch is asked. */
  branch?: string
  run: (at: At, branch: string) => Promise<void>
}) {
  const { t } = useI18n()
  const id = useId()
  const [value, setValue] = useState(address)
  const [ref, setRef] = useState(branch ?? '')
  const { working, note, setNote, go } = useAct(() => onOpenChange(false))
  useEffect(() => {
    if (!open) return
    setValue(address)
    setRef(branch ?? '')
  }, [open, address, branch])
  const close = (o: boolean) => {
    if (working) return
    setNote('')
    onOpenChange(o)
  }
  const save = () => {
    const text = value.trim()
    const next = ref.trim()
    // Nothing changed is nothing to send.
    if (text && text === address.trim() && next === (branch ?? '').trim()) return close(false)
    const at = onGitHub(text)
    if (!at) return setNote(t(ADDRESS))
    void go(() => run(at, next))
  }
  const enter = (e: { key?: string }) => {
    if (e.key === 'Enter') save()
  }
  return (
    <Dialog open={open} onOpenChange={close}>
      <DialogContent maxW={480} showCloseButton={false}>
        <DialogTitle>{title}</DialogTitle>
        <SizableText size="$2" color="$soft">
          {says}
        </SizableText>
        <YStack gap="$1.5">
          <SizableText render="label" size="$2" color="$soft" htmlFor={`${id}-address`}>
            {t('Repository')}
          </SizableText>
          <Input
            id={`${id}-address`}
            autoFocus
            value={value}
            onChangeText={setValue}
            placeholder={t('https://github.com/owner/name')}
            aria-label={t('Repository')}
            onKeyDown={enter}
          />
        </YStack>
        {branch !== undefined ? (
          <YStack gap="$1.5">
            <SizableText render="label" size="$2" color="$soft" htmlFor={`${id}-branch`}>
              {t('Branch')}
            </SizableText>
            <Input id={`${id}-branch`} value={ref} onChangeText={setRef} placeholder="main" aria-label={t('Branch')} onKeyDown={enter} />
          </YStack>
        ) : null}
        <Said note={note} />
        <XStack gap="$2" justify="flex-end">
          <Button size="sm" variant="ghost" disabled={working} onPress={() => close(false)}>
            {t('Cancel')}
          </Button>
          <Button size="sm" disabled={working} onPress={save}>
            {act}
          </Button>
        </XStack>
      </DialogContent>
    </Dialog>
  )
}
