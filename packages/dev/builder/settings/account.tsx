/**
 * Account: who is signed in and how Hanzo should know them — the photo and
 * full name on their Hanzo identity, what to call them, what they work on and
 * the instructions every coding run reads — then the organizations they belong
 * to, the one this page acts in, a way to act in another, and logging out.
 *
 * The email is how the person signs in, so it is shown and not edited here.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a name, an email and
 * an organization's id are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Check, Copy, LogOut } from '@hanzogui/lucide-icons-2'
import { Button, Input, Textarea } from '@hanzo/ui'
import { ChipSelect } from '@hanzo/ui/product'
import { useEffect, useRef, useState } from 'react'

import { told } from '../api/call.ts'
import type { Patch } from '../api/pref.ts'
import { PHOTO, photo, rename } from '../api/profile.ts'
import { copy } from '../clipboard.ts'
import { useHost, useTarget } from '../host.tsx'
import { usePrefs } from '../prefs.tsx'
import { WORK } from '../roles.ts'
import { Card, Field, Group, Heading, Note, Row, Soft } from './ui.tsx'

/** Long enough for real guidance, short enough to sit in the settings document beside everything else. */
const INSTRUCTIONS = 3000

export function Account() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const { prefs, save } = usePrefs()
  const who = host.person
  const [name, setName] = useState(who?.name ?? '')
  const [shownName, setShownName] = useState(who?.name ?? '')
  const [face, setFace] = useState(who?.avatar ?? '')
  const [callName, setCallName] = useState('')
  const [work, setWork] = useState('')
  const [instructions, setInstructions] = useState('')
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState('')
  const picker = useRef<HTMLInputElement | null>(null)

  const [copied, setCopied] = useState(false)

  // The identity can arrive after the page does.
  useEffect(() => {
    setName(who?.name ?? '')
    setShownName(who?.name ?? '')
    setFace(who?.avatar ?? '')
  }, [who?.name, who?.avatar])

  useEffect(() => {
    setCallName(prefs.callName ?? '')
    setWork(prefs.work ?? '')
    setInstructions(prefs.instructions ?? '')
  }, [prefs.callName, prefs.work, prefs.instructions])

  if (!who) {
    return (
      <YStack gap="$5">
        <Heading title={t('Account')} />
        <Soft>{t('Sign in to see your account.')}</Soft>
        {host.signIn ? (
          <XStack justify="center">
            <Button size="sm" onPress={() => host.signIn?.()}>
              {t('Sign in')}
            </Button>
          </XStack>
        ) : null}
      </YStack>
    )
  }

  const patch: Patch = {}
  if (callName.trim() !== (prefs.callName ?? '')) patch.callName = callName.trim() || null
  // A work is only ever chosen from the list, never emptied.
  if (work !== (prefs.work ?? '')) patch.work = work
  if (instructions.trim() !== (prefs.instructions ?? '').trim()) patch.instructions = instructions.trim() || null
  const renamed = name.trim() !== shownName.trim()
  const changed = renamed || Object.keys(patch).length > 0

  const submit = async () => {
    setBusy(true)
    setNote('')
    try {
      if (renamed) setShownName((await rename(target, name)).displayName || name.trim())
      if (Object.keys(patch).length) await save(patch)
      setNote(t('Saved'))
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setBusy(false)
    }
  }

  const upload = async (file: File) => {
    setBusy(true)
    setNote('')
    try {
      setFace(await photo(target, file))
      setNote(t('Your photo is saved'))
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setBusy(false)
    }
  }

  const org = host.org
  const orgs = host.memberships ?? (org ? [org] : [])
  // The list's words in the reader's language; a work the list does not hold is the person's own, as given.
  const works = WORK.map((w) => ({ ...w, label: t(w.label) }))
  const chosenWork = works.find((w) => w.id === work) ?? (work ? { id: work, label: work } : null)

  return (
    <YStack gap="$6">
      <Heading
        title={t('Account')}
        detail={t('Your Hanzo identity, and what Hanzo should know about you.')}
        action={
          host.signOut ? (
            <Button size="sm" variant="outline" onPress={() => host.signOut?.()}>
              <LogOut size={14} /> {t('Log out')}
            </Button>
          ) : undefined
        }
      />

      <Group title={t('Profile')}>
        <YStack gap="$4">
          <XStack items="center" gap="$3">
            {face ? (
              <img src={face} alt="" width={48} height={48} style={{ borderRadius: 999, objectFit: 'cover' }} />
            ) : (
              <YStack width={48} height={48} rounded={999} bg="$raised" items="center" justify="center">
                <SizableText size="$5" color="$ink">
                  {(shownName || who.email || '?').charAt(0).toUpperCase()}
                </SizableText>
              </YStack>
            )}
            <YStack flex={1} minW={0}>
              <SizableText size="$4" color="$ink" numberOfLines={1}>
                {shownName || who.email}
              </SizableText>
              <SizableText size="$2" color="$soft" numberOfLines={1}>
                {who.email}
              </SizableText>
            </YStack>
            <Button size="sm" variant="outline" disabled={busy} onPress={() => picker.current?.click()}>
              {t('Change photo')}
            </Button>
            <input
              ref={picker}
              type="file"
              accept={PHOTO}
              hidden
              style={{ display: 'none' }}
              tabIndex={-1}
              aria-label={t('Photo')}
              onChange={(e) => {
                const input = e.currentTarget
                const file = input.files?.[0]
                input.value = ''
                if (file) void upload(file)
              }}
            />
          </XStack>
          <Field label={t('Full name')}>
            <Input value={name} onChangeText={setName} aria-label={t('Full name')} placeholder={t('Your name')} />
          </Field>
          <Field label={t('Email')} hint={t('You sign in with this address, so it is not changed here.')}>
            <SizableText size="$2" color="$soft" numberOfLines={1}>
              {who.email || t('No email on this account')}
            </SizableText>
          </Field>
          <Field label={t('What should Hanzo call you?')}>
            <Input value={callName} onChangeText={setCallName} aria-label={t('What should Hanzo call you?')} placeholder={shownName.split(' ')[0] || t('A first name')} />
          </Field>
          <Field label={t('What best describes your work?')}>
            <XStack>
              <ChipSelect
                name={t('Work')}
                label={chosenWork?.label ?? t('Choose one')}
                chosen={chosenWork}
                items={works}
                onChange={(w) => setWork(w.id)}
                placement="bottom-start"
                width={260}
              />
            </XStack>
          </Field>
          <Field
            label={t('Instructions for Hanzo')}
            hint={t('Every coding run reads these before it starts. {used}/{most}', { used: instructions.length, most: INSTRUCTIONS })}
          >
            <Textarea
              value={instructions}
              onChangeText={(v) => setInstructions(v.slice(0, INSTRUCTIONS))}
              aria-label={t('Instructions for Hanzo')}
              placeholder={t('How you like to work: the languages and tools you prefer, the style to follow, what to avoid.')}
              rows={5}
            />
          </Field>
          <XStack items="center" gap="$3">
            <Button size="sm" disabled={busy || !changed} onPress={() => void submit()}>
              {t('Save')}
            </Button>
            <Note>{note}</Note>
          </XStack>
        </YStack>
      </Group>

      {org ? (
        <Group title={t('Organization')} detail={t('The organization this page acts in. Runs, codebases and settings belong to it.')}>
          <Card>
            <Row
              first
              mono
              title={org}
              detail={host.admin ? t('Organization ID · you are an admin') : t('Organization ID')}
              trailing={
                <Button
                  size="icon-sm"
                  variant="ghost"
                  aria-label={copied ? t('Organization ID copied') : t('Copy organization ID')}
                  onPress={() => void copy(org).then((ok) => ok && setCopied(true))}
                >
                  {copied ? <Check size={14} /> : <Copy size={14} />}
                </Button>
              }
            />
          </Card>
        </Group>
      ) : null}

      <Group title={t('Organizations')} detail={t('Every organization you belong to. Switch to act in another.')}>
        {orgs.length ? (
          <Card>
            {orgs.map((o, i) => (
              <Row
                key={o}
                first={i === 0}
                title={o}
                detail={o === host.org ? (host.admin ? t('Acting in {org} as an admin', { org: o }) : t('Acting in {org}', { org: o })) : undefined}
                trailing={
                  o === host.org ? (
                    <Check size={16} />
                  ) : host.chooseOrg ? (
                    <Button size="sm" variant="ghost" onPress={() => host.chooseOrg?.(o)}>
                      {t('Switch')}
                    </Button>
                  ) : null
                }
              />
            ))}
          </Card>
        ) : (
          <Soft>{t('You belong to no organization yet.')}</Soft>
        )}
      </Group>
    </YStack>
  )
}
