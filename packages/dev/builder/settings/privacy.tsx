/**
 * Privacy: the two data-sharing answers IAM records for the person — whether
 * Hanzo may train on their data, and whether anonymous usage is counted — and
 * who can see the organization's projects: a public one is in the community
 * catalogue with its source readable by anyone, until it is made private.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a project's name is
 * drawn as given.
 */
import { YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Button, Switch } from '@hanzo/ui'
import { useState } from 'react'

import { told } from '../api/call.ts'
import { consent, setConsent, type Consent } from '../api/consent.ts'
import { change, projects, type Project } from '../api/projects.ts'
import { useRead } from '../data.ts'
import { useHost, useTarget } from '../host.tsx'
import { Card, Group, Heading, Note, Row, Soft } from './ui.tsx'

export function Privacy() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const answers = useRead(signed ? () => consent(target) : null, null as Consent | null, [target, signed])
  const published = useRead(signed ? () => projects(target) : null, [] as Project[], [target, signed])
  const [shown, setShown] = useState<Consent | null>(null)
  const [busy, setBusy] = useState('')
  const [note, setNote] = useState('')

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('Privacy')} />
        <Soft>{t('Sign in to see your privacy settings.')}</Soft>
      </YStack>
    )
  }

  const now = shown ?? answers.value
  const answer = async (change: Partial<Consent>) => {
    setBusy('consent')
    setNote('')
    try {
      setShown(await setConsent(target, change))
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setBusy('')
    }
  }

  const flip = async (p: Project) => {
    setBusy(p.slug)
    setNote('')
    try {
      const saved = await change(target, p.slug, { visibility: p.visibility === 'private' ? 'public' : 'private' })
      published.reload()
      // The two visibilities this page offers are said in words; any other is the platform's, as given.
      setNote(
        saved.visibility === 'public'
          ? t('{name} is public', { name: saved.name })
          : saved.visibility === 'private'
            ? t('{name} is private', { name: saved.name })
            : saved.visibility
              ? t('{name} is {visibility}', { name: saved.name, visibility: saved.visibility })
              : t('{name} is saved', { name: saved.name }),
      )
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setBusy('')
    }
  }

  const training = now?.training ?? ''
  return (
    <YStack gap="$6">
      <Heading title={t('Privacy')} detail={t('What Hanzo may do with your data, and what your organization shows the world.')} />
      <Group title={t('Your data')} detail={t('Recorded on your Hanzo account and honoured by every Hanzo surface.')}>
        {answers.error && !now ? (
          <Soft>{told(answers.error, t)}</Soft>
        ) : !now ? (
          <Soft>{t('Reading your answers…')}</Soft>
        ) : (
          <Card>
            <Row
              first
              title={t('Help improve Hanzo models')}
              detail={
                training === 'granted'
                  ? t('Hanzo may train on your prompts and runs.')
                  : training === 'refused'
                    ? t('Hanzo does not train on your data.')
                    : t('Not answered yet. Until you say yes, Hanzo does not train on your data.')
              }
              trailing={
                <Switch
                  aria-label={t('Help improve Hanzo models')}
                  checked={training === 'granted'}
                  disabled={busy === 'consent'}
                  onCheckedChange={(on: boolean) => void answer({ training: on ? 'granted' : 'refused' })}
                />
              }
            />
            <Row
              title={t('Usage insights')}
              detail={t('Anonymous counts of how the product is used. No prompts, code or answers.')}
              trailing={
                <Switch
                  aria-label={t('Usage insights')}
                  checked={now.insights}
                  disabled={busy === 'consent'}
                  onCheckedChange={(on: boolean) => void answer({ insights: on })}
                />
              }
            />
          </Card>
        )}
      </Group>
      <Group
        title={t('Projects')}
        detail={t('A public project is listed in the community catalogue, and anyone can read its source. Keeping one private needs a paid plan.')}
      >
        {published.error ? (
          <Soft>{told(published.error, t)}</Soft>
        ) : published.loading && !published.value.length ? (
          <Soft>{t('Reading projects…')}</Soft>
        ) : !published.value.length ? (
          <Soft>{t('Your organization has no projects yet.')}</Soft>
        ) : (
          <Card>
            {published.value.map((p, i) => (
              <Row
                key={p.slug}
                first={i === 0}
                title={p.name}
                detail={p.visibility === 'private' ? t('Private') : p.visibility === 'public' ? t('Public') : t('Visibility not reported')}
                trailing={
                  p.visibility ? (
                    <Button size="sm" variant="outline" disabled={busy === p.slug} onPress={() => void flip(p)}>
                      {p.visibility === 'private' ? t('Make public') : t('Make private')}
                    </Button>
                  ) : null
                }
              />
            ))}
          </Card>
        )}
      </Group>
      <Note>{note}</Note>
    </YStack>
  )
}
