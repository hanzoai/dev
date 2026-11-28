/**
 * Environments: every codebase in the organization that has one — what a sandbox
 * run does to its checkout before the agent starts, and the secrets it exports.
 * One opens in the same editor a run shows beside it. A member reads; an org
 * admin saves, sets secrets and forgets an environment, which is the platform's
 * rule and not a choice made here.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a codebase's name is
 * its own, drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { ChevronLeft, ChevronRight } from '@hanzogui/lucide-icons-2'
import { phrase } from '@hanzo/i18n'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { Button } from '@hanzo/ui'
import { useState } from 'react'

import { told } from '../api/call.ts'
import { environments, forget, type Environment as Env } from '../api/environment.ts'
import { Confirm } from '../ask.tsx'
import { useRead } from '../data.ts'
import { Environment } from '../environment.tsx'
import { useHost, useTarget } from '../host.tsx'
import { Card, Heading, Note, Row, Soft } from './ui.tsx'

const STATE: Record<Env['state'], string> = { none: phrase('Not set up'), proposed: phrase('Proposed, waiting for review'), ready: phrase('Ready') }

/** The line under a codebase: where it stands, its secrets, when it was saved, each fact in the reader's words. */
function about(e: Env, t: Say): string {
  const parts = [t(STATE[e.state])]
  if (e.secrets.length) parts.push(e.secrets.length === 1 ? t('1 secret') : t('{n} secrets', { n: e.secrets.length }))
  const when = e.updated ? new Date(e.updated) : null
  if (when && !Number.isNaN(when.getTime())) parts.push(t('Updated {date}', { date: when.toLocaleDateString(undefined, { month: 'short', day: 'numeric', year: 'numeric' }) }))
  return parts.join(' · ')
}

export function Environments() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const list = useRead(signed ? () => environments(target) : null, [] as Env[], [target, signed])
  const [open, setOpen] = useState('')
  const [forgetting, setForgetting] = useState(false)
  const [note, setNote] = useState('')

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('Environments')} />
        <Soft>{t('Sign in to see your organization’s environments.')}</Soft>
      </YStack>
    )
  }

  if (open) {
    return (
      <YStack gap="$5">
        <XStack items="center" gap="$2">
          <Button size="sm" variant="ghost" onPress={() => setOpen('')}>
            <ChevronLeft size={14} {...{ 'data-i18n-mirror': '' }} /> {t('All environments')}
          </Button>
          <XStack flex={1} />
          {host.admin ? (
            <Button size="sm" variant="outline" onPress={() => setForgetting(true)}>
              {t('Forget')}
            </Button>
          ) : null}
        </XStack>
        <Environment repo={open} busy={false} />
        <Confirm
          open={forgetting}
          onOpenChange={setForgetting}
          title={t('Forget {repo}’s environment?', { repo: open })}
          says={t('Later runs on this codebase start bare: its scripts are dropped and every secret it holds is removed from KMS. This cannot be undone.')}
          act={t('Forget')}
          run={async () => {
            await forget(target, open)
            setNote(t('{repo}’s environment is forgotten', { repo: open }))
            setOpen('')
            list.reload()
          }}
        />
      </YStack>
    )
  }

  return (
    <YStack gap="$5">
      <Heading
        title={t('Environments')}
        detail={t('What a sandbox run does to a codebase before its agent starts: the install script, the start command, and the secrets it exports.')}
      />
      {list.error ? (
        <Soft>{told(list.error, t)}</Soft>
      ) : list.loading && list.value.length === 0 ? (
        <Soft>{t('Reading environments…')}</Soft>
      ) : list.value.length === 0 ? (
        <Soft>{t('No codebase has an environment yet. Set one up from New, or from a run’s Environment tab.')}</Soft>
      ) : (
        <Card>
          {list.value.map((e, i) => (
            <YStack
              key={e.repo}
              render="button"
              aria-label={t('Open {repo}', { repo: e.repo })}
              onPress={() => {
                setNote('')
                setOpen(e.repo)
              }}
              cursor="pointer"
              hoverStyle={{ bg: '$hover' }}
              $platform-web={{ textAlign: 'start' }}
            >
              <Row first={i === 0} title={e.repo} detail={about(e, t)} trailing={<ChevronRight size={16} opacity={0.6} {...{ 'data-i18n-mirror': '' }} />} />
            </YStack>
          ))}
        </Card>
      )}
      {!host.admin ? (
        <SizableText size="$1" color="$soft">
          {t('An org admin saves an environment and sets its secrets.')}
        </SizableText>
      ) : null}
      <Note>{note}</Note>
    </YStack>
  )
}
