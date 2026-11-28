/**
 * Members: who is in the organization and in what role, and — for an org admin —
 * who has been invited, inviting someone by email, and withdrawing an invitation.
 * IAM holds both lists and decides who may read and write them.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a person's name,
 * address and id, an invitation's code and IAM's own state are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { UserPlus } from '@hanzogui/lucide-icons-2'
import { Button, Input } from '@hanzo/ui'
import { useState } from 'react'

import { told } from '../api/call.ts'
import { EMAIL, invitations, invite, revoke, roster, type Invitation, type Member } from '../api/members.ts'
import { useRead } from '../data.ts'
import { useHost, useTarget } from '../host.tsx'
import { Card, day, Group, Heading, Note, Row, Soft } from './ui.tsx'

/** A role by its name, English and marked (`phrase`), drawn through `t`. */
const ROLE = { owner: phrase('Owner'), admin: phrase('Admin'), member: phrase('Member') } as const

function Initial({ name }: { name: string }) {
  return (
    <YStack width={28} height={28} rounded={999} bg="$raised" items="center" justify="center" shrink={0}>
      <SizableText size="$2" color="$ink">
        {(name || '?').charAt(0).toUpperCase()}
      </SizableText>
    </YStack>
  )
}

export function Members() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const org = host.org ?? ''
  const signed = Boolean(host.person && org)
  const people = useRead(signed ? () => roster(target, org) : null, [] as Member[], [target, org, signed])
  const asked = useRead(signed && host.admin ? () => invitations(target, org) : null, [] as Invitation[], [target, org, signed, host.admin])
  const [email, setEmail] = useState('')
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('Members')} />
        <Soft>{host.person ? t('Choose an organization to see its members.') : t('Sign in to see this organization’s members.')}</Soft>
      </YStack>
    )
  }

  const act = async (what: () => Promise<unknown>, done: string, after: () => void) => {
    setWorking(true)
    setNote('')
    try {
      await what()
      after()
      setNote(done)
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setWorking(false)
    }
  }

  const send = () => {
    const address = email.trim()
    // Invite is offered only once something is typed.
    if (!EMAIL.test(address)) {
      setNote(t('{email} is not an email address', { email: address }))
      return
    }
    void act(() => invite(target, org, address), t('{email} is invited. IAM sends no email: share the code with them.', { email: address }), () => {
      setEmail('')
      asked.reload()
    })
  }

  const open = asked.value.filter((i) => i.state === 'Active' && i.used < i.seats)

  return (
    <YStack gap="$6">
      <Heading title={t('Members')} detail={t('The people who can act in {org}, and the role each one holds.', { org })} />

      <Group title={t('Members')} detail={people.value.length ? t('{n} in {org}', { n: people.value.length, org }) : undefined}>
        {people.error ? (
          <Soft>{told(people.error, t)}</Soft>
        ) : people.loading && !people.value.length ? (
          <Soft>{t('Reading members…')}</Soft>
        ) : people.value.length === 0 ? (
          <Card>
            <Soft>{t('No one is on this organization’s roster yet.')}</Soft>
          </Card>
        ) : (
          <Card>
            {people.value.map((m, i) => (
              <Row
                key={m.user}
                first={i === 0}
                leading={<Initial name={m.name} />}
                title={m.name}
                detail={[m.user, m.since ? t('since {day}', { day: day(m.since) }) : ''].filter(Boolean).join(' · ')}
                trailing={
                  <SizableText size="$1" color={m.role === 'member' ? '$soft' : '$ink'}>
                    {t(ROLE[m.role])}
                  </SizableText>
                }
              />
            ))}
          </Card>
        )}
      </Group>

      {host.admin ? (
        <Group title={t('Invitations')} detail={t('An invitation is a one-seat code for one address, redeemed when that person signs up.')}>
          <Card>
            <XStack gap="$2" items="center" px="$3" py="$3" flexWrap="wrap">
              <YStack flex={1} minW={200}>
                <Input value={email} onChangeText={setEmail} placeholder="name@company.com" aria-label={t('Email to invite')} inputMode="email" autoCapitalize="none" />
              </YStack>
              <Button size="sm" disabled={working || !email.trim()} onPress={send}>
                <UserPlus size={14} /> {t('Invite')}
              </Button>
            </XStack>
            {asked.error ? (
              <YStack borderTopWidth={1} borderColor="$borderColor">
                <Soft>{told(asked.error, t)}</Soft>
              </YStack>
            ) : null}
            {asked.value.map((i) => (
              <Row
                key={i.name}
                title={i.email || i.name}
                mono={!i.email}
                detail={[
                  i.code ? t('Code {code}', { code: i.code }) : '',
                  t('{used} of {seats} joined', { used: i.used, seats: i.seats }),
                  // IAM's own word for a state, as given; none at all is ours.
                  i.state !== 'Active' ? i.state.toLowerCase() || t('not redeemable') : '',
                  day(i.created) ? t('invited {day}', { day: day(i.created) }) : '',
                ]
                  .filter(Boolean)
                  .join(' · ')}
                trailing={
                  <Button size="sm" variant="ghost" disabled={working} aria-label={t('Revoke the invitation for {who}', { who: i.email || i.name })} onPress={() => void act(() => revoke(target, i), t('The invitation is withdrawn'), asked.reload)}>
                    {t('Revoke')}
                  </Button>
                }
              />
            ))}
          </Card>
          {!asked.loading && !asked.error && asked.value.length > 0 && open.length === 0 ? (
            <SizableText size="$1" color="$soft">
              {t('Every invitation has been used or stopped.')}
            </SizableText>
          ) : null}
        </Group>
      ) : (
        <SizableText size="$1" color="$soft">
          {t('An org admin invites people and sees the open invitations.')}
        </SizableText>
      )}

      <Note>{note}</Note>
    </YStack>
  )
}
