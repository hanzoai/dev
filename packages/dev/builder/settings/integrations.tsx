/**
 * Integrations: your own GitHub connection, the GitHub accounts the
 * organization has installed the platform's App on, its Slack workspace, and
 * the social accounts its automations post to.
 *
 * Connecting leaves this page once, for GitHub's or Slack's own consent screen,
 * and the platform's callback brings the person back here with the answer
 * (../back.ts), which this page finishes and reports. Anyone connects their own
 * GitHub; installing the App and connecting Slack are an org admin's, which is
 * the platform's rule. A GitHub change made here, on Projects or in the Hanzo
 * app's Quick setup is said on the window (../api/github.ts `changed`), and
 * this page reads GitHub again.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); an account's,
 * a channel's and a provider's names, and the platform's own notes, are drawn
 * as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Github, Hash, Lock, Megaphone, Slack } from '@hanzogui/lucide-icons-2'
import { Button } from '@hanzo/ui'
import { useEffect, useState } from 'react'

import { told } from '../api/call.ts'
import { changed, connect, connection, disconnect as unlinkGithub, heard, installations, type Connection, type Installation } from '../api/github.ts'
import { authorize, channels, disconnect, read, SOCIAL, type Channels, type Connector } from '../api/provider.ts'
import { here, useBack } from '../back.ts'
import { useRead } from '../data.ts'
import { useHost, useTarget } from '../host.tsx'
import { Card, day, Group, Heading, Note, Row, Soft } from './ui.tsx'

export function Integrations() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const mine = useRead(signed ? () => connection(target) : null, null as Connection | null, [target, signed])
  const installed = useRead(signed ? () => installations(target) : null, [] as Installation[], [target, signed])
  const slack = useRead(signed ? () => read(target, 'slack') : null, null as Connector | null, [target, signed])
  const joined = slack.value?.connected ?? false
  const rooms = useRead(signed && joined ? () => channels(target) : null, { channels: [], next: '' } as Channels, [target, signed, joined])
  const social = useSocial(signed, SOCIAL.map((p) => p.id))
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')
  const { reload: readMine } = mine
  const { reload: readInstalled } = installed
  useEffect(
    () =>
      heard(() => {
        readMine()
        readInstalled()
      }),
    [readMine, readInstalled],
  )
  useBack(target, signed, setNote, () => {
    slack.reload()
    social.reload()
  })

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('Integrations')} />
        <Soft>{t('Sign in to see this organization’s integrations.')}</Soft>
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

  /** Off to the provider's consent page; it is the one hop away from this page. */
  const away = async (where: () => Promise<string>) => {
    setWorking(true)
    setNote('')
    try {
      host.open(await where())
    } catch (e) {
      setNote(told(e, t))
      setWorking(false)
    }
  }

  const me = mine.value
  const s = slack.value

  return (
    <YStack gap="$6">
      <Heading
        title={t('Integrations')}
        detail={host.org ? t('What {org} and you have connected.', { org: host.org }) : t('What this organization and you have connected.')}
      />

      <Group title="GitHub" detail={t('Your connection brings your repositories; the App brings an account’s to the organization.')}>
        <Card>
          <Row
            first
            leading={<Github size={16} />}
            title={t('Your GitHub')}
            detail={
              mine.error
                ? told(mine.error, t)
                : !me
                  ? t('Reading…')
                  : !me.configured
                    ? t('This deployment cannot connect GitHub yet.')
                    : me.connected
                      ? t('Connected as @{login}', { login: me.login })
                      : t('Not connected')
            }
            trailing={
              me?.configured ? (
                me.connected ? (
                  <Button size="sm" variant="outline" disabled={working} onPress={() => void act(() => unlinkGithub(target), t('Your GitHub is disconnected'), changed)}>
                    {t('Disconnect')}
                  </Button>
                ) : (
                  <Button size="sm" variant="outline" disabled={working} onPress={() => void away(() => connect(target, here()))}>
                    {t('Connect')}
                  </Button>
                )
              ) : null
            }
          />
          {installed.error ? (
            <Row title={t('App installations')} detail={told(installed.error, t)} />
          ) : (
            installed.value.map((i) => (
              <Row
                key={i.login}
                leading={<Github size={16} />}
                title={i.login}
                detail={[i.type, i.grant === 'all' ? t('every repository') : i.grant === 'selected' ? t('selected repositories') : ''].filter(Boolean).join(' · ')}
                trailing={
                  <XStack gap="$2" items="center">
                    <SizableText size="$1" color={i.connected ? '$ink' : '$soft'}>
                      {i.connected ? t('Installed') : t('Removed on GitHub')}
                    </SizableText>
                    {/* An org admin lets go of one account; the platform uninstalls the App there unless another organization uses it. */}
                    {host.admin ? (
                      <Button
                        size="sm"
                        variant="outline"
                        disabled={working}
                        onPress={() => void act(() => disconnect(target, 'github', i.login), t('@{login} is disconnected', { login: i.login }), changed)}
                      >
                        {t('Disconnect')}
                      </Button>
                    ) : null}
                  </XStack>
                }
              />
            ))
          )}
          {!installed.error && !installed.loading && installed.value.length === 0 ? (
            <Row title={t('No GitHub accounts installed')} detail={t('Install the App on a GitHub account to bring its repositories here.')} />
          ) : null}
        </Card>
        {host.admin ? (
          <XStack>
            <Button size="sm" variant="outline" disabled={working} onPress={() => void away(() => authorize(target, 'github', here()))}>
              {t('Install on a GitHub account')}
            </Button>
          </XStack>
        ) : (
          <SizableText size="$1" color="$soft">
            {t('An org admin installs the App on a GitHub account.')}
          </SizableText>
        )}
      </Group>

      <Group title="Slack" detail={t('Talk to Hanzo from your workspace’s channels.')}>
        <Card>
          <Row
            first
            leading={<Slack size={16} />}
            title={s?.connected && s.account ? s.account : 'Slack'}
            detail={
              slack.error
                ? told(slack.error, t)
                : !s
                  ? t('Reading…')
                  : s.connected
                    ? s.since
                      ? t('Connected since {day}', { day: day(s.since) })
                      : t('Connected')
                    : s.available
                      ? t('Not connected')
                      : s.note || t('This deployment cannot connect Slack yet.')
            }
            trailing={
              host.admin && s && (s.connected || s.available) ? (
                s.connected ? (
                  <Button size="sm" variant="outline" disabled={working} onPress={() => void act(() => disconnect(target, 'slack'), t('{name} is disconnected', { name: 'Slack' }), slack.reload)}>
                    {t('Disconnect')}
                  </Button>
                ) : (
                  <Button size="sm" variant="outline" disabled={working} onPress={() => void away(() => authorize(target, 'slack', here()))}>
                    {t('Connect')}
                  </Button>
                )
              ) : null
            }
          />
          {joined && rooms.error ? <Row title={t('Channels')} detail={told(rooms.error, t)} /> : null}
          {joined
            ? rooms.value.channels.map((c) => (
                <Row
                  key={c.id}
                  leading={c.private ? <Lock size={14} /> : <Hash size={14} />}
                  title={c.name || c.id}
                  trailing={
                    <SizableText size="$1" color={c.member ? '$ink' : '$soft'}>
                      {c.member ? t('Joined') : t('Not joined')}
                    </SizableText>
                  }
                />
              ))
            : null}
        </Card>
        {!host.admin ? (
          <SizableText size="$1" color="$soft">
            {t('An org admin connects the Slack workspace.')}
          </SizableText>
        ) : null}
      </Group>

      <Group title={t('Social accounts')} detail={t('Where automations post what their agent writes.')}>
        <Card>
          {SOCIAL.map((p, i) => {
            const c = social.value[p.id]
            return (
              <Row
                key={p.id}
                first={i === 0}
                leading={<Megaphone size={16} />}
                title={c?.connected && c.account ? `${p.name} · ${c.account}` : p.name}
                detail={
                  social.error
                    ? told(social.error, t)
                    : !c
                      ? t('Reading…')
                      : c.connected
                        ? // Connected, then what the account takes, as two sentences.
                          p.takes
                          ? `${c.since ? t('Connected since {day}.', { day: day(c.since) }) : t('Connected.')} ${t(p.takes)}`
                          : c.since
                            ? t('Connected since {day}', { day: day(c.since) })
                            : t('Connected')
                        : c.available
                          ? p.takes ? t(p.takes) : t('Not connected')
                          : p.takes ? `${t(p.takes)} ${t(p.needs)}` : t(p.needs)
                }
                trailing={
                  host.admin && c && (c.connected || c.available) ? (
                    c.connected ? (
                      <Button size="sm" variant="outline" disabled={working} onPress={() => void act(() => disconnect(target, p.id), t('{name} is disconnected', { name: p.name }), social.reload)}>
                        {t('Disconnect')}
                      </Button>
                    ) : (
                      <Button size="sm" variant="outline" disabled={working} onPress={() => void away(() => authorize(target, p.id, here()))} aria-label={t('Connect {name}', { name: p.name })}>
                        {t('Connect')}
                      </Button>
                    )
                  ) : null
                }
              />
            )
          })}
        </Card>
        {!host.admin ? (
          <SizableText size="$1" color="$soft">
            {t('An org admin connects the organization’s social accounts.')}
          </SizableText>
        ) : null}
      </Group>

      <Note>{note}</Note>
    </YStack>
  )
}

/**
 * The named social connectors, by id: whether each can be connected here and
 * whether it is. One that could not be read is left out, and its row reads on
 * as "Reading…" rather than taking the others with it.
 */
export function useSocial(signed: boolean, ids: readonly string[]) {
  const t = useTarget()
  const key = ids.join(',')
  return useRead(
    signed
      ? async () => {
          const got = await Promise.allSettled(ids.map((id) => read(t, id)))
          return Object.fromEntries(got.flatMap((r, i) => (r.status === 'fulfilled' ? [[ids[i]!, r.value] as const] : [])))
        }
      : null,
    {} as Record<string, Connector>,
    [t, signed, key],
  )
}
