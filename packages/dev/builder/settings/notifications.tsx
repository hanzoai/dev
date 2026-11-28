/**
 * Notifications: the organization's webhooks. Each is an https address that
 * every matching event is POSTed to, signed with a secret shown once, when the
 * webhook is added. A test sends one signed event now; the log says what each
 * delivery got back.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); an address, an event
 * pattern, a description and what an endpoint answered are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Copy, Plus, Webhook, X } from '@hanzogui/lucide-icons-2'
import { Button, Input } from '@hanzo/ui'
import { useState } from 'react'

import { told } from '../api/call.ts'
import { add, deliveries, endpoints, patterns, refuse, remove, test, type Attempt, type Endpoint } from '../api/webhooks.ts'
import { useRead } from '../data.ts'
import { copy } from '../clipboard.ts'
import { useHost, useTarget } from '../host.tsx'
import { Card, day, Field, Group, Heading, Note, Row, Soft } from './ui.tsx'

const mono = { fontFamily: 'var(--f-mono, ui-monospace, monospace)' }

export function Notifications() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const list = useRead(signed ? () => endpoints(target) : null, [] as Endpoint[], [target, signed])
  const [adding, setAdding] = useState(false)
  const [url, setUrl] = useState('')
  const [events, setEvents] = useState('')
  const [description, setDescription] = useState('')
  const [secret, setSecret] = useState('')
  const [log, setLog] = useState<{ id: string; rows: Attempt[] } | null>(null)
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('Notifications')} />
        <Soft>{t('Sign in to see this organization’s webhooks.')}</Soft>
      </YStack>
    )
  }

  const act = async (what: () => Promise<string>) => {
    setWorking(true)
    setNote('')
    try {
      setNote(await what())
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setWorking(false)
    }
  }

  const create = () => {
    const draft = { url, events: patterns(events), description }
    const why = refuse(draft)
    if (why) {
      setNote(t(why))
      return
    }
    void act(async () => {
      const e = await add(target, draft)
      setSecret(e.secret)
      setAdding(false)
      setUrl('')
      setEvents('')
      setDescription('')
      list.reload()
      return t('{url} is added', { url: e.url })
    })
  }

  const trial = (e: Endpoint) =>
    void act(async () => {
      const r = await test(target, e.id)
      if (log?.id === e.id) setLog({ id: e.id, rows: await deliveries(target, e.id) })
      // What the endpoint answered is its own; the sentence around it is ours.
      return r.delivered
        ? t('The test reached {url}: {status} in {ms} ms', { url: e.url, status: r.status, ms: r.ms })
        : r.error
          ? t('The test did not reach {url}: {error}', { url: e.url, error: r.error })
          : r.status
            ? t('The test did not reach {url}: it answered {status}', { url: e.url, status: r.status })
            : t('The test did not reach {url}: no answer', { url: e.url })
    })

  const drop = (e: Endpoint) =>
    void act(async () => {
      await remove(target, e.id)
      if (log?.id === e.id) setLog(null)
      list.reload()
      return t('{url} is deleted', { url: e.url })
    })

  const show = (e: Endpoint) => {
    if (log?.id === e.id) {
      setLog(null)
      return
    }
    void act(async () => {
      setLog({ id: e.id, rows: await deliveries(target, e.id) })
      return ''
    })
  }

  return (
    <YStack gap="$6">
      <Heading
        title={t('Notifications')}
        detail={
          host.org
            ? t('Webhooks: events in {org}, POSTed and signed to an address you run.', { org: host.org })
            : t('Webhooks: events in this organization, POSTed and signed to an address you run.')
        }
        action={
          adding || !host.admin ? undefined : (
            <Button size="sm" variant="outline" onPress={() => setAdding(true)}>
              <Plus size={14} /> {t('Add webhook')}
            </Button>
          )
        }
      />

      {adding ? (
        <YStack gap="$3" p="$4" borderWidth={1} borderColor="$borderColor" rounded="$3">
          <Field label={t('Endpoint URL')} hint={t('https only.')}>
            <Input value={url} onChangeText={setUrl} placeholder="https://example.com/hooks/hanzo" aria-label={t('Endpoint URL')} autoCapitalize="none" inputMode="url" />
          </Field>
          <Field label={t('Events')} hint={t('Subject patterns, comma separated, such as {example} — leave empty for every event.', { example: 'commerce.order.>' })}>
            <Input value={events} onChangeText={setEvents} placeholder="commerce.order.>, event.>" aria-label={t('Events')} autoCapitalize="none" />
          </Field>
          <Field label={t('Description')}>
            <Input value={description} onChangeText={setDescription} placeholder={t('What this endpoint is for')} aria-label={t('Description')} />
          </Field>
          <XStack gap="$2" justify="flex-end">
            <Button size="sm" variant="ghost" onPress={() => setAdding(false)}>
              {t('Cancel')}
            </Button>
            <Button size="sm" disabled={working || !url.trim()} onPress={create}>
              {t('Add webhook')}
            </Button>
          </XStack>
        </YStack>
      ) : null}

      {secret ? (
        <YStack gap="$2" p="$4" borderWidth={1} borderColor="$ink" rounded="$3">
          <SizableText size="$2" color="$ink">
            {t('Signing secret')}
          </SizableText>
          <SizableText size="$1" color="$soft">
            {t('Shown this once. Keep it where your endpoint checks each delivery’s signature.')}
          </SizableText>
          <XStack items="center" gap="$2">
            <SizableText flex={1} size="$2" color="$ink" fontFamily="$mono" $platform-web={{ wordBreak: 'break-all' }} aria-label={t('Signing secret')}>
              {secret}
            </SizableText>
            <Button size="sm" variant="outline" onPress={() => void copy(secret).then((ok) => ok && setNote(t('The secret is copied')))}>
              <Copy size={14} /> {t('Copy')}
            </Button>
            <XStack render="button" aria-label={t('Hide the secret')} p="$1" onPress={() => setSecret('')}>
              <X size={14} />
            </XStack>
          </XStack>
        </YStack>
      ) : null}

      <Group title={t('Webhooks')}>
        {list.error ? (
          <Soft>{told(list.error, t)}</Soft>
        ) : list.loading && !list.value.length ? (
          <Soft>{t('Reading webhooks…')}</Soft>
        ) : list.value.length === 0 ? (
          <Card>
            <Soft>{t('No webhooks yet.')}</Soft>
          </Card>
        ) : (
          <Card>
            {list.value.map((e, i) => (
              <YStack key={e.id} borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                <Row
                  first
                  mono
                  leading={<Webhook size={16} />}
                  title={e.url}
                  detail={[
                    e.description,
                    e.events.length ? e.events.join(', ') : t('every event'),
                    e.status === 'disabled' ? t('disabled') : '',
                    t('{delivered} delivered, {failed} failed this week', { delivered: e.deliveries, failed: e.failures }),
                  ]
                    .filter(Boolean)
                    .join(' · ')}
                  trailing={
                    host.admin ? (
                      <XStack render="button" aria-label={t('Delete {url}', { url: e.url })} p="$1" onPress={() => drop(e)}>
                        <X size={14} />
                      </XStack>
                    ) : undefined
                  }
                />
                <XStack gap="$2" px="$3" pb="$2.5" flexWrap="wrap">
                  {host.admin ? (
                    <Button size="sm" variant="outline" disabled={working} aria-label={t('Send a test to {url}', { url: e.url })} onPress={() => trial(e)}>
                      {t('Send test')}
                    </Button>
                  ) : null}
                  <Button size="sm" variant="ghost" disabled={working} aria-label={t('Deliveries to {url}', { url: e.url })} onPress={() => show(e)}>
                    {log?.id === e.id ? t('Hide deliveries') : t('Deliveries')}
                  </Button>
                </XStack>
                {log?.id === e.id ? (
                  <YStack px="$3" pb="$3" gap="$1.5">
                    {log.rows.length === 0 ? (
                      <SizableText size="$1" color="$soft">
                        {t('Nothing delivered yet.')}
                      </SizableText>
                    ) : (
                      log.rows.map((a, n) => (
                        <XStack key={`${a.at}-${n}`} gap="$3" items="baseline">
                          <SizableText size="$1" color={a.status === 'ok' ? '$ink' : '$soft'} width={64}>
                            {a.status === 'ok' ? t('OK') : a.status === 'failed' ? t('Failed') : t('Retrying')}
                          </SizableText>
                          <SizableText flex={1} size="$1" color="$soft" numberOfLines={1} style={mono}>
                            {`${a.subject}${a.code ? ` → ${a.code}` : ''}${a.error ? ` · ${a.error}` : ''}`}
                          </SizableText>
                          <SizableText size="$1" color="$soft" $max-sm={{ display: 'none' }}>
                            {day(a.at)}
                          </SizableText>
                        </XStack>
                      ))
                    )}
                  </YStack>
                ) : null}
              </YStack>
            ))}
          </Card>
        )}
      </Group>

      <Note>{note}</Note>
    </YStack>
  )
}
