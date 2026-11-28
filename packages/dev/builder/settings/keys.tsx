/**
 * API keys: your keys for calling the Hanzo API from code, as many as you need —
 * a secret key for a server, a publishable key for a page's source. Each is its
 * own credential: creating one never touches another, and revoking one stops only
 * that one. A secret key is shown once, when it is made. A limit narrows what a
 * key reaches; it never reaches further than you do.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a key's name, its
 * prefix and its kind:name limits are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { Button, Dialog, DialogContent, DialogTitle, Input } from '@hanzo/ui'
import { CopyButton } from '@hanzo/ui/product'
import { useState } from 'react'

import { told } from '../api/call.ts'
import { keys, KINDS, limits, mint, revoke, type Key, type Kind, type Minted } from '../api/keys.ts'
import { Confirm } from '../ask.tsx'
import { useRead } from '../data.ts'
import { useHost, useTarget } from '../host.tsx'
import { Card, Field, Heading, Note, Once, Row, Soft } from './ui.tsx'

/** Each kind's words, whole, so a language says the kind its own way: its name, its create control and label, and the dialog's titles. */
const NAME: Record<Kind, string> = { secret: phrase('Secret key'), publishable: phrase('Publishable key') }
const CREATE: Record<Kind, string> = { secret: phrase('Create secret key'), publishable: phrase('Create publishable key') }
const CREATE_A: Record<Kind, string> = { secret: phrase('Create a secret key'), publishable: phrase('Create a publishable key') }
const NEW: Record<Kind, string> = { secret: phrase('Your new secret key'), publishable: phrase('Your new publishable key') }

/** The statuses IAM names besides active, as words; one it does not is drawn as given. */
const STATUS: Record<string, string> = { expired: phrase('Expired'), revoked: phrase('Revoked'), disabled: phrase('Disabled') }

/** The line under a key: its head, what it reaches, when it was made, and where it stands. */
function about(k: Key, t: Say): string {
  const when = k.created ? new Date(k.created) : null
  return [
    k.type === 'secret' && k.prefix ? `${k.prefix}…` : '',
    k.type === 'publishable' ? t('Publishable') : '',
    k.limit.length ? t('Reaches {limits}', { limits: k.limit.join(', ') }) : t('Unrestricted'),
    when && !Number.isNaN(when.getTime()) ? t('Created {date}', { date: when.toLocaleDateString(undefined, { month: 'short', day: 'numeric', year: 'numeric' }) }) : '',
    k.status !== 'active' ? (STATUS[k.status] ? t(STATUS[k.status]!) : k.status[0].toUpperCase() + k.status.slice(1)) : '',
  ]
    .filter(Boolean)
    .join(' · ')
}

export function Keys() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const list = useRead(signed ? () => keys(target) : null, [] as Key[], [target, signed])
  const [making, setMaking] = useState<Kind | null>(null)
  const [revoking, setRevoking] = useState<Key | null>(null)
  const [note, setNote] = useState('')

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('API keys')} />
        <Soft>{t('Sign in to see your API keys.')}</Soft>
      </YStack>
    )
  }

  return (
    <YStack gap="$5">
      <Heading title={t('API keys')} detail={t('Your keys for calling {api} from code. Make one for each app; revoking one stops only that one.', { api: 'api.hanzo.ai' })} />
      <XStack gap="$2">
        {KINDS.map((type) => (
          <Button key={type} size="sm" variant="outline" aria-label={t(CREATE_A[type])} onPress={() => setMaking(type)}>
            {t(CREATE[type])}
          </Button>
        ))}
      </XStack>
      {list.error ? (
        <Soft>{told(list.error, t)}</Soft>
      ) : list.loading && list.value.length === 0 ? (
        <Soft>{t('Reading your keys…')}</Soft>
      ) : list.value.length === 0 ? (
        <Soft>{t('No keys yet.')}</Soft>
      ) : (
        <Card>
          {list.value.map((k, i) => (
            <YStack key={k.id} borderTopWidth={i ? 1 : 0} borderColor="$borderColor" opacity={k.status === 'active' ? 1 : 0.55}>
              <Row
                first
                title={k.name || t(NAME[k.type])}
                detail={about(k, t)}
                trailing={
                  k.status === 'revoked' ? null : (
                    <Button size="sm" variant="ghost" aria-label={t('Revoke {name}', { name: k.name || k.id })} onPress={() => setRevoking(k)}>
                      {t('Revoke')}
                    </Button>
                  )
                }
              />
              {k.type === 'publishable' && k.key ? (
                <XStack items="center" gap="$2" px="$3" pb="$2.5">
                  <SizableText
                    flex={1}
                    minW={0}
                    size="$1"
                    color="$soft"
                    numberOfLines={1}
                    aria-label={t('Publishable key')}
                    style={{ fontFamily: 'var(--f-mono, ui-monospace, monospace)' }}
                  >
                    {k.key}
                  </SizableText>
                  <CopyButton value={k.key} label={t('Copy publishable key')} />
                </XStack>
              ) : null}
            </YStack>
          ))}
        </Card>
      )}
      <Note>{note}</Note>

      <Making
        type={making}
        onClose={() => setMaking(null)}
        onMade={(m) => {
          setNote(t('{name} created', { name: m.name || t(NAME[m.type]) }))
          list.reload()
        }}
      />
      <Confirm
        open={revoking !== null}
        onOpenChange={(o) => !o && setRevoking(null)}
        title={revoking?.name ? t('Revoke {name}?', { name: revoking.name }) : t('Revoke this key?')}
        says={t('It stops working within a minute, and anything using it is refused. Your other keys keep working.')}
        act={t('Revoke')}
        // It acts only while it is open, which is while a key is chosen.
        run={async () => {
          await revoke(target, revoking!.id)
          setNote(t('{name} revoked', { name: revoking!.name || t(NAME[revoking!.type]) }))
          list.reload()
        }}
      />
    </YStack>
  )
}

/** Creating one key: a name and an optional limit, then the key itself, once. */
function Making({ type, onClose, onMade }: { type: Kind | null; onClose: () => void; onMade: (m: Minted) => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const [name, setName] = useState('')
  const [limit, setLimit] = useState('')
  const [made, setMade] = useState<Minted | null>(null)
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')

  const close = () => {
    if (working) return
    setName('')
    setLimit('')
    setMade(null)
    setNote('')
    onClose()
  }

  // Pressed only in the open dialog, which is open only for a type.
  const go = async (type: Kind) => {
    setNote('')
    let list: string[]
    try {
      list = limits(limit)
    } catch (e) {
      setNote(told(e, t))
      return
    }
    setWorking(true)
    try {
      const m = await mint(target, type, name, list)
      setMade(m)
      onMade(m)
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setWorking(false)
    }
  }

  return (
    <Dialog open={type !== null} onOpenChange={(o) => !o && close()}>
      <DialogContent maxW={480} showCloseButton={false}>
        <DialogTitle>{made ? t(NEW[made.type]) : type ? t(CREATE_A[type]) : ''}</DialogTitle>
        {made ? (
          <YStack gap="$3">
            {made.type === 'secret' ? (
              <Once value={made.key} label={t('Secret key')} copy={t('Copy secret key')} />
            ) : (
              <XStack items="center" gap="$2" px="$3" py="$3" rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$raised">
                <SizableText flex={1} minW={0} size="$2" color="$ink" style={{ fontFamily: 'var(--f-mono, ui-monospace, monospace)', wordBreak: 'break-all' }}>
                  {made.key}
                </SizableText>
                <CopyButton value={made.key} label={t('Copy publishable key')} />
              </XStack>
            )}
            {made.limit.length ? (
              <SizableText size="$1" color="$soft">
                {t('It reaches {limits} and nothing else.', { limits: made.limit.join(', ') })}
              </SizableText>
            ) : null}
            <XStack justify="flex-end">
              <Button size="sm" onPress={close}>
                {t('Done')}
              </Button>
            </XStack>
          </YStack>
        ) : (
          <YStack gap="$3">
            <Field label={t('Name')} hint={t('What it is for, so you can tell it from your other keys.')}>
              <Input value={name} onChangeText={setName} placeholder={t('production')} aria-label={t('Name')} />
            </Field>
            <Field
              label={t('Limit')}
              hint={t('Optional. What it may reach, as {form} — {examples}, {read} for read-only. Empty reaches whatever you do.', {
                form: 'kind:name',
                examples: 'model:zen5, project:acme, product:train',
                read: 'read:*',
              })}
            >
              <Input value={limit} onChangeText={setLimit} placeholder="model:zen5, project:acme" aria-label={t('Limit')} autoCapitalize="none" />
            </Field>
            {note ? (
              <SizableText size="$1" color="$soft" role="status">
                {note}
              </SizableText>
            ) : null}
            <XStack gap="$2" justify="flex-end">
              <Button size="sm" variant="ghost" disabled={working} onPress={close}>
                {t('Cancel')}
              </Button>
              <Button size="sm" disabled={working} onPress={() => void go(type!)}>
                {t('Create')}
              </Button>
            </XStack>
          </YStack>
        )}
      </DialogContent>
    </Dialog>
  )
}
