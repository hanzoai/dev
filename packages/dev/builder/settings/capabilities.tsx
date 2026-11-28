/**
 * Capabilities: what the agent may use, by where it comes from — connector
 * actions, functions, services, agents, skills and the organization's own MCP
 * servers. A tool that is off is still listed to the agent's discovery but is
 * refused when called. Switching a kind on turns on every tool of that kind;
 * off turns them all off. It is the organization's setting, not only yours.
 *
 * Its words are the reader's language (@hanzo/i18n `t`).
 */
import { SizableText, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { Switch } from '@hanzo/ui'
import { useState } from 'react'

import { told } from '../api/call.ts'
import { activate, SOURCES, tools, type Source, type Tool } from '../api/capabilities.ts'
import { useRead } from '../data.ts'
import { useHost, useTarget } from '../host.tsx'
import { Card, Group, Heading, Note, Row, Soft } from './ui.tsx'

/** Each kind's name and the sentence that says what it is, English and marked (`phrase`), drawn through `t`. */
const KINDS: Record<Source, { label: string; about: string }> = {
  connector: { label: phrase('Connector actions'), about: phrase('Actions in the apps your organization connected.') },
  function: { label: phrase('Functions'), about: phrase('Functions your organization wrote.') },
  'zap-service': { label: phrase('Services'), about: phrase('Routes of the services your organization runs.') },
  agent: { label: phrase('Agents'), about: phrase('Your organization’s agents, called as tools.') },
  skill: { label: phrase('Skills'), about: phrase('Skills attached to agents.') },
  mcp: { label: phrase('MCP servers'), about: phrase('Tools on the MCP servers your organization added.') },
}

export function Capabilities() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const list = useRead(signed ? () => tools(target) : null, [] as Tool[], [target, signed])
  const [busy, setBusy] = useState<Source | ''>('')
  const [note, setNote] = useState('')

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('Capabilities')} />
        <Soft>{t('Sign in to see what the agent may use.')}</Soft>
      </YStack>
    )
  }

  const kinds = SOURCES.map((s) => ({ source: s, all: list.value.filter((x) => x.source === s) })).filter((k) => k.all.length)

  const flip = async (source: Source, all: Tool[], on: boolean) => {
    setBusy(source)
    setNote('')
    try {
      await activate(target, all.map((x) => x.name), on)
      list.reload()
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setBusy('')
    }
  }

  return (
    <YStack gap="$6">
      <Heading
        title={t('Capabilities')}
        detail={
          host.org
            ? t('What the agent may use in {org}. A tool that is off is refused when the agent calls it.', { org: host.org })
            : t('What the agent may use in this organization. A tool that is off is refused when the agent calls it.')
        }
      />
      <Group
        title={t('Tools')}
        detail={
          host.admin
            ? t('Switching a kind on or off applies to every tool of that kind, for everyone in the organization.')
            : t('An org admin switches these, for everyone in the organization.')
        }
      >
        {list.error ? (
          <Soft>{told(list.error, t)}</Soft>
        ) : list.loading && !list.value.length ? (
          <Soft>{t('Reading tools…')}</Soft>
        ) : !kinds.length ? (
          <Soft>{t('No tools are available to this organization yet.')}</Soft>
        ) : (
          <Card>
            {kinds.map(({ source, all }, i) => {
              const on = all.filter((x) => x.activated).length
              // What the kind is, then how much of it is on: two sentences.
              const count = on === all.length ? t('All {n} on.', { n: all.length }) : on ? t('{on} of {n} on.', { on, n: all.length }) : t('{n} available, all off.', { n: all.length })
              return (
                <Row
                  key={source}
                  first={i === 0}
                  title={t(KINDS[source].label)}
                  detail={`${t(KINDS[source].about)} ${count}`}
                  trailing={
                    host.admin ? (
                      <Switch
                        aria-label={t(KINDS[source].label)}
                        checked={on === all.length}
                        disabled={busy !== ''}
                        onCheckedChange={(v: boolean) => void flip(source, all, v)}
                      />
                    ) : (
                      <SizableText size="$1" color="$soft">
                        {on === all.length ? t('On') : on ? t('Partly on') : t('Off')}
                      </SizableText>
                    )
                  }
                />
              )
            })}
          </Card>
        )}
      </Group>
      <Note>{note}</Note>
    </YStack>
  )
}
