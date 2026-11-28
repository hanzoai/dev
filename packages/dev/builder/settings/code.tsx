/**
 * Code: where and how a new run starts — the mode and the place, saved to the
 * person's own settings for anyone whose New has no choice of its own kept yet
 * — and the model and effort, which are the one choice Chat and Dev share
 * (mind.tsx): changed here, changed in both composers. Beside them, the
 * codebases' environments and the organization's machines, each on its own page.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a model's and a
 * machine's names are drawn as given.
 */
import { YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { ChevronRight } from '@hanzogui/lucide-icons-2'
import { ChipSelect } from '@hanzo/ui/product'
import { ModelPicker } from '@hanzo/ui/models'
import { useState } from 'react'

import { told } from '../api/call.ts'
import { unhonoured } from '../api/coding.ts'
import { ENSO } from '../api/models.ts'
import { SANDBOX } from '../api/places.ts'
import type { Code as Defaults } from '../api/pref.ts'
import { usePlaces } from '../data.ts'
import { useHost, useTarget } from '../host.tsx'
import { MODES } from '../landing.tsx'
import { EFFORTS, useCatalog, useChoice } from '../mind.ts'
import { usePrefs } from '../prefs.tsx'
import { path } from '../route.ts'
import { said } from './machines.tsx'
import { Card, Group, Heading, Note, Row, Soft } from './ui.tsx'

export function Code() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const { prefs, save } = usePrefs()
  const catalog = useCatalog(target)
  const [choice, choose] = useChoice()
  const places = usePlaces(target, signed)
  const [note, setNote] = useState('')

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('Code')} />
        <Soft>{t('Sign in to set how your runs start.')}</Soft>
      </YStack>
    )
  }

  const now = { mode: 'build' as const, place: '', ...prefs.code }
  const keep = (change: Defaults) => {
    setNote('')
    save({ code: { ...prefs.code, ...change } }).catch((e: unknown) => setNote(told(e, t)))
  }

  const where = (places.value.length ? places.value : [SANDBOX]).map((p) => ({
    id: p.id || 'sandbox',
    label: p.id ? p.label : t('Default'),
    // A machine offline now can still be the default; its status shows beside it.
    hint: p.id ? [said(p.status, t), p.capacity].filter(Boolean).join(' · ') : t('The platform’s own sandbox'),
    place: p.id,
  }))
  // The sandbox is always listed, so a place not found is a machine that is not: it reads as its id.
  const place = where.find((w) => w.place === now.place) ?? { id: now.place, label: now.place, place: now.place }
  // A default is never premium: a premium model is picked for one run, in New.
  const offered = catalog.models.filter((m) => m.class !== 'premium')
  const go = (section: 'environments' | 'machines') => host.go(path({ kind: 'settings', section }))
  // The choices in the reader's words, kept by id.
  const efforts = EFFORTS.map((e) => ({ ...e, label: t(e.label) }))
  const modes = MODES.map((m) => ({ ...m, label: t(m.label), hint: t(m.hint) }))
  const effort = efforts.find((e) => e.id === choice.effort) ?? efforts[0]!
  const mode = modes.find((m) => m.id === now.mode)!

  return (
    <YStack gap="$6">
      <Heading title={t('Code')} detail={t('How a new run starts. The model and effort are the ones Chat uses too.')} />
      <Group title={t('Defaults')}>
        <Card>
          <Row
            first
            title={t('Model')}
            detail={t('Enso picks one for each step; any other model runs them all.')}
            trailing={
              <ModelPicker
                size="sm"
                name={t('Default model')}
                models={offered}
                scope="chat"
                value={choice.model || ENSO}
                onChange={(id) => choose({ model: id })}
                loading={catalog.loading}
                error={catalog.error}
              />
            }
          />
          <Row
            title={t('Effort')}
            detail={t('How long the agent thinks before it acts.')}
            trailing={
              <ChipSelect
                name={t('Default effort')}
                label={effort.label}
                chosen={effort}
                items={efforts}
                onChange={(e) => choose({ effort: e.id })}
                placement="bottom-end"
                width={180}
              />
            }
          />
          <Row
            title={t('Mode')}
            detail={mode.hint}
            trailing={
              <ChipSelect
                name={t('Default mode')}
                label={mode.label}
                chosen={mode}
                items={modes}
                onChange={(m) => keep({ mode: m.id })}
                placement="bottom-end"
                width={260}
              />
            }
          />
          <Row
            title={t('Where it runs')}
            detail={t('The Hanzo sandbox, or one of your organization’s machines.')}
            trailing={
              <ChipSelect
                name={t('Default place')}
                label={place.label}
                chosen={place}
                items={where}
                onChange={(w) => keep({ place: w.place })}
                placeholder={t('Search machines…')}
                placement="bottom-end"
                loading={places.loading}
                error={places.error ? told(places.error, t) : null}
              />
            }
          />
        </Card>
        <Note>{unhonoured(now.mode, now.place, t)}</Note>
      </Group>
      <Group title={t('Workspace')}>
        <Card>
          <Row
            first
            title={t('Environments')}
            detail={t('What a run installs and starts in each codebase, and the secrets it exports.')}
            trailing={<Link label={t('Open Environments')} onPress={() => go('environments')} />}
          />
          <Row
            title={t('Machines')}
            detail={t('Your organization’s own computers that take runs.')}
            trailing={<Link label={t('Open Machines')} onPress={() => go('machines')} />}
          />
        </Card>
      </Group>
      <Note>{note}</Note>
    </YStack>
  )
}

/** A row's way to its own page; `label` is what it opens, already said. */
function Link({ label, onPress }: { label: string; onPress: () => void }) {
  return (
    <YStack render="button" aria-label={label} p="$1" rounded="$2" hoverStyle={{ bg: '$hover' }} onPress={onPress}>
      <ChevronRight size={16} {...{ 'data-i18n-mirror': '' }} />
    </YStack>
  )
}
