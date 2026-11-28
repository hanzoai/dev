/**
 * General: how the builder looks and listens — the theme, the text size, the
 * motion, and the language dictation hears. Each choice is saved to the
 * person's own settings as it is made, so it follows them to every device.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a language is named
 * in its own words, as given.
 */
import { YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { ChipSelect } from '@hanzo/ui/product'
import { useState } from 'react'

import { told } from '../api/call.ts'
import type { Motion, Patch, Text, Theme } from '../api/pref.ts'
import { useHost } from '../host.tsx'
import { usePrefs } from '../prefs.tsx'
import { LANGUAGES } from '@hanzo/ui/chat/pure'
import { spoken } from '../voice.ts'
import { Card, Group, Heading, Note, Row, Soft } from './ui.tsx'

/** The choices, each word English and marked (`phrase`): drawn through `t`. */
const THEMES: { id: Theme; label: string }[] = [
  { id: 'system', label: phrase('System') },
  { id: 'light', label: phrase('Light') },
  { id: 'dark', label: phrase('Dark') },
]

const TEXTS: { id: Text; label: string }[] = [
  { id: 'small', label: phrase('Small') },
  { id: 'medium', label: phrase('Medium') },
  { id: 'large', label: phrase('Large') },
]

const MOTIONS: { id: Motion; label: string; hint: string }[] = [
  { id: 'system', label: phrase('System'), hint: phrase('As this device is set') },
  { id: 'reduced', label: phrase('Reduced'), hint: phrase('No animation anywhere') },
]

export function General() {
  const { t } = useI18n()
  const host = useHost()
  const { prefs, save } = usePrefs()
  const [note, setNote] = useState('')

  if (!host.person) {
    return (
      <YStack gap="$5">
        <Heading title={t('General')} />
        <Soft>{t('Sign in to see your settings.')}</Soft>
      </YStack>
    )
  }

  const keep = (patch: Patch) => {
    setNote('')
    save(patch).catch((e: unknown) => setNote(told(e, t)))
  }

  // The choices in the reader's words, kept by id.
  const themes = THEMES.map((x) => ({ ...x, label: t(x.label) }))
  const texts = TEXTS.map((x) => ({ ...x, label: t(x.label) }))
  const motions = MOTIONS.map((x) => ({ ...x, label: t(x.label), hint: t(x.hint) }))
  // Each choice is one of its list: a saved one was read against it, and a host's theme is one of THEMES.
  const theme = themes.find((x) => x.id === (prefs.theme ?? host.theme ?? 'dark'))!
  const text = texts.find((x) => x.id === (prefs.text ?? 'medium'))!
  const motion = motions.find((x) => x.id === (prefs.motion ?? 'system'))!
  const language = LANGUAGES.find((l) => l.id === prefs.language) ?? LANGUAGES.find((l) => l.id === spoken())!

  return (
    <YStack gap="$6">
      <Heading title={t('General')} detail={t('How Hanzo looks and listens. Saved to your account, so it follows you.')} />
      <Group title={t('Appearance')}>
        <Card>
          {host.chooseTheme ? (
            <Row
              first
              title={t('Theme')}
              detail={t('System follows this device’s light or dark setting.')}
              trailing={
                <ChipSelect
                  name={t('Theme')}
                  label={theme.label}
                  chosen={theme}
                  items={themes}
                  onChange={(x) => keep({ theme: x.id })}
                  placement="bottom-end"
                  width={200}
                />
              }
            />
          ) : null}
          <Row
            first={!host.chooseTheme}
            title={t('Text size')}
            detail={t('The transcript, the composer and every screen.')}
            trailing={
              <ChipSelect
                name={t('Text size')}
                label={text.label}
                chosen={text}
                items={texts}
                onChange={(x) => keep({ text: x.id === 'medium' ? null : x.id })}
                placement="bottom-end"
                width={200}
              />
            }
          />
          <Row
            title={t('Motion')}
            detail={t('Reduced stops animations and transitions on this page.')}
            trailing={
              <ChipSelect
                name={t('Motion')}
                label={motion.label}
                chosen={motion}
                items={motions}
                onChange={(x) => keep({ motion: x.id === 'system' ? null : x.id })}
                placement="bottom-end"
                width={220}
              />
            }
          />
        </Card>
      </Group>
      <Group title={t('Voice')}>
        <Card>
          <Row
            first
            title={t('Dictation language')}
            detail={t('What the microphone in the composer listens for.')}
            trailing={
              <ChipSelect
                name={t('Dictation language')}
                label={language.label}
                chosen={language}
                items={LANGUAGES}
                onChange={(l) => keep({ language: l.id })}
                placeholder={t('Search languages…')}
                placement="bottom-end"
                width={220}
              />
            }
          />
        </Card>
      </Group>
      <Note>{note}</Note>
    </YStack>
  )
}
