/**
 * Settings: every setting the builder has, on this page — the account, the
 * workspace's environments, machines and keys, what the agent brings to a run,
 * and the organization's integrations, members, usage and notifications. A
 * section is its own address, so it can be linked to and comes back on reload.
 * Its words are the reader's language (@hanzo/i18n `t`).
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'

import { useHost } from '../host.tsx'
import { path, type Section } from '../route.ts'
import { heading } from './list.ts'
import { ENTRIES, GROUPS } from './sections.tsx'

export function Settings({ section }: { section: Section }) {
  const { t } = useI18n()
  const host = useHost()
  // Every section the route grammar names has an entry.
  const entry = ENTRIES.find((e) => e.id === section)!
  const go = (id: Section) => host.go(path({ kind: 'settings', section: id }))

  return (
    <XStack flex={1} minH={0} minW={0}>
      {/* A host whose sidebar lists the sections draws them there; the page is the section, full width. */}
      {host.sections ? null : (
      <YStack
        role="navigation"
        aria-label={t('Settings')}
        width={220}
        shrink={0}
        borderInlineEndWidth={1}
        borderColor="$borderColor"
        px="$2"
        py="$4"
        gap="$4"
        overflow="scroll"
        $max-md={{ display: 'none' }}
      >
        <SizableText size="$4" color="$ink" px="$2">
          {t('Settings')}
        </SizableText>
        {GROUPS.map((g) => {
          const list = ENTRIES.filter((e) => e.group === g)
          return (
            <YStack key={g} gap="$0.5">
              {/* The first group sits under the page's own "Settings" heading, and Code leads its own part: neither is labelled twice. */}
              {heading(g) ? (
                <SizableText size="$1" color="$soft" px="$2" pb="$1">
                  {t(heading(g))}
                </SizableText>
              ) : null}
              {list.map((e) => (
                <XStack
                  key={e.id}
                  render="button"
                  aria-label={t(e.label)}
                  aria-current={e.id === entry.id ? 'page' : undefined}
                  onPress={() => go(e.id)}
                  px="$2"
                  py="$1.5"
                  rounded="$2"
                  bg={e.id === entry.id ? '$hover' : 'transparent'}
                  hoverStyle={{ bg: '$hover' }}
                >
                  <SizableText size="$2" color={e.id === entry.id ? '$ink' : '$soft'}>
                    {t(e.label)}
                  </SizableText>
                </XStack>
              ))}
            </YStack>
          )
        })}
      </YStack>
      )}
      <YStack flex={1} minW={0} minH={0} overflow="scroll">
        <XStack flexWrap="wrap" gap="$1.5" px="$4" pt="$3" $md={{ display: 'none' }}>
          {ENTRIES.map((e) => (
            <XStack
              key={e.id}
              render="button"
              aria-label={t(e.label)}
              onPress={() => go(e.id)}
              px="$2.5"
              py="$1"
              rounded="$10"
              borderWidth={1}
              borderColor={e.id === entry.id ? '$ink' : '$borderColor'}
            >
              <SizableText size="$1" color={e.id === entry.id ? '$ink' : '$soft'}>
                {t(e.label)}
              </SizableText>
            </XStack>
          ))}
        </XStack>
        <YStack width="100%" maxW={760} self="center" px="$5" py="$6">
          {entry.body()}
        </YStack>
      </YStack>
    </XStack>
  )
}
