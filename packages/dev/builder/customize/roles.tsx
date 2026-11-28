/**
 * Skills for a person's work: pick what you do, see the skills that suit it, add them all in one
 * click or leave some out first. In Customize > Skills it is always there; as the first run's card
 * (`first`) it asks once, and the answer (`Prefs.work`, `Prefs.roles`) follows the account. Adding a
 * skill switches it on for the org, which is an org admin's to do; a member reads what is recommended.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Check } from '@hanzogui/lucide-icons-2'
import { Button } from '@hanzo/ui'
import { useEffect, useMemo, useState } from 'react'

import { active, brand, tool } from '../api/skills.ts'
import { toggle, type Tool } from '../api/tools.ts'
import { useHost, useHosted, useTarget } from '../host.tsx'
import { usePrefs, type PrefState } from '../prefs.tsx'
import { recommend, WORK } from '../roles.ts'
import { useLoad } from './load.ts'
import { say } from './say.ts'
import { FilterPills, Part } from './ui.tsx'

interface Notice {
  text: string
  error?: boolean
}

/** Drawn only where the builder's host and settings are mounted, so a chat drawn without them draws nothing. */
export function Roles({ first = false }: { first?: boolean }) {
  const hosted = useHosted()
  const state = usePrefs(false)
  return hosted && state ? <Suggest first={first} state={state} /> : null
}

function Suggest({ first, state }: { first: boolean; state: PrefState }) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const may = signed && host.admin
  const kept = state?.prefs.work ?? ''
  const [work, setWork] = useState(kept)
  const shelf = useLoad(signed ? () => brand(target) : null, null, [target.api, signed])
  const on = useLoad(signed ? () => active(target) : null, [] as Tool[], [target, signed])
  const [left, setLeft] = useState<ReadonlySet<string>>(new Set())
  const [busy, setBusy] = useState(false)
  const [notice, setNotice] = useState<Notice | null>(null)
  // The card stays up with its word once the skills are added, and goes on the next visit.
  const [said, setSaid] = useState(false)

  // The account's work arrives after the first frame; take it up unless the person has already chosen another.
  useEffect(() => {
    if (kept) setWork((now) => now || kept)
  }, [kept])
  useEffect(() => setLeft(new Set()), [work])

  const lit = useMemo(() => new Set(on.value.map((x) => x.name)), [on.value])
  const suited = useMemo(() => recommend(work, shelf.value), [work, shelf.value])
  const fresh = suited.filter((e) => !lit.has(tool(e.name)))
  const taken = fresh.filter((e) => !left.has(e.name))

  // The first run's card: only a signed-in admin who has said nothing about their work and not put this off.
  if (!signed) return null
  if (first && !said && (!may || kept || state.prefs.roles || shelf.error)) return null

  const choose = (id: string) => {
    setWork(id)
    setNotice(null)
  }

  const add = async () => {
    setBusy(true)
    setNotice(null)
    try {
      await toggle(target, taken.map((e) => tool(e.name)))
      await state.save({ work, roles: 'done' })
      on.reload()
      setSaid(true)
      setNotice({ text: t('{n} skills are on', { n: taken.length }) })
    } catch (e) {
      setNotice({ text: say(e, t), error: true })
    } finally {
      setBusy(false)
    }
  }

  const later = async () => {
    try {
      await state.save({ roles: 'skipped' })
    } catch (e) {
      setNotice({ text: say(e, t), error: true })
    }
  }

  // Put off, the card is gone; added, it is gone from the next visit.
  if (first && state.prefs.roles === 'skipped') return null

  return (
    <Part
      title={first ? t('What do you do?') : t('Recommended for your work')}
      detail={t('Pick your work and Hanzo suggests the skills that suit it. Add them all in one click, or leave some out first.')}
    >
      <YStack gap="$3" data-slot="roles" data-first={first || undefined}>
        <FilterPills label={t('Your work')} value={work} options={WORK.map((w) => ({ id: w.id, label: t(w.label) }))} onChange={choose} maxVisibleDesktop={11} mobileLimit={5} />
        {work && !suited.length ? (
          <SizableText size="$2" color="$soft" data-slot="roles-none">
            {t('Nothing is recommended for this yet. Browse the skills to add any you like.')}
          </SizableText>
        ) : null}
        {suited.length ? (
          <YStack gap="$1.5" role="group" aria-label={t('Recommended skills')}>
            {suited.map((e) => {
              const isOn = lit.has(tool(e.name))
              const picked = !isOn && !left.has(e.name)
              return (
                <XStack
                  key={e.name}
                  render="button"
                  role="checkbox"
                  aria-checked={isOn || picked}
                  aria-disabled={isOn || !may || undefined}
                  data-slot="roles-skill"
                  data-skill={e.name}
                  onClick={() => {
                    if (isOn || !may) return
                    setLeft((s) => {
                      const next = new Set(s)
                      if (next.has(e.name)) next.delete(e.name)
                      else next.add(e.name)
                      return next
                    })
                  }}
                  items="flex-start"
                  gap="$2.5"
                  p="$2.5"
                  rounded="$3"
                  borderWidth={1}
                  borderColor={picked ? 'var(--border-strong)' : '$borderColor'}
                  bg={picked ? '$panel' : 'transparent'}
                  cursor={isOn || !may ? 'default' : 'pointer'}
                  $max-sm={{ minH: 44 }}
                >
                  <XStack width={18} height={18} rounded={5} borderWidth={1} borderColor="var(--border-strong)" items="center" justify="center" bg={isOn || picked ? '$ink' : 'transparent'} shrink={0} mt={1}>
                    {isOn || picked ? <Check size={12} color="var(--background)" aria-hidden /> : null}
                  </XStack>
                  <YStack flex={1} minW={0} gap="$0.5" items="flex-start">
                    <SizableText size="$2" fontWeight="600" color="$ink" numberOfLines={1}>
                      {e.name}
                    </SizableText>
                    <SizableText size="$1" color="$soft" numberOfLines={2} text="start">
                      {e.description}
                    </SizableText>
                  </YStack>
                  {isOn ? (
                    <SizableText size="$1" color="$soft" shrink={0}>
                      {t('Added')}
                    </SizableText>
                  ) : null}
                </XStack>
              )
            })}
          </YStack>
        ) : null}
        {suited.length && !may ? (
          <SizableText size="$2" color="$soft">
            {t('An org admin adds skills. Ask them to add these.')}
          </SizableText>
        ) : null}
        {notice ? (
          <SizableText size="$2" color={notice.error ? 'var(--state-error)' : '$soft'} role={notice.error ? 'alert' : 'status'}>
            {notice.text}
          </SizableText>
        ) : null}
        <XStack gap="$2" items="center" flexWrap="wrap">
          {suited.length ? (
            <Button data-slot="roles-add" size="sm" disabled={!may || busy || !taken.length} onPress={() => void add()}>
              {taken.length ? t('Add {n} skills', { n: taken.length }) : t('All added')}
            </Button>
          ) : null}
          {first ? (
            <Button data-slot="roles-later" size="sm" variant="outline" borderColor="var(--border-strong)" disabled={busy} onPress={() => void later()}>
              {t('Not now')}
            </Button>
          ) : null}
        </XStack>
      </YStack>
    </Part>
  )
}
