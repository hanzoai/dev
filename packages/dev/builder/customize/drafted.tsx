/**
 * Make with AI: one box at the top of Browse. A name ("Marie Curie") or a
 * description ("a skeptical CFO") and a Hanzo model drafts the agent, which is
 * shown once, editable, with Save; saving makes the org's own agent and opens its
 * profile. A real historical figure gets a public-domain portrait from Wikimedia
 * Commons where one is free to use, any other agent the emoji the model chose.
 * Nothing is made before Save.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { AlertCircle, Sparkles, X } from '@hanzogui/lucide-icons-2'
import { Button, Input, Skeleton, Textarea } from '@hanzo/ui'
import { useState } from 'react'

import { RING } from '../pane.ts'
import { agents, CAST_CAP, CAST_TASK, create, EMPTY, free, handle, type Agent } from '../api/agents.ts'
import { portrait, saved, sketch, type Portrait, type Sketch } from '../api/draft.ts'
import { useTarget } from '../host.tsx'
import { say } from './say.ts'
import { Field, Mark, Sheet } from './ui.tsx'

/** How long a portrait is waited for before the draft is shown without one. */
const WAIT = 6000

function ErrorLine({ text }: { text: string }) {
  if (!text) return null
  return (
    <XStack items="center" gap="$1.5" role="alert">
      <AlertCircle size={14} color="var(--state-error)" aria-hidden />
      <SizableText size="$1" color="var(--state-error)" flex={1} minW={0}>
        {text}
      </SizableText>
    </XStack>
  )
}

function DraftSkeleton({ label }: { label: string }) {
  return (
    <XStack role="status" aria-busy={true} aria-label={label} items="flex-start" gap="$3" p="$3" rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$background">
      <Skeleton layout="block" width={40} height={40} rounded="$3" shrink={0} />
      <YStack flex={1} minW={0} gap="$1.5">
        <Skeleton layout="block" width="36%" height={13} rounded="$2" />
        <Skeleton layout="block" width="76%" height={11} rounded="$2" />
        <Skeleton layout="block" width="58%" height={11} rounded="$2" />
      </YStack>
    </XStack>
  )
}

export function Drafted({ onMade }: { onMade: (a: Agent) => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const [line, setLine] = useState('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState('')
  const [draft, setDraft] = useState<{ s: Sketch; face: Portrait | null } | null>(null)

  const make = async () => {
    if (!line.trim() || busy) return
    setBusy(true)
    setError('')
    try {
      const s = await sketch(target, line)
      const face = s.figure ? await portrait(s.figure, AbortSignal.timeout(WAIT)) : null
      setDraft({ s, face })
    } catch (e) {
      setError(say(e, t))
    } finally {
      setBusy(false)
    }
  }

  return (
    <YStack gap="$2.5" width="100%" minW={0}>
      <XStack width="100%" minW={0} gap="$2" items="center" $max-sm={{ flexDirection: 'column', items: 'stretch' }}>
        <XStack
          flex={1}
          minW={0}
          height={38}
          $max-sm={{ flexBasis: 'auto', grow: 0, width: '100%', minH: 44, height: 44 }}
          px="$3"
          gap="$2"
          items="center"
          rounded="$3"
          borderWidth={1}
          borderColor="var(--border-strong)"
          bg="$background"
          focusWithinStyle={RING}
        >
          <Sparkles size={14} color="$soft" shrink={0} aria-hidden />
          <Input
            unstyled
            flex={1}
            minW={0}
            height={36}
            p={0}
            borderWidth={0}
            bg="transparent"
            fontSize="$2"
            color="$ink"
            style={{ outlineStyle: 'none' }} // the wrapper draws the focus ring
            value={line}
            onChangeText={setLine}
            disabled={busy}
            placeholder={t('A name or a description: Marie Curie, a skeptical CFO…')}
            aria-label={t('Make with AI')}
            onKeyDown={(e: { key?: string }) => {
              if (e.key === 'Escape' && line && !busy) setLine('')
            }}
            onSubmitEditing={() => void make()}
          />
          {line && !busy ? (
            <XStack
              render="button"
              aria-label={t('Clear')}
              onPress={() => setLine('')}
              minW={24}
              minH={24}
              $touchable={{ minW: 44, minH: 44 }}
              $max-sm={{ minW: 44, minH: 44 }}
              items="center"
              justify="center"
              rounded="$2"
              borderWidth={0}
              bg="transparent"
              cursor="pointer"
              shrink={0}
              hoverStyle={{ bg: '$hover' }}
              focusVisibleStyle={RING}
            >
              <X size={13} color="$soft" aria-hidden />
            </XStack>
          ) : null}
        </XStack>
        <Button size="sm" variant="primary" shrink={0} $max-sm={{ width: '100%' }} disabled={!line.trim() || busy} onPress={() => void make()}>
          {busy ? t('Drafting…') : t('Make with AI')}
        </Button>
      </XStack>
      {busy ? <DraftSkeleton label={t('Drafting agent…')} /> : null}
      <ErrorLine text={error} />
      {draft ? <Review draft={draft} onClose={() => setDraft(null)} onMade={onMade} /> : null}
    </YStack>
  )
}

function Review({ draft, onClose, onMade }: { draft: { s: Sketch; face: Portrait | null }; onClose: () => void; onMade: (a: Agent) => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const [name, setName] = useState(draft.s.name)
  const [role, setRole] = useState(draft.s.role)
  const [instructions, setInstructions] = useState(draft.s.instructions)
  const [opening, setOpening] = useState(draft.s.opening)
  const [working, setWorking] = useState(false)
  const [error, setError] = useState('')

  const save = async () => {
    setWorking(true)
    setError('')
    try {
      const have = await agents(target)
      const made = await create(target, {
        ...EMPTY,
        name: free(handle(name), have),
        description: role.trim(),
        instructions: saved({ instructions, opening }),
        cap: CAST_CAP,
        task: CAST_TASK,
        avatar: draft.face?.url ?? '',
        emoji: draft.face ? '' : draft.s.emoji,
      })
      onMade(made)
    } catch (e) {
      setError(say(e, t))
      setWorking(false)
    }
  }

  return (
    <Sheet title={t('New agent')} open onOpenChange={(o) => !o && onClose()}>
      <XStack items="center" gap="$3">
        <Mark name={name || draft.s.name} size={48} logo={draft.face?.url} icon={!draft.face && draft.s.emoji ? <SizableText size="$8">{draft.s.emoji}</SizableText> : undefined} />
        <YStack flex={1} minW={0} gap="$1">
          <Input value={name} onChangeText={setName} aria-label={t('Name')} borderColor="var(--border-strong)" />
          {draft.face?.licence ? (
            <SizableText size="$1" color="$soft" numberOfLines={1}>
              {t('Portrait: {licence}', { licence: draft.face.licence })}
            </SizableText>
          ) : null}
        </YStack>
      </XStack>
      <Field label={t('Role')}>
        <Input value={role} onChangeText={setRole} aria-label={t('Role')} borderColor="var(--border-strong)" />
      </Field>
      <Field label={t('Instructions')}>
        <Textarea value={instructions} onChangeText={setInstructions} aria-label={t('Instructions')} rows={8} borderColor="var(--border-strong)" />
      </Field>
      <Field label={t('Opening line')}>
        <Input value={opening} onChangeText={setOpening} aria-label={t('Opening line')} borderColor="var(--border-strong)" />
      </Field>
      <ErrorLine text={error} />
      <XStack gap="$2" items="center" justify="flex-end">
        <Button size="sm" variant="ghost" disabled={working} onPress={onClose}>
          {t('Cancel')}
        </Button>
        <Button size="sm" variant="primary" disabled={working || !name.trim() || !instructions.trim()} onPress={() => void save()}>
          {working ? t('Saving…') : t('Save')}
        </Button>
      </XStack>
    </Sheet>
  )
}
