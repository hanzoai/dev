/**
 * A conversation in Dev (api/talk.ts): what the person said and what the agent
 * answered, at one address however many turns it takes — the way Chat reads.
 *
 * Every send continues the same session. While a turn works, what is said is that
 * turn's next input; once it has answered, the next turn. A build is never steered
 * from here — the words are the conversation's, and a build is steered from its
 * own page, which its card opens. The agent decides what a turn needs: an answer,
 * a sandbox (leased on first use and kept for the conversation), or a coding run,
 * whose outcome is said here when it ends. A question a turn asks — in Manual,
 * whether a build may start — is answered on its card. The line under the
 * transcript is what the turn is doing now, from its own progress. The mode pill
 * is the conversation's: Plan, Auto or Manual, kept from the turn it is sent with.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Steer } from '@hanzo/ui/agents'
import { Composer, measure } from '@hanzo/ui/chat'
import { useLimits } from '@hanzo/ui/product/useLimits'
import { useMemo, useState } from 'react'

import { DEV_MODES, MODES, type Mode } from './gear.ts'

import { Refusal } from './api/call.ts'
import { limits as readLimits } from './api/models.ts'
import { log, message, stop, type Event } from './api/sessions.ts'
import { answer, now, say as talk } from './api/talk.ts'
import { cards, merge, outcome } from './api/turn.ts'
import { verdict } from './api/verdict.ts'
import { useRead, type RunState } from './data.ts'
import { useHost, useTarget } from './host.tsx'
import { level, useMind } from './mind.ts'
import { pane } from './pane.ts'
import { speaker } from './reply.tsx'
import { path } from './route.ts'
import { cleanErrorText } from './thread.ts'
import { carrying, compose, pick, type Attached } from './tools.tsx'
import { Transcript } from './transcript.tsx'

export function Talk({ id, state }: { id: string; state: RunState }) {
  const { t: say } = useI18n()
  const host = useHost()
  const t = useTarget()
  const signed = Boolean(host.person)
  const { detail, record, status, refused } = state
  // The whole conversation from its first word: the detail carries the latest
  // fifty events, and a turn that builds says many. Live frames are added to it.
  const whole = useRead(signed ? () => log(t, id) : null, [] as Event[], [t, id, signed])
  const events = useMemo(() => merge(whole.value, state.events), [whole.value, state.events])
  const [draft, setDraft] = useState('')
  const [files, setFiles] = useState<Attached[]>([])
  const [note, setNote] = useState('')
  const [busy, setBusy] = useState(false)
  const { limits } = useLimits(signed ? (signal) => readLimits(t, signal) : null, t)
  const mind = useMind(t, (limits as { plan?: string } | null)?.plan, host.topUp)
  const listen = useMemo(() => speaker(t), [t])
  const shown = useMemo(() => cards(events, '', say), [events, say])
  const terminalError = useMemo(() => outcome(events).status === 'error', [events])
  const working = status === 'running' && !terminalError
  const addFunds = () => (host.topUp ? host.topUp() : host.go(path({ kind: 'settings', section: 'billing' })))
  const doing = useMemo(() => now(events), [events])
  // The conversation's mode, as it keeps it until the person picks another: what
  // they pick is sent with what they say next, and kept for every turn after.
  const [picked, setPicked] = useState<Mode | null>(null)
  const mode: Mode = picked ?? (DEV_MODES.includes(record?.mode as Mode) ? (record?.mode as Mode) : 'auto')

  /** One act at a time; what it answers, or the reason it was refused, is the note. */
  const act = async (work: () => Promise<string>) => {
    if (busy) return
    setBusy(true)
    setNote('')
    try {
      setNote(await work())
    } catch (e) {
      setNote(cleanErrorText((e as Error).message))
    } finally {
      setBusy(false)
    }
  }

  const send = () => {
    const words = compose(draft.trim(), files)
    if (!words) return
    const next = () => talk(t, { session: id, input: words, model: mind.model, effort: level(mind.effort), ...(picked ? { mode: picked } : {}) })
    void act(async () => {
      if (!working) await next()
      else
        try {
          await message(t, id, words)
        } catch (e) {
          // The turn answered as the words were sent: they are the next turn.
          if (!(e instanceof Refusal) || e.status !== 409) throw e
          await next()
        }
      setDraft('')
      setFiles([])
      return ''
    })
  }

  const empty = (
    <SizableText size="$2" color="$soft">
      {!signed ? say('Sign in to follow this conversation.') : detail.error ? say(detail.error.message) : detail.loading ? say('Reading this conversation…') : say('Thinking…')}
    </SizableText>
  )

  return (
    <YStack data-slot="talk" {...pane} display="grid" gridTemplateRows="auto minmax(0, 1fr) auto" width="100%" height="100%" minH={0} minW={0}>
      <XStack
        px="$4"
        pt="$3"
        pb="$2"
        items="center"
        minH={44}
        style={{ paddingInlineEnd: 'calc(16px + var(--hanzo-shell-top-right-inset, 0px))' }}
      >
        <SizableText size="$4" color="$ink" numberOfLines={1}>
          {record?.title || say('New conversation')}
        </SizableText>
      </XStack>
      <Transcript
        cards={shown}
        live={working}
        empty={empty}
        listen={signed ? listen : undefined}
        onVerdict={(v) => verdict(t, id, '', v)}
        onRun={(session) => host.go(session)}
        onAnswer={(question, label) => void act(async () => (await answer(t, id, question, label, true), ''))}
        onAddFunds={addFunds}
      />
      <YStack pb="$3" pt="$2" gap="$2">
        <YStack {...measure} gap="$2">
          {note || (signed && refused) ? (
            <SizableText size="$1" color="$soft" role="status">
              {note || refused}
            </SizableText>
          ) : null}
          {working ? (
            <XStack items="center" gap="$2" px="$3" py="$2" rounded="$10" borderWidth={1} borderColor="$borderColor">
              <SizableText size="$2" color="$soft" flex={1} minW={0} numberOfLines={1} role="status">
                {doing || say('Thinking…')}
              </SizableText>
              <Steer
                onCommand={(c) => {
                  if (c === 'stop') void act(async () => (await stop(t, id), say('Stopped')))
                }}
                withhold={['pause', 'resume', ...(busy ? (['stop'] as const) : [])]}
              />
            </XStack>
          ) : null}
        </YStack>
        <Composer
          column
          value={draft}
          onChange={setDraft}
          onSend={send}
          onStop={working ? () => void act(async () => (await stop(t, id), say('Stopped'))) : undefined}
          busy={working && !draft.trim()}
          ready={Boolean(draft.trim())}
          disabled={!signed || busy || !record}
          placeholder={!signed ? say('Sign in to reply') : working ? say('Add to what it is doing') : say('Reply')}
          label={say('Reply to this conversation')}
          plus={{ upload: () => pick(files, setFiles, setNote, say) }}
          carried={carrying(files, setFiles)}
          mind={signed && !working ? mind : null}
          mode={signed && !working ? { value: mode, set: (m) => setPicked(m as Mode), items: MODES.filter((m) => DEV_MODES.includes(m.id)) } : null}
        />
      </YStack>
    </YStack>
  )
}
