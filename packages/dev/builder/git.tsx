/**
 * A run's Git tab: what it pushed, read from the forge. Diff is the net change
 * from its base, Review is the pull request it opened and what reviewers said,
 * with Merge while it is open, Commits is its branch's history since the base.
 * While the run works it is read again every little while, because a run
 * pushes when it finishes a step.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); branches, paths,
 * commit messages, a pull request's title and its reviews are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { ChevronDown, ChevronRight } from '@hanzogui/lucide-icons-2'
import { Button } from '@hanzo/ui'
import { useEffect, useState } from 'react'

import { merge, read, type Change, type Changes } from './api/changes.ts'
import { useRead } from './data.ts'
import { useHost, useTarget } from './host.tsx'
import { Out } from './out.tsx'

type View = 'diff' | 'review' | 'commits'

const NONE: Changes = { repo: '', base: '', head: '', commits: [], files: [], pull: null }
const AGAIN = 15_000
const mono = { fontFamily: 'var(--f-mono, ui-monospace, monospace)', whiteSpace: 'pre' as const }

export function Git({ session, title, live }: { session: string; title: string; live: boolean }) {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const signed = Boolean(host.person)
  const [view, setView] = useState<View>('diff')
  const got = useRead(signed ? () => read(target, session) : null, NONE, [target, session, signed, live])
  const { reload } = got

  useEffect(() => {
    if (!live) return
    const every = setInterval(reload, AGAIN)
    return () => clearInterval(every)
  }, [live, reload])

  const c = got.value
  const added = c.files.reduce((n, f) => n + f.additions, 0)
  const removed = c.files.reduce((n, f) => n + f.deletions, 0)

  return (
    <YStack gap="$3">
      <YStack gap="$1">
        <SizableText size="$3" color="$ink" numberOfLines={1}>
          {title}
        </SizableText>
        {c.head ? (
          <SizableText size="$1" color="$soft" numberOfLines={1}>
            {`${c.head} → ${c.base}`}
            {c.files.length ? ` · ${c.files.length === 1 ? t('1 file') : t('{n} files', { n: c.files.length })} +${added} −${removed}` : ''}
          </SizableText>
        ) : null}
      </YStack>
      <XStack gap="$1">
        {(['diff', 'review', 'commits'] as const).map((v) => (
          <XStack
            key={v}
            render="button"
            aria-label={t(label(v))}
            onPress={() => setView(v)}
            px="$2.5"
            py="$1"
            rounded="$2"
            bg={view === v ? '$hover' : 'transparent'}
          >
            <SizableText size="$2" color={view === v ? '$ink' : '$soft'}>
              {t(label(v))}
            </SizableText>
          </XStack>
        ))}
      </XStack>
      {!signed ? (
        <Soft>{t('Sign in to see what this run pushed.')}</Soft>
      ) : got.error ? (
        <Soft>{got.error.message}</Soft>
      ) : got.loading && !c.head ? (
        <Soft>{t('Reading the branch…')}</Soft>
      ) : view === 'diff' ? (
        c.files.length ? (
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" overflow="hidden">
            {c.files.map((f, i) => (
              <File key={f.path} change={f} first={i === 0} />
            ))}
          </YStack>
        ) : (
          <Soft>{t('No pushed changes')}</Soft>
        )
      ) : view === 'review' ? (
        c.pull ? (
          <YStack gap="$3">
            <YStack gap="$1.5" p="$3" borderWidth={1} borderColor="$borderColor" rounded="$3">
              <SizableText size="$2" color="$ink">
                {`#${c.pull.number} ${c.pull.title}`}
              </SizableText>
              <SizableText size="$1" color="$soft">
                {c.pull.state === 'open'
                  ? c.pull.mergeable === false
                    ? t('Open · conflicts with its base')
                    : c.pull.mergeable
                      ? t('Open · ready to merge')
                      : t('Open')
                  : c.pull.state === 'merged'
                    ? t('Merged')
                    : t('Closed')}
              </SizableText>
              {c.pull.url.startsWith('https://') ? (
                <Out href={c.pull.url} label={t('Open pull request {pr}', { pr: `#${c.pull.number}` })}>
                  <SizableText size="$1" color="$ink" textDecorationLine="underline">
                    {t('Open on the forge')}
                  </SizableText>
                </Out>
              ) : null}
              <Merge session={session} open={c.pull.state === 'open'} onMerged={reload} />
            </YStack>
            {c.pull.reviews.length ? (
              c.pull.reviews.map((r, i) => (
                <YStack key={i} gap="$1" pb="$2" borderBottomWidth={1} borderColor="$borderColor">
                  <SizableText size="$1" color="$soft">
                    {[r.author || t('Someone'), verdict(r.state, t), when(r.at)].filter(Boolean).join(' · ')}
                  </SizableText>
                  {r.body ? (
                    <SizableText size="$2" color="$ink">
                      {r.body}
                    </SizableText>
                  ) : null}
                </YStack>
              ))
            ) : (
              <Soft>{t('No reviews yet')}</Soft>
            )}
          </YStack>
        ) : (
          <Soft>{t('No pull request yet. The run opens one when it pushes its changes.')}</Soft>
        )
      ) : c.commits.length ? (
        <YStack>
          {c.commits.map((k) => (
            <XStack key={k.sha} gap="$3" py="$2" borderBottomWidth={1} borderColor="$borderColor" items="baseline">
              <SizableText size="$1" color="$soft" style={mono}>
                {k.sha.slice(0, 7)}
              </SizableText>
              <YStack flex={1} minW={0} gap="$0.5">
                <SizableText size="$2" color="$ink" numberOfLines={2}>
                  {k.message}
                </SizableText>
                <SizableText size="$1" color="$soft" numberOfLines={1}>
                  {[k.author, when(k.date)].filter(Boolean).join(' · ')}
                </SizableText>
              </YStack>
            </XStack>
          ))}
        </YStack>
      ) : (
        <Soft>{t('No pushed commits')}</Soft>
      )}
    </YStack>
  )
}

/**
 * Merge for a run's pull request, shown while it is open: `POST …/merge` lands
 * the run's branch in its base at the commit the forge holds now. One press is
 * one merge — pressed again while it is out, nothing more is sent — and a
 * refusal says the forge's reason. `onMerged` reads the run again, which is
 * what closes the button: the pull request reads merged.
 */
export function Merge({ session, open, onMerged }: { session: string; open: boolean; onMerged: () => void }) {
  const target = useTarget()
  const { t } = useI18n()
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState('')
  const press = async () => {
    if (busy) return
    setBusy(true)
    setNote('')
    try {
      const done = await merge(target, session)
      setNote(done.base ? t('Merged into {branch}', { branch: done.base }) : t('Merged'))
      onMerged()
    } catch (e) {
      setNote((e as Error).message)
    } finally {
      setBusy(false)
    }
  }
  if (!open && !note) return null
  return (
    <XStack items="center" gap="$2" flexWrap="wrap">
      {open ? (
        <Button size="sm" variant="outline" aria-busy={busy} onPress={() => void press()}>
          {busy ? t('Merging…') : t('Merge')}
        </Button>
      ) : null}
      {note ? (
        <SizableText size="$1" color="$soft" role="status">
          {note}
        </SizableText>
      ) : null}
    </XStack>
  )
}

function File({ change: f, first }: { change: Change; first: boolean }) {
  const { t } = useI18n()
  const [open, setOpen] = useState(false)
  const lines = f.patch ? f.patch.split('\n') : []
  return (
    <YStack borderTopWidth={first ? 0 : 1} borderColor="$borderColor">
      <XStack
        render="button"
        aria-label={open ? t('Hide {path}', { path: f.path }) : t('Show {path}', { path: f.path })}
        onPress={() => setOpen(!open)}
        items="center"
        gap="$2"
        px="$3"
        py="$2"
        hoverStyle={{ bg: '$hover' }}
      >
        {open ? <ChevronDown size={14} /> : <ChevronRight size={14} {...{ 'data-i18n-mirror': '' }} />}
        <SizableText size="$1" color="$soft" width={12}>
          {f.status[0]!.toUpperCase()}
        </SizableText>
        <SizableText flex={1} minW={0} size="$2" color="$ink" numberOfLines={1} text="start">
          {f.from ? `${f.from} → ${f.path}` : f.path}
        </SizableText>
        <SizableText size="$1" color="$green10">{`+${f.additions}`}</SizableText>
        <SizableText size="$1" color="$red10">{`−${f.deletions}`}</SizableText>
      </XStack>
      {open ? (
        <YStack px="$3" pb="$2" overflow="scroll">
          {lines.length ? (
            <Patch text={f.patch} />
          ) : (
            <SizableText size="$1" color="$soft">
              {f.truncated ? t('Too large to show here.') : t('Binary file')}
            </SizableText>
          )}
          {lines.length && f.truncated ? (
            <SizableText size="$1" color="$soft">
              {t('The rest of this file’s change is too large to show here.')}
            </SizableText>
          ) : null}
        </YStack>
      ) : null}
    </YStack>
  )
}

/** A unified diff's lines, coloured by what each does: the Git tab's, and a transcript's edit card's. */
export function Patch({ text }: { text: string }) {
  return (
    <>
      {text.split('\n').map((l, i) => (
        <SizableText
          key={i}
          size="$1"
          style={mono}
          color={l.startsWith('@@') ? '$soft' : l.startsWith('+') ? '$green10' : l.startsWith('-') ? '$red10' : '$ink'}
        >
          {l || ' '}
        </SizableText>
      ))}
    </>
  )
}

/** A view's name, marked (`phrase`): drawn through `t`. */
function label(v: View): string {
  return v === 'diff' ? phrase('Diff') : v === 'review' ? phrase('Review') : phrase('Commits')
}

/** What a reviewer did, in the reader's language; a state the forge names that is not one of these is drawn as given. */
function verdict(state: string, t: Say): string {
  switch (state.toUpperCase()) {
    case 'APPROVED':
      return t('approved')
    case 'REQUEST_CHANGES':
      return t('asked for changes')
    case 'COMMENT':
      return t('commented')
    default:
      return state.toLowerCase() || t('reviewed')
  }
}

function when(iso: string): string {
  const d = new Date(iso)
  return Number.isNaN(d.getTime()) ? '' : d.toLocaleString()
}

function Soft({ children }: { children: string }) {
  return (
    <YStack py="$6" items="center">
      <SizableText size="$2" color="$soft" style={{ textAlign: 'center' }}>
        {children}
      </SizableText>
    </YStack>
  )
}
