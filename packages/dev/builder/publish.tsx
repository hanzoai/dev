/**
 * Adding a repository to a project — the one publish path, from a repo row
 * ("Add to project") and from a workspace's Publish.
 *
 * `POST /v1/platform/apps` builds the repository and writes its declaration.
 * On a branch (everyone) that is a review: nothing deploys until it merges,
 * and the review link is the thing to open. An org admin can take a green
 * build to main, which CD applies: that is the second call, with the build's
 * tag, because main refuses an image that is not built yet.
 *
 * A repository in the org's own code workspace answers `live` instead: its
 * build puts it on the project's address when it finishes, so there is no
 * review and nothing to take to main. Its build is followed on the board like
 * any other, and `onLive` is told when it succeeds, so the host reads the
 * project's address again.
 *
 * Every state shown is one the platform reported; a refusal shows the
 * platform's own sentence. The dialog's own words are the reader's language
 * (@hanzo/i18n `t`).
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { ExternalLink } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogDescription, DialogTitle, Input } from '@hanzo/ui'
import { useEffect, useRef, useState } from 'react'

import { builds, declare, label, type Declared } from './api/platform.ts'
import { useHost, useTarget } from './host.tsx'
import { Out } from './out.tsx'

export interface Source {
  /** The https clone URL. */
  repo: string
  /** What to call it: `owner/name` or a project's name. */
  title: string
  ref: string
  /** The app name to declare. */
  name: string
  /** The project to add it to, when there already is one. */
  project?: string
  /** Current preview URL for this project, shown separately from the live site URL (#365). */
  preview?: string
}

type Phase = 'ask' | 'sending' | 'declared' | 'shipping' | 'shipped' | 'failed'

const TERMINAL_FAIL = /^(failed|fail|error|errored|canceled|cancelled)$/i
const TERMINAL_OK = /^(succeeded|success|done|complete|completed|ok)$/i

export function Publish({ source, onClose, onLive }: { source: Source | null; onClose: () => void; onLive?: () => void }) {
  const target = useTarget()
  const host = useHost()
  const { t } = useI18n()
  const [project, setProject] = useState('')
  const [appName, setAppName] = useState('')
  const [phase, setPhase] = useState<Phase>('ask')
  const [out, setOut] = useState<Declared | null>(null)
  const [status, setStatus] = useState('')
  const [error, setError] = useState('')
  const told = useRef(onLive)
  told.current = onLive

  useEffect(() => {
    setProject(source?.project ?? source?.name ?? '')
    setAppName(source?.name ?? '')
    setPhase('ask')
    setOut(null)
    setStatus('')
    setError('')
  }, [source])

  const green = TERMINAL_OK.test(status)
  const buildFailed = TERMINAL_FAIL.test(status)

  // The build is asynchronous; stop polling once it reaches a terminal success or failure state.
  useEffect(() => {
    const id = out?.build?.id
    if (!id || phase !== 'declared' || green || buildFailed) return
    let live = true
    const tick = async () => {
      try {
        const row = (await builds(target)).find((b) => b.id === id)
        if (live && row) setStatus(row.status)
      } catch {
        /* the status stays as last read */
      }
    }
    void tick()
    const every = setInterval(tick, 5000)
    return () => {
      live = false
      clearInterval(every)
    }
  }, [target, out, phase, green, buildFailed])

  // A site's build going green is the site going live, at the project's address.
  useEffect(() => {
    if (out?.live && green) told.current?.()
  }, [out, green])

  if (!source) return null

  const resolvedName = label(appName || source.name)
  const resolvedProject = label(project || source.name)
  const locked = phase === 'sending' || phase === 'shipping' || phase === 'shipped' || (phase === 'declared' && !buildFailed)
  const previewHost = (() => {
    if (!source.preview) return ''
    try {
      return new URL(source.preview).hostname.toLowerCase()
    } catch {
      return ''
    }
  })()
  const sameAsPreview = Boolean(previewHost && previewHost.startsWith(`${resolvedName}.`))

  const send = async () => {
    setPhase('sending')
    setOut(null)
    setStatus('')
    setError('')
    try {
      const d = await declare(target, { repo: source.repo, ref: source.ref, name: resolvedName, project: resolvedProject, mode: 'branch' })
      setOut(d)
      setStatus(d.build?.status ?? '')
      setPhase('declared')
    } catch (e) {
      setError((e as Error).message)
      setPhase('failed')
    }
  }

  // The build this write started; its status is what the platform last said of it.
  const built = out?.build?.id ?? ''
  const liveUrl = out?.hosts?.[0] ?? ''

  // Offered only once that build is green.
  const ship = async () => {
    setPhase('shipping')
    setError('')
    try {
      await declare(target, { repo: source.repo, ref: source.ref, name: resolvedName, project: resolvedProject, mode: 'commit', tag: built })
      setPhase('shipped')
    } catch (e) {
      setError((e as Error).message)
      setPhase('declared')
    }
  }

  return (
    <Dialog open onOpenChange={(o) => !o && onClose()}>
      <DialogContent maxW={480}>
        <DialogTitle>{t('Add to project')}</DialogTitle>
        <DialogDescription>
          {out?.live
            ? t('Build {repo} at {ref} and publish it at its project’s address.', { repo: source.title, ref: source.ref })
            : `${t('Build {repo} at {ref} and declare it in a project.', { repo: source.title, ref: source.ref })} ${host.admin ? t('As an admin you can take a green build to main.') : t('It opens a review; merging it is what deploys.')}`}
        </DialogDescription>

        {source.preview ? (
          <XStack items="center" justify="space-between" gap="$2" px="$2.5" py="$2" rounded="$2" bg="$panel" borderWidth={1} borderColor="$borderColor" minW={0}>
            <SizableText size="$1" color="$soft" shrink={0}>
              {t('Preview URL')}
            </SizableText>
            <XStack flex={1} minW={0} justify="flex-end">
              <Out href={source.preview}>
                <XStack items="center" gap="$1" minW={0} {...{ 'data-i18n-ltr': '' }}>
                  <SizableText size="$1" color="$ink" textDecorationLine="underline" numberOfLines={1} minW={0}>
                    {source.preview}
                  </SizableText>
                  <ExternalLink size={11} opacity={0.6} shrink={0} />
                </XStack>
              </Out>
            </XStack>
          </XStack>
        ) : null}

        <YStack gap="$1.5">
          <SizableText render="label" size="$2" color="$soft" htmlFor="publish-app-name">
            {t('Live app name')}
          </SizableText>
          <Input
            id="publish-app-name"
            value={appName}
            onChangeText={(v: string) => setAppName(v)}
            disabled={locked}
            placeholder={label(source.name)}
            aria-label={t('Live app name')}
          />
          <SizableText size="$1" color="$soft">
            {sameAsPreview
              ? t('Declared as {name} — change this if you want a separate name from the preview.', { name: resolvedName })
              : t('Declared as {name}', { name: resolvedName })}
          </SizableText>
        </YStack>

        <YStack gap="$1.5">
          <SizableText render="label" size="$2" color="$soft" htmlFor="publish-project">
            {t('Project')}
          </SizableText>
          <Input
            id="publish-project"
            value={project}
            onChangeText={(v: string) => setProject(v)}
            disabled={locked}
            placeholder={label(source.name)}
            aria-label={t('Project')}
          />
          <SizableText size="$1" color="$soft">
            {t('Saved as {name}', { name: resolvedProject })}
          </SizableText>
        </YStack>

        {out ? (
          <YStack gap="$2" role={buildFailed ? 'alert' : 'status'}>
            {out.notice ? (
              <SizableText size="$2" color="$soft">
                {out.notice}
              </SizableText>
            ) : null}
            <SizableText size="$2" color={buildFailed ? '$red10' : '$ink'}>
              {phase === 'shipped'
                ? t('Declared on main — CD applies it on its next pass.')
                : out.live && green
                  ? t('Build {id} succeeded — it is live at the project’s address.', { id: built })
                  : status
                    ? t('Build {id}: {status}', { id: built, status })
                    : t('Build {id}: building', { id: built })}
            </SizableText>
            {liveUrl ? (
              <XStack items="center" justify="space-between" gap="$2" px="$2.5" py="$2" rounded="$2" bg="$panel" borderWidth={1} borderColor="$borderColor" minW={0}>
                <SizableText size="$1" color="$soft" shrink={0}>
                  {t('Live URL')}
                </SizableText>
                <XStack flex={1} minW={0} justify="flex-end">
                  <Out href={liveUrl}>
                    <XStack items="center" gap="$1" minW={0} {...{ 'data-i18n-ltr': '' }}>
                      <SizableText size="$1" color="$ink" textDecorationLine="underline" numberOfLines={1} minW={0}>
                        {liveUrl}
                      </SizableText>
                      <ExternalLink size={11} opacity={0.6} shrink={0} />
                    </XStack>
                  </Out>
                </XStack>
              </XStack>
            ) : null}
            {out.review ? (
              <Out href={out.review}>
                <SizableText size="$2" color="$ink" textDecorationLine="underline">
                  {t('Open the review')}
                </SizableText>
                <ExternalLink size={12} opacity={0.6} shrink={0} />
              </Out>
            ) : null}
          </YStack>
        ) : null}

        {error ? (
          <SizableText size="$2" color="$red10" role="alert">
            {error}
          </SizableText>
        ) : null}

        <XStack gap="$2" justify="flex-end">
          <Button variant="outline" size="sm" onPress={onClose}>
            {phase === 'shipped' || (phase === 'declared' && !buildFailed) ? t('Done') : t('Cancel')}
          </Button>
          {phase === 'ask' || phase === 'failed' || phase === 'sending' || buildFailed ? (
            <Button size="sm" disabled={phase === 'sending'} onPress={() => void send()}>
              {phase === 'sending' ? t('Adding…') : buildFailed ? t('Try again') : t('Add to project')}
            </Button>
          ) : host.admin && !out?.live && (phase === 'declared' || phase === 'shipping') ? (
            <Button size="sm" disabled={!green || phase === 'shipping'} onPress={() => void ship()}>
              {phase === 'shipping' ? t('Shipping…') : green ? t('Ship to main') : t('Waiting for the build')}
            </Button>
          ) : null}
        </XStack>
      </DialogContent>
    </Dialog>
  )
}
