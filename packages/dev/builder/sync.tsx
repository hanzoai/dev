/**
 * Bring repositories the GitHub connection already grants onto the forge.
 *
 * The list is GET /v1/provider/github/repos, narrowed to the accounts this
 * organization added (github.ts `added`): those are the only ones the import
 * brings repositories in through. The queue is POST
 * /v1/provider/github/repos/import, which mirrors `owner/name` into this
 * organization's forge in the background. An account named unread and holding
 * no repositories is shown as needing an organization admin. One repository
 * brought in becomes the one New starts the next run on (choice.ts `pinHub`).
 *
 * Its words are the reader's language (@hanzo/i18n `t`); account and
 * repository names, and Forge, the forge's own name, are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { Check, ChevronLeft, ChevronRight, Plus, X } from '@hanzogui/lucide-icons-2'
import { Button, Input } from '@hanzo/ui'
import { useEffect, useMemo, useState, type ReactNode } from 'react'

import { here, useBack } from './back.ts'
import { Refusal } from './api/call.ts'
import { accounts, added, bring, grants, type Grant, type Grants } from './api/github.ts'
import { authorize } from './api/provider.ts'
import { pinHub, readPending, writePending, type Pending } from './choice.ts'
import { useRead } from './data.ts'
import { useHost, useTarget } from './host.tsx'
import { path } from './route.ts'

const EMPTY: Grants = { repos: [], unread: [] }

export function Sync() {
  const host = useHost()
  const target = useTarget()
  const { t } = useI18n()
  const signed = Boolean(host.person)
  const [q, setQ] = useState('')
  const [owner, setOwner] = useState('')
  const [step, setStep] = useState<'pick' | 'access'>('pick')
  const [picked, setPicked] = useState<string[]>([])
  const [busy, setBusy] = useState(false)
  const [syncing, setSyncing] = useState(false)
  const [error, setError] = useState('')
  // The import was refused because this organization holds no GitHub account of its own (the list is the person's).
  const [unbound, setUnbound] = useState(false)
  const list = useRead(signed ? () => added(target) : null, EMPTY, [target, signed])
  const rows = list.value.repos
  const orgs = useMemo(() => accounts(list.value), [list.value])
  const needle = q.trim().toLowerCase()
  const shownOrgs = orgs.filter((a) => !needle || a.name.toLowerCase().includes(needle))
  const ofOwner = rows.filter((r) => r.owner === owner)
  const shownRepos = ofOwner.filter((r) => !needle || r.name.toLowerCase().includes(needle) || r.fullName.toLowerCase().includes(needle))
  const chosen = rows.filter((r) => picked.includes(r.fullName))
  const lead = chosen[0]?.name ?? ''
  const more = Math.max(0, chosen.length - 1)
  const dest = host.org || 'Forge'

  const toggle = (fullName: string) => {
    setPicked((cur) => (cur.includes(fullName) ? cur.filter((n) => n !== fullName) : [...cur, fullName]))
  }

  const selectShown = () => {
    const names = shownRepos.map((r) => r.fullName)
    const all = names.every((n) => picked.includes(n))
    setPicked((cur) => (all ? cur.filter((n) => !names.includes(n)) : [...new Set([...cur, ...names])]))
  }

  // Coming back from GitHub's App install lands here, says what happened, and reads the list again.
  useBack(target, signed, setError, list.reload)

  /**
   * Install the platform's GitHub App on another account — an organization admin's
   * act — and come back to this page: the App, not a person's own sign-in, is what
   * grants an organization's repositories.
   */
  const addOrg = async () => {
    setError('')
    if (!host.admin) {
      setError(t('An organization admin installs the GitHub App on another account.'))
      return
    }
    try {
      host.open(await authorize(target, 'github', here()))
    } catch (e) {
      setError(e instanceof Refusal && e.status === 403 ? t('An organization admin installs the GitHub App on another account.') : t('GitHub could not be reached to install the App. Try again.'))
    }
  }

  const send = async () => {
    setBusy(true)
    setError('')
    setUnbound(false)
    try {
      const queued = await bring(target, picked)
      // Each waits under the name it takes on the forge, as the import says it, else as GitHub's listing says it will.
      const next: Pending[] = [
        ...readPending(host.org).filter((p) => !picked.includes(p.fullName)),
        ...chosen.map((r) => ({ fullName: r.fullName, name: queued.codebases[r.fullName.toLowerCase()] || r.codebase || r.name })),
      ]
      writePending(host.org, next)
      // One repository brought in is the one the person means to work on next, so
      // New opens on it rather than on what it held before. Several say no one.
      if (picked.length === 1 && chosen.length === 1) pinHub(host.org, chosen[0])
      setSyncing(true)
    } catch (e) {
      // The list can be the person's own GitHub, which reaches accounts this organization has not connected; the
      // import goes through the organization's own, and the platform's 409 says only "not connected".
      if (e instanceof Refusal && e.status === 409) {
        setUnbound(true)
        setError(
          t(
            'Your GitHub lists these repositories, but {place} has no GitHub account of its own to bring them in through. Add organization installs the Hanzo app on one for {place}; an account another Hanzo workspace uses has to be disconnected there first.',
            { place: dest },
          ),
        )
      } else setError((e as Error).message)
    } finally {
      setBusy(false)
    }
  }

  const back = () => {
    host.go(path({ kind: 'screen', screen: 'projects' }))
  }

  useEffect(() => {
    if (!syncing) return
    let stop = false
    const tick = async () => {
      try {
        const g = await grants(target)
        const landed = new Set(g.repos.filter((r) => r.imported).map((r) => r.fullName))
        const left = readPending(host.org).filter((p) => !landed.has(p.fullName))
        writePending(host.org, left)
        if (!stop && picked.every((n) => landed.has(n))) back()
      } catch {
        /* the button stays on Syncing until the next look */
      }
    }
    const id = window.setInterval(() => void tick(), 4000)
    return () => {
      stop = true
      window.clearInterval(id)
    }
  }, [syncing, target, host.org, host.go, picked])

  return (
    <YStack flex={1} minH={0} overflow="scroll">
      <XStack px="$6" pt="$5" pb="$2" gap="$2" items="center">
        <XStack render="button" aria-label={t('Back to projects')} onPress={back}>
          <SizableText size="$2" color="$soft">
            {t('Projects')}
          </SizableText>
        </XStack>
        <SizableText size="$2" color="$soft">
          /
        </SizableText>
        <SizableText size="$2" color="$ink">
          {t('Sync')}
        </SizableText>
      </XStack>
      <YStack flex={1} items="center" justify="center" px="$6" pb="$10" gap="$5">
        <XStack justify="center" gap="$6">
          <Step label={t('Select repositories')} done={step === 'access'} current={step === 'pick'} />
          <Step label={t('Repository access')} done={false} current={step === 'access'} />
        </XStack>
        <YStack borderWidth={1} borderColor="$borderColor" rounded="$5" bg="$panel" px="$6" py="$6" gap="$4" width="100%" maxW={460}>
          {step === 'pick' && !owner ? (
            <>
              <YStack gap="$1" items="center">
                <SizableText size="$5" color="$ink">
                  {t('Select repositories')}
                </SizableText>
                <SizableText size="$2" color="$soft" text="center">
                  {t('Choose repositories to bring into {place}', { place: dest })}
                </SizableText>
              </YStack>
              <Panel>
                <Input value={q} onChangeText={setQ} placeholder={t('Search organizations')} aria-label={t('Search organizations')} disabled={!signed} />
                {/* No count while the first read is out: "0 organizations" then is not what this organization has. */}
                {signed && !(list.loading && orgs.length === 0) ? (
                  <SizableText size="$1" color="$soft">
                    {orgs.length === 1 ? t('1 organization') : t('{n} organizations', { n: orgs.length })}
                  </SizableText>
                ) : null}
                {list.error ? <Soft>{t('The GitHub connection could not be read right now.')}</Soft> : null}
                {!signed ? (
                  <Soft>{t('Sign in to choose repositories the GitHub connection can read.')}</Soft>
                ) : list.loading && rows.length === 0 && orgs.length === 0 ? (
                  <Soft>{t('Reading the connection…')}</Soft>
                ) : list.error && orgs.length === 0 ? null : shownOrgs.length === 0 ? (
                  <Soft>
                    {orgs.length === 0
                      ? t('No GitHub account is added to {place} yet. Add organization installs the Hanzo app on one.', { place: dest })
                      : t('Nothing matches.')}
                  </Soft>
                ) : (
                  shownOrgs.map((a) => (
                    <XStack
                      key={a.name}
                      render="button"
                      aria-label={a.blocked ? t('{name} needs an organization admin', { name: a.name }) : t('Open {name}', { name: a.name })}
                      disabled={a.blocked}
                      items="center"
                      gap="$2"
                      py="$2"
                      borderTopWidth={1}
                      borderColor="$borderColor"
                      onPress={() => {
                        setOwner(a.name)
                        setQ('')
                      }}
                    >
                      <SizableText flex={1} size="$2" color={a.blocked ? '$soft' : '$ink'} numberOfLines={1}>
                        {a.name}
                      </SizableText>
                      {a.blocked ? (
                        <SizableText size="$1" color="$soft">
                          {t('Needs organization admin')}
                        </SizableText>
                      ) : (
                        <ChevronRight size={14} {...{ 'data-i18n-mirror': '' }} />
                      )}
                    </XStack>
                  ))
                )}
                <XStack render="button" aria-label={t('Add organization')} items="center" gap="$2" py="$2" onPress={() => void addOrg()} disabled={!signed}>
                  <Plus size={14} />
                  <SizableText size="$2" color="$ink">
                    {t('Add organization')}
                  </SizableText>
                </XStack>
              </Panel>
            </>
          ) : null}
          {step === 'pick' && owner ? (
            <>
              <YStack gap="$1" items="center">
                <SizableText size="$5" color="$ink">
                  {t('Select repositories')}
                </SizableText>
                <SizableText size="$2" color="$soft" text="center">
                  {t('Choose repositories to bring into {place}', { place: dest })}
                </SizableText>
              </YStack>
              <Panel>
                <XStack items="center" gap="$2">
                  <XStack render="button" aria-label={t('All organizations')} onPress={() => setOwner('')}>
                    <ChevronLeft size={16} {...{ 'data-i18n-mirror': '' }} />
                  </XStack>
                  <SizableText flex={1} size="$2" color="$ink" numberOfLines={1}>
                    {owner}
                  </SizableText>
                </XStack>
                <Input value={q} onChangeText={setQ} placeholder={t('Search repositories')} aria-label={t('Search repositories')} />
                <XStack justify="space-between" items="center">
                  <SizableText size="$1" color="$soft">
                    {ofOwner.length === 1 ? t('1 repository') : t('{n} repositories', { n: ofOwner.length })}
                  </SizableText>
                  <XStack render="button" aria-label={t('Select all')} onPress={selectShown} disabled={shownRepos.length === 0}>
                    <SizableText size="$1" color="$ink">
                      {t('Select all')}
                    </SizableText>
                  </XStack>
                </XStack>
                {shownRepos.length === 0 ? (
                  <Soft>{t('Nothing matches.')}</Soft>
                ) : (
                  shownRepos.map((r) => (
                    <RepoRow key={r.fullName} repo={r} on={picked.includes(r.fullName)} onPress={() => toggle(r.fullName)} />
                  ))
                )}
                <XStack render="button" aria-label={t('Grant more repositories')} items="center" gap="$2" py="$2" onPress={() => void addOrg()}>
                  <Plus size={14} />
                  <SizableText size="$2" color="$ink">
                    {t('Add repositories')}
                  </SizableText>
                </XStack>
              </Panel>
            </>
          ) : null}
          {step === 'access' ? (
            <>
              <YStack gap="$1" items="center">
                <SizableText size="$5" color="$ink">
                  {t('Repository access')}
                </SizableText>
                <SizableText size="$2" color="$soft" text="center">
                  {t('Review who can read these repositories')}
                </SizableText>
              </YStack>
              <XStack gap="$2" items="center">
                <XStack flex={1} items="center" gap="$2" px="$3" py="$2" rounded="$3" borderWidth={1} borderColor="$borderColor">
                  <XStack render="button" aria-label={t('Clear the selection')} onPress={() => { setPicked([]); setStep('pick') }}>
                    <X size={12} />
                  </XStack>
                  <SizableText flex={1} size="$2" color="$ink" numberOfLines={1}>
                    {more ? t('{name}, +{n} more', { name: lead, n: more }) : lead}
                  </SizableText>
                </XStack>
                <Button size="sm" variant="outline" onPress={() => setStep('pick')}>
                  {t('Edit')}
                </Button>
              </XStack>
              <XStack justify="space-between" gap="$4">
                <YStack gap="$1">
                  <SizableText size="$1" color="$soft">
                    {t('Who gets access')}
                  </SizableText>
                  <SizableText size="$2" color="$ink">
                    {t('{name} · You', { name: host.person?.name || host.person?.email || t('You') })}
                  </SizableText>
                </YStack>
                <YStack gap="$1" items="flex-end">
                  <SizableText size="$1" color="$soft">
                    {t('Role')}
                  </SizableText>
                  <SizableText size="$2" color="$ink">
                    {host.admin ? t('Repository admin') : t('Member')}
                  </SizableText>
                </YStack>
              </XStack>
              <SizableText size="$2" color="$soft" text="center">
                {t('Anyone with access to {place}', { place: dest })}
              </SizableText>
            </>
          ) : null}
          {error ? <Soft>{error}</Soft> : null}
          {unbound && step === 'access' ? (
            <Button width="100%" variant="outline" onPress={() => void addOrg()}>
              <Plus size={14} />
              {t('Add organization')}
            </Button>
          ) : null}
          {step === 'pick' ? (
            <Button width="100%" disabled={!owner || picked.length === 0} onPress={() => setStep('access')}>
              {t('Continue')}
            </Button>
          ) : (
            <YStack gap="$3" items="center" width="100%">
              <Button width="100%" disabled={busy || syncing || picked.length === 0} onPress={() => void send()}>
                {syncing || busy
                  ? t('Syncing…')
                  : picked.length === 1
                    ? t('Sync 1 repository to {place}', { place: dest })
                    : t('Sync {n} repositories to {place}', { n: picked.length, place: dest })}
              </Button>
              {syncing ? null : (
                <XStack render="button" aria-label={t('Sync later')} onPress={back}>
                  <SizableText size="$2" color="$soft">
                    {t('Sync later')}
                  </SizableText>
                </XStack>
              )}
            </YStack>
          )}
          {!signed ? (
            <Button width="100%" onPress={() => host.signIn?.()}>
              {t('Sign in')}
            </Button>
          ) : null}
        </YStack>
      </YStack>
    </YStack>
  )
}

function Step({ label, done, current }: { label: string; done: boolean; current: boolean }) {
  return (
    <XStack items="center" gap="$2">
      <YStack
        width={14}
        height={14}
        rounded={999}
        borderWidth={1}
        items="center"
        justify="center"
      >
        {done ? <Check size={10} color="white" /> : null}
      </YStack>
      <SizableText size="$1" color={current || done ? '$ink' : '$soft'}>
        {label}
      </SizableText>
    </XStack>
  )
}

function Panel({ children }: { children: ReactNode }) {
  return (
    <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" px="$3" py="$2" gap="$2" maxH={360} overflow="scroll">
      {children}
    </YStack>
  )
}

function Soft({ children }: { children: string }) {
  return (
    <SizableText size="$2" color="$soft">
      {children}
    </SizableText>
  )
}

function RepoRow({ repo, on, onPress }: { repo: Grant; on: boolean; onPress: () => void }) {
  const { t } = useI18n()
  return (
    <XStack
      render="button"
      aria-label={on ? t('Selected {name}', { name: repo.name }) : t('Select {name}', { name: repo.name })}
      aria-pressed={on}
      items="center"
      gap="$2"
      py="$2"
      borderTopWidth={1}
      borderColor="$borderColor"
      onPress={onPress}
    >
      <YStack width={14} height={14} rounded="$1" borderWidth={1} borderColor={on ? '$ink' : '$borderColor'} bg={on ? '$ink' : 'transparent'} />
      <SizableText flex={1} size="$2" color="$ink" numberOfLines={1}>
        {repo.name}
      </SizableText>
      {repo.imported || repo.status === 'pending' ? (
        <SizableText size="$1" color="$soft">
          {repo.imported ? (repo.status === 'conflict' ? t('On Forge · diverged') : t('On Forge')) : t('Syncing…')}
        </SizableText>
      ) : null}
    </XStack>
  )
}
