/**
 * Machines: the organization's own computers a run can be sent to, beside the
 * Hanzo sandbox. `hanzo link` run on a machine registers it under its hostname
 * and keeps it online while it runs; one can also be registered here by hand.
 * The member who linked a machine, or an org admin, renames it, drains it,
 * mints its claim key or removes it. Under each, the folders its owner lets the
 * web use, and on Hanzo Desktop a way to share one of this computer's
 * (shared.tsx).
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a machine's name,
 * hostname, id and hardware, and the commands to type, are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n, type Say } from '@hanzo/i18n/react'
import {
  Activity,
  ChevronDown,
  ChevronUp,
  Cloud,
  Cpu,
  HardDrive,
  Info,
  Laptop,
  MoreHorizontal,
  Plus,
  Server,
  Terminal,
  Zap,
} from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogTitle, DropdownMenu, Input, Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@hanzo/ui'
import { CopyButton } from '@hanzo/ui/product'
import { useEffect, useState } from 'react'

import { told } from '../api/call.ts'
import {
  add,
  change,
  displayMachine,
  formatBytes,
  formatPercent,
  formatRelative,
  key,
  KINDS,
  machineKey,
  machines,
  remove,
  state,
  type Kind,
  type Machine,
} from '../api/machines.ts'
import { Confirm, Rename } from '../ask.tsx'
import { useRead } from '../data.ts'
import { useDevMode } from '../dev-mode.ts'
import { useHost, useLocal, useTarget } from '../host.tsx'
import { rename, useDeviceNames } from '../names.ts'
import { Shared } from './shared.tsx'
import { Card, Field, Group, Heading, Note, Once, Row, Soft } from './ui.tsx'

/** What `hanzo link` needs, in the order it is typed on the machine. */
const LINK = ['hanzo login', 'hanzo link'].join('\n')

/** How often the list is read again, so a machine that comes up or goes quiet shows it: the agent's beat. */
const BEAT = 30_000

/** The list before its first answer; every answer is a new array, even an empty one. */
const UNREAD: Machine[] = []

/** A machine's status in a line, the platform's word or `not seen yet`, said in the reader's language; one it does not name, as given. */
const STATES: Record<string, string> = {
  online: phrase('online'),
  offline: phrase('offline'),
  draining: phrase('draining'),
  'not seen yet': phrase('not seen yet'),
  '': phrase('unknown'),
}

/** What a machine is, as the platform names its kinds, said in the reader's language; one it does not name, as given. */
const KIND: Record<string, string> = { laptop: phrase('laptop'), gpu: phrase('gpu'), cloud: phrase('cloud'), cluster: phrase('cluster'), machine: phrase('machine') }

/** A status as a line says it. Code's place list says it the same way. */
export const said = (status: string, t: Say): string => (status in STATES ? t(STATES[status]!) : status)

const kindOf = (kind: string, t: Say): string => (KIND[kind] ? t(KIND[kind]) : kind)

/** The line under a machine: what it is, where, how big, and what it is doing. */
function about(m: Machine, t: Say): string {
  const load = m.running ? t('{n} running', { n: m.running }) : m.sessions ? t('{n} runs', { n: m.sessions }) : ''
  return [said(state(m), t), kindOf(m.kind, t), m.host && m.host !== m.label ? m.host : '', m.capacity, load].filter(Boolean).join(' · ')
}

function Dot({ m }: { m: Machine }) {
  const on = state(m) === 'online'
  return (
    <YStack
      width={8}
      height={8}
      rounded={999}
      shrink={0}
      bg={on ? '$green10' : 'transparent'}
      borderWidth={on ? 0 : 1}
      borderColor={m.status === 'draining' ? '$yellow10' : '$soft'}
      aria-hidden
    />
  )
}

function KindIcon({ kind, size = 16 }: { kind: string; size?: number }) {
  switch (kind) {
    case 'laptop':
      return <Laptop size={size} />
    case 'gpu':
      return <Cpu size={size} />
    case 'cloud':
      return <Cloud size={size} />
    case 'cluster':
      return <Server size={size} />
    default:
      return <Terminal size={size} />
  }
}

function StatusBadge({ m }: { m: Machine }) {
  const { t } = useI18n()
  const s = state(m)
  if (!s) return null
  let color: '$green10' | '$yellow10' | '$blue10' | '$soft' = '$soft'
  let text = s
  if (s === 'online') {
    color = '$green10'
    text = t('Online')
  } else if (s === 'draining') {
    color = '$yellow10'
    text = t('Draining')
  } else if (s === 'not seen yet') {
    color = '$blue10'
    text = t('Not seen yet')
  } else if (s === 'offline') {
    color = '$soft'
    text = t('Offline')
  }
  return (
    <XStack px="$2" py="$0.5" rounded="$10" borderWidth={1} borderColor={color} items="center" gap="$1.5">
      <Dot m={m} />
      <SizableText size="$1" color={color} fontWeight="500">
        {text}
      </SizableText>
    </XStack>
  )
}

export function Machines() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const names = useDeviceNames(signed ? target : null)
  const [devMode] = useDevMode()
  const list = useRead(signed ? () => machines(target) : null, UNREAD, [target, signed])
  const [adding, setAdding] = useState(false)
  const [renaming, setRenaming] = useState<Machine | null>(null)
  const [removing, setRemoving] = useState<Machine | null>(null)
  const [keying, setKeying] = useState<Machine | null>(null)
  const [minted, setMinted] = useState<{ machine: Machine; key: string } | null>(null)
  const [expandedId, setExpandedId] = useState<string | null>(null)
  const [collapsedDevIds, setCollapsedDevIds] = useState<Record<string, boolean>>({})
  const [showAdvancedLink, setShowAdvancedLink] = useState(false)
  const [note, setNote] = useState('')
  // This computer's hostname, on a host that has the computer: its machine is the one registered under it.
  const local = useLocal()
  const [hostname, setHostname] = useState('')
  useEffect(() => {
    let live = true
    void local?.host?.().then(
      (h) => live && setHostname(h),
      // A computer that cannot say its name is not found among the machines, and shares nothing from here.
      () => {},
    )
    return () => {
      live = false
    }
  }, [local])

  // Read again on the agent's beat; "Reading machines…" stays for the first read only.
  const again = list.reload
  useEffect(() => {
    if (!signed) return
    const id = setInterval(again, BEAT)
    return () => clearInterval(id)
  }, [again, signed])

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('Machines')} />
        <Soft>{t('Sign in to see your linked computers.')}</Soft>
      </YStack>
    )
  }

  const drain = async (m: Machine, status: 'online' | 'draining') => {
    setNote('')
    try {
      const display = displayMachine(m, { devMode, say: t, names })
      await change(target, m.id, { status })
      setNote(status === 'draining' ? t('{name} takes no new runs', { name: display.title }) : t('{name} takes runs again', { name: display.title }))
      list.reload()
    } catch (e) {
      setNote(told(e, t))
    }
  }

  return (
    <YStack gap="$6">
      <Heading
        title={t('Machines')}
        detail={
          devMode
            ? t('Your organization’s own computers. A run sent to one runs there instead of in the cloud workspace.')
            : t('Connect your computer to run tasks locally in your Hanzo folder.')
        }
      />

      <Group title={t('Link a machine')} detail={t('Run these on your computer. It stays connected while {command} runs.', { command: 'hanzo link' })}>
        <XStack items="flex-start" gap="$2" px="$3" py="$2.5" rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$raised">
          <SizableText flex={1} minW={0} size="$2" color="$ink" aria-label={t('Link commands')} {...{ 'data-i18n-ltr': '' }} fontFamily="$mono" $platform-web={{ whiteSpace: 'pre-wrap' }}>
            {LINK}
          </SizableText>
          <CopyButton value={LINK} label={t('Copy the commands')} />
        </XStack>

        <XStack items="center" justify="space-between" flexWrap="wrap" gap="$2">
          <XStack items="center" gap="$2">
            <Button size="sm" variant="ghost" onPress={() => setAdding(true)}>
              <Plus size={14} /> {t('Register one by hand')}
            </Button>
            <Button size="sm" variant="ghost" onPress={() => setShowAdvancedLink(!showAdvancedLink)}>
              {showAdvancedLink ? <ChevronUp size={14} /> : <ChevronDown size={14} />}
              {showAdvancedLink ? t('Hide options') : t('Advanced options')}
            </Button>
          </XStack>
          <SizableText size="$1" color="$soft">
            {devMode ? t('Supported: macOS, Linux, Windows, NVIDIA CUDA GPUs, Cloud VMs') : t('Works on macOS, Windows, and Linux')}
          </SizableText>
        </XStack>

        {showAdvancedLink ? (
          <YStack gap="$2.5" p="$3" rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$raised">
            <YStack gap="$1">
              <SizableText size="$2" color="$ink" fontWeight="600">
                {t('1. Background daemon (system service)')}
              </SizableText>
              <SizableText size="$1" color="$soft">
                {t('Run worker continuously in the background without keeping a terminal open:')}
              </SizableText>
              <XStack items="center" justify="space-between" px="$2.5" py="$1.5" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor">
                <SizableText size="$1" color="$ink" {...{ 'data-i18n-ltr': '' }} fontFamily="$mono">
                  hanzo code --serve
                </SizableText>
                <CopyButton value="hanzo code --serve" label={t('Copy daemon command')} />
              </XStack>
            </YStack>

            <YStack gap="$1" pt="$1">
              <SizableText size="$2" color="$ink" fontWeight="600">
                {t('2. Install CLI from shell')}
              </SizableText>
              <XStack items="center" justify="space-between" px="$2.5" py="$1.5" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor">
                <SizableText size="$1" color="$ink" {...{ 'data-i18n-ltr': '' }} fontFamily="$mono">
                  curl -fsSL https://hanzo.sh | sh
                </SizableText>
                <CopyButton value="curl -fsSL https://hanzo.sh | sh" label={t('Copy install command')} />
              </XStack>
            </YStack>
          </YStack>
        ) : null}
      </Group>

      <Group title={t('Linked')}>
        {list.value === UNREAD ? (
          <Soft>{list.error ? told(list.error, t) : t('Loading devices…')}</Soft>
        ) : list.value.length === 0 ? (
          <YStack gap="$3">
            <Soft>{t('No computer is linked yet.')}</Soft>
            <Card>
              <YStack p="$4" gap="$3">
                <XStack items="center" gap="$2.5">
                  <Server size={20} />
                  <YStack gap="$0.5">
                    <SizableText size="$3" color="$ink" fontWeight="600">
                      {t('Connect your computer')}
                    </SizableText>
                    <SizableText size="$2" color="$soft">
                      {t('Run tasks directly on your own computer in your Hanzo folder.')}
                    </SizableText>
                  </YStack>
                </XStack>
                <YStack gap="$2" pt="$1">
                  <XStack items="center" gap="$2">
                    <SizableText size="$1" color="$soft" fontFamily="$mono" width={20}>
                      1.
                    </SizableText>
                    <SizableText size="$2" color="$ink">
                      {t('Install Hanzo CLI: {command}', { command: <SizableText size="$2" color="$ink" fontFamily="$mono" {...{ 'data-i18n-ltr': '' }}>curl -fsSL https://hanzo.sh | sh</SizableText> })}
                    </SizableText>
                  </XStack>
                  <XStack items="center" gap="$2">
                    <SizableText size="$1" color="$soft" fontFamily="$mono" width={20}>
                      2.
                    </SizableText>
                    <SizableText size="$2" color="$ink">
                      {t('Sign in with your account: {command}', { command: <SizableText size="$2" color="$ink" fontFamily="$mono" {...{ 'data-i18n-ltr': '' }}>hanzo login</SizableText> })}
                    </SizableText>
                  </XStack>
                  <XStack items="center" gap="$2">
                    <SizableText size="$1" color="$soft" fontFamily="$mono" width={20}>
                      3.
                    </SizableText>
                    <SizableText size="$2" color="$ink">
                      {t('Link the machine: {command}', { command: <SizableText size="$2" color="$ink" fontFamily="$mono" {...{ 'data-i18n-ltr': '' }}>hanzo link</SizableText> })}
                    </SizableText>
                  </XStack>
                </YStack>
                <XStack pt="$1">
                  <Button size="sm" variant="outline" onPress={() => setAdding(true)}>
                    <Plus size={14} /> {t('Register one by hand')}
                  </Button>
                </XStack>
              </YStack>
            </Card>
          </YStack>
        ) : (
          <Card>
            {list.value.map((m, i) => {
              const display = displayMachine(m, { devMode, say: t, names })
              const showDetails = devMode ? !collapsedDevIds[m.id] : expandedId === m.id
              const toggleDetails = () => {
                if (devMode) {
                  setCollapsedDevIds((prev) => ({ ...prev, [m.id]: !prev[m.id] }))
                } else {
                  setExpandedId(expandedId === m.id ? null : m.id)
                }
              }
              const calmDetail = [
                said(state(m), t),
                m.running ? t('{n} active', { n: m.running }) : m.sessions ? t('{n} runs', { n: m.sessions }) : '',
              ]
                .filter(Boolean)
                .join(' · ')
              return (
                <YStack key={m.id} borderTopWidth={i > 0 ? 1 : 0} borderColor="$borderColor">
                  <Row
                    first={i === 0}
                    leading={
                      <XStack items="center" gap="$2">
                        <Dot m={m} />
                        <KindIcon kind={m.kind} size={16} />
                      </XStack>
                    }
                    title={display.title}
                    detail={devMode ? about(m, t) : calmDetail}
                    trailing={
                      <XStack items="center" gap="$2">
                        {!devMode && display.detail ? (
                          <TooltipProvider delay={150}>
                            <Tooltip placement="top" offset={6}>
                              <TooltipTrigger asChild>
                                <XStack
                                  render="button"
                                  aria-label={display.detail}
                                  p="$1"
                                  opacity={0.75}
                                >
                                  <Info size={14} color="$soft" />
                                </XStack>
                              </TooltipTrigger>
                              <TooltipContent
                                maxW={280}
                                px="$3"
                                py="$2"
                                z="var(--z-popover)"
                              >
                                <SizableText fontFamily="$body" size="$1" color="$ink">
                                  {display.detail}
                                </SizableText>
                              </TooltipContent>
                            </Tooltip>
                          </TooltipProvider>
                        ) : null}
                        <StatusBadge m={m} />
                        {m.serving ? (
                          <XStack px="$1.5" py="$0.5" rounded="$10" borderWidth={1} borderColor="$green10">
                            <SizableText size="$1" color="$green10">
                              {t('Serving')}
                            </SizableText>
                          </XStack>
                        ) : null}
                        {m.running > 0 ? (
                          <XStack px="$1.5" py="$0.5" rounded="$10" bg="$raised" items="center" gap="$1">
                            <Activity size={12} color="$green10" />
                            <SizableText size="$1" color="$ink">
                              {t('{n} active', { n: m.running })}
                            </SizableText>
                          </XStack>
                        ) : null}
                        {!devMode ? (
                          <Button size="sm" variant="ghost" onPress={toggleDetails}>
                            {showDetails ? <ChevronUp size={13} /> : <ChevronDown size={13} />}
                            {showDetails ? t('Hide details') : t('Advanced details')}
                          </Button>
                        ) : (
                          <CopyButton value={`hanzo link --target ${m.id}`} label={t('Copy link command for {name}', { name: display.title })} />
                        )}
                        <DropdownMenu
                          trigger={
                            <XStack render="button" aria-label={t('Actions for {name}', { name: display.title })} px="$1.5" py="$1" rounded="$2" hoverStyle={{ bg: '$hover' }}>
                              <MoreHorizontal size={16} />
                            </XStack>
                          }
                          items={[
                            { key: 'details', label: showDetails ? t('Hide advanced details') : t('Advanced details'), onSelect: toggleDetails },
                            { key: 'rename', label: t('Rename'), onSelect: () => setRenaming(m) },
                            ...(devMode || showDetails
                              ? [
                                  m.status === 'draining'
                                    ? { key: 'online', label: t('Take runs again'), onSelect: () => void drain(m, 'online') }
                                    : { key: 'drain', label: t('Drain'), description: t('Finish what it has, take nothing new'), onSelect: () => void drain(m, 'draining') },
                                  { key: 'key', label: t('Claim key'), onSelect: () => setKeying(m) },
                                ]
                              : []),
                            { type: 'separator' as const },
                            { key: 'remove', label: t('Remove'), destructive: true, onSelect: () => setRemoving(m) },
                          ]}
                        />
                      </XStack>
                    }
                  />

                  <Shared m={m} here={hostname !== '' && m.host === hostname} onChanged={list.reload} />

                  {/* Advanced details: hardware specs, live telemetry, keys & CLI flags (collapsed unless opened or Developer mode is ON) */}
                  {showDetails ? (
                    <YStack px="$4" pb="$3" pt="$1" gap="$2.5">
                      {/* Specs bar */}
                      {m.spec ? (
                        <XStack flexWrap="wrap" gap="$2" items="center">
                          {m.spec.os ? (
                            <XStack px="$2" py="$1" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor" items="center" gap="$1.5">
                              <Terminal size={12} />
                              <SizableText size="$1" color="$ink">
                                {m.spec.os}{m.spec.arch ? ` · ${m.spec.arch}` : ''}
                              </SizableText>
                            </XStack>
                          ) : null}
                          {m.spec.cpus ? (
                            <XStack px="$2" py="$1" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor" items="center" gap="$1.5">
                              <Cpu size={12} />
                              <SizableText size="$1" color="$ink">
                                {t('{n} cores', { n: m.spec.cpus })}
                              </SizableText>
                            </XStack>
                          ) : null}
                          {m.spec.memory ? (
                            <XStack px="$2" py="$1" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor" items="center" gap="$1.5">
                              <HardDrive size={12} />
                              <SizableText size="$1" color="$ink">
                                {t('{size} RAM', { size: formatBytes(m.spec.memory) })}
                              </SizableText>
                            </XStack>
                          ) : null}
                          {m.spec.gpus && m.spec.gpus.length > 0 ? (
                            <XStack px="$2" py="$1" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor" items="center" gap="$1.5">
                              <Zap size={12} color="$yellow10" />
                              <SizableText size="$1" color="$ink">
                                {m.spec.gpus.map((g) => `${g.model || g.vendor || 'GPU'}${g.memory ? ` (${formatBytes(g.memory)})` : ''}`).join(', ')}
                              </SizableText>
                            </XStack>
                          ) : null}
                        </XStack>
                      ) : null}

                      {/* Live Metrics */}
                      {m.metrics && (m.metrics.cpuUtil || m.metrics.load1 || m.metrics.memUsed || m.metrics.gpuUtil) ? (
                        <XStack flexWrap="wrap" gap="$3" p="$2.5" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor" items="center">
                          {m.metrics.cpuUtil ? (
                            <YStack gap="$0.5">
                              <SizableText size="$1" color="$soft">
                                {t('CPU Utilization')}
                              </SizableText>
                              <SizableText size="$2" color="$ink" fontWeight="600">
                                {formatPercent(m.metrics.cpuUtil)} {m.metrics.cpuTemp ? `· ${m.metrics.cpuTemp}°C` : ''}
                              </SizableText>
                            </YStack>
                          ) : m.metrics.load1 ? (
                            <YStack gap="$0.5">
                              <SizableText size="$1" color="$soft">
                                {t('Load (1m, 5m, 15m)')}
                              </SizableText>
                              <SizableText size="$2" color="$ink" fontWeight="600">
                                {m.metrics.load1}, {m.metrics.load5 ?? 0}, {m.metrics.load15 ?? 0}
                              </SizableText>
                            </YStack>
                          ) : null}

                          {m.metrics.memUsed ? (
                            <YStack gap="$0.5">
                              <SizableText size="$1" color="$soft">
                                {t('Memory')}
                              </SizableText>
                              <SizableText size="$2" color="$ink" fontWeight="600">
                                {m.spec?.memory ? `${formatBytes(m.metrics.memUsed)} / ${formatBytes(m.spec.memory)}` : t('{size} used', { size: formatBytes(m.metrics.memUsed) })}
                              </SizableText>
                            </YStack>
                          ) : null}

                          {m.metrics.gpuUtil !== undefined && m.metrics.gpuUtil > 0 ? (
                            <YStack gap="$0.5">
                              <SizableText size="$1" color="$soft">
                                {t('GPU')}
                              </SizableText>
                              <SizableText size="$2" color="$ink" fontWeight="600">
                                {formatPercent(m.metrics.gpuUtil)} {m.metrics.gpuTemp ? `· ${m.metrics.gpuTemp}°C` : ''} {m.metrics.gpuPower ? `· ${m.metrics.gpuPower}W` : ''}
                              </SizableText>
                            </YStack>
                          ) : null}

                          {m.metrics.model ? (
                            <YStack gap="$0.5">
                              <SizableText size="$1" color="$soft">
                                {t('Serving Model')}
                              </SizableText>
                              <SizableText size="$2" color="$ink" fontWeight="600">
                                {m.metrics.model} {m.metrics.decode ? `(${m.metrics.decode} tok/s)` : ''}
                              </SizableText>
                            </YStack>
                          ) : null}
                        </XStack>
                      ) : null}

                      {/* Quick Commands & Advanced Actions */}
                      <XStack justify="space-between" items="center" flexWrap="wrap" gap="$2" pt="$0.5">
                        <XStack items="center" gap="$3" flexWrap="wrap">
                          <SizableText size="$1" color="$soft" fontFamily="$mono">
                            {t('ID: {id}', { id: m.id })}
                          </SizableText>
                          {m.host ? (
                            <SizableText size="$1" color="$soft" fontFamily="$mono">
                              {m.host}
                            </SizableText>
                          ) : null}
                          {m.seen ? (
                            <SizableText size="$1" color="$soft">
                              {t('Last heartbeat: {when}', { when: formatRelative(m.seen) })}
                            </SizableText>
                          ) : null}
                        </XStack>
                        <XStack items="center" gap="$2" flexWrap="wrap">
                          <Button size="sm" variant="outline" onPress={() => setKeying(m)}>
                            {t('Claim key')}
                          </Button>
                          <Button
                            size="sm"
                            variant="outline"
                            onPress={() => void drain(m, m.status === 'draining' ? 'online' : 'draining')}
                          >
                            {m.status === 'draining' ? t('Take runs again') : t('Drain')}
                          </Button>
                          <XStack items="center" gap="$1" px="$2" py="$0.5" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor">
                            <SizableText size="$1" color="$ink" {...{ 'data-i18n-ltr': '' }} fontFamily="$mono">
                              hanzo run --target {m.id}
                            </SizableText>
                            <CopyButton value={`hanzo run --target ${m.id}`} label={t('Copy run command for {name}', { name: display.title })} />
                          </XStack>
                        </XStack>
                      </XStack>
                    </YStack>
                  ) : null}
                </YStack>
              )
            })}
          </Card>
        )}
        {list.error && list.value !== UNREAD ? <Soft>{told(list.error, t)}</Soft> : null}
      </Group>
      <Note>{note}</Note>

      <Adding
        open={adding}
        onOpenChange={setAdding}
        onAdded={(m) => {
          setNote(t('{name} is registered', { name: m.label }))
          list.reload()
        }}
      />
      <Rename
        open={renaming !== null}
        onOpenChange={(o) => !o && setRenaming(null)}
        title={t('Rename machine')}
        name={renaming ? displayMachine(renaming, { devMode, say: t, names }).title : ''}
        // Each dialog acts only while it is open, which is while its machine is chosen. The name is the person's, kept on
        // their account (names.ts): the machine's own record is renamed back to its hostname by its next heartbeat.
        run={async (label) => {
          // Typed back to the name it has by default, it keeps no name of the person's.
          const own = displayMachine(renaming!, { devMode, say: t }).title
          await rename(machineKey(renaming!), label === own ? '' : label, target)
          setNote(t('Renamed to {name}', { name: label }))
        }}
      />
      <Confirm
        open={removing !== null}
        onOpenChange={(o) => !o && setRemoving(null)}
        title={removing ? t('Remove {name}?', { name: displayMachine(removing, { devMode, say: t, names }).title }) : t('Remove this machine?')}
        says={t('Runs can no longer be sent to it, and its claim key stops working. Linking it again registers it anew.')}
        act={t('Remove')}
        run={async () => {
          await remove(target, removing!.id)
          setNote(t('{name} is removed', { name: displayMachine(removing!, { devMode, say: t, names }).title }))
          list.reload()
        }}
      />
      <Confirm
        open={keying !== null}
        onOpenChange={(o) => !o && setKeying(null)}
        title={keying ? t('Mint a claim key for {name}?', { name: displayMachine(keying, { devMode, say: t, names }).title }) : t('Mint a claim key for this machine?')}
        says={t('A key it had before stops working, so a runner using it stops getting runs until it has the new one. The new key is shown once.')}
        act={t('Mint key')}
        run={async () => {
          const k = await key(target, keying!.id)
          setMinted({ machine: keying!, key: k })
        }}
      />
      <Dialog open={minted !== null} onOpenChange={(o) => !o && setMinted(null)}>
        <DialogContent maxW={480} showCloseButton={false}>
          <DialogTitle>{t('Claim key for {name}', { name: minted ? displayMachine(minted.machine, { devMode, say: t, names }).title : '' })}</DialogTitle>
          {minted ? (
            <YStack gap="$3">
              <Once value={minted.key} label={t('Claim key')} copy={t('Copy claim key')}>
                <SizableText size="$1" color="$soft">
                  {t('A runner on this machine sends it as {header} when it claims the runs sent here, at {route}.', {
                    header: 'X-Target-Key',
                    route: `POST /v1/agent/targets/${minted.machine.id}/claim`,
                  })}
                </SizableText>
              </Once>

              <YStack gap="$1" p="$2.5" rounded="$2" bg="$raised" borderWidth={1} borderColor="$borderColor">
                <SizableText size="$1" color="$ink" fontWeight="600">
                  {t('Start background worker with this key:')}
                </SizableText>
                <XStack items="center" justify="space-between" px="$2" py="$1" rounded="$2" bg="$raised">
                  <SizableText size="$1" color="$ink" {...{ 'data-i18n-ltr': '' }} fontFamily="$mono">
                    export HANZO_TARGET_KEY="{minted.key}"
                  </SizableText>
                  <CopyButton value={`export HANZO_TARGET_KEY="${minted.key}"\nhanzo code --serve --target ${minted.machine.id}`} label={t('Copy key command')} />
                </XStack>
              </YStack>
            </YStack>
          ) : null}
          <XStack justify="flex-end" pt="$2">
            <Button size="sm" onPress={() => setMinted(null)}>
              {t('Done')}
            </Button>
          </XStack>
        </DialogContent>
      </Dialog>
    </YStack>
  )
}

/** Registering a machine by hand: its name, what it is, and the hostname its runs report. */
function Adding({ open, onOpenChange, onAdded }: { open: boolean; onOpenChange: (o: boolean) => void; onAdded: (m: Machine) => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const [label, setLabel] = useState('')
  const [kind, setKind] = useState<Kind>('machine')
  const [hostname, setHostname] = useState('')
  const [capacity, setCapacity] = useState('')
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')

  const close = () => {
    if (working) return
    setLabel('')
    setKind('machine')
    setHostname('')
    setCapacity('')
    setNote('')
    onOpenChange(false)
  }

  const save = async () => {
    setWorking(true)
    setNote('')
    try {
      const targetLabel = label.trim() || hostname.trim()
      if (!targetLabel) throw new Error(phrase('A machine needs a name or a hostname'))
      const m = await add(target, { label: targetLabel, kind, host: hostname, capacity: capacity.trim() || undefined })
      setWorking(false)
      onAdded(m)
      close()
    } catch (e) {
      setNote(told(e, t))
      setWorking(false)
    }
  }

  return (
    <Dialog open={open} onOpenChange={(o) => !o && close()}>
      <DialogContent maxW={460} showCloseButton={false}>
        <DialogTitle>{t('Register a machine')}</DialogTitle>
        <YStack gap="$3">
          <Field label={t('Name')} hint={t('Left empty, it takes the hostname.')}>
            <Input value={label} onChangeText={setLabel} aria-label={t('Machine name')} autoFocus />
          </Field>
          <Field label={t('Kind')}>
            <XStack flexWrap="wrap" gap="$1.5" role="radiogroup" aria-label={t('Kind')}>
              {KINDS.map((k) => (
                <XStack
                  key={k}
                  render="button"
                  role="radio"
                  aria-checked={k === kind}
                  aria-label={kindOf(k, t)}
                  onPress={() => setKind(k)}
                  px="$2.5"
                  py="$1"
                  rounded="$10"
                  borderWidth={1}
                  borderColor={k === kind ? '$ink' : '$borderColor'}
                  items="center"
                  gap="$1.5"
                >
                  <KindIcon kind={k} size={14} />
                  <SizableText size="$1" color={k === kind ? '$ink' : '$soft'}>
                    {kindOf(k, t)}
                  </SizableText>
                </XStack>
              ))}
            </XStack>
          </Field>
          <Field label={t('Hostname')} hint={t('Optional. What its runs report. {command}, signed in as you on a machine with this hostname, takes this row over.', { command: 'hanzo link' })}>
            <Input value={hostname} onChangeText={setHostname} aria-label={t('Hostname')} autoCapitalize="none" />
          </Field>
          <Field label={t('Capacity / Hardware')} hint={t('Optional human summary (e.g. {example}).', { example: '16 vCPU / 64G / 1× RTX 4090' })}>
            <Input value={capacity} onChangeText={setCapacity} aria-label={t('Capacity')} placeholder={t('e.g. {example}', { example: '16 vCPU / 64G / 1× RTX 4090' })} />
          </Field>
          {note ? (
            <SizableText size="$1" color="$soft" role="status">
              {note}
            </SizableText>
          ) : null}
          <XStack gap="$2" justify="flex-end">
            <Button size="sm" variant="ghost" disabled={working} onPress={close}>
              {t('Cancel')}
            </Button>
            <Button size="sm" disabled={working} onPress={() => void save()}>
              {t('Register')}
            </Button>
          </XStack>
        </YStack>
      </DialogContent>
    </Dialog>
  )
}
