/**
 * Plugins: TypeScript connectors the org builds for the runtime to run.
 *
 * Browse offers building one, then lists what this deployment mounts, which is
 * built in and read-only. Yours is what the org built, each with the source that
 * runs. Building is the gate — the platform bundles and compiles the source and
 * keeps it only if both succeed, and a failure says why in the bundler's words.
 * Describe an API instead of writing the TypeScript and a model writes it; the
 * result opens to be read. A plugin carries no credential: it names the
 * connector whose credential it reads when it runs.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a plugin's name, its
 * source and the bundler's reasons are drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { AlertCircle, Check, Code2, Puzzle, Trash2 } from '@hanzogui/lucide-icons-2'
import { Button, DropdownMenu, Input, Textarea } from '@hanzo/ui'
import { Code } from '@hanzo/ui/chat'
import { useState } from 'react'

import { authored, build, mounted, remove, type Mount, type Plugin } from '../api/plugins.ts'
import { useHost, useTarget } from '../host.tsx'
import { useLoad } from './load.ts'
import { say } from './say.ts'
import { Choice, day, Empty, Failed, Field, Grid, Mark, matches, MenuTrigger, Part, Sheet, Soft, Tile, TilesSkeleton, type Pane, useCount, Visitor } from './ui.tsx'
import { Confirm } from '../ask.tsx'

const INITIAL_MOUNTS = 6

const kb = (n: number): string => (n < 1024 ? `${n} B` : `${(n / 1024).toFixed(1)} KB`)

interface Notice {
  text: string
  error?: boolean
}

function StatusBanner({ notice }: { notice: Notice | null }) {
  if (!notice?.text) return null
  return (
    <XStack items="center" gap="$1.5" role={notice.error ? 'alert' : 'status'}>
      {notice.error ? <AlertCircle size={14} color="var(--state-error)" aria-hidden /> : <Check size={14} color="var(--state-success)" aria-hidden />}
      <SizableText size="$1" color={notice.error ? 'var(--state-error)' : '$soft'} flex={1} minW={0}>
        {notice.text}
      </SizableText>
    </XStack>
  )
}

export function Plugins({ view, q, adding, onAdding, onView, onCount }: Pane) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const mine = useLoad(signed ? () => authored(target) : null, [] as Plugin[], [target, signed])
  const mounts = useLoad(signed && view === 'discover' ? () => mounted(target) : null, [] as Mount[], [target, signed, view])
  const [opened, setOpened] = useState<Plugin | null>(null)
  const [deleting, setDeleting] = useState<Plugin | null>(null)
  const [mountDetail, setMountDetail] = useState<Mount | null>(null)
  const [showAllMounts, setShowAllMounts] = useState(false)
  const [notice, setNotice] = useState<Notice | null>(null)
  useCount(mine.value.length, onCount)

  const sheets = (
    <>
      {adding ? (
        <Build
          onClose={() => onAdding(false)}
          onBuilt={(p, said) => {
            onAdding(false)
            onView('yours')
            mine.reload()
            setNotice({ text: said })
            setOpened(p)
          }}
        />
      ) : null}
      {opened ? (
        <Source
          plugin={opened}
          onClose={() => setOpened(null)}
          onDeleted={() => {
            setNotice({ text: t('{name} is deleted', { name: opened.name }) })
            setOpened(null)
            mine.reload()
          }}
        />
      ) : null}
      {deleting ? (
        <Confirm
          title={t('Delete {name}?', { name: deleting.name })}
          act={t('Delete')}
          says={t('The runtime can no longer load it.')}
          open
          onOpenChange={(o) => !o && setDeleting(null)}
          run={async () => {
            const gone = deleting
            await remove(target, gone.id)
            setDeleting(null)
            setNotice({ text: t('{name} is deleted', { name: gone.name }) })
            mine.reload()
          }}
        />
      ) : null}
      {mountDetail ? <MountSheet mount={mountDetail} onClose={() => setMountDetail(null)} /> : null}
    </>
  )

  if (!signed) {
    return <Visitor>{view === 'discover' ? t('Sign in to browse plugins.') : t('Sign in to see your plugins.')}</Visitor>
  }

  if (view === 'discover') {
    const shown = mounts.value.filter((m) => matches(q, m.name, ...m.prefixes))
    const searching = Boolean(q.trim())
    const visible = searching || showAllMounts ? shown : shown.slice(0, INITIAL_MOUNTS)
    return (
      <YStack gap="$6">
        <StatusBanner notice={notice} />
        <XStack
          items="center"
          gap="$3"
          p="$4"
        >
          <Mark name={t('Build your own')} size={36} icon={<Code2 size={16} color="$soft" aria-hidden />} />
          <YStack flex={1} minW={0} gap="$0.5" $max-sm={{ flexBasis: 'auto', grow: 0, minW: 0, width: '100%' }}>
            <SizableText size="$3" fontWeight="500" color="$ink">
              {t('Build your own')}
            </SizableText>
            <SizableText size="$2" color="$soft">
              {t('Write a connector in TypeScript, or describe an API and Hanzo writes it.')}
            </SizableText>
          </YStack>
          <Button size="sm" variant="outline" borderColor="var(--border-strong)" shrink={0} onPress={() => onAdding(true)}>
            {t('Build a plugin')}
          </Button>
        </XStack>
        <Part title={t('Built in')} detail={t('Part of Hanzo. Always on, nothing to add.')}>
          {mounts.error && !mounts.value.length ? (
            <Failed error={mounts.error} onRetry={mounts.reload} />
          ) : mounts.loading && !mounts.value.length ? (
            <TilesSkeleton label={t('Loading built-in plugins')} />
          ) : !shown.length ? (
            <Soft>{mounts.value.length ? t('Nothing built in matches “{q}”.', { q: q.trim() }) : t('Nothing built in is listed.')}</Soft>
          ) : (
            <YStack gap="$3">
              <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
                {t('{n} built in', { n: shown.length })}
              </SizableText>
              <Grid label={t('Mounted subsystems')}>
                {visible.map((m) => (
                  <Tile
                    key={m.name}
                    title={m.name}
                    detail={m.prefixes.join(' · ')}
                    meta={m.enabled ? t('On') : t('Off')}
                    mark={<Mark name={m.name} icon={<Puzzle size={15} color="$soft" aria-hidden />} />}
                    onOpen={() => setMountDetail(m)}
                  />
                ))}
              </Grid>
              {!searching && shown.length > INITIAL_MOUNTS ? (
                <XStack justify="flex-start">
                  <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => setShowAllMounts((v) => !v)}>
                    {showAllMounts ? t('Show fewer') : t('Show all {n} built-in plugins', { n: shown.length })}
                  </Button>
                </XStack>
              ) : null}
            </YStack>
          )}
        </Part>
        {sheets}
      </YStack>
    )
  }

  const shown = mine.value.filter((p) => matches(q, p.name, p.provider))
  return (
    <YStack gap="$3">
      <StatusBanner notice={notice} />
      {mine.error && !mine.value.length ? (
        <Failed error={mine.error} onRetry={mine.reload} />
      ) : mine.loading && !mine.value.length ? (
        <TilesSkeleton label={t('Loading your plugins')} count={4} />
      ) : !mine.value.length ? (
        <Empty title={t('No plugins yet')} detail={t('Write a connector in TypeScript, or describe an API and Hanzo writes it.')}>
          <Button size="sm" onPress={() => onAdding(true)}>
            {t('Build a plugin')}
          </Button>
        </Empty>
      ) : !shown.length ? (
        <Soft>{t('No plugin of yours matches “{q}”.', { q: q.trim() })}</Soft>
      ) : (
        <Grid label={t('Your plugins')}>
          {shown.map((p) => (
            <Tile
              key={p.id}
              title={p.name}
              detail={p.provider ? t('Reads the {provider} connector’s credential when it runs.', { provider: p.provider }) : t('Needs no credential.')}
              meta={day(p.built) ? t('Built {date}', { date: day(p.built) }) : undefined}
              mark={<Mark name={p.name} icon={<Code2 size={15} color="$soft" aria-hidden />} />}
              onOpen={() => setOpened(p)}
              action={
                <DropdownMenu
                  placement="bottom-end"
                  offset={6}
                  trigger={<MenuTrigger label={t('Actions for {name}', { name: p.name })} />}
                  items={[
                    { key: 'source', label: t('View source'), icon: <Code2 size={15} />, onSelect: () => setOpened(p) },
                    { type: 'separator' as const },
                    { key: 'delete', label: t('Delete plugin'), icon: <Trash2 size={15} />, destructive: true, onSelect: () => setDeleting(p) },
                  ]}
                />
              }
            />
          ))}
        </Grid>
      )}
      {sheets}
    </YStack>
  )
}

/** Read-only inspection sheet for a built-in mounted subsystem. */
function MountSheet({ mount, onClose }: { mount: Mount; onClose: () => void }) {
  const { t } = useI18n()
  return (
    <Sheet title={mount.name} open onOpenChange={(o) => !o && onClose()} width={480}>
      <XStack items="center" gap="$2">
        <YStack width={8} height={8} rounded={999} bg={mount.enabled ? 'var(--state-success)' : '$soft'} shrink={0} aria-hidden />
        <SizableText size="$2" color="$soft">
          {mount.enabled ? t('Built in and always on for every run.') : t('Built in, currently off in this deployment.')}
        </SizableText>
      </XStack>
      <Field label={t('Route prefixes')}>
        <YStack borderWidth={1} borderColor="$borderColor" bg="$panel" rounded="$3" overflow="hidden">
          {mount.prefixes.length ? (
            mount.prefixes.map((prefix, i) => (
              <XStack key={prefix} px="$3" py="$2" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                <SizableText size="$1" color="$ink" fontFamily="$mono">
                  {prefix}
                </SizableText>
              </XStack>
            ))
          ) : (
            <XStack px="$3" py="$2">
              <SizableText size="$1" color="$soft">
                {t('No route prefixes listed.')}
              </SizableText>
            </XStack>
          )}
        </YStack>
      </Field>
    </Sheet>
  )
}

/** Build a plugin from its TypeScript, or from a description of the API it calls. */
function Build({ onClose, onBuilt }: { onClose: () => void; onBuilt: (p: Plugin, said: string) => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const [name, setName] = useState('')
  const [provider, setProvider] = useState('')
  const [way, setWay] = useState<'source' | 'spec'>('source')
  // Each way keeps its own text, so switching between them loses neither.
  const [source, setSource] = useState('')
  const [spec, setSpec] = useState('')
  const [working, setWorking] = useState(false)
  const [notice, setNotice] = useState<Notice | null>(null)

  const body = way === 'source' ? source : spec
  const canBuild = !working && Boolean(name.trim()) && Boolean(body.trim())

  const go = async () => {
    setWorking(true)
    setNotice({ text: way === 'spec' ? t('Writing and building it…') : t('Building it…') })
    try {
      const out = await build(target, { name: name.trim(), provider, source: way === 'source' ? source : '', spec: way === 'spec' ? spec : '' })
      const built = { name: out.plugin.name, size: kb(out.bytes) }
      onBuilt(out.plugin, out.generated ? t('{name} built: {size}, written from your description — read it below.', built) : t('{name} built: {size}.', built))
    } catch (e) {
      setNotice({ text: say(e, t), error: true })
    } finally {
      setWorking(false)
    }
  }

  return (
    <Sheet title={t('Build a plugin')} open onOpenChange={(o) => !o && onClose()} width={680}>
      <Field label={t('Name')} hint={t('One lowercase word: letters, digits, _ or -.')}>
        <Input value={name} onChangeText={setName} placeholder="acme-crm" aria-label={t('Name')} autoCapitalize="none" borderColor="var(--border-strong)" />
      </Field>
      <Field label={t('Connector')} hint={t('Optional. The connector whose key it uses when it runs; a plugin never holds a key itself.')}>
        <Input value={provider} onChangeText={setProvider} placeholder="acme" aria-label={t('Connector')} autoCapitalize="none" borderColor="var(--border-strong)" />
      </Field>
      <Choice
        label={t('Build from')}
        value={way}
        options={[
          { id: 'source', label: 'TypeScript' },
          { id: 'spec', label: t('Describe an API') },
        ]}
        onChange={setWay}
      />
      <Field
        label={way === 'source' ? t('Source') : t('The API')}
        hint={
          way === 'source'
            ? t('Kept only if it builds. A failed build says why.')
            : t('An OpenAPI document, or the endpoints in plain words. You read the TypeScript before it runs.')
        }
      >
        <Textarea
          value={body}
          onChangeText={way === 'source' ? setSource : setSpec}
          aria-label={way === 'source' ? t('Source') : t('The API')}
          placeholder={way === 'source' ? 'export const acme = createProvider({ … })' : t('POST /v1/things creates a thing…')}
          rows={14}
          fontFamily="$mono"
        />
      </Field>
      {notice ? (
        notice.error ? (
          <StatusBanner notice={notice} />
        ) : (
          <SizableText size="$1" color="$soft" role="status">
            {notice.text}
          </SizableText>
        )
      ) : null}
      <XStack gap="$2" justify="flex-end">
        <Button size="sm" variant="ghost" onPress={onClose}>
          {t('Cancel')}
        </Button>
        <Button size="sm" variant="primary" disabled={!canBuild} onPress={() => void go()}>
          {working ? (way === 'spec' ? t('Writing & building…') : t('Building…')) : t('Build')}
        </Button>
      </XStack>
    </Sheet>
  )
}

/** A built plugin's source, as the runtime will run it, and deleting it. */
function Source({ plugin, onClose, onDeleted }: { plugin: Plugin; onClose: () => void; onDeleted: () => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const [asking, setAsking] = useState(false)
  return (
    <Sheet title={plugin.name} open onOpenChange={(o) => !o && onClose()} width={760}>
      <SizableText size="$2" color="$soft">
        {[
          plugin.provider ? t('Reads the {provider} connector’s credential when it runs.', { provider: plugin.provider }) : t('Needs no credential.'),
          day(plugin.built) ? t('Built {date}.', { date: day(plugin.built) }) : '',
        ]
          .filter(Boolean)
          .join(' ')}
      </SizableText>
      <Code language="TypeScript" value={plugin.source}>
        <SizableText size="$1" color="$ink" fontFamily="$mono" $platform-web={{ whiteSpace: 'pre' }}>
          {plugin.source}
        </SizableText>
      </Code>
      <YStack items="flex-start">
        <Button size="sm" variant="ghost" tone="danger" onPress={() => setAsking(true)}>
          <Trash2 size={14} /> {t('Delete plugin')}
        </Button>
      </YStack>
      <Confirm
        title={t('Delete {name}?', { name: plugin.name })}
        act={t('Delete')}
        says={t('The runtime can no longer load it.')}
        open={asking}
        onOpenChange={setAsking}
        run={async () => {
          await remove(target, plugin.id)
          onDeleted()
        }}
      />
    </Sheet>
  )
}
