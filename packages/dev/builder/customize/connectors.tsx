/**
 * Connectors: MCP servers whose tools an agent calls.
 *
 * Browse is the shelf (our copy of the public MCP registry), then the fleet's
 * own servers, which every run can call and nothing adds; that list is read
 * only when asked for, since listing it asks every subsystem at once. A listing
 * is added here only when it serves streamable HTTP; one that ships only a
 * package needs somewhere to run it first, so it has no Add. Yours is the
 * servers the org added — by URL, or off the shelf — each with one switch for
 * all its tools on its card and one per tool inside; a tool is called only
 * while it is on. Adding a server switches its tools on. A server's secret is
 * sealed in KMS and never shown again.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a server's name,
 * address, description and tools are the registry's or the org's, drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { AlertCircle, Check, ChevronDown, ChevronUp, ExternalLink, KeyRound, Plug, Search, Server as ServerIcon, SlidersHorizontal, Trash2, X } from '@hanzogui/lucide-icons-2'
import { Button, DropdownMenu, Input, Switch } from '@hanzo/ui'
import { useEffect, useMemo, useState } from 'react'

import { add, one, owns, ready, remove, servers, shelf, titleOf, type Adding, type Listing, type Page, type Server } from '../api/connectors.ts'
import { natives, type Native } from '../api/mcp.ts'
import { toggle, tools, type Tool } from '../api/tools.ts'
import { RING, RING_INSET } from '../pane.ts'
import { useHost, useTarget } from '../host.tsx'
import { Out } from '../out.tsx'
import { route } from '../route.ts'
import { useLoad } from './load.ts'
import { say } from './say.ts'
import { Add, Empty, Failed, Featured, Field, FilterPills, Grid, Mark, matches, MenuTrigger, Part, RowsSkeleton, Sheet, Soft, Tile, TilesSkeleton, type Pane, useCount, Visitor } from './ui.tsx'
import { Confirm } from '../ask.tsx'

const PAGE = 48
const BUILTIN_INITIAL = 6


type ShelfFilter = 'all' | 'hosted' | 'official'

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

/** The part of a URL a person reads: its host and path. */
const where = (url: string): string => url.replace(/^https?:\/\//, '').replace(/\/$/, '')

export function Connectors({ view, q, adding, onAdding, onView, onCount }: Pane) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  // What every run in the org connects to is an org admin's to change; a member reads it.
  const may = signed && host.admin
  const mine = useLoad(signed ? () => servers(target) : null, [] as Server[], [target, signed])
  // Every server's tools, for the switch on each card; read once for all of them.
  const plane = useLoad(signed && view === 'yours' && mine.value.length ? () => tools(target, { source: 'mcp' }) : null, [] as Tool[], [target, signed, view, mine.value])
  const [opened, setOpened] = useState<Server | null>(null)
  const [removing, setRemoving] = useState<Server | null>(null)
  const [listing, setListing] = useState<Listing | null>(null)
  const [connecting, setConnecting] = useState<Listing | null>(null)
  const [busy, setBusy] = useState('')
  const [notice, setNotice] = useState<Notice | null>(null)
  useCount(mine.value.length, onCount)

  /** Switch every tool a server brings on or off at once, then read them again. */
  const flip = async (s: Server, next: boolean) => {
    const its = plane.value.filter((x) => owns(s, x.name)).map((x) => x.name)
    if (!its.length) return
    setBusy(s.id)
    setNotice(null)
    try {
      await toggle(target, next ? its : [], next ? [] : its)
      plane.reload()
      setNotice({ text: next ? t('{name} is on', { name: s.name }) : t('{name} is off', { name: s.name }) })
    } catch (e) {
      setNotice({ text: say(e, t), error: true })
    } finally {
      setBusy('')
    }
  }

  const connect =
    adding || connecting ? (
      <Connect
        listing={connecting}
        onClose={() => {
          setConnecting(null)
          onAdding(false)
        }}
        onAdded={(said) => {
          setConnecting(null)
          onAdding(false)
          mine.reload()
          onView('yours')
          setNotice({ text: said })
        }}
      />
    ) : null
  const detail = listing ? (
    <About
      listing={listing}
      added={mine.value.some((s) => s.listing === listing.id)}
      signed={may}
      onAdd={() => {
        setConnecting(listing)
        setListing(null)
      }}
      onClose={() => setListing(null)}
    />
  ) : null
  const confirmRemove = removing ? (
    <Confirm
      title={t('Delete {name}?', { name: removing.name })}
      act={t('Delete')}
      says={t('Its tools leave your organization and every agent, and its sealed secret is destroyed.')}
      open
      onOpenChange={(o) => !o && setRemoving(null)}
      run={async () => {
        const gone = removing
        await remove(target, gone.id)
        setRemoving(null)
        mine.reload()
        setNotice({ text: t('{name} is removed', { name: gone.name }) })
      }}
    />
  ) : null

  if (view === 'discover') {
    return (
      <YStack gap="$6">
        <StatusBanner notice={notice} />
        <Shelf
          q={q}
          signed={signed}
          may={may}
          added={new Set(mine.value.map((s) => s.listing).filter(Boolean))}
          onOpen={setListing}
          onAdd={setConnecting}
        />
        <Natives q={q} />
        {detail}
        {connect}
      </YStack>
    )
  }

  if (!signed) {
    return <Visitor>{t('Sign in to see your connectors.')}</Visitor>
  }
  const shown = mine.value.filter((s) => matches(q, s.name, s.url, s.listing))
  return (
    <YStack gap="$3">
      <StatusBanner notice={notice} />
      {mine.error && !mine.value.length ? (
        <Failed error={mine.error} onRetry={mine.reload} />
      ) : mine.loading && !mine.value.length ? (
        <TilesSkeleton label={t('Loading your connectors')} count={4} />
      ) : !mine.value.length ? (
        <Empty title={t('No connectors yet')} detail={may ? t('Connect an app from Browse, or add any MCP server by its URL.') : t('An org admin adds connectors. Browse what there is.')}>
          <Button size="sm" variant="outline" onPress={() => onView('discover')}>
            {t('Browse connectors')}
          </Button>
          {may ? (
            <Button size="sm" onPress={() => onAdding(true)}>
              {t('Add by URL')}
            </Button>
          ) : null}
        </Empty>
      ) : !shown.length ? (
        <Soft>{t('No connector of yours matches “{q}”.', { q: q.trim() })}</Soft>
      ) : (
        <Grid label={t('Your connectors')}>
          {shown.map((s) => {
            const its = plane.value.filter((x) => owns(s, x.name))
            const lit = its.filter((x) => x.activated).length
            return (
              <Tile
                key={s.id}
                title={s.name}
                detail={where(s.url)}
                meta={[
                  its.length ? t('{on} of {n} tools on', { on: lit, n: its.length }) : plane.loading ? '' : t('No tools listed yet'),
                  s.listing ? t('From Browse') : t('Added by URL'),
                  s.secret ? t('Secret in KMS') : '',
                  s.admitted ? '' : t('Not in runs until an admin adds it again'),
                ]
                  .filter(Boolean)
                  .join(' · ')}
                mark={<Mark name={s.name} icon={<Plug size={15} color="$soft" aria-hidden />} />}
                onOpen={() => setOpened(s)}
                action={
                  <XStack items="center" gap="$1.5" shrink={0}>
                    {may && its.length ? (
                      <Switch checked={lit > 0} disabled={busy === s.id} onCheckedChange={(v: boolean) => void flip(s, v)} aria-label={t('{name} on', { name: s.name })} />
                    ) : its.length ? (
                      <SizableText size="$1" color="$soft">
                        {lit ? t('On') : t('Off')}
                      </SizableText>
                    ) : null}
                    <DropdownMenu
                      placement="bottom-end"
                      offset={6}
                      trigger={<MenuTrigger label={t('Actions for {name}', { name: s.name })} />}
                      items={[
                        { key: 'inspect', label: t('Inspect tools'), icon: <SlidersHorizontal size={15} />, onSelect: () => setOpened(s) },
                        ...(may && its.length
                          ? [
                              {
                                key: 'toggle',
                                label: lit > 0 ? t('Turn all off') : t('Turn all on'),
                                icon: <Check size={15} />,
                                disabled: busy === s.id,
                                onSelect: () => void flip(s, lit === 0),
                              },
                            ]
                          : []),
                        ...(may
                          ? [
                              { type: 'separator' as const },
                              { key: 'remove', label: t('Remove connector'), icon: <Trash2 size={15} />, destructive: true, onSelect: () => setRemoving(s) },
                            ]
                          : []),
                      ]}
                    />
                  </XStack>
                }
              />
            )
          })}
        </Grid>
      )}
      {plane.error && mine.value.length ? <StatusBanner notice={{ text: t('Tools: {reason}', { reason: say(plane.error, t) }), error: true }} /> : null}
      {opened ? (
        <Detail
          server={opened}
          may={may}
          onClose={() => {
            setOpened(null)
            plane.reload()
          }}
          onDeleted={() => {
            setNotice({ text: t('{name} is removed', { name: opened.name }) })
            setOpened(null)
            mine.reload()
          }}
        />
      ) : null}
      {confirmRemove}
      {connect}
    </YStack>
  )
}

/** The shelf: featured first, then by name, a page at a time. The platform searches it. */
function Shelf({ q, signed, may, added, onOpen, onAdd }: { q: string; signed: boolean; may: boolean; added: Set<string>; onOpen: (l: Listing) => void; onAdd: (l: Listing) => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const text = useSettled(q.trim(), 300)
  const first = useLoad(signed ? () => shelf(target, { text, limit: PAGE }) : null, null as Page | null, [target, signed, text])
  const [more, setMore] = useState<Listing[]>([])
  const [filter, setFilter] = useState<ShelfFilter>('all')
  const [paging, setPaging] = useState(false)
  const [note, setNote] = useState('')
  useEffect(() => setMore([]), [first.value])

  if (!signed) {
    return (
      <Part title={t('Connect an app')} detail={t('Give runs the tools you already use.')}>
        <Visitor>{t('Sign in to browse connectors.')}</Visitor>
      </Part>
    )
  }
  const page = first.value
  const loaded = [...(page?.listings ?? []), ...more]
  const list = loaded.filter((l) => (filter === 'hosted' ? ready(l) : filter === 'official' ? l.official : true))
  const hero = !text && filter === 'all' ? list.find((l) => l.featured) : undefined
  const rest = hero ? list.filter((l) => l !== hero) : list
  // A listing that ships only as a package has no Add: its card says it needs a place to run.
  const action = (l: Listing) => (may && ready(l) ? <Add name={titleOf(l)} added={added.has(l.id)} onPress={() => onAdd(l)} /> : undefined)

  const next = async () => {
    setPaging(true)
    setNote('')
    try {
      const got = await shelf(target, { text, limit: PAGE, offset: loaded.length })
      setMore((m) => [...m, ...got.listings])
    } catch (e) {
      setNote(say(e, t))
    } finally {
      setPaging(false)
    }
  }

  return (
    <Part title={t('Connect an app')} detail={t('Give runs the tools you already use. Adding one turns its tools on for your organization.')}>
      {first.error && !page ? (
        <Failed error={first.error} onRetry={first.reload} />
      ) : !page ? (
        <TilesSkeleton label={t('Loading connectors')} withHero />
      ) : (
        <YStack gap="$3">
          {loaded.length > 0 ? (
            <FilterPills<ShelfFilter>
              label={t('Filter connectors')}
              value={filter}
              options={[
                { id: 'all', label: t('All') },
                { id: 'hosted', label: t('Hosted') },
                { id: 'official', label: t('Official') },
              ]}
              onChange={setFilter}
              maxVisibleDesktop={6}
              mobileLimit={4}
            />
          ) : null}
          {!list.length ? (
            <Soft
              action={
                filter !== 'all' ? (
                  <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => setFilter('all')}>
                    {t('Show all connectors')}
                  </Button>
                ) : undefined
              }
            >
              {text
                ? t('No connector matches “{q}”.', { q: text })
                : filter !== 'all'
                  ? t('No connectors in this view on the loaded page.')
                  : t('There are no connectors to add yet.')}
            </Soft>
          ) : (
            <>
              {hero ? (
                <Featured
                  title={titleOf(hero)}
                  detail={hero.description}
                  meta={facts(hero, t)}
                  mark={<Mark name={titleOf(hero)} logo={hero.logo} size={48} />}
                  onOpen={() => onOpen(hero)}
                  action={action(hero)}
                />
              ) : null}
              <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
                {filter !== 'all'
                  ? t('{shown} matching in {loaded} loaded', { shown: list.length, loaded: loaded.length })
                  : loaded.length < page.total
                    ? t('{shown} of {n} connectors', { shown: loaded.length, n: page.total })
                    : t('{n} connectors', { n: page.total })}
              </SizableText>
              <Grid label={t('Servers on the shelf')}>
                {rest.map((l) => (
                  <Tile key={l.id} title={titleOf(l)} detail={l.description} meta={facts(l, t)} mark={<Mark name={titleOf(l)} logo={l.logo} />} onOpen={() => onOpen(l)} action={action(l)} />
                ))}
              </Grid>
              {note ? <StatusBanner notice={{ text: note, error: true }} /> : null}
              {loaded.length < page.total ? (
                <XStack justify="center">
                  <Button size="sm" variant="outline" borderColor="var(--border-strong)" disabled={paging} onPress={() => void next()}>
                    {paging ? t('Loading…') : t('Show more')}
                  </Button>
                </XStack>
              ) : null}
            </>
          )}
        </YStack>
      )}
    </Part>
  )
}

/** The facts a card carries under its description, said in the reader's language. */
const facts = (l: Listing, t: Say): string => [l.vendor, l.official ? t('Official') : '', ready(l) ? t('Hosted') : t('Package only: needs a place to run')].filter(Boolean).join(' · ')

/** `value` once it has held still for `ms`. */
function useSettled<T>(value: T, ms: number): T {
  const [settled, setSettled] = useState(value)
  useEffect(() => {
    const id = setTimeout(() => setSettled(value), ms)
    return () => clearTimeout(id)
  }, [value, ms])
  return settled
}

/**
 * The fleet's own servers, from the MCP server's tools/list. Every run can call
 * them. Listed when asked for — that list starts every subsystem it names — and
 * at once on the old MCP address, which exists to show them.
 */
function Natives({ q }: { q: string }) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const r = route(host.path)
  const [asked, setAsked] = useState(r.kind === 'screen' && r.screen === 'mcp')
  const list = useLoad(asked ? () => natives(target) : null, [] as Native[], [target, asked])
  const [open, setOpen] = useState<string | null>(null)
  const [showAll, setShowAll] = useState(false)
  const shown = list.value.filter((s) => matches(q, s.name, s.description, ...s.ops))
  const ops = shown.reduce((n, s) => n + s.ops.length, 0)
  const searching = Boolean(q.trim())
  const visible = searching || showAll ? shown : shown.slice(0, BUILTIN_INITIAL)
  return (
    <Part
      title={t('Built in')}
      detail={t('Hanzo’s own servers. Always on: every run can call them, and nothing needs adding.')}
      action={
        asked ? undefined : (
          <Button size="sm" variant="outline" onPress={() => setAsked(true)}>
            {t('Show built-in servers')}
          </Button>
        )
      }
    >
      {!asked ? null : list.loading && !list.value.length ? (
        <RowsSkeleton label={t('Loading built-in servers')} />
      ) : list.error && !list.value.length ? (
        <Failed error={list.error} onRetry={list.reload} />
      ) : !shown.length ? (
        <Soft>{list.value.length ? t('No built-in server matches “{q}”.', { q: q.trim() }) : t('No built-in servers are listed.')}</Soft>
      ) : (
        <YStack gap="$2">
          <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
            {t('{servers} servers · {ops} operations', { servers: shown.length, ops })}
          </SizableText>
          {visible.map((s) => {
            const on = open === s.name
            return (
              <YStack key={s.name} borderWidth={1} borderColor="$borderColor" bg="$panel" rounded="$3" overflow="hidden">
                <XStack
                  render="button"
                  aria-expanded={on}
                  aria-label={s.name}
                  width="100%"
                  items="center"
                  gap="$3"
                  px="$3"
                  py="$2.5"
                  minH={44}
                  focusVisibleStyle={RING_INSET}
                  $platform-web={{ textAlign: 'start' }}
                  onPress={() => setOpen(on ? null : s.name)}
                >
                  <ServerIcon size={14} color="$soft" aria-hidden />
                  <YStack flex={1} minW={0} gap="$0.5">
                    <SizableText size="$3" color="$ink">
                      {s.name}
                    </SizableText>
                    {s.description ? (
                      <SizableText size="$1" color="$soft" numberOfLines={on ? undefined : 2}>
                        {s.description}
                      </SizableText>
                    ) : null}
                  </YStack>
                  <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']} shrink={0}>
                    {s.ops.length}
                  </SizableText>
                  {on ? <ChevronUp size={14} color="$soft" aria-hidden /> : <ChevronDown size={14} color="$soft" aria-hidden />}
                </XStack>
                {on ? (
                  <YStack px="$3" py="$2" gap="$1" borderTopWidth={1} borderColor="$borderColor">
                    {s.ops.length ? (
                      s.ops.map((op) => (
                        <SizableText key={op} size="$1" color="$ink" fontFamily="$mono">
                          {op}
                        </SizableText>
                      ))
                    ) : (
                      <SizableText size="$1" color="$soft">
                        {t('This server lists no operations.')}
                      </SizableText>
                    )}
                  </YStack>
                ) : null}
              </YStack>
            )
          })}
          {!searching && shown.length > BUILTIN_INITIAL ? (
            <XStack justify="flex-start">
              <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => setShowAll((v) => !v)}>
                {showAll ? t('Show fewer') : t('Show all {n} built-in servers', { n: shown.length })}
              </Button>
            </XStack>
          ) : null}
        </YStack>
      )}
    </Part>
  )
}

/** One listing in full: what it is, where it is served, and adding it. */
function About({ listing, added, signed, onAdd, onClose }: { listing: Listing; added: boolean; signed: boolean; onAdd: () => void; onClose: () => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const full = useLoad(() => one(target, listing.id), listing, [target, listing.id])
  const l = full.value
  const links = [l.site, l.repo].filter((u) => u.startsWith('https://'))
  return (
    <Sheet title={titleOf(l)} open onOpenChange={(o) => !o && onClose()}>
      <XStack gap="$3" items="center">
        <Mark name={titleOf(l)} logo={l.logo} size={40} />
        <YStack flex={1} minW={0}>
          <SizableText size="$2" color="$ink" numberOfLines={1} fontFamily="$mono">
            {l.name}
          </SizableText>
          <SizableText size="$1" color="$soft" numberOfLines={1}>
            {[l.vendor, l.version ? `v${l.version}` : '', l.official ? t('Official') : ''].filter(Boolean).join(' · ')}
          </SizableText>
        </YStack>
      </XStack>
      {l.description ? (
        <SizableText size="$2" color="$soft">
          {l.description}
        </SizableText>
      ) : null}
      {l.remotes.length ? (
        <Field label={t('Served at')}>
          {l.remotes.map((r) => (
            <SizableText key={r.url} size="$1" color="$ink" fontFamily="$mono" numberOfLines={1}>
              {r.url} · {r.transport}
            </SizableText>
          ))}
        </Field>
      ) : null}
      {l.packages.length ? (
        <Field label={t('Packages')} hint={ready(l) ? undefined : t('It ships only as a package, so it needs a place to run before it can be added here.')}>
          {l.packages.map((p) => (
            <SizableText key={`${p.registry}:${p.identifier}`} size="$1" color="$ink" fontFamily="$mono" numberOfLines={1}>
              {[p.registry, p.identifier, p.version, p.runtime].filter(Boolean).join(' · ')}
            </SizableText>
          ))}
        </Field>
      ) : null}
      {links.length ? (
        <XStack gap="$4" flexWrap="wrap">
          {links.map((u) => (
            <Out key={u} href={u}>
              <SizableText size="$2" color="$soft">
                {where(u)}
              </SizableText>
              <ExternalLink size={12} color="$soft" aria-hidden />
            </Out>
          ))}
        </XStack>
      ) : null}
      {full.error ? <StatusBanner notice={{ text: t('Showing what Browse listed. {reason}', { reason: say(full.error, t) }), error: true }} /> : null}
      {signed && ready(l) ? (
        <XStack justify="flex-end">
          <Button size="sm" disabled={added} onPress={onAdd}>
            {added ? t('Added') : t('Add to your connectors')}
          </Button>
        </XStack>
      ) : null}
    </Sheet>
  )
}

/**
 * Add a server: a listing off the shelf, or any MCP URL. The secret, when it
 * needs one, is sealed in KMS as it is added; then its tools are switched on.
 */
function Connect({ listing, onClose, onAdded }: { listing: Listing | null; onClose: () => void; onAdded: (said: string) => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const [name, setName] = useState('')
  const [url, setUrl] = useState('')
  const [header, setHeader] = useState('Authorization')
  const [secret, setSecret] = useState('')
  const [working, setWorking] = useState(false)
  const [error, setError] = useState('')

  const canSubmit = !working && (Boolean(listing) || (Boolean(name.trim()) && Boolean(url.trim()))) && (!secret || Boolean(header.trim()))

  const save = async () => {
    setWorking(true)
    setError('')
    const a: Adding = listing ? { listing: listing.id, name, header, secret } : { name, url, header, secret }
    try {
      const s = await add(target, a)
      let said = t('{name} is added.', { name: s.name })
      try {
        const its = (await tools(target, { source: 'mcp' })).filter((x) => owns(s, x.name)).map((x) => x.name)
        if (its.length) await toggle(target, its)
        said = its.length ? t('{name} is added, and its {n} tools are on.', { name: s.name, n: its.length }) : t('{name} is added. It listed no tools yet.', { name: s.name })
      } catch (e) {
        said = t('{name} is added, but its tools could not be switched on. {reason}', { name: s.name, reason: say(e, t) })
      }
      onAdded(said)
    } catch (e) {
      setError(say(e, t))
    } finally {
      setWorking(false)
    }
  }

  return (
    <Sheet title={listing ? t('Add {name}', { name: titleOf(listing) }) : t('Add by URL')} open onOpenChange={(o) => !o && onClose()} width={500}>
      {listing ? (
        <SizableText size="$2" color="$soft">
          {listing.description || listing.name}
        </SizableText>
      ) : (
        <SizableText size="$2" color="$soft">
          {t('Any MCP server that speaks HTTP. Its tools are on once it is added.')}
        </SizableText>
      )}
      <Field label={t('Name')} hint={listing ? t('Leave it empty to use the listing’s own.') : undefined}>
        <Input value={name} onChangeText={setName} placeholder={listing ? titleOf(listing) : t('Docs')} aria-label={t('Name')} />
      </Field>
      {listing ? null : (
        <Field label="URL" hint={t('A public http(s) address. Private and loopback addresses are refused.')}>
          <Input value={url} onChangeText={setUrl} placeholder="https://mcp.example.com/mcp" aria-label="URL" autoCapitalize="none" />
        </Field>
      )}
      <Field label={t('Secret')} hint={t('Optional. Kept sealed in KMS, sent on every call, and never shown again.')}>
        <Input value={secret} onChangeText={setSecret} placeholder="Bearer …" aria-label={t('Secret')} secureTextEntry />
      </Field>
      {secret ? (
        <Field label={t('Header')}>
          <Input value={header} onChangeText={setHeader} aria-label={t('Header')} autoCapitalize="none" />
        </Field>
      ) : null}
      {error ? <StatusBanner notice={{ text: error, error: true }} /> : null}
      <XStack gap="$2" justify="flex-end">
        <Button size="sm" variant="ghost" onPress={onClose}>
          {t('Cancel')}
        </Button>
        <Button size="sm" variant="primary" disabled={!canSubmit} onPress={() => void save()}>
          {working ? t('Adding…') : t('Add')}
        </Button>
      </XStack>
    </Sheet>
  )
}

/** One of the org's servers: where it is, its secret, its tools and their switches, and removing it. */
function Detail({ server, may, onClose, onDeleted }: { server: Server; may: boolean; onClose: () => void; onDeleted: () => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const list = useLoad(() => tools(target, { source: 'mcp' }), [] as Tool[], [target, server.id])
  const its = useMemo(() => list.value.filter((x) => owns(server, x.name)), [list.value, server])
  const [toolQuery, setToolQuery] = useState('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState('')
  const [asking, setAsking] = useState(false)
  const off = its.filter((x) => !x.activated).map((x) => x.name)
  const filteredTools = useMemo(
    () => (toolQuery.trim() ? its.filter((x) => matches(toolQuery, x.name.slice(server.id.length + 1), x.description)) : its),
    [its, server.id, toolQuery],
  )

  const flip = async (on: string[], offNames: string[]) => {
    setBusy(true)
    setError('')
    try {
      await toggle(target, on, offNames)
      list.reload()
    } catch (e) {
      setError(say(e, t))
    } finally {
      setBusy(false)
    }
  }

  return (
    <Sheet title={server.name} open onOpenChange={(o) => !o && onClose()}>
      <YStack gap="$1.5" p="$3" rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$panel">
        <SizableText size="$1" color="$ink" fontFamily="$mono" numberOfLines={2}>
          {server.url}
        </SizableText>
        <XStack items="center" gap="$1.5">
          {server.secret ? <KeyRound size={12} color="$soft" aria-hidden /> : <ServerIcon size={12} color="$soft" aria-hidden />}
          <SizableText size="$1" color="$soft" flex={1} minW={0}>
            {server.secret
              ? server.header
                ? t('A secret is sealed in KMS and sent in {header}.', { header: server.header })
                : t('A secret is sealed in KMS and sent in its header.')
              : t('No secret: it is called as it is.')}
          </SizableText>
        </XStack>
      </YStack>
      <XStack items="center" gap="$2">
        <SizableText size="$3" fontWeight="500" color="$ink" flex={1}>
          {its.length ? t('Tools ({n})', { n: its.length }) : t('Tools')}
        </SizableText>
        {its.length && may ? (
          <Button size="sm" variant="outline" disabled={busy} onPress={() => void (off.length ? flip(off, []) : flip([], its.map((x) => x.name)))}>
            {off.length ? t('Turn all on') : t('Turn all off')}
          </Button>
        ) : null}
      </XStack>
      {its.length > 8 ? (
        <Input
          value={toolQuery}
          onChangeText={setToolQuery}
          placeholder={t('Filter tools…')}
          aria-label={t('Filter tools')}
          startAdornment={<Search size={14} color="$soft" aria-hidden />}
          endAdornment={
            toolQuery ? (
              <XStack
                render="button"
                aria-label={t('Clear search')}
                onPress={() => setToolQuery('')}
                minW={24}
                minH={24}
                items="center"
                justify="center"
                focusVisibleStyle={RING}
              >
                <X size={13} color="$soft" aria-hidden />
              </XStack>
            ) : undefined
          }
          onKeyDown={(e: { key?: string }) => {
            if (e.key === 'Escape' && toolQuery) setToolQuery('')
          }}
        />
      ) : null}
      {list.loading && !list.value.length ? (
        <RowsSkeleton label={t('Loading tools')} />
      ) : list.error && !list.value.length ? (
        <Failed error={list.error} onRetry={list.reload} />
      ) : !its.length ? (
        <Soft>{t('No tools listed yet. A server that does not answer shows its tools once it does.')}</Soft>
      ) : !filteredTools.length ? (
        <Soft>{t('No tool matches “{q}”.', { q: toolQuery.trim() })}</Soft>
      ) : (
        <YStack borderWidth={1} borderColor="$borderColor" bg="$panel" rounded="$3" overflow="hidden">
          {filteredTools.map((x, i) => {
            const short = x.name.slice(server.id.length + 1)
            return (
              <XStack key={x.name} items="center" gap="$3" px="$3" py="$2" borderTopWidth={i ? 1 : 0} borderColor="$borderColor">
                <YStack flex={1} minW={0} gap="$0.5">
                  <SizableText size="$2" color="$ink" fontFamily="$mono" numberOfLines={1}>
                    {short}
                  </SizableText>
                  {x.description ? (
                    <SizableText size="$1" color="$soft" numberOfLines={2}>
                      {x.description}
                    </SizableText>
                  ) : null}
                </YStack>
                {may ? (
                  <Switch checked={x.activated} disabled={busy} onCheckedChange={(v: boolean) => void flip(v ? [x.name] : [], v ? [] : [x.name])} aria-label={t('{name} on', { name: short })} />
                ) : (
                  <SizableText size="$1" color="$soft">
                    {x.activated ? t('On') : t('Off')}
                  </SizableText>
                )}
              </XStack>
            )
          })}
        </YStack>
      )}
      {error ? (
        <StatusBanner notice={{ text: error, error: true }} />
      ) : !may ? (
        <SizableText size="$1" color="$soft" role="status">
          {t('Only an org admin can switch or remove connectors.')}
        </SizableText>
      ) : null}
      {may ? (
        <XStack justify="flex-start">
          <Button size="sm" variant="ghost" tone="danger" onPress={() => setAsking(true)}>
            <Trash2 size={14} /> {t('Remove connector')}
          </Button>
        </XStack>
      ) : null}
      <Confirm
        title={t('Delete {name}?', { name: server.name })}
        act={t('Delete')}
        says={t('Its tools leave your organization and every agent, and its sealed secret is destroyed.')}
        open={asking}
        onOpenChange={setAsking}
        run={async () => {
          await remove(target, server.id)
          onDeleted()
        }}
      />
    </Sheet>
  )
}
