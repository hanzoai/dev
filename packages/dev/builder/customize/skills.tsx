/**
 * Skills: what the agent knows how to do, each a SKILL.md it reads when an agent
 * names it.
 *
 * Browse is Hanzo's catalogue; adding one switches it on for the org, which is
 * all adding is: the platform keeps no copy. Yours is the org's own skills —
 * written here, each with its switch — and the catalogue's skills it has added.
 * A skill written here is switched on when it is saved.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a skill's name,
 * description and SKILL.md are the platform's or the org's, drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { AlertCircle, BookOpen, Check, Eye, Pencil, SlidersHorizontal, Trash2 } from '@hanzogui/lucide-icons-2'
import { Button, DropdownMenu, Input, Skeleton, Switch, Textarea } from '@hanzo/ui'
import { Code } from '@hanzo/ui/chat'
import { useEffect, useMemo, useState } from 'react'

import { active, authored, brand, document, nameOf, remove, tool, write, type Catalogue, type Entry, type Skill } from '../api/skills.ts'
import { toggle, type Tool } from '../api/tools.ts'
import { useHost, useTarget } from '../host.tsx'
import { useLoad } from './load.ts'
import { Roles } from './roles.tsx'
import { say } from './say.ts'
import { Add, day, Empty, Failed, Field, FilterPills, Grid, Mark, matches, MenuTrigger, Part, Sheet, Soft, Tile, TilesSkeleton, type Pane, useCount, Visitor } from './ui.tsx'
import { Confirm } from '../ask.tsx'

const PAGE = 60
const ANY = ''


/** A skill to read: one of the catalogue's, or one the org added from it. */
interface Reading {
  name: string
  description: string
  product: string
}

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

function DocSkeleton({ label }: { label: string }) {
  return (
    <YStack role="status" aria-busy={true} aria-label={label} p="$3.5" gap="$2" rounded="$3" borderWidth={1} borderColor="$borderColor" bg="$panel">
      <Skeleton layout="block" width="38%" height={13} rounded="$2" />
      <Skeleton layout="block" width="94%" height={11} rounded="$2" />
      <Skeleton layout="block" width="82%" height={11} rounded="$2" />
      <Skeleton layout="block" width="88%" height={11} rounded="$2" />
      <Skeleton layout="block" width="64%" height={11} rounded="$2" />
      <Skeleton layout="block" width="76%" height={11} rounded="$2" />
    </YStack>
  )
}

export function Skills({ view, q, adding, onAdding, onView, onCount }: Pane) {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  // What every run in the org loads is an org admin's to change; a member reads it.
  const may = signed && host.admin
  const own = useLoad(signed ? () => authored(target) : null, [] as Skill[], [target, signed])
  const on = useLoad(signed ? () => active(target) : null, [] as Tool[], [target, signed])
  const shelf = useLoad(view === 'discover' ? () => brand(target) : null, null as Catalogue | null, [target.api, view])
  const [editing, setEditing] = useState<Skill | null>(null)
  const [reading, setReading] = useState<Reading | null>(null)
  const [deleting, setDeleting] = useState<Skill | null>(null)
  const [busy, setBusy] = useState('')
  const [notice, setNotice] = useState<Notice | null>(null)
  const [product, setProduct] = useState(ANY)
  const [shown, setShown] = useState(PAGE)

  useEffect(() => setShown(PAGE), [q, product])

  const lit = useMemo(() => new Set(on.value.map((x) => x.name)), [on.value])
  const mine = useMemo(() => new Set(own.value.map((s) => s.name)), [own.value])
  const added = on.value.filter((x) => nameOf(x.name) && !mine.has(nameOf(x.name)))
  useCount(own.value.length + added.length, onCount)

  const productOptions = useMemo(() => {
    const c = shelf.value
    if (!c) return [] as { id: string; label: string }[]
    const names = c.products.length
      ? [...c.products].sort((a, b) => b.count - a.count || a.name.localeCompare(b.name)).map((p) => p.name)
      : [...new Set(c.skills.map((e) => e.product).filter(Boolean))].sort((a, b) => a.localeCompare(b))
    return names.length > 1 ? [{ id: ANY, label: t('All') }, ...names.map((name) => ({ id: name, label: name }))] : []
  }, [shelf.value, t])

  /** Switch one skill on or off, then read what is on again. */
  const flip = async (name: string, next: boolean) => {
    setBusy(name)
    setNotice(null)
    try {
      await toggle(target, next ? [tool(name)] : [], next ? [] : [tool(name)])
      on.reload()
      setNotice({ text: next ? t('{name} is on', { name }) : t('{name} is off', { name }) })
    } catch (e) {
      setNotice({ text: say(e, t), error: true })
    } finally {
      setBusy('')
    }
  }

  const reader = reading ? (
    <Reader
      skill={reading}
      added={lit.has(tool(reading.name))}
      signed={may}
      busy={busy === reading.name}
      onFlip={(next) => void flip(reading.name, next)}
      onClose={() => setReading(null)}
    />
  ) : null
  const editor =
    adding || editing ? (
      <Editor
        skill={editing}
        may={may}
        onClose={() => {
          setEditing(null)
          onAdding(false)
        }}
        onSaved={(s, fresh) => {
          setEditing(null)
          onAdding(false)
          own.reload()
          on.reload()
          onView('yours')
          setNotice({ text: fresh ? t('{name} is saved and on', { name: s.name }) : t('{name} is saved', { name: s.name }) })
        }}
        onDeleted={(name) => {
          setEditing(null)
          own.reload()
          on.reload()
          setNotice({ text: t('{name} is deleted', { name }) })
        }}
      />
    ) : null
  const confirmDelete = deleting ? (
    <Confirm
      title={t('Delete {name}?', { name: deleting.name })}
      act={t('Delete')}
      says={t('The skill leaves your organization and every agent that names it.')}
      open
      onOpenChange={(o) => !o && setDeleting(null)}
      run={async () => {
        const gone = deleting
        await remove(target, gone.id)
        setDeleting(null)
        own.reload()
        on.reload()
        setNotice({ text: t('{name} is deleted', { name: gone.name }) })
      }}
    />
  ) : null

  if (view === 'discover') {
    const all = shelf.value?.skills ?? []
    const found = all.filter((e) => (!product || e.product === product) && matches(q, e.name, e.description, e.product))
    return (
      <YStack gap="$3">
        <StatusBanner notice={notice} />
        {signed && !q.trim() ? <Roles /> : null}
        <Part title={t('From Hanzo')} detail={t('Add one and every run in your organization can use it.')}>
          {shelf.error && !shelf.value ? (
            <Failed error={shelf.error} onRetry={shelf.reload} />
          ) : !shelf.value ? (
            <TilesSkeleton label={t('Loading skills')} />
          ) : (
            <YStack gap="$3">
              {productOptions.length > 1 ? (
                <FilterPills label={t('Product')} value={product} options={productOptions} onChange={setProduct} maxVisibleDesktop={6} mobileLimit={4} />
              ) : null}
              {!found.length ? (
                <Soft
                  action={
                    product ? (
                      <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => setProduct(ANY)}>
                        {t('Show all products')}
                      </Button>
                    ) : undefined
                  }
                >
                  {q.trim()
                    ? t('No skill matches “{q}”.', { q: q.trim() })
                    : product
                      ? t('No skills in {product}.', { product })
                      : t('There are no skills to add yet.')}
                </Soft>
              ) : (
                <>
                  <SizableText size="$1" color="$soft" fontVariant={['tabular-nums']}>
                    {found.length === all.length ? t('{n} skills', { n: all.length }) : t('{found} of {n} skills', { found: found.length, n: all.length })}
                  </SizableText>
                  <Grid label={t('Skills to add')}>
                    {found.slice(0, shown).map((e: Entry) => (
                      <Tile
                        key={e.name}
                        title={e.name}
                        detail={e.description}
                        meta={e.product || undefined}
                        mark={<Mark name={e.name} icon={<BookOpen size={15} color="$soft" aria-hidden />} />}
                        onOpen={() => setReading(e)}
                        action={may ? <Add name={e.name} added={lit.has(tool(e.name))} busy={busy === e.name} onPress={() => void flip(e.name, true)} /> : undefined}
                      />
                    ))}
                  </Grid>
                  {found.length > shown ? (
                    <XStack justify="center">
                      <Button size="sm" variant="outline" borderColor="var(--border-strong)" onPress={() => setShown(shown + PAGE)}>
                        {t('Show more')}
                      </Button>
                    </XStack>
                  ) : null}
                </>
              )}
            </YStack>
          )}
        </Part>
        {reader}
        {editor}
      </YStack>
    )
  }

  if (!signed) {
    return <Visitor>{t('Sign in to see your skills.')}</Visitor>
  }
  const ownShown = own.value.filter((s) => matches(q, s.name, s.description))
  const addedShown = added.filter((x) => matches(q, nameOf(x.name), x.description))
  const error = own.error ?? on.error
  return (
    <YStack gap="$5">
      <StatusBanner notice={notice} />
      {error && !own.value.length && !on.value.length ? (
        <Failed
          error={error}
          onRetry={() => {
            own.reload()
            on.reload()
          }}
        />
      ) : (own.loading || on.loading) && !own.value.length && !on.value.length ? (
        <TilesSkeleton label={t('Loading your skills')} count={4} />
      ) : !own.value.length && !added.length ? (
        <Empty title={t('No skills yet')} detail={may ? t('Add one from Hanzo, or write your own.') : t('An org admin adds skills. Browse what there is.')}>
          <Button size="sm" variant="outline" onPress={() => onView('discover')}>
            {t('Browse skills')}
          </Button>
          {may ? (
            <Button size="sm" onPress={() => onAdding(true)}>
              {t('Write a skill')}
            </Button>
          ) : null}
        </Empty>
      ) : !ownShown.length && !addedShown.length ? (
        <Soft>{t('No skill of yours matches “{q}”.', { q: q.trim() })}</Soft>
      ) : (
        <>
          {ownShown.length ? (
            <Part title={t('Made by your team')} detail={t('Off keeps a skill but leaves it out of every run.')}>
              <Grid label={t('Your skills')}>
                {ownShown.map((s) => {
                  const activeNow = lit.has(tool(s.name))
                  return (
                    <Tile
                      key={s.id}
                      title={s.name}
                      detail={s.description || firstLine(s.content)}
                      meta={
                        [s.source ? t('From {source}', { source: s.source }) : day(s.created) ? t('Saved {date}', { date: day(s.created) }) : '', s.admitted ? '' : t('Not in runs until an admin saves it')]
                          .filter(Boolean)
                          .join(' · ') || undefined
                      }
                      mark={<Mark name={s.name} />}
                      onOpen={() => setEditing(s)}
                      action={
                        <XStack items="center" gap="$1.5" shrink={0}>
                          {may ? (
                            <Switch checked={activeNow} disabled={busy === s.name} onCheckedChange={(v: boolean) => void flip(s.name, v)} aria-label={t('{name} on', { name: s.name })} />
                          ) : (
                            <SizableText size="$1" color="$soft">
                              {activeNow ? t('On') : t('Off')}
                            </SizableText>
                          )}
                          <DropdownMenu
                            placement="bottom-end"
                            offset={6}
                            trigger={<MenuTrigger label={t('Actions for {name}', { name: s.name })} />}
                            items={
                              may
                                ? [
                                    { key: 'edit', label: t('Edit skill'), icon: <Pencil size={15} />, onSelect: () => setEditing(s) },
                                    {
                                      key: 'toggle',
                                      label: activeNow ? t('Turn off') : t('Turn on'),
                                      icon: <SlidersHorizontal size={15} />,
                                      disabled: busy === s.name,
                                      onSelect: () => void flip(s.name, !activeNow),
                                    },
                                    { type: 'separator' as const },
                                    { key: 'delete', label: t('Delete'), icon: <Trash2 size={15} />, destructive: true, onSelect: () => setDeleting(s) },
                                  ]
                                : [{ key: 'view', label: t('View SKILL.md'), icon: <Eye size={15} />, onSelect: () => setEditing(s) }]
                            }
                          />
                        </XStack>
                      }
                    />
                  )
                })}
              </Grid>
            </Part>
          ) : null}
          {addedShown.length ? (
            <Part title={t('Added from Hanzo')} detail={t('On for every run in your organization.')}>
              <Grid label={t('Added skills')}>
                {addedShown.map((x) => {
                  const name = nameOf(x.name)
                  return (
                    <Tile
                      key={x.name}
                      title={name}
                      detail={x.description}
                      mark={<Mark name={name} icon={<BookOpen size={15} color="$soft" aria-hidden />} />}
                      onOpen={() => setReading({ name, description: x.description, product: '' })}
                      action={
                        <DropdownMenu
                          placement="bottom-end"
                          offset={6}
                          trigger={<MenuTrigger label={t('Actions for {name}', { name })} />}
                          items={[
                            { key: 'read', label: t('Read SKILL.md'), icon: <BookOpen size={15} />, onSelect: () => setReading({ name, description: x.description, product: '' }) },
                            ...(may
                              ? [
                                  { type: 'separator' as const },
                                  {
                                    key: 'remove',
                                    label: t('Remove'),
                                    icon: <Trash2 size={15} />,
                                    destructive: true,
                                    disabled: busy === name,
                                    onSelect: () => void flip(name, false),
                                  },
                                ]
                              : []),
                          ]}
                        />
                      }
                    />
                  )
                })}
              </Grid>
            </Part>
          ) : null}
        </>
      )}
      {reader}
      {editor}
      {confirmDelete}
    </YStack>
  )
}

const firstLine = (md: string): string =>
  md
    .split('\n')
    .map((l) => l.replace(/^#+\s*/, '').trim())
    .find(Boolean) ?? ''

/** One catalogue skill: what it is, its SKILL.md, and adding or removing it. */
function Reader({
  skill,
  added,
  signed,
  busy,
  onFlip,
  onClose,
}: {
  skill: Reading
  added: boolean
  signed: boolean
  busy: boolean
  onFlip: (next: boolean) => void
  onClose: () => void
}) {
  const { t } = useI18n()
  const target = useTarget()
  const doc = useLoad(() => document(target, skill.name), '', [target.api, skill.name])
  return (
    <Sheet title={skill.name} open onOpenChange={(o) => !o && onClose()} width={720}>
      {skill.product ? (
        <XStack items="center" gap="$1.5">
          <BookOpen size={13} color="$soft" aria-hidden />
          <SizableText size="$1" color="$soft">
            {skill.product}
          </SizableText>
        </XStack>
      ) : null}
      {skill.description ? (
        <SizableText size="$2" color="$soft">
          {skill.description}
        </SizableText>
      ) : null}
      {signed ? (
        <XStack gap="$2.5" items="center" flexWrap="wrap" $max-sm={{ flexDirection: 'column', items: 'stretch' }}>
          <Button size="sm" variant={added ? 'outline' : 'primary'} borderColor={added ? 'var(--border-strong)' : undefined} disabled={busy} onPress={() => onFlip(!added)}>
            {busy ? t('Updating…') : added ? t('Remove') : t('Add to your skills')}
          </Button>
          <SizableText size="$1" color="$soft" flex={1}>
            {added ? t('On for your organization.') : t('Every run in your organization can use it once added.')}
          </SizableText>
        </XStack>
      ) : null}
      {doc.error ? (
        <Failed error={doc.error} onRetry={doc.reload} />
      ) : doc.loading && !doc.value ? (
        <DocSkeleton label={t('Loading SKILL.md')} />
      ) : (
        <Code language="SKILL.md" value={doc.value}>
          <SizableText size="$1" color="$ink" fontFamily="$mono" $platform-web={{ whiteSpace: 'pre-wrap' }}>
            {doc.value}
          </SizableText>
        </Code>
      )}
    </Sheet>
  )
}

/** Write a skill, or revise one of the org's own. Its name is its id, so a saved skill keeps its name. */
function Editor({
  skill,
  may,
  onClose,
  onSaved,
  onDeleted,
}: {
  skill: Skill | null
  /** Whether this person may write and delete the org's skills: an org admin. */
  may: boolean
  onClose: () => void
  onSaved: (s: Skill, fresh: boolean) => void
  onDeleted: (name: string) => void
}) {
  const { t } = useI18n()
  const target = useTarget()
  const [name, setName] = useState(skill?.name ?? '')
  const [description, setDescription] = useState(skill?.description ?? '')
  const [content, setContent] = useState(skill?.content ?? '')
  const [working, setWorking] = useState(false)
  const [error, setError] = useState('')
  const [asking, setAsking] = useState(false)

  const save = async () => {
    setWorking(true)
    setError('')
    try {
      const saved = await write(target, { name: name.trim(), description: description.trim(), content })
      // A new skill is switched on as it is saved; a revised one keeps its switch.
      if (!skill) await toggle(target, [tool(saved.name)])
      onSaved(saved, !skill)
    } catch (e) {
      setError(say(e, t))
    } finally {
      setWorking(false)
    }
  }

  return (
    <Sheet title={skill ? skill.name : t('New skill')} open onOpenChange={(o) => !o && onClose()} width={680}>
      {skill?.source ? (
        <SizableText size="$2" color="$soft">
          {t('Read from {source}. The next push of that repository replaces what is saved here.', { source: skill.source })}
        </SizableText>
      ) : null}
      <Field label={t('Name')} hint={skill ? t('A skill keeps its name. Save under another name to make a new one.') : t('One lowercase word: letters, digits, _ or -.')}>
        <Input value={name} onChangeText={setName} placeholder="issue-triage" aria-label={t('Name')} disabled={Boolean(skill) || !may} autoCapitalize="none" borderColor="var(--border-strong)" />
      </Field>
      <Field label={t('Description')} hint={t('One line. An agent reads it to decide whether it needs the skill.')}>
        <Input value={description} onChangeText={setDescription} placeholder={t('How we triage an incoming issue')} aria-label={t('Description')} disabled={!may} borderColor="var(--border-strong)" />
      </Field>
      <Field label="SKILL.md" hint={t('Markdown: what it is for, when to use it, and the steps.')}>
        <Textarea value={content} onChangeText={setContent} placeholder={t('# Triage\n\n1. Read the issue…')} aria-label="SKILL.md" disabled={!may} rows={14} fontFamily="$mono" borderColor="var(--border-strong)" />
      </Field>
      {error ? (
        <StatusBanner notice={{ text: error, error: true }} />
      ) : !may ? (
        <SizableText size="$1" color="$soft" role="status">
          {t('Only an org admin can write or delete skills.')}
        </SizableText>
      ) : null}
      <XStack gap="$2" items="center" flexWrap="wrap">
        {skill && may ? (
          <Button size="sm" variant="ghost" tone="danger" onPress={() => setAsking(true)}>
            <Trash2 size={14} /> {t('Delete')}
          </Button>
        ) : null}
        <XStack flex={1} />
        <Button size="sm" variant="ghost" onPress={onClose}>
          {may ? t('Cancel') : t('Close')}
        </Button>
        {may ? (
          <Button size="sm" variant="primary" disabled={working || !name.trim() || !content.trim()} onPress={() => void save()}>
            {working ? t('Saving…') : t('Save')}
          </Button>
        ) : null}
      </XStack>
      {skill ? (
        <Confirm
          title={t('Delete {name}?', { name: skill.name })}
          act={t('Delete')}
          says={t('The skill leaves your organization and every agent that names it.')}
          open={asking}
          onOpenChange={setAsking}
          run={async () => {
            await remove(target, skill.id)
            onDeleted(skill.name)
          }}
        />
      ) : null}
    </Sheet>
  )
}
