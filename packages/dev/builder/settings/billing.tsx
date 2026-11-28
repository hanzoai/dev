/**
 * Billing: the plan this organization is on and the way to change it, its
 * credits (`Credits`: the balance, grants, top-ups, auto-reload, this month's
 * spend and its limit), the cards it pays with, its invoices, and ending the
 * plan. Changing plan is the Plans screen; this page says what is true now.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); a plan's name, a
 * card's, an invoice's lines and its status are billing's, drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n, type Say } from '@hanzo/i18n/react'
import { CreditCard, Download, Plus, X } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogTitle, Input, toast } from '@hanzo/ui'
import { useEffect, useState } from 'react'

import {
  cancel,
  chosen,
  current,
  detach,
  formatAddress,
  invoices,
  label,
  makeDefault,
  methods,
  money,
  pdf,
  reactivate,
  subscriptions,
  tier,
  updateMethod,
  type Address,
  type Invoice,
  type Method,
  type Subscription,
} from '../api/billing.ts'
import { told } from '../api/call.ts'
import { useRead } from '../data.ts'
import { useHost, useTarget } from '../host.tsx'
import { Title } from '../meter.tsx'
import { label as named } from '../plan.ts'
import { path } from '../route.ts'
import { AddCard } from './card.tsx'
import { Credits } from './credits.tsx'
import { Card, day, Field, Group, Heading, Note, Row, Soft } from './ui.tsx'
import { Confirm } from '../ask.tsx'

export function Billing() {
  const { t } = useI18n()
  const host = useHost()
  const target = useTarget()
  const signed = Boolean(host.person)
  const subs = useRead(signed ? () => subscriptions(target) : null, [] as Subscription[], [target, signed])
  const cards = useRead(signed ? () => methods(target) : null, [] as Method[], [target, signed])
  const bills = useRead(signed ? () => invoices(target) : null, [] as Invoice[], [target, signed])
  const billed = useRead(signed ? () => tier(target) : null, null, [target, signed])
  const [adding, setAdding] = useState(false)
  const [cardsVersion, setCardsVersion] = useState(0)
  const [ending, setEnding] = useState(false)
  const [editingBilling, setEditingBilling] = useState<Method | null>(null)
  const [open, setOpen] = useState<Invoice | null>(null)
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')

  if (!signed) {
    return (
      <YStack gap="$5">
        <Heading title={t('Billing')} />
        <Soft>{t('Sign in to see this organization’s billing.')}</Soft>
      </YStack>
    )
  }

  const plan = current(subs.value)
  // The plan by its family, `Max` with `20x` beside it; the subscription names
  // it, else the rung billing serves.
  const title = named(plan?.plan || billed.value?.plan, billed.value?.tier) ?? (plan ? { name: plan.name, tag: '' } : null)
  // Nothing is said about the plan until both reads have answered: a page that
  // has not asked yet is not a page on Free.
  const reading = (subs.loading && !subs.value.length) || (billed.value === null && !billed.error && !subs.error)
  const empty = !cards.error && !cards.loading && cards.value.length === 0
  const defaultCard = chosen(cards.value)

  const reloadCards = () => {
    cards.reload()
    setCardsVersion((v) => v + 1)
  }

  // A done act is a toast; a refused one stays on the page, beside what was tried.
  const act = async (what: () => Promise<unknown>, done: string, after: () => void) => {
    setWorking(true)
    setNote('')
    try {
      await what()
      after()
      toast.success(done)
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setWorking(false)
    }
  }

  return (
    <YStack gap="$6">
      <Heading
        title={t('Billing')}
        detail={host.org ? t('The plan {org} is on, the cards it pays with, and its invoices.', { org: host.org }) : t('The plan this organization is on, the cards it pays with, and its invoices.')}
      />

      <XStack items="center" gap="$3" px="$4" py="$4" borderWidth={1} borderColor="$borderColor" rounded="$3" flexWrap="wrap">
        <YStack flex={1} minW={200} gap="$1" data-slot="billing-plan">
          {reading ? (
            <SizableText size="$5" color="$ink">
              {t('Reading your plan…')}
            </SizableText>
          ) : (
            <Title label={title ?? { name: phrase('Free'), tag: '' }} size="$5" />
          )}
          <SizableText size="$2" color="$soft">
            {subs.error ? told(subs.error, t) : plan ? terms(plan, t) : title ? t('Your plan.') : t('No paid plan. Usage is paid from credits.')}
          </SizableText>
        </YStack>
        <Button size="sm" variant="outline" onPress={() => host.go(path({ kind: 'screen', screen: 'plans' }))}>
          {t('Adjust plan')}
        </Button>
      </XStack>

      <Credits target={target} admin={host.admin} cardsVersion={cardsVersion} onCard={() => setAdding(true)} />

      <Group
        title={t('Payment')}
        detail={t('The card a plan renews on and a top-up charges.')}
        action={
          host.admin && (cards.error || cards.value.length > 0) ? (
            <Button size="sm" variant="outline" onPress={() => setAdding(true)}>
              <Plus size={14} /> {t('Add card')}
            </Button>
          ) : undefined
        }
      >
        {cards.error ? (
          <Soft>{told(cards.error, t)}</Soft>
        ) : cards.loading && !cards.value.length ? (
          <Soft>{t('Reading cards…')}</Soft>
        ) : empty ? (
          <XStack items="center" gap="$3" px="$4" py="$4" borderWidth={1} borderColor="$borderColor" rounded="$3" flexWrap="wrap">
            <CreditCard size={20} />
            <YStack flex={1} minW={180} gap="$1">
              <SizableText size="$3" color="$ink">
                {t('No card on file.')}
              </SizableText>
              <SizableText size="$2" color="$soft">
                {host.admin ? t('Add one to pay for a plan or a top-up. Adding it charges nothing.') : t('An org admin adds cards.')}
              </SizableText>
            </YStack>
            {host.admin ? (
              <Button variant="primary" onPress={() => setAdding(true)}>
                <Plus size={16} /> {t('Add card')}
              </Button>
            ) : null}
          </XStack>
        ) : (
          <Card>
            {cards.value.map((m, i) => (
              <Row
                key={m.id}
                first={i === 0}
                leading={<CreditCard size={16} />}
                title={method(m, t)}
                detail={
                  [
                    m.expires ? t('Expires {date}', { date: m.expires }) : '',
                    m.default ? t('Default') : '',
                    m.billingAddress ? formatAddress(m.billingAddress) : '',
                  ]
                    .filter(Boolean)
                    .join(' · ') || undefined
                }
                trailing={
                  host.admin ? (
                    <XStack gap="$2" items="center">
                      {!m.default ? (
                        <Button
                          size="sm"
                          variant="ghost"
                          aria-label={t('Make {card} default', { card: method(m, t) })}
                          disabled={working}
                          onPress={() => void act(() => makeDefault(target, m.id), t('{card} is now the default', { card: method(m, t) }), reloadCards)}
                        >
                          {t('Make default')}
                        </Button>
                      ) : null}
                      <Button
                        size="sm"
                        variant="ghost"
                        aria-label={t('Edit billing info for {card}', { card: method(m, t) })}
                        disabled={working}
                        onPress={() => setEditingBilling(m)}
                      >
                        {t('Edit info')}
                      </Button>
                      <Button
                        size="sm"
                        variant="ghost"
                        aria-label={t('Remove {card}', { card: method(m, t) })}
                        disabled={working}
                        onPress={() => void act(() => detach(target, m.id), t('{card} is removed', { card: method(m, t) }), reloadCards)}
                      >
                        {t('Remove')}
                      </Button>
                    </XStack>
                  ) : undefined
                }
              />
            ))}
          </Card>
        )}
        {!host.admin && cards.value.length ? (
          <SizableText size="$1" color="$soft">
            {t('An org admin adds and removes cards.')}
          </SizableText>
        ) : null}
      </Group>

      <Group
        title={t('Billing details')}
        detail={t('Information printed on invoices and payment receipts.')}
        action={
          host.admin && defaultCard ? (
            <Button size="sm" variant="outline" onPress={() => setEditingBilling(defaultCard)}>
              {defaultCard.billingAddress ? t('Edit details') : t('Add billing info')}
            </Button>
          ) : undefined
        }
      >
        {defaultCard?.billingAddress ? (
          <Card>
            <YStack p="$3" gap="$1.5">
              {defaultCard.billingAddress.name ? (
                <SizableText size="$3" color="$ink" style={{ fontWeight: 600 }}>
                  {defaultCard.billingAddress.name}
                </SizableText>
              ) : null}
              {defaultCard.billingAddress.line1 ? (
                <SizableText size="$2" color="$soft">
                  {defaultCard.billingAddress.line1}
                  {defaultCard.billingAddress.line2 ? `, ${defaultCard.billingAddress.line2}` : ''}
                </SizableText>
              ) : null}
              {defaultCard.billingAddress.city || defaultCard.billingAddress.state || defaultCard.billingAddress.postalCode ? (
                <SizableText size="$2" color="$soft">
                  {[defaultCard.billingAddress.city, defaultCard.billingAddress.state, defaultCard.billingAddress.postalCode]
                    .filter(Boolean)
                    .join(' ')}
                </SizableText>
              ) : null}
              {defaultCard.billingAddress.country ? (
                <SizableText size="$2" color="$soft">
                  {defaultCard.billingAddress.country}
                </SizableText>
              ) : null}
            </YStack>
          </Card>
        ) : (
          <XStack items="center" justify="space-between" gap="$3" px="$4" py="$3" borderWidth={1} borderColor="$borderColor" rounded="$3" flexWrap="wrap">
            <YStack flex={1} minW={180} gap="$1">
              <SizableText size="$2" color="$ink">
                {t('No billing address on file.')}
              </SizableText>
              <SizableText size="$1" color="$soft">
                {t('Add your company name and address so they appear on monthly invoices and receipts.')}
              </SizableText>
            </YStack>
            {host.admin ? (
              <Button
                size="sm"
                variant="outline"
                onPress={() => (defaultCard ? setEditingBilling(defaultCard) : setAdding(true))}
              >
                <Plus size={14} /> {defaultCard ? t('Add billing info') : t('Set billing info')}
              </Button>
            ) : null}
          </XStack>
        )}
      </Group>

      <Group title={t('Invoices')}>
        {bills.error ? (
          <Soft>{told(bills.error, t)}</Soft>
        ) : bills.loading && !bills.value.length ? (
          <Soft>{t('Reading invoices…')}</Soft>
        ) : bills.value.length === 0 ? (
          <Card>
            <Soft>{t('No invoices yet.')}</Soft>
          </Card>
        ) : (
          <Card>
            <XStack px="$3" py="$2" gap="$3">
              <SizableText flex={1} size="$1" color="$soft">
                {t('Date')}
              </SizableText>
              <SizableText width={90} size="$1" color="$soft" text="end">
                {t('Total')}
              </SizableText>
              <SizableText width={70} size="$1" color="$soft" $max-sm={{ display: 'none' }}>
                {t('Status')}
              </SizableText>
              <YStack width={64} />
            </XStack>
            {bills.value.map((b) => (
              <XStack key={b.id} items="center" gap="$3" px="$3" py="$2.5" borderTopWidth={1} borderColor="$borderColor">
                <SizableText flex={1} size="$2" color="$ink" numberOfLines={1}>
                  {day(b.date) || b.number || b.id}
                </SizableText>
                <SizableText width={90} size="$2" color="$ink" text="end">
                  {money(b.total, b.currency)}
                </SizableText>
                <SizableText width={70} size="$1" color="$soft" $max-sm={{ display: 'none' }}>
                  {word(b.status)}
                </SizableText>
                <XStack width={64} justify="flex-end">
                  <Button size="sm" variant="ghost" aria-label={t('View invoice {invoice}', { invoice: b.number || day(b.date) })} onPress={() => setOpen(b)}>
                    {t('View')}
                  </Button>
                </XStack>
              </XStack>
            ))}
          </Card>
        )}
      </Group>

      {plan ? (
        <Group title={t('Cancellation')}>
          <Card>
            <Row
              first
              title={plan.ending ? t('Your plan ends on {date}', { date: day(plan.ends) }) : t('Cancel plan')}
              detail={plan.ending ? t('Keep it, and it renews as before.') : t('It runs to the end of the period you paid for, then ends.')}
              trailing={
                !host.admin ? undefined : plan.ending ? (
                  <Button size="sm" variant="outline" disabled={working} onPress={() => void act(() => reactivate(target, plan.id), t('Your plan renews as before'), subs.reload)}>
                    {t('Keep plan')}
                  </Button>
                ) : (
                  <Button size="sm" variant="destructive" disabled={working} onPress={() => setEnding(true)}>
                    {t('Cancel')}
                  </Button>
                )
              }
            />
          </Card>
        </Group>
      ) : null}

      <Note>{note}</Note>

      <AddCard
        open={adding}
        onOpenChange={setAdding}
        onSaved={(m) => {
          reloadCards()
          toast.success(t('{card} is saved', { card: method(m, t) }))
        }}
      />
      <EditBilling
        open={editingBilling !== null}
        onOpenChange={(o) => !o && setEditingBilling(null)}
        card={editingBilling}
        onSaved={() => {
          reloadCards()
        }}
      />
      {plan ? (
        <Ending
          plan={plan}
          open={ending}
          working={working}
          onOpenChange={setEnding}
          onConfirm={() =>
            act(() => cancel(target, plan.id), t('Your plan ends on {date}', { date: day(plan.ends) }), () => {
              setEnding(false)
              subs.reload()
            })
          }
        />
      ) : null}
      {open ? <Bill invoice={open} onClose={() => setOpen(null)} /> : null}
    </YStack>
  )
}

function EditBilling({
  open,
  onOpenChange,
  card,
  onSaved,
}: {
  open: boolean
  onOpenChange: (o: boolean) => void
  card: Method | null
  onSaved: () => void
}) {
  const { t } = useI18n()
  const target = useTarget()
  const [name, setName] = useState('')
  const [line1, setLine1] = useState('')
  const [line2, setLine2] = useState('')
  const [city, setCity] = useState('')
  const [stateVal, setStateVal] = useState('')
  const [postalCode, setPostalCode] = useState('')
  const [country, setCountry] = useState('US')
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')

  useEffect(() => {
    if (card?.billingAddress) {
      setName(card.billingAddress.name ?? '')
      setLine1(card.billingAddress.line1 ?? '')
      setLine2(card.billingAddress.line2 ?? '')
      setCity(card.billingAddress.city ?? '')
      setStateVal(card.billingAddress.state ?? '')
      setPostalCode(card.billingAddress.postalCode ?? '')
      setCountry(card.billingAddress.country ?? 'US')
    } else {
      setName('')
      setLine1('')
      setLine2('')
      setCity('')
      setStateVal('')
      setPostalCode('')
      setCountry('US')
    }
    setNote('')
  }, [card])

  const save = async () => {
    if (!card) return
    setWorking(true)
    setNote('')
    try {
      const address: Address = {
        name: name.trim() || undefined,
        line1: line1.trim() || undefined,
        line2: line2.trim() || undefined,
        city: city.trim() || undefined,
        state: stateVal.trim() || undefined,
        postalCode: postalCode.trim() || undefined,
        country: country.trim() || undefined,
      }
      await updateMethod(target, card.id, { billingAddress: address })
      onSaved()
      onOpenChange(false)
      toast.success(t('Billing details updated'))
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setWorking(false)
    }
  }

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent maxW={460} showCloseButton={false}>
        <XStack items="center" justify="space-between" gap="$2">
          <DialogTitle>{t('Billing details')}</DialogTitle>
          <XStack render="button" aria-label={t('Close')} p="$1" onPress={() => onOpenChange(false)}>
            <X size={16} />
          </XStack>
        </XStack>
        <YStack gap="$3">
          <SizableText size="$2" color="$soft">
            {t('These details appear on your invoices and receipts.')}
          </SizableText>
          <Field label={t('Business or personal name')}>
            <Input value={name} onChangeText={setName} placeholder={t('e.g. Webby AI, Inc. or Joshua Lynch')} />
          </Field>
          <Field label={t('Address line 1')}>
            <Input value={line1} onChangeText={setLine1} placeholder={t('Street address or P.O. Box')} />
          </Field>
          <Field label={t('Address line 2')} hint={t('Optional')}>
            <Input value={line2} onChangeText={setLine2} placeholder={t('Suite, unit, building, floor')} />
          </Field>
          <XStack gap="$2">
            <YStack flex={1}>
              <Field label={t('City')}>
                <Input value={city} onChangeText={setCity} placeholder={t('City')} />
              </Field>
            </YStack>
            <YStack width={100}>
              <Field label={t('State')}>
                <Input value={stateVal} onChangeText={setStateVal} placeholder={t('State / Prov')} />
              </Field>
            </YStack>
          </XStack>
          <XStack gap="$2">
            <YStack flex={1}>
              <Field label={t('Postal / ZIP code')}>
                <Input value={postalCode} onChangeText={setPostalCode} placeholder={t('ZIP or Postal Code')} />
              </Field>
            </YStack>
            <YStack width={110}>
              <Field label={t('Country')}>
                <Input value={country} onChangeText={setCountry} placeholder={t('Country (US)')} />
              </Field>
            </YStack>
          </XStack>
          {note ? (
            <SizableText size="$2" color="$red10" role="alert">
              {note}
            </SizableText>
          ) : null}
          <XStack gap="$2" justify="flex-end" pt="$2">
            <Button size="sm" variant="ghost" disabled={working} onPress={() => onOpenChange(false)}>
              {t('Cancel')}
            </Button>
            <Button size="sm" disabled={working} onPress={() => void save()}>
              {t('Save details')}
            </Button>
          </XStack>
        </YStack>
      </DialogContent>
    </Dialog>
  )
}

/** What a plan costs by its interval, alone and for its seats: one sentence each, so a language says the interval its own way. */
const COST: Record<string, { one: string; seats: string }> = {
  month: { one: phrase('{amount} a month.'), seats: phrase('{amount} a month for {seats} seats.') },
  year: { one: phrase('{amount} a year.'), seats: phrase('{amount} a year for {seats} seats.') },
}

/** What the plan costs, and when it renews or ends, in the words of `t`. */
function terms(p: Subscription, t: Say): string {
  const amount = money(p.price * p.seats, p.currency)
  const by = COST[p.interval]
  const cost = by
    ? t(p.seats > 1 ? by.seats : by.one, { amount, seats: p.seats })
    : p.seats > 1
      ? t('{amount} a {interval} for {seats} seats.', { amount, interval: p.interval, seats: p.seats })
      : t('{amount} a {interval}.', { amount, interval: p.interval })
  if (!p.ends) return cost
  return `${cost} ${p.ending ? t('Ends on {date}.', { date: day(p.ends) }) : t('Renews on {date}.', { date: day(p.ends) })}`
}

/** A saved method as a row names it (api/billing.ts `label`); a card with no brand is said in the words of `t`. */
const method = (m: Method, t: Say): string => (m.last4 && !m.brand ? t('Card •••• {last4}', { last4: m.last4 }) : label(m))

const word = (s: string): string => (s ? s.charAt(0).toUpperCase() + s.slice(1).replace(/_/g, ' ') : '')

function Ending({
  plan,
  open,
  working,
  onOpenChange,
  onConfirm,
}: {
  plan: Subscription
  open: boolean
  working: boolean
  onOpenChange: (o: boolean) => void
  onConfirm: () => Promise<void>
}) {
  const { t } = useI18n()
  return (
    <Confirm
      open={open}
      onOpenChange={(o) => !working && onOpenChange(o)}
      title={t('Cancel your plan?')}
      says={
        day(plan.ends)
          ? t('{plan} stays on until {date}, and nothing is charged after that. You can keep it any time before then.', { plan: plan.name, date: day(plan.ends) })
          : t('{plan} stays on until the end of this period, and nothing is charged after that. You can keep it any time before then.', { plan: plan.name })
      }
      act={t('Cancel plan')}
      no={t('Keep plan')}
      run={onConfirm}
    />
  )
}

/** One invoice, read in the page, with its PDF to keep. Drawn while it is open, so each opens anew. */
function Bill({ invoice: b, onClose }: { invoice: Invoice; onClose: () => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const [note, setNote] = useState('')
  const [working, setWorking] = useState(false)

  const keep = async () => {
    setWorking(true)
    setNote('')
    try {
      const blob = await pdf(target, b.id)
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `${b.number || b.id}.pdf`
      a.click()
      window.setTimeout(() => URL.revokeObjectURL(url), 1000)
    } catch (e) {
      setNote(told(e, t))
    } finally {
      setWorking(false)
    }
  }

  const line = (what: string, cents: number, strong = false) => (
    <XStack key={what} justify="space-between" gap="$3">
      <SizableText size="$2" color={strong ? '$ink' : '$soft'}>
        {what}
      </SizableText>
      <SizableText size="$2" color={strong ? '$ink' : '$soft'}>
        {money(cents, b.currency)}
      </SizableText>
    </XStack>
  )

  return (
    <Dialog open onOpenChange={(o) => !o && onClose()}>
      <DialogContent maxW={480} showCloseButton={false}>
        <XStack items="center" justify="space-between" gap="$2">
          <DialogTitle>{b.number ? t('Invoice {number}', { number: b.number }) : t('Invoice')}</DialogTitle>
          <XStack render="button" aria-label={t('Close')} p="$1" onPress={onClose}>
            <X size={16} />
          </XStack>
        </XStack>
        <YStack gap="$3">
          <SizableText size="$2" color="$soft">
            {[day(b.date), word(b.status), b.start && b.end ? t('for {start} – {end}', { start: day(b.start), end: day(b.end) }) : ''].filter(Boolean).join(' · ')}
          </SizableText>
          <YStack gap="$1.5" borderTopWidth={1} borderColor="$borderColor" pt="$3">
            {b.lines.length ? b.lines.map((l, i) => line(l.description || t('Line {n}', { n: i + 1 }), l.amount)) : line(t('Subtotal'), b.subtotal)}
          </YStack>
          <YStack gap="$1.5" borderTopWidth={1} borderColor="$borderColor" pt="$3">
            {b.discount ? line(t('Discount'), -b.discount) : null}
            {b.tax ? line(t('Tax'), b.tax) : null}
            {line(t('Total'), b.total, true)}
            {b.credit ? line(t('Credit applied'), -b.credit) : null}
            {line(t('Paid'), b.paid)}
            {b.due ? line(t('Due'), b.due, true) : null}
          </YStack>
          <Note>{note}</Note>
          <XStack justify="flex-end">
            <Button size="sm" variant="outline" disabled={working} onPress={() => void keep()}>
              <Download size={14} /> {t('Download PDF')}
            </Button>
          </XStack>
        </YStack>
      </DialogContent>
    </Dialog>
  )
}
