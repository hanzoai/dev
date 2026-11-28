/**
 * Adding a card. The card number is typed into the payment processor's own
 * field — its iframe, served from its own host — and never reaches this page or
 * the platform: the field hands back a single-use token, and the platform vaults
 * the card from that. Which processor, and its sandbox or live account, is the
 * platform's answer at /v1/billing/settings, so the card is saved to the account
 * it will be charged on.
 *
 * Its words are the reader's language (@hanzo/i18n `t`); what the processor's
 * field says of a card is the processor's, drawn as given.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { ChevronDown, ChevronUp, X } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogTitle, Input } from '@hanzo/ui'
import { useEffect, useRef, useState } from 'react'

import { processor, save, type Address, type Method, type Processor } from '../api/billing.ts'
import { told } from '../api/call.ts'
import { useRead } from '../data.ts'
import { useTarget } from '../host.tsx'
import { Field as FormField } from './ui.tsx'

/** The part of Square's Web Payments SDK this uses. */
interface Field {
  attach(el: HTMLElement): Promise<void>
  tokenize(): Promise<{ status: string; token?: string; errors?: { message?: string }[] }>
  destroy(): Promise<void>
}
interface Sdk {
  payments(application: string, location: string): { card(options?: object): Promise<Field> }
}

declare global {
  interface Window {
    Square?: Sdk
  }
}

/** Anything but production loads the sandbox SDK, so a misread setting never arms real charges. */
export const sdk = (environment: string): string =>
  environment === 'production' ? 'https://web.squarecdn.com/v1/square.js' : 'https://sandbox.web.squarecdn.com/v1/square.js'

const loads = new Map<string, Promise<Sdk>>()

function load(environment: string): Promise<Sdk> {
  if (window.Square) return Promise.resolve(window.Square)
  const src = sdk(environment)
  const was = loads.get(src)
  if (was) return was
  const p = new Promise<Sdk>((resolve, reject) => {
    const s = document.createElement('script')
    s.src = src
    s.async = true
    s.onload = () => (window.Square ? resolve(window.Square) : reject(new Error(phrase('The card form did not load'))))
    s.onerror = () => {
      loads.delete(src)
      reject(new Error(phrase('The card form did not load')))
    }
    document.head.appendChild(s)
  })
  loads.set(src, p)
  return p
}

const ready = (p: Processor | null): p is Processor => Boolean(p && p.provider === 'square' && p.application && p.location)

export function AddCard({ open, onOpenChange, onSaved }: { open: boolean; onOpenChange: (o: boolean) => void; onSaved: (m: Method) => void }) {
  const { t } = useI18n()
  const target = useTarget()
  const cfg = useRead(open ? () => processor(target) : null, null as Processor | null, [target, open])
  const [box, setBox] = useState<HTMLDivElement | null>(null)
  const field = useRef<Field | null>(null)
  const [mounted, setMounted] = useState(false)
  const [working, setWorking] = useState(false)
  const [note, setNote] = useState('')
  const [name, setName] = useState('')
  const [postalCode, setPostalCode] = useState('')
  const [moreAddress, setMoreAddress] = useState(false)
  const [line1, setLine1] = useState('')
  const [city, setCity] = useState('')
  const [stateVal, setStateVal] = useState('')
  const [country, setCountry] = useState('US')
  const p = cfg.value

  useEffect(() => {
    if (!open || !box || !ready(p)) return
    let gone = false
    setMounted(false)
    setNote('')
    load(p.environment)
      .then((square) => square.payments(p.application, p.location).card())
      .then(async (f) => {
        if (gone) {
          void f.destroy().catch(() => {})
          return
        }
        await f.attach(box)
        field.current = f
        setMounted(true)
      })
      .catch((e: unknown) => {
        if (!gone) setNote(e instanceof Error ? told(e, t) : t('The card form did not load'))
      })
    return () => {
      gone = true
      const f = field.current
      field.current = null
      setMounted(false)
      if (f) void f.destroy().catch(() => {})
    }
  }, [open, box, p])

  // Save is offered only while the field is drawn.
  const add = async () => {
    const f = field.current!
    setWorking(true)
    setNote('')
    try {
      const r = await f.tokenize()
      if (r.status !== 'OK' || !r.token) throw new Error(r.errors?.map((e) => e.message).filter(Boolean).join('; ') || phrase('The card details are incomplete'))
      const billingAddress: Address | undefined =
        name.trim() || postalCode.trim() || line1.trim() || city.trim() || stateVal.trim()
          ? {
              name: name.trim() || undefined,
              postalCode: postalCode.trim() || undefined,
              line1: line1.trim() || undefined,
              city: city.trim() || undefined,
              state: stateVal.trim() || undefined,
              country: country.trim() || 'US',
            }
          : undefined
      const m = await save(target, r.token, billingAddress)
      onSaved(m)
      onOpenChange(false)
    } catch (e) {
      setNote(e instanceof Error ? told(e, t) : t('The card was not saved'))
    } finally {
      setWorking(false)
    }
  }

  const nonSquareConfigured = Boolean(p && p.provider && p.provider !== 'square')
  const why = cfg.error
    ? told(cfg.error, t)
    : !cfg.loading && !ready(p)
      ? nonSquareConfigured
        ? t('Cards for your region are added securely on the payment page.')
        : t('This deployment takes no cards yet.')
      : ''
  const payCardsHref =
    typeof window !== 'undefined'
      ? `https://hanzo.ai/pay/cards?returnUrl=${encodeURIComponent(window.location.href)}`
      : 'https://hanzo.ai/pay/cards'

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent maxW={460} showCloseButton={false}>
        <XStack items="center" justify="space-between" gap="$2">
          <DialogTitle>{t('Add a card')}</DialogTitle>
          <XStack render="button" aria-label={t('Close')} p="$1" onPress={() => onOpenChange(false)}>
            <X size={16} />
          </XStack>
        </XStack>
        <YStack gap="$3">
          {why ? (
            <SizableText size="$2" color="$soft">
              {why}
            </SizableText>
          ) : (
            <>
              <YStack minH={96} aria-label={t('Card details')}>
                <div ref={setBox} style={{ minHeight: 96 }} />
                {!mounted && !note ? (
                  <SizableText size="$1" color="$soft">
                    {t('Loading the card form…')}
                  </SizableText>
                ) : null}
              </YStack>

              <XStack gap="$2">
                <YStack flex={1}>
                  <FormField label={t('Name on card')} hint={t('Optional')}>
                    <Input value={name} onChangeText={setName} placeholder={t('Name or business')} />
                  </FormField>
                </YStack>
                <YStack width={120}>
                  <FormField label={t('Postal code')} hint={t('Optional')}>
                    <Input value={postalCode} onChangeText={setPostalCode} placeholder={t('ZIP / Postal')} />
                  </FormField>
                </YStack>
              </XStack>

              <XStack>
                <Button size="sm" variant="ghost" onPress={() => setMoreAddress(!moreAddress)}>
                  {moreAddress ? <ChevronUp size={14} /> : <ChevronDown size={14} />}
                  {moreAddress ? t('Hide address') : t('Add billing address')}
                </Button>
              </XStack>

              {moreAddress ? (
                <YStack gap="$2" pt="$1">
                  <FormField label={t('Street address')}>
                    <Input value={line1} onChangeText={setLine1} placeholder={t('Address line 1')} />
                  </FormField>
                  <XStack gap="$2">
                    <YStack flex={1}>
                      <FormField label={t('City')}>
                        <Input value={city} onChangeText={setCity} placeholder={t('City')} />
                      </FormField>
                    </YStack>
                    <YStack width={90}>
                      <FormField label={t('State')}>
                        <Input value={stateVal} onChangeText={setStateVal} placeholder={t('State')} />
                      </FormField>
                    </YStack>
                    <YStack width={90}>
                      <FormField label={t('Country')}>
                        <Input value={country} onChangeText={setCountry} placeholder={t('Country (US)')} />
                      </FormField>
                    </YStack>
                  </XStack>
                </YStack>
              ) : null}

              <SizableText size="$1" color="$soft">
                {p && p.environment !== 'production'
                  ? t('Sandbox: use a test card. Nothing is charged.')
                  : t('The card number goes to the payment processor, never to Hanzo. Saving it charges nothing.')}
              </SizableText>
            </>
          )}
          {note ? (
            <SizableText size="$2" color="$red10" role="alert">
              {note}
            </SizableText>
          ) : null}
          <XStack gap="$2" justify="flex-end">
            <Button size="sm" variant="ghost" onPress={() => onOpenChange(false)}>
              {t('Cancel')}
            </Button>
            {nonSquareConfigured ? (
              <Button size="sm" onPress={() => window.location.assign(payCardsHref)}>
                {t('Add card on hanzo.ai/pay')}
              </Button>
            ) : (
              <Button size="sm" disabled={!mounted || working} onPress={() => void add()}>
                {t('Save card')}
              </Button>
            )}
          </XStack>
        </YStack>
      </DialogContent>
    </Dialog>
  )
}
