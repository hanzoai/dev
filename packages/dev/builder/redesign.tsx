/**
 * Redesign a site from its address, in New on This computer (hanzo-inc/app issue 312, hanzoai/desktop issue 15).
 *
 * An address pasted alone, or with a request to redesign it, is offered as a redesign and never taken for one on its
 * own: the offer says what Hanzo will do and one press does it, and an ask that only holds a link is sent as it is
 * (site.ts `urlOf`). The steps are the host's and the agent's (site.ts): the host opens the site and saves a copy
 * into a folder, serves that folder's `redesign/` live and shows it in Chrome; the agent reads the copy, writes a
 * critique as its answer and builds the new site there. The card says which step it is in, the live address, and how
 * to open it again; the preview keeps following the folder, so every later run in it shows as it is saved.
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import { ExternalLink, Globe } from '@hanzogui/lucide-icons-2'
import { Button, Dialog, DialogContent, DialogTitle, Input } from '@hanzo/ui'
import { useId, useMemo, useRef, useState, type ReactNode } from 'react'

import type { Folder, Local } from './local.ts'
import { plainAnswer } from './plain.ts'
import { addressOf, brief, folderName, hostOf, stopped, urlOf, type Asked, type Site } from './site.ts'

type Stage = 'opening' | 'building' | 'done' | 'failed'

/** One redesign, as the card shows it. */
interface Going {
  /** The site's address, and what a person calls it. */
  url: string
  host: string
  /** Where the redesign is built; null until the site is saved. */
  folder: Folder | null
  stage: Stage
  /** The live address, once it is being served. */
  preview: string | null
  /** The agent's critique and summary, once it has finished. */
  output: string
  /** What it did on the way, drawn only in Developer mode. */
  process: string
  /** How far the agent's first-run setup has got, while the host fetches it. */
  setup: number | null
  /** Why it stopped, when it did. */
  why: string
}

export interface Redesign {
  /** The site the draft asks for, when New is where a redesign can start. */
  found: Asked | null
  /** Whether a step of one is under way. */
  busy: boolean
  /** Start one. False when it could not begin. */
  start: (asked: Asked) => Promise<boolean>
  /** Ask for an address in a dialog, then start. */
  ask: () => void
  /** Whether this host can redesign a site at all. */
  can: boolean
  /** The offer, the dialog and the card, to draw above the composer. */
  view: ReactNode
}

export function useRedesign({
  local,
  site,
  here,
  draft,
  chosen,
  carried,
  devMode,
  started,
}: {
  local: Local | null
  site: Site | null
  /** New is on This computer. */
  here: boolean
  draft: string
  /** The folder the person chose themselves, or null for Auto: a chosen folder is built in, Auto makes a new one. */
  chosen: Folder | null
  /** What the run carries beside the ask: the skills and memory the person turned on. */
  carried: string
  /** Developer mode: the process is drawn as well as the answer. */
  devMode: boolean
  /** The ask was taken. */
  started: () => void
}): Redesign {
  const { t } = useI18n()
  const can = Boolean(site && local && here)
  const found = useMemo(() => (can ? urlOf(draft) : null), [can, draft])
  const [going, setGoing] = useState<Going | null>(null)
  const [asking, setAsking] = useState(false)
  const busy = going?.stage === 'opening' || going?.stage === 'building'
  // The folder being served, so a new redesign lets the last one go.
  const served = useRef<string | null>(null)

  const start = async (asked: Asked): Promise<boolean> => {
    if (!site || !local || busy) return false
    started()
    const host = hostOf(asked.url)
    const update = (patch: Partial<Going>) => setGoing((g) => (g ? { ...g, ...patch } : g))
    setGoing({ url: asked.url, host, folder: null, stage: 'opening', preview: null, output: '', process: '', setup: null, why: '' })
    try {
      if (served.current) {
        const last = served.current
        served.current = null
        await site.stop(last).catch(() => undefined)
      }
      const folder = chosen ?? (await site.folder(folderName(asked.url)))
      const saved = await site.capture(asked.url, folder.path)
      // The folder exists now, and is the one Auto picks next: what is said after this goes to the redesign.
      local.use(folder.path)
      const preview = await site.preview(folder.path)
      served.current = folder.path
      update({ folder, stage: 'building', preview: preview.url })
      // The preview opens at once, on the page that says the design is being built; a browser that will not open it
      // is no reason to stop, and the card has the address and the button.
      void site.show(preview.url).catch(() => undefined)
      const prompt = [carried, brief(saved, asked.note)].filter(Boolean).join('\n\n')
      const done = await local.run(folder.path, prompt, (percent) => update({ setup: percent }))
      const answer = { output: done.output.trim(), process: (done.process ?? '').trim() }
      update(done.ok ? { stage: 'done', ...answer } : { stage: 'failed', ...answer, why: t('Hanzo Dev stopped before it finished. Try again.') })
      return true
    } catch (e) {
      update({ stage: 'failed', why: stopped(e, t) })
      return false
    }
  }

  const show = () => {
    if (going?.preview) void site?.show(going.preview).catch(() => undefined)
  }

  const view = (
    <>
      {found && !busy ? <Offer host={hostOf(found.url)} onStart={() => void start(found)} /> : null}
      {going ? <Card going={going} devMode={devMode} onShow={show} /> : null}
      {site ? <Asking open={asking} onOpenChange={setAsking} onStart={(url) => void start({ url, note: '' })} /> : null}
    </>
  )

  return { found, busy, start, ask: () => setAsking(true), can, view }
}

/** The offer: what Hanzo will do with the address, and the press that does it. */
function Offer({ host, onStart }: { host: string; onStart: () => void }) {
  const { t } = useI18n()
  return (
    <XStack data-slot="redesign-offer" items="center" justify="space-between" gap="$3" rowGap="$2" mx="$3" px="$3" py="$2" flexWrap="wrap">
      <XStack items="center" gap="$2" flex={1} minW={180}>
        <Globe size={14} color="$soft" />
        <YStack flex={1} minW={0}>
          <SizableText size="$2" color="$ink" fontWeight="500">
            {t('Redesign {site}?', { site: host })}
          </SizableText>
          <SizableText size="$1" color="$soft">
            {t('Hanzo opens it, reviews it, and builds a new version here while you watch it in Chrome.')}
          </SizableText>
        </YStack>
      </XStack>
      <Button size="sm" variant="primary" onPress={onStart}>
        {t('Redesign this site')}
      </Button>
    </XStack>
  )
}

/** Where a redesign is, step by step, with the live address and what the agent said. */
function Card({ going, devMode, onShow }: { going: Going; devMode: boolean; onShow: () => void }) {
  const { t } = useI18n()
  const head =
    going.stage === 'opening'
      ? t('Opening {site}…', { site: going.host })
      : going.stage === 'building'
        ? t('Redesigning {site}…', { site: going.host })
        : going.stage === 'done'
          ? t('Redesigned {site}', { site: going.host })
          : t('Could not redesign {site}', { site: going.host })
  const fetching = going.stage === 'building' && going.setup !== null && going.setup < 100
  const said = going.output && going.folder ? plainAnswer(going.output, going.folder, t) : going.output
  const step =
    going.stage === 'opening'
      ? t('Opening the site and saving a copy to work from.')
      : going.stage === 'building'
        ? t('Reviewing the site and building the new design. The preview changes as the files are saved.')
        : going.why
  return (
    <YStack data-slot="redesign" role="status" aria-live="polite" gap="$1.5" mx="$3" px="$3" py="$2.5" rounded="$4" borderWidth={1} borderColor="$borderColor" bg="$background" minW={0}>
      <XStack items="center" gap="$2" minW={0}>
        <Globe size={13} color="$soft" />
        <SizableText size="$2" color="$ink" fontWeight="500" numberOfLines={1} flex={1} minW={0}>
          {head}
        </SizableText>
      </XStack>
      {step ? (
        <SizableText size="$1" color="$soft">
          {step}
        </SizableText>
      ) : null}
      {going.preview ? (
        <XStack data-slot="redesign-preview" items="center" justify="space-between" gap="$2" minW={0}>
          <SizableText size="$1" color="$soft" numberOfLines={1} flex={1} minW={0} data-i18n-ltr="">
            {t('Live preview: {address}', { address: going.preview })}
          </SizableText>
          <Button size="sm" variant="outline" onPress={onShow}>
            <XStack items="center" gap="$1.5">
              <ExternalLink size={13} color="$soft" />
              <SizableText size="$2" color="$ink">
                {t('Open in Chrome')}
              </SizableText>
            </XStack>
          </Button>
        </XStack>
      ) : null}
      {going.folder ? (
        <SizableText size="$1" color="$soft" numberOfLines={1} data-i18n-ltr="">
          {going.folder.path}
        </SizableText>
      ) : null}
      {fetching ? (
        <SizableText data-slot="redesign-setup" size="$1" color="$soft">
          {t('Setting up Hanzo Dev on this computer… {percent}%', { percent: going.setup })}
        </SizableText>
      ) : null}
      {said ? (
        <YStack maxH={320} overflowY="auto" minW={0}>
          <SizableText data-slot="redesign-answer" size="$2" color="$ink" whiteSpace="pre-wrap" wordWrap="break-word">
            {said}
          </SizableText>
        </YStack>
      ) : null}
      {devMode && going.process ? (
        <YStack data-slot="redesign-process" gap="$1" minW={0}>
          <SizableText size="$1" color="$soft" fontWeight="500">
            {t('What Hanzo ran')}
          </SizableText>
          <SizableText size="$1" color="$soft" fontFamily="$mono" whiteSpace="pre-wrap" wordWrap="break-word" maxH={240} overflowY="auto" data-i18n-ltr="">
            {going.process}
          </SizableText>
        </YStack>
      ) : null}
    </YStack>
  )
}

/** Asking for an address, for a person who has not pasted one. */
function Asking({ open, onOpenChange, onStart }: { open: boolean; onOpenChange: (o: boolean) => void; onStart: (url: string) => void }) {
  const { t } = useI18n()
  const id = useId()
  const [value, setValue] = useState('')
  const [note, setNote] = useState('')
  const close = (o: boolean) => {
    if (!o) {
      setValue('')
      setNote('')
    }
    onOpenChange(o)
  }
  const go = () => {
    const url = addressOf(value)
    if (!url) return setNote(t('Paste a site’s address, like https://example.com.'))
    close(false)
    onStart(url)
  }
  return (
    <Dialog open={open} onOpenChange={close}>
      <DialogContent maxW={480} showCloseButton={false}>
        <DialogTitle>{t('Redesign a site')}</DialogTitle>
        <SizableText size="$2" color="$soft">
          {t('Hanzo opens the site, reviews it, and builds a new version here while you watch it in Chrome.')}
        </SizableText>
        <YStack gap="$1.5">
          <SizableText render="label" size="$2" color="$soft" htmlFor={`${id}-site`}>
            {t('Site address')}
          </SizableText>
          <Input
            id={`${id}-site`}
            autoFocus
            value={value}
            onChangeText={setValue}
            placeholder="https://example.com"
            aria-label={t('Site address')}
            onKeyDown={(e: { key?: string }) => {
              if (e.key === 'Enter') go()
            }}
          />
        </YStack>
        {note ? (
          <SizableText size="$1" color="$soft" role="status">
            {note}
          </SizableText>
        ) : null}
        <XStack gap="$2" justify="flex-end">
          <Button size="sm" variant="ghost" onPress={() => close(false)}>
            {t('Cancel')}
          </Button>
          <Button size="sm" onPress={go}>
            {t('Redesign')}
          </Button>
        </XStack>
      </DialogContent>
    </Dialog>
  )
}
