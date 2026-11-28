/**
 * What sits under an answer, the same in Chat and Dev: copy what was written,
 * rate/vote, listen to it, share it, regenerate/redo it, branch in a new chat,
 * and open what it made in the side panel. A surface adds its own after these.
 *
 * An answer's artifacts are its fenced blocks (`artifacts`): a page, a picture,
 * a file it wrote out whole. One opens as a page tab in the side panel, rendered
 * from its own bytes (`blob`).
 */
import { SizableText, XStack, YStack } from '@hanzo/gui'
import { useI18n } from '@hanzo/i18n/react'
import {
  Dialog,
  DialogContent,
  DialogTitle,
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
  Input,
} from '@hanzo/ui'
import {
  ArrowUp,
  Check,
  Copy,
  GitBranch,
  Globe,
  Lightbulb,
  Link2,
  MoreHorizontal,
  PanelRight,
  RefreshCw,
  Share,
  Square,
  ThumbsDown,
  ThumbsUp,
  Volume2,
} from '@hanzogui/lucide-icons-2'
import { mouth, speech } from '@hanzo/voice'
import { useEffect, useMemo, useRef, useState, type ReactNode } from 'react'

import { copy as toClipboard } from './clipboard.ts'
import type { Target } from './api/call.ts'

/** A fenced block an answer wrote out. */
export interface Artifact {
  /** The name it was given, or what it is: `index.html`, `html block`. */
  name: string
  body: string
  /** What a browser should take it as. */
  mime: string
}

export interface RedoAction {
  mode: 'retry' | 'think' | 'web' | 'prompt'
  prompt?: string
}

const FENCE = /```([^\n`]*)\n([\s\S]*?)```/g

const MIME: Record<string, string> = {
  html: 'text/html',
  htm: 'text/html',
  svg: 'image/svg+xml',
  css: 'text/css',
  csv: 'text/csv',
  json: 'application/json',
  md: 'text/markdown',
  markdown: 'text/markdown',
  txt: 'text/plain',
}

const suffix = (s: string): string => {
  const dot = s.lastIndexOf('.')
  return dot > 0 ? s.slice(dot + 1).toLowerCase() : ''
}

/** An info string is a language, a file name (a dot, no spaces), or both (`html index.html`): the file name wins. */
const called = (info: string, n: number): string => {
  const words = info.trim().split(/\s+/).filter(Boolean)
  const file = words.find((w) => w.includes('.') && suffix(w))
  if (file) return file
  return words[0] ? `${words[0]} block` : `Block ${n}`
}

/** Every fenced block in `text`, in order. */
export function artifacts(text: string): Artifact[] {
  const out: Artifact[] = []
  let n = 0
  for (const [, info = '', body = ''] of text.matchAll(FENCE)) {
    n += 1
    const name = called(info, n)
    out.push({ name, body, mime: MIME[suffix(name) || info.trim().toLowerCase().split(/\s+/)[0] || ''] ?? 'text/plain' })
  }
  return out
}

/** Whether a browser draws it as a page or a picture rather than as source. */
export const renders = (a: Artifact): boolean => a.mime === 'text/html' || a.mime === 'image/svg+xml'

/** The artifact worth opening first: the last page or picture, else the last block. */
export function lead(list: readonly Artifact[]): Artifact | null {
  return [...list].reverse().find(renders) ?? list[list.length - 1] ?? null
}

/** An address for an artifact's own bytes, typed so a browser renders it. */
export const blob = (a: Artifact): string => URL.createObjectURL(new Blob([a.body], { type: a.mime }))

/** Markdown as a sentence a voice can read: code said as code, marks dropped. */
export function speakable(raw: string): string {
  return raw
    .replace(/```[\s\S]*?```/g, ' [code snippet] ')
    .replace(/`([^`]+)`/g, '$1')
    .replace(/\[([^\]]+)\]\([^)]+\)/g, '$1')
    .replace(/^#{1,6}\s+/gm, '')
    .replace(/[*_]{1,3}([^*_]+)[*_]{1,3}/g, '$1')
    .replace(/^>\s+/gm, '')
    .replace(/^[-*_]{3,}\s*$/gm, '')
    .replace(/^[\s]*[-+*]\s+/gm, '')
    .replace(/\n+/g, '. ')
    .replace(/\s+/g, ' ')
    .trim()
}

/** Reads `text` aloud, calls `done` when it stops for any reason, and answers the way to stop it. */
export type Listen = (text: string, done: () => void) => () => void

let hush: (() => void) | null = null

/** A reader on the platform's voice (`/v1/audio/speech`), one reply at a time on the page. */
export function speaker(t: Target): Listen {
  return (text, done) => {
    hush?.()
    const said = speakable(text)
    if (!said) {
      done()
      return () => {}
    }
    const lips = mouth({ speech: speech({ baseUrl: t.api, token: t.token }) })
    const stop = () => {
      lips.hush()
      if (hush === stop) hush = null
    }
    hush = stop
    lips.say(said).then(done, done)
    return stop
  }
}

export type Vote = 'up' | 'down'

/** Show a verdict at once and send it; if sending fails, put the earlier one back, unless the person has voted again since (`last` counts the votes). */
export function rated(
  prev: Vote | null,
  next: Vote,
  send: ((vote: Vote) => void | Promise<void>) | undefined,
  set: (to: Vote | null) => void,
  last: { n: number },
): void {
  const mine = ++last.n
  set(next)
  if (!send) return
  Promise.resolve()
    .then(() => send(next))
    .catch(() => {
      if (last.n === mine) set(prev)
    })
}

/** One quiet control in the row. It rests dim and comes up under a pointer or focus. */
function Act({
  label,
  onPress,
  children,
  lit = false,
  slot,
}: {
  label: string
  onPress?: () => void
  children: ReactNode
  lit?: boolean
  slot?: string
}) {
  return (
    <XStack
      render="button"
      role="button"
      aria-label={label}
      data-slot={slot}
      onPress={onPress}
      items="center"
      justify="center"
      gap="$1"
      px="$1.5"
      height={28}
      minW={28}
      rounded="$2"
      cursor="pointer"
      bg={lit ? '$hover' : 'transparent'}
      opacity={lit ? 1 : 0.65}
      hoverStyle={{ opacity: 1, bg: '$hover' }}
      focusStyle={{ opacity: 1 }}
    >
      {children}
    </XStack>
  )
}

/** Copy, Rate/Vote, Listen, Share, Redo, Branch and Open under an answer; `children` follow them. */
export function Reply({
  text,
  title,
  listen,
  onOpen,
  onRedo,
  onBranch,
  onRate,
  timestamp,
  children,
}: {
  text: string
  title?: string
  listen?: Listen
  onOpen?: () => void
  onRedo?: (action: RedoAction) => void
  onBranch?: () => void
  onRate?: (vote: Vote) => void | Promise<void>
  timestamp?: string
  children?: ReactNode
}) {
  const { t: say } = useI18n()
  const [took, setTook] = useState(false)
  const [speaking, setSpeaking] = useState(false)
  const [vote, setVote] = useState<Vote | null>(null)
  const votes = useRef({ n: 0 })
  const [redoOpen, setRedoOpen] = useState(false)
  const [shareOpen, setShareOpen] = useState(false)
  const [shareCopied, setShareCopied] = useState(false)
  const [redoPrompt, setRedoPrompt] = useState('')
  const stop = useRef<(() => void) | null>(null)

  const stamp = useMemo(() => {
    if (timestamp) return timestamp
    try {
      return new Intl.DateTimeFormat(undefined, {
        month: 'short',
        day: 'numeric',
        year: 'numeric',
        hour: 'numeric',
        minute: '2-digit',
      }).format(new Date())
    } catch {
      return 'Just now'
    }
  }, [timestamp])

  useEffect(() => {
    if (!took) return
    const t = setTimeout(() => setTook(false), 1400)
    return () => clearTimeout(t)
  }, [took])

  useEffect(() => {
    if (!shareCopied) return
    const t = setTimeout(() => setShareCopied(false), 1600)
    return () => clearTimeout(t)
  }, [shareCopied])

  useEffect(() => () => stop.current?.(), [])

  const shareUrl = useMemo(() => {
    if (typeof window === 'undefined') return 'https://hanzo.ai/chat'
    return window.location.href
  }, [shareOpen])

  const triggerRedo = (payload: RedoAction) => {
    setRedoOpen(false)
    setRedoPrompt('')
    onRedo?.(payload)
  }

  const rate = (next: Vote) => rated(vote, next, onRate, setVote, votes.current)

  return (
    <XStack data-slot="reply" ml="$-1" items="center" gap="$1" flexWrap="wrap">
      <Act
        slot="reply-copy"
        label={took ? say('Copied') : say('Copy this answer')}
        lit={took}
        onPress={() => void toClipboard(text).then((ok) => ok && setTook(true))}
      >
        {took ? <Check size={15} /> : <Copy size={15} />}
      </Act>

      <DropdownMenu>
        <DropdownMenuTrigger asChild>
          <Act slot="reply-rate" label={say('Rate this response')} lit={vote !== null}>
            {vote === 'down' ? <ThumbsDown size={15} /> : <ThumbsUp size={15} />}
          </Act>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="start" minW={176} data-slot="reply-rate-menu">
          <DropdownMenuItem
            data-slot="reply-rate-up"
            onSelect={() => rate('up')}
          >
            <XStack items="center" gap="$2">
              <ThumbsUp size={15} />
              <SizableText size="$2" color="$ink">
                {say('Good response')}
              </SizableText>
            </XStack>
          </DropdownMenuItem>
          <DropdownMenuItem
            data-slot="reply-rate-down"
            onSelect={() => rate('down')}
          >
            <XStack items="center" gap="$2">
              <ThumbsDown size={15} />
              <SizableText size="$2" color="$ink">
                {say('Bad response')}
              </SizableText>
            </XStack>
          </DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>

      {listen ? (
        <Act
          slot="reply-listen"
          label={speaking ? say('Stop reading') : say('Listen to this answer')}
          lit={speaking}
          onPress={() => {
            if (speaking) {
              stop.current?.()
              setSpeaking(false)
              return
            }
            setSpeaking(true)
            stop.current = listen(text, () => setSpeaking(false))
          }}
        >
          {speaking ? <Square size={14} /> : <Volume2 size={15} />}
        </Act>
      ) : null}

      <Act slot="reply-share" label={say('Share message')} lit={shareOpen} onPress={() => setShareOpen(true)}>
        <Share size={15} />
      </Act>

      {onRedo ? (
        <DropdownMenu open={redoOpen} onOpenChange={setRedoOpen}>
          <DropdownMenuTrigger asChild>
            <Act slot="reply-redo" label={say('Regenerate response')} lit={redoOpen}>
              <RefreshCw size={15} />
            </Act>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="start" minW={232} data-slot="reply-redo-menu">
            <XStack items="center" gap="$1.5" px="$2" py="$1.5">
              <Input
                data-slot="reply-redo-input"
                placeholder={say('Ask to change response')}
                value={redoPrompt}
                onChangeText={setRedoPrompt}
                onKeyDown={(e: { key?: string; preventDefault?: () => void; stopPropagation?: () => void }) => {
                  e.stopPropagation?.()
                  if (e.key === 'Enter' && redoPrompt.trim()) {
                    e.preventDefault?.()
                    triggerRedo({ mode: 'prompt', prompt: redoPrompt.trim() })
                  }
                }}
                flex={1}
                size="sm"
              />
              <Act
                label={say('Submit change')}
                lit={Boolean(redoPrompt.trim())}
                onPress={() => {
                  if (redoPrompt.trim()) {
                    triggerRedo({ mode: 'prompt', prompt: redoPrompt.trim() })
                  }
                }}
              >
                <ArrowUp size={14} />
              </Act>
            </XStack>
            <DropdownMenuSeparator />
            <DropdownMenuItem data-slot="redo-retry" onSelect={() => triggerRedo({ mode: 'retry' })}>
              <XStack items="center" gap="$2">
                <RefreshCw size={15} />
                <SizableText size="$2" color="$ink">
                  {say('Try again')}
                </SizableText>
                <SizableText size="$2" color="$hint">
                  · Auto
                </SizableText>
              </XStack>
            </DropdownMenuItem>
            <DropdownMenuItem data-slot="redo-think" onSelect={() => triggerRedo({ mode: 'think' })}>
              <XStack items="center" gap="$2">
                <Lightbulb size={15} />
                <SizableText size="$2" color="$ink">
                  {say('Think longer')}
                </SizableText>
              </XStack>
            </DropdownMenuItem>
            <DropdownMenuItem data-slot="redo-web" onSelect={() => triggerRedo({ mode: 'web' })}>
              <XStack items="center" gap="$2">
                <Globe size={15} />
                <SizableText size="$2" color="$ink">
                  {say('Search the web')}
                </SizableText>
              </XStack>
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      ) : null}

      {onOpen ? (
        <Act slot="reply-open" label={say('Open in the side panel')} onPress={onOpen}>
          <PanelRight size={14} />
          <SizableText size="$1" color="$ink">
            {say('Open')}
          </SizableText>
        </Act>
      ) : null}

      {children}

      <Dialog open={shareOpen} onOpenChange={setShareOpen}>
        <DialogContent maxW={540} width="100%" gap="$3.5" data-slot="reply-share-modal">
          <DialogTitle>{title || say('Conversation')}</DialogTitle>
          <YStack
            p="$3.5"
            rounded="$4"
            bg="$panel"
            borderWidth={1}
            borderColor="$borderColor"
            maxH={220}
            overflow="scroll"
          >
            <SizableText size="$2" color="$ink" lineHeight="$4">
              {text}
            </SizableText>
          </YStack>
          <XStack items="flex-start" gap="$2.5">
            <Link2 size={16} />
            <YStack gap="$0.5" flex={1}>
              <SizableText size="$2" fontWeight="600" color="$ink">
                {say('Copy link to this run')}
              </SizableText>
              <SizableText size="$1" color="$soft">
                {say('Members of your workspace with access can open this link.')}
              </SizableText>
            </YStack>
          </XStack>
          <XStack items="center" justify="center" gap="$5" pt="$1">
            <YStack
              render="button"
              role="button"
              data-slot="share-copy-link"
              items="center"
              gap="$1.5"
              cursor="pointer"
              onPress={() => void toClipboard(shareUrl).then((ok) => ok && setShareCopied(true))}
            >
              <XStack width={44} height={44} rounded={999} bg="$hover" items="center" justify="center">
                {shareCopied ? <Check size={17} /> : <Link2 size={17} />}
              </XStack>
              <SizableText size="$1" color="$soft">
                {shareCopied ? say('Copied!') : say('Copy link')}
              </SizableText>
            </YStack>
            <YStack
              render="button"
              role="button"
              data-slot="share-x"
              items="center"
              gap="$1.5"
              cursor="pointer"
              onPress={() => {
                const snippet = text.length > 200 ? `${text.slice(0, 197)}...` : text
                window.open(`https://x.com/intent/tweet?text=${encodeURIComponent(snippet)}&url=${encodeURIComponent(shareUrl)}`, '_blank', 'noopener,noreferrer')
              }}
            >
              <XStack width={44} height={44} rounded={999} bg="$hover" items="center" justify="center">
                <SizableText size="$3" fontWeight="700" color="$ink">
                  𝕏
                </SizableText>
              </XStack>
              <SizableText size="$1" color="$soft">
                X
              </SizableText>
            </YStack>
            <YStack
              render="button"
              role="button"
              data-slot="share-linkedin"
              items="center"
              gap="$1.5"
              cursor="pointer"
              onPress={() => {
                window.open(`https://www.linkedin.com/sharing/share-offsite/?url=${encodeURIComponent(shareUrl)}`, '_blank', 'noopener,noreferrer')
              }}
            >
              <XStack width={44} height={44} rounded={999} bg="$hover" items="center" justify="center">
                <SizableText size="$3" fontWeight="700" color="$ink">
                  in
                </SizableText>
              </XStack>
              <SizableText size="$1" color="$soft">
                LinkedIn
              </SizableText>
            </YStack>
            <YStack
              render="button"
              role="button"
              data-slot="share-reddit"
              items="center"
              gap="$1.5"
              cursor="pointer"
              onPress={() => {
                window.open(`https://www.reddit.com/submit?url=${encodeURIComponent(shareUrl)}&title=${encodeURIComponent(title || 'Hanzo Chat')}`, '_blank', 'noopener,noreferrer')
              }}
            >
              <XStack width={44} height={44} rounded={999} bg="$hover" items="center" justify="center">
                <SizableText size="$3" fontWeight="700" color="$ink">
                  r/
                </SizableText>
              </XStack>
              <SizableText size="$1" color="$soft">
                Reddit
              </SizableText>
            </YStack>
          </XStack>
        </DialogContent>
      </Dialog>
    </XStack>
  )
}

/** Which model answered, where the gateway says it was not the one asked for or a router. */
export function Served(_props: { served: string; asked?: string }) {
  return null
}
