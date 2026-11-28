/**
 * Markdown an agent writes, drawn as text (markdown.ts reads it).
 *
 * @hanzo/ui leaves the markdown pipeline to the surface, and the builder needs
 * only the part agents use. Nothing here produces HTML from the text: every
 * piece is a React element holding a string, and a link is drawn only for an
 * http(s) address, opened in a new tab.
 *
 * Set at Chat's sizes (@hanzo/rooms `Prose`): running text at $2 on a $3 line,
 * headings a rung apart from $6, code in @hanzo/ui's `Code`, blocks $2 apart,
 * so an answer reads the same in Chat and in a run.
 */
import { ScrollView, SizableText, XStack, YStack } from '@hanzo/gui'
import { Code } from '@hanzo/ui/chat'
import type { ReactNode } from 'react'

import { blocks, spans, type Block } from './markdown.ts'
import { Out } from './out.tsx'

const mono = { fontFamily: 'var(--f-mono, ui-monospace, monospace)' }

function Inline({ text }: { text: string }) {
  return (
    <>
      {spans(text).map((s, i) => {
        switch (s.kind) {
          case 'code':
            return (
              <SizableText key={i} size="$1" color="$ink" bg="$hover" px="$1" rounded="$1" style={mono}>
                {s.text}
              </SizableText>
            )
          case 'strong':
            return (
              <SizableText key={i} size="$2" lineHeight="$3" color="$ink" fontWeight="600">
                {s.text}
              </SizableText>
            )
          case 'em':
            return (
              <SizableText key={i} size="$2" lineHeight="$3" color="$ink" fontStyle="italic">
                {s.text}
              </SizableText>
            )
          case 'link':
            return (
              <Out key={i} href={s.href}>
                <SizableText size="$2" lineHeight="$3" color="$ink" textDecorationLine="underline">
                  {s.text}
                </SizableText>
              </Out>
            )
          default:
            return s.text
        }
      })}
    </>
  )
}

const SIZES = ['$6', '$4', '$3', '$2', '$2', '$2'] as const

function Draw({ block }: { block: Block }): ReactNode {
  switch (block.kind) {
    case 'heading':
      return (
        <SizableText size={SIZES[block.level - 1]!} color="$ink" fontWeight="600">
          <Inline text={block.text} />
        </SizableText>
      )
    case 'code':
      return (
        <Code language={block.lang || 'text'} value={block.text}>
          {block.text}
        </Code>
      )
    case 'rule':
      return <YStack height={1} bg="$borderColor" my="$1" />
    case 'quote':
      return (
        <YStack borderLeftWidth={2} borderColor="$borderColor" pl="$3">
          <SizableText size="$2" lineHeight="$3" color="$soft">
            <Inline text={block.text} />
          </SizableText>
        </YStack>
      )
    case 'list':
      return (
        <YStack gap="$1" role="list">
          {block.items.map((item, i) => (
            <XStack key={i} gap="$1" pl={item.depth * 16} role="listitem">
              <SizableText size="$2" lineHeight="$3" color="$soft" minW={14} style={{ textAlign: 'right' }}>
                {block.ordered ? `${block.start + i}.` : '•'}
              </SizableText>
              <SizableText flex={1} minW={0} size="$2" lineHeight="$3" color="$ink">
                <Inline text={item.text} />
              </SizableText>
            </XStack>
          ))}
        </YStack>
      )
    case 'table':
      return (
        <ScrollView horizontal showsHorizontalScrollIndicator={false}>
          <YStack borderWidth={1} borderColor="$borderColor" rounded="$3" overflow="hidden" minW="100%">
            {[block.head, ...block.rows].map((row, r) => (
              <XStack key={r} borderTopWidth={r ? 1 : 0} borderColor="$borderColor" bg={r ? 'transparent' : '$hover'}>
                {block.head.map((_, c) => (
                  <SizableText key={c} flex={1} minW={96} px="$2" py="$1.5" size="$2" color="$ink" fontWeight={r ? '400' : '600'}>
                    <Inline text={row[c] ?? ''} />
                  </SizableText>
                ))}
              </XStack>
            ))}
          </YStack>
        </ScrollView>
      )
    default:
      return (
        <SizableText size="$2" lineHeight="$3" color="$ink">
          <Inline text={block.text} />
        </SizableText>
      )
  }
}

/** A markdown text, drawn. */
export function Prose({ text }: { text: string }) {
  return (
    <YStack data-slot="prose" gap="$2" minW={0}>
      {blocks(text).map((b, i) => (
        <Draw key={i} block={b} />
      ))}
    </YStack>
  )
}
