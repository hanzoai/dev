/**
 * Finding a run: the rail's search icon. The org's coding runs as the platform
 * lists them, newest first, filtered by status and paged back as far as they
 * go; what is typed narrows the runs read so far by name and codebase. Until
 * the first page lands it searches the rail's own recents.
 */
import { ScrollView, SizableText, XStack, YStack } from '@hanzo/gui'
import { phrase } from '@hanzo/i18n'
import { useI18n } from '@hanzo/i18n/react'
import { Button, Dialog, DialogContent, DialogTitle, Input } from '@hanzo/ui'
import type { RailSession } from '@hanzo/ui/chat'
import { useEffect, useMemo, useRef, useState } from 'react'

import { page, type Session } from './api/sessions.ts'
import { useHost, useTarget } from './host.tsx'

/** The statuses the platform filters on, and every run. */
const FILTERS = [
  { id: '', label: phrase('All') },
  { id: 'running', label: phrase('Running') },
  { id: 'paused', label: phrase('Paused') },
  { id: 'done', label: phrase('Done') },
  { id: 'error', label: phrase('Error') },
] as const

const PAGE = 50

interface Hit {
  id: string
  title: string
  line: string
}

export function Find({
  open,
  onOpenChange,
  recents,
  onOpen,
}: {
  open: boolean
  onOpenChange: (o: boolean) => void
  recents: RailSession[]
  onOpen: (id: string) => void
}) {
  const { t: say } = useI18n()
  const host = useHost()
  const t = useTarget()
  const signed = Boolean(host.person)
  const [q, setQ] = useState('')
  const [status, setStatus] = useState('')
  const [rows, setRows] = useState<Session[] | null>(null)
  const [next, setNext] = useState('')
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState('')
  // A page asked for under another status or an earlier opening lands on nothing.
  const asked = useRef(0)

  const read = (after: string) => {
    const mine = after ? asked.current : ++asked.current
    setLoading(true)
    setError('')
    return page(t, { kind: 'coding', status, limit: PAGE, after })
      .then((p) => {
        if (mine !== asked.current) return
        setRows((prev) => (after && prev ? [...prev, ...p.sessions.filter((s) => !prev.some((r) => r.id === s.id))] : p.sessions))
        setNext(p.next)
      })
      .catch((e: unknown) => {
        if (mine === asked.current) setError((e as Error).message)
      })
      .finally(() => {
        if (mine === asked.current) setLoading(false)
      })
  }

  // Opening, or another status, reads from the newest again.
  useEffect(() => {
    if (!open || !signed) return
    setRows(null)
    setNext('')
    void read('')
    // `read` closes over the status and target this effect is keyed on.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [open, signed, status, t])

  const hits = useMemo((): Hit[] => {
    const n = q.trim().toLowerCase()
    const all: Hit[] = rows
      ? rows.map((s) => ({ id: s.id, title: s.title || say('Untitled session'), line: [s.repo, s.status].filter(Boolean).join(' · ') }))
      : status
        ? []
        : recents.map((r) => ({ id: r.id, title: r.title, line: '' }))
    return n ? all.filter((h) => `${h.title} ${h.line}`.toLowerCase().includes(n)) : all
  }, [q, rows, recents, status, say])

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent maxW={520}>
        <DialogTitle>{say('Find a session')}</DialogTitle>
        <Input
          autoFocus
          value={q}
          onChangeText={setQ}
          placeholder={say('Search sessions…')}
          aria-label={say('Search sessions')}
          onKeyDown={(e: { key?: string }) => {
            if (e.key === 'Enter' && hits[0]) onOpen(hits[0].id)
          }}
        />
        <XStack gap="$1" flexWrap="wrap" role="group" aria-label={say('Status')}>
          {FILTERS.map((f) => (
            <XStack
              key={f.id || 'all'}
              render="button"
              aria-label={say(f.label)}
              aria-pressed={status === f.id}
              onPress={() => setStatus(f.id)}
              px="$2.5"
              py="$1"
              rounded="$10"
              borderWidth={1}
              borderColor={status === f.id ? '$ink' : '$borderColor'}
              bg={status === f.id ? '$hover' : 'transparent'}
              hoverStyle={{ bg: '$hover' }}
            >
              <SizableText size="$1" color={status === f.id ? '$ink' : '$soft'}>
                {say(f.label)}
              </SizableText>
            </XStack>
          ))}
        </XStack>
        <ScrollView maxH={360}>
          <YStack gap="$1" role="list">
            {hits.map((h) => (
              <XStack
                key={h.id}
                role="listitem"
                render="button"
                aria-label={h.title}
                onPress={() => onOpen(h.id)}
                px="$2"
                py="$1.5"
                rounded="$3"
                hoverStyle={{ bg: '$hover' }}
                items="center"
                gap="$3"
              >
                <SizableText flex={1} minW={0} size="$2" color="$ink" numberOfLines={1} style={{ textAlign: 'left' }}>
                  {h.title}
                </SizableText>
                {h.line ? (
                  <SizableText size="$1" color="$soft" numberOfLines={1} shrink={0} maxW="45%">
                    {h.line}
                  </SizableText>
                ) : null}
              </XStack>
            ))}
            {!loading && !error && hits.length === 0 ? (
              <SizableText size="$2" color="$soft" px="$2">
                {rows && next ? say('No session read so far matches.') : say('No session matches.')}
              </SizableText>
            ) : null}
          </YStack>
        </ScrollView>
        {error ? (
          <SizableText size="$1" color="$soft" role="status">
            {error}
          </SizableText>
        ) : null}
        {loading ? (
          <SizableText size="$1" color="$soft" role="status">
            {say('Reading sessions…')}
          </SizableText>
        ) : next ? (
          <XStack>
            <Button size="sm" variant="outline" onPress={() => void read(next)}>
              {say('Older sessions')}
            </Button>
          </XStack>
        ) : null}
      </DialogContent>
    </Dialog>
  )
}
