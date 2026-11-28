/**
 * The language dictation listens in, and the platform's ear. The microphone
 * itself is the composer's (@hanzo/ui/chat, @hanzo/voice): heard by the
 * platform, never by a browser's built-in recognizer, which ships the audio to
 * its vendor.
 */
import { LANGUAGES } from '@hanzo/ui/chat/pure'
import { speech, type Speech } from '@hanzo/voice'
import { useMemo } from 'react'

import type { Target } from './api/call.ts'
import { usePrefs } from './prefs.tsx'

/** The platform's ear and voice for a target: dictation, talk and Listen all hear through it. */
export function useEar(t: Target): Speech {
  return useMemo(() => speech({ baseUrl: t.api, token: t.token, ...(t.org ? { org: t.org } : {}) }), [t])
}

/** The language this browser is set to, when it is one dictation offers; English otherwise. */
export function spoken(): string {
  const nav = typeof navigator !== 'undefined' ? navigator.language.split('-')[0] : 'en'
  return LANGUAGES.some((l) => l.id === nav) ? nav! : 'en'
}

/** The person's saved language (Settings → General), else this browser's, else English. */
export function useSpoken(): string {
  const { prefs } = usePrefs()
  return LANGUAGES.some((l) => l.id === prefs.language) ? prefs.language! : spoken()
}
