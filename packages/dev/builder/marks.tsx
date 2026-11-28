/**
 * The mark of each product and place, once. The rail, a phone's bar, the sidebars,
 * the palette and every landing page draw these and no other, so the same thing
 * wears the same icon everywhere. `icon` is the place; `make` is its New: Chat
 * starts a conversation (the pen), Dev a task (`</>`), Tabs a terminal; a place
 * with nothing of its own to start draws its own icon.
 */
import { Clapperboard, Clock, CodeXml, Library, MessageSquare, Puzzle, SquarePen, SquareTerminal, WandSparkles } from '@hanzogui/lucide-icons-2'

export type Mark = 'chat' | 'dev' | 'design' | 'studio' | 'tabs' | 'library' | 'scheduled' | 'customize'

/** An icon as the rail and the sidebars draw it. */
export type Icon = typeof SquarePen

export const MARKS: Record<Mark, { icon: Icon; make: Icon }> = {
  chat: { icon: MessageSquare, make: SquarePen },
  dev: { icon: CodeXml, make: CodeXml },
  design: { icon: WandSparkles, make: WandSparkles },
  studio: { icon: Clapperboard, make: Clapperboard },
  tabs: { icon: SquareTerminal, make: SquareTerminal },
  library: { icon: Library, make: Library },
  scheduled: { icon: Clock, make: Clock },
  customize: { icon: Puzzle, make: Puzzle },
}
