/**
 * @hanzo/build — the builder as a component.
 *
 * Mount `<Builder host={…} />` under a gui root in a box with a height. The
 * host says where the platform is, who is signed in, and how to move; the
 * builder does everything else. hanzo.build hosts it whole. A host with a rail
 * of its own mounts it with `rail={false}` and draws the builder's parts in that
 * rail: its places (`nav`), its runs (`useSessions`, or `DevSection` for both),
 * and at the foot the Slack card and the account menu (`Slack`, `Who`), under
 * `HostProvider`. `Grip` (the kit's, passed on) sizes that rail.
 *
 * The side panel's column, for a host that draws it beside its own middle
 * column (Chat): `useSplit`, `SideProvider` and `PrefsProvider` above both, so
 * the width the person drags is kept in their preferences (side.tsx).
 *
 * The plan, once for every surface that shows one: `label` names it (`Max` with
 * `20x` beside it), `useStanding` reads where an account stands, `Meter` is the
 * block an account menu leads with, `Plan` the Usage view and `Credits` the
 * Billing one. Each reads as a `Target`, so a host without a builder mounts it.
 * The pure half — naming, shares, the banner's words — is also `@hanzo/build/plan`,
 * which loads in Node without the components.
 */
export { Builder } from './builder.tsx'
export { Library, Outputs, Preview, Thumb, want } from './library.tsx'
/** The Library's one reader, for a host that lists what a person made itself: `items(target, filter)` answers one page. */
export { items, SOURCE, type Filter as ItemFilter, type Item, type Kind as ItemKind, type Page as ItemPage, type Scope as ItemScope, type Source } from './api/library.ts'
export { Scheduled, schedule } from './scheduled.tsx'
export { Editor } from './automations.tsx'
/**
 * The screens a host draws in its own pane, each under `HostProvider` alone, so
 * they sit inside the host's one look (theme and text size) and never under the
 * builder's own `Look`: Library, Scheduled tasks and one task's `Editor` above,
 * `Customize` (one tab), an agent's `Profile`, and the `Forge`'s screens
 * (Projects, Issues, Sync, Connectors).
 */
export { Customize } from './customize/index.tsx'
export { Profile } from './customize/agents.tsx'
export { Forge } from './forge.tsx'
export { library, nav, NEW, own, sections, tabs, useScheduled, type Group } from './nav.tsx'
export { MARKS, type Icon, type Mark } from './marks.tsx'
export { useFunds, type Funds } from './mind.ts'
export { DevSection, dot, DOTS, useSessions } from './section.tsx'
export { HostProvider } from './host.tsx'
export { Slack, useWho, Who } from './foot.tsx'
export { Grip, type GripProps } from '@hanzo/ui'
export { Gutter, type GutterProps } from './gutter.tsx'
export { administers } from './claims.ts'
export { Meter, Title } from './meter.tsx'
export { Plan } from './settings/plan.tsx'
export { Credits, SEPARATE } from './settings/credits.tsx'
export { useStanding, rows, left, type Standing, type Row } from './standing.ts'
export { kind, label, said, share, spent, ways, when, type Label, type Share, type State } from './plan.ts'
export type { Limits } from './api/limits.ts'
export type { Target } from './api/call.ts'
export type { Browse, Browser, Host, Person, Shown, Ask, Spend, SpendKind } from './host.tsx'
export type { Done, Folder, Local } from './local.ts'
export type { Live, Saved, Site } from './site.ts'
export type { Session, Status } from './api/sessions.ts'
export { mark as markRun, marks as runMarks, useMarks as useRunMarks, type RunMark, type RunMarks } from './runmeta.ts'
export {
  cleanErrorText,
  groupSessions,
  isAlreadyRunningError,
  isSessionActive,
  isSessionLive,
  linkThread,
  resolveSessionState,
  threadChain,
  threadRoot,
  useThreadLinks,
  type ThreadLinks,
} from './thread.ts'
export { cacheBustUrl, hasUnpublishedEdits, previewRevision, resolvePreviewSource, type PreviewInputs, type PreviewKind, type PreviewSource } from './preview-url.ts'
export { talks } from './api/talk.ts'
export { detect, type Detected } from './api/skills.ts'
export { Roles } from './customize/roles.tsx'
export { recommend, WORK } from './roles.ts'
export type { Read } from './data.ts'
export { href, parse, path, PLACES, route, SESSION, SLUG, type Route, type Screen } from './route.ts'

/**
 * What Chat draws the same way (@hanzo/rooms imports these): the model and
 * effort (`useMind`, one choice kept for both), the row under an answer
 * (`Reply`), and the side panel (`Panel` over a `Deck` of tabs). The composer is
 * @hanzo/ui/chat's, for Chat and Dev alike.
 */
export { catalog, EFFORTS, FIRST, grants, isFree, keep, kept, level, nameOf, pacing, useCatalog, useChoice, useMind, type Choice, type Effort, type Mind } from './mind.ts'
export { DEV_HANDOFF, takeHandoff } from './handoff.ts'
export { useEar } from './voice.ts'
export { BARE, CHAT_MODES, DEV_MODES, gearOf, MODES, useGear, useLanguage, type Gear, type GearState, type Mode as GearMode } from './gear.ts'
export { skillsSaid, useCarry, type Carry } from './carry.tsx'
export { artifacts, blob, lead, renders, Reply, Served, speakable, speaker, type Artifact, type Listen } from './reply.tsx'
export { frames, Page, Panel, PanelToggle, type Kind, type PageProps, type PanelProps, type Suggestion } from './panel.tsx'
export { SideProvider, useSplit, type Split } from './side.tsx'
export { PrefsProvider } from './prefs.tsx'
export { deviceKey, friendly, heldNames, NAMES_KEY, rename, shellKey, useDeviceNames, type Names } from './names.ts'
export { DEV_MODE_EVENT, DEV_MODE_KEY, DEV_MODE_NOTE, formatDeviceLabel, getDevMode, setDevMode, useDevMode, type DeviceLabelOptions } from './dev-mode.ts'
export { cardCategory, categoryOf, formatDuration, formatTurnTimestamp, plainPath, shouldShowTimestamp, type WorkCategory } from './plain.ts'
export { inferActivityCategory, SteerStack, Transcript, TRANSCRIPT_MOTION_CSS, WorkIcon, type QueuedMessage, type TranscriptView } from './transcript.tsx'
export { Rename } from './ask.tsx'
export { kinds, Summary, useKinds, type Entry, type Section } from './summary.tsx'

/**
 * The composer's plugins, the same in Chat and Dev: `usePlugins` answers those
 * that run for this person, `shelf` is the `+` menu's Plugins shelf, and `prep`
 * asks the ones that are on what a message carries (Deep research is the
 * Research switch, `research`).
 */
export { active, BUILD, merge, prep, shelf, usePlugins, research, type Plugins } from './plugins/index.ts'
export type { Composer as PluginComposer, Plugin, Prep, Turn } from './plugin.ts'
export { address, destination, located, name, PAGE, SEARCH, useDeck, useKey, usePanel, useShown, type Deck, type Tab, type Tabs } from './tabs.ts'

/**
 * Up to three suggestions under the composer (above it on a phone), each from
 * the person's own activity: `useSuggestions` reads and makes them (a model call
 * when they are due; never signed out, never while the account's `suggestions`
 * preference is off), `setSuggestions` is the one way to switch them, and
 * `Suggestions` draws them with the sentence that says so and its Turn off. A
 * `schedule` row's task is made by `book`, which asks through the host's confirm.
 * The row is `Hint` here: `Suggestion` is the side panel's.
 */
export { hand, setSuggestions, Suggestions, taken, useSuggestions, type Suggested, type SuggestionsProps, type Switch as SuggestionsSwitch } from './suggestions.tsx'
export { book } from './suggest.ts'
export type { Act, Suggestion as Hint } from './suggest.ts'
