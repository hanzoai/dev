/**
 * The Big Five (OCEAN) as the International Personality Item Pool asks it, and
 * how an answer sheet is scored.
 *
 * Long: the IPIP-NEO-120. Johnson, J. A. (2014). Measuring thirty facets of
 * the Five Factor Model with a 120-item public domain inventory: Development of
 * the IPIP-NEO-120. Journal of Research in Personality, 51, 78-89. Statements
 * and keys: https://ipip.ori.org/30FacetNEO-PI-RItems.htm; facet names:
 * https://ipip.ori.org/newNEO_FacetsTable.htm. In Johnson's order, item n is
 * facet (n - 1) mod 30 of N1 E1 O1 A1 C1 N2 … C6, as in the MIT-licensed
 * `five-factor-e` package (`ipipneo`, NeuroQuest AI), whose reversed-item list
 * agrees with every key here.
 *
 * Short: the 50-item IPIP Big-Five factor markers, in their published order,
 * each with the factor and key printed beside it:
 * https://ipip.ori.org/new_ipip-50-item-scale.htm (Goldberg, L. R. (1992). The
 * development of markers for the Big-Five factor structure. Psychological
 * Assessment, 4, 26-42). Factor IV there is Emotional Stability; here it is
 * Neuroticism, every key turned over, so Neuroticism = 6 - Emotional Stability.
 *
 * The IPIP's items and scales are in the public domain (https://ipip.ori.org).
 *
 * Answers run 1 (very inaccurate) to 5 (very accurate). An item keyed minus
 * counts 6 - answer. A scale's score is the mean of its answered items: raw,
 * 1 to 5, against no norm table.
 */
import { phrase } from '@hanzo/i18n'

export type Domain = 'O' | 'C' | 'E' | 'A' | 'N'
export const DOMAINS: readonly Domain[] = ['O', 'C', 'E', 'A', 'N']
export type Facet = `${Domain}${1 | 2 | 3 | 4 | 5 | 6}`
export type Form = 'short' | 'long'

export interface Item {
  text: string
  domain: Domain
  /** The long form's facet; null on the short form. */
  facet: Facet | null
  /** +1 scores the answer, -1 scores 6 - answer. */
  key: 1 | -1
}

export const DOMAIN: Record<Domain, string> = {
  O: phrase('Openness'),
  C: phrase('Conscientiousness'),
  E: phrase('Extraversion'),
  A: phrase('Agreeableness'),
  N: phrase('Neuroticism'),
}

export const FACET: Record<Facet, string> = {
  N1: phrase('Anxiety'),
  N2: phrase('Anger'),
  N3: phrase('Depression'),
  N4: phrase('Self-consciousness'),
  N5: phrase('Immoderation'),
  N6: phrase('Vulnerability'),
  E1: phrase('Friendliness'),
  E2: phrase('Gregariousness'),
  E3: phrase('Assertiveness'),
  E4: phrase('Activity level'),
  E5: phrase('Excitement-seeking'),
  E6: phrase('Cheerfulness'),
  O1: phrase('Imagination'),
  O2: phrase('Artistic interests'),
  O3: phrase('Emotionality'),
  O4: phrase('Adventurousness'),
  O5: phrase('Intellect'),
  O6: phrase('Liberalism'),
  A1: phrase('Trust'),
  A2: phrase('Morality'),
  A3: phrase('Altruism'),
  A4: phrase('Cooperation'),
  A5: phrase('Modesty'),
  A6: phrase('Sympathy'),
  C1: phrase('Self-efficacy'),
  C2: phrase('Orderliness'),
  C3: phrase('Dutifulness'),
  C4: phrase('Achievement-striving'),
  C5: phrase('Self-discipline'),
  C6: phrase('Cautiousness'),
}

/** A domain's six facets, in order. */
export const FACETS = (d: Domain): Facet[] => ([1, 2, 3, 4, 5, 6] as const).map((n): Facet => `${d}${n}`)

/** The five answers, as the IPIP words them, for 1 to 5. */
export const ANCHORS = [phrase('Very inaccurate'), phrase('Moderately inaccurate'), phrase('Neither accurate nor inaccurate'), phrase('Moderately accurate'), phrase('Very accurate')] as const

type Key = '+' | '-'

/** The IPIP-NEO-120, as published: facet, key, statement. */
const long: readonly (readonly [Facet, Key, string])[] = [
  ['N1', '+', phrase('Worry about things.')],
  ['E1', '+', phrase('Make friends easily.')],
  ['O1', '+', phrase('Have a vivid imagination.')],
  ['A1', '+', phrase('Trust others.')],
  ['C1', '+', phrase('Complete tasks successfully.')],
  ['N2', '+', phrase('Get angry easily.')],
  ['E2', '+', phrase('Love large parties.')],
  ['O2', '+', phrase('Believe in the importance of art.')],
  ['A2', '-', phrase('Use others for my own ends.')],
  ['C2', '+', phrase('Like to tidy up.')],
  ['N3', '+', phrase('Often feel blue.')],
  ['E3', '+', phrase('Take charge.')],
  ['O3', '+', phrase('Experience my emotions intensely.')],
  ['A3', '+', phrase('Love to help others.')],
  ['C3', '+', phrase('Keep my promises.')],
  ['N4', '+', phrase('Find it difficult to approach others.')],
  ['E4', '+', phrase('Am always busy.')],
  ['O4', '+', phrase('Prefer variety to routine.')],
  ['A4', '-', phrase('Love a good fight.')],
  ['C4', '+', phrase('Work hard.')],
  ['N5', '+', phrase('Go on binges.')],
  ['E5', '+', phrase('Love excitement.')],
  ['O5', '+', phrase('Love to read challenging material.')],
  ['A5', '-', phrase('Believe that I am better than others.')],
  ['C5', '+', phrase('Am always prepared.')],
  ['N6', '+', phrase('Panic easily.')],
  ['E6', '+', phrase('Radiate joy.')],
  ['O6', '+', phrase('Tend to vote for liberal political candidates.')],
  ['A6', '+', phrase('Sympathize with the homeless.')],
  ['C6', '-', phrase('Jump into things without thinking.')],
  ['N1', '+', phrase('Fear for the worst.')],
  ['E1', '+', phrase('Feel comfortable around people.')],
  ['O1', '+', phrase('Enjoy wild flights of fantasy.')],
  ['A1', '+', phrase('Believe that others have good intentions.')],
  ['C1', '+', phrase('Excel in what I do.')],
  ['N2', '+', phrase('Get irritated easily.')],
  ['E2', '+', phrase('Talk to a lot of different people at parties.')],
  ['O2', '+', phrase('See beauty in things that others might not notice.')],
  ['A2', '-', phrase('Cheat to get ahead.')],
  ['C2', '-', phrase('Often forget to put things back in their proper place.')],
  ['N3', '+', phrase('Dislike myself.')],
  ['E3', '+', phrase('Try to lead others.')],
  ['O3', '+', phrase("Feel others' emotions.")],
  ['A3', '+', phrase('Am concerned about others.')],
  ['C3', '+', phrase('Tell the truth.')],
  ['N4', '+', phrase('Am afraid to draw attention to myself.')],
  ['E4', '+', phrase('Am always on the go.')],
  ['O4', '-', phrase('Prefer to stick with things that I know.')],
  ['A4', '-', phrase('Yell at people.')],
  ['C4', '+', phrase("Do more than what's expected of me.")],
  ['N5', '-', phrase('Rarely overindulge.')],
  ['E5', '+', phrase('Seek adventure.')],
  ['O5', '-', phrase('Avoid philosophical discussions.')],
  ['A5', '-', phrase('Think highly of myself.')],
  ['C5', '+', phrase('Carry out my plans.')],
  ['N6', '+', phrase('Become overwhelmed by events.')],
  ['E6', '+', phrase('Have a lot of fun.')],
  ['O6', '+', phrase('Believe that there is no absolute right and wrong.')],
  ['A6', '+', phrase('Feel sympathy for those who are worse off than myself.')],
  ['C6', '-', phrase('Make rash decisions.')],
  ['N1', '+', phrase('Am afraid of many things.')],
  ['E1', '-', phrase('Avoid contacts with others.')],
  ['O1', '+', phrase('Love to daydream.')],
  ['A1', '+', phrase('Trust what people say.')],
  ['C1', '+', phrase('Handle tasks smoothly.')],
  ['N2', '+', phrase('Lose my temper.')],
  ['E2', '-', phrase('Prefer to be alone.')],
  ['O2', '-', phrase('Do not like poetry.')],
  ['A2', '-', phrase('Take advantage of others.')],
  ['C2', '-', phrase('Leave a mess in my room.')],
  ['N3', '+', phrase('Am often down in the dumps.')],
  ['E3', '+', phrase('Take control of things.')],
  ['O3', '-', phrase('Rarely notice my emotional reactions.')],
  ['A3', '-', phrase('Am indifferent to the feelings of others.')],
  ['C3', '-', phrase('Break rules.')],
  ['N4', '+', phrase('Only feel comfortable with friends.')],
  ['E4', '+', phrase('Do a lot in my spare time.')],
  ['O4', '-', phrase('Dislike changes.')],
  ['A4', '-', phrase('Insult people.')],
  ['C4', '-', phrase('Do just enough work to get by.')],
  ['N5', '-', phrase('Easily resist temptations.')],
  ['E5', '+', phrase('Enjoy being reckless.')],
  ['O5', '-', phrase('Have difficulty understanding abstract ideas.')],
  ['A5', '-', phrase('Have a high opinion of myself.')],
  ['C5', '-', phrase('Waste my time.')],
  ['N6', '+', phrase("Feel that I'm unable to deal with things.")],
  ['E6', '+', phrase('Love life.')],
  ['O6', '-', phrase('Tend to vote for conservative political candidates.')],
  ['A6', '-', phrase("Am not interested in other people's problems.")],
  ['C6', '-', phrase('Rush into things.')],
  ['N1', '+', phrase('Get stressed out easily.')],
  ['E1', '-', phrase('Keep others at a distance.')],
  ['O1', '+', phrase('Like to get lost in thought.')],
  ['A1', '-', phrase('Distrust people.')],
  ['C1', '+', phrase('Know how to get things done.')],
  ['N2', '-', phrase('Am not easily annoyed.')],
  ['E2', '-', phrase('Avoid crowds.')],
  ['O2', '-', phrase('Do not enjoy going to art museums.')],
  ['A2', '-', phrase("Obstruct others' plans.")],
  ['C2', '-', phrase('Leave my belongings around.')],
  ['N3', '-', phrase('Feel comfortable with myself.')],
  ['E3', '-', phrase('Wait for others to lead the way.')],
  ['O3', '-', phrase("Don't understand people who get emotional.")],
  ['A3', '-', phrase('Take no time for others.')],
  ['C3', '-', phrase('Break my promises.')],
  ['N4', '-', phrase('Am not bothered by difficult social situations.')],
  ['E4', '-', phrase('Like to take it easy.')],
  ['O4', '-', phrase('Am attached to conventional ways.')],
  ['A4', '-', phrase('Get back at others.')],
  ['C4', '-', phrase('Put little time and effort into my work.')],
  ['N5', '-', phrase('Am able to control my cravings.')],
  ['E5', '+', phrase('Act wild and crazy.')],
  ['O5', '-', phrase('Am not interested in theoretical discussions.')],
  ['A5', '-', phrase('Boast about my virtues.')],
  ['C5', '-', phrase('Have difficulty starting tasks.')],
  ['N6', '-', phrase('Remain calm under pressure.')],
  ['E6', '+', phrase('Look at the bright side of life.')],
  ['O6', '-', phrase('Believe that we should be tough on crime.')],
  ['A6', '-', phrase('Try not to think about the needy.')],
  ['C6', '-', phrase('Act without thinking.')],
]

/** The 50 markers, as published: factor (I Extraversion, II Agreeableness, III Conscientiousness, IV Emotional Stability, V Intellect/Imagination), key, statement. */
const short: readonly (readonly [1 | 2 | 3 | 4 | 5, Key, string])[] = [
  [1, '+', phrase('Am the life of the party.')],
  [2, '-', phrase('Feel little concern for others.')],
  [3, '+', phrase('Am always prepared.')],
  [4, '-', phrase('Get stressed out easily.')],
  [5, '+', phrase('Have a rich vocabulary.')],
  [1, '-', phrase("Don't talk a lot.")],
  [2, '+', phrase('Am interested in people.')],
  [3, '-', phrase('Leave my belongings around.')],
  [4, '+', phrase('Am relaxed most of the time.')],
  [5, '-', phrase('Have difficulty understanding abstract ideas.')],
  [1, '+', phrase('Feel comfortable around people.')],
  [2, '-', phrase('Insult people.')],
  [3, '+', phrase('Pay attention to details.')],
  [4, '-', phrase('Worry about things.')],
  [5, '+', phrase('Have a vivid imagination.')],
  [1, '-', phrase('Keep in the background.')],
  [2, '+', phrase("Sympathize with others' feelings.")],
  [3, '-', phrase('Make a mess of things.')],
  [4, '+', phrase('Seldom feel blue.')],
  [5, '-', phrase('Am not interested in abstract ideas.')],
  [1, '+', phrase('Start conversations.')],
  [2, '-', phrase("Am not interested in other people's problems.")],
  [3, '+', phrase('Get chores done right away.')],
  [4, '-', phrase('Am easily disturbed.')],
  [5, '+', phrase('Have excellent ideas.')],
  [1, '-', phrase('Have little to say.')],
  [2, '+', phrase('Have a soft heart.')],
  [3, '-', phrase('Often forget to put things back in their proper place.')],
  [4, '-', phrase('Get upset easily.')],
  [5, '-', phrase('Do not have a good imagination.')],
  [1, '+', phrase('Talk to a lot of different people at parties.')],
  [2, '-', phrase('Am not really interested in others.')],
  [3, '+', phrase('Like order.')],
  [4, '-', phrase('Change my mood a lot.')],
  [5, '+', phrase('Am quick to understand things.')],
  [1, '-', phrase("Don't like to draw attention to myself.")],
  [2, '+', phrase('Take time out for others.')],
  [3, '-', phrase('Shirk my duties.')],
  [4, '-', phrase('Have frequent mood swings.')],
  [5, '+', phrase('Use difficult words.')],
  [1, '+', phrase("Don't mind being the center of attention.")],
  [2, '+', phrase("Feel others' emotions.")],
  [3, '+', phrase('Follow a schedule.')],
  [4, '-', phrase('Get irritated easily.')],
  [5, '+', phrase('Spend time reflecting on things.')],
  [1, '-', phrase('Am quiet around strangers.')],
  [2, '+', phrase('Make people feel at ease.')],
  [3, '+', phrase('Am exacting in my work.')],
  [4, '-', phrase('Often feel blue.')],
  [5, '+', phrase('Am full of ideas.')],
]

const FACTOR = { 1: 'E', 2: 'A', 3: 'C', 4: 'N', 5: 'O' } as const

export const ITEMS: Record<Form, readonly Item[]> = {
  long: long.map(([facet, key, text]) => ({ text, domain: facet[0] as Domain, facet, key: key === '+' ? 1 : -1 })),
  short: short.map(([factor, key, text]) => ({ text, domain: FACTOR[factor], facet: null, key: (key === '+') !== (factor === 4) ? 1 : -1 })),
}

export interface Score {
  domains: Record<Domain, number>
  /** The long form's 30; empty for the short form. */
  facets: Partial<Record<Facet, number>>
}

/** Every scale's mean over its answered items (0 where none is); `answers[i]` is item i's 1 to 5, 0 unanswered. */
export function score(form: Form, answers: readonly number[]): Score {
  const sums = new Map<string, [number, number]>()
  const add = (scale: string, v: number) => {
    const [s, n] = sums.get(scale) ?? [0, 0]
    sums.set(scale, [s + v, n + 1])
  }
  ITEMS[form].forEach((item, i) => {
    const a = answers[i] ?? 0
    if (!a) return
    const v = item.key === 1 ? a : 6 - a
    add(item.domain, v)
    if (item.facet) add(item.facet, v)
  })
  const mean = (scale: string) => {
    const [s, n] = sums.get(scale) ?? [0, 0]
    return n ? s / n : 0
  }
  return {
    domains: Object.fromEntries(DOMAINS.map((d) => [d, mean(d)])) as Record<Domain, number>,
    facets: form === 'long' ? Object.fromEntries(DOMAINS.flatMap(FACETS).map((f) => [f, mean(f)])) : {},
  }
}

/** How a high (≥ 3.5) and a low (≤ 2.5) domain reads in the clone's instructions. Prompt text: English. */
const READS: Record<Domain, readonly [high: string, low: string]> = {
  O: [
    'Curious and imaginative: drawn to ideas, abstraction and new approaches; writes with range and the odd metaphor.',
    'Practical and concrete: trusts the proven over the novel; writes plainly, with examples rather than theory.',
  ],
  C: ['Organized and thorough: plans ahead and follows through; writes in a structured, precise way.', 'Spontaneous and flexible: improvises rather than plans; writes loosely and gets to the point.'],
  E: ['Outgoing and energetic: thinks out loud and takes the lead; writes with warmth and enthusiasm.', 'Reserved and reflective: thinks before speaking; writes briefly and without fuss.'],
  A: ['Warm and cooperative: assumes good intent; writes tactfully and generously.', 'Direct and skeptical: questions claims and says what they think; writes bluntly, without softening.'],
  N: ['Sensitive to pressure: alert to what could go wrong; writes with care and caveats.', 'Calm and steady: unflustered under pressure; writes with an even tone.'],
}

const NAME: Record<Form, string> = { long: 'IPIP-NEO-120', short: 'IPIP 50-item Big-Five markers' }
const fixed = (m: number) => m.toFixed(1)

/**
 * How `name` tends to think and write, from a whole answer sheet's score: a
 * line for each domain at either end, the facets furthest out (long form), and
 * the five means.
 */
export function nature(name: string, form: Form, s: Score): string {
  const lines = DOMAINS.flatMap((d) => (s.domains[d] >= 3.5 ? [READS[d][0]] : s.domains[d] <= 2.5 ? [READS[d][1]] : []))
  const far = (Object.entries(s.facets) as [Facet, number][]).sort((a, b) => Math.abs(b[1] - 3) - Math.abs(a[1] - 3))
  const list = (fs: [Facet, number][]) => fs.slice(0, 3).map(([f, m]) => `${FACET[f]} (${fixed(m)})`).join(', ')
  const high = list(far.filter(([, m]) => m >= 4.25))
  const low = list(far.filter(([, m]) => m <= 1.75))
  if (high || low) lines.push(`Stands out: ${[high && `high ${high}`, low && `low ${low}`].filter(Boolean).join('; ')}.`)
  const means = `Big Five (${NAME[form]}, means on 1 to 5): ${DOMAINS.map((d) => `${DOMAIN[d]} ${fixed(s.domains[d])}`).join(', ')}.`
  return [lines.length ? `How ${name} thinks and writes:\n${lines.map((l) => `- ${l}`).join('\n')}` : '', means].filter(Boolean).join('\n')
}

/** Both answer sheets and the form being taken: what this browser keeps between visits. */
export interface Answers {
  form: Form
  short: number[]
  long: number[]
}

const sheet = (form: Form, v: unknown): number[] => {
  const n = ITEMS[form].length
  return Array.isArray(v) && v.length === n && v.every((a) => Number.isInteger(a) && a >= 0 && a <= 5) ? (v as number[]) : Array<number>(n).fill(0)
}

const KEY = (who: string) => `hanzo.build.ocean.${who}`

/** The answers kept for `who`, or blank sheets. */
export function kept(who: string): Answers {
  let v: Record<string, unknown> = {}
  try {
    const raw = JSON.parse(window.localStorage.getItem(KEY(who)) ?? 'null')
    if (raw && typeof raw === 'object') v = raw as Record<string, unknown>
  } catch {
    /* blank sheets */
  }
  return { form: v.form === 'long' ? 'long' : 'short', short: sheet('short', v.short), long: sheet('long', v.long) }
}

export function keep(who: string, a: Answers) {
  try {
    window.localStorage.setItem(KEY(who), JSON.stringify(a))
  } catch {
    /* kept only while the dialog is open */
  }
}
