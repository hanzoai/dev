/**
 * Open Collective Integration
 *
 * Fetches collective information and handles contribution processing.
 */

import got from 'got'
import Decimal from 'decimal.js'
import { createLogger, createRateLimiter, withRetry } from '../utils/index.js'

const logger = createLogger('open-collective')

const OC_API_URL = 'https://api.opencollective.com/graphql/v2'

export interface OpenCollectiveConfig {
  apiKey?: string
  rateLimit?: number
}

export class OpenCollectiveClient {
  private rateLimit: () => Promise<void>
  private apiKey?: string

  constructor(config: OpenCollectiveConfig = {}) {
    this.apiKey = config.apiKey
    this.rateLimit = createRateLimiter(config.rateLimit ?? 5)
  }

  /**
   * Get collective information by slug
   */
  async getCollective(slug: string): Promise<Collective | undefined> {
    await this.rateLimit()

    const query = `
      query GetCollective($slug: String!) {
        collective(slug: $slug) {
          id
          slug
          name
          description
          website
          githubHandle
          twitterHandle
          currency
          stats {
            balance {
              value
              currency
            }
            yearlyBudget {
              value
              currency
            }
            totalAmountReceived {
              value
              currency
            }
            backers {
              all
            }
          }
          tiers {
            nodes {
              id
              name
              description
              amount {
                value
                currency
              }
              interval
              button
              goal {
                value
                currency
              }
            }
          }
        }
      }
    `

    try {
      const response = await withRetry(
        async () => {
          return got.post(OC_API_URL, {
            json: { query, variables: { slug } },
            headers: this.getHeaders(),
            responseType: 'json',
          }).json<GraphQLResponse<{ collective: CollectiveResponse }>>()
        },
        { maxAttempts: 3, baseDelayMs: 500, maxDelayMs: 5000 }
      )

      if (response.errors) {
        logger.warn(`GraphQL errors for collective ${slug}`, { errors: response.errors })
        return undefined
      }

      const c = response.data?.collective
      if (!c) return undefined

      return {
        id: c.id,
        slug: c.slug,
        name: c.name,
        description: c.description,
        website: c.website,
        githubHandle: c.githubHandle,
        twitterHandle: c.twitterHandle,
        currency: c.currency,
        stats: {
          balance: c.stats?.balance?.value ?? 0,
          yearlyBudget: c.stats?.yearlyBudget?.value ?? 0,
          totalReceived: c.stats?.totalAmountReceived?.value ?? 0,
          backerCount: c.stats?.backers?.all ?? 0,
        },
        tiers: c.tiers?.nodes?.map((t) => ({
          id: t.id,
          name: t.name,
          description: t.description,
          amount: t.amount?.value ?? 0,
          currency: t.amount?.currency ?? c.currency,
          interval: t.interval,
        })) ?? [],
      }
    } catch (error) {
      logger.warn(`Failed to fetch collective ${slug}`, { error })
      return undefined
    }
  }

  /**
   * Search for collectives
   */
  async searchCollectives(searchTerm: string, limit: number = 10): Promise<Collective[]> {
    await this.rateLimit()

    const query = `
      query SearchCollectives($searchTerm: String!, $limit: Int!) {
        accounts(searchTerm: $searchTerm, limit: $limit, type: COLLECTIVE) {
          nodes {
            id
            slug
            name
            description
            website
            currency
          }
        }
      }
    `

    try {
      const response = await got.post(OC_API_URL, {
        json: { query, variables: { searchTerm, limit } },
        headers: this.getHeaders(),
        responseType: 'json',
      }).json<GraphQLResponse<{ accounts: { nodes: CollectiveResponse[] } }>>()

      return (response.data?.accounts?.nodes ?? []).map((c) => ({
        id: c.id,
        slug: c.slug,
        name: c.name,
        description: c.description,
        website: c.website,
        currency: c.currency,
        stats: {
          balance: 0,
          yearlyBudget: 0,
          totalReceived: 0,
          backerCount: 0,
        },
        tiers: [],
      }))
    } catch (error) {
      logger.warn(`Failed to search collectives for "${searchTerm}"`, { error })
      return []
    }
  }

  /**
   * Create a contribution to a collective
   * Note: This requires proper OAuth setup and is typically done via their web UI
   */
  async createContribution(
    collectiveSlug: string,
    amount: Decimal,
    _options: ContributionOptions = {}
  ): Promise<ContributionResult> {
    // For programmatic contributions, Open Collective recommends:
    // 1. Using their web UI for one-time donations
    // 2. Setting up recurring contributions via their API with OAuth
    // 3. For organizations, using their fiscal host features

    logger.info(`Contribution request to ${collectiveSlug}`, {
      amount: amount.toString(),
    })

    // Return a URL for manual contribution instead
    const contributionUrl = `https://opencollective.com/${collectiveSlug}/contribute`

    return {
      success: false,
      requiresManualAction: true,
      contributionUrl,
      message: 'Please complete the contribution manually via Open Collective',
    }
  }

  /**
   * Check if a collective accepts contributions
   */
  async canAcceptContributions(slug: string): Promise<boolean> {
    const collective = await this.getCollective(slug)
    return collective !== undefined && collective.tiers.length > 0
  }

  private getHeaders(): Record<string, string> {
    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
    }
    if (this.apiKey) {
      headers['Api-Key'] = this.apiKey
    }
    return headers
  }
}

// Type definitions
interface GraphQLResponse<T> {
  data?: T
  errors?: Array<{ message: string }>
}

interface CollectiveResponse {
  id: string
  slug: string
  name: string
  description?: string
  website?: string
  githubHandle?: string
  twitterHandle?: string
  currency: string
  stats?: {
    balance?: { value: number; currency: string }
    yearlyBudget?: { value: number; currency: string }
    totalAmountReceived?: { value: number; currency: string }
    backers?: { all: number }
  }
  tiers?: {
    nodes?: Array<{
      id: string
      name: string
      description?: string
      amount?: { value: number; currency: string }
      interval?: string
      button?: string
      goal?: { value: number; currency: string }
    }>
  }
}

export interface Collective {
  id: string
  slug: string
  name: string
  description?: string
  website?: string
  githubHandle?: string
  twitterHandle?: string
  currency: string
  stats: {
    balance: number
    yearlyBudget: number
    totalReceived: number
    backerCount: number
  }
  tiers: Array<{
    id: string
    name: string
    description?: string
    amount: number
    currency: string
    interval?: string
  }>
}

export interface ContributionOptions {
  tierId?: string
  interval?: 'month' | 'year' | 'one-time'
  currency?: string
  description?: string
}

export interface ContributionResult {
  success: boolean
  transactionId?: string
  requiresManualAction?: boolean
  contributionUrl?: string
  message?: string
}
