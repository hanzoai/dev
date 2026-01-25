/**
 * GitHub Integration
 *
 * Fetches contributor data, FUNDING.yml, and sponsors information.
 */

import { Octokit } from '@octokit/rest'
import { graphql } from '@octokit/graphql'
import type { FundingInfo, GitContributor, GitAnalysisResult } from '../types/index.js'
import { createLogger, createRateLimiter, withRetry, normalizeGitHubUrl, extractGitHubOwnerRepo } from '../utils/index.js'

const logger = createLogger('github')

export interface GitHubConfig {
  token: string
  rateLimit?: number // requests per second
}

export class GitHubClient {
  private octokit: Octokit
  private graphqlClient: typeof graphql
  private rateLimit: () => Promise<void>

  constructor(config: GitHubConfig) {
    this.octokit = new Octokit({ auth: config.token })
    this.graphqlClient = graphql.defaults({
      headers: {
        authorization: `token ${config.token}`,
      },
    })
    this.rateLimit = createRateLimiter(config.rateLimit ?? 5)
  }

  /**
   * Fetch FUNDING.yml from a repository
   */
  async getFunding(owner: string, repo: string): Promise<FundingInfo | undefined> {
    await this.rateLimit()

    try {
      // Try .github/FUNDING.yml first
      let content = await this.getFileContent(owner, repo, '.github/FUNDING.yml')

      // Try FUNDING.yml in root
      if (!content) {
        content = await this.getFileContent(owner, repo, 'FUNDING.yml')
      }

      if (!content) {
        return undefined
      }

      return this.parseFundingYml(content)
    } catch (error) {
      logger.debug(`Failed to fetch FUNDING.yml for ${owner}/${repo}`, { error })
      return undefined
    }
  }

  /**
   * Fetch contributors with commit statistics
   */
  async getContributors(
    owner: string,
    repo: string,
    options: { maxContributors?: number } = {}
  ): Promise<GitContributor[]> {
    await this.rateLimit()

    const { maxContributors = 100 } = options

    try {
      const contributors = await withRetry(
        async () => {
          const response = await this.octokit.repos.listContributors({
            owner,
            repo,
            per_page: Math.min(maxContributors, 100),
            anon: 'false',
          })
          return response.data
        },
        { maxAttempts: 3, baseDelayMs: 1000, maxDelayMs: 10000 }
      )

      const result: GitContributor[] = []

      for (const contributor of contributors) {
        if (!contributor.login) continue

        // Fetch detailed commit stats for top contributors
        let additions = 0
        let deletions = 0
        let firstCommit: Date | undefined
        let lastCommit: Date | undefined

        if (result.length < 20) {
          await this.rateLimit()
          try {
            const commits = await this.octokit.repos.listCommits({
              owner,
              repo,
              author: contributor.login,
              per_page: 1,
            })

            if (commits.data.length > 0) {
              const latestCommit = commits.data[0]
              if (latestCommit?.commit?.author?.date) {
                lastCommit = new Date(latestCommit.commit.author.date)
              }
            }
          } catch {
            // Ignore errors for individual contributor stats
          }
        }

        result.push({
          name: contributor.login,
          email: '', // GitHub API doesn't expose email directly
          username: contributor.login,
          commitCount: contributor.contributions ?? 0,
          additions,
          deletions,
          firstCommit: firstCommit ?? new Date(),
          lastCommit: lastCommit ?? new Date(),
        })
      }

      return result
    } catch (error) {
      logger.warn(`Failed to fetch contributors for ${owner}/${repo}`, { error })
      return []
    }
  }

  /**
   * Analyze a repository for contribution data
   */
  async analyzeRepository(repositoryUrl: string): Promise<GitAnalysisResult | undefined> {
    const ownerRepo = extractGitHubOwnerRepo(repositoryUrl)
    if (!ownerRepo) {
      logger.debug(`Invalid GitHub URL: ${repositoryUrl}`)
      return undefined
    }

    const { owner, repo } = ownerRepo
    const contributors = await this.getContributors(owner, repo)

    return {
      repositoryUrl: normalizeGitHubUrl(repositoryUrl) ?? repositoryUrl,
      totalCommits: contributors.reduce((sum, c) => sum + c.commitCount, 0),
      contributors,
      analyzedAt: new Date(),
    }
  }

  /**
   * Fetch user profile by username
   */
  async getUser(username: string): Promise<GitHubUser | undefined> {
    await this.rateLimit()

    try {
      const response = await this.octokit.users.getByUsername({ username })
      return {
        login: response.data.login,
        name: response.data.name ?? undefined,
        email: response.data.email ?? undefined,
        blog: response.data.blog ?? undefined,
        company: response.data.company ?? undefined,
        location: response.data.location ?? undefined,
        bio: response.data.bio ?? undefined,
        type: response.data.type as 'User' | 'Organization',
      }
    } catch (error) {
      logger.debug(`Failed to fetch user ${username}`, { error })
      return undefined
    }
  }

  /**
   * Check if a user has GitHub Sponsors enabled
   */
  async hasSponsorsEnabled(username: string): Promise<boolean> {
    await this.rateLimit()

    try {
      const response = await this.graphqlClient<SponsorsQuery>(`
        query($login: String!) {
          user(login: $login) {
            hasSponsorsListing
            sponsorsListing {
              slug
            }
          }
        }
      `, { login: username })

      return response.user?.hasSponsorsListing ?? false
    } catch {
      return false
    }
  }

  /**
   * Get sponsorship tiers for a user
   */
  async getSponsorshipTiers(username: string): Promise<SponsorshipTier[]> {
    await this.rateLimit()

    try {
      const response = await this.graphqlClient<SponsorsListingQuery>(`
        query($login: String!) {
          user(login: $login) {
            sponsorsListing {
              tiers(first: 20) {
                nodes {
                  id
                  name
                  description
                  monthlyPriceInDollars
                  isOneTime
                  isCustomAmount
                }
              }
            }
          }
        }
      `, { login: username })

      return (
        response.user?.sponsorsListing?.tiers?.nodes?.map((tier) => ({
          id: tier.id,
          name: tier.name,
          description: tier.description ?? undefined,
          monthlyPrice: tier.monthlyPriceInDollars,
          isOneTime: tier.isOneTime,
          isCustomAmount: tier.isCustomAmount,
        })) ?? []
      )
    } catch {
      return []
    }
  }

  /**
   * Get file content from a repository
   */
  private async getFileContent(
    owner: string,
    repo: string,
    path: string
  ): Promise<string | undefined> {
    try {
      const response = await this.octokit.repos.getContent({
        owner,
        repo,
        path,
      })

      if ('content' in response.data && response.data.encoding === 'base64') {
        return Buffer.from(response.data.content, 'base64').toString('utf-8')
      }

      return undefined
    } catch {
      return undefined
    }
  }

  /**
   * Parse FUNDING.yml content
   */
  private parseFundingYml(content: string): FundingInfo {
    const funding: FundingInfo = {}

    for (const line of content.split('\n')) {
      const trimmed = line.trim()
      if (!trimmed || trimmed.startsWith('#')) continue

      const colonIndex = trimmed.indexOf(':')
      if (colonIndex === -1) continue

      const key = trimmed.slice(0, colonIndex).trim().toLowerCase()
      const value = trimmed.slice(colonIndex + 1).trim()

      // Handle array values [user1, user2] or single values
      const parseValue = (val: string): string[] => {
        if (val.startsWith('[') && val.endsWith(']')) {
          return val
            .slice(1, -1)
            .split(',')
            .map((v) => v.trim().replace(/^['"]|['"]$/g, ''))
            .filter(Boolean)
        }
        const cleaned = val.replace(/^['"]|['"]$/g, '').trim()
        return cleaned ? [cleaned] : []
      }

      switch (key) {
        case 'github':
          funding.github = parseValue(value)
          break
        case 'open_collective':
          funding.openCollective = parseValue(value)[0]
          break
        case 'ko_fi':
          funding.koFi = parseValue(value)[0]
          break
        case 'patreon':
          funding.patreon = parseValue(value)[0]
          break
        case 'tidelift':
          funding.tidelift = parseValue(value)[0]
          break
        case 'custom':
          funding.custom = parseValue(value)
          break
      }
    }

    return funding
  }
}

// Type definitions
export interface GitHubUser {
  login: string
  name?: string
  email?: string
  blog?: string
  company?: string
  location?: string
  bio?: string
  type: 'User' | 'Organization'
}

export interface SponsorshipTier {
  id: string
  name: string
  description?: string
  monthlyPrice: number
  isOneTime: boolean
  isCustomAmount: boolean
}

interface SponsorsQuery {
  user?: {
    hasSponsorsListing: boolean
    sponsorsListing?: {
      slug: string
    }
  }
}

interface SponsorsListingQuery {
  user?: {
    sponsorsListing?: {
      tiers?: {
        nodes?: Array<{
          id: string
          name: string
          description?: string
          monthlyPriceInDollars: number
          isOneTime: boolean
          isCustomAmount: boolean
        }>
      }
    }
  }
}
