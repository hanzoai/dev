/**
 * Attribution Engine
 *
 * Maps dependencies to their authors and calculates fair share distributions.
 */

import {
  type Dependency,
  type Author,
  type AuthorShare,
  type PackageAttribution,
  type FundingInfo,
  type Ecosystem,
  ContributorRole,
  type GitAnalysisResult,
} from '../types/index.js'
import { GitHubClient } from '../integrations/github.js'
import { NpmRegistryClient } from '../integrations/npm-registry.js'
import { CratesRegistryClient } from '../integrations/crates-registry.js'
import { generateId, createLogger, extractGitHubOwnerRepo, normalizeEmail } from '../utils/index.js'

const logger = createLogger('attribution')

export interface AttributionConfig {
  github: {
    token: string
    rateLimit?: number
  }
  npm?: {
    token?: string
    rateLimit?: number
  }
  crates?: {
    token?: string
    rateLimit?: number
  }
  /**
   * Weight factors for share calculation
   */
  weights?: {
    manifestAuthorWeight: number    // Default: 0.6 (60%)
    contributorWeight: number       // Default: 0.4 (40%)
    maxContributors: number         // Default: 20
  }
}

export interface AttributionResult {
  attribution: PackageAttribution
  authors: Author[]
  fundingInfo?: FundingInfo
  repositoryAnalysis?: GitAnalysisResult
}

export class AttributionEngine {
  private github: GitHubClient
  private npmRegistry: NpmRegistryClient
  private cratesRegistry: CratesRegistryClient
  private config: AttributionConfig
  private authorCache: Map<string, Author> = new Map()

  constructor(config: AttributionConfig) {
    this.config = {
      ...config,
      weights: {
        manifestAuthorWeight: 0.6,
        contributorWeight: 0.4,
        maxContributors: 20,
        ...config.weights,
      },
    }

    this.github = new GitHubClient({
      token: config.github.token,
      rateLimit: config.github.rateLimit,
    })

    this.npmRegistry = new NpmRegistryClient(config.npm)
    this.cratesRegistry = new CratesRegistryClient(config.crates)
  }

  /**
   * Attribute a dependency to its authors
   */
  async attributeDependency(
    dependency: Dependency
  ): Promise<AttributionResult> {
    logger.info(`Attributing ${dependency.ecosystem}:${dependency.name}`)

    const { ecosystem, name, version } = dependency

    // Fetch package metadata from registry
    const packageInfo = await this.fetchPackageInfo(ecosystem, name, version)

    // Extract repository URL
    const repositoryUrl = this.extractRepositoryUrl(packageInfo, ecosystem)

    // Fetch FUNDING.yml if we have a GitHub repo
    let fundingInfo: FundingInfo | undefined
    let repositoryAnalysis: GitAnalysisResult | undefined

    if (repositoryUrl) {
      const ownerRepo = extractGitHubOwnerRepo(repositoryUrl)
      if (ownerRepo) {
        fundingInfo = await this.github.getFunding(ownerRepo.owner, ownerRepo.repo)
        repositoryAnalysis = await this.github.analyzeRepository(repositoryUrl)
      }
    }

    // Merge funding info from package and FUNDING.yml
    const mergedFunding = this.mergeFundingInfo(
      this.extractFundingFromPackage(packageInfo, ecosystem),
      fundingInfo
    )

    // Calculate author shares
    const { shares, authors } = await this.calculateAuthorShares(
      packageInfo,
      ecosystem,
      repositoryAnalysis
    )

    const attribution: PackageAttribution = {
      id: generateId(),
      packageName: name,
      ecosystem,
      version,
      authors: shares,
      fundingUrls: this.extractFundingUrls(mergedFunding),
      license: this.extractLicense(packageInfo, ecosystem),
      repositoryUrl,
      lastFetched: new Date(),
    }

    return {
      attribution,
      authors,
      fundingInfo: mergedFunding,
      repositoryAnalysis,
    }
  }

  /**
   * Batch attribution for multiple dependencies
   */
  async attributeDependencies(
    dependencies: Dependency[]
  ): Promise<Map<string, AttributionResult>> {
    const results = new Map<string, AttributionResult>()

    // Process sequentially to respect rate limits
    for (const dep of dependencies) {
      try {
        const result = await this.attributeDependency(dep)
        results.set(`${dep.ecosystem}:${dep.name}`, result)
      } catch (error) {
        logger.error(`Failed to attribute ${dep.ecosystem}:${dep.name}`, { error })
      }
    }

    return results
  }

  /**
   * Calculate author shares for a package
   */
  private async calculateAuthorShares(
    packageInfo: PackageInfo | undefined,
    ecosystem: Ecosystem,
    repoAnalysis?: GitAnalysisResult
  ): Promise<{ shares: AuthorShare[]; authors: Author[] }> {
    const shares: AuthorShare[] = []
    const authors: Author[] = []
    const weights = this.config.weights!

    // Extract manifest authors
    const manifestAuthors = this.extractManifestAuthors(packageInfo, ecosystem)

    // Calculate manifest author shares
    if (manifestAuthors.length > 0) {
      const manifestSharePerAuthor = weights.manifestAuthorWeight / manifestAuthors.length

      for (const authorInfo of manifestAuthors) {
        const author = await this.resolveOrCreateAuthor(authorInfo)
        authors.push(author)
        shares.push({
          authorId: author.id,
          sharePercentage: manifestSharePerAuthor,
          role: authorInfo.role as ContributorRole,
        })
      }
    }

    // Calculate contributor shares from git analysis
    if (repoAnalysis && repoAnalysis.contributors.length > 0) {
      const topContributors = repoAnalysis.contributors.slice(0, weights.maxContributors)
      const totalCommits = topContributors.reduce((sum, c) => sum + c.commitCount, 0)

      for (const contributor of topContributors) {
        const sharePercentage = weights.contributorWeight * (contributor.commitCount / totalCommits)

        // Check if this contributor is already in shares (from manifest)
        const existingIndex = shares.findIndex((s) => {
          const existingAuthor = authors.find((a) => a.id === s.authorId)
          return (
            existingAuthor?.githubUsername === contributor.username ||
            existingAuthor?.emails.some((e) =>
              normalizeEmail(e) === normalizeEmail(contributor.email)
            )
          )
        })

        if (existingIndex >= 0) {
          // Add to existing share
          const existingShare = shares[existingIndex]
          if (existingShare) {
            existingShare.sharePercentage += sharePercentage
          }
        } else {
          // Create new author entry
          const author = await this.resolveOrCreateAuthor({
            name: contributor.name,
            email: contributor.email || undefined,
            githubUsername: contributor.username,
            role: 'contributor',
          })
          authors.push(author)
          shares.push({
            authorId: author.id,
            sharePercentage,
            role: ContributorRole.Contributor,
          })
        }
      }
    }

    // Normalize shares to sum to 1.0
    const totalShare = shares.reduce((sum, s) => sum + s.sharePercentage, 0)
    if (totalShare > 0) {
      for (const share of shares) {
        share.sharePercentage /= totalShare
      }
    }

    return { shares, authors }
  }

  /**
   * Resolve or create an author entity
   */
  private async resolveOrCreateAuthor(info: {
    name: string
    email?: string
    githubUsername?: string
    role: string
  }): Promise<Author> {
    // Check cache first
    const cacheKey = info.email ?? info.githubUsername ?? info.name
    const cached = this.authorCache.get(cacheKey)
    if (cached) return cached

    // Fetch GitHub profile if we have a username
    let githubUser
    if (info.githubUsername) {
      githubUser = await this.github.getUser(info.githubUsername)
    }

    const author: Author = {
      id: generateId(),
      name: githubUser?.name ?? info.name,
      emails: info.email ? [normalizeEmail(info.email)] : [],
      githubUsername: info.githubUsername ?? githubUser?.login,
      verified: false,
      createdAt: new Date(),
      updatedAt: new Date(),
    }

    // Merge email from GitHub if available
    if (githubUser?.email && !author.emails.includes(normalizeEmail(githubUser.email))) {
      author.emails.push(normalizeEmail(githubUser.email))
    }

    // Cache the author
    this.authorCache.set(cacheKey, author)

    return author
  }

  /**
   * Fetch package info from registry
   */
  private async fetchPackageInfo(
    ecosystem: Ecosystem,
    name: string,
    version?: string
  ): Promise<PackageInfo | undefined> {
    switch (ecosystem) {
      case 'npm':
        return this.npmRegistry.getPackage(name, version) as Promise<PackageInfo | undefined>
      case 'cargo':
        return this.cratesRegistry.getCrate(name, version) as Promise<PackageInfo | undefined>
      // PyPI and Go would need their own registry clients
      default:
        return undefined
    }
  }

  /**
   * Extract manifest authors from package info
   */
  private extractManifestAuthors(
    packageInfo: PackageInfo | undefined,
    ecosystem: Ecosystem
  ): ManifestAuthor[] {
    if (!packageInfo) return []

    switch (ecosystem) {
      case 'npm': {
        const pkg = packageInfo as NpmPkg
        return this.npmRegistry.extractAuthors(pkg).map((a) => ({
          ...a,
          role: a.role,
        }))
      }
      case 'cargo': {
        const crate = packageInfo as CratesPkg
        const authors = this.cratesRegistry.extractAuthors(crate)
        return authors.map((a) => ({
          name: a.name,
          email: a.email,
          githubUsername: a.githubUsername,
          role: a.role,
        }))
      }
      default:
        return []
    }
  }

  /**
   * Extract repository URL from package info
   */
  private extractRepositoryUrl(
    packageInfo: PackageInfo | undefined,
    ecosystem: Ecosystem
  ): string | undefined {
    if (!packageInfo) return undefined

    switch (ecosystem) {
      case 'npm': {
        const pkg = packageInfo as NpmPkg
        return this.npmRegistry.extractRepositoryUrl(pkg)
      }
      case 'cargo': {
        const crate = packageInfo as CratesPkg
        return crate.repository
      }
      default:
        return undefined
    }
  }

  /**
   * Extract funding info from package metadata
   */
  private extractFundingFromPackage(
    packageInfo: PackageInfo | undefined,
    ecosystem: Ecosystem
  ): FundingInfo {
    if (!packageInfo) return {}

    switch (ecosystem) {
      case 'npm': {
        const pkg = packageInfo as NpmPkg
        return this.npmRegistry.extractFunding(pkg)
      }
      case 'cargo': {
        const crate = packageInfo as CratesPkg
        return this.cratesRegistry.extractFunding(crate)
      }
      default:
        return {}
    }
  }

  /**
   * Extract license from package info
   */
  private extractLicense(
    packageInfo: PackageInfo | undefined,
    _ecosystem: Ecosystem
  ): string | undefined {
    if (!packageInfo) return undefined
    return (packageInfo as { license?: string }).license
  }

  /**
   * Merge funding info from multiple sources
   */
  private mergeFundingInfo(
    ...sources: (FundingInfo | undefined)[]
  ): FundingInfo {
    const merged: FundingInfo = {
      github: [],
      custom: [],
    }

    for (const source of sources) {
      if (!source) continue

      if (source.github) {
        for (const gh of source.github) {
          if (!merged.github!.includes(gh)) {
            merged.github!.push(gh)
          }
        }
      }
      if (source.openCollective && !merged.openCollective) {
        merged.openCollective = source.openCollective
      }
      if (source.koFi && !merged.koFi) {
        merged.koFi = source.koFi
      }
      if (source.patreon && !merged.patreon) {
        merged.patreon = source.patreon
      }
      if (source.tidelift && !merged.tidelift) {
        merged.tidelift = source.tidelift
      }
      if (source.custom) {
        for (const url of source.custom) {
          if (!merged.custom!.includes(url)) {
            merged.custom!.push(url)
          }
        }
      }
    }

    return merged
  }

  /**
   * Extract funding URLs from funding info
   */
  private extractFundingUrls(funding: FundingInfo): string[] {
    const urls: string[] = []

    if (funding.github) {
      for (const user of funding.github) {
        urls.push(`https://github.com/sponsors/${user}`)
      }
    }
    if (funding.openCollective) {
      urls.push(`https://opencollective.com/${funding.openCollective}`)
    }
    if (funding.koFi) {
      urls.push(`https://ko-fi.com/${funding.koFi}`)
    }
    if (funding.patreon) {
      urls.push(`https://patreon.com/${funding.patreon}`)
    }
    if (funding.tidelift) {
      urls.push(funding.tidelift)
    }
    if (funding.custom) {
      urls.push(...funding.custom)
    }

    return urls
  }
}

// Internal types
type PackageInfo = NpmPkg | CratesPkg

interface NpmPkg {
  name: string
  version: string
  author?: string | { name: string; email?: string }
  contributors?: Array<string | { name: string; email?: string }>
  maintainers?: Array<{ name: string; email?: string }>
  repository?: string | { url: string }
  funding?: string | { url: string } | Array<{ url: string }>
  license?: string
}

interface CratesPkg {
  name: string
  version: string
  authors?: string[]
  repository?: string
  license?: string
}

interface ManifestAuthor {
  name: string
  email?: string
  githubUsername?: string
  role: string
}
