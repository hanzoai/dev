/**
 * NPM Registry Integration
 *
 * Fetches package metadata from npmjs.com
 */

import got from 'got'
import type { NpmPackageInfo, FundingInfo } from '../types/index.js'
import { createLogger, createRateLimiter, withRetry, extractEmailFromAuthorString } from '../utils/index.js'

const logger = createLogger('npm-registry')

const NPM_REGISTRY_URL = 'https://registry.npmjs.org'

export interface NpmRegistryConfig {
  token?: string
  rateLimit?: number
}

export class NpmRegistryClient {
  private rateLimit: () => Promise<void>
  private headers: Record<string, string>

  constructor(config: NpmRegistryConfig = {}) {
    this.rateLimit = createRateLimiter(config.rateLimit ?? 10)
    this.headers = {
      'Accept': 'application/json',
    }
    if (config.token) {
      this.headers['Authorization'] = `Bearer ${config.token}`
    }
  }

  /**
   * Fetch package metadata
   */
  async getPackage(name: string, version?: string): Promise<NpmPackageInfo | undefined> {
    await this.rateLimit()

    const url = version
      ? `${NPM_REGISTRY_URL}/${encodeURIComponent(name)}/${version}`
      : `${NPM_REGISTRY_URL}/${encodeURIComponent(name)}/latest`

    try {
      const response = await withRetry(
        async () => {
          return got.get(url, {
            headers: this.headers,
            responseType: 'json',
          }).json<NpmPackageResponse>()
        },
        {
          maxAttempts: 3,
          baseDelayMs: 500,
          maxDelayMs: 5000,
          shouldRetry: (err) => {
            if (err instanceof got.HTTPError) {
              return err.response.statusCode !== 404
            }
            return true
          },
        }
      )

      return {
        name: response.name,
        version: response.version,
        description: response.description,
        author: response.author,
        contributors: response.contributors,
        maintainers: response.maintainers,
        repository: response.repository,
        homepage: response.homepage,
        license: response.license,
        funding: response.funding,
      }
    } catch (error) {
      if (error instanceof got.HTTPError && error.response.statusCode === 404) {
        logger.debug(`Package not found: ${name}@${version ?? 'latest'}`)
      } else {
        logger.warn(`Failed to fetch package ${name}`, { error })
      }
      return undefined
    }
  }

  /**
   * Extract author information from package
   */
  extractAuthors(pkg: NpmPackageInfo): PackageAuthorInfo[] {
    const authors: PackageAuthorInfo[] = []
    const seen = new Set<string>()

    // Primary author
    if (pkg.author) {
      const author = this.parseAuthor(pkg.author)
      if (author && !seen.has(author.email ?? author.name)) {
        seen.add(author.email ?? author.name)
        authors.push({ ...author, role: 'primary' })
      }
    }

    // Maintainers
    if (pkg.maintainers) {
      for (const m of pkg.maintainers) {
        if (!seen.has(m.email ?? m.name)) {
          seen.add(m.email ?? m.name)
          authors.push({
            name: m.name,
            email: m.email,
            role: 'maintainer',
          })
        }
      }
    }

    // Contributors
    if (pkg.contributors) {
      for (const c of pkg.contributors) {
        const author = this.parseAuthor(c)
        if (author && !seen.has(author.email ?? author.name)) {
          seen.add(author.email ?? author.name)
          authors.push({ ...author, role: 'contributor' })
        }
      }
    }

    return authors
  }

  /**
   * Extract funding information from package
   */
  extractFunding(pkg: NpmPackageInfo): FundingInfo {
    const funding: FundingInfo = {
      custom: [],
    }

    if (!pkg.funding) return funding

    const processFunding = (f: string | { type: string; url: string }) => {
      if (typeof f === 'string') {
        this.categorizeFundingUrl(f, funding)
      } else if (f.url) {
        this.categorizeFundingUrl(f.url, funding)
      }
    }

    if (Array.isArray(pkg.funding)) {
      for (const f of pkg.funding) {
        processFunding(f)
      }
    } else {
      processFunding(pkg.funding)
    }

    return funding
  }

  /**
   * Extract repository URL from package
   */
  extractRepositoryUrl(pkg: NpmPackageInfo): string | undefined {
    if (!pkg.repository) return undefined

    if (typeof pkg.repository === 'string') {
      return this.normalizeRepositoryUrl(pkg.repository)
    }

    if (pkg.repository.url) {
      return this.normalizeRepositoryUrl(pkg.repository.url)
    }

    return undefined
  }

  private parseAuthor(
    author: string | { name: string; email?: string; url?: string }
  ): { name: string; email?: string; url?: string } | undefined {
    if (typeof author === 'string') {
      const { name, email } = extractEmailFromAuthorString(author)
      return { name, email }
    }
    return author
  }

  private categorizeFundingUrl(url: string, funding: FundingInfo): void {
    const lower = url.toLowerCase()

    if (lower.includes('github.com/sponsors')) {
      const match = url.match(/github\.com\/sponsors\/([^/?#]+)/)
      if (match?.[1]) {
        funding.github = funding.github ?? []
        funding.github.push(match[1])
      }
    } else if (lower.includes('opencollective.com')) {
      const match = url.match(/opencollective\.com\/([^/?#]+)/)
      if (match?.[1]) {
        funding.openCollective = match[1]
      }
    } else if (lower.includes('ko-fi.com')) {
      const match = url.match(/ko-fi\.com\/([^/?#]+)/)
      if (match?.[1]) {
        funding.koFi = match[1]
      }
    } else if (lower.includes('patreon.com')) {
      const match = url.match(/patreon\.com\/([^/?#]+)/)
      if (match?.[1]) {
        funding.patreon = match[1]
      }
    } else if (lower.includes('tidelift.com')) {
      funding.tidelift = url
    } else {
      funding.custom = funding.custom ?? []
      if (!funding.custom.includes(url)) {
        funding.custom.push(url)
      }
    }
  }

  private normalizeRepositoryUrl(url: string): string {
    // Handle shorthand formats like "github:user/repo"
    if (url.startsWith('github:')) {
      return `https://github.com/${url.slice(7)}`
    }
    if (url.startsWith('gitlab:')) {
      return `https://gitlab.com/${url.slice(7)}`
    }
    if (url.startsWith('bitbucket:')) {
      return `https://bitbucket.org/${url.slice(10)}`
    }

    // Clean up git URLs
    return url
      .replace(/^git\+/, '')
      .replace(/\.git$/, '')
      .replace(/^git:\/\//, 'https://')
      .replace(/^git@github\.com:/, 'https://github.com/')
  }
}

// Type definitions
interface NpmPackageResponse {
  name: string
  version: string
  description?: string
  author?: string | { name: string; email?: string; url?: string }
  contributors?: Array<string | { name: string; email?: string; url?: string }>
  maintainers?: Array<{ name: string; email?: string }>
  repository?: { type?: string; url: string } | string
  homepage?: string
  license?: string
  funding?: string | { type: string; url: string } | Array<{ type: string; url: string }>
}

export interface PackageAuthorInfo {
  name: string
  email?: string
  url?: string
  role: 'primary' | 'maintainer' | 'contributor'
}
