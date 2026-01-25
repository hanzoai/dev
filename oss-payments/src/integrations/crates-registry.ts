/**
 * Crates.io Registry Integration
 *
 * Fetches package metadata from crates.io (Rust ecosystem)
 */

import got from 'got'
import type { CratesPackageInfo, FundingInfo } from '../types/index.js'
import { createLogger, createRateLimiter, withRetry, extractEmailFromAuthorString } from '../utils/index.js'

const logger = createLogger('crates-registry')

const CRATES_API_URL = 'https://crates.io/api/v1'

export interface CratesRegistryConfig {
  token?: string
  rateLimit?: number
}

export class CratesRegistryClient {
  private rateLimit: () => Promise<void>
  private headers: Record<string, string>

  constructor(config: CratesRegistryConfig = {}) {
    // crates.io requires a User-Agent
    this.headers = {
      'Accept': 'application/json',
      'User-Agent': 'oss-payments/1.0 (https://github.com/hanzoai/oss-payments)',
    }
    if (config.token) {
      this.headers['Authorization'] = config.token
    }
    // crates.io has strict rate limits (1 req/sec)
    this.rateLimit = createRateLimiter(config.rateLimit ?? 1)
  }

  /**
   * Fetch crate metadata
   */
  async getCrate(name: string, version?: string): Promise<CratesPackageInfo | undefined> {
    await this.rateLimit()

    const url = version
      ? `${CRATES_API_URL}/crates/${encodeURIComponent(name)}/${version}`
      : `${CRATES_API_URL}/crates/${encodeURIComponent(name)}`

    try {
      const response = await withRetry(
        async () => {
          return got.get(url, {
            headers: this.headers,
            responseType: 'json',
          }).json<CratesResponse>()
        },
        {
          maxAttempts: 3,
          baseDelayMs: 1000,
          maxDelayMs: 10000,
          shouldRetry: (err) => {
            if (err instanceof got.HTTPError) {
              // Don't retry 404s
              return err.response.statusCode !== 404
            }
            return true
          },
        }
      )

      // If no version specified, use the latest
      const versionData = version
        ? response.version
        : response.crate?.newest_version
          ? await this.getCrateVersion(name, response.crate.newest_version)
          : undefined

      return {
        name: response.crate?.id ?? name,
        version: versionData?.num ?? response.crate?.newest_version ?? '',
        description: response.crate?.description,
        authors: versionData?.authors ?? [],
        repository: response.crate?.repository ?? versionData?.repository,
        homepage: response.crate?.homepage,
        license: versionData?.license,
        documentation: response.crate?.documentation,
      }
    } catch (error) {
      if (error instanceof got.HTTPError && error.response.statusCode === 404) {
        logger.debug(`Crate not found: ${name}@${version ?? 'latest'}`)
      } else {
        logger.warn(`Failed to fetch crate ${name}`, { error })
      }
      return undefined
    }
  }

  /**
   * Fetch specific version details
   */
  private async getCrateVersion(
    name: string,
    version: string
  ): Promise<CrateVersionData | undefined> {
    await this.rateLimit()

    try {
      const response = await got.get(
        `${CRATES_API_URL}/crates/${encodeURIComponent(name)}/${version}`,
        {
          headers: this.headers,
          responseType: 'json',
        }
      ).json<{ version: CrateVersionData }>()

      return response.version
    } catch {
      return undefined
    }
  }

  /**
   * Fetch crate owners (maintainers)
   */
  async getCrateOwners(name: string): Promise<CrateOwner[]> {
    await this.rateLimit()

    try {
      const response = await got.get(
        `${CRATES_API_URL}/crates/${encodeURIComponent(name)}/owners`,
        {
          headers: this.headers,
          responseType: 'json',
        }
      ).json<{ users: CrateOwner[] }>()

      return response.users ?? []
    } catch (error) {
      logger.debug(`Failed to fetch owners for crate ${name}`, { error })
      return []
    }
  }

  /**
   * Extract author information from crate
   */
  extractAuthors(crate: CratesPackageInfo, owners: CrateOwner[] = []): PackageAuthorInfo[] {
    const authors: PackageAuthorInfo[] = []
    const seen = new Set<string>()

    // Authors from Cargo.toml
    if (crate.authors) {
      for (const authorStr of crate.authors) {
        const { name, email } = extractEmailFromAuthorString(authorStr)
        const key = email ?? name
        if (!seen.has(key)) {
          seen.add(key)
          authors.push({
            name,
            email,
            role: 'primary',
          })
        }
      }
    }

    // Owners (maintainers) from crates.io
    for (const owner of owners) {
      const key = owner.login
      if (!seen.has(key)) {
        seen.add(key)
        authors.push({
          name: owner.name ?? owner.login,
          email: undefined, // crates.io doesn't expose emails
          githubUsername: owner.login,
          role: 'maintainer',
          url: owner.url,
        })
      }
    }

    return authors
  }

  /**
   * Crates.io doesn't have native funding info, so we return empty
   * and rely on FUNDING.yml from GitHub repository
   */
  extractFunding(_crate: CratesPackageInfo): FundingInfo {
    return {}
  }
}

// Type definitions
interface CratesResponse {
  crate?: {
    id: string
    name: string
    description?: string
    homepage?: string
    repository?: string
    documentation?: string
    newest_version?: string
    downloads: number
    recent_downloads: number
  }
  version?: CrateVersionData
  versions?: CrateVersionData[]
}

interface CrateVersionData {
  id: number
  crate: string
  num: string
  license?: string
  yanked: boolean
  downloads: number
  size: number
  checksum: string
  authors?: string[]
  repository?: string
  created_at: string
  updated_at: string
}

interface CrateOwner {
  id: number
  login: string
  name?: string
  url?: string
  avatar?: string
  kind: 'user' | 'team'
}

export interface PackageAuthorInfo {
  name: string
  email?: string
  githubUsername?: string
  url?: string
  role: 'primary' | 'maintainer' | 'contributor'
}
