/**
 * Utility Tests
 */

import { describe, it, expect } from 'vitest'
import {
  extractEmailFromAuthorString,
  normalizeGitHubUrl,
  extractGitHubOwnerRepo,
  packageKey,
  parsePackageKey,
  isValidEmail,
  isValidEthereumAddress,
  isValidBitcoinAddress,
  createRateLimiter,
  withRetry,
} from '../src/utils/index.js'

describe('extractEmailFromAuthorString', () => {
  it('extracts name and email from standard format', () => {
    const result = extractEmailFromAuthorString('John Doe <john@example.com>')
    expect(result.name).toBe('John Doe')
    expect(result.email).toBe('john@example.com')
  })

  it('handles email-only format', () => {
    const result = extractEmailFromAuthorString('john@example.com')
    expect(result.name).toBe('john')
    expect(result.email).toBe('john@example.com')
  })

  it('handles name-only format', () => {
    const result = extractEmailFromAuthorString('John Doe')
    expect(result.name).toBe('John Doe')
    expect(result.email).toBeUndefined()
  })
})

describe('normalizeGitHubUrl', () => {
  it('normalizes SSH URLs', () => {
    expect(normalizeGitHubUrl('git@github.com:user/repo.git'))
      .toBe('https://github.com/user/repo')
  })

  it('normalizes git:// URLs', () => {
    expect(normalizeGitHubUrl('git://github.com/user/repo.git'))
      .toBe('https://github.com/user/repo')
  })

  it('removes .git suffix from HTTPS URLs', () => {
    expect(normalizeGitHubUrl('https://github.com/user/repo.git'))
      .toBe('https://github.com/user/repo')
  })

  it('returns undefined for non-GitHub URLs', () => {
    expect(normalizeGitHubUrl('https://gitlab.com/user/repo'))
      .toBeUndefined()
  })
})

describe('extractGitHubOwnerRepo', () => {
  it('extracts owner and repo from HTTPS URL', () => {
    const result = extractGitHubOwnerRepo('https://github.com/hanzoai/oss-payments')
    expect(result?.owner).toBe('hanzoai')
    expect(result?.repo).toBe('oss-payments')
  })

  it('extracts owner and repo from SSH URL', () => {
    const result = extractGitHubOwnerRepo('git@github.com:hanzoai/oss-payments.git')
    expect(result?.owner).toBe('hanzoai')
    expect(result?.repo).toBe('oss-payments')
  })

  it('returns undefined for invalid URLs', () => {
    expect(extractGitHubOwnerRepo('not-a-url')).toBeUndefined()
    expect(extractGitHubOwnerRepo('https://gitlab.com/user/repo')).toBeUndefined()
  })
})

describe('packageKey', () => {
  it('creates package key', () => {
    expect(packageKey('lodash', 'npm')).toBe('npm:lodash')
  })

  it('handles scoped packages', () => {
    expect(packageKey('@types/node', 'npm')).toBe('npm:@types/node')
  })
})

describe('parsePackageKey', () => {
  it('parses package key', () => {
    const result = parsePackageKey('npm:lodash')
    expect(result.ecosystem).toBe('npm')
    expect(result.name).toBe('lodash')
  })

  it('handles scoped packages', () => {
    const result = parsePackageKey('npm:@types/node')
    expect(result.ecosystem).toBe('npm')
    expect(result.name).toBe('@types/node')
  })
})

describe('isValidEmail', () => {
  it('validates correct emails', () => {
    expect(isValidEmail('test@example.com')).toBe(true)
    expect(isValidEmail('user.name@domain.org')).toBe(true)
  })

  it('rejects invalid emails', () => {
    expect(isValidEmail('not-an-email')).toBe(false)
    expect(isValidEmail('@domain.com')).toBe(false)
    expect(isValidEmail('user@')).toBe(false)
  })
})

describe('isValidEthereumAddress', () => {
  it('validates correct addresses', () => {
    expect(isValidEthereumAddress('0x742d35Cc6634C0532925a3b844Bc9e7595f2bD40')).toBe(true)
    expect(isValidEthereumAddress('0x0000000000000000000000000000000000000000')).toBe(true)
  })

  it('rejects invalid addresses', () => {
    expect(isValidEthereumAddress('0x123')).toBe(false)
    expect(isValidEthereumAddress('742d35Cc6634C0532925a3b844Bc9e7595f2bD40')).toBe(false)
  })
})

describe('isValidBitcoinAddress', () => {
  it('validates correct addresses', () => {
    // Legacy
    expect(isValidBitcoinAddress('1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2')).toBe(true)
    // P2SH
    expect(isValidBitcoinAddress('3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy')).toBe(true)
    // Bech32
    expect(isValidBitcoinAddress('bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq')).toBe(true)
  })

  it('rejects invalid addresses', () => {
    expect(isValidBitcoinAddress('0x742d35Cc6634C0532925a3b844Bc9e7595f2bD40')).toBe(false)
    expect(isValidBitcoinAddress('notabitcoinaddress')).toBe(false)
  })
})

describe('createRateLimiter', () => {
  it('limits request rate', async () => {
    const rateLimit = createRateLimiter(10) // 10 per second = 100ms interval

    const start = Date.now()

    await rateLimit()
    await rateLimit()
    await rateLimit()

    const elapsed = Date.now() - start

    // Should take at least 200ms for 3 calls at 100ms intervals
    // (first call is instant, second and third wait)
    expect(elapsed).toBeGreaterThanOrEqual(180) // Allow some variance
  })
})

describe('withRetry', () => {
  it('succeeds on first try', async () => {
    let attempts = 0
    const result = await withRetry(
      async () => {
        attempts++
        return 'success'
      },
      { maxAttempts: 3, baseDelayMs: 10, maxDelayMs: 100 }
    )

    expect(result).toBe('success')
    expect(attempts).toBe(1)
  })

  it('retries on failure', async () => {
    let attempts = 0
    const result = await withRetry(
      async () => {
        attempts++
        if (attempts < 3) throw new Error('Failed')
        return 'success'
      },
      { maxAttempts: 3, baseDelayMs: 10, maxDelayMs: 100 }
    )

    expect(result).toBe('success')
    expect(attempts).toBe(3)
  })

  it('throws after max attempts', async () => {
    let attempts = 0

    await expect(
      withRetry(
        async () => {
          attempts++
          throw new Error('Always fails')
        },
        { maxAttempts: 3, baseDelayMs: 10, maxDelayMs: 100 }
      )
    ).rejects.toThrow('Always fails')

    expect(attempts).toBe(3)
  })

  it('respects shouldRetry option', async () => {
    let attempts = 0

    await expect(
      withRetry(
        async () => {
          attempts++
          const error = new Error('Non-retryable')
          ;(error as Error & { code: string }).code = 'FATAL'
          throw error
        },
        {
          maxAttempts: 3,
          baseDelayMs: 10,
          maxDelayMs: 100,
          shouldRetry: (err) => (err as Error & { code?: string }).code !== 'FATAL',
        }
      )
    ).rejects.toThrow('Non-retryable')

    expect(attempts).toBe(1) // No retries
  })
})
