/**
 * Utility functions for OSS Payment System
 */

import { randomUUID } from 'crypto'
import Decimal from 'decimal.js'

// ============================================================================
// ID Generation
// ============================================================================

export function generateId(): string {
  return randomUUID()
}

// ============================================================================
// Decimal Utilities
// ============================================================================

export function toDecimal(value: number | string | Decimal): Decimal {
  return new Decimal(value)
}

export function sumDecimals(values: Decimal[]): Decimal {
  return values.reduce((sum, val) => sum.plus(val), new Decimal(0))
}

export function percentageOf(amount: Decimal, percentage: number): Decimal {
  return amount.mul(percentage)
}

// ============================================================================
// Date Utilities
// ============================================================================

export function daysSince(date: Date): number {
  const now = new Date()
  const diff = now.getTime() - date.getTime()
  return Math.floor(diff / (1000 * 60 * 60 * 24))
}

export function isWithinDays(date: Date, days: number): boolean {
  return daysSince(date) < days
}

// ============================================================================
// String Utilities
// ============================================================================

export function normalizeEmail(email: string): string {
  return email.toLowerCase().trim()
}

export function extractEmailFromAuthorString(
  author: string
): { name: string; email?: string } {
  // Handle format: "Name <email@example.com>"
  const match = author.match(/^([^<]+)\s*<([^>]+)>/)
  if (match) {
    return {
      name: match[1]?.trim() ?? author,
      email: match[2]?.trim(),
    }
  }

  // Handle format: "email@example.com"
  if (author.includes('@')) {
    return {
      name: author.split('@')[0] ?? author,
      email: author,
    }
  }

  return { name: author.trim() }
}

export function normalizeGitHubUrl(url: string): string | undefined {
  if (!url) return undefined

  // Remove trailing .git
  let normalized = url.replace(/\.git$/, '')

  // Convert SSH to HTTPS
  if (normalized.startsWith('git@github.com:')) {
    normalized = normalized.replace('git@github.com:', 'https://github.com/')
  }

  // Handle git:// protocol
  if (normalized.startsWith('git://github.com/')) {
    normalized = normalized.replace('git://', 'https://')
  }

  // Validate it's a GitHub URL
  if (!normalized.startsWith('https://github.com/')) {
    return undefined
  }

  return normalized
}

export function extractGitHubOwnerRepo(
  url: string
): { owner: string; repo: string } | undefined {
  const normalized = normalizeGitHubUrl(url)
  if (!normalized) return undefined

  const match = normalized.match(/github\.com\/([^/]+)\/([^/]+)/)
  if (match?.[1] && match[2]) {
    return { owner: match[1], repo: match[2] }
  }
  return undefined
}

// ============================================================================
// Package Key Utilities
// ============================================================================

export function packageKey(name: string, ecosystem: string): string {
  return `${ecosystem}:${name}`
}

export function parsePackageKey(key: string): { name: string; ecosystem: string } {
  const [ecosystem, ...nameParts] = key.split(':')
  return {
    ecosystem: ecosystem ?? '',
    name: nameParts.join(':'),
  }
}

// ============================================================================
// Rate Limiting
// ============================================================================

export async function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

export function createRateLimiter(requestsPerSecond: number) {
  let lastRequest = 0
  const minInterval = 1000 / requestsPerSecond

  return async function rateLimit(): Promise<void> {
    const now = Date.now()
    const elapsed = now - lastRequest
    if (elapsed < minInterval) {
      await sleep(minInterval - elapsed)
    }
    lastRequest = Date.now()
  }
}

// ============================================================================
// Retry Logic
// ============================================================================

export interface RetryOptions {
  maxAttempts: number
  baseDelayMs: number
  maxDelayMs: number
  shouldRetry?: (error: unknown) => boolean
}

export async function withRetry<T>(
  fn: () => Promise<T>,
  options: RetryOptions
): Promise<T> {
  const { maxAttempts, baseDelayMs, maxDelayMs, shouldRetry } = options
  let lastError: unknown

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      return await fn()
    } catch (error) {
      lastError = error

      if (shouldRetry && !shouldRetry(error)) {
        throw error
      }

      if (attempt === maxAttempts) {
        throw error
      }

      // Exponential backoff with jitter
      const delay = Math.min(
        baseDelayMs * Math.pow(2, attempt - 1) + Math.random() * 100,
        maxDelayMs
      )
      await sleep(delay)
    }
  }

  throw lastError
}

// ============================================================================
// Validation Helpers
// ============================================================================

export function isValidEmail(email: string): boolean {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/
  return emailRegex.test(email)
}

export function isValidUrl(url: string): boolean {
  try {
    new URL(url)
    return true
  } catch {
    return false
  }
}

export function isValidEthereumAddress(address: string): boolean {
  return /^0x[a-fA-F0-9]{40}$/.test(address)
}

export function isValidBitcoinAddress(address: string): boolean {
  // Basic validation - supports legacy, SegWit, and Bech32
  return /^(1|3|bc1)[a-zA-HJ-NP-Z0-9]{25,62}$/.test(address)
}

// ============================================================================
// Batch Processing
// ============================================================================

export async function processBatch<T, R>(
  items: T[],
  batchSize: number,
  processor: (batch: T[]) => Promise<R[]>
): Promise<R[]> {
  const results: R[] = []

  for (let i = 0; i < items.length; i += batchSize) {
    const batch = items.slice(i, i + batchSize)
    const batchResults = await processor(batch)
    results.push(...batchResults)
  }

  return results
}

export async function processParallel<T, R>(
  items: T[],
  concurrency: number,
  processor: (item: T) => Promise<R>
): Promise<R[]> {
  const results: R[] = new Array(items.length)
  let currentIndex = 0

  async function processNext(): Promise<void> {
    while (currentIndex < items.length) {
      const index = currentIndex++
      const item = items[index]
      if (item !== undefined) {
        results[index] = await processor(item)
      }
    }
  }

  const workers = Array.from({ length: Math.min(concurrency, items.length) }, processNext)
  await Promise.all(workers)

  return results
}

// ============================================================================
// Logging Utilities
// ============================================================================

export type LogLevel = 'debug' | 'info' | 'warn' | 'error'

export interface Logger {
  debug(message: string, meta?: Record<string, unknown>): void
  info(message: string, meta?: Record<string, unknown>): void
  warn(message: string, meta?: Record<string, unknown>): void
  error(message: string, meta?: Record<string, unknown>): void
}

export function createLogger(prefix: string): Logger {
  const log = (level: LogLevel, message: string, meta?: Record<string, unknown>) => {
    const timestamp = new Date().toISOString()
    const metaStr = meta ? ` ${JSON.stringify(meta)}` : ''
    console[level](`[${timestamp}] [${prefix}] ${level.toUpperCase()}: ${message}${metaStr}`)
  }

  return {
    debug: (msg, meta) => log('debug', msg, meta),
    info: (msg, meta) => log('info', msg, meta),
    warn: (msg, meta) => log('warn', msg, meta),
    error: (msg, meta) => log('error', msg, meta),
  }
}
