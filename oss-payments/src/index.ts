/**
 * OSS Payment System
 *
 * Automated tracking and distribution of payments to open-source authors.
 *
 * @packageDocumentation
 */

// Core service
export { OSSPaymentService, createOSSPaymentService } from './core/index.js'

// Types
export * from './types/index.js'

// Scanners
export {
  DependencyScanner,
  NpmScanner,
  CargoScanner,
  PythonScanner,
  GoScanner,
  createScanner,
  detectEcosystem,
  getManifestPatterns,
  getLockfilePatterns,
  type ScannerOptions,
} from './scanners/index.js'

// Attribution
export { AttributionEngine, type AttributionConfig, type AttributionResult } from './attribution/index.js'

// Distribution
export {
  DistributionCalculator,
  createDistributionCalculator,
  type DistributionInput,
  type CalculatedPayment,
} from './distribution/index.js'

// Integrations
export {
  GitHubClient,
  NpmRegistryClient,
  CratesRegistryClient,
  OpenCollectiveClient,
  type GitHubConfig,
  type NpmRegistryConfig,
  type CratesRegistryConfig,
  type OpenCollectiveConfig,
} from './integrations/index.js'

// Database
export { createDatabase, tables } from './db/index.js'

// Utilities
export {
  generateId,
  toDecimal,
  sumDecimals,
  percentageOf,
  daysSince,
  normalizeEmail,
  extractEmailFromAuthorString,
  normalizeGitHubUrl,
  extractGitHubOwnerRepo,
  createRateLimiter,
  withRetry,
  createLogger,
  type Logger,
  type LogLevel,
} from './utils/index.js'
